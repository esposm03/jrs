mod util;

use std::{
    borrow::Cow,
    collections::HashMap,
    sync::atomic::{AtomicU32, Ordering},
};

use cafebabe::{
    bytecode::{ByteCode, Opcode},
    constant_pool::{LiteralConstant, Loadable, MemberRef, NameAndType},
    descriptor::ReturnDescriptor,
};
use cranelift_codegen::{
    ir::{condcodes::IntCC, types, Block, Function, InstBuilder, Signature, UserFuncName, Value},
    Context,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module};
use util::{fieldtype_to_abiparam, MethodData, Stack, Variables};

static NEXT_FUNC_NUMBER: AtomicU32 = AtomicU32::new(0);

pub struct Codegen {
    module: JITModule,
    functions: HashMap<MemberRef<'static>, FuncId>,
}

/// Code generation for single functions
pub struct FuncCodegen<'a> {
    pub codegen: &'a mut Codegen,
    pub builder: FunctionBuilder<'a>,
    pub context: Context,
    pub signature: Signature,
    pub blocks: HashMap<usize, Block>,

    pub bytecode: ByteCode<'a>,
    pub max_stack: usize,
    pub max_locals: usize,
}

impl<'a> FuncCodegen<'a> {
    /// Build a function implementing the given method
    pub fn build(codegen: &'a mut Codegen, method_data: MethodData) -> FuncId {
        // Signature
        let mut signature = codegen.module.make_signature();
        for param in method_data.parameters {
            signature.params.push(fieldtype_to_abiparam(&param));
        }
        if let ReturnDescriptor::Return(ret) = method_data.result {
            signature.returns.push(fieldtype_to_abiparam(&ret));
        }

        // Function & builder
        let func_num = NEXT_FUNC_NUMBER.fetch_add(1, Ordering::SeqCst);
        let mut func =
            Function::with_name_signature(UserFuncName::user(0, func_num), signature.clone());
        let mut func_ctx = FunctionBuilderContext::new();
        let builder = FunctionBuilder::new(&mut func, &mut func_ctx);
        let func_id = codegen
            .module
            .declare_function(&method_data.name, Linkage::Local, &signature)
            .unwrap();

        let mut func_codegen = FuncCodegen {
            codegen,
            signature,
            bytecode: method_data.bytecode,
            builder,
            context: Context::new(),
            blocks: HashMap::new(),
            max_stack: method_data.max_stack,
            max_locals: method_data.max_locals,
        };

        // Entry block
        let block = func_codegen.builder.create_block();
        func_codegen.blocks.insert(0, block);
        func_codegen
            .builder
            .append_block_params_for_function_params(block);
        func_codegen.builder.switch_to_block(block);
        func_codegen.builder.ensure_inserted_block();

        func_codegen.translate_block(0);
        func_codegen.builder.seal_all_blocks();
        func_codegen.builder.finalize();

        func_codegen
            .codegen
            .module
            .define_function(func_id, &mut func_codegen.context)
            .unwrap();

        func_id
    }

    /// Translate a single extended basic block
    fn translate_block(&mut self, start_offset: usize) {
        use Opcode::*;

        let (mut stack, mut vars) = self.enter_block();
        let opcode_offset = self.bytecode.get_opcode_index(start_offset).unwrap();
        let opcodes = &self.bytecode.opcodes[opcode_offset..];

        for (opcode_offset, opcode) in opcodes {
            match opcode {
                IconstM1 => stack.push(self.builder.ins().iconst(types::I32, -1)),
                Iconst0 => stack.push(self.builder.ins().iconst(types::I32, 0)),
                Iconst1 => stack.push(self.builder.ins().iconst(types::I32, 1)),
                Iconst2 => stack.push(self.builder.ins().iconst(types::I32, 2)),
                Iconst3 => stack.push(self.builder.ins().iconst(types::I32, 3)),
                Iconst4 => stack.push(self.builder.ins().iconst(types::I32, 4)),
                Iconst5 => stack.push(self.builder.ins().iconst(types::I32, 5)),
                Istore(i) => vars[*i] = stack.pop(),
                Astore(i) => vars[*i] = stack.pop(),
                Iadd => {
                    let x = stack.pop();
                    let y = stack.pop();
                    stack.push(self.builder.ins().iadd(x, y));
                }
                Invokespecial(MemberRef {
                    class_name: Cow::Borrowed("java/lang/Object"),
                    name_and_type:
                        NameAndType {
                            name: Cow::Borrowed("<init>"),
                            descriptor: Cow::Borrowed("()V"),
                        },
                }) => {}

                Invokevirtual(member_ref) => {
                    let func = self.codegen.functions[&member_ref];
                    let _local_func = self
                        .codegen
                        .module
                        .declare_func_in_func(func, &mut self.builder.func);
                    todo!()
                }

                Ldc(Loadable::LiteralConstant(LiteralConstant::Integer(i))) => {
                    stack.push(self.builder.ins().iconst(types::I32, *i as i64))
                }

                Return => {
                    self.builder.ins().return_(&[]);
                    return;
                }

                IfIcmpeq(succ_destaddr) => {
                    let x = stack.pop();
                    let y = stack.pop();
                    let succ_destaddr = *succ_destaddr as usize;
                    let fail_destaddr = self.bytecode.next_offset(*opcode_offset).unwrap();
                    let succ_block = self.block_at_offset(succ_destaddr);
                    let fail_block = self.block_at_offset(fail_destaddr);
                    let block_params = self.exit_block(&stack, &vars);

                    self.builder
                        .ins()
                        .br_icmp(IntCC::Equal, x, y, succ_block, &block_params);
                    self.builder.ins().jump(fail_block, &block_params);

                    self.jump(succ_destaddr);
                    self.jump(fail_destaddr);

                    return;
                }

                opcode => todo!("opcode not implemented: {opcode:?}"),
            }
        }
    }

    /// Retrieve the stack and the variables from clif block parameters
    fn enter_block(&mut self) -> (Stack, Variables) {
        let block = self.builder.current_block().unwrap();
        let params = self.builder.block_params(block);

        let (stack, vars) = params.split_at(self.max_stack as usize);
        (
            Stack::from_slice(self.max_stack, stack),
            Variables::from_slice(self.max_locals, vars),
        )
    }

    /// Returns the arguments to pass to a new block or similar
    fn exit_block(&mut self, stack: &Stack, vars: &Variables) -> Vec<Value> {
        let mut vec = Vec::from(stack.inner().clone());
        vec.extend_from_slice(vars.inner());
        vec
    }

    /// Jump to another block
    fn jump(&mut self, dest_offset: usize) {
        self.translate_block(dest_offset);
    }

    /// Get (or create) the block starting at opcode `offset`
    fn block_at_offset(&mut self, offset: usize) -> Block {
        *self
            .blocks
            .entry(offset)
            .or_insert_with(|| self.builder.create_block())
    }
}
