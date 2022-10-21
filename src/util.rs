use std::{
    borrow::Cow,
    collections::VecDeque,
    ops::{Index, IndexMut},
};

use cafebabe::{
    attributes::{AttributeData, AttributeInfo},
    bytecode::ByteCode,
    descriptor::{BaseType, FieldType, ReturnDescriptor, Ty},
    MethodInfo,
};
use cranelift_codegen::ir::{types, AbiParam, Value};

pub fn get_attribute<'a>(attrs: &[AttributeInfo<'a>], name: &'a str) -> Option<AttributeData<'a>> {
    attrs
        .iter()
        .find(|attr| attr.name == name)
        .map(|attr| attr.data.clone())
}

pub fn fieldtype_to_abiparam(field_type: &FieldType) -> AbiParam {
    match field_type {
        FieldType::Ty(ty) => match ty {
            Ty::Base(base_type) => match base_type {
                BaseType::Byte => AbiParam::new(types::B1),
                BaseType::Short => AbiParam::new(types::I16),
                BaseType::Int => AbiParam::new(types::I32),
                BaseType::Long => AbiParam::new(types::I64),
                BaseType::Char => AbiParam::new(types::I32),
                BaseType::Boolean => AbiParam::new(types::I8),
                BaseType::Float => AbiParam::new(types::F32),
                BaseType::Double => AbiParam::new(types::F64),
            },
            Ty::Object(_o) => AbiParam::new(types::R64),
        },
        FieldType::Array { .. } => AbiParam::new(types::R64),
    }
}

/// Various information about a method
pub struct MethodData<'a> {
    pub class: Cow<'a, str>,
    pub name: Cow<'a, str>,
    pub parameters: Vec<FieldType<'a>>,
    pub result: ReturnDescriptor<'a>,
    pub bytecode: ByteCode<'a>,

    pub max_locals: usize,
    pub max_stack: usize,
}
impl<'a> MethodData<'a> {
    /// Construct a `MethodData` using the `MethodInfo` extracted from the parser
    pub fn new(class: Cow<'a, str>, method_info: &'a MethodInfo) -> Self {
        let parameters = method_info.descriptor.parameters.clone();
        let result = method_info.descriptor.result.clone();
        let name = method_info.name.clone();
        let max_locals;
        let max_stack;

        let code_data =
            get_attribute(&method_info.attributes, "Code").expect("Method has no `code` attribute");
        let bytecode = if let AttributeData::Code(code) = code_data {
            max_locals = code.max_locals as usize;
            max_stack = code.max_stack as usize;
            code.bytecode.unwrap()
        } else {
            unreachable!();
        };

        MethodData {
            class,
            name,
            parameters,
            result,
            bytecode,
            max_locals,
            max_stack,
        }
    }
}

/// A representation of a java stack using cranelift values
#[derive(Clone)]
pub struct Stack {
    max_size: usize,
    inner: VecDeque<Value>,
}
impl Stack {
    pub fn from_slice(max_size: usize, values: &[Value]) -> Self {
        Stack {
            max_size,
            inner: values.to_vec().into(),
        }
    }

    pub fn push(&mut self, v: Value) {
        if self.inner.len() == self.max_size {
            self.inner.pop_back();
        }
        self.inner.push_front(v);
    }

    pub fn pop(&mut self) -> Value {
        self.inner.pop_front().unwrap()
    }

    pub fn inner(&self) -> &VecDeque<Value> {
        &self.inner
    }
}

/// A representation of java local variables using cranelift values
#[derive(Clone)]
pub struct Variables {
    inner: Vec<Value>,
}
impl Variables {
    pub fn new(max_size: usize) -> Self {
        Variables {
            inner: vec![Value::from_u32(u32::MAX - 1); max_size],
        }
    }

    pub fn from_slice(max_size: usize, values: &[Value]) -> Self {
        let mut vars = Variables::new(max_size);

        for (i, v) in values.iter().enumerate() {
            vars[i] = *v;
        }

        vars
    }

    pub fn inner(&self) -> &[Value] {
        &self.inner
    }
}
impl<T: Into<usize>> Index<T> for Variables {
    type Output = Value;

    fn index(&self, index: T) -> &Self::Output {
        &self.inner[index.into()]
    }
}
impl<T: Into<usize>> IndexMut<T> for Variables {
    fn index_mut(&mut self, index: T) -> &mut Self::Output {
        &mut self.inner[index.into()]
    }
}
