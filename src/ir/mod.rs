use std::collections::HashMap;
use std::fmt::Error;

use log::info;

use crate::ast::{Ast, types};
use crate::ast::types::{
    AstNode, BinaryNode, BinaryOperator, FunctionNode, UnaryNode, UnaryOperator,
};

pub struct Inter {
    pub basic_blocks: Vec<BasicBlock>,
    pub functions: Vec<Function>,
}

impl Inter {
    pub fn new(tree: Ast) -> Result<Self, Error> {
        info!("Converting AST to intermediate representation");
        let result = convert_tree(tree)?;
        info!("IR conversion done");
        Ok(result)
    }
}

pub enum IR {
    Binary {
        op: IRBin,
        dest: Temp,
        left: Temp,
        right: Temp,
    },
    Unary {
        op: IRUnary,
        dest: Temp,
        operand: Temp,
    },
    Move {
        dest: Temp,
        src: Temp,
    },
    Load {
        dest: Temp,
        src: Temp,
        offset: i64,
    },
    LoadImm {
        dest: Temp,
        value: i64,
    },
    LoadFloat {
        dest: Temp,
        value: f64,
    },
    Store {
        dest: Temp,
        src: Temp,
        offset: i64,
    },
    Label(Label), // not sure if I actually need this
    Branch(Label),
    CondBranch {
        op: IRBin,
        reg1: Temp,
        reg2: Temp,
        label: Label, // assume we fall through on false condition
    },
    Call {
        func: Label,
        args: Vec<Temp>,
        ret: Option<Temp>,
    },
    Return(Option<Temp>),
}

enum Bool {
    Operation(IRBin),
    Constant(bool),
}

impl From<&BinaryOperator> for IRBin {
    fn from(op: &BinaryOperator) -> Self {
        match op {
            BinaryOperator::Add => Self::Add,
            BinaryOperator::And | BinaryOperator::BitAnd => Self::And,
            BinaryOperator::Sub => Self::Sub,
            BinaryOperator::Div => Self::Div,
            BinaryOperator::BitXor => Self::Xor,
            BinaryOperator::Mul => Self::Mul,
            BinaryOperator::Or | BinaryOperator::BitOr => Self::Or,
            BinaryOperator::ShiftLeft => Self::Sll,
            BinaryOperator::ShiftRight => Self::Srl,
            BinaryOperator::Eq => Self::Eq,
            BinaryOperator::NotEq => Self::Ne,
            BinaryOperator::Less => Self::Lt,
            BinaryOperator::LessEq => Self::Le,
            BinaryOperator::Greater => Self::Gt,
            BinaryOperator::GreaterEq => Self::Ge,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IRBin {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Xor,
    Sll, // shift left logical
    Srl, // shift right logical
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone)]
pub enum IRUnary {
    Neg, // not real instruction: resolve with MSB flip
    Not,
}

impl From<&UnaryOperator> for IRUnary {
    fn from(op: &UnaryOperator) -> IRUnary {
        match op {
            UnaryOperator::Neg => Self::Neg,
            UnaryOperator::Not | UnaryOperator::BitNot => Self::Not,
        }
    }
}

/// Register typedef. IR assumes an infinite register model so assigns a temporary identifier.
#[derive(Clone)]
pub struct Temp(pub usize);

pub struct Label(pub String);

pub struct BasicBlock {
    label: Label,
    instructions: Vec<IR>,
    pred: Vec<Label>,
    succ: Vec<Label>,
}

impl BasicBlock {
    fn new(label: Label) -> Self {
        BasicBlock {
            label,
            instructions: Vec::new(),
            pred: Vec::new(),
            succ: Vec::new(),
        }
    }

    /// Pushes an instruction into the vector.
    fn push(&mut self, instr: IR) {
        self.instructions.push(instr)
    }
}

pub struct Function {
    name: Label,
    params: Vec<Temp>,
    blocks: Vec<BasicBlock>,
}

fn convert_tree(tree: Ast) -> Result<Inter, Error> {
    let gen = IRGenerator::new();

    todo!()
}

struct IRGenerator {
    temp_counter: usize,
    label_counter: usize,
    variables: HashMap<String, Temp>,
    basic_blocks: Vec<BasicBlock>,
    functions: Vec<Function>,
}

impl IRGenerator {
    fn new() -> Self {
        Self {
            temp_counter: 0,
            label_counter: 0,
            variables: HashMap::new(),
            basic_blocks: Vec::new(),
            functions: Vec::new(),
        }
    }

    fn new_temp(&mut self) -> Temp {
        let temp = Temp(self.temp_counter);
        self.temp_counter += 1;
        temp
    }

    fn new_label(&mut self, prefix: &str) -> Label {
        let label = Label(format!("{}{}", prefix, self.label_counter));
        self.label_counter += 1;
        label
    }

    fn push_instr(&mut self, instr: IR) {
        if self.basic_blocks.is_empty() {
            let label = self.new_label("bb");
            let bb = BasicBlock::new(label);
            self.basic_blocks.push(bb);
        }
        self.basic_blocks.last_mut().unwrap().push(instr);
    }

    fn resolve_node(&mut self, node: &AstNode) -> Result<(), Error> {
        match node {
            AstNode::Function(node) => self.convert_function(node),
            _ => todo!(),
        }
    }

    fn resolve_expr(&mut self, node: &AstNode) -> Result<Temp, Error> {
        Ok(match node {
            AstNode::Binary(node) => {
                let op = IRBin::from(&node.op);
                let left = self.resolve_expr(&node.left)?;
                let right = self.resolve_expr(&node.right)?;

                let dest = self.new_temp();
                self.push_instr(IR::Binary {
                    op,
                    dest: dest.clone(),
                    left,
                    right,
                });
                dest
            }
            AstNode::Literal(node) => {
                let temp = self.new_temp();
                let instr: IR = match node {
                    types::Literal::Int(i) => IR::LoadImm {
                        dest: temp.clone(),
                        value: *i,
                    },
                    types::Literal::Float(f) => IR::LoadFloat {
                        dest: temp.clone(),
                        value: *f,
                    },
                    types::Literal::Bool(b) => IR::LoadImm {
                        dest: temp.clone(),
                        value: *b as i64,
                    },
                };
                self.push_instr(instr);
                temp
            }
            _ => todo!(),
        })
    }

    fn convert_function(&mut self, node: &FunctionNode) -> Result<(), Error> {
        todo!()
    }
}
