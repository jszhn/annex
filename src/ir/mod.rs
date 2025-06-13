use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt::Error;

use log::info;

use crate::ast::types::{
    ArrDeclNode, AstNode, BinaryOperator, BlockNode, ForNode, FunctionCallNode, FunctionNode,
    IfNode, UnaryOperator, VarDeclNode, WhileNode,
};
use crate::ast::{types, Ast};

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
        offset: Temp,
    },
    LoadImm {
        dest: Temp,
        value: u64,
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
        true_label: Label, // assume we fall through on false condition
        false_label: Label,
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

#[derive(Debug, PartialEq, Clone)]
pub enum IRBin {
    Assign, // =
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Xor,
    Sll, // shift left logical
    Srl, // shift right logical
    Eq,  // ==
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl From<&BinaryOperator> for IRBin {
    fn from(op: &BinaryOperator) -> Self {
        match op {
            BinaryOperator::Assign => Self::Assign,
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

#[derive(Clone)]
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
    variables: Vec<HashMap<String, Temp>>,
    basic_blocks: Vec<BasicBlock>,
    functions: HashMap<Label, Function>,
}

impl IRGenerator {
    fn new() -> Self {
        Self {
            temp_counter: 0,
            label_counter: 0,
            variables: Vec::new(),
            basic_blocks: Vec::new(),
            functions: HashMap::new(),
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

    fn new_block(&mut self, label: Label) {
        self.push_instr(IR::Label(label.clone()));
        self.basic_blocks.push(BasicBlock {
            label,
            instructions: Vec::new(),
            pred: Vec::new(),
            succ: Vec::new(),
        })
    }

    /// IR generation-specific enter scope function.
    fn enter(&mut self) {
        self.variables.push(HashMap::new());
    }

    fn exit(&mut self) {
        if self.variables.len() > 1 {
            self.variables.pop();
        }
    }

    /// Searches the scoped hash tables for the specified key.
    fn search_var(&mut self, key: &String) -> Option<Temp> {
        for scope in self.variables.iter().rev() {
            if let Some(val) = scope.get(key) {
                return Some(val.clone());
            }
        }
        None
    }

    /// Adds a new variable to the current scope.
    /// Primary use should be in scalar/array declarations.
    fn new_var(&mut self, key: String, val: Temp) {
        let curr = self.variables.last_mut();
        match curr {
            Some(table) => {
                _ = table.insert(key, val);
            }
            None => {
                self.enter();
                self.new_var(key, val);
            }
        }
    }

    fn update_var(&mut self, key: String, val: Temp) {
        for scope in self.variables.iter_mut().rev() {
            if let Some(val) = scope.get(&key) {
                _ = scope.insert(key, val.clone()); // discard previous temp register ID
                break;
            }
        }
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
            AstNode::Parameter(_) => unreachable!(), // parameter nodes should only be in function handler
            AstNode::Function(node) => self.convert_function(node)?,
            AstNode::VarDecl(node) => self.convert_scalar_decl(node)?,
            AstNode::ArrDecl(node) => self.convert_arr_decl(node)?,
            AstNode::Return(node) => {
                let temp = match node {
                    Some(expr) => Some(self.resolve_expr(expr)?),
                    None => None,
                };
                self.push_instr(IR::Return(temp))
            }
            AstNode::While(node) => self.convert_while(node)?,
            AstNode::For(node) => self.convert_for(node)?,
            AstNode::If(node) => self.convert_if(node)?,
            AstNode::Binary(_)
            | AstNode::Unary(_)
            | AstNode::Literal(_)
            | AstNode::Identifier(_)
            | AstNode::Arr(_) => _ = self.resolve_expr(node)?,
            AstNode::FunctionCall(node) => _ = self.convert_call(node)?,
            AstNode::Break | AstNode::Continue => self.convert_loop_control(node)?,
            AstNode::Block(node) => self.convert_block(node)?,
        }
        Ok(())
    }

    /// Resolves AST expression nodes. We assume that all expressions return a value,
    ///     hence the `Temp` return type.
    fn resolve_expr(&mut self, node: &AstNode) -> Result<Temp, Error> {
        Ok(match node {
            AstNode::Arr(node) => {
                // array access
                let ptr = self.search_var(&node.name).unwrap();
                let offset = self.resolve_expr(&node.access)?;
                let temp = self.new_temp();
                self.push_instr(IR::Load {
                    dest: temp.clone(),
                    src: ptr,
                    offset,
                });
                temp
            }
            AstNode::Identifier(label) => self.search_var(label).unwrap(),
            AstNode::FunctionCall(node) => self.convert_call(node)?.unwrap(), // this is really sloppy but the semantic analysis should've ensured we're not calling a void function (no return) in the middle of an expression (returns, typed)
            AstNode::Binary(node) => {
                let op = IRBin::from(&node.op);
                let left = self.resolve_expr(&node.left)?;
                let right = self.resolve_expr(&node.right)?;

                let dest = self.new_temp();
                self.push_instr(IR::Binary {
                    op: op.clone(),
                    dest: dest.clone(),
                    left,
                    right,
                });
                if op == IRBin::Assign {
                    // if assignment, we should update variable table
                    if let AstNode::Identifier(str) = *node.left.clone() {
                        self.update_var(str, dest.clone());
                    }
                }
                dest
            }
            AstNode::Unary(node) => {
                let op = IRUnary::from(&node.op);
                let operand = self.resolve_expr(&node.expr)?;

                let dest = self.new_temp();
                match op {
                    IRUnary::Not => {
                        self.push_instr(IR::Unary {
                            op,
                            dest: dest.clone(),
                            operand,
                        });
                    }
                    IRUnary::Neg => {
                        // flip MSB. this works for signed ints and IEEE floats
                        let imm = self.new_temp();
                        self.push_instr(IR::LoadImm {
                            dest: imm.clone(),
                            value: 0b10000000000000000000000000000000,
                        });
                        self.push_instr(IR::Binary {
                            op: IRBin::Xor,
                            dest: dest.clone(),
                            left: imm,
                            right: operand,
                        });
                    }
                }
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
                        value: *b as u64,
                    },
                };
                self.push_instr(instr);
                temp
            }
            _ => unreachable!(),
        })
    }

    fn convert_function(&mut self, node: &FunctionNode) -> Result<(), Error> {
        self.enter();
        let label = Label(node.name.clone());
        let _: Vec<Temp> = node
            .params
            .iter()
            .map(|param| {
                let temp = self.new_temp();
                self.new_var(param.name.clone(), temp.clone());
                temp
            })
            .collect();

        self.new_block(label);
        self.resolve_node(&node.body)?;
        self.exit();
        Ok(())
    }

    fn convert_scalar_decl(&mut self, node: &VarDeclNode) -> Result<(), Error> {
        let init = if let Some(initialiser) = &node.initialiser {
            Some(self.resolve_expr(initialiser)?)
        } else {
            None
        };
        let key = node.name.clone(); // add name to hash table
        let value = match init {
            Some(temp) => temp,
            None => {
                let reg = self.new_temp();
                self.push_instr(IR::LoadImm {
                    dest: reg.clone(),
                    value: 0,
                });
                reg
            }
        };
        self.new_var(key, value);
        Ok(())
    }

    fn convert_arr_decl(&mut self, node: &ArrDeclNode) -> Result<(), Error> {
        // todo: figure this out
        // probably going to store memory addresses with array declarations
        // then do an explicit load or store instruction
        Ok(())
    }

    fn convert_while(&mut self, node: &WhileNode) -> Result<(), Error> {
        let cond_label = self.new_label("while_cond");
        let body_label = self.new_label("while_body");
        let end_label = self.new_label("while_end");

        self.new_block(cond_label);
        let cond_temp = self.resolve_expr(&node.condition)?;
        let zero = self.new_temp();
        self.push_instr(IR::LoadImm {
            dest: zero.clone(),
            value: 0,
        });
        self.push_instr(IR::CondBranch {
            op: IRBin::Ne,
            reg1: cond_temp.clone(),
            reg2: zero.clone(),
            true_label: body_label.clone(),
            false_label: end_label.clone(),
        });

        self.new_block(body_label);
        self.resolve_expr(&node.body)?;

        self.new_block(end_label);
        Ok(())
    }

    fn convert_for(&mut self, node: &ForNode) -> Result<(), Error> {
        let init_label = self.new_label("for_init");
        let body_label = self.new_label("for_body");
        let cond_label = self.new_label("for_cond");
        let update_label = self.new_label("for_update");
        let end_label = self.new_label("for_end");

        self.new_block(init_label.clone());
        self.resolve_node(&node.init)?;

        self.new_block(cond_label.clone());
        let cond_temp = self.resolve_expr(&node.condition)?;
        let zero = self.new_temp();
        self.push_instr(IR::LoadImm {
            dest: zero.clone(),
            value: 0,
        });
        self.push_instr(IR::CondBranch {
            op: IRBin::Ne,
            reg1: cond_temp.clone(),
            reg2: zero.clone(),
            true_label: body_label.clone(),
            false_label: end_label.clone(),
        });

        self.new_block(update_label);
        self.resolve_node(&node.body)?;
        // take unconditional branch
        self.push_instr(IR::Branch(cond_label));

        self.new_block(end_label);
        Ok(())
    }

    fn convert_if(&mut self, node: &IfNode) -> Result<(), Error> {
        let zero = self.new_temp();
        self.push_instr(IR::LoadImm {
            dest: zero.clone(),
            value: 0,
        });
        let end_label = self.new_label("if_end");

        // first if condition
        let then_label = self.new_label("if_then");
        let mut next_label = if !node.elif_branches.is_empty() {
            self.new_label("elif")
        } else if node.else_branch.is_some() {
            self.new_label("else")
        } else {
            end_label.clone()
        };
        let if_cond = self.resolve_expr(&node.condition)?;
        self.push_instr(IR::CondBranch {
            op: IRBin::Eq,
            reg1: zero.clone(),
            reg2: if_cond,
            true_label: then_label.clone(),
            false_label: next_label.clone(),
        });

        // first then
        self.new_block(then_label.clone());
        self.resolve_node(&node.then_branch)?;

        for (elif_cond_node, elif_body_node) in &node.elif_branches {
            self.new_block(next_label.clone());
            next_label = if let Some(_) = node
                .elif_branches
                .iter()
                .skip_while(|(c, _)| c != elif_cond_node)
                .nth(1)
            {
                self.new_label("elif")
            } else if node.else_branch.is_some() {
                self.new_label("else")
            } else {
                end_label.clone()
            };

            let elif_cond = self.resolve_expr(elif_cond_node)?;
            self.push_instr(IR::CondBranch {
                op: IRBin::Eq,
                reg1: zero.clone(),
                reg2: elif_cond,
                true_label: then_label.clone(),
                false_label: next_label.clone(),
            });

            self.new_block(Label("elif_then".to_string()));
            self.resolve_node(elif_body_node)?;
        }

        // else branch
        if let Some(el) = &node.else_branch {
            self.new_block(next_label.clone()); // this value should be else
            self.resolve_node(el)?;
        }

        self.new_block(end_label);
        Ok(())
    }

    fn convert_call(&mut self, node: &FunctionCallNode) -> Result<Option<Temp>, Error> {
        if node.return_type.is_none() {
            // by this point function calls should have type parameter filled out by semantic analysis pass
            return Err(Error);
        }

        let label = Label(node.name.clone());
        let temp = match node.return_type {
            Some(types::Type::Void) => None,
            Some(_) => Some(self.new_temp()),
            _ => unreachable!(), // will this panic lmao
        };
        self.push_instr(IR::Call {
            func: label,
            args: Vec::new(), // todo: figure out how to represent function parameters
            ret: temp,
        });

        Ok(None)
    }

    fn convert_loop_control(&mut self, node: &AstNode) -> Result<(), Error> {
        let jump_label = match node {
            // todo: finish
            AstNode::Break => Label("todo".to_string()),
            AstNode::Continue => Label("todo".to_string()),
            _ => unreachable!(),
        };
        self.push_instr(IR::Branch(jump_label));
        Ok(())
    }

    /// This function assumes that a new basic block has already been put in place
    /// by other functions (`if`, `while`, `for`, etc). It does resolve its own scopes.
    fn convert_block(&mut self, node: &BlockNode) -> Result<(), Error> {
        self.enter();
        for n in &node.elems {
            self.resolve_node(n)?;
        }
        self.exit();
        Ok(())
    }
}
