use crate::ast::oper::*;
use crate::ast::types::*;
use crate::sem::err::SemError;
use crate::sem::table::VarEntry;
use crate::sem::SemanticAnalyser;

/// todo: these methods should realistically be implemented in the context of a visitor trait
impl SemanticAnalyser {
    pub fn visit(&mut self, node: &AstNode) -> Result<Option<Type>, SemError> {
        match node {
            AstNode::Function(func) => self.visit_function(func),
            AstNode::FunctionCall(call) => self.visit_call(call),
            AstNode::Parameter(param) => {
                // parameters are not visited, they are handled in the function visit
                Ok(Some(param.typ.clone()))
            }
            AstNode::Binary(expr) => self.visit_binary(expr),
            AstNode::Unary(expr) => self.visit_unary(expr),
            AstNode::If(node) => self.visit_if(node),
            AstNode::While(node) => self.visit_while(node),
            AstNode::For(node) => self.visit_for(node),
            AstNode::Return(node) => self.visit_return(node),
            AstNode::VarDecl(decl) => self.visit_var_decl(decl),
            AstNode::ArrDecl(decl) => self.visit_arr_decl(decl),
            AstNode::Break | AstNode::Continue => {
                // break and continue do not return a type
                Ok(None)
            }
            AstNode::Block(block) => {
                // todo: realistically, the blocks we enter in the if/for/etc visitors should be here
                self.scoped_vars.enter();
                for stmt in &block.elems {
                    self.visit(stmt)?;
                }
                self.scoped_vars.exit();
                Ok(None)
            }
            AstNode::Identifier(id) => self.visit_identifier(id),
            AstNode::Literal(lit) => match lit {
                Literal::Int(_) => Ok(Some(Type::I64)),
                Literal::Float(_) => Ok(Some(Type::F64)),
                Literal::Bool(_) => Ok(Some(Type::Bool)),
            },
            AstNode::Arr(_) => {
                // Array nodes are handled in the visit_arr_decl method
                Ok(Some(Type::Void)) // or whatever is appropriate
            }
        }
    }

    fn visit_function(&mut self, node: &FunctionNode) -> Result<Option<Type>, SemError> {
        self.curr_func = Some(node.clone());
        self.scoped_vars.enter();

        for param in &node.params {
            if self.scoped_vars.lookup_current(&param.name).is_some() {
                self.add_err(SemError::id_redefined(param.name.clone()));
            } else {
                let is_array = param.size.is_some();
                let entry = VarEntry::new(param.typ.clone(), is_array);
                self.scoped_vars.insert(param.name.clone(), entry);
            }
        }

        self.visit(&node.body)?;

        self.scoped_vars.exit();
        self.curr_func = None;

        Ok(None)
    }

    fn visit_call(&mut self, call: &FunctionCallNode) -> Result<Option<Type>, SemError> {
        // function should be declared before use
        if !self.functions.contains(&call.name) {
            self.add_err(SemError::id_undefined(call.name.clone()));
            return Ok(Some(Type::Void));
        }

        // Clone necessary data from func_entry to avoid borrowing conflicts
        let (params, return_typ) = {
            let func_entry = self.functions.lookup(&call.name).unwrap();
            (func_entry.params.clone(), func_entry.return_typ.clone())
        };

        let mut arg_types = Vec::new();
        for arg in &call.args {
            let typ = self.visit(arg)?;
            arg_types.push(typ);
        }

        // Create a temporary FuncEntry to use compare_signature
        let temp_func_entry = crate::sem::table::FuncEntry::new(params, return_typ.clone());

        if !temp_func_entry.compare_signature(&arg_types) {
            let expected_types: Vec<String> = temp_func_entry
                .params
                .iter()
                .map(|param| param.typ.to_string())
                .collect::<Vec<_>>();

            let actual_types: Vec<String> = arg_types
                .iter()
                .map(|typ| typ.as_ref().map_or("void".to_string(), |t| t.to_string()))
                .collect::<Vec<_>>();

            self.add_err(SemError::invalid_function_call(
                call.name.clone(),
                format!(
                    "expected ({}) but got ({})",
                    expected_types.join(", "),
                    actual_types.join(", ")
                ),
            ));
        }

        Ok(Some(return_typ))
    }

    fn visit_binary(&mut self, node: &BinaryNode) -> Result<Option<Type>, SemError> {
        let left_typ = self.visit(&node.left)?.unwrap_or_default();
        let right_typ = self.visit(&node.right)?.unwrap_or_default();

        if left_typ == Type::Void || right_typ == Type::Void {
            self.add_err(SemError::invalid_operands_typ());
            return Ok(None);
        }

        match node.op {
            // numeric and bitwise operators
            BinaryOperator::Add
            | BinaryOperator::Sub
            | BinaryOperator::Mul
            | BinaryOperator::Div
            | BinaryOperator::BitAnd
            | BinaryOperator::BitOr
            | BinaryOperator::BitXor => {
                if !left_typ.is_numeric() || !right_typ.is_numeric() {
                    self.add_err(SemError::invalid_operands(
                        &node.op,
                        left_typ,
                        Some(right_typ),
                    ));

                    return Ok(Default::default());
                }
            }
            // bit shift operators. right operand must be an integer
            BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight => {
                if !left_typ.is_numeric() || !right_typ.is_int() {
                    self.add_err(SemError::invalid_operands(
                        &node.op,
                        left_typ,
                        Some(right_typ),
                    ));

                    return Ok(Default::default());
                }
            }
            // comparison operators
            BinaryOperator::Eq
            | BinaryOperator::NotEq
            | BinaryOperator::Greater
            | BinaryOperator::Less
            | BinaryOperator::GreaterEq
            | BinaryOperator::LessEq => {
                if !self.types_compare(&left_typ, &right_typ) {
                    self.add_err(SemError::invalid_operands(
                        &node.op,
                        left_typ.clone(),
                        Some(right_typ),
                    ));
                }
            }
            // logical operators, both operands must be boolean
            BinaryOperator::And | BinaryOperator::Or => {
                if !self.types_compare(&left_typ, &right_typ)
                    || (!left_typ.is_bool() && !right_typ.is_bool())
                {
                    self.add_err(SemError::invalid_operands(
                        &node.op,
                        left_typ.clone(),
                        Some(right_typ),
                    ));
                }
            }
            BinaryOperator::Assign => {
                if !self.types_compare(&left_typ, &right_typ) {
                    self.add_err(SemError::invalid_assignment(left_typ.clone(), right_typ));
                }
            }
            BinaryOperator::Array => {
                // todo: why is this here? we have an ArrAccessNode
                if !right_typ.is_int() {
                    self.add_err(SemError::type_mismatch(right_typ, Type::I64));
                }
            }
        }

        Ok(Some(left_typ.clone()))
    }

    fn visit_unary(&mut self, node: &UnaryNode) -> Result<Option<Type>, SemError> {
        let expr_typ = self.visit(&node.expr)?.unwrap_or_default();

        match node.op {
            UnaryOperator::Neg => {
                if !expr_typ.is_numeric() {
                    self.add_err(SemError::invalid_operands(&node.op, expr_typ.clone(), None));
                }
            }
            UnaryOperator::Not => {
                if !expr_typ.is_bool() {
                    self.add_err(SemError::invalid_operands(&node.op, expr_typ.clone(), None));
                }
            }
            UnaryOperator::BitNot => {
                if !expr_typ.is_numeric() {
                    self.add_err(SemError::invalid_operands(&node.op, expr_typ.clone(), None));
                }
            }
        }

        Ok(Some(expr_typ))
    }

    fn visit_if(&mut self, node: &IfNode) -> Result<Option<Type>, SemError> {
        let cond_typ = self.visit(&node.condition)?.unwrap_or_default();
        if !cond_typ.is_bool() {
            self.add_err(SemError::invalid_condition(cond_typ));
        }

        self.scoped_vars.enter();
        self.visit(&node.then_branch)?;
        self.scoped_vars.exit();

        for (elif_cond, elif_body) in &node.elif_branches {
            let elif_cond_typ = self.visit(elif_cond)?.unwrap_or_default();
            if !elif_cond_typ.is_bool() {
                self.add_err(SemError::invalid_condition(elif_cond_typ));
            }

            self.scoped_vars.enter();
            self.visit(elif_body)?;
            self.scoped_vars.exit();
        }

        if let Some(else_branch) = &node.else_branch {
            self.scoped_vars.enter();
            self.visit(else_branch)?;
            self.scoped_vars.exit();
        }

        Ok(None)
    }

    fn visit_while(&mut self, node: &WhileNode) -> Result<Option<Type>, SemError> {
        self.scoped_vars.enter();

        let cond_typ = self.visit(&node.condition)?.unwrap_or_default();
        if !cond_typ.is_bool() {
            self.add_err(SemError::invalid_condition(cond_typ));
        }
        self.visit(&node.body)?;

        self.scoped_vars.exit();
        Ok(None)
    }

    fn visit_for(&mut self, node: &ForNode) -> Result<Option<Type>, SemError> {
        self.scoped_vars.enter();
        self.visit(&node.init)?;

        let cond_type = self.visit(&node.condition)?.unwrap_or_default();
        if !cond_type.is_bool() {
            self.add_err(SemError::invalid_condition(cond_type));
        }

        self.visit(&node.update)?;
        self.visit(&node.body)?;
        self.scoped_vars.exit();
        Ok(None)
    }

    fn visit_return(&mut self, node: &Option<Box<AstNode>>) -> Result<Option<Type>, SemError> {
        if let Some(func) = self.curr_func.clone() {
            match node {
                Some(expr) => {
                    let return_typ = self.visit(expr)?.unwrap_or_default();

                    // return type should match function definition
                    if !self.types_compare(&return_typ, &func.return_type) {
                        self.add_err(SemError::invalid_return_type(func.return_type, return_typ));
                    }
                }
                None => {
                    // void return in non-void function
                    if func.return_type != Type::Void {
                        self.add_err(SemError::invalid_return_type(func.return_type, Type::Void));
                    }
                }
            }
        }
        Ok(None)
    }

    fn visit_var_decl(&mut self, node: &VarDeclNode) -> Result<Option<Type>, SemError> {
        // no two variables in the same scope can have the same name
        if self.scoped_vars.lookup_current(&node.name).is_some() {
            self.add_err(SemError::id_redefined(node.name.clone()));
            return Ok(None);
        }

        // check initialiser type, if it exists
        if let Some(init) = &node.initialiser {
            // check if the type of the initializer matches the variable type
            let typ = self.visit(init);
            match typ {
                Ok(Some(init_typ)) if !self.types_compare(&node.typ, &init_typ) => {
                    // LHS type should match RHS type
                    self.add_err(SemError::type_mismatch(node.typ.clone(), init_typ));
                }
                Ok(None) => {
                    self.add_err(SemError::internal_error(
                        "could not find type for expression",
                    ));
                    return Ok(None);
                }
                Err(e) => return Err(e),
                _ => {}
            }
        }

        let var_entry = VarEntry::new(node.typ.clone(), false);
        self.scoped_vars.insert(node.name.clone(), var_entry);

        Ok(None)
    }

    fn visit_arr_decl(&mut self, arr: &ArrDeclNode) -> Result<Option<Type>, SemError> {
        // no two variables in the same scope can have the same name
        if self.scoped_vars.lookup_current(&arr.name).is_some() {
            self.add_err(SemError::id_redefined(arr.name.clone()));
            return Ok(None);
        }

        // check that array size is an integer
        let size_type = self.visit(&arr.size)?;
        if let Some(size_type) = size_type {
            if !size_type.is_int() {
                self.add_err(SemError::invalid_array_size(
                    arr.name.clone(),
                    "array size must be an integer",
                ));
            }
        } else {
            self.add_err(SemError::internal_error(
                "could not find type for array size expression",
            ));
            return Ok(None);
        }

        // check that array size is a positive literal
        if let AstNode::Literal(Literal::Int(size_val)) = *arr.size {
            if size_val == 0 {
                self.add_err(SemError::invalid_array_size(
                    arr.name.clone(),
                    "array size must be a positive integer",
                ));
            }
        }

        // check initialiser type, if it exists
        if let Some(init) = &arr.initialiser {
            let typ = self.visit(init);
            match typ {
                Ok(Some(init_typ)) if !self.types_compare(&arr.typ, &init_typ) => {
                    // LHS type should match RHS type
                    self.add_err(SemError::type_mismatch(arr.typ.clone(), init_typ));
                }
                Ok(None) => {
                    self.add_err(SemError::internal_error(
                        "could not find type for expression",
                    ));
                    return Ok(None);
                }
                Err(e) => return Err(e),
                _ => {}
            }
        }

        let var_entry = VarEntry::new(arr.typ.clone(), true);
        self.scoped_vars.insert(arr.name.clone(), var_entry);

        Ok(None)
    }

    fn visit_identifier(&mut self, id: &String) -> Result<Option<Type>, SemError> {
        // variables should be defined before use
        if let Some(var_entry) = self.scoped_vars.lookup(id) {
            Ok(Some(var_entry.typ.clone()))
        } else {
            self.add_err(SemError::id_undefined(id.clone()));
            Ok(Some(Type::Void))
        }
    }

    fn types_compare(&self, left: &Type, right: &Type) -> bool {
        match (left, right) {
            (a, b) if a == b => true,
            // simplified integer promotion rules
            (Type::I8, Type::I16) | (Type::I8, Type::I32) | (Type::I8, Type::I64) => true,
            (Type::I16, Type::I32) | (Type::I16, Type::I64) => true,
            (Type::I32, Type::I64) => true,
            (Type::U8, Type::U16) | (Type::U8, Type::U32) | (Type::U8, Type::U64) => true,
            (Type::U16, Type::U32) | (Type::U16, Type::U64) => true,
            (Type::U32, Type::U64) => true,
            // float promotion
            (Type::F32, Type::F64) => true,
            _ => false,
        }
    }
}
