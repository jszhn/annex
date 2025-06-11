use std::collections::HashMap;

use crate::ast::{Ast, types::*};
use crate::sem::err::{SemError, Location};
use crate::sem::sym_table::{ScopedSymTable, SymTable, VarEntry, FuncEntry};

pub struct SemanticAnalyzer {
    errors: Vec<SemError>,
    global_functions: SymTable<FuncEntry>,
    scoped_variables: ScopedSymTable,
    current_function: Option<FunctionNode>,
    expression_types: HashMap<*const AstNode, Type>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            errors: Vec::new(),
            global_functions: SymTable::new(0),
            scoped_variables: ScopedSymTable::new(),
            current_function: None,
            expression_types: HashMap::new(),
        }
    }

    pub fn analyze(&mut self, ast: &Ast) -> Result<(), Vec<SemError>> {
        let _ = self.visit_program(ast.get_head_ref());
        
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn add_error(&mut self, error: SemError) {
        self.errors.push(error);
    }

    fn visit_program(&mut self, node: &AstNode) -> Result<(), SemError> {
        match node {
            AstNode::Block(program) => {
                // First pass: collect all function declarations
                for elem in &program.elems {
                    if let AstNode::Function(func) = elem {
                        self.visit_function_declaration(func)?;
                    }
                }

                // Second pass: analyze function bodies and global variables
                for elem in &program.elems {
                    match elem {
                        AstNode::Function(func) => {
                            self.visit_function_definition(func)?;
                        }
                        AstNode::VarDecl(var) => {
                            self.visit_variable_declaration(var)?;
                        }
                        AstNode::ArrDecl(arr) => {
                            self.visit_array_declaration(arr)?;
                        }
                        _ => {
                            self.add_error(SemError::internal_error(
                                Location::unknown(),
                                "Unexpected node type at program level".to_string(),
                            ));
                        }
                    }
                }
                Ok(())
            }
            _ => Err(SemError::internal_error(
                Location::unknown(),
                "Expected program block at root".to_string(),
            )),
        }
    }

    fn visit_function_declaration(&mut self, func: &FunctionNode) -> Result<(), SemError> {
        // Check 3: no two functions may be defined with the same name, return type, and signature
        if self.global_functions.contains(&func.name) {
            self.add_error(SemError::identifier_redefined(
                Location::unknown(),
                func.name.clone(),
            ));
            return Ok(());
        }

        let entry = FuncEntry::new(func.return_type.clone(), func.params.clone());
        self.global_functions.insert(func.name.clone(), entry);
        Ok(())
    }

    fn visit_function_definition(&mut self, func: &FunctionNode) -> Result<(), SemError> {
        self.current_function = Some(func.clone());
        self.scoped_variables.enter();

        // Add parameters to local scope
        for param in &func.params {
            if self.scoped_variables.contains_current(&param.name) {
                self.add_error(SemError::identifier_redefined(
                    Location::unknown(),
                    param.name.clone(),
                ));
            } else {
                let is_array = param.size.is_some();
                let var_entry = VarEntry::new(param.typ.clone(), is_array);
                self.scoped_variables.insert_current(param.name.clone(), var_entry);
            }
        }

        self.visit_node(&func.body)?;

        self.scoped_variables.exit();
        self.current_function = None;
        Ok(())
    }

    fn visit_variable_declaration(&mut self, var: &VarDeclNode) -> Result<(), SemError> {
        // Check 1: no two variables may have the same name in the same scope
        if self.scoped_variables.contains_current(&var.name) {
            self.add_error(SemError::identifier_redefined(
                Location::unknown(),
                var.name.clone(),
            ));
            return Ok(());
        }

        // Check type of initializer if present
        if let Some(init) = &var.initialiser {
            let init_type = self.visit_expression(init)?;
            // Check 6: type of LHS for assignment operator should be same as RHS
            if !self.types_compatible(&var.typ, &init_type) {
                self.add_error(SemError::invalid_assignment(
                    Location::unknown(),
                    Type::from_enum(&var.typ),
                    Type::from_enum(&init_type),
                ));
            }
        }

        let var_entry = VarEntry::new(var.typ.clone(), false);
        self.scoped_variables.insert_current(var.name.clone(), var_entry);
        Ok(())
    }

    fn visit_array_declaration(&mut self, arr: &ArrDeclNode) -> Result<(), SemError> {
        // Check 1: no two variables may have the same name in the same scope
        if self.scoped_variables.contains_current(&arr.name) {
            self.add_error(SemError::identifier_redefined(
                Location::unknown(),
                arr.name.clone(),
            ));
            return Ok(());
        }

        // Check 11: array sizes at compile time must be positive integers
        let size_type = self.visit_expression(&arr.size)?;
        if !self.is_integer_type(&size_type) {
            self.add_error(SemError::invalid_array_size(
                Location::unknown(),
                arr.name.clone(),
            ));
        }

        // Check if size is a positive literal
        if let AstNode::Literal(Literal::Int(size_val)) = &**&arr.size {
            if *size_val <= 0 {
                self.add_error(SemError::invalid_array_size(
                    Location::unknown(),
                    arr.name.clone(),
                ));
            }
        }

        // Check type of initializer if present
        if let Some(init) = &arr.initialiser {
            let init_type = self.visit_expression(init)?;
            if !self.types_compatible(&arr.typ, &init_type) {
                self.add_error(SemError::invalid_assignment(
                    Location::unknown(),
                    Type::from_enum(&arr.typ),
                    Type::from_enum(&init_type),
                ));
            }
        }

        let var_entry = VarEntry::new(arr.typ.clone(), true);
        self.scoped_variables.insert_current(arr.name.clone(), var_entry);
        Ok(())
    }

    fn visit_node(&mut self, node: &AstNode) -> Result<(), SemError> {
        match node {
            AstNode::Block(block) => self.visit_block(block),
            AstNode::If(if_node) => self.visit_if(if_node),
            AstNode::While(while_node) => self.visit_while(while_node),
            AstNode::For(for_node) => self.visit_for(for_node),
            AstNode::Return(return_node) => self.visit_return(return_node),
            AstNode::VarDecl(var) => self.visit_variable_declaration(var),
            AstNode::ArrDecl(arr) => self.visit_array_declaration(arr),
            AstNode::Binary(_binary) => {
                self.visit_expression(node)?;
                Ok(())
            }
            AstNode::Unary(_unary) => {
                self.visit_expression(node)?;
                Ok(())
            }
            AstNode::FunctionCall(_call) => {
                self.visit_expression(node)?;
                Ok(())
            }
            AstNode::Identifier(_) => {
                self.visit_expression(node)?;
                Ok(())
            }
            AstNode::Literal(_) => {
                self.visit_expression(node)?;
                Ok(())
            }
            AstNode::Arr(_arr_access) => {
                self.visit_expression(node)?;
                Ok(())
            }
            AstNode::Break | AstNode::Continue => Ok(()),
            _ => {
                self.add_error(SemError::internal_error(
                    Location::unknown(),
                    format!("Unhandled node type in visit_node: {:?}", node),
                ));
                Ok(())
            }
        }
    }

    fn visit_block(&mut self, block: &BlockNode) -> Result<(), SemError> {
        self.scoped_variables.enter();
        
        for elem in &block.elems {
            self.visit_node(elem)?;
        }
        
        self.scoped_variables.exit();
        Ok(())
    }

    fn visit_if(&mut self, if_node: &IfNode) -> Result<(), SemError> {
        // Check 7: condition of if should be a bool
        let cond_type = self.visit_expression(&if_node.condition)?;
        if !self.is_bool_type(&cond_type) {
            self.add_error(SemError::invalid_condition(
                Location::unknown(),
                Type::from_enum(&cond_type),
            ));
        }

        self.visit_node(&if_node.then_branch)?;

        for (elif_cond, elif_body) in &if_node.elif_branches {
            let elif_cond_type = self.visit_expression(elif_cond)?;
            if !self.is_bool_type(&elif_cond_type) {
                self.add_error(SemError::invalid_condition(
                    Location::unknown(),
                    Type::from_enum(&elif_cond_type),
                ));
            }
            self.visit_node(elif_body)?;
        }

        if let Some(else_branch) = &if_node.else_branch {
            self.visit_node(else_branch)?;
        }

        Ok(())
    }

    fn visit_while(&mut self, while_node: &WhileNode) -> Result<(), SemError> {
        // Check 7: condition of while should be a bool
        let cond_type = self.visit_expression(&while_node.condition)?;
        if !self.is_bool_type(&cond_type) {
            self.add_error(SemError::invalid_condition(
                Location::unknown(),
                Type::from_enum(&cond_type),
            ));
        }

        self.visit_node(&while_node.body)?;
        Ok(())
    }

    fn visit_for(&mut self, for_node: &ForNode) -> Result<(), SemError> {
        self.scoped_variables.enter();

        self.visit_node(&for_node.init)?;

        // Check 7: condition of for should be a bool
        let cond_type = self.visit_expression(&for_node.condition)?;
        if !self.is_bool_type(&cond_type) {
            self.add_error(SemError::invalid_condition(
                Location::unknown(),
                Type::from_enum(&cond_type),
            ));
        }

        self.visit_node(&for_node.update)?;
        self.visit_node(&for_node.body)?;

        self.scoped_variables.exit();
        Ok(())
    }

    fn visit_return(&mut self, return_node: &Option<Box<AstNode>>) -> Result<(), SemError> {
        if let Some(current_func) = self.current_function.clone() {
            match return_node {
                Some(expr) => {
                    let return_type = self.visit_expression(expr)?;
                    // Check 9: return statement must match the return type of the function
                    if !self.types_compatible(&current_func.return_type, &return_type) {
                        self.add_error(SemError::invalid_return_type(
                            Location::unknown(),
                            Type::from_enum(&current_func.return_type),
                            Type::from_enum(&return_type),
                        ));
                    }
                }
                None => {
                    // Check 9: void return in non-void function
                    if current_func.return_type != Type::Void {
                        self.add_error(SemError::invalid_return_type(
                            Location::unknown(),
                            Type::from_enum(&current_func.return_type),
                            "void".to_string(),
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    fn visit_expression(&mut self, node: &AstNode) -> Result<Type, SemError> {
        let result_type = match node {
            AstNode::Literal(lit) => self.visit_literal(lit),
            AstNode::Identifier(name) => self.visit_identifier(name),
            AstNode::Binary(binary) => self.visit_binary_expression(binary),
            AstNode::Unary(unary) => self.visit_unary_expression(unary),
            AstNode::FunctionCall(call) => self.visit_function_call(call),
            AstNode::Arr(arr_access) => self.visit_array_access(arr_access),
            _ => {
                self.add_error(SemError::internal_error(
                    Location::unknown(),
                    format!("Unexpected expression type: {:?}", node),
                ));
                Ok(Type::Void)
            }
        }?;

        // Store type information for this expression
        let node_ptr = node as *const AstNode;
        self.expression_types.insert(node_ptr, result_type.clone());

        Ok(result_type)
    }

    fn visit_literal(&mut self, lit: &Literal) -> Result<Type, SemError> {
        match lit {
            Literal::Int(_) => Ok(Type::I32), // Default integer type
            Literal::Float(_) => Ok(Type::F64), // Default float type
            Literal::Bool(_) => Ok(Type::Bool),
        }
    }

    fn visit_identifier(&mut self, name: &String) -> Result<Type, SemError> {
        // Check 2: a variable must not be used before it is first declared in a visible scope
        if let Some(var_entry) = self.scoped_variables.lookup(name) {
            Ok(var_entry.typ.clone())
        } else {
            self.add_error(SemError::identifier_undefined(
                Location::unknown(),
                name.clone(),
            ));
            Ok(Type::Void) // Error type
        }
    }

    fn visit_binary_expression(&mut self, binary: &BinaryNode) -> Result<Type, SemError> {
        let left_type = self.visit_expression(&binary.left)?;
        let right_type = self.visit_expression(&binary.right)?;

        match binary.op {
            // Arithmetic operations
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul | BinaryOperator::Div |
            BinaryOperator::BitAnd | BinaryOperator::BitOr | BinaryOperator::BitXor |
            BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight => {
                // Check 4: operands of arithmetic operators should make sense (no bool type!)
                if self.is_bool_type(&left_type) || self.is_bool_type(&right_type) {
                    self.add_error(SemError::invalid_arithmetic_operands(
                        Location::unknown(),
                        BinaryOperator::from_enum(&binary.op).to_string(),
                        if self.is_bool_type(&left_type) { 
                            Type::from_enum(&left_type) 
                        } else { 
                            Type::from_enum(&right_type) 
                        },
                    ));
                    return Ok(Type::Void);
                }

                // Check 5: operands must be of the same type
                if !self.types_compatible(&left_type, &right_type) {
                    self.add_error(SemError::invalid_logical_operands(
                        Location::unknown(),
                        BinaryOperator::from_enum(&binary.op).to_string(),
                        Type::from_enum(&left_type),
                        Type::from_enum(&right_type),
                    ));
                    return Ok(Type::Void);
                }

                Ok(left_type) // Result has the same type as operands
            }

            // Logical operations
            BinaryOperator::And | BinaryOperator::Or => {
                // Check 5: operands of logical operators must be of the same type
                if !self.types_compatible(&left_type, &right_type) {
                    self.add_error(SemError::invalid_logical_operands(
                        Location::unknown(),
                        BinaryOperator::from_enum(&binary.op).to_string(),
                        Type::from_enum(&left_type),
                        Type::from_enum(&right_type),
                    ));
                }

                // Logical operators should work with boolean types
                if !self.is_bool_type(&left_type) && !self.is_bool_type(&right_type) {
                    self.add_error(SemError::invalid_logical_operands(
                        Location::unknown(),
                        BinaryOperator::from_enum(&binary.op).to_string(),
                        Type::from_enum(&left_type),
                        Type::from_enum(&right_type),
                    ));
                }

                Ok(Type::Bool)
            }

            // Comparison operations
            BinaryOperator::Eq | BinaryOperator::NotEq | BinaryOperator::Greater |
            BinaryOperator::Less | BinaryOperator::GreaterEq | BinaryOperator::LessEq => {
                // Check 5: operands must be of the same type
                if !self.types_compatible(&left_type, &right_type) {
                    self.add_error(SemError::invalid_logical_operands(
                        Location::unknown(),
                        BinaryOperator::from_enum(&binary.op).to_string(),
                        Type::from_enum(&left_type),
                        Type::from_enum(&right_type),
                    ));
                }

                Ok(Type::Bool) // Comparison always returns bool
            }

            // Assignment
            BinaryOperator::Assign => {
                // Check 6: type of LHS for assignment operator should be same as RHS
                if !self.types_compatible(&left_type, &right_type) {
                    self.add_error(SemError::invalid_assignment(
                        Location::unknown(),
                        Type::from_enum(&left_type),
                        Type::from_enum(&right_type),
                    ));
                }

                Ok(left_type) // Assignment returns the type of the LHS
            }

            // Array access
            BinaryOperator::Array => {
                // This should be handled by visit_array_access, but handle here as fallback
                Ok(left_type)
            }
        }
    }

    fn visit_unary_expression(&mut self, unary: &UnaryNode) -> Result<Type, SemError> {
        let operand_type = self.visit_expression(&unary.expr)?;

        match unary.op {
            UnaryOperator::Neg => {
                // Check 4: arithmetic unary operators should not work with bool
                if self.is_bool_type(&operand_type) {
                    self.add_error(SemError::invalid_arithmetic_operands(
                        Location::unknown(),
                        UnaryOperator::from_enum(&unary.op).to_string(),
                        Type::from_enum(&operand_type),
                    ));
                    return Ok(Type::Void);
                }
                Ok(operand_type)
            }
            UnaryOperator::BitNot => {
                // Bitwise not should work with integers only
                if !self.is_integer_type(&operand_type) {
                    self.add_error(SemError::invalid_arithmetic_operands(
                        Location::unknown(),
                        UnaryOperator::from_enum(&unary.op).to_string(),
                        Type::from_enum(&operand_type),
                    ));
                    return Ok(Type::Void);
                }
                Ok(operand_type)
            }
            UnaryOperator::Not => {
                // Logical not should ideally work with bool, but allow other types
                Ok(Type::Bool)
            }
        }
    }

    fn visit_function_call(&mut self, call: &FunctionCallNode) -> Result<Type, SemError> {
        // Check 2: function must be declared before use
        if !self.global_functions.contains(&call.name) {
            self.add_error(SemError::identifier_undefined(
                Location::unknown(),
                call.name.clone(),
            ));
            return Ok(Type::Void);
        }

        // Clone the function entry to avoid borrowing issues
        let func_entry = self.global_functions.get_ref(&call.name).unwrap().clone();

        // Evaluate argument types
        let mut arg_types = Vec::new();
        for arg in &call.args {
            let arg_type = self.visit_expression(arg)?;
            arg_types.push(arg_type);
        }

        // Check 10: function call must have the same number/types of args as its definition
        if !func_entry.matches_signature(&arg_types) {
            let expected_types: Vec<String> = func_entry.params.iter()
                .map(|p| Type::from_enum(&p.typ))
                .collect();
            let actual_types: Vec<String> = arg_types.iter()
                .map(|t| Type::from_enum(t))
                .collect();
            
            self.add_error(SemError::invalid_function_call(
                Location::unknown(),
                call.name.clone(),
                format!("expected ({}) but got ({})", 
                    expected_types.join(", "), 
                    actual_types.join(", ")),
            ));
        }

        Ok(func_entry.return_type.clone())
    }

    fn visit_array_access(&mut self, arr_access: &ArrAccessNode) -> Result<Type, SemError> {
        // Check 2: array must be declared before use
        if let Some(var_entry) = self.scoped_variables.lookup(&arr_access.name).cloned() {
            // Check 12: scalars must not be used as arrays
            if !var_entry.is_array() {
                self.add_error(SemError::invalid_array_usage(
                    Location::unknown(),
                    arr_access.name.clone(),
                    "variable is not an array".to_string(),
                ));
                return Ok(Type::Void);
            }

            // Check that index is an integer type
            let index_type = self.visit_expression(&arr_access.access)?;
            if !self.is_integer_type(&index_type) {
                self.add_error(SemError::type_mismatch(
                    Location::unknown(),
                    "integer".to_string(),
                    Type::from_enum(&index_type),
                ));
            }

            // Return element type
            match &var_entry.typ {
                Type::Array(element_type, _) => Ok((**element_type).clone()),
                _ => Ok(var_entry.typ.clone()), // Fallback, though this shouldn't happen
            }
        } else {
            self.add_error(SemError::identifier_undefined(
                Location::unknown(),
                arr_access.name.clone(),
            ));
            Ok(Type::Void)
        }
    }

    // Helper methods
    fn types_compatible(&self, t1: &Type, t2: &Type) -> bool {
        match (t1, t2) {
            // Exact matches
            (a, b) if a == b => true,
            // Integer promotion rules (simplified)
            (Type::I8, Type::I16) | (Type::I8, Type::I32) | (Type::I8, Type::I64) => true,
            (Type::I16, Type::I32) | (Type::I16, Type::I64) => true,
            (Type::I32, Type::I64) => true,
            (Type::U8, Type::U16) | (Type::U8, Type::U32) | (Type::U8, Type::U64) => true,
            (Type::U16, Type::U32) | (Type::U16, Type::U64) => true,
            (Type::U32, Type::U64) => true,
            // Float promotion
            (Type::F32, Type::F64) => true,
            _ => false,
        }
    }

    fn is_integer_type(&self, typ: &Type) -> bool {
        matches!(typ, Type::I8 | Type::I16 | Type::I32 | Type::I64 |
                      Type::U8 | Type::U16 | Type::U32 | Type::U64)
    }

    fn is_float_type(&self, typ: &Type) -> bool {
        matches!(typ, Type::F32 | Type::F64)
    }

    fn is_bool_type(&self, typ: &Type) -> bool {
        matches!(typ, Type::Bool)
    }

    fn is_numeric_type(&self, typ: &Type) -> bool {
        self.is_integer_type(typ) || self.is_float_type(typ)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parse::Parser;

    fn analyze_code(source: &str) -> Result<(), Vec<SemError>> {
        let tokens = Lexer::new(source.to_string()).expect("Lexing failed");
        let parse_tree = Parser::new(tokens).expect("Parsing failed");
        let ast = Ast::new(parse_tree).expect("AST generation failed");
        
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&ast)
    }

    #[test]
    fn test_valid_program() {
        let source = "fn main{} void { var i32 x = 5; }";
        assert!(analyze_code(source).is_ok());
    }

    #[test]
    fn test_variable_redefinition() {
        let source = "fn main{} void { var i32 x = 5; var i32 x = 10; }";
        let result = analyze_code(source);
        assert!(result.is_err());
        
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0].kind, crate::sem::err::SemErrorKind::IdentifierRedefined));
    }

    #[test]
    fn test_undefined_variable() {
        let source = "fn main{} void { var i32 x = y; }";
        let result = analyze_code(source);
        assert!(result.is_err());
        
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0].kind, crate::sem::err::SemErrorKind::IdentifierUndefined));
    }

    #[test]
    fn test_function_redefinition() {
        let source = "fn test{} void {} fn test{} void {}";
        let result = analyze_code(source);
        assert!(result.is_err());
        
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0].kind, crate::sem::err::SemErrorKind::IdentifierRedefined));
    }

    #[test]
    fn test_type_mismatch_assignment() {
        let source = "fn main{} void { var i32 x = true; }";
        let result = analyze_code(source);
        assert!(result.is_err());
        
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0].kind, crate::sem::err::SemErrorKind::InvalidAssignment));
    }

    #[test]
    fn test_arithmetic_with_bool() {
        let source = "fn main{} void { var bool x = true + false; }";
        let result = analyze_code(source);
        assert!(result.is_err());
        
        let errors = result.unwrap_err();
        assert!(!errors.is_empty());
        assert!(errors.iter().any(|e| matches!(e.kind, crate::sem::err::SemErrorKind::InvalidArithmeticOperands)));
    }

    #[test]
    fn test_invalid_condition() {
        let source = "fn main{} void { if{5} { var i32 x = 1; } }";
        let result = analyze_code(source);
        assert!(result.is_err());
        
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0].kind, crate::sem::err::SemErrorKind::InvalidCondition));
    }

    #[test]
    fn test_return_type_mismatch() {
        let source = "fn test{} i32 { return true; }";
        let result = analyze_code(source);
        assert!(result.is_err());
        
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0].kind, crate::sem::err::SemErrorKind::InvalidReturnType));
    }

    #[test]
    fn test_invalid_function_call() {
        let source = "fn test{var i32 x} i32 { return x; } fn main{} void { test{true}; }";
        let result = analyze_code(source);
        assert!(result.is_err());
        
        let errors = result.unwrap_err();
        assert!(!errors.is_empty());
        assert!(errors.iter().any(|e| matches!(e.kind, crate::sem::err::SemErrorKind::InvalidFunctionCall)));
    }

    #[test]
    fn test_negative_array_size() {
        let source = "fn main{} void { var i32[-1] arr; }";
        let result = analyze_code(source);
        assert!(result.is_err());
        
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0].kind, crate::sem::err::SemErrorKind::InvalidArraySize));
    }

    #[test]
    fn test_scalar_used_as_array() {
        let source = "fn main{} void { var i32 x = 5; var i32 y = x[0]; }";
        let result = analyze_code(source);
        assert!(result.is_err());
        
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0].kind, crate::sem::err::SemErrorKind::InvalidArrayUsage));
    }

    #[test]
    fn test_valid_array_usage() {
        let source = "fn main{} void { var i32[10] arr = 0; var i32 x = arr[5]; }";
        assert!(analyze_code(source).is_ok());
    }

    #[test]
    fn test_complex_valid_program() {
        let source = r#"
            fn add{var i32 a, var i32 b} i32 {
                return a + b;
            }
            
            fn main{} void {
                var i32 x = 5;
                var i32 y = 10;
                var i32 result = add{x, y};
                
                if{result > 10} {
                    var i32[5] arr = 0;
                    arr[0] = result;
                }
            }
        "#;
        assert!(analyze_code(source).is_ok());
    }
}