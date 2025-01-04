//
//  Reference EBNF grammar specification for the
//          Annex programming language
//

grammar annex;

program: (function_decl | scalar_decl | array_decl)*;

function_decl: 'fn' ID '{' param_list '}' return_type scope;
param_list: (param (',' param)*)? ;
param: type ID;

scalar_decl: ('const' | 'var' | 'vol') type ID ';';
array_decl: ('const' | 'var' | 'vol') type '[' int_const ']' ID ';';

type: var_type | array_type;
array_type: var_type '[' expr ']';
var_type: 'i8' | 'i16' | 'i32' | 'i64' | 'u8' | 'u16' | 'u32' | 'u64' | 'bool' | 'f32' | 'f64';
return_type: type | 'void';

constant: bool_const | int_const | float_const;

scope: '{' (decl | stmt)* '}';

stmt: expr ';'
    | if_stmt
    | while_stmt
    | for_stmt
    | return_stmt
    | scope
    | break_stmt
    | continue_stmt;

decl: scalar_decl | array_decl;

if_stmt: 'if' '{' expr '}' stmt ('elif' '{' expr '}' stmt)* ('else' stmt)?;
while_stmt: 'while' '{' expr '}' stmt;
for_stmt: 'for' '{' expr ';' expr ';' expr '}' stmt;
return_stmt: 'return' ';' | 'return' expr ';';
break_stmt: 'break' ';';
continue_stmt: 'continue' ';';

expr: constant
    | ID
    | array_access
    | expr binary_op expr
    | unary_op expr
    | '{' expr '}'
    | '(' expr ')';

array_access: ID '[' expr ']';

binary_op: '+' | '-' | '*' | '/' | '>' | '<' | '>>' | '<<' | 'and' | 'or' | '=' | '|' | '&' | '^';
unary_op: '-' | '~' | '?';

bool_const: BOOL;
int_const: INT;
float_const: FLOAT;

// token definitions
BOOL: 'true' | 'false';
INT: ([0] | [1-9][0-9]*);
FLOAT: ([0] | [1-9][0-9]*) '.' ([0] | [1-9][0-9]*);
ID: [a-zA-Z_][a-zA-Z0-9_]*;
WS: ([\r\n] | [ ] | [\t])+ -> skip;
COMMENT: '//' (~[\r\n])* -> skip;