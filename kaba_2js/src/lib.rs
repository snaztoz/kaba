use kaba_compiler::{AstNode, AstNodeVariant, Literal, Result, SymbolTable};

#[cfg(target_arch = "wasm32")]
pub mod wasm;

pub fn compile(src: &str) -> Result<String> {
    let src = kaba_compiler::normalize_newlines(src);
    let (ast, sym_table) = kaba_compiler::compile(&src)?;

    let mut buff = String::new();
    if let AstNodeVariant::Program { body, .. } = &ast.variant {
        compile_body(body, &sym_table, &mut buff);
        buff.push_str("main();");
    }

    Ok(buff)
}

fn compile_body(body: &[AstNode], sym_table: &SymbolTable, buff: &mut String) {
    for stmt in body {
        compile_statement(stmt, sym_table, buff);
    }
}

fn compile_statement(stmt: &AstNode, sym_table: &SymbolTable, buff: &mut String) {
    match &stmt.variant {
        AstNodeVariant::FunctionDefinition { .. } => {
            compile_function_definition(stmt, sym_table, buff)
        }
        AstNodeVariant::VariableDeclaration { .. } => {
            compile_variable_declaration(stmt, sym_table, buff)
        }
        AstNodeVariant::If { .. } => compile_conditional_branches(stmt, sym_table, buff),
        AstNodeVariant::While { .. } => compile_while_loop(stmt, sym_table, buff),
        AstNodeVariant::Each { .. } => compile_each_loop(stmt, sym_table, buff),
        AstNodeVariant::Continue => {
            buff.push_str("continue;");
        }
        AstNodeVariant::Break => {
            buff.push_str("break;");
        }
        AstNodeVariant::Return { expr, .. } => {
            buff.push_str("return");
            if let Some(expr) = expr {
                buff.push(' ');
                compile_expression(expr, sym_table, buff);
            }
            buff.push(';');
        }
        AstNodeVariant::Debug { expr, .. } => {
            buff.push_str("$print(");
            compile_expression(expr, sym_table, buff);
            buff.push_str(");");
        }
        _ => {
            compile_expression(stmt, sym_table, buff);
            buff.push(';');
        }
    }
}

fn compile_function_definition(stmt: &AstNode, sym_table: &SymbolTable, buff: &mut String) {
    if let AstNodeVariant::FunctionDefinition {
        sym, params, body, ..
    } = &stmt.variant
    {
        let entry = &sym_table[&sym.id];
        buff.push_str(&format!("function {}(", entry.name));

        let params_str = params
            .iter()
            .map(|p| String::from(&sym_table[&p.sym.id].name))
            .collect::<Vec<_>>()
            .join(",");
        buff.push_str(&format!("{params_str}){{"));

        compile_body(body, sym_table, buff);
        buff.push('}');
    }
}

fn compile_variable_declaration(stmt: &AstNode, sym_table: &SymbolTable, buff: &mut String) {
    if let AstNodeVariant::VariableDeclaration { sym, val, .. } = &stmt.variant {
        let entry = &sym_table[&sym.id];

        buff.push_str(&format!("let {}=", entry.name));
        compile_expression(val, sym_table, buff);

        buff.push(';');
    }
}

fn compile_conditional_branches(stmt: &AstNode, sym_table: &SymbolTable, buff: &mut String) {
    if let AstNodeVariant::If {
        cond,
        body,
        or_else,
        ..
    } = &stmt.variant
    {
        buff.push_str("if(");
        compile_expression(cond, sym_table, buff);
        buff.push_str("){");
        compile_body(body, sym_table, buff);
        buff.push('}');

        if let Some(or_else) = or_else {
            match &or_else.variant {
                AstNodeVariant::If { .. } => {
                    buff.push_str("else ");
                    compile_conditional_branches(or_else, sym_table, buff)
                }
                AstNodeVariant::Else { body, .. } => {
                    buff.push_str("else{");
                    compile_body(body, sym_table, buff);
                    buff.push('}');
                }
                _ => (),
            }
        }
    }
}

fn compile_while_loop(stmt: &AstNode, sym_table: &SymbolTable, buff: &mut String) {
    if let AstNodeVariant::While { cond, body, .. } = &stmt.variant {
        buff.push_str("while(");
        compile_expression(cond, sym_table, buff);
        buff.push_str("){");
        compile_body(body, sym_table, buff);
        buff.push('}');
    }
}

fn compile_each_loop(stmt: &AstNode, sym_table: &SymbolTable, buff: &mut String) {
    if let AstNodeVariant::Each {
        elem_sym,
        iterable,
        body,
        ..
    } = &stmt.variant
    {
        buff.push_str(&format!("for(let {} of ", sym_table[&elem_sym.id].name));
        compile_expression(iterable, sym_table, buff);
        buff.push_str("){");
        compile_body(body, sym_table, buff);
        buff.push('}');
    }
}

fn compile_expression(expr: &AstNode, sym_table: &SymbolTable, buff: &mut String) {
    match &expr.variant {
        AstNodeVariant::Assign { .. }
        | AstNodeVariant::AddAssign { .. }
        | AstNodeVariant::SubAssign { .. }
        | AstNodeVariant::MulAssign { .. }
        | AstNodeVariant::DivAssign { .. }
        | AstNodeVariant::ModAssign { .. }
        | AstNodeVariant::Eq { .. }
        | AstNodeVariant::Neq { .. }
        | AstNodeVariant::Or { .. }
        | AstNodeVariant::And { .. }
        | AstNodeVariant::Gt { .. }
        | AstNodeVariant::Gte { .. }
        | AstNodeVariant::Lt { .. }
        | AstNodeVariant::Lte { .. }
        | AstNodeVariant::Add { .. }
        | AstNodeVariant::Sub { .. }
        | AstNodeVariant::Mul { .. }
        | AstNodeVariant::Div { .. }
        | AstNodeVariant::Mod { .. } => compile_binary_expression(expr, sym_table, buff),

        AstNodeVariant::Not { expr, .. } => {
            buff.push('!');
            compile_expression(expr, sym_table, buff);
        }
        AstNodeVariant::Neg { expr, .. } => {
            buff.push('-');
            compile_expression(expr, sym_table, buff);
        }

        AstNodeVariant::FunctionCall { callee, args, .. } => {
            compile_expression(callee, sym_table, buff);
            buff.push('(');
            for (i, el) in args.iter().enumerate() {
                compile_expression(el, sym_table, buff);
                if i != args.len() - 1 {
                    buff.push(',');
                }
            }
            buff.push(')');
        }
        AstNodeVariant::IndexAccess { object, index, .. } => {
            compile_expression(object, sym_table, buff);
            buff.push('[');
            compile_expression(index, sym_table, buff);
            buff.push(']');
        }

        AstNodeVariant::Symbol { name, .. } => buff.push_str(name),
        AstNodeVariant::Literal { lit, .. } => compile_literal(lit, sym_table, buff),

        _ => unreachable!(),
    }
}

fn compile_binary_expression(expr: &AstNode, sym_table: &SymbolTable, buff: &mut String) {
    let op = match &expr.variant {
        AstNodeVariant::Assign { .. } => "=",
        AstNodeVariant::AddAssign { .. } => "+=",
        AstNodeVariant::SubAssign { .. } => "-=",
        AstNodeVariant::MulAssign { .. } => "*=",
        AstNodeVariant::DivAssign { .. } => "/=",
        AstNodeVariant::ModAssign { .. } => "%=",
        AstNodeVariant::Eq { .. } => "==",
        AstNodeVariant::Neq { .. } => "!=",
        AstNodeVariant::Or { .. } => "||",
        AstNodeVariant::And { .. } => "&&",
        AstNodeVariant::Gt { .. } => ">",
        AstNodeVariant::Gte { .. } => ">=",
        AstNodeVariant::Lt { .. } => "<",
        AstNodeVariant::Lte { .. } => "<=",
        AstNodeVariant::Add { .. } => "+",
        AstNodeVariant::Sub { .. } => "-",
        AstNodeVariant::Mul { .. } => "*",
        AstNodeVariant::Div { .. } => "/",
        AstNodeVariant::Mod { .. } => "%",
        _ => unreachable!(),
    };

    let (lhs, rhs) = match &expr.variant {
        AstNodeVariant::Assign { lhs, rhs, .. }
        | AstNodeVariant::AddAssign { lhs, rhs, .. }
        | AstNodeVariant::SubAssign { lhs, rhs, .. }
        | AstNodeVariant::MulAssign { lhs, rhs, .. }
        | AstNodeVariant::DivAssign { lhs, rhs, .. }
        | AstNodeVariant::ModAssign { lhs, rhs, .. }
        | AstNodeVariant::Eq { lhs, rhs, .. }
        | AstNodeVariant::Neq { lhs, rhs, .. }
        | AstNodeVariant::Or { lhs, rhs, .. }
        | AstNodeVariant::And { lhs, rhs, .. }
        | AstNodeVariant::Gt { lhs, rhs, .. }
        | AstNodeVariant::Gte { lhs, rhs, .. }
        | AstNodeVariant::Lt { lhs, rhs, .. }
        | AstNodeVariant::Lte { lhs, rhs, .. }
        | AstNodeVariant::Add { lhs, rhs, .. }
        | AstNodeVariant::Sub { lhs, rhs, .. }
        | AstNodeVariant::Mul { lhs, rhs, .. }
        | AstNodeVariant::Div { lhs, rhs, .. }
        | AstNodeVariant::Mod { lhs, rhs, .. } => (lhs, rhs),

        _ => unreachable!(),
    };

    compile_expression(lhs, sym_table, buff);
    buff.push_str(op);
    compile_expression(rhs, sym_table, buff);
}

fn compile_literal(lit: &Literal, sym_table: &SymbolTable, buff: &mut String) {
    match lit {
        Literal::Int(n) => buff.push_str(&n.to_string()),
        Literal::Float(n) => buff.push_str(&n.to_string()),
        Literal::Bool(b) => buff.push_str(&b.to_string()),
        Literal::Char(ch) => buff.push_str(&format!("\"{ch}\"")),
        Literal::String(s) => buff.push_str(&format!("\"{s}\"")),
        Literal::Array { elems, .. } => {
            buff.push('[');
            for (i, el) in elems.iter().enumerate() {
                compile_expression(el, sym_table, buff);
                if i != elems.len() - 1 {
                    buff.push(',');
                }
            }
            buff.push(']');
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    fn wrap(program: &str) -> String {
        format!("{program}main();")
    }

    #[test]
    fn main_function() {
        let result = compile("def main {return;}");

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), wrap("function main(){return;}"));
    }

    #[test]
    fn var_declaration_with_literals() {
        let result = compile(indoc! {"
            def main {
                var x = 5;
                var foo = [[]int [int 1, 2, x], [int]];
            }
        "});

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            wrap("function main(){let x=5;let foo=[[1,2,x],[]];}")
        )
    }

    #[test]
    fn var_declaration_with_math_expr() {
        let result = compile(indoc! {"
            def main {
                var x = 5;
                var y = 1 + 5 / x - 2 * 7 % 2;
            }
        "});

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            wrap("function main(){let x=5;let y=1+5/x-2*7%2;}"),
        );
    }

    #[test]
    fn char_and_string_val() {
        let result = compile(
            r#"
            def main {
                var x = '1';
                var y = "Hello, World!";
            }
        "#,
        );

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            wrap(r#"function main(){let x="1";let y="Hello, World!";}"#),
        );
    }

    #[test]
    fn binary_operations() {
        let result = compile(indoc! {"
            def main {
                var x = 5;
                x += 1;
                x -= 1;
                x *= 2;
                x /= 2;
                x %= 2;
                var y = x >= 0 == false;
            }
        "});

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            wrap("function main(){let x=5;x+=1;x-=1;x*=2;x/=2;x%=2;let y=x>=0==false;}")
        );
    }

    #[test]
    fn conditional_branches() {
        let result = compile(indoc! {"
            def main {
                if !false {
                    if false {} else {}
                } else if true {}
            }
        "});

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            wrap("function main(){if(!false){if(false){}else{}}else if(true){}}"),
        );
    }

    #[test]
    fn while_loop() {
        let result = compile(indoc! {"
            def main {
                while true {
                    var i = 0;
                    break;
                }
            }
        "});

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            wrap("function main(){while(true){let i=0;break;}}")
        );
    }

    #[test]
    fn each_loop() {
        let result = compile(indoc! {"
            def main {
                var arr = [int 1, 2, 3];
                each n in arr {
                    var i = n * 2;
                }
            }
        "});

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            wrap("function main(){let arr=[1,2,3];for(let n of arr){let i=n*2;}}")
        );
    }

    #[test]
    fn index_access() {
        let result = compile(indoc! {"
            def main {
                var arr = [int 1, 2, 3];
                arr[0];
            }
        "});

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            wrap("function main(){let arr=[1,2,3];arr[0];}")
        );
    }

    #[test]
    fn function_call() {
        let result = compile(indoc! {"
            def foo: int {
                return 5;
            }

            def main {
                var x = foo();
            }
        "});

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            wrap("function foo(){return 5;}function main(){let x=foo();}")
        );
    }

    #[test]
    fn print_value() {
        let result = compile(indoc! {r#"
            def main {
                debug "Hello, World!";
            }
        "#});

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            wrap("function main(){$print(\"Hello, World!\");}")
        );
    }
}
