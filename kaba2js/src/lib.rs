use kabac::{AstNode, Literal, Result, SymbolTableData};

#[cfg(target_arch = "wasm32")]
pub mod wasm;

pub fn compile(src: &str) -> Result<String> {
    let (ast, sym_table) = kabac::compile(src)?;

    let mut buff = String::new();
    if let AstNode::Program { body, .. } = ast {
        compile_body(&body, &sym_table, &mut buff);
        buff.push_str("main();");
    }

    Ok(buff)
}

fn compile_body(body: &[AstNode], sym_table: &SymbolTableData, buff: &mut String) {
    for stmt in body {
        compile_statement(stmt, sym_table, buff);
    }
}

fn compile_statement(stmt: &AstNode, sym_table: &SymbolTableData, buff: &mut String) {
    match stmt {
        AstNode::FunctionDefinition { .. } => compile_function_definition(stmt, sym_table, buff),
        AstNode::VariableDeclaration { .. } => compile_variable_declaration(stmt, sym_table, buff),
        AstNode::If { .. } => compile_conditional_branches(stmt, sym_table, buff),
        AstNode::While { .. } => compile_while_loop(stmt, sym_table, buff),
        AstNode::Each { .. } => compile_each_loop(stmt, sym_table, buff),
        AstNode::Continue { .. } => {
            buff.push_str("continue;");
        }
        AstNode::Break { .. } => {
            buff.push_str("break;");
        }
        AstNode::Return { expr, .. } => {
            buff.push_str("return");
            if let Some(expr) = expr {
                buff.push(' ');
                compile_expression(expr, sym_table, buff);
            }
            buff.push(';');
        }
        AstNode::Debug { expr, .. } => {
            buff.push_str("$print(");
            compile_expression(expr, sym_table, buff);
            buff.push_str(");");
        }
        expr => {
            compile_expression(expr, sym_table, buff);
            buff.push(';');
        }
    }
}

fn compile_function_definition(stmt: &AstNode, sym_table: &SymbolTableData, buff: &mut String) {
    if let AstNode::FunctionDefinition {
        sym_id,
        params,
        body,
        ..
    } = stmt
    {
        let entry = &sym_table[sym_id];
        buff.push_str(&format!("function {}(", entry.name));

        let params_str = params
            .iter()
            .map(|p| String::from(&sym_table[&p.sym_id].name))
            .collect::<Vec<_>>()
            .join(",");
        buff.push_str(&format!("{params_str}){{"));

        compile_body(body, sym_table, buff);
        buff.push('}');
    }
}

fn compile_variable_declaration(stmt: &AstNode, sym_table: &SymbolTableData, buff: &mut String) {
    if let AstNode::VariableDeclaration { sym_id, val, .. } = stmt {
        let entry = &sym_table[sym_id];

        buff.push_str(&format!("let {}=", entry.name));
        compile_expression(val, sym_table, buff);

        buff.push(';');
    }
}

fn compile_conditional_branches(stmt: &AstNode, sym_table: &SymbolTableData, buff: &mut String) {
    if let AstNode::If {
        cond,
        body,
        or_else,
        ..
    } = stmt
    {
        buff.push_str("if(");
        compile_expression(cond, sym_table, buff);
        buff.push_str("){");
        compile_body(body, sym_table, buff);
        buff.push('}');

        match or_else.as_deref() {
            Some(AstNode::If { .. }) => {
                buff.push_str("else ");
                compile_conditional_branches(or_else.as_ref().unwrap(), sym_table, buff)
            }
            Some(AstNode::Else { body, .. }) => {
                buff.push_str("else{");
                compile_body(body, sym_table, buff);
                buff.push('}');
            }
            _ => (),
        }
    }
}

fn compile_while_loop(stmt: &AstNode, sym_table: &SymbolTableData, buff: &mut String) {
    if let AstNode::While { cond, body, .. } = stmt {
        buff.push_str("while(");
        compile_expression(cond, sym_table, buff);
        buff.push_str("){");
        compile_body(body, sym_table, buff);
        buff.push('}');
    }
}

fn compile_each_loop(stmt: &AstNode, sym_table: &SymbolTableData, buff: &mut String) {
    if let AstNode::Each {
        elem_sym_id,
        iterable,
        body,
        ..
    } = stmt
    {
        buff.push_str(&format!("for(let {} of ", sym_table[elem_sym_id].name));
        compile_expression(iterable, sym_table, buff);
        buff.push_str("){");
        compile_body(body, sym_table, buff);
        buff.push('}');
    }
}

fn compile_expression(expr: &AstNode, sym_table: &SymbolTableData, buff: &mut String) {
    match expr {
        AstNode::Assign { .. }
        | AstNode::AddAssign { .. }
        | AstNode::SubAssign { .. }
        | AstNode::MulAssign { .. }
        | AstNode::DivAssign { .. }
        | AstNode::ModAssign { .. }
        | AstNode::Eq { .. }
        | AstNode::Neq { .. }
        | AstNode::Or { .. }
        | AstNode::And { .. }
        | AstNode::Gt { .. }
        | AstNode::Gte { .. }
        | AstNode::Lt { .. }
        | AstNode::Lte { .. }
        | AstNode::Add { .. }
        | AstNode::Sub { .. }
        | AstNode::Mul { .. }
        | AstNode::Div { .. }
        | AstNode::Mod { .. } => compile_binary_expression(expr, sym_table, buff),

        AstNode::Not { expr, .. } => {
            buff.push('!');
            compile_expression(expr, sym_table, buff);
        }
        AstNode::Neg { expr, .. } => {
            buff.push('-');
            compile_expression(expr, sym_table, buff);
        }

        AstNode::FunctionCall { callee, args, .. } => {
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
        AstNode::IndexAccess { object, index, .. } => {
            compile_expression(object, sym_table, buff);
            buff.push('[');
            compile_expression(index, sym_table, buff);
            buff.push(']');
        }

        AstNode::Symbol { name, .. } => buff.push_str(name),
        AstNode::Literal { lit, .. } => compile_literal(lit, sym_table, buff),

        _ => unreachable!(),
    }
}

fn compile_binary_expression(expr: &AstNode, sym_table: &SymbolTableData, buff: &mut String) {
    let op = match expr {
        AstNode::Assign { .. } => "=",
        AstNode::AddAssign { .. } => "+=",
        AstNode::SubAssign { .. } => "-=",
        AstNode::MulAssign { .. } => "*=",
        AstNode::DivAssign { .. } => "/=",
        AstNode::ModAssign { .. } => "%=",
        AstNode::Eq { .. } => "==",
        AstNode::Neq { .. } => "!=",
        AstNode::Or { .. } => "||",
        AstNode::And { .. } => "&&",
        AstNode::Gt { .. } => ">",
        AstNode::Gte { .. } => ">=",
        AstNode::Lt { .. } => "<",
        AstNode::Lte { .. } => "<=",
        AstNode::Add { .. } => "+",
        AstNode::Sub { .. } => "-",
        AstNode::Mul { .. } => "*",
        AstNode::Div { .. } => "/",
        AstNode::Mod { .. } => "%",
        _ => unreachable!(),
    };

    let (lhs, rhs) = match expr {
        AstNode::Assign { lhs, rhs, .. }
        | AstNode::AddAssign { lhs, rhs, .. }
        | AstNode::SubAssign { lhs, rhs, .. }
        | AstNode::MulAssign { lhs, rhs, .. }
        | AstNode::DivAssign { lhs, rhs, .. }
        | AstNode::ModAssign { lhs, rhs, .. }
        | AstNode::Eq { lhs, rhs, .. }
        | AstNode::Neq { lhs, rhs, .. }
        | AstNode::Or { lhs, rhs, .. }
        | AstNode::And { lhs, rhs, .. }
        | AstNode::Gt { lhs, rhs, .. }
        | AstNode::Gte { lhs, rhs, .. }
        | AstNode::Lt { lhs, rhs, .. }
        | AstNode::Lte { lhs, rhs, .. }
        | AstNode::Add { lhs, rhs, .. }
        | AstNode::Sub { lhs, rhs, .. }
        | AstNode::Mul { lhs, rhs, .. }
        | AstNode::Div { lhs, rhs, .. }
        | AstNode::Mod { lhs, rhs, .. } => (lhs, rhs),

        _ => unreachable!(),
    };

    compile_expression(lhs, sym_table, buff);
    buff.push_str(op);
    compile_expression(rhs, sym_table, buff);
}

fn compile_literal(lit: &Literal, sym_table: &SymbolTableData, buff: &mut String) {
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
        let result = compile("fn main() {return;}");

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), wrap("function main(){return;}"));
    }

    #[test]
    fn var_declaration_with_literals() {
        let result = compile(indoc! {"
            fn main() {
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
            fn main() {
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
            fn main() {
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
            fn main() {
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
            fn main() {
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
            fn main() {
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
            fn main() {
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
            fn main() {
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
            fn foo(): int {
                return 5;
            }

            fn main() {
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
            fn main() {
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
