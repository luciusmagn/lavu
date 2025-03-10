use color_eyre::eyre::bail;
use color_eyre::Result;

use crate::ast::ast1::SExp;

use super::ast1::Atom;

pub enum Definition {
    Plain(String, SExp),
    LambdaShorthand(SExp, Vec<SExp>),
}

impl Definition {
    // TODO: Probably produce report
    fn name(&self) -> Result<String> {
        match self {
            Definition::Plain(name, _) => Ok(name.clone()),
            Definition::LambdaShorthand(SExp::List(arglist, _), _) => {
                if let Some(SExp::Atom(Atom::Identifier(name), _)) =
                    arglist.get(0)
                {
                    Ok(name.clone())
                } else {
                    bail!("The name of a (define ...) must be an identifier")
                }
            }
            _ => bail!("(define ...) must be followed by either a list or an identifier"),
        }
    }
}

pub enum Stage2Ast {
    SExp(SExp),
    Definition(Definition),
}
