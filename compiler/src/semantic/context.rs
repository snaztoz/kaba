use super::scope::ScopeStack;

#[derive(Default)]
pub struct Context {
    pub scope_stack: ScopeStack
}
