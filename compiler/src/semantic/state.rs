use super::scope::ScopeStack;

#[derive(Default)]
pub struct SharedState {
    pub ss: ScopeStack,
}
