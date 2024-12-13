use scope::ScopeStack;

pub mod scope;

#[derive(Default)]
pub struct SharedState {
    pub ss: ScopeStack,
}
