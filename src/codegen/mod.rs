pub mod riscv64;

thread_local! {
     pub static INTERNALS: Vec<String> = include!("internals").iter().map(|s| s.to_string()).collect();
}

pub fn is_internal_function(name: &str) -> bool {
    INTERNALS.with(|i| i.iter().find(|n| *n == name).is_some())
}

pub fn internal_function_names() -> Vec<String> {
    INTERNALS.with(|i| i.clone())
}
