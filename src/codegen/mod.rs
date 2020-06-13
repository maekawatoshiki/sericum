pub mod common;
#[cfg(feature = "riscv64")]
pub mod riscv64;
#[cfg(feature = "x86_64")]
pub mod x64;
#[cfg(feature = "riscv64")]
pub use riscv64 as arch;
#[cfg(feature = "x86_64")]
pub use x64 as arch;

thread_local! {
     pub static INTERNALS: Vec<String> = include!("internals").iter().map(|s| s.to_string()).collect();
}

pub fn is_internal_function(name: &str) -> bool {
    INTERNALS.with(|i| i.iter().find(|n| *n == name).is_some())
}

pub fn internal_function_names() -> Vec<String> {
    INTERNALS.with(|i| i.clone())
}
