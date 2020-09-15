pub mod executor;
pub mod jit;

pub fn roundup(n: i32, align: i32) -> i32 {
    (n + align - 1) & !(align - 1)
}
