#[macro_export]
macro_rules! some_then {
    ($x:ident, $e:expr, $t:expr) => {{
        if let Some($x) = $e {
            $t
        }
    }};
}
