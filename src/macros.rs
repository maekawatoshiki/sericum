#[macro_export]
macro_rules! some_then {
    ($x:ident, $e:expr, $t:expr) => {{
        if let Some($x) = $e {
            $t
        }
    }};
}

#[macro_export]
macro_rules! when_debug {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            $($arg)*;
        }
    };
}
