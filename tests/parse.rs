#[macro_export]
macro_rules! set_insta_env {
    () => {
        fn set() {
            let key = "INSTA_UPDATE";
            unsafe {
                std::env::set_var(key, "allow");
            }
        }
    };
}

