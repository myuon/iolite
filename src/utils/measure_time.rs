#[macro_export]
macro_rules! measure_time {
    ($e:expr, $b:block, $flag:expr) => {{
        let now = std::time::Instant::now();

        let result = $b;

        if $flag {
            eprintln!($e, now.elapsed().as_millis());
        }

        result
    }};
    ($e:expr, $b:block) => {
        measure_time!($e, $b, true)
    };
}
