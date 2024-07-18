#[macro_export]
macro_rules! measure_time {
    ($e:expr, $b:block) => {{
        let now = std::time::Instant::now();

        let result = $b;

        eprintln!($e, now.elapsed().as_millis());

        result
    }};
}
