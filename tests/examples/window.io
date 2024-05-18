fun main() {
  let app = extcall_app_default();

  let title = "Hello, World!";
  extcall_window_new(100, 200, 300, 400, title.ptr as rawptr, title.length);

  extcall_app_run(app);

  return nil;
}
