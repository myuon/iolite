fun main() {
  let app = extcall_app_default();

  let title = "Hello, World!";
  let window = extcall_window_new(100, 200, 300, 400, title.ptr as rawptr, title.length);

  let flex = extcall_flex_default_fill();
  extcall_flex_column(flex);
  extcall_flex_set_margins(flex, 30, 40, 30, 40);

  let button_title = "+";
  let button_inc = extcall_button_default(button_title.ptr as rawptr, button_title.length);

  let button_title = "-";
  let button_dec = extcall_button_default(button_title.ptr as rawptr, button_title.length);

  extcall_flex_end(flex);

  extcall_window_end(window);
  extcall_window_show(window);

  extcall_app_run(app);

  return nil;
}
