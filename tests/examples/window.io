fun main() {
  extcall_ui_init();
  let window = extcall_ui_new_window("Hello World".ptr as rawptr, 400, 320, 0);
  extcall_ui_control_show(window);
  extcall_ui_new_area();

  extcall_ui_main();

  return nil;
}
