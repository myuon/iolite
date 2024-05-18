fun main() {
  extcall_ui_init();
  let window = extcall_ui_new_window("Hello World".ptr as rawptr, 400, 320, 0);
  extcall_ui_control_show(window);
  extcall_ui_new_area();

  let draw = fun () {
    print_str("draw\n");

    return nil;
  };
  extcall_ui_new_area_handler(draw.ptr, draw.env);

  extcall_ui_main();

  return nil;
}
