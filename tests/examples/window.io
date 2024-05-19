fun main() {
  let app = extcall_app_default();
  let count = 0;

  let title = "Hello, World!";
  let window = extcall_window_new(100, 200, 300, 400, title.ptr as rawptr, title.length);

  let flex = extcall_flex_default_fill();
  extcall_flex_column(flex);
  extcall_flex_set_margins(flex, 30, 40, 30, 40);

  let button_title = "+";
  let button_inc = extcall_button_default(button_title.ptr as rawptr, button_title.length);

  let frame_title = int_to_string(count);
  let frame = extcall_frame_default();
  extcall_frame_set_label(frame, frame_title.ptr as rawptr, frame_title.length);

  let button_title = "-";
  let button_dec = extcall_button_default(button_title.ptr as rawptr, button_title.length);

  extcall_flex_end(flex);

  extcall_window_end(window);
  extcall_window_show(window);

  let callback = fun () {
    count = count + 1;

    let label = int_to_string(count);
    extcall_frame_set_label(frame, label.ptr as rawptr, label.length);

    return nil;
  };
  extcall_button_set_callback(button_inc, callback.ptr, callback.env);

  let callback = fun () {
    if (count > 0) {
      count = count - 1;
    }

    let label = int_to_string(count);
    extcall_frame_set_label(frame, label.ptr as rawptr, label.length);

    return nil;
  };
  extcall_button_set_callback(button_dec, callback.ptr, callback.env);

  while (extcall_app_wait(app)) {
    extcall_app_redraw(app);
  }

  return nil;
}
