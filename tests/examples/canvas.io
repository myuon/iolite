fun main() {
  let ui = UI::init();
  let window = Window::build(ui, "Area Canvas", 200, 200);

  let hbox = HorizontalBox::build();

  window.set_child(hbox.to_control());
  window.show();

  let area = Area::build(fun (param: AreaDrawParams) {
    print_str("draw\n");

    return nil;
  });

  ui.main();
}
