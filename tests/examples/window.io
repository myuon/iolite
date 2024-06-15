fun main() {
  let ui = UI::init();

  let window = Window::build(ui, "Hello, World!", 300, 200);
  let layout = VerticalBox::build();

  window.set_child(layout.to_control());
  window.show();

  ui.main();
}
