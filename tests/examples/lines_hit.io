fun main() {
  let sdl_context = SDL::init();
  let video = sdl_context.video();
  let window = video.window("Lines hit", 800, 600);

  let canvas = window.get_canvas();

  let event_pump = sdl_context.event_pump();

  let target = 0;
  let line_a = Line {
    start: Vec2 {
      x: 50,
      y: 50,
    },
    end: Vec2 {
      x: 750,
      y: 550,
    },
  };
  let line_b = Line {
    start: Vec2 {
      x: 50,
      y: 550,
    },
    end: Vec2 {
      x: 750,
      y: 50,
    },
  };

  let prev_left_button = false;

  while (true) {
    let event  = event_pump.poll();
    if (event.is_quit()) {
      return nil;
    }

    if ((!prev_left_button) && event_pump.is_mouse_button_down(MouseButton::LEFT())) {
      let position = event_pump.mouse_position();
      if (target / 2 == 0) {
        if (target % 2 == 0) {
          line_a.start = Vec2 {
            x: position.x,
            y: position.y,
          };
        } else {
          line_a.end = Vec2 {
            x: position.x,
            y: position.y,
          };
        }
      } else {
        if (target % 2 == 0) {
          line_b.start = Vec2 {
            x: position.x,
            y: position.y,
          };
        } else {
          line_b.end = Vec2 {
            x: position.x,
            y: position.y,
          };
        }
      }

      target = (target + 1) % 4;
    }
    prev_left_button = event_pump.is_mouse_button_down(MouseButton::LEFT());

    canvas.set_draw_color(0, 0, 0);
    canvas.clear();

    let hit = line_a.intersects(line_b);

    if (hit) {
      canvas.set_draw_color(255, 0, 0);
    } else {
      canvas.set_draw_color(255, 255, 255);
    }
    line_a.render(canvas);

    if (hit) {
      canvas.set_draw_color(0, 255, 0);
    } else {
      canvas.set_draw_color(255, 255, 255);
    }
    line_b.render(canvas);

    canvas.present();
  }

  return nil;
}
