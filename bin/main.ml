open Nottui
open Nia_tty.Core


let () =
  Lwd.set base (
    Nottui_widgets.h_pane
      (Nottui_widgets.scroll_area (varying_width contents))
      (Lwd.pure Ui.empty)
  );
  Ui_loop.run ~quit_on_escape:true (Nottui_widgets.window_manager_view wm)