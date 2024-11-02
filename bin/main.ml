open Nottui
open Nia_tty.Core

let () =
  Lwd.set base doc;
  Ui_loop.run ~quit_on_escape:true (Nottui_widgets.window_manager_view wm)
