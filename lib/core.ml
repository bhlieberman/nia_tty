open Nottui

module P = Nottui_pretty

module W = Nottui_widgets


let base = Lwd.var W.empty_lwd

let wm = W.window_manager (Lwd.join (Lwd.get base))

let doc: ((Ui.t Lwd.t) Lwd_table.t) = Lwd_table.make ()


let () = Lwd_table.append' doc Layout.frame

let doc = 
  let mconcat = Lwd_utils.lift_monoid Ui.pack_x in
  Lwd.join (Lwd_table.reduce mconcat doc)
