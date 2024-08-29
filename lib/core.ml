open Nottui

module P = Nottui_pretty

module W = Nottui_widgets


let base = Lwd.var W.empty_lwd

let wm = W.window_manager (Lwd.join (Lwd.get base))

let doc: ((P.t Lwd.t) Lwd_table.t) = Lwd_table.make ()

let () = Lwd_table.append' doc (Lwd.map Poem.button_pane ~f:(fun canto -> P.ui canto))

let doc = 
  Lwd.join (Lwd_table.reduce (Lwd_utils.lift_monoid (P.empty, P.(^^))) doc)

let varying_width f =
  let width = Lwd.var 0 in
  Lwd.map (f (Lwd.get width)) ~f:(fun ui ->
      Ui.size_sensor
        (fun ~w ~h:_ -> if Lwd.peek width <> w then Lwd.set width w)
        (Ui.resize ~sw:1 ~sh:1 ~w:0 ui))

let contents width = Lwd.map2 ~f:P.pretty width doc