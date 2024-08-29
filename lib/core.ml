open Nottui
open Notty

module P = Nottui_pretty

module W = Nottui_widgets

let canto_i = In_channel.with_open_text "/home/slothrop/dev/ocaml/niaserie/assets/nia/canto_I.txt"
  In_channel.input_all

let canto_ii = In_channel.with_open_text "/home/slothrop/dev/ocaml/niaserie/assets/nia/canto_II.txt"
  In_channel.input_all

let canto_iv = In_channel.with_open_text "/home/slothrop/dev/ocaml/niaserie/assets/nia/canto_IV.txt"
  In_channel.input_all

let base = Lwd.var W.empty_lwd

let wm = W.window_manager (Lwd.join (Lwd.get base))

let canto_selector text f choices =
  let curr = Lwd.var (match text with | "Canto I" -> canto_i
  | "Canto II" -> canto_ii | "Canto IV" -> canto_iv | _ -> failwith "No match") in
  W.vbox [
    W.main_menu_item wm text (fun () ->
    Lwd.pure @@ Ui.vcat (
      List.map (fun choice -> W.sub_entry choice (fun () -> f choice)) choices
    ));
    (Lwd.map (Lwd.get curr) ~f:(fun s -> W.string ~attr:A.(bg red ++ st bold) s))
    ]

let cantos = 
  let cs = ["Canto I"; "Canto II"; "Canto IV"] in
  let choice = Lwd.var (List.hd cs) in
  Lwd.join (
    Lwd.map (Lwd.get choice)
      ~f:(fun current -> canto_selector current (Lwd.set choice) cs)
  )

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