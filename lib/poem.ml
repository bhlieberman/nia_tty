module W = Nottui_widgets

let canto_view c i =
  let dir =
    match c with
    | 1 -> "canto_I"
    | 2 -> "canto_II"
    | 4 -> "canto_IV"
    | _ -> failwith "not possible"
  in
  let dirname = Filename.concat "assets" dir in
  let fname = if i == 0 then "body.txt" else Format.sprintf "par_%d.txt" i in
  let text =
    In_channel.with_open_text
      (Filename.concat dirname fname)
      In_channel.input_all
  in
  W.string text

let button_pane =
  let open Nottui in
  let open Notty in
  let color = function
    | 1 -> A.(bg red)
    | 2 -> A.(bg yellow)
    | 3 -> A.(bg blue)
    | 4 -> A.(bg green)
    | 5 -> A.(bg magenta)
    | _ -> A.(bg (gray 3))
  in
  let level = Lwd.var 0 in
  let canto = Lwd.var 1 in
  let go_up current = if current != 1 then pred current else current in
  let go_down current = if current != 4 then succ current else current in
  let left_button i =
    W.button ~attr:(color i) "(((((" (fun () ->
        Lwd.set level (pred @@ Lwd.peek level))
  in
  let right_button i =
    W.button ~attr:(color i) ")))))" (fun () ->
        Lwd.set level (succ @@ Lwd.peek level))
  in
  let up_button _i =
    W.button
      ~attr:A.(bg white ++ fg red)
      "-"
      (fun () -> Lwd.set canto (go_up (Lwd.peek canto)))
  in
  let down_button _ =
    W.button
      ~attr:A.(bg white ++ fg red)
      "+"
      (fun () -> Lwd.set canto (go_down (Lwd.peek canto)))
  in
  W.hbox
    [
      Lwd.map (Lwd.get level) ~f:(fun s -> left_button s);
      W.vbox
        [
          Lwd.map2 (Lwd.get canto) (Lwd.get level) ~f:(fun c i ->
              Ui.vcat [ up_button c; canto_view c i; down_button c ]);
        ];
      Lwd.map (Lwd.get level) ~f:(fun s -> right_button s);
    ]
