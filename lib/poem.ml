module W = Nottui_widgets

let canto_view c i =
  let dir =
    match c with
    | 0 -> "canto_I"
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
  W.string ~attr:Notty.A.(st bold) text

let parens_button ~label ~f ~(i : int Lwd.var) =
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
  let button_box =
    [
      Lwd.pure @@ Ui.space 0 25;
      Lwd.map (Lwd.get i) ~f:(fun curr ->
          W.button ~attr:(color curr) label (fun () -> Lwd.set i (f curr)));
      Lwd.pure @@ Ui.space 0 25;
    ]
    |> W.vbox
  in
  button_box

let button_pane =
  let open Nottui in
  let open Notty in
  let level = Lwd.var (Lwd_utils.clampi 0 ~min:0 ~max:5) in
  let canto = Lwd.var (Lwd_utils.clampi 1 ~min:1 ~max:4) in
  let up_button =
    W.button
      ~attr:A.(bg white ++ fg red)
      "-"
      (fun () -> Utils.update pred canto)
  in
  let down_button =
    W.button
      ~attr:A.(bg white ++ fg red)
      "+"
      (fun () -> Utils.update succ canto)
  in
  W.hbox
    [
      Lwd.pure @@ Ui.space 10 0;
      W.vbox
        [
          W.hbox
            [
              parens_button ~label:"(((((" ~f:pred ~i:level;
              Lwd.join
              @@ Lwd.map2 (Lwd.get canto) (Lwd.get level) ~f:(fun c i ->
                     List.map Lwd.pure
                       [
                         up_button;
                         canto_view (if c == 3 then 4 else c) i;
                         down_button;
                       ]
                     |> W.vbox);
              parens_button ~label:")))))" ~f:succ ~i:level;
            ];
        ];
      Lwd.pure @@ Ui.space 10 0;
    ]
