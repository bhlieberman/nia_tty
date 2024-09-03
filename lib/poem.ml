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
  text

(** Collapsible parens view (WIP) *)
let inner_parens c i =
  let in_text = canto_view c i in
  let p =
    Re.(
      seq
        [
          repn (str "(") 1 None;
          repn (alt [ word (rep alpha); space ]) 1 None;
          repn (str ")") 1 None;
        ])
    |> Re.compile
  in
  let all_matches = Re.matches p in_text in
  let widgets =
    List.map
      (fun m ->
        W.unfoldable ~folded_by_default:true
          (Lwd.pure @@ W.string "...")
          (fun _ -> Lwd.pure (W.string m)))
      all_matches
  in
  let replaced =
    Re.split p in_text |> List.map (Fun.compose Lwd.pure W.string)
  in
  Seq.interleave (List.to_seq replaced) (List.to_seq widgets)
  |> List.of_seq |> Lwd_utils.flatten_l |> Lwd.map ~f:Nottui.Ui.vcat

let parens_button ~(label : char) ~f ~(i : int Lwd.var) =
  let open Nottui in
  let open Notty in
  let color = function
    | 0 -> A.(bg (rgb_888 ~r:24 ~g:48 ~b:100) ++ fg white ++ st bold)
    | 1 -> A.(bg red ++ fg white ++ st bold)
    | 2 -> A.(bg yellow ++ fg white ++ st bold)
    | 3 -> A.(bg blue ++ fg white ++ st bold)
    | 4 -> A.(bg green ++ fg white ++ st bold)
    | 5 -> A.(bg magenta ++ fg white ++ st bold)
    | _ -> A.(bg white ++ st bold)
  in
  let button_box =
    [
      Lwd.pure @@ Ui.space 0 25;
      Lwd.map (Lwd.get i) ~f:(fun curr ->
          let parens =
            let open Seq in
            let open Fun in
            if curr >= 1 then
              let before, p, after =
                ( init (pred curr) (const ' '),
                  init curr (const label),
                  init (pred curr) (const ' ') )
                (* this needs adjusting *)
              in
              let init : char Seq.t = Seq.empty in
              before |> append init |> flip append p |> flip append after
              |> String.of_seq
            else "     "
          in
          W.button ~attr:(color curr) parens (fun () -> Lwd.set i (f curr)));
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
  let canto_button ~label ~f =
    W.hbox
      [
        Lwd.pure @@ Ui.space 25 0;
        Lwd.pure
        @@ W.button
             ~attr:A.(bg white ++ fg red ++ st bold)
             label
             (fun () -> Utils.update f canto);
      ]
  in
  W.vbox
    [
      W.hbox
        [
          parens_button ~label:'(' ~f:pred ~i:level;
          Lwd.pure (Ui.space 10 0);
          Lwd.join
          @@ Lwd.map2 (Lwd.get canto) (Lwd.get level) ~f:(fun c i ->
                 let text = inner_parens (if c == 3 then 4 else c) i in
                 (* let lines = String.split_on_char '\n' text in
                    let ln = List.length lines in
                    let body =
                      if ln > 10 then W.scroll_area (W.string text |> Lwd.pure)
                      else Lwd.pure (W.string text)
                    in *)
                 [
                   Lwd.pure (Ui.space 0 10);
                   canto_button ~label:"     -     " ~f:pred;
                   Lwd.pure (Ui.space 0 5);
                   text;
                   Lwd.pure (Ui.space 0 5);
                   canto_button ~label:"     +     " ~f:succ;
                   Lwd.pure (Ui.space 0 10);
                 ]
                 |> W.vbox);
          Lwd.pure (Ui.space 10 0);
          parens_button ~label:')' ~f:succ ~i:level;
        ];
    ]
