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

  (** Collapsible parens view *)
let inner_parens c i =
  let parens_re = Re.Str.regexp "(*[a-zA-Z]*)*" in
  let in_text = canto_view c i in
  let _ = Re.Str.search_forward parens_re in_text 0 in
  let match_begin = Re.Str.match_beginning () in
  let _match_end = Re.Str.match_end () in (* find out how to concatenate images like this: hcat ? *)
  let _before_after = String.to_seq in_text in
  let _before = Seq.take (pred match_begin) in
  let parens = Re.Str.matched_string in_text in
  W.unfoldable ~folded_by_default:true (Lwd.pure @@ W.string parens)

let parens_button ~(label : char) ~f ~(i : int Lwd.var) =
  let open Nottui in
  let open Notty in
  let color = function
    | 0 -> A.(bg white ++ fg (gray 3) ++ st bold)
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
            let before, p, after =
              ( Seq.init (pred curr) (Fun.const ' '),
                Seq.init curr (Fun.const label),
                Seq.init (curr - pred curr) (Fun.const ' ') ) (* this needs adjusting *)
            in
            let init : char Seq.t = Seq.empty in
            Seq.append init before |> Seq.append p |> Seq.append after
            |> String.of_seq
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
  let level = Lwd.var (Lwd_utils.clampi 2 ~min:0 ~max:5) in
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
                 let text = canto_view (if c == 3 then 4 else c) i in
                 let lines = String.split_on_char '\n' text in
                 let ln = List.length lines in
                 let body =
                   if ln > 10 then W.scroll_area (W.string text |> Lwd.pure)
                   else Lwd.pure (W.string text)
                 in
                 [
                   Lwd.pure (Ui.space 0 10);
                   canto_button ~label:"     -     " ~f:pred;
                   Lwd.pure (Ui.space 0 5);
                   body;
                   Lwd.pure (Ui.space 0 5);
                   canto_button ~label:"     +     " ~f:succ;
                   Lwd.pure (Ui.space 0 10);
                 ]
                 |> W.vbox);
          Lwd.pure (Ui.space 10 0);
          parens_button ~label:')' ~f:succ ~i:level;
        ];
    ]
