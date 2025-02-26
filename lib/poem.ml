module W = Nottui_widgets
module P = Parens.ParensMatcher
open Lwd_infix
open Lwd.Infix

module Canto = struct
  type t = { id : int; body : string; subtree : string list }
  (** Module for working on data representation of the poem *)
end

module State = struct
  let canto = Lwd.var (Lwd_utils.clampi 1 ~min:1 ~max:4)
  let level = Lwd.var (Lwd_utils.clampi 1 ~min:0 ~max:5)

  let make_parens_button_label (label : string) =
    let$ snapshot = Lwd.get level in
    let space_around = Seq.init snapshot (Fun.const ' ') |> String.of_seq in
    let parens =
      Seq.unfold (fun i -> if i < snapshot then Some (i, succ i) else None) 0
      |> List.of_seq
      |> List.map (fun i -> if i == 0 then "" else label)
      |> String.concat ""
    in
    space_around ^ parens ^ space_around
end

let canto_view c i =
  let dir =
    match c with
    | 0 -> Some "canto_I"
    | 1 -> Some "canto_I"
    | 2 -> Some "canto_II"
    | 4 -> Some "canto_IV"
    | _ -> None
  in
  let dirname =
    Option.bind dir (fun d -> Filename.concat "assets" d |> Option.some)
  in
  let fname = if i == 0 then "body.txt" else Format.sprintf "par_%d.txt" i in
  try
    In_channel.with_open_text
      (Filename.concat (Option.get dirname) fname)
      In_channel.input_all
  with _ ->
    "You tried to read a part of the poem that doesn't exist.\n\
     Please write it yourself..."

(** Function to create modal on click of button-like text elements *)
let makeModal i txt =
  let open Nottui in
  let$ modal =
    let modal_body = W.string ~attr:Notty.A.(bg white ++ st bold) txt in
    W.hbox
      [
        Lwd.pure (Ui.space 15 0);
        W.vbox [ Lwd.pure (Ui.space 0 10); Lwd.pure modal_body ];
      ]
  in
  Nottui.Ui.zcat [ i; modal ]

(** Collapsible parens view (WIP) *)
let inner_parens c i =
  let open Nottui in
  let in_text = canto_view c i in
  let p = P.parens in
  let all_matches = Re.matches p in_text in
  let widgets =
    List.map
      (fun m ->
        let len = String.length m in
        W.unfoldable ~folded_by_default:true
          (Lwd.pure @@ W.string "...")
          (fun _ ->
            if len = 0 then
              let _halves = String.split_on_char '\n' m in
              let first_half, second_half =
                (List.nth _halves 0, List.nth _halves 1)
              in
              Nottui.Ui.hcat [ W.string first_half; W.string second_half ]
              |> Lwd.pure
            else W.string m |> Lwd.pure))
      all_matches
  in
  let replaced =
    Re.split p in_text |> List.map (Fun.compose Lwd.pure W.string)
  in
  let$ fixed_size =
    Seq.interleave (List.to_seq replaced) (List.to_seq widgets)
    |> List.of_seq |> Lwd_utils.flatten_l |> Lwd.map ~f:Nottui.Ui.vcat
  in
  Lwd.pure (Ui.resize ~h:10 fixed_size) |> W.scroll_area

let parens_button ~label =
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
  let$ current_idx = Lwd.get State.level
  and$ parens_text = State.make_parens_button_label label in
  let parens_button =
    let f = match label with "(" -> pred | ")" -> succ | _ -> Fun.id in
    W.button ~attr:(color current_idx) parens_text (fun () ->
        State.level $= f (Lwd.peek State.level))
  in
  let children =
    [
      Lwd.pure (Ui.space 0 20); Lwd.pure parens_button; Lwd.pure (Ui.space 0 20);
    ]
  in
  W.vbox children

let status_bar =
  let open Notty in
  let make_label i =
    string_of_int i |> W.string ~attr:A.(bg blue ++ fg white) |> Lwd.pure
  in
  let current_level = Lwd.get State.level >>= make_label in
  let current_canto = Lwd.get State.canto >>= make_label in
  Lwd_utils.flatten_l [ current_level; current_canto ]

let button_pane =
  let open Nottui in
  let open Notty in
  let canto_button ~label =
    W.hbox
      [
        Lwd.pure @@ Ui.space 25 0;
        Lwd.pure
        @@ W.button
             ~attr:A.(bg white ++ fg red ++ st bold)
             label
             (fun () ->
               State.canto
               $= (match label with
                  | " + " -> succ
                  | " - " -> pred
                  | _ -> Fun.id)
                  @@ Lwd.peek State.canto);
      ]
  in
  let$* canto_level = Lwd.get State.canto
  and$ parens_level = Lwd.get State.level in
  let resizeable_body = inner_parens canto_level parens_level in
  let canto_button_top = canto_button ~label:" - " in
  let canto_button_bottom = canto_button ~label:" + " in
  let$* parens_button_left = parens_button ~label:"("
  and$ parens_button_right = parens_button ~label:")" in
  W.vbox
    [
      W.hbox
        [
          parens_button_left;
          Lwd.pure (Ui.space 5 0);
          W.vbox
            [
              Lwd.pure (Ui.space 0 10);
              canto_button_top;
              Lwd.pure (Ui.space 0 5);
              Lwd.join resizeable_body;
              Lwd.pure (Ui.space 0 5);
              canto_button_bottom;
              Lwd.pure (Ui.space 0 10);
            ];
          Lwd.pure (Ui.space 5 0);
          parens_button_right;
        ];
    ]
