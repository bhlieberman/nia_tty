module W = Nottui_widgets
module P = Parens.ParensMatcher

module Canto = struct
  type t = { id : int; body : string; subtree : string list }
  (** Module for working on data representation of the poem *)
end

module State = struct
  let canto = Lwd.var (Lwd_utils.clampi 1 ~min:1 ~max:4)
  let level = Lwd.var (Lwd_utils.clampi 0 ~min:0 ~max:5)
end

let memo : (string, string array) Hashtbl.t = Hashtbl.create 3

let updateMemo (m : (string, string array) Hashtbl.t) (canto : string)
    (idx : int) =
  let vs : string array = [||] in
  vs.(idx) <- canto;
  Hashtbl.add m canto vs

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
  let open Lwd_infix in
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
  Seq.interleave (List.to_seq replaced) (List.to_seq widgets)
  |> List.of_seq |> Lwd_utils.flatten_l |> Lwd.map ~f:Nottui.Ui.vcat

let big_button ~attr p f =
  let open Nottui in
  let area =
    Ui.mouse_area (fun ~x:_ ~y:_ _ ->
        f ();
        `Handled)
  in
  let raw_image = Notty.I.char attr ' ' 10 2 in
  area @@ Ui.hcat [ Ui.atom raw_image; W.string p ]

let parens_button ~label ~f =
  let open Nottui in
  let open Notty in
  let open Lwd_infix in
  let color = function
    | 0 -> A.(bg (rgb_888 ~r:24 ~g:48 ~b:100) ++ fg white ++ st bold)
    | 1 -> A.(bg red ++ fg white ++ st bold)
    | 2 -> A.(bg yellow ++ fg white ++ st bold)
    | 3 -> A.(bg blue ++ fg white ++ st bold)
    | 4 -> A.(bg green ++ fg white ++ st bold)
    | 5 -> A.(bg magenta ++ fg white ++ st bold)
    | _ -> A.(bg white ++ st bold)
  in
  let$ current_idx = Lwd.get State.level in
  let safe_idx = if current_idx == 0 then 1 else current_idx in
  let parens_text =
    (* Should move these Seq.init calls to State module *)
    let space_around = Seq.init (pred safe_idx) (Fun.const ' ') in
    let make_label = Seq.init safe_idx (Fun.const label) in
    if safe_idx >= 1 then
      let before, p, after =
        (space_around, make_label, space_around)
        (* this needs adjusting *)
      in
      before |> Seq.append Seq.empty |> Fun.flip Seq.append p
      |> Fun.flip Seq.append after |> String.of_seq
    else "     "
  in
  let parens_button =
    W.button ~attr:(color current_idx) parens_text (fun () ->
        Utils.update f State.level)
  in
  let children =
    [
      Lwd.pure (Ui.space 0 20); Lwd.pure parens_button; Lwd.pure (Ui.space 0 20);
    ]
  in
  W.vbox children

let status_bar =
  let open Lwd_infix in
  let open Notty in
  let$ current_level = Lwd.get State.level
  and$ current_canto = Lwd.get State.canto in
  let current_level_view =
    string_of_int current_level |> W.string ~attr:A.(bg blue ++ fg white)
  in
  let current_canto_view =
    string_of_int current_canto |> W.string ~attr:A.(bg blue ++ fg white)
  in
  [ current_level_view; current_canto_view ]

let button_pane =
  let open Nottui in
  let open Notty in
  let open Lwd_infix in
  let canto_button ~label ~f =
    W.hbox
      [
        Lwd.pure @@ Ui.space 25 0;
        Lwd.pure
        @@ W.button
             ~attr:A.(bg white ++ fg red ++ st bold)
             label
             (fun () -> Utils.update f State.canto);
      ]
  in
  let$* canto_level = Lwd.get State.canto
  and$ parens_level = Lwd.get State.level in
  let$* resizeable_body =
    let$ static_body = inner_parens canto_level parens_level in
    Ui.resize ~h:10 static_body |> Lwd.pure |> W.scroll_area
  in
  let canto_button_top = canto_button ~label:" - " ~f:pred in
  let canto_button_bottom = canto_button ~label:" +" ~f:succ in
  let$* parens_button_left = parens_button ~label:'(' ~f:pred in
  let$* parens_button_right = parens_button ~label:')' ~f:succ in
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
              resizeable_body;
              Lwd.pure (Ui.space 0 5);
              canto_button_bottom;
              Lwd.pure (Ui.space 0 10);
            ];
          Lwd.pure (Ui.space 5 0);
          parens_button_right;
        ];
    ]
