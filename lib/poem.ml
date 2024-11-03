module W = Nottui_widgets

module P = Parens.ParensMatcher

module Canto = struct
  type t = { id : int; body : string; subtree : string list }
  (** Module for working on data representation of the poem *)
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
  let text =
    try
      In_channel.with_open_text
        (Filename.concat (Option.get dirname) fname)
        In_channel.input_all
    with _ ->
      "You tried to read a part of the poem that doesn't exist.\n\
       Please write it yourself..."
  in
  text

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

let big_button ~attr p f =
  let open Nottui in
  let area =
    Ui.mouse_area (fun ~x:_ ~y:_ _ ->
        f ();
        `Handled)
  in
  let raw_image = Notty.I.char attr ' ' 10 2 in
  area @@ Ui.hcat [ Ui.atom raw_image; W.string p ]

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
      Lwd.pure (Ui.space 0 25);
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
          big_button ~attr:(color curr) parens (fun () -> Lwd.set i (f curr)));
      Lwd.pure @@ Ui.space 0 25;
    ]
    |> W.vbox
  in
  button_box

let button_pane =
  let open Nottui in
  let open Notty in
  let level = Lwd.var (Lwd_utils.clampi 2 ~min:0 ~max:5) in
  let canto = Lwd.var (Lwd_utils.clampi 2 ~min:1 ~max:4) in
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
                 let handle_exn = if c == 3 then 4 else c in
                 let text = inner_parens handle_exn i in
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
