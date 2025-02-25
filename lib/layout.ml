module W = Nottui_widgets

let borders children =
  let open Nottui in
  let open Notty.I in
  let open Notty.A in
  let top = char (bg white ++ fg red) '*' 100 1 <-> vpad 20 19 (void 40 0) in
  let bottom = char (bg white ++ fg red) '*' 100 1 in
  let left = char (bg white ++ fg red) '*' 1 40 <|> hpad 40 19 (void 40 0) in
  let right = char (bg white ++ fg red) '*' 1 40 <|> hpad 40 19 (void 40 0) in
  let sides = left <|> right in
  let y_sides = top <-> bottom in
  Lwd.map
    ~f:(fun children ->
      let joined =
        Ui.zcat
          [
            Ui.hcat
              [
                Ui.atom (hpad 5 5 (void 0 0));
                children;
                Ui.atom (hpad 0 5 (void 0 0));
              ];
            Ui.atom sides;
            Ui.atom y_sides;
          ]
      in
      W.scroll_area @@ Lwd.pure joined)
    children

let frame = borders Poem.button_pane
