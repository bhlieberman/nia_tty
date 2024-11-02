module W = Nottui_widgets

let borders children =
  let open Nottui in
  let open Notty.I in
  let open Notty.A in
  let top = char (bg white ++ fg red) '*' 100 1 in
  let left = char (bg white ++ fg red) '*' 1 40 in
  let right = char (bg white ++ fg red) '*' 1 40 in
  let inner =
    Lwd.map
      ~f:(fun children ->
        let joined =
          Ui.zcat
            [ Ui.atom top;
              Ui.hcat
                [
                  Ui.atom left;
                  Ui.atom (hpad 5 5 (void 39 0));
                  Ui.atom (hpad 5 5 (void 39 0));
                  Ui.atom right;
                ];
                children;
              Ui.atom top;
            ]
        in
        Lwd.pure joined)
      children
  in
  Lwd.join inner

let frame = borders Poem.button_pane
