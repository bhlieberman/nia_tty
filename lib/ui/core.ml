module W = Nottui_widgets
open Nottui
open Notty

module PoemView = struct
  let spacer length =
    Seq.repeat (Ui.atom (I.vpad 0 10 (I.void 0 0))) |> Seq.take (pred length)

  let text_pane ss =
    let body = List.map (fun s -> W.string ~attr:A.(bg white) s) ss in
    let times = List.length body in
    let spaced =
      List.to_seq body |> Seq.interleave (spacer times) |> List.of_seq
    in
    Lwd_utils.reduce Ui.pack_y spaced

  let show = text_pane (Fs.Bfs.Files.data ())
end
