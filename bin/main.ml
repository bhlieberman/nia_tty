open Nottui
open Nia_tty.Core

module TermInfo = struct
  let term = Notty_unix.Term.create ()

  let term' () = term

  (* Kudos to Darren Li and the docfd library for inspiration here. *)
  let _term_width_height = Lwd.var (0, 0)
end

let () =
  
  let renderer = Renderer.make () in
  Lwd.set base doc;
  let root = Lwd.observe (Lwd.join (Lwd.get base)) in
  Ui_loop.step ~process_event:true ~renderer TermInfo.term root
