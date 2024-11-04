module Files = struct
  open Dolog
  open Re

  let assets = "/home/slothrop/dev/ocaml/nia_tty/assets"

  let dirs =
    Sys.readdir assets |> Array.to_list
    |> List.filter (fun x -> Filename.concat assets x |> Sys.is_directory)

  let is_dir s =
    let r = Re.Str.regexp "canto" in
    Re.Str.string_match r s 40

  let bfs dirs =
    (* make a queue for holding remaining files *)
    let remaining = Queue.of_seq dirs in
    (* track the text already processed *)
    let buf = ref [] in
    Log.info "beginning BFS";
    let rec aux queue =
      (* for correctly resolving file paths *)
      let current_item = ref (Queue.peek remaining) in
      if is_dir !current_item then
        current_item := Filename.concat assets !current_item
      else ();
      Log.info "current directory: %s" !current_item;
      let item = try Queue.take queue with Queue.Empty -> "" in
      try
        if Sys.is_directory !current_item then
          Queue.add_seq queue (Array.to_seq (Sys.readdir item))
        else if Sys.is_regular_file (Filename.concat !current_item item) then
          let text =
            In_channel.with_open_text item (fun ic -> In_channel.input_lines ic)
          in
          buf := List.append !buf text
        else ()
      with exn ->
        Log.error "Exception raised: %s" (Printexc.to_string exn);
        aux queue
    in
    aux remaining;
    !buf

  (** Breadth-first search of assets directory *)
  let bfs_two () =
    (* Steps:
       1. Check working directory
       2. Read top-level directory
       3. Make a queue
       4. Create recursive function to iterate through queue
         4a. Check if head of queue is a directory
         4b. If it is, put the contents on the queue (after path resolution)
         4c. If not, read the file and put it in the buffer
         4d. Recur
    *)
    Log.set_log_level Log.INFO;
    Sys.chdir (Unix.realpath assets);
    (* Ensure 'assets' is defined correctly *)
    let start_dir = Filename.current_dir_name |> Unix.realpath in
    let files = Sys.readdir start_dir |> Array.to_seq |> Queue.of_seq in
    let buf = ref [] in

    let rec aux () =
      if Queue.is_empty files then ()
      else
        let item = Queue.take files in
        let full_path = Filename.concat start_dir item in

        if Sys.is_directory full_path then (
          (* Log.info "Entering directory: %s" full_path; *)
          let subfiles =
            let fs = Sys.readdir full_path in
            (* qualify the paths *)
            fs
            |> Array.to_seq
          in
          (* add the files on to the queue *)
          Queue.add_seq files subfiles;
          aux () (* Recur with updated queue *))
        else if Sys.is_regular_file item && Filename.extension item <> ".gif" then (
          Log.info "Processing file: %s" full_path;
          try
            let text =
              In_channel.with_open_text full_path In_channel.input_lines
            in
            Log.info "Read file: %s" full_path;
            buf := List.append !buf text;
            aux ()
          with exn ->
            Log.error "Error processing file %s: %s" full_path
              (Printexc.to_string exn);
            aux ())
        else aux ()
    in

    aux ();
    !buf

  let _ = bfs_two ()

  let run () =
    Sys.chdir assets;
    try bfs (List.to_seq dirs)
    with exn ->
      Log.error "Exception raised: %s" (Printexc.to_string exn);
      []
end

open Dolog
open Files
