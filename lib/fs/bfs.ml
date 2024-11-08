module Files = struct

  let files = Sys.readdir "assets"

  let data () =
    let open Stdlib in
    let par_1 = ref [] in
    let par_2 = ref [] in
    let par_3 = ref [] in
    let par_4 = ref [] in
    let par_5 = ref [] in
    let filenames d =
      Array.map
        (fun s -> Format.sprintf "par_%i.txt" s |> Filename.concat d)
        [| 1; 2; 3; 4; 5 |]
    in
    let disp =
      Array.to_seq files
      |> Seq.filter_map (fun s ->
             if Re.Str.string_partial_match (Re.Str.regexp "canto") s 0 then
               Filename.concat "assets" s |> Unix.realpath |> Option.some
             else None)
      |> Seq.to_dispenser
    in
    let rec run_disp = function
      | Some item ->
          let filenames = filenames item in
          Array.iteri
            (fun idx filename ->
              let contents =
                In_channel.with_open_text filename In_channel.input_all
              in
              match idx with
              | 1 -> par_1 := List.cons contents !par_1
              | 2 -> par_2 := List.cons contents !par_2
              | 3 -> par_3 := List.cons contents !par_3
              | 4 -> par_4 := List.cons contents !par_4
              | 5 -> par_5 := List.cons contents !par_5
              | _ -> ())
            filenames;
          run_disp (disp ())
      | None -> !par_1 @ !par_2 @ !par_3 @ !par_4 @ !par_5
    in
    run_disp (disp ())
end
