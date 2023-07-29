open Base
open Js_of_ocaml

let parse_wordlist words =
  String.split_lines words |> List.map ~f:String.strip
  |> List.map ~f:String.lowercase

let rec all_substrings word len =
  if String.length word < len then []
  else String.prefix word len :: all_substrings (String.drop_prefix word 1) len

let most_common_substring words sub_len num_elems =
  let all_counts = Hashtbl.create (module String) in
  List.iter words ~f:(fun word ->
      all_substrings word sub_len
      |> List.iter ~f:(fun s ->
             Hashtbl.update all_counts s ~f:(function
               | None -> 1
               | Some count -> count + 1)));
  List.take
    (Hashtbl.to_alist all_counts
    |> List.sort ~compare:(fun (_, a) (_, b) -> Int.descending a b)
    |> List.map ~f:(fun (s, count) -> Printf.sprintf "%s: %d" s count))
    num_elems
  |> String.concat ~sep:"\n"

let () =
  Js.Unsafe.global##.parseWordList := Js.wrap_callback parse_wordlist;
  Js.Unsafe.global##.mostCommonSubstrings
  := Js.wrap_callback most_common_substring
