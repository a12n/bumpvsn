(*
Copyright © 2017 Anton Yabchinskiy

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

open Batteries

module Vsn = struct
  type t = int * int * int
  type part = Major | Minor | Patch

  let bump (a, b, c) = function
    | Major -> (a + 1, 0, 0)
    | Minor -> (a, b + 1, 0)
    | Patch -> (a, b, c + 1)

  let part_of_string = function
    | "major" | "maj" | "M" -> Major
    | "minor" | "min" | "m" -> Minor
    | "patch" | "pat" | "p" -> Patch
    | "micro" | "mic" | "u" -> Patch
    | invalid -> failwith ("Invalid version part \"" ^ invalid ^ "\"")

  let to_string (a, b, c) = Printf.sprintf "%d.%d.%d" a b c
end

module Re_ext = struct
  include Re

  let dot = char '.'
  let no_case_str = no_case % str
  let non_neg_int = rep1 digit
  let vsn_num_part = group non_neg_int
  let vsn_num = seq [bow; vsn_num_part; dot; vsn_num_part; dot; vsn_num_part; eow]

  let get_group = Group.get
  let get_int_group groups i = int_of_string (get_group groups i)
end

(* ---- *)

let bump ~extract ~replace filename part =
  let old_text = input_file filename in
  let old_vsn, state = extract old_text in
  let vsn = Vsn.bump old_vsn part in
  let text = replace old_text state vsn in
  output_file ~filename ~text;
  Printf.printf "%s\t%s\t%s\n" filename
    (Vsn.to_string old_vsn) (Vsn.to_string vsn)

(* ---- *)

let plain_bump ?(filename="vsn") =
  let re = Re_ext.(vsn_num |> compile) in
  let extract text =
    let grps = Re_ext.exec re text in
    Re_ext.(get_int_group grps 1,
            get_int_group grps 2,
            get_int_group grps 3),
    () in
  let replace text () vsn =
    Re_ext.replace_string re (Vsn.to_string vsn) text in
  bump ~extract ~replace filename

(* ---- *)

let rebar_bump part =
  let re = Re_ext.(seq [wordc; str ".app.src"; eos] |> compile) in
  let basename = Array.find (Re_ext.execp re) (Sys.readdir "src") in
  let filename = Filename.concat "src" basename in
  plain_bump ~filename part

(* ---- *)

let cmake_bump =
  let set_cmd_re var_re =
    Re_ext.(seq [ bow;
                  no_case_str "set"; rep blank; char '(';
                  group var_re;
                  rep1 blank;
                  group non_neg_int;
                  char ')' ] |>
            compile) in
  let extract text =
    (* TODO: [@warning "-8"] *)
    let [a_var, a; b_var, b; c_var, c] =
      List.map (fun part_str ->
          let re = set_cmd_re Re_ext.(seq [ rep wordc;
                                            no_case_str part_str;
                                            rep wordc ]) in
          let grps = Re_ext.exec re text in
          Re_ext.(get_group grps 1, get_int_group grps 2)
        ) ["major"; "minor"; "patch"] in
    (a, b, c), (a_var, b_var, c_var) in
  let replace text (a_var, b_var, c_var) (a, b, c) =
    List.fold_left
      (fun text (var_name, n) ->
         let re = set_cmd_re Re_ext.(str var_name) in
         Re_ext.replace_string re
           (Printf.sprintf "set(%s %d)" var_name n) text
      ) text [a_var, a; b_var, b; c_var, c] in
  bump ~extract ~replace "CMakeLists.txt"

(* ---- *)

let part_from_argv argv =
  try Vsn.part_of_string argv.(1)
  with (Invalid_argument _) -> (
      let _, part =
        List.find_exn
          (fun (part_str, _) ->
             let re = Re_ext.(no_case_str part_str |> compile) in
             Re_ext.execp re argv.(0))
          (Failure "Don't know which part to bump")
          [ "major", Vsn.Major;
            "minor", Vsn.Minor;
            "patch", Vsn.Patch ] in
      part
    )

let () =
  let bump_funs = [
    cmake_bump;
    plain_bump;
    plain_bump ~filename:"META";
    plain_bump ~filename:"opam";
    plain_bump ~filename:"bar-descriptor.xml";
    rebar_bump;
  ] in
  let part = part_from_argv Sys.argv in
  let try_bump f =
    try f part; true
    with _ -> false in
  let bumped = List.(map try_bump bump_funs |> reduce (||)) in
  if not bumped then exit 1
