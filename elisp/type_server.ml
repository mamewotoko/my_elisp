(************************************************************
   type_server.ml	Created      : Tue Aug 12 03:47:08 2003
  			Last modified: Thu Jan 08 02:52:20 2004
  Compile: ../../../ocamlcomp.sh -custom -I ../../unix/ -I ../../str/ -I ../../../parsing -I ../../../typing -I ../../../utils search_type.cma list2.ml searchid.ml search_type.ml str.cma ../../str/libstr.a type_server.ml -o type_server #
  Compile: ocamlc -i -I ../../../parsing -I ../../../typing -I ../../../utils unix.cma toplevellib.cma list2.ml searchid.ml search_type.ml str.cma type_server.ml -o type_server #
  FTP Directory: sources/ocaml #
************************************************************)
(**

  @author Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
  otherlibs/labltk/browser において上にかいたコマンドラインでコンパイルする

  SEARCH\tint\tExact
  SEARCH\tint\tIncluded
  SEARCHVALUE\tint\tExact
  SEARCHTYPE\tint\tExact

  __CommandError
  Unix.read
  ....
  __END

*)
open Env
open Types

type annotated_result = Value of (string * string) | Type of string
type search_mode = ALL | TYPE | VALUE

let delimiter = Str.regexp "\t"
let newline_regexp = Str.regexp "\n"
let annotation_mode = true


let mode_str2mode s =
  if s = "Exact" then `Exact
  else if s = "Included" then `Included
  else raise Exit

let add_type_annotation env id =
  let idstr = Search_type.longident_to_string id in
  try 
    let (_, v) = Env.lookup_value id env in
    let t = v.val_type in
    let tstr = 
      begin
	Printtyp.type_expr Format.str_formatter t;
	Format.flush_str_formatter ()
      end in
    Value(idstr, tstr)
  with Not_found ->
    Type(idstr)
;;

let rec iter () =
  let command = read_line () in
  let commandlst = Str.split delimiter command in
  (match commandlst with
    searchmode:: t_str :: modeopt ->
      (try 
	let search_mode = if searchmode = "SEARCHTYPE" then TYPE else if searchmode = "SEARCHVALUE" then VALUE else ALL in
	let mode = match modeopt with [mode_str] -> mode_str2mode mode_str | _ -> `Included in
	let lst = Searchid.search_string_type t_str mode in
	print_endline "__START";
	List.iter (fun (x, _) ->
	  if annotation_mode then
	    let type_annotation = add_type_annotation !Searchid.start_env x in
	    match (search_mode, type_annotation) with
	      (TYPE, Type(typename))
	    | (ALL, Type(typename)) -> print_endline ("__RESULT\t"^typename)
	    | (VALUE, Value(valuename, typestr))
	    | (ALL, Value(valuename, typestr)) -> 
		let onelinetypestr = Str.global_replace newline_regexp " " typestr in
		print_endline ("__RESULT\t"^(valuename ^ " : " ^ onelinetypestr))
	    | _ -> ()
	  else 
	    print_endline ("__RESULT\t"^(Search_type.longident_to_string x))) lst;
	print_endline "__END"
      with e -> print_endline ("__SEARCH_ERROR\t"^Printexc.to_string e))
  | _ -> print_endline "__COMMAND_ERROR");
  iter ()
;;

let _ =
  Search_type.init (); 
  try 
    Printexc.print iter ()
  with End_of_file -> exit 0
;;
