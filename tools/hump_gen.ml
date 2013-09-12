(** *)

open Hump_rdf;;

(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)

let invalid_graph f uri =
  let msg = "invalid graph detected in "^f^" "^(Rdf_uri.string uri) in
  failwith msg
;;

let contribs g =
  let l = g.Rdf_graph.subjects_of
    ~pred: Rdf_rdf.rdf_type ~obj: (Rdf_term.Uri hump_contrib)
  in
  List.map
    (function Rdf_term.Uri uri -> uri | _ -> assert false)
    l
;;

let authors g =
  let l = g.Rdf_graph.subjects_of
    ~pred: Rdf_rdf.rdf_type ~obj: (Rdf_term.Uri hump_author)
  in
  List.map
    (function Rdf_term.Uri uri -> uri | _ -> assert false)
    l
;;

let name g uri =
  match g.Rdf_graph.objects_of ~sub: (Rdf_term.Uri uri) ~pred: foaf_name with
    (Rdf_term.Literal lit) :: _ -> lit.Rdf_term.lit_value
  | _ -> invalid_graph "name" uri
;;

let id g uri =
  match g.Rdf_graph.objects_of ~sub: (Rdf_term.Uri uri) ~pred: hump_id with
    (Rdf_term.Literal lit) :: _ -> lit.Rdf_term.lit_value
  | _ -> invalid_graph "id" uri
;;

let xml_of_contrib g uri =
  let cname = name g uri in
  Xtmpl.E (("","contrib"), [("","title"), cname], [])
;;

let mkdir s =
  let com = "mkdir -p "^(Filename.quote s) in
  match Sys.command com with
    0 -> ()
  | n -> failwith ("Command failed: "^com)
;;

let gen_contrib outdir g uri =
  let path = Rdf_uri.path uri in
  match List.rev path with
    file :: dir :: _ ->
      let dir = Filename.concat outdir dir in
      mkdir dir;
      let file = Filename.concat dir (file^".html") in
      let xml = xml_of_contrib g uri in
      file_of_string ~file (Xtmpl.string_of_xml xml)
  | _ -> assert false
;;

let gen_contribs outdir g = List.iter (gen_contrib outdir g) (contribs g);;

let main () =
  let outdir = ref Filename.current_dir_name in
  let options = [
      "-o", Arg.Set_string outdir, "<dir> set output directory; default is "^(!outdir) ;
    ]
  in
  let files = ref [] in
  Arg.parse options (fun f -> files := f :: !files)
    (Printf.sprintf "Usage: %s [options]\nwhere options are:" Sys.argv.(0));
  match List.rev !files with
    [] -> failwith "Please give at least one turtle file"
  | files ->
      let g = Rdf_graph.open_graph base_uri in
      List.iter (fun file -> ignore(Rdf_ttl.from_file g base_uri file)) files;
      gen_contribs !outdir g
;;

(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)

let () = safe_main main;;