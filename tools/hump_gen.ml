(** *)

open Hump_rdf;;

(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)

(*c==v=[String.split_string]=1.1====*)
let split_string ?(keep_empty=false) s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" ->
            if keep_empty then
              "" :: iter "" (pos + 1)
            else
              iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
(*/c==v=[String.split_string]=1.1====*)

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

let exec_select g q =
  let ds = Rdf_ds.simple_dataset g in
  let q =
    List.fold_right
      (fun (name, uri) acc ->
        "PREFIX "^name^": <"^(Rdf_uri.string uri)^">\n"^acc)
       [
         "hump", hump_uri ;
         "dc", dc_uri ; "foaf", foaf_uri ; "rdf", Rdf_rdf.rdf_"" ;
       ]
       q
  in
  try
    let q = Rdf_sparql.parse_from_string q in
    Rdf_sparql.select ~base: base_uri ds q
  with
    Rdf_sparql.Error e ->
      failwith (q^"\n"^(Rdf_sparql.string_of_error e))
;;

let contrib_authors g uri =
  let q = "SELECT ?uri
     WHERE { <"^(Rdf_uri.string uri)^"> dc:creator ?uri .
             ?uri foaf:lastName ?lastname .
             ?uri foaf:firstName ?firstname . }
     ORDER BY DESC(?lastname) DESC(?firstname)"
  in
  let f acc sol =
    match Rdf_sparql.get_term sol "uri" with
      Rdf_term.Uri uri -> uri :: acc
    | _ -> acc
  in
  List.fold_left f [] (exec_select g q)
;;

let author_contribs g uri =
  let q = "SELECT ?uri
     WHERE { ?uri dc:creator <"^(Rdf_uri.string uri)^"> .
             ?uri foaf:name ?name . }
     ORDER BY DESC(?name) "
  in
  let f acc sol =
    match Rdf_sparql.get_term sol "uri" with
      Rdf_term.Uri uri -> uri :: acc
    | _ -> acc
  in
  List.fold_left f [] (exec_select g q)
;;

let literal_obj g uri pred =
  match g.Rdf_graph.objects_of ~sub: (Rdf_term.Uri uri) ~pred with
    (Rdf_term.Literal lit) :: _ -> Some (lit.Rdf_term.lit_value)
  | _ -> None

let name g uri =
  match literal_obj g uri foaf_name with
    Some s -> s
  | None -> invalid_graph "name" uri
;;

let id g uri =
  match literal_obj g uri hump_id with
    Some s -> s
  | None -> invalid_graph "id" uri
;;

let desc g uri = literal_obj g uri dc_desc ;;

let section ?id title xmls =
  let atts =
    (("","title"), title) ::
      (match id with None -> [] | Some id -> [("","id"), id])
  in
  Xtmpl.E (("","section"), atts, xmls)
;;

let div ?id ?cls xmls =
  let atts =
    (match id with None -> [] | Some id -> [("","id"), id]) @
    (match cls with None -> [] | Some l -> [("","class"), String.concat ", " l])
  in
  Xtmpl.E (("","div"), atts, xmls)
;;

let li xmls = Xtmpl.E (("","li"), [], xmls);;

let xml_of_contrib g uri =
  let cname = name g uri in
  let desc =
    match desc g uri with
      None -> []
    | Some s ->
        let l = split_string s ['\n' ; '\r'] in
        List.map (fun s -> Xtmpl.E (("","p"), [], [Xtmpl.D s])) l
  in
  let authors =
    let l = contrib_authors g uri in
    let f uri =
      let hid =
        match List.rev (Rdf_uri.path uri) with
          s1 :: s2 :: _ -> s2 ^ "/" ^ s1
        | _ -> assert false
      in
      li [Xtmpl.E (("","elt"), [("","href"), hid], [])]
    in
    let authors = List.map f l in
    [ section ~id: "authors" "Authors"
      [ Xtmpl.E (("","ul"), [], authors) ]
    ]
  in
  Xtmpl.E (("","contrib"),
   [("","title"), cname ;
    ("","with-contents"), "true" ;
   ],
   [ Xtmpl.E (("","contents"), [], desc @ authors) ]
  )
;;

let xml_of_author g uri =
  let name = name g uri in
  let contribs =
    let l = author_contribs g uri in
    let f uri =
      let hid =
        match List.rev (Rdf_uri.path uri) with
          s1 :: s2 :: _ -> s2 ^ "/" ^ s1
        | _ -> assert false
      in
      li [Xtmpl.E (("","elt"), [("","href"), hid], [])]
    in
    let contribs = List.map f l in
    [ section ~id: "contribs" "Contribs"
      [ Xtmpl.E (("","ul"), [], contribs) ]
    ]
  in
  Xtmpl.E (("","author"),
   [("","title"), name ;
    ("","with-contents"), "true" ;
   ],
   [ Xtmpl.E (("","contents"), [],
        contribs ;
      )
   ]
  )
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

let gen_author outdir g uri =
  let path = Rdf_uri.path uri in
  match List.rev path with
    file :: dir :: _ ->
      let dir = Filename.concat outdir dir in
      mkdir dir;
      let file = Filename.concat dir (file^".html") in
      let xml = xml_of_author g uri in
      file_of_string ~file (Xtmpl.string_of_xml xml)
  | _ -> assert false
;;

let gen_authors outdir g = List.iter (gen_author outdir g) (authors g);;

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
      gen_contribs !outdir g ;
      gen_authors !outdir g
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