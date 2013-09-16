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

(*c==v=[String.lowercase]=1.0====*)
let lowercase s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    let c =
      match s.[i] with
      | 'à' | 'â' | 'ä' -> 'a'
      | 'é' | 'è' | 'ê' | 'ë' -> 'e'
      | 'î' | 'ï' -> 'i'
      | 'ô' | 'ö' -> 'o'
      | 'ù' | 'û' | 'ü' -> 'u'
      | 'ç' -> 'c'
      | c -> Char.lowercase c
    in
    Buffer.add_char b c
  done;
  Buffer.contents b
(*/c==v=[String.lowercase]=1.0====*)



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

let authors g =
  let q = "SELECT distinct ?uri ?lastname ?firstname
     WHERE { _:a dc:creator ?uri .
             ?uri foaf:lastName ?lastname .
             ?uri foaf:firstName ?firstname . }
     ORDER BY DESC(LCASE(?lastname)) DESC(LCASE(?firstname))"
  in
  let f acc sol =
    let lastname =
      try Rdf_sparql.get_string sol "lastname"
      with _ -> ""
    in
    let firstname =
      try Rdf_sparql.get_string sol "firstname"
      with _ -> ""
    in
    match Rdf_sparql.get_term sol "uri" with
      Rdf_term.Uri uri -> (uri, lastname, firstname) :: acc
    | _ -> acc
  in
  List.fold_left f [] (exec_select g q)
;;

let contribs g =
  let q = "SELECT distinct ?uri
     WHERE { ?uri dc:creator _:a .
             ?uri foaf:name ?name .}
     ORDER BY DESC(LCASE(?name))"
  in
  let f acc sol =
    match Rdf_sparql.get_term sol "uri" with
      Rdf_term.Uri uri -> uri :: acc
    | _ -> acc
  in
  List.fold_left f [] (exec_select g q)
;;

let contrib_authors g uri =
  let q = "SELECT distinct ?uri
     WHERE { <"^(Rdf_uri.string uri)^"> dc:creator ?uri .
             ?uri foaf:lastName ?lastname .
             ?uri foaf:firstName ?firstname . }
     ORDER BY DESC(LCASE(?lastname)) DESC(LCASE(?firstname))"
  in
  let f acc sol =
    match Rdf_sparql.get_term sol "uri" with
      Rdf_term.Uri uri -> uri :: acc
    | _ -> acc
  in
  List.fold_left f [] (exec_select g q)
;;

let author_contribs g uri =
  let q = "SELECT distinct ?uri
     WHERE { ?uri dc:creator <"^(Rdf_uri.string uri)^"> .
             ?uri foaf:name ?name . }
     ORDER BY DESC(LCASE(?name)) "
  in
  let f acc sol =
    match Rdf_sparql.get_term sol "uri" with
      Rdf_term.Uri uri -> uri :: acc
    | _ -> acc
  in
  List.fold_left f [] (exec_select g q)
;;

let contrib_licenses g uri =
  let q = "SELECT distinct ?license
     WHERE { <"^(Rdf_uri.string uri)^"> dc:rights ?license }
     ORDER BY DESC(LCASE(?license))"
  in
  let f acc sol =
    let s =
      try Rdf_sparql.get_string sol "license"
      with _ -> Rdf_term.string_of_term (Rdf_sparql.get_term sol "license")
    in
    s :: acc
  in
  List.fold_left f [] (exec_select g q)
;;

let contrib_status g uri =
  let q = "SELECT distinct ?status
     WHERE { <"^(Rdf_uri.string uri)^"> <"^(Rdf_uri.string hump_status)^"> ?status }"
  in
  let f acc sol =
    let uri = Rdf_sparql.get_iri sol hump_uri "status" in
    try
      let pair = List.find (fun (u,_) -> Rdf_uri.compare u uri = 0) Hump_rdf.status_strings in
      (snd pair) :: acc
    with Not_found -> acc
  in
  List.fold_left f [] (exec_select g q)
;;

let contrib_releases g uri =
  let q = "SELECT ?date ?version
     WHERE { <"^(Rdf_uri.string uri)^"> <"^(Rdf_uri.string hump_release)^"> _:rel .
             _:rel dc:date ?date .
             _:rel foaf:name ?version . }
       ORDER BY DESC(?date)"
  in
  let f acc sol =
    let date = Rdf_sparql.get_datetime sol "date" in
    let version = Rdf_sparql.get_string sol "version" in
    (date, version) :: acc
  in
  List.fold_left f [] (exec_select g q)
;;

let contrib_tags g uri =
  let q = "SELECT distinct ?tag
     WHERE { <"^(Rdf_uri.string uri)^"> <"^(Rdf_uri.string hump_tag)^"> ?tag }
     ORDER BY DESC(LCASE(?tag))"
  in
  let f acc sol =
    let s =
      try Rdf_sparql.get_string sol "tag"
      with _ -> Rdf_term.string_of_term (Rdf_sparql.get_term sol "tag")
    in
    s :: acc
  in
  List.fold_left f [] (exec_select g q)
;;

let contrib_kinds g uri =
  let q = "SELECT distinct ?kind
     WHERE { <"^(Rdf_uri.string uri)^"> <"^(Rdf_uri.string hump_kind)^"> ?kind }
     ORDER BY DESC(LCASE(?kind))"
  in
  let f acc sol =
    let s =
      try Rdf_sparql.get_string sol "kind"
      with _ -> Rdf_term.string_of_term (Rdf_sparql.get_term sol "kind")
    in
    s :: acc
  in
  List.fold_left f [] (exec_select g q)
;;

let contrib_homepage g uri =
  let q = "SELECT distinct ?uri
     WHERE { <"^(Rdf_uri.string uri)^"> foaf:homepage ?uri .
           FILTER (ISURI(?uri)) }"
  in
  match exec_select g q with
    [] -> None
  | sol :: _ -> Some (Rdf_sparql.get_iri sol hump_uri "uri")
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

let xml_info_table ?id rows =
  let f_row (xml1, xml2) =
    Xtmpl.E (("","tr"),[],
     [
       Xtmpl.E (("","th"),[],xml1) ;
       Xtmpl.E (("","td"),[],xml2) ;
     ])
  in
  let atts =
    let att_id =
      match id with None -> [] | Some d -> [("","id"), d]
    in
    (("","class"), "table") :: att_id
  in
  Xtmpl.E
    (("","table"), atts,
     [
      Xtmpl.E (("","tbody"),[], List.map f_row rows)
     ])
;;

let concat sep l =
  let rec iter acc = function
    [] -> List.rev acc
  | [x] -> iter (x :: acc) []
  | h :: q -> iter (sep :: h :: acc) q
  in
  iter [] l
;;

let string_of_date d = Netdate.format ~fmt: "%b %e, %Y" d;;

let first_sentence s =
  let flags = [`UTF8 ; `MULTILINE] in
  let pat = "\\.[ \\n\\r]" in
  let rex = Pcre.regexp ~flags pat in
  try
     match Pcre.pcre_exec ~rex ~pos: 0 s with
       [| |] -> assert false
     | t ->
        let p = t.(0) in
        let s1 = String.sub s 0 (p+1) in
        let len = String.length s in
        let s2 = String.sub s (p+1) (len - p - 1) in
        (s1, s2)
  with Not_found ->
    (s, "")
;;

let xml_of_contrib g uri =
  let cname = name g uri in
  let desc =
    match desc g uri with
      None -> []
    | Some s ->
        match first_sentence s with
          (s1, s2) ->
            [ Xtmpl.E (("","div"), [], [Xtmpl.D s1]) ;
                Xtmpl.E (("","sep_"), [], []) ;
              Xtmpl.E (("","p"), [], [Xtmpl.D s2]) ;
            ]
              (*let l = split_string s ['\n' ; '\r'] in
                 List.map (fun s -> Xtmpl.E (("","p"), [], [Xtmpl.D s])) l*)
  in
  let authors =
    let l = contrib_authors g uri in
    let f uri =
      let hid =
        match List.rev (Rdf_uri.path uri) with
          s1 :: s2 :: _ -> s2 ^ "/" ^ s1
        | _ -> assert false
      in
      Xtmpl.E (("","elt"), [("","href"), hid], [])
    in
    let authors = List.map f l in
    let authors = concat (Xtmpl.D ", ") authors in
    ([Xtmpl.D "Authors"], authors)
  in
  let homepage =
    let xml = match contrib_homepage g uri with
        None -> []
      | Some uri ->
          let uri_s = Rdf_uri.string uri in
          [Xtmpl.E (("","a"), [("","escamp_"), "href" ; ("","href"), uri_s],
           [Xtmpl.E (("", "code"), [], [Xtmpl.D uri_s])]
            )
          ]
    in
    ([Xtmpl.D "Homepage"], xml)
  in
  let (date, last_rel) =
    let l = contrib_releases g uri in
    let (date, xml) =
      match l with
        [] -> (None, [])
      | (date, version) :: _ ->
          let d = Netdate.format ~fmt: "%Y/%m/%d" date in
          (Some d, [ Xtmpl.D (version ^ " ("^(string_of_date date)^")")])
    in
    (date, ([Xtmpl.D "Last release"], xml))
  in
  let status =
    let xml = concat (Xtmpl.D ", ")
      (List.map (fun s -> Xtmpl.D s)
       (contrib_status g uri))
    in
    ([Xtmpl.D "Status"], xml)
  in
  let licenses =
    let xml = concat (Xtmpl.D ", ")
      (List.map (fun s -> Xtmpl.D s)
       (contrib_licenses g uri))
    in
    ([Xtmpl.D "License(s)"], xml)
  in
  let tags =
    let xml = [ Xtmpl.E (("","elt-keywords"), [("","sep"), "<main_/>"], []) ] in
    (*    let xml = List.map
       (fun s -> Xtmpl.E (("","div"), [("","class"), "tag"], [Xtmpl.D s]))
       (contrib_tags g uri)
       in
       *)
    ([Xtmpl.D "Tag(s)"], xml)
  in
  let kinds =
    let xml =  [ Xtmpl.E (("","elt-topics"), [("","sep"), "<main_/>"], []) ] in
    (*List.map
      (fun s -> Xtmpl.E (("","div"), [("","class"), "kind"], [Xtmpl.D s]))
        (contrib_kinds g uri)
    in*)
    ([Xtmpl.D "Kind(s)"], xml)
  in

  let info = xml_info_table [
      homepage ;
      last_rel ;
      authors ;
      kinds ;
      status ;
      licenses ;
      tags ;
    ]
  in
  Xtmpl.E (("","contrib"),
   [("","title"), cname ;
    ("","keywords"), String.concat ", " (contrib_tags g uri) ;
     ("","topics"), String.concat ", " (contrib_kinds g uri) ;
     ("","with-contents"), "true" ;
   ]@ (match date with None -> [] | Some d -> [("","date"), d] ),
   [ Xtmpl.E (("","contents"), [], desc @ [info]) ]
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

module SMap = Map.Make(String);;

let xml_index list =
  let group (name, hid) map =
    if String.length name > 0 then
      begin
        let n = Rdf_utf8.utf8_nb_bytes_of_char name.[0] in
        let id = String.capitalize (lowercase (String.sub name 0 n)) in
        let l =
          try SMap.find id map
          with Not_found -> []
        in
        SMap.add id ((name, hid)::l) map
      end
    else
      map
  in
  (* fold right to keep order of list after insertion in groups *)
  let groups = List.fold_right group list SMap.empty in

  let links = SMap.fold
    (fun id _ acc ->
      (Xtmpl.D " - ") ::
      (Xtmpl.E (("","elt"), [("","href"), "#"^id], [Xtmpl.D id])) ::
      acc

    )
    groups [Xtmpl.D " - "]
  in

  let f_elt (name, hid) =
    li [Xtmpl.E (("","elt"), [("","href"), hid], [Xtmpl.D name])]
  in
  let f_group id list acc =
    (Xtmpl.E (("","section"), [("","id"), id ; ("","title"), id],
      [ Xtmpl.E (("","ul"),[], List.map f_elt list)])
    ) :: acc
  in
  (Xtmpl.E (("","section"),[], List.rev links)) ::
   (List.rev (SMap.fold f_group groups []))
;;

let gen_contrib outdir g uri =
  let path = Rdf_uri.path uri in
  match List.rev path with
    file :: dir :: _ ->
      let dir = Filename.concat outdir dir in
      let file = Filename.concat dir (file^".html") in
      let xml = xml_of_contrib g uri in
      file_of_string ~file (Xtmpl.string_of_xml xml)
  | _ -> assert false
;;

let gen_contrib_index outdir g =
  let contribs = contribs g in
  let contribs =
    let f uri =
      let hid =
        match List.rev (Rdf_uri.path uri) with
          s1 :: s2 :: _ -> s2 ^ "/" ^ s1
        | _ -> assert false
      in
      (name g uri, hid)
    in
    xml_index (List.map f contribs)
  in
  let xml =
    Xtmpl.E (("","page"),
     [("","title"), "Contribs" ],
     contribs
    )
  in
  let dir = Filename.concat outdir "contribs" in
  mkdir dir ;
  let file = Filename.concat dir "index.html" in
  file_of_string ~file (Xtmpl.string_of_xml xml)
;;

let gen_contribs outdir g =
  gen_contrib_index outdir g ;
  List.iter (gen_contrib outdir g) (contribs g)
;;

let gen_author outdir g (uri, _, _) =
  let path = Rdf_uri.path uri in
  match List.rev path with
    file :: dir :: _ ->
      let dir = Filename.concat outdir dir in
      let file = Filename.concat dir (file^".html") in
      let xml = xml_of_author g uri in
      file_of_string ~file (Xtmpl.string_of_xml xml)
  | _ -> assert false
;;

let gen_author_index outdir g =
  let authors = authors g in
  let authors =
    let f (uri, name, firstname) =
      let hid =
        match List.rev (Rdf_uri.path uri) with
          s1 :: s2 :: _ -> s2 ^ "/" ^ s1
        | _ -> assert false
      in
      (name^" "^firstname, hid)
    in
    xml_index (List.map f authors)
  in
  let xml =
    Xtmpl.E (("","page"),
     [("","title"), "Authors" ],
     authors
    )
  in
  let dir = Filename.concat outdir "authors" in
  mkdir dir ;
  let file = Filename.concat dir "index.html" in
  file_of_string ~file (Xtmpl.string_of_xml xml)
;;

let gen_authors outdir g =
  gen_author_index outdir g ;
  List.iter (gen_author outdir g) (authors g)
;;

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