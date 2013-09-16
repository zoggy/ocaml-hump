
module SQL = Mysql;;

(** UTF-8 stuff *)

let utf8 s =
  let module C = CamomileLibraryDefault.Camomile.CharEncoding in
  C.recode_string
    ~in_enc: C.latin1 ~out_enc: C.utf8 s
;;

(*c==v=[String.replace_in_string]=1.0====*)
let replace_in_string ~pat ~subs ~s =
  let len_pat = String.length pat in
  let len = String.length s in
  let b = Buffer.create len in
  let rec iter pos =
    if pos >= len then
      ()
    else
      if pos + len_pat > len then
        Buffer.add_string b (String.sub s pos (len - pos))
      else
        if String.sub s pos len_pat = pat then
          (
           Buffer.add_string b subs;
           iter (pos+len_pat)
          )
        else
          (
           Buffer.add_char b s.[pos];
           iter (pos+1);
          )
  in
  iter 0;
  Buffer.contents b
(*/c==v=[String.replace_in_string]=1.0====*)


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

(*c==v=[String.strip_string]=1.0====*)
let strip_string s =
  let len = String.length s in
  let rec iter_first n =
    if n >= len then
      None
    else
      match s.[n] with
        ' ' | '\t' | '\n' | '\r' -> iter_first (n+1)
      | _ -> Some n
  in
  match iter_first 0 with
    None -> ""
  | Some first ->
      let rec iter_last n =
        if n <= first then
          None
        else
          match s.[n] with
            ' ' | '\t' | '\n' | '\r' -> iter_last (n-1)
          |	_ -> Some n
      in
      match iter_last (len-1) with
        None -> String.sub s first 1
      |	Some last -> String.sub s first ((last-first)+1)
(*/c==v=[String.strip_string]=1.0====*)

(** Connection *)

let _login =
  try Unix.getenv "USER"
  with Not_found ->
    try Filename.basename (Unix.getenv "HOME")
    with Not_found -> ""

let ref_db = ref None

let get_db () =
  match !ref_db with
    None -> failwith "Not connected"
  | Some connection -> connection
;;

let db_name = ref "hump"
let db_user = ref _login
let db_passwd = ref None
let db_host = ref "localhost"
let db_port = ref None

let connect () =
  let db = {
    SQL.dbhost = Some !db_host ;
    dbname = Some !db_name ;
    dbpwd = !db_passwd ;
    dbport = !db_port;
    dbuser = Some !db_user;
    dbsocket = None ;
    }
  in
  try ref_db := Some (SQL.connect db)
  with SQL.Error s ->
    ref_db := None ; prerr_endline s
;;

let connected_to_db () = !ref_db <> None

(** Queries *)


type release = { rel_version : string ; rel_date : float ; }
type status = Devcode | Alpha | Beta | Stable | Mature

let unopt_string = function None -> "" | Some s -> s;;
let unopt_int = function None -> failwith "None instead of int" | Some n -> int_of_string n;;
let unopt_float = function None -> failwith "None instead of float" | Some n -> float_of_string n;;
let map_opt f = function None -> None | Some v -> Some (f v);;

let get_contribs db =
  let result =
    SQL.exec db
    "select description, id, name, url, version, status, date from contribs"
  in
  let f a =
    match a with
      [| desc ; id ; name ; url ; version ; status; date |] ->
        begin
          (unopt_int id,
           unopt_string name,
           unopt_string desc,
           unopt_string url,
           unopt_string version,
           unopt_float date
          )
        end
    | _ -> failwith "Bad result in get_contribs"
  in
  SQL.map result f
;;

let get_authors db =
  let result =
    SQL.exec db
    "select id, name, firstname, url from authors"
  in
  let f a =
    match a with
      [| id ; name ; firstname ; url |] ->
        begin
          (unopt_int id,
           strip_string (unopt_string name),
           strip_string (unopt_string firstname),
           map_opt strip_string url
          )
        end
    | _ -> failwith "Bad result in get_authors"
  in
  SQL.map result f
;;

let get_prop db id =
  let result = SQL.exec db
    (Printf.sprintf "select kind, name from props where id=%d" id)
  in
  let f a =
    match a with
      [| idkind ; name |] ->
        (unopt_int idkind, unopt_string name)
    | _ -> failwith "Bad result in get_prop"
  in
  match SQL.map result f with
    x :: _ -> x
  | [] -> assert false
;;

let get_contrib_props db =
  let result =
    SQL.exec db
    "select idcontrib, idprop from contrib_prop"
  in
  let f a =
    match a with
      [| idcontrib ; idprop |] ->
        (unopt_int idcontrib, get_prop db (unopt_int idprop))
    | _ -> failwith "Bad result in get_contrib_props"
  in
  SQL.map result f
;;

let get_contrib_authors db idcontrib =
  let result =
    SQL.exec db
    ("select idauthor from contrib_author where idcontrib="^(string_of_int idcontrib))
  in
  let f a =
    match a with
      [| idauthor |] -> unopt_int idauthor
    | _ -> failwith "Bad result in get_contrib_authors"
  in
  SQL.map result f
;;

let get_contrib_license db idcontrib =
  let result =
    SQL.exec db
      ("select name from contrib_prop as C, props as P \
        where P.kind=1 and P.id=C.idprop and C.idcontrib="^(string_of_int idcontrib))
  in
  let f a =
    match a with
      [| name |] -> unopt_string name
    | _ -> failwith "Bad result in get_contrib_license"
  in
  match SQL.map result f with
    [] -> None
  | l :: _ -> Some l
;;

let get_contrib_status db idcontrib =
  let result =
    SQL.exec db
      ("select status from contribs where id="^(string_of_int idcontrib))
  in
  let f a =
    match a with
      [| None |] -> None
    | [| Some s |] ->
        begin
          match int_of_string s with
            0 -> Some Devcode
          | 1 -> Some Alpha
          | 2 -> Some Beta
          | 3 -> Some Stable
          | 4 -> Some Mature
          | _ -> None
        end
    | _ -> failwith "Bad result in get_contrib_status"
  in
  match SQL.map result f with
    [] -> None
  | l :: _ -> l
;;


type author =
  {
    aut_label : string ;
    aut_name : string ;
    aut_firstname : string ;
    aut_id : int ;
    aut_home : Rdf_uri.uri option ;
  }


type contrib =
  {
    c_label : string ;
    c_name : string ;
    c_desc : string ;
    c_authors : author list ;
    c_home : Rdf_uri.uri ;
    c_releases : release list ;
    c_license : string option;
    c_status : status option;
    c_tags : string list ;
    c_kinds : string list ;
  }

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

module IMap = Map.Make (struct type t = int let compare = Pervasives.compare end)

let topics ?(kind=2) db =
  let res = SQL.exec db
    ("select P.id, P.name, A.idfather \
     from props as P, assoc_prop as A \
     where P.kind="^(string_of_int kind)^
     " and P.id<>"^(string_of_int kind)^" and A.idchild=P.id")
  in
  let f acc = function
    [| idprop ; name ; idfather |] ->
      let idprop = unopt_int idprop in
      let name = unopt_string name in
      let idfather =
        match unopt_int idfather with
          n when n=kind -> None
        | n -> Some n
      in
      IMap.add idprop (name, idfather) acc
  | _ -> failwith "Bad result in topics"
  in
  let l = SQL.map res (fun r -> r) in
  List.fold_left f IMap.empty l
;;

let kinds db = topics ~kind: 0 db ;;

let get_tag_fullname topics id =
  let rec iter acc id =
    try
      let (name, father) = IMap.find id topics in
      let name = utf8 (lowercase name) in
      match father with
        None -> name :: acc
      | Some id -> iter (name::acc) id
    with Not_found -> acc
  in
  String.concat "::" (iter [] id)
;;

let get_contrib_tags db topics id =
  let result =
    SQL.exec db
      ("select idprop from contrib_prop as C, props as P \
        where P.kind=2 and P.id=C.idprop and C.idcontrib="^(string_of_int id))
  in
  let f a =
    match a with
      [| idprop |] -> unopt_int idprop
    | _ -> failwith "Bad result in get_contrib_tags"
  in
  let idprops = SQL.map result f in
  List.map (get_tag_fullname topics) idprops
;;

let get_contrib_kinds db kinds id =
  let result =
    SQL.exec db
      ("select idprop from contrib_prop as C, props as P \
        where P.kind=0 and P.id=C.idprop and C.idcontrib="^(string_of_int id))
  in
  let f a =
    match a with
      [| idprop |] -> unopt_int idprop
    | _ -> failwith "Bad result in get_contrib_kinds"
  in
  let idprops = SQL.map result f in
  List.map (get_tag_fullname kinds) idprops
;;

let mk_label s =
  let s = lowercase (String.concat "-" (split_string s [' '])) in
  let s = List.fold_left
    (fun s (pat, subs) -> replace_in_string ~pat ~subs ~s)
    s [ "#", "sharp" ]
  in
  utf8 s
;;

let authors db =
  let f (id, name, firstname, url) =
    let label =
      let s = name ^ " " ^ firstname in
      mk_label s
    in
    { aut_name = utf8 name ; aut_firstname = utf8 firstname ;
      aut_id = id ; aut_home = map_opt (fun s -> Rdf_uri.uri (utf8 s)) url ;
      aut_label = label ;
    }
  in
  List.map f (get_authors db)

let contribs db authors topics kinds =
  let f (id, name, desc, url, version, date) =
    {
      c_label = mk_label name ;
      c_name = utf8 name ;
      c_desc = utf8 desc ;
      c_authors = List.map (fun id -> IMap.find id authors) (get_contrib_authors db id) ;
      c_home = Rdf_uri.uri (utf8 (strip_string url)) ;
      c_releases = [ { rel_version = version ; rel_date = date } ] ;
      c_license = get_contrib_license db id ;
      c_status = get_contrib_status db id ;
      c_tags = get_contrib_tags db topics id ;
      c_kinds = get_contrib_kinds db kinds id ;
    }

  in
  List.map f (get_contribs db)
;;

(** File generation *)

open Hump_rdf;;


let status_uri = function
  Devcode -> hump_devcode
| Alpha -> hump_alpha
| Beta -> hump_beta
| Stable -> hump_stable
| Mature -> hump_mature
;;

let gen_ttl g file =
  let tmp = Filename.temp_file "importhump" ".ttl" in
  g.Rdf_graph.set_namespaces [
    dc_uri, "dc" ;
    foaf_uri, "foaf" ; hump_uri, "hump" ;
    authors_uri, "authors" ; contribs_uri, "contribs" ;
  ];
  Rdf_ttl.to_file g tmp ;
  let com = "grep -v '@prefix' "^(Filename.quote tmp)^" > "^(Filename.quote file) in
  match Sys.command com with
    0 -> Sys.remove tmp
  | n ->  failwith ("Command failed: "^com)
;;

let author_file outdir a =
  Filename.concat outdir (Filename.concat "authors" (a.aut_label^".ttl"));;

let contrib_file outdir c =
  Filename.concat outdir (Filename.concat "contribs" ((lowercase c.c_name)^".ttl"));;

let gen_author outdir _ a =
  let g = Rdf_graph.open_graph base_uri in
  let sub = Rdf_term.Uri (author_uri_ a.aut_label) in
  g.Rdf_graph.add_triple ~sub ~pred: hump_id ~obj: (lit (a.aut_label));
  g.Rdf_graph.add_triple ~sub ~pred: Rdf_rdf.rdf_type ~obj: (Rdf_term.Uri hump_author) ;
  g.Rdf_graph.add_triple ~sub ~pred: foaf_name ~obj: (lit (a.aut_firstname^" "^a.aut_name)) ;
  g.Rdf_graph.add_triple ~sub ~pred: foaf_lastname ~obj: (lit a.aut_name) ;
  g.Rdf_graph.add_triple ~sub ~pred: foaf_firstname ~obj: (lit a.aut_firstname) ;
  (
   match a.aut_home with
     None -> ()
   | Some s -> g.Rdf_graph.add_triple ~sub ~pred: foaf_homepage ~obj: (Rdf_term.Uri s)
  ) ;
  let file = author_file outdir a in
  gen_ttl g file
;;

let gen_authors outdir authors = IMap.iter (gen_author outdir) authors;;

let gen_contrib outdir c =
  let bn =
    let cpt = ref 0 in
    fun () -> incr cpt; Rdf_term.Blank_ (Rdf_term.blank_id_of_string ("b"^(string_of_int !cpt)))
  in
  try
    let g = Rdf_graph.open_graph base_uri in
    let sub = Rdf_term.Uri (contrib_uri_ c.c_label) in
    g.Rdf_graph.add_triple ~sub ~pred: hump_id ~obj: (lit c.c_label);
    g.Rdf_graph.add_triple ~sub ~pred: Rdf_rdf.rdf_type ~obj: (Rdf_term.Uri hump_contrib) ;
    g.Rdf_graph.add_triple ~sub ~pred: foaf_name ~obj: (lit c.c_name);
    g.Rdf_graph.add_triple ~sub ~pred: foaf_homepage ~obj: (Rdf_term.Uri c.c_home);
    g.Rdf_graph.add_triple ~sub ~pred: dc_desc ~obj: (lit c.c_desc);

    let f_aut author =
      let obj = Rdf_term.Uri (author_uri_ author.aut_label) in
      g.Rdf_graph.add_triple ~sub ~pred: dc_creator ~obj
    in
    List.iter f_aut c.c_authors;

    let f_rel rel =
      let b = bn () in
      g.Rdf_graph.add_triple ~sub ~pred: hump_release ~obj: b;
      g.Rdf_graph.add_triple ~sub: b ~pred: foaf_name ~obj: (lit rel.rel_version);
      g.Rdf_graph.add_triple ~sub: b ~pred: dc_date
        ~obj: (Rdf_term.term_of_datetime ~d: rel.rel_date ());
    in
    List.iter f_rel c.c_releases;

    (match c.c_license with
      None -> ()
    | Some s -> g.Rdf_graph.add_triple ~sub ~pred: dc_rights ~obj: (lit s);
    );

    (match c.c_status with
      None -> ()
    | Some s -> g.Rdf_graph.add_triple ~sub
         ~pred: hump_status ~obj: (Rdf_term.Uri (status_uri s))
    );
    let f_tag tag =
      g.Rdf_graph.add_triple ~sub ~pred: hump_tag ~obj: (lit tag)
    in
    List.iter f_tag c.c_tags;

    let f_kind kind =
      g.Rdf_graph.add_triple ~sub ~pred: hump_kind ~obj: (lit kind)
    in
    List.iter f_kind c.c_kinds;

    let file = contrib_file outdir c in
    gen_ttl g file
  with
    Failure s -> failwith ("Contrib "^(c.c_name)^": "^s)
;;

let gen_contribs outdir contribs = List.iter (gen_contrib outdir) contribs;;

let outdir = ref Filename.current_dir_name ;;

let options =
  [
    "-d", Arg.Set_string db_name,
    "<db> Connect to database <db> (default is: "^ !db_name ^")";

    "-h", Arg.Set_string db_host,
    "<host> Connect to database on <host> (default is: "^ !db_host ^")";

    "-p", Arg.String (fun s -> db_passwd := Some s),
    "<password> Connect to database using <password> (default is no passwd)" ;

    "-P", Arg.Int (fun n -> db_port := Some n),
    "<port> Connect to database on <port>";

    "-u", Arg.Set_string db_user,
    "<user> Connect to database as <user> (default is: "^ !db_user ^")";

    "-o", Arg.Set_string outdir,
    "<d> Output to <d> instead of "^ !outdir ;

  ]


let mkdir s =
  let com = "mkdir -p "^(Filename.quote s) in
  match Sys.command com with
    0 -> ()
  | n -> failwith ("Command failed: "^com)
;;

let main () =
  Arg.parse options (fun _ -> ())
    (Printf.sprintf "Usage: %s [options]\nwhere options are:" Sys.argv.(0));
  connect();
  if not (connected_to_db()) then failwith "Not connected. Aborting.";

  let db = get_db () in
  let authors = authors db in
  let authors = List.fold_left
    (fun map author -> IMap.add author.aut_id author map)
    IMap.empty authors
  in
  let kinds = kinds db in
  let topics = topics db in
  let contribs = contribs db authors topics kinds in
  SQL.disconnect (get_db());

  mkdir (Filename.concat !outdir "authors");
  mkdir (Filename.concat !outdir "contribs");
  gen_authors !outdir authors;
  gen_contribs !outdir contribs;
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


