(** *)


let base_uri = Rdf_uri.uri "http://ocaml.org/hump";;
let authors_uri = Rdf_uri.concat base_uri "authors/";;
let author_uri_ label = Rdf_uri.append authors_uri (Netencoding.Url.encode label);;

let contribs_uri = Rdf_uri.concat base_uri "contribs/";;
let contrib_uri_ label = Rdf_uri.append contribs_uri (Netencoding.Url.encode label);;

let hump_uri = Rdf_uri.uri "http://ocaml.org/hump.rdf#";;
let hump_ s = Rdf_uri.append hump_uri s;;
let hump_release = hump_"release";;
let hump_status = hump_"status";;
let hump_tag = hump_"tag";;
let hump_kind = hump_"kind";;
let hump_author = hump_"author";;
let hump_contrib = hump_"contrib";;
let hump_id = hump_"id";;

let hump_devcode = hump_"devcode";;
let hump_alpha = hump_"alpha";;
let hump_beta = hump_"beta";;
let hump_stable = hump_"stable";;
let hump_mature = hump_"mature";;

let status_strings =
  [ hump_devcode, "Development code" ;
    hump_alpha, "Alpha" ;
    hump_beta, "Beta" ;
    hump_stable, "Stable" ;
    hump_mature, "Mature" ;
  ]
;;

let dc_uri = Rdf_uri.uri "http://purl.org/dc/elements/1.1/";;
let dc_ = Rdf_uri.append dc_uri;;
let dc_desc = dc_"description";;
let dc_date = dc_"date";;
let dc_creator = dc_"creator";;
let dc_rights = dc_"rights";;

let foaf_uri = Rdf_uri.uri "http://xmlns.com/foaf/0.1/";;
let foaf_ = Rdf_uri.append foaf_uri ;;
let foaf_name = foaf_"name";;
let foaf_firstname = foaf_"firstName";;
let foaf_lastname = foaf_"lastName";;
let foaf_homepage = foaf_"homepage";;

let lit s = Rdf_term.term_of_literal_string s;;

