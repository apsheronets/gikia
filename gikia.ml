(* This file is part of gikia.
 *
 * gikia is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * gikia is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with gikia.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2010-2013 Alexander Markov *)

open Lwt;
open Printf;
open ExtLib;
open Io;
open Utils;
open Init;

type request = {
  hostname: string;
  segpath: list string;
  headers: list (string * string);
};

value markup =
  let default = Markup.Polebrush in
  try
    match Init.markup.val with
    [ "textile" | "Textile" | "TEXTILE" -> Markup.Textile
    | "polebrush" | "Polebrush" | "POLEBRUSH" -> Markup.Polebrush
    | "html" | "Html" | "HTML" | "xhtml" | "Xhtml" | "XHTML" -> Markup.Html
    | _ -> default ]
  with _ -> default;

module Markup = Markup.Make (struct
  value markup = markup;
  value escape_html = Init.escape_html.val;
end);

open Views;
open Routes;

(* Makes an article *)
value render_article hostname file =
  Markup.get_page file#absolute_path >>= fun (title, content) ->
  let title =
    match title with
    [ Some title -> title
    | None -> file#path ] in
  let links = link_to_atom file#segpath in
  let a =
    object
      method title = title;
      method content = content;
      method links = links;
      method file = file;
      method hostname = hostname;
      method regular_file = True;
    end in
  render_with_layout Article.f a;

value render_full_history hostname =
  let title = sprintf "Full history of %s" hostname in
  Vcs.get_changes prefix >>= fun changes ->
  let a =
    object
      method title = title;
      method changes = changes;
      method hostname = hostname;
      method links = "";
    end in
  render_with_layout Full_history.f a;

value render_history request file =
  let path =
    match file#segpath with
    [ [] -> None
    | _ -> Some file#absolute_path ] in
  let title = sprintf "History of %s" file#path in
  Vcs.get_changes prefix ?path >>= fun changes ->
  let links = link_to_atom file#segpath in
  let a =
    object
      method title = title;
      method changes = changes;
      method links = links;
      method file = file;
      method hostname = request.hostname;
      method regular_file = True; (* FIXME *)
    end in
  render_with_layout History.f a;

value render_full_change hostname hash =
  let title = sprintf "Full patch %s" hash in
  Vcs.get_full_diff prefix hash >>= fun diff ->
  Vcs.wdiff diff >>= fun wdiff ->
  let wdiff = Vcs.xhtml_of_full_wdiff wdiff in
  let a =
    object
      method wdiff = wdiff;
      method title = title;
      method links = "";
      method hostname = hostname;
    end in
  render_with_layout View_full_change.f a;

value render_change hostname segpath hash =
  let file = new file segpath in
  let title = sprintf "Patch %s" hash in
  Vcs.get_diff prefix file#absolute_path hash >>= fun diff ->
  Vcs.wdiff diff >>= fun wdiff ->
  let wdiff = Vcs.xhtml_of_wdiff wdiff in
  let a =
    object
      method links = link_to_atom file#segpath;
      method wdiff = wdiff;
      method title = title;
      method hostname = hostname;
      method hash = hash;
      method file = file;
      method regular_file = True; (* FIXME *)
    end in
  render_with_layout View_change.f a;

(* Makes a page with a list of directory content *)
value render_index hostname prefix dir =
  (* Returns True if file is invisible *)
  let invisible filename =
    if filename = "style.css" or filename = "_darcs"
    then True
    else if filename.[0] = '.'
      then True
      else False in
  (Lwt_unix.opendir dir#absolute_path >>= fun dh ->
    let rec loop acc =
      catch
        (fun () ->
          Lwt_unix.readdir dh >>= fun file ->
          loop [file::acc])
        (fun
          [ End_of_file ->
              Lwt_unix.closedir dh >>= fun () -> return acc
          | e -> fail e]) in
    loop [])
  >>= fun files ->
  (files >>
  List.filter (fun x -> not (invisible x)) >>
  Lwt_list.map_p
    (fun x ->
      let file = new file (dir#segpath @ [x]) in
      file#kind_of_file >>= (fun
      [ Io.Page ->
          Markup.get_header_of_page file#absolute_path >>= fun
          [ Some s -> return (False, s)
          | None   -> return (False, x) ]
      | Io.Dir -> return (True, (x ^ "/"))
      | _ -> return (False, x) ] ) >>= fun (is_a_dir, title) ->
             (* FIXME: hey, we need dtfilter here! *)
      return (is_a_dir, file#path, title)
      ) ) >>= fun lis ->
  let cmp x y =
    match (x, y) with
    [ (False, True) -> 1
    | (True, False) -> -1
    | _ -> 0 ] in
  let lis = List.sort ~cmp:(fun (x,_,_) (y,_,_) -> cmp x y) lis in
  let dirname = dir#path in
  let title = "Index of " ^ dirname in
  let a =
    object
      method links = link_to_atom dir#segpath;
      method lis = lis;
      method title = title;
      method hostname = hostname;
      method file = dir;
      method regular_file = False;
    end in
  render_with_layout Index.f a;

(* error 404 page *)
value render_404 =
  let a =
    object
      method title = "error 404: not found";
      method content =
        "<h1>error 404: not found</h1>\n<p>no such page</p>";
      method links = ""; (* FIXME *)
    end in
  render (Error.f a);

(* error 410 page *)
value render_410 path =
  let a =
    object
      method title = "error 410: gone";
      method content =
        sprintf "<h1>error 410: gone</h1>\n<p>This page has been removed.</p><p>But you still can browse its %s.</p>" (link_to_history path);
    end in
  render (Error.f a);

value render_500 =
  let a =
    object
      method title = "error 500: Internal Server Error";
      method content =
        sprintf "<h1>error 500: Internal Server Error</h1>\n<p>Sorry, something happend with our webserver.</p>";
    end in
  render (Error.f a);

open Amall_http;
module IO = IO_Lwt;
module I = Iteratees.Make(IO);

value send_404 =
  { rs_status_code = 404
  ; rs_reason_phrase = "Not found"
  ; rs_headers = { rs_all = [] }
  ; rs_body = Body_string render_404
  };

value send_410 path =
  { rs_status_code = 410
  ; rs_reason_phrase = "Gone"
  ; rs_headers = { rs_all = [] }
  ; rs_body = Body_string (render_410 path)
  };

value send_500 =
  { rs_status_code = 500
  ; rs_reason_phrase = "Internal Server Error"
  ; rs_headers = { rs_all = [] }
  ; rs_body = Body_string render_500
  };

value send_file ?content_type file =
  let rs_all = [] in
  let rs_all =
    match content_type with
    [ None -> rs_all
    | Some s -> [("Content-Type", s)::rs_all] ] in
  file#last_modified_header >>= fun last_modified ->
  let rs_all = [last_modified::rs_all] in
  file#size >|= fun size ->
  { rs_status_code = 200
  ; rs_reason_phrase = "OK"
  ; rs_headers = { rs_all }
  ; rs_body = File_contents file#absolute_path size
  };

value send_not_modified last_modified =
 { rs_status_code = 304
 ; rs_reason_phrase = "Not Modified"
 ; rs_headers = { rs_all = [last_modified] }
 ; rs_body = No_body };

value send_file ?content_type request file =
  catch (fun () ->
    file#mtime >>= fun mtime ->
    let last_modified =
      CalendarLib.Calendar.Precise.from_unixfloat mtime
      >> CalendarLib.Calendar.Precise.to_gmt in
    let (if_modified_since, _) =
      List.assoc "If-Modified-Since" request.headers
      >> calendar_of_rfc2282 in
    if CalendarLib.Calendar.Precise.compare last_modified if_modified_since > 0
    then send_file ?content_type file
    else
      file#last_modified_header >|=
      send_not_modified)
  (fun _ -> send_file ?content_type file); (* TODO: log it *)


value redirect_to path =
  let location_header = ("Location", path) in
  { rs_status_code = 301
  ; rs_reason_phrase = "Moved Permanently"
  ; rs_headers = { rs_all = [location_header] }
  (*; rs_body = No_body*)
  ; rs_body = Body_string "moved" (* FIXME *)
  };

value send_ok_with ?(headers=[]) ?content_type body =
  let headers =
    match content_type with
    [ None -> [("Content-Type", "text/html")::headers]
    | Some s -> [("Content-Type", s)::headers] ] in
  { rs_status_code = 200
  ; rs_reason_phrase = "OK"
  ; rs_headers = { rs_all = headers }
  ; rs_body = Body_string body
  };

value main_handler file request header =
  file#kind_of_file >>= fun
  [ Io.Page when (try List.last file#segpath = "index" with _ -> False) ->
      (* do not handle /path/index *)
      return & redirect_to & params_to_string &
        List.rev & List.drop 1 & List.rev file#segpath
  | Io.Page ->
      render_article request.hostname file >>= fun body ->
      Lwt.return & send_ok_with ~headers:[header] body
  | Io.Other | Io.Html | Io.VcsFile ->
      send_file request file
  | Io.Dir ->
      let index = new file (file#segpath @ ["index"]) in
      index#kind_of_file >>= fun
      [ Io.Page ->
          render_article request.hostname index >>= fun body ->
          return & send_ok_with ~headers:[header] body
      | _ ->
          render_index request.hostname prefix file >>= fun body ->
          return & send_ok_with ~headers:[header] body ]
  | Io.NotExists ->
      let file_in_public = new file ~prefix:gikia_public_dir request.segpath in
      file_in_public#exists >>= fun file_exists ->
      if file_exists
      then send_file request file_in_public
      else
        (*match _p with
        [ ["sitemap.xml"] ->
            I.lift &
              application/xml
              Sitemap.make main_service prefix >>= fun body ->
              send_ok_with ~content_type:"application/xml" body
        | _ ->*)
            Vcs.where_file_leaves prefix file#absolute_path file#segpath >>= fun
            [ None -> return send_404
            | Some Vcs.Removed -> return (send_410 request.segpath)
            | Some (Vcs.MovedTo dst) ->
                return & redirect_to (params_to_string dst) ] (* ] *)
  | Io.Incorrect -> return send_404 ];

value send_source request =
  let file = new file request.segpath in
  file#kind_of_file >>= fun
  [ Io.Page ->
      send_file ~content_type:"text/plain" request file
  | Io.NotExists ->
      Vcs.where_file_leaves prefix file#absolute_path file#segpath >>= fun
      [ None -> return send_404
      | Some Vcs.Removed -> return (send_410 request.segpath)
      | Some (Vcs.MovedTo dst) ->
          return & redirect_to (url_to_src dst) ]
  | _ -> return send_404 ];

open Cache;
open CalendarLib;
module C = CalendarLib.Calendar.Precise;

value send_body_or_304 request chunk send_f =
  match chunk.mtime with
  (* case 1: we have too dynamic page *)
  [ AlwaysFresh -> send_f & just_modified_header ()
  | Mtime mtime ->
      let if_modified_since =
        try
          let (c, tz) =
            List.assoc "If-Modified-Since" request.headers
            >> calendar_of_rfc2282 in
          Some c
        with _ -> None in
      match if_modified_since with
      (* case 2: we didn't got If-Modified-Since header from client *)
      [ None -> send_f & http_header_of_mtime mtime
      | Some if_modified_since ->
          let last_modified =
            Calendar.Precise.from_unixfloat mtime >> Calendar.Precise.to_gmt in
          if Calendar.Precise.compare last_modified if_modified_since > 0
          (* case 3: user's copy is too old, our is too new *)
          then send_f & http_header_of_mtime mtime
          (* case 4: user's copy is up to date, send 304 *)
          else return & send_not_modified & http_header_of_mtime mtime ]
  ];

value send_chunk ?content_type request chunk =
  let send_body last_modified =
    let rs_all =
      match content_type with
      [ None -> [("Content-Type", "text/html")]
      | Some s -> [("Content-Type", s)] ] in
    let rs_all =
      [last_modified::rs_all] in
    !!(chunk.body) >|= fun body ->
    { rs_status_code = 200
    ; rs_reason_phrase = "OK"
    ; rs_headers = { rs_all }
    ; rs_body = Body_string body
    } in
  send_body_or_304 request chunk send_body;

value cache_responce request chunk =
  let send_responce last_modified =
    !!(chunk.body) last_modified in
  send_body_or_304 request chunk send_responce;

value render_full_atom request file =
  let make_iri hash = absolutify request.hostname & url_to_full_change hash in
  let link = absolutify request.hostname & url_to_history [] in
  let title = sprintf "Recent changes to %s" request.hostname in
  Atom.of_repo ~file ~title ~link make_iri;

value render_atom request file =
  let make_iri hash = absolutify request.hostname & url_to_full_change hash in
  let link = absolutify request.hostname & url_to_history request.segpath in
  let title = sprintf "Recent changes to %s" file#path in
  Atom.of_page ~file ~title ~link make_iri;

open Am_All;
open Amall_types;

module S = Amall_http_service.Service(IO)(I);

value (my_listener, http_root, _ws_root) = S.listener_create (`Inet_any port.val);

(* v v v amall kludges v v v *)
(* gds wont' fix that *)
value my_endpoint =
  ( http_root
  , `Fallback []
  )
;
(* ^ ^ ^ amall kludges ^ ^ ^ *)

value my_func segpath rq =
  let headers =
    rq.rq_headers.rq_all in
  let (hostname, segpath) =
    match segpath with
    [ [_; hostname :: t] -> (hostname, t)
    | _ -> assert False ] in
  let hostname =
    match default_host.val with
    [ "" -> hostname
    | s -> s ] in
  let segpath =
    segpath >>
    List.filter (fun [ "" -> False | _ -> True ]) >>
    List.map Cd_Strings.Strings.Onebyte.urldecode >>
    (* stupid dtfilter *)
    List.filter (fun x -> not (ExtLib.String.exists x "..")) in
  let params =
    match rq.rq_uri.Uri_type.query with
    [ None -> []
    | Some s -> Uri.parse_params s ] in
  let request = { hostname; segpath; headers } in
  let file_related_chunk ?content_type file body =
    catch
      (fun () ->
        file#mtime >>= fun mtime ->
        return (Mtime mtime))
      (fun _ -> return AlwaysFresh) >>= fun mtime ->
    send_chunk ?content_type request { mtime; body } in
  catch (fun () ->
    let file = new file segpath in
    match (segpath, params) with
    [ ([],      [("show", "log")]) ->
        file_related_chunk file (lazy (render_full_history hostname))
    | (segpath, [("show", "log")]) ->
        file_related_chunk file (lazy (render_history request file))
    | ([],      [("show", "atom")]) ->
        file_related_chunk ~content_type:"application/atom+xml" file (lazy (render_full_atom request file))
    | (segpath, [("show", "atom")]) ->
        file_related_chunk ~content_type:"application/atom+xml" file (lazy (render_atom request file))
    | (segpath, [("show", "source")]) -> send_source request
    | ([],      [("hash", hash)]) ->
        file_related_chunk file (lazy (render_full_change hostname hash))
    | (segpath, [("hash", hash) :: _]) ->
        file_related_chunk file (lazy (render_change hostname segpath hash))
    | (segpath, []) ->
        catch
          (fun () ->
            file#mtime >>= fun mtime ->
            return (Mtime mtime))
          (fun _ -> return AlwaysFresh) >>= fun mtime ->
        let body = lazy (main_handler file request) in
        cache_responce request { mtime; body }
    | _ -> Lwt.return send_404 ] )
  (fun e ->
    Lwt_io.eprintf "ERROR: Responce failed with %s\n" (Printexc.to_string e) >>= fun () ->
    Lwt.return send_500)
;

value my_func segpath rq =
  I.lift &
    Lwt_unix.with_timeout 50. (fun () ->
      my_func segpath rq);

value () = S.mount_http my_endpoint my_func;

value () = S.listener_run my_listener;
