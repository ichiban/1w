open Batteries

type t =
  {
    http_method : string;
    version : int * int;
    uri : string;
    headers : (string * string) list;
    body : string;
  }
type request = t

let http_method r = r.http_method
let version r = r.version
let uri r = r.uri
let headers r = r.headers
let body r = r.body

type version = (int * int)
type headers = (string * string) list

module Builder =
  struct				 
    type t =
      | Empty
      | HttpMethod of Text.t
      | Uri of string * Text.t
      | Major of string * string * Text.t
      | Minor of string * string * int * Text.t
      | Field of string * string * version * headers * Text.t * Text.t
      | Value of string * string * version * headers * string * Text.t * Text.t
      | Body of string * string * version * headers * Text.t
							
    let empty = Empty
		    
    let with_method s = function
      | Empty -> HttpMethod (Text.of_string s)
      | HttpMethod m -> HttpMethod (Text.append m @@ Text.of_string s)
      | _ -> undefined ~message:"method" ()
		       
    let with_uri u = function
      | HttpMethod m -> Uri (Text.to_string m, Text.of_string u)
      | Uri (hm, uri) -> Uri (hm, Text.append uri @@ Text.of_string u)
      | _ -> undefined ~message:"uri" ()
		       
    let with_major v = function
      | Uri (hm, u) -> Major (hm, Text.to_string u, Text.of_string v)
      | Major (hm, u, maj) -> Major (hm, u, Text.append maj @@ Text.of_string v)
      | _ -> undefined ~message:"major" ()
		       
    let with_minor v = function
      | Major (hm, u, maj) ->
	 let maj' = maj |> Text.to_string |> Int.of_string in
	 Minor (hm, u, maj', Text.of_string v)
      | Minor (hm, u, maj, min) ->
	 Minor (hm, u, maj, Text.append min @@ Text.of_string v)
      | _ -> undefined ~message:"minor" ()
		       
    let with_field f = function
      | Minor (hm, u, maj, min) ->
	 let min' = min |> Text.to_string |> Int.of_string in
	 Field (hm, u, (maj, min'), [], Text.of_string f, Text.empty)
      | Field (hm, v, u, h, field, b) ->
	 Field (hm, v, u, h, Text.append field @@ Text.of_string f, b)
      | Value (hm, ver, u, h, f, value, b) ->
	 let headers = (f, Text.to_string value) :: h in
	 Field (hm, ver, u, headers, Text.of_string f, b)
      | Body (hm, v, u, h, b) -> Field (hm, v, u, h, Text.of_string f, b)
      | _ -> undefined ~message:"field" ()
		       
    let with_value v = function
      | Field (hm, ver, u, h, f, b) ->
	 Value (hm, ver, u, h, Text.to_string f, Text.of_string v, b)
      | Value (hm, ver, u, h, f, value, b) ->
	 Value (hm, ver, u, h, f, Text.append value @@ Text.of_string v, b)
      | _ -> undefined ~message:"value" ()
		       
    let with_body b = function
      | Value (hm, ver, u, h, f, v, body) ->
	 let headers = (f, Text.to_string v) :: h in
	 Body (hm, ver, u, headers, Text.append body @@ Text.of_string b)
      | Body (hm, ver, u, h, body) ->
	 Body (hm, ver, u, h, Text.append body @@ Text.of_string b)
      | _ -> undefined ~message:"body" ()
		       
    let to_request = function
      | Minor (hm, u, maj, min) ->
	 let min' = min |> Text.to_string |> Int.of_string in
	 {
	   http_method = hm;
	   version = (maj, min');
	   uri = u;
	   headers = [];
	   body = "";
	 }
      | Value (hm, u, ver, h, f, v, b) ->
	 {
	   http_method = hm;
	   version = ver;
	   uri = u;
	   headers = (f, Text.to_string v) :: h;
	   body = Text.to_string b;
	 }
      | Body (hm, u, ver, h, b) ->
	 {
	   http_method = hm;
	   version = ver;
	   uri = u;
	   headers = h;
	   body = Text.to_string b;
	 }
      | _ -> undefined ()
  end
