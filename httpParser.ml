open Batteries

let connection = "connection"
let content_length = "content-length"
let transfer_encoding = "transfer-encoding"
let upgrade = "upgrade"
let chunked = "chunked"
let keep_alive = "keep-alive"
let close = "close"

let invalid_token = '\000'
let tokens =
  let ___ = invalid_token in
  let quo = '\'' in
  [| ___; ___; ___; ___; ___; ___; ___; ___;
     ___; ___; ___; ___; ___; ___; ___; ___;
     ___; ___; ___; ___; ___; ___; ___; ___;
     ___; ___; ___; ___; ___; ___; ___; ___;
     ___; '!'; ___; '#'; '$'; '%'; '&'; quo;
     ___; ___; '*'; '+'; ___; '-'; '.'; ___;
     '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
     '8'; '9'; ___; ___; ___; ___; ___; ___;
     ___; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g';
     'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o';
     'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w';
     'x'; 'y'; 'z'; ___; ___; ___; '^'; '_';
     '`'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g';
     'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o';
     'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w';
     'x'; 'y'; 'z'; ___; '|'; ___; '~'; ___ |]

let unhex =
  let __ = -1 in
  [| __; __; __; __; __; __; __; __;
     __; __; __; __; __; __; __; __;
     __; __; __; __; __; __; __; __;
     __; __; __; __; __; __; __; __;
     __; __; __; __; __; __; __; __;
     __; __; __; __; __; __; __; __;
     00; 01; 02; 03; 04; 05; 06; 07;
     08; 09; __; __; __; __; __; __;
     __; 10; 11; 12; 13; 14; 15; __;
     __; __; __; __; __; __; __; __;
     __; __; __; __; __; __; __; __;
     __; __; __; __; __; __; __; __;
     __; 10; 11; 12; 13; 14; 15; __;
     __; __; __; __; __; __; __; __;
     __; __; __; __; __; __; __; __;
     __; __; __; __; __; __; __; __ |]

let normal_uri_char =
  let yes = true in
  let ___ = false in
  let meh = not Config.strict in
  [| ___; ___; ___; ___; ___; ___; ___; ___;
     ___; meh; ___; ___; meh; ___; ___; ___;
     ___; ___; ___; ___; ___; ___; ___; ___;
     ___; ___; ___; ___; ___; ___; ___; ___;
     ___; yes; yes; ___; yes; yes; yes; yes;
     yes; yes; yes; yes; yes; yes; yes; yes;
     yes; yes; yes; yes; yes; yes; yes; yes;
     yes; yes; yes; yes; yes; yes; yes; ___;
     yes; yes; yes; yes; yes; yes; yes; yes;
     yes; yes; yes; yes; yes; yes; yes; yes;
     yes; yes; yes; yes; yes; yes; yes; yes;
     yes; yes; yes; yes; yes; yes; yes; yes;
     yes; yes; yes; yes; yes; yes; yes; yes;
     yes; yes; yes; yes; yes; yes; yes; yes;
     yes; yes; yes; yes; yes; yes; yes; yes;
     yes; yes; yes; yes; yes; yes; yes; ___ |]

type mark = int
type index = int
type content_length = int option
type flags =
  {
    chunked : bool;
    connection_keep_alive : bool;
    connection_close : bool;
    connection_upgrade : bool;
    trailing : bool;
    upgrade : bool;
  }
type nread = int
type size = int

type header_state =
  | General
  | C
  | Co
  | Con
  | MatchingConnection of index
  | MatchingContentLength of index
  | MatchingTransferEncoding of index
  | MatchingUpgrade of index
  | Connection
  | ContentLength
  | TransferEncoding
  | Upgrade
  | MatchingTransferEncodingChunked of index
  | MatchingConnectionTokenStart
  | MatchingConnectionKeepAlive of index
  | MatchingConnectionClose of index
  | MatchingConnectionUpgrade of index
  | MatchingConnectionToken
  | TransferEncodingChunked
  | ConnectionKeepAlive
  | ConnectionClose
  | ConnectionUpgrade
      
type state =
  | Dead
  | Start
  | MethodStart
  | Method of nread * mark
  | SpacesBeforeUri
  | Asterisk of mark
  | Uri of nread * mark
  | HttpStart
  | H
  | Ht
  | Htt
  | Http
  | MajorFirst
  | Major of nread * mark
  | MinorFirst
  | Minor of nread * mark
  | LineAlmostDone
  | HeaderFieldStart of flags * content_length
  | HeaderField of nread * flags * content_length * header_state * mark
  | HeaderValueDiscardWs of nread * flags * content_length * header_state
  | HeaderValueDiscardWsAlmostDone of nread * flags * content_length * header_state
  | HeaderValueDiscardLws of nread * flags * content_length * header_state
  | HeaderValueStart of nread * flags * content_length * header_state
  | HeaderValue of nread * flags * content_length * header_state * mark
  | HeaderValueLws of nread * flags * content_length * header_state
  | HeaderAlmostDone of nread * flags * content_length * header_state
  | ChunkSizeStart
  | ChunkSize of size
  | ChunkParameters of size
  | ChunkSizeAlmostDone of size
  | HeadersAlmostDone of flags * content_length
  | HeadersDone of flags * content_length
  | ChunkData of size * mark
  | ChunkDataAlmostDone of mark
  | ChunkDataDone
  | BodyIdentity of index * mark
  | BodyIdentityEof of mark
  | MessageDone

exception HeaderOverflow
exception ClosedConnection
exception InvalidVersion
exception InvalidMethod
exception InvalidUri
exception LfExpected
exception InvalidHeaderToken
exception InvalidContentLength
exception InvalidChunkSize
exception InvalidConstant
exception Strict
	    
let empty_flags =
  {
    chunked = false;
    connection_keep_alive = false;
    connection_close = false;
    connection_upgrade = false;
    trailing = false;
    upgrade = false;
  }

let trailing_flags =
  { empty_flags with trailing = true }

type data_callback = string -> int -> int -> unit
type callback = unit -> unit
type callbacks =
  {
    on_message_begin : callback;
    on_method : data_callback;
    on_uri : data_callback;
    on_version_major : data_callback;
    on_version_minor : data_callback;
    on_header_field : data_callback;
    on_header_value : data_callback;
    on_headers_complete : callback;
    on_body : data_callback;
    on_message_complete : callback;
    on_chunk_header : callback;
    on_chunk_complete : callback;
  }

type t = {
    mutable state : state;
    mutable nread : nread;
    callbacks : callbacks;
  }

let make callbacks = { state = Start; nread = 0; callbacks = callbacks }

let is_alpha c =
  let l = Char.lowercase c in
  l >= 'a' && l <= 'z'

let is_num c =
  c >= '0' && c <= '9'
	   
let is_alphanum c = is_alpha c || is_num c
					 
let is_mark = function
  | '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' -> true
  | _ -> false
	   
let is_userinfo_char = function
  | '%' | ';' | ':' | '&' | '=' | '+' | '$' | ',' -> true
  | c -> is_alphanum c || is_mark c
				  
let strict_token c =
  try
    Char.code c |> Array.get tokens
  with
    Invalid_argument _ -> invalid_token
					      
let token c =
  let result = Char.code c |> Array.get tokens in
  if Config.strict then
    result
  else if c = ' ' then
    ' '
  else
    result

let is_token c =
  token c != invalid_token

let is_uri_char c =
  let code = Char.code c in
  try
    Array.get normal_uri_char code
  with
    Invalid_argument _ -> not Config.strict

let unhex c =
  try
    Array.get unhex (Char.code c)
  with
    Invalid_argument _ -> -1

let strict_check cond =
  if Config.strict && not cond then
    raise Strict

let new_message flags =
  if Config.strict && flags.connection_close then
    Dead
  else
    Start

let digit ch = Char.code ch - Char.code '0'

let rec parse_message_char callbacks data state p =
  let ch = String.get data p in
  match state, ch with
  (* this state is used after a 'Connection: close' message
   * the parser will error out if it reads another message
   *)
  | Dead, '\r'
  | Dead, '\n' -> Dead
  | Dead, _ -> raise ClosedConnection
  | Start, '\r' -> Start
  | Start, '\n' -> Start
  | Start, _ ->
     callbacks.on_message_begin ();
     parse_message_char callbacks data MethodStart p
  | MethodStart, ch when is_token ch -> Method (1, p)
  | MethodStart, _ -> raise InvalidMethod
  | Method (n, m), _ when n >= Config.max_method_size ->
     raise InvalidMethod
  | Method (n, m), ' ' ->
     callbacks.on_method data m p;
     SpacesBeforeUri
  | Method (n, m), ch when is_token ch -> Method (n + 1, m)
  | Method _, _ -> raise InvalidMethod
  | SpacesBeforeUri, ' ' -> SpacesBeforeUri
  | SpacesBeforeUri, '*' -> Asterisk p
  | SpacesBeforeUri, ch when is_uri_char ch -> Uri (1, p)
  | SpacesBeforeUri, _ -> raise InvalidUri
  | Asterisk m, ' ' ->
     callbacks.on_uri data m p;
     HttpStart
  | Asterisk m, _ -> raise InvalidUri
  | Uri (n, m), _ when n >= Config.max_uri_size ->
     raise InvalidUri
  | Uri (n, m), ' ' ->
     callbacks.on_uri data m p;
     HttpStart
  | Uri (n, m), ch when is_uri_char ch -> Uri (n + 1, m)
  | Uri (n, m), _ -> raise InvalidUri
  | HttpStart, 'H' -> H
  | HttpStart, ' ' -> HttpStart
  | HttpStart, _ -> raise InvalidConstant
  | H, ch ->
     strict_check (ch = 'T');
     Ht
  | Ht, ch ->
     strict_check (ch = 'T');
     Htt
  | Htt, ch ->
     strict_check (ch = 'P');
     Http
  | Http, ch ->
     strict_check (ch = '/');
     MajorFirst
  | MajorFirst, ch when is_num ch -> Major (1, p)
  | MajorFirst, _ -> raise InvalidVersion
  | Major (n, m), _ when n >= Config.max_version_digit_size ->
     raise InvalidVersion
  | Major (n, m), '.' ->
     callbacks.on_version_major data m p;
     MinorFirst
  | Major (n, m), ch when is_num ch -> Major (n + 1, m)
  | Major _, _ -> raise InvalidVersion
  | MinorFirst, ch when is_num ch -> Minor (1, p)
  | MinorFirst, _ -> raise InvalidVersion
  | Minor (n, m), _ when n >= Config.max_version_digit_size ->
     raise InvalidVersion
  | Minor (n, m), '\r' ->
     callbacks.on_version_minor data m p;
     LineAlmostDone
  | Minor (n, m), '\n' ->
     callbacks.on_version_minor data m p;
     HeaderFieldStart (empty_flags, None)
  | Minor (n, m), ch when is_num ch -> Minor (n + 1, m)
  | Minor _, _ -> raise InvalidVersion
  | LineAlmostDone, '\n' ->  HeaderFieldStart (empty_flags, None)
  | LineAlmostDone, _ -> raise LfExpected
  | HeaderField (n, flags, cl, _, _), _
  | HeaderValueDiscardWs (n, flags, cl, _), _
  | HeaderValueDiscardWsAlmostDone (n, flags, cl, _), _
  | HeaderValueDiscardLws (n, flags, cl, _), _
  | HeaderValueStart (n, flags, cl, _), _
  | HeaderValue (n, flags, cl, _, _), _
  | HeaderValueLws (n, flags, cl, _), _
  | HeaderAlmostDone (n, flags, cl, _), _
       when n >= Config.max_header_size ->
     raise HeaderOverflow;
  | HeaderFieldStart (flags, cl), '\r' -> HeadersAlmostDone (flags, cl)
  | HeaderFieldStart (flags, cl), '\n' ->
     (* they might be just sending \n instead of \r\n
      *  so this would be the second \n
      *  to denote the end of headers
      *)
     parse_message_char callbacks data (HeadersAlmostDone (flags, cl)) p
  | HeaderFieldStart (flags, cl), ch when not (is_token ch) ->
     Printf.printf "invalid token: %d\n" (Char.code ch);
     raise InvalidHeaderToken
  | HeaderFieldStart (flags, cl), ch when token ch = 'c' ->
     HeaderField (1, flags, cl, C, p)
  | HeaderFieldStart (flags, cl), ch when token ch = 't' ->
     HeaderField (1, flags, cl, MatchingTransferEncoding 0, p)
  | HeaderFieldStart (flags, cl), ch when token ch = 'u' ->
     HeaderField (1, flags, cl, MatchingUpgrade 0, p)
  | HeaderFieldStart (flags, cl), _ ->
     HeaderField (1, flags, cl, General, p)
  | HeaderField (n, flags, cl, hs, m), ':' ->
     callbacks.on_header_field data m p;
     HeaderValueDiscardWs (n + 1, flags, cl, hs)
  | HeaderField _, ch when not (is_token ch) -> Printf.printf "here\n%!"; raise InvalidHeaderToken
  | HeaderField (n, flags, cl, General, m), _ ->
     HeaderField (n + 1, flags, cl, General, m)
  | HeaderField (n, flags, cl, C, m), ch when token ch = 'o' ->
     HeaderField (n + 1, flags, cl, Co, m)
  | HeaderField (n, flags, cl, Co, m), ch when token ch = 'n' ->
     HeaderField (n + 1, flags, cl, Con, m)
  | HeaderField (n, flags, cl, Con, m), ch when token ch = 'n' ->
     HeaderField (n + 1, flags, cl, MatchingConnection 3, m)
  | HeaderField (n, flags, cl, Con, m), ch when token ch = 't' ->
     HeaderField (n + 1, flags, cl, MatchingContentLength 3, m)
  | HeaderField (n, flags, cl, MatchingContentLength i, m), ch
       when i > String.length connection - 1
	    || token ch != String.get connection i ->
     HeaderField (n + 1, flags, cl, General, m)
  | HeaderField (n, flags, cl, MatchingContentLength i, m), _
       when i = String.length connection - 2 ->
     HeaderField (n + 1, flags, cl, ContentLength, m)
  | HeaderField (n, flags, cl, MatchingConnection i, m), ch
       when i > String.length connection - 1
	    || token ch != String.get connection i ->
     HeaderField (n + 1, flags, cl, General, m)
  | HeaderField (n, flags, cl, MatchingConnection i, m), _
       when i = String.length connection - 2->
     HeaderField (n + 1, flags, cl, Connection, m)
  | HeaderField (n, flags, cl, MatchingContentLength i, m), ch
       when i > String.length content_length - 1
	    || token ch != String.get content_length i ->
     HeaderField (n + 1, flags, cl, General, m)
  | HeaderField (n, flags, cl, MatchingContentLength i, m), _
       when i = String.length content_length - 2 ->
     HeaderField (n + 1, flags, cl, ContentLength, m)
  | HeaderField (n, flags, cl, MatchingTransferEncoding i, m), ch
       when i > String.length transfer_encoding - 1
	    || token ch != String.get transfer_encoding i ->
     HeaderField (n + 1, flags, cl, General, m)
  | HeaderField (n, flags, cl, MatchingTransferEncoding i, m), _
       when i = String.length transfer_encoding - 2 ->
     HeaderField (n + 1, flags, cl, TransferEncoding, m)
  | HeaderField (n, flags, cl, MatchingUpgrade i, m), ch
       when i > String.length upgrade - 1
	    || token ch != String.get upgrade i ->
     HeaderField (n + 1, flags, cl, General, m)
  | HeaderField (n, flags, cl, MatchingUpgrade i, m), _
       when i = String.length upgrade - 2 ->
     HeaderField (n + 1, flags, cl, Upgrade, m)
  | HeaderField (n, flags, cl, Connection, m), ' '
  | HeaderField (n, flags, cl, ContentLength, m), ' '
  | HeaderField (n, flags, cl, TransferEncoding, m), ' '
  | HeaderField (n, flags, cl, Upgrade, m), ' ' ->
     HeaderField (n + 1, flags, cl, General, m)
  | HeaderField (n, flags, cl, hs, m), _ ->
     HeaderField (n + 1, flags, cl, hs, m)
  | HeaderValueDiscardWs (n, flags, cl, hs), ' '
  | HeaderValueDiscardWs (n, flags, cl, hs), '\t' ->
     HeaderValueDiscardWs (n + 1, flags, cl, hs)
  | HeaderValueDiscardWs (n, flags, cl, hs), '\r' ->
     HeaderValueDiscardWsAlmostDone (n + 1, flags, cl, hs)
  | HeaderValueDiscardWs (n, flags, cl, hs), '\n' ->
     HeaderValueDiscardLws (n + 1, flags, cl, hs)
  | HeaderValueDiscardWs (n, flags, cl, hs), _ ->
     parse_message_char callbacks data (HeaderValueStart (n, flags, cl, hs)) p
  | HeaderValueDiscardLws (n, flags, cl, hs), ' '
  | HeaderValueDiscardLws (n, flags, cl, hs), '\t' ->
     parse_message_char callbacks data (HeaderValueStart (n, flags, cl, hs)) p
  | HeaderValueDiscardLws (n, flags, cl, ConnectionKeepAlive), _ ->
     HeaderFieldStart ({ flags with connection_keep_alive = true }, cl)
  | HeaderValueDiscardLws (n, flags, cl, ConnectionClose), _ ->
     HeaderFieldStart ({ flags with connection_close = true }, cl)
  | HeaderValueDiscardLws (n, flags, cl, TransferEncodingChunked), _ ->
     HeaderFieldStart ({ flags with chunked = true }, cl)
  | HeaderValueDiscardLws (n, flags, cl, ConnectionUpgrade), _ ->
     HeaderFieldStart ({ flags with connection_upgrade = true }, cl)
  | HeaderValueDiscardLws (n, flags, cl, _), _ ->
     HeaderFieldStart (flags, cl)
  | HeaderValueStart (n, flags, cl, Upgrade), _ ->
     HeaderValue (n + 1, { flags with upgrade = true }, cl, General, p)
  | HeaderValueStart (n, flags, cl, TransferEncoding), ch
       when Char.lowercase ch = 'c' ->
     HeaderValue (n + 1, flags, cl, MatchingTransferEncodingChunked 1, p)
  | HeaderValueStart (n, flags, cl, TransferEncoding), _ ->
     HeaderValue (n + 1, flags, cl, General, p)
  | HeaderValueStart (n, flags, cl, ContentLength), ch when not (is_num ch) ->
     raise InvalidContentLength
  | HeaderValueStart (n, flags, _, ContentLength), ch ->
     HeaderValue (n + 1, flags, Some (digit ch), ContentLength, p)
  | HeaderValueStart (n, flags, cl, Connection), ch when Char.lowercase ch = 'k' ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionKeepAlive 1, p)
  | HeaderValueStart (n, flags, cl, Connection), ch when Char.lowercase ch = 'c' ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionClose 1, p)
  | HeaderValueStart (n, flags, cl, Connection), ch when Char.lowercase ch = 'u' ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionUpgrade 1, p)
  | HeaderValueStart (n, flags, cl, Connection), ch ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionToken, p)
  | HeaderValueStart (n, flags, cl, MatchingConnectionTokenStart), _ ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionTokenStart, p)
  | HeaderValueStart (n, flags, cl, _), _ ->
     HeaderValue (n + 1, flags, cl, General, p)
  | HeaderValue (n, flags, cl, hs, m), '\r' ->
     callbacks.on_header_value data m p;
     HeaderAlmostDone (n + 1, flags, cl, hs)
  | HeaderValue (n, flags, cl, hs, m), '\n' ->
     callbacks.on_header_value data m p;
     parse_message_char callbacks data (HeaderAlmostDone (n, flags, cl, hs)) p
  | HeaderValue (n, flags, cl, General, m), ch ->
     HeaderValue (n + 1, flags, cl, General, m)
  | HeaderValue (n, flags, Some cl, ContentLength, m), ' ' ->
     HeaderValue (n + 1, flags, Some cl, ContentLength, m)
  | HeaderValue (n, flags, Some cl, ContentLength, m), ch when not (is_num ch) ->
     raise InvalidContentLength
  | HeaderValue (n, flags, Some cl, ContentLength, m), ch ->
     let content_length = 10 * cl + digit ch in
     if Config.max_content_size < content_length then
       raise InvalidContentLength;
     HeaderValue (n + 1, flags, Some content_length, ContentLength, m)
  | HeaderValue (n, flags, cl, MatchingTransferEncodingChunked i, m), ch
       when i > String.length chunked - 1 ||
	      Char.lowercase ch != String.get chunked i ->
     HeaderValue (n + 1, flags, cl, General, m)
  | HeaderValue (n, flags, cl, MatchingTransferEncodingChunked i, m), _
       when i == String.length chunked - 2 ->
     HeaderValue (n + 1, flags, cl, TransferEncodingChunked, m)
  | HeaderValue (n, flags, cl, MatchingTransferEncodingChunked i, m), _ ->
     let header_state = MatchingTransferEncodingChunked (i + 1) in
     HeaderValue (n + 1, flags, cl, header_state, m)
  | HeaderValue (n, flags, cl, MatchingConnectionTokenStart, m), ch
       when Char.lowercase ch = 'k' ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionKeepAlive 1, m)
  | HeaderValue (n, flags, cl, MatchingConnectionTokenStart, m), ch
       when Char.lowercase ch = 'c' ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionClose 1, m)
  | HeaderValue (n, flags, cl, MatchingConnectionTokenStart, m), ch
       when Char.lowercase ch = 'u' ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionUpgrade 1, m)
  | HeaderValue (n, flags, cl, MatchingConnectionTokenStart, m), ch
       when strict_token ch != invalid_token ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionToken, m)
  | HeaderValue (n, flags, cl, MatchingConnectionTokenStart, m), ' '
  | HeaderValue (n, flags, cl, MatchingConnectionTokenStart, m), '\t' ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionTokenStart, m)
  | HeaderValue (n, flags, cl, MatchingConnectionTokenStart, m), _ ->
     HeaderValue (n + 1, flags, cl, General, m)
  | HeaderValue (n, flags, cl, MatchingConnectionKeepAlive i, m), ch
       when i > String.length keep_alive - 1 ||
	      Char.lowercase ch != String.get keep_alive i ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionToken, m)
  | HeaderValue (n, flags, cl, MatchingConnectionKeepAlive i, m), _
       when i = String.length keep_alive - 2 ->
     HeaderValue (n + 1, flags, cl, ConnectionKeepAlive, m)
  | HeaderValue (n, flags, cl, MatchingConnectionKeepAlive i, m), _ ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionKeepAlive (i + 1), m)
  | HeaderValue (n, flags, cl, MatchingConnectionClose i, m), ch
       when i > String.length close - 1 ||
	      Char.lowercase ch != String.get close i ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionToken, m)
  | HeaderValue (n, flags, cl, MatchingConnectionClose i, m), _
       when i = String.length close - 2 ->
     HeaderValue (n + 1, flags, cl, ConnectionClose, m)
  | HeaderValue (n, flags, cl, MatchingConnectionClose i, m), _ ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionClose (i + 1), m)
  | HeaderValue (n, flags, cl, MatchingConnectionUpgrade i, m), ch
       when i > String.length upgrade - 1 ||
	      Char.lowercase ch != String.get upgrade i ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionToken, m)
  | HeaderValue (n, flags, cl, MatchingConnectionUpgrade i, m), _
       when i = String.length upgrade - 2 ->
     HeaderValue (n + 1, flags, cl, ConnectionUpgrade, m)
  | HeaderValue (n, flags, cl, MatchingConnectionUpgrade i, m), _ ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionUpgrade (i + 1), m)
  | HeaderValue (n, flags, cl, MatchingConnectionToken, m), ',' ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionTokenStart, m)
  | HeaderValue (n, flags, cl, MatchingConnectionToken, m), _ ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionToken, m)
  | HeaderValue (n, flags, cl, TransferEncodingChunked, m), ' ' ->
     HeaderValue (n + 1, flags, cl, TransferEncodingChunked, m)
  | HeaderValue (n, flags, cl, TransferEncodingChunked, m), _ ->
     HeaderValue (n + 1, flags, cl, General, m)
  | HeaderValue (n, flags, cl, ConnectionKeepAlive, m), ',' ->
     let flags = { flags with connection_keep_alive = true } in
     HeaderValue (n + 1, flags, cl, MatchingConnectionTokenStart, m)
  | HeaderValue (n, flags, cl, ConnectionClose, m), ',' ->
     let flags = { flags with connection_close = true } in
     HeaderValue (n + 1, flags, cl, MatchingConnectionTokenStart, m)
  | HeaderValue (n, flags, cl, ConnectionUpgrade, m), ',' ->
     let flags = { flags with connection_upgrade = true } in
     HeaderValue (n + 1, flags, cl, MatchingConnectionTokenStart, m)
  | HeaderValue (n, flags, cl, ConnectionKeepAlive, m), ' ' ->
     HeaderValue (n + 1, flags, cl, ConnectionKeepAlive, m)
  | HeaderValue (n, flags, cl, ConnectionClose, m), ' ' ->
     HeaderValue (n + 1, flags, cl, ConnectionClose, m)
  | HeaderValue (n, flags, cl, ConnectionUpgrade, m), ' ' ->
     HeaderValue (n + 1, flags, cl, ConnectionUpgrade, m)
  | HeaderValue (n, flags, cl, ConnectionKeepAlive, m), _
  | HeaderValue (n, flags, cl, ConnectionClose, m), _
  | HeaderValue (n, flags, cl, ConnectionUpgrade, m), _ ->
     HeaderValue (n + 1, flags, cl, MatchingConnectionToken, m)
  | HeaderValue (n, flags, cl, _, m), _ ->
     HeaderValue (n + 1, flags, cl, General, m)
  | HeaderAlmostDone (n, flags, cl, hs), '\n' ->
     HeaderValueLws (n + 1, flags, cl, hs)
  | HeaderAlmostDone (n, flags, cl, hs), _ ->
     if Config.strict then
       raise Strict;
     HeaderValueLws (n + 1, flags, cl, hs)
  | HeaderValueLws (n, flags, cl, hs), ' '
  | HeaderValueLws (n, flags, cl, hs), '\t' ->
     parse_message_char callbacks data (HeaderValueStart (n, flags, cl, hs)) p
  | HeaderValueLws (n, flags, cl, ConnectionKeepAlive), _ ->
     let flags = { flags with connection_keep_alive = true } in
     parse_message_char callbacks data (HeaderFieldStart (flags, cl)) p
  | HeaderValueLws (n, flags, cl, ConnectionClose), _ ->
     let flags = { flags with connection_close = true } in
     parse_message_char callbacks data (HeaderFieldStart (flags, cl)) p
  | HeaderValueLws (n, flags, cl, TransferEncodingChunked), _ ->
     let flags = { flags with chunked = true } in
     parse_message_char callbacks data (HeaderFieldStart (flags, cl)) p
  | HeaderValueLws (n, flags, cl, ConnectionUpgrade), _ ->
     let flags = { flags with connection_upgrade = true } in
     parse_message_char callbacks data (HeaderFieldStart (flags, cl)) p
  | HeaderValueLws (n, flags, cl, _), _ ->
     parse_message_char callbacks data (HeaderFieldStart (flags, cl)) p
  | HeaderValueDiscardWsAlmostDone (n, flags, cl, hs), ch ->
     strict_check (ch = '\n');
     HeaderValueDiscardLws (n, flags, cl, hs)
  | HeadersAlmostDone (flags, cl), ch when flags.trailing ->
     strict_check (ch = '\n');
     callbacks.on_chunk_complete ();
     parse_message_char callbacks data MessageDone p
  | HeadersAlmostDone (flags, cl), ch ->
     strict_check (ch = '\n');
     callbacks.on_headers_complete ();
     parse_message_char callbacks data (HeadersDone (flags, cl)) p
  | HeadersDone ({ upgrade = true; connection_upgrade = true; _ } as flags, _), _
  | HeadersDone ({ chunked = true; _ } as flags, Some _), _ ->
     (* Exit, the rest of the message is in a different protocol. *)
     callbacks.on_message_complete ();
     new_message flags
  | HeadersDone ({ chunked = true; _ }, _), _ -> ChunkSizeStart
  | HeadersDone (flags, Some 0), _
  | HeadersDone (flags, None), _ ->
     callbacks.on_message_complete ();
     new_message flags
  | HeadersDone (flags, Some cl), _ -> BodyIdentity (cl, p)
  | BodyIdentity (0, m), _ ->
     callbacks.on_body data m (p - 1);
     parse_message_char callbacks data MessageDone p
  | BodyIdentity (i, m), _ -> BodyIdentity (i - 1, m)
  | BodyIdentityEof m, _ -> BodyIdentityEof m
  | MessageDone, _ ->
     callbacks.on_message_complete ();
     MessageDone
  | ChunkSizeStart, ch ->
     let unhex_val = unhex ch in
     if unhex_val = -1 then
       raise InvalidChunkSize;
     ChunkSize unhex_val
  | ChunkSize size, '\r' -> ChunkSizeAlmostDone size
  | ChunkSize size, ';'
  | ChunkSize size, ' ' -> ChunkParameters size
  | ChunkSize size, ch ->
     let unhex_val = unhex ch in
     if unhex_val = -1 then
       raise InvalidChunkSize;
     let size = 16 * size + unhex_val in
     if Config.max_chunk_size < size then
       raise InvalidChunkSize;
     ChunkSize size
  | ChunkParameters size, '\r' -> ChunkSizeAlmostDone size
  | ChunkParameters size, _ -> ChunkParameters size
  | ChunkSizeAlmostDone 0, ch ->
     strict_check (ch = '\n');
     callbacks.on_chunk_header ();
     HeaderFieldStart (trailing_flags, None)
  | ChunkSizeAlmostDone size, ch ->
     strict_check (ch = '\n');
     callbacks.on_chunk_header ();
     ChunkData (size, p)
  | ChunkData (0, m), ch ->
     ChunkDataAlmostDone m
  | ChunkData (size, m), ch ->
     ChunkData (size - 1, m)
  | ChunkDataAlmostDone m, ch ->
     strict_check (ch = '\r');
     callbacks.on_body data m p;
     ChunkDataDone
  | ChunkDataDone, ch ->
     strict_check (ch = '\n');
     callbacks.on_chunk_complete ();
     ChunkSizeStart
				   
let reset_mark parser data p =
  let state = parser.state in
  let callbacks = parser.callbacks in
  let p = p + 1 in
  match state with
  | Method (n, m) ->
     callbacks.on_method data m p;
     Method (n, 0)		 
  | Asterisk m ->
     Asterisk 0
  | Uri (n, m) ->
     callbacks.on_uri data m p;
     Uri (n, 0)
  | Major (n, m) ->
     callbacks.on_version_major data m p;
     Major (n, 0)
  | Minor (n, m) ->
     callbacks.on_version_minor data m p;
     Minor (n, 0)				
  | HeaderField (n, f, cl, hs, m) ->
     callbacks.on_header_field data m p;
     HeaderField (n, f, cl, hs, 0)
  | HeaderValue (n, f, cl, hs, m) ->
     callbacks.on_header_value data m p;
     HeaderValue (n, f, cl, hs, 0)
  | ChunkData (s, m) ->
     callbacks.on_body data m p;
     ChunkData (s, 0)
  | ChunkDataAlmostDone m ->
     ChunkDataAlmostDone 0
  | BodyIdentity (i, m) ->
     callbacks.on_body data m p;
     BodyIdentity (i, 0)
  | BodyIdentityEof m ->
     callbacks.on_body data m p;
     BodyIdentityEof 0
  | s -> s

let execute parser data len =
  let last_place = len - 1 in
  let places = 0 -- last_place in
  let parse_char = parse_message_char parser.callbacks data in
  let state = fold parse_char parser.state places in
  parser.state <- state;
  parser.nread <- parser.nread + len;
  let state = reset_mark parser data last_place in
  parser.state <- state

let is_message_done parser =
  match parser.state with
  | MessageDone -> true
  | _ -> false

let state_to_string = function
  | Dead -> "Dead"
  | Start -> "Start"
  | MethodStart -> "MethodStart"
  | Method _ -> "Method"
  | SpacesBeforeUri -> "SpacesBeforeUri"
  | Asterisk _ -> "Asterisk"
  | Uri _ -> "Uri"
  | HttpStart -> "HttpStart"
  | H -> "H"
  | Ht -> "Ht"
  | Htt -> "Htt"
  | Http -> "Http"
  | MajorFirst -> "MajorFirst"
  | Major _ -> "Major"
  | MinorFirst -> "MinorFirst"
  | Minor _ -> "Minor"
  | LineAlmostDone -> "LineAlmostDone"
  | HeaderFieldStart _ -> "HeaderFieldStart"
  | HeaderField _ -> "HeaderField"
  | HeaderValueDiscardWs _ -> "HeaderValueDiscardWs"
  | HeaderValueDiscardWsAlmostDone _ -> "HeaderValueDiscardWsAlmostDone"
  | HeaderValueDiscardLws _ -> "HeaderValueDiscardLws"
  | HeaderValueStart _ -> "HeaderValueStart"
  | HeaderValue _ -> "HeaderValue"
  | HeaderValueLws _ -> "HeaderValueLws"
  | HeaderAlmostDone _ -> "HeaderAlmostDone"
  | ChunkSizeStart -> "ChunkSizeStart"
  | ChunkSize _ -> "ChunkSize"
  | ChunkParameters _ -> "ChunkParameters"
  | ChunkSizeAlmostDone _ -> "ChunkSizeAlmostDone"
  | HeadersAlmostDone _ -> "HeadersAlmostDone"
  | HeadersDone _ -> "HeadersDone"
  | ChunkData _ -> "ChunkData"
  | ChunkDataAlmostDone _ -> "ChunkDataAlmostDone"
  | ChunkDataDone -> "ChunkDataDone"
  | BodyIdentity _ -> "BodyIdentity"
  | BodyIdentityEof _ -> "BodyIdentityEof"
  | MessageDone -> "MessageDone"
	  
let to_string parser =
  state_to_string parser.state
