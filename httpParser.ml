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
  | MatchingConnection
  | MatchingContentLength
  | MatchingTransferEncoding
  | MatchingUpgrade
  | Connection
  | ContentLength
  | TransferEncoding
  | Upgrade
  | MatchingTransferEncodingChunked
  | MatchingConnectionTokenStart
  | MatchingConnectionKeepAlive
  | MatchingConnectionClose
  | MatchingConnectionUpgrade
  | MatchingConnectionToken
  | TransferEncodingChunked
  | ConnectionKeepAlive
  | ConnectionClose
  | ConnectionUpgrade
      
type state =
  | Dead
  | Start
  | MethodStart
  | Method
  | SpacesBeforeUri
  | Asterisk
  | Uri
  | HttpStart
  | H
  | Ht
  | Htt
  | Http
  | MajorFirst
  | Major
  | MinorFirst
  | Minor
  | LineAlmostDone
  | HeaderFieldStart
  | HeaderField
  | HeaderValueDiscardWs
  | HeaderValueDiscardWsAlmostDone
  | HeaderValueDiscardLws
  | HeaderValueStart
  | HeaderValue
  | HeaderValueLws
  | HeaderAlmostDone
  | ChunkSizeStart
  | ChunkSize
  | ChunkParameters
  | ChunkSizeAlmostDone
  | HeadersAlmostDone
  | HeadersDone
  | ChunkData
  | ChunkDataAlmostDone
  | ChunkDataDone
  | BodyIdentity
  | BodyIdentityEof
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
    mutable mark : int;
    mutable count : int;
    mutable content_length : int;
    
    mutable header_state : header_state;
    mutable header_index : int;
    
    mutable chunked : bool;
    mutable connection_keep_alive : bool;
    mutable connection_close : bool;
    mutable connection_upgrade : bool;
    mutable trailing : bool;
    mutable upgrade : bool;

    callbacks : callbacks;
  }

let make callbacks =
  {
    state = Start;
    nread = 0;
    mark = 0;
    count = 0;
    content_length = -1;
    header_state = General;
    header_index = 0;
    chunked = false;
    connection_keep_alive = false;
    connection_close = false;
    connection_upgrade = false;
    trailing = false;
    upgrade = false;
    callbacks = callbacks
  }

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

let new_message parser =
  parser.state <- if Config.strict && parser.connection_close then
		    Dead
		  else
		    Start

let digit ch = Char.code ch - Char.code '0'

let count_up parser =
  parser.count <- parser.count + 1

let count_down parser =
  parser.count <- parser.count - 1

let empty_flags parser =
  parser.chunked <- false;
  parser.connection_keep_alive <- false;
  parser.connection_close <- false;
  parser.connection_upgrade <- false;
  parser.trailing <- false;
  parser.upgrade <- false
			 
let rec parse_char parser data p =
  let callbacks = parser.callbacks in
  let ch = String.get data p in
  match parser.state, ch with
  (* this state is used after a 'Connection: close' message
   * the parser will error out if it reads another message
   *)
  | Dead, '\r'
  | Dead, '\n' -> ()
  | Dead, _ -> raise ClosedConnection
  | Start, '\r' -> ()
  | Start, '\n' -> ()
  | Start, _ ->
     callbacks.on_message_begin ();
     parser.state <- MethodStart;
     parse_char parser data p;
  | MethodStart, ch when is_token ch ->
     parser.state <- Method;
     parser.count <- 1;
     parser.mark <- p;
  | MethodStart, _ -> raise InvalidMethod
  | Method, _ when parser.count >= Config.max_method_size -> raise InvalidMethod
  | Method, ' ' ->
     callbacks.on_method data parser.mark p;
     parser.state <- SpacesBeforeUri;
  | Method, ch when is_token ch ->
     count_up parser;
  | Method, _ -> raise InvalidMethod
  | SpacesBeforeUri, ' ' ->
     parser.state <- SpacesBeforeUri;
  | SpacesBeforeUri, '*' ->
     parser.state <- Asterisk;
     parser.mark <- p;
  | SpacesBeforeUri, ch when is_uri_char ch ->
     parser.state <- Uri;
     parser.count <- 1;
     parser.mark <- p;
  | SpacesBeforeUri, _ -> raise InvalidUri
  | Asterisk, ' ' ->
     callbacks.on_uri data parser.mark p;
     parser.state <- HttpStart;
  | Asterisk, _ -> raise InvalidUri
  | Uri, _ when parser.count >= Config.max_uri_size -> raise InvalidUri
  | Uri, ' ' ->
     callbacks.on_uri data parser.mark p;
     parser.state <- HttpStart;
  | Uri, ch when is_uri_char ch ->
     count_up parser;
  | Uri, _ -> raise InvalidUri
  | HttpStart, 'H' ->
     parser.state <- H;
  | HttpStart, ' ' ->
     parser.state <- HttpStart;
  | HttpStart, _ -> raise InvalidConstant
  | H, ch ->
     strict_check (ch = 'T');
     parser.state <- Ht;
  | Ht, ch ->
     strict_check (ch = 'T');
     parser.state <- Htt;
  | Htt, ch ->
     strict_check (ch = 'P');
     parser.state <- Http;
  | Http, ch ->
     strict_check (ch = '/');
     parser.state <- MajorFirst;
  | MajorFirst, ch when is_num ch ->
     parser.state <- Major;
     parser.count <- 1;
     parser.mark <- p;
  | MajorFirst, _ -> raise InvalidVersion
  | Major, _ when parser.count >= Config.max_version_digit_size -> raise InvalidVersion
  | Major, '.' ->
     callbacks.on_version_major data parser.mark p;
     parser.state <- MinorFirst;
  | Major, ch when is_num ch ->
     count_up parser;
  | Major, _ -> raise InvalidVersion
  | MinorFirst, ch when is_num ch ->
     parser.state <- Minor;
     parser.count <- 1;
     parser.mark <- p;
  | MinorFirst, _ -> raise InvalidVersion
  | Minor, _ when parser.count >= Config.max_version_digit_size -> raise InvalidVersion
  | Minor, '\r' ->
     callbacks.on_version_minor data parser.mark p;
     parser.state <- LineAlmostDone;
  | Minor, '\n' ->
     callbacks.on_version_minor data parser.mark p;
     parser.state <- HeaderFieldStart;
     empty_flags parser;
     parser.content_length <- -1;
  | Minor, ch when is_num ch ->
     count_up parser;
  | Minor, _ -> raise InvalidVersion
  | LineAlmostDone, '\n' ->
     parser.state <- HeaderFieldStart;
     empty_flags parser;
     parser.content_length <- -1;
  | LineAlmostDone, _ -> raise LfExpected
  | HeaderField, _
  | HeaderValueDiscardWs, _
  | HeaderValueDiscardWsAlmostDone, _
  | HeaderValueDiscardLws, _
  | HeaderValueStart, _
  | HeaderValue, _
  | HeaderValueLws, _
  | HeaderAlmostDone, _ when parser.count >= Config.max_header_size ->
     raise HeaderOverflow
  | HeaderFieldStart, '\r' ->
     parser.state <- HeadersAlmostDone;
  | HeaderFieldStart, '\n' ->
     (* they might be just sending \n instead of \r\n
      *  so this would be the second \n
      *  to denote the end of headers
      *)
     parser.state <- HeadersAlmostDone;
     parse_char parser data p;
  | HeaderFieldStart, ch when not (is_token ch) -> raise InvalidHeaderToken
  | HeaderFieldStart, ch when token ch = 'c' ->
     parser.state <- HeaderField;
     parser.count <- 1;
     parser.mark <- p;
     parser.header_state <- C;
  | HeaderFieldStart, ch when token ch = 't' ->
     parser.state <- HeaderField;
     parser.count <- 1;
     parser.mark <- p;
     parser.header_state <- MatchingTransferEncoding;
     parser.header_index <- 0;
  | HeaderFieldStart, ch when token ch = 'u' ->
     parser.state <- HeaderField;
     parser.count <- 1;
     parser.mark <- p;
     parser.header_state <- MatchingUpgrade;
     parser.header_index <- 0;
  | HeaderFieldStart, _ ->
     parser.state <- HeaderField;
     parser.count <- 1;
     parser.mark <- p;
     parser.header_state <- General;
  | HeaderField, ':' ->
     callbacks.on_header_field data parser.mark p;
     parser.state <- HeaderValueDiscardWs;
     count_up parser;
  | HeaderField, ch when not (is_token ch) -> raise InvalidHeaderToken
  | HeaderField, _ ->
     count_up parser;
     begin
       match parser.header_state, ch with
       | General, _ -> ()
       | C, ch when token ch = 'o' ->
	  parser.header_state <- Co;
       | Co, ch when token ch = 'n' ->
	  parser.header_state <- Con;
       | Con, ch when token ch = 't' ->
	  parser.header_state <- MatchingContentLength;
	  parser.header_index <- 3;
       | MatchingContentLength, ch
	    when parser.header_index > String.length connection - 1
		 || token ch != String.get connection parser.header_index ->
	  parser.header_state <- General;
       | MatchingContentLength, _
	    when parser.header_index = String.length connection - 2 ->
	  parser.header_state <- ContentLength
       | MatchingConnection, ch
	    when parser.header_index > String.length connection - 1
		 || token ch != String.get connection parser.header_index ->
	  parser.header_state <- General;
       | MatchingConnection, _
	    when parser.header_index = String.length connection - 2->
	  parser.header_state <- Connection;
       | MatchingContentLength, ch
	    when parser.header_index > String.length content_length - 1
		 || token ch != String.get content_length parser.header_index ->
	  parser.header_state <- General;
       | MatchingContentLength, _
	    when parser.header_index = String.length content_length - 2 ->
	  parser.header_state <- ContentLength;
       | MatchingTransferEncoding, ch
	    when parser.header_index > String.length transfer_encoding - 1
		 || token ch != String.get transfer_encoding parser.header_index ->
	  parser.header_state <- General;
       | MatchingTransferEncoding, _
	    when parser.header_index = String.length transfer_encoding - 2 ->
	  parser.header_state <- TransferEncoding;
       | MatchingUpgrade, ch
	    when parser.header_index > String.length upgrade - 1
		 || token ch != String.get upgrade parser.header_index ->
	  parser.header_state <- General;
       | MatchingUpgrade, _
	    when parser.header_index = String.length upgrade - 2 ->
	  parser.header_state <- Upgrade;
       | Connection, ' '
       | ContentLength, ' '
       | TransferEncoding, ' '
       | Upgrade, ' ' ->
	  parser.header_state <- General;
       | _, _ -> ()
     end
  | HeaderValueDiscardWs, ' '
  | HeaderValueDiscardWs, '\t' ->
     count_up parser;
  | HeaderValueDiscardWs, '\r' ->
     parser.state <- HeaderValueDiscardWsAlmostDone;
     count_up parser;
  | HeaderValueDiscardWs, '\n' ->
     parser.state <- HeaderValueDiscardLws;
     count_up parser;
  | HeaderValueDiscardWs, _ ->
     parser.state <- HeaderValueStart;
     parse_char parser data p;
  | HeaderValueDiscardLws, ' '
  | HeaderValueDiscardLws, '\t' ->
     parser.state <- HeaderValueStart;
     parse_char parser data p;
  | HeaderValueDiscardLws, _ ->
     parser.state <- HeaderFieldStart;
     begin
       match parser.header_state with
       | ConnectionKeepAlive ->
	  parser.connection_keep_alive <- true;
       | ConnectionClose ->
	  parser.connection_close <- true;
       | TransferEncodingChunked ->
	  parser.chunked <- true;
       | ConnectionUpgrade ->
	  parser.connection_upgrade <- true;
       | _ -> ()
     end;
  | HeaderValueStart, _ ->
     parser.state <- HeaderValue;
     count_up parser;
     begin
       match parser.header_state with
       | Upgrade ->
	  parser.upgrade <- true;
	  parser.header_state <- General;
       | TransferEncoding when Char.lowercase ch = 'c' ->
	  parser.header_state <- MatchingTransferEncodingChunked;
	  parser.header_index <- 1;
       | TransferEncoding ->
	  parser.header_state <- General
       | ContentLength when not (is_num ch) -> raise InvalidContentLength
       | ContentLength ->
	  parser.content_length <- digit ch;
       | Connection when Char.lowercase ch = 'k' ->
	  parser.header_state <- MatchingConnectionKeepAlive;
	  parser.header_index <- 1;
       | Connection when Char.lowercase ch = 'c' ->
	  parser.header_state <- MatchingConnectionClose;
	  parser.header_index <- 1;
       | Connection when Char.lowercase ch = 'u' ->
	  parser.header_state <- MatchingConnectionUpgrade;
	  parser.header_index <- 1;
       | Connection ->
	  parser.header_state <- MatchingConnectionToken;
       | MatchingConnectionTokenStart ->
	  parser.header_state <- MatchingConnectionTokenStart;
       | _ ->
	  parser.header_state <- General;
     end;
  | HeaderValue, '\r' ->
     callbacks.on_header_value data parser.mark p;
     parser.state <- HeaderAlmostDone;
     count_up parser;
  | HeaderValue, '\n' ->
     callbacks.on_header_value data parser.mark p;
     parser.state <- HeaderAlmostDone;
     parse_char parser data p;
  | HeaderValue, _ ->
     count_up parser;
     begin
       match parser.header_state, ch with
       | General, _ -> ()
       | ContentLength, ' ' when parser.content_length > 0 -> ()
       | ContentLength, ch when not (is_num ch) && parser.content_length > 0 ->
	  raise InvalidContentLength
       | ContentLength, ch when parser.content_length > 0 ->
	  let content_length = 10 * parser.content_length + digit ch in
	  if Config.max_content_size < content_length then
	    raise InvalidContentLength;
	  parser.content_length <- content_length;
       | MatchingTransferEncodingChunked, ch
	    when parser.header_index > String.length chunked - 1 ||
		   Char.lowercase ch != String.get chunked parser.header_index ->
	  parser.header_state <- General;
       | MatchingTransferEncodingChunked, _
	    when parser.header_index == String.length chunked - 2 ->
	  parser.header_state <- TransferEncodingChunked;
       | MatchingTransferEncodingChunked, _ ->
	  parser.header_index <- parser.header_index + 1;
       | MatchingConnectionTokenStart, ch when Char.lowercase ch = 'k' ->
	  parser.header_state <- MatchingConnectionKeepAlive;
	  parser.header_index <- 1;
       | MatchingConnectionTokenStart, ch when Char.lowercase ch = 'c' ->
	  parser.header_state <- MatchingConnectionClose;
	  parser.header_index <- 1;
       | MatchingConnectionTokenStart, ch when Char.lowercase ch = 'u' ->
	  parser.header_state <- MatchingConnectionUpgrade;
	  parser.header_index <- 1;
       | MatchingConnectionTokenStart, ch when strict_token ch != invalid_token ->
	  parser.header_state <- MatchingConnectionToken;
       | MatchingConnectionTokenStart, ' '
       | MatchingConnectionTokenStart, '\t' -> ()
       | MatchingConnectionTokenStart, _ ->
	  parser.header_state <- General;
       | MatchingConnectionKeepAlive, ch
	    when parser.header_index > String.length keep_alive - 1 ||
		   Char.lowercase ch != String.get keep_alive parser.header_index ->
	  parser.header_state <- MatchingConnectionToken;
       | MatchingConnectionKeepAlive, _
	    when parser.header_index = String.length keep_alive - 2 ->
	  parser.header_state <- ConnectionKeepAlive;
       | MatchingConnectionKeepAlive, _ ->
	  parser.header_index <- parser.header_index + 1;
       | MatchingConnectionClose, ch
	    when parser.header_index > String.length close - 1 ||
		   Char.lowercase ch != String.get close parser.header_index ->
	  parser.header_state <- MatchingConnectionToken;
       | MatchingConnectionClose, _
	    when parser.header_index = String.length close - 2 ->
	  parser.header_state <- ConnectionClose;
       | MatchingConnectionClose, _ ->
	  parser.header_index <- parser.header_index + 1;
       | MatchingConnectionUpgrade, ch
	    when parser.header_index > String.length upgrade - 1 ||
		   Char.lowercase ch != String.get upgrade parser.header_index ->
	  parser.header_state <- MatchingConnectionToken;
       | MatchingConnectionUpgrade, _
	    when parser.header_index = String.length upgrade - 2 ->
	  parser.header_state <- ConnectionUpgrade;
       | MatchingConnectionUpgrade, _ ->
	  parser.header_index <- parser.header_index + 1;
       | MatchingConnectionToken, ',' ->
	  parser.header_state <- MatchingConnectionTokenStart;
       | MatchingConnectionToken, _ ->
	  parser.header_state <- MatchingConnectionToken;
       | TransferEncodingChunked, ' ' -> ()
       | TransferEncodingChunked, _ ->
	  parser.header_state <- General;
       | ConnectionKeepAlive, ',' ->
	  parser.header_state <- MatchingConnectionTokenStart;
	  parser.connection_keep_alive <- true;
       | ConnectionClose, ',' ->
	  parser.header_state <- MatchingConnectionTokenStart;
	  parser.connection_close <- true;
       | ConnectionUpgrade, ',' ->
	  parser.header_state <- MatchingConnectionTokenStart;
	  parser.connection_upgrade <- true;
       | ConnectionKeepAlive, ' ' ->
	  parser.header_state <- ConnectionKeepAlive;
       | ConnectionClose, ' ' ->
	  parser.header_state <- ConnectionClose;
       | ConnectionUpgrade, ' ' ->
	  parser.header_state <- ConnectionUpgrade;
       | ConnectionKeepAlive, _
       | ConnectionClose, _
       | ConnectionUpgrade, _ ->
	  parser.header_state <- MatchingConnectionToken;
       | _, _ ->
	  parser.header_state <- General;
     end;
  | HeaderAlmostDone, '\n' ->
     parser.state <- HeaderValueLws;
     count_up parser;
  | HeaderAlmostDone, _ ->
     if Config.strict then
       raise Strict;
     parser.state <- HeaderValueLws;
     count_up parser;
  | HeaderValueLws, ' '
  | HeaderValueLws, '\t' ->
     parser.state <- HeaderValueStart;
     parse_char parser data p;
  | HeaderValueLws, _ ->
     begin
       match parser.header_state with
       | ConnectionKeepAlive ->
	  parser.connection_keep_alive <- true;
       | ConnectionClose ->
	  parser.connection_close <- true;
       | TransferEncodingChunked ->
	  parser.chunked <- true;
       | ConnectionUpgrade ->
	  parser.connection_upgrade <- true;
       | _ -> ()
     end;
     parser.state <- HeaderFieldStart;
     parse_char parser data p;
  | HeaderValueDiscardWsAlmostDone, ch ->
     strict_check (ch = '\n');
     parser.state <- HeaderValueDiscardLws;
  | HeadersAlmostDone, ch when parser.trailing ->
     strict_check (ch = '\n');
     callbacks.on_chunk_complete ();
     parser.state <- MessageDone;
     parse_char parser data p;
  | HeadersAlmostDone, ch ->
     strict_check (ch = '\n');
     callbacks.on_headers_complete ();
     parser.state <- HeadersDone;
     parse_char parser data p;
  | HeadersDone, _
  | HeadersDone, _
       when (parser.upgrade && parser.connection_upgrade)
	    || (parser.chunked && parser.content_length > 0) ->
     (* Exit, the rest of the message is in a different protocol. *)
     callbacks.on_message_complete ();
     new_message parser;
  | HeadersDone, _ when parser.chunked ->
     parser.state <- ChunkSizeStart;
  | HeadersDone, _ when parser.content_length <= 0 ->
     callbacks.on_message_complete ();
     new_message parser;
  | HeadersDone, _ ->
     parser.state <- BodyIdentity;
  | BodyIdentity, _ when parser.count = 0 ->
     callbacks.on_body data parser.mark (p - 1);
     parser.state <- MessageDone;
     parse_char parser data p;
  | BodyIdentity, _ ->
     count_down parser;
  | BodyIdentityEof, _ -> ()
  | MessageDone, _ ->
     callbacks.on_message_complete ();
  | ChunkSizeStart, ch ->
     let unhex_val = unhex ch in
     if unhex_val = -1 then
       raise InvalidChunkSize;
     parser.state <- ChunkSize;
     parser.count <- unhex_val;
  | ChunkSize, '\r' ->
     parser.state <- ChunkSizeAlmostDone;
  | ChunkSize, ';'
  | ChunkSize, ' ' ->
     parser.state <- ChunkParameters;
  | ChunkSize, ch ->
     let unhex_val = unhex ch in
     if unhex_val = -1 then
       raise InvalidChunkSize;
     let size = 16 * parser.count + unhex_val in
     if Config.max_chunk_size < size then
       raise InvalidChunkSize;
     parser.count <- size;
  | ChunkParameters, '\r' ->
     parser.state <- ChunkSizeAlmostDone;
  | ChunkParameters, _ ->
     parser.state <- ChunkParameters;
  | ChunkSizeAlmostDone, ch when parser.count = 0 ->
     strict_check (ch = '\n');
     callbacks.on_chunk_header ();
     parser.state <- HeaderFieldStart;
     empty_flags parser;
     parser.trailing <- true;
     parser.content_length <- -1;
  | ChunkSizeAlmostDone, ch ->
     strict_check (ch = '\n');
     callbacks.on_chunk_header ();
     parser.state <- ChunkData;
     parser.mark <- p;
  | ChunkData, ch when parser.count = 0 ->
     parser.state <- ChunkDataAlmostDone;
  | ChunkData, ch ->
     count_down parser;
  | ChunkDataAlmostDone, ch ->
     strict_check (ch = '\r');
     callbacks.on_body data parser.mark p;
     parser.state <- ChunkDataDone;
  | ChunkDataDone, ch ->
     strict_check (ch = '\n');
     callbacks.on_chunk_complete ();
     parser.state <- ChunkSizeStart
				   
let reset_mark parser data p =
  let callbacks = parser.callbacks in
  let p = p + 1 in
  begin
    match parser.state with
    | Method -> callbacks.on_method data parser.mark p;
    | Uri -> callbacks.on_uri data parser.mark p;
    | Major -> callbacks.on_version_major data parser.mark p;
    | Minor -> callbacks.on_version_minor data parser.mark p;
    | HeaderField -> callbacks.on_header_field data parser.mark p;
    | HeaderValue -> callbacks.on_header_value data parser.mark p;
    | ChunkData -> callbacks.on_body data parser.mark p;
    | BodyIdentity -> callbacks.on_body data parser.mark p;
    | BodyIdentityEof -> callbacks.on_body data parser.mark p;
    | _ -> ()
  end;
  parser.mark <- 0

let execute parser data len =
  let last = len - 1 in
  for i = 0 to last do
    parse_char parser data i
  done;
  parser.nread <- parser.nread + len;
  reset_mark parser data last

let is_message_done parser =
  match parser.state with
  | MessageDone -> true
  | _ -> false

let state_to_string = function
  | Dead -> "Dead"
  | Start -> "Start"
  | MethodStart -> "MethodStart"
  | Method -> "Method"
  | SpacesBeforeUri -> "SpacesBeforeUri"
  | Asterisk -> "Asterisk"
  | Uri -> "Uri"
  | HttpStart -> "HttpStart"
  | H -> "H"
  | Ht -> "Ht"
  | Htt -> "Htt"
  | Http -> "Http"
  | MajorFirst -> "MajorFirst"
  | Major -> "Major"
  | MinorFirst -> "MinorFirst"
  | Minor -> "Minor"
  | LineAlmostDone -> "LineAlmostDone"
  | HeaderFieldStart -> "HeaderFieldStart"
  | HeaderField -> "HeaderField"
  | HeaderValueDiscardWs -> "HeaderValueDiscardWs"
  | HeaderValueDiscardWsAlmostDone -> "HeaderValueDiscardWsAlmostDone"
  | HeaderValueDiscardLws -> "HeaderValueDiscardLws"
  | HeaderValueStart -> "HeaderValueStart"
  | HeaderValue -> "HeaderValue"
  | HeaderValueLws -> "HeaderValueLws"
  | HeaderAlmostDone -> "HeaderAlmostDone"
  | ChunkSizeStart -> "ChunkSizeStart"
  | ChunkSize -> "ChunkSize"
  | ChunkParameters -> "ChunkParameters"
  | ChunkSizeAlmostDone -> "ChunkSizeAlmostDone"
  | HeadersAlmostDone -> "HeadersAlmostDone"
  | HeadersDone -> "HeadersDone"
  | ChunkData -> "ChunkData"
  | ChunkDataAlmostDone -> "ChunkDataAlmostDone"
  | ChunkDataDone -> "ChunkDataDone"
  | BodyIdentity -> "BodyIdentity"
  | BodyIdentityEof -> "BodyIdentityEof"
  | MessageDone -> "MessageDone"
	  
let to_string parser =
  state_to_string parser.state
