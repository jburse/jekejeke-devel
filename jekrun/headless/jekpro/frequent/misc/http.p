/**
 * This module provides a HTTP server based on Pythonesk dispatch of
 * a server object. The class of the server object need only implement
 * a predicate dispatch/4 with the Pythonesk convention that the receiver
 * appears in the first argument. The server can be run by providing the
 * server object that will be responsible for handling HTTP requests:
 *
 * ?- run_http(&lt;object&gt;, &lt;port&gt;), fail; true.
 *
 * The server currently implements a minimal subset of the HTTP/1.1
 * protocol restricted to GET method. The server will read the request
 * line and the header lines. The server is able to gen-erate error
 * messages through the predicate dispatch_error/2. The server will
 * generate a 404 error when dispatch/4 failed. The following HTTP/1.1
 * errors have been realized:
 *
 * * 400 Bad Request: Request could not be parsed.
 * * 404 Not Found: Server object did not succeeds.
 * * 415 Unsupported Media Type: Server object could not decode parameters.
 * * 422 Unprocessable Entity: Server object could not validate parameters.
 * * 501 Not Implemented: Request method not supported.
 *
 * The predicate http_parameter/3 can be used by the server object to
 * access URI query parameters. The predicates response_text/2,
 * response_binary/2 and html_escape/2 can be used to generate dynamic
 * content by the server object. The predicates dispatch_text/3 and
 * dispatch_binary/3 can be used by the server object to deliver
 * static content.
 *
 * * 101 Switching Protocols: Server object can start web socket worker.
 * * 200 Ok: Server object delivers content and optionally meta-data.
 * * 302 Found: Server object redirects to new location.
 * * 304 Not Modified: Server object notifies that meta-data did not change.
 *
 * The web server also supports the above HTTP/1.1 codes, which might
 * have additional response headers. The predicate dispatch_upgrade/2
 * will automatically generate a web socket accept key and can be used
 * to implement upgrade/4. The predicate dispatch_redirect/2 requires a
 * location, whereas the predicate dispatch_head/3 requires meta-data.
 *
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- package(library(jekpro/frequent/misc)).

:- module(http, []).
:- use_module(library(runtime/distributed)).
:- use_module(library(misc/socket)).
:- use_module(library(stream/console)).
:- use_module(library(stream/xml)).
:- use_module(library(system/uri)).
:- use_module(library(basic/lists)).
:- use_module(library(system/domain)).
:- use_module(library(system/zone)).
:- use_module(library(misc/text)).

/***************************************************************/
/* HTTP Server                                                 */
/***************************************************************/

/**
 * run_http(O, P):
 * The predicate runs a web server with object O at port P.
 */
% run_http(+Object, +Integer)
:- public run_http/2.
run_http(Object, Port) :-
   balance((accept(Object, Port, Session), handle(Object, Session))).

/**
 * accept(O, P, S):
 * The predicate repeatedly succeeds with accepted sessions S
 * at port P for the object O.
 */
% accept(+Object, +Integer, -Socket)
:- private accept/3.
accept(Object, Port, Session) :-
   setup_call_cleanup(
      accept_new(Object, Port, Server),
      (repeat, server_accept(Server, Session)),
      accept_close(Object, Server)).

% accept_new(+Object, +Integer, -Server)
:- private accept_new/3.
accept_new(Object, Port, Server) :-
   server_new(Port, Server),
   Object::initialized(Server).

% accept_close(+Object, +Server)
:- private accept_close/2.
accept_close(Object, Server) :-
   close(Server),
   Object::destroyed(Server).

/**
 * handle(O, S):
 * The predicate handles with object O a session S.
 */
% handle(+Object, +Socket)
:- private handle/2.
handle(Object, Session) :-
   catch(receive_http(Method, URI, Header, Session), _, fail), !,
   handle_method(Object, Method, URI, Header, Session).
handle(_, Session) :-                             /* Bad Request */
   dispatch_error(400, Session).

% receive_http(-Atom, -Atom, -List, +Socket)
:- private receive_http/4.
receive_http(Method, URI, Header, Session) :-
   open(Session, read, Request, [type(binary)]),
   read_request(Request, Method, URI),
   read_header(Request, Header).

% handle_method(+Object, +Atom, +Atom, +List, +Socket)
:- private handle_method/5.
handle_method(Object, 'GET', URI, Header, Session) :- !,
   handle_get(Object, URI, Header, Session).
handle_method(_, _, _, _, Session) :-             /* Not Implemented */
   dispatch_error(501, Session).

% handle_get(+Object, +Atom, +List, +Socket)
:- private handle_get/4.
handle_get(Object, URI, Header, Session) :-
   make_link(Spec, Parameter, _, URI),
   handle_object(Object, Spec, request(Parameter, Header), Session), !.
handle_get(_, _, _, Session) :-                   /* Not Found */
   dispatch_error(404, Session).

% handle_object(+Object, +Atom, +Request, +Socket)
:- private handle_object/4.
handle_object(Object, Spec, Request, Session) :-
   http_header(Request, upgrade, websocket),
   http_header(Request, connection, 'Upgrade'), !,
   Object::upgrade(Spec, Request, Session).
handle_object(Object, Spec, Request, Session) :-
   Object::dispatch(Spec, Request, Session).

/**
 * initialized(O, S):
 * The predicate is called when the socket S
 * is initialized for object O.
 */
% initialized(+Object, +Socket)
:- public initialized/2.
initialized(_, _).

/**
 * destroyed(O, S):
 * The predicate is called when the socket S
 * is destroyed for object O.
 */
% destroyed(+Object, +Socket)
:- public destroyed/2.
destroyed(_, _).

/**
 * dispatch(O, P, R, S):
 * The predicate succeeds in dispatching the request for object
 * O, with path P, with request R and the socket S.
 */
% dispatch(+Object, +Spec, +Request, +Socket)
:- public dispatch/4.
dispatch(_, '/images/cookie.gif', Request, Session) :- !,
   dispatch_binary(library(misc/images/cookie), Request, Session).

/**
 * upgrade(O, P, R, S):
 * The predicate succeeds in upgrading the request for object
 * O, with path P, with request R and the socket S.
 */
% upgrade(+Object, +Spec, +Request, +Socket)
:- public upgrade/4.
:- static upgrade/4.

/***************************************************************/
/* HTTP Requests                                               */
/***************************************************************/

/**
 * http_parameter(R, N, V):
 * The predicate succeeds in V with the value of the parameter named N
 * from the request R.
 */
% http_parameter(+Request, +Atom, -Atom)
:- public http_parameter/3.
http_parameter(request(Parameter, _), Name, Value) :-
   member(Name-Value, Parameter).

/**
 * http_header(R, N, V):
 * The predicate succeeds in V with the value of the header named N
 * from the request R.
 */
:- public http_header/3.
http_header(request(_, Header), Name, Value) :-
   member(Name-Value, Header).

% read_request(+Stream, -Atom, -Atom)
:- private read_request/3.
read_request(Request, Method, URI) :-
   read_punch_max(Request, 1024, Punch),
   atom_block(What, Punch),
   \+ atom_length(What, 1024),
   atom_split(What, ' ', [Method, URI, _]).

% read_header(+Stream, -List)
:- private read_header/2.
read_header(Request, List) :-
   read_punch_max(Request, 1024, Punch),
   atom_block(What, Punch),
   \+ atom_length(What, 1024),
   read_header(What, Request, List).

% read_header(+Atom, +Stream, -List)
:- private read_header/3.
read_header('', _, []) :- !.
read_header(What, Request, List) :-
   make_header(What, List, List2),
   read_header(Request, List2).

% make_header(+Atom, -List, +List)
:- private make_header/3.
make_header(Line, List, List2) :-
   atom_split(Line, ': ', [Name, Rest]),
   downcase_atom(Name, Name2),
   make_header2(Name2, Rest, List, List2).

% make_header2(+Atom, +Atom, -List, +List)
:- private make_header2/4.
make_header2(Name, Rest, [Name-Rest|List], List) :-
   Name = 'if-modified-since', !.
make_header2(Name, Rest, List, List2) :-
   atom_split(Rest, ', ', Values),
   make_header3(Values, Name, List, List2).

% make_header3(+Atom, +List, -List, +List)
:- private make_header3/4.
make_header3([], _, List, List).
make_header3([Value|Rest], Name, [Name-Value|List], List2) :-
   make_header3(Rest, Name, List, List2).

/***************************************************************/
/* Error HTTP Responses                                        */
/***************************************************************/

/**
 * dispatch_error(E, O):
 * The predicate sends the error code E to the socket O. The
 * error codes from 4xx and 5xx are supported.
 */
% dispatch_error(+Integer, +Socket)
:- public dispatch_error/2.
dispatch_error(Code, Session) :-
   catch(handle_error(Code, Session), _, true).

% handle_error(+Integer, +Socket)
:- private handle_error/2.
handle_error(Code, Session) :-
   setup_call_cleanup(
      open(Session, write, Response),
      send_error(Code, Response),
      close(Response)).

/**
 * response_error(C, O):
 * Send an error code C to the text output stream O.
 */
% response_error(+Integer, +Stream)
:- private response_error/2.
response_error(400, Response) :-
   write(Response, 'HTTP/1.1 400 Bad Request\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
   make_header_date(Headers, []),
   response_text_headers(Headers, Response),
   write(Response, '\r\n').
response_error(404, Response) :-
   write(Response, 'HTTP/1.1 404 Not Found\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
   make_header_date(Headers, []),
   response_text_headers(Headers, Response),
   write(Response, '\r\n').
response_error(415, Response) :-
   write(Response, 'HTTP/1.1 415 Unsupported Media Type\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
   make_header_date(Headers, []),
   response_text_headers(Headers, Response),
   write(Response, '\r\n').
response_error(422, Response) :-
   write(Response, 'HTTP/1.1 422 Unprocessable Entity\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
   make_header_date(Headers, []),
   response_text_headers(Headers, Response),
   write(Response, '\r\n').
response_error(501, Response) :-
   write(Response, 'HTTP/1.1 501 Not Implemented\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
   make_header_date(Headers, []),
   response_text_headers(Headers, Response),
   write(Response, '\r\n').

/**
 * send_error(C, O):
 * Send an error code C page to the text output stream O.
 */
% send_error(+Integer, +Stream)
:- private send_error/2.
send_error(400, Response) :- !,
   setup_call_cleanup(
      open_resource(library(misc/pages/err400), Stream),
      (response_error(400, Response), send_lines(Stream, Response)),
      close(Stream)).
send_error(404, Response) :- !,
   setup_call_cleanup(
      open_resource(library(misc/pages/err404), Stream),
      (response_error(404, Response), send_lines(Stream, Response)),
      close(Stream)).
send_error(415, Response) :- !,
   setup_call_cleanup(
      open_resource(library(misc/pages/err415), Stream),
      (response_error(415, Response), send_lines(Stream, Response)),
      close(Stream)).
send_error(422, Response) :- !,
   setup_call_cleanup(
      open_resource(library(misc/pages/err422), Stream),
      (response_error(422, Response), send_lines(Stream, Response)),
      close(Stream)).
send_error(501, Response) :- !,
   setup_call_cleanup(
      open_resource(library(misc/pages/err501), Stream),
      (response_error(501, Response), send_lines(Stream, Response)),
      close(Stream)).

/***************************************************************/
/* Text HTTP Response                                          */
/***************************************************************/

/**
 * response_text(C, H, O):
 * Send an OK response C with meta-data headers H to the text
 * output stream O.
 */
% response_text(+Integer, +List, +Stream)
:- public response_text/3.
response_text(200, Headers, Response) :-
   write(Response, 'HTTP/1.1 200 OK\r\n'),
   make_header_date(Headers2, Headers),
   response_text_headers(Headers2, Response),
   write(Response, '\r\n').

% response_text_headers(+List, +Stream)
:- private response_text_headers/2.
response_text_headers(X, _) :- var(X),
   throw(error(instantiation_error, _)).
response_text_headers([], _) :- !.
response_text_headers([Name-Value|Rest], Response) :- !,
   write(Response, Name),
   write(Response, ': '),
   write(Response, Value),
   write(Response, '\r\n'),
   response_text_headers(Rest, Response).
response_text_headers(X, _) :-
   throw(error(type_error(list, X), _)).

/**
 * dispatch_text(F, R, O):
 * The predicate sends the text resource F for request R to the socket O.
 * Mime type determined from resource. Meta data determined from resource
 * and validated with request conditions.
 */
% dispatch_text(+File, +Request, +Socket)
:- public dispatch_text/3.
dispatch_text(File, Request, Session) :-
   catch(meta_text(File, Headers), _, fail),
   dispatch_text(File, Request, Headers, Session).

% dispatch_text(+File, +Request, +List, +Socket)
:- private dispatch_text/4.
dispatch_text(File, Request, Headers, Session) :-
   validate_meta(Request, Headers), !,
   catch(handle_text(File, Headers, Session), _, true).
dispatch_text(_, _, Headers, Session) :-
   dispatch_head(304, Headers, Session).

% handle_text(+File, +List, +Socket)
:- private handle_text/3.
handle_text(File, Headers, Session) :-
   setup_call_cleanup(
      open(Session, write, Response),
      send_text(File, Headers, Response),
      close(Response)).

% send_text(+File, +List, +Stream)
:- private send_text/3.
send_text(File, Headers, Response) :-
   setup_call_cleanup(
      open_resource(File, Stream),
      (response_text(200, Headers, Response),
      send_lines(Stream, Response)),
      close(Stream)).

% send_lines(+Stream, +Stream)
:- private send_lines/2.
send_lines(Stream, Response) :-
   read_line_max(Stream, 1024, Line), !,
   write(Response, Line),
   (  \+ atom_length(Line, 1024)
   -> write(Response, '\r\n'),
      send_lines(Stream, Response)
   ;  send_lines2(Stream, Response)).
send_lines(_, _).

% send_lines2(+Stream, +Stream)
:- private send_lines2/2.
send_lines2(Stream, Response) :-
   read_line_max(Stream, 1024, Line), !,
   write(Response, Line),
   (  \+ atom_length(Line, 1024)
   -> write(Response, '\r\n'),
      send_lines(Stream, Response)
   ;  send_lines2(Stream, Response)).
send_lines2(_, Response) :-
   write(Response, '\r\n').

/***************************************************************/
/* Binary HTTP Response                                        */
/***************************************************************/

/**
 * response_binary(C, H, O):
 * Send an OK response C with meta-data headers H to the binary
 * output stream O. The OK responses from 2xx and 3xx are supported.
 */
% response_binary(+Integer, +List, +Stream)
:- public response_binary/3.
response_binary(200, Headers, Response) :-
   write_atom(Response, 'HTTP/1.1 200 OK\r\n'),
   make_header_date(Headers2, Headers),
   response_binary_headers(Headers2, Response),
   write_atom(Response, '\r\n').
response_binary(304, Headers, Response) :-
   write_atom(Response, 'HTTP/1.1 304 Not Modified\r\n'),
   make_header_date(Headers2, Headers),
   response_binary_headers(Headers2, Response),
   write_atom(Response, '\r\n').

% response_binary_headers(+List, +Stream)
:- private response_binary_headers/2.
response_binary_headers(X, _) :- var(X),
   throw(error(instantiation_error, _)).
response_binary_headers([], _) :- !.
response_binary_headers([Name-Value|Rest], Response) :- !,
   write_atom(Response, Name),
   write_atom(Response, ': '),
   write_atom(Response, Value),
   write_atom(Response, '\r\n'),
   response_binary_headers(Rest, Response).
response_binary_headers(X, _) :-
   throw(error(type_error(list, X), _)).

% write_atom(+Stream, +Atom)
:- private write_atom/2.
write_atom(Response, Atom) :-
   atom_block(Atom, Block),
   write_block(Response, Block).

/**
 * dispatch_binary(F, R, O):
 * The predicate sends  the binary resource F for request R to the socket O.
 * Mime type determined from resource. Meta data determined from resource and
 * validated with request conditions.
 */
% dispatch_binary(+File, +Request, +Socket)
:- public dispatch_binary/3.
dispatch_binary(File, Request, Session) :-
   catch(meta_binary(File, Headers), _, fail),
   dispatch_binary(File, Request, Headers, Session).

% dispatch_binary(+File, +Request, +List, +Socket)
:- private dispatch_binary/4.
dispatch_binary(File, Request, Headers, Session) :-
   validate_meta(Request, Headers), !,
   catch(handle_binary(File, Headers, Session), _, true).
dispatch_binary(_, _, Headers, Session) :-
   dispatch_head(304, Headers, Session).

% handle_binary(+File, +List, +Socket)
:- private handle_binary/3.
handle_binary(File, Headers, Session) :-
   setup_call_cleanup(
      open(Session, write, Response, [type(binary)]),
      send_binary(File, Headers, Response),
      close(Response)).

% send_binary(+File, +List, +Stream)
:- private send_binary/3.
send_binary(File, Headers, Response) :-
   setup_call_cleanup(
      open_resource(File, Stream, [type(binary)]),
      (response_binary(200, Headers, Response),
      send_blocks(Stream, Response)),
      close(Stream)).

% send_blocks(+Stream, +Stream)
:- private send_blocks/2.
send_blocks(Stream, Response) :-
   read_block(Stream, 1024, Block), !,
   write_block(Response, Block),
   send_blocks(Stream, Response).
send_blocks(_, _).

/***************************************************************/
/* Special HTTP Responses                                      */
/***************************************************************/

/**
 * dispatch_upgrade(R, O):
 * Send an upgrade response from request R to the socket O.
 */
% dispatch_upgrade(+Request, +Socket)
:- public dispatch_upgrade/2.
dispatch_upgrade(Request, Session) :-
   catch(handle_upgrade(Request, Session), _, true).

% handle_upgrade(+Request, +Socket)
:- private handle_upgrade/2.
handle_upgrade(Request, Session) :-
   setup_call_cleanup(
      open(Session, write, Output, [type(binary)]),
      response_upgrade(Request, Output),
      flush_output(Output)).

% response_upgrade(+Request, +Stream)
:- private response_upgrade/2.
response_upgrade(Request, Response) :-
   http_header(Request, 'sec-websocket-key', Key),
   http_header(Request, 'sec-websocket-version', '13'),
   atom_concat(Key, '258EAFA5-E914-47DA-95CA-C5AB0DC85B11', Key2),
   atom_block(Key2, Block),
   sha1_hash(Block, Hash),
   base64_block(Key3, Hash),
   write_atom(Response, 'HTTP/1.1 101 Switching Protocols\r\n'),
   write_atom(Response, 'Upgrade: websocket\r\n'),
   write_atom(Response, 'Connection: Upgrade\r\n'),
   write_atom(Response, 'Sec-WebSocket-Accept: '),
   write_atom(Response, Key3),
   write_atom(Response, '\r\n'),
   make_header_date(Headers, []),
   response_binary_headers(Headers, Response),
   write_atom(Response, '\r\n').

/**
 * dispatch_redirect(L, O):
 * Send a redirect response to location L to the socket O.
 */
% dispatch_redirect(+Atom, +Socket)
:- public dispatch_redirect/2.
dispatch_redirect(Location, Session) :-
   catch(handle_redirect(Location, Session), _, true).

% handle_redirect(+Atom, +Socket)
:- private handle_redirect/2.
handle_redirect(Location, Session) :-
   setup_call_cleanup(
      open(Session, write, Response),
      response_redirect(Location, Response),
      close(Response)).

% response_redirect(+Atom, +Stream)
:- private response_redirect/2.
response_redirect(Location, Response) :-
   write(Response, 'HTTP/1.1 302 Found\r\n'),
   write(Response, 'Location: '),
   write(Response, Location),
   write(Response, '\r\n'),
   make_header_date(Headers, []),
   response_text_headers(Headers, Response),
   write(Response, '\r\n').

/***************************************************************/
/* Date & Version Utility                                      */
/***************************************************************/

/**
 * meta_binary(F, L):
 * The predicate succeeds in L with the meta headers of the text file F.
 */
% meta_binary(+File, -List)
:- private meta_binary/2.
meta_binary(File, Headers) :-
   setup_call_cleanup(
      open_resource(File, Stream, [type(binary)]),
      (stream_property(Stream, last_modified(Millis)),
      stream_property(Stream, version_tag(ETag)),
      stream_property(Stream, mime_type(MimeType))),
      close(Stream)),
   make_header_last(Millis, Headers, Headers2),
   make_header_etag(ETag, Headers2, Headers3),
   make_header_ctyp(MimeType, '', Headers3, []).

/**
 * meta_text(F, L):
 * The predicate succeeds in L with the meta headers of the text file F.
 */
% meta_text(+File, -List)
:- private meta_text/2.
meta_text(File, Headers) :-
   setup_call_cleanup(
      open_resource(File, Stream),
      (stream_property(Stream, last_modified(Millis)),
      stream_property(Stream, version_tag(ETag)),
      stream_property(Stream, mime_type(MimeType))),
      close(Stream)),
   make_header_last(Millis, Headers, Headers2),
   make_header_etag(ETag, Headers2, Headers3),
   make_header_ctyp(MimeType, 'UTF-8', Headers3, []).

% make_header_last(+Integer, -List, +List)
:- private make_header_last/3.
make_header_last(0, Headers, Headers) :- !.
make_header_last(Millis, ['Last-Modified'-Formatted|Rest], Rest) :-
   rfc1123_atom(Millis, Formatted).

% make_header_etag(+Integer, -List, +List)
:- private make_header_etag/3.
make_header_etag('', Headers, Headers) :- !.
make_header_etag(ETag, ['ETag'-Quoted|Rest], Rest) :-
   atom_split(Quoted, '', ['"', ETag, '"']).

% make_header_ctyp(+Atom, +Atom, -List, +List)
:- private make_header_ctyp/4.
make_header_ctyp('', '', Headers, Headers) :- !.
make_header_ctyp(MimeType, '', ['Content-Type'-MimeType|Rest], Rest) :- !.
make_header_ctyp(MimeType, Encoding, ['Content-Type'-ContentType|Rest], Rest) :-
   atom_split(ContentType, '', [MimeType, '; charset=', Encoding]).

% make_header_date(-List, +List)
:- private make_header_date/2.
make_header_date(['Date'-Formatted|Rest], Rest) :-
   statistics(wall, Millis),
   rfc1123_atom(Millis, Formatted).

/***************************************************************/
/* Precondition Validation                                     */
/***************************************************************/

/**
 * validate_meta(R, H):
 * The predicate succeeds when the resource meta-data in the
 * headers H satisfies the conditions in the request R.
 */
% validate_meta(+Request, +List)
:- public validate_meta/2.
validate_meta(Request, Headers) :-
   http_header(Request, 'if-none-match', _), !,
   validate_make_header_etag(Request, Headers).
validate_meta(Request, Headers) :-
   http_header(Request, 'if-modified-since', Formatted), !,
   validate_make_header_last(Formatted, Headers).
validate_meta(_, _).

% validate_make_header_etag(+Atom, +List)
:- private validate_make_header_etag/2.
validate_make_header_etag(Request, Headers) :-
   member('ETag'-Quoted2, Headers),
   http_header(Request, 'if-none-match', Quoted),
   Quoted2 == Quoted, !, fail.
validate_make_header_etag(_, _).

% validate_make_header_last(+Atom, +List)
:- private validate_make_header_last/2.
validate_make_header_last(Formatted, Headers) :-
   member('Last-Modified'-Formatted2, Headers),
   rfc1123_atom(Millis2, Formatted2),
   rfc1123_atom(Millis, Formatted),
   Millis >= Millis2, !, fail.
validate_make_header_last(_, _).

/***************************************************************/
/* Head HTTP Response                                          */
/***************************************************************/

/**
 * dispatch_head(C, H, O):
 * Send an OK response C with meta data headers H to the socket O.
 * The OK responses from 2xx and 3xx are supported.
 */
% dispatch_head(+Integer, +List, +Socket)
:- public dispatch_head/3.
dispatch_head(Code, Headers, Session) :-
   catch(handle_head(Code, Headers, Session), _, true).

% handle_head(+Integer, +List, +Socket)
:- private handle_head/3.
handle_head(Code, Headers, Session) :-
   setup_call_cleanup(
      open(Session, write, Response, [type(binary)]),
      response_binary(Code, Headers, Response),
      close(Response)).
