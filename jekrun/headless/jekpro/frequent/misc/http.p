/**
 * This module provides a HTTP server based on Pythonesk dispatch of
 * a server object. The class of the server object need only implement
 * a predicate dispatch/4 with the Pythonesk convention that the receiver
 * appears in the first argument. The server can be run by providing the
 * server object that will be responsible for handling HTTP requests:
 *
 * ?- run_http(&lt;object&gt;, &lt;port&gt;), fail; true.
 *
 * The server currently implements a minimal subset of the HTTP/1.1 protocol
 * restricted to GET method. The server will read the request line and the
 * header lines. The server is able to generate error messages in the case
 * the request is erroneous or in case the server object cannot handle
 * the request. The following HTTP/1.1 errors have been realized:
 *
 * * 400 Bad Request: Request could not be parsed.
 * * 404 Not Found: Server object did not succeeds.
 * * 501 Not Implemented: Request method not supported.
 *
 * The predicate http_parameter/3 can be used by the server object to access
 * URI query parameters. The predicate response_text/1, response_binary/1 and
 * html_escape/1 can be used to generate dynamic content by the server
 * object. The predicates send_text/2 and send_binary/2 can be used by the
 * server object to deliver static content.
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
   balance((  accept(Object, Port, Session),
              handle(Object, Session))).

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
      (  repeat,
         server_accept(Server, Session)),
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
   open(Session, read, Request, [type(binary)]),
   read_request(Request, Method, URI),
   read_header(Request, Header), !,
   handle_method(Object, Method, URI, Header, Session).
handle(_, Session) :-
   catch(handle_error(400, Session), _, true).
/* Bad Request */

% handle_method(+Object, +Atom, +Atom, +List, +Socket)
:- private handle_method/5.
handle_method(Object, 'GET', URI, Header, Session) :- !,
   handle_get(Object, URI, Header, Session).
handle_method(_, _, _, _, Session) :-
   catch(handle_error(501, Session), _, true).
/* Not Implemented */

% handle_get(+Object, +Atom, +List, +Socket)
:- private handle_get/4.
handle_get(Object, URI, Header, Session) :-
   make_uri(Spec, Query, _, URI),
   make_parameter(Query, Parameter),
   handle_object(Object, Spec, request(Parameter,Header), Session), !.
handle_get(_, _, _, Session) :-
   catch(handle_error(404, Session), _, true).
/* Not Found */

% handle_error(+Integer, +Socket)
:- private handle_error/2.
handle_error(Code, Session) :-
   setup_call_cleanup(
      open(Session, write, Response),
      send_error(Code, Response),
      close(Response)).

% handle_object(+Object, +Atom, +Request, +Socket)
:- private handle_object/4.
handle_object(Object, Spec, Request, Session) :-
   http_header(Request, 'Upgrade', websocket),
   http_header(Request, 'Connection', 'Upgrade'), !,
   Object::upgrade(Spec, Request, Session).
handle_object(Object, Spec, Request, Session) :-
   Object::dispatch(Spec, Request, Session).

/**
 * initialized(O, S):
 * The predicate is called when the server S
 * is initialized for object O.
 */
% initialized(+Object, +Server)
:- public initialized/2.
:- static initialized/2.

/**
 * destroyed(O, S):
 * The predicate is called when the server S
 * is destroyed for object O.
 */
% destroyed(+Object, +Server)
:- public destroyed/2.
:- static destroyed/2.

/**
 * dispatch(O, P, R, S):
 * The predicate succeeds in dispatching the request for object
 * O, with path P, with request R and the session S.
 */
% dispatch(+Object, +Spec, +Request, +Session)
:- public dispatch/4.
dispatch(_, '/images/cookie.gif', _, Session) :- !,
   catch(handle_binary(library(misc/images/cookie), Session), _, true).

/**
 * upgrade(O, P, R, S):
 * The predicate succeeds in upgrading the request for object
 * O, with path P, with request R and the session S.
 */
% upgrade(+Object, +Spec, +Request, +Session)
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
http_parameter(request(Parameter,_), Name, Value) :-
   member(Name-Value, Parameter).

/**
 * http_header(R, N, V):
 * The predicate succeeds in V with the value of the header named N
 * from the request R.
 */
:- public http_header/3.
http_header(request(_,Header), Name, Value) :-
   member(Name-Value, Header).

% read_request(+Stream, -Atom, -Atom)
:- private read_request/3.
read_request(Request, Method, URI) :-
   read_punch_max(Request, 1024, Punch),
   atom_block(What, Punch),
   \+ atom_length(What, 1024),
   atom_split(What, ' ', [Method,URI,_]).

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
   atom_split(Line, ': ', [Name,Rest]),
   atom_split(Rest, ', ', Values),
   make_header(Values, Name, List, List2).

% make_header(+Atom, +List, -List, +List)
:- private make_header/3.
make_header([], _, List, List).
make_header([Value|Rest], Name, [Name-Value|List], List2) :-
   make_header(Rest, Name, List, List2).

% make_parameter(+Atom, -List)
:- private make_parameter/2.
make_parameter('', []) :- !.
make_parameter(Query, [Name-Value|List]) :-
   make_query(Name, Value, Rest, Query),
   make_parameter(Rest, List).

/***************************************************************/
/* HTTP Response Text                                          */
/***************************************************************/

/**
 * response_text(O):
 * Send an OK response to the text output stream.
 */
% response_text(+Stream)
:- public response_text/1.
response_text(Response) :-
   write(Response, 'HTTP/1.1 200 OK\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
   write(Response, '\r\n').

/**
 * handle_text(F, O):
 * The predicate sends the HTML resource F to the session O.
 */
% handle_text(+File, +Socket)
:- public handle_text/2.
handle_text(File, Session) :-
   setup_call_cleanup(
      open(Session, write, Response),
      send_text(File, Response),
      close(Response)).

% send_text(+File, +Stream)
:- private send_text/2.
send_text(File, Response) :-
   setup_call_cleanup(
      open_resource(File, Stream),
      (  response_text(Response),
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
/* HTTP Response Binary                                        */
/***************************************************************/

/**
 * response_binary(O):
 * Send an OK response to the binary output stream.
 */
% response_binary(+Stream)
:- public response_binary/1.
response_binary(Response) :-
   write_atom(Response, 'HTTP/1.1 200 OK\r\n'),
   write_atom(Response, 'Content-Type: application/octet-stream\r\n'),
   write_atom(Response, '\r\n').

% write_atom(+Stream, +Atom)
:- public write_atom/2.
write_atom(Response, Atom) :-
   atom_block(Atom, Block),
   write_block(Response, Block).

/**
 * handle_binary(F, O):
 * The predicate sends the binary resource F to the socket O.
 */
% handle_binary(+File, +Socket)
:- public handle_binary/2.
handle_binary(File, Session) :-
   setup_call_cleanup(
      open(Session, write, Response, [type(binary)]),
      send_binary(File, Response),
      close(Response)).

% send_binary(+File, +Stream)
:- private send_binary/2.
send_binary(File, Response) :-
   setup_call_cleanup(
      open_resource(File, Stream, [type(binary)]),
      (  response_binary(Response),
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
/* HTTP Response Dynamic                                       */
/***************************************************************/

/**
 * html_escape(O, T):
 * The predicate sends the text T escaped to the output stream O.
 */
:- public html_escape/2.
html_escape(Response, Text) :-
   text_escape(Text, Escape),
   write(Response, Escape).

/***************************************************************/
/* HTTP Response Redirect                                      */
/***************************************************************/

/**
 * response_redirect(L, O):
 * Send a redirect response to location L to the text output stream O.
 */
% response_redirect(+Atom, +Stream)
:- public response_redirect/2.
response_redirect(Location, Response) :-
   write(Response, 'HTTP/1.1 302 Found\r\n'),
   write(Response, 'Location: '),
   write(Response, Location),
   write(Response, '\r\n'),
   write(Response, '\r\n').

/***************************************************************/
/* HTTP Response Upgrade                                       */
/***************************************************************/

/**
 * response_upgrade(R, O):
 * Send an upgrade response from request R to the binary output stream O.
 */
% response_upgrade(+Request, +Stream)
:- public response_upgrade/2.
response_upgrade(Request, Response) :-
   http_header(Request, 'Sec-WebSocket-Key', Key),
   http_header(Request, 'Sec-WebSocket-Version', '13'),
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
   write_atom(Response, '\r\n').

/***************************************************************/
/* Internal Error Generator                                    */
/***************************************************************/

/**
 * response_error(C, O):
 * Send an error code C to the text output stream O.
 */
% response_error(+Integer, +Stream)
:- private response_error/2.
response_error(400, Response) :- !,
   write(Response, 'HTTP/1.1 400 Bad Request\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
   write(Response, '\r\n').
response_error(404, Response) :- !,
   write(Response, 'HTTP/1.1 404 Not Found\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
   write(Response, '\r\n').
response_error(501, Response) :- !,
   write(Response, 'HTTP/1.1 501 Not Implemented\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
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
      (  response_error(400, Response),
         send_lines(Stream, Response)),
      close(Stream)).
send_error(404, Response) :- !,
   setup_call_cleanup(
      open_resource(library(misc/pages/err404), Stream),
      (  response_error(404, Response),
         send_lines(Stream, Response)),
      close(Stream)).
send_error(501, Response) :- !,
   setup_call_cleanup(
      open_resource(library(misc/pages/err501), Stream),
      (  response_error(501, Response),
         send_lines(Stream, Response)),
      close(Stream)).
