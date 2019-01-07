/**
 * This module provides a HTTP server based on Pythonesk dispatch of
 * a server object. The class of the server object need only implement
 * a predicate dispatch/4 with the Pythonesk convention that the receiver
 * appears in the first argument. The server can be started by providing
 * the server object that will be responsible for handling HTTP requests:
 *
 * ?- server(<object>, <port>), fail; true.
 *
 * The server currently implements a minimal subset of the HTTP/1.0
 * protocol. The server will only read the first line of a request and
 * only process GET methods. The server is able to generate error messages
 * in the case the request is erroneous or in case the server object cannot
 * handle the request. The following HTTP/1.0 errors have been realized:
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

:- package(library(jekdev/reference/notebook)).

:- module(httpsrv, []).
:- use_module(library(runtime/distributed)).
:- use_module(library(misc/socket)).
:- use_module(library(stream/console)).
:- use_module(library(stream/xml)).
:- use_module(library(system/uri)).
:- use_module(library(basic/lists)).

/***************************************************************/
/* HTTP Server                                                 */
/***************************************************************/

/**
 * server(O, P):
 * The predicate runs a web server with object O at port P.
 */
% server(+Object, +Integer)
:- public server/2.
server(Object, Port) :-
   balance((  accept(Port, Session),
              handle(Object, Session))).

/**
 * accept(P, S):
 * The predicate repeatedly succeeds with accepted sessions S at port P.
 */
% accept(+Integer, -Socket)
:- private accept/2.
accept(Port, Session) :-
   setup_call_cleanup(
      server_new(Port, Server),
      (  repeat,
         server_accept(Server, Session)),
      close(Server)).

/**
 * handle(O, S):
 * The predicate handles with object O a session S.
 */
% handle(+Object, +Socket)
:- private handle/2.
handle(Object, Session) :-
   open(Session, read, Request),
   read_line_max(Request, 1024, What),
   \+ atom_length(What, 1024),
   atom_list_concat([Method,URI,_], ' ', What), !,
   handle_method(Object, Method, URI, Session).
handle(_, Session) :-
   setup_call_cleanup(
      open(Session, write, Response),
      send_error(Response, 400),                  /* Bad Request */
      close(Response)).

% handle_method(+Object, +Atom, +Atom, +Socket)
:- private handle_method/4.
handle_method(Object, 'GET', URI, Session) :- !,
   handle_get(Object, URI, Session).
handle_method(_, _, _, Session) :-
   setup_call_cleanup(
      open(Session, write, Response),
      send_error(Response, 501),                  /* Not Implemented */
      close(Response)).

% handle_get(+Object, +Atom, +Socket)
:- private handle_get/3.
handle_get(Object, URI, Session) :-
   make_uri(Spec, Query, _, URI),
   params(Query, Assoc),
   Object::dispatch(Spec, Assoc, Session), !.
handle_get(_, _, Session) :-
   setup_call_cleanup(
      open(Session, write, Response),
      send_error(Response, 404),                  /* Not Found */
      close(Response)).

% params(+Atom, -Assoc)
:- private params/2.
params('', []) :- !.
params(Query, [Name-Value|Assoc]) :-
   make_query(Name, Value, Rest, Query),
   params(Rest, Assoc).

/**
 * dispatch(O, P, A, S):
 * The predicate succeeds in dispatching the request for object
 * O, with path P, with parameter list A and the session S.
 */
% dispatch(+Object, +Spec, +Assoc, +Session)
:- public dispatch/4.
dispatch(_, '/images/cookie.gif', _, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response, [type(binary)]),
      send_binary(library(notebook/images/cookie), Response),
      close(Response)).

/***************************************************************/
/* HTTP Requests                                               */
/***************************************************************/

/**
 * http_parameter(A, N, V):
 * The predicate succeeds in V with the value of the parameter named N
 * from the parameter list A.
 */
% http_parameter(+Assoc, +Atom, -Atom)
:- public http_parameter/3.
http_parameter(Assoc, Name, Value) :-
   member(Name-Found, Assoc), !,
   Value = Found.

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
   write(Response, 'HTTP/1.0 200 OK\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
   write(Response, '\r\n').

/**
 * send_text(F, O):
 * The predicate sends the HTML resource F to the output stream O.
 */
% send_text(+File, +Stream)
:- public send_text/2.
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
   write_bytes(Response, "HTTP/1.0 200 OK\r\n"),
   write_bytes(Response, "Content-Type: application/octet-stream\r\n"),
   write_bytes(Response, "\r\n").

% write_bytes(+Stream, +Bytes)
:- private write_bytes/2.
write_bytes(Response, Bytes) :-
   block_bytes(Block, Bytes),
   write_block(Response, Block).

/**
 * send_binary(F, O):
 * The predicate sends the binary resource F to the output stream O.
 */
% send_binary(+File, +Stream)
:- public send_binary/2.
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
 * Send a redirect response to the text output stream O.
 */
% response_redirect(+Atom, +Stream)
:- public response_redirect/2.
response_redirect(Location, Response) :-
   write(Response, 'HTTP/1.0 302 Found\r\n'),
   write(Response, 'Location: '),
   write(Response, Location),
   write(Response, '\r\n'),
   write(Response, '\r\n').

/***************************************************************/
/* Internal Error Generator                                    */
/* https://www.w3.org/Protocols/HTTP/1.0/spec.html             */
/***************************************************************/

/**
 * response_error(O, C):
 * Send an error code C to the text output stream O.
 */
% response_error(+Stream, +Integer)
:- private response_error/2.
response_error(Response, 400) :- !,
   write(Response, 'HTTP/1.0 400 Bad Request\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
   write(Response, '\r\n').
response_error(Response, 404) :- !,
   write(Response, 'HTTP/1.0 404 Not Found\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
   write(Response, '\r\n').
response_error(Response, 501) :- !,
   write(Response, 'HTTP/1.0 501 Not Implemented\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
   write(Response, '\r\n').

/**
 * send_error(O, C):
 * Send an error code C page to the text output stream O.
 */
% send_error(+Stream, +Integer)
:- private send_error/2.
send_error(Response, 400) :- !,
   setup_call_cleanup(
      open_resource(library(notebook/pages/err400), Stream),
      (  response_error(Response, 400),
         send_lines(Stream, Response)),
      close(Stream)).
send_error(Response, 404) :- !,
   setup_call_cleanup(
      open_resource(library(notebook/pages/err404), Stream),
      (  response_error(Response, 404),
         send_lines(Stream, Response)),
      close(Stream)).
send_error(Response, 501) :- !,
   setup_call_cleanup(
      open_resource(library(notebook/pages/err501), Stream),
      (  response_error(Response, 501),
         send_lines(Stream, Response)),
      close(Stream)).
