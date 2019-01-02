/**
 * This module provides a HTTP server based on Pythonesk dispatch.
 * The server can be started by providing an object that
 * will be responsible for handling HTTP requests:
 *
 * ?- server(<object>, <port>), fail; true.
 *
 * The class of the object need only implement a predicate
 * dispatch/4 with the Pythoneks convention that the receiver
 * object appears in the first argument.
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

:- package(library(jekdev/reference/wire)).

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
   read_line(Request, What),
   atom_list_concat([_,URI,_], ' ', What),
   make_uri(Spec, Query, _, URI),
   params(Query, Assoc),
   Object::dispatch(Spec, Assoc, Session).

% params(+Atom, -Assoc)
:- private params/2.
params('', []) :- !.
params(Query, [Name-Value|Assoc]) :-
   make_query(Name, Value, Rest, Query),
   params(Rest, Assoc).

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
 * send_text(F, O):
 * The predicate sends the HTML resource F to the output stream O.
 */
% send_text(+File, +Stream)
:- public send_text/2.
send_text(File, Response) :-
   setup_call_cleanup(
      open_resource(File, Stream),
      (  send_html(Response),
         send_lines(Stream, Response)),
      close(Stream)).

% send_html(+Stream)
:- private send_html/1.
send_html(Response) :-
   write(Response, 'HTTP/1.0 200 OK\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
   write(Response, '\r\n').

% send_lines(+Stream, +Stream)
:- private send_lines/2.
send_lines(Stream, Response) :-
   read_line(Stream, Line), !,
   write(Response, Line),
   write(Response, '\r\n'),
   send_lines(Stream, Response).
send_lines(_, _).

/***************************************************************/
/* HTTP Response Binary                                        */
/***************************************************************/

/**
 * send_binary(F, O):
 * The predicate sends the binary resource F to the output stream O.
 */
% send_binary(+File, +Stream)
:- public send_binary/2.
send_binary(File, Response) :-
   setup_call_cleanup(
      open_resource(File, Stream, [type(binary)]),
      (  send_octet(Response),
         send_blocks(Stream, Response)),
      close(Stream)).

% send_octet(+Stream)
:- private send_octet/1.
send_octet(Response) :-
   write_bytes(Response, "HTTP/1.0 200 OK\r\n"),
   write_bytes(Response, "Content-Type: application/octet-stream\r\n"),
   write_bytes(Response, "\r\n").

% write_bytes(+Stream, +Bytes)
:- private write_bytes/2.
write_bytes(Response, Bytes) :-
   block_bytes(Block, Bytes),
   write_block(Response, Block).

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

/**
 * html_begin(O, T):
 * html_begin(O, T, S):
 * The predicate sends the html begin with title T to the output stream O.
 */
% html_begin(+Stream, +Atom)
:- public html_begin/2.
html_begin(Response, Title) :-
   html_begin(Response, Title, []).

% html_begin(+Stream, +Atom, +List)
:- public html_begin/3.
html_begin(Response, Title, Opt) :-
   send_html(Response),
   write(Response, '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\r\n'),
   write(Response, '<html>\r\n'),
   write(Response, '  <head>\r\n'),
   write(Response, '      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">\r\n'),
   write(Response, '      <title>'),
   html_escape(Response, Title),
   write(Response, '</title>\r\n'),
   html_begin_opt(Response, Opt),
   write(Response, '  </head>\r\n'),
   write(Response, '  <body>\r\n').

% html_begin_opt(+Stream, List)
:- private html_begin_opt/2.
html_begin_opt(Response, [tree|Opt]) :-
   script_tree(Response),
   html_begin_opt(Response, Opt).
html_begin_opt(_, []).

/**
 * script_tree(O):
 * The predicate sends the script tree to the output stream O.
 */
% script_tree(+Stream)
:- private script_tree/1.
script_tree(Response) :-
   write(Response, '    <script type="text/javascript">\r\n'),
   write(Response, '        function openClose(id) {\r\n'),
   write(Response, '            var elem = document.getElementById(id);\r\n'),
   write(Response, '            var img = document.getElementById(id + "_img");\r\n'),
   write(Response, '            if (elem.style.display == "none") {\r\n'),
   write(Response, '                elem.style.display = "block";\r\n'),
   write(Response, '                img.alt = "open";\r\n'),
   write(Response, '                img.src = "/open.gif";\r\n'),
   write(Response, '            } else {\r\n'),
   write(Response, '                elem.style.display = "none";\r\n'),
   write(Response, '                img.alt = "closed";\r\n'),
   write(Response, '                img.src = "/closed.gif";\r\n'),
   write(Response, '            }\r\n'),
   write(Response, '        }\r\n'),
   write(Response, '    </script>\r\n').

/**
 * html_end(O):
 * The predicate sends the html end to the output stream O.
 */
% html_end(+Stream)
:- public html_end/1.
html_end(Response) :-
   write(Response, '   </body>\r\n'),
   write(Response, '</html>\r\n').

