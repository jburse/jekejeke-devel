/**
 * This module allows the batch reporting of coverage analysis. Beforehand
 * the module tracker needs to be used to produce the coverage analysis.
 * The predicate cover_batch/0 can then be used to generate a number of
 * files that list and summarize the results in HTML format. The reporting
 * tool makes an additional assumption about the source names:
 *
 * source --> package "/" module.
 *
 * The first level HTML page will thus present the analysis grouped by
 * packages. The second level HTML page will thus present the analysis of
 * apackage grouped by modules. The current implementation shows hit and
 * miss counts not only as numbers but also as coloured bars. Furthermore
 * links to the original source clauses will be generated.
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
 * server(P):
 * The predicate runs a web server at port P.
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
 * handle(S):
 * The predicate handles a session S.
 */
% handle(+Object, +Socket)
:- private handle/2.
handle(Object, Session) :-
   open(Session, read, Request),
   read_line(Request, What),
   atom_list_concat([_,URI,_], ' ', What),
   make_uri(Spec, Query, _, URI),
   params(Query, Assoc),
   setup_call_cleanup(
      open(Session, write, Response),
      Object::dispatch(Spec, Assoc, Response),
      close(Response)).

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
/* HTTP Responses                                              */
/***************************************************************/

/**
 * send_file(F, O):
 * The predicate sends the file F to the output stream O.
 */
% send_file(+File, +Stream)
:- public send_file/2.
send_file(File, Response) :-
   setup_call_cleanup(
      open_resource(File, Stream),
      (  send_ok(Response),
         send_text(Stream, Response)),
      close(Stream)).

% send_ok(+Stream)
:- private send_ok/1.
send_ok(Response) :-
   write(Response, 'HTTP/1.0 200 OK\r\n'),
   write(Response, 'Content-Type: text/html; charset=UTF-8\r\n'),
   write(Response, '\r\n').

% send_text(+Stream, +Stream)
:- private send_text/2.
send_text(Stream, Response) :-
   read_line(Stream, Line), !,
   write(Response, Line),
   write(Response, '\r\n'),
   send_text(Stream, Response).
send_text(_, _).

/**
 * html_begin(O, T):
 * The predicate sends the html begin with title T to the output stream O.
 */
% html_begin(+Stream, +Atom)
:- public html_begin/2.
html_begin(Response, Title) :-
   send_ok(Response),
   write(Response, '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\r\n'),
   write(Response, '<html>\r\n'),
   write(Response, '  <head>\r\n'),
   write(Response, '      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">\r\n'),
   write(Response, '      <title>'),
   html_escape(Response, Title),
   write(Response, '</title>\r\n'),
   write(Response, '  </head>\r\n'),
   write(Response, '  <body>\r\n').

/**
 * html_escape(O, T):
 * The predicate sends the text T escaped to the output stream O.
 */
:- public html_escape/2.
html_escape(Response, Text) :-
   text_escape(Text, Escape),
   write(Response, Escape).

/**
 * html_end(O):
 * The predicate sends the html end to the output stream O.
 */
% html_end(+Stream)
:- public html_end/1.
html_end(Response) :-
   write(Response, '   </body>\r\n'),
   write(Response, '</html>\r\n').
