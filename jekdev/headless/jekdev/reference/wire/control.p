/**
 * t.b.d.
 *
 * Currently the HTTP object class only deals with GET requests
 * and provides a simple frame set based view. Extensions towards
 * web sockets and JSON RPC are planned.
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

:- module(control, []).
:- use_module(library(notebook/httpsrv)).
:- use_module(library(misc/socket)).
:- use_module(library(runtime/distributed)).
:- use_module(hooks/stack).

/**
 * upgrade(O, P, R, S):
 * The predicate succeeds in upgrading the request for object
 * O, with path P, with request R and the session S.
 */
% upgrade(+Object, +Spec, +Request, +Session)
:- override upgrade/4.
:- public upgrade/4.
upgrade(_, '/stack', Request, Session) :-
   http_parameter(Request, thread, Name), !,
   setup_call_cleanup(
      open(Session, write, Output, [type(binary)]),
      response_upgrade(Request, Output),
      flush_output(Output)),
   websock_new(Session, WebSession),
   spawn(worker_stack(Name, WebSession)).

/***************************************************************/
/* HTTP Response Text                                          */
/***************************************************************/

% subscribe_thread(+Atom, +Stream)
:- public subscribe_thread/2.
subscribe_thread(Name, Response) :-
   write(Response, '<script type="text/javascript">\r\n'),
   write(Response, '   window.onload = function() {\r\n'),
   write(Response, '      thethread = "'),
   write(Response, Name),
   write(Response, '";\r\n'),
   write(Response, '      thesocket=new WebSocket("ws://"+location.host+\r\n'),
   write(Response, '         "/talkback/stack?thread="+thethread);\r\n'),
   write(Response, '      thesocket.onmessage = onMessage;\r\n'),
   write(Response, '      function onMessage(event) {\r\n'),
   write(Response, '         location.reload();\r\n'),
   write(Response, '      };\r\n'),
   write(Response, '   };\r\n'),
   write(Response, '   window.onbeforeunload = function() {\r\n'),
   write(Response, '      thesocket.close();\r\n'),
   write(Response, '   };\r\n'),
   write(Response, '</script>\r\n').
