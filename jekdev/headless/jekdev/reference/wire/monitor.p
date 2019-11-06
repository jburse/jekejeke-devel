/**
 * This module provides a HTTP object class to inspect the Prolog
 * threads, Prolog stack frames and Prolog variables of a
 * Prolog instance.
 *
 * The HTTP object supports GET dispatch and GET upgrade to web
 * sockets. The web sockets are used to notify the HTTP client
 * of state changes.
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

:- module(monitor, []).
:- use_module(library(misc/http)).
:- use_module(library(runtime/distributed)).
:- use_module(library(system/thread)).
:- use_module(library(misc/socket)).
:- use_module(library(inspection/frame)).
:- use_module(library(inspection/store)).
:- use_module(hooks/room).
:- use_module(pages/index).

/**
 * start_monitor:
 * The predicate starts the monitor in background.
 */
% start_monitor
:- public start_monitor/0.
start_monitor :-
   current_prolog_flag(sys_monitor_config, P),
   start_monitor(P).

% start_monitor(+Integer)
:- private start_monitor/1.
start_monitor(-1) :- !.
start_monitor(P) :-
   submit((run_http(wire/monitor, P), fail; true), _).

/**
 * initialized(O, S):
 * The predicate is called when the socket S
 * is initialized for object O.
 */
% initialized(+Object, +Socket)
:- override initialized/2.
:- public initialized/2.
initialized(_, Server) :-
   server_port(Server, Port),
   set_prolog_flag(sys_monitor_running, Port).

/**
 * destroyed(O, S):
 * The predicate is called when the socket S
 * is destroyed for object O.
 */
% destroyed(+Object, +Socket)
:- override destroyed/2.
:- public destroyed/2.
destroyed(_, _) :-
   set_prolog_flag(sys_monitor_running, -1).

/**
 * dispatch(O, P, R, S):
 * The predicate succeeds in dispatching the request for object
 * O, with path P, with request R and the socket S.
 */
% dispatch(+Object, +Spec, +Request, +Socket)
:- override dispatch/4.
:- public dispatch/4.
dispatch(_, '/images/closed.gif', Request, Session) :- !,
   dispatch_binary(library(wire/images/closed), Request, Session).
dispatch(_, '/images/open.gif', Request, Session) :- !,
   dispatch_binary(library(wire/images/open), Request, Session).
dispatch(_, '/images/blank.gif', Request, Session) :- !,
   dispatch_binary(library(wire/images/blank), Request, Session).
dispatch(_, '/images/break.gif', Request, Session) :- !,
   dispatch_binary(library(wire/images/break), Request, Session).
dispatch(_, '/index.jsp', Request, Session) :- !,
   dispatch_index(Request, Session).
dispatch(_, Path, Request, Session) :-
   sub_atom(Path, 0, Pos, '/desktop/'), !,
   Pos2 is Pos-1,
   sub_atom(Path, Pos2, _, 0, Path2),
   wire/desktop::dispatch(Path2, Request, Session).
dispatch(_, Path, Request, Session) :-
   sub_atom(Path, 0, Pos, '/mobile/'), !,
   Pos2 is Pos-1,
   sub_atom(Path, Pos2, _, 0, Path2),
   wire/mobile::dispatch(Path2, Request, Session).
dispatch(Object, Spec, Request, Session) :-
   misc/http:dispatch(Object, Spec, Request, Session).

/**
 * upgrade(O, P, R, S):
 * The predicate succeeds in upgrading the request for object
 * O, with path P, with request R and the socket S.
 */
% upgrade(+Object, +Spec, +Request, +Socket)
:- override upgrade/4.
:- public upgrade/4.
upgrade(_, Path, Request, Session) :-
   sub_atom(Path, 0, Pos, '/talkback/'), !,
   Pos2 is Pos-1,
   sub_atom(Path, Pos2, _, 0, Path2),
   wire/view::upgrade(Path2, Request, Session).

/**
 * user:goal_tracing(P, F):
 * The predicate can be used to define a custom debugger call back
 * for the port P and the frame F.
 */
% user:goal_tracing(+Atom, +Frame)
:- public user:goal_tracing/2.
:- multifile user:goal_tracing/2.
user:goal_tracing(Port, Frame) :-
   tracing_broadcast(Port, Frame), fail.

% tracing_broadcast(+Atom, +Frame)
:- private tracing_broadcast/2.
tracing_broadcast(_, Frame) :-
   sys_notrace_frame(Frame), !.
tracing_broadcast(Port, _) :-
   sys_leashed_port(Port), !,
   thread_current(Thread),
   statistics(wall, Millis),
   set_thread_flag(Thread, sys_thread_lastmod, Millis),
   current_thread_flag(Thread, sys_thread_name, Name),
   broadcast_subscribers(reload, Name).
tracing_broadcast(_, _).

/**
 * user:store_changing(S):
 * The predicate can be used to define a custom debugger call back
 * for the store S.
 */
% user:store_changing(+Store)
:- public user:store_changing/1.
:- multifile user:store_changing/1.
user:store_changing(Store) :-
   changing_broadcast(Store), fail.

% changing_broadcast(+Store)
:- private changing_broadcast/1.
changing_broadcast(Store) :-
   statistics(wall, Millis),
   set_store_property(Store, sys_lastmod(Millis)),
   store_property(Store, sys_name(Name)),
   broadcast_subscribers(reload, Name).
