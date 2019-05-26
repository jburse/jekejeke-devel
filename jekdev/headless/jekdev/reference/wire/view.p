/**
 * The base class for a thread monitor.
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

:- module(view, []).
:- use_module(monitor).
:- use_module(library(misc/http)).
:- use_module(library(stream/xml)).
:- use_module(library(inspection/frame)).
:- use_module(library(inspection/store)).
:- use_module(pages/thread).
:- use_module(pages/stack).
:- use_module(pages/frame).
:- use_module(pages/source).
:- use_module(library(system/thread)).
:- use_module(library(system/zone)).
:- use_module(hooks/call).
:- use_module(hooks/code).
:- use_module(hooks/list).
:- use_module(hooks/tree).

/**
 * dispatch(O, P, A, S):
 * The predicate succeeds in dispatching the request for object
 * O, with path P, with parameter list A and the session S.
 */
% dispatch(+Object, +Spec, +Request, +Session)
:- override dispatch/4.
:- public dispatch/4.
dispatch(_, '/frame.jsp', Request, Session) :- !,
   dispatch_frame(Request, Session).
dispatch(_, '/source.jsp', Request, Session) :- !,
   dispatch_source(Request, Session).
dispatch(Object, '/stack.jsp', Request, Session) :- !,
   dispatch_stack(Object, Request, Session).
dispatch(Object, '/thread.jsp', Request, Session) :- !,
   dispatch_thread(Object, Request, Session).

/**
 * upgrade(O, P, R, S):
 * The predicate succeeds in upgrading the request for object
 * O, with path P, with request R and the session S.
 */
% upgrade(+Object, +Spec, +Request, +Session)
:- override upgrade/4.
:- public upgrade/4.
upgrade(_, '/call', Request, Session) :- !,
   upgrade_call(Request, Session).
upgrade(_, '/code', Request, Session) :- !,
   upgrade_code(Request, Session).
upgrade(_, '/list', Request, Session) :- !,
   upgrade_list(Request, Session).
upgrade(_, '/tree', Request, Session) :- !,
   upgrade_tree(Request, Session).

/*************************************************************/
/* Some Utility                                              */
/*************************************************************/

% frame_deref(+Frame, -Frame)
:- public frame_deref/2.
frame_deref(Frame, Other) :-
   frame_property(Frame, sys_call_goal(trace_goal(_, Other))), !.
frame_deref(Frame, Frame).

% find_call_stack(+Frame, +Count, -Frame)
:- public find_call_stack/3.
find_call_stack(null, _, _) :- !, fail.
find_call_stack(Frame, 0, Frame) :- !.
find_call_stack(Frame, Count, Result) :-
   frame_property(Frame, sys_parent_frame(Other)),
   Count2 is Count-1,
   find_call_stack(Other, Count2, Result).

/***************************************************************/
/* Date & Version Utility                                      */
/***************************************************************/

% meta_thread(+Thread, -List)
:- public meta_thread/2.
meta_thread(Thread, Headers) :-
   current_thread_flag(Thread, sys_thread_lastmod, Millis),
   make_header_all(Millis, Headers, []).

% meta_both(+Thread, -List)
:- public meta_both/2.
meta_both(Thread, Headers) :-
   current_thread_flag(Thread, sys_thread_lastmod, Millis1),
   current_thread_flag(Thread, sys_thread_store, Store),
   store_property(Store, sys_lastmod(Millis2)),
   Millis is max(Millis1,Millis2),
   make_header_all(Millis, Headers, []).

% make_header_all(+Integer, -List, +List)
:- private make_header_all/3.
make_header_all(0, Headers, Headers) :- !.
make_header_all(Millis, ['Content-Type'-'text/html; charset=UTF-8',
                           'Last-Modified'-Formatted,
                           'ETag'-Quoted|Rest], Rest) :-
   rfc1123_atom(Millis, Formatted),
   atom_number(Atom, Millis),
   atom_split(Quoted, '', ['"',Atom,'"']).

/***************************************************************/
/* HTML Response Text                                          */
/***************************************************************/

% html_begin(+Stream, +Atom, +List)
:- public html_begin/3.
html_begin(Response, Title, Opt) :-
   raw_begin(Response, Title, Opt),
   write(Response, '  <body>\r\n').

% html_h3(+Stream, +Atom)
:- public html_h3/2.
html_h3(Response, Title) :-
   write(Response, '    <h3>'),
   html_escape(Response, Title),
   write(Response, '    </h3>\r\n').

% html_end(+Stream)
:- public html_end/1.
html_end(Response) :-
   write(Response, '   </body>\r\n'),
   raw_end(Response).

% raw_begin(+Stream, +Atom, +List)
:- public raw_begin/3.
raw_begin(Response, Title, Opt) :-
   write(Response, '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\r\n'),
   write(Response, '<html>\r\n'),
   write(Response, '  <head>\r\n'),
   write(Response, '      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">\r\n'),
   write(Response, '      <meta name="viewport" content="width=device-width, initial-scale=1.0">\r\n'),
   write(Response, '      <title>'),
   html_escape(Response, Title),
   write(Response, '</title>\r\n'),
   html_begin_opt(Response, Opt),
   write(Response, '  </head>\r\n').

% html_begin_opt(+Stream, List)
:- private html_begin_opt/2.
html_begin_opt(Response, [tree|Opt]) :-
   script_tree(Response),
   html_begin_opt(Response, Opt).
html_begin_opt(Response, [lines|Opt]) :-
   style_lines(Response),
   html_begin_opt(Response, Opt).
html_begin_opt(Response, [margin|Opt]) :-
   script_margin(Response),
   html_begin_opt(Response, Opt).
html_begin_opt(_, []).

% raw_end(+Stream)
:- public raw_end/1.
raw_end(Response) :-
   write(Response, '</html>\r\n').
