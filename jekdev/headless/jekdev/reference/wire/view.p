/**
 * The base class for a monitor.
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
:- use_module(library(inspection/frame)).
:- use_module(pages/thread).
:- use_module(pages/stack).
:- use_module(pages/frame).
:- use_module(pages/source).

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
dispatch(_, '/toggle.class', Request, Session) :- !,
   dispatch_toggle(Request, Session).
dispatch(Object, '/stack.jsp', Request, Session) :- !,
   dispatch_stack(Object, Request, Session).
dispatch(Object, '/thread.jsp', _, Session) :- !,
   dispatch_thread(Object, Session).

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
/* HTTP Response Text                                          */
/***************************************************************/

% frame_begin(+Stream, +Atom)
:- public frame_begin/2.
frame_begin(Response, Title) :-
   frame_begin(Response, Title, []).

% frame_begin(+Stream, +Atom, +List)
:- public frame_begin/3.
frame_begin(Response, Title, Opt) :-
   response_text('', Response),
   html_begin(Response, Title, Opt),
   write(Response, '<h3>'),
   html_escape(Response, Title),
   write(Response, '</h3>\r\n').

% html_begin(+Stream, +Atom, +List)
:- private html_begin/3.
html_begin(Response, Title, Opt) :-
   write(Response, '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\r\n'),
   write(Response, '<html>\r\n'),
   write(Response, '  <head>\r\n'),
   write(Response, '      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">\r\n'),
   write(Response, '      <meta name="viewport" content="width=device-width, initial-scale=1.0">\r\n'),
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
html_begin_opt(Response, [lines|Opt]) :-
   style_lines(Response),
   html_begin_opt(Response, Opt).
html_begin_opt(Response, [margin|Opt]) :-
   script_margin(Response),
   html_begin_opt(Response, Opt).
html_begin_opt(_, []).

/**
 * frame_end(O):
 * The predicate sends the html end to the output stream O.
 */
% frame_end(+Stream)
:- public frame_end/1.
frame_end(Response) :-
   write(Response, '   </body>\r\n'),
   write(Response, '</html>\r\n').
