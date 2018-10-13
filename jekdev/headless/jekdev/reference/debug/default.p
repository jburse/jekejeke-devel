/**
 * We can distinguish a couple of debugging modes. In any debugging
 * mode the goals in the body of a clause will be decorated by port
 * hooks. Depending on the debugging mode the hooks will check a
 * certain condition and when this condition is met the current
 * goal is shown. Whether the interpreter will also prompt the end-user
 * depends on the current leash mode. The debugging modes are:
 *
 * Mode:       Condition:
 * off         Run until paused or aborted
 * step_in     Run until next port
 * step_over   Run until predicate is executed
 * step_out    Run until parent predicate is executed
 * on          Run until next spy or break point
 *
 * Predicates that have the sys_notrace predicate property set are
 * ignored in any debugging mode. A couple of predicates that
 * resemble commands, e.g. listing/1, trace/1, etc., have the
 * sys_notrace predicate property by default set. Predicates that
 * are invoked from within a source which has the sys_notrace source
 * property set are as well ignored in any debugging mode. The
 * system sources have the sys_notrace source property by default set.
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

:- use_package(foreign(jekdev/reference/debug)).

:- module(user, []).

:- use_module(library(experiment/simp)).
:- use_module(library(inspection/provable)).
:- use_module(library(stream/console)).

/***********************************************************************/
/* Debugging Mode                                                      */
/***********************************************************************/

/**
 * nodebug:
 * The predicate switches to the off mode.
 */
:- public nodebug/0.
nodebug :-
   set_prolog_flag(sys_skip_frame, null),
   set_prolog_flag(debug, off).
:- set_predicate_property(nodebug/0, sys_notrace).

/**
 * trace:
 * The predicate switches to the step in mode.
 */
:- public trace/0.
trace :-
   set_prolog_flag(sys_skip_frame, null),
   set_prolog_flag(debug, step_in).
:- set_predicate_property(trace/0, sys_notrace).

/**
 * skip:
 * The predicate switches to the skip mode.
 */
:- public skip/0.
skip :-
   set_prolog_flag(sys_skip_frame, null),
   set_prolog_flag(debug, step_over).
:- set_predicate_property(skip/0, sys_notrace).

/**
 * out:
 * The predicate switches to the out mode.
 */
:- public out/0.
out :-
   set_prolog_flag(sys_skip_frame, null),
   set_prolog_flag(debug, step_out).
:- set_predicate_property(out/0, sys_notrace).

/**
 * debug:
 * The predicate switches to the debug mode.
 */
:- public debug/0.
debug :-
   set_prolog_flag(sys_skip_frame, null),
   set_prolog_flag(debug, on).
:- set_predicate_property(debug/0, sys_notrace).

/**
 * leash(L):
 * Leash the ports that are listed in L, unleash the ports that are
 * not listed in L. When prompted, unleashed ports do not await user
 * interaction but simply continue.
 * The mnemonics that work for the predicate are listed in the API
 * documentation.
 */
% leash(+AtomOrList)
:- public leash/1.
leash(Name) :-
   sys_name_flags(Name, Flags), !,
   set_prolog_flag(sys_leash, Flags).
leash(Flags) :-
   set_prolog_flag(sys_leash, Flags).
:- set_predicate_property(leash/1, sys_notrace).

/**
 * visible(L):
 * Show the ports that are listed in L, hide the ports that are not
 * listed in L. When traced, hidden ports are not prompted but simply
 * continue. The predicate accepts the same mnemonics as the
 * predicate leash/1.
 */
% visible(+AtomOrList)
:- public visible/1.
visible(Name) :-
   sys_name_flags(Name, Flags), !,
   set_prolog_flag(sys_visible, Flags).
visible(Flags) :-
   set_prolog_flag(sys_visible, Flags).
:- set_predicate_property(visible/1, sys_notrace).

% sys_name_flags(+Atom, -List)
:- private sys_name_flags/2.
sys_name_flags(off, []).
sys_name_flags(loose, [call]).
sys_name_flags(half, [call,redo]).
sys_name_flags(tight, [call,redo,fail]).
sys_name_flags(full, [call,exit,redo,fail]).
sys_name_flags(all, [call,exit,redo,fail,head]).

/***********************************************************************/
/* Spy & Break Points                                                  */
/***********************************************************************/

/**
 * debugging:
 * The predicate shows the debug mode, the spy points and the break points.
 */
% debugging
:- public debugging/0.
debugging :-
   current_prolog_flag(debug, X),
   ttywrite_term((:-set_prolog_flag(debug,X)), [quoted(true),context(0)]),
   ttywrite('.'), ttynl, fail.
debugging :-
   current_prolog_flag(sys_visible, X),
   ttywrite_term((:-visible(X)), [quoted(true),context(0)]),
   ttywrite('.'), ttynl, fail.
debugging :-
   current_prolog_flag(sys_leash, X),
   ttywrite_term((:-leash(X)), [quoted(true),context(0)]),
   ttywrite('.'), ttynl, fail.
debugging :-
   spying(X),
   ttywrite_term((:-spy(X)), [quoted(true),context(0)]),
   ttywrite('.'), ttynl, fail.
debugging :-
   breaking(X, Y),
   ttywrite_term((:-break(X,Y)), [quoted(true),context(0)]),
   ttywrite('.'), ttynl, fail.
debugging.

/**
 * spy(P):
 * The predicate adds the predicate P to the spy points.
 */
% spy(+Indicator)
:- public spy/1.
:- special(spy/1, 'SpecialDefault', 0).

/**
 * nospy(P):
 * The predicate removes the predicate P from the spy points.
 */
% nospy(+Indicator)
:- public nospy/1.
:- special(nospy/1, 'SpecialDefault', 1).

/**
 * spying(P):
 * The predicate succeeds in P for every spy point.
 */
% spying(-Indicator)
:- public spying/1.
spying(I) :-
   sys_spying(L),
   sys_member(I, L).

% sys_spying(-List)
:- private sys_spying/1.
:- special(sys_spying/1, 'SpecialDefault', 2).

/**
 * break(F, L):
 * The predicate adds the file F and the line number L to the break points.
 */
% break(+Callable, +Integer)
:- public break/2.
break(P, L) :-
   absolute_file_name(P, Q),
   sys_break(Q, L).

% sys_break(+Pin, +Integer)
:- private sys_break/2.
:- special(sys_break/2, 'SpecialDefault', 3).

/**
 * nobreak(F, L):
 * The predicate removes the file F and the line number L from the break points.
 */
% nobreak(+Callable, +Integer)
:- public nobreak/2.
nobreak(P, L) :-
   absolute_file_name(P, Q),
   sys_nobreak(Q, L).

% sys_nobreak(+Pin, +Integer)
:- private sys_nobreak/2.
:- special(sys_nobreak/2, 'SpecialDefault', 4).

/**
 * breaking(F, L):
 * For every break point the predicate succeeds
 * with the file F and the line number L.
 */
% breaking(-Callable, -Integer)
:- public breaking/2.
breaking(P, L) :-
   sys_breaking(R),
   sys_member(Q-L, R),
   absolute_file_name(P, Q).

% sys_breaking(+List)
:- private sys_breaking/1.
:- special(sys_breaking/1, 'SpecialDefault', 5).

/*******************************************************************************/
/* Debugger Hooks                                                              */
/*******************************************************************************/

% The cosmetics hook
% sys_repose_goal(+PortGoal, -PortGoal, -Context)
:- public sys_repose_goal/3.
sys_repose_goal(P-H, Q-K, C) :-
   expose_goal(P-H, Q-J, C),
   rebuild_goal_arg(C, J, K).

/**
 * sys_trace(P, F):
 * The default trace hook.
 */
% sys_trace(+Atom, +Frame)
:- public sys_trace/2.
:- special(sys_trace/2, 'SpecialDefault', 6).
