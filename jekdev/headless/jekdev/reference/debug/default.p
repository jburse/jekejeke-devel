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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- use_package(foreign(jekdev/reference/debug)).

:- module(user, []).

:- use_module(library(experiment/simp)).
:- use_module(library(inspection/provable)).

:- public prefix(spy).
:- op(1150, fx, spy).

:- public prefix(nospy).
:- op(1150, fx, nospy).

:- public prefix(pin).
:- op(1150, fx, pin).

:- public prefix(nopin).
:- op(1150, fx, nopin).

/*******************************************************************************/
/* Debugging Mode                                                              */
/*******************************************************************************/

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

/*******************************************************************************/
/* Spy & Break Points                                                          */
/*******************************************************************************/

/**
 * spy(P):
 * The predicate adds the predicates P to the spy points.
 */
% spy +Indicators
:- public (spy)/1.
spy [P|Q] :- !,
   sys_spy(P),
   (spy Q).
spy P,Q :- !,
   sys_spy(P),
   (spy Q).
spy [] :- !.
spy P :-
   sys_spy(P).
:- set_predicate_property((spy)/1, sys_notrace).

% sys_spy(+Indicator)
:- private sys_spy/1.
:- special(sys_spy/1, 'SpecialDefault', 1).

/**
 * nospy(P):
 * The predicate removes the predicates P from the spy points.
 */
% nospy +Indicators
:- public (nospy)/1.
nospy [P|Q] :- !,
   sys_nospy(P),
   (nospy Q).
nospy P,Q :- !,
   sys_nospy(P),
   (nospy Q).
nospy [] :- !.
nospy P :-
   sys_nospy(P).
:- set_predicate_property((nospy)/1, sys_notrace).

% sys_nospy(+Indicator)
:- private sys_nospy/1.
:- special(sys_nospy/1, 'SpecialDefault', 2).

/**
 * pin(F:L):
 * The predicate adds the file F and the line number L to the break points.
 */
% pin +Atom:+Integer
:- public (pin)/1.
pin [P:L|R] :- !,
   absolute_file_name(P, Q),
   sys_pin(Q, L),
   (pin R).
pin P:L,R :- !,
   absolute_file_name(P, Q),
   sys_pin(Q, L),
   (pin R).
pin [] :- !.
pin P:L :-
   absolute_file_name(P, Q),
   sys_pin(Q, L).
:- set_predicate_property((pin)/1, sys_notrace).

% sys_pin(+Pin, +Integer)
:- private sys_pin/2.
:- special(sys_pin/2, 'SpecialDefault', 3).

/**
 * nopin(F:L):
 * The predicate removes the file F and the line number L from the break points.
 */
% nopin +Atom:+Integer
:- public (nopin)/1.
nopin [P:L|R] :- !,
   absolute_file_name(P, Q),
   sys_nopin(Q, L),
   (nopin R).
nopin P:L,R :- !,
   absolute_file_name(P, Q),
   sys_nopin(Q, L),
   (nopin R).
nopin [] :- !.
nopin P:L :-
   absolute_file_name(P, Q),
   sys_nopin(Q, L).
:- set_predicate_property((nopin)/1, sys_notrace).

% sys_nopin(+Pin, +Integer)
:- private sys_nopin/2.
:- special(sys_nopin/2, 'SpecialDefault', 4).

/*******************************************************************************/
/* Display Debugger State                                                      */
/*******************************************************************************/

/**
 * debugging:
 * The predicate shows the spying of the predicates and the current debug mode.
 */
% debugging
:- public debugging/0.
:- special(debugging/0, 'SpecialDefault', 0).
:- set_predicate_property(debugging/0, sys_notrace).

/*******************************************************************************/
/* Debugger Hooks                                                              */
/*******************************************************************************/

% The cosmetics hook
% sys_repose_goal(+PortGoal, -PortGoal, -Context)
:- public sys_repose_goal/3.
sys_repose_goal(P-H, Q-K, C) :-
   expose_goal(P-H, Q-J, C),
   sys_rebuild_goal_arg(C, J, K).

/**
 * sys_trace(P, F):
 * The default trace hook.
 */
% sys_trace(+Atom, +Frame)
:- public sys_trace/2.
:- special(sys_trace/2, 'SpecialDefault', 5).