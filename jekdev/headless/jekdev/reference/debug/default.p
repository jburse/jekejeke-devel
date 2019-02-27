/**
 * We can distinguish a couple of debugging modes. In any debugging mode
 * the instrumented fork of a clause will be executed. Depending on
 * the debugging mode the instrumentation will check a certain
 * condition and when this condition is met the current execution
 * is suspended and a callback is called. The debugging modes are:
 *
 * Mode:       Condition:
 * off         Run until paused or aborted
 * step_in     Run until next port
 * step_over   Run until predicate is executed
 * step_out    Run until parent predicate is executed
 * on          Run until next spy or break point
 *
 * When the current goal is shown additional information about the
 * debugging mode, the invoca-tion depth and the current port is shown
 * as well. Last call optimization is currently defunct while debugging,
 * the invocation depth is therefore larger than usual. The information
 * is provided in the following format on the display console:
 *
 * <mode><depth><port> <goal> ?
 *
 * -: The debug mode is off
 *  : The debug mode is step in
 * =: The debug mode is step over
 * >: The debug mode is step out
 * *: The debug mode is on
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

:- use_module(library(inspection/provable)).
:- use_module(library(stream/console)).
:- use_module(library(system/thread)).
:- use_module(library(inspection/frame)).
:- use_module(library(inspection/store)).

/***********************************************************************/
/* Debugging Mode                                                      */
/***********************************************************************/

/**
 * debug:
 * The predicate switches to the on mode.
 */
:- public debug/0.
debug :-
   set_prolog_flag(debug, on).
:- set_predicate_property(debug/0, sys_notrace).

/**
 * trace:
 * The predicate switches to the step in mode.
 */
:- public trace/0.
trace :-
   set_prolog_flag(debug, step_in).
:- set_predicate_property(trace/0, sys_notrace).

/**
 * skip:
 * The predicate switches to the step over mode.
 */
:- public skip/0.
skip :-
   set_prolog_flag(debug, step_over).
:- set_predicate_property(skip/0, sys_notrace).

/**
 * out:
 * The predicate switches to the step out mode.
 */
:- public out/0.
out :-
   set_prolog_flag(debug, step_out).
:- set_predicate_property(out/0, sys_notrace).

/**
 * nodebug:
 * The predicate switches to the off mode.
 */
:- public nodebug/0.
nodebug :-
   set_prolog_flag(debug, off).
:- set_predicate_property(nodebug/0, sys_notrace).

/**
 * visible(L):
 * Show the ports that are listed in L, hide the ports that are not
 * listed in L. In debug mode, hidden ports are not further debugged
 * but simply continue. The following mnemonics work for the predicate.
 */
% visible(+AtomOrList)
:- public visible/1.
visible(Name) :-
   var(Name),
   throw(error(instantiation_error,_)).
visible(Name) :-
   sys_name_flags(Name, Flags), !,
   set_prolog_flag(sys_visible, Flags).
visible(Flags) :-
   set_prolog_flag(sys_visible, Flags).
:- set_predicate_property(visible/1, sys_notrace).

% sys_name_flags(+Atom, -List)
:- public sys_name_flags/2.
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
   write_term((:-set_prolog_flag(debug,X)), [context(0)]),
   write('.'), nl, fail.
debugging :-
   current_prolog_flag(sys_visible, X),
   write_term((:-visible(X)), [context(0)]),
   write('.'), nl, fail.
debugging :-
   current_prolog_flag(sys_leash, X),
   write_term((:-leash(X)), [context(0)]),
   write('.'), nl, fail.
debugging :-
   spying(X),
   write_term((:-spy(X)), [context(0)]),
   write('.'), nl, fail.
debugging :-
   breaking(X, Y),
   write_term((:-break(X,Y)), [context(0)]),
   write('.'), nl, fail.
debugging.
:- set_predicate_property(debugging/0, sys_notrace).

/**
 * spy(P):
 * The predicate adds the predicate P to the spy points.
 */
% spy(+Indicator)
:- public spy/1.
:- special(spy/1, 'SpecialDefault', 0).
:- set_predicate_property(spy/1, sys_notrace).

/**
 * nospy(P):
 * The predicate removes the predicate P from the spy points.
 */
% nospy(+Indicator)
:- public nospy/1.
:- special(nospy/1, 'SpecialDefault', 1).
:- set_predicate_property(nospy/1, sys_notrace).

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
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_store, Store),
   set_store_property(Store, sys_break(Q,L)),
   change_store(Store).
:- set_predicate_property(break/2, sys_notrace).

/**
 * nobreak(F, L):
 * The predicate removes the file F and the line number L from the break points.
 */
% nobreak(+Callable, +Integer)
:- public nobreak/2.
nobreak(P, L) :-
   absolute_file_name(P, Q),
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_store, Store),
   reset_store_property(Store, sys_break(Q,L)),
   change_store(Store).
:- set_predicate_property(nobreak/2, sys_notrace).

/**
 * breaking(F, L):
 * For every break point the predicate succeeds
 * with the file F and the line number L.
 */
% breaking(-Callable, -Integer)
:- public breaking/2.
breaking(P, L) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_store, Store),
   store_property(Store, sys_break(Q,L)),
   absolute_file_name(P, Q).
