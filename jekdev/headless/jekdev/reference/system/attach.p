/**
 * This module provide debugger attachment for a Prolog engine. The
 * ordinary spy points and break points from the default debugger
 * are stored on the knowledge base level. The spy points and break
 * points provided by this module are stored in the current engine.
 *
 * The predicate tdebugging/0 allows listing all the Prolog engine
 * locale spy points and break points. The predicates tspy/1 and tbreak/2
 * allow adding spy points respectively break points. The predicates
 * tnospy/1 and tnobreak/2 allow removing them.
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

:- package(library(jekdev/reference/system)).
:- use_package(foreign(jekdev/reference/system)).

:- module(attach, []).
:- use_module(library(stream/console)).
:- use_module(library(system/thread)).

/***********************************************************************/
/* Thead Debugging Mode                                                */
/***********************************************************************/

/**
 * tclear:
 * The predicate switches the engine to the inherit mode.
 */
:- public tclear/0.
tclear :-
   thread_current(Thread),
   set_thread_flag(Thread, sys_tdebug, inherit).
:- set_predicate_property(tclear/0, sys_notrace).

/**
 * tdebug:
 * The predicate switches the engine to the on mode.
 */
:- public tdebug/0.
tdebug :-
   thread_current(Thread),
   set_thread_flag(Thread, sys_tdebug, on).
:- set_predicate_property(tdebug/0, sys_notrace).

/**
 * ttrace:
 * The predicate switches the engine to the step in mode.
 */
:- public ttrace/0.
ttrace :-
   thread_current(Thread),
   set_thread_flag(Thread, sys_tdebug, step_in).
:- set_predicate_property(ttrace/0, sys_notrace).

/**
 * tskip:
 * The predicate switches the engine to the step over mode.
 */
:- public tskip/0.
tskip :-
   thread_current(Thread),
   set_thread_flag(Thread, sys_tdebug, step_over).
:- set_predicate_property(tskip/0, sys_notrace).

/**
 * tup:
 * The predicate switches the engine to the step out mode.
 */
:- public tup/0.
tup :-
   thread_current(Thread),
   set_thread_flag(Thread, sys_tdebug, step_out).
:- set_predicate_property(tup/0, sys_notrace).

/**
 * tnodebug:
 * The predicate switches the engine to the off mode.
 */
:- public tnodebug/0.
tnodebug :-
   thread_current(Thread),
   set_thread_flag(Thread, sys_tdebug, off).
:- set_predicate_property(tnodebug/0, sys_notrace).

/**
 * tleash(L):
 * Leash the ports of the engine that are listed in L, unleash the
 * of the engine ports that are not listed in L. When prompted,
 * unleashed ports do not await user interaction but simply continue.
 * The predicate accepts the same mnemonics as the predicate visible/1.
 */
% tleash(+AtomOrList)
:- public tleash/1.
tleash(Name) :-
   var(Name),
   throw(error(instantiation_error,_)).
tleash(Name) :-
   sys_name_flags(Name, Flags), !,
   set_prolog_flag(sys_tleash, Flags).
tleash(Flags) :-
   set_prolog_flag(sys_tleash, Flags).
:- set_predicate_property(tleash/1, sys_notrace).

/**
 * tvisible(L):
 * Show the ports of the engine that are listed in L, hide the ports
 * of the engine that are not listed in L. In debug mode, hidden ports
 * are not further debugged but simply continue. The predicate accepts
 * the same mnemonics as the predicate visible/1.
 */
% tvisible(+AtomOrList)
:- public tvisible/1.
tvisible(Name) :-
   var(Name),
   throw(error(instantiation_error,_)).
tvisible(Name) :-
   sys_name_flags(Name, Flags), !,
   set_prolog_flag(sys_tvisible, Flags).
tvisible(Flags) :-
   set_prolog_flag(sys_tvisible, Flags).
:- set_predicate_property(tvisible/1, sys_notrace).

/***********************************************************************/
/* Thread Spy & Break Points                                           */
/***********************************************************************/

/**
 * tdebugging:
 * The predicate shows the engine spy points and the engine break points.
 */
% tdebugging
:- public tdebugging/0.
tdebugging :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_tdebug, X),
   tdebugging_tdebug(X, C),
   write_term((:-C), [context(0)]),
   write('.'), nl, fail.
tdebugging :-
   current_prolog_flag(sys_tvisible, X),
   write_term((:-tvisible(X)), [context(0)]),
   write('.'), nl, fail.
tdebugging :-
   current_prolog_flag(sys_tleash, X),
   write_term((:-tleash(X)), [context(0)]),
   write('.'), nl, fail.
tdebugging :-
   tspying(X),
   write_term((:-tspy(X)), [context(0)]),
   write('.'), nl, fail.
tdebugging :-
   tbreaking(X, Y),
   write_term((:-tbreak(X,Y)), [context(0)]),
   write('.'), nl, fail.
tdebugging.
:- set_predicate_property(tdebugging/0, sys_notrace).

:- private tdebugging_tdebug/2.
tdebugging_tdebug(inherit, tclear).
tdebugging_tdebug(on, tdebug).
tdebugging_tdebug(step_in, ttrace).
tdebugging_tdebug(step_over, tskip).
tdebugging_tdebug(step_out, tup).
tdebugging_tdebug(off, tnodebug).

/**
 * tspy(P):
 * The predicate adds the predicate P to the engine spy points.
 */
% tspy(+Indicator)
:- public tspy/1.
:- special(tspy/1, 'SpecialAttach', 0).
:- set_predicate_property(tspy/1, sys_notrace).

/**
 * tnospy(P):
 * The predicate removes the predicates P from the engine spy points.
 */
% nospy(+Indicator)
:- public tnospy/1.
:- special(tnospy/1, 'SpecialAttach', 1).
:- set_predicate_property(tnospy/1, sys_notrace).

/**
 * tspying(P):
 * The predicate succeeds in P for every engine spy point.
 */
% tspying(-Indicator)
:- public tspying/1.
tspying(I) :-
   sys_tspying(L),
   sys_member(I, L).

% sys_tspying(-List)
:- private sys_tspying/1.
:- special(sys_tspying/1, 'SpecialAttach', 2).

/**
 * tbreak(F, L):
 * The predicate adds the file F and the line number L to the engine break points.
 */
% tbreak(+Atom, +Integer)
:- public tbreak/2.
tbreak(P, L) :-
   absolute_file_name(P, Q),
   sys_tbreak(Q, L).
:- set_predicate_property(tbreak/2, sys_notrace).

% sys_tbreak(+Pin, +Integer)
:- private sys_tbreak/2.
:- special(sys_tbreak/2, 'SpecialAttach', 3).

/**
 * tnobreak(F, L):
 * The predicate removes the file F and the line number L from the engine break points.
 */
% tnobreak(+Atom, +Integer)
:- public tnobreak/2.
tnobreak(P, L) :-
   absolute_file_name(P, Q),
   sys_tnobreak(Q, L).
:- set_predicate_property(tnobreak/2, sys_notrace).

% sys_tnobreak(+Pin, +Integer)
:- private sys_tnobreak/2.
:- special(sys_tnobreak/2, 'SpecialAttach', 4).

/**
 * tbreaking(F, L):
 * For every engine break point the predicate succeeds
 * with the file F and the line number L.
 */
% tbreaking(-Callable, -Integer)
:- public tbreaking/2.
tbreaking(P, L) :-
   sys_tbreaking(R),
   sys_member(Q-L, R),
   absolute_file_name(P, Q).

% sys_tbreaking(+List)
:- private sys_tbreaking/1.
:- special(sys_tbreaking/1, 'SpecialAttach', 5).
