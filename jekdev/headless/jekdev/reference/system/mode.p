/**
 * The Prolog interpreter activates the goals of the debug clause
 * fork in case some debug mode is set. The Prolog flag sys_cloak
 * allows temporarily disabling any debug mode for a Prolog
 * interpreter. The predicate sys_ignore/1 will run a goal with
 * this flag set.
 *
 * The debug clause fork is pickled with the instrumentation built-ins
 * sys_in/0, sys_out/0 and sys_at/0. The end-user is not supposed to
 * use them. They need to have public access so that they can be called
 * from any clause fork.
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

:- use_package(foreign(jekdev/reference/system)).

:- module(user, []).

/**
 * sys_ignore(A):
 * The predicate succeeds whenever A succeeds. The goal A is invoked
 * with the mode cloak temporarily set to on.
 */
% sys_ignore(+Goal)
:- public sys_ignore/1.
:- meta_predicate sys_ignore(0).
:- special(sys_ignore/1, 'SpecialMode', 0).
:- set_predicate_property(sys_ignore/1, sys_notrace).

/******************************************************************/
/* sys_in/0                                                       */
/*   call: 0                                                      */
/*   fail: 1                                                      */
/******************************************************************/

/**
 * sys_in:
 * This instrumentation hook should succeed. It is called
 * before a goal is called every time a goal is called.
 */
:- public sys_in/0.
:- static sys_in/0.
:- set_predicate_property(sys_in/0, sys_noinstrument).
:- set_predicate_property(sys_in/0, sys_nowakeup).
:- set_predicate_property(sys_in/0, sys_nostack).
sys_in :-
   sys_notrace_chk(0), !.
sys_in :-
   sys_port_show(0), sys_in2.

:- private sys_in2/0.
:- static sys_in2/0.
:- set_predicate_property(sys_in2/0, sys_noinstrument).
:- set_predicate_property(sys_in2/0, sys_nowakeup).
:- set_predicate_property(sys_in2/0, sys_nostack).
sys_in2.
sys_in2 :-
   sys_port_show(1), fail.

/******************************************************************/
/* sys_out/0                                                      */
/*   exit: 2                                                      */
/*   redo: 3                                                      */
/******************************************************************/

/**
 * sys_out:
 * This instrumentation hook should succeed. It is called
 * after a goal exits every time a goal exits.
 */
:- public sys_out/0.
:- static sys_out/0.
:- set_predicate_property(sys_out/0, sys_noinstrument).
:- set_predicate_property(sys_out/0, sys_nowakeup).
:- set_predicate_property(sys_out/0, sys_nostack).
sys_out :-
   sys_notrace_chk(2), !.
sys_out :-
   sys_port_show(2), sys_out2.

:- private sys_out2/0.
:- static sys_out2/0.
:- set_predicate_property(sys_out2/0, sys_noinstrument).
:- set_predicate_property(sys_out2/0, sys_nowakeup).
:- set_predicate_property(sys_out2/0, sys_nostack).
sys_out2 :-
   sys_cut_chk(3), !.
sys_out2 :-
   sys_goal_chk(3), !, sys_goal_cut.
sys_out2.
sys_out2 :-
   sys_port_show(3), fail.

/******************************************************************/
/* sys_at/0                                                       */
/*   head: 4                                                      */
/*   chop: 5                                                      */
/******************************************************************/

/**
 * sys_at:
 * This instrumentation hook should succeed. It is called
 * after a head unification succeeds and before the attribute
 * variable unify hooks are called.
 */
:- public sys_at/0.
:- static sys_at/0.
:- set_predicate_property(sys_at/0, sys_noinstrument).
:- set_predicate_property(sys_at/0, sys_nowakeup).
:- set_predicate_property(sys_at/0, sys_nostack).
sys_at :-
   sys_notrace_chk(4), !.
sys_at :-
   sys_port_show(4), sys_at2.

:- private sys_at2/0.
:- static sys_at2/0.
:- set_predicate_property(sys_at2/0, sys_noinstrument).
:- set_predicate_property(sys_at2/0, sys_nowakeup).
:- set_predicate_property(sys_at2/0, sys_nostack).
sys_at2 :-
   sys_clause_chk(5), !.
sys_at2.
sys_at2 :-
   sys_port_show(5), fail.

/**
 * sys_notrace_chk(P):
 * The predicate succeeds when the debugged goal for port
 * P is called from a sys_notrace source or has itself
 * a sys_notrace predicate.
 */
% sys_notrace_chk(+Integer)
:- private sys_notrace_chk/1.
:- special(sys_notrace_chk/1, 'SpecialMode', 1).

/**
 * sys_port_show(P):
 * The predicate succeeds when the debugged goal for port P
 * should not be traced according to the debug mode, the spy
 * points or the break points. Otherwise the predicate calls
 * the trace goal user hook.
 */
:- private sys_port_show/1.
:- special(sys_port_show/1, 'SpecialMode', 2).

/**
 * sys_cut_chk(P):
 * The predicate succeeds when there are no previous
 * choice points in the current clause of port P.
 */
:- private sys_cut_chk/1.
:- special(sys_cut_chk/1, 'SpecialMode', 3).

/**
 * sys_goal_chk(P):
 * The predicate succeeds when the previous choice point is
 * a choice point of the call instrumentation of port P.
 */
:- private sys_goal_chk/1.
:- special(sys_goal_chk/1, 'SpecialMode', 4).

/**
 * sys_goal_cut:
 * The predicate succeeds in removing the current choice point.
 */
:- private sys_goal_cut/0.
:- special(sys_goal_cut/0, 'SpecialMode', 5).

/**
 * sys_clause_chk(P):
 * The predicate succeeds when the previous choice point
 * is not a choice point for the current clause of port P.
 */
:- private sys_clause_chk/1.
:- special(sys_clause_chk/1, 'SpecialMode', 6).
