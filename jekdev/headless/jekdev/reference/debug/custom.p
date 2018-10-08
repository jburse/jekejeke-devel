/**
 * A call back can replace the default debugger prompt. The instrumented
 * code of a clause automatically checks for debugging conditions.
 * The typical conditions are those from debug mode or trace mode
 * described in the previous section. When the corresponding condition
 * is met, the interface part of the debugger will be invoked. In
 * particular the interpreter will then call the system
 * predicate trace_goal/2.
 *
 * The system predicate trace_goal/2 is customizable by the end-user
 * via additional rules for the multi-file predicate goal_tracing/2.
 * If the additional multi-file rules fail, the system predicate will
 * invoke the default debugger user interface. The default debugger
 * user interface will use the system predicate expose_goal/3 to apply
 * some cosmetics before ports or ancestors are displayed. This
 * predicate is customizable by the end-user via additional rules for
 * the multi-file predicate goal_exposing/3.
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

:- module(user, []).

/**
 * goal_tracing(P, F):
 * The predicate can be used to define a custom debugger user interface.
 */
:- public goal_tracing/2.
:- multifile goal_tracing/2.
:- static goal_tracing/2.

/**
 * trace_goal(P, F):
 * The system predicate invokes the debugger user interface for
 * the port P and the frame F.
 */
% trace_goal(+Port, +Frame)
:- public trace_goal/2.
trace_goal(P, F) :-
   goal_tracing(P, F), !.
trace_goal(P, F) :-
   sys_trace(P, F).
:- set_predicate_property(trace_goal/2, sys_notrace).

/**
 * goal_exposing(I, O, C):
 * The predicate can be used to define a custom port and goal cosmetics.
 */
:- public goal_exposing/3.
:- multifile goal_exposing/3.
:- static goal_exposing/3.

/**
 * expose_goal(I, O, C):
 * The predicate applies port and goal cosmetics to I and unifies the
 * result with O. The desired context unifies with C.
 */
% expose_goal(+PortGoal, -PortGoal, -Context)
:- public expose_goal/3.
expose_goal(I, O, C) :-
   goal_exposing(I, O, C), !.
expose_goal(P-I, P-I, 0).
