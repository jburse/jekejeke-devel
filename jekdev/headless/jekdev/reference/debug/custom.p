/**
 * A call back can replace the default debugger user interface. The
 * instrumented code of a clause automatically checks for debugging
 * conditions. The typical conditions are those from debug mode, spy
 * points and break points described in the previous section. When the
 * corresponding condition is met, the interpreter will call the
 * predicate trace_goal/2.
 *
 * The system predicate trace_goal/2 is customizable by the end-user
 * via additional rules for the multi-file predicate goal_tracing/2.
 * If the additional multi-file rules fail, the system predicate will
 * invoke the default debugger user interface. The behaviour of the
 * default debugger user interface can be further configured by the
 * predicate leash/1.
 *
 * Predicates that have the sys_notrace predicate property set can also
 * meet some condition, but are not display by the default debugger user
 * interface. A couple of predicates that resemble commands, e.g.
 * listing/1, trace/1, etc., have the sys_notrace predicate property
 * by set, so that they do not clutter interactive debugging.
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

/**
 * goal_tracing(P, F):
 * The predicate can be used to define a custom debugger
 * call back for the port P and the frame F.
 */
% goal_tracing(+Atom, +Frame)
:- public goal_tracing/2.
:- multifile goal_tracing/2.
:- static goal_tracing/2.

/**
 * trace_goal(P, F):
 * The predicate invokes the current debugger call back for
 * the port P and the frame F. If no call back is defined then
 * the current goal is traced and optionally prompted.
 */
% trace_goal(+Port, +Frame)
:- public trace_goal/2.
trace_goal(Port, Frame) :-
   goal_tracing(Port, Frame), !.
trace_goal(_, Frame) :-
   sys_notrace_frame(Frame), !.
trace_goal(Port, Frame) :-
   sys_leashed_port(Port), !,
   sys_trace_prompt(Port, Frame).
trace_goal(Port, Frame) :-
   sys_trace(Port, Frame).

/**
 * store_changing(S):
 * The predicate can be used to define a custom debugger
 * call back for the store S.
 */
% store_changing(+Store)
:- public store_changing/1.
:- multifile store_changing/1.
:- static store_changing/1.

/**
 * change_store(S):
 * The predicate invokes the current debugger call back for
 * the store S. If no call back is defined then does nothing.
 */
% change_store(+Store)
:- public change_store/1.
change_store(Store) :-
   store_changing(Store), !.
change_store(_).

/**
 * leash(L):
 * Leash the ports that are listed in L, unleash the ports that are
 * not listed in L. When prompted, unleashed ports do not await user
 * interaction but simply continue. The predicate accepts the same
 * mnemonics as the predicate visible/1.
 */
% leash(+AtomOrList)
:- public leash/1.
leash(Name) :-
   var(Name),
   throw(error(instantiation_error,_)).
leash(Name) :-
   sys_name_flags(Name, Flags), !,
   set_prolog_flag(sys_leash, Flags).
leash(Flags) :-
   set_prolog_flag(sys_leash, Flags).
:- set_predicate_property(leash/1, sys_notrace).

/******************************************************************/
/* Default Debugger                                               */
/******************************************************************/

/**
 * sys_notrace_frame(F):
 * The predicate succeeds if the goal of frame F is sys_notrace.
 */
% sys_notrace_frame(+Frame)
:- public sys_notrace_frame/1.
:- special(sys_notrace_frame/1, 'SpecialDefault', 3).

/**
 * sys_leashed_port(P):
 * The predicate succeeds when the port P is among the leashed ports.
 */
:- public sys_leashed_port/1.
:- special(sys_leashed_port/1, 'SpecialDefault', 4).

/**
 * sys_trace(P, F):
 * The predicate displays the port P and the goal of frame F,
 * and the continues debugging.
 */
% sys_trace(+Atom, +Frame)
:- private sys_trace/2.
:- special(sys_trace/2, 'SpecialDefault', 5).

/**
 * sys_trace_prompt(P, F):
 * The predicate displays the port P and the goal of frame F,
 * prompts the user for actions, and then continues debugging.
 */
% sys_trace_prompt(+Atom, +Frame)
:- private sys_trace_prompt/2.
:- special(sys_trace_prompt/2, 'SpecialDefault', 6).
