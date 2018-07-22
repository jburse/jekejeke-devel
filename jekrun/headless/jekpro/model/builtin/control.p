/**
 * The interpreter has the capability to interrupt its normal flow
 * by exception handling. An interruption happens when an exception
 * is thrown or when a signal is raised. An exception can be an
 * arbitrary Prolog term. Some exception terms are recognized by
 * the interpreter so as to display a user-friendly stack trace.
 * In particular we recognize:
 *
 * error(Message, Context):
 *     The exception is an error.
 * warning(Message, Context):
 *     The exception is a warning.
 * cause(Primary, Secondary):
 *     The exception is a composite of a primary exception
 *     and a secondary exception.
 *
 * The predicate throw/1 can be used to throw an exception. If the
 * context is a variable the predicate will automatically instantiate
 * the variable with the current stack trace. The predicate catch/1
 * can be used to catch a thrown exception. The predicate will not
 * catch reserved exceptions. Currently system errors are the only
 * reserved exceptions. System errors are used when interrupting the
 * execution of the interpreter.
 *
 * The predicate try_call_finally/3 will execute a preamble and a
 * postscript for a goal. The postscript is executed even when a
 * possibly reserved exception is thrown. The variant call_finally/2
 * doesn't take a preamble. The behaviour of these predicates is
 * different from setup_call_cleanup/3 in that they execute the
 * postscript and preamble multiple times and in that they don’t
 * react on events in the continuation.
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

:- sys_context_property(here, C),
   set_source_property(C, use_package(foreign(jekpro/model/builtin))).
:- sys_context_property(here, C),
   reset_source_property(C, sys_source_visible(public)).

/******************************************************************/
/* Basic Control                                                  */
/******************************************************************/

/**
 * fail: [ISO 7.8.2]
 * false: [TC2 8.15.5]
 * The predicate fails.
 */
% fail
:- special(fail/0, 'SpecialControl', 0).
:- set_predicate_property(fail/0, visible(public)).
:- set_predicate_property(fail/0, sys_notrace).
% false
:- special(false/0, 'SpecialControl', 0).
:- set_predicate_property(false/0, visible(public)).
:- set_predicate_property(false/0, sys_notrace).

/**
 * true: [ISO 7.8.1]
 * The predicate succeeds once.
 */
% true
:- special(true/0, 'SpecialControl', 1).
:- set_predicate_property(true/0, visible(public)).
:- set_predicate_property(true/0, sys_notrace).
% otherwise
:- special(otherwise/0, 'SpecialControl', 1).
:- set_predicate_property(otherwise/0, visible(public)).
:- set_predicate_property(otherwise/0, sys_notrace).

/**
 * !: [ISO 7.8.4]
 * The predicate removes pending choice points between the first
 * non-cut-transparent parent goal invocation and this goal and
 * then succeeds once.
 */
% !
:- special(!/0, 'SpecialControl', 2).
:- set_predicate_property(!/0, visible(public)).
:- set_predicate_property(!/0, sys_notrace).

/******************************************************************/
/* Throw Catch                                                    */
/******************************************************************/

/**
 * throw(E): [ISO 7.8.9]
 * The predicate fills the stack trace if necessary
 * and then raises the exception E.
 */
% throw(+Exception)
throw(V) :-
   var(V),
   throw(error(instantiation_error,_)).
throw(error(M,T)) :-
   var(T), !,
   sys_fetch_stack(T),
   sys_raise(error(M,T)).
throw(warning(M,T)) :-
   var(T), !,
   sys_fetch_stack(T),
   sys_raise(warning(M,T)).
throw(B) :-
   sys_raise(B).
:- set_predicate_property(throw/1, visible(public)).
:- set_predicate_property(throw/1, sys_notrace).

/**
 * sys_fetch_stack(T):
 * The predicate retrieves the current stack trace.
 */
% sys_fetch_stack(-Trace)
:- special(sys_fetch_stack/1, 'SpecialControl', 3).
:- set_predicate_property(sys_fetch_stack/1, visible(private)).

/**
 * sys_raise(E):
 * The predicate raises the exception E.
 */
% sys_raise(+Exception)
:- special(sys_raise/1, 'SpecialControl', 4).
:- set_predicate_property(sys_raise/1, visible(public)).

/**
 * catch(A, E, B): [ISO 7.8.9]
 * The predicate succeeds whenever A succeeds. When an exception is thrown
 * during the execution of A, this exception is non-reserved and this exception
 * unifies with E then the predicate succeeds whenever B succeeds. Otherwise
 * this exception is re-thrown.
 */
% catch(+Goal, +Pattern, +Goal)
catch(A, E, B) :-
   sys_trap(A, F, sys_handle_ball(F, E, B)).
:- set_predicate_property(catch/3, visible(public)).
:- set_predicate_property(catch/3, (meta_predicate catch(0,?,0))).
:- set_predicate_property(catch/3, sys_notrace).

/**
 * sys_handle_ball(F, E, B):
 * The predicate handles the exception F for the pattern E and the goal B.
 */
% sys_handle_ball(+Exception, +Pattern, +Goal)
sys_handle_ball(F, _, _) :-
   sys_reserved_ball(F), !,
   sys_raise(F).
sys_handle_ball(E, E, B) :- !,
   call(B).
sys_handle_ball(F, _, _) :-
   sys_raise(F).
:- set_predicate_property(sys_handle_ball/3, visible(private)).
:- set_predicate_property(sys_handle_ball/3, (meta_predicate sys_handle_ball(?,?,0))).

/**
 * sys_reserved_ball(E):
 * The predicate succeeds when E is a reserved exception.
 */
% sys_reserved_ball(+Exception)
sys_reserved_ball(V) :-
   var(V), !, fail.
sys_reserved_ball(error(V,_)) :-
   var(V), !, fail.
sys_reserved_ball(error(system_error(_),_)) :- !.
sys_reserved_ball(cause(E,_)) :-
   sys_reserved_ball(E).
:- set_predicate_property(sys_reserved_ball/1, visible(private)).

/**
 * sys_trap(A, E, B):
 * The predicate succeeds whenever A succeeds. When an exception is
 * thrown during the execution of A and this exception unifies with E
 * then the predicate succeeds whenever B succeeds. Otherwise this
 * exception is re-thrown.
 */
% sys_trap(+Goal, +Pattern, +Goal)
:- special(sys_trap/3, 'SpecialControl', 5).
:- set_predicate_property(sys_trap/3, visible(public)).
:- set_predicate_property(sys_trap/3, (meta_predicate sys_trap(0,?,0))).

/******************************************************************/
/* Try Finally                                                    */
/******************************************************************/

/**
 * try_call_finally(S, G, T):
 * The predicate succeeds whenever G succeeds. Calling S on the
 * call and redo port, and calling T on the exit, fail and exception
 * port. The predicate can also handle reserved exception.
 */
% try_call_finally(+Goal, +Goal, +Goal)
try_call_finally(S, G, T) :-
   sys_or_fail(S, T),
   sys_trap(G, E, sys_before_ball(T, E)),
   sys_or_fail(T, S).
:- set_predicate_property(try_call_finally/3, visible(public)).
:- set_predicate_property(try_call_finally/3, (meta_predicate try_call_finally(0,0,0))).
:- set_predicate_property(try_call_finally/3, sys_notrace).

% sys_or_fail(+Goal, +Goal)
sys_or_fail(S, _) :-
   call(S).
sys_or_fail(_, T) :-
   call(T), fail.
:- set_predicate_property(sys_or_fail/2, visible(private)).

% sys_before_ball(+Goal, +Term)
sys_before_ball(T, E) :-
   call(T),
   sys_raise(E).
:- set_predicate_property(sys_before_ball/2, visible(private)).

/**
 * call_finally(G, T):
 * The predicate succeeds whenever G succeeds. Calling T on the
 * exit, fail and exception port. The predicate can also handle
 * reserved exception.
 */
% call_finally(+Goal, +Goal)
call_finally(G, T) :-
   sys_or_fail(T),
   sys_trap(G, E, sys_before_ball(T, E)),
   call(T).
:- set_predicate_property(call_finally/2, visible(public)).
:- set_predicate_property(call_finally/2, (meta_predicate call_finally(0,0))).
:- set_predicate_property(call_finally/2, sys_notrace).

% sys_or_fail(+Goal)
sys_or_fail(_).
sys_or_fail(T) :-
   call(T), fail.
:- set_predicate_property(sys_or_fail/1, visible(private)).
