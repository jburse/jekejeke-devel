/**
 * The backtracking control flow can be modified by the cut (!)/0.
 * The cut will remove the choice points from the head to the cut,
 * including a head choice point. Common programming patterns
 * involving the cut are provided in the forms of the predicate
 * once/1 and (\+)/1. Both predicates will remove any backtracking
 * from the goal argument.
 *
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
 * reserved exceptions.
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

:- sys_callable_property_chk(here, sys_context/1, [sys_context(C)]),
   set_source_property(C, use_package(foreign(jekpro/model/builtin))).
:- sys_callable_property_chk(here, sys_context/1, [sys_context(C)]),
   reset_source_property(C, sys_source_visible(public)).

:- sys_neutral_oper(prefix(\+)).
:- set_oper_property(prefix(\+), op(900, fy)).
:- set_oper_property(prefix(\+), visible(public)).

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
% false
:- special(false/0, 'SpecialControl', 0).
:- set_predicate_property(false/0, visible(public)).

/**
 * true: [ISO 7.8.1]
 * otherwise:
 * The predicate succeeds once.
 */
% true
:- special(true/0, 'SpecialControl', 1).
:- set_predicate_property(true/0, visible(public)).
% otherwise
:- special(otherwise/0, 'SpecialControl', 1).
:- set_predicate_property(otherwise/0, visible(public)).

/**
 * !: [ISO 7.8.4]
 * The predicate removes pending choice points between the first
 * non-cut-transparent parent goal invocation and this goal and
 * then succeeds once.
 */
% !
:- special(!/0, 'SpecialControl', 2).
:- set_predicate_property(!/0, visible(public)).

/**
 * once(A): [ISO 8.15.2]
 * The predicate succeeds once if A succeeds. Otherwise,
 * the predicate fails.
 */
% once(+Goal)
once(X) :- X, !.
:- set_predicate_property(once/1, visible(public)).
:- set_predicate_property(once/1, meta_predicate([0])).
:- sys_callable_property_chk(here, sys_context/1, [sys_context(C)]),
   set_predicate_property(once/1, sys_meta_predicate(C)).

/**
 * \+ A: [ISO 8.15.1]
 * When A succeeds, then the predicate fails. Otherwise,
 * the predicate succeeds.
 */
% \+(+Goal)
\+ X :- X, !, fail.
\+ _.
:- set_predicate_property((\+)/1, visible(public)).
:- set_predicate_property((\+)/1, meta_predicate([0])).
:- sys_callable_property_chk(here, sys_context/1, [sys_context(C)]),
   set_predicate_property((\+)/1, sys_meta_predicate(C)).

/******************************************************************/
/* Throw                                                          */
/******************************************************************/

/**
 * throw(B): [ISO 7.8.9]
 * throw(E, B):
 * The predicate fills the stack trace if necessary
 * and then raises the exception B. The binary predicate
 * allows specifying  a primary exception E.
 */
% throw(+Exception)
throw(V) :- var(V), throw(error(instantiation_error, _)).
throw(B) :-
   sys_fill_stack(B),
   sys_raise(B).
:- set_predicate_property(throw/1, visible(public)).

% throw(+Exception, +Exception)
throw(_, V) :- var(V), throw(error(instantiation_error, _)).
throw(E, B) :-
   sys_append_cause(E, B, C),
   sys_fill_stack(B),
   sys_raise(C).
:- set_predicate_property(throw/2, visible(public)).

% sys_append_cause(+Exception, +Exception, -Exception)
sys_append_cause(E, _, _) :- var(E), throw(error(instantiation_error, _)).
sys_append_cause(cause(E, F), B, cause(E, C)) :- !,
   sys_append_cause(F, B, C).
sys_append_cause(E, B, cause(E, B)).
:- set_predicate_property(sys_append_cause/3, visible(private)).

% sys_fill_stack(+Exception)
sys_fill_stack(error(_, T)) :- var(T), !,
   sys_fetch_stack(T).
sys_fill_stack(warning(_, T)) :- var(T), !,
   sys_fetch_stack(T).
sys_fill_stack(_).
:- set_predicate_property(sys_fill_stack/1, visible(private)).

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

/******************************************************************/
/* Catch                                                          */
/******************************************************************/

/**
 * catch(A, E, B): [ISO 7.8.9]
 * The predicate succeeds whenever A succeeds. When an exception is thrown
 * during the execution of A, this exception is non-reserved and this exception
 * unifies with E then the predicate succeeds whenever B succeeds. Otherwise,
 * the exception is re-thrown.
 */
% catch(+Goal, +Pattern, +Goal)
catch(A, E, B) :-
   sys_trap(A, E, sys_ball_handler(E, B)).
:- set_predicate_property(catch/3, visible(public)).
:- set_predicate_property(catch/3, meta_predicate([0, ?, 0])).
:- sys_callable_property_chk(here, sys_context/1, [sys_context(C)]),
   set_predicate_property(catch/3, sys_meta_predicate(C)).

% sys_ball_handler(+Exception, +Goal)
sys_ball_handler(E, _) :- sys_error_type(E, system_error(_)), !,
   sys_raise(E).
sys_ball_handler(E, _) :- sys_error_type(E, limit_error(_)), !,
   sys_raise(E).
sys_ball_handler(_, B) :-
   B.
:- set_predicate_property(sys_ball_handler/2, visible(private)).
:- set_predicate_property(sys_ball_handler/2, meta_predicate([?, 0])).
:- sys_callable_property_chk(here, sys_context/1, [sys_context(C)]),
   set_predicate_property(sys_ball_handler/2, sys_meta_predicate(C)).

/**
 * sys_trap(A, E, B):
 * The predicate succeeds whenever A succeeds. When an exception is
 * thrown during the execution of A and this exception unifies with E
 * then the predicate succeeds whenever B succeeds. Otherwise, the
 * exception is re-thrown.
 */
% sys_trap(+Goal, +Pattern, +Goal)
:- special(sys_trap/3, 'SpecialControl', 5).
:- set_predicate_property(sys_trap/3, visible(public)).
:- set_predicate_property(sys_trap/3, meta_predicate([0, ?, 0])).
:- sys_callable_property_chk(here, sys_context/1, [sys_context(C)]),
   set_predicate_property(sys_trap/3, sys_meta_predicate(C)).

/**
 * sys_error_type(E, T):
 * The predicate succeeds in T with the error type of exception E.
 */
% sys_error_type(+Exception, -Type)
sys_error_type(error(T, _), T).
sys_error_type(cause(E, _), T) :- sys_error_type(E, T).
:- set_predicate_property(sys_error_type/2, visible(public)).
