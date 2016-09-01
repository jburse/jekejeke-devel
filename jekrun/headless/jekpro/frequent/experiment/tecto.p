/**
 * Higher order function application and higher order function abstraction
 * has been extended to DCG grammars. This extension uses the same syntax
 * as the non-grammar variants. Thus it is possible to use call/n in a DCG
 * grammar to dynamically build non-terminals. It is further possible
 * to use (\)/2 to have lambda expressions in DCG grammars.
 *
 * Example:
 * ?- [user].
 * fruit(apple) --> "apple".
 * fruit(orange) --> "orange".
 * fruit(pear) --> "pear".
 *
 * Yes
 * ?- phrase(call(fruit, pear), X), atom_codes(Y, X).
 * X = [112,101,97,114],
 * Y = pear
 *
 * Goal expansion is frozen into a phrase goal when the first argument of
 * call/n is a variable. Goal expansion is attempt again when the corresponding
 * phrase goal is called. Since goal expansion is already attempted the first
 * time at compile-time one has to be careful not to compile diverging
 * expressions. The following results in a looping interpreter:
 *
 * Example:
 * ?- [user].
 * p --> call(X\call(X,X),X\call(X,X)).
 * Error: Execution aborted since memory threshold exceeded.
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

/**
 * Implementation note: This module defines additional clauses
 * for the 3 multifile predicates parse_impl/3, phrase_expansion/4
 * and sys_phrase_expansion/4 from the dcg module. In this way it
 * extends the expansion and execution of dcgs.
 */

:- package(library(jekpro/frequent/experiment)).

:- module(tecto, []).
:- use_module(library(experiment/abstract)).

/**********************************************************/
/* Goal Execution                                         */
/**********************************************************/

% user:phrase(+Goal, +Term, -Term)
:- public user:phrase/3.
:- multifile user:phrase/3.
:- meta_predicate user:phrase(2,?,?).
user:phrase(call(P, _), _, _) :-
   sys_var(P),
   throw(error(instantiation_error,_)).
user:phrase(call(P, _, _), _, _) :-
   sys_var(P),
   throw(error(instantiation_error,_)).
user:phrase(call(P, _, _, _), _, _) :-
   sys_var(P),
   throw(error(instantiation_error,_)).
user:phrase(call(P, _, _, _, _), _, _) :-
   sys_var(P),
   throw(error(instantiation_error,_)).
user:phrase(call(P, _, _, _, _, _), _, _) :-
   sys_var(P),
   throw(error(instantiation_error,_)).

% user:sys_phrase_delay(+Goal)
:- public user:sys_phrase_delay/1.
:- multifile user:sys_phrase_delay/1.
user:sys_phrase_delay(call(P,_)) :-
   sys_var(P).
user:sys_phrase_delay(call(P,_,_)) :-
   sys_var(P).
user:sys_phrase_delay(call(P,_,_,_)) :-
   sys_var(P).
user:sys_phrase_delay(call(P,_,_,_,_)) :-
   sys_var(P).
user:sys_phrase_delay(call(P,_,_,_,_,_)) :-
   sys_var(P).

/**********************************************************/
/* Goal Rewriting Steadfast                               */
/**********************************************************/

% user:phrase_expansion(+Grammar, +List, -List, -Goal)
:- public user:phrase_expansion/4.
:- multifile user:phrase_expansion/4.
:- meta_predicate user:phrase_expansion(2,?,?,0).
:- set_predicate_property(user:phrase_expansion/4, sys_noexpand).

/**
 * call(P, Y1, .., Yn) (grammar):
 * The grammar connective is defined for 1 ≤ n ≤ 5. The phrase
 * call(p(X1, .., Xm), Y1, .., Yn) expands to the phrase
 * p(X1, .., Xm, Y1, .., Yn).
 */
user:phrase_expansion(call(P, _), _, _, _) :-
   sys_var(P), !, fail.
user:phrase_expansion(call(P, A), I, O, phrase(Q, I, O)) :-
   sys_modext_args(P, A, Q).
user:phrase_expansion(call(P, _, _), _, _, _) :-
   sys_var(P), !, fail.
user:phrase_expansion(call(P, A, B), I, O, phrase(Q, I, O)) :-
   sys_modext_args(P, A, B, Q).
user:phrase_expansion(call(P, _, _, _), _, _, _) :-
   sys_var(P), !, fail.
user:phrase_expansion(call(P, A, B, C), I, O, phrase(Q, I, O)) :-
   sys_modext_args(P, A, B, C, Q).
user:phrase_expansion(call(P, _, _, _, _), _, _, _) :-
   sys_var(P), !, fail.
user:phrase_expansion(call(P, A, B, C, D), I, O, phrase(Q, I, O)) :-
   sys_modext_args(P, A, B, C, D, Q).
user:phrase_expansion(call(P, _, _, _, _, _), _, _, _) :-
   sys_var(P), !, fail.
user:phrase_expansion(call(P, A, B, C, D, E), I, O, phrase(Q, I, O)) :-
   sys_modext_args(P, A, B, C, D, E, Q).

/**
 * \(X, A, Y1, .., Yn) (grammar):
 * The grammar connective is defined for 1 ≤ n ≤ 5. The phrase
 * \(X, A, Y1, .., Yn) expands to the phrase call(A[X/Y1], Y2, ..., Yn).
 */
user:phrase_expansion(\(X, A, Y), I, O, phrase(Q, I, O)) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X,B,L), rec(Y,Q,L)).
user:phrase_expansion(\(X, A, Y, Z), I, O, phrase(call(Q, Z), I, O)) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X,B,L), rec(Y,Q,L)).
user:phrase_expansion(\(X, A, Y, Z, T), I, O, phrase(call(Q, Z, T), I, O)) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X,B,L), rec(Y,Q,L)).
user:phrase_expansion(\(X, A, Y, Z, T, U), I, O, phrase(call(Q, Z, T, U), I, O)) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X,B,L), rec(Y,Q,L)).
user:phrase_expansion(\(X, A, Y, Z, T, U, V), I, O, phrase(call(Q, Z, T, U, V), I, O)) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X,B,L), rec(Y,Q,L)).

% user:phrase_abnormal(+Grammar)
:- public user:phrase_abnormal/1.
:- multifile user:phrase_abnormal/1.
user:phrase_abnormal(call(_,_)).
user:phrase_abnormal(call(_,_,_)).
user:phrase_abnormal(call(_,_,_,_)).
user:phrase_abnormal(call(_,_,_,_,_)).
user:phrase_abnormal(call(_,_,_,_,_,_)).
user:phrase_abnormal(\(_,_,_)).
user:phrase_abnormal(\(_,_,_,_)).
user:phrase_abnormal(\(_,_,_,_,_)).
user:phrase_abnormal(\(_,_,_,_,_,_)).
user:phrase_abnormal(\(_,_,_,_,_,_,_)).

/**********************************************************/
/* Goal Rewriting Non-Steadfast                           */
/**********************************************************/

% user:sys_phrase_expansion(+Grammar, +List, -List, -Goal)
:- public user:sys_phrase_expansion/4.
:- multifile user:sys_phrase_expansion/4.
:- meta_predicate user:sys_phrase_expansion(2,?,?,0).
:- set_predicate_property(user:sys_phrase_expansion/4, sys_noexpand).

user:sys_phrase_expansion(call(P, A), I, O, phrase(call(P, A), I, O)) :-
   sys_var(P).
user:sys_phrase_expansion(call(P, A), I, O, sys_phrase(Q, I, O)) :-
   sys_modext_args(P, A, Q).
user:sys_phrase_expansion(call(P, A, B), I, O, phrase(call(P, A, B), I, O)) :-
   sys_var(P).
user:sys_phrase_expansion(call(P, A, B), I, O, sys_phrase(Q, I, O)) :-
   sys_modext_args(P, A, B, Q).
user:sys_phrase_expansion(call(P, A, B, C), I, O, phrase(call(P, A, B, C), I, O)) :-
   sys_var(P).
user:sys_phrase_expansion(call(P, A, B, C), I, O, sys_phrase(Q, I, O)) :-
   sys_modext_args(P, A, B, C, Q).
user:sys_phrase_expansion(call(P, A, B, C, D), I, O, phrase(call(P, A, B, C, D), I, O)) :-
   sys_var(P).
user:sys_phrase_expansion(call(P, A, B, C, D), I, O, sys_phrase(Q, I, O)) :-
   sys_modext_args(P, A, B, C, D, Q).
user:sys_phrase_expansion(call(P, A, B, C, D, E), I, O, phrase(call(P, A, B, C, D, E), I, O)) :-
   sys_var(P).
user:sys_phrase_expansion(call(P, A, B, C, D, E), I, O, sys_phrase(Q, I, O)) :-
   sys_modext_args(P, A, B, C, D, E, Q).
user:sys_phrase_expansion(\(X, A, Y), I, O, sys_phrase(Q, I, O)) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X,B,L), rec(Y,Q,L)).
user:sys_phrase_expansion(\(X, A, Y, Z), I, O, sys_phrase(call(Q, Z), I, O)) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X,B,L), rec(Y,Q,L)).
user:sys_phrase_expansion(\(X, A, Y, Z, T), I, O, sys_phrase(call(Q, Z, T), I, O)) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X,B,L), rec(Y,Q,L)).
user:sys_phrase_expansion(\(X, A, Y, Z, T, U), I, O, sys_phrase(call(Q, Z, T, U), I, O)) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X,B,L), rec(Y,Q,L)).
user:sys_phrase_expansion(\(X, A, Y, Z, T, U, V), I, O, sys_phrase(call(Q, Z, T, U, V), I, O)) :-
   sys_goal_kernel(A, B),
   sys_goal_globals(X^A, L),
   copy_term(rec(X,B,L), rec(Y,Q,L)).
