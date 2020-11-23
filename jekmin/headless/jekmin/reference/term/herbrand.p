/**
 * This Jekejeke Minlog module provides a subject to occurs check
 * constraint sto/1 and a term inequality constraint dif/2. To allow
 * good performance both constraints perform an attribute variable
 * verification before their attribute variables are instantiated
 * and new sub constraints are registered:
 *
 * Example:
 * ?- sto(X), X = f(X).
 * No
 * ?- dif(f(X,X),f(Y,Z)), X = Y.
 * Y = X,
 * dif(X, Z)
 *
 * The subject to occurs check has to be initially called with
 * an acyclic term, but it will subsequently assure the subject to
 * occurs check as required. The subject to occurs check is ordered
 * so that it is always checked before some inequality constraint
 * is checked against the same variable.
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

:- package(library(jekmin/reference/term)).
:- use_package(library(jekmin/reference/minimal)).
:- use_package(library(jekpro/frequent/misc)).

:- module(herbrand, []).
:- use_module(library(minimal/assume)).
:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).
:- use_module(library(experiment/cont)).
:- use_module(library(advanced/sets)).
:- use_module(library(experiment/maps)).

/********************************************************/
/* Constraint Posting                                   */
/********************************************************/

/**
 * sto(T):
 * The predicate continues the subject to occurs check for
 * the term T. The term T has to be acyclic when calling
 * this predicate.
 */
% sto(+Term)
:- public sto/1.
sto(T) :-
   term_variables(T, L),
   sys_ensure_stos(L).

% sys_ensure_stos(+List)
:- private sys_ensure_stos/1.
sys_ensure_stos([X|Y]) :-
   sys_ensure_sto(X),
   sys_ensure_stos(Y).
sys_ensure_stos([]).

% sys_ensure_stos(+Var)
:- private sys_ensure_sto/1.
sys_ensure_sto(V) :-
   sys_clause_hook(V, sys_hook_sto, _), !.
sys_ensure_sto(V) :-
   sys_ensure_serno(V),
   sys_compile_hook(V, sys_hook_sto, K),
   deposita_ref(K).

/**
 * dif(S, T):
 * The predicate checks S and T for inequality and establishes
 * variable constraints, so that this inequality is maintained
 * in the continuation.
 */
% dif(+Term, +Term)
:- public dif/2.
dif(X, Y) :-
   sys_reduce(X, Y, [], G), !, G \== [],
   sys_freeze_var(W, S),
   W = sys_data(N, G),
   sys_listeners(G, R),
   term_variables(R, L),
   sys_serno_hooks(L, sys_hook_dif(S), N),
   depositz_ref(N).
dif(_, _).

/**
 * dif_with_occurs_check(S, T):
 * The predicate checks S and T for inequality with occurs check
 * and establishes variable constraints, so that this inequality
 * is maintained in the continuation.
 */
% dif_with_occurs_check(+Term, +Term)
:- public dif_with_occurs_check/2.
dif_with_occurs_check(X, Y) :-
   sys_reduce_with_occurs_check(X, Y, [], G), !, G \== [],
   sys_freeze_var(W, S),
   W = sys_data(N, G),
   term_variables(G, L),
   sys_serno_hooks(L, sys_hook_dif_with_occurs_check(S), N),
   depositz_ref(N).
dif_with_occurs_check(_, _).

/**
 * unifiable(S, T, L):
 * The predicate checks S and T for equality and returns
 * in L a binding that would make S and T this equal.
 */
% unifiable(+Term, +Term, +List)
:- public unifiable/3.
unifiable(X, Y, G) :-
   sys_reduce(X, Y, [], G).

/**
 * unifiable_with_occurs_check(S, T, L):
 * The predicate checks S and T for equality with occurs check
 * and returns in L a binding that would make S and T this equal.
 */
% unifiable_with_occurs_check(+Term, +Term, +List)
:- public unifiable_with_occurs_check/3.
unifiable_with_occurs_check(X, Y, G) :-
   sys_reduce_with_occurs_check(X, Y, [], G).

/**
 * sys_listeners(G, L):
 */
% sys_listeners(+Map, -Set)
:- private sys_listeners/2.
sys_listeners([X-Y|G], [X, Y|L]) :-
   var(Y), !,
   sys_listeners(G, L).
sys_listeners([X-_|G], [X|L]) :-
   sys_listeners(G, L).
sys_listeners([], []).

/******************************************************/
/* Equality Reduction                                 */
/******************************************************/

/**
 * sys_reduce(S, T, L, R):
 * The predicate derefs the terms S and T and then checks for
 * equality and establishes residual pairs R as an extension
 * of the residual pairs L.
 */
% sys_reduce(+Term, +Term, +Map, -Map).
:- private sys_reduce/4.
sys_reduce(S, T, L, R) :-
   sys_deref_term(S, L, A),
   sys_deref_term(T, L, B),
   sys_reduce2(A, B, L, R).

/**
 * sys_deref_term(S, L, T):
 * The predicate succeeds with the deref T of the term S
 * in the map L.
 */
% sys_deref_term(+Term, +Map, -Term)
:- private sys_deref_term/3.
sys_deref_term(X, L, T) :- var(X), get(L, X, S), !,
   sys_deref_term(S, L, T).
sys_deref_term(S, _, S).

/**
 * sys_reduce2(S, T, L, R):
 * The predicate checks the terms S and T for equality and
 * establishes residual pairs R as an extension of the
 * residual pairs L.
 */
% sys_reduce2(+Term, +Term, +Map, -Map).
:- private sys_reduce2/4.
sys_reduce2(X, Y, L, R) :- var(X), var(Y), X == Y, !,
   R = L.
sys_reduce2(X, T, L, R) :- var(X), !,
   R = [X-T|L].
sys_reduce2(T, X, L, R) :- var(X), !,
   R = [X-T|L].
sys_reduce2(S, T, _, _) :-
   functor(S, F, N),
   functor(T, G, M),
   F/N \== G/M, !, fail.
sys_reduce2(S, T, L, R) :-
   S =.. [_|F],
   T =.. [_|G],
   sys_reduce3(F, G, L, R).

/**
 * sys_reduce3(S, T, L, R):
 * The predicate checks the lists S and T for equality and
 * establishes residual pairs R as an extension of the
 * residual pairs L.
 */
% sys_reduce3(+List, +List, +Map, +Map, -Map)
:- private sys_reduce3/4.
sys_reduce3([S|F], [T|G], L, R) :-
   sys_reduce(S, T, L, H),
   sys_reduce3(F, G, H, R).
sys_reduce3([], [], L, L).

/******************************************************/
/* Equality With Occurs Check Reduction               */
/******************************************************/

/**
 * sys_reduce_with_occurs_check(S, T, L, R):
 * The predicate derefs the terms S and T and then checks for
 * equality with occurs check and establishes residual pairs R
 * as an extension of the residual pairs L.
 */
% sys_reduce_with_occurs_check(+Term, +Term, +Map, -Map).
:- private sys_reduce_with_occurs_check/4.
sys_reduce_with_occurs_check(S, T, L, R) :-
   sys_deref_term(S, L, A),
   sys_deref_term(T, L, B),
   sys_reduce_with_occurs_check2(A, B, L, R).

/**
 * sys_reduce_with_occurs_check2(S, T, L, R):
 * The predicate checks the terms S and T for equality with
 * occurs check and establishes residual pairs R as an
 * extension of the residual pairs L.
 */
% sys_reduce_with_occurs_check2(+Term, +Term, +Map, -Map).
:- private sys_reduce_with_occurs_check2/4.
sys_reduce_with_occurs_check2(X, Y, L, R) :- var(X), var(Y), X == Y, !,
   R = L.
sys_reduce_with_occurs_check2(X, T, L, R) :- var(X), !, \+ sys_occurs(T, X, L),
   R = [X-T|L].
sys_reduce_with_occurs_check2(T, X, L, R) :- var(X), !, \+ sys_occurs(T, X, L),
   R = [X-T|L].
sys_reduce_with_occurs_check2(S, T, _, _) :-
   functor(S, F, N),
   functor(T, G, M),
   F/N \== G/M, !, fail.
sys_reduce_with_occurs_check2(S, T, L, R) :-
   S =.. [_|F],
   T =.. [_|G],
   sys_reduce_with_occurs_check3(F, G, L, R).

/**
 * sys_reduce_with_occurs_check3(S, T, L, R):
 * The predicate checks the lists S and T for equality and
 * establishes residual pairs R as an extension of the
 * residual pairs L.
 */
% sys_reduce_with_occurs_check3(+List, +List, +Map, +Map, -Map)
:- private sys_reduce_with_occurs_check3/4.
sys_reduce_with_occurs_check3([S|F], [T|G], L, R) :-
   sys_reduce_with_occurs_check(S, T, L, H),
   sys_reduce_with_occurs_check3(F, G, H, R).
sys_reduce_with_occurs_check3([], [], L, L).

/******************************************************/
/* Variable Occurence                                 */
/******************************************************/

/**
 * sys_occurs(S, V, L):
 * The predicate derefs the term S and then checks whether
 * the variable V occurs in the term S. The variable bindings
 * are given by the map L.
 */
% sys_occurs(+Term, +Variable, +Map)
:- private sys_occurs/3.
sys_occurs(S, V, L) :-
   sys_deref_term(S, L, A),
   sys_occurs2(A, V, L).

/**
 * sys_occurs2(S, V, L):
 * The predicate checks whether the variable V occurs
 * in the term S. The variable bindings are given by
 * the map L.
 */
:- private sys_occurs2/3.
sys_occurs2(S, V, _) :- var(S), !, V == S.
sys_occurs2(S, V, L) :-
   S =.. [_|F],
   sys_occurs3(F, V, L).

/**
 * sys_occurs3(F, V, L):
 * The predicate checks whether the variable V occurs
 * in the list F. The variable bindings are given by
 * the map L.
 */
% sys_occurs3(+List, +Variable, +Map)
:- private sys_occurs3/3.
sys_occurs3([S|_], V, L) :-
   sys_occurs(S, V, L), !.
sys_occurs3([_|F], V, L) :-
   sys_occurs3(F, V, L).

/********************************************************/
/* Attribute Hooks                                      */
/********************************************************/

/**
 * sys_hook_sto(V, T):
 * The predicate tests the occurence of V in T and continues
 * the subject to occurs check for the term T.
 */
% sys_hook_sto(+Var, +Term)
:- private sys_hook_sto/2.
sys_hook_sto(V, T) :-
   term_variables(T, L),
   (  contains(L, V) -> fail
   ;  sys_ensure_stos(L)).

/**
 * sys_hook_dif(S, V, T):
 */
% sys_hook_dif(+Warp, +Var, +Term)
:- private sys_hook_dif/3.
sys_hook_dif(S, _, _) :-
   sys_melt_var(S, sys_data(L, G)),
   withdrawz_ref(L),
   sys_make(G, P, Q),
   sys_assume_cont(dif(P, Q)).

/**
 * sys_hook_dif_with_occurs_check(S, V, T):
 */
% sys_hook_dif_with_occurs_check(+Warp, +Var, +Term)
:- private sys_hook_dif_with_occurs_check/3.
sys_hook_dif_with_occurs_check(S, _, _) :-
   sys_melt_var(S, sys_data(L, G)),
   withdrawz_ref(L),
   sys_make(G, P, Q),
   sys_assume_cont(dif_with_occurs_check(P, Q)).

/********************************************************/
/* Constraint Projection                                */
/********************************************************/

/**
 * sys_current_eq(V, E):
 * The predicate succeeds for each equation E with variables
 * wrapped that listens on the variable V.
 */
% sys_current_eq(+Var, -Handle)
:- public residue:sys_current_eq/2.
:- multifile residue:sys_current_eq/2.
residue:sys_current_eq(V, sto(K)) :-
   sys_clause_hook(V, sys_hook_sto, _),
   sys_freeze_var(V, K).
residue:sys_current_eq(V, dif(S)) :-
   sys_clause_hook(V, sys_hook_dif(S), _).
residue:sys_current_eq(V, dif_with_occurs_check(S)) :-
   sys_clause_hook(V, sys_hook_dif_with_occurs_check(S), _).

/**
 * sys_unwrap_eq(H, I, O):
 * The predicate converts equation H with variables wrapped into
 * equations I with variables unwrapped. The list uses the end O.
 */
% sys_unwrap_eq(+Handle, -Goals, +Goals)
:- public residue:sys_unwrap_eq/3.
:- multifile residue:sys_unwrap_eq/3.
residue:sys_unwrap_eq(sto(K), [sto(V)|L], L) :-
   sys_melt_var(K, V).
residue:sys_unwrap_eq(dif(S), [dif(P, Q)|L], L) :-
   sys_melt_var(S, sys_data(_, G)),
   sys_make(G, P, Q).
residue:sys_unwrap_eq(dif_with_occurs_check(S), [dif_with_occurs_check(P, Q)|L], L) :-
   sys_melt_var(S, sys_data(_, G)),
   sys_make(G, P, Q).

/**
 * sys_make(M, L, R):
 * The predicate converts the non-empty map M into two comma
 * lists L and R.
 */
% sys_make(+Map, -List, -List)
:- private sys_make/3.
sys_make([X-T, U|V], (X, L), (T, R)) :- !,
   sys_make([U|V], L, R).
sys_make([X-T], X, T).
