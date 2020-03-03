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
 * in the continuation. The inequality neither decides for or
 * against the occurs check.
 */
% dif(+Term, +Term)
:- public dif/2.
dif(X, Y) :-
   sys_reduce_dif(X, Y, [], G), !, G \== [],
   sys_freeze_var(W, S),
   W = sys_data_dif(N, G),
   sys_listeners_difs(G, R),
   term_variables(R, L),
   sys_serno_hooks(L, sys_hook_dif(S), N),
   depositz_ref(N).
dif(_, _).

/**
 * sys_listeners_difs(G, L):
 */
% sys_listeners_difs(+Map, -Set)
:- private sys_listeners_difs/2.
sys_listeners_difs([X-Y|G], [X, Y|L]) :-
   var(Y), !,
   sys_listeners_difs(G, L).
sys_listeners_difs([X-_|G], [X|L]) :-
   sys_listeners_difs(G, L).
sys_listeners_difs([], []).

/******************************************************/
/* Inequality Reduction                               */
/******************************************************/

/**
 * sys_reduce_dif(S, T, L, R):
 * The predicate derefs the terms S and T and then checks for
 * inequality and establishes residual pairs R as an extension
 * of the residual pairs L.
 */
% sys_reduce_dif(+Term, +Term, +Map, -Map).
:- private sys_reduce_dif/4.
sys_reduce_dif(S, T, L, R) :-
   sys_deref_term(S, L, A),
   sys_deref_term(T, L, B),
   sys_reduce_uninst(A, B, L, R).

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
 * sys_reduce_uninst(S, T, L, R):
 * The predicate checks the terms S and T for inequality and
 * establishes residual pairs R as an extension of the
 * residual pairs L.
 */
% sys_reduce_uninst(+Term, +Term, +Map, -Map).
:- private sys_reduce_uninst/4.
sys_reduce_uninst(X, Y, L, R) :- var(X), var(Y), X == Y, !,
   R = L.
sys_reduce_uninst(X, T, L, R) :- var(X), !,
   put(L, X, T, R).
sys_reduce_uninst(T, X, L, R) :- var(X), !,
   put(L, X, T, R).
sys_reduce_uninst(S, T, _, _) :-
   functor(S, F, N),
   functor(T, G, M),
   F/N \== G/M, !, fail.
sys_reduce_uninst(S, T, L, R) :-
   S =.. [_|F],
   T =.. [_|G],
   sys_reduce_difs(F, G, L, R).

/**
 * sys_reduce_difs(S, T, L, R):
 * The predicate checks the lists S and T for inequality and
 * establishes residual pairs R as an extension of the residual
 * pairs R, taking into account the map M.
 */
% sys_reduce_difs(+List, +List, +Map, +Map, -Map)
:- private sys_reduce_difs/4.
sys_reduce_difs([S|F], [T|G], L, R) :-
   sys_reduce_dif(S, T, L, H),
   sys_reduce_difs(F, G, H, R).
sys_reduce_difs([], [], L, L).

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
   (  contains(V, L) -> fail
   ;  sys_ensure_stos(L)).

/**
 * sys_hook_dif(S, V, T):
 */
% sys_hook_dif(+Warp, +Var, +Term)
:- private sys_hook_dif/3.
sys_hook_dif(S, _, _) :-
   sys_melt_var(S, sys_data_dif(L, G)),
   withdrawz_ref(L),
   sys_make_dif(G, P, Q),
   sys_assume_cont(dif(P, Q)).

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
   sys_melt_var(S, sys_data_dif(_, G)),
   sys_make_dif(G, P, Q).

/**
 * sys_make_dif(M, L, R):
 * The predicate converts the non-empty map M into two comma
 * lists L and R.
 */
% sys_make_dif(+Map, -List, -List)
:- private sys_make_dif/3.
sys_make_dif([X-T, U|V], (X, L), (T, R)) :- !,
   sys_make_dif([U|V], L, R).
sys_make_dif([X-T], X, T).
