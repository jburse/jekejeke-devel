/**
 * This Jekejeke Minlog module provides a subject to occurs check
 * constraint sto/1 and a term inequality constraint neq/2. To allow
 * good performance both constraints perform an attribute variable
 * verification before their attribute variables are instantiated
 * and new sub constraints are registered:
 *
 * Example:
 * ?- sto(X), X = f(X).
 * No
 * ?- neq(f(X,X),f(Y,Z)), X = Y.
 * Y = X,
 * neq(X, Z)
 *
 * The subject to occurs check has to be initially called with
 * an acyclic term, but it will subsequently assure the subject to
 * occurs check as required. There is no interaction of the
 * subject to occurs check with the term inequality, and the term
 * inequality neither decided for or against the occurs check.
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

:- package(library(jekmin/reference/term)).
:- use_package(library(jekmin/reference/minimal)).
:- use_package(library(jekpro/frequent/misc)).

:- module(herbrand, []).
:- use_module(library(minimal/assume)).
:- use_module(library(misc/struc)).
:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).
:- use_module(library(advanced/sets)).
:- use_module(library(experiment/maps)).

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
   ;  sys_assume_hooks(L)).

/**
 * sys_hook_neq(S, V, _):
 */
% sys_hook_neq(+Warp, +Var, +Term)
:- private sys_hook_neq/3.
sys_hook_neq(S, _, _) :-
   sys_melt_var(S, G),
   sys_listeners_neqs(G, L),
   sys_retire_hooks(L, sys_hook_neq(S)),
   sys_make_and(G, F),
   sys_assume_cont(F).

% sys_retire_hooks(+Set, +Term)
:- private sys_retire_hooks/2.
:- meta_predicate sys_retire_hooks(?,2).
sys_retire_hooks([V|L], H) :-
   sys_clause_hook(V, H, K),
   sys_retire_ref(K),
   sys_retire_hooks(L, H).
sys_retire_hooks([], _).

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
residue:sys_current_eq(V, neq(S)) :-
   sys_clause_hook(V, sys_hook_neq(S), _).

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
residue:sys_unwrap_eq(neq(S), [F|L], L) :-
   sys_melt_var(S, G),
   sys_make_and(G, F).

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
   sys_assume_hooks(L).

/**
 * sys_assume_hooks(L):
 * The predicate continues the subject to occurs check for the
 * variables in the list L.
 */
% sys_assume_hooks(+Set)
:- private sys_assume_hooks/1.
:- meta_predicate sys_assume_hooks(?,2).
sys_assume_hooks([X|Y]) :-
   sys_ensure_hook(X, sys_hook_sto),
   sys_assume_hooks(Y).
sys_assume_hooks([]).

/**
 * neq(S, T):
 * The predicate checks S and T for inequality and establishes
 * variable constraints, so that this inequality is maintained
 * in the continuation. The inequality neither decides for or
 * against the occurs check.
 */
% neq(+Term, +Term)
:- public neq/2.
neq(X, Y) :-
   sys_reduce_neq(X, Y, [], G), !,
   G \== [],
   sys_freeze_var(W, S),
   W = G,
   sys_listeners_neqs(G, L),
   sys_assume_hooks(L, sys_hook_neq(S)).
neq(_, _).

% sys_assume_hooks(+Set, +Closure)
:- private sys_assume_hooks/2.
sys_assume_hooks([V|L], H) :-
   sys_compile_hook(V, H, K),
   sys_assume_ref(K),
   sys_assume_hooks(L, H).
sys_assume_hooks([], _).

/******************************************************/
/* Inequality Reduction                               */
/******************************************************/

/**
 * sys_reduce_neq(S, T, L, R):
 * The predicate derefs the terms S and T and then checks for
 * inequatlity and establishes residual pairs L in an extension
 * of the residual pairs R.
 */
% sys_reduce_neq(+Term, +Term, +Map, -Map).
:- private sys_reduce_neq/4.
sys_reduce_neq(S, T, L, R) :-
   sys_deref_term(S, A, L),
   sys_deref_term(T, B, L),
   sys_reduce_uninst(A, B, L, R).

/**
 * sys_deref_term(S, T, L):
 * The predicate succeeds with the deref T of the term S
 * in the map L.
 */
% sys_deref_term(+Term, -Term, +Map)
:- private sys_deref_term/3.
sys_deref_term(X, T, L) :-
   var(X),
   get(L, X, S), !,
   sys_deref_term(S, T, L).
sys_deref_term(S, S, _).

/**
 * sys_reduce_uninst(S, T, L, R):
 * The predicate checks the terms S and T for inequatlity and
 * establishes residual pairs L in an extension of the
 * residual pairs R.
 */
% sys_reduce_uninst(+Term, +Term, +Map, -Map).
:- private sys_reduce_uninst/4.
sys_reduce_uninst(X, Y, L, R) :-
   var(X),
   var(Y),
   X == Y, !,
   R = L.
sys_reduce_uninst(T, X, L, R) :-
   var(X), !,
   put(L, X, T, R).
sys_reduce_uninst(X, T, L, R) :-
   var(X), !,
   put(L, X, T, R).
sys_reduce_uninst(S, T, _, _) :-
   functor(S, F, N),
   functor(T, G, M),
   F/N \== G/M, !, fail.
sys_reduce_uninst(S, T, L, R) :-
   S =.. [_|F],
   T =.. [_|G],
   sys_reduce_neqs(F, G, L, R).

/**
 * sys_reduce_neqs(S, T, L, R):
 * The predicate checks the lists S and T for inequatlity and
 * establishes residual pairs L in extension of the residual
 * pairs R.
 */
% sys_reduce_neqs(+List, +List, +Map, -Map)
:- private sys_reduce_neqs/4.
sys_reduce_neqs([S|F], [T|G], L, R) :-
   sys_reduce_neqs(F, G, L, M),
   sys_reduce_neq(S, T, M, R).
sys_reduce_neqs([], [], L, L).

/**
 * sys_listeners_neqs(G, L):
 */
% sys_listeners_neqs(+Map, -Set)
:- private sys_listeners_neqs/2.
sys_listeners_neqs([X-Y|G], [X,Y|L]) :-
   var(Y), !,
   sys_listeners_neqs(G, L).
sys_listeners_neqs([X-_|G], [X|L]) :-
   sys_listeners_neqs(G, L).
sys_listeners_neqs([], []).

/**
 * sys_make_and(G, F):
 */
% sys_make_and(+Map, -Goal)
:- private sys_make_and/2.
sys_make_and([X-T,U|V], neq((X,P),(T,Q))) :- !,
   sys_make_and([U|V], neq(P,Q)).
sys_make_and([X-T], neq(X,T)).
