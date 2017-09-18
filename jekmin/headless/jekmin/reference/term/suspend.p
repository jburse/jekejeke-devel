/**
 * This Jekejeke Minlog module provides the delay of goals
 * until certain variable conditions are satisfied. The predicate
 * freeze/2 delays until the first argument is instantiated. The
 * predicate when/2 delays until the first argument succeeds.
 *
 * Example:
 * ?- freeze(X, (write(foo), nl)).
 * freeze(X, (write(foo), nl))
 * ?- freeze(X, (write(foo), nl)), X = a.
 * foo
 * X = a
 *
 * The delayed goal is allowed to fail or to succeed multiple times.
 * The when/2 predicate currently understands as conditions conjunction
 * (C1; C2), disjunction (C1; C2), variable instantiation nonvar(V)
 * and ground-ness ground(V).
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

:- module(suspend, []).
:- use_module(library(basic/lists)).
:- use_module(library(minimal/assume)).
:- use_module(library(experiment/trail)).
:- use_module(library(experiment/attr)).
:- use_module(library(experiment/ref)).

/********************************************************/
/* Attribute Hooks                                      */
/********************************************************/

/**
 * sys_hook_freeze(R, V, _):
 */
% sys_hook_freeze(+Ref, +Var, +Term)
:- private sys_hook_freeze/3.
sys_hook_freeze(R, V, _) :-
   sys_melt_var(R, sys_data_freeze(G)),
   sys_assume_cont(freeze(V,G)).

/**
 * sys_hook_when(R, _, _):
 */
% sys_hook_when(+Ref, +Var, +Term)
:- private sys_hook_when/3.
sys_hook_when(R, _, _) :-
   sys_melt_var(R, sys_data_when(L,C,G)),
   sys_retire_ref(L),
   sys_assume_cont(when(C,G)).

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
residue:sys_current_eq(V, freeze(R,S)) :-
   sys_clause_hook(V, sys_hook_freeze(S), _),
   sys_freeze_var(V, R).
residue:sys_current_eq(V, when(R)) :-
   sys_clause_hook(V, sys_hook_when(R), _).

/**
 * sys_unwrap_eq(H, I, O):
 * The predicate converts equation H with variables wrapped into
 * equations I with variables unwrapped. The list uses the end O.
 */
% sys_unwrap_eq(+Handle, -Goals, +Goals)
:- public residue:sys_unwrap_eq/3.
:- multifile residue:sys_unwrap_eq/3.
residue:sys_unwrap_eq(freeze(R,S), [freeze(V,G)|L], L) :-
   sys_melt_var(S, sys_data_freeze(G)),
   sys_melt_var(R, V).
residue:sys_unwrap_eq(when(R), [when(C,G)|L], L) :-
   sys_melt_var(R, sys_data_when(_,C,G)).

/********************************************************/
/* Constraint Posting                                   */
/********************************************************/

/**
 * freeze(T, G):
 * If T is a variable further checks are delayed until the
 * variable is instantiated with a non-variable or another
 * variable. Otherwise the goal G is directly called.
 */
% freeze(+Term, +Goal)
:- public freeze/2.
:- meta_predicate freeze(?,0).
freeze(V, G) :-
   var(V), !,
   sys_freeze_var(W, R),
   W = sys_data_freeze(G),
   sys_compile_hook(V, sys_hook_freeze(R), K),
   sys_assume_ref(K).
freeze(_, G) :-
   call(G).

/**
 * when(C, G):
 * If C simplifies to a non-trivial condition further simplifications
 * are delayed until a variable in C is instantiated with a non-variable
 * or another variable. Otherwise the goal G is directly called.
 */
:- public when/2.
:- meta_predicate when(?,0).
when(C, G) :-
   sys_cond_simp(C, D),
   D \== true, !,
   sys_freeze_var(W, R),
   W = sys_data_when(N,D,G),
   term_variables(D, M),
   sys_compile_hooks(M, R, N),
   sys_assume_ref(N).
when(_, G) :- G.

% sys_compile_hooks(+List, +Ref, -List)
:- private sys_compile_hooks/3.
sys_compile_hooks([V|M], R, [K|W]) :-
   sys_compile_hook(V, sys_hook_when(R), K),
   sys_compile_hooks(M, R, W).
sys_compile_hooks([], _, []).

/**
 * sys_cond_simp(C, D):
 * The predicate succeeds with a simplified version D of the
 * condition C.
 */
% sys_cond_simp(+Term, -Term)
:- private sys_cond_simp/2.
sys_cond_simp(V, _) :-
   var(V),
   throw(error(instantiation_error,_)).
sys_cond_simp((C,D), R) :- !,
   sys_cond_simp(C, A),
   sys_cond_simp(D, B),
   sys_cond_and(A, B, R).
sys_cond_simp((C;D), R) :- !,
   sys_cond_simp(C, A),
   sys_cond_simp(D, B),
   sys_cond_or(A, B, R).
sys_cond_simp(nonvar(X), nonvar(X)) :-
   var(X), !.
sys_cond_simp(nonvar(_), true) :- !.
sys_cond_simp(ground(X), R) :- !,
   term_variables(X, L),
   sys_cond_ground(L, R).
sys_cond_simp(T, _) :-
   throw(error(type_error(when_cond,T),_)).

% sys_cond_and(+Term, +Term, -Term)
:- private sys_cond_and/3.
sys_cond_and(true, X, X) :- !.
sys_cond_and(X, true, X) :- !.
sys_cond_and(X, Y, (X,Y)).

% sys_cond_or(+Term, +Term, -Term)
:- private sys_cond_or/3.
sys_cond_or(true, _, true) :- !.
sys_cond_or(_, true, true) :- !.
sys_cond_or(X, Y, (X;Y)).

% sys_cond_ground(+List, -Term)
:- private sys_cond_ground/2.
sys_cond_ground([X,Y|Z], (ground(X),R)) :-
   sys_cond_ground([Y|Z], R).
sys_cond_ground([X], ground(X)).
sys_cond_ground([], true).

