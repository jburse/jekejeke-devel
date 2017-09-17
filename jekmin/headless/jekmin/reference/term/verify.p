/**
 * This module provides a type 2 interface to attributed
 * variables. The trailed state of the at-tributed variable is
 * modelled as key value pairs and can be accessed and modified
 * by the predicates put_atts/3, get_atts/3 and del_atts/2. When
 * an attributed variable with type 2 states gets instantiated
 * the attributed variable verify hooks are called immediately.
 *
 * Examples:
 * ?- [user].
 * foo:verify_attributes(V, _, true) :-
 *     get_atts(V, foo, L), write('L='), write(L), nl.
 * ^D
 * Yes
 * ?- put_atts(X, foo, [X,Y]), put_atts(Y, foo, [X,Y]), [X,Y]=[1,2].
 * L=[_A,_B]
 * L=[1,_B]
 * X = 1,
 * Y = 2
 * ?- put_atts(X, foo, [X,Y]), put_atts(Y, foo, [X,Y]), X=Y.
 * L=[_A,_B]
 * Y = X
 *
 * The verify hook verify_attributes/3 has to be declared inside
 * the module of the key. The hook is called before the variable
 * has been instantiated and it should return a goal which gets
 * scheduled. The hook is allowed to fail or succeed, but it is
 * called only once. If the hook fails the surrounding unification
 * will also fail.
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

:- module(verify, []).
:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).
:- use_module(library(minimal/assume)).
:- use_module(library(experiment/ref)).

/**************************************************************/
/* Trailed State                                              */
/**************************************************************/

/**
 * put_atts(V, K, W):
 * The predicate assigns the value W to the key K of the variable V.
 * The assignment is automatically undone upon backtracking.
 */
% put_atts(+Var, +Key, +Value)
:- public put_atts/3.
put_atts(V, K, W) :-
   del_atts(V, K),
   sys_freeze_var(H, F),
   H = wrap(W),
   sys_compile_hook(V, atts(K, F), R),
   sys_assume_ref(R).

/**
 * get_atts(V, K, W):
 * The predicate succeeds for the value W of the key K of the variable V.
 */
% get_atts(+Var, +Key, -Value)
:- public get_atts/3.
get_atts(V, K, W) :-
   sys_clause_hook(V, atts(K, F), _),
   sys_melt_var(F, wrap(W)).

/**
 * del_atts(V, K):
 * The predicate de-assigns the key K from the variable V.
 * The de-assignment is automatically undone upon backtracking.
 */
% del_atts(+Var, +Key)
del_atts(V, K) :-
   sys_clause_hook(V, atts(K, _), R), !,
   sys_retire_ref(R).
del_atts(_, _).

/**************************************************************/
/* Attribute Hooks                                            */
/**************************************************************/

/**
 * K:verify_attributes(V, T, G) (hook):
 * This predicate has to be implemented as a hook for a key K.
 * It will be called with the variable V and the term T before
 * the unification. It should return a goal which will be called
 * after the unification.
 */
% atts(+Key, +Ref, +Var, +Term)
:- private atts/4.
atts(K, _, V, T) :-
   K:verify_attributes(V, T, G),
   sys_assume_cont(G).
