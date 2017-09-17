/**
 * This module provides a type 1 interface to attributed variables.
 * The trailed state of the at-tributed variable is modelled as
 * key value pairs and can be accessed and modified by the predicates
 * put_attr/3, get_attr/3 and del_attr/2. When an attributed variable
 * with type 1 state gets instantiated a call to the attributed
 * variable unify hooks gets scheduled.
 *
 * Examples:
 * ?- [user].
 * foo:attr_unify_hook(L, _) :- write('L='), write(L), nl.
 * ^D
 * Yes
 * ?- put_attr(X, foo, [X,Y]), put_attr(Y, foo, [X,Y]), [X,Y]=[1,2].
 * L=[1,2]
 * L=[1,2]
 * X = 1,
 * Y = 2
 * ?- put_attr(X, foo, [X,Y]), put_attr(Y, foo, [X,Y]), X=Y.
 * L=[_A,_A]
 * Y = X
 *
 * The unify hook attr_unify_hook/2 has to be declared inside the
 * module of the key. The scheduled hook is called after the variable
 * has been instantiated and at the next calls port if the surrounding
 * unification was successful. The hook is allowed to fail
 * or to succeed non-deterministically.
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

:- module(unify, []).
:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).
:- use_module(library(minimal/assume)).
:- use_module(library(experiment/ref)).

/**************************************************************/
/* Trailed State                                              */
/**************************************************************/

/**
 * put_attr(V, K, W):
 * The predicate assigns the value W to the key K of the variable V.
 * The assignment is automatically undone upon backtracking.
 */
% put_attr(+Var, +Key, +Value)
:- public put_attr/3.
put_attr(V, K, W) :-
   del_attr(V, K),
   sys_freeze_var(H, F),
   H = wrap(W),
   sys_compile_hook(V, attr(K, F), R),
   sys_assume_ref(R).

/**
 * get_attr(V, K, W):
 * The predicate succeeds for the value W of the key K of the variable V.
 */
% get_attr(+Var, +Key, -Value)
:- public get_attr/3.
get_attr(V, K, W) :-
   sys_clause_hook(V, attr(K, F), _),
   sys_melt_var(F, wrap(W)).

/**
 * del_attr(V, K):
 * The predicate de-assigns the key K from the variable V.
 * The de-assignment is automatically undone upon backtracking.
 */
% del_attr(+Var, +Key)
del_attr(V, K) :-
   sys_clause_hook(V, attr(K, _), R), !,
   sys_retire_ref(R).
del_attr(_, _).

/**************************************************************/
/* Attribute Hooks                                            */
/**************************************************************/

/**
 * K:attr_unify_hook(W, T) (hook):
 * This predicate has to be implemented as a hook for a key K.
 * It will be called with the value W and the term T after
 * the unification.
 */
% attr(+Key, +Ref, +Var, +Term)
:- private attr/4.
attr(K, F, _, T) :-
   sys_melt_var(F, wrap(W)),
   sys_assume_cont(K:attr_unify_hook(W,T)).
