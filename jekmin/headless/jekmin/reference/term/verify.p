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
 * foo:verify_attributes(L, _) :-
 *     write('L='), write(L), nl.
 * ^D
 * Yes
 * ?- put_atts(X, foo, [X,Y]), put_atts(Y, foo, [X,Y]), [X,Y]=[1,2].
 * L=[_A,_B]
 * L=[1,_B]
 * ?- put_atts(X, foo, [X,Y]), put_atts(Y, foo, [X,Y]), X=Y.
 * L=[_A,_B]
 *
 * The verify hook verify_attributes/2 has to be declared inside the
 * module of the key. The hook is called before the variable has
 * been instantiated and receive the attribute value in its first
 * argument. The hook is allowed to fail or succeed, but it is called
 * only once. If the hook fails the surrounding unification will also fail.
 *
 * The goals hook portray_attributes/3 has to be optionally declared
 * inside the module of the key. When needed the hook is called
 * only once. If the hook is missing or if it fails a single goal
 * for a put_atts/3 call is generated. The goals are used in the
 * top-level display of answers and they can be retrieved by the
 * call_residue/2 predicate from the module residue.
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
:- use_package(library(jekpro/frequent/misc)).

:- module(verify, []).
:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).
:- use_module(library(minimal/assume)).
:- use_module(library(experiment/ref)).
:- use_module(library(experiment/cont)).

/**************************************************************/
/* Trailed State                                              */
/**************************************************************/

/**
 * put_atts(V, K, W):
 * The predicate assigns the value W to the key K of the variable V.
 * The assignment is automatically undone upon backtracking.
 */
% put_atts(+Var, +Term, +Term)
:- public put_atts/3.
put_atts(V, K, W) :-
   del_atts(V, K),
   sys_freeze_var(H, F),
   H = wrap(W),
   sys_ensure_serno(V),
   sys_compile_hook(V, atts(K, F), R),
   deposita_ref(R).

/**
 * get_atts(V, K, W):
 * The predicate succeeds for the value W of the key K of the variable V.
 */
% get_atts(+Var, -Term, -Term)
:- public get_atts/3.
get_atts(V, K, W) :-
   sys_clause_hook(V, atts(K, F), _),
   sys_melt_var(F, wrap(W)).

/**
 * del_atts(V, K):
 * The predicate de-assigns the key K from the variable V.
 * The de-assignment is automatically undone upon backtracking.
 */
% del_atts(+Var, +Term)
:- public del_atts/2.
del_atts(V, K) :-
   ground(K), !,
   del_atts2(V, K).
del_atts(_, _) :-
   throw(error(instantiation_error,_)).

% del_atts2(+Var, +Term)
:- private del_atts2/2.
del_atts2(V, K) :-
   sys_clause_hook(V, atts(K, _), R), !,
   withdrawa_ref(R).
del_atts2(_, _).

/**************************************************************/
/* Attribute Hooks                                            */
/**************************************************************/

/**
 * K:verify_attributes(V, T, G) (hook):
 * This predicate has to be implemented as a hook for a key K.
 * It will be called with the variable V and the term T before
 * the unification. It should return a goal in G which will be
 * called after the unification.
 */
% atts(+Key, +Ref, +Var, +Term)
:- private atts/4.
atts(K, F, _, T) :-
   sys_melt_var(F, wrap(W)),
   K:verify_attributes(W, T).

/**
 * sys_current_eq(V, H):
 * The predicate succeeds for each equation H with variables
 * wrapped that listens on the variable V. Constraint solvers
 * should extend this multi-file predicate.
 */
% sys_current_eq(+Var, -Handle)
:- public residue:sys_current_eq/2.
:- multifile residue:sys_current_eq/2.
residue:sys_current_eq(V, atts(R,K,F)) :-
   sys_clause_hook(V, atts(K, F), _),
   sys_freeze_var(V, R).

/**
 * K:portray_attributes(V, I, O) (hook):
 * This predicate has to be optionally implemented as a hook
 * for a key K. It will be called when the goals by the variable
 * V are needed. It should return a list of goals in I. The list
 * uses the end O.
 */
% sys_unwrap_eq(+Handle, -Goals, +Goals)
:- public residue:sys_unwrap_eq/3.
:- multifile residue:sys_unwrap_eq/3.
residue:sys_unwrap_eq(atts(R,K,_), I, O) :-
   current_predicate(K:portray_attributes/3),
   sys_melt_var(R, V),
   K:portray_attributes(V, I, O), !.
residue:sys_unwrap_eq(atts(R,K,F), [put_atts(V,K,W)|L], L) :-
   sys_melt_var(R, V),
   sys_melt_var(F, wrap(W)).
