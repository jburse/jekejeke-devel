/**
 * Clauses and attribute variable hooks are identified by their reference
 * data type. The predicate sys_assume_ref/1 will assume the given clause
 * or hook for the duration of the continuation, whereas the predicate
 * sys_retire_ref/1 will retire the given clause or hook for the
 * duration of the continuation.
 *
 * Example:
 * hook(V, T) :-
 *    write('bind '), write(V),
 *    write(' to '), write(T), nl.
 *
 * ?- sys_ensure_hook(X, hook), X = 99.
 * bind _A to 99
 * X = 99.
 *
 * The predicate sys_ensure_hook/2 takes care of the compilation of a hook
 * into a hook reference in case the hook is not yet associated with an
 * attribute variable for the duration of the continuation. The variant
 * sys_ensure_hook/3 does the same job during the execution of the given goal.
 *
 * The predicate sys_assume_cont/1 temporarily pushes the given goal on the
 * continuation queue. For more information on the continuation queue see
 * the module cont.
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

:- package(library(jekmin/reference/minimal)).

:- module(assume, []).
:- use_module(library(experiment/ref)).
:- use_module(library(experiment/attr)).
:- use_module(library(experiment/trail)).
:- use_module(library(experiment/cont)).

/***************************************************************/
/* Clause & Hook References                                    */
/***************************************************************/

/**
 * sys_assume_ref(R):
 * The predicate temporarily inserts the clause or hook referenced
 * by R at the bottom for the duration of the continuation
 */
% sys_assume_ref(+Ref)
:- public sys_assume_ref/1.
sys_assume_ref(V) :-
   var(V),
   throw(error(instantiation_error,_)).
sys_assume_ref([R|L]) :- !,
   sys_assume_ref(L),
   sys_assume_ref(R).
sys_assume_ref([]) :- !.
sys_assume_ref(R) :-
   sys_atomic((  recordz_ref(R),
                 sys_unbind(erase_ref(R)))), !.
sys_assume_ref(R) :-
   compiled_ref(R, Q),
   throw(error(permission_error(add,assume,Q),_)).

/**
 * sys_retire_ref(R):
 * The predicate temporarily removes the clause or hook referenced
 * by R for the duration of the continuation.
 */
% sys_retire_ref(+Ref)
:- public sys_retire_ref/1.
sys_retire_ref(V) :-
   var(V),
   throw(error(instantiation_error,_)).
sys_retire_ref([R|L]) :- !,
   sys_retire_ref(R),
   sys_retire_ref(L).
sys_retire_ref([]) :- !.
sys_retire_ref(R) :-
   sys_atomic((  erase_ref(R),
                 sys_unbind(recordz_ref(R)))), !.
sys_retire_ref(R) :-
   compiled_ref(R, Q),
   throw(error(permission_error(remove,assume,Q),_)).

/***************************************************************/
/* Attribute Variables                                         */
/***************************************************************/

/**
 * sys_ensure_hook(A, H, G):
 * The predicate temporarily ensures that the hook H is in the
 * hook list of the attribute variable A for the duration
 * of the goal G and succeeds whenever G succeeds.
 */
% sys_ensure_hook(+Attr, +Closure, +Goal)
:- public sys_ensure_hook/3.
:- meta_predicate sys_ensure_hook(?,2,0).
sys_ensure_hook(V, H, G) :-
   sys_clause_hook(V, H, _), !,
   call(G).
sys_ensure_hook(V, H, G) :-
   sys_compile_hook(V, H, K),
   sys_assume_ref(K),
   call(G),
   sys_retire_ref(K).

/**
 * sys_ensure_hook(A, H):
 * The predicate temporarily ensures that the hook H is in the
 * hook list of the attribute variable A for the duration
 * of the continuation.
 */
% sys_ensure_hook(+Attr, +Closure)
:- public sys_ensure_hook/2.
:- meta_predicate sys_ensure_hook(?,2).
sys_ensure_hook(V, H) :-
   sys_clause_hook(V, H, _), !.
sys_ensure_hook(V, H) :-
   sys_compile_hook(V, H, K),
   sys_assume_ref(K).

/***************************************************************/
/* Continuation Queue                                          */
/***************************************************************/

/**
 * sys_assume_cont(G):
 * The predicate temporarily pushes the goal G on the continuation queue.
 */
% sys_assume_cont(+Term)
:- public sys_assume_cont/1.
sys_assume_cont(G) :-
   sys_atomic((  cont_push(G),
                 sys_unbind(cont_pop))).
