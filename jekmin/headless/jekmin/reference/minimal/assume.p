/**
 * Clauses and attribute variable hooks are identified by their reference
 * data type. The predicates deposita_ref/1 respectively depositz_ref/1
 * will assume the given clause or hook for the duration of the
 * continuation, whereas the predicate withdrawa_ref/1 respectively
 * withdrawz_ref/1 will retire the given clause or hook for the duration
 * of the continuation.
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
 * deposita_ref(R):
 * The predicate temporarily inserts the clause or hook referenced
 * by R at the top for the duration of the continuation
 */
% deposita_ref(+Ref)
:- public deposita_ref/1.
deposita_ref(V) :-
   var(V),
   throw(error(instantiation_error,_)).
deposita_ref([R|L]) :- !,
   deposita_ref(R),
   deposita_ref(L).
deposita_ref([]) :- !.
deposita_ref(R) :-
   sys_atomic((  recorda_ref(R),
                 sys_unbind(erase_ref(R)))).

/**
 * depositz_ref(R):
 * The predicate temporarily inserts the clause or hook referenced
 * by R at the bottom for the duration of the continuation
 */
% depositz_ref(+Ref)
:- public depositz_ref/1.
depositz_ref(V) :-
   var(V),
   throw(error(instantiation_error,_)).
depositz_ref([R|L]) :- !,
   depositz_ref(R),
   depositz_ref(L).
depositz_ref([]) :- !.
depositz_ref(R) :-
   sys_atomic((  recordz_ref(R),
                 sys_unbind(erase_ref(R)))).

/**
 * withdrawa_ref(R):
 * The predicate temporarily removes the clause or hook referenced
 * by R for the duration of the continuation. The undo will
 * happen at the top.
 */
% withdrawa_ref(+Ref)
:- public withdrawa_ref/1.
withdrawa_ref(V) :-
   var(V),
   throw(error(instantiation_error,_)).
withdrawa_ref([R|L]) :- !,
   withdrawz_ref(L),
   withdrawz_ref(R).
withdrawa_ref([]) :- !.
withdrawa_ref(R) :-
   sys_atomic((  erase_ref(R),
                 sys_unbind(recorda_ref(R)))).

/**
 * withdrawz_ref(R):
 * The predicate temporarily removes the clause or hook referenced
 * by R for the duration of the continuation. The undo will
 * happen at the top.
 */
% withdrawz_ref(+Ref)
:- public withdrawz_ref/1.
withdrawz_ref(V) :-
   var(V),
   throw(error(instantiation_error,_)).
withdrawz_ref([R|L]) :- !,
   withdrawz_ref(L),
   withdrawz_ref(R).
withdrawz_ref([]) :- !.
withdrawz_ref(R) :-
   sys_atomic((  erase_ref(R),
                 sys_unbind(recordz_ref(R)))).

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
   depositz_ref(K),
   call(G),
   withdrawz_ref(K).

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
   depositz_ref(K).

/***************************************************************/
/* Continuation Queue                                          */
/***************************************************************/

/**
 * sys_assume_cont(G):
 * The predicate temporarily pushes the goal G on the continuation queue.
 */
% sys_assume_cont(+Term)
:- public sys_assume_cont/1.
:- meta_predicate sys_assume_cont(0).
sys_assume_cont(G) :-
   sys_atomic((  cont_push(G),
                 sys_unbind(cont_pop))).
