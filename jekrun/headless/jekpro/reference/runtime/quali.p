/**
 * For qualified names a notation based on the colon (:) operator can be
 * used when invoking predicates or evaluable functions. The module name
 * itself can be structured by means of the slash (/)/2 operator and
 * the set ({})/1 operator. This gives rise to a new primitive goal syntax
 * which reads as follows:
 *
 * goal         --> module ":" goal
 *                | receiver "::" goal
 *                | callable.
 *
 * receiver     --> package "/" callable
 *                | reference
 *                | callable.
 *
 * Under the hood qualified names are flattened to atoms with the help
 * of an inline atom cache. Further the colon notation will also resolve
 * module names based on the class loader of the call-site, the prefix
 * list of the call-site and the prefix list of the system. A qualified
 * predicate will be also searched in the re-export chain of the
 * given module name.
 *
 * Examples:
 * ?- basic/lists:member(X, [1]).
 * X = 1
 * ?- 'jekpro.frequent.basic.lists\bmember'(X, [1]).
 * X = 1
 *
 * Finally there is also a double colon notation based on the (::)/2
 * operator that can be used to invoke reference types and term objects.
 * The reference type or term object itself is prepended Python style to
 * the callable before invoking it. For auto loaded Java classes the
 * re-export chain contains the super class and implemented interfaces.
 *
 * Examples:
 * ?- 'System':err(X), X::println('abc').
 * X = 0r47733fca
 * ?- current_error(X), X::write('abc'), X::nl.
 * abc
 * X = 0r398aef8b
 *
 * If an unqualified predicate with the same name is defined, then this is
 * the fall-back and hence the "write" and "nl" work. The predicates sys_callable/1,
 * sys_var/1, sys_functor/3 and sys_univ/2 are the adaptation to callable/1,
 * var/1, functor/3 and (=..)/2, in that these predicates respect the colon
 * (:)/2 and double colon (::)/2 notation.
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

:- use_package(foreign(jekpro/reference/runtime)).

:- module(user, []).

:- public infix(:).
:- op(600, xfy, :).
:- set_oper_property(infix(:), nspl).
:- set_oper_property(infix(:), nspr).

:- public infix(::).
:- op(600, xfy, ::).
:- set_oper_property(infix(::), nspl).
:- set_oper_property(infix(::), nspr).

/*******************************************************/
/* Qualified Calls & Evaluations                       */
/*******************************************************/

/**
 * M:C:
 * The predicate calls the callable C by qualifying the predicate
 * name of C by the module name M. The call is performed in the
 * same call-site as the colon notation.
 */
% +Slash : +Compound:
:- public : /2.
:- virtual : /2.
:- set_predicate_property(: /2, (meta_predicate? :0)).
:- sys_get_context(here, C),
   set_predicate_property(: /2, sys_meta_predicate(C)).
:- special(: /2, 'SpecialQuali', 0).
:- set_predicate_property(: /2, sys_notrace).

/**
 * R::C:
 * The predicate calls the callable C by qualifying the predicate
 * name of C by the module name of R and prepending R itself. The
 * call is performed in the same call-site as the colon notation.
 */
% +Slash :: +Compound:
:- public :: /2.
:- virtual :: /2.
:- set_predicate_property(:: /2, (meta_predicate? :: ::(0))).
:- sys_get_context(here, C),
   set_predicate_property(:: /2, sys_meta_predicate(C)).
:- special(:: /2, 'SpecialQuali', 1).
:- set_predicate_property(:: /2, sys_notrace).

/**
 * M:E:
 * The function evaluates the expression E by qualifying the function
 * name of E by the module name M. The evaluation is performed in the
 * same call-site as the colon notation.
 */
% +Slash : +Compound:
:- public : /3.
:- virtual : /3.
:- set_predicate_property(: /3, (meta_predicate:(?,1,?))).
:- sys_get_context(here, C),
   set_predicate_property(: /3, sys_meta_predicate(C)).
:- special(: /3, 'EvaluableQuali', 0).
:- set_predicate_property(: /3, sys_notrace).

/**
 * R::E:
 * The function evaluates the expression E by qualifying the function
 * name of E by the module name of R and prepending R itself. The
 * evaluation is performed in the same call-site as the colon notation.
 */
% +Slash :: +Compound:
:- public :: /3.
:- virtual :: /3.
:- set_predicate_property(:: /3, (meta_predicate::(?,::(1),?))).
:- sys_get_context(here, C),
   set_predicate_property(:: /3, sys_meta_predicate(C)).
:- special(:: /3, 'EvaluableQuali', 1).
:- set_predicate_property(:: /3, sys_notrace).

/******************************************************************/
/* Improved Callable & Var                                        */
/******************************************************************/

/**
 * sys_callable(T):
 * Check whether T is a qualified goal with zero place holders.
 */
% sys_callable(+Term)
:- public sys_callable/1.
sys_callable(G) :-
   sys_type_goal(G, N),
   N = 0.

/**
 * sys_var(T):
 * Check whether T is a qualified goal with non-zero place holders.
 */
% sys_var(+Goal)
:- public sys_var/1.
sys_var(G) :-
   sys_type_goal(G, N),
   N \= 0.

/**
 * sys_type_goal(T, N):
 * Check whether T is a colon notation with N place holders.
 */
% sys_type_goal(+Term, -Integer)
:- private sys_type_goal/2.
sys_type_goal(S, 1) :-
   var(S), !.
sys_type_goal(S:T, O) :- !,
   sys_type_module(S, M),
   sys_type_goal(T, N),
   O is M+N.
sys_type_goal(S::T, O) :- !,
   sys_type_receiver(S, M),
   sys_type_goal(T, N),
   O is M+N.
sys_type_goal(S, 0) :-
   callable(S).

/**
 * sys_type_module(T, N):
 * Check whether T is a module with N place holders.
 * See pred.p for syntax.
 */
% sys_type_module(+Term, -Integer)
:- private sys_type_module/2.
sys_type_module(S, 1) :-
   var(S), !.
sys_type_module(S, 0) :-
   reference(S), !.
sys_type_module(S/T, O) :- !,
   sys_type_package(S, M),
   sys_type_atom(T, N),
   O is M+N.
sys_type_module({S}, O) :- !,
   sys_type_array(S, O).
sys_type_module(S, 0) :-
   atom(S).

/**
 * sys_type_package(T, N):
 * Check whether T is a package with N place holders.
 * See pred.p for syntax.
 */
% sys_type_package(+Term, -Integer)
:- private sys_type_package/2.
sys_type_package(S, 1) :-
   var(S), !.
sys_type_package(S/T, O) :- !,
   sys_type_package(S, M),
   sys_type_atom(T, N),
   O is M+N.
sys_type_package(S, 0) :-
   atom(S).

/**
 * sys_type_atom(T, N):
 * Check whether T is an atom with N place holders.
 */
% sys_type_atom(+Term, -Integer)
:- private sys_type_atom/2.
sys_type_atom(S, 1) :-
   var(S), !.
sys_type_atom(S, 0) :-
   atom(S).

/**
 * sys_type_array(T, N):
 * Check whether T is a package with N place holders.
 * See pred.p for syntax.
 */
% sys_type_array(+Term, -Integer)
:- private sys_type_array/2.
sys_type_array(S, 1) :-
   var(S), !.
sys_type_array(S/T, O) :- !,
   sys_type_package(S, M),
   sys_type_atom(T, N),
   O is M+N.
sys_type_array({S}, O) :- !,
   sys_type_array(S, O).
sys_type_array(S, 0) :-
   atom(S).

/**
 * sys_type_receiver(T, N):
 * Check whether T is a receiver with N place holders.
 * See quali.p for syntax.
 */
% sys_type_receiver(+Term, -Integer)
:- private sys_type_receiver/2.
sys_type_receiver(S, 1) :-
   var(S), !.
sys_type_receiver(S, 0) :-
   reference(S), !.
sys_type_receiver(S/T, O) :- !,
   sys_type_package(S, M),
   sys_type_callable(T, N),
   O is M+N.
sys_type_receiver(S, 0) :-
   callable(S).

/**
 * sys_type_callable(T, N):
 * Check whether T is a callable with N place holders.
 */
% sys_type_callable(+Term, -Integer)
:- private sys_type_callable/2.
sys_type_callable(S, 1) :-
   var(S), !.
sys_type_callable(S, 0) :-
   callable(S).

/******************************************************************/
/* Improved Functor & Univ                                        */
/******************************************************************/

/**
 * sys_functor(T, F, A):
 * The predicate unifies F with the possibly quantified functor
 * of T and unifies A with the arity of T.
 */
% sys_functor(+-Term, -+Term, -+Integer)
:- public sys_functor/3.
sys_functor(T, F, A) :-
   var(T), !,
   sys_functor2(F, A, T).
sys_functor(K, J, A) :-
   K = M:T, !,
   sys_functor(T, F, A),
   sys_replace_site(J, K, M:F).
sys_functor(K, J, B) :-
   K = R::T, !,
   sys_get_module(R, M),
   sys_functor(T, F, A),
   sys_replace_site(J, K, M:F),
   B is A+1.
sys_functor(T, F, A) :-
   functor(T, F, A).

% sys_functor2(+Term, +Integer, -Term)
:- private sys_functor2/3.
sys_functor2(F, _, _) :-
   var(F),
   throw(error(instantiation_error,_)).
sys_functor2(J, A, K) :-
   J = M:F, !,
   sys_functor2(F, A, T),
   sys_replace_site(K, J, M:T).
sys_functor2(F, A, T) :-
   functor(T, F, A).

/**
 * sys_univ(T, [F|L]):
 * The predicate unifies F with the possibly qualified functor of T
 * and unifies L with the arguments of T.
 */
% sys_univ(+-Term, -+List)
:- public sys_univ/2.
sys_univ(T, U) :-
   var(T), !,
   sys_univ2(U, T).
sys_univ(K, [J|L]) :-
   K = M:T, !,
   sys_univ(T, [F|L]),
   sys_replace_site(J, K, M:F).
sys_univ(K, [J,R|L]) :-
   K = R::T, !,
   sys_get_module(R, M),
   sys_univ(T, [F|L]),
   sys_replace_site(J, K, M:F).
sys_univ(T, U) :-
   T =.. U.

% sys_univ2(+List, -Term)
:- private sys_univ2/2.
sys_univ2([F|_], _) :-
   var(F),
   throw(error(instantiation_error,_)).
sys_univ2([J,R|L], K) :-
   J = M:F,
   sys_get_module_test(R, N),
   N == M, !,
   sys_univ2([F|L], T),
   sys_replace_site(K, J, R::T).
sys_univ2([J|L], K) :-
   J = M:F, !,
   sys_univ2([F|L], T),
   sys_replace_site(K, J, M:T).
sys_univ2(U, T) :-
   T =.. U.
