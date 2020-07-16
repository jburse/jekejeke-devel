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
 * operator that can be used to send message to a receiver. The receiver
 * itself is prepended Python style to the callable before invoking it.
 * For auto loaded Java classes the reexport chain contains the super
 * class and implemented interfaces. If an unqualified predicate with
 * the same name is defined, then this fall-back is called.
 *
 * Examples:
 * ?- 'System':err(X), X::println('abc').
 * X = 0r47733fca
 * ?- current_error(X), X::write('abc'), X::nl.
 * abc
 * X = 0r398aef8b
 *
 * The predicates sys_callable/1, sys_var/1, sys_functor/3 and sys_univ/2
 * are the adaptations of callable/1, var/1, functor/3 and (=..)/2 in that
 * these predicates respect the module colon (:)/2 and receiver double
 * colon (::)/2 notation. A qualified functor  may only contains the colon
 * (:)/2 notation. The predicate sys_module/2 can be used to retrieve
 * the class reference or module name of a receiver.
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

:- package(library(jekpro/reference/runtime)).

:- module(quali, []).
:- use_module(library(basic/proxy)).

/******************************************************************/
/* Improved Callable & Var                                        */
/******************************************************************/

/**
 * callable(T):
 * Check whether T is a fully qualified callable.
 */
% callable(+Term)
:- public callable/1.
:- override callable/1.
callable(G) :-
   sys_type_goal(G, N),
   N = 0.

/**
 * var(T):
 * Check whether T is a half qualified or not a qualified callable.
 */
% var(+Goal)
:- public var/1.
:- override var/1.
var(G) :-
   sys_type_goal(G, N),
   N \= 0.

/**
 * sys_type_goal(T, N):
 * Check whether T is a colon notation with N place holders.
 */
% sys_type_goal(+Term, -Integer)
:- private sys_type_goal/2.
sys_type_goal(S, 1) :-
   user:var(S), !.
sys_type_goal(S:T, O) :- !,
   sys_type_module(S, M),
   sys_type_goal(T, N),
   O is M+N.
sys_type_goal(S::T, O) :- !,
   sys_type_receiver(S, M),
   sys_type_goal(T, N),
   O is M+N.
sys_type_goal(S, 0) :-
   user:callable(S).

/**
 * sys_type_module(T, N):
 * Check whether T is a module with N place holders.
 * See pred.p for syntax.
 */
% sys_type_module(+Term, -Integer)
:- private sys_type_module/2.
sys_type_module(S, 1) :-
   user:var(S), !.
sys_type_module(S/T, O) :- !,
   sys_type_package(S, M),
   sys_type_atom(T, N),
   O is M+N.
sys_type_module({S}, O) :- !,
   sys_type_class(S, O).
sys_type_module(S, 0) :-
   reference(S), !.
sys_type_module(S, 0) :-
   atom(S).

/**
 * sys_type_class(T, N):
 * Check whether T is a package with N place holders.
 * See pred.p for syntax.
 */
% sys_type_class(+Term, -Integer)
:- private sys_type_class/2.
sys_type_class(S, 1) :-
   user:var(S), !.
sys_type_class(S/T, O) :- !,
   sys_type_package(S, M),
   sys_type_atom(T, N),
   O is M+N.
sys_type_class({S}, O) :- !,
   sys_type_class(S, O).
sys_type_class(S, 0) :-
   atom(S).

/**
 * sys_type_receiver(T, N):
 * Check whether T is a receiver with N place holders.
 * See quali.p for syntax.
 */
% sys_type_receiver(+Term, -Integer)
:- private sys_type_receiver/2.
sys_type_receiver(S, 1) :-
   user:var(S), !.
sys_type_receiver(S/T, O) :- !,
   sys_type_package(S, M),
   sys_type_callable(T, N),
   O is M+N.
sys_type_receiver(S, 0) :-
   reference(S), !.
sys_type_receiver(S, 0) :-
   user:callable(S).

/**
 * sys_type_package(T, N):
 * Check whether T is a package with N place holders.
 * See pred.p for syntax.
 */
% sys_type_package(+Term, -Integer)
:- private sys_type_package/2.
sys_type_package(S, 1) :-
   user:var(S), !.
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
   user:var(S), !.
sys_type_atom(S, 0) :-
   atom(S).

/**
 * sys_type_callable(T, N):
 * Check whether T is a callable with N place holders.
 */
% sys_type_callable(+Term, -Integer)
:- private sys_type_callable/2.
sys_type_callable(S, 1) :-
   user:var(S), !.
sys_type_callable(S, 0) :-
   user:callable(S).

/******************************************************************/
/* Improved Univ                                                  */
/******************************************************************/

/**
 * =..(O, L):
 * The predicate succeeds in L with the functor and arguments
 * of the receiver object or qualified callable O.
 */
% =..(+Term, -List)
:- public =.. /2.
:- override =.. /2.
=..(O, L) :- user:var(O), !,
   univ2(L, O).
=..(K, L) :- K = R:O, !,
   =..(O, [H|J]),
   sys_replace_site(F, K, R:H),
   L = [F|J].
=..(K, L) :- K = R::O, !,
   sys_get_class(R, I),
   =..(O, [H|J]),
   sys_replace_site(F, K, I:H),
   L = [F, R|J].
=..(O, L) :-
   user: =..(O, L).

% univ2(+List, -Term)
:- private univ2/2.
univ2([F|_], _) :- user:var(F),
   throw(error(instantiation_error, _)).
univ2([K, R|L], O) :- K = I:F,
   sys_is_class(R),
   sys_get_class(R, J),
   I == J, !,
   univ2([F|L], H),
   sys_replace_site(O, K, R::H).
univ2([K|L], O) :- K = R:F, !,
   univ2([F|L], H),
   sys_replace_site(O, K, R:H).
univ2(L, O) :-
   user: =..(O, L).

% sys_is_class(+Term)
:- private sys_is_class/1.
sys_is_class(O) :- user:var(O), !, fail.
sys_is_class(_/O) :- !,
   sys_is_class(O),
   sys_is_class(_).

/******************************************************************/
/* Improved Functor & Arg                                         */
/******************************************************************/

/**
 * functor(O, F, A):
 * The predicate succeeds in F with the functor and in A with the arity
 * of the receiver object or qualified callable O.
 */
% functor(+Term, -Package, -Integer)
:- public functor/3.
:- override functor/3.
functor(O, F, A) :- user:var(O), !,
   functor2(F, A, O).
functor(K, F, A) :- K = R:O, !,
   functor(O, H, A),
   sys_replace_site(F, K, R:H).
functor(K, F, A) :- K = R::O, !,
   sys_get_class(R, J),
   functor(O, H, B),
   sys_replace_site(F, K, J:H),
   A is B+1.
functor(O, F, A) :-
   user:functor(O, F, A).

% functor2(+Package, +Integer, -Term)
:- private functor2/3.
functor2(F, _, _) :- user:var(F),
   throw(error(instantiation_error, _)).
functor2(K, A, O) :- K = R:F, !,
   functor2(F, A, H),
   sys_replace_site(O, K, R:H).
functor2(F, A, O) :-
   user:functor(O, F, A).
