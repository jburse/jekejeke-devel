/**
 * For qualified names a notation based on the colon (:) operator can be
 * used when invoking predicates or evaluable functions. The module name
 * itself can be structured by means of the slash (/)/2 operator and
 * the set ({})/1 operator. This gives rise to a new primitive goal syntax,
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
 * To validate the above goal syntax the predicates var/1 and callable/1
 * provided by this module can be used. Further, the module provides
 * constructor and destructors of the new goal syntax by the predicates
 * (=..)/2 and functor/3. The module overrides the usual ISO core standard
 * predicates semantics.
 *
 * Examples:
 * ?- foo(1)::bar(2) =.. L.
 * L = [foo:bar, foo(1), 2]
 *
 * ?- C =.. [foo:bar, foo(1), 2].
 * C = foo(1)::bar(2)
 *
 * The Jekejeke Prolog object orientation is based on the Pythonesk
 * convention that the receiver object is found in the first argument
 * of a routine. The destructors uses the Pythonesk convention on a message
 * sending call. The constructor then applies a heuristics to reconstruct
 * the Pythonesk convention from a qualified call.
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
 * var(T):
 * The predicate succeeds when the term T adheres to the
 * colon notation with at least one non-argument variable.
 */
% var(+Goal)
:- public var/1.
:- override var/1.
var(G) :-
   sys_type_goal(G, N),
   N \= 0.

/**
 * callable(T):
 * The predicate succeeds when the term T adheres to the
 * colon notation without a non-argument variable.
 */
% callable(+Term)
:- public callable/1.
:- override callable/1.
callable(G) :-
   sys_type_goal(G, N),
   N = 0.

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
/* Improved Functor                                               */
/******************************************************************/

/**
 * functor(O, F, A):
 * The predicate succeeds in F with the functor and in A with the arity
 * of the colon notation O.
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

/******************************************************************/
/* Improved Univ                                                  */
/******************************************************************/

/**
 * O =.. L:
 * The predicate succeeds in L with the functor and arguments
 * of the colon notation O.
 */
% +Term =.. -List
:- public =.. /2.
:- override =.. /2.
O =.. L :- user:var(O), !,
   univ2(L, O).
K =.. L :- K = R:O, !,
   =..(O, [H|J]),
   sys_replace_site(F, K, R:H),
   L = [F|J].
K =.. L :- K = R::O, !,
   sys_get_class(R, I),
   =..(O, [H|J]),
   sys_replace_site(F, K, I:H),
   L = [F, R|J].
O =.. L :-
   user:(O =.. L).

% univ2(+List, -Term)
:- private univ2/2.
univ2([F|_], _) :- user:var(F),
   throw(error(instantiation_error, _)).
univ2([K, R|L], O) :- K = I:F, univ3(R),
   sys_get_class(R, J),
   I == J, !,
   univ2([F|L], H),
   sys_replace_site(O, K, R::H).
univ2([K|L], O) :- K = R:F, !,
   univ2([F|L], H),
   sys_replace_site(O, K, R:H).
univ2(L, O) :-
   user:(O =.. L).

% univ3(+Term)
:- private univ3/1.
univ3(O) :- user:var(O), !, fail.
univ3(_/O) :- !, univ3(O).
univ3(_).

/******************************************************************/
/* Improved Extend/Shrink                                         */
/******************************************************************/

/**
 * sys_extend_term(F, L, T):
 * The predicate adds the arguments L to the
 * term P and unifies the result with Q.
 */
% sys_extend_term(+Term, +List, -Term)
:- public sys_extend_term/3.
:- override sys_extend_term/3.
sys_extend_term(F, _, _) :- user:var(F),
   throw(error(instantiation_error, _)).
sys_extend_term(F, L, T) :- F = R:O, !,
   sys_extend_term(O, L, H),
   sys_replace_site(T, F, R:H).
sys_extend_term(F, L, T) :- F = R::O, !,
   sys_extend_term(O, L, H),
   sys_replace_site(T, F, R::H).
sys_extend_term(F, L, T) :-
   user:sys_extend_term(F, L, T).

/**
 * sys_shrink_term(T, N, F, L):
 * The predicate removes N arguments from T
 * and unifies the results with F and L.
 */
% sys_shrink_term(+Term, +Integer, -Term, -List)
:- public sys_shrink_term/4.
:- override sys_shrink_term/4.
sys_shrink_term(F, _, _, _) :- user:var(F),
   throw(error(instantiation_error, _)).
sys_shrink_term(T, N, F, L) :- T = R:O, !,
   sys_shrink_term(O, N, H, L),
   sys_replace_site(F, T, R:H).
sys_shrink_term(T, N, F, L) :- T = R::O, !,
   sys_shrink_term(O, N, H, L),
   sys_replace_site(F, T, R::H).
sys_shrink_term(T, N, F, L) :-
   user:sys_shrink_term(T, N, F, L).
