/**
 * Term building allows the access and construction of arbitrary terms.
 * Since some of the predicates require arguments to be instantiated
 * these predicates are not logically complete. Nevertheless most of
 * the predicates are flexible enough so that they can be called in
 * both directions. Namely they can be called with the modes (+, -),
 * (-, +) and (+, +), whereby + indicates an instantiated argument.
 *
 * Examples:
 * ?- functor(1+2, F, A).
 * F = +,
 * A = 2
 *
 * ?- functor(X, +, 2).
 * X = _A+_B
 *
 * For performance reasons the interpreter performs unification without
 * occurs check. This can result in cyclic structures which are not
 * logically sound in the usual Herbrand model interpretation. The cyclic
 * structures might result in infinitely looping programs or in a looping
 * during term output. For programs that need a logically sound unification
 * a special predicate is provided which does only instantiate variables
 * when the check fails.
 *
 * Examples:
 * ?- X = f(X).
 * X = <cyclic term>
 *
 * ?- unify_with_occurs_check(X, f(X)).
 * No
 *
 * Most of the predicates in this module are ISO core standard predicates.
 * Among the non-ISO core standard predicate is the predicate set_arg/4
 * which performs a non-destructive argument replacement. Further, the
 * predicates sys_extend_term/3 and sys_shrink_term/4 can be used to add
 * respectively remove arguments. These predicates can be bootstrapped from
 * (=..)/2 but have special implementations for better speed.
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

:- use_package(foreign(jekpro/reference/structure)).

:- module(user, []).

:- public infix(=).
:- op(700, xfx, =).

:- public infix(=..).
:- op(700, xfx, =..).

:- public infix(\=).
:- op(700, xfx, \=).

/**
 * functor(X, F, A): [ISO 8.5.1]
 * If X is a non-variable f or f(X1,..,Xn) then the predicate succeeds
 * in F with f and in A with n. Otherwise the predicate succeeds in X
 * with f or f(X1,..,Xn) where X1,..,Xn are fresh variables and
 * f from F and n from N.
 */
% functor(+-Term, -+Atom, -+Integer)
% already defined in member

/**
 * arg(K, X, Y): [ISO 8.5.2]
 * If K is a positive integer in the range of an arity and X is a
 * callable f(A1, .., An) then the predicate succeeds when 1≤k≤n
 * and Ak unifies with Y.
 */
% arg(+Integer, +Term, -Term)
:- public arg/3.
:- special(arg/3, 'SpecialUniv', 0).

/**
 * set_arg(K, X, Y, Z):
 * If K is a positive integer in the range of an arity and X is
 * a callable f(A1, .., An) then the predicate succeeds when 1≤k≤n
 * and Z unifies with f(A1, .., Ak-1, Y, Ak+1, .., An).
 */
% set_arg(+Integer, +Term, +Term, -Term)
:- public set_arg/4.
:- special(set_arg/4, 'SpecialUniv', 1).

/**
 * X = .. Y: [ISO 8.5.3]
 * If X is a non-variable f or f(X1,..,Xn) then the predicate succeeds in Y with
 * [f,X1,..,Xn]. Otherwise the predicates succeeds in X with f or f(X1,..,Xn)
 * where [f,X1,..,Xn] from Y.
 */
% +-Term =.. -+List
:- public =.. /2.
T =.. [F|L] :- var(T), !,
   sys_must_be_atomic(F),
   sys_extend_term(F, L, T).
T =.. [F|L] :- !,
   sys_term_to_arity(T, N),
   sys_shrink_term(T, N, F, L).
_ =.. X :-
   throw(error(type_error(list, X), _)).

:- private sys_must_be_atomic/1.
:- special(sys_must_be_atomic/1, 'SpecialUniv', 2).

:- private sys_term_to_arity/2.
:- special(sys_term_to_arity/2, 'SpecialUniv', 3).

/**
 * sys_extend_term(F, L, T):
 * The predicate adds the arguments L to the
 * term F and unifies the result with T.
 */
% sys_extend_term(+Term, +List, -Term)
:- public sys_extend_term/3.
:- special(sys_extend_term/3, 'SpecialUniv', 4).

/**
 * sys_shrink_term(T, N, F, L):
 * The predicate removes N arguments from T
 * and unifies the results with F and L.
 */
% sys_shrink_term(+Term, +Integer, -Term, -List)
:- public sys_shrink_term/4.
:- special(sys_shrink_term/4, 'SpecialUniv', 5).

/**
 * X = Y: [ISO 8.2.1]
 * The predicate succeeds when X and Y unify, no occurs check is performed.
 */
% +Term = +Term
% already defined in member.p

/**
 * unify_with_occurs_check(X, Y): [ISO 8.2.2]
 * The predicate succeeds when X and Y unify, occurs check is performed.
 */
% unify_with_occurs_check(+Term, +Term)
:- public unify_with_occurs_check/2.
:- special(unify_with_occurs_check/2, 'SpecialUniv', 6).

/**
 * X \= Y: [ISO 8.2.3]
 * The predicate succeeds when X and Y do not unify, no occurs check is performed.
 */
% +Term \= +Term
:- public \= /2.
:- special(\= /2, 'SpecialUniv', 7).

/**
 * subsumes_term(X, Y): [ISO 8.2.4]
 * The predicate succeeds if X subsumes Y without keeping the bindings.
 */
:- public subsumes_term/2.
:- special(subsumes_term/2, 'SpecialUniv', 8).

/**
 * subsumes(X, Y):
 * The predicate succeeds if X subsumes Y.
 */
:- public subsumes/2.
:- special(subsumes/2, 'SpecialUniv', 9).

/**
 * copy_term(X, Y): [ISO 8.5.4]
 * The predicate creates a copy of X and succeeds when the copy unifies with Y.
 */
% copy_term(+Term, -Term)
:- public copy_term/2.
:- special(copy_term/2, 'SpecialUniv', 10).

