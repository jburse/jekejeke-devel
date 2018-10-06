/**
 * Term building allows the access and construction of arbitrary terms.
 * Since some of the predicates require arguments to be instantiated
 * these predicates are not logically complete. Nevertheless most of
 * the predicates are flexible enough so that they can be called in
 * both directions. Namely they can be called with the modes (+, -),
 * (-, +) and (+, +), whereby + indicates an instantiated argument.
 *
 * For performance reasons the interpreter performs unification without
 * occurs check. This can result in cyclic structures which are not
 * logically sound in the usual Herbrand model interpretation. The cyclic
 * structures might result in infinitely looping programs or in a looping
 * during term output. For programs that need a logically sound unification
 * a special predicate is provided which does only instantiate variables
 * when the check fails.
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
 * X = .. Y: [ISO 8.5.3]
 * If X is atomic then the predicate succeeds when Y unifies with
 * [X]. If X is the compound F(A1, .., An) then the predicate succeeds
 * when Y unifies with [F, A1, …, An]. If Y is [C] and C is atomic
 * then the predicate succeeds when X unifies with C. If Y is [F, A1, …, An]
 * and F is an atom then the predicate succeeds when X unifies with F(A1, .., An).
 */
% +-Term =.. -+List
:- public =.. /2.
T =.. L :-
   var(T), !,
   sys_list_to_term(L, T).
T =.. L :-
   sys_term_to_list(T, L).

:- private sys_list_to_term/2.
:- special(sys_list_to_term/2, 'SpecialUniv', 6).

:- private sys_term_to_list/2.
:- special(sys_term_to_list/2, 'SpecialUniv', 7).

/**
 * functor(X, N, A): [ISO 8.5.1]
 * If X is atomic then the predicate succeeds when N unifies with X
 * and A unifies with 0. If X is the compound f(A1, .., An) then the
 * predicate succeeds when N unifies with f and A unifies with n. If N
 * is atomic and A is 0 then the predicate succeeds when Y unifies
 * with N. If N is an atom, A is an integer n≥1 and A1, …, An are
 * fresh arguments then the predicate succeeds when Y unifies
 * with N(A1, .., An).
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
:- special(arg/3, 'SpecialUniv', 1).

/**
 * set_arg(K, X, Y, Z):
 * If K is a positive integer in the range of an arity and X is
 * a callable f(A1, .., An) then the predicate succeeds when 1≤k≤n
 * and Z unifies with f(A1, .., Ak-1, Y, Ak+1, .., An).
 */
% set_arg(+Integer, +Term, +Term, -Term)
:- public set_arg/4.
:- special(set_arg/4, 'SpecialUniv', 2).

/**
 * X = Y: [ISO 8.2.1]
 * The predicate succeeds when X and Y unify, no occurs check is performed.
 */
% +Term = +Term
:- public = /2.
:- special(= /2, 'SpecialUniv', 3).

/**
 * unify_with_occurs_check(X, Y): [ISO 8.2.2]
 * The predicate succeeds when X and Y unify, occurs check is performed.
 */
% unify_with_occurs_check(+Term, +Term)
:- public unify_with_occurs_check/2.
:- special(unify_with_occurs_check/2, 'SpecialUniv', 4).

/**
 * X \= Y: [ISO 8.2.3]
 * The predicate succeeds when X and Y do not unify, no occurs check is performed.
 */
% +Term \= +Term
:- public \= /2.
:- special(\= /2, 'SpecialUniv', 5).

