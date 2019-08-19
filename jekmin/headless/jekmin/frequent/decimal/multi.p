/**
 * This module provides a generic multi precision evaluator. The
 * predicate main mp/3 allows evaluating an expression for a requested
 * precision. This module imports the modules poly and trigo, so
 * that arithmetic and trigonometric functions are already defined.
 *
 * Examples:
 * ?- X is mp(pi, 20).
 * X = 0d3.1415926535897932384
 *
 * ?- X is mp(pi, 40).
 * X = 0d3.141592653589793238462643383279502884198
 *
 * The predicate mp/3 falls back to is/2 when an expression is
 * undefined. The predicate can be extended by adding clauses to
 * the multi file predicates mp_math/3 and mp_abnormal/1. The
 * predicate mp_memo/4 can be used to cache constants.
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

:- package(library(jekmin/frequent/decimal)).

:- module(multi, []).
:- use_module(library(decimal/arith)).
:- use_module(library(decimal/scale)).
:- use_module(poly).
:- use_module(trigo).

/**
 * mp(E, P, R):
 * The predicate succeeds in R with the expression E evaluated to
 * the precision P.
 */
% mp(+Expression, +Integer, -Decimal)
:- meta_predicate mp(#(1), ?, ?).
:- public mp/3.
:- virtual mp/3.
mp(E, P, R) :-
   C is new_context(P),
   mp_math(E, C, R).

/**
 * mp_math(E, P, R):
 * The predicate succeeds in R with the expression E evaluated to
 * the context P. This predicate is multi file and can be thus
 * extended.
 */
% mp_math(+Expression, +Context, -Decimal)
:- meta_predicate mp_math(#(1), ?, ?).
:- multifile mp_math/3.
:- public mp_math/3.
mp_math(V, _, _) :- var(V),
   throw(error(instantiation_error, _)).
mp_math(N, _, R) :- number(N), !,
   R = N.
mp_math(E, P, R) :- \+ mp_abnormal(E), !,
   E =.. [F|L],
   mp_list(L, P, K),
   D =.. [F|K],
   R is D.

% mp_list(+List, +Context, -List)
:- private mp_list/3.
mp_list([X|L], P, [Y|K]) :-
   mp_math(X, P, Y),
   mp_list(L, P, K).
mp_list([], _, []).

/**
 * mp_abnormal(E):
 * The predicate succeeds when the expression E has a non-default
 * handling. This predicate is multi file and can be thus
 * extended.
 */
:- multifile mp_abnormal/1.
:- public mp_abnormal/1.
:- static mp_abnormal/1.

/**
 * mp_memo(A, E, P, R):
 * The predicate succeeds in R with the expression E evaluated to
 * the context P. The result is memorized under the name A.
 */
% mp_memo(+Atom, +Expression, +Context, -Decimal)
:- public mp_memo/4.
mp_memo(A, _, P, R) :-
   N is requested(P),
   mp_cache(A, N, H), !,
   R = H.
mp_memo(A, E, P, R) :-
   mp_math(E, P, H),
   N is requested(P),
   assertz(mp_cache(A, N, H)),
   R = H.

/**
 * mp_clear.
 * The predicate succeeds in clearing the memo cache.
 */
% mp_clear
:- public mp_clear/0.
mp_clear :-
   retractall(mp_cache(_, _, _)).

% mp_cache(+Atom, +Integer, -Decimal)
:- private mp_cache/3.
:- thread_local mp_cache/3.

