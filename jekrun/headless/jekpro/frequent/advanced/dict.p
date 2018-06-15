/**
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

:- package(library(jekpro/frequent/advanced)).

:- module(dict, []).

/**
 * dict_get(D, K, V):
 * The predicate succeeds in V for the first value with key K in dict D.
 */
% dict_get(+Dict, +Key, -Value)
:- public dict_get/3.
dict_get(M, K, V) :-
   M =.. [_|L],
   dict_get2(L, K, W),
   V = W.

% dict_get2(+List, +Key, -Value)
:- private dict_get2/3.
dict_get2([K,V|_], K, V) :- !.
dict_get2([_,_|L], K, V) :-
   dict_get2(L, K, V).

/**
 * dict_put(D, K, V, E):
 * The predicate succeeds in E with a new dict, where the first value of
 * key K in dict D is replaced by the value V.
 */
% dict_put(+Dict, +Key, +Value, -Dict)
:- public dict_put/4.
dict_put(M, K, V, N) :-
   M =.. [F|L],
   dict_put2(L, K, V, R),
   N =.. [F|R].

% dict_put2(+List, +Key, +Value, -List)
:- private dict_put2/4.
dict_put2([K,_|L], K, V, [K,V|L]) :- !.
dict_put2([J,W|L], K, V, [J,W|R]) :-
   dict_put2(L, K, V, R).
dict_put2([], K, V, [K,V]).

/**
 * dict_del(D, K, E):
 * The predicate succeeds in E with a new dict, where the first value
 * of key K in dict D is removed.
 */
% dict_del(+Dict, +Key, -Dict)
:- public dict_del/3.
dict_del(M, K, N) :-
   M =.. [F|L],
   dict_del2(L, K, R),
   N =.. [F|R].

% dict_del(+List, +Key, -List)
:- private dict_del2/3.
dict_del2([K,_|L], K, L) :- !.
dict_del2([J,W|L], K, [J,W|R]) :-
   dict_del2(L, K, R).
dict_del2([], _, []).

