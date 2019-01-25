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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- package(library(jekpro/frequent/advanced)).

:- module(json, []).

/**
 * get_json(K, S, V):
 * The predicate succeeds with the value V of the key K in the
 * JSON object S.
 */
% get_json(+Term, +Dict, -Term)
:- public get_json/3.
get_json(_, T, _) :-
   var(T),
   throw(error(instantiation_error,_)).
get_json(_, {}, _) :- !, fail.
get_json(_, {M}, _) :-
   var(M),
   throw(error(instantiation_error,_)).
get_json(K, {M}, W) :-
   ground(K), !,
   get_json_unord(M, K, W).
get_json(K, {M}, W) :- !,
   get_json_enum(M, K, W).
get_json(_, T, _) :-
   throw(error(type_error(json,T),_)).

% get_json_unord(+Map, +Term, -Term)
% See experiment/maps:get/3
:- private get_json_unord/3.
get_json_unord(K:V, K, W) :- !,
   W = V.
get_json_unord((K:V,_), K, W) :- !,
   W = V.
get_json_unord((_,M), K, V) :-
   get_json_unord(M, K, V).

% get_json_enum(+Map, +Term, -Term)
:- private get_json_enum/3.
get_json_enum(K:V, K, V).
get_json_enum((K:V,_), K, V).
get_json_enum((_,M), K, V) :-
   get_json_enum(M, K, V).
