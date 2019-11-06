/**
 * This module provides JSON object access. Compared to tagged
 * structures the JSON objects do not have a tag and their key
 * value pairs are not automatically sorted. In addition, it is
 * more common for JSON objects to use strings instead of atoms
 * as keys. Further, except for the strings syntax, JSON objects
 * do not require some new syntax.
 *
 * Examples:
 * ?- set_prolog_flag(double_quotes, string).
 * Yes
 * ?- X = {"y":2,"x":1}.
 * X = {"y":2,"x":1}
 *
 * The set of predicates for JSON objects is again modelled after
 * the corresponding SWI-Prolog library for Prolog dicts. The predicates
 * thus resembles our predicates for tagged structures. The JSON
 * objects can be also used in connection with the dot notation.
 * Again, this functionality is provided through the module "func".
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

:- package(library(jekdev/reference/notebook)).

:- module(json, []).

/**
 * is_json(X):
 * The predicate succeeds when X is a JSON object.
 */
% is_json(+Term)
:- public is_json/1.
is_json(X) :- var(X), !, fail.
is_json({}) :- !.
is_json({_}).

/**
 * get_json(K, S, V):
 * The predicate succeeds with the value V of the key K in the
 * JSON object S.
 */
% get_json(+Term, +Json, -Term)
:- public get_json/3.
get_json(_, T, _) :- var(T),
   throw(error(instantiation_error, _)).
get_json(_, {}, _) :- !, fail.
get_json(_, {M}, _) :- var(M),
   throw(error(instantiation_error, _)).
get_json(K, {M}, W) :- ground(K), !,
   get_json_unord(M, K, W).
get_json(K, {M}, W) :- !,
   get_json_enum(M, K, W).
get_json(_, T, _) :-
   throw(error(type_error(json, T), _)).

% get_json_unord(+Map, +Term, -Term)
% See experiment/maps:get/3
:- private get_json_unord/3.
get_json_unord(K:V, K, W) :- !, W = V.
get_json_unord((K:V, _), K, W) :- !, W = V.
get_json_unord((_, M), K, V) :-
   get_json_unord(M, K, V).

% get_json_enum(+Map, +Term, -Term)
:- private get_json_enum/3.
get_json_enum(K:V, K, V).
get_json_enum((K:V, _), K, V).
get_json_enum((_, M), K, V) :-
   get_json_enum(M, K, V).

/**
 * select_json(S, T, R):
 * The predicate succeeds when R unifies with the removal of the
 * key value pairs of S from the JSON object T.
 */
% select_json(+Json, +Json, -Json)
:- public select_json/3.
select_json(_, T, _) :- var(T),
   throw(error(instantiation_error, _)).
select_json(S, {}, R) :- !,
   select_json2(S, true, R).
select_json(_, {M}, _) :- var(M),
   throw(error(instantiation_error, _)).
select_json(S, {M}, R) :- !,
   select_json2(S, M, R).
select_json(_, T, _) :-
   throw(error(type_error(json, T), _)).

% select_json2(+Json, +Map, -Json)
:- private select_json2/3.
select_json2(S, _, _) :- var(S),
   throw(error(instantiation_error, _)).
select_json2({}, M, R) :- !,
   make_json(M, R).
select_json2({M}, _, _) :- var(M),
   throw(error(instantiation_error, _)).
select_json2({N}, M, R) :- !,
   del_json_unord(N, M, O),
   make_json(O, R).
select_json2(S, _, _) :-
   throw(error(type_error(json, S), _)).

% del_json_unord(+Map, +Map, -Map)
:- private del_json_unord/3.
del_json_unord(_, true, _) :- !, fail.
del_json_unord(K:V, M, N) :-
   del_json_unord(M, K, V, N).
del_json_unord((K:V, M), N, O) :-
   del_json_unord(N, K, V, H),
   del_json_unord(M, H, O).

/**
 * del_json(K, S, V, T):
 * The predicate succeeds in T with the deletion of the key K
 * from the JSON object S and in V with the old value.
 */
% del_json(+Term, +Json, -Term, -Json)
:- public del_json/4.
del_json(_, T, _, _) :- var(T),
   throw(error(instantiation_error, _)).
del_json(_, {}, _, _) :- !, fail.
del_json(_, {M}, _, _) :- var(M),
   throw(error(instantiation_error, _)).
del_json(K, {M}, V, R) :- ground(K), !,
   del_json_unord(M, K, V, N),
   make_json(N, R).
del_json(K, {M}, V, R) :- !,
   del_json_enum(M, K, V, N),
   make_json(N, R).
del_json(_, T, _, _) :-
   throw(error(type_error(json, T), _)).

% del_json_unord(+Map, +Term, -Term, -Map)
% See experiment/maps:remove/3
:- private del_json_unord/4.
del_json_unord(K:V, K, W, true) :- !, W = V.
del_json_unord((K:V, M), K, W, M) :- !, W = V.
del_json_unord((X, M), K, V, R) :-
   del_json_unord(M, K, V, N),
   make_and(N, X, R).

% del_json_enum(+Map, +Term, -Term, -Map)
:- private del_json_enum/4.
del_json_enum(K:V, K, V, true).
del_json_enum((K:V, M), K, V, M).
del_json_enum((X, M), K, V, R) :-
   del_json_enum(M, K, V, N),
   make_and(N, X, R).

% make_and(+Map, +Pair, -Map)
:- private make_and/3.
make_and(true, X, X) :- !.
make_and(M, X, (X, M)).

/**
 * put_json(S, T, R):
 * The predicate succeeds in R with the replacement of the
 * key value pairs of S in the JSON object T.
 */
% put_json(+Json, +Json, -Json)
:- public put_json/3.
put_json(_, T, _) :- var(T),
   throw(error(instantiation_error, _)).
put_json(S, {}, R) :- !,
   put_json2(S, true, R).
put_json(_, {M}, _) :- var(M),
   throw(error(instantiation_error, _)).
put_json(S, {M}, R) :- !,
   put_json2(S, M, R).
put_json(_, T, _) :-
   throw(error(type_error(json, T), _)).

% put_json2(+Json, +Map, -Json)
:- private put_json2/3.
put_json2(S, _, _) :- var(S),
   throw(error(instantiation_error, _)).
put_json2({}, M, R) :- !,
   make_json(M, R).
put_json2({M}, _, _) :- var(M),
   throw(error(instantiation_error, _)).
put_json2({N}, M, R) :- !,
   put_json_unord(N, M, O),
   make_json(O, R).
put_json2(S, _, _) :-
   throw(error(type_error(json, S), _)).

% put_json_unord(+Map, +Map, -Map)
:- private put_json_unord/3.
put_json_unord(N, true, N) :- !.
put_json_unord(K:V, M, N) :-
   put_json_unord(M, K, V, N).
put_json_unord((K:V, M), N, O) :-
   put_json_unord(N, K, V, H),
   put_json_unord(M, H, O).

/**
 * put_json(K, S, V, T):
 * The predicate succeeds in T with the replacement of the
 * new value V for the key K by in the JSON object S.
 */
% put_json(+Term, +Json, +Term, -Json)
:- public put_json/4.
put_json(_, T, _, _) :- var(T),
   throw(error(instantiation_error, _)).
put_json(K, _, _, _) :- \+ ground(K),
   throw(error(instantiation_error, _)).
put_json(K, {}, V, R) :- !,
   make_json(K:V, R).
put_json(_, {M}, _, _) :- var(M),
   throw(error(instantiation_error, _)).
put_json(K, {M}, V, R) :- !,
   put_json_unord(M, K, V, N),
   make_json(N, R).
put_json(_, T, _, _) :-
   throw(error(type_error(json, T), _)).

% put_json_unord(+Map, +Term, +Term, -Map)
% See experiment/maps:put/4
:- private put_json_unord/4.
put_json_unord(K:_, K, W, K:W) :- !.
put_json_unord(K:V, J, W, (K:V, J:W)).
put_json_unord((K:_, M), K, W, (K:W, M)) :- !.
put_json_unord((X, M), K, V, (X, N)) :-
   put_json_unord(M, K, V, N).

% make_json(+Map, -Json)
:- private make_json/2.
make_json(true, R) :- !, R = {}.
make_json(M, {M}).
