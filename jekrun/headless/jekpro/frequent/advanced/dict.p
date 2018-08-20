/**
 * This module provides tagged structure access. Tagged structures are
 * also known as Prolog dicts. They have their own syntax as either an
 * empty dict Term0 {} or a non-empty dict Term0 { Term1 } which are
 * short-hands for ordinary compounds. When this module is imported,
 * the syntax will be enabled in the importing module.
 *
 * Examples:
 * ?- P = point{x:1,y:2}, get_dict(y, P, Y).
 * Y = 2
 * ?- P = point{x:1,y:2}, Tag{y:Y} :< P.
 * Tag = point,
 * Y = 2
 *
 * We do not yet have an automatic sorting of the keys of a Prolog
 * dict. This is planned for further releases. The predicates are
 * modelled after the SWI-Prolog built-ins for Prolog dicts. Our
 * data structure would allow more bidirectionality, but we adopted
 * most instantiation tests of SWI-Prolog.
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

:- package(library(jekpro/frequent/advanced)).
:- use_package(foreign(jekpro/frequent/advanced)).

:- module(dict, []).

:- public postfix(sys_struct).
:- op(100, yf, sys_struct).

:- public infix(:<).
:- op(700, xfx, :<).

/**
 * is_dict(X):
 * The predicate succeeds when X is a tagged structure.
 */
% is_dict(+Term)
:- public is_dict/1.
is_dict(X) :-
   var(X), !, fail.
is_dict(_{}) :- !.
is_dict(_{_}).

/**
 * is_dict(X, T):
 * The predicate succeeds when X is a tagged structure and
 * when T unifies with the tag of the tagged structure.
 */
% is_dict(+Term, -Term)
:- public is_dict/2.
is_dict(X, _) :-
   var(X), !, fail.
is_dict(Tag{}, Tag) :- !.
is_dict(Tag{_}, Tag).

/**
 * dict_pairs(X, T, L):
 * The predicate succees in X with the tagged structure
 * that has tag T and key value pairs L.
 */
% dict_pairs(+-Dict, -+Term, -+List)
:- public dict_pairs/3.
dict_pairs(X, T, L) :-
   var(X), !,
   dict_pairs2(L, T, X).
dict_pairs(T{}, S, L) :- !,
   S = T,
   L = [].
dict_pairs(T{L}, S, R) :- !,
   S = T,
   map_to_list(L, R).
dict_pairs(T, _, _) :-
   throw(error(type_error(dict,T),_)).

% dict_pairs(+List, +Term, -Term)
:- private dict_pairs2/3.
dict_pairs2(L, _, _) :-
   var(L),
   throw(error(instantiation_error,_)).
dict_pairs2([], T, T{}).
dict_pairs2([K-V|L], T, T{R}) :-
   list_to_map(L, K, V, R).

% list_to_map(+List, +Term, +Term, -Map)
:- private list_to_map/4.
list_to_map(L, _, _, _) :-
   var(L),
   throw(error(instantiation_error,_)).
list_to_map(_, K, _, _) :-
   \+ ground(K),
   throw(error(instantiation_error,_)).
list_to_map([], K, V, K:V).
list_to_map([J-W|L], K, V, R) :-
   list_to_map(L, J, W, H),
   make_map(K, H, V, R).

% make_map(+Term, +Map, +Term, -Map)
:- private make_map/4.
make_map(K, M, _, _) :-
   get_dict2(M, K, _),
   throw(error(domain_eror(duplicate_key,K),_)).
make_map(K, M, V, (K:V,M)).

% map_to_list(+Map, -List)
:- private map_to_list/2.
map_to_list(L, _) :-
   var(L),
   throw(error(instantiation_error,_)).
map_to_list(K:V, [K-V]).
map_to_list((K:V,L), [K-V|R]) :-
   map_to_list(L, R).

/**
 * get_dict(K, S, V):
 * The predicate succeeds with the value V of the key K in the
 * tagged structure S.
 */
% get_dict(+Term, +Dict, -Term)
:- public get_dict/3.
get_dict(_, T, _) :-
   var(T),
   throw(error(instantiation_error,_)).
get_dict(_, _{}, _) :- !, fail.
get_dict(K, _{M}, W) :-
   ground(K), !,
   get_dict2(M, K, W), !.
get_dict(K, _{M}, W) :- !,
   get_dict2(M, K, W).
get_dict(_, T, _) :-
   throw(error(type_error(dict,T),_)).

% get_dict2(+Map, +Term, -Term)
:- private get_dict2/3.
get_dict2(M, _, _) :-
   var(M),
   throw(error(instantiation_error,_)).
get_dict2(K:V, K, V).
get_dict2((K:V,_), K, V).
get_dict2((_,M), K, V) :-
   get_dict2(M, K, V).

/**
 * S :< T:
 * The predicate succeeds when the tags of S and T unify and when
 * the key value pairs of the tagged structure S appear in the
 * tagged structure T.
 */
% +Dict :< +Dict
:- public :< /2.
Tag{} :< Tag{} :- !.
Tag{} :< Tag{_} :- !.
Tag{_} :< Tag{} :- !, fail.
Tag{D} :< Tag{E} :-
   select_dict2(D, E).

% select_dict2(+Map, +Map)
:- private select_dict2/2.
select_dict2(M, _) :-
   var(M),
   throw(error(instantiation_error,_)).
select_dict2(K:V, D) :-
   get_dict2(D, K, V), !.
select_dict2((K:V,D), E) :-
   get_dict2(E, K, V), !,
   select_dict2(D, E).

/**
 * del_dict(K, S, V, T):
 * The predicate succeeds in T with the deletion of the key K
 * from the tagged structure S and in V with the old value.
 */
% del_dict(+Term, +Dict, -Term, -Dict)
:- public del_dict/4.
del_dict(_, T, _, _) :-
   var(T),
   throw(error(instantiation_error,_)).
del_dict(_, _{}, _, _) :- !, fail.
del_dict(K, T{M}, V, R) :-
   ground(K), !,
   del_dict2(M, K, V, N),
   make_dict(N, T, H), !,
   R = H.
del_dict(K, T{M}, V, R) :- !,
   del_dict2(M, K, V, N),
   make_dict(N, T, H),
   R = H.
del_dict(_, T, _, _) :-
   throw(error(type_error(dict,T),_)).

% del_dict2(+Map, +Term, -Term, -Map)
del_dict2(M, _, _, _) :-
   var(M),
   throw(error(instantiation_error,_)).
del_dict2(K:V, K, V, true).
del_dict2((K:V,M), K, V, M).
del_dict2((X,M), K, V, R) :-
   del_dict2(M, K, V, N),
   make_and(N, X, R).

% make_and(+Map, +Pair, -Map)
:- private make_and/3.
make_and(true, X, X) :- !.
make_and(M, X, (X,M)).

% make_dict(+Map, +Term, -Dict)
:- private make_dict/3.
make_dict(true, T, T{}) :- !.
make_dict(M, T, T{M}).

/**
 * put_dict(K, S, V, T):
 * The predicate succeeds in T with the replacement of the
 * new value V for the key K by in the tagged structure S.
 */
% put_dict(+Term, +Dict, +Term, -Dict)
:- public put_dict/4.
put_dict(_, T, _, _) :-
   var(T),
   throw(error(instantiation_error,_)).
put_dict(K, _, _, _) :-
   \+ ground(K),
   throw(error(instantiation_error,_)).
put_dict(K, T{}, V, R) :- !,
   R = T{K:V}.
put_dict(K, T{M}, V, R) :- !,
   put_dict2(M, K, V, N),
   R = T{N}.
put_dict(_, T, _, _) :-
   throw(error(type_error(dict,T),_)).

% put_dict(+Map, +Term, +Term, -Map)
:- private put_dict2/4.
put_dict2(M, K, V, R) :-
   del_dict2(M, K, _, N), !,
   make_and(N, K:V, R).
put_dict2(M, K, V, (K:V,M)).
