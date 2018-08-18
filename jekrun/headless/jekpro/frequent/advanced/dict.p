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
dict_pairs(T{}, T, []) :- !.
dict_pairs(T{L}, T, R) :-
   map_to_list(L, R).

% dict_pairs(+List, +Term, -Term)
:- private dict_pairs2/3.
dict_pairs2(L, _, _) :-
   var(L),
   throw(error(instantiation_error,_)).
dict_pairs2([], T, T{}).
dict_pairs2([X|L], T, T{R}) :-
   list_to_map(L, X, R).

% list_to_map(+List, +Pair, -Map)
:- private list_to_map/3.
list_to_map(L, _, _) :-
   var(L),
   throw(error(instantiation_error,_)).
list_to_map([], K-V, K:V).
list_to_map([X|L], K-V, R) :-
   list_to_map(L, X, H),
   make_map(K, H, V, R).

% make_map(+Term, +Map, +Term, -Map)
:- private make_map/4.
make_map(K, D, _, _) :-
   get_dict2(D, K, _),
   throw(error(domain_eror(duplicate_key,K),_)).
make_map(K, D, V, (K:V,D)).

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
get_dict(_, _{}, _) :- !, fail.
get_dict(K, _{D}, W) :-
   var(K), !,
   get_dict2(D, K, W).
get_dict(K, _{D}, W) :-
   get_dict2(D, K, W), !.

% get_dict2(+Map, +Term, -Term)
:- private get_dict2/3.
get_dict2(L, _, _) :-
   var(L),
   throw(error(instantiation_error,_)).
get_dict2(K:V, K, V).
get_dict2((K:V,_), K, V).
get_dict2((_,D), K, V) :-
   get_dict2(D, K, V).

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
select_dict2(K:V, D) :- !,
   get_dict2(K, D, V).
select_dict2((K:V,D), E) :-
   get_dict2(K, E, V),
   select_dict2(D, E).

/**
 * del_dict(K, S, T):
 * The predicate succeeds in T with the deletion of the key K from
 * the tagged structure S.
 */
% del_dict(+Term, +Dict, -Dict)

/**
 * put_dict(K, S, V, T):
 * The predicate succeeds in T with the replacement of the value
 * for the key K by in the tagged structure S.
 */
% put_dict(+Term, +Dict, +Term, -Dict)

