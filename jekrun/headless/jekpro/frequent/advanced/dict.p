/**
 * This module provides tagged structure access. Tagged structures are
 * also known as Prolog dicts. They have their own syntax as either an
 * empty dict Term0 {} or a non-empty dict Term0 { Term1 } which are
 * short-hands for ordinary compounds. When this module is imported,
 * the syntax will be enabled in the importing module.
 *
 * Examples:
 * ?- X = point{y:2, x:1}.
 * X = point{x:1,y:2}
 * ?- point{y:2, x:1} = point{x:1, y:2}.
 * Yes
 *
 * The keys inside a Prolog dictionary are not restricted to any Prolog
 * term category. All that is required is that they are ground. With the
 * introduction of function expansion in the Jekejeke Prolog runtime
 * library, the Prolog dictionaries will be automatically pre-sorted
 * during consult. This assures that they are equal even if they
 * differ in their key order.
 *
 * Example:
 * ?- P = point{x:1,y:2}, get_dict(y, P, Y).
 * Y = 2
 * ?- P = point{x:1,y:2}, Tag{y:Y} :< P.
 * Tag = point,
 * Y = 2
 *
 * The set of predicates for Prolog dicts is modelled after the
 * corresponding SWI-Prolog library. We have adopted most of the
 * instantiation checks and most of the type checks. What this library
 * does not provide is the dot notation and the corresponding facility to
 * define and invoke functions for Prolog dictionaries.
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

/**
 * Obtained rights, copyright notice of SWI-Prolog 7.7.18 the C
 * source code pl-dict.c when we adopted the API specification.
 *
 *    Author:        Jan Wielemaker
 *    E-mail:        J.Wielemaker@vu.nl
 *    WWW:           www.swi-prolog.org
 *    Copyright (c)  2013-2017, VU University Amsterdam
 *    All rights reserved.
 *
 *    Redistribution and use in source and binary forms, with or without
 *    modification, are permitted provided that the following conditions
 *    are met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 *  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 *  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 *  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE
 */
:- package(library(jekpro/frequent/advanced)).

:- module(dict, []).

:- public postfix(sys_struct).
:- op(100, yf, sys_struct).

:- public infix(:<).
:- op(700, xfx, :<).

:- public infix(>:<).
:- op(700, xfx, >:<).

% user:rest_expansion(+Term, -Term)
:- public user:rest_expansion/2.
:- multifile user:rest_expansion/2.
user:rest_expansion(_{D}, _) :-
   var(D), !, fail.
user:rest_expansion(T{D}, T{M}) :-
   map_to_list(D, L),
   keysort(L, H),
   L \== H, !,
   list_to_map(H, M).

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
 * The predicate succeeds in X with the tagged structure
 * that has tag T and key value pairs L.
 */
% dict_pairs(+-Dict, -+Term, -+List)
:- public dict_pairs/3.
dict_pairs(X, T, L) :-
   var(X), !,
   keysort(L, H),
   list_to_map(H, M),
   make_dict(M, T, X).
dict_pairs(T{}, S, L) :- !,
   S = T,
   L = [].
dict_pairs(T{M}, S, L) :- !,
   S = T,
   map_to_list(M, L).
dict_pairs(T, _, _) :-
   throw(error(type_error(dict,T),_)).

% list_to_map(+List, -Map)
:- private list_to_map/2.
list_to_map(L, _) :-
   var(L),
   throw(error(instantiation_error,_)).
list_to_map([], true).
list_to_map([K-V|L], R) :-
   list_to_map(L, H),
   make_map(K, V, H, R).

% make_map(+Term, +Term, +Map, -Map)
:- private make_map/4.
make_map(K, _, _, _) :-
   \+ ground(K),
   throw(error(instantiation_error,_)).
make_map(K, _, M, _) :-
   get_dict_ord(M, K, _),
   throw(error(domain_eror(duplicate_key,K),_)).
make_map(K, V, M, R) :-
   make_and(M, K:V, R).

% make_and(+Map, +Pair, -Map)
:- private make_and/3.
make_and(true, X, X) :- !.
make_and(M, X, (X,M)).

% make_dict(+Map, +Term, -Dict)
:- private make_dict/3.
make_dict(true, T, R) :- !,
   R = T{}.
make_dict(M, T, T{M}).

% map_to_list(+Map, -List)
:- private map_to_list/2.
map_to_list(M, _) :-
   var(M),
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
get_dict(_, _{M}, _) :-
   var(M),
   throw(error(instantiation_error,_)).
get_dict(K, _{M}, W) :-
   ground(K), !,
   get_dict_ord(M, K, W).
get_dict(K, _{M}, W) :- !,
   get_dict_enum(M, K, W).
get_dict(_, T, _) :-
   throw(error(type_error(dict,T),_)).

% get_dict_ord(+Map, +Term, -Term)
% See experiment/ordmaps:ord_get/3
:- private get_dict_ord/3.
get_dict_ord(K:_, J, _) :-
   J @< K, !, fail.
get_dict_ord(K:V, K, W) :- !,
   W = V.
get_dict_ord((K:_,_), J, _) :-
   J @< K, !, fail.
get_dict_ord((K:V,_), K, W) :- !,
   W = V.
get_dict_ord((_,M), K, V) :-
   get_dict_ord(M, K, V).

% get_dict_enum(+Map, +Term, -Term)
:- private get_dict_enum/3.
get_dict_enum(K:V, K, V).
get_dict_enum((K:V,_), K, V).
get_dict_enum((_,M), K, V) :-
   get_dict_enum(M, K, V).

/**
 * S :< T:
 * The predicate succeeds when the tags of S and T unify and when
 * the key value pairs of the tagged structure S appear in the
 * tagged structure T.
 */
% +Dict :< +Dict
:- public :< /2.
T{} :< T{} :- !.
T{} :< T{_} :- !.
T{_} :< T{} :- !, fail.
_{D} :< _ :-
   var(D),
   throw(error(instantiation_error,_)).
_ :< _{E} :-
   var(E),
   throw(error(instantiation_error,_)).
T{D} :< T{E} :-
   select_dict(D, E).

% select_dict(+Map, +Map)
:- private select_dict/2.
select_dict(_, true) :- !, fail.
select_dict(K:V, D) :-
   select_dict_ord(D, K, V, _).
select_dict((K:V,D), E) :-
   select_dict_ord(E, K, V, F),
   select_dict(D, F).

% select_dict_ord(+Map, +Term, -Term, -Map)
% See avanced/ordsets:ord_subset/3
:- private select_dict_ord/4.
select_dict_ord(K:_, J, _, _) :-
   J @< K, !, fail.
select_dict_ord(K:V, K, W, true) :- !,
   W = V.
select_dict_ord((K:_,_), J, _, _) :-
   J @< K, !, fail.
select_dict_ord((K:V,M), K, W, M) :- !,
   W = V.
select_dict_ord((_,M), K, V, N) :-
   select_dict_ord(M, K, V, N).

/**
 * S >:< T:
 * The predicate succeeds when the tags of S and T unify and when
 * the values for the common keys of S and T unify.
 */
% +Dict >:< +Dict
:- public >:< /2.
T{} >:< T{} :- !.
T{} >:< T{_} :- !.
T{_} >:< T{} :- !.
_{D} >:< _ :-
   var(D),
   throw(error(instantiation_error,_)).
_ >:< _{E} :-
   var(E),
   throw(error(instantiation_error,_)).
T{D} >:< T{E} :-
   join_dict(D, E).

% join_dict(+Map, +Map)
:- private join_dict/2.
join_dict(_, true) :- !.
join_dict(K:V, D) :-
   join_dict_ord(D, K, V, _).
join_dict((K:V,D), E) :-
   join_dict_ord(E, K, V, F),
   join_dict(D, F).

% join_dict_ord(+Map, +Term, -Term, -Map)
% See avanced/ordsets:ord_intersection/3
:- private join_dict_ord/4.
join_dict_ord(K:V, J, _, K:V) :-
   J @< K, !.
join_dict_ord(K:V, K, W, true) :- !,
   W = V.
join_dict_ord(_:_, _, _, true).
join_dict_ord((K:V,M), J, _, (K:V,M)) :-
   J @< K, !.
join_dict_ord((K:V,M), K, W, M) :- !,
   W = V.
join_dict_ord((_,M), K, V, N) :-
   join_dict_ord(M, K, V, N).

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
del_dict(_, _{M}, _, _) :-
   var(M),
   throw(error(instantiation_error,_)).
del_dict(K, T{M}, V, R) :-
   ground(K), !,
   del_dict_ord(M, K, V, N),
   make_dict(N, T, R).
del_dict(K, T{M}, V, R) :- !,
   del_dict_enum(M, K, V, N),
   make_dict(N, T, R).
del_dict(_, T, _, _) :-
   throw(error(type_error(dict,T),_)).

% del_dict_ord(+Map, +Term, -Term, -Map)
% See experiment/ordmaps:ord_remove/3
:- private del_dict_ord/4.
del_dict_ord(K:_, J, _, true) :-
   J @< K, !, fail.
del_dict_ord(K:V, K, W, true) :- !,
   W = V.
del_dict_ord((K:_,_), J, _, _) :-
   J @< K, !, fail.
del_dict_ord((K:V,M), K, W, M) :- !,
   W = V.
del_dict_ord((X,M), K, V, R) :-
   del_dict_ord(M, K, V, N),
   make_and(N, X, R).

% del_dict_enum(+Map, +Term, -Term, -Map)
:- private del_dict_enum/4.
del_dict_enum(K:V, K, V, true).
del_dict_enum((K:V,M), K, V, M).
del_dict_enum((X,M), K, V, R) :-
   del_dict_enum(M, K, V, N),
   make_and(N, X, R).

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
   make_dict(K:V, T, R).
put_dict(_, _{M}, _, _) :-
   var(M),
   throw(error(instantiation_error,_)).
put_dict(K, T{M}, V, R) :- !,
   put_dict_ord(M, K, V, N),
   make_dict(N, T, R).
put_dict(_, T, _, _) :-
   throw(error(type_error(dict,T),_)).

% put_dict_ord(+Map, +Term, +Term, -Map)
% See experiment/ordmaps:ord_put/3
:- private put_dict_ord/4.
put_dict_ord(K:V, J, W, (J:W,K:V)) :-
   J @< K, !.
put_dict_ord(K:_, K, W, K:W) :- !.
put_dict_ord(K:V, J, W, (K:V,J:W)).
put_dict_ord((K:V,M), J, W, (J:W,K:V,M)) :-
   J @< K, !.
put_dict_ord((K:_,M), K, W, (K:W,M)) :- !.
put_dict_ord((X,M), K, V, (X,N)) :-
   put_dict_ord(M, K, V, N).

/**
 * put_dict(S, T, R):
 * The predicate succeeds in R with the replacement of the
 * key value pairs of S in the tagged structure T.
 */
% put_dict(+Dict, +Dict, -Dict)
:- public put_dict/3.
put_dict(_, T, _) :-
   var(T),
   throw(error(instantiation_error,_)).
put_dict(S, T{}, R) :- !,
   put_dict2(S, T, true, R).
put_dict(_, _{M}, _) :-
   var(M),
   throw(error(instantiation_error,_)).
put_dict(S, T{M}, R) :- !,
   put_dict2(S, T, M, R).
put_dict(_, T, _) :-
   throw(error(type_error(dict,T),_)).

% put_dict2(+Dict, +Tag, +Map, -Dict)
:- private put_dict2/4.
put_dict2(S, _, _, _) :-
   var(S),
   throw(error(instantiation_error,_)).
put_dict2(_{}, T, M, R) :- !,
   make_dict(M, T, R).
put_dict2(_{M}, _, _, _) :-
   var(M),
   throw(error(instantiation_error,_)).
put_dict2(_{N}, T, M, R) :- !,
   put_dict_ord(N, M, O),
   make_dict(O, T, R).
put_dict2(S, _, _) :-
   throw(error(type_error(dict,S),_)).

% put_dict_ord(+Map, +Map, -Map)
:- private put_dict_ord/3.
put_dict_ord(N, true, N) :- !.
put_dict_ord(K:V, M, N) :-
   put_dict_ord(M, K, V, N).
put_dict_ord((K:V,M), N, O) :-
   put_dict_ord(N, K, V, M, O).

% put_dict_ord(+Map, +Term, +Term, +Map, -Map)
% See experiment/ordmaps:ord_put/3
:- private put_dict_ord/5.
put_dict_ord(K:V, J, W, M, (J:W,N)) :-
   J @< K, !,
   put_dict_ord(M, K:V, N).
put_dict_ord(K:_, K, W, M, (K:W,M)) :- !.
put_dict_ord(K:V, J, W, M, (K:V,J:W,M)).
put_dict_ord((K:V,M), J, W, N, (J:W,O)) :-
   J @< K, !,
   put_dict_ord(N, (K:V,M), O).
put_dict_ord((K:_,M), K, W, N, (K:W,O)) :- !,
   put_dict_ord(N, M, O).
put_dict_ord((X,M), K, V, N, (X,O)) :-
   put_dict_ord(M, K, V, N, O).
