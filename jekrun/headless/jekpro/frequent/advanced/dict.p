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
 * dict. This is planned for further releases. The only predicates for
 * dicts provided by this module are so far the predicate get_dict/3 to
 * access the value of a key and the predicate (:<)/2 to match the tag
 * and to select multiple values.
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
 * get_dict(K, S, V):
 * The predicate succeeds with the value V of the key K in the
 * tagged structure S.
 */
% get_dict(+Term, +Struct, -Term)
:- public get_dict/3.
get_dict(_, _{}, _) :- fail.
get_dict(K, _{D}, W) :-
   get_dict2(K, D, W).

% get_dict2(+Term, +Pairs, -Term)
:- private get_dict2/3.
get_dict2(K, K:V, W) :- !,
   W = V.
get_dict2(K, (K:V,_), W) :- !,
   W = V.
get_dict2(K, (_,D), W) :-
   get_dict2(K, D, W).

/**
 * S :< T:
 * The predicate succeeds when the tags of S and T unify and when
 * the key value pairs of the tagged structure S appear in the
 * tagged structure T.
 */
% +Struct :< Struct
:- public :< /2.
Tag{} :< Tag{}.
Tag{} :< Tag{_}.
Tag{_} :< Tag{} :- fail.
Tag{D} :< Tag{E} :-
   select_dict2(D, E).

% select_dict2(+Pairs, +Pairs)
:- private select_dict2/2.
select_dict2(K:V, D) :- !,
   get_dict2(K, D, V).
select_dict2((K:V,D), E) :-
   get_dict2(K, E, V),
   select_dict2(D, E).
