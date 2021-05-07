/**
 * This module provides some choice operators for answer set
 * programming via forward chaining. Since we use the (:-)/2
 * operator already for backward chaining rules, the operator
 * (<=)/2 form the module "delta" needs to be used to write
 * answer set programming rules. Take this example:
 *
 * Example:
 * :- p, q, r.
 * {p,r}.
 * {q} :- p.
 * {r} :- p.
 *
 * This would need to be written as follows. For answer set
 * programming constraints a forward chaining rule with a fail
 * action is suggested. Further answer set programming ordinary
 * facts and disjunctive facts need a start condition such as "init",
 * which can then be used to produce an anwser set:
 *
 * Example:
 * fail <= posted(p), posted(q), posted(r).
 * choose([p,r]) <= posted(init).
 * choose([q]) <= posted(p).
 * choose([r]) <= posted(p).
 * ?- post(init).
 *
 * Our approach to answer set programming allows explicit mixing of
 * forward chaining and backward chaining in that a forward chaining
 * rule allows backward chaining goals in its condition part. The
 * only predicate that this module currently provides is a choose/1
 * operator that allows satisfiability search.
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

:- package(library(jekmin/reference/minimal)).

:- module(asp, []).
:- use_module(library(basic/random)).
:- use_module(library(basic/lists)).
:- reexport(library(minimal/delta)).

/**************************************************************/
/* Primitive Choice                                           */
/**************************************************************/

/**
 * choose(L):
 * choose(L, G):
 * If a positive literal from L is already satisfied, the construct
 * does nothing before further solving. Otherwise, the construct posts
 * each positive literal from L in input order before further solving.
 */
% choose(+List)
:- public choose/1.
choose(L) :- sys_least(L), !.
choose(L) :-
   member(A, L),
   post(A).

% choose(+List, +Goal)
:- public choose/2.
:- meta_predicate choose(?, 0).
choose(L, G) :- sys_least(L), !, G.
choose(L, G) :-
   member(A, L),
   post(A, G).

/**
 * random_choose(L):
 * random_choose(L, G):
 * If a positive literal from L is already satisfied, the construct
 * does nothing before further solving. Otherwise, the construct posts
 * each positive literal from L in random order before further solving.
 */
% random_choose(+List)
:- public random_choose/1.
random_choose(L) :- sys_least(L), !.
random_choose(L) :-
   random_permutation(L, H),
   member(A, H),
   post(A).

% random_choose(+List, +Goal)
:- public random_choose/2.
:- meta_predicate random_choose(?, 0).
random_choose(L, G) :- sys_least(L), !, G.
random_choose(L, G) :-
   random_permutation(L, H),
   member(A, H),
   post(A, G).

/**
 * sys_least(L):
 * The predicate succeeds when at least one positive literal
 * from L already exists in the forward store.
 */
% sys_least(+List)
:- private sys_least/1.
sys_least([A|_]) :- clause(A, true), !.
sys_least([_|L]) :- sys_least(L).

/**************************************************************/
/* Minimum Choice                                             */
/**************************************************************/

/**
 * min_choose(N, L):
 * min_choose(N, L, G):
 * If N positive literals from L are already satisfied, the construct
 * does nothing before further solving. Otherwise, the construct posts
 * N positive literals from L in input order before further solving.
 */
% min_choose(+Integer, +List)
:- public min_choose/2.
min_choose(N, L) :-
   sys_filter(N, L, M, R),
   min_choose2(M, R).

% min_choose2(+Integer, +List)
:- private min_choose2/2.
min_choose2(0, _) :- !.
min_choose2(N, L) :-
   M is N-1,
   length(L, K),
   J is K-N,
   min_member(J, A, L, R),
   post(A),
   min_choose(M, R).

% min_choose(+Integer, +List, +Goal)
:- public min_choose/3.
:- meta_predicate min_choose(?, ?, 0).
min_choose(N, L, G) :-
   sys_filter(N, L, M, R),
   min_choose2(M, R, G).

% min_choose2(+Integer, +List, +Goal)
:- private min_choose2/3.
:- meta_predicate min_choose2(?, ?, 0).
min_choose2(0, _, G) :- !, G.
min_choose2(N, L, G) :-
   M is N-1,
   length(L, K),
   J is K-N,
   min_member(J, A, L, R),
   post(A, min_choose(M, R, G)).

/**
 * random_min_choose(N, L):
 * random_min_choose(N, L, G):
 * If N positive literal from L are already satisfied, the construct
 * does nothing before further solving. Otherwise, the construct posts
 * N positive literals from L in random order before further solving.
 */
% random_min_choose(+Integer, +List)
:- public random_min_choose/2.
random_min_choose(N, L) :-
   sys_filter(N, L, M, R),
   random_min_choose2(M, R).

% random_min_choose2(+Integer, +List)
:- private random_min_choose2/2.
random_min_choose2(0, _) :- !.
random_min_choose2(N, L) :-
   M is N-1,
   length(L, K),
   J is K-N,
   random_permutation(L, H),
   min_member(J, A, H, R),
   post(A),
   min_choose(M, R).

% random_min_choose(+Integer, +List, +Goal)
:- public random_min_choose/3.
:- meta_predicate random_min_choose(?, ?, 0).
random_min_choose(N, L, G) :-
   sys_filter(N, L, M, R),
   random_min_choose2(M, R, G).

% random_min_choose2(+Integer, +List, +Goal)
:- public random_min_choose2/3.
:- meta_predicate random_min_choose2(?, ?, 0).
random_min_choose2(0, _, G) :- !, G.
random_min_choose2(N, L, G) :-
   M is N-1,
   length(L, K),
   J is K-N,
   random_permutation(L, H),
   min_member(J, A, H, R),
   post(A, random_min_choose(M, R, G)).

% min_member(+Integer, -Elem, +List, -List)
% https://stackoverflow.com/q/10388109/502187
:- private min_member/4.
min_member(0, X, [X|L], L) :- !.
min_member(_, X, [X|L], L).
min_member(N, X, [_|L], R) :-
   M is N-1,
   min_member(M, X, L, R).

/**
 * sys_filter(N, L, M, R):
 * The predicate succeeds in M and R after filtering maximally
 * N positive literals from the list L.
 */
:- private sys_filter/4.
sys_filter(0, L, 0, L) :- !.
sys_filter(N, [], N, []) :- !.
sys_filter(N, [A|L], M, R) :- clause(A, true), !,
   H is N-1,
   sys_filter(H, L, M, R).
sys_filter(N, [A|L], M, [A|R]) :-
   sys_filter(N, L, M, R).
