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
/* Minimum Choice                                             */
/**************************************************************/

% min_choose(+Integer, +List)
:- public min_choose/2.
min_choose(N, L) :-
   length(L, K), N =< K, !,
   J is K+1-N,
   combination(J, L, R),
   min_choose2(R).

% min_choose(+Matrice)
:- private min_choose2/1.
min_choose2([X|L]) :-
   choose(X),
   min_choose2(L).
min_choose2([]).

% random_min_choose(+Integer, +List)
:- public random_min_choose/2.
random_min_choose(N, L) :-
   length(L, K), N =< K, !,
   J is K+1-N,
   combination(J, L, H),
   random_permutation(H, R),
   random_min_choose2(R).

% random_min_choose2(+Matrice)
:- private random_min_choose2/1.
random_min_choose2([X|L]) :-
   random_choose(X),
   random_min_choose2(L).
random_min_choose2([]).

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
choose(L) :- sys_least_one(L), !.
choose([A|L]) :- choose2(L, A).

% choose(+List, +Goal)
:- public choose/2.
:- meta_predicate choose(?, 0).
choose(L, G) :- sys_least_one(L), !, G.
choose([A|L], G) :- choose2(L, A, G).

/**
 * random_choose(L):
 * random_choose(L, G):
 * If a positive literal from L is already satisfied, the construct
 * does nothing before further solving. Otherwise, the construct posts
 * each positive literal from L in random order before further solving.
 */
% random_choose(+List)
:- public random_choose/1.
random_choose(L) :- sys_least_one(L), !.
random_choose(L) :- random_permutation(L, [A|H]), choose2(H, A).

% random_choose(+List, +Goal)
:- public random_choose/2.
:- meta_predicate random_choose(?, 0).
random_choose(L, G) :- sys_least_one(L), !, G.
random_choose(L, G) :- random_permutation(L, [A|H]), choose2(H, A, G).

/**
 * choose2(L, A):
 * choose2(L, A, G):
 * The predicate posts the positive literal A and then each positive
 * literal from L in input order before further solving.
 */
% choose2(+List, +Term)
:- private choose2/2.
choose2([], A) :- !, post(A).
choose2([_|_], A) :- post(A).
choose2([A|L], _) :- choose2(L, A).

% choose2(+List, +Term, +Goal)
:- private choose2/3.
:- meta_predicate choose2(?, -1, 0).
choose2([], A, G) :- !, post(A, G).
choose2([_|_], A, G) :- post(A, G).
choose2([A|L], _, G) :- choose2(L, A, G).

/**
 * sys_last_one(L):
 * The predicate succeeds when at least one positive literal
 * from L already exists in the forward store.
 */
% sys_least_one(+List)
:- private sys_least_one/1.
sys_least_one([A|_]) :- clause(A, true), !.
sys_least_one([_|L]) :- sys_least_one(L).

/**************************************************************/
/* Combination                                                */
/* https://stackoverflow.com/q/10388109/502187                */
/**************************************************************/

% combination(+Integer, +List, -Matrice)
:- private combination/3.
combination(N, L, R) :-
   reverse(L, H),
   combination2(N, H, [], R, []).

% combination2(+Integer, +List, +List, -Matrice, +Matrice)
:- private combination2/5.
combination2(0, _, H, [H|J], J) :-
   !.
combination2(N, L, H, P, Q) :-
   N > 0,
   M is N-1,
   combination3(N, M, L, H, P, Q).

% combination3(+Integer, +Integer, +List, +List, -Matrice, +Matrice)
:- private combination3/6.
combination3(N, M, [X|L], H, P, R) :-
   combination3(N, M, L, H, P, Q),
   combination2(M, L, [X|H], Q, R).
combination3(_, _, [], _, Q, Q).