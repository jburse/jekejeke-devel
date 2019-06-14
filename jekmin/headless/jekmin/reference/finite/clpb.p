/**
 * As a convenience the SAT solver provides a single solving
 * technique in two incarnations. We provide the following
 * solving technique when there is an attempt to label multiple
 * variables at once or to count the number of solutions.
 *
 * * Brute Finite Search
 *
 * Although variable rankings are found in the literature, we
 * didn’t implement some special search strategy, since we did
 * not yet find a solution to overcome the ranking overhead. The
 * given variables are tried in the given input order. Counting
 * further depends on labelling, since it is no yet able to use
 * counts derived from a single BDD tree.
 *
 * Examples:
 * ?- sat(X=<Y), sat(Y=<Z), sat(Z=<X), labeling([X,Y,Z]).
 * X = 0,
 * Y = 0,
 * Z = 0 ;
 * X = 1,
 * Y = 1,
 * Z = 1
 * ?- sat(X=<Y), sat(Y=<Z), sat(Z=<X), sat_count([X,Y,Z], N).
 * N = 2,
 * sat((X->1;Z->0;1)),
 * sat((X->(Y->1;0);1)),
 * sat((Y->(Z->1;0);1))
 *
 * The Boolean constraint can be used to define more complex
 * conditions. A recurring problem is stating the cardinality
 * of a couple of Boolean expressions. The predicate card/2 has
 * been defined as a corresponding convenience.
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

:- package(library(jekmin/reference/finite)).
:- module(clpb, []).

:- reexport(tree).
:- use_module(library(term/unify)).
:- use_module(library(basic/lists)).
:- use_module(library(basic/random)).
:- use_module(library(experiment/trail)).

/**
 * sat(A):
 * If A is an expression then its satisfiability is posted.
 */
% sat(+Expr)
:- public sat/1.
sat(A) :-
   expr_tree(A, T),
   sat_post(T).

/**
 * sat_post(T):
 * If T is a tree then its satisfiability is posted.
 */
% sat_post(+Tree)
:- private sat_post/1.
sat_post(T) :-
   expr_vars(T, L),
   sat_add_vars(L, H),
   sat_trivial(T, H),
   sat_propagate(T).

/**
 * sat_add_vars(M, V):
 * Posit references to the variable V in the list M.
 */
% sat_add_vars(+List, +Fresh)
:- private sat_add_vars/2.
sat_add_vars([K|L], H) :-
   sys_melt_var(K, A),
   sat_add_var(A, K, H),
   sat_add_vars(L, H).
sat_add_vars([], _).

% sat_add_var(+Var, +Integer, +Fresh)
:- private sat_add_var/3.
sat_add_var(A, K, H) :-
   get_attr(A, clpb, sat_ref(K,F)), !,
   put_attr(A, clpb, sat_ref(K,[H|F])).
sat_add_var(A, K, H) :-
   put_attr(A, clpb, sat_ref(K,[H])).

/**
 * sat_trivial(S, H):
 * The predicate checks the trivial cases of a new constraint S and handle H.
 */
% sat_trivial(+Tree, +Fresh)
:- private sat_trivial/2.
sat_trivial(one, H) :- !,
   del_attr(H, clpb).
sat_trivial(zero, _) :- !, fail.
sat_trivial(S, H) :-
   put_attr(H, clpb, sat_root(S)).

/**
 * sat_propagate(S):
 * The predicate propagates an existing sat constraint S.
 */
% sat_propagate(+Tree)
:- private sat_propagate/1.
sat_propagate(node3(X,_,_,zero)) :- !,
   sys_melt_var(X, Y),
   Y = 1.
sat_propagate(node3(X,_,zero,_)) :- !,
   sys_melt_var(X, Y),
   Y = 0.
sat_propagate(node3(X,_,node3(Y,_,A,zero),node3(Y,_,zero,A))) :- !,
   sys_melt_var(X, Z),
   sys_melt_var(Y, T),
   Z = T.
sat_propagate(_).

/*****************************************************************/
/* Unify Hook                                                    */
/*****************************************************************/

/**
 * attr_unify_hook(A, W):
 * The predicate is called when a varable with attribute
 * term A got the value W assigned.
 */
% attr_unify_hook(+Attr, +Term)
:- public attr_unify_hook/2.
:- override attr_unify_hook/2.
attr_unify_hook(sat_ref(K,F), W) :-
   var(W),
   get_attr(W, clpb, sat_ref(I,G)), !,
   sat_union(F, G, E),
   put_attr(W, clpb, sat_ref(I,E)),
   sat_unify(G, K, node3(I,[I],one,zero)).
attr_unify_hook(sat_ref(K,F), W) :-
   var(W), !,
   var_map_new(W, I),
   put_attr(W, clpb, sat_ref(I,F)),
   sat_unify(F, K, node3(I,[I],one,zero)).
attr_unify_hook(sat_ref(K,F), 0) :- !,
   sat_unify(F, K, zero).
attr_unify_hook(sat_ref(K,F), 1) :- !,
   sat_unify(F, K, one).
attr_unify_hook(sat_ref(_,_), W) :-
   throw(error(type_error(sat_value,W),_)).

/**
 * sat_union(F, G, R):
 * The predicate succeeds in R with the union of the roots F and G.
 */
:- private sat_union/3.
sat_union([X|F], G, E) :-
   sat_contains(X, G), !,
   sat_union(F, G, E).
sat_union([X|F], G, [X|E]) :-
   sat_union(F, G, E).
sat_union([], G, G).

/**
 * sat_contains(X, G):
 * The predicate if G contains the root X.
 */
:- private sat_contains/2.
sat_contains(X, [Y|_]) :-
   X == Y, !.
sat_contains(X, [_|G]) :-
   sat_contains(X, G).

/**
 * set_unify(F, K, W):
 * The predicate is called when a reference list
 * F got the constant value W assigned.
 */
% sat_unify(+List, +Index, +Tree)
:- private sat_unify/3.
sat_unify([H|F], K, W) :-
   get_attr(H, clpb, sat_root(T)), !,
   sat_assign(T, K, W, S),
   sat_trivial(S, H),
   sat_unify(F, K, W),
   sat_propagate(S).
sat_unify([_|F], K, W) :-
   sat_unify(F, K, W).
sat_unify([], _, _).

/**
 * sat_assign(T, U, W, S):
 * The predicate assigns the tree W to U in the root H.
 */
% sat_assign(+Tree, +Index, +Tree, -Tree)
:- private sat_assign/4.
sat_assign(T, U, one, S) :- !,
   tree_one(T, U, S).
sat_assign(T, U, zero, S) :- !,
   tree_zero(T, U, S).
sat_assign(T, U, W, S) :-
   tree_equiv(node3(U,[U],one,zero), W, P),
   tree_and(T, P, Q),
   tree_exists(Q, U, S).

/*****************************************************************/
/* Portraying the Attributes                                     */
/*****************************************************************/

/**
 * attribute_goals(V, I, O):
 * The predicate is called when the goal list I of
 * the variable V is required. The list should end in O.
 */
% attribute_goals(+Variable, -List, +List)
:- public attribute_goals/3.
:- override attribute_goals/3.
attribute_goals(A, R, S) :-
   get_attr(A, clpb, sat_ref(K,F)),
   sat_goals(F, K, R, S).

/**
 * sat_goals(F, K, I, O):
 * The predicate succeeds with the goal list I
 * of the reference list F. This list should end in O.
 */
% sat_goals(+List, +Index, -List, +List)
:- private sat_goals/4.
sat_goals([H|F], K, [sat(E)|R], S) :-
   get_attr(H, clpb, sat_root(node3(K,W,C,D))), !,
   expr_pretty(node3(K,W,C,D), E),
   sat_goals(F, K, R, S).
sat_goals([_|F], K, R, S) :-
   sat_goals(F, K, R, S).
sat_goals([], _, R, R).

/*****************************************************************/
/* Labeling & Counting                                           */
/*****************************************************************/

/**
 * labeling(L):
 * The predicate labels the variables in L.
 */
% labeling(+List)
:- public labeling/1.
labeling(L) :-
   sys_plan_list(L, R),
   sys_labeling(R).

% sys_labeling(+List)
:- private sys_labeling/1.
sys_labeling([B|L]) :-
   sys_sat_value(B),
   sys_labeling(L).
sys_labeling([]).

/**
 * random_labeling(L):
 * The predicate randomly labels the variables in L.
 */
% random_labeling(+List)
:- public random_labeling/1.
random_labeling(L) :-
   sys_plan_list(L, R),
   sys_random_labeling(R).

% sys_random_labeling(+List)
:- private sys_random_labeling/1.
sys_random_labeling([B|L]) :-
   sys_random_sat_value(B),
   sys_random_labeling(L).
sys_random_labeling([]).

/**
 * sat_count(L, N):
 * The predicate silently labels the variables in L and
 * succeeds in N with the count of the solutions.
 */
% sat_count(+List, -Integer)
:- public sat_count/2.
sat_count(L, N) :-
   sys_plan_list(L, R),
   sys_sat_count(R, N).

% sys_sat_count(+List, -Integer)
:- private sys_sat_count/2.
sys_sat_count([B|L], N) :-
   findall(M, (  sys_sat_value(B),
                 sys_sat_count(L, M)), R),
   sys_sat_sum(R, N).
sys_sat_count([], 1).

/**
 * sys_sat_value(B):
 * The predicate succeeds in B with a sat value.
 */
% sys_sat_value(-Boolean)
:- private sys_sat_value/1.
sys_sat_value(0).
sys_sat_value(1).

/**
 * sys_random_sat_value(B):
 * The predicate succeeds in B with a random sat value.
 */
% sys_random_sat_value(-Boolean)
sys_random_sat_value(B) :-
   var(B), !,
   findall(C, sys_sat_value(C), L),
   random_permutation(L, R),
   member(B, R).
sys_random_sat_value(B) :-
   sys_sat_value(B).

/**
 * sys_sat_sum(L, N):
 * The predicate succeeds in N with the some of L.
 */
% sys_sat_sum(+List, -Integer)
:- private sys_sat_sum/2.
sys_sat_sum([M|L], N) :- !,
   sys_sat_sum(L, H),
   N is M+H.
sys_sat_sum([], 0).

/*****************************************************************/
/* Plan List                                                     */
/*****************************************************************/

% sys_plan_list(+List, -List)
:- private sys_plan_list/2.
sys_plan_list(L, R) :-
   sys_expr_list(L, H),
   sys_vars_list(H, J),
   sys_map_list(J, R).

% sys_expr_list(+List, -List)
:- private sys_expr_list/2.
sys_expr_list(L, _) :-
   var(L),
   throw(error(instantiation_error,_)).
sys_expr_list([A|L], [T|R]) :- !,
   expr_tree(A, T),
   sys_expr_list(L, R).
sys_expr_list([], []) :- !.
sys_expr_list(L, _) :-
   throw(error(type_error(list,L),_)).

% sys_vars_list(+List, -List)
:- private sys_vars_list/2.
sys_vars_list([A|L], W) :-
   expr_vars(A, U),
   sys_vars_list(L, V),
   vars_union(U, V, W).
sys_vars_list([], []).

% sys_map_list(+List, -List)
:- private sys_map_list/2.
sys_map_list([A|L], [B|R]) :-
   sys_melt_var(A, B),
   sys_map_list(L, R).
sys_map_list([], []).

/*****************************************************************/
/* Cardinality Constraint                                        */
/*****************************************************************/

/**
 * card(N, L):
 * If N is an integer and L is an expression list then the constraint
 * that the number of true expressions amounts exactly to N is posted.
 */
% card(+Integer, +List)
:- public card/2.
card(N, _) :-
   var(N),
   throw(error(instantiation_error,_)).
card(N, _) :-
   \+ integer(N),
   throw(error(type_error(integer,N),_)).
card(N, _) :-
   N < 0, !, fail.
card(N, L) :-
   length(L, M),
   M < N, !, fail.
card(N, L) :-
   sys_expr_list(L, H),
   sys_exactly(H, N, N, [S]),
   sat_post(S).

% sys_exactly(+List, +Integer, +Integer, -List)
:- private sys_exactly/4.
sys_exactly([X|L], N, 0, R) :- !,
   sys_exactly(L, N, 0, S),
   sys_exactly_same(S, X, R).
sys_exactly([X|L], N, M, R) :-
   H is M-1,
   sys_exactly(L, N, H, S),
   sys_exactly_less(S, X, R).
sys_exactly([], N, M, L) :-
   sys_exactly_base(N, M, L).

% sys_exactly_same(+List, +Term, -List)
:- private sys_exactly_same/3.
sys_exactly_same([A,B|L], Z, [C|R]) :- !,
   tree_ite(B, A, Z, C),
   sys_exactly_same([B|L], Z, R).
sys_exactly_same([A], Z, [B]) :-
   tree_ite(zero, A, Z, B).

% sys_exactly_less(+List, +Term, -List)
:- private sys_exactly_less/3.
sys_exactly_less([A,B|L], Z, [C|R]) :- !,
   tree_ite(B, A, Z, C),
   sys_exactly_less([B|L], Z, R).
sys_exactly_less([_], _, []).

% sys_exactly_base(+Integer, +Integer, -List)
:- private sys_exactly_base/3.
sys_exactly_base(0, _, [X]) :- !,
   X = one.
sys_exactly_base(N, M, [X|L]) :-
   H is N-1,
   X = zero,
   sys_exactly_base(H, M, L).
