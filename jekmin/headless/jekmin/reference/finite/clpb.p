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

:- package(library(jekmin/reference/finite)).
:- module(clpb, []).

:- reexport(tree).
:- use_module(library(term/unify)).
:- use_module(library(basic/lists)).

/**
 * sat(A):
 * If A is an expression then its satisfiability is posted.
 */
% sat(+Expr)
:- public sat/1.
sat(A) :-
   expr_tree(A, T),
   expr_vars(T, L),
   sat_post(T, L).

/**
 * sat_post(T, L):
 * If T is a tree and L are its variables then its satisfiability is posted.
 */
% sat_post(+Tree, +List)
:- private sat_post/2.
sat_post(T, L) :-
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
   map_back(K, A),
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
sat_propagate(node(X,_,zero)) :- !,
   map_back(X, Y),
   Y = 1.
sat_propagate(node(X,zero,_)) :- !,
   map_back(X, Y),
   Y = 0.
sat_propagate(node(X,node(Y,A,zero),node(Y,zero,A))) :- !,
   map_back(X, Z),
   map_back(Y, T),
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
   sat_unify(G, K, node(I,one,zero)).
attr_unify_hook(sat_ref(K,F), W) :-
   var(W), !,
   map_new(W, I),
   put_attr(W, clpb, sat_ref(I,F)),
   sat_unify(F, K, node(I,one,zero)).
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
   tree_one(U, T, S).
sat_assign(T, U, zero, S) :- !,
   tree_zero(U, T, S).
sat_assign(T, U, W, S) :-
   tree_equiv(node(U,one,zero), W, P),
   tree_and(T, P, Q),
   tree_exists(U, Q, S).

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
   get_attr(H, clpb, sat_root(node(K,C,D))), !,
   expr_pretty(node(K,C,D), E),
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
   var(L),
   throw(error(instantiation_error,_)).
labeling([B|L]) :- !,
   sat_value(B),
   labeling(L).
labeling([]) :- !.
labeling(L) :-
   throw(error(type_error(list,L),_)).

/**
 * sat_value(B):
 * The predicate succeeds in B with a sat value.
 */
% sat_value(-Boolean)
:- private sat_value/1.
sat_value(0).
sat_value(1).

/**
 * sat_count(L, N):
 * The predicate silently labels the variables in L and
 * succeeds in N with the count of the solutions.
 */
% sat_count(+List, -Integer)
:- public sat_count/2.
sat_count(L, _) :-
   var(L),
   throw(error(instantiation_error,_)).
sat_count([B|L], N) :- !,
   findall(M, (  sat_value(B),
                 sat_count(L, M)), R),
   sat_sum(R, N).
sat_count([], N) :- !,
   N = 1.
sat_count(L, _) :-
   throw(error(type_error(list,L),_)).

/**
 * sat_sum(L, N):
 * The predicate succeeds in N with the some of L.
 */
% sat_sum(+List, -Integer)
:- private sat_sum/2.
sat_sum([M,O], N) :- !,
   N is M+O.
sat_sum([N], N).
sat_sum([], 0).

/*****************************************************************/
/* Cardinality Constraint                                        */
/*****************************************************************/

/**
 * card(N, L):
 * If N is an integer and L is a variable list then he constraint
 * that the number of true variables amounts exactly to N is posted.
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
   map_new_list(L, H),
   sort(H, R),
   exactly(R, N, N, [S]),
   sat_post(S, R).

% map_new_list(+List, -List)
:- private map_new_list/2.
map_new_list(L, _) :-
   var(L),
   throw(error(instantiation_error,_)).
map_new_list([X|L], [S|R]) :-
   var(X), !,
   map_new(X, S),
   map_new_list(L, R).
map_new_list([X|_], _) :-
   throw(error(type_error(var,X),_)).
map_new_list([], []) :- !.
map_new_list(L, _) :-
   throw(error(type_error(list,L),_)).

% exactly(+List, +Integer, +Integer, -List)
:- private exactly/4.
exactly([X|L], N, 0, R) :- !,
   exactly(L, N, 0, S),
   exactly_same(S, X, R).
exactly([X|L], N, M, R) :-
   H is M-1,
   exactly(L, N, H, S),
   exactly_less(S, X, R).
exactly([], N, M, L) :-
   exactly_base(N, M, L).

% exactly_same(+List, +Term, -List)
:- private exactly_same/3.
exactly_same([A,B|L], Z, [C|R]) :- !,
   C = node(Z,B,A),
   exactly_same([B|L], Z, R).
exactly_same([A], Z, [B]) :-
   B = node(Z,zero,A).

% exactly_less(+List, +Term, -List)
:- private exactly_less/3.
exactly_less([A,B|L], Z, [C|R]) :- !,
   C = node(Z,B,A),
   exactly_less([B|L], Z, R).
exactly_less([_], _, []).

% exactly_base(+Integer, +Integer, -List)
:- private exactly_base/3.
exactly_base(0, _, [X]) :- !,
   X = one.
exactly_base(N, M, [X|L]) :-
   H is N-1,
   X = zero,
   exactly_base(H, M, L).