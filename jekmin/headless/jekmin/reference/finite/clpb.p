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

/**
 * Implementation note for BDD constraint solving. A unify hook is
 * used, so that during forward checking new unifications can be
 * posted leading to new forward checking:
 *
 * - Each Boolean variable has an attribute of the form:
 *     clpb:bdd_ref(+Ref, +List)
 * where Ref is the frozen variable and List is a list of fresh
 * and shared variables representing BDDs. The List gets updated
 * during model setup and during forward checking.
 *
 * - Each variable representing a BDD has an attribute of the form:
 *     clpb:bdd_root(+BDD)
 * where BDD is the binary decision diagram. The BDD gets
 * updated during forward checking.
 */
:- package(library(jekmin/reference/finite)).
:- module(clpb, []).

:- reexport(tree).
:- use_module(library(term/unify)).
:- use_module(library(basic/lists)).
:- use_module(library(basic/random)).
:- use_module(library(experiment/trail)).
:- use_module(library(advanced/sets)).

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
   bdd_add_vars(L, H),
   sat_trivial(T, H),
   sat_propagate(T).

/**
 * bdd_add_vars(L, H):
 * The predicate succeeds in adding references from the variables
 * in the list L to the fresh and shared variable H.
 */
% bdd_add_vars(+List, +Var)
:- private bdd_add_vars/2.
bdd_add_vars([K|L], H) :-
   sys_melt_var(K, A),
   bdd_add_var(A, K, H),
   bdd_add_vars(L, H).
bdd_add_vars([], _).

% bdd_add_var(+Var, +Ref, +Var)
:- private bdd_add_var/3.
bdd_add_var(A, K, H) :-
   get_attr(A, clpb, bdd_ref(K,F)), !,
   put_attr(A, clpb, bdd_ref(K,[H|F])).
bdd_add_var(A, K, H) :-
   put_attr(A, clpb, bdd_ref(K,[H])).

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
   put_attr(H, clpb, bdd_root(S)).

/**
 * sat_propagate(S):
 * The predicate propagates an existing sat constraint S.
 */
% sat_propagate(+Tree)
:- private sat_propagate/1.
sat_propagate(node(X,_,_,zero)) :- !,
   sys_melt_var(X, Y),
   Y = 1.
sat_propagate(node(X,_,zero,_)) :- !,
   sys_melt_var(X, Y),
   Y = 0.
sat_propagate(node(X,_,node(Y,_,A,zero),node(Y,_,zero,A))) :- !,
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
attr_unify_hook(bdd_ref(K,F), W) :-
   var(W),
   get_attr(W, clpb, bdd_ref(I,G)), !,
   union(F, G, E),
   put_attr(W, clpb, bdd_ref(I,E)),
   bdd_unify(G, K, node(I,[I],one,zero)).
attr_unify_hook(bdd_ref(K,F), W) :-
   var(W), !,
   var_map_new(W, I),
   put_attr(W, clpb, bdd_ref(I,F)),
   bdd_unify(F, K, node(I,[I],one,zero)).
attr_unify_hook(bdd_ref(K,F), 0) :- !,
   bdd_unify(F, K, zero).
attr_unify_hook(bdd_ref(K,F), 1) :- !,
   bdd_unify(F, K, one).
attr_unify_hook(bdd_ref(_,_), W) :-
   throw(error(type_error(sat_value,W),_)).

/**
 * set_unify(F, K, W):
 * The predicate is called when a reference list
 * F got the constant value W assigned.
 */
% bdd_unify(+List, +Ref, +Tree)
:- private bdd_unify/3.
bdd_unify([H|F], K, W) :-
   get_attr(H, clpb, bdd_root(T)), !,
   sat_assign(T, K, W, S),
   sat_trivial(S, H),
   bdd_unify(F, K, W),
   sat_propagate(S).
bdd_unify([_|F], K, W) :-
   bdd_unify(F, K, W).
bdd_unify([], _, _).

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
   tree_equiv(node(U,[U],one,zero), W, P),
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
attribute_goals(A, S, S) :-
   get_attr(A, clpb, bdd_ref(_,_)).
attribute_goals(A, [sat(E)|S], S) :-
   get_attr(A, clpb, bdd_root(T)),
   expr_pretty(T, E).

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
   expr_value(B),
   labeling(L).
labeling([]) :- !.
labeling(L) :-
   throw(error(type_error(list,L),_)).

/**
 * random_labeling(L):
 * The predicate randomly labels the variables in L.
 */
% random_labeling(+List)
:- public random_labeling/1.
random_labeling(L) :-
   var(L),
   throw(error(instantiation_error,_)).
random_labeling([B|L]) :- !,
   sys_random_sat_value(B),
   random_labeling(L).
random_labeling([]) :- !.
random_labeling(L) :-
   throw(error(type_error(list,L),_)).

/**
 * sys_random_sat_value(B):
 * The predicate succeeds in B with a random sat value.
 */
% sys_random_sat_value(-Boolean)
:- private sys_random_sat_value/1.
sys_random_sat_value(B) :-
   var(B), !,
   findall(C, expr_value(C), L),
   random_permutation(L, R),
   member(B, R).
sys_random_sat_value(B) :-
   expr_value(B).

/**
 * count(L, N):
 * The predicate silently labels the variables in L and
 * succeeds in N with the count of the solutions.
 */
% count(+List, -Integer)
:- public count/2.
count(L, _) :-
   var(L),
   throw(error(instantiation_error,_)).
count([B|L], N) :- !,
   findall(M, (  expr_value(B),
                 count(L, M)), R),
   sys_sat_sum(R, N).
count([], N) :- !,
   N = 1.
count(L, _) :-
   throw(error(type_error(list,L),_)).

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
/* Weighted Maximum                                              */
/*****************************************************************/

/**
 * weighted_maximum(W, L, O):
 * The predicate succeeds in O with the maximum of the weighted
 * sum from the values L and the weights W, and then succeeds
 * for all corresponding labelings of L.
 */
% weighted_maximum(+list, +List, -Number)
:- public weighted_maximum/3.
weighted_maximum(R, L, O) :-
   sys_find_start(R, L, K),
   term_variables(L, Z),
   watch_add_vars(R, L, H, 0, V, 0, J),
   sys_find_maximum(V, J, H, R, L, Z, K, O),
   U is O-V,
   pseudo_two(U, J, H, Z, >=),
   labeling(L).

% sys_find_start(+List, +List, -Number)
:- private sys_find_start/3.
sys_find_start(R, L, K) :-
   catch((  random_labeling(L),
            sys_stop_value(R, L, 0, K),
            throw(sys_start_bound(K))),
      sys_start_bound(K),
      true).

% sys_find_maximum(+Number, +Number, +Var, +List, +List, +Vars, +Number, -Number)
:- private sys_find_maximum/8.
sys_find_maximum(V, J, H, R, L, Z, K, O) :-
   catch((  U is K-V,
            pseudo_two(U, J, H, Z, >),
            random_labeling(L),
            sys_stop_value(R, L, 0, P),
            throw(sys_new_bound(P))),
      sys_new_bound(P),
      true), !,
   sys_find_maximum(V, J, H, R, L, Z, P, O).
sys_find_maximum(_, _, _, _, _, _, K, K).

/**
 * sys_stop_value(R, L, S, T):
 * The predicate succeeds in T with the scalar product
 * of the weight R and the values L plus the number S.
 */
% sys_stop_value(+List, +List, +Number, -Number)
:- private sys_stop_value/4.
sys_stop_value([V|R], [B|L], S, T) :-
   H is S+B*V,
   sys_stop_value(R, L, H, T).
sys_stop_value([], [], S, S).
