/**
 * The SAT solver provides a couple solving technique in various
 * incarnations. We provide intelligent backtracking search when
 * there is an attempt to label multiple variables at once via
 * the predicate labeling/1 or to count the number of solutions
 * via the predicate count/2. For compati-bility, we also provide
 * a predicate sat_count/2.
 *
 * * Intelligent Backtracking Search
 *
 * Although variable rankings are found in the literature, we
 * did not implement some special search strategy, since we did
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
 * ?- sat(X=<Y), sat(Y=<Z), sat(Z=<X), count([X,Y,Z], N).
 * N = 2,
 * sat((X->1;Z->0;1)),
 * sat((X->(Y->1;0);1)),
 * sat((Y->(Z->1;0);1))
 *
 * The backtracking is intelligent in as far as the Prolog interpreter
 * eliminates choice points. The normalization into BDD can directly
 * detect an antology in that it reduces to 0. The predicate sat/1 will
 * then fail and cause backtracking. To detect an antology among
 * overlapping sat/1 constraints the predicate labeling/1 needs to be used.
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

:- package(library(jekmin/reference/boole)).
:- module(clpb, []).

:- reexport(tree).
:- reexport(pseudo).
:- use_module(library(term/unify)).
:- use_module(library(basic/lists)).
:- use_module(library(experiment/trail)).
:- use_module(library(advanced/sets)).
:- use_module(library(term/verify)).

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
   get_attr(A, clpb, bdd_ref(K, F)), !,
   put_attr(A, clpb, bdd_ref(K, [H|F])).
bdd_add_var(A, K, H) :-
   put_attr(A, clpb, bdd_ref(K, [H])).

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
sat_propagate(node(X, _, _, zero)) :- !,
   sys_melt_var(X, Y),
   Y = 1.
sat_propagate(node(X, _, zero, _)) :- !,
   sys_melt_var(X, Y),
   Y = 0.
sat_propagate(node(X, _, node(Y, _, A, zero), node(Y, _, zero, A))) :- !,
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
attr_unify_hook(bdd_ref(K, F), W) :- var(W),
   get_attr(W, clpb, bdd_ref(I, G)), !,
   union(F, G, E),
   put_attr(W, clpb, bdd_ref(I, E)),
   bdd_unify(G, K, node(I, [I], one, zero)).
attr_unify_hook(bdd_ref(K, F), W) :- var(W), !,
   var_map_new(W, I),
   put_attr(W, clpb, bdd_ref(I, F)),
   bdd_unify(F, K, node(I, [I], one, zero)).
attr_unify_hook(bdd_ref(K, F), 0) :- !,
   bdd_unify(F, K, zero).
attr_unify_hook(bdd_ref(K, F), 1) :- !,
   bdd_unify(F, K, one).
attr_unify_hook(bdd_ref(_, _), W) :-
   throw(error(type_error(sat_value, W), _)).

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
   tree_equiv(node(U, [U], one, zero), W, P),
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
attribute_goals(A, S, S) :-
   get_attr(A, clpb, bdd_ref(_, _)).
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
labeling(L) :- var(L),
   throw(error(instantiation_error, _)).
labeling([B|L]) :- !,
   expr_value(B),
   labeling(L).
labeling([]) :- !.
labeling(L) :-
   throw(error(type_error(list, L), _)).

/**
 * count(L, N):
 * The predicate silently labels the variables in L and
 * succeeds in N with the count of the solutions.
 */
% count(+List, -Integer)
:- public count/2.
count(L, _) :- var(L),
   throw(error(instantiation_error, _)).
count([B|L], N) :- !,
   findall(M, (expr_value(B), count(L, M)), R),
   sys_sat_sum(R, N).
count([], N) :- !,
   N = 1.
count(L, _) :-
   throw(error(type_error(list, L), _)).

/**
 * sat_count(E, N):
 * The predicate silently posts the expression E and silently labels
 * the variables in E and succeeds in N with the count of the solution.
 */
% sat_count(+Boolean, -Integer)
:- public sat_count/2.
sat_count(E, N) :-
   term_variables(E, L),
   sat(E), !,
   count(L, N).
sat_count(_, 0).

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
