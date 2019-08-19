/**
 * The SAT solver allows denoting Boolean value expressions. These
 * expressions can contain native Prolog variables. Boolean expressions
 * constraints are posted to the SAT solver via the predicate sat/1.
 * Internally the SAT constraint is normalized into a BDD tree. The
 * resulting BDD tree is automatically shown by the top-level:
 *
 * Example:
 * ?- sat(X#Y).
 * sat((X->(Y->0;1);Y->1;0))
 *
 * BDD tree reductions and attribute variable hooks guard the interaction
 * between SAT constraints. Interval arithmetic is used in reducing
 * pseudo Boolean constraints. Currently the following inference rules
 * have been implemented for Boolean expressions constraints and
 * pseudo Boolean constraints:
 *
 * * Unit Propagation
 * * Alias Propagation
 *
 * Unit propagation and alias propagation provide forward checking since
 * they might lead to fail-ure of subsequent constraints. Where permitted
 * Boolean expression constraints are replaced by native Prolog
 * unification =/2, thus leading to further forward checking. It is also
 * allowed mixing native Prolog unification =/2 with constraints.
 *
 * Examples:
 * ?- X=Y, sat(X+Y).
 * X = 1,
 * Y = 1
 * ?- sat(X+Y), X=Y.
 * X = 1,
 * Y = 1
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
 * Implementation note for pseudo boolean constraint solving. A
 * verify hook is used, since no new unification result from
 * forward checking only success or failure.
 *
 * - Each Boolean variable has an attribute of the form:
 *     tree:watch_ref(+Map)
 * where Map is a map from fresh and shared variables representing
 * a watcher to the variable weight. The Map gets updated
 * during model setup and during forward checking.
 *
 *- Each variable representing a watcher has an attribute of the form:
 *     tree:watch_root(+Interval, +Vars, +Comparator, +Value)
 * Where Interval is the estimated interval, where Vars are the
 * variables and where Comparator is the comparator and where
 * Value is the compared value.
 * The Maximum and the Value gets updated during forward checking.
 */
:- package(library(jekmin/reference/finite)).
:- module(tree, []).

:- use_module(library(basic/lists)).
:- use_module(library(term/unify)).
:- use_module(library(experiment/trail)).
:- use_module(library(minimal/assume)).
:- use_module(library(experiment/ref)).
:- use_module(library(experiment/attr)).
:- use_module(library(advanced/ordsets)).
:- use_module(library(experiment/ordmaps)).
:- use_module(library(experiment/maps)).
:- use_module(library(term/verify)).

:- public infix(#).
:- op(500, yfx, #).

:- public prefix(~).
:- op(300, fy, ~).

/********************************************************/
/* Expressions                                          */
/********************************************************/

/**
 * expr_tree(E, T):
 * The predicate succeeds in T with the tree of the expression E.
 */
% expr_tree(+Expr, -Tree)
/**
 * V (SAT):
 * A native Prolog variable V represents a Boolean variable.
 */
expr_tree(X, R) :- var(X), !,
   var_map_new(X, Y),
   R = node(Y, [Y], one, zero).
/**
 * 0 (SAT):
 * 1 (SAT):
 * The constant 0 or 1 represents a Boolean constant.
 */
expr_tree(0, R) :- !,
   R = zero.
expr_tree(1, R) :- !,
   R = one.
/**
 * ~ A (SAT):
 * If A is an expression then the negation ~A is also an expression.
 */
expr_tree(~A, R) :- !,
   expr_tree(A, P),
   tree_not(P, R).
/**
 * A + B (SAT):
 * If A and B are expressions then the disjunction A+B is also an expression.
 */
expr_tree(A+B, R) :- !,
   expr_tree(A, P),
   expr_tree(B, Q),
   tree_or(P, Q, R).
/**
 * A * B (SAT):
 * If A and B are expressions then the conjunction A*B is also an expression.
 */
expr_tree(A*B, R) :- !,
   expr_tree(A, P),
   expr_tree(B, Q),
   tree_and(P, Q, R).
/**
 * A =< B (SAT):
 * If A and B are expressions then the implication A=<B is also an expression.
  */
expr_tree(A =< B, R) :- !,
   expr_tree(A, P),
   expr_tree(B, Q),
   tree_imply(P, Q, R).
/**
 * A >= B (SAT):
 * If A and B are expressions then the implication B=<A is also an expression.
 */
expr_tree(A >= B, R) :- !,
   expr_tree(A, P),
   expr_tree(B, Q),
   tree_imply(Q, P, R).
/**
 * A > B (SAT):
 * If A and B are expressions then the difference A>B is also an expression.
 */
expr_tree(A > B, R) :- !,
   expr_tree(A, P),
   expr_tree(B, Q),
   tree_diff(P, Q, R).
/**
 * A < B (SAT):
 * If A and B are expressions then the difference B>A is also an expression.
 */
expr_tree(A < B, R) :- !,
   expr_tree(A, P),
   expr_tree(B, Q),
   tree_diff(Q, P, R).
/**
 * A =:= B (SAT):
 * If A and B are expressions then the equality A=:=B is also an expression.
 */
expr_tree(A =:= B, R) :- !,
   expr_tree(A, P),
   expr_tree(B, Q),
   tree_equiv(P, Q, R).
/**
 * A # B (SAT):
 * If A and B are expressions then the xor A#B is also an expression.
 */
expr_tree(A#B, R) :- !,
   expr_tree(A, P),
   expr_tree(B, Q),
   tree_xor(P, Q, R).
/**
 * A -> B; C (SAT):
 * If A, B and C are expressions then the if-then-else A->B;C is also an expression.
 */
expr_tree((A -> B; C), S) :- !,
   expr_tree(A, P),
   expr_tree(B, Q),
   expr_tree(C, R),
   tree_ite(Q, R, P, S).
/**
 * V^A (SAT):
 * If V is a native Prolog variable and A is an expression then the Boolean
 * existential quantification V^A is also an expression.
 */
expr_tree(X^A, R) :- !,
   var_map_new(X, Y),
   expr_tree(A, P),
   tree_exists(P, Y, R).
/**
 * card(P, L):
 * If P is a integer/integer-integer list and L is an expression list then
 * the cardinality constraint card(P, L) is also an expression.
 */
expr_tree(card(P, L), R) :- !,
   sys_expr_list(L, H),
   tree_card(P, H, J),
   tree_list(J, zero, R).
expr_tree(E, _) :-
   throw(error(type_error(sat_expr, E), _)).

/**
 * expr_pretty(T, E):
 * The predicate succeeds in E with the expression of the tree T.
 */
% expr_pretty(+Tree, -Expr)
expr_pretty(zero, R) :- !,
   R = 0.
expr_pretty(one, R) :- !,
   R = 1.
expr_pretty(node(X, _, A, B), R) :-
   sys_melt_var(X, Y),
   expr_pretty(A, C),
   expr_pretty(B, D),
   R = (Y -> C; D).

/*****************************************************************/
/* Connectives                                                   */
/*****************************************************************/

/**
 * tree_not(P, Q):
 * The predicate succeeds in Q with the P complemented.
 */
% tree_not(+Tree, -Tree)
:- private tree_not/2.
tree_not(zero, R) :- !,
   R = one.
tree_not(one, R) :- !,
   R = zero.
tree_not(node(X, W, A, B), node(X, W, C, D)) :-
   tree_not(A, C),
   tree_not(B, D).

/**
 * tree_and(P, Q, R):
 * The predicate succeeds in R with the boolean and of P and Q.
 */
% tree_and(+Tree, +Tree, -Tree)
tree_and(zero, _, R) :- !,
   R = zero.
tree_and(_, zero, R) :- !,
   R = zero.
tree_and(one, A, R) :- !,
   R = A.
tree_and(A, one, R) :- !,
   R = A.
tree_and(node(X, _, A, B), node(Y, _, C, D), R) :- X == Y, !,
   tree_and(A, C, E),
   tree_and(B, D, F),
   tree_make(E, F, X, R).
tree_and(node(X, _, A, B), N, R) :- N = node(Y, _, _, _), X @< Y, !,
   tree_and(A, N, E),
   tree_and(B, N, F),
   tree_make(E, F, X, R).
tree_and(N, node(Y, _, C, D), R) :-
   tree_and(N, C, E),
   tree_and(N, D, F),
   tree_make(E, F, Y, R).

/**
 * tree_or(P, Q, R):
 * The predicate succeeds in R with the boolean or of P and Q.
 */
% tree_or(+Tree, +Tree, -Tree)
:- private tree_or/3.
tree_or(one, _, R) :- !,
   R = one.
tree_or(_, one, R) :- !,
   R = one.
tree_or(zero, A, R) :- !,
   R = A.
tree_or(A, zero, R) :- !,
   R = A.
tree_or(node(X, _, A, B), node(Y, _, C, D), R) :- X == Y, !,
   tree_or(A, C, E),
   tree_or(B, D, F),
   tree_make(E, F, X, R).
tree_or(node(X, _, A, B), N, R) :- N = node(Y, _, _, _), X @< Y, !,
   tree_or(A, N, E),
   tree_or(B, N, F),
   tree_make(E, F, X, R).
tree_or(N, node(Y, _, C, D), R) :-
   tree_or(N, C, E),
   tree_or(N, D, F),
   tree_make(E, F, Y, R).

/**
 * tree_imply(P, Q, R):
 * The predicate succeeds in R with the boolean conditional of P and Q.
 */
% tree_imply(+Tree, +Tree, -Tree)
:- private tree_imply/3.
tree_imply(zero, _, R) :- !,
   R = one.
tree_imply(A, zero, R) :- !,
   tree_not(A, R).
tree_imply(one, A, R) :- !,
   R = A.
tree_imply(_, one, R) :- !,
   R = one.
tree_imply(node(X, _, A, B), node(Y, _, C, D), R) :- X == Y, !,
   tree_imply(A, C, E),
   tree_imply(B, D, F),
   tree_make(E, F, X, R).
tree_imply(node(X, _, A, B), N, R) :- N = node(Y, _, _, _), X @< Y, !,
   tree_imply(A, N, E),
   tree_imply(B, N, F),
   tree_make(E, F, X, R).
tree_imply(N, node(Y, _, C, D), R) :-
   tree_imply(N, C, E),
   tree_imply(N, D, F),
   tree_make(E, F, Y, R).

/**
 * tree_equiv(P, Q, R):
 * The predicate succeeds in R with the boolean bi-conditional of P and Q.
 */
% tree_equiv(+Tree, +Tree, -Tree)
tree_equiv(one, A, R) :- !,
   R = A.
tree_equiv(A, one, R) :- !,
   R = A.
tree_equiv(zero, A, R) :- !,
   tree_not(A, R).
tree_equiv(A, zero, R) :- !,
   tree_not(A, R).
tree_equiv(node(X, _, A, B), node(Y, _, C, D), R) :- X == Y, !,
   tree_equiv(A, C, E),
   tree_equiv(B, D, F),
   tree_make(E, F, X, R).
tree_equiv(node(X, _, A, B), N, R) :- N = node(Y, _, _, _), X @< Y, !,
   tree_equiv(A, N, E),
   tree_equiv(B, N, F),
   tree_make(E, F, X, R).
tree_equiv(N, node(Y, _, C, D), R) :-
   tree_equiv(N, C, E),
   tree_equiv(N, D, F),
   tree_make(E, F, Y, R).

/**
 * tree_xor(P, Q, R):
 * The predicate succeeds in R with the boolean difference of P and Q.
 */
:- private tree_diff/3.
tree_diff(P, Q, R) :-
   tree_imply(P, Q, A),
   tree_not(A, R).

/**
 * tree_xor(P, Q, R):
 * The predicate succeeds in R with the boolean xor of P and Q.
 */
:- private tree_xor/3.
tree_xor(P, Q, R) :-
   tree_equiv(P, Q, A),
   tree_not(A, R).

/**
 * tree_ite(Q, R, P, S):
 * The predicate succeeds in S with the boolean if-then-else of P, Q and R.
 */
% tree_ite(+Tree, +Tree, +Tree, -Tree)
tree_ite(Q, R, P, S) :-
   tree_imply(P, Q, A),
   tree_or(P, R, B),
   tree_and(A, B, S).

/*****************************************************************/
/* Quantifiers                                                   */
/*****************************************************************/

/**
 * tree_exists(Q, P, R):
 * The predicate succeeds in R with existential removing the variable P from Q.
 */
% tree_exists(+Tree, +Index, -Tree)
tree_exists(zero, _, R) :- !,
   R = zero.
tree_exists(one, _, R) :- !,
   R = one.
tree_exists(node(X, _, A, B), Y, R) :- X == Y, !,
   tree_or(A, B, R).
tree_exists(node(Y, _, A, B), X, R) :- Y @< X, !,
   tree_exists(A, X, C),
   tree_exists(B, X, D),
   tree_make(C, D, Y, R).
tree_exists(N, _, N).

/**
 * tree_one(Q, P, R):
 * The predicate succeeds in R with substituting 1 for the variable P in Q.
 */
% tree_one(+Tree, +Index,-Tree)
tree_one(zero, _, R) :- !,
   R = zero.
tree_one(one, _, R) :- !,
   R = one.
tree_one(node(X, _, A, _), Y, R) :- X == Y, !,
   R = A.
tree_one(node(Y, _, A, B), X, R) :- Y @< X, !,
   tree_one(A, X, C),
   tree_one(B, X, D),
   tree_make(C, D, Y, R).
tree_one(N, _, N).

/**
 * tree_zero(Q, P, R):
 * The predicate succeeds in R with substituting 0 for the variable P in Q.
 */
% tree_zero(+Tree, +Index, -Tree)
tree_zero(zero, _, R) :- !,
   R = zero.
tree_zero(one, _, R) :- !,
   R = one.
tree_zero(node(X, _, _, B), Y, R) :- X == Y, !,
   R = B.
tree_zero(node(Y, _, A, B), X, R) :- Y @< X, !,
   tree_zero(A, X, C),
   tree_zero(B, X, D),
   tree_make(C, D, Y, R).
tree_zero(N, _, N).

/*****************************************************************/
/* Tree Make                                                     */
/*****************************************************************/

/**
 * tree_make(A, B, X, R):
 * The predicate succeeds in R with a new tree (X->A;B).
 */
% tree_make(+Tree, +Tree, +Index, -Tree)
:- private tree_make/4.
tree_make(A, A, _, R) :- !,
   R = A.
tree_make(A, B, X, node(X, [X|W], A, B)) :-
   expr_vars(A, U),
   expr_vars(B, V),
   ord_union(U, V, W).

/**
 * expr_vars(T, L):
 * The predicate succeeds in L with the variable
 * indexes in the tree T.
 */
% expr_vars(+Tree, -List)
:- public expr_vars/2.
expr_vars(zero, R) :- !,
   R = [].
expr_vars(one, R) :- !,
   R = [].
expr_vars(node(_, W, _, _), W).

/*****************************************************************/
/* List Arguments                                                */
/*****************************************************************/

/**
 * tree(L, S, T):
 * The predicate succeeds in T with the disjunction of
 * the tree S with the trees of the list L.
 */
% tree_list(+List, +Tree, -Tree)
:- private tree_list/3.
tree_list([A|L], S, T) :-
   tree_or(S, A, H),
   tree_list(L, H, T).
tree_list([], T, T).

/**
 * sys_expr_list(L, R).
 * The predicate succeeds in R with the list of trees
 * for the list of expressions L.
 */
% sys_expr_list(+List, -List)
:- private sys_expr_list/2.
sys_expr_list(L, _) :- var(L),
   throw(error(instantiation_error, _)).
sys_expr_list([A|L], [T|R]) :- !,
   expr_tree(A, T),
   sys_expr_list(L, R).
sys_expr_list([], []) :- !.
sys_expr_list(L, _) :-
   throw(error(type_error(list, L), _)).

/*****************************************************************/
/* Cardinality Constraint                                        */
/*****************************************************************/

% tree_card(+List, +List, -List)
:- private tree_card/3.
tree_card(P, _, _) :- var(P),
   throw(error(instantiation_error, _)).
tree_card([N-M|P], L, [A|B]) :- !,
   H is max(0, N),
   length(L, K),
   J is min(K, M),
   tree_range(H, J, L, A),
   tree_card(P, L, B).
tree_card([N|P], L, [A|B]) :- !,
   tree_point(N, L, A),
   tree_card(P, L, B).
tree_card([], _, []) :- !.
tree_card(P, _, _) :-
   throw(error(type_error(list, P), _)).

% tree_point(+Integer, +List, -Tree)
:- private tree_point/3.
tree_point(N, _, X) :-
   N < 0, !, X = zero.
tree_point(N, L, X) :-
   length(L, M), M < N, !, X = zero.
tree_point(N, L, S) :-
   sys_exactly(L, N, N, [S]).

% tree_range(+Integer, +Integer, +List, -Tree)
:- private tree_range/4.
tree_range(N, M, _, X) :-
   M < N, !, X = zero.
tree_range(N, M, L, S) :-
   sys_exactly(L, M, N, H),
   tree_list(H, zero, S).

% sys_exactly(+List, +Integer, +Integer, -List)
:- private sys_exactly/4.
sys_exactly([X|L], N, 0, R) :- !,
   sys_exactly(L, N, 0, S),
   sys_exactly_same(S, X, R).
sys_exactly([X|L], N, M, R) :-
   H is M-1,
   sys_exactly(L, N, H, S),
   sys_exactly_less(S, X, R).
sys_exactly([], N, _, L) :-
   sys_exactly_base(N, L).

% sys_exactly_same(+List, +Term, -List)
:- private sys_exactly_same/3.
sys_exactly_same([A, B|L], Z, [C|R]) :- !,
   tree_ite(B, A, Z, C),
   sys_exactly_same([B|L], Z, R).
sys_exactly_same([A], Z, [B]) :-
   tree_ite(zero, A, Z, B).

% sys_exactly_less(+List, +Term, -List)
:- private sys_exactly_less/3.
sys_exactly_less([A, B|L], Z, [C|R]) :- !,
   tree_ite(B, A, Z, C),
   sys_exactly_less([B|L], Z, R).
sys_exactly_less([_], _, []).

% sys_exactly_base(+Integer, -List)
:- private sys_exactly_base/2.
sys_exactly_base(0, [X]) :- !,
   X = one.
sys_exactly_base(N, [X|L]) :-
   H is N-1,
   X = zero,
   sys_exactly_base(H, L).

/*****************************************************************/
/* Pseudo Booleans                                               */
/*****************************************************************/

/**
 * watch_add_vars(R, L, H, S, T, P, Q):
 * The predicate succeeds in adding watchers to the fresh
 * and shared variable H, and returns in T the number S
 * plus the partial weighted sum, and returns in Q the
 * interval P plus the estimated interval.
 */
% watch_add_vars(+List, +List, +Var, +Number, -Number, +Interval, -Interval)
watch_add_vars(_, L, _, _, _, _, _) :- var(L),
   throw(error(instantiation_error, _)).
watch_add_vars([V|R], [B|L], H, S, T, P, Q) :- var(B),
   get_atts(B, tree, watch_ref(F)), !,
   map_include(V, H, F, G, P, J),
   put_atts(B, tree, watch_ref(G)),
   watch_add_vars(R, L, H, S, T, J, Q).
watch_add_vars([V|R], [B|L], H, S, T, P, Q) :- var(B), !,
   put_atts(B, tree, watch_ref([H-V])),
   interval_addition(P, V, J),
   watch_add_vars(R, L, H, S, T, J, Q).
watch_add_vars([V|R], [B|L], H, S, T, P, Q) :- !,
   expr_value(B),
   J is S+B*V,
   watch_add_vars(R, L, H, J, T, P, Q).
watch_add_vars([], [], _, S, S, P, P) :- !.
watch_add_vars(_, L, _, _, _, _, _) :-
   throw(error(type_error(list, L), _)).

% map_include(+Number, +Var, +Map, -Map, +Number, -Number)
:- private map_include/6.
map_include(V, H, F, G, S, T) :- get(F, H, W), !,
   J is V+W,
   interval_subtract(S, W, K),
   interval_addition(K, J, T),
   put(F, H, J, G).
map_include(V, H, F, [H-V|F], S, T) :-
   interval_addition(S, V, T).

% expr_value(+Boolean)
expr_value(0).
expr_value(1).

% expr_value_reverse(+Boolean)
expr_value_reverse(1).
expr_value_reverse(0).

/*****************************************************************/
/* Root Addition                                                 */
/*****************************************************************/

% watch_trivial(+Vars, +Interval, +List, +Comparator, +Number)
watch_trivial(H, J, _, C, U) :-
   watch_success(C, J, U), !, del_atts(H, tree).
watch_trivial(_, J, _, C, U) :-
   watch_failure(C, J, U), !, fail.
watch_trivial(H, J, L, C, U) :-
   put_atts(H, tree, watch_root(J, L, C, U)).

% watch_success((+Comparator, +Interval, +Number)
:- private watch_success/3.
watch_success(>, F-_, U) :- !, F > U.
watch_success(>=, F-_, U) :- !, F >= U.
watch_success(<, _-T, U) :- !, T < U.
watch_success(=<, _-T, U) :- !, T =< U.
watch_success(=:=, F-T, U) :- !, T =< U, F >= U.
watch_success(=\=, _-T, U) :- T < U, !.
watch_success(=\=, F-_, U) :- !, F > U.
watch_success(C, _, _) :-
   throw(error(type_error(comparator, C), _)).

% watch_failure(+Comparator, +Interval, +Number)
:- private watch_failure/3.
watch_failure(>, _-T, U) :- !, T =< U.
watch_failure(>=, _-T, U) :- !, T < U.
watch_failure(<, F-_, U) :- !, F >= U.
watch_failure(=<, F-_, U) :- !, F > U.
watch_failure(=:=, _-T, U) :- T < U, !.
watch_failure(=:=, F-_, U) :- !, F > U.
watch_failure(=\=, F-T, U) :- !, T =< U, F >= U.
watch_failure(C, _, _) :-
   throw(error(type_error(comparator, C), _)).

/*****************************************************************/
/* Verify Hook                                                   */
/*****************************************************************/

/**
 * verify_attributes(A, W):
 * The predicate is called when a varable with attribute
 * term A got the value W assigned.
 */
% verify_attributes(+Attr, +Term)
:- public verify_attributes/2.
:- override verify_attributes/2.
verify_attributes(watch_ref(F), W) :- var(W),
   get_atts(W, tree, watch_ref(G)), !,
   map_union(F, G, E),
   put_atts(W, tree, watch_ref(E)).
verify_attributes(watch_ref(F), W) :- var(W), !,
   put_atts(W, tree, watch_ref(F)).
verify_attributes(watch_ref(F), 0) :- !,
   watch_unify(F, 0).
verify_attributes(watch_ref(F), 1) :- !,
   watch_unify(F, 1).
verify_attributes(watch_ref(_, _), W) :-
   throw(error(type_error(sat_value, W), _)).

% watch_unify(+Map, +Boolean)
:- private watch_unify/2.
watch_unify([H-V|F], B) :-
   get_atts(H, tree, watch_root(J, Z, C, U)), !,
   W is U-B*V,
   interval_subtract(J, V, K),
   watch_trivial(H, K, Z, C, W),
   watch_unify(F, B).
watch_unify([_|F], B) :-
   watch_unify(F, B).
watch_unify([], _).

% map_union(+Map, +Map, -Map)
:- private map_union/3.
map_union([H-V|L], M, R) :- get(M, H, W), !,
   S is V+W,
   watch_update(H, V, W, S),
   put(M, H, S, N),
   map_union(L, N, R).
map_union([H-V|L], M, [H-V|R]) :-
   map_union(L, M, R).
map_union([], M, M).

% watch_update(+Var, +Number, +Number, +Number)
:- private watch_update/4.
watch_update(H, V, W, S) :-
   get_atts(H, tree, watch_root(J, Z, C, U)), !,
   interval_subtract(J, V, P),
   interval_subtract(P, W, Q),
   interval_addition(Q, S, K),
   watch_trivial(H, K, Z, C, U).
watch_update(_, _, _, _).

% interval_subtract(+Interval, +Number, -Interval)
:- private interval_subtract/3.
interval_subtract(F-T, V, U-T) :- V =< 0, !, U is F-V.
interval_subtract(F-T, V, F-U) :- U is T-V.

% interval_addition(+Interval, +Number, -Interval)
:- private interval_addition/3.
interval_addition(F-T, V, U-T) :- V =< 0, !, U is F+V.
interval_addition(F-T, V, F-U) :- U is T+V.

/*****************************************************************/
/* Portraying the Attributes                                     */
/*****************************************************************/

/**
 * portray_attributes(V, I, O):
 * The predicate is called when the goal list I of
 * the variable V is required. The list should end in O.
 */
% portray_attributes(+Variable, -List, +List)
:- public portray_attributes/3.
:- override portray_attributes/3.
portray_attributes(A, S, S) :-
   get_atts(A, tree, watch_ref(_)).
portray_attributes(A, [pseudo(R, L, C, U)|S], S) :-
   get_atts(A, tree, watch_root(_, Z, C, U)),
   term_variables(Z, L), L \== [], !,
   watch_get_weights(L, A, R).
portray_attributes(A, S, S) :-
   get_atts(A, tree, watch_root(_, _, _, _)).

% watch_get_weights(+List, +Var, -List)
:- private watch_get_weights/3.
watch_get_weights([B|L], H, [V|R]) :-
   get_atts(B, tree, watch_ref(M)),
   get(M, H, V),
   watch_get_weights(L, H, R).
watch_get_weights([], _, []).

/*****************************************************************/
/* Variable Lookup                                               */
/*****************************************************************/

/**
 * var_map_new(X, Y):
 * The predicate succeeds in Y with new mapping of X.
 */
% var_map_new(+Variable, -Index)
var_map_new(X, R) :- sys_clause_hook(X, sys_hook_var(Y), _), !,
   R = Y.
var_map_new(X, R) :-
   sys_ensure_serno(X),
   sys_freeze_var(X, Y),
   sys_compile_hook(X, sys_hook_var(Y), K),
   depositz_ref(K),
   R = Y.

/**
 * sys_hook_var(R, V, T):
 */
% sys_hook_var(+Ref, +Var, +Term)
:- private sys_hook_var/3.
sys_hook_var(_, _, _).
