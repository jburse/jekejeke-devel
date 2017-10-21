/**
 * The SAT solver allows denoting Boolean value expressions. These
 * expressions can contain native Prolog variables. Boolean expressions
 * are posted to the SAT solver via the predicate sat/1. Internally
 * the SAT constraint is normalized into a BDD tree. The resulting
 * BDD tree is automatically shown by the top-level:
 *
 * Example:
 * ?- sat(X#Y).
 * sat((X->(Y->0;1);Y->1;0))
 *
 * BDD tree reductions and attribute variable hooks guard the interaction
 * between SAT con-straints. Currently the following inference rule sets
 * have been implemented for Boolean value expressions:
 *
 * * Forward Checking
 *
 * The forward checking consists of the two inference rules constant
 * elimination and constant back propagation. Where permitted SAT
 * constraints are replaced by native Prolog unification. It is also
 * allowed mixing native Prolog unification =/2 with SAT constraints:
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- package(library(jekmin/reference/finite)).
:- module(tree, []).

:- use_module(library(basic/lists)).
:- use_module(library(term/state)).
:- use_module(library(term/unify)).

:- public infix(#).
:- op(500, yfx, #).

:- public prefix(~).
:- op(300, fy, ~).

/********************************************************/
/* Expressions                                          */
/********************************************************/

/**
 * expr_eval(E, T):
 * The predicate succeeds in T with the tree of the expression E.
 */
% expr_eval(+Expr, -Tree)

/**
 * V (SAT):
 * A native Prolog variable V represents a Boolean variable.
 */
expr_eval(X, R) :-
   var(X), !,
   map_new(X, Y),
   R = node(Y,one,zero).
/**
 * 0 (SAT):
 * 1 (SAT):
 * The constant 0 or 1 represents a Boolean constant.
 */
expr_eval(0, R) :- !,
   R = zero.
expr_eval(1, R) :- !,
   R = one.
/**
 * ~ A (SAT):
 * If A is an expression then the negation ~A is also an expression.
 */
expr_eval(~A, R) :- !,
   expr_eval(A, P),
   tree_not(P, R).
/**
 * A + B (SAT):
 * If A and B are expressions then the disjunction A+B is also an expression.
 */
expr_eval(A+B, R) :- !,
   expr_eval(A, P),
   expr_eval(B, Q),
   tree_or(P, Q, R).
/**
 * A * B (SAT):
 * If A and B are expressions then the conjunction A*B is also an expression.
 */
expr_eval(A*B, R) :- !,
   expr_eval(A, P),
   expr_eval(B, Q),
   tree_and(P, Q, R).
/**
 * A =< B (SAT):
 * If A and B are expressions then the implication A=<B is also an expression.
  */
expr_eval(A=<B, R) :- !,
   expr_eval(A, P),
   expr_eval(B, Q),
   tree_imply(P, Q, R).
/**
 * A >= B (SAT):
 * If A and B are expressions then the implication B=<A is also an expression.
 */
expr_eval(A>=B, R) :- !,
   expr_eval(A, P),
   expr_eval(B, Q),
   tree_imply(Q, P, R).
/**
 * A > B (SAT):
 * If A and B are expressions then the difference A>B is also an expression.
 */
expr_eval(A>B, R) :- !,
   expr_eval(A, P),
   expr_eval(B, Q),
   tree_diff(P, Q, R).
/**
 * A < B (SAT):
 * If A and B are expressions then the difference B>A is also an expression.
 */
expr_eval(A<B, R) :- !,
   expr_eval(A, P),
   expr_eval(B, Q),
   tree_diff(Q, P, R).
/**
 * A =:= B (SAT):
 * If A and B are expressions then the equality A=:=B is also an expression.
 */
expr_eval(A=:=B, R) :- !,
   expr_eval(A, P),
   expr_eval(B, Q),
   tree_equiv(P, Q, R).
/**
 * A # B (SAT):
 * If A and B are expressions then the xor A#B is also an expression.
 */
expr_eval(A#B, R) :- !,
   expr_eval(A, P),
   expr_eval(B, Q),
   tree_xor(P, Q, R).
/**
 * A -> B; C (SAT):
 * If A, B and C are expressions then the if-then-else A->B;C is also an expression.
 */
expr_eval((A->B;C), S) :- !,
   expr_eval(A, P),
   expr_eval(B, Q),
   expr_eval(C, R),
   tree_ite(P, Q, R, S).
/**
 * V^A (SAT):
 * If V is a native Prolog variable and A is an expression then the Boolean existential quantification V^A is also an expression.
 */
expr_eval(X^A, R) :- !,
   map_new(X, Y),
   expr_eval(A, P),
   tree_exists(Y, P, R).
expr_eval(E, _) :-
   throw(error(type_error(sat_expr,E),_)).

/**
 * expr_pretty(T, E):
 * The predicate succeeds in E with the expression of the tree T.
 */
% expr_pretty(+Tree, -Expr)
expr_pretty(zero, R) :- !,
   R = 0.
expr_pretty(one, R) :- !,
   R = 1.
expr_pretty(node(X,A,B), R) :-
   map_back(X, Y),
   expr_pretty(A, C),
   expr_pretty(B, D),
   R = (Y->C;D).

/**
 * expr_vars(T, L):
 * The predicae succeeds in L with the indexes in the tree T.
 */
% expr_vars(+Tree, -List)
expr_vars(zero, R) :- !,
   R = [].
expr_vars(one, R) :- !,
   R = [].
expr_vars(node(X,A,B), [X|S]) :-
   expr_vars(A, L),
   expr_vars(B, M),
   vars_union(L, M, S).

% vars_union(+List, +List, -List)
:- private vars_union/3.
vars_union([], X, R) :- !,
   R = X.
vars_union(X, [], R) :- !,
   R = X.
vars_union([X|L], [Y|M], R) :-
   X < Y, !,
   vars_union(L, [Y|M], S),
   R = [X|S].
vars_union([X|L], [X|M], R) :- !,
   vars_union(L, M, S),
   R = [X|S].
vars_union(L, [Y|M], [Y|S]) :-
   vars_union(L, M, S).

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
tree_not(node(X,A,B), R) :- !,
   tree_not(A, C),
   tree_not(B, D),
   tree_make(C, D, X, R).

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
tree_and(node(X,A,B), node(Y,C,D), R) :-
   X < Y, !,
   tree_and(A, node(Y,C,D), E),
   tree_and(B, node(Y,C,D), F),
   tree_make(E, F, X, R).
tree_and(node(X,A,B), node(X,C,D), R) :- !,
   tree_and(A, C, E),
   tree_and(B, D, F),
   tree_make(E, F, X, R).
tree_and(N, node(Y,C,D), R) :-
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
tree_or(node(X,A,B), node(Y,C,D), R) :-
   X < Y, !,
   tree_or(A, node(Y,C,D), E),
   tree_or(B, node(Y,C,D), F),
   tree_make(E, F, X, R).
tree_or(node(X,A,B), node(X,C,D), R) :- !,
   tree_or(A, C, E),
   tree_or(B, D, F),
   tree_make(E, F, X, R).
tree_or(N, node(Y,C,D), R) :-
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
tree_imply(node(X,A,B), node(Y,C,D), R) :-
   X < Y, !,
   tree_imply(A, node(Y,C,D), E),
   tree_imply(B, node(Y,C,D), F),
   tree_make(E, F, X, R).
tree_imply(node(X,A,B), node(X,C,D), R) :- !,
   tree_imply(A, C, E),
   tree_imply(B, D, F),
   tree_make(E, F, X, R).
tree_imply(N, node(Y,C,D), R) :-
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
tree_equiv(node(X,A,B), node(Y,C,D), R) :-
   X < Y, !,
   tree_equiv(A, node(Y,C,D), E),
   tree_equiv(B, node(Y,C,D), F),
   tree_make(E, F, X, R).
tree_equiv(node(X,A,B), node(X,C,D), R) :- !,
   tree_equiv(A, C, E),
   tree_equiv(B, D, F),
   tree_make(E, F, X, R).
tree_equiv(N, node(Y,C,D), R) :-
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
 * tree_ite(P, Q, R, S):
 * The predicate succeeds in S with the boolean if-then-else of P, Q and R.
 */
% tree_ite(+Tree, +Tree, +Tree, -Tree)
:- private tree_ite/4.
tree_ite(P, Q, R, S) :-
   tree_imply(P, Q, A),
   tree_or(P, R, B),
   tree_and(A, B, S).

/**
 * tree_make(A, B, X, R):
 * The predicate succeeds in R with a new tree (X->A;B).
 */
% tree_make(+Tree, +Tree, +Index, -Tree)
:- private tree_make/4.
tree_make(A, A, _, R) :- !,
   R = A.
tree_make(A, B, X, node(X,A,B)).

/*****************************************************************/
/* Quantifiers                                                   */
/*****************************************************************/

/**
 * tree_exists(P, Q, R):
 * The predicate succeeds in R with existential removing the variable P from Q.
 */
% tree_exists(+Index, +Tree, -Tree)
tree_exists(_, zero, R) :- !,
   R = zero.
tree_exists(_, one, R) :- !,
   R = one.
tree_exists(X, node(Y,A,B), R) :-
   Y < X, !,
   tree_exists(X, A, C),
   tree_exists(X, B, D),
   tree_make(C, D, Y, R).
tree_exists(X, node(X,A,B), R) :- !,
   tree_or(A, B, R).
tree_exists(_, N, N).

/**
 * tree_one(P, Q, R):
 * The predicate succeeds in R with substituting 1 for the variable P in Q.
 */
% tree_one(+Index, +Tree, -Tree)
tree_one(_, zero, R) :- !,
   R = zero.
tree_one(_, one, R) :- !,
   R = one.
tree_one(X, node(Y,A,B), R) :-
   Y < X, !,
   tree_one(X, A, C),
   tree_one(X, B, D),
   tree_make(C, D, Y, R).
tree_one(X, node(X,A,_), R) :- !,
   R = A.
tree_one(_, N, N).

/**
 * tree_zero(P, Q, R):
 * The predicate succeeds in R with substituting 0 for the variable P in Q.
 */
% tree_zero(+Index, +Tree, -Tree)
tree_zero(_, zero, R) :- !,
   R = zero.
tree_zero(_, one, R) :- !,
   R = one.
tree_zero(X, node(Y,A,B), R) :-
   Y < X, !,
   tree_zero(X, A, C),
   tree_zero(X, B, D),
   tree_make(C, D, Y, R).
tree_zero(X, node(X,_,B), R) :- !,
   R = B.
tree_zero(_, N, N).

/*****************************************************************/
/* Variables                                                     */
/*****************************************************************/

/**
 * map_new(X, Y):
 * The predicate succeeds in Y with new mapping of X.
 */
% map_new(+Variable, -Index)
map_new(X, R) :-
   get_attr(X, tree, var_index(Y)), !,
   R = Y.
map_new(X, R) :-
   map_extend(X, Y),
   put_attr(X, tree, var_index(Y)),
   R = Y.

% map_extend(+Variable, -Index)
:- private map_extend/2.
map_extend(X, Y) :-
   nb_current(var_map, M), !,
   M =.. [_|L],
   append(L, [X], R),
   N =.. [vector|R],
   b_setval(var_map, N),
   length(R, Y).
map_extend(X, 1) :-
   b_setval(var_map, vector(X)).

/**
 * map_back(X, Y):
 * The predicate succeeds in Y with the unmapping of X.
 */
% map_back(+Index, -Variable)
map_back(X, Y) :-
   nb_current(var_map, M),
   arg(X, M, Y).

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
attr_unify_hook(_, _).

/*****************************************************************/
/* Goals Hook                                                   */
/*****************************************************************/

/**
 * attribute_goals(V, I, O):
 * The predicate is called when the goal list I of
 * the variable V is required. The list should end in O.
 */
% attribute_goals(+Variable, -List, +List)
:- public attribute_goals/3.
attribute_goals(_, R, R).

