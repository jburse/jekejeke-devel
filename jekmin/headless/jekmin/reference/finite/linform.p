/**
 * The finite domain solver allows denoting integer value expressions.
 * These expressions can contain native Prolog variables. Integer
 * expressions are posted to the finite domain solver via the
 * predicate #=/2. Internally integer equations are broken down
 * into elementary integer equations with the help of new native
 * Prolog variables. The resulting elementary integer equations are
 * automatically shown by the top-level.
 *
 * Example:
 * ?- X*Y*Z #= T.
 * _C*Z #= T,
 * X*Y #= _C
 *
 * Forward chaining rules and attribute variable hooks guard the
 * interaction between the elementary integer equations. A minimal
 * logic reading of forward chaining rules with delete has been
 * presented in [1]. It has also been shown there how forward
 * chaining rules with delete can do simplifications if the
 * constraint store is held in the forward store. Currently the
 * following inference rule sets have been implemented for integer
 * value expressions:
 *
 * * Forward Checking
 * * Duplicate Detection
 *
 * The forward checking consists of the two inference rules constant
 * elimination and constant back propagation, depending on whether the
 * constants equations have first arrived in the for-ward store or the
 * elementary integer equations. The forward checking also provides the
 * inference rules of union find and variable renaming. Where permitted
 * the #=/2 integer equations are replaced by native Prolog unification.
 * It is also allowed mixing native Prolog unification =/2 with integer
 * equations:
 *
 * Examples:
 * ?- X = Y, 4 #= X+Y.
 * X = 2,
 * Y = 2
 *
 * ?- 4 #= X+Y, X = Y.
 * X = 2,
 * Y = 2
 *
 * The duplicate detection has only been implemented for elementary
 * integer equations that are the scalar product of a constant vector
 * and a variable vector. The duplicate detection does not yet work for
 * arbitrary products. It is handy in that it reduces the number of
 * elementary equations and might also detect inconsistencies early on:
 *
 * Examples:
 * ?- 2*X #= 4*Y, 3*X #= 6*Y.
 * X #= 2*Y.
 * Yes
 *
 * ?- X #= Y+1, Y #= X+1.
 * No
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
 * Implementation note:
 *
 * The following data structure is used:
 *     Prod = [Integer * Atom | Prod] | [].
 */

% :- package(library(ordered)).
:- package(library(jekmin/reference/finite)).
:- use_package(library(jekpro/frequent/misc)).

:- module(linform, []).
:- use_module(library(minimal/assume)).
:- use_module(library(minimal/hypo)).
:- use_module(library(minimal/delta)).
:- use_module(library(basic/lists)).
:- use_module(library(misc/residue)).
:- use_module(library(experiment/trail)).
:- use_module(library(experiment/attr)).
:- use_module(library(experiment/cont)).
:- use_module(library(misc/elem)).
:- use_module(library(term/suspend)).
:- use_module(helper).
:- use_module(intset).

:- public infix(#=).
:- op(700, xfx, #=).

:- public infix(#\=).
:- op(700, xfx, #\=).

:- public infix(#<).
:- op(700, xfx, #<).

:- public infix(#=<).
:- op(700, xfx, #=<).

:- public infix(#>).
:- op(700, xfx, #>).

:- public infix(#>=).
:- op(700, xfx, #>=).

% sys_in(+Wrap, +Set, +Bound)
% sys_in(X,S,_) = X in S
:- multifile intset:sys_in/3.
:- thread_local intset:sys_in/3.
:- multifile intset:sys_in/4.

% sys_set_agent(+Ref, , +Wrap, +Prod, +Set, +Bound)
:- multifile intset:sys_set_agent/5.
:- thread_local intset:sys_set_agent/5.
% :- multifile intset:sys_set_agent/6.

/**********************************************************/
/* Equality Constraint                                    */
/**********************************************************/

/**
 * A #= B:
 * If A and B are value expressions then their equality is posted.
 */
% #=(+Expr, +Expr)
:- public #= /2.
X #= Y :-
   sys_value_expr(Y, P, C),
   sys_value_expr_inv(X, Q, D),
   K is D-C,
   sys_flip_prod(Q, J),
   sys_add_prod(J, P, E),
   post(sys_lin(E, K)).

/**********************************************************/
/* Comparison Constraints                                 */
/**********************************************************/

/**
 * A #\= B:
 * If A and B are value expressions then their inequality is posted.
 */
% #\=(+Expr,+Expr)
:- public #\= /2.
X #\= Y :-
   sys_compare_expr(Y, X, [.. -1,1...]).

/**
 * A #< B:
 * If A and B are value expressions then is posted
 * that A is less than B.
 */
% #<(+Expr,+Expr)
:- public #< /2.
X #< Y :-
   sys_compare_expr(Y, X, [1...]).

/**
 * A #> B:
 * If A and B are value expressions then is posted
 * that A is greater than B.
 */
% #>(+Expr,+Expr)
:- public #> /2.
X #> Y :-
   sys_compare_expr(Y, X, [.. -1]).

/**
 * A #=< B:
 * If A and B are value expressions then is posted
 * that A is less or equal than B.
 */
% #=<(+Expr,+Expr)
:- public #=< /2.
X #=< Y :-
   sys_compare_expr(Y, X, [0...]).

/**
 * A #>= B:
 * If A and B are value expressions then is posted
 * that A is greater or equal than B.
 */
% #>=(+Expr,+Expr)
:- public #>= /2.
X #>= Y :-
   sys_compare_expr(Y, X, [..0]).

/**
 * sys_compare_expr(Y, X, S):
 * Y - X in S with opposite directionality for X.
 */
% sys_compare_expr(+Expr, +Expr, +Set)
sys_compare_expr(Y, X, S) :-
   sys_value_expr(Y, P, C),
   sys_value_expr_inv(X, Q, D),
   K is D-C,
   sys_flip_prod(Q, J),
   sys_add_prod(J, P, E),
   sys_add_set(S, K, T),
   post(sys_set(E, T)).

/**********************************************************/
/* Global Constraint                                      */
/**********************************************************/

/**
 * all_different([A1, .., An]):
 * If A1, .., An are value expressions then their inequality is posted.
 */
% all_different(+List)
% SWI-Prolog like naming
% Does a validation of the list
:- public all_different/1.
all_different(V) :-
   var(V),
   throw(error(instantiation_error,_)).
all_different([X|Y]) :- !,
   all_different(Y),
   sys_nq_list(Y, X).
all_different([]) :- !.
all_different(X) :-
   throw(error(type_error(list,X),_)).

% sys_nq_list(+List, +Expr)
% Doesn't do a validation of the list
:- private sys_nq_list/2.
sys_nq_list([Y|Z], X) :-
   X #\= Y,
   sys_nq_list(Z, X).
sys_nq_list([], _).

/**********************************************************/
/* Linear Constraints                                     */
/**********************************************************/

% sys_hook_lin(+Var, +Term)
:- private sys_hook_lin/2.
sys_hook_lin(V, W) :-
   var(W), !,
   sys_freeze_var(V, R),
   sys_fresh_var(W, S),
   sys_assume_cont(post(sys_var_lin(R, S))).
sys_hook_lin(V, T) :-
   integer(T), !,
   sys_freeze_var(V, R),
   sys_assume_cont(post(sys_const_lin(R, T))).
sys_hook_lin(_, T) :-
   throw(error(type_error(integer,T),_)).

% sys_var_lin(+Wrap, +Wrap)
% sys_var_lin(X, Y) = X = Y
:- private sys_var_lin/2.
:- thread_local sys_var_lin/2.
:- private sys_var_lin/3.

/* Union Find */
true <=
   phaseout_posted(sys_var_lin(_, _)).

% sys_const_lin(+Wrap, +Integer)
% sys_const_lin(X, C) = X = C
:- private sys_const_lin/2.
:- thread_local sys_const_lin/2.
:- private sys_const_lin/3.

/* Constant Elimination */
true <=
   phaseout_posted(sys_const_lin(_, _)).

% sys_lin(+Prod, +Integer)
% sys_lin(P, I) = P = I

% sys_lin_ref(+Ref, +Prod, +Integer)
% sys_lin_ref(R, P, I) = P = I

% sys_lin_waits(+Wrap, +Ref)
:- multifile sys_lin_waits/2.
:- thread_local sys_lin_waits/2.

% sys_lin_agent(+Ref, +Wrap, +Prod, +Integer)
% sys_lin_agent(R, X, P, I) = P = I
:- multifile sys_lin_agent/4.
:- thread_local sys_lin_agent/4.
:- multifile sys_lin_agent/5.

/* Create Surrogate */
post(sys_lin_ref(R, L, T)) <=
   phaseout_posted(sys_lin(L, T)),
   surrogate_new(R).

/* Trivial Cases */
fail <=
   phaseout_posted(sys_lin_ref(_, [], K)),
   K \== 0, !.
true <=
   phaseout_posted(sys_lin_ref(_, [], _)), !.
/* Agent start */
assumez(sys_lin_waits(X, R)) <=
   posted(sys_lin_ref(R, L, _)),
   member(_*X, L).
post(sys_lin_agent(R, X, [A*X|B], T)) <=
   phaseout_posted(sys_lin_ref(R, [A*X|B], T)).

% sys_lin_remove(+Ref)
% Remove helper
:- private sys_lin_remove/2.
true <=
   posted(sys_lin_remove(V)),
   phaseout(sys_lin_waits(_, V)).
true <=
   phaseout_posted(sys_lin_remove(_)).

/* GCD Normalisation */
fail <=
   phaseout_posted(sys_lin_agent(_, _, P, T)),
   sys_gcd_prod(P, G),
   0 =\= T rem G, !.
post(sys_lin_agent(V, X, R, H)) <=
   phaseout_posted(sys_lin_agent(V, X, P, T)),
   sys_gcd_prod(P, G), !,
   sys_div_prod(P, G, R),
   H is T//G.
post(sys_lin_agent(V, X, R, H)) <=
   phaseout_posted(sys_lin_agent(V, X, [A*X|B], T)),
   A < 0, !,
   sys_flip_prod([A*X|B], R),
   H is -T.
/* Unification Trigger */
sys_melt_const(X, A) <=
   phaseout_posted(sys_lin_agent(V, X, [1*X], A)), !,
   phaseout(sys_lin_waits(X, V)).
sys_melt_join(Y, X) <=
   phaseout_posted(sys_lin_agent(V, X, [1*X,-1*Y], 0)), !,
   phaseout(sys_lin_waits(X, V)),
   phaseout(sys_lin_waits(Y, V)).
/* Lin & Lin Intersection */
fail <=
   phaseout_posted(sys_lin_agent(_, X, L, C)),
   sys_lin_agent(_, X, L, D),
   C \== D, !.
post(sys_lin_remove(V)) <=
   phaseout_posted(sys_lin_agent(V, X, L, _)),
   sys_lin_agent(_, X, L, _), !.
/* Lin & Set Intersection */
fail <=
   phaseout_posted(sys_lin_agent(_, X, L, C)),
   sys_set_agent(_, X, L, S, _),
   \+ sys_elem_set(S, C), !.
post(intset:sys_set_remove(V)) <=
   posted(sys_lin_agent(_, X, L, _)),
   phaseout(sys_set_agent(V, X, L, _, _)).
/* Union Find */
post(sys_lin_remove(V)),
post(sys_lin_ref(V, D, T)) <=
   phaseout_posted(sys_lin_agent(V, _, L, T)),
   sys_lin_waits(X, V),
   sys_bound_var(X),
   sys_melt_var(X, H),
   var(H), !,
   sys_fresh_var(H, Y),
   sys_pick_prod(B, X, L, E),
   sys_add_prod([B*Y], E, D).
/* Constant Elimination */
post(sys_lin_agent(V, Z, [A*Z|D], H)) <=
   phaseout_posted(sys_lin_agent(V, _, L, T)),
   phaseout(sys_lin_waits(X, V)),
   sys_bound_var(X),
   sys_melt_var(X, C),
   integer(C), !,
   sys_pick_prod(B, X, L, [A*Z|D]),
   H is T-B*C.
/* Hook Adding */
sys_melt_hook(X, sys_hook_lin) <=
   posted(sys_lin_agent(V, _, _, _)),
   sys_lin_waits(X, V).
/* Set Diffusion, Directed, Bounds */
post(intset:sys_in(X, L, U)) <=
   posted(sys_lin_agent(_, _, [A*X|B], T)),
   sys_bound_poly(B, Q),
   sys_flip_range(Q, R),
   sys_add_range(R, T, S),
   (  sys_div_range(S, A, U)
   -> L = [U]
   ;  U = ...,
      L = []).
/* Variable Rename */
post(sys_lin_remove(V)),
post(sys_lin_ref(V, D, T)) <=
   posted(sys_var_lin(X, Y)),
   sys_lin_waits(X, V),
   phaseout(sys_lin_agent(V, _, L, T)),
   sys_pick_prod(B, X, L, E),
   sys_add_prod([B*Y], E, D).
/* Constant Backpropagation */
post(sys_lin_agent(V, Z, [A*Z|D], H)) <=
   posted(sys_const_lin(X, C)),
   phaseout(sys_lin_waits(X, V)),
   phaseout(sys_lin_agent(V, _, L, T)),
   sys_pick_prod(B, X, L, [A*Z|D]),
   H is T-B*C.
/* Set Update, Directed, Bounds */
post(intset:sys_in(X, L, U)) <=
   posted(intset:sys_in(Y, _, E)),
   E \== ...,
   sys_lin_waits(Y, V),
   sys_lin_agent(V, _, [A*X|B], T),
   Y \== X,
   sys_pick_prod(C, Y, B, D),
   sys_rampup_poly(D, C, E, Q),
   sys_flip_range(Q, R),
   sys_add_range(R, T, S),
   (  sys_div_range(S, A, U)
   -> L = [U]
   ;  U = ...,
      L = []).

% residue:sys_current_eq(+Var, -Handle)
:- public residue:sys_current_eq/2.
:- multifile residue:sys_current_eq/2.
:- discontiguous residue:sys_current_eq/2.
residue:sys_current_eq(V, L#=C) :-
   sys_clause_hook(V, sys_hook_lin, _),
   sys_freeze_var(V, X),
   sys_lin_waits(X, K),
   sys_lin_agent(K, _, L, C).

% residue:sys_unwrap_eq(+Handle, -Goals, +Goals)
:- public residue:sys_unwrap_eq/3.
:- multifile residue:sys_unwrap_eq/3.
:- discontiguous residue:sys_unwrap_eq/3.
residue:sys_unwrap_eq([A*X|B]#=K, [E#=F|L], L) :-
   sys_pretty_lin([A*X], 0, E),
   sys_flip_prod(B, C),
   sys_pretty_lin(C, K, F).

/**********************************************************/
/* Scalar Product Parsing                                 */
/**********************************************************/

/**
 * V (finite):
 * An native Prolog variable V represents an integer variable.
 */
sys_value_expr(X, [1*B], 0) :-
   var(X), !,
   sys_fresh_var(X, B).

/**
 * I (finite):
 * An integer I represents an integer constant.
 */
% sys_value_expr(+Expr, -Prod, -Integer)
sys_value_expr(A, [], A) :-
   integer(A), !.

/**
 * A + B (finite):
 * If A and B are value expressions then A+B is also a value expression.
 */
sys_value_expr(A+B, E, K) :- !,
   sys_value_expr(A, P, C),
   sys_value_expr(B, Q, D),
   K is C+D,
   sys_add_prod(Q, P, E).

/**
  * A – B (finite):
  * If A and B are value expressions then A-B is also a value expression.
  */
sys_value_expr(A-B, E, K) :- !,
   sys_value_expr(A, P, C),
   sys_value_expr(B, Q, D),
   K is C-D,
   sys_flip_prod(Q, J),
   sys_add_prod(J, P, E).

/**
 * - A (finite):
 * If A is a value expression then -A is also a value expression.
 */
sys_value_expr(-A, E, K) :- !,
   sys_value_expr(A, P, B),
   K is -B,
   sys_flip_prod(P, E).

/**
 * A * B (finite):
 * If A and B are value expressions then A*B is also a value expression.
 */
sys_value_expr(X*Y, S, H) :- !,
   sys_value_expr(X, L, A),
   sys_fresh_var(_, V),
   sys_value_expr(Y, R, C),
   sys_fresh_var(_, W),
   sys_fresh_var(_, G),
   sys_mul_lin(L, A, R, C, S, H, V, W, G).

/**
 * abs(A) (finite):
 * If A is a value expression then abs(A) is also a value expression.
 */
sys_value_expr(abs(X), S, H) :- !,
   sys_value_expr(X, L, A),
   sys_fresh_var(_, V),
   sys_fresh_var(_, G),
   sys_abs_lin(L, A, S, H, V, G).

/**
 * C (finite):
 * A callable C is also a value expression.
 */
sys_value_expr(C, L, A) :-
   sys_callable(C), !,
   X is C,
   sys_value_expr(X, L, A).

sys_value_expr(A, _, _) :-
   throw(error(type_error(fd_value,A),_)).

/**
 * sys_value_expr_inv(E, P, K):
 * Convert an expression E into a scalar product P and a constant K.
 * Does the same as sys_value_expr/3, except that all equations are
 * directed in the opposite.
 */
% sys_value_expr_inv(+Expr, -Prod, -Integer)
sys_value_expr_inv(X, [1*B], 0) :-
   var(X), !,
   sys_fresh_var(X, B).
sys_value_expr_inv(A, [], A) :-
   integer(A), !.
sys_value_expr_inv(A+B, E, K) :- !,
   sys_value_expr_inv(A, P, C),
   sys_value_expr_inv(B, Q, D),
   K is C+D,
   sys_add_prod(Q, P, E).
sys_value_expr_inv(A-B, E, K) :- !,
   sys_value_expr_inv(A, P, C),
   sys_value_expr_inv(B, Q, D),
   K is C-D,
   sys_flip_prod(Q, J),
   sys_add_prod(J, P, E).
sys_value_expr_inv(-A, E, K) :- !,
   sys_value_expr_inv(A, P, B),
   K is -B,
   sys_flip_prod(P, E).
sys_value_expr_inv(X*Y, S, H) :- !,
   sys_fresh_var(_, G),
   sys_fresh_var(_, V),
   sys_value_expr_inv(X, L, A),
   sys_fresh_var(_, W),
   sys_value_expr_inv(Y, R, C),
   sys_mul_lin(L, A, R, C, S, H, V, W, G).
sys_value_expr_inv(abs(X), S, H) :- !,
   sys_fresh_var(_, G),
   sys_fresh_var(_, V),
   sys_value_expr_inv(X, L, A),
   sys_abs_lin(L, A, S, H, V, G).
sys_value_expr_inv(C, L, A) :-
   sys_callable(C), !,
   X is C,
   sys_value_expr(X, L, A).
sys_value_expr_inv(A, _, _) :-
   throw(error(type_error(fd_value,A),_)).

/**********************************************************/
/* Scalar Product Unparsing                               */
/**********************************************************/

% sys_pretty_lin(+Prod, +Integer, -Expr)
sys_pretty_lin(P, 0, E) :- !,
   sys_pretty_prod(P, E).
sys_pretty_lin(P, K, E) :-
   K < 0, !,
   H is -K,
   sys_pretty_prod(P, J),
   sys_pretty_sub(J, H, E).
sys_pretty_lin(P, K, E) :-
   sys_pretty_prod(P, J),
   sys_pretty_add(J, K, E).

% sys_pretty_prod(+Prod, -Expr)
:- private sys_pretty_prod/2.
sys_pretty_prod([1*X|A], C) :- !,
   sys_pretty_prod(A, B),
   sys_melt_var(X, Y),
   sys_pretty_add(B, Y, C).
sys_pretty_prod([-1*X|A], C) :- !,
   sys_pretty_prod(A, B),
   sys_melt_var(X, Y),
   sys_pretty_sub(B, Y, C).
sys_pretty_prod([A*X|B], D) :-
   A < 0, !,
   sys_pretty_prod(B, C),
   sys_melt_var(X, Y),
   H is -A,
   sys_pretty_sub(C, H*Y, D).
sys_pretty_prod([A*X|B], D) :-
   sys_pretty_prod(B, C),
   sys_melt_var(X, Y),
   sys_pretty_add(C, A*Y, D).
sys_pretty_prod([], 0).

% sys_pretty_add(+Expr, +Expr, -Expr)
:- private sys_pretty_add/3.
sys_pretty_add(X, Y, X+Y) :-
   var(X), !.
sys_pretty_add(0, X, X) :- !.
sys_pretty_add(X, Y, X+Y).

% sys_pretty_sub(+Expr, +Expr, -Expr)
:- private sys_pretty_sub/3.
sys_pretty_sub(X, Y, X-Y) :-
   var(X), !.
sys_pretty_sub(0, X, -X) :- !.
sys_pretty_sub(X, Y, X-Y).

% sys_pretty_in(+Set, +Lin, -Goal)
sys_pretty_in([..I,J...], [A*X|B], E#\=F) :-
   K is I+1,
   K is J-1, !,
   sys_pretty_lin([A*X], 0, E),
   sys_flip_prod(B, C),
   sys_pretty_lin(C, K, F).
sys_pretty_in([..I], [A*X|B], E#=<F) :- !,
   sys_pretty_lin([A*X], 0, E),
   sys_flip_prod(B, C),
   sys_pretty_lin(C, I, F).
sys_pretty_in([I...], [A*X|B], E#>F) :-
   K is I-1, !,
   sys_pretty_lin([A*X], 0, E),
   sys_flip_prod(B, C),
   sys_pretty_lin(C, K, F).
sys_pretty_in([I], [A*X|B], E#=F) :-
   integer(I), !,                                 /* Guard Included */
   sys_pretty_lin([A*X], 0, E),
   sys_flip_prod(B, C),
   sys_pretty_lin(C, I, F).
sys_pretty_in(S, L, E in F) :-
   sys_pretty_lin(L, 0, E),
   sys_pretty_set(S, F).

/**********************************************************/
/* Multiplication Function                                */
/**********************************************************/

% sys_mul_lin(+Prod, +Integer, +Prod, +Integer, -Prod, -Integer, +Wrap, +Wrap, +Wrap)
:- private sys_mul_lin/9.
sys_mul_lin([], A, L, C, R, H, _, _, _) :- !,
   sys_mul_prod(L, A, R),
   H is A*C.
sys_mul_lin(L, A, [], C, R, H, _, _, _) :- !,
   sys_mul_prod(L, C, R),
   H is A*C.
sys_mul_lin(L, A, R, C, B, K, V, W, G) :-
   sys_mul_arg(L, A, [M*E], P, V),
   sys_mul_arg(R, C, [N*F], Q, W),
   post(clpfd:sys_mulv(E, F, G)),
   H is M*N,
   I is M*Q,
   J is N*P,
   K is P*Q,
   sys_make_prod(I, E, [], S),
   sys_make_prod(J, F, [], T),
   sys_add_prod(S, T, U),
   sys_add_prod([H*G], U, B).

% sys_mul_arg(+Prod, +Integer, -Prod, -Integer, +Wrap)
:- private sys_mul_arg/5.
sys_mul_arg([M], B, [M], B, _) :- !.
sys_mul_arg(L, A, [1*E], 0, E) :-
   sys_flip_prod(L, H),
   sys_add_prod([1*E], H, R),
   post(sys_lin(R, A)).

/**********************************************************/
/* Absolute Function                                      */
/**********************************************************/

% sys_abs_lin(+Prod, +Integer, -Prod, -Integer, +Wrap, +Wrap)
:- private sys_abs_lin/6.
sys_abs_lin([], A, [], H, _, _) :- !,
   H is abs(A).
sys_abs_lin(L, A, [H*G], 0, V, G) :-
   sys_abs_arg(L, A, [M*E], V),
   post(clpfd:sys_absv(E, G)),
   H is abs(M).

% sys_abs_arg(+Prod, +Integer, -Prod, +Wrap)
:- private sys_abs_arg/4.
sys_abs_arg([M], 0, [M], _) :- !.
sys_abs_arg(L, A, [1*E], E) :-
   sys_flip_prod(L, H),
   sys_add_prod([1*E], H, R),
   post(sys_lin(R, A)).

/**********************************************************/
/* Scalar Product Operations                              */
/**********************************************************/

% sys_make_prod(+Integer, +Wrap, +Prod, -Prod)
sys_make_prod(0, _, L, L) :- !.
sys_make_prod(A, X, L, [A*X|L]).

% sys_add_prod(+Prod, +Prod, -Prod)
sys_add_prod([], X, X) :- !.
sys_add_prod(X, [], X) :- !.
sys_add_prod([A*X|B], [C*X|D], J) :- !,
   sys_add_prod(B, D, H),
   I is A+C,
   sys_make_prod(I, X, H, J).
sys_add_prod([A*X|B], [C*Y|D], [A*X|H]) :-
   X @> Y, !,
   sys_add_prod(B, [C*Y|D], H).
sys_add_prod(B, [C*Y|D], [C*Y|H]) :-
   sys_add_prod(B, D, H).

% sys_mul_prod(+Prod, +Integer, -Prod)
sys_mul_prod(_, 0, []) :- !.
sys_mul_prod([A*X|B], C, [D*X|E]) :-
   D is A*C,
   sys_mul_prod(B, C, E).
sys_mul_prod([], _, []).

% sys_pick_prod(+Integer, +Wrap, +Prod, -Prod)
sys_pick_prod(A, X, [A*X|B], B) :- !.
sys_pick_prod(A, X, [H|L], [H|R]) :-
   sys_pick_prod(A, X, L, R).

/**********************************************************/
/* Greates Common Divisor                                 */
/**********************************************************/

% sys_gcd_prod(+Prod, -Integer)
% Fails if gcd is 1 or -1
sys_gcd_prod([A*_], A) :- !,
   abs(A) =\= 1.
sys_gcd_prod([A*_|L], C) :-
   abs(A) =\= 1,
   sys_gcd_prod(L, B),
   C is gcd(A,B),
   abs(C) =\= 1.

% sys_flip_prod(+Prod, -Prod)
sys_flip_prod([], []).
sys_flip_prod([A*X|B], [C*X|D]) :-
   C is -A,
   sys_flip_prod(B, D).

% sys_div_prod(+Prod, +Integer, -Prod)
sys_div_prod([], _, []).
sys_div_prod([A*X|B], E, [C*X|D]) :-
   C is A//E,
   sys_div_prod(B, E, D).

/**********************************************************/
/* Poly Bounds                                            */
/**********************************************************/

% sys_bound_poly(+Prod, -Range)
% Fails for trivial ranges
sys_bound_poly([], 0).
sys_bound_poly([A*X], U) :- !,
   sys_in(X, _, R),
   R \== ...,
   sys_pump_range(R, A, U).
sys_bound_poly([A*X|B], U) :-
   sys_bound_poly(B, T),
   sys_in(X, _, R),
   R \== ...,
   sys_pump_range(R, A, S),
   sys_blur_range(S, T, U).

% sys_rampup_poly(+Prod, +Integer, +Range, -Range)
% Fails for trivial ranges
sys_rampup_poly([], A, R, U) :- !,
   sys_pump_range(R, A, U).
sys_rampup_poly(B, A, R, U) :-
   sys_bound_poly(B, T),
   sys_pump_range(R, A, S),
   sys_blur_range(S, T, U).

% sys_bound_divisor(+Wrap, -Range)
% Succeeds even for trivial ranges
sys_bound_divisor(X, R) :-
   sys_in(X, _, R), !.
sys_bound_divisor(_, ...).

% sys_bound_factor(+Wrap, -Range)
% Fails for trivial ranges
sys_bound_factor(X, R) :-
   sys_in(X, _, R),
   R \== ... .
