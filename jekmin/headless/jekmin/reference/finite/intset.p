/**
 * Set expressions are posted via the predicate in/2 respectively ins/2 and
 * are similarly handled to equality. Further forward checking and duplicate
 * detection is also applied. But element hood provides additional
 * functionality. Namely we find the following additional inference rule for
 * the element hood:
 *
 * * Interval Consistency
 *
 * The interval consistency has been both implemented for the scalar
 * product and multiplication. A restricted form of interval consistency
 * has been implemented. Namely intervals only propagate to the lexical
 * head of a scalar product respectively multiplication. Directed interval
 * consistency also works for the recently introduced abs/1 function.
 *
 * Examples:
 * ?- X in 0..9, Y #= X+1, Z #= Y+1, X #= Z+1.
 * Z #= X-1,
 * Z in 2..8,
 * ...
 *
 * ?- X in 0..sup, Y #= X+1, Z #= Y+1, X #= Z+1.
 * Z #= X-1,
 * Z #> 1,
 * ...
 *
 * This directed form of interval consistency is weaker than the full
 * interval consistency. The directed form of interval consistency might
 * accept equations that would otherwise be detected as inconsistent.
 * But it has the advantage that it never loops and therefore also works
 * for unbounded domains.
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
 * The following range notation is used:
 *     ...     = { X | true }
 *     A...    = { X | A =< X }
 *     ..A     = { X | X =< A }
 *     A..B    = { X | A =< X & X =< B }
 *     A       = { X | A =:= X }
 *
 * The following data structure is used:
 *     Set = [Range | Set] | [].
 */

% :- package(library(ordered)).
:- package(library(jekmin/reference/finite)).
:- use_package(library(jekpro/frequent/misc)).

:- module(intset, []).
:- use_module(library(minimal/assume)).
:- use_module(library(minimal/hypo)).
:- use_module(library(minimal/delta)).
:- use_module(library(basic/lists)).
:- use_module(library(misc/residue)).
:- use_module(library(experiment/trail)).
:- use_module(library(experiment/attr)).
:- use_module(library(misc/elem)).
:- use_module(helper).
:- use_module(linform).

:- public infix(..).
:- op(100, xfx, ..).

:- public prefix(..).
:- op(100, fx, ..).

:- public postfix(...).
:- op(100, xf, ...).

:- public infix(in).
:- op(700, xfx, in).

:- public infix(ins).
:- op(700, xfx, ins).

% sys_lin_waits(+Wrap, +Ref)
:- multifile linform:sys_lin_waits/2.
:- thread_local linform:sys_lin_waits/2.
% sys_lin_agent(+Ref, +Wrap, +Prod, +Integer)
:- multifile linform:sys_lin_agent/4.
:- thread_local linform:sys_lin_agent/4.
% :- multifile clpfd:sys_lin_agent/5.

/***************************************************/
/* Membership Constraint                           */
/***************************************************/

/**
 * A in S:
 * If A is a value expression and S is a set expression then
 * the element hood is posted.
 */
% in(+Expr,+Set)
:- public in/2.
X in U :-
   sys_expr_set(U, S),
   sys_value_expr(X, P, C),
   H is -C,
   sys_add_set(S, H, T),
   post(sys_set(P, T)).

/**
 * [A1, .., An] ins S:
 * If A1, .., An are value expression and S is a set expression
 * then the element hood is posted for each value expression.
 */
% +List ins +Set
% SWI-Prolog like naming.
:- public ins/2.
V ins _ :-
   var(V),
   throw(error(instantiation_error,_)).
[] ins _ :- !.
[X|Y] ins D :- !,
   X in D,
   Y ins D.
X ins _ :-
   throw(error(type_error(list,X),_)).

/**********************************************************/
/* Domain Constraint                                      */
/**********************************************************/

% sys_hook_in(+Var, +Term)
:- private sys_hook_in/2.
sys_hook_in(V, W) :-
   var(W), !,
   sys_freeze_var(V, R),
   sys_fresh_var(W, S),
   sys_assume_cont(post(sys_var_in(R, S))).
sys_hook_in(V, T) :-
   integer(T), !,
   sys_freeze_var(V, R),
   sys_assume_cont(post(sys_const_in(R, T))).
sys_hook_in(_, T) :-
   throw(error(type_error(integer,T),_)).

% sys_var_in(+Wrap, +Wrap)
% sys_var_in(X, Y) = X = Y
:- private sys_var_in/2.
:- thread_local sys_var_in/2.
:- private sys_var_in/3.

/* Union Find */
true
<= phaseout_posted(sys_var_in(_, _)).

% sys_const_in(+Wrap, +Integer)
% sys_const_in(X, C) = X = C
:- private sys_const_in/2.
:- thread_local sys_const_in/2.
:- private sys_const_in/3.

/* Constant Elimination */
true
<= phaseout_posted(sys_const_in(_, _)).

% sys_in(+Wrap, +Set, +Bound)
% sys_in(X,S,_) = X in S
:- multifile sys_in/3.
:- thread_local sys_in/3.
:- multifile sys_in/4.

/* Trivial Cases */
fail
<= phaseout_posted(sys_in(_, [], _)), !.
sys_melt_const(X, C)
<= phaseout_posted(sys_in(X, [C], _)),
   integer(C), !.
/* In & In intersection */
post(sys_in(X, W, R))
<= phaseout_posted(sys_in(X, S, P)),
   phaseout(sys_in(X, T, Q)),
   (  sys_inter_range(P, Q, R)
   -> sys_inter_set(S, T, W)
   ;  W = [],
      R = ...),
   W \== T, !.
true
<= phaseout_posted(sys_in(X, _, _)),
   sys_in(X, _, _), !.
/* Union Find */
post(sys_in(Y, T, S))
<= phaseout_posted(sys_in(X, T, S)),
   sys_bound_var(X),
   sys_melt_var(X, H),
   var(H), !,
   sys_fresh_var(H, Y).
/* Constant Elimination */
fail
<= phaseout_posted(sys_in(X, T, _)),
   sys_bound_var(X),
   sys_melt_var(X, C),
   integer(C),
   \+ sys_elem_set(T, C), !.
true
<= phaseout_posted(sys_in(X, _, _)),
   sys_bound_var(X),
   sys_melt_var(X, C),
   integer(C), !.
/* Hook Adding */
sys_melt_hook(X, sys_hook_in)
<= posted(sys_in(X, _, _)).
/* Variable Rename */
post(sys_in(Y, T, S))
<= posted(sys_var_in(X, Y)),
   phaseout(sys_in(X, T, S)).
/* Constant Backpropagation */
fail
<= posted(sys_const_in(X, H)),
   phaseout(sys_in(X, T, _)),
   \+ sys_elem_set(T, H).
true
<= posted(sys_const_in(X, H)),
   phaseout(sys_in(X, T, _)),
   sys_elem_set(T, H).

% residue:sys_current_eq(+Var, -Handle)
:- public residue:sys_current_eq/2.
:- multifile residue:sys_current_eq/2.
:- discontiguous residue:sys_current_eq/2.
residue:sys_current_eq(V, in(X,S)) :-
   sys_clause_hook(V, sys_hook_in, _),
   sys_freeze_var(V, X),
   sys_in(X, S, _).

% residue:sys_unwrap_eq(+Handle, -Goals, +Goals)
:- public residue:sys_unwrap_eq/3.
:- multifile residue:sys_unwrap_eq/3.
:- discontiguous residue:sys_unwrap_eq/3.
residue:sys_unwrap_eq(in(X,S), [G|L], L) :-
   sys_pretty_in(S, [1*X], G).

/**********************************************************/
/* Set Constraint                                         */
/**********************************************************/

% sys_hook_set(+Var, +Term)
:- private sys_hook_set/2.
sys_hook_set(V, W) :-
   var(W), !,
   sys_freeze_var(V, R),
   sys_fresh_var(W, S),
   sys_assume_cont(post(sys_var_set(R, S))).
sys_hook_set(V, T) :-
   integer(T), !,
   sys_freeze_var(V, R),
   sys_assume_cont(post(sys_const_set(R, T))).
sys_hook_set(_, T) :-
   throw(error(type_error(integer,T),_)).

% sys_var_set(+Wrap, +Wrap)
% sys_var_set(X, Y) = X = Y
:- private sys_var_set/2.
:- thread_local sys_var_set/2.
:- private sys_var_set/3.

/* Union Find */
true
<= phaseout_posted(sys_var_set(_, _)).

% sys_const_set(+Wrap, +Integer)
% sys_const_set(X, C) = X = C
:- private sys_const_set/2.
:- thread_local sys_const_set/2.
:- private sys_const_set/3.

/* Constant Elimination */
true
<= phaseout_posted(sys_const_set(_, _)).

% sys_set(+Prod, +Set)
% sys_set(P, S) = P in S

% sys_set_ref(+Ref, +Prod, +Set, +Bound)
% sys_set_ref(R, P, S, B) = P in S

% sys_set_waits(+Wrap, +Ref)
:- private sys_set_waits/2.
:- thread_local sys_set_waits/2.

% sys_set_agent(+Ref, , +Wrap, +Prod, +Set, +Bound)
% sys_set_agent(R, X, P, S, B) = P in S
:- multifile sys_set_agent/5.
:- thread_local sys_set_agent/5.
:- multifile sys_set_agent/6.

/* Create Surrogate */
post(sys_set_ref(V, L, S, T))
<= phaseout_posted(sys_set(L, S)),
   surrogate_new(V),
   sys_bound_set(S, T).

/* Trivial Cases */
fail
<= phaseout_posted(sys_set_ref(_, [], S, _)),
   \+ sys_elem_set(S, 0), !.
true
<= phaseout_posted(sys_set_ref(_, [], _, _)), !.
/* Agent start */
assumez(sys_set_waits(X, V))
<= posted(sys_set_ref(V, L, _, _)),
   member(_*X, L).
post(sys_set_agent(V, X, [A*X|B], S, T))
<= phaseout_posted(sys_set_ref(V, [A*X|B], S, T)).

% sys_set_remove(+Ref)
% Remove helper
true
<= posted(sys_set_remove(V)),
   phaseout(sys_set_waits(_, V)).
true
<= phaseout_posted(sys_set_remove(_)).

% sys_set_move(+Ref)
% Move helper
:- private sys_set_move/1.
:- thread_local sys_set_move/1.
:- private sys_set_move/2.
assumez(linform:sys_lin_waits(X, V))
<= posted(sys_set_move(V)),
   phaseout(sys_set_waits(X, V)).
true
<= phaseout_posted(sys_set_move(_)).

/* Degenerate Sets */
true
<= phaseout_posted(sys_set_agent(_, _, _, [...], _)), !.
fail
<= phaseout_posted(sys_set_agent(_, _, _, [], _)), !.
post(sys_set_move(V)),
post(linform:sys_lin_agent(V, X, L, Y))
<= phaseout_posted(sys_set_agent(V, X, L, [Y], _)),
   integer(Y), !.
/* GCD Normalisation */
fail
<= phaseout_posted(sys_set_agent(_, _, P, _, U)),
   sys_gcd_prod(P, G),
   \+ sys_div_range(U, G, _), !.
post(sys_set_agent(V, X, R, T, W))
<= phaseout_posted(sys_set_agent(V, X, P, S, U)),
   sys_gcd_prod(P, G), !,
   sys_div_prod(P, G, R),
   sys_div_set(S, G, T),
   sys_div_range(U, G, W).
post(sys_set_agent(V, X, R, T, W))
<= phaseout_posted(sys_set_agent(V, X, [A*X|B], S, U)),
   A < 0, !,
   sys_flip_prod([A*X|B], R),
   sys_flip_set(S, [], T),
   sys_flip_range(U, W).
/* Simple Trigger */
post(sys_in(X, S, T))
<= phaseout_posted(sys_set_agent(V, X, [1*X], S, T)), !,
   phaseout(sys_set_waits(X, V)).
/* Set & Set Intersection */
post(sys_set_remove(J)),
post(sys_set_agent(K, X, L, W, R))
<= phaseout_posted(sys_set_agent(J, X, L, S, P)),
   phaseout(sys_set_agent(K, X, L, T, Q)),
   (  sys_inter_range(P, Q, R)
   -> sys_inter_set(S, T, W)
   ;  W = [],
      R = ...),
   W \== T, !.
post(sys_set_remove(K))
<= phaseout_posted(sys_set_agent(K, X, L, _, _)),
   sys_set_agent(_, X, L, _, _), !.
/* Set & Lin Intersection */
fail
<= phaseout_posted(sys_set_agent(_, X, L, S, _)),
   sys_lin_agent(_, X, L, C),
   \+ sys_elem_set(S, C), !.
post(sys_set_remove(V))
<= phaseout_posted(sys_set_agent(V, X, L, _, _)),
   sys_lin_agent(_, X, L, _), !.
/* Union Find */
post(sys_set_remove(V)),
post(sys_set_ref(V, D, S, T))
<= phaseout_posted(sys_set_agent(V, _, L, S, T)),
   sys_set_waits(X, V),
   sys_bound_var(X),
   sys_melt_var(X, H),
   var(H), !,
   sys_fresh_var(H, Y),
   sys_pick_prod(B, X, L, E),
   sys_add_prod([B*Y], E, D).
/* Constant Elimination */
post(sys_set_agent(V, Z, [A*Z|D], W, U))
<= phaseout_posted(sys_set_agent(V, _, L, S, T)),
   phaseout(sys_set_waits(X, V)),
   sys_bound_var(X),
   sys_melt_var(X, C),
   integer(C), !,
   sys_pick_prod(B, X, L, [A*Z|D]),
   H is -B*C,
   sys_add_set(S, H, W),
   sys_add_range(T, H, U).
/* Hook Adding */
sys_melt_hook(X, sys_hook_set)
<= posted(sys_set_agent(V, _, _, _, _)),
   sys_set_waits(X, V).
/* Redundancy Elimination */
% sys_set_remove(V) <=
%    &- sys_set_agent(V, _, B, S, _), sys_bound_poly(B, R),
%    sys_inter_set(S, [R], W), W==[R], !.
/* Set Diffusion, Directed, Bounds */
post(sys_in(X, L, U))
<= posted(sys_set_agent(_, _, [A*X|B], _, T)),
   T \== ...,
   sys_bound_poly(B, Q),
   sys_flip_range(Q, R),
   sys_blur_range(R, T, S),
   (  sys_div_range(S, A, U)
   -> L = [U]
   ;  L = [],
      U = ...).
/* Variable Rename */
post(sys_set_remove(V)),
post(sys_set_ref(V, D, S, T))
<= posted(sys_var_set(X, Y)),
   sys_set_waits(X, V),
   phaseout(sys_set_agent(V, _, L, S, T)),
   sys_pick_prod(B, X, L, E),
   sys_add_prod([B*Y], E, D).
/* Constant Backpropagation */
post(sys_set_agent(V, Z, [A*Z|D], W, U))
<= posted(sys_const_set(X, C)),
   phaseout(sys_set_waits(X, V)),
   phaseout(sys_set_agent(V, _, L, S, T)),
   sys_pick_prod(B, X, L, [A*Z|D]),
   H is -B*C,
   sys_add_set(S, H, W),
   sys_add_range(T, H, U).
/* Redundancy Backpropagation */
% sys_set_remove(V) <=
%    sys_in(Y, _, E), E\==(...), sys_set_waits(Y, V), &- sys_set_agent(V, _, B, S, _),
%    sys_pick_prod(C, Y, B, D), sys_rampup_poly(D, C, E, R), sys_inter_set(S, [R], W), W==[R].
/* Set Update, Directed, Bounds */
post(sys_in(X, L, U))
<= posted(sys_in(Y, _, E)),
   E \== ...,
   sys_set_waits(Y, V),
   sys_set_agent(V, _, [A*X|B], _, T),
   T \== ...,
   Y \== X,
   sys_pick_prod(C, Y, B, D),
   sys_rampup_poly(D, C, E, Q),
   sys_flip_range(Q, R),
   sys_blur_range(R, T, S),
   (  sys_div_range(S, A, U)
   -> L = [U]
   ;  L = [],
      U = ...).

% residue:sys_current_eq(+Var, -Goal)
residue:sys_current_eq(V, set(L,S)) :-
   sys_clause_hook(V, sys_hook_set, _),
   sys_freeze_var(V, X),
   sys_set_waits(X, K),
   sys_set_agent(K, _, L, S, _).

% residue:sys_unwrap_eq(+Goal, -Goal)
residue:sys_unwrap_eq(set(L,S), [G|R], R) :-
   sys_pretty_in(S, L, G).

/**********************************************************/
/* Set Parsing                                            */
/**********************************************************/

% sys_expr_set(+Expr, -Set)
sys_expr_set(V, _) :-
   var(V),
   throw(error(instantiation_error,_)).

/**
 * I (finite):
 * An integer I represents a singleton set {I}.
 */
sys_expr_set(A, [A]) :-
   integer(A), !.

/**
 * I..J (finite):
 * If I and J are integers then the expression represents the
 * interval [I..J]. It is also possible that I takes the value
 * inf or that J takes the value sup. The expression then denotes
 * the corresponding half interval or even the full integer domain.
 */
sys_expr_set(A..B, S) :- !,
   sys_expr_range(A, B, S).

/**
 * A \/ B (finite):
 * If A and B are set expressions then the union is also a set expression.
 */
sys_expr_set(X\/Y, E) :- !,
   sys_expr_set(X, A),
   sys_expr_set(Y, B),
   sys_union_set(A, B, E).

/**
 * A /\ B (finite):
 * If A and B are set expressions then the intersection is also a set expression.
 */
sys_expr_set(X/\Y, E) :- !,
   sys_expr_set(X, A),
   sys_expr_set(Y, B),
   sys_inter_set(A, B, E).

/**
 * \ A (finite):
 * If A is a set expressions then the complement is also a set expression.
 */
sys_expr_set(\X, E) :- !,
   sys_expr_set(X, A),
   sys_comp_set(A, E).

sys_expr_set(A, _) :-
   throw(error(type_error(fd_set,A),_)).

% sys_expr_range(+Integer, +Integer, -Set)
:- private sys_expr_range/3.
sys_expr_range(V, _, _) :-
   var(V),
   throw(error(instantiation_error,_)).
sys_expr_range(_, V, _) :-
   var(V),
   throw(error(instantiation_error,_)).
sys_expr_range(A, B, L) :-
   integer(A),
   integer(B), !,
   (  sys_make_range(A, B, R)
   -> L = [R]
   ;  L = []).
sys_expr_range(inf, A, [..A]) :-
   integer(A), !.
sys_expr_range(A, sup, [A...]) :-
   integer(A), !.
sys_expr_range(inf, sup, [...]) :- !.
sys_expr_range(A, B, _) :-
   throw(error(type_error(fd_set,A..B),_)).

/************************************************/
/* Set Unparsing                                */
/************************************************/

% sys_pretty_set(+Set,-Expr)
sys_pretty_set([X|Y], Z) :-
   sys_pretty_range(X, R),
   sys_pretty2(Y, R, Z).

% sys_pretty2(+Set,+Range,-Expr)
:- private sys_pretty2/3.
sys_pretty2([], R, R).
sys_pretty2([X|Y], R, Z) :-
   sys_pretty_range(X, S),
   sys_pretty2(Y, R\/S, Z).

% sys_pretty_range(+Range,-Range)
:- private sys_pretty_range/2.
sys_pretty_range(..., inf..sup) :- !.
sys_pretty_range(A..., A..sup) :- !.
sys_pretty_range(..A, inf..A) :- !.
sys_pretty_range(A..B, A..B) :- !.
sys_pretty_range(A, A).

/************************************************/
/* Set Membership                               */
/************************************************/

% sys_elem_set(+Set, +Integer)
sys_elem_set([U|_], X) :-
   sys_elem_range(U, X), !.
sys_elem_set([U|Y], X) :-
   sys_upper_range(U, A)
-> (  A < X
   -> sys_elem_set(Y, X); fail); fail.

% sys_elem_range(+Range, +Integer)
:- private sys_elem_range/2.
sys_elem_range(..., _) :- !.
sys_elem_range(B..., A) :- !,
   B =< A.
sys_elem_range(..B, A) :- !,
   A =< B.
sys_elem_range(B..C, A) :- !,
   B =< A,
   A =< C.
sys_elem_range(B, A) :-
   A =:= B.

/************************************************/
/* Set Translation & Flip                       */
/************************************************/

% sys_add_set(+Set,+Integer,-Set)
% Smart version which awoids unnecessary addition
sys_add_set(S, 0, S) :- !.
sys_add_set(S, N, R) :-
   sys_add_set2(S, N, R).

% sys_add_set2(+Set,+Integer,-Set)
% Dumb version which does always add
:- private sys_add_set2/3.
sys_add_set2([], _, []).
sys_add_set2([U|X], A, [V|Y]) :-
   sys_add_range(U, A, V),
   sys_add_set2(X, A, Y).

% sys_add_range(+Range,+Integer,-Range)
sys_add_range(..., _, ...) :- !.
sys_add_range(..A, B, ..C) :- !,
   C is A+B.
sys_add_range(A..., B, C...) :- !,
   C is A+B.
sys_add_range(A..B, C, D..E) :- !,
   D is A+C,
   E is B+C.
sys_add_range(A, B, C) :-
   C is A+B.

% sys_flip_set(+Set,+Set,-Set)
sys_flip_set([], T, T).
sys_flip_set([U|X], Y, T) :-
   sys_flip_range(U, V),
   sys_flip_set(X, [V|Y], T).

% sys_flip_range(+Range,-Range)
sys_flip_range(..., ...) :- !.
sys_flip_range(..A, C...) :- !,
   C is -A.
sys_flip_range(A..., ..C) :- !,
   C is -A.
sys_flip_range(A..B, E..D) :- !,
   D is -A,
   E is -B.
sys_flip_range(A, C) :-
   C is -A.

/************************************************/
/* Set Intersection                             */
/************************************************/

% sys_inter_set(+Set, +Set, -Set)
sys_inter_set([], _, []).
sys_inter_set([_|_], [], []).
sys_inter_set([U|Y], [V|Z], [X|T]) :-
   sys_inter_range(U, V, X), !,
   (  sys_upper_range(U, A)
   -> (  sys_upper_range(V, B)
      -> (  A < B
         -> sys_inter_set(Y, [V|Z], T)
         ;  sys_inter_set([U|Y], Z, T))
      ;  T = Y)
   ;  T = Z).
sys_inter_set([U|Y], [V|Z], T) :-
   sys_upper_range(U, A)
-> (  sys_upper_range(V, B)
   -> (  A < B
      -> sys_inter_set(Y, [V|Z], T)
      ;  sys_inter_set([U|Y], Z, T))
   ;  sys_inter_set(Y, [V|Z], T))
;  sys_inter_set([U|Y], Z, T).

% sys_inter_range(+Range, +Range, -Range)
% Fails if the intersection is empty.
sys_inter_range(..., X, X) :- !.
sys_inter_range(X, ..., X) :- !.
sys_inter_range(A..., B..., C...) :- !,
   C is max(A,B).
sys_inter_range(A..., B..C, R) :- !,
   D is max(A,B),
   sys_make_range(D, C, R).
sys_inter_range(A..B, C..., R) :- !,
   D is max(A,C),
   sys_make_range(D, B, R).
sys_inter_range(A..., ..B, R) :- !,
   sys_make_range(A, B, R).
sys_inter_range(..A, B..., R) :- !,
   sys_make_range(B, A, R).
sys_inter_range(..A, ..B, ..C) :- !,
   C is min(A,B).
sys_inter_range(..A, B..C, R) :- !,
   D is min(A,C),
   sys_make_range(B, D, R).
sys_inter_range(A..B, ..C, R) :- !,
   D is min(B,C),
   sys_make_range(A, D, R).
sys_inter_range(A..B, C..D, R) :- !,
   E is max(A,C),
   F is min(B,D),
   sys_make_range(E, F, R).
sys_inter_range(A, B..., A) :- !,
   B =< A.
sys_inter_range(A..., B, B) :- !,
   A =< B.
sys_inter_range(A, ..B, A) :- !,
   A =< B.
sys_inter_range(..A, B, B) :- !,
   B =< A.
sys_inter_range(A, B..C, A) :- !,
   B =< A,
   A =< C.
sys_inter_range(A..B, C, C) :- !,
   A =< C,
   C =< B.
sys_inter_range(A, B, A) :-
   A =:= B.

/************************************************/
/* Set Union                                    */
/************************************************/

% sys_union_set(+Set, +Set, -Set)
:- private sys_union_set/3.
sys_union_set([], T, T) :- !.
sys_union_set(T, [], T).
sys_union_set([U|Y], [V|Z], T) :-
   sys_union_range(U, V, X), !,
   sys_more_set(Y, Z, X, T).
sys_union_set([U|Y], [V|Z], T) :-
   sys_upper_range(U, A)
-> (  sys_upper_range(V, B)
   -> (  A < B
      -> sys_union_set(Y, [V|Z], S),
         T = [U|S]
      ;  sys_union_set([U|Y], Z, S),
         T = [V|S])
   ;  sys_union_set(Y, [V|Z], S),
      T = [U|S])
;  sys_union_set([U|Y], Z, S),
   T = [V|S].

% sys_more_set(+Set, +Set, +Range, -Set)
:- private sys_more_set/4.
sys_more_set([], Y, X, T) :-
   sys_insert_set(Y, X, T).
sys_more_set([U|Y], [], X, T) :-
   sys_insert_set([U|Y], X, T).
sys_more_set([U|Y], [V|Z], S, T) :-
   sys_union_range(U, S, X), !,
   sys_more_set(Y, [V|Z], X, T).
sys_more_set([U|Y], [V|Z], S, T) :-
   sys_union_range(V, S, X), !,
   sys_more_set([U|Y], Z, X, T).
sys_more_set([U|Y], [V|Z], X, [X|T]) :-
   sys_union_set([U|Y], [V|Z], T).

% sys_insert_set(+Set, +Range, -Set)
:- private sys_insert_set/3.
sys_insert_set([U|Y], V, T) :-
   sys_union_range(U, V, X), !,
   sys_insert_set(Y, X, T).
sys_insert_set(T, X, [X|T]).

% sys_union_range(+Range, +Range, -Range)
% Fails if the union is multiple ranges.
:- private sys_union_range/3.
sys_union_range(..., _, ...) :- !.
sys_union_range(_, ..., ...) :- !.
sys_union_range(A..., B..., C...) :- !,
   C is min(A,B).
sys_union_range(A..., B..C, D...) :- !,
   A-1 =< C,
   D is min(A,B).
sys_union_range(A..B, C..., D...) :- !,
   C-1 =< B,
   D is min(A,C).
sys_union_range(A..., ..B, ...) :- !,
   A-1 =< B.
sys_union_range(..A, B..., ...) :- !,
   B-1 =< A.
sys_union_range(..A, ..B, ..C) :- !,
   C is max(A,B).
sys_union_range(..A, B..C, ..D) :- !,
   B-1 =< A,
   D is max(A,C).
sys_union_range(A..B, ..C, ..D) :- !,
   A-1 =< C,
   D is max(B,C).
sys_union_range(A..B, C..D, E..F) :- !,
   C-1 =< B,
   A-1 =< D,
   E is min(A,C),
   F is max(B,D).
sys_union_range(A, B..., B...) :-
   B =< A, !.
sys_union_range(A, B..., A...) :- !,
   A =:= B-1.
sys_union_range(A..., B, A...) :-
   A =< B, !.
sys_union_range(A..., B, B...) :- !,
   B =:= A-1.
sys_union_range(A, ..B, ..B) :-
   A =< B, !.
sys_union_range(A, ..B, ..A) :- !,
   A =:= B+1.
sys_union_range(..A, B, ..A) :-
   B =< A, !.
sys_union_range(..A, B, ..B) :- !,
   B =:= A+1.
sys_union_range(A, B..C, B..C) :-
   B =< A,
   A =< C, !.
sys_union_range(A, B..C, A..C) :-
   A =:= B-1, !.
sys_union_range(A, B..C, B..A) :- !,
   A =:= C+1.
sys_union_range(A..B, C, A..B) :-
   A =< C,
   C =< B, !.
sys_union_range(A..B, C, C..B) :-
   C =:= A-1, !.
sys_union_range(A..B, C, A..C) :- !,
   C =:= B+1.
sys_union_range(A, B, A) :-
   A =:= B, !.
sys_union_range(A, B, A..B) :-
   A =:= B-1, !.
sys_union_range(A, B, B..A) :-
   A =:= B+1.

/*******************************************************************/
/* Set Complement                                                  */
/*******************************************************************/

% sys_comp_set(+Set, -Set)
sys_comp_set([X|L], H) :-
   sys_lower_range(X, A)
-> B is A-1,
   (  sys_upper_range(X, C)
   -> D is C+1,
      sys_comp_rest(L, D, J)
   ;  J = []),
   H = [..B|J]
;  sys_upper_range(X, A)
-> B is A+1,
   sys_comp_rest(L, B, H)
;  H = [].
sys_comp_set([], [...]).

% sys_comp_rest(+Set, +Integer, -Set)
:- private sys_comp_rest/3.
sys_comp_rest([X|L], A, [R|H]) :-
   sys_lower_range(X, B),
   C is B-1,
   sys_make_range(A, C, R),
   (  sys_upper_range(X, D)
   -> E is D+1,
      sys_comp_rest(L, E, H)
   ;  H = []).
sys_comp_rest([], A, [A...]).

/************************************************/
/* Set Bounds                                   */
/************************************************/

% sys_bound_set(+Set, -Bound)
sys_bound_set(S, R) :-
   sys_lower_set(S, A)
-> (  sys_upper_set(S, B)
   -> sys_make_range(A, B, R)
   ;  R = A...)
;  sys_upper_set(S, B)
-> R = ..B
;  R = ... .

% sys_upper_set(+Set, -Integer)
% Fails if set does not have supremum
:- private sys_upper_set/2.
sys_upper_set([X], A) :- !,
   sys_upper_range(X, A).
sys_upper_set([_|Y], A) :-
   sys_upper_set(Y, A).

% sys_lower_set(+Set, -Integer)
% Fails if set does not have infimum
:- private sys_lower_set/2.
sys_lower_set([X|_], A) :-
   sys_lower_range(X, A).

/************************************************/
/* Set Division                                 */
/************************************************/

% sys_div_range(+Range, +Integer, -Range)
% Fails if resulting range is empty
% Smart version which avoids unnecessary division
sys_div_range(S, 1, S) :- !.
sys_div_range(S, -1, R) :- !,
   sys_flip_range(S, R).
sys_div_range(S, N, R) :-
   N > 0, !,
   sys_div_range2(S, N, R).
sys_div_range(S, N, R) :- !,
   sys_flip_range(S, H),
   J is -N,
   sys_div_range2(H, J, R).

% sys_div_range2(+Range, +Integer, -Range)
% Fails if resulting range is empty
% Dumb version which does always divide
:- private sys_div_range2/3.
sys_div_range2(..., _, ...) :- !.
sys_div_range2(A..., B, C...) :- !,
   C is (A+B-1)div B.
sys_div_range2(..A, B, ..C) :- !,
   C is A div B.
sys_div_range2(A..B, C, R) :- !,
   D is (A+C-1)div C,
   E is B div C,
   sys_make_range(D, E, R).
sys_div_range2(A, B, C) :-
   0 =:= A rem B,
   C is A//B.

% sys_div_set(+Set, +Integer, -Set)
% Smart version which avoids unnecessary division
sys_div_set(S, 1, S) :- !.
sys_div_set(S, -1, R) :- !,
   sys_flip_set(S, [], R).
sys_div_set(S, N, R) :-
   N > 0, !,
   sys_div_set2(S, N, R).
sys_div_set(S, N, R) :- !,
   sys_flip_set(S, [], H),
   J is -N,
   sys_div_set2(H, J, R).

% sys_div_set2(+Set,+Integer,-Set)
% Dumb version which does always divide
:- private sys_div_set2/3.
sys_div_set2([], _, []).
sys_div_set2([U|X], A, T) :-
   sys_div_range2(U, A, V), !,
   sys_div_set2(X, A, Y),
   sys_insert_set(Y, V, T).
sys_div_set2([_|X], A, Y) :-
   sys_div_set2(X, A, Y).

/************************************************/
/* Set Pumping                                  */
/************************************************/

% sys_pump_range(+Range, +Integer, -Range)
% Smart varsion that only pumps when necessary
sys_pump_range(R, 1, R) :- !.
sys_pump_range(R, -1, S) :- !,
   sys_flip_range(R, S).
sys_pump_range(R, C, S) :-
   C > 0, !,
   sys_pump_range2(R, C, S).
sys_pump_range(R, C, S) :-
   sys_flip_range(R, I),
   H is -C,
   sys_pump_range2(I, H, S).

% sys_pump_range2(+Range, +Integer, -Range)
% Dumb version that always pumps
:- private sys_pump_range2/3.
sys_pump_range2(..., _, ...) :- !.
sys_pump_range2(A..., B, C...) :- !,
   C is A*B.
sys_pump_range2(..A, B, ..C) :- !,
   C is A*B.
sys_pump_range2(A..B, C, D..E) :- !,
   D is A*C,
   E is B*C.
sys_pump_range2(A, B, C) :-
   C is A*B.

/************************************************/
/* Interval Arithmetic                          */
/************************************************/

% sys_blur_range(+Range, +Range, -Range)
% sys_blur_range(A, B, C): C = {a+b | a in A, b in B}
% Minkowski sum of two ranges
sys_blur_range(..., _, ...) :- !.
sys_blur_range(_, ..., ...) :- !.
sys_blur_range(A..., B..., C...) :- !,
   C is A+B.
sys_blur_range(A.._, B..., C...) :- !,
   C is A+B.
sys_blur_range(A..., B.._, C...) :- !,
   C is A+B.
sys_blur_range(_..., .._, ...) :- !.
sys_blur_range(.._, _..., ...) :- !.
sys_blur_range(..A, ..B, ..C) :- !,
   C is A+B.
sys_blur_range(_..A, ..B, ..C) :- !,
   C is A+B.
sys_blur_range(..A, _..B, ..C) :- !,
   C is A+B.
sys_blur_range(A..B, C..D, E..F) :- !,
   E is A+C,
   F is B+D.
sys_blur_range(A, B..., C...) :- !,
   C is A+B.
sys_blur_range(A..., B, C...) :- !,
   C is A+B.
sys_blur_range(A, ..B, ..C) :- !,
   C is A+B.
sys_blur_range(..A, B, ..C) :- !,
   C is A+B.
sys_blur_range(A, B..C, D..E) :- !,
   D is A+B,
   E is A+C.
sys_blur_range(A..B, C, D..E) :- !,
   D is A+C,
   E is B+C.
sys_blur_range(A, B, C) :-
   C is A+B.

% sys_cross_range(+Range, +Range, -Range)
% sys_cross_range(A, B, C): C >= {a*b | a in A, b in B}
% Interval Arithmetic Multiplication
sys_cross_range(..., 0, 0) :- !.
sys_cross_range(..., _, ...) :- !.
sys_cross_range(0, ..., 0) :- !.
sys_cross_range(_, ..., ...) :- !.
sys_cross_range(A..., B..., C...) :-
   A >= 0,
   B >= 0, !,
   C is A*B.
sys_cross_range(_..., _..., ...) :- !.
sys_cross_range(A..B, C..., D...) :-
   A >= 0, !,
   D is min(A*C,B*C).
sys_cross_range(A..B, C..., ..D) :-
   B =< 0, !,
   D is max(A*C,B*C).
sys_cross_range(_.._, _..., ...) :- !.
sys_cross_range(A..., B..C, D...) :-
   B >= 0, !,
   D is min(A*B,A*C).
sys_cross_range(A..., B..C, ..D) :-
   C =< 0, !,
   D is max(A*B,A*C).
sys_cross_range(_..., _.._, ...) :- !.
sys_cross_range(A..., ..B, ..C) :-
   A >= 0,
   B =< 0, !,
   C is A*B.
sys_cross_range(_..., .._, ...) :- !.
sys_cross_range(..A, B..., ..C) :-
   A =< 0,
   B >= 0, !,
   C is A*B.
sys_cross_range(.._, _..., ...) :- !.
sys_cross_range(..A, ..B, ..C) :-
   A =< 0,
   B =< 0, !,
   C is A*B.
sys_cross_range(.._, .._, ...) :- !.
sys_cross_range(A..B, ..C, ..D) :-
   A >= 0, !,
   D is max(A*C,B*C).
sys_cross_range(A..B, ..C, D...) :-
   B =< 0, !,
   D is min(A*C,B*C).
sys_cross_range(_.._, .._, ...) :- !.
sys_cross_range(..A, B..C, ..D) :-
   B >= 0, !,
   D is max(A*B,A*C).
sys_cross_range(..A, B..C, D...) :-
   C =< 0, !,
   D is min(A*B,A*C).
sys_cross_range(.._, _.._, ...) :- !.
sys_cross_range(A..B, C..D, E..F) :- !,
   X is A*C,
   Y is A*D,
   Z is B*C,
   T is B*D,
   E is min(min(X,Y),min(Z,T)),
   F is max(max(X,Y),max(Z,T)).
sys_cross_range(0, _..., 0) :- !.
sys_cross_range(A, B..., C...) :-
   A > 0, !,
   C is A*B.
sys_cross_range(A, B..., ..C) :- !,
   C is A*B.
sys_cross_range(_..., 0, 0) :- !.
sys_cross_range(A..., B, C...) :-
   B > 0, !,
   C is A*B.
sys_cross_range(A..., B, ..C) :- !,
   C is A*B.
sys_cross_range(0, .._, 0) :- !.
sys_cross_range(A, ..B, ..C) :-
   A > 0, !,
   C is A*B.
sys_cross_range(A, ..B, C...) :- !,
   C is A*B.
sys_cross_range(.._, 0, 0) :- !.
sys_cross_range(..A, B, ..C) :-
   B > 0, !,
   C is A*B.
sys_cross_range(..A, B, C...) :- !,
   C is A*B.
sys_cross_range(0, _.._, 0) :- !.
sys_cross_range(A, B..C, D..E) :- !,
   X is A*B,
   Y is A*C,
   D is min(X,Y),
   E is max(X,Y).
sys_cross_range(_.._, 0, 0) :- !.
sys_cross_range(A..B, C, D..E) :- !,
   X is A*C,
   Y is B*C,
   D is min(X,Y),
   E is max(X,Y).
sys_cross_range(A, B, C) :-
   C is A*B.

% sys_slash_range(+Range, +Range, -Range)
% sys_slash_range(A, B, C): C >= {c | a in A, b in B, a = b*c}
% Interval Arithmetic Division
% Fails if the output range is empty
sys_slash_range(..., _, ...) :- !.

sys_slash_range(_..., ..., ...) :- !.
sys_slash_range(A..B, ..., ...) :-
   A =< 0,
   0 =< B, !.
sys_slash_range(A..B, ..., X..B) :-
   A > 0, !,
   X is -B.
sys_slash_range(A.._, ..., A..X) :-
   X is -A.
sys_slash_range(.._, ..., ...) :- !.
sys_slash_range(0, ..., ...) :- !.
sys_slash_range(A, ..., X..A) :-
   A > 0, !,
   X is -A.
sys_slash_range(A, ..., A..X) :-
   X is -A.

sys_slash_range(A..., C..., ...) :-
   A =< 0,
   C =< 0, !.
sys_slash_range(A..., C..., E...) :-
   A =< 0, !,
   E is (A+C-1)div C.
sys_slash_range(_..., C..., 1...) :-
   C >= 0, !.
sys_slash_range(_..., _..., ...) :- !.

sys_slash_range(A..B, C..., ...) :-
   B >= 0,
   A =< 0,
   C =< 0, !.
sys_slash_range(A..B, C..., R) :-
   B >= 0,
   A =< 0, !,
   E is (A+C-1)div C,
   F is B div C,
   sys_make_range(E, F, R).
sys_slash_range(A..B, C..., R) :-
   A > 0,
   C >= 0, !,
   X is max(C,1),
   F is B div X,
   sys_make_range(1, F, R).
sys_slash_range(A.._, C..., R) :-
   C >= 0, !,
   X is max(C,1),
   E is (A+X-1)div X,
   sys_make_range(E, -1, R).
sys_slash_range(A..B, _..., X..B) :-
   A > 0, !,
   X is -B.
sys_slash_range(A.._, _..., A..X) :- !,
   X is -A.

sys_slash_range(A..., C..D, ...) :-
   A =< 0,
   D >= 0,
   C =< 0, !.
sys_slash_range(A..., C.._, E...) :-
   A =< 0,
   C > 0, !,
   E is (A+C-1)div C.
sys_slash_range(A..., _..D, ..F) :-
   A =< 0, !,
   F is -((A-D-1)div-D).
sys_slash_range(A..., C..D, E...) :-
   C >= 0, !,
   E is max((A+D-1)div D,1).
sys_slash_range(A..., C..D, ..F) :-
   D =< 0, !,
   F is min(-((A-C-1)div-C),-1).
sys_slash_range(_..., _.._, ...) :- !.

sys_slash_range(A..., ..D, ...) :-
   A =< 0,
   D >= 0, !.
sys_slash_range(A..., ..D, ..F) :-
   A =< 0, !,
   F is -((A-D-1)div-D).
sys_slash_range(_..., ..D, .. -1) :-
   D =< 0, !.
sys_slash_range(_..., .._, ...) :- !.

sys_slash_range(..B, C..., ...) :-
   B >= 0,
   C =< 0, !.
sys_slash_range(..B, C..., ..F) :-
   B >= 0, !,
   F is B div C.
sys_slash_range(.._, C..., .. -1) :-
   C >= 0, !.
sys_slash_range(.._, _..., ...) :- !.

sys_slash_range(..B, ..D, ...) :-
   B >= 0,
   D >= 0, !.
sys_slash_range(..B, ..D, E...) :-
   B >= 0, !,
   E is -(B div-D).
sys_slash_range(.._, ..D, 1...) :-
   D =< 0, !.
sys_slash_range(.._, .._, ...) :- !.

sys_slash_range(A..B, ..D, ...) :-
   B >= 0,
   A =< 0,
   D >= 0, !.
sys_slash_range(A..B, ..D, R) :-
   B >= 0,
   A =< 0, !,
   E is -(B div-D),
   F is -((A-D-1)div-D),
   sys_make_range(E, F, R).
sys_slash_range(A..B, ..D, R) :-
   A > 0,
   D =< 0, !,
   X is min(D,-1),
   E is -(B div-X),
   sys_make_range(E, -1, R).
sys_slash_range(A.._, ..D, R) :-
   D =< 0, !,
   X is min(D,-1),
   F is -((A-X-1)div-X),
   sys_make_range(1, F, R).
sys_slash_range(A..B, .._, X..B) :-
   A > 0, !,
   X is -B.
sys_slash_range(A.._, .._, A..X) :- !,
   X is -A.

sys_slash_range(..B, C..D, ...) :-
   B >= 0,
   D >= 0,
   C =< 0, !.
sys_slash_range(..B, C.._, ..F) :-
   B >= 0,
   C > 0, !,
   F is B div C.
sys_slash_range(..B, _..D, E...) :-
   B >= 0, !,
   E is -(B div-D).
sys_slash_range(..B, C..D, ..F) :-
   C >= 0, !,
   F is min(B div D,-1).
sys_slash_range(..B, C..D, E...) :-
   D =< 0, !,
   E is max(-(B div-C),1).
sys_slash_range(.._, _.._, ...) :- !.

sys_slash_range(A..B, C..D, ...) :-
   B >= 0,
   A =< 0,
   D >= 0,
   C =< 0, !.
sys_slash_range(A..B, C.._, R) :-
   B >= 0,
   A =< 0,
   C > 0, !,
   E is (A+C-1)div C,
   F is B div C,
   sys_make_range(E, F, R).
sys_slash_range(A..B, _..D, R) :-
   B >= 0,
   A =< 0, !,
   E is -(B div-D),
   F is -((A-D-1)div-D),
   sys_make_range(E, F, R).
sys_slash_range(A..B, C..D, R) :-
   A > 0,
   C >= 0, !,
   X is max(C,1),
   E is max((A+D-1)div D,1),
   F is B div X,
   sys_make_range(E, F, R).
sys_slash_range(A..B, C..D, R) :-
   C >= 0, !,
   X is max(C,1),
   E is (A+X-1)div X,
   F is min(B div D,-1),
   sys_make_range(E, F, R).
sys_slash_range(A..B, C..D, R) :-
   A > 0,
   D =< 0, !,
   X is min(D,-1),
   E is -(B div-X),
   F is min(-((A-C-1)div-C),-1),
   sys_make_range(E, F, R).
sys_slash_range(A..B, C..D, R) :-
   D =< 0, !,
   X is min(D,-1),
   E is max(-(B div-C),1),
   F is -((A-X-1)div-X),
   sys_make_range(E, F, R).
sys_slash_range(A..B, _.._, X..B) :-
   A > 0, !,
   X is -B.
sys_slash_range(A.._, _.._, A..X) :- !,
   X is -A.

sys_slash_range(0, A..., ...) :-
   A =< 0, !.
sys_slash_range(0, _..., 0) :- !.
sys_slash_range(A, B..., R) :-
   A > 0,
   B >= 0, !,
   X is max(B,1),
   E is A div X,
   sys_make_range(1, E, R).
sys_slash_range(A, B..., R) :-
   B >= 0, !,
   X is max(B,1),
   D is (A+X-1)div X,
   sys_make_range(D, -1, R).
sys_slash_range(A, _..., X..A) :-
   A > 0, !,
   X is -A.
sys_slash_range(A, _..., A..X) :- !,
   X is -A.

sys_slash_range(A..., 0, ...) :-
   A =< 0, !.
sys_slash_range(_..., 0, _) :- !, fail.
sys_slash_range(A..., B, C...) :-
   B > 0, !,
   C is (A+B-1)div B.
sys_slash_range(A..., B, ..C) :- !,
   C is (-A-B-1)div-B.

sys_slash_range(0, ..B, ...) :-
   B >= 0, !.
sys_slash_range(0, .._, 0) :- !.
sys_slash_range(A, ..C, R) :-
   A > 0,
   C =< 0, !,
   X is min(C,-1),
   D is -(A div-X),
   sys_make_range(D, -1, R).
sys_slash_range(A, ..C, R) :-
   C =< 0, !,
   X is min(C,-1),
   E is -((A-X-1)div-X),
   sys_make_range(1, E, R).
sys_slash_range(A, .._, X..A) :-
   A > 0, !,
   X is -A.
sys_slash_range(A, .._, A..X) :- !,
   X is -A.

sys_slash_range(..A, 0, ...) :-
   A >= 0, !.
sys_slash_range(.._, 0, _) :- !, fail.
sys_slash_range(..A, B, ..C) :-
   B > 0, !,
   C is A div B.
sys_slash_range(..A, B, C...) :- !,
   C is -A div-B.

sys_slash_range(0, A..B, ...) :-
   B >= 0,
   A =< 0, !.
sys_slash_range(0, _.._, 0) :- !.
sys_slash_range(A, B..C, R) :-
   A > 0,
   B >= 0, !,
   X is max(B,1),
   D is max((A+C-1)div C,1),
   E is A div X,
   sys_make_range(D, E, R).
sys_slash_range(A, B..C, R) :-
   B >= 0, !,
   X is max(B,1),
   D is (A+X-1)div X,
   E is min(A div C,-1),
   sys_make_range(D, E, R).
sys_slash_range(A, B..C, R) :-
   A > 0,
   C =< 0, !,
   X is min(C,-1),
   D is -(A div-X),
   E is min(-((A-B-1)div-B),-1),
   sys_make_range(D, E, R).
sys_slash_range(A, B..C, R) :-
   C =< 0, !,
   X is min(C,-1),
   D is max(-(A div-B),1),
   E is -((A-X-1)div-X),
   sys_make_range(D, E, R).
sys_slash_range(A, _.._, X..A) :-
   A > 0, !,
   X is -A.
sys_slash_range(A, _.._, A..X) :- !,
   X is -A.

sys_slash_range(A..B, 0, ...) :-
   B >= 0,
   A =< 0, !.
sys_slash_range(_.._, 0, _) :- !, fail.
sys_slash_range(A..B, C, R) :-
   C > 0, !,
   D is (A+C-1)div C,
   E is B div C,
   sys_make_range(D, E, R).
sys_slash_range(A..B, C, R) :- !,
   D is (-B-C-1)div-C,
   E is -A div-C,
   sys_make_range(D, E, R).

sys_slash_range(0, 0, ...) :- !.
sys_slash_range(_, 0, _) :- !, fail.
sys_slash_range(A, B, C) :-
   0 =:= A rem B,
   C is A//B.

/************************************************/
/* Multiplication Special Cases                 */
/************************************************/

% sys_square_range(+Range, -Range)
% sys_square_range(A, B): B >= {b | a in A, b = a*a}
% Interval Arithmetic Square
sys_square_range(..., 0...) :- !.
sys_square_range(A..., 0...) :-
   A =< 0, !.
sys_square_range(A..., B...) :- !,
   B is A*A.
sys_square_range(..A, 0...) :-
   A >= 0, !.
sys_square_range(..A, B...) :- !,
   B is A*A.
sys_square_range(A..B, 0..C) :-
   A =< 0,
   B >= 0, !,
   C is max(A*A,B*B).
sys_square_range(A..B, C..D) :-
   A > 0, !,
   C is A*A,
   D is B*B.
sys_square_range(A..B, C..D) :- !,
   C is B*B,
   D is A*A.
sys_square_range(A, B) :-
   B is A*A.

% sys_root_range(+Range, -Range)
% sys_root_range(A, B): B >= {b | a in A, a = b*b}
% Interval Arithmetic Root
% Fails if the output range is empty
sys_root_range(..., ...) :- !.
sys_root_range(_..., ...) :- !.
sys_root_range(..0, 0) :- !.
sys_root_range(..A, _) :-
   A < 0, !, fail.
sys_root_range(..A, H..B) :- !,
   B is isqrt(A),
   H is -B.
sys_root_range(_..0, 0) :- !.
sys_root_range(_..A, _) :-
   A < 0, !, fail.
sys_root_range(_..A, H..B) :- !,
   B is isqrt(A),
   H is -B.
sys_root_range(0, 0) :- !.
sys_root_range(A, _) :-
   A < 0, !, fail.
sys_root_range(A, H..B) :-
   sqrtrem(A, B, R),
   R =:= 0,
   H is -B.

% sys_prem_range(+Range, -Range)
% sys_prem_range(A, B): B >= {b | a in A, b = b*a}
% Interval Arithmetic Premisse
sys_prem_range(..., ...) :- !.
sys_prem_range(..A, ...) :-
   A >= 1, !.
sys_prem_range(.._, 0) :- !.
sys_prem_range(A..., ...) :-
   A =< 1, !.
sys_prem_range(_..., 0) :- !.
sys_prem_range(A..B, ...) :-
   A =< 1,
   B >= 1, !.
sys_prem_range(_.._, 0) :- !.
sys_prem_range(1, ...) :- !.
sys_prem_range(_, 0) :- !.

% sys_conc_range(+Range, -Range)
% sys_conc_range(A, B): B >= {b | a in A, a = a*b}
% Interval Arithmetic Conclusion
sys_conc_range(..., ...) :- !.
sys_conc_range(..A, ...) :-
   A >= 0, !.
sys_conc_range(.._, 1) :- !.
sys_conc_range(A..., ...) :-
   A =< 0, !.
sys_conc_range(_..., 1) :- !.
sys_conc_range(A..B, ...) :-
   A =< 0,
   B >= 0, !.
sys_conc_range(_.._, 1) :- !.
sys_conc_range(0, ...) :- !.
sys_conc_range(_, 1) :- !.

/************************************************/
/* Absolute Function                            */
/************************************************/

% sys_abs_range(+Range, -Range)
% sys_abs_range(A, B): B >= {b | a in A, b = abs(a)}
% Interval Arithmetic Absolute
sys_abs_range(..., 0...) :- !.
sys_abs_range(A..., 0...) :-
   A =< 0, !.
sys_abs_range(A..., B...) :- !,
   B is A.
sys_abs_range(..A, 0...) :-
   A >= 0, !.
sys_abs_range(..A, B...) :- !,
   B is -A.
sys_abs_range(A..B, 0..C) :-
   A =< 0,
   B >= 0, !,
   C is max(-A,B).
sys_abs_range(A..B, A..B) :-
   A > 0, !.
sys_abs_range(A..B, C..D) :- !,
   C is -B,
   D is -A.
sys_abs_range(A, B) :-
   B is abs(A).

% sys_invabs_range(+Range, -Range)
% sys_invabs_range(A, B): B >= {b | a in A, a = abs(b)}
% Interval Arithmetic Root
% Fails if the output range is empty
sys_invabs_range(..., ...) :- !.
sys_invabs_range(_..., ...) :- !.
sys_invabs_range(..0, 0) :- !.
sys_invabs_range(..A, _) :-
   A < 0, !, fail.
sys_invabs_range(..A, H..A) :- !,
   H is -A.
sys_invabs_range(_..0, 0) :- !.
sys_invabs_range(_..A, _) :-
   A < 0, !, fail.
sys_invabs_range(_..A, H..A) :- !,
   H is -A.
sys_invabs_range(0, 0) :- !.
sys_invabs_range(A, _) :-
   A < 0, !, fail.
sys_invabs_range(A, H..A) :-
   H is -A.

/************************************************/
/* Range API                                    */
/************************************************/

% sys_make_range(+Integer, +Integer, -Range)
% Fails if the range is empty.
sys_make_range(A, B, A..B) :-
   A < B, !.
sys_make_range(A, B, A) :-
   A =:= B.

% sys_lower_range(+Range, -Integer)
% Fails if range does not have infimum
sys_lower_range(..., _) :- !, fail.
sys_lower_range(A..., A) :- !.
sys_lower_range(.._, _) :- !, fail.
sys_lower_range(A.._, A) :- !.
sys_lower_range(A, A).

% sys_upper_range(+Range, -Integer)
% Fails if range does not have supremum
sys_upper_range(..., _) :- !, fail.
sys_upper_range(_..., _) :- !, fail.
sys_upper_range(..A, A) :- !.
sys_upper_range(_..A, A) :- !.
sys_upper_range(A, A).
