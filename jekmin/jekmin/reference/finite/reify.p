/**
 * As a further convenience we also provide reification. Reification comes
 * with a set of Boolean operators (#\)/1, (#\/)/2, etc.. and the possibility
 * to embed membership (in)/2 and equalities respectively inequalities (#=)/2,
 * (#>)/2, etc.. in Boolean expressions. The constraint solver main use case
 * is as follows. The reified variable should on one hand allow firing the
 * constraint, but it should also reflect the entailed of the constraint.
 *
 * Examples:
 * ?- X #< 100-Y #<==> B, B = 0.
 * B = 0,
 * X #> -Y+99
 *
 * ?- X #< 100-Y #<==> B, Y = 74, X = 25.
 * X = 25,
 * Y = 74,
 * B = 1
 *
 * Under the hood the reification is implemented via a couple of new guarded
 * constraints. We find the guarded domain range, the guarded equality and
 * the guarded membership. The latter two guarded constraints are based on
 * the scalar product. Each reified constraint is modelled by a pair of guarded
 * constraints. At the moment only the main use case is implemented, so we
 * don’t find yet interactions of the guarded constraints.
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
:- use_package(library(jekpro/frequent/misc)).

:- module(reify, []).
:- use_module(library(basic/lists)).
:- use_module(library(minimal/assume)).
:- use_module(library(minimal/hypo)).
:- use_module(library(minimal/delta)).
:- use_module(library(misc/residue)).
:- use_module(library(experiment/trail)).
:- use_module(library(experiment/attr)).
:- use_module(library(experiment/cont)).
:- use_module(library(basic/random)).
:- use_module(helper).
:- use_module(intset).
:- use_module(linform).

:- public infix(#<==>).
:- op(760, xfx, #<==>).

:- public infix(#==>).
:- op(760, xfx, #==>).

:- public infix(#<==).
:- op(760, xfx, #<==).

:- public infix(#\/).
:- op(740, yfx, #\/).

:- public infix(#/\).
:- op(720, yfx, #/\).

:- public prefix(#\).
:- op(700, fy, #\).

% sys_in(+Wrap, +Set, +Bound)
% sys_in(X,S,_) = X in S
:- multifile intset:sys_in/3.
:- thread_local intset:sys_in/3.
:- multifile intset:sys_in/4.
% sys_lin_agent(+Ref, +Wrap, +Prod, +Integer)
:- multifile linform:sys_lin_agent/4.
:- thread_local linform:sys_lin_agent/4.
:- multifile linform:sys_lin_agent/5.
% sys_set_agent(+Ref, , +Wrap, +Prod, +Set, +Bound)
:- multifile intset:sys_set_agent/5.
:- thread_local intset:sys_set_agent/5.
:- multifile intset:sys_set_agent/6.

/*******************************************************************/
/* Compile Cnf                                                     */
/*******************************************************************/

/**
 * A #==> B:
 * If A and B are Boolean expressions then the implication is posted.
 */
% (Bool #==> Bool)
:- public #==> /2.
:- meta_predicate 0 #==> 0.
A #==> B :-
   sys_reify_expr(A #==> B, 1).

/**
 * A #<== B:
 * If A and B are Boolean expressions then the converse implication is posted.
 */
% (Bool #<== Bool)
:- public #<== /2.
:- meta_predicate 0 #<== 0.
A #<== B :-
   sys_reify_expr(A #<== B, 1).

/**
 * A #<==> B:
 * If A and B are Boolean expressions then the bi-implication is posted.
 */
% (Bool #<==> Bool)
:- public #<==> /2.
:- meta_predicate 0 #<==> 0.
A #<==> B :-
   sys_reify_expr(A #<==> B, 1).

/**
 * A #\/ B:
 * If A and B are Boolean expressions then the disjunction is posted.
 */
% (Bool #\/ Bool)
:- public #\/ /2.
:- meta_predicate 0 #\/ 0.
A #\/ B :-
   sys_reify_expr(A #\/ B, 1).

/**
 * A #/\ B:
 * If A and B are Boolean expressions then the conjunction is posted.
 */
% (Bool #/\ Bool)
:- public #/\ /2.
:- meta_predicate 0 #/\ 0.
A #/\ B :-
   sys_reify_expr(A #/\ B, 1).

/**
 * #\ A:
 * If A is a Boolean expression then the negation is posted.
 */
% (#\ Bool)
:- public (#\)/1.
:- meta_predicate #\ 0.
#\ A :-
   sys_reify_expr(#\ A, 1).

/*******************************************************************/
/* Reify Expr                                                      */
/*******************************************************************/

/**
 * sys_reify_expr(E, B):
 * The predicate succeeds for a reification of the Boolean
 * expression E that yields the Boolean value B.
 */
% sys_reify_expr(+BoolExpr, -BoolVal)
:- private sys_reify_expr/2.
sys_reify_expr(A, B) :- var(A), !, B = A, A in 0..1.
sys_reify_expr(A #<==> B, C) :- !,
   sys_reify_expr(A, H),
   sys_reify_expr(B, J),
   sys_reify_lin(H, J, C).
sys_reify_expr(A #<== B, C) :- !,
   sys_reify_expr(A, H),
   sys_reify_expr(B, J),
   sys_reify_set(J, H, [..0], C).
sys_reify_expr(A #==> B, C) :- !,
   sys_reify_expr(A, H),
   sys_reify_expr(B, J),
   sys_reify_set(J, H, [0...], C).
sys_reify_expr(A #\/ B, C) :- !,
   sys_reify_expr(A, H),
   sys_reify_expr(B, J),
   sys_reify_set(H, -J, [.. -1, 1...], C).
sys_reify_expr(A #/\ B, C) :- !,
   sys_reify_expr(A, H),
   sys_reify_expr(B, J),
   sys_reify_lin(1-H, J-1, C).
sys_reify_expr(#\ A, C) :- !,
   sys_reify_expr(A, B),
   sys_reify_in(B, 0, C).
sys_reify_expr(0, B) :- !, B = 0.
sys_reify_expr(1, B) :- !, B = 1.
sys_reify_expr(X in U, B) :- !,
   sys_reify_in(X, U, B).
sys_reify_expr(X #= Y, B) :- !,
   sys_reify_lin(X, Y, B).
sys_reify_expr(X #\= Y, B) :- !,
   sys_reify_set(X, Y, [.. -1, 1...], B).
sys_reify_expr(X #< Y, B) :- !,
   sys_reify_set(Y, X, [1...], B).
sys_reify_expr(X #> Y, B) :- !,
   sys_reify_set(Y, X, [.. -1], B).
sys_reify_expr(X #=< Y, B) :- !,
   sys_reify_set(Y, X, [0...], B).
sys_reify_expr(X #>= Y, B) :- !,
   sys_reify_set(Y, X, [..0], B).
sys_reify_expr(A, _) :-
   throw(error(type_error(fd_bool, A), _)).

/**
 * sys_reify_in(E, U, B):
 * Post a reified membership E in U #<==> B.
 * Will be translated into two guarded memberships.
 */
% sys_reify_in(+Expr, +Set, +Var)
:- private sys_reify_in/3.
sys_reify_in(X, U, B) :- var(B), !,
   sys_expr_set(U, S),
   sys_value_expr(X, P, C),
   H is -C,
   sys_add_set(S, H, T),
   sys_fresh_var(B, W),
   sys_comp_set(T, V),
   post(intset:sys_in(W, [0..1], 0..1)),
   post(sys_lot(P, T, W, 0)),
   post(sys_lot(P, V, W, 1)).
sys_reify_in(X, U, 1) :- !,
   X in U.
sys_reify_in(X, U, 0) :- !,
   X in \U.
sys_reify_in(_, _, B) :-
   throw(error(type_error(fd_bool, B), _)).

/**
 * sys_reify_lin(E, F, B):
 * Post a reified equivalence E #= F #<==> B.
 * Will be translated into a guarded equivalence and a guarded membership.
 */
% sys_reify_lin(+Expr, +Expr, +Var)
:- private sys_reify_lin/3.
sys_reify_lin(X, Y, B) :- var(B), !,
   sys_value_expr(Y, P, C),
   sys_value_expr_inv(X, Q, D),
   K is D-C,
   sys_flip_prod(Q, J),
   sys_add_prod(J, P, E),
   sys_fresh_var(B, U),
   I is K-1,
   V is K+1,
   post(intset:sys_in(U, [0..1], 0..1)),
   post(sys_pit(E, K, U, 0)),
   post(sys_lot(E, [..I, V...], U, 1)).
sys_reify_lin(X, Y, 1) :- !,
   X #= Y.
sys_reify_lin(X, Y, 0) :- !,
   X #\= Y.
sys_reify_lin(_, _, B) :-
   throw(error(type_error(fd_bool, B), _)).

/**
 * sys_reify_set(E, F, S, B):
 * Post a reified comparison E S F #<==> B.
 * Will be translated into two guarded memberships.
 */
% sys_reify_set(+Expr, +Expr, +Set, +Var)
:- private sys_reify_set/4.
sys_reify_set(Y, X, S, B) :- var(B), !,
   sys_value_expr(Y, P, C),
   sys_value_expr_inv(X, Q, D),
   K is D-C,
   sys_flip_prod(Q, J),
   sys_add_prod(J, P, E),
   sys_add_set(S, K, T),
   sys_fresh_var(B, U),
   sys_comp_set(T, V),
   post(intset:sys_in(U, [0..1], 0..1)),
   post(sys_lot(E, T, U, 0)),
   post(sys_lot(E, V, U, 1)).
sys_reify_set(Y, X, S, 1) :- !,
   sys_compare_expr(Y, X, S).
sys_reify_set(Y, X, S, 0) :- !,
   sys_comp_set(S, T),
   sys_compare_expr(Y, X, T).
sys_reify_set(_, _, _, B) :-
   throw(error(type_error(fd_bool, B), _)).

/*******************************************************************/
/* Guarded Domain Constraints                                      */
/*******************************************************************/

% sys_hook_at(+Var, +Term)
:- private sys_hook_at/2.
sys_hook_at(V, W) :- var(W), !,
   sys_freeze_var(V, R),
   sys_fresh_var(W, S),
   sys_assume_cont(post(sys_var_at(R, S))).
sys_hook_at(V, T) :- integer(T), !,
   sys_freeze_var(V, R),
   sys_assume_cont(post(sys_const_at(R, T))).
sys_hook_at(_, T) :-
   throw(error(type_error(integer, T), _)).

% sys_var_at(+Wrap, +Wrap)
% sys_var_at(X, Y) = X = Y
:- private sys_var_at/2.
:- thread_local sys_var_at/2.
:- private sys_var_at/3.

/* Union Find */
true <=
   phaseout_posted(sys_var_at(_, _)).

% sys_const_at(+Wrap, +Integer)
% sys_const_at(X, C) = X = C
:- private sys_const_at/2.
:- thread_local sys_const_at/2.
:- private sys_const_at/3.

/* Constant Elimination */
true <=
   phaseout_posted(sys_const_at(_, _)).

% sys_at(+Wrap, +Set, +Bound, +Wrap, +Integer)
% sys_at(X,S,_,B,C) = X in S v B = C
:- thread_local sys_at/5.

/* Trivial Cases */
sys_melt_const(F, G) <=
   phaseout_posted(sys_at(_, [], _, F, G)), !.
/* Boolean Linear Trigger */
sys_melt_join(X, B) <=
   phaseout_posted(sys_at(X, [1], _, B, 0)), phaseout(sys_at(X, [0], _, B, 1)), !.
sys_melt_join(X, B) <=
   phaseout_posted(sys_at(X, [0], _, B, 1)), phaseout(sys_at(X, [1], _, B, 0)), !.
/* At & At intersection */
post(sys_at(X, W, R, B, C)) <=
   phaseout_posted(sys_at(X, S, P, B, C)), phaseout(sys_at(X, T, Q, B, C)),
   (  sys_inter_range(P, Q, R)
   -> sys_inter_set(S, T, W)
   ;  W = [], R = ...),
   W \== T, !.
true <=
   phaseout_posted(sys_at(X, _, _, B, C)), sys_at(X, _, _, B, C), !.
/* At & In intersection */
post(sys_at(X, W, R, B, C)) <=
   phaseout_posted(sys_at(X, S, P, B, C)), sys_in(X, T, Q),
   (  sys_inter_range(P, Q, R)
   -> sys_inter_set(S, T, W)
   ;  W = [], R = ...),
   W \== S, !.
/* In & At intersection */
post(sys_at(X, W, R, B, C)) <=
   posted(intset:sys_in(X, T, Q)), phaseout(sys_at(X, S, P, B, C)),
   (  sys_inter_range(P, Q, R)
   -> sys_inter_set(S, T, W)
   ;  W = [], R = ...),
   W \== S.
/* Union Find, Guard Included */
post(sys_at(Y, T, S, F, G)) <=
   phaseout_posted(sys_at(X, T, S, F, G)), sys_bound_var(X), sys_melt_var(X, H), var(H), !,
   sys_fresh_var(H, Y).
post(sys_at(X, T, S, Y, G)) <=
   phaseout_posted(sys_at(X, T, S, F, G)),
   sys_bound_var(F), sys_melt_var(F, H), var(H), !,
   sys_fresh_var(H, Y).
/* Constant Elimination, Guard Included */
sys_melt_const(F, G) <=
   phaseout_posted(sys_at(X, T, _, F, G)),
   sys_bound_var(X), sys_melt_var(X, C), integer(C), \+ sys_elem_set(T, C), !.
true <=
   phaseout_posted(sys_at(X, _, _, _, _)), sys_bound_var(X), sys_melt_var(X, C), integer(C), !.
post(intset:sys_in(X, T, B)) <=
   phaseout_posted(sys_at(X, T, B, F, G)), sys_bound_var(F), sys_melt_var(F, C), integer(C), C \== G, !.
true <=
   phaseout_posted(sys_at(_, _, _, F, _)), sys_bound_var(F), sys_melt_var(F, C), integer(C), !.
/* Hook Adding, Guard Included */
sys_melt_hook(X, sys_hook_at), sys_melt_hook(F, sys_hook_at) <=
   posted(sys_at(X, _, _, F, _)).
/* Set Diffusion, Directed, Bounds */
post(intset:sys_in(X, [U], U)) <=
   posted(sys_at(X, _, S, B, 0)), sys_at(X, _, T, B, 1),
   sys_union_bounds(S, T, U), U \== ... .
post(intset:sys_in(X, [U], U)) <=
   posted(sys_at(X, _, S, B, 1)), sys_at(X, _, T, B, 0),
   sys_union_bounds(S, T, U), U \== ... .
/* Variable Rename, Guard Included */
post(sys_at(Y, T, S, F, G)) <=
   posted(sys_var_at(X, Y)), phaseout(sys_at(X, T, S, F, G)).
post(sys_at(X, T, S, Y, G)) <=
   posted(sys_var_at(F, Y)), phaseout(sys_at(X, T, S, F, G)), F \== X.
/* Constant Backpropagation, Guard Included */
sys_melt_const(F, G) <=
   posted(sys_const_at(X, H)), phaseout(sys_at(X, T, _, F, G)), \+ sys_elem_set(T, H).
true <=
   posted(sys_const_at(X, H)), phaseout(sys_at(X, T, _, _, _)), sys_elem_set(T, H).
post(intset:sys_in(X, T, B)) <=
   posted(sys_const_at(F, H)), phaseout(sys_at(X, T, B, F, G)), F \== X, H \== G.
true <=
   posted(sys_const_at(F, H)), phaseout(sys_at(X, _, _, F, H)), F \== X.

% residue:sys_current_eq(+Var, -Handle)
:- public residue:sys_current_eq/2.
:- multifile residue:sys_current_eq/2.
:- discontiguous residue:sys_current_eq/2.
residue:sys_current_eq(V, in(X, S) #\/ D #= E) :-
   sys_clause_hook(V, sys_hook_at, _),
   sys_freeze_var(V, X),
   sys_at(X, S, _, D, E).

% residue:sys_unwrap_eq(+Handle, -Goals, +Goals)
:- public residue:sys_unwrap_eq/3.
:- multifile residue:sys_unwrap_eq/3.
:- discontiguous residue:sys_unwrap_eq/3.
residue:sys_unwrap_eq(in(X, S) #\/ P #= Q, [G #\/ R #= Q|L], L) :- !,
   sys_pretty_in(S, [1*X], G),
   sys_melt_var(P, R).

/*******************************************************************/
/* Guarded Linear Constraint                                       */
/*******************************************************************/

% sys_hook_pit(+Var, +Term)
:- private sys_hook_pit/2.
sys_hook_pit(V, W) :- var(W), !,
   sys_freeze_var(V, R),
   sys_fresh_var(W, S),
   sys_assume_cont(post(sys_var_pit(R, S))).
sys_hook_pit(V, T) :- integer(T), !,
   sys_freeze_var(V, R),
   sys_assume_cont(post(sys_const_pit(R, T))).
sys_hook_pit(_, T) :-
   throw(error(type_error(integer, T), _)).

% sys_var_pit(+Wrap, +Wrap)
% sys_var_pit(X, Y) = X = Y
:- private sys_var_pit/2.
:- thread_local sys_var_pit/2.
:- private sys_var_pit/3.

/* Union Find */
true <=
   phaseout_posted(sys_var_pit(_, _)).

% sys_const_pit(+Wrap, +Integer)
% sys_const_pit(X, C) = X = C
:- private sys_const_pit/2.
:- thread_local sys_const_pit/2.
:- private sys_const_pit/3.

/* Constant Elimination */
true <=
   phaseout_posted(sys_const_pit(_, _)).

% sys_pit(+Prod, +Integer, +Wrap, +Integer)
% sys_pit(P, I, B, C) = P = I v B = C
% sys_pit_ref(+Ref, +Prod, +Integer, +Wrap, +Integer)
:- private sys_pit_ref/6.
% sys_pit_waits(+Wrap, +Ref)
:- private sys_pit_waits/2.
:- thread_local sys_pit_waits/2.
% sys_pit_agent(+Ref, +Wrap, +Prod, +Integer, +Wrap, +Integer)
:- private sys_pit_agent/6.
:- thread_local sys_pit_agent/6.
:- private sys_pit_agent/7.

/* Create Surrogate */
post(sys_pit_ref(R, L, T, B, C)) <=
   phaseout_posted(sys_pit(L, T, B, C)), genref(R).

/* Trivial Cases */
sys_melt_const(B, C) <=
   phaseout_posted(sys_pit_ref(_, [], K, B, C)), K \== 0, !.
true <=
   phaseout_posted(sys_pit_ref(_, [], _, _, _)), !.
/* Agent start */
assumez(sys_pit_waits(X, R)) <=
   posted(sys_pit_ref(R, L, _, _, _)), member(_*X, L).
post(sys_pit_agent(R, X, [A*X|B], T, C, D)) <=
   phaseout_posted(sys_pit_ref(R, [A*X|B], T, C, D)).

% sys_pit_remove(+Ref)
% Remove helper
:- private sys_pit_remove/2.
true <=
   posted(sys_pit_remove(V)), phaseout(sys_pit_waits(_, V)).
true <=
   phaseout_posted(sys_pit_remove(_)).

/* GCD Normalization */
post(sys_pit_remove(V)), sys_melt_const(B, C) <=
   phaseout_posted(sys_pit_agent(V, _, P, T, B, C)), sys_gcd_prod(P, G), 0 =\= T rem G, !.
post(sys_pit_agent(V, X, R, H, B, C)) <=
   phaseout_posted(sys_pit_agent(V, X, P, T, B, C)), sys_gcd_prod(P, G), !,
   sys_div_prod(P, G, R), H is T//G.
post(sys_pit_agent(V, X, R, H, C, D)) <=
   phaseout_posted(sys_pit_agent(V, X, [A*X|B], T, C, D)), A < 0, !,
   sys_flip_prod([A*X|B], R), H is -T.
/* Unification Trigger */
post(sys_at(X, [T], T, B, C)) <=
   phaseout_posted(sys_pit_agent(V, X, [1*X], T, B, C)), !, phaseout(sys_pit_waits(X, V)).
/* Pit & Pit Intersection */
post(sys_pit_remove(W)), post(sys_pit_remove(V)), sys_melt_const(F, G) <=
   phaseout_posted(sys_pit_agent(W, X, L, C, F, G)), phaseout(sys_pit_agent(V, X, L, D, F, G)), C \== D, !.
post(sys_pit_remove(V)) <=
   phaseout_posted(sys_pit_agent(V, X, L, _, F, G)), sys_pit_agent(_, X, L, _, F, G), !.
/* Pit & Lot Intersection */
post(sys_pit_remove(W)), post(sys_lot_remove(V)), sys_melt_const(F, G) <=
   phaseout_posted(sys_pit_agent(W, X, L, C, F, G)), phaseout(sys_lot_agent(V, X, L, S, _, F, G)), \+ sys_elem_set(S, C), !.
post(sys_lot_remove(V)) <=
   posted(sys_pit_agent(_, X, L, _, F, G)), phaseout(sys_lot_agent(V, X, L, _, _, F, G)).
/* Pit & Set Intersection */
post(sys_pit_remove(V)), sys_melt_const(F, G) <=
   phaseout_posted(sys_pit_agent(V, X, L, C, F, G)), sys_set_agent(_, X, L, S, _), \+ sys_elem_set(S, C), !.
post(sys_pit_remove(V)) <=
   phaseout_posted(sys_pit_agent(V, X, L, _, _, _)), sys_set_agent(_, X, L, _, _), !.
/* Set & Pit Intersection */
post(sys_pit_remove(V)), sys_melt_const(F, G) <=
   posted(intset:sys_set_agent(_, X, L, S, _)), phaseout(sys_pit_agent(V, X, L, C, F, G)), \+ sys_elem_set(S, C).
post(sys_pit_remove(V)) <=
   posted(intset:sys_set_agent(_, X, L, S, _)), phaseout(sys_pit_agent(V, X, L, C, _, _)), sys_elem_set(S, C).
/* Pit & Lin Intersection */
post(sys_pit_remove(V)), sys_melt_const(F, G) <=
   phaseout_posted(sys_pit_agent(V, X, L, C, F, G)), sys_lin_agent(_, X, L, D), C \== D, !.
post(sys_pit_remove(V)) <=
   phaseout_posted(sys_pit_agent(V, X, L, _, _, _)), sys_lin_agent(_, X, L, _), !.
/* Lin & Pit Intersection */
post(sys_pit_remove(V)), sys_melt_const(F, G) <=
   posted(linform:sys_lin_agent(_, X, L, C)), phaseout(sys_pit_agent(V, X, L, D, F, G)), C \== D.
post(sys_pit_remove(V)) <=
   posted(linform:sys_lin_agent(_, X, L, C)), phaseout(sys_pit_agent(V, X, L, C, _, _)).
% not yet implemented
/* Union Find, Guard Included */
post(sys_pit_remove(V)), post(sys_pit_ref(V, D, T, F, G)) <=
   phaseout_posted(sys_pit_agent(V, _, L, T, F, G)), sys_pit_waits(X, V),
   sys_bound_var(X), sys_melt_var(X, H), var(H), !,
   sys_fresh_var(H, Y), sys_pick_prod(B, X, L, E), sys_add_prod([B*Y], E, D).
post(sys_pit_agent(V, X, L, T, Y, G)) <=
   phaseout_posted(sys_pit_agent(V, X, L, T, F, G)),
   sys_bound_var(F), sys_melt_var(F, H), var(H), !,
   sys_fresh_var(H, Y).
/* Constant Elimination, Guard Included */
post(sys_pit_agent(V, Z, [A*Z|D], H, F, G)) <=
   phaseout_posted(sys_pit_agent(V, _, L, T, F, G)), phaseout(sys_pit_waits(X, V)),
   sys_bound_var(X), sys_melt_var(X, C), integer(C), !,
   sys_pick_prod(B, X, L, [A*Z|D]), H is T-B*C.
post(sys_pit_remove(V)), post(clpfd:sys_lin_ref(V, L, T)) <=
   phaseout_posted(sys_pit_agent(V, _, L, T, F, G)),
   sys_bound_var(F), sys_melt_var(F, C), integer(C), C \== G, !.
post(sys_pit_remove(V)) <=
   phaseout_posted(sys_pit_agent(V, _, _, _, F, _)),
   sys_bound_var(F), sys_melt_var(F, C), integer(C), !.
/* Hook Adding, Guard Included */
sys_melt_hook(X, sys_hook_pit) <=
   posted(sys_pit_agent(V, _, _, _, _, _)), sys_pit_waits(X, V).
sys_melt_hook(F, sys_hook_pit) <=
   posted(sys_pit_agent(_, _, _, _, F, _)).
/* Set Diffusion, Directed, Bounds */
% needs adaptation
/* Variable Rename, Guard Included */
post(sys_pit_remove(V)), post(sys_pit_ref(V, D, T, F, G)) <=
   posted(sys_var_pit(X, Y)), sys_pit_waits(X, V), phaseout(sys_pit_agent(V, _, L, T, F, G)),
   sys_pick_prod(B, X, L, E), sys_add_prod([B*Y], E, D).
post(sys_pit_agent(V, X, L, T, Y, G)) <=
   posted(sys_var_pit(F, Y)), phaseout(sys_pit_agent(V, X, L, T, F, G)), \+ sys_pit_waits(F, V).
/* Constant Backpropagation, Guard Included */
post(sys_pit_agent(V, Z, [A*Z|D], H, F, G)) <=
   posted(sys_const_pit(X, C)),
   phaseout(sys_pit_waits(X, V)), phaseout(sys_pit_agent(V, _, L, T, F, G)),
   sys_pick_prod(B, X, L, [A*Z|D]), H is T-B*C.
post(sys_pit_remove(V)), post(clpfd:sys_lin_ref(V, L, T)) <=
   posted(sys_const_pit(F, H)),
   phaseout(sys_pit_agent(V, _, L, T, F, G)), \+ sys_pit_waits(F, V), H \== G.
post(sys_pit_remove(V)) <=
   posted(sys_const_pit(F, H)),
   phaseout(sys_pit_agent(V, _, _, _, F, H)), \+ sys_pit_waits(F, V).
/* Set Update, Directed, Bounds */
% needs adaptation

% residue:sys_current_eq(+Var, -Goal)
residue:sys_current_eq(V, L #= C #\/ D #= E) :-
   sys_clause_hook(V, sys_hook_pit, _),
   sys_freeze_var(V, X),
   sys_pit_waits(X, K),
   sys_pit_agent(K, _, L, C, D, E).

% residue:sys_unwrap_eq(+Goal, -Goal)
residue:sys_unwrap_eq([A*X|B] #= K #\/ P #= Q, [E #= F #\/ R #= Q|L], L) :- !,
   sys_pretty_lin([A*X], 0, E),
   sys_flip_prod(B, C),
   sys_pretty_lin(C, K, F),
   sys_melt_var(P, R).

/*******************************************************************/
/* Guarded Set Constraint                                          */
/*******************************************************************/

% sys_hook_lot(+Var, +Term)
:- private sys_hook_lot/2.
sys_hook_lot(V, W) :- var(W), !,
   sys_freeze_var(V, R),
   sys_fresh_var(W, S),
   sys_assume_cont(post(sys_var_lot(R, S))).
sys_hook_lot(V, T) :- integer(T), !,
   sys_freeze_var(V, R),
   sys_assume_cont(post(sys_const_lot(R, T))).
sys_hook_lot(_, T) :-
   throw(error(type_error(integer, T), _)).

% sys_var_lot(+Wrap, +Wrap)
% sys_var_lot(X, Y) = X = Y
:- private sys_var_lot/2.
:- thread_local sys_var_lot/2.
:- private sys_var_lot/3.

/* Union Find */
true <=
   phaseout_posted(sys_var_lot(_, _)).

% sys_const_lot(+Wrap, +Integer)
% sys_const_lot(X, C) = X = C
:- private sys_const_lot/2.
:- thread_local sys_const_lot/2.
:- private sys_const_lot/3.

/* Constant Elimination */
true <=
   phaseout_posted(sys_const_lot(_, _)).

% sys_lot(+Prod, +Set, +Wrap, +Integer)
% sys_lot(P, S, B, C) = P in S v B = C
% sys_lot_ref(+Ref, +Prod, +Set, +Bound, +Wrap, +Integer)
:- private sys_lot_ref/7.
% sys_lot_waits(+Wrap, +Ref)
:- private sys_lot_waits/2.
:- thread_local sys_lot_waits/2.
% sys_lot_agent(+Ref, , +Wrap, +Prod, +Set, +Bound, +Wrap, +Integer)
:- private sys_lot_agent/7.
:- thread_local sys_lot_agent/7.
:- private sys_lot_agent/8.

/* Create Surrogate */
post(sys_lot_ref(V, L, S, T, B, C)) <=
   phaseout_posted(sys_lot(L, S, B, C)), genref(V), sys_bound_set(S, T).

/* Trivial Cases */
sys_melt_const(B, C) <=
   phaseout_posted(sys_lot_ref(_, [], S, _, B, C)), \+ sys_elem_set(S, 0), !.
true <=
   phaseout_posted(sys_lot_ref(_, [], _, _, _, _)), !.
/* Agent start */
assumez(sys_lot_waits(X, V)) <=
   posted(sys_lot_ref(V, L, _, _, _, _)), member(_*X, L).
post(sys_lot_agent(V, X, [A*X|B], S, T, C, D)) <=
   phaseout_posted(sys_lot_ref(V, [A*X|B], S, T, C, D)).

% sys_lot_remove(+Ref)
% Remove helper
:- private sys_lot_remove/2.
true <=
   posted(sys_lot_remove(V)), phaseout(sys_lot_waits(_, V)).
true <=
   phaseout_posted(sys_lot_remove(_)).

% sys_lot_move(+Ref)
% Move helper
:- private sys_lot_move/1.
:- thread_local sys_lot_move/1.
:- private sys_lot_move/2.
assumez(sys_pit_waits(X, V)) <=
   posted(sys_lot_move(V)), phaseout(sys_lot_waits(X, V)).
true <=
   phaseout_posted(sys_lot_move(_)).

/* Degenerate Sets */
true <=
   phaseout_posted(sys_lot_agent(_, _, _, [...], _, _, _)), !.
post(sys_lot_remove(V)), sys_melt_const(B, C) <=
   phaseout_posted(sys_lot_agent(V, _, _, [], _, B, C)), !.
post(sys_lot_move(V)), post(sys_pit_agent(V, X, L, Y, B, C)) <=
   phaseout_posted(sys_lot_agent(V, X, L, [Y], _, B, C)), integer(Y), !.
/* GCD Normalization */
post(sys_lot_remove(V)), sys_melt_const(B, C) <=
   phaseout_posted(sys_lot_agent(V, _, P, _, U, B, C)), sys_gcd_prod(P, G), \+ sys_div_range(U, G, _), !.
post(sys_lot_agent(V, X, R, T, W, B, C)) <=
   phaseout_posted(sys_lot_agent(V, X, P, S, U, B, C)), sys_gcd_prod(P, G), !,
   sys_div_prod(P, G, R), sys_div_set(S, G, T), sys_div_range(U, G, W).
post(sys_lot_agent(V, X, R, T, W, C, D)) <=
   phaseout_posted(sys_lot_agent(V, X, [A*X|B], S, U, C, D)), A < 0, !,
   sys_flip_prod([A*X|B], R), sys_flip_set(S, [], T), sys_flip_range(U, W).
/* Simple Trigger */
post(sys_at(X, S, T, B, C)) <=
   phaseout_posted(sys_lot_agent(V, X, [1*X], S, T, B, C)), !, phaseout(sys_lot_waits(X, V)).
/* Lot & Lot Intersection */
post(sys_lot_remove(J)), post(sys_lot_agent(K, X, L, W, R, F, G)) <=
   phaseout_posted(sys_lot_agent(J, X, L, S, P, F, G)),
   phaseout(sys_lot_agent(K, X, L, T, Q, F, G)),
   (  sys_inter_range(P, Q, R)
   -> sys_inter_set(S, T, W)
   ;  W = [], R = ...),
   W \== T, !.
post(sys_lot_remove(K)) <=
   phaseout_posted(sys_lot_agent(K, X, L, _, _, F, G)),
   sys_lot_agent(_, X, L, _, _, F, G), !.
/* Lot & Pit Intersection */
post(sys_lot_remove(W)), post(sys_pit_remove(V)), sys_melt_const(F, G) <=
   phaseout_posted(sys_lot_agent(W, X, L, S, _, F, G)),
   sys_pit_agent(V, X, L, C, F, G), \+ sys_elem_set(S, C), !.
post(sys_lot_remove(V)) <=
   phaseout_posted(sys_lot_agent(V, X, L, _, _, F, G)),
   sys_pit_agent(_, X, L, _, F, G), !.
/* Lot & Set Intersection */
post(sys_lot_agent(J, X, L, W, R, F, G)) <=
   phaseout_posted(sys_lot_agent(J, X, L, S, P, F, G)),
   sys_set_agent(_, X, L, T, Q),
   (  sys_inter_range(P, Q, R)
   -> sys_inter_set(S, T, W)
   ;  W = [], R = ...),
   W \== S, !.
/* Set & Lot Intersection */
post(sys_lot_agent(J, X, L, W, R, F, G)) <=
   posted(intset:sys_set_agent(_, X, L, T, Q)), phaseout(sys_lot_agent(J, X, L, S, P, F, G)),
   (  sys_inter_range(P, Q, R)
   -> sys_inter_set(S, T, W)
   ;  W = [], R = ...),
   W \== S.
/* Lot & Lin Intersection */
post(sys_lot_remove(V)), sys_melt_const(F, G) <=
   phaseout_posted(sys_lot_agent(V, X, L, S, _, F, G)),
   sys_lin_agent(_, X, L, C), \+ sys_elem_set(S, C), !.
post(sys_lot_remove(V)) <=
   phaseout_posted(sys_lot_agent(V, X, L, _, _, _, _)), sys_lin_agent(_, X, L, _), !.
/* Lin & Lot Intersection */
post(sys_lot_remove(V)), sys_melt_const(F, G) <=
   posted(linform:sys_lin_agent(_, X, L, C)), phaseout(sys_lot_agent(V, X, L, S, _, F, G)),
   \+ sys_elem_set(S, C).
post(sys_lot_remove(V)) <=
   posted(linform:sys_lin_agent(_, X, L, C)), phaseout(sys_lot_agent(V, X, L, S, _, _, _)),
   sys_elem_set(S, C).
/* Union Find, Guard Included */
post(sys_lot_remove(V)), post(sys_lot_ref(V, D, S, T, F, G)) <=
   phaseout_posted(sys_lot_agent(V, _, L, S, T, F, G)), sys_lot_waits(X, V),
   sys_bound_var(X), sys_melt_var(X, H), var(H), !,
   sys_fresh_var(H, Y), sys_pick_prod(B, X, L, E), sys_add_prod([B*Y], E, D).
post(sys_lot_agent(V, X, L, S, T, Y, G)) <=
   phaseout_posted(sys_lot_agent(V, X, L, S, T, F, G)),
   sys_bound_var(F), sys_melt_var(F, H), var(H), !,
   sys_fresh_var(H, Y).
/* Constant Elimination, Guard Included */
post(sys_lot_agent(V, Z, [A*Z|D], W, U, F, G)) <=
   phaseout_posted(sys_lot_agent(V, _, L, S, T, F, G)), phaseout(sys_lot_waits(X, V)),
   sys_bound_var(X), sys_melt_var(X, C), integer(C), !,
   sys_pick_prod(B, X, L, [A*Z|D]), H is -B*C,
   sys_add_set(S, H, W), sys_add_range(T, H, U).
post(sys_lot_remove(V)), post(clpfd:sys_set_ref(V, L, S, T)) <=
   phaseout_posted(sys_lot_agent(V, _, L, S, T, F, G)),
   sys_bound_var(F), sys_melt_var(F, C), integer(C), C \== G, !.
post(sys_lot_remove(V)) <=
   phaseout_posted(sys_lot_agent(V, _, _, _, _, F, _)),
   sys_bound_var(F), sys_melt_var(F, C), integer(C), !.
/* Hook Adding, Guard Included */
sys_melt_hook(X, sys_hook_lot) <=
   posted(sys_lot_agent(V, _, _, _, _, _, _)), sys_lot_waits(X, V).
sys_melt_hook(F, sys_hook_lot) <=
   posted(sys_lot_agent(_, _, _, _, _, F, _)).
/* Set Diffusion, Directed, Bounds */
% needs adaptation
/* Variable Rename, Guard Included */
post(sys_lot_remove(V)), post(sys_lot_ref(V, D, S, T, F, G)) <=
   posted(sys_var_lot(X, Y)),
   sys_lot_waits(X, V), phaseout(sys_lot_agent(V, _, L, S, T, F, G)),
   sys_pick_prod(B, X, L, E), sys_add_prod([B*Y], E, D).
post(sys_lot_agent(V, X, L, S, T, Y, G)) <=
   posted(sys_var_lot(F, Y)),
   phaseout(sys_lot_agent(V, X, L, S, T, F, G)), \+ sys_lot_waits(F, V).
/* Constant Backpropagation, Guard Included */
post(sys_lot_agent(V, Z, [A*Z|D], W, U, F, G)) <=
   posted(sys_const_lot(X, C)),
   phaseout(sys_lot_waits(X, V)), phaseout(sys_lot_agent(V, _, L, S, T, F, G)),
   sys_pick_prod(B, X, L, [A*Z|D]), H is -B*C,
   sys_add_set(S, H, W), sys_add_range(T, H, U).
post(sys_lot_remove(V)), post(clpfd:sys_set_ref(V, L, S, T)) <=
   posted(sys_const_lot(F, H)),
   phaseout(sys_lot_agent(V, _, L, S, T, F, G)), \+ sys_lot_waits(F, V), H \== G.
post(sys_lot_remove(V)) <=
   posted(sys_const_lot(F, H)),
   phaseout(sys_lot_agent(V, _, _, _, _, F, H)), \+ sys_lot_waits(F, V).
/* Set Update, Directed, Bounds */
% needs adaptation

% residue:sys_current_eq(+Var, -Goal)
residue:sys_current_eq(V, set(L, S) #\/ D #= E) :-
   sys_clause_hook(V, sys_hook_lot, _),
   sys_freeze_var(V, X),
   sys_lot_waits(X, K),
   sys_lot_agent(K, _, L, S, _, D, E).

% residue:sys_unwrap_eq(+Goal, -Goal)
residue:sys_unwrap_eq(set(L, S) #\/ P #= Q, [G #\/ R #= Q|T], T) :-
   sys_pretty_in(S, L, G),
   sys_melt_var(P, R).

/**********************************************************/
/* Set Bounds                                             */
/**********************************************************/

% sys_union_bounds(+Range, +Range, -Range)
:- private sys_union_bounds/3.
sys_union_bounds(P, Q, R) :-
   sys_lower_bounds(P, Q, A)
-> (  sys_upper_bounds(P, Q, B)
   -> sys_make_range(A, B, R)
   ;  R = A...)
;  sys_upper_bounds(P, Q, B)
-> R = ..B
;  R = ... .

% sys_lower_bounds(+Range, +Range, -Integer)
% Fails if either range does not have infimum
:- private sys_lower_bounds/3.
sys_lower_bounds(P, Q, C) :-
   sys_lower_range(P, A),
   sys_lower_range(Q, B),
   C is min(A, B).

% sys_upper_bounds(+Range, +Range, -Integer)
% Fails if either range does nnot have supremum
:- private sys_upper_bounds/3.
sys_upper_bounds(P, Q, C) :-
   sys_upper_range(P, A),
   sys_upper_range(Q, B),
   C is max(A, B).