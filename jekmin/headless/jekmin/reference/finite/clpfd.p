/**
 * As a convenience the finite domain solver provides a couple of
 * comparisons between integers. The following features are provided
 * in connection with integer comparison:
 *
 * * Constraint Factoring
 * * Global Constraints
 *
 * Our finite domain solver is probably unique in that it allows posting
 * element hood for arbitrary expressions. This feature is used to
 * internally implement integer comparison. We find the usual comparisons
 * such as #\=/2, #</2, #>/2, #=</2 and #>=/2.  Comparisons are
 * reconstructed from element hood when displaying constraints.
 *
 * Example:
 * ?- Y - X in 0..sup.
 * X #=< Y
 *
 * The constraint solver also attempts to combine element hood constraints.
 * Element hood constraints over the same expression are intersected
 * similarly to domain constraints. Consequently comparisons can be
 * contracted, subsumed or conflict. Also interaction with equations is
 * possible, which are then treated as singleton element hood constraints.
 *
 * Example:
 * ?- X #> Y, X #= Y.
 * No
 *
 * The integer comparisons can be used to define more complex conditions.
 * A recurring problem is stating the inequality of a couple of value
 * expressions. The predicate all_different/2 has been defined as a
 * corresponding convenience.
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

% :- package(library(ordered)).
:- package(library(jekmin/reference/finite)).
:- use_package(library(jekmin/reference/minimal)).
:- use_package(library(jekpro/frequent/misc)).

:- module(clpfd, []).
:- use_module(library(basic/lists)).
:- use_module(library(minimal/assume)).
:- use_module(library(minimal/hypo)).
:- use_module(library(minimal/delta)).
:- use_module(library(misc/struc)).
:- use_module(library(misc/elem)).
:- use_module(library(experiment/surrogate)).
:- use_module(library(experiment/trail)).
:- use_module(library(experiment/ref)).
:- use_module(library(experiment/attr)).
:- use_module(helper).
:- reexport(linform).
:- reexport(intset).
:- reexport(reify).
:- reexport(enum).

% sys_in(+Wrap, +Set, +Bound)
% sys_in(X,S,_) = X in S
:- multifile intset:sys_in/3.
:- thread_local intset:sys_in/3.
:- multifile intset:sys_in/4.

/**********************************************************/
/* Multiplication Constraints                             */
/**********************************************************/

% sys_hook_mul(+Var, +Term)
:- private sys_hook_mul/2.
sys_hook_mul(V, W) :-
   var(W), !,
   sys_freeze_var(V, R),
   sys_fresh_var(W, S),
   sys_assume_cont(<= +sys_var_mul(R,S)).
sys_hook_mul(V, T) :-
   integer(T), !,
   sys_freeze_var(V, R),
   sys_assume_cont(<= +sys_const_mul(R,T)).
sys_hook_mul(_, T) :-
   throw(error(type_error(integer,T),_)).

% sys_var_mul(+Wrap, +Wrap)
% sys_var_mul(X, Y) = X = Y
:- private sys_var_mul/2.
:- thread_local sys_var_mul/2.
:- private sys_var_mul/3.

/* Union Find */
unit <=
   = sys_var_mul(_, _).

% sys_const_mul(+Wrap, +Integer)
% sys_const_mul(X, C) = X = C
:- private sys_const_mul/2.
:- thread_local sys_const_mul/2.
:- private sys_const_mul/3.

/* Constant Elimination */
unit <=
   = sys_const_mul(_, _).

% sys_mulv(+Wrap, +Wrap, +Warp)
% sys_mulv(X, Y, Z), X*Y = Z

% sys_mulv_ord(+Wrap, +Wrap, +Wrap)
% sys_mulv_ord(X, Y, Z), X*Y = Z, X @< Y
:- multifile sys_mulv_ord/3.
:- thread_local sys_mulv_ord/3.
:- private sys_mulv_ord/4.

/* Special Case */
+ sys_sqrv(X, Y) <=
   = sys_mulv(X, X, Y), !.
+ sys_impv(X, Y) <=
   = sys_mulv(X, Y, X), !.
+ sys_impv(X, Y) <=
   = sys_mulv(Y, X, X), !.
/* Order Arguments */
+ sys_mulv_ord(X, Y, Z) <=
   = sys_mulv(X, Y, Z),
   {X @< Y}, !.
+ sys_mulv_ord(X, Y, Z) <=
   = sys_mulv(Y, X, Z).

/* Mul & Mul Intersection */
sys_melt_join(T, Z) <=
   = sys_mulv_ord(X, Y, Z),
   sys_mulv_ord(X, Y, T), !.
sys_melt_const(Z, C) <=
   = sys_mulv_ord(X, Y, Z),
   sys_mulc_ord(X, Y, C), !.
/* Union Find */
+ sys_mulv(T, Y, Z) <=
   = sys_mulv_ord(X, Y, Z),
   {sys_bound_var(X),
    sys_melt_var(X, H),
    var(H)}, !,
   {sys_fresh_var(H, T)}.
+ sys_mulv(X, T, Z) <=
   = sys_mulv_ord(X, Y, Z),
   {sys_bound_var(Y),
    sys_melt_var(Y, H),
    var(H)}, !,
   {sys_fresh_var(H, T)}.
+ sys_mulv(X, Y, T) <=
   = sys_mulv_ord(X, Y, Z),
   {sys_bound_var(Z),
    sys_melt_var(Z, H),
    var(H)}, !,
   {sys_fresh_var(H, T)}.
/* Constant Elimination */
+ sys_lin(L, 0) <=
   = sys_mulv_ord(X, Y, Z),
   {sys_bound_var(X),
    sys_melt_var(X, C),
    integer(C)}, !,
   {sys_make_prod(C, Y, [], H),
    sys_add_prod(H, [-1*Z], L)}.
+ sys_lin(L, 0) <=
   = sys_mulv_ord(X, Y, Z),
   {sys_bound_var(Y),
    sys_melt_var(Y, C),
    integer(C)}, !,
   {sys_make_prod(C, X, [], H),
    sys_add_prod(H, [-1*Z], L)}.
+ sys_mulc_ord(X, Y, C) <=
   = sys_mulv_ord(X, Y, Z),
   {sys_bound_var(Z),
    sys_melt_var(Z, C),
    integer(C)}, !.
/* Hook Adding */
sys_melt_hook(X, sys_hook_mul) /\
sys_melt_hook(Y, sys_hook_mul) /\
sys_melt_hook(Z, sys_hook_mul) <=
   + sys_mulv_ord(X, Y, Z).
/* Set Diffusion, Directed, Bounds */
+ (intset:sys_in(Z, [S], S)) <=
   + sys_mulv_ord(X, Y, Z),
   {Z @> Y}, !,
   {sys_bound_factor(X, R),
    sys_bound_factor(Y, T),
    sys_cross_range(R, T, S),
    S \== ...}.
+ (intset:sys_in(Y, L, S)) <=
   + sys_mulv_ord(X, Y, Z),
   {sys_bound_factor(Z, R),
    sys_bound_divisor(X, T),
    (  sys_slash_range(R, T, S)
    -> S \== ...,
       L = [S]
    ;  L = [],
       S = ...)}.
/* Variable Rename */
+ sys_mulv(T, Y, Z) <=
   + sys_var_mul(X, T),
   - sys_mulv_ord(X, Y, Z).
+ sys_mulv(X, T, Z) <=
   + sys_var_mul(Y, T),
   - sys_mulv_ord(X, Y, Z),
   {Y \== X}.
+ sys_mulv(X, Y, T) <=
   + sys_var_mul(Z, T),
   - sys_mulv_ord(X, Y, Z),
   {Z \== X,
    Z \== Y}.
/* Constant Backpropagation */
+ sys_lin(L, 0) <=
   + sys_const_mul(X, C),
   - sys_mulv_ord(X, Y, Z),
   {sys_make_prod(C, Y, [], H),
    sys_add_prod(H, [-1*Z], L)}.
+ sys_lin(L, 0) <=
   + sys_const_mul(Y, C),
   - sys_mulv_ord(X, Y, Z),
   {Y \== X,
    sys_make_prod(C, X, [], H),
    sys_add_prod(H, [-1*Z], L)}.
+ sys_mulc_ord(X, Y, C) <=
   + sys_const_mul(Z, C),
   - sys_mulv_ord(X, Y, Z),
   {Z \== X,
    Z \== Y}.
/* Set Update, Directed, Bounds */
+ (intset:sys_in(Z, [S], S)) <=
   + (intset:sys_in(X, _, R)),
   {R \== ...},
   sys_mulv_ord(X, Y, Z),
   {Z @> Y,
    sys_bound_factor(Y, T),
    sys_cross_range(R, T, S),
    S \== ...}.
+ (intset:sys_in(Z, [S], S)) <=
   + (intset:sys_in(Y, _, R)),
   {R \== ...},
   sys_mulv_ord(X, Y, Z),
   {Z @> Y,
    sys_bound_factor(X, T),
    sys_cross_range(R, T, S),
    S \== ...}.
+ (intset:sys_in(Y, L, S)) <=
   + (intset:sys_in(Z, _, R)),
   {R \== ...},
   sys_mulv_ord(X, Y, Z),
   {Z @=< Y,
    sys_bound_divisor(X, T),
    (  sys_slash_range(R, T, S)
    -> S \== ...,
       L = [S]
    ;  L = [],
       S = ...)}.
+ (intset:sys_in(Y, L, S)) <=
   + (intset:sys_in(X, _, R)),
   sys_mulv_ord(X, Y, Z),
   {Z @=< Y,
    sys_bound_factor(Z, T),
    (  sys_slash_range(T, R, S)
    -> S \== ...,
       L = [S]
    ;  L = [],
       S = ...)}.

% sys_mulc(+Wrap, +Wrap, +Integer)
% sys_mulc(X,Y,C), X*Y = C

% sys_mulc_ord(+Wrap, +Wrap, +Integer)
% sys_mulc_ord(X,Y,C), X*Y = C, X @< Y
:- private sys_mulc_ord/3.
:- thread_local sys_mulc_ord/3.
:- private sys_mulc_ord/4.

/* Special Case */
+ sys_sqrc(X, C) <=
   = sys_mulc(X, X, C), !.
/* Order Arguments */
+ sys_mulc_ord(X, Y, C) <=
   = sys_mulc(X, Y, C),
   {X @< Y}, !.
+ sys_mulc_ord(X, Y, C) <=
   = sys_mulc(Y, X, C).

/* Mul & Mul Intersection */
sys_melt_const(T, C) <=
   = sys_mulc_ord(X, Y, C),
   sys_mulv_ord(X, Y, T), !.
zero <=
   = sys_mulc_ord(X, Y, C),
   sys_mulc_ord(X, Y, D),
   {C \== D}, !.
unit <=
   = sys_mulc_ord(X, Y, C),
   sys_mulc_ord(X, Y, C), !.
/* Union Find */
+ sys_mulc(Z, Y, C) <=
   = sys_mulc_ord(X, Y, C),
   {sys_bound_var(X),
    sys_melt_var(X, H),
    var(H)}, !,
   {sys_fresh_var(H, Z)}.
+ sys_mulc(X, Z, C) <=
   = sys_mulc_ord(X, Y, C),
   {sys_bound_var(Y),
    sys_melt_var(Y, H),
    var(H)}, !,
   {sys_fresh_var(H, Z)}.
/* Constant Elimination */
+ sys_lin(L, C) <=
   = sys_mulc_ord(X, Y, C),
   {sys_bound_var(X),
    sys_melt_var(X, D),
    integer(D)}, !,
   {sys_make_prod(D, Y, [], L)}.
+ sys_lin(L, C) <=
   = sys_mulc_ord(X, Y, C),
   {sys_bound_var(Y),
    sys_melt_var(Y, D),
    integer(D)}, !,
   {sys_make_prod(D, X, [], L)}.
/* Hook Adding */
sys_melt_hook(X, sys_hook_mul) /\
sys_melt_hook(Y, sys_hook_mul) <=
   + sys_mulc_ord(X, Y, _).
/* Set Diffusion, Directed, Bounds */
+ (intset:sys_in(Y, L, S)) <=
   + sys_mulc_ord(X, Y, C),
   {sys_bound_divisor(X, R),
    (  sys_slash_range(C, R, S)
    -> S \== ...,
       L = [S]
    ;  L = [],
       S = ...)}.
/* Variable Rename */
+ sys_mulc(Z, Y, C) <=
   + sys_var_mul(X, Z),
   - sys_mulc_ord(X, Y, C).
+ sys_mulc(X, Z, C) <=
   + sys_var_mul(Y, Z),
   - sys_mulc_ord(X, Y, C),
   {Y \== X}.
/* Constant Backpropagation */
+ sys_lin(L, C) <=
   + sys_const_mul(X, S),
   - sys_mulc_ord(X, Y, C),
   {sys_make_prod(S, Y, [], L)}.
+ sys_lin(L, C) <=
   + sys_const_mul(Y, S),
   - sys_mulc_ord(X, Y, C),
   {Y \== X,
    sys_make_prod(S, X, [], L)}.
/* Set Update, Directed, Bounds */
+ (intset:sys_in(Y, L, S)) <=
   + (intset:sys_in(X, _, R)),
   sys_mulc_ord(X, Y, C),
   {sys_slash_range(C, R, S)
 -> S \== ...,
    L = [S]
 ;  L = [],
    S = ...}.

% residue:sys_current_eq(+Var, -Handle)
:- public residue:sys_current_eq/2.
:- multifile residue:sys_current_eq/2.
:- discontiguous residue:sys_current_eq/2.
residue:sys_current_eq(V, mulv(X,Y,Z)) :-
   sys_clause_hook(V, sys_hook_mul, _),
   sys_freeze_var(V, X),
   sys_mulv_ord(X, Y, Z).
residue:sys_current_eq(V, mulv(X,Y,Z)) :-
   sys_clause_hook(V, sys_hook_mul, _),
   sys_freeze_var(V, Y),
   sys_mulv_ord(X, Y, Z).
residue:sys_current_eq(V, mulv(X,Y,Z)) :-
   sys_clause_hook(V, sys_hook_mul, _),
   sys_freeze_var(V, Z),
   sys_mulv_ord(X, Y, Z).
residue:sys_current_eq(V, mulc(X,Y,C)) :-
   sys_clause_hook(V, sys_hook_mul, _),
   sys_freeze_var(V, X),
   sys_mulc_ord(X, Y, C).
residue:sys_current_eq(V, mulc(X,Y,C)) :-
   sys_clause_hook(V, sys_hook_mul, _),
   sys_freeze_var(V, Y),
   sys_mulc_ord(X, Y, C).

% residue:sys_unwrap_eq(+Handle, -Goals, +Goals)
:- public residue:sys_unwrap_eq/3.
:- multifile residue:sys_unwrap_eq/3.
:- discontiguous residue:sys_unwrap_eq/3.
residue:sys_unwrap_eq(mulv(X,Y,Z), [C#=A*B|L], L) :-
   Z @> Y, !,
   sys_melt_var(X, A),
   sys_melt_var(Y, B),
   sys_melt_var(Z, C).
residue:sys_unwrap_eq(mulv(X,Y,Z), [A*B#=C|L], L) :-
   sys_melt_var(X, A),
   sys_melt_var(Y, B),
   sys_melt_var(Z, C).
residue:sys_unwrap_eq(mulc(X,Y,C), [C#=A*B|L], L) :-
   sys_melt_var(X, A),
   sys_melt_var(Y, B).

/**********************************************************/
/* Special Cases Constraints                              */
/**********************************************************/

% sys_hook_spez(+Var, +Term)
:- private sys_hook_spez/2.
sys_hook_spez(V, W) :-
   var(W), !,
   sys_freeze_var(V, R),
   sys_fresh_var(W, S),
   sys_assume_cont(<= +sys_var_spez(R,S)).
sys_hook_spez(V, T) :-
   integer(T), !,
   sys_freeze_var(V, R),
   sys_assume_cont(<= +sys_const_spez(R,T)).
sys_hook_spez(_, T) :-
   throw(error(type_error(integer,T),_)).

% sys_var_spez(+Wrap, +Wrap)
% sys_var_spez(X, Y) = X = Y
:- private sys_var_spez/2.
:- thread_local sys_var_spez/2.
:- private sys_var_spez/3.

/* Union Find */
unit <=
   = sys_var_spez(_, _).

% sys_const_spez(+Wrap, +Integer)
% sys_const_spez(X, C) = X = C
:- private sys_const_spez/2.
:- thread_local sys_const_spez/2.
:- private sys_const_spez/3.

/* Constant Elimination */
unit <=
   = sys_const_spez(_, _).

% sys_sqrv(+Wrap, +Wrap)
% sys_sqrv(X,Y), X*X = Y
:- private sys_sqrv/2.
:- thread_local sys_sqrv/2.
:- private sys_sqrv/3.
/* Special Case */
+ (intset:sys_in(X, [0..1], 0..1)) <=
   = sys_sqrv(X, X), !.
/* Sqr & Sqr Intersection */
sys_melt_join(T, Z) <=
   = sys_sqrv(X, Z),
   sys_sqrv(X, T), !.
/* Union Find */
+ sys_sqrv(T, Y) <=
   = sys_sqrv(X, Y),
   {sys_bound_var(X),
    sys_melt_var(X, H),
    var(H)}, !,
   {sys_fresh_var(H, T)}.
+ sys_sqrv(X, T) <=
   = sys_sqrv(X, Y),
   {sys_bound_var(Y),
    sys_melt_var(Y, H),
    var(H)}, !,
   {sys_fresh_var(H, T)}.
/* Constant Elimination */
sys_melt_const(Y, D) <=
   = sys_sqrv(X, Y),
   {sys_bound_var(X),
    sys_melt_var(X, C),
    integer(C)}, !,
   {D is C*C}.
+ sys_sqrc(X, C) <=
   = sys_sqrv(X, Y),
   {sys_bound_var(Y),
    sys_melt_var(Y, C),
    integer(C)}, !.
/* Hook Adding */
sys_melt_hook(X, sys_hook_spez) /\
sys_melt_hook(Y, sys_hook_spez) <=
   + sys_sqrv(X, Y).
/* Set Diffusion, Directed, Bounds */
+ (intset:sys_in(X, L, S)) <=
   + sys_sqrv(X, Y),
   {X @>= Y}, !,
   {sys_bound_factor(Y, R),
    (  sys_root_range(R, S)
    -> S \== ...,
       L = [S]
    ;  L = [],
       S = ...)}.
+ (intset:sys_in(Y, [S], S)) <=
   + sys_sqrv(X, Y),
   {sys_bound_divisor(X, R),
    sys_square_range(R, S)}.
/* Variable Rename */
+ sys_sqrv(T, Y) <=
   + sys_var_spez(X, T),
   - sys_sqrv(X, Y).
+ sys_sqrv(X, T) <=
   + sys_var_spez(Y, T),
   - sys_sqrv(X, Y),
   {Y \== X}.
/* Constant Backpropagation */
sys_melt_const(Y, D) <=
   + sys_const_spez(X, C),
   - sys_sqrv(X, Y),
   {D is C*C}.
+ sys_sqrc(X, C) <=
   + sys_const_spez(Y, C),
   - sys_sqrv(X, Y),
   {Y \== X}.
/* Set Update, Directed, Bounds */
+ (intset:sys_in(X, L, S)) <=
   + (intset:sys_in(Y, _, R)),
   {R \== ...},
   sys_sqrv(X, Y),
   {X @>= Y,
    (  sys_root_range(R, S)
    -> S \== ...,
       L = [S]
    ;  L = [],
       S = ...)}.
+ (intset:sys_in(Y, [S], S)) <=
   + (intset:sys_in(X, _, R)),
   sys_sqrv(X, Y),
   {X @< Y,
    sys_square_range(R, S)}.

% sys_sqrc(+Wrap, +Integer)
% sys_sqrc(X,C), X*X = C
:- private sys_sqrc/2.
:- thread_local sys_sqrc/2.
:- private sys_sqrc/3.
% Special Case
sys_melt_const(X, 0) <=
   = sys_sqrc(X, 0), !.
zero <=
   = sys_sqrc(_, C),
   {C < 0}, !.
+ (intset:sys_in(X, [H,D], H..D)) <=
   = sys_sqrc(X, C),
   {sqrtrem(C, D, R),
    R =:= 0,
    H is -D}, !.
zero <=
   = sys_sqrc(_, _).

% sys_impv(+Wrap, +Wrap)
% sys_impv(X,Y), X*Y = X
:- private sys_impv/2.
:- thread_local sys_impv/2.
:- private sys_impv/3.
/* Special Case */
+ (intset:sys_in(X, [0..1], 0..1)) <=
   = sys_impv(X, X), !.
/* Union Find */
+ sys_impv(T, Y) <=
   = sys_impv(X, Y),
   {sys_bound_var(X),
    sys_melt_var(X, H),
    var(H)}, !,
   {sys_fresh_var(H, T)}.
+ sys_impv(X, T) <=
   = sys_impv(X, Y),
   {sys_bound_var(Y),
    sys_melt_var(Y, H),
    var(H)}, !,
   {sys_fresh_var(H, T)}.
/* Constant Elimination */
+ sys_lin(L, C) <=
   = sys_impv(X, Y),
   {sys_bound_var(X),
    sys_melt_var(X, C),
    integer(C)}, !,
   {sys_make_prod(C, Y, [], L)}.
+ sys_lin(L, 0) <=
   = sys_impv(X, Y),
   {sys_bound_var(Y),
    sys_melt_var(Y, C),
    integer(C)}, !,
   {D is C-1,
    sys_make_prod(D, X, [], L)}.
/* Hook Adding */
sys_melt_hook(X, sys_hook_spez) /\
sys_melt_hook(Y, sys_hook_spez) <=
   + sys_impv(X, Y).
/* Set Diffusion, Directed, Bounds */
+ (intset:sys_in(X, [S], S)) <=
   + sys_impv(X, Y),
   {X @>= Y}, !,
   {sys_bound_factor(Y, R),
    sys_prem_range(R, S),
    S \== ...}.
+ (intset:sys_in(Y, [S], S)) <=
   + sys_impv(X, Y),
   {sys_bound_factor(X, R),
    sys_conc_range(R, S),
    S \== ...}.
/* Variable Rename */
+ sys_impv(T, Y) <=
   + sys_var_spez(X, T),
   - sys_impv(X, Y).
+ sys_impv(X, T) <=
   + sys_var_spez(Y, T),
   - sys_impv(X, Y),
   {Y \== X}.
/* Constant Backpropagation */
+ sys_lin(L, C) <=
   + sys_const_spez(X, C),
   - sys_impv(X, Y),
   {sys_make_prod(C, Y, [], L)}.
+ sys_lin(L, 0) <=
   + sys_const_spez(Y, C),
   - sys_impv(X, Y),
   {Y \== X,
    D is C-1,
    sys_make_prod(D, X, [], L)}.
/* Set Update, Directed, Bounds */
+ (intset:sys_in(X, [S], S)) <=
   + (intset:sys_in(Y, _, R)),
   {R \== ...},
   sys_impv(X, Y),
   {X @>= Y,
    sys_prem_range(R, S),
    S \== ...}.
+ (intset:sys_in(Y, [S], S)) <=
   + (intset:sys_in(X, _, R)),
   {R \== ...},
   sys_impv(X, Y),
   {X @< Y,
    sys_conc_range(R, S),
    S \== ...}.

% residue:sys_current_eq(+Var, -Goal)
residue:sys_current_eq(V, sqrv(X,Y)) :-
   sys_clause_hook(V, sys_hook_spez, _),
   sys_freeze_var(V, X),
   sys_sqrv(X, Y).
residue:sys_current_eq(V, sqrv(X,Y)) :-
   sys_clause_hook(V, sys_hook_spez, _),
   sys_freeze_var(V, Y),
   sys_sqrv(X, Y).
residue:sys_current_eq(V, impv(X,Y)) :-
   sys_clause_hook(V, sys_hook_spez, _),
   sys_freeze_var(V, X),
   sys_impv(X, Y).
residue:sys_current_eq(V, impv(X,Y)) :-
   sys_clause_hook(V, sys_hook_spez, _),
   sys_freeze_var(V, Y),
   sys_impv(X, Y).

% residue:sys_unwrap_eq(+Goal, -Goal)
residue:sys_unwrap_eq(sqrv(X,Y), [A*A#=B|L], L) :-
   X @>= Y, !,
   sys_melt_var(X, A),
   sys_melt_var(Y, B).
residue:sys_unwrap_eq(sqrv(X,Y), [B#=A*A|L], L) :-
   sys_melt_var(X, A),
   sys_melt_var(Y, B).
residue:sys_unwrap_eq(impv(X,Y), [A#=B*A|L], L) :-
   X @>= Y, !,
   sys_melt_var(X, A),
   sys_melt_var(Y, B).
residue:sys_unwrap_eq(impv(X,Y), [A#=A*B|L], L) :-
   sys_melt_var(X, A),
   sys_melt_var(Y, B).

/**********************************************************/
/* Absolute Constraints                                   */
/**********************************************************/

% sys_hook_abs(+Var, +Term)
:- private sys_hook_abs/2.
sys_hook_abs(V, W) :-
   var(W), !,
   sys_freeze_var(V, R),
   sys_fresh_var(W, S),
   sys_assume_cont(<= +sys_var_abs(R,S)).
sys_hook_abs(V, T) :-
   integer(T), !,
   sys_freeze_var(V, R),
   sys_assume_cont(<= +sys_const_abs(R,T)).
sys_hook_abs(_, T) :-
   throw(error(type_error(integer,T),_)).

% sys_var_abs(+Wrap, +Wrap)
% sys_var_abs(X, Y) = X = Y
:- private sys_var_abs/2.
:- thread_local sys_var_abs/2.
:- private sys_var_abs/3.

/* Union Find */
unit <=
   = sys_var_abs(_, _).

% sys_const_abs(+Wrap, +Integer)
% sys_const_abs(X, C) = X = C
:- private sys_const_abs/2.
:- thread_local sys_const_abs/2.
:- private sys_const_abs/3.

/* Constant Elimination */
unit <=
   = sys_const_abs(_, _).

% sys_absv(+Wrap, +Wrap)
% sys_absv(X,Y), abs(X) = Y
:- multifile sys_absv/2.
:- thread_local sys_absv/2.
/* Special Case */
+ (intset:sys_in(X, [0...], 0...)) <=
   = sys_absv(X, X), !.
/* Abs & Abs Intersection */
sys_melt_join(T, Z) <=
   = sys_absv(X, Z),
   sys_absv(X, T), !.
/* Union Find */
+ sys_absv(T, Y) <=
   = sys_absv(X, Y),
   {sys_bound_var(X),
    sys_melt_var(X, H),
    var(H)}, !,
   {sys_fresh_var(H, T)}.
+ sys_absv(X, T) <=
   = sys_absv(X, Y),
   {sys_bound_var(Y),
    sys_melt_var(Y, H),
    var(H)}, !,
   {sys_fresh_var(H, T)}.
/* Constant Elimination */
sys_melt_const(Y, D) <=
   = sys_absv(X, Y),
   {sys_bound_var(X),
    sys_melt_var(X, C),
    integer(C)}, !,
   {D is abs(C)}.
+ sys_absc(X, C) <=
   = sys_absv(X, Y),
   {sys_bound_var(Y),
    sys_melt_var(Y, C),
    integer(C)}, !.
/* Hook Adding */
sys_melt_hook(X, sys_hook_abs) /\
sys_melt_hook(Y, sys_hook_abs) <=
   + sys_absv(X, Y).
/* Set Diffusion, Directed, Bounds */
+ (intset:sys_in(X, L, S)) <=
   + sys_absv(X, Y),
   {X @>= Y}, !,
   {sys_bound_factor(Y, R),
    (  sys_invabs_range(R, S)
    -> S \== ...,
       L = [S]
    ;  L = [],
       S = ...)}.
+ (intset:sys_in(Y, [S], S)) <=
   + sys_absv(X, Y),
   {sys_bound_divisor(X, R),
    sys_abs_range(R, S)}.
/* Variable Rename */
+ sys_absv(T, Y) <=
   + sys_var_abs(X, T),
   - sys_absv(X, Y).
+ sys_absv(X, T) <=
   + sys_var_abs(Y, T),
   - sys_absv(X, Y),
   {Y \== X}.
/* Constant Backpropagation */
sys_melt_const(Y, D) <=
   + sys_const_abs(X, C),
   - sys_absv(X, Y),
   {D is abs(C)}.
+ sys_absc(X, C) <=
   + sys_const_abs(Y, C),
   - sys_absv(X, Y),
   {Y \== X}.
/* Set Update, Directed, Bounds */
+ (intset:sys_in(X, L, S)) <=
   + (intset:sys_in(Y, _, R)),
   {R \== ...},
   sys_absv(X, Y),
   {X @>= Y,
    (  sys_invabs_range(R, S)
    -> S \== ...,
       L = [S]
    ;  L = [],
       S = ...)}.
+ (intset:sys_in(Y, [S], S)) <=
   + (intset:sys_in(X, _, R)),
   sys_absv(X, Y),
   {X @< Y,
    sys_abs_range(R, S)}.

% sys_absc(+Wrap, +Integer)
% sys_absc(X,C), abs(X) = C
:- private sys_absc/2.
:- thread_local sys_absc/2.
:- private sys_absc/3.
% Special Case
sys_melt_const(X, 0) <=
   = sys_absc(X, 0), !.
zero <=
   = sys_absc(_, C),
   {C < 0}, !.
+ (intset:sys_in(X, [H,C], H..C)) <=
   = sys_absc(X, C),
   {H is -C}.

% residue:sys_current_eq(+Var, -Goal)
residue:sys_current_eq(V, absv(X,Y)) :-
   sys_clause_hook(V, sys_hook_abs, _),
   sys_freeze_var(V, X),
   sys_absv(X, Y).
residue:sys_current_eq(V, absv(X,Y)) :-
   sys_clause_hook(V, sys_hook_abs, _),
   sys_freeze_var(V, Y),
   sys_absv(X, Y).

% residue:sys_unwrap_eq(+Goal, -Goal)
residue:sys_unwrap_eq(absv(X,Y), [abs(A)#=B|L], L) :-
   X @>= Y, !,
   sys_melt_var(X, A),
   sys_melt_var(Y, B).
residue:sys_unwrap_eq(absv(X,Y), [B#=abs(A)|L], L) :-
   sys_melt_var(X, A),
   sys_melt_var(Y, B).
