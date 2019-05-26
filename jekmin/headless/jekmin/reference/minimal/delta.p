/**
 * This module provides a couple of operators and predicates to define
 * forward chaining clauses. A forward chaining clause is recognized by
 * the (<=)/2 operator. A forward chaining clause has the form "Action
 * <= Condition" where the action part can be arbitrary Prolog goals. A
 * forward chaining clause will be rewritten into multiple delta
 * computation rules.
 *
 * Action <= Condition     % Forward Chaining Clause.
 *
 * The module provides a new hypothetical reasoning verbs post/1. This
 * verb first invokes the delta computation rules to determine a new
 * agenda, then uses assumez/1 to assume the given fact and finally
 * continues with the new agenda. By delete set inclusion the delta
 * computation can also yield counter factual reasoning.
 *
 * Example:
 * :- forward q/2.
 * post(q(X)) <= posted(p(X)).
 *
 * ?- post(p(a)), q(X).
 * X = a
 *
 * The delta computation has functor F/N+1 for an arriving fact with
 * a functor F/N. Each literal in the body of a forward chaining
 * clause has to be annotated either for delete set inclusion
 * phaseout/1, for delta computation posted/1 or for both
 * phaseout_posted/1. Non-annotated lit-erals are condition goals
 * that are executed in backward chaining fashion.
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

:- package(library(jekmin/reference/minimal)).
:- use_package(library(jekpro/frequent/experiment)).

:- module(delta, []).
:- use_module(library(minimal/hypo)).
:- use_module(library(basic/lists)).
:- use_module(library(experiment/simp)).
:- use_module(library(minimal/assume)).
:- reexport(library(experiment/ref)).

:- public infix(<=).
:- op(1200, xfx, <=).

:- public prefix(forward).
:- op(1150, fy, forward).

/**
 * forward P, ...:
 * The predicate sets the predicate P to discontiguous,
 * sys_notrace and static.
 */
% forward +Indicators
:- public (forward)/1.
:- meta_predicate forward(0).
forward L :-
   (discontiguous L),
   (sys_notrace L),
   (static L).

/**********************************************************/
/* Continuation Forwarder                                 */
/**********************************************************/

/**
 * post(F):
 * post(F, G):
 * The fact F is not directly assumed. Instead, it is first
 * treated as a new fact for delta computation.
 */
% post(+Term)
:- public post/1.
:- meta_predicate post(-1).
post(X) :-
   findall(U, call(X, U), L),
   withdraw_delta(L, assumez(X), R, Y), !, Y, R.
post(X) :-
   throw(error(permission_error(remove,delta,X),_)).

% withdraw_delta(+Deltas, +Goal, -Goal, -Goal)
:- private withdraw_delta/4.
withdraw_delta([sys_delta(Z,S)|Y], X, (Z,T), W) :- !,
   withdrawz_ref(S),
   withdraw_delta(Y, X, T, W).
withdraw_delta([sys_nabla(Z,S)|Y], _, (Z,T), true) :-
   withdrawz_ref(S),
   withdraw_delta2(Y, T).
withdraw_delta([], X, true, X).

% withdraw_delta2(+Deltas, -Goal)
:- private withdraw_delta2/2.
withdraw_delta2([sys_delta(Z,S)|Y], (Z,T)) :-
   withdrawz_ref(S),
   withdraw_delta2(Y, T).
withdraw_delta2([], true).

/**********************************************************/
/* Hypothetical Forwarder                                 */
/**********************************************************/

% post(+Term, +Goal)
:- public post/2.
:- meta_predicate post(-1,0).
post(X, H) :-
   findall(U, call(X, U), L),
   withdraw_delta(L, assumez(X), R, Y), !,
   call((  Y, R), H),
   depositz_delta(L).
post(X, _) :-
   throw(error(permission_error(remove,delta,X),_)).

% depositz_delta(+Deltas)
:- private depositz_delta/1.
depositz_delta([sys_delta(_,S)|Y]) :- !,
   depositz_ref(S),
   depositz_delta(Y).
depositz_delta([sys_nabla(_,S)|Y]) :-
   depositz_ref(S),
   depositz_delta2(Y).
depositz_delta([]).

% depositz_delta2(+Deltas)
:- private depositz_delta2/1.
depositz_delta2([sys_delta(_,S)|Y]) :-
   depositz_ref(S),
   depositz_delta2(Y).
depositz_delta2([]).

/***********************************************************/
/* Disjunction Reduction & Distribution                    */
/***********************************************************/

% simp:goal_simplification(+Goal, -Goal)
:- public simp:goal_simplification/2.
:- multifile simp:goal_simplification/2.
:- meta_predicate simp:goal_simplification(0,0).
:- discontiguous simp:goal_simplification/2.

% sys_keep(+Term, +Goal)
:- private sys_keep/2.
:- meta_predicate sys_keep(-1,0).
sys_keep(_, _) :-
   throw(error(existence_error(body,sys_keep/2),_)).

% sys_drop(+Term, +Goal)
:- private sys_drop/2.
:- meta_predicate sys_drop(-1,0).
sys_drop(_, _) :-
   throw(error(existence_error(body,sys_drop/2),_)).

% sys_special(+Goal)
:- private sys_special/1.
sys_special(A) :-
   var(A), !, fail.
sys_special(sys_keep(_,_)).
sys_special(sys_drop(_,_)).
sys_special((A,_)) :-
   var(A), !, fail.
sys_special((sys_keep(_,_),_)).
sys_special((sys_drop(_,_),_)).
sys_special((A;_)) :-
   sys_special(A).
sys_special((_;A)) :-
   sys_special(A).

% disjunction + sys_none reduction
simp:goal_simplification((  sys_none; C), C).
simp:goal_simplification((  C; sys_none), C).

% conjunction + sys_none reduction
simp:goal_simplification((  sys_none, _), sys_none).
simp:goal_simplification((  _, sys_none), sys_none).

% conjunction + disjunction distribution
simp:goal_simplification((  (  A; B), C), R) :-
   (  sys_special(A)
   ;  sys_special(B)),
   simplify_goal((  A, C), H),
   simplify_goal((  B, C), J),
   simplify_goal((  H; J), R).
simp:goal_simplification((  A,
                            (  B; C)), R) :-
   (  sys_special(B)
   ;  sys_special(C)),
   simplify_goal((  A, B), H),
   simplify_goal((  A, C), J),
   simplify_goal((  H; J), R).

/**********************************************************/
/* New, Old & OldNew Reduction                            */
/**********************************************************/

/**
 * sys_new(A):
 * This predicate cannot be executed. It only serves as a
 * goal expansion wrapper for the delta of a goal.
 */
% sys_new(+Goal)
:- private sys_new/1.
:- meta_predicate sys_new(0).
sys_new(_) :-
   throw(error(existence_error(body,sys_new/1),_)).

simp:goal_simplification(sys_new(A), _) :-
   var(A), !, fail.

/**
 * phaseout(P):
 * The condition succeeds when F is an old fact and
 * it then gets included for deletion.
 */
:- public phaseout/1.
:- meta_predicate phaseout(-1).
phaseout(_) :-
   throw(error(existence_error(body,phaseout/1),_)).

simp:goal_simplification(sys_new(phaseout(_)), sys_none).

/**
 * posted(F):
 * The condition reacts if F is a new fact.
 */
:- public posted/1.
:- meta_predicate posted(-1).
posted(_) :-
   throw(error(existence_error(body,posted/1),_)).

simp:goal_simplification(sys_new(posted(H)), sys_keep(H, true)).

/**
 * phaseout_posted(P):
 * The condition reacts if F is a new fact and
 * it then gets included for deletion
 */
:- public phaseout_posted/1.
:- meta_predicate phaseout_posted(-1).
phaseout_posted(_) :-
   throw(error(existence_error(body,phaseout_posted/1),_)).

simp:goal_simplification(sys_new(phaseout_posted(H)), sys_drop(H, true)).

/**
 * A, B:
 * The condition reacts when A or B react, and the other succeeds, or both react.
 */
simp:goal_simplification(sys_new((  A, B)), R) :-
   simplify_goal(sys_new(A), U),
   simplify_goal(sys_old(B), V),
   simplify_goal((  U, V), J),
   simplify_goal(sys_oldnew(A), P),
   simplify_goal(sys_new(B), Q),
   simplify_goal((  P, Q), H),
   simplify_goal((  J; H), R).

/**
 * A; B:
 * The condition reacts when A or B react.
 */
simp:goal_simplification(sys_new((  A; B)), R) :-
   simplify_goal(sys_new(A), U),
   simplify_goal(sys_new(B), V),
   simplify_goal((  U; V), R).

/**
 * P:
 * Whenever P succeeds in backward chaining the condition succeeds.
 */
simp:goal_simplification(sys_new(_), sys_none).

/**
 * sys_old(A):
 * This predicate cannot be executed. It only serves as a
 * goal expansion wrapper for the old of a goal.
 */
% sys_old(+Goal)
:- private sys_old/1.
:- meta_predicate sys_old(0).
sys_old(_) :-
   throw(error(existence_error(body,sys_old/1),_)).

/**
 * sys_check(A):
 * This predicate cannot be executed. It only serves as a
 * goal expansion wrapper the old of a callable.
 */
% sys_check(+Term)
:- private sys_check/1.
:- meta_predicate sys_check(-1).
sys_check(_) :-
   throw(error(existence_error(body,sys_check/1),_)).

/**
 * sys_withdraw(A):
 * This predicate cannot be executed. It only serves as a
 * goal expansion wrapper for the withdraw of the old of a callable..
 */
% sys_withdraw(+Term)
:- private sys_withdraw/1.
:- meta_predicate sys_withdraw(-1).
sys_withdraw(_) :-
   throw(error(existence_error(body,sys_withdraw/1),_)).

simp:goal_simplification(sys_old(A), _) :-
   var(A), !, fail.
simp:goal_simplification(sys_old(phaseout(H)), sys_withdraw(H)).
simp:goal_simplification(sys_old(posted(H)), sys_check(H)).
simp:goal_simplification(sys_old(phaseout_posted(H)), sys_withdraw(H)).
simp:goal_simplification(sys_old((  A, B)), R) :-
   simplify_goal(sys_old(A), U),
   simplify_goal(sys_old(B), V),
   simplify_goal((  U, V), R).
simp:goal_simplification(sys_old((  A; B)), R) :-
   simplify_goal(sys_old(A), U),
   simplify_goal(sys_old(B), V),
   simplify_goal((  U; V), R).
simp:goal_simplification(sys_old(H), H).

/**
 * sys_oldnew(A):
 * This predicate cannot be executed. It only serves as a
 * goal expansion wrapper for the delta of a goal or for the
 * old of a goal.
 */
% sys_oldnew(+Goal)
:- private sys_oldnew/1.
:- meta_predicate sys_oldnew(0).
sys_oldnew(_) :-
   throw(error(existence_error(body,sys_oldnew/1),_)).

simp:goal_simplification(sys_oldnew(A), _) :-
   var(A), !, fail.
simp:goal_simplification(sys_oldnew(phaseout(H)), sys_withdraw(H)).
simp:goal_simplification(sys_oldnew(posted(H)), (  sys_check(H)
                                                ;  sys_keep(H, true))).
simp:goal_simplification(sys_oldnew(phaseout_posted(H)), (  sys_withdraw(H)
                                                         ;  sys_drop(H, true))).
simp:goal_simplification(sys_oldnew((  A, B)), R) :-
   simplify_goal(sys_oldnew(A), U),
   simplify_goal(sys_oldnew(B), V),
   simplify_goal((  U, V), R).
simp:goal_simplification(sys_oldnew((  A; B)), R) :-
   simplify_goal(sys_oldnew(A), U),
   simplify_goal(sys_oldnew(B), V),
   simplify_goal((  U; V), R).
simp:goal_simplification(sys_oldnew(H), H).

/**********************************************************/
/* Keep, Drop & Minus Shifting                            */
/**********************************************************/

% usual blockers
simp:goal_simplification((  _, A, _), _) :-
   var(A), !, fail.

% replace sys_keep/sys_keep by sys_keep
simp:goal_simplification((  sys_keep(B, P),
                            sys_keep(C, Q)), sys_keep(B, H)) :-
   sys_unify(B, C, C, true, L),
   simplify_goal((  Q, L), J),
   simplify_goal((  P, J), H).
simp:goal_simplification((  sys_keep(_, _),
                            sys_keep(_, _)), sys_none).
simp:goal_simplification((  sys_keep(B, P),
                            sys_keep(C, Q), D), (  sys_keep(B, H), D)) :-
   sys_unify(B, C, C, true, L),
   simplify_goal((  Q, L), J),
   simplify_goal((  P, J), H).
simp:goal_simplification((  sys_keep(_, _),
                            sys_keep(_, _), _), sys_none).

% replace sys_keep/sys_drop by sys_drop
simp:goal_simplification((  sys_keep(B, P),
                            sys_drop(C, Q)), sys_drop(B, H)) :-
   sys_unify(B, C, C, true, L),
   simplify_goal((  Q, L), J),
   simplify_goal((  P, J), H).
simp:goal_simplification((  sys_keep(_, _),
                            sys_drop(_, _)), sys_none).
simp:goal_simplification((  sys_keep(B, P),
                            sys_drop(C, Q), D), (  sys_drop(B, H), D)) :-
   sys_unify(B, C, C, true, L),
   simplify_goal((  Q, L), J),
   simplify_goal((  P, J), H).
simp:goal_simplification((  sys_keep(_, _),
                            sys_drop(_, _), _), sys_none).

% replace sys_drop/sys_keep by sys_drop
simp:goal_simplification((  sys_drop(B, P),
                            sys_keep(C, Q)), sys_drop(B, H)) :-
   sys_unify(B, C, C, true, L),
   simplify_goal((  Q, L), J),
   simplify_goal((  P, J), H).
simp:goal_simplification((  sys_drop(_, _),
                            sys_keep(_, _)), sys_none).
simp:goal_simplification((  sys_drop(B, P),
                            sys_keep(C, Q), D), (  sys_drop(B, H), D)) :-
   sys_unify(B, C, C, true, L),
   simplify_goal((  Q, L), J),
   simplify_goal((  P, J), H).
simp:goal_simplification((  sys_drop(_, _),
                            sys_keep(_, _), _), sys_none).

% replace sys_drop/sys_drop by sys_drop
simp:goal_simplification((  sys_drop(B, P),
                            sys_drop(C, Q)), sys_drop(B, H)) :-
   sys_unify(B, C, C, true, L),
   simplify_goal((  Q, L), J),
   simplify_goal((  P, J), H).
simp:goal_simplification((  sys_drop(_, _),
                            sys_drop(_, _)), sys_none).
simp:goal_simplification((  sys_drop(B, P),
                            sys_drop(C, Q), D), (  sys_drop(B, H), D)) :-
   sys_unify(B, C, C, true, L),
   simplify_goal((  Q, L), J),
   simplify_goal((  P, J), H).
simp:goal_simplification((  sys_drop(_, _),
                            sys_drop(_, _), _), sys_none).

% move sys_keep to front
simp:goal_simplification((  A,
                            sys_keep(C, Q)), (  sys_keep(C, Q), A)).
simp:goal_simplification((  A,
                            sys_keep(C, Q), D), (  sys_keep(C, Q), A, D)).

% move sys_drop to front
simp:goal_simplification((  A,
                            sys_drop(C, Q)), (  sys_drop(C, Q), A)).
simp:goal_simplification((  A,
                            sys_drop(C, Q), D), (  sys_drop(C, Q), A, D)).

/**********************************************************/
/* Unification Reduction                                  */
/**********************************************************/

/**
 * sys_unify(A, B, C, L, R):
 * The predicate succeeds when A and B can be possibly
 * unified. The list R then contains further constraints.
 * Otherwise the predicate fails.
 */
% sys_unify(+Term, +Term, +Term, +List, -List)
:- private sys_unify/5.
sys_unify(A, B, D, L, R) :-
   var(A), !,
   sys_replace_site(G, D, sys_eq(A,B)),
   simplify_goal((  L, G), R).
sys_unify(A, B, D, L, R) :-
   var(B), !,
   sys_replace_site(G, D, sys_eq(A,B)),
   simplify_goal((  L, G), R).
sys_unify(A, B, _, _, _) :-
   functor(A, P, Q),
   functor(B, R, S),
   P/Q \== R/S, !, fail.
sys_unify(A, B, D, P, Q) :-
   A =.. [_|L],
   B =.. [_|R],
   sys_unify_list(L, R, D, P, Q).

% sys_unify_list(+List, +List, +Term, +List, -List)
:- private sys_unify_list/5.
sys_unify_list([], [], _, L, L).
sys_unify_list([X|Y], [Z|T], D, P, Q) :-
   sys_unify(X, Z, D, P, H),
   sys_unify_list(Y, T, D, H, Q).

/**********************************************************/
/* Withdraw Collection                                    */
/**********************************************************/

/**
 * sys_minus(A, L, R, B):
 * The predicate succeeds in B with the rewritten A. It will
 * rewrite sys_check/1 and sys_widthdraw/1 goals. For
 * sys_withdraw/1 it will collect clause references in R.
 */
% sys_minus(+Goal, +List, -List, -Goal)
:- private sys_minus/4.
sys_minus(A, I, I, A) :-
   var(A), !.
sys_minus(sys_check(A), I, I, Q) :- !,
   sys_replace_site(Q, A, clause(A,true)).
sys_minus(sys_withdraw(A), I, [R|I], Q) :- !,
   sys_replace_site(Q, A, clause_ref(A,true,R)).
sys_minus((A,B), I, O, J) :- !,
   sys_minus(A, I, H, P),
   sys_minus(B, H, O, Q),
   simplify_goal((  P, Q), J).
sys_minus(A, I, I, A).

/**********************************************************/
/* Term Rewriting                                         */
/**********************************************************/

/**
 * sys_plus(A):
 * This predicate cannot be executed. It only serves as a goal
 * expansion wrapper for the action of a forward chaining rule.
 */
% sys_plus(+Term)
:- private sys_plus/1.
:- meta_predicate sys_plus(-1).
sys_plus(_) :-
   throw(error(existence_error(body,sys_plus/1),_)).

% user:term_expansion(+Term, -Term)
:- public user:term_expansion/2.
:- multifile user:term_expansion/2.
:- meta_predicate user:term_expansion(-1,-1).

% simp:term_simplification(+Term, -Term)
:- public simp:term_simplification/2.
:- multifile simp:term_simplification/2.
:- meta_predicate simp:term_simplification(-1,-1).
:- discontiguous simp:term_simplification/2.

/**
 * A <= C:
 * The construct defines a forward chaining clause with action A
 * and condition C. The forward chaining clause is rewritten into
 * delta computations.
 */
:- public <= /2.
:- meta_predicate <=(-1,-1).
<=(_, _) :-
   throw(error(existence_error(body,<= /2),_)).

user:term_expansion((  H <= B), (sys_plus(H) :-
                                   sys_new(B))).

% The usual blocking
simp:term_simplification((_ :- A, _), _) :-
   var(A), !, fail.

% Or Distribution
simp:term_simplification((A :- D; E), J) :-
   (  sys_special(D)
   ;  sys_special(E)),
   simplify_term((A :- D), U),
   simplify_term((A :- E), H),
   simplify_term(U /\ H, J).

% Detect Keep & Minus and Drop & Minus Combination
simp:term_simplification((sys_plus(B) :-
                            sys_keep(D, Q)),
        (:- discontiguous I) /\
        (:- sys_notrace I) /\
        (E :- H)) :-
   sys_replace_site(G, D, sys_eq(M,sys_delta(B,[]))),
   simplify_goal((  Q, G), H),
   sys_modext_args(D, M, E),
   sys_functor(E, F, A),
   sys_make_indicator(F, A, I).
simp:term_simplification((sys_plus(B) :-
                            sys_drop(D, Q)),
        (:- discontiguous I) /\
        (:- sys_notrace I) /\
        (E :- H)) :-
   sys_replace_site(G, D, sys_eq(M,sys_nabla(B,[]))),
   simplify_goal((  Q, G), H),
   sys_modext_args(D, M, E),
   sys_functor(E, F, A),
   sys_make_indicator(F, A, I).
simp:term_simplification((sys_plus(B) :-
                            sys_keep(D, Q), P),
        (:- discontiguous I) /\
        (:- sys_notrace I) /\
        (F :- N)) :-
   sys_minus(P, [], R, E),
   sys_replace_site(G, D, sys_eq(M,sys_delta(B,R))),
   simplify_goal((  Q, E), O),
   simplify_goal((  O, G), N),
   sys_modext_args(D, M, F),
   sys_functor(F, H, A),
   sys_make_indicator(H, A, I).
simp:term_simplification((sys_plus(B) :-
                            sys_drop(D, Q), P),
        (:- discontiguous I) /\
        (:- sys_notrace I) /\
        (F :- N)) :-
   sys_minus(P, [], R, E),
   sys_replace_site(G, D, sys_eq(M,sys_nabla(B,R))),
   simplify_goal((  Q, E), O),
   simplify_goal((  O, G), N),
   sys_modext_args(D, M, F),
   sys_functor(F, H, A),
   sys_make_indicator(H, A, I).
