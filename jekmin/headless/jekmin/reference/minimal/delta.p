/**
 * This Jekejeke Minlog module provides a couple of operators and
 * predicates to define forward chaining clauses. A forward chaining
 * clause is recognized by the (<=)/2 operator. A forward chaining clause
 * will be rewritten into multiple delta computation rules. A delta
 * computation rule is able to react to the arrival of a single fact.
 * The left hand side accepts all constructors already known from
 * hypothetical reasoning:
 *
 * C <= B     % Forward Clause.
 *
 * The delta computation has functor F/N+1 for an arriving fact with a
 * functor F/N. The forward clause body is a conjunction of literals.
 * Each literal in the body has to be annotated either for delete set
 * inclusion (-)/1, for delta computation (+)/1 or for both (=)/1. If a
 * literal is not annotated only its existence in the database is checked.
 * The set {}/1 constructor can be used to denote a backward chaining
 * goal. The cut !/0 need not be wrapped.
 *
 * This Jekejeke Prolog module additionally provides the plus sign (+)/1
 * as a new constructor for hypothetical reasoning. Namely the embedded
 * implication (=>)/2 and the (<=)/1 continuation variant will both trigger
 * the forward chaining closure computation for surviving arriving facts
 * that are wrapped in a plus sign (+)/1. The computation will iterate
 * over the delete set removal, the surviving fact assumption and the
 * newly implied constructs:
 *
 * Examples:
 * ?- [user].
 * q(X) <= +p(X).
 *
 * Yes
 * ?- +p(a) => q(X).
 * X = a
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
:- use_module(library(minimal/assume)).
:- use_module(library(minimal/hypo)).
:- use_module(library(experiment/ref)).
:- use_module(library(basic/lists)).
:- use_module(library(experiment/simp)).
:- use_module(library(experiment/ref)).

:- public infix(<=).
:- op(1200, xfx, <=).

% Already defined in elem.p
% :- public prefix(-).
% :- op(200, fy, -).

% Already defined in elem.p
% :- public prefix(+).
% :- op(200, fy, +).

:- public prefix(=).
:- op(200, fy, =).

:- public prefix(cosmetic).
:- op(1150, fy, cosmetic).

:- public prefix(forward).
:- op(1150, fy, forward).

/***********************************************************/
/* Delta Directives                                        */
/***********************************************************/

/**
 * cosmetic P, ...:
 * The predicate sets the predicate P to cosmetic.
 */
% cosmetic +Indicators
:- public (cosmetic)/1.
:- meta_predicate (cosmetic 0).
cosmetic [P|Q] :- !,
   sys_cosmetic(P),
   (cosmetic Q).
cosmetic P, Q :- !,
   sys_cosmetic(P),
   (cosmetic Q).
cosmetic [] :- !.
cosmetic P :-
   sys_cosmetic(P).

% sys_cosmetic(+Indicator)
:- private sys_cosmetic/1.
sys_cosmetic(I) :-
   sys_neutral_predicate(I),
   set_predicate_property(I, cosmetic).

/**
 * forward P, ...:
 * The predicate sets the predicate P to cosmetic and static.
 */
% forward +Indicators
:- public (forward)/1.
:- meta_predicate (forward 0).
forward L :-
   (cosmetic L),
   (static L).

/**********************************************************/
/* Continuation Forwarder                                 */
/**********************************************************/

/**
 * +P:
 * The construct prevents that the literal P is directly assumed.
 * Instead the literal P is treated as a fact that arrives in
 * the forward chaining engine.
 */
% hypo: <=(+Rule)
:- public hypo:(<=)/1.
:- multifile hypo:(<=)/1.
:- meta_predicate hypo:(<= -1).
hypo:(<= + X) :- !,
   sys_modext_args(X, U, G),
   findall(U, G, L),
   sys_retire_delta_first(L, X, R, Y),
   sys_hypo_conj([Y|R]).

% hypo:hypo_abnormal(+Rule)
:- public hypo:hypo_abnormal/1.
:- multifile hypo:hypo_abnormal/1.
hypo:hypo_abnormal(+_).

% sys_hypo_conj(+Posts)
:- private sys_hypo_conj/1.
sys_hypo_conj([X|Y]) :-
   <= X,
   sys_hypo_conj(Y).
sys_hypo_conj([]).

% sys_retire_delta_first(+Deltas, +Post, -Posts, -Post)
:- private sys_retire_delta_first/4.
sys_retire_delta_first([sys_keep(Z,S)|Y], X, [Z|T], W) :- !,
   sys_retire_delta_first(Y, X, T, W),
   sys_retire_ref(S).
sys_retire_delta_first([sys_drop(Z,S)|Y], _, [Z|T], unit) :-
   sys_retire_delta_second(Y, T), !,
   sys_retire_ref(S).
sys_retire_delta_first([_|_], X, _, _) :-
   throw(error(permission_error(remove,delta,X),_)).
sys_retire_delta_first([], X, [], X).

% sys_retire_delta_second(+Deltas, -Posts)
:- private sys_retire_delta_second/2.
sys_retire_delta_second([sys_keep(Z,S)|Y], [Z|T]) :- !,
   sys_retire_delta_second(Y, T),
   sys_retire_ref(S).
sys_retire_delta_second([], []).

/**********************************************************/
/* Hypothetical Forwarder                                 */
/**********************************************************/

% hypo: =>(+Rule, +Goal)
:- public hypo: => /2.
:- multifile hypo: => /2.
:- meta_predicate hypo:(-1=>0).
hypo:(+ X => B) :- !,
   sys_modext_args(X, U, G),
   findall(U, G, L),
   sys_retire_delta_first(L, X, R, Y),
   sys_hypo_conj([Y|R], B),
   sys_assume_delta_first(L).

% sys_hypo_conj(+Posts, +Goal)
:- private sys_hypo_conj/2.
sys_hypo_conj([X|Y], G) :- X =>
      sys_hypo_conj(Y, G).
sys_hypo_conj([], G) :-
   call(G).

% sys_assume_delta_first(+Deltas)
:- private sys_assume_delta_first/1.
sys_assume_delta_first([sys_keep(_,S)|Y]) :- !,
   sys_assume_ref(S),
   sys_assume_delta_first(Y).
sys_assume_delta_first([sys_drop(_,S)|Y]) :-
   sys_assume_ref(S),
   sys_assume_delta_second(Y).
sys_assume_delta_first([]).

% sys_assume_delta_second(+Deltas)
:- private sys_assume_delta_second/1.
sys_assume_delta_second([sys_keep(_,S)|Y]) :- !,
   sys_assume_ref(S),
   sys_assume_delta_second(Y).
sys_assume_delta_second([]).

/***********************************************************/
/* Disjunction Reduction                                   */
/***********************************************************/

% simp:goal_simplification(+Goal, -Goal)
:- public simp:goal_simplification/2.
:- multifile simp:goal_simplification/2.
:- meta_predicate simp:goal_simplification(0,0).
:- discontiguous simp:goal_simplification/2.

% sys_none
:- private sys_none/0.
sys_none :-
   throw(error(existence_error(body,sys_none/0),_)).

% sys_or(+Goal, +Goal)
:- private sys_or/2.
:- meta_predicate sys_or(0,0).
sys_or(_, _) :-
   throw(error(existence_error(body,sys_or/2),_)).

% disjunction + disjunction reduction
simp:goal_simplification(sys_or(A, _), _) :-
   var(A), !, fail.
simp:goal_simplification(sys_or(sys_none, A), A).
simp:goal_simplification(sys_or(sys_or(A, B), C), J) :-
   simplify_goal(sys_or(B, C), H),
   simplify_goal(sys_or(A, H), J).
simp:goal_simplification(sys_or(_, A), _) :-
   var(A), !, fail.
simp:goal_simplification(sys_or(A, sys_none), A).

% conjunction + disjunction reduction
simp:goal_simplification((  sys_none, _), sys_none).
simp:goal_simplification((  _, sys_none), sys_none).
simp:goal_simplification((  sys_or(A, B), C), R) :-
   simplify_goal((  A, C), H),
   simplify_goal((  B, C), J),
   simplify_goal(sys_or(H, J), R).
simp:goal_simplification((  A,
                            sys_or(B, C)), R) :-
   simplify_goal((  A, B), H),
   simplify_goal((  A, C), J),
   simplify_goal(sys_or(H, J), R).

/**********************************************************/
/* New, Old & OldNew Reduction                            */
/**********************************************************/

% sys_new(+Goal)
:- private sys_new/1.
:- meta_predicate sys_new(0).
sys_new(_) :-
   throw(error(existence_error(body,sys_new/1),_)).

% user:goal_expansion(+Term, -Term)
:- public user:goal_expansion/2.
:- multifile user:goal_expansion/2.
:- meta_predicate user:goal_expansion(0,0).
:- discontiguous user:goal_expansion/2.

user:goal_expansion(sys_new(A), _) :-
   var(A), !, fail.

/**
 * A, B:
 * The forward predicate reacts when either A or B react, and
 * the other succeeds, or when both react. The predicate
 * succeeds when A and B succeed
 */
user:goal_expansion(sys_new((  A, B)),
        sys_or((  sys_old(A),
                  sys_new(B)),
           (  sys_new(A),
              sys_oldnew(B)))).

/**
 * {A}:
 * The goal A is checked via backward chaining.
 */
:- public {}/1.
:- meta_predicate {0}.
{_} :-
   throw(error(existence_error(body,{}/1),_)).

user:goal_expansion(sys_new({_}), sys_none).

/**
 * !:
 * The cut does also apply in a forward clause.
 */
user:goal_expansion(sys_new(!), sys_none).

/**
 * -P:
 * The literal P is a fact from the database that is removed.
 */
% already defined in hypo.p
% :- public (-)/1.
% :- meta_predicate -(0).
% -(_) :- throw(error(existence_error(body, (-)/1), _)).

user:goal_expansion(sys_new(- _), sys_none).

/**
 * +P:
 * The literal P is a fact that can arrive.
 */
:- public (+)/1.
:- meta_predicate +0.
+ _ :-
   throw(error(existence_error(body,(+)/1),_)).

user:goal_expansion(sys_new(+ H), sys_keep(H)).

/**
 * =P:
 * The literal P is a fact that can arrive and that is removed.
 */
:- public (=)/1.
:- meta_predicate =0.
= _ :-
   throw(error(existence_error(body,(=)/1),_)).

user:goal_expansion(sys_new(= H), sys_drop(H)).

/**
 * P:
 * The literal P is a fact from the database.
 */
user:goal_expansion(sys_new(_), sys_none).

% sys_old(+Goal)
:- private sys_old/1.
:- meta_predicate sys_old(0).
sys_old(_) :-
   throw(error(existence_error(body,sys_old/1),_)).

user:goal_expansion(sys_old(A), _) :-
   var(A), !, fail.
user:goal_expansion(sys_old((  A, B)), (  sys_old(A),
                                          sys_old(B))).
user:goal_expansion(sys_old({H}), H).
user:goal_expansion(sys_old(!), !).
user:goal_expansion(sys_old(- H), - H).
user:goal_expansion(sys_old(+ H), + H).
user:goal_expansion(sys_old(= H), - H).
user:goal_expansion(sys_old(H), + H).

% sys_oldnew(+Goal)
:- private sys_oldnew/1.
:- meta_predicate sys_oldnew(0).
sys_oldnew(_) :-
   throw(error(existence_error(body,sys_oldnew/1),_)).

user:goal_expansion(sys_oldnew(A), _) :-
   var(A), !, fail.
user:goal_expansion(sys_oldnew((  A, B)), (  sys_oldnew(A),
                                             sys_oldnew(B))).
user:goal_expansion(sys_oldnew({H}), H).
user:goal_expansion(sys_oldnew(!), !).
user:goal_expansion(sys_oldnew(- H), - H).
user:goal_expansion(sys_oldnew(+ H), sys_or(+ H, sys_keep(H))).
user:goal_expansion(sys_oldnew(= H), sys_or(- H, sys_drop(H))).
user:goal_expansion(sys_oldnew(H), + H).

/**********************************************************/
/* Keep, Drop & Minus Shifting                            */
/**********************************************************/

% sys_keep(+Goal)
:- private sys_keep/1.
:- meta_predicate sys_keep(-1).
sys_keep(_) :-
   throw(error(existence_error(body,sys_keep/1),_)).

% sys_drop(+Goal)
:- private sys_drop/1.
:- meta_predicate sys_drop(-1).
sys_drop(_) :-
   throw(error(existence_error(body,sys_drop/1),_)).

% move sys_keep to front, and replace sys_keep/sys_keep by sys_keep/sys_unify
simp:goal_simplification((  sys_keep(B),
                            sys_keep(C)), R) :-
   simplify_goal(sys_unify(B, C), U),
   simplify_goal((  sys_keep(B), U), R).
simp:goal_simplification((  A,
                            sys_keep(C)), R) :-
   simplify_goal((  sys_keep(C), A), R).
simp:goal_simplification((  _, A, _), _) :-
   var(A), !, fail.
simp:goal_simplification((  sys_keep(B),
                            sys_keep(C), D), J) :-
   simplify_goal(sys_unify(B, C), U),
   simplify_goal((  U, D), H),
   simplify_goal((  sys_keep(B), H), J).
simp:goal_simplification((  A,
                            sys_keep(C), D), J) :-
   simplify_goal((  A, D), H),
   simplify_goal((  sys_keep(C), H), J).

% sys_minus(+List, +List)
:- private sys_minus/2.
sys_minus(_, _) :-
   throw(error(existence_error(body,sys_minus/2),_)).

% move sys_minus to front, stop at sys_keep, replace goal by clause
simp:goal_simplification((  sys_keep(_),
                            sys_minus(_, _)), _) :- !, fail.
simp:goal_simplification((  sys_keep(_),
                            sys_minus(_, _), _), _) :- !, fail.
simp:goal_simplification((  sys_drop(A),
                            sys_minus(L, R)),
        (  sys_keep(A),
           sys_minus([A|L], R))).
simp:goal_simplification((  sys_drop(A),
                            sys_minus(L, R), B),
        (  sys_keep(A),
           sys_minus([A|L], R), B)).
simp:goal_simplification((  + A, _), _) :-
   var(A), !, fail.
simp:goal_simplification((  + A,
                            sys_minus(L, R)),
        (  sys_minus(L, R), C)) :-
   sys_replace_site(C, A, clause(A,true)).
simp:goal_simplification((  + A,
                            sys_minus(L, R), B),
        (  sys_minus(L, R), D, B)) :-
   sys_replace_site(D, A, clause(A,true)).
simp:goal_simplification((  - A, _), _) :-
   var(A), !, fail.
simp:goal_simplification((  - A,
                            sys_minus(L, R)),
        (  sys_minus(L, [P|R]), C)) :-
   sys_replace_site(C, A, clause_ref(A,true,P)).
simp:goal_simplification((  - A,
                            sys_minus(L, R), B),
        (  sys_minus(L, [P|R]), D, B)) :-
   sys_replace_site(D, A, clause_ref(A,true,P)).
simp:goal_simplification((  A,
                            sys_minus(L, R)), (  sys_minus(L, R), A)).
simp:goal_simplification((  A,
                            sys_minus(L, R), B), (  sys_minus(L, R), A, B)).

/***********************************************************/
/* Unification Reduction                                   */
/***********************************************************/

% sys_unify(+Term, +Term)
:- private sys_unify/2.
sys_unify(_, _) :-
   throw(error(existence_error(body,sys_unify/2),_)).

% perform unification
simp:goal_simplification(sys_unify(A, B), true) :-
   var(A),
   var(B),
   A == B.
simp:goal_simplification(sys_unify(A, B), A = B) :-
   var(A).
simp:goal_simplification(sys_unify(A, B), A = B) :-
   var(B).
simp:goal_simplification(sys_unify(A, B), sys_none) :-
   functor(A, P, Q),
   functor(B, R, S),
   P/Q \== R/S.
simp:goal_simplification(sys_unify(A, B), C) :-
   A =.. [_|L],
   B =.. [_|R],
   sys_unify_list(L, R, C).

% sys_unify_list(+List, +List, +Goal)
:- private sys_unify_list/3.
sys_unify_list([], [], true).
sys_unify_list([X|Y], [Z|T], C) :-
   simplify_goal(sys_unify(X, Z), A),
   sys_unify_list(Y, T, B),
   simplify_goal((  A, B), C).

/**********************************************************/
/* Term Rewriting                                         */
/**********************************************************/

% user:term_expansion(+Term, -Term)
:- public user:term_expansion/2.
:- multifile user:term_expansion/2.
:- meta_predicate user:term_expansion(-1,-1).

% simp:term_simplification(+Term, -Term)
:- public simp:term_simplification/2.
:- multifile simp:term_simplification/2.
:- meta_predicate simp:term_simplification(-1,-1).
:- discontiguous simp:term_simplification/2.

% Or Distribution
simp:term_simplification((_ :- A), _) :-
   var(A), !, fail.
simp:term_simplification((A :-
                            sys_or(D, E)), J) :-
   simplify_term((A :- D), U),
   simplify_term((A :- E), H),
   simplify_term(U /\ H, J).

:- private sys_plus/1.
:- meta_predicate sys_plus(0).
sys_plus(_) :-
   throw(error(existence_error(body,sys_plus/1),_)).

:- public (<=)/2.
:- meta_predicate (0<= -1).
(_ <= _) :-
   throw(error(existence_error(body,(<=)/2),_)).

/**
 * H <= B:
 * The construct defines a forward chaining clause with head H and
 * body B. The clause is rewritten by the term expansion mechanism
 * in possibly multiple ordinary clauses for possibly multiple
 * delta predicates. The forward clause doesn't support syntactic
 * forward chaining.
 */
user:term_expansion((H <= B), (sys_plus(H) :-
                                 sys_new(B),
                                 sys_minus([], []))).

% Detect Keep & Minus Combination
simp:term_simplification((A :- _), _) :-
   var(A), !, fail.
simp:term_simplification((_ :- A, _), _) :-
   var(A), !, fail.
simp:term_simplification((_ :- _, A), _) :-
   var(A), !, fail.
simp:term_simplification((sys_plus(B) :-
                            sys_keep(D),
                            sys_minus([U], R)),
        (:- discontiguous I) /\
        (:- cosmetic I) /\
        (E :- G)) :-
   D == U,
   sys_replace_site(G, D, M=sys_drop(B,R)),
   sys_modext_args(D, M, E),
   sys_functor(E, F, A),
   sys_make_indicator(F, A, I).
simp:term_simplification((sys_plus(B) :-
                            sys_keep(D),
                            sys_minus([], R)),
        (:- discontiguous I) /\
        (:- cosmetic I) /\
        (E :- G)) :-
   sys_replace_site(G, D, M=sys_keep(B,R)),
   sys_modext_args(D, M, E),
   sys_functor(E, F, A),
   sys_make_indicator(F, A, I).
simp:term_simplification((sys_plus(_) :-
                            sys_keep(_),
                            sys_minus(_, _)), _) :-
   throw(error(syntax_error(sys_minus_unexpected),_)).
simp:term_simplification((_ :- _, A, _), _) :-
   var(A), !, fail.
simp:term_simplification((sys_plus(B) :-
                            sys_keep(D),
                            sys_minus([U], R), E),
        (:- discontiguous I) /\
        (:- cosmetic I) /\
        (F :- N)) :-
   D == U,
   sys_replace_site(G, D, M=sys_drop(B,R)),
   simplify_goal((  E, G), N),
   sys_modext_args(D, M, F),
   sys_functor(F, H, A),
   sys_make_indicator(H, A, I).
simp:term_simplification((sys_plus(B) :-
                            sys_keep(D),
                            sys_minus([], R), E),
        (:- discontiguous I) /\
        (:- cosmetic I) /\
        (F :- N)) :-
   sys_replace_site(G, D, M=sys_keep(B,R)),
   simplify_goal((  E, G), N),
   sys_modext_args(D, M, F),
   sys_functor(F, H, A),
   sys_make_indicator(H, A, I).
simp:term_simplification((sys_plus(_) :-
                            sys_keep(_),
                            sys_minus(_, _), _), _) :-
   throw(error(syntax_error(sys_minus_unexpected),_)).

/**********************************************************/
/* Port Cosmetics                                         */
/**********************************************************/

% goal_exposing(+PortGoal, -PortGoal, -Context)
:- public user:goal_exposing/3.
:- multifile user:goal_exposing/3.
user:goal_exposing(P-G, Q-O, -1) :-
   sys_callable(G),
   sys_functor(G, F, A),
   sys_make_indicator(F, A, I),
   predicate_property(I, cosmetic), !,
   sys_expose_helper(P, G, Q, O).

% sys_expose_helper(+Port, +Goal, -Port, -Goal)
:- private sys_expose_helper/4.
sys_expose_helper(call, G, post, O) :-
   sys_fetch_event(G, O).
sys_expose_helper(fail, G, done, O) :-
   sys_fetch_event(G, O).
sys_expose_helper(exit, G, diff, O) :-
   sys_fetch_delta(G, O).
sys_expose_helper(redo, G, more, O) :-
   sys_fetch_delta(G, O).
sys_expose_helper(goal, G, fact, O) :-
   sys_fetch_event(G, O).

% sys_fetch_event(+Goal, -Goal)
:- private sys_fetch_event/2.
sys_fetch_event(G, J) :-
   sys_univ(G, [F|L]),
   last(L, _, R),
   sys_univ(J, [F|R]).

% sys_fetch_delta(+Goal, -Delta)
:- private sys_fetch_delta/2.
sys_fetch_delta(G, O) :-
   sys_univ(G, [F|L]),
   last(L, X, R),
   sys_expose_delta(X, F, R, O).

% sys_expose_delta(+Elem, +Atom, +List, -Goal)
:- private sys_expose_delta/4.
sys_expose_delta(sys_keep(A,_), _, _, A).
sys_expose_delta(U, F, R, B) :-
   U = sys_drop(A,_),
   sys_univ(J, [F|R]),
   sys_replace_site(B, U, (A<= -J)).
