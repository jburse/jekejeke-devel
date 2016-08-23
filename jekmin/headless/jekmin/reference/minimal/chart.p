/**
 * This Jekejeke Minlog module allows executing definite clause grammars
 * (DCGs) in a forward manner. The ordinary DCG rules of Jekejeke Prolog
 * are not suitable for this purpose since they model terminals by
 * [T|O] = I. We therefore provide special chart DCG rules as part of
 * Jekejeke Minilog which model terminals by ‘D’(T,I), O is I+1. Chart
 * DCG rules are identified by the (==>)/2 operator:
 *
 * P ==> Q.      % chart DCG rule with forward chaining.
 *
 * Chart DCG rules do currently not allow for push backs. The term
 * expansion augments the head and body by two additional parameters
 * that are to represent the sentence position before and after the
 * parsing. A predicate identifier p/n will thus be turned into a
 * predicate identifier p/n+2. Further the DCG chart operator (==>)/2
 * is replaced by the forward chaining operator (<=)/2:
 *
 * chart(P, I, O) <= sys_sys_chart(Q, I, O).
 *
 * The term expansion will then go to work and tackle the head, whereas
 * the goal expansion will tackle the body. Compared to ordinary DCG
 * rules, the chart DCG rules support fewer cnstructs. For example we
 * do not yet support the conditional (->)/2 and the higher order calls
 * call/n. On the other hand we already support the grammar negation (\+)/1
 * and the forward chaining annotation (+)/1 is automatically placed:
 *
 * Let’s consider the following example chart DCG rule:
 *
 * p(X) ==> "a", q(X), {r(X)}.  % chart DCG rule
 *
 * As an intermediate results the chart DCG rule will be turned into:
 *
 * +p(X, I, O) <= +'D'(97, I), {H is I+1}, q(X, H, O), {r(X)}.
 *
 * The above rule will then be turned into a delta computation for the
 * predicate ’D’/2. In general only the first literal of a DCG chart rule
 * will be translated into a delta computation rule, improving efficiency.
 * The chart/3 construct can also be used in hypothetical queries to
 * generate the ‘D’/2 facts for a given sentence or in ordinary backward
 * chaining query the result of parsing. See the palindrom example
 * for more details.
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

:- package(library(jekmin/reference/minimal)).

:- module(chart, []).
:- use_module(library(minimal/hypo)).
:- use_module(library(minimal/delta)).

:- public infix(==>).
:- op(1200, xfx, ==>).

/**********************************************************/
/* Goal Rewriting I                                       */
/**********************************************************/

/**
 * chart(A, I, O):
 * Succeeds when the index I starts with the phrase A giving the remainder O.
 */
% chart(+Phrase, +Pos, +Pos)
:- public chart/3.
:- meta_predicate chart(2,?,?).
chart(P, _, _) :-
   sys_var(P),
   throw(error(instantiation_error,_)).
chart(P, I, O) :-
   expand_goal(sys_chart(P, I, O), Q),
   call(Q).

% user:goal_expansion(+Goal, -Goal)
:- public user:goal_expansion/2.
:- multifile user:goal_expansion/2.
:- meta_predicate user:goal_expansion(0,0).
:- discontiguous user:goal_expansion/2.

/**
 * chart(A, O):
 * Succeeds when the first index starts with the phrase A giving the remainder O.
 */
% chart(+Goal, -Integer)
:- public chart/2.
:- meta_predicate chart(2,?).
chart(_, _) :-
   throw(error(existence_error(body,chart/2),_)).

user:goal_expansion(chart(P, N), chart(P, 0, N)).
user:goal_expansion(chart(P, _, _), _) :-
   sys_var(P), !, fail.
user:goal_expansion(chart((  A, B), I, O), (  chart(A, I, H),
                                              chart(B, H, O))).
user:goal_expansion(chart(P, _, _), P) :-
   P = fail.
user:goal_expansion(chart({A}, I, I), A).
user:goal_expansion(chart(U, I, I), Q) :-
   U = (\+A),
   sys_replace_site(Q, U, \+chart(A,I,_)).
user:goal_expansion(chart(P, I, I), P) :-
   P = !.
user:goal_expansion(chart([], I, I), true).
user:goal_expansion(chart(U, I, O), (  Q,
                                       H is I + 1,
                                       chart(B, H, O))) :-
   U = [A|B],
   sys_replace_site(Q, U, 'D'(A,I)).
user:goal_expansion(chart(P, I, O), Q) :-
   sys_modext_args(P, I, O, Q).

/**********************************************************/
/* Goal Rewriting II                                      */
/**********************************************************/

:- public sys_sys_chart/3.
:- meta_predicate sys_sys_chart(2,?,?).
sys_sys_chart(_, _, _) :-
   throw(error(existence_error(body,sys_sys_chart/3),_)).
user:goal_expansion(sys_sys_chart(P, _, _), _) :-
   sys_var(P), !, fail.
user:goal_expansion(sys_sys_chart((  A, B), I, O), (  sys_sys_chart(A, I, H),
                                                      sys_chart(B, H, O))).
user:goal_expansion(sys_sys_chart(U, I, O), (  + Q,
                                               {H is I + 1},
                                               sys_chart(B, H, O))) :-
   U = [A|B],
   sys_replace_site(Q, U, 'D'(A,I)).
user:goal_expansion(sys_sys_chart(P, I, O), + Q) :-
   sys_modext_args(P, I, O, Q).

:- public sys_chart/3.
:- meta_predicate sys_chart(2,?,?).
sys_chart(_, _, _) :-
   throw(error(existence_error(body,sys_chart/3),_)).

% sys_chart(+Goal, +Integer, -Integer)
user:goal_expansion(sys_chart(P, _, _), _) :-
   sys_var(P), !, fail.

/**
 * fail:
 * The grammar connective fails.
 */
user:goal_expansion(sys_chart(P, _, _), P) :-
   P = fail.

/**
 * A, B:
 * The predicate reacts when either A or B react, and the other
 * succeeds, or when both react. The predicate succeeds when A
 * and B succeed. The output of A is conjoined with the input of B.
 */
user:goal_expansion(sys_chart((  A, B), I, O), (  sys_chart(A, I, H),
                                                  sys_chart(B, H, O))).

/**
 * {A}:
 * The grammar connective succeeds whenever the goal argument A succeeds.
 * The goal argument A is cut transparent and not chart translated.
 */
user:goal_expansion(sys_chart({A}, I, I), {A}).

/**
 * \+ A:
 * When A succeeds, then the predicate fails. Otherwise the predicate
 * succeeds. The second argument is left loose.
 */
user:goal_expansion(sys_chart(U, I, I), Q) :-
   U = (\+A),
   sys_replace_site(Q, U, {\+chart(A,I,_)}).

/**
 * !:
 * The grammar connective removes pending choice and then succeeds once.
 */
user:goal_expansion(sys_chart(P, I, I), P) :-
   P = !.

/**
 * [A1, ..., An]:
 * The grammar connective denotes terminals A1, ..., An that can arrive.
 */
user:goal_expansion(sys_chart([], I, I), true).
user:goal_expansion(sys_chart(U, I, O), (  Q,
                                           {H is I + 1},
                                           sys_chart(B, H, O))) :-
   U = [A|B],
   sys_replace_site(Q, U, 'D'(A,I)).

/**
 * P:
 * The literal is a fact that can arrive. The literal is augmented
 * by two arguments.
 */
user:goal_expansion(sys_chart(P, I, O), Q) :-
   sys_modext_args(P, I, O, Q).

/**********************************************************/
/* Term Rewriting                                         */
/**********************************************************/

% hypo: =>(+Rule, +Goal)
:- public hypo: => /2.
:- multifile hypo: => /2.
:- meta_predicate hypo:(-1=>0).

/**
 * chart(A, I, O):
 * Assumes that the phrase A starting with index I gives the remainder O.
 */
% chart(+Term, +Integer, -Integer)
hypo:(chart(P, _, _) => _) :-
   sys_var(P),
   throw(error(instantiation_error,_)).
hypo:(chart(P, I, O) => G) :- !,
   expand_term(chart(P, I, O), Q), Q => G.

% hypo:post_impl(+Term)
:- public hypo:(<=)/1.
:- multifile hypo:(<=)/1.
:- meta_predicate hypo:(<= -1).
hypo:(<= chart(P, _, _)) :-
   sys_var(P),
   throw(error(instantiation_error,_)).
hypo:(<= chart(P, I, O)) :- !,
   expand_term(chart(P, I, O), Q),
   <= Q.

:- public hypo:hypo_abnormal/1.
:- multifile hypo:hypo_abnormal/1.
hypo:hypo_abnormal(chart(_,_,_)).

% user:term_expansion(+Term, -Term)
:- public user:term_expansion/2.
:- multifile user:term_expansion/2.
:- meta_predicate user:term_expansion(-1,-1).
:- discontiguous user:term_expansion/2.

/**
 * chart(A, O):
 * Assumes that the phrase A starting with the first index gives the remainder O.
 */
% chart(+Term, -Integer)
user:term_expansion(chart(P, N), chart(P, 0, N)).
user:term_expansion(chart(P, _, _), _) :-
   sys_var(P), !, fail.

/**
 * [A1, ..., An]:
 * The grammar connective directly assumes the terminals A1, ..., An.
 */
user:term_expansion(chart([], I, I), unit).
user:term_expansion(chart(U, I, O), chart(B, H, O) /\
                                    + Q) :-
   U = [A|B],
   sys_replace_site(Q, U, 'D'(A,I)),
   H is I + 1.

/**
 * P:
 * The literal P is directly assumed. The literal is augmented by two arguments.
 */
user:term_expansion(chart(P, I, O), + Q) :-
   sys_modext_args(P, I, O, Q).

/**
 * H ==> B:
 * The construct defines a forward chart rule with chart head H and
 * chart body B.
 */
:- public ==> /2.
:- meta_predicate (2==> -3).
(_ ==> _) :-
   throw(error(existence_error(body,==> /2),_)).

user:term_expansion((P ==> A), (chart(P, I, O) <=
                                  sys_sys_chart(A, I, O))).
