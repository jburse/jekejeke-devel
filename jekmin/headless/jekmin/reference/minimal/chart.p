/**
 * This Jekejeke Minlog module allows executing definite clause grammars
 * (DCGs) in a forward manner. The ordinary DCG rules of Jekejeke Prolog
 * are not suitable for this purpose since they model terminals by
 * [T|O] = I. We therefore provide special chart DCG rules as part of
 * which model terminals by ‘D’(T,I,O). Chart DCG rules are identified
 * by the (==:)/2 operator:
 *
 * P ==: Q.      % chart DCG rule.
 *
 * Chart DCG rules do currently not allow for push backs. The term
 * expansion augments the head and body by two additional parameters
 * that are to represent the sentence position before and after the
 * parsing. A predicate identifier p/n will thus be turned into a
 * predicate identifier p/n+2. Further the DCG chart operator (==:)/2
 * is replaced by the forward chaining operator (<=)/2:
 *
 * chart_post(P, I, O) <= chart_posted(Q, I, O).
 *
 * The expansion will then go to work and tackle the head and the body.
 * Compared to ordinary DCG rules, the chart DCG rules support fewer
 * constructs. For example we do not yet support the conditional (->)/2
 * and the higher order calls call/n. On the other hand the look-ahead
 * negation (\+) is already supported.
 *
 * Let us consider the following example chart DCG rule:
 *
 * p(X) ==: "a", q(X), {r(X)}.  % chart DCG rule
 *
 * As an intermediate results the chart DCG rule will be turned into:
 *
 * post(p(X, I, O)) <= posted('D'(97, I, H)), q(X, H, O), r(X).
 *
 * The above rule will then be turned into a delta computation for the
 * predicate ’D’/3. In general only the first literal of a DCG chart rule
 * will be translated into a delta computation rule, improving efficiency.
 * The words/3 construct can be used to generate the ‘D’/3 facts and the
 * chart/3 construct can be used to query the result of parsing. See the
 * palindrom example for more details.
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

:- module(chart, []).
:- reexport(library(minimal/delta)).

:- public infix(==:).
:- op(1200, xfx, ==:).

/**********************************************************/
/* Word Posting                                           */
/**********************************************************/

/**
 * words([A1, ..., An], I, O):
 * words([A1, ..., An], I, O, G):
 * Post the words A1,..,An from index I to index O before further solving.
 */
% words(+List, +Integer, -Integer)
:- public words/3.
words(L, _, _) :-
   var(L),
   throw(error(instantiation_error,_)).
words(L, I, O) :-
   expand_goal(words(L, I, O), Q),
   call(Q).

% words(+List, +Integer, -Integer, +Goal)
:- public words/4.
words(L, _, _, _) :-
   var(L),
   throw(error(instantiation_error,_)).
words(L, I, O, G) :-
   expand_goal(words(L, I, O), Q),
   call(Q, G).

% user:goal_expansion(+Goal, -Goal)
:- public user:goal_expansion/2.
:- multifile user:goal_expansion/2.
:- meta_predicate user:goal_expansion(0,0).
:- discontiguous user:goal_expansion/2.

user:goal_expansion(words(L, _, _), _) :-
   var(L), !, fail.
user:goal_expansion(words([], I, I), true).
user:goal_expansion(words(U, I, O), (  words(B, H, O), G)) :-
   U = [A|B],
   H is I+1,
   sys_replace_site(Q, U, 'D'(A,I,H)),
   sys_replace_site(G, Q, post(Q)).

/**********************************************************/
/* Chart Checking                                         */
/**********************************************************/

/**
 * chart(A, I, O):
 * Succeeds when there is a phrase A from index I to index O.
 */
% chart(+Phrase, +Integer, -Integer)
:- public chart/3.
:- meta_predicate chart(2,?,?).
chart(P, _, _) :-
   sys_var(P),
   throw(error(instantiation_error,_)).
chart(P, I, O) :-
   expand_goal(chart(P, I, O), Q),
   call(Q).

/**********************************************************/
/* Goal Rewriting Steadfast                               */
/**********************************************************/

/**
 * P:
 * The non-terminal P is checked.
 */
user:goal_expansion(chart(P, I, O), chart(P, I, O)) :-
   sys_var(P).
user:goal_expansion(chart(P, I, O), R) :-
   chart_expansion(P, I, O, R).
user:goal_expansion(chart(P, I, O), Q) :-
   sys_modext_args(P, I, O, Q).

/**
 * chart_expansion(A, I, O, G)
 * Succeeds when the phrase A extended by the input I and the
 * output O results in the steadfast goal G.
 */
% chart_expansion(+Phrase, +List, -List, -Goal)
:- private chart_expansion/4.
:- meta_predicate chart_expansion(2,?,?,0).
:- discontiguous chart_expansion/4.
:- set_predicate_property(chart_expansion/4, sys_noexpand).

/**
 * fail:
 * The grammar fails.
 */
chart_expansion(P, _, _, P) :-
   P = fail.

/**
 * A, B:
 * The output of A is conjoined with the input of B.
 */
chart_expansion((  A, B), I, O, (  chart(A, I, H),
                                   sys_chart(B, H, O))) :-
   sys_var(A).
chart_expansion((  U, B), I, O, (  P,
                                   chart(B, I, O))) :-
   chart_barrier(U, I, P).
chart_expansion((  A, B), I, O, (  chart(A, I, H),
                                   sys_chart(B, H, O))).

/**
 * A; B:
 * The grammar succeeds when A succeeds or when B succeeds.
 */
chart_expansion((  A; B), I, O, (  chart(A, I, O)
                                ;  chart(B, I, O))).

/**
 * [A1, ..., An]:
 * The terminals A1, ..., An are checked.
 */
chart_expansion(U, I, O, Q) :-
   U = [],
   sys_replace_site(Q, U, I=O).
chart_expansion(U, I, O, (  Q,
                            sys_chart(B, H, O))) :-
   U = [A|B],
   sys_replace_site(Q, U, 'D'(A,I,H)).

chart_expansion(U, I, O, (  P, Q)) :-
   chart_barrier(U, I, P),
   sys_replace_site(Q, U, I=O).

:- private chart_barrier/3.
:- meta_predicate chart_barrier(2,?,0).
:- discontiguous chart_barrier/3.
:- set_predicate_property(chart_barrier/3, sys_noexpand).

/**
 * !:
 * The choice points are removed.
 */
chart_barrier(U, _, U) :-
   U = !.

/**
 * {A}:
 * The auxiliary condition is checked.
 */
chart_barrier({A}, _, A).

/**
 * \+ A:
 * The negation of A is checked. The output of A is left loose.
 */
chart_barrier(U, I, Q) :-
   U = (\+A),
   sys_replace_site(Q, U, {\+chart(A,I,_)}).

/**********************************************************/
/* Goal Rewriting Non-Steadfast                           */
/**********************************************************/

:- private sys_chart/3.
:- meta_predicate sys_chart(2,?,?).
sys_chart(_, _, _) :-
   throw(error(existence_error(body,sys_chart/3),_)).

user:goal_expansion(sys_chart(P, I, O), chart(P, I, O)) :-
   sys_var(P).
user:goal_expansion(sys_chart(P, I, O), R) :-
   sys_chart_expansion(P, I, O, R).
user:goal_expansion(sys_chart(P, I, O), Q) :-
   sys_modext_args(P, I, O, Q).

/**
 * sys_chart_expansion(A, I, O, G)
 * Succeeds when the phrase A extended by the input I and the
 * output O results in the not-necessarily steadfast goal G.
 */
% sys_chart_expansion(+Grammar, +List, -List, -Goal)
:- private sys_chart_expansion/4.
:- meta_predicate sys_chart_expansion(2,?,?,0).
:- set_predicate_property(sys_chart_expansion/4, sys_noexpand).
sys_chart_expansion(P, _, _, P) :-
   P = fail.
sys_chart_expansion((  A, B), I, O, (  sys_chart(A, I, H),
                                       sys_chart(B, H, O))) :-
   sys_var(A).
sys_chart_expansion((  U, B), I, O, (  P,
                                       chart(B, I, O))) :-
   chart_barrier(U, I, P).
sys_chart_expansion((  A, B), I, O, (  sys_chart(A, I, H),
                                       sys_chart(B, H, O))).
sys_chart_expansion((  A; B), I, O, (  chart(A, I, O)
                                    ;  chart(B, I, O))).
sys_chart_expansion(U, I, I, Q) :-
   U = [],
   sys_replace_site(Q, U, true).
sys_chart_expansion(U, I, O, (  Q,
                                sys_chart(B, H, O))) :-
   U = [A|B],
   sys_replace_site(Q, U, 'D'(A,I,H)).
sys_chart_expansion(U, I, O, (  P, Q)) :-
   chart_barrier(U, I, P),
   sys_replace_site(Q, U, I=O).

/**********************************************************/
/* Chart DCG Rule                                         */
/**********************************************************/

% user:term_expansion(+Term, -Term)
:- public user:term_expansion/2.
:- multifile user:term_expansion/2.
:- meta_predicate user:term_expansion(-1,-1).
:- discontiguous user:term_expansion/2.

/**
 * H ==: B:
 * Chart DCG rule with chart head H and chart body B.
 */
:- public ==: /2.
:- meta_predicate (-3==: -3).
(_ ==: _) :-
   throw(error(existence_error(body,==: /2),_)).

user:term_expansion((P ==: A), (chart_post(P, I, O) <=
                                  chart_posted(A, I, O))).

/**
 * chart_post(H, I, O):
 * This predicate cannot be executed. It only serves as a goal
 * expansion wrapper, that generates the post head of a chart rule.
 */
:- private chart_post/3.
:- meta_predicate chart_post(2,?,?).
chart_post(_, _, _) :-
   throw(error(existence_error(body,chart_post/3),_)).

user:goal_expansion(chart_post(P, I, O), H) :-
   sys_modext_args(P, I, O, Q),
   sys_replace_site(H, Q, post(Q)).

/**
 * chart_posted(B, I, O):
 * This predicate cannot be executed. It only serves as a goal
 * expansion wrapper, that generates the posted body of a chart rule.
 */
:- private chart_posted/3.
:- meta_predicate chart_posted(2,?,?).
chart_posted(_, _, _) :-
   throw(error(existence_error(body,chart_posted/3),_)).

user:goal_expansion(chart_posted(P, _, _), _) :-
   sys_var(P),
   throw(error(instantiation_error,_)).
user:goal_expansion(chart_posted((  A, B), I, O), (  chart_posted(A, I, H),
                                                     sys_chart(B, H, O))).
user:goal_expansion(chart_posted((  A; B), I, O), (  chart_posted(A, I, O)
                                                  ;  chart_posted(B, I, O))).
user:goal_expansion(chart_posted(U, I, O), (  posted(Q),
                                              sys_chart(B, H, O))) :-
   U = [A|B],
   sys_replace_site(Q, U, 'D'(A,I,H)).
user:goal_expansion(chart_posted(P, I, O), posted(Q)) :-
   sys_modext_args(P, I, O, Q).

