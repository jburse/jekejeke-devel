/**
 * Definite clause grammars (DCG) are an extension of context free
 * grammars [5]. DCGs allow arbitrary tree structures to be built in
 * the course of parsing and they allow extra conditions dependent on
 * auxiliary computations. A grammar rule can have one of the following
 * two forms. The second form is known as a push-back grammar rule,
 * since it will complete with re-installing R:
 *
 * P    --> Q.   % DCG rule without push back
 * P, R --> Q.   % DCG rule with push back
 *
 * The term expansion augments the head by two additional parameters
 * that are to represent the sentence position before and after the
 * non-terminal that is defined. A grammar head with predicate identifier
 * p/n will be turned into a normal Prolog head with predicate identifier
 * p/n+2. The new predicate identifier can be used in system predicates
 * such as listing/1, spy/1, etc... The outcome of this first expansion
 * is basically:
 *
 * phrase(P, I, O) :- phrase(Q, I, O).
 * phrase(P, I, O) :- phrase(Q, I, H), phrase(R, O, H).
 *
 * The term expansion will then go to work and tackle the head of the
 * new Prolog rule, whereas the goal expansion will tackle the body.
 * The goal expansion will introduce unifications (=)/2 here and then
 * to keep the expansion steadfast. One requirement is that the two
 * queries phrase(G, I, O) and (phrase(G, I, H), H = O) should return
 * the same results. This allows for example for a consistent definition
 * of phrase(G, I) as an expansion to phrase(G, I, []).
 *
 * Example:
 * ?- [user].
 * factor(X) --> "(", expr(X), ")".
 *
 * Yes
 * ?- listing(factor/3).
 * factor(X, [40|A], B) :- expr(X, A, [41|B]).
 *
 * We see in the example that the translation does not make use of the
 * connection predicate ‘C’/3 for terminals. Instead terminals are directly
 * based on the list definition of ‘C’/3 and translated into corresponding
 * list equations. If possible these equations are merged into the head
 * or into the body goals of the grammar rule. This gives better performance
 * but renders the grammar mechanism not anymore customizable via ‘C’/3.
 *
 * The DCG grammar mechanism is extensible. The invocation part can be
 * extended by simultaneously adding custom clauses to the multi-file
 * predicates phrase/3 and sys_phrase_delay/1. The expansion part can be
 * extended by simultaneously adding custom clauses to the multi-file
 * predicates phrase_expansion/4, phrase_abnormal/1 and
 * sys_phrase_expansion/4. The module tecto is an example where this
 * mechanism is used to define additional grammar constructs that are
 * not strictly non-terminals.
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

/**
 * Implementation note: This module defines 3 multifile predicates
 * parse/3, sys_phrase_delay/1, phrase_expansion/4, phrase_abnormal/4
 * and sys_phrase_expansion/4 so that the expansion and execution of dcgs
 * can be extended. This is for exmple used by the module tecto.
 */

:- module(user, []).

/**********************************************************/
/* Goal Execution                                         */
/**********************************************************/

/**
 * phrase(A, I, O):
 * Succeeds when the list I starts with the phrase A giving the
 * remainder O. Can be used for parsing when I is input and for
 * un-parsing when I is output.
 */
% phrase(+Goal, +List, -List)
:- public phrase/3.
:- multifile phrase/3.
:- meta_predicate phrase(2,?,?).
phrase(P, _, _) :-
   sys_var(P),
   throw(error(instantiation_error,_)).
phrase(P, I, O) :-
   \+ sys_phrase_delay(P), !,
   expand_goal(phrase(P, I, O), Q),
   call(Q).

:- public sys_phrase/3.
:- meta_predicate sys_phrase(2,?,?).
sys_phrase(_, _, _) :-
   throw(error(existence_error(body,sys_phrase/3),_)).

/**
 * sys_phrase_delay(A):
 * Succeeds for those phrases A that are extended in
 * phrase/3.
 */
% sys_phrase_delay(+Goal)
:- public sys_phrase_delay/1.
:- multifile sys_phrase_delay/1.
:- static sys_phrase_delay/1.

/**********************************************************/
/* Goal Rewriting Steadfast                               */
/**********************************************************/

% goal_expansion(+Goal, -Goal)
:- discontiguous goal_expansion/2.
:- public goal_expansion/2.
:- multifile goal_expansion/2.
:- meta_predicate goal_expansion(0,0).

/**
 * phrase(A, I):
 * Succeeds when the list I starts with the phrase A giving
 * the empty remainder.
 */
% phrase(+Goal, +List)
:- public phrase/2.
:- meta_predicate phrase(2,?).
phrase(_, _) :-
   throw(error(existence_error(body,phrase/2),_)).

goal_expansion(phrase(P, I), phrase(P, I, [])).

goal_expansion(phrase(P, _, _), _) :-
   sys_var(P), !, fail.

/**
 * P (grammar):
 * The grammar non-terminal P succeeds whenever the callable P extended
 * by the current input and output succeeds.
 */
goal_expansion(phrase(P, I, O), Q) :-
   \+ phrase_abnormal(P), !,
   sys_modext_args(P, I, O, Q).

goal_expansion(phrase(P, I, O), R) :-
   phrase_expansion(P, I, O, R).

/**
 * phrase_expansion(A, I, O, G)
 * Succeeds when the phrase A extended by the input I and the
 * output O results in the steadfast goal G.
 */
% phrase_expansion(+Grammar, +List, -List, -Goal)
:- multifile phrase_expansion/4.
:- public phrase_expansion/4.
:- meta_predicate phrase_expansion(2,?,?,0).
:- discontiguous phrase_expansion/4.
:- set_predicate_property(phrase_expansion/4, sys_noexpand).

/**
 * A, B (grammar):
 * The grammar connective succeeds whenever A and B succeed. The
 * output of A is conjoined with the input of B.
 */
:- public ','/4.
:- meta_predicate ','(2,2,?,?).
','(_, _, _, _) :-
   throw(error(existence_error(body,','/4),_)).

phrase_expansion((  A, B), I, O, (  phrase(A, I, H),
                                    sys_phrase(B, H, O))).

/**
 * A ; B (grammar):
 * The grammar connective succeeds whenever A or B succeeds.
 * The goal arguments A and B are cut transparent.
 * A -> B; C (grammar):
 * The grammar connective succeeds when A succeeds and then
 * whenever B succeeds, or else whenever C succeeds. The goal
 * arguments B and C are cut transparent. The output of A is
 * conjoined with the input of B.
 * A *-> B; C (grammar):
 * The grammar connective succeeds whenever A succeeds and then
 * whenever B succeeds, or else whenever C succeeds. The goal
 * arguments B and C are cut transparent. The output of A is
 * conjoined with the input of B.
 */
:- public ;/4.
:- meta_predicate ;(2,2,?,?).
;(_, _, _, _) :-
   throw(error(existence_error(body,;/4),_)).

phrase_expansion((  A; B), I, O, (  phrase(A, I, O)
                                 ;  phrase(B, I, O))).

/**
 * A -> B (grammar):
 * The grammar connective succeeds when A succeeds and then
 * whenever B succeeds. The goal argument B is cut transparent.
 * The output of A is conjoined with the input of B.
 */
:- public -> /4.
:- meta_predicate ->(2,2,?,?).
->(_, _, _, _) :-
   throw(error(existence_error(body,-> /4),_)).

phrase_expansion((  A -> B), I, O, (  phrase(A, I, H)
                                   -> phrase(B, H, O))).

/**
 * A *-> B (grammar):
 * The grammar connective succeeds whenever A succeeds and then
 * whenever B succeeds. The goal argument B is cut transparent.
 * The output of A is conjoined with the input of B.
 */
:- public *-> /4.
:- meta_predicate *->(2,2,?,?).
*->(_, _, _, _) :-
   throw(error(existence_error(body,*-> /4),_)).

phrase_expansion((  A *-> B), I, O, (  phrase(A, I, H)
                                    *->phrase(B, H, O))).

/**
 * call(A) (grammar):
 * Whenever the goal argument A succeeds then the grammar connective succeeds.
  */
phrase_expansion(call(P), I, O, phrase(P, I, O)).

/**
 * fail (grammar):
 * The grammar connective fails.
 */
:- public fail/2.
fail(_, _) :-
   throw(error(existence_error(body,fail/2),_)).

phrase_expansion(P, _, _, P) :-
   P = fail.

/**
 * {A} (grammar):
 * The grammar connective succeeds whenever the goal argument A succeeds.
 * The goal argument A is cut transparent and not grammar translated.
 */
:- public {}/3.
:- meta_predicate {}(0,?,?).
{}(_, _, _) :-
   throw(error(existence_error(body,{}/3),_)).

phrase_expansion(U, I, O, (  A, Q)) :-
   U = {A},
   sys_replace_site(Q, U, O=I).

/**
 * \+ A (grammar):
 * When the goal argument A succeeds, then the grammar connective fails.
 * Otherwise the grammar connective succeeds. The second argument
 * is left loose.
 */
:- public (\+)/3.
:- meta_predicate \+(2,?,?).
\+(_, _, _) :-
   throw(error(existence_error(body,(\+)/3),_)).

phrase_expansion(U, I, O, (  P, Q)) :-
   U = (\+A),
   sys_replace_site(P, U, \+phrase(A,I,_)),
   sys_replace_site(Q, U, O=I).

/**
 * ! (grammar):
 * The grammar connective removes pending choice and then succeeds once.
 */
:- public !/2.
!(_, _) :-
   throw(error(existence_error(body,!/2),_)).

phrase_expansion(U, I, O, (  U, Q)) :-
   U = !,
   sys_replace_site(Q, U, O=I).

/**
 * [A1, …, An] (grammar):
 * The grammar connective succeeds when the terminals A1, …, An can be consumed.
 */
:- public []/2.
[](_, _) :-
   throw(error(existence_error(body,[]/2),_)).

phrase_expansion(U, I, O, Q) :-
   U = [],
   sys_replace_site(Q, U, O=I).

:- public '.'/4.
:- meta_predicate '.'(2,2,?,?).
'.'(_, _, _, _) :-
   throw(error(existence_error(body,'.'/4),_)).

phrase_expansion(U, H, O, (  Q,
                             sys_phrase(B, I, O))) :-
   U = [A|B],
   sys_replace_site(Q, U, [A|I]=H).

/**
 * phrase_abnormal(A):
 * Succeeds for those phrases A that are extended in
 * phrase_expansion/4 and sys_phrase_expansion/4.
 */
% phrase_abnormal(+Grammar)
:- multifile phrase_abnormal/1.
:- public phrase_abnormal/1.
phrase_abnormal((_,_)).
phrase_abnormal((_;_)).
phrase_abnormal((_->_)).
phrase_abnormal((_*->_)).
phrase_abnormal(call(_)).
phrase_abnormal(fail).
phrase_abnormal({_}).
phrase_abnormal(\+_).
phrase_abnormal(!).
phrase_abnormal([]).
phrase_abnormal([_|_]).

/**********************************************************/
/* Goal Rewriting Non-Steadfast                           */
/**********************************************************/

goal_expansion(sys_phrase(P, I, O), phrase(P, I, O)) :-
   sys_var(P).
goal_expansion(sys_phrase(P, I, O), Q) :-
   \+ phrase_abnormal(P), !,
   sys_modext_args(P, I, O, Q).
goal_expansion(sys_phrase(P, I, O), R) :-
   sys_phrase_expansion(P, I, O, R).

/**
 * sys_phrase_expansion(A, I, O, G)
 * Succeeds when the phrase A extended by the input I and the
 * output O results in the not-necessarily steadfast goal G.
 */
% sys_phrase_expansion(+Grammar, +List, -List, -Goal)
:- multifile sys_phrase_expansion/4.
:- public sys_phrase_expansion/4.
:- meta_predicate sys_phrase_expansion(2,?,?,0).
:- set_predicate_property(sys_phrase_expansion/4, sys_noexpand).

sys_phrase_expansion((  A, B), I, O, (  sys_phrase(A, I, H),
                                        sys_phrase(B, H, O))).
sys_phrase_expansion((  A; B), I, O, (  phrase(A, I, O)
                                     ;  phrase(B, I, O))).
sys_phrase_expansion((  A -> B), I, O, (  sys_phrase(A, I, H)
                                       -> phrase(B, H, O))).
sys_phrase_expansion((  A *-> B), I, O, (  sys_phrase(A, I, H)
                                        *->phrase(B, H, O))).
sys_phrase_expansion(call(P), I, O, sys_phrase(P, I, O)).
sys_phrase_expansion(fail, _, _, fail).
sys_phrase_expansion(U, I, O, (  A, Q)) :-
   U = {A},
   sys_replace_site(Q, U, O=I).
sys_phrase_expansion(U, I, O, (  P, Q)) :-
   U = (\+A),
   sys_replace_site(P, U, \+phrase(A,I,_)),
   sys_replace_site(Q, U, O=I).
sys_phrase_expansion(U, I, O, (  U, Q)) :-
   U = !,
   sys_replace_site(Q, U, O=I).
sys_phrase_expansion([], I, I, true).
sys_phrase_expansion([A|B], [A|I], O, sys_phrase(B, I, O)).

/**********************************************************/
/* Term Rewriting                                         */
/**********************************************************/

% term_expansion(+Term, -Term)
:- public term_expansion/2.
:- multifile term_expansion/2.
:- meta_predicate term_expansion(-1,-1).
:- discontiguous term_expansion/2.

/**
 * P (grammar):
 * The grammar non-terminal P is defined with the callable P extended
 * by the current input and output.
 */
term_expansion(phrase(P, _, _), _) :-
   sys_var(P),
   throw(error(instantiation_error,_)).
term_expansion(phrase(P, I, O), Q) :-
   sys_modext_args(P, I, O, Q).

/**
 * H --> B:
 * The construct defines a grammar rule with grammar head H and
 * grammar body B.
 */
:- public infix(-->).
:- op(1200, xfx, -->).
:- public --> /2.
:- meta_predicate (2--> -3).
(_ --> _) :-
   throw(error(existence_error(body,--> /2),_)).

term_expansion((P --> _), _) :-
   sys_var(P),
   throw(error(instantiation_error,_)).

/**
 * H, P --> B:
 * The construct defines a push back with grammar head H and,
 * push back P and grammar body B.
 */
term_expansion((P, B --> C),
   (phrase(P, I, O) :-
      sys_phrase(C, I, H),
      phrase(B, O, H))).
term_expansion((P --> B),
   (phrase(P, I, O) :-
      sys_phrase(B, I, O))).