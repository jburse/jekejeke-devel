/**
 * This Jekejeke Minlog module provides a couple of connectives for
 * hypothetical reasoning. Hypothetical reasoning provides a new logical
 * operator (=>)/2 for embedded implication. This logical operator takes a
 * construct of the left side which is assumed and goal on the right side
 * which is executed. The predicate (<=)/1 is a variant of the predicate
 * (=>)/2 where the continuation is invoked after assuming the construct.
 * The general forms are:
 *
 * C => G     % Embedded Implication.
 * <= C       % Continuation Variant.
 *
 * The way the interpreter proceeds when assuming the construct C depends on
 * the used constructs. We currently support the backward chaining escape
 * {}/1, the retract (-)/1, the conjunction (/\)/2, the verum unit/0 and the
 * falsum zero/0. If none of these constructs is encountered the construct
 * can be either a fact or a rule, which will be assumed. New head predicates
 * will be assumed thread locale. Based on the scope and for performance reasons
 * inter-clausal variables in facts or rules are currently not supported:
 *
 * Examples:
 * ?- p => p.
 * Yes
 * ?- p => ((q :- p) => q).
 * Yes
 *
 * The predicates (=>)/2 and (<=)/1 can be extended by the end-user by
 * adding new clauses to these predicates. The end-user needs to extend
 * the predicate hypo_abnormal/1 as well, by patterns for the newly supported
 * constructs. The construct (-)/1 can be extended by the end-user by adding
 * new clauses to the predicates (=>)/2 and (<=)/1. The end-user needs to
 * extend the predicate minus_abnormal/1 as well, by patterns for the
 * newly supported retract.
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

% :- current_prolog_flag(sys_context, X), reset_source_property(X, sys_notrace).

:- package(library(jekmin/reference/minimal)).

:- module(hypo, []).
:- use_module(library(minimal/assume)).
:- use_module(library(experiment/ref)).
:- use_module(library(experiment/trail)).

:- public infix(=>).
:- op(700, xfx, =>).

:- public prefix(<=).
:- op(700, fx, <=).

/********************************************************/
/* Embedded Implication                                 */
/********************************************************/

/**
 * {A}:
 * The goal A is checked via backward chaining.
 */
:- public => /2.
:- multifile => /2.
:- discontiguous => /2.
:- meta_predicate -1=>0.
:- set_predicate_property(=> /2, sys_notrace).

A => _ :-
   var(A),
   throw(error(instantiation_error,_)).
{A} => B :- !, A, B.

/**
 * -P:
 * The construct retires P.
 */
:- public (-)/1.
:- meta_predicate -0.
- _ :-
   throw(error(existence_error(body,(-)/1),_)).

- A => _ :-
   var(A),
   throw(error(instantiation_error,_)).
- A => B :-
   \+ minus_abnormal(A), !,
   clause_ref(A, true, R),
   sys_retire_ref(R), B,
   sys_assume_ref(R).

/**
 * P /\ Q:
 * The construct assumes P and then Q before solving.
 */
% already defined in expand.p
% :- public (/\)/2.
% :- meta_predicate /\(0,0).
% /\(_,_) :- throw(error(existence_error(body, (/\)/2), _)).
% :- set_predicate_property((/\)/2, sys_rule).

A /\ B => C :- !, A =>
      (B => C).

/**
 * unit:
 * The construct does nothing before further solving.
 */
% already defined in expand.p
% :- public unit/0.
% unit :- throw(error(existence_error(body, unit/0), _)).

unit => A :- !, A.

/**
 * zero:
 * The construct prevents further solving.
 */
:- public zero/0.
zero :-
   throw(error(existence_error(body,zero/0),_)).

zero => _ :- !, fail.

/**
 * C => G:
 * The predicate temporarily assumes the construct C while
 * solving the goal G. The predicate succeeds whenever the
 * construct C and then the goal G succeed. The predicate is
 * multi-file and can be extended by the end-user.
 */
% +Rule => +Goal
A => B :-
   \+ hypo_abnormal(A), !,
   assumable_ref(A, R),
   sys_assume_ref(R), B,
   sys_retire_ref(R).

/**
 * hypo_abnormal(A):
 * The predicate succeeds for those constructs A that are extended
 * in (=>)/2 or (<=)/1. The predicate is multi-file and can be
 * extended by the end-user.
 */
% hypo_abnormal(+Term)
:- public hypo_abnormal/1.
:- multifile hypo_abnormal/1.
hypo_abnormal({_}).
hypo_abnormal(-_).
hypo_abnormal(_/\_).
hypo_abnormal(unit).
hypo_abnormal(zero).

/**
 * minus_abnormal(A):
 * The predicate succeeds for those retracts A that are extended
 * in the (-)/1 construct. The predicate is multi-file and can be
 * extended by the end-user.
 */
% minus_abnormal(+Term)
:- public minus_abnormal/1.
:- multifile minus_abnormal/1.
:- static minus_abnormal/1.

/********************************************************/
/* Continuation Variant                                 */
/********************************************************/

:- public (<=)/1.
:- multifile (<=)/1.
:- meta_predicate <= -1.
:- set_predicate_property((<=)/1, sys_notrace).
<= A :-
   var(A),
   throw(error(instantiation_error,_)).
<= {A} :- !, A.
<= - A :-
   var(A),
   throw(error(instantiation_error,_)).
<= - A :-
   \+ minus_abnormal(A), !,
   clause_ref(A, true, R),
   sys_retire_ref(R).
<= A /\ B :- !,
   <= A,
   <= B.
<= unit :- !.
<= zero :- !, fail.

/**
 * <= C:
 * The predicate temporarily assumes the construct C while
 * solving the continuation. The predicate succeeds whenever
 * the construct C succeeds. The predicate is multi-file and
 * can be extended by the end-user.
 */
% <= +Rule
<= A :-
   \+ hypo_abnormal(A), !,
   assumable_ref(A, R),
   sys_assume_ref(R).
