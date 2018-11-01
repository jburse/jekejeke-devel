/**
 * This module provides a couple of primitives for hypothetical
 * reasoning. The primitives come in two flavours. The first
 * flavour is the continuation variant where the effect of the
 * primitive persists for the continuation. The second flavour
 * is the temporary variant where the effect of the primitive
 * only persist for the duration of a given goal.
 *
 * <verb>(<arguments>)               % Continuation Variant
 * <verb>(<arguments>, <goal>)       % Temporary Variant
 *
 * Since the temporary variant uses an additional goal argument,
 * given the continuation variant the temporary variant can be easily
 * invoked with the help of call/2. The clause primitives are
 * implemented with the help of clause references and the module
 * assume. The other primitives extend some logical meta-predicates to
 * the temporary variant.
 *
 * Examples:
 * ?- assumez(p), p.
 * Yes
 * ?- assumez(p), retire(p), p.
 * No
 *
 * The continuation variants of true/1, fail/1, (',')/3 and (;)/3 are
 * already defined in the runtime library. By default the assume
 * predicate will create a thread local predicate, if the predicate of
 * the head of the given clause was not yet defined. Further the retire
 * predicate will silently fail if the predicate of the head of the given
 * clause was not yet defined.
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

:- module(hypo, []).
:- use_module(library(minimal/assume)).
:- use_module(library(experiment/ref)).
:- use_module(library(experiment/simp)).

/********************************************************/
/* Continuation Variant                                 */
/********************************************************/

/**
 * true(G):
 * The construct does nothing before further solving.
 */
% already defined in control.p
% :- public true/0.
% true.

% true(+Goal)
:- public true/1.
:- meta_predicate true(0).
true(G) :- G.

/**
 * fail(G):
 * The construct prevents further solving.
 */
% already defined in control.p
% :- public fail/0.
% :- static fail/0.

% fail(+Goal)
:- public fail/1.
:- meta_predicate fail(0).
:- static fail/1.

/**
 * assumea(C):
 * assumea(C, G):
 * The construct assumes the clause C at the top before further solving.
 */
% assumea(+Term)
:- public assumea/1.
:- meta_predicate assumea(-1).
assumea(C) :-
   assumable_ref(C, R),
   deposita_ref(R).

% assumea(+Term, +Goal)
:- public assumea/2.
:- meta_predicate assumea(-1,0).
assumea(C, G) :-
   assumable_ref(C, R),
   deposita_ref(R), G,
   withdrawa_ref(R).

/**
 * assumez(C):
 * assumez(C, G):
 * The construct assumes the clause C at the bottom before further solving.
 */
% assumez(+Term)
:- public assumez/1.
:- meta_predicate assumez(-1).
assumez(C) :-
   assumable_ref(C, R),
   depositz_ref(R).

% assumez(+Term, +Goal)
:- public assumez/2.
:- meta_predicate assumez(-1,0).
assumez(C, G) :-
   assumable_ref(C, R),
   depositz_ref(R), G,
   withdrawz_ref(R).

/**
 * retire(C):
 * retire(C, G):
 * The construct retires the clause P before further solving.
 * Need not preserve the input order.
 */
% retire(-Term)
:- public retire/1.
:- meta_predicate retire(-1).
retire(C) :-
   clause_ref(C, R),
   retire2(R).

% retire2(+Ref)
:- private retire2/1.
retire2(R) :-
   withdrawz_ref(R), !.
retire2(_).

% retire(-Term, +Goal)
:- public retire/2.
:- meta_predicate retire(-1,0).
retire(C, G) :-
   clause_ref(C, R),
   retire2(R, G).

% retire2(+Ref, +Goal)
:- private retire2/2.
:- meta_predicate retire2(?,0).
retire2(R, G) :-
   withdrawz_ref(R), !, G,
   depositz_ref(R).
retire2(_, G) :- G.

/**
 * retireall(H):
 * retireall(H, G):
 * The construct retires all the clauses with head H before further solving.
 * Need not preserve the input order.
 */
% retireall(+Term)
:- public retireall/1.
:- meta_predicate retireall(-1).
retireall(H) :-
   findall(R, clause_ref(H, _, R), L),
   withdrawz_ref(L).

% retireall(+Term, +Goal)
:- public retireall/2.
:- meta_predicate retireall(-1,0).
retireall(H, G) :-
   findall(R, clause_ref(H, _, R), L),
   withdrawz_ref(L), G,
   depositz_ref(L).

/**
 * ','(A, B, G):
 * The construct does P and then Q before further solving.
 */
% already defined in logic.p
% :- public (',')/2.
% :- meta_predicate [(0,0)].
% A, B :- A, B.

% ','(+Goal, +Goal, +Goal)
:- public ','/3.
:- meta_predicate ','(0,0,0).
','(A, B, G) :-
   call(A, call(B,G)).

/**
 * ;(A, B, G):
 * The construct does A before further solving or
 * it does B before further solving.
 */
% already defined in logic.p
% :- public (;)/2.
% :- meta_predicate (0;0).
% A; _ :- A.
% _; B :- B.

% ;(+Goal, +Goal, +Goal)
:- public ;/3.
:- meta_predicate ;(0,0,0).
;(A, _, G) :-
   call(A, G).
;(_, B, G) :-
   call(B, G).
