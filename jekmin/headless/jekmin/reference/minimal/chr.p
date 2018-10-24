/**
 * This module provides constraint handling rules rewriting to
 * forward chaining rules.
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

:- module(chr, []).
:- reexport(library(minimal/delta)).

:- public infix(==>).
:- op(1180, xfx, ==>).

:- public infix(<=>).
:- op(1180, xfx, <=>).

:- public infix(\).
:- op(1100, xfx, \).

% user:goal_expansion(+Goal, -Goal)
:- public user:goal_expansion/2.
:- multifile user:goal_expansion/2.
:- meta_predicate user:goal_expansion(0,0).
:- discontiguous user:goal_expansion/2.

:- private body_post/1.
:- meta_predicate body_post(0).
body_post(_) :-
   throw(error(existence_error(body,body_post/1),_)).

user:goal_expansion(body_post(P), _) :-
   sys_var(P), !, fail.
user:goal_expansion(body_post((  A, B)), (  body_post(A),
                                            body_post(B))).
user:goal_expansion(body_post(true), true).
user:goal_expansion(body_post(fail), fail).
user:goal_expansion(body_post(P), Q) :-
   sys_replace_site(Q, P, post(P)).

:- private head_posted/1.
:- meta_predicate head_posted(0).
head_posted(_) :-
   throw(error(existence_error(body,head_posted/1),_)).

user:goal_expansion(head_posted(P), _) :-
   sys_var(P), !, fail.
user:goal_expansion(head_posted((  A, B)), (  head_posted(A),
                                              head_posted(B))).
user:goal_expansion(head_posted(P), Q) :-
   sys_replace_site(Q, P, posted(P)).

% user:term_expansion(+Term, -Term)
:- public user:term_expansion/2.
:- multifile user:term_expansion/2.
:- meta_predicate user:term_expansion(-1,-1).
:- discontiguous user:term_expansion/2.

/**
 * H ==> B:
 * Propagation rules.
 */
:- public ==> /2.
:- meta_predicate (-1==> -1).
_ ==> _ :-
   throw(error(existence_error(body,==> /2),_)).

user:term_expansion((  H
                    ==>G | B), (  body_post(B)
                               <= head_posted(H), G)).
user:term_expansion((  H ==> B), (  body_post(B)
                                 <= head_posted(H))).

:- private head_phaseout_posted/1.
:- meta_predicate head_phaseout_posted(0).
head_phaseout_posted(_) :-
   throw(error(existence_error(body,head_phaseout_posted/1),_)).

user:goal_expansion(head_phaseout_posted(P), _) :-
   sys_var(P), !, fail.
user:goal_expansion(head_phaseout_posted((  A, B)), (  head_phaseout_posted(A),
                                                       head_phaseout_posted(B))).
user:goal_expansion(head_phaseout_posted(P), Q) :-
   sys_replace_site(Q, P, phaseout_posted(P)).

:- private head_phaseout/1.
:- meta_predicate head_phaseout(0).
head_phaseout(_) :-
   throw(error(existence_error(body,head_phaseout/1),_)).

user:goal_expansion(head_phaseout(P), _) :-
   sys_var(P), !, fail.
user:goal_expansion(head_phaseout((  A, B)), (  head_phaseout(A),
                                                head_phaseout(B))).
user:goal_expansion(head_phaseout(P), Q) :-
   sys_replace_site(Q, P, phaseout(P)).

/**
 * H <=> B:
 * Simplification and simpagation rules.
 */
:- public <=> /2.
:- meta_predicate (-1<=> -1).
_ <=> _ :-
   throw(error(existence_error(body,<=> /2),_)).

user:term_expansion((  H \ J
                    <=>G | B), (  body_post(B)
                               <= head_posted(H),
                                  head_phaseout_posted(J), G)).
user:term_expansion((  H \ J <=> B), (  body_post(B)
                                     <= head_posted(H),
                                        head_phaseout_posted(J))).
user:term_expansion((  H
                    <=>G | B), (  body_post(B)
                               <= head_phaseout_posted(H), G)).
user:term_expansion((  H <=> B), (  body_post(B)
                                 <= head_phaseout_posted(H))).
