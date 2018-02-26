/**
 * This module provides directives for notebooks. Notebooks are
 * Prolog modules that have a secondary purpose than only being
 * consulted. The secondary purpose is that they are to contain
 * query which are automatically displayed to the end-user and which
 * can be modified by the end-user.
 *
 * Without specifying how the displaying or the modification is
 * performed we provide directives to embed queries inside Prolog
 * modules. The idea is that normal ISO comments are the text cells
 * of a notebook, that normal ISO clauses/directives are the program
 * cells of a notbook and that a new directive marks queries.
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

:- package(library(jekdev/reference/notebook)).

:- module(directives, []).
:- use_module(library(stream/console)).
:- use_module(library(runtime/session)).

:- public prefix(?-).
:- op(1200, fx, ?-).

/**
 * notebook:
 * The predicate succeeds in setting this module to notebook.
 */
:- public notebook/0.
:- static notebook/0.
:- set_predicate_property(notebook/0, sys_nostack).
notebook :-
   sys_parent_goal(G),
   sys_get_context(G, C),
   set_source_property(C, sys_source_annotation((makedot|filler))).

/**
 * ?- G:
 * The directive succeeds always. Whenever the goal G succeeds,
 * the directive will show the solution. When the goal G fails,
 * the directive will show an internationalized no.
 */
:- public (?-)/1.
:- meta_predicate (?- -1).
(?- _) :-
   throw(error(existence_error(body,(?-)/1),_)).

:- private sys_show_all/1.
:- meta_predicate sys_show_all(0).
sys_show_all(G) :-
   sys_get_answer(G, A),
   sys_show_answer(A), fail.
sys_show_all(_).

:- private sys_show_answer/1.
sys_show_answer(the(R)) :-
   sys_show_name_or_eq_list(R),
   ttywrite(' ;'), ttynl.
sys_show_answer(last(R)) :-
   sys_show_name_or_eq_list(R), ttynl.
sys_show_answer(no) :- sys_show_no, ttynl.
sys_show_answer(ball(E)) :-
   print_stack_trace(E).

:- private sys_get_answer/2.
:- meta_predicate sys_get_answer(0,?).
sys_get_answer(G, A) :-
   sys_trap(sys_get_answer2(G, A),
      E,
      (  E == error(system_error(user_close),_)
      -> sys_raise(E)
      ;  A = ball(E))).

:- private sys_get_answer2/2.
:- meta_predicate sys_get_answer2(0,?).
sys_get_answer2(G, A) :-
   current_prolog_flag(sys_choices, X),
   call(G),
   current_prolog_flag(sys_choices, Y),
   sys_get_name_or_eq_list(R),
   (  X == Y -> !,
      A = last(R)
   ;  A = the(R)).
sys_get_answer2(_, A) :-
   A = no.

:- public '.'/1.
:- meta_predicate '.'(0).
'.'(_) :-
   throw(error(existence_error(body,'.'/1),_)).

:- public user:term_expansion/2.
:- multifile user:term_expansion/2.
:- meta_predicate user:term_expansion(-1,-1).
user:term_expansion(V, _) :-
   var(V), !, fail.
user:term_expansion(S, T) :-
   S = '.'(T), !,
   sys_get_variable_names(N),
   ttywrite_term(S, [quoted(true),context(-1),
                       variable_names(N),annotation((makedot|filler))]), ttyflush_output.
user:term_expansion((?- G), unit) :-
   sys_show_all(G).
