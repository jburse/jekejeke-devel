/**
 * This module provides directives for notebooks. Notebookds are
 * Prolog modules that have a secondary purpose than only being
 * consulted. The secondary purpose is that they are to contain
 * query which are automatically evaluated by the system and which
 * can be modified by the end-user.
 *
 * Without specifying how the evaluation or the modification is
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
 * ?- G:
 * The predicate succeeds always. Whenever the goal G succeeds,
 * the predicate will show the solution. When the goal G fails,
 * the predicate will show an internationalized no.
 */
:- public user:term_expansion/2.
:- multifile user:term_expansion/2.
:- meta_predicate user:term_expansion(-1,-1).
user:term_expansion((?- G), (:- sys_show_all(G))).

:- private sys_show_all/1.
:- meta_predicate sys_show_all(0).
sys_show_all(G) :-
   current_prolog_flag(sys_choices, X),
   call(G),
   current_prolog_flag(sys_choices, Y), sys_show_vars,
   (  X == Y -> !, ttynl
   ;  ttywrite(' ;'), ttynl, fail).
sys_show_all(_) :- sys_show_no, ttynl.
