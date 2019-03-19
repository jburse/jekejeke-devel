/**
 * This module allows executing test cases and analysing the
 * coverage of the tested code. The test cases are the same
 * as for the module runner. But contrary to the module runner
 * test results are not collected by this module. Instead this
 * module installs a debugger hook and collects a coverage map.
 *
 * The coverage map is stored by the following facts:
 *
 *      :- public cover_summary/1.
 *      :- dynamic cover_summary/1.
 *      % cover_summary(OkNok)
 *
 *      :- public cover_source/2.
 *      :- dynamic cover_source/2.
 *      % cover_source(Source, OkNok)
 *
 *      :- public cover_predicate/4.
 *      :- dynamic cover_predicate/4.
 *      % cover_predicate(Fun, Arity, Source, OkNok)
 *
 *      :- public cover/5.
 *      :- dynamic cover/5.
 *      % cover(Fun, Arity, Source, Line, OkNok)
 *
 * The debugger hook slows down the execution of test cases by a
 * factor of 3-4. The collection is done in two phases. First the
 * predicate tracker_batch/0 has to be called. Then the predicate
 * analyze_batch/0 has to be called. The later predicate needs
 * text/1 facts that designate the sources that should appear in
 * the coverage map.
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

:- package(library(jekdev/reference/testing)).

:- module(tracker, []).
:- use_module(library(inspection/frame)).
:- use_module(library(inspection/provable)).
:- use_module(library(advanced/arith)).
:- use_module(library(inspection/base)).
:- use_module(library(inspection/notation)).
:- use_module(library(system/file)).
:- use_module(runner).
:- use_module(helper).

:- public text/1.
:- multifile text/1.
:- static text/1.

/****************************************************************/
/* Hit Update                                                   */
/****************************************************************/

% cover_hit(File, Line)
:- private cover_hit/2.
:- dynamic cover_hit/2.

% reset_cover_hit
:- private reset_cover_hit/0.
reset_cover_hit :-
   retract(cover_hit(_, _)), fail.
reset_cover_hit.

% update_cover_hit(+File, +Line)
:- private update_cover_hit/2.
update_cover_hit(O, L) :-
   cover_hit(O, L), !.
update_cover_hit(O, L) :-
   assertz(cover_hit(O, L)).

/****************************************************************/
/* Trace Hook                                                   */
/****************************************************************/

% is_entry(+Port)
:- private is_entry/1.
is_entry(head).
is_entry(exit).

% goal_tracing(+Port, +Frame)
:- multifile user:goal_tracing/2.
:- public user:goal_tracing/2.
user:goal_tracing(P, Q) :-
   is_entry(P),
   frame_property(Q, sys_call_goal(G)),
   callable_property(G, source_file(O)),
   callable_property(G, line_no(L)), !,
   update_cover_hit(O, L).
user:goal_tracing(_, _).

/****************************************************************/
/* Run Bodies                                                   */
/****************************************************************/

% sys_cover_body(+Body)
:- private sys_cover_body/1.
sys_cover_body(Body) :-
   catch(Body, _, fail), !.
sys_cover_body(_).

% sys_cover_body_debug(+Body)
:- private sys_cover_body_debug/1.
sys_cover_body_debug(Body) :-
   catch(Body, _, fail), !,
   write(' success'), nl.
sys_cover_body_debug(_) :-
   write(' fail'), nl.

/**
 * tracker_batch:
 * Run the test cases and collect the raw coverage map.
 */
% tracker_batch
:- public tracker_batch/0.
tracker_batch :- reset_texts, reset_cover_hit,
   visible([head,exit]), trace,
   rule_ref(case(_, _, _, _), Body, _),
   sys_cover_body(Body), fail.
tracker_batch :- nodebug,
   visible([call,exit,redo,fail]), set_texts.

/**
 * tracker_batch_debug:
 * Run the test cases and collect the raw coverage map.
 */
% tracker_batch_debug
:- public tracker_batch_debug/0.
tracker_batch_debug :- reset_texts, reset_cover_hit,
   visible([head,exit]), trace,
   rule_ref(case(_, _, _, Case), Body, _),
   write('Case='),
   write(Case-Body),
   sys_cover_body_debug(Body), fail.
tracker_batch_debug :- nodebug,
   visible([call,exit,redo,fail]), set_texts.

% reset_texts
:- private reset_texts/0.
reset_texts :-
   text(X),
   absolute_file_name(X, Y),
   reset_source_property(Y, sys_notrace), fail.
reset_texts.

% set_texts
:- private set_texts/0.
set_texts :-
   text(X),
   absolute_file_name(X, Y),
   set_source_property(Y, sys_notrace), fail.
set_texts.

/****************************************************************/
/* Summary Update                                               */
/****************************************************************/

% cover_summary(OkNok)
:- public cover_summary/1.
:- dynamic cover_summary/1.

% sys_remove_summary
:- private sys_remove_summary/0.
sys_remove_summary :-
   retract(cover_summary(_)), fail.
sys_remove_summary.

% sys_update_summary(+OkNok)
:- private sys_update_summary/1.
sys_update_summary(L) :-
   retract(cover_summary(R)), !,
   sys_add_oknok(L, R, S),
   assertz(cover_summary(S)).
sys_update_summary(L) :-
   assertz(cover_summary(L)).

/****************************************************************/
/* Source Update                                                */
/****************************************************************/

% cover_source(Source, OkNok)
:- public cover_source/2.
:- dynamic cover_source/2.

% sys_remove_source
:- private sys_remove_source/0.
sys_remove_source :-
   retract(cover_source(_, _)), fail.
sys_remove_source.

% sys_update_source(+Source, +OkNok)
:- private sys_update_source/2.
sys_update_source(Source, L) :-
   retract(cover_source(Source, R)), !,
   sys_add_oknok(L, R, S),
   assertz(cover_source(Source, S)).
sys_update_source(Source, L) :-
   assertz(cover_source(Source, L)).

/****************************************************************/
/* Predicate Update                                             */
/****************************************************************/

% cover_predicate(Fun, Arity, File, OkNok)
:- public cover_predicate/4.
:- dynamic cover_predicate/4.

% sys_remove_predicate
:- private sys_remove_predicate/0.
sys_remove_predicate :-
   retract(cover_predicate(_, _, _, _)), fail.
sys_remove_predicate.

% sys_update_predicate(+Atom, +Integer, +Atom, +OkNok)
:- private sys_update_predicate/4.
sys_update_predicate(Fun, Arity, File, L) :-
   retract(cover_predicate(Fun, Arity, File, R)), !,
   sys_add_oknok(L, R, S),
   assertz(cover_predicate(Fun, Arity, File, S)).
sys_update_predicate(Fun, Arity, File, L) :-
   assertz(cover_predicate(Fun, Arity, File, L)).

/****************************************************************/
/* Clause Update                                                */
/****************************************************************/

% cover(Fun, Arity, File, Line, OkNok)
:- public cover/5.
:- dynamic cover/5.

% sys_remove_cover
:- private sys_remove_cover/0.
sys_remove_cover :-
   retract(cover(_, _, _, _, _)), fail.
sys_remove_cover.

% sys_update_cover(+Atom, +Integer, +Atom, +Integer, +OkNok)
:- private sys_update_cover/5.
sys_update_cover(Fun, Arity, File, Line, L) :-
   retract(cover(Fun, Arity, File, Line, R)), !,
   sys_add_oknok(L, R, S),
   assertz(cover(Fun, Arity, File, Line, S)).
sys_update_cover(Fun, Arity, File, Line, L) :-
   assertz(cover(Fun, Arity, File, Line, L)).

/****************************************************************/
/* Analyze Text                                                 */
/****************************************************************/

% sys_find_hit(+Atom, +Integer, +Integer, -OkNok)
:- private sys_find_hit/4.
sys_find_hit(OrigSrcPin, A, B, 1-0) :-
   C is B-1,
   between(A, C, L),
   cover_hit(OrigSrcPin, L), !.
%   cover_hit(OrigSrcPin, L),
%   A =< L, L < B, !.
sys_find_hit(_, _, _, 0-1).

% sys_find_indicator(+Atom, +Atom, +Integer, +Integer, -Atom, -Integer)
:- private sys_find_indicator/6.
sys_find_indicator(SrcPin, OrigSrcPin, A, B, Fun, Arity) :-
   C is B-1,
   between(A, C, L),
   sys_location(SrcPin, OrigSrcPin, L, Indicator),
   sys_indicator_colon(Fun/Arity, Indicator).
%   source_property(SrcPin, sys_location(Indicator, OrigSrcPin, L)),
%   A =< L, L < B,
%   short_indicator(Indicator, OrigSrcPin, ShortIndicator),
%   sys_make_indicator(Fun, Arity, ShortIndicator).

% sys_analyze_text(+File)
:- private sys_analyze_text/1.
sys_analyze_text(InName) :-
   absolute_file_name(InName, SrcPin),
   setup_call_cleanup(
      open(InName, read, InStream),
      sys_analyze_text(InStream, SrcPin, SrcPin),
      close(InStream)).

% sys_analyze_text(+Stream, +Atom, +Atom)
:- private sys_analyze_text/3.
sys_analyze_text(InStream, SrcPin, OrigSrcPin) :- repeat,
   read_term(InStream, Term, [source(SrcPin),line_no(Line1)]),
   (  Term == end_of_file -> !
   ;  (  at_end_of_stream(InStream)
      -> stream_property(InStream, line_no(L)),
         Line2 is L+1
      ;  stream_property(InStream, line_no(Line2))),
      sys_find_hit(OrigSrcPin, Line1, Line2, OkNok),
      sys_find_indicator(SrcPin, OrigSrcPin, Line1, Line2, Fun, Arity),
      sys_update_cover(Fun, Arity, OrigSrcPin, Line1, OkNok),
      sys_update_predicate(Fun, Arity, OrigSrcPin, OkNok),
      sys_update_source(OrigSrcPin, OkNok),
      sys_update_summary(OkNok),
      (  Term = (:-begin_module(Module))
      -> absolute_file_name(verbatim(Module), LocalSrcPin),
         sys_analyze_text(InStream, LocalSrcPin, OrigSrcPin), fail
      ;  Term = (:-end_module) -> !; fail)).

/**
 * analyze_batch:
 * Relate the raw coverage map with the sources given as text/1 facts.
 */
% analyze_batch
:- public analyze_batch/0.
analyze_batch :- sys_remove_cover, sys_remove_predicate, sys_remove_source, sys_remove_summary,
   text(X),
   sys_analyze_text(X), fail.
analyze_batch.

/*************************************************************/
/* List Sources                                              */
/*************************************************************/

% list_cover_source
:- public list_cover_source/0.
list_cover_source :-
   write('Ok\tNok\tSource'), nl, list_cover_source_data,
   cover_summary(Ok-Nok),
   write(Ok),
   write('\t'),
   write(Nok),
   write('\tTotal'), nl.

% list_cover_source_data
:- private list_cover_source_data/0.
list_cover_source_data :-
   cover_source_view(_, Directory, Name, Ok-Nok),
   write(Ok),
   write('\t'),
   write(Nok),
   write('\t'),
   write(Directory),
   write(/),
   write(Name), nl, fail.
list_cover_source_data.

/***************************************************************/
/* Data View                                                  */
/***************************************************************/

% cover_source_view(-Atom, -Atom, -Atom, -Pair))
cover_source_view(Source, Directory, Name, OkNok) :-
   cover_source(Source, OkNok),
   make_path(D1, N1, Source),
   make_path(_, Directory, D1),
   make_name(Name, _, N1).


