/**
 * Theory files have the syntax of a theory text, see section 4.3.
 * When consulted the contained facts and rules are asserted. The
 * encountered head predicates of the facts and rules are declared
 * static by default. Further any directives in the theory text are
 * executed in the order they appear. These directives might
 * additionally declare foreign predicates. Upon consulting again
 * a source, the related declared predicates are first abolished.
 *
 * The consult performs various style checks. The facts and rules are
 * checked for singleton variables. Singleton variables need to be
 * input in the form of anonymous variables (_). The facts and rules
 * for the same head predicate need to form one block. The directive
 * discontiguous/1 allows exempting a predicate from this style check.
 * The special file name user can be used to consult from the standard input.
 *
 * Normally the facts and rules for the same head predicate come from
 * one source only. The directive multifile/1 allows exempting a
 * predicate from this style check. Multi-file predicates behave
 * differently during re-consult. If a declared predicate spans
 * multiple sources only the clauses belonging to the re-consulted
 * source are retracted. The defined predicate will only be abolished
 * when it does not belong to any source anymore.
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

:- sys_get_context(here, C),
   set_source_property(C, use_package(foreign(jekpro/reference/bootload))).
:- sys_get_context(here, C),
   reset_source_property(C, sys_source_visible(public)).

:- op(1150, fy, override).
:- set_oper_property(prefix(override), visible(public)).
:- op(1150, fy, multifile).
:- set_oper_property(prefix(multifile), visible(public)).
:- op(1150, fy, discontiguous).
:- set_oper_property(prefix(discontiguous), visible(public)).

/***************************************************************/
/* Consult Predicates                                          */
/***************************************************************/

/**
 * ensure_loaded(R): [ISO 7.4.2.8]
 * The predicate ensures that the relative source path R is loaded. If the
 * current time is after the expiration of the source then it will connect
 * to the source. If the source was not modified since its last modified
 * then it will consult the source.
 */
% ensure_loaded(Path)
ensure_loaded(Path) :-
   var(Path),
   throw(error(instantiation_error,_)).
ensure_loaded(X) :-
   sys_eq(X, user), !,
   sys_import_file(X, []).
ensure_loaded(Path) :-
   absolute_file_name(Path, Pin),
   sys_load_file(Pin, [condition(on),sys_link(use_module)]).
:- set_predicate_property(ensure_loaded/1, visible(public)).

/**
 * consult(R):
 * First retract the old facts and rules of the relative source path R.
 * Then assert the new facts and rules from the relative source path R.
 * During assert also process the directives from the relative source
 * path R. Before assert the scope is temporarily changed to the relative
 * source path R.
 */
% consult(Path)
consult(Path) :-
   var(Path),
   throw(error(instantiation_error,_)).
consult(X) :-
   sys_eq(X, user), !,
   sys_import_file(X, []).
consult(Path) :-
   absolute_file_name(Path, Pin),
   sys_load_file(Pin, [sys_link(use_module)]).
:- set_predicate_property(consult/1, visible(public)).

/**
 * unload_file(R):
 * Detach the source identified by the relative source path R.
 */
% unload_file(Path)
unload_file(Path) :-
   absolute_file_name(Path, Pin),
   sys_detach_file(Pin, [sys_link(reexport)]).
:- set_predicate_property(unload_file/1, visible(public)).

/**
 * [S1, ..., Sm]:
 * The predicate processes the path specifications S1, ..., Sm. The
 * following path specifications are recognized. For a list of the
 * recognized path specifications see the API documentation.
 */
% [Path|Goal]
[Path|_] :-
   var(Path),
   throw(error(instantiation_error,_)).
[+Path|Y] :- !,
   consult(Path),
   call(Y).
[-Path|Y] :- !,
   unload_file(Path),
   call(Y).
[Path|Y] :-
   ensure_loaded(Path),
   call(Y).
:- set_predicate_property('.'/2, sys_notrace).
:- set_predicate_property('.'/2, visible(public)).

[].
:- set_predicate_property([]/0, sys_notrace).
:- set_predicate_property([]/0, visible(public)).

/**
 * make:
 * The predicate ensures that all used sources are loaded.
 */
% make
make :-
   sys_load_file(user, [condition(on)]).
:- set_predicate_property(make/0, visible(public)).

/**
 * rebuild:
 * The predicate consults all used sources.
 */
% rebuild
rebuild :-
   sys_load_file(user, []).
:- set_predicate_property(rebuild/0, visible(public)).

/**
 * include(R): [ISO 7.4.2.7]
 * Doesn’t retract the old facts and rules of the relative source
 * path R. Asserts the new facts and rules from the relative source
 * path R. Processes the directives from the relative source path R.
 * Doesn’t change the scope to the relative source path R.
 */
% include(Path)
include(Path) :-
   absolute_file_name(Path, Pin),
   sys_import_file(Pin, []).
:- set_predicate_property(include/1, visible(public)).

/**
 * The predicate consults or ensure loads the relative source
 * path R. The predicate will handle recursive consults, record
 * dependencies and unload unused sources before finishing. The
 * following options are recognized by the predicate. For the
 * list of options see the API documentation.
 */
% sys_load_file(+Pin, +Opts)
:- special(sys_load_file/2, 'SpecialLoad', 0).

/**
 * sys_detach_file(R, O):
 * The predicate removes the user dependency of the relative
 * source path R. The predicate will unload unused sources before
 * finishing. The following options are recognized by the predicate.
 * For the list of options see the API documentation.
 */
% sys_detach_file(+Pin, +Opts)
:- special(sys_detach_file/2, 'SpecialLoad', 1).
:- set_predicate_property(sys_detach_file/2, visible(private)).

/**
 * sys_import_file(R, O):
 * The predicate includes the relative source path R. The following
 * options are recognized by the predicate. For the list of options
 * see the API documentation.
 */
% sys_import_file(+Pin, +Opts)
:- special(sys_import_file/2, 'SpecialLoad', 2).
:- set_predicate_property(sys_import_file/2, visible(private)).

/***************************************************************/
/* Style Checks                                                */
/***************************************************************/

/**
 * discontiguous I, ...: [ISO 7.4.2.3]
 * The predicate sets the predicate indicator I to discontinuous.
 */
% discontiguous(+Indicators)
discontiguous [P|Q] :- !,
   sys_discontiguous(P),
   (discontiguous Q).
discontiguous P,Q :- !,
   sys_discontiguous(P),
   (discontiguous Q).
discontiguous [] :- !.
discontiguous P :-
   sys_discontiguous(P).
:- set_predicate_property((discontiguous)/1, visible(public)).

% sys_discontiguous(+Indicator)
sys_discontiguous(V) :-
   var(V),
   throw(error(instantiation_error,_)).
sys_discontiguous(D) :-
   sys_declaration_indicator(D, I), !,
   sys_discontiguous(I),
   call(D).
sys_discontiguous(I) :-
   sys_neutral_predicate(I),
   sys_make_indicator(J, _, I),
   sys_get_context(J, C),
   set_predicate_property(I, (discontiguous C)).
:- set_predicate_property(sys_discontiguous/1, visible(private)).

/**
 * multifile I, ...:
 * The predicate sets the predicate indicator I to multi-file.
 */
% multifile(+Indicators)
multifile [P|Q] :- !,
   sys_multifile(P),
   (multifile Q).
multifile P,Q :- !,
   sys_multifile(P),
   (multifile Q).
multifile [] :- !.
multifile P :-
   sys_multifile(P).
:- set_predicate_property((multifile)/1, visible(public)).

% sys_multifile(+Indicator)
sys_multifile(V) :-
   var(V),
   throw(error(instantiation_error,_)).
sys_multifile(D) :-
   sys_declaration_indicator(D, I), !,
   sys_multifile(I),
   call(D).
sys_multifile(I) :-
   sys_make_indicator(F, _, I),
   sys_get_context(F, C),
   sys_once(sys_and(predicate_property(I,sys_usage(D)),
               sys_not(sys_eq(C,D)))),
   sys_not(predicate_property(I,sys_multifile(D))),
   throw(error(permission_error(promote,multifile,I),_)).
sys_multifile(I) :-
   sys_neutral_predicate(I),
   sys_make_indicator(J, _, I),
   sys_get_context(J, C),
   set_predicate_property(I, sys_accessible_multifile(C)),
   set_predicate_property(I, multifile).
:- set_predicate_property(sys_multifile/1, visible(private)).

% first defined in special.p
% sys_declaration_indicator(+Declaration, -Indicator).
:- sys_neutral_predicate(sys_declaration_indicator/2).
:- set_predicate_property(sys_declaration_indicator/2, visible(public)).
:- sys_get_context(here, C),
   set_predicate_property(sys_declaration_indicator/2, sys_accessible_public(C)).
:- set_predicate_property(sys_declaration_indicator/2, multifile).
:- sys_get_context(here, C),
   set_predicate_property(sys_declaration_indicator/2, sys_accessible_multifile(C)).
sys_declaration_indicator((discontiguous D), I) :-
   sys_declaration_indicator(D, I).
sys_declaration_indicator((multifile D), I) :-
   sys_declaration_indicator(D, I).

/***************************************************************/
/* Listing Non-Automatic Members                               */
/***************************************************************/

/**
 * listing:
 * The predicate lists the user clauses of the user syntax operators,
 * evaluable functions and predicates. Only non-automatic evaluable
 * functions and predicates are listed.
 */
% listing
listing :-
   listing(_).
:- set_predicate_property(listing/0, visible(public)).
:- set_predicate_property(listing/0, sys_notrace).

/**
 * listing(I):
 * The predicate lists the user clauses of the user syntax operators,
 * evaluable functions and predicates that match the pattern I. Only
 * non-automatic evaluable functions and predicates are listed.
 */
% listing(-Indicator)
listing(I) :-
   ground(I),
   sys_listing_check(I, U),
   sys_show_base(U),
   sys_listing_show(I, U), fail.
listing(I) :-
   sys_not(ground(I)),
   bagof(I, sys_listing_match(I, U), B),
   sys_show_base(U),
   sys_member(I, B),
   sys_listing_show(I, U), fail.
listing(_).
:- set_predicate_property(listing/1, visible(public)).
:- set_predicate_property(listing/1, sys_notrace).

% sys_listing_check(+Indicator, -Source)
sys_listing_check(infix(I), U) :- !,
   sys_syntax_property_chk(infix(I), sys_accessible_usage/1, R),
   sys_member(sys_accessible_usage(U), R).
sys_listing_check(prefix(I), U) :- !,
   sys_syntax_property_chk(prefix(I), sys_accessible_usage/1, R),
   sys_member(sys_accessible_usage(U), R).
sys_listing_check(postfix(I), U) :- !,
   sys_syntax_property_chk(postfix(I), sys_accessible_usage/1, R),
   sys_member(sys_accessible_usage(U), R).
sys_listing_check(I, U) :-
   sys_provable_property_chk(I, automatic/0, []),
   sys_provable_property_chk(I, sys_accessible_usage/1, R),
   sys_member(sys_accessible_usage(U), R).
:- set_predicate_property(sys_listing_check/2, visible(private)).

% sys_listing_show(+Indicator, +Source)
sys_listing_show(infix(I), _) :- !,
   sys_show_syntax(infix(I)).
sys_listing_show(prefix(I), _) :- !,
   sys_show_syntax(prefix(I)).
sys_listing_show(postfix(I), _) :- !,
   sys_show_syntax(postfix(I)).
sys_listing_show(I, U) :-
   sys_show_provable_source(I, U).
:- set_predicate_property(sys_listing_show/2, visible(private)).

% sys_listing_match(-Indicator, -Source)
sys_listing_match(infix(I), U) :-
   sys_current_syntax(L),
   sys_member(infix(I), L),
   sys_syntax_property_chk(infix(I), sys_accessible_usage/1, R),
   sys_member(sys_accessible_usage(U), R).
sys_listing_match(prefix(I), U) :-
   sys_current_syntax(L),
   sys_member(prefix(I), L),
   sys_syntax_property_chk(prefix(I), sys_accessible_usage/1, R),
   sys_member(sys_accessible_usage(U), R).
sys_listing_match(postfix(I), U) :-
   sys_current_syntax(L),
   sys_member(postfix(I), L),
   sys_syntax_property_chk(postfix(I), sys_accessible_usage/1, R),
   sys_member(sys_accessible_usage(U), R).
sys_listing_match(I, U) :-
   sys_current_provable(L),
   sys_member(I, L),
   sys_provable_property_chk(I, automatic/0, []),
   sys_provable_property_chk(I, sys_accessible_usage/1, R),
   sys_member(sys_accessible_usage(U), R).
:- set_predicate_property(sys_listing_match/2, visible(private)).

% also used by automatic.p in debugger
:- special(sys_show_provable_source/2, 'SpecialLoad', 3).
:- set_predicate_property(sys_show_provable_source/2, visible(public)).

% also used by automatic.p in debugger
:- special(sys_show_syntax/1, 'SpecialLoad', 4).
:- set_predicate_property(sys_show_syntax/1, visible(public)).

% also used by automatic.p in debugger
:- special(sys_show_base/1, 'SpecialLoad', 5).
:- set_predicate_property(sys_show_base/1, visible(public)).

% move from provable.p in debugger
:- special(sys_current_provable/1, 'SpecialLoad', 6).
:- set_predicate_property(sys_current_provable/1, visible(public)).

% move from provable.p in debugger
:- special(sys_provable_property_chk/3, 'SpecialLoad', 7).
:- set_predicate_property(sys_provable_property_chk/3, visible(public)).

% move from syntax.p in debugger
:- special(sys_current_syntax/1, 'SpecialLoad', 8).
:- set_predicate_property(sys_current_syntax/1, visible(public)).

% move from syntax.p in debugger
:- special(sys_syntax_property_chk/3, 'SpecialLoad', 9).
:- set_predicate_property(sys_syntax_property_chk/3, visible(public)).

/****************************************************************/
/* Resource Handling                                            */
/****************************************************************/

/**
 * sys_register_file(R):
 * The predicate registers the relative source path R in the
 * knowledge base. The relative source path is automatically
 * unregistered when its call-site relative source is unloaded.
 */
% sys_register_file(+Pin)
:- special(sys_register_file/1, 'SpecialLoad', 10).
