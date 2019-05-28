/**
 * A knowledge base consists of zero, one or more predicates. Each predicate
 * is either a built-in or a defined predicate. Built-ins are implemented via
 * Java methods whereas defined predicates are implemented via associated
 * clauses. A defined predicate can have zero, one or more associated clauses.
 * Each defined predicate is either static, dynamic or thread local. A predicate
 * is identified by a predicate indicator:
 *
 * indicator --> module ":" indicator.
 *             | atom "/" integer
 *
 * module    --> package "/" atom
 *             | "{" array "}"
 *             | reference
 *             | atom.
 *
 * array     --> package "/" atom.
 *             | "{" array "}"
 *             | atom.
 *
 * package   --> package "/" atom.
 *             | atom.
 *
 * The name of a predicate is qualified when it starts with a module name
 * separated by the colon (:) operator. Unqualified predicate names are
 * extended by the module name of the Prolog text if the Prolog text has
 * been elevated to a module, or by the module names of the corresponding
 * public or package local predicates found in dependent modules.
 *
 * Examples:
 * call/1		            % is a predicate indicator
 * (=)/2			        % is a predicate indicator
 * basic/lists:member/2	    % is a predicate indicator
 *
 * A non-dynamic predicate without clauses can be declared via the directive
 * static/1. The context of a clause is determined from the predicate
 * name atom of the clause head.
 *
 * The predicate current_predicate/1 succeeds for a predicate that is visible
 * in the current context. The different visibility parameters are documented
 * in the module system section. Properties of a predicate can be accessed
 * and modified by the predicates predicate_property/2, set_predicate_property/2
 * and reset_predicate_property/3.
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

:- sys_context_property(here, C),
   set_source_property(C, use_package(foreign(jekpro/reference/reflect))).
:- sys_context_property(here, C),
   reset_source_property(C, sys_source_visible(public)).

:- sys_op(1150, fx, static).
:- set_oper_property(prefix(static), visible(public)).

/**
 * static P, ...:
 * The predicate sets the predicate P to static.
 */
% static +Indicators
static [P|Q] :- !,
   sys_static(P),
   static(Q).
static P,Q :- !,
   sys_static(P),
   static(Q).
static [] :- !.
static P :-
   sys_static(P).
:- set_predicate_property((static)/1, visible(public)).

sys_static(I) :-
   sys_ensure_shared_static(I),
   sys_check_style_predicate(I).
:- set_predicate_property(sys_static/1, visible(private)).

:- special(sys_ensure_shared_static/1, 'SpecialPred', 0).
:- set_predicate_property(sys_ensure_shared_static/1, visible(private)).

/**
 * current_predicate(P): [ISO 8.8.2]
 * The predicate succeeds for the visible predicates P.
 */
% current_predicate(-Indicator)
current_predicate(I) :-
   ground(I), !,
   sys_current_predicate_chk(I).
current_predicate(I) :-
   sys_current_predicate(L),
   sys_member(I, L).
:- set_predicate_property(current_predicate/1, visible(public)).

:- special(sys_current_predicate/1, 'SpecialPred', 1).
:- set_predicate_property(sys_current_predicate/1, visible(private)).

:- special(sys_current_predicate_chk/1, 'SpecialPred', 2).
:- set_predicate_property(sys_current_predicate_chk/1, visible(private)).

/**
 * predicate_property(P, Q):
 * The predicate succeeds for the properties Q of the predicate P.
 */
% predicate_property(+-Indicator, -+Property)
predicate_property(I, R) :-
   ground(I), !,
   sys_predicate_property2(I, R).
predicate_property(I, R) :-
   var(R), !,
   sys_current_predicate(L),
   sys_member(I, L),
   sys_predicate_property(I, P),
   sys_member(R, P).
predicate_property(I, R) :-
   sys_predicate_property_idx(R, P),
   sys_member(I, P).
:- set_predicate_property(predicate_property/2, visible(public)).

% sys_predicate_property2(+Indicator, -Property)
sys_predicate_property2(I, R) :-
   var(R), !,
   sys_predicate_property(I, P),
   sys_member(R, P).
sys_predicate_property2(I, R) :-
   functor(R, F, A),
   sys_predicate_property_chk(I, F/A, P),
   sys_member(R, P).
:- set_predicate_property(sys_predicate_property2/2, visible(private)).

:- special(sys_predicate_property/2, 'SpecialPred', 3).
:- set_predicate_property(sys_predicate_property/2, visible(private)).

:- special(sys_predicate_property_chk/3, 'SpecialPred', 4).
:- set_predicate_property(sys_predicate_property_chk/3, visible(private)).

:- special(sys_predicate_property_idx/2, 'SpecialPred', 5).
:- set_predicate_property(sys_predicate_property_idx/2, visible(private)).

/**
 * set_predicate_property(P, Q):
 * The predicate assigns the property Q to the predicate P.
 */
% set_predicate_property(+Indicator, +Property)
% already defined in special
% :- special(set_predicate_property/2, 'SpecialPred', 6).
% :- set_predicate_property(set_predicate_property/2, visible(public)).

/**
 * reset_predicate_property(P, Q):
 * The predicate de-assigns the property Q from the predicate P.
 */
% reset_predicate_property(+Indicator, +Property)
% already defined in special
% :- special(reset_predicate_property/2, 'SpecialPred', 7).
% :- set_predicate_property(reset_predicate_property/2, visible(public)).

% first defined in special.p
% sys_declaration_indicator(+Declaration, -Indicator).
:- sys_neutral_predicate(sys_declaration_indicator/2).
:- set_predicate_property(sys_declaration_indicator/2, visible(public)).
:- sys_context_property(here, C),
   set_predicate_property(sys_declaration_indicator/2, sys_public(C)).
:- set_predicate_property(sys_declaration_indicator/2, multifile).
:- sys_context_property(here, C),
   set_predicate_property(sys_declaration_indicator/2, sys_multifile(C)).
sys_declaration_indicator(static(I), I).

/**
 * sys_make_indicator(F, A, I):
 * The predicate succeeds when I is the indicator for the possibly
 * quantified name F and the arity A.
 */
% sys_make_indicator(+-NameColon, +-Integer, -+IndicatorColon)
sys_make_indicator(F, A, I) :-
   var(F), !,
   sys_make_indicator2(I, F, A).
sys_make_indicator(K, A, J) :-
   K = :(M,F), !,
   sys_make_indicator(F, A, I),
   sys_replace_site(J, K, :(M,I)).
sys_make_indicator(F, A, F/A).
:- set_predicate_property(sys_make_indicator/3, visible(public)).

% sys_make_indicator2(+IndicatorColon, -NameColon, -Integer)
sys_make_indicator2(I, _, _) :-
   var(I),
   throw(error(instantiation_error,_)).
sys_make_indicator2(J, K, A) :-
   J = :(M,I), !,
   sys_make_indicator2(I, F, A),
   sys_replace_site(K, J, :(M,F)).
sys_make_indicator2(F/A, G, B) :- !,
   F = G,
   A = B.
sys_make_indicator2(I, _, _) :-
   throw(error(type_error(predicate_indicator,I),_)).
:- set_predicate_property(sys_make_indicator2/3, visible(private)).

/**********************************************************/
/* Moved From Debugger                                    */
/**********************************************************/

% moved from provable.p in debugger
:- special(sys_provable_property_chk/3, 'SpecialPred', 8).
:- set_predicate_property(sys_provable_property_chk/3, visible(public)).

% moved from provable.p in debugger
:- special(sys_provable_property_idx/2, 'SpecialPred', 9).
:- set_predicate_property(sys_provable_property_idx/2, visible(public)).
