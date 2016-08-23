/**
 * Body conversion converts a goal of the form X where X is a variable,
 * into a goal of the form call(X). And goals which are not of the
 * form X where X is a callable are rejected by this conversion. Body
 * conversion is in effect when clauses are asserted, either dynamically
 * or statically. The effect can be seen by the following example. In
 * the static rule for the predicate p/0 the variable X will be wrapped
 * via a call/1:
 *
 * Example
 * ?- [user].
 * p :- q(X), X.
 * ^D
 * ?- listing.
 * p :-
 *     q(A),
 *     call(A).
 *
 * Body conversion is also in effect when goals are executed, either
 * directly or deferred via me-ta-arguments. The body conversion can
 * be switched off via the flag sys_body_convert. The flag only affects
 * the body conversion for the Prolog session queries, for the Prolog
 * text clauses and for the Prolog text directives. The dynamic clause
 * assertions and the deferred meta-arguments are not affected by the
 * flag, these places will still do body conversion.
 *
 * The body conversion is table driven. The meta-predicate declarations
 * and determine how arguments are traversed. The predicate properties
 * sys_body/0 and sys_rule/0 will indicate that the meta-predicates
 * should be traversed during body conversion respectively rule conversion.
 * To facilitate the declaration of body and rule conversion the predicates
 * sys_neutral_predicate/1 and sys_neutral_evaluable/1 allow defining
 * dictionary entries that are not yet completely defined and
 * cannot be executed.
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
   set_source_property(C, use_package(foreign(jekpro/model/builtin))).
:- sys_get_context(here, C),
   reset_source_property(C, sys_source_visible(public)).

/**
 * sys_neutral_predicate(I):
 * If no predicate has yet been defined for the predicate indicator I,
 * defines a corresponding neutral predicate.
 */
% sys_neutral_predicate(+Indicator)
:- special(sys_neutral_predicate/1, 'SpecialBody', 0).
:- set_predicate_property(sys_neutral_predicate/1, visible(public)).

/**
 * sys_neutral_oper(I):
 * If no syntax operator has yet been defined for the syntax operator
 * indicator I, defines a corresponding neutral syntax operator.
 */
% sys_neutral_oper(+Indicator)
:- special(sys_neutral_oper/1, 'SpecialBody', 2).
:- set_predicate_property(sys_neutral_oper/1, visible(public)).

% sys_replace_site(-Term, +Term, +Term)
:- special(sys_replace_site/3, 'SpecialBody', 3).
:- set_predicate_property(sys_replace_site/3, visible(public)).

% sys_parent_goal(-Term)
:- special(sys_parent_goal/1, 'SpecialBody', 4).
:- set_predicate_property(sys_parent_goal/1, visible(public)).
