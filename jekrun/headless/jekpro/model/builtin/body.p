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
 *     q(X),
 *     call(X).
 *
 * Body conversion is also in effect when goals are executed, either
 * directly or deferred via meta-arguments. The body conversion can
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
 * To facilitate the declaration the predicate sys_neutral_predicate/1 allows
 * defining dictionary entries that are not yet completely defined.
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
   set_source_property(C, use_package(foreign(jekpro/model/builtin))).
:- sys_context_property(here, C),
   reset_source_property(C, sys_source_visible(public)).

% already defined in special.p
% :- public prefix(':-').

% already defined in special.p
% :- public infix(':-').

% already defined in special.p
% :- public infix(',').

/**
 * :- A:
 * The predicate cannot be executed and exists only to configure
 * the body conversion table.
 */
% :- +Goal
(:- _) :- throw(error(existence_error(body, (:-)/1), _)).
:- set_predicate_property((:-)/1, visible(public)).
:- set_predicate_property((:-)/1, meta_predicate((:- -1))).
:- sys_context_property(here, C),
   set_predicate_property((:-)/1, sys_meta_predicate(C)).

/**
 * A :- B:
 * The predicate cannot be executed and exists only to configure
 * the body conversion table.
 */
% +Term :- +Goal
(_ :- _) :- throw(error(existence_error(body, (:-)/2), _)).
:- set_predicate_property((:-)/2, visible(public)).
:- set_predicate_property((:-)/2, meta_predicate((0 :- -1))).
:- sys_context_property(here, C),
   set_predicate_property((:-)/2, sys_meta_predicate(C)).

/**
 * A, B: [ISO 7.8.5]
 * The predicate succeeds whenever A and B succeed. Both goal
 * arguments A and B are cut transparent.
 */

% (+Goal, +Goal)
:- sys_neutral_predicate(','/2).
:- set_predicate_property(','/2, sys_nobarrier).
:- set_predicate_property(','/2, sys_proto).
A, B :- A, B.                                           % Proto
:- set_predicate_property(','/2, sys_notrace).
:- set_predicate_property(','/2, visible(public)).
:- set_predicate_property(','/2, meta_predicate((0, 0))).
:- sys_context_property(here, C),
   set_predicate_property(','/2, sys_meta_predicate(C)).

/**
 * call(A): [ÍSO 7.8.3]
 * The predicate succeeds whenever A succeeds. The goal argument
 * A is converted before calling.
 */
% call(+Goal)
:- special(call/1, 'SpecialBody', 0).
:- set_predicate_property(call/1, visible(public)).
:- set_predicate_property(call/1, meta_predicate(call(0))).
:- sys_context_property(here, C),
   set_predicate_property(call/1, sys_meta_predicate(C)).

% sys_alter(+Directive, +Directive)
:- special(sys_alter/2, 'SpecialBody', 1).
:- set_predicate_property(sys_alter/2, visible(public)).

% sys_guard(+Directive)
:- special(sys_guard/1, 'SpecialBody', 2).
:- set_predicate_property(sys_guard/1, visible(public)).

% sys_begin
:- special(sys_begin/0, 'SpecialBody', 3).
:- set_predicate_property(sys_begin/0, visible(public)).

% sys_commit
:- special(sys_commit/0, 'SpecialBody', 4).
:- set_predicate_property(sys_commit/0, visible(public)).

% sys_soft_begin
:- special(sys_soft_begin/0, 'SpecialBody', 5).
:- set_predicate_property(sys_soft_begin/0, visible(public)).

% sys_soft_commit
:- special(sys_soft_commit/0, 'SpecialBody', 6).
:- set_predicate_property(sys_soft_commit/0, visible(public)).
