/**
 * Lexical comparison allows comparing terms without an attempt to
 * instantiate the terms. These predicates are therefore meta-logical.
 * The predicates (==)/2 and (\==)/2 perform an equality test. The
 * predicates (@<)/2, (@=<)/2, (@>)/2 and (@>=)/2 use a linear ordering
 * based on a lexical comparison. The lexical comparison first looks
 * on the basic type of the involved terms. The ordering of the basic
 * types is as follows:
 *
 * variable < decimal < float < integer < reference < atom < compound
 *
 * Variables are ordered according to their internal instantiation
 * numbering. Integers, floats and decimals are arithmetically ordered.
 * But there is no mixing of integers, floats and decimals. Atoms are
 * ordered according to their internal character representation. For
 * compounds first the arity is compared, then the functor is compared
 * and finally the arguments from left to right. The predicate
 * compare/3 returns <, = or >.
 *
 * Examples:
 * ?- 1 @< 2.0.
 * No
 * ?- compare(O, 1, 2.0).
 * O = >
 *
 * Reference types can always be used in equality tests. Whether a
 * reference type can be compared depends on whether it implements the
 * Java Comparable interface. The predicates locale_compare/[3,4]
 * allow a locale comparison. In locale comparison the atoms and
 * functors are ordered according to a locale specific Java collator.
 * Locale comparison for reference types is not yet supported.
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

:- use_package(foreign(jekpro/reference/structure)).

:- module(user, []).

:- public infix(==).
:- op(700, xfx, ==).

:- public infix(\==).
:- op(700, xfx, \==).

:- public infix(@<).
:- op(700, xfx, @<).

:- public infix(@=<).
:- op(700, xfx, @=<).

:- public infix(@>).
:- op(700, xfx, @>).

:- public infix(@>=).
:- op(700, xfx, @>=).

/**
 * X == Y: [ISO 8.4.1]
 * The predicate succeeds when X is lexically equal to Y.
 */
% +Term == +Term
:- public == /2.
:- special(== /2, 'SpecialLexical', 0).

/**
 * X \== Y: [ISO 8.4.1]
 * The predicate succeeds when X is not lexically equal to Y.
 */
% +Term \== +Term
:- public \== /2.
:- special(\== /2, 'SpecialLexical', 1).

/**
 * X @< Y: [ISO 8.4.1]
 * The predicate succeeds when X is lexically before Y.
 */
% +Term @< +Term
:- public @< /2.
:- special(@< /2, 'SpecialLexical', 2).

/**
 * X @=< Y: [ISO 8.4.1]
 * The predicate succeeds when X is lexically before or equal to Y.
 */
% +Term @=< +Term
:- public @=< /2.
:- special(@=< /2, 'SpecialLexical', 3).

/**
 * X @> Y: [ISO 8.4.1]
 * The predicate succeeds when X is lexically after Y.
 */
% +Term @> +Term
:- public @> /2.
:- special(@> /2, 'SpecialLexical', 4).

/**
 * X @>= Y: [ISO 8.4.1]
 * The predicate succeeds when X is lexically after or equal to Y.
 */
% +Term @>= +Term
:- public @>= /2.
:- special(@>= /2, 'SpecialLexical', 5).

/**
 * compare(O, X, Y): [TC2 8.4.2]
 * The predicate succeeds when O unifies with the result of comparing
 * X to Y. The result is one of the following atoms <, = or >.
 */
% compare(-Atom, +Term, +Term)
:- public compare/3.
:- special(compare/3, 'SpecialLexical', 6).

/**
 * locale_compare(O, X, Y):
 * locale_compare(C, O, X, Y):
 * The predicate succeeds when O unifies with the result of locale
 * comparing X to Y. The result is one of the following atoms <, =
 * or >. The quaternary predicate allows specifying a locale C.
 */
% locale_compare(-Atom, +Term, +Term)
:- public locale_compare/3.
locale_compare(O, X, Y) :-
   current_prolog_flag(sys_locale, C),
   locale_compare(C, O, X, Y).

% locale_compare(+Atom, -Atom, +Term, +Term)
:- public locale_compare/4.
:- special(locale_compare/4, 'SpecialLexical', 7).
