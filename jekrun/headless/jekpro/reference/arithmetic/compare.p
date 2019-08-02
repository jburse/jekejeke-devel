/**
 * The arithmetic comparisons are more flexible than the lexical
 * comparisons. They are defined for integers, floats and decimals.
 * For decimals comparison across scales is supported. The same
 * widening as already defined for the basic operations applies as well:
 *
 * Examples:
 * 1 < 2
 * 1.0 < 2.0
 * 0d1.00D < 2
 * 1 =:= 0d1.00
 *
 * We also provide evaluable functions min/2 and max/2. These functions are
 * based on the aforementioned arithmetic comparison. The type of the return
 * value depends on the order of the arguments of these evaluable functions.
 *
 * min, max: integer x integer -> integer
 * min, max: float x float -> float
 * min, max: decimal x decimal -> decimal
 * epsilon: float
 *
 * The constant epsilon allows querying the smallest float number that when
 * added to one will still result in a float number different from one
 * without any rounding.
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

:- use_package(foreign(jekpro/reference/arithmetic)).
:- use_package(foreign(jekpro/frequent/basic)).

:- module(user, []).

:- public infix(=:=).
:- op(700, xfx, =:=).

:- public infix(=\=).
:- op(700, xfx, =\=).

:- public infix(<).
:- op(700, xfx, <).

:- public infix(=<).
:- op(700, xfx, =<).

:- public infix(>).
:- op(700, xfx, >).

:- public infix(>=).
:- op(700, xfx, >=).

/**
 * X =:= Y: [ISO 8.7.1]
 * Succeeds when X arithmetically equals Y, otherwise fails.
 */
% +Expr =:= +Expr
:- public =:= /2.
:- set_predicate_property(=:= /2, meta_predicate(=:=(#(1),#(1)))).
:- sys_context_property(here, C),
   set_predicate_property(=:= /2, sys_meta_predicate(C)).
:- special(=:= /2, 'SpecialCompare', 0).

/**
 * X =\= Y: [ISO 8.7.1]
 * Succeeds when X does not arithmetically equal Y, otherwise fails.
 */
% +Expr =\= +Expr
:- public =\= /2.
:- set_predicate_property(=\= /2, meta_predicate(=\=(#(1),#(1)))).
:- sys_context_property(here, C),
   set_predicate_property(=\= /2, sys_meta_predicate(C)).
:- special(=\= /2, 'SpecialCompare', 1).

/**
 * X < Y: [ISO 8.7.1]
 * Succeeds when X is arithmetically less than Y, otherwise fails.
 */
% +Expr < +Expr
:- public < /2.
:- set_predicate_property(< /2, meta_predicate(<(#(1),#(1)))).
:- sys_context_property(here, C),
   set_predicate_property(< /2, sys_meta_predicate(C)).
:- special(< /2, 'SpecialCompare', 2).

/**
 * X =< Y: [ISO 8.7.1]
 * Succeeds when X is arithmetically less or equal to Y, otherwise fails.
 */
% +Expr =< +Expr
:- public =< /2.
:- set_predicate_property(=< /2, meta_predicate(=<(#(1),#(1)))).
:- sys_context_property(here, C),
   set_predicate_property(=< /2, sys_meta_predicate(C)).
:- special(=< /2, 'SpecialCompare', 3).

/**
 * X > Y: [ISO 8.7.1]
 * Succeeds when X is arithmetically greater than Y, otherwise fails.
 */
% +Expr > +Expr
% already defined in member.p

/**
 * X >= Y: [ISO 8.7.1]
 * Succeeds when X is arithmetically greater or equal to Y, otherwise fails.
 */
% +Expr >= +Expr
:- public >= /2.
:- set_predicate_property(>= /2, meta_predicate(>=(#(1),#(1)))).
:- sys_context_property(here, C),
   set_predicate_property(>= /2, sys_meta_predicate(C)).
:- special(>= /2, 'SpecialCompare', 5).

/**
 * min(X, Y): [TC2 9.3.9]
 * If X and Y are both numbers then the function returns the minimum of X and Y.
 */
% min: integer x integer -> integer
% min: float x float -> float
% min: decimal x decimal -> decimal
:- public min/3.
:- special(min/3, 'EvaluableCompare', 0).

/**
 * max(X, Y): [TC2 9.3.8]
 * If X and Y are both numbers then the function returns the maximum of X and Y.
 */
% max: integer x integer -> integer
% max: float x float -> float
% max: decimal x decimal -> decimal
:- public max/3.
:- special(max/3, 'EvaluableCompare', 1).

/**
 * epsilon: [N208 9.7.3]
 * Returns the ulp of 64-bit one.
 */
:- public epsilon/1.
% :- foreign_const(epsilon/1, 'SpecialCompare', 'EPSILON').
:- foreign_fun(epsilon/1, 'ForeignHyper', epsilon).

/**
 * epsilon32:
 * Returns the ulp of 32-bit one.
 */
:- public epsilon32/1.
% :- foreign_const(epsilon32/1, 'SpecialCompare', 'EPSILON32').
:- foreign_fun(epsilon32/1, 'ForeignHyper', epsilon32).
