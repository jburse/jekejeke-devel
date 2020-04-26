/**
 * This module provides access to the variant data type. The data type
 * is a comparator with further attributes. The comparator can be
 * a collator or it can be a callback into Prolog.
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

:- package(library(jekpro/frequent/advanced)).
:- use_package(foreign(jekpro/frequent/advanced)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/reference/structure)).

:- module(variant, []).

/**
 * sys_variant_comparator(O, C):
 * The predicate succeeds in C with the variant comparator
 * for the sort options O.
 */
% sys_variant_comparator(+List, -Comparator)
:- foreign(sys_variant_comparator/2, 'ForeignVariant',
      sysVariantComparator('Interpreter', 'Object')).

/**
 * sys_variant_eager(C):
 * The predicate succeeds if the variant comparator is eager.
 */
% sys_variant_eager(+Comparator)
:- foreign(sys_variant_eager/1, 'ForeignVariant',
      sysVariantEager('AbstractLexical')).

/**
 * sys_variant_reverse(C):
 * The predicate succeeds if the variant comparator is reverse.
 */
% sys_variant_reverse(+Comparator)
:- foreign(sys_variant_reverse/1, 'ForeignVariant',
      sysVariantReverse('AbstractLexical')).

/**
 * sys_variant_natural(C):
 * The predicate succeeds if the variant comparator is natural.
 */
% sys_variant_natural(+Comparator)
:- foreign(sys_variant_natural/1, 'ForeignVariant',
      sysVariantNatural('AbstractLexical')).

/**
 * sys_variant_dynamic(C):
 * The predicate succeeds if the variant comparator is shared dynamic.
 */
% sys_variant_dynamic(+Comparator)
:- foreign(sys_variant_dynamic/1, 'ForeignVariant',
      sysVariantDynamic('AbstractLexical')).

/**
 * sys_variant_group_local(C):
 * The predicate succeeds if the variant comparator is shared group local.
 */
% sys_variant_group_local(+Comparator)
:- foreign(sys_variant_group_local/1, 'ForeignVariant',
      sysVariantGroupLocal('AbstractLexical')).

/**
 * sys_variant_key(P):
 * The predicate succeeds in P with a sys_variant_key.
 */
% sys_variant_key(-Key)
:- private sys_variant_key/1.
:- foreign_constructor(sys_variant_key/1, 'VariantKey', new).
