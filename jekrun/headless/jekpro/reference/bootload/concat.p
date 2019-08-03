/**
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
   set_source_property(C, use_package(foreign(jekpro/reference/bootload))).
:- sys_context_property(here, C),
   reset_source_property(C, sys_source_visible(public)).

/*************************************************************************/
/* Atom Helper                                                           */
/*************************************************************************/

/**
 * sys_atom_concat(A, B, C):
 * Succeeds when C unifies with the concatenation of A and B.
 */
% sys_atom_concat(+Atom, +Atom, -Atom)
:- foreign(sys_atom_concat/3, 'ForeignConcat',
   sysAtomConcat('String', 'String')).
:- set_predicate_property(sys_atom_concat/3, visible(public)).

/**
 * sys_atom(A):
 * Succeeds when A is an atom.
 */
% sys_atom(+Term)
:- foreign(sys_atom/1, 'ForeignConcat',
   sysAtom('Object')).
:- set_predicate_property(sys_atom/1, visible(public)).
