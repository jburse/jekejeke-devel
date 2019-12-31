/**
 * This module provides access to the byte array datatype. The
 * byte data type is a reference data type to the Java array byte[]
 * and might be modified as a side effect. But it can also be used
 * immutably as a block of bytes.
 *
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

:- package(library(jekpro/reference/structure)).
:- use_package(foreign(jekpro/reference/structure)).
:- module(bytes, []).

/**
 * atom_block(A, B):
 * atom_block(A, B, O):
 * If A is a variable, then the predicate succeeds in A with the
 * atom for the block B. Otherwise the predicate succeeds in B
 * with the atom for the block B. The ternary predicate allows
 * specifying encoding options.
 */
% atom_block(+-Atom, -+Bytes)
:- public atom_block/2.
atom_block(A, B) :- var(A), !,
   sys_block_to_atom(B, A).
atom_block(A, B) :-
   sys_atom_to_block(A, B).

:- private sys_block_to_atom/2.
:- foreign(sys_block_to_atom/2, 'ForeignBytes',
      sysBlockToAtom({byte})).

:- private sys_atom_to_block/2.
:- foreign(sys_atom_to_block/2, 'ForeignBytes',
      sysAtomToBlock('String')).

% atom_block(+-Atom, -+Bytes, +List)
:- public atom_block/3.
atom_block(A, B, O) :- var(A), !,
   sys_block_to_atom(B, O, A).
atom_block(A, B, O) :-
   sys_atom_to_block(A, O, B).

:- private sys_block_to_atom/3.
:- foreign(sys_block_to_atom/3, 'ForeignBytes',
      sysBlockToAtom({byte}, 'Object')).

:- private sys_atom_to_block/3.
:- foreign(sys_atom_to_block/3, 'ForeignBytes',
      sysAtomToBlock('String', 'Object')).
