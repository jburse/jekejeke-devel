/**
 * This module provides access to the byte array datatype. The byte
 * data type is a reference data type to the Java array byte[] and
 * might be modified as a side effect. Nevertheless, it can also be
 * used immutably as a block of bytes, which gets collected by Java GC.
 *
 * The predicates atom_block/[2,3] allows converting between atoms
 * and byte arrays. The binary predicate insists on atom code points
 * between 0 and 255. The ternary predicate allows the full code
 * point range and defaults the encoding option to UTF-8.
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
:- use_package(foreign(matula/util/config)).
:- module(bytes, []).

/**
 * atom_block(A, B):
 * atom_block(A, B, O):
 * If A is a variable, then the predicate succeeds in A with the
 * atom for the block B. Otherwise the predicate succeeds in B
 * with the atom for the block B. The ternary predicate allows
 * specifying encoding options O.
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

/**
 * term_block(T, B):
 * term_block(T, B, O):
 * The predicate succeeds when the block B is the UTF-8 serialization
 * of the term T. The ternary predicate accepts read respectively
 * write options O.
 */
% term_block(+-Term, -+Bytes)
:- public term_block/2.
term_block(T, B) :-
   term_block(T, B, []).

% term_block(+-Term, -+Bytes, +List)
:- public term_block/3.
term_block(T, B, O) :- var(B), !,
   memory_write(S),
   open(S, write, K, [buffer(0)]),
   write_term(K, T, [quoted(true)|O]),
   flush_output(K),
   memory_get(K, B).
term_block(T, B, O) :-
   memory_read(B, S),
   open(S, read, K, [buffer(0)]),
   read_term(K, T, [terminator(end_of_file)|O]).

/**
 * term_atom(T, A):
 * term_atom(T, A, O):
 * The predicate succeeds when the atom A is the serialization
 * of the term T. The ternary predicate accepts read respectively
 * write options O.
 */
% term_atom(+-Term, -+Atom)
:- public term_atom/2.
term_atom(T, A) :-
   term_atom(T, A, []).

% term_atom(+-Term, -+Atom, +List)
:- public term_atom/3.
term_atom(T, A, O) :- var(A), !,
   memory_write(S),
   open(S, write, K, [buffer(0)]),
   write_term(K, T, [quoted(true)|O]),
   flush_output(K),
   memory_get(K, [], A).
term_atom(T, A, O) :-
   atom_block(A, B, []),
   memory_read(B, S),
   open(S, read, K, [buffer(0)]),
   read_term(K, T, [terminator(end_of_file)|O]).

/********************************************************************/
/* Memory Socket                                                    */
/********************************************************************/

/**
 * memory_write(S):
 * The predicate succeeds in S with a new write memory socket.
 */
% memory_write(-Socket)
:- public memory_write/1.
:- foreign_constructor(memory_write/1, 'Memory', new).

/**
 * memory_read(B, S):
 * The predicate succeeds in S with a new read memory socket
 * for the block B.
 */
% memory_read(+Bytes, -Socket)
:- public memory_read/2.
:- foreign_constructor(memory_read/2, 'Memory', new({byte})).

/**
 * memory_get(K, B):
 * The predicate succeeds in B with the block of the
 * output stream K.
 */
% memory_get(+Stream, -Bytes)
:- public memory_get/2.
:- foreign(memory_get/2, 'ForeignBytes', sysMemoryGet('Object')).

/**
 * memory_get(K, O, B):
 * The predicate succeeds in B with the atom of the
 * output stream K in the encoding options O.
 */
% memory_get(+Stream, +List, -Bytes)
:- public memory_get/3.
:- foreign(memory_get/3, 'ForeignBytes', sysMemoryGet('Object', 'Object')).
