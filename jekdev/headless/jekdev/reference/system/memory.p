/**
 * This module provides non-random access memory streams. A new read
 * memory stream can be created via the predicate memory_read/3. A new
 * write memory stream can be created via the predicate memory_write/2.
 * The content of write memory stream can be retrieved via the predicate
 * memory_get/2.
 *
 * Example:
 * ?- memory_write([], S), write(S, foo), memory_get(S, atom(A)).
 * S = 0r1479d830,
 * A = foo
 *
 * ?- memory_read(atom('foo.\n'), [], S), read(S, T).
 * S = 0r28100c13,
 * T = foo
 *
 * The current implementation is limited in that the streams cannot
 * be created with the reposition property. It is therefore not possibly
 * to use the methods set_stream_position/2 or set_stream_length/2. Otherwise
 * all the byte, char, term and stream operations can be applied as if they
 * were file or web streams.
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

:- package(library(jekdev/reference/system)).
:- use_package(foreign(jekdev/reference/system)).

:- module(memory, []).

/**
 * memory_read(T, O, S)):
 * The predicate succeeds with a new read memory stream S for the
 * initial data T and the open options O. For the data format and
 * a listing of the open options see the API documentation.
 */
% memory_read(+Term, +Options, -Stream)
:- public memory_read/3.
:- foreign(memory_read/3, 'ForeignMemory',
      sysMemoryRead('Object','Object')).

/**
 * memory_write(O, S):
 * The predicate succeeds with a new write memory stream S for the
 * open options O. The predicate recognizes the same open options
 * as the predicate memory_read/3.
 */
% memory_write(+Options, -Stream)
:- public memory_write/2.
:- foreign(memory_write/2, 'ForeignMemory',
      sysMemoryWrite('Object')).

/**
 * memory_get(S, T):
 * The predicate succeeds with the current data T of the write
 * stream S. The predicate recognizes the same data formats as
 * the predicate memory_read/3.
 */
% memory_get(+Stream, -Term)
:- public memory_get/2.
:- foreign(memory_get/2, 'ForeignMemory',
      sysMemoryGet('Object')).



