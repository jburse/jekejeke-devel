/**
 * This module provides a couple of simple utilities to deal with
 * the generation and parsing of XML texts. The predicate text_escape/2
 * can be used to escape and un-escape texts. The predicate text_escape/2
 * will escape the characters '"<>&' and the character 0xA0. It is
 * suitable for attribute values in double quotes and for texts.
 *
 * Examples:
 * ?- text_escape('&lt;abc&gt;', X).
 * X = '&amp;lt;abc&amp;gt;'
 * ?- text_escape(X, '&amp;lt;abc&amp;gt;').
 * X = '&lt;abc&gt;'
 *
 * The predicate base64_block/2 can be used to base64 encode and decode
 * a byte block. This code allows representing 8-bit bytes as ASCII
 * characters. When generating base64 code the predicate will produce
 * 10 blocks of 4 characters effectively encoding 30 bytes. While decoding
 * the terminating characters = indicate the number of fill bytes.
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

:- package(library(jekpro/frequent/stream)).
:- use_package(foreign(matula/util/system)).

:- module(xml, []).

/****************************************************************/
/* Entity Conversion                                            */
/****************************************************************/

/**
 * text_escape(T, E):
 * If T is a variable then the predicate succeeds when T unifies with
 * the text unescape of E. Otherwise the predicate succeeds when E unifies
 * with the text escape of T.
 */
% text_escape(+-Atom, -+Atom)
:- public text_escape/2.
text_escape(X, Y) :- var(X), !,
   sys_text_unescape(Y, X).
text_escape(X, Y) :-
   sys_text_escape(X, Y).

:- private sys_text_escape/2.
:- foreign(sys_text_escape/2, 'ForeignXml',
   sysTextEscape('String')).

:- private sys_text_unescape/2.
:- foreign(sys_text_unescape/2, 'ForeignXml',
   sysTextUnescape('String')).

/**
 * html_escpae(T):
 * html_escape(O, T):
 * The predicate sends the text T escaped to the current output. The
 * binary predicate allows specifying an output stream O.
 */
% html_escape(+Atom)
:- public html_escape/1.
html_escape(Text) :-
   text_escape(Text, Escape),
   write(Escape).

% html_escape(+Stream, +Atom)
:- public html_escape/2.
html_escape(Response, Text) :-
   text_escape(Text, Escape),
   write(Response, Escape).

/****************************************************************/
/* Base64 Conversion                                            */
/****************************************************************/

/**
 * hex_block(T, E):
 * If T is a variable then the predicate succeeds when T unifies with
 * the hex encode of E. Otherwise the predicate succeeds when E unifies
 * with the hex decode of T.
 */
% hex_block(+-Atom, -+Bytes)
:- public hex_block/2.
hex_block(X, Y) :-
   var(X), !,
   sys_hex_encode(Y, X).
hex_block(X, Y) :-
   sys_hex_decode(X, Y).

:- private sys_hex_decode/2.
:- foreign(sys_hex_decode/2, 'ForeignXml',
   sysHexDecode('String')).

:- private sys_hex_encode/2.
:- foreign(sys_hex_encode/2, 'ForeignXml',
   sysHexEncode({byte})).

/**
 * base64_block(T, E):
 * If T is a variable then the predicate succeeds when T unifies with
 * the based64 encode of E. Otherwise the predicate succeeds when E unifies
 * with the base64 decode of T.
 */
% base64_block(+-Atom, -+Bytes)
:- public base64_block/2.
base64_block(X, Y) :-
   var(X), !,
   sys_base64_encode(Y, X).
base64_block(X, Y) :-
   sys_base64_decode(X, Y).

:- private sys_base64_decode/2.
:- foreign(sys_base64_decode/2, 'ForeignXml',
   sysBase64Decode('String')).

:- private sys_base64_encode/2.
:- foreign(sys_base64_encode/2, 'ForeignXml',
   sysBase64Encode({byte})).
