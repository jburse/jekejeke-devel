/**
 * This module provides a couple of simple utilities to deal with
 * the generation and parsing of uniform resource identifiers (URIs).
 * The predicates make_query/4, make_spec/4 and make_uri/4 allow
 * constructing and deconstructing queries, specs and URIs. The
 * predicates work bidirectional without loss of data.
 *
 * Example:
 * ?- make_query(X, Y, Z, 'a%3Db=c%26d&e').
 * X = 'a=b',
 * Y = 'c&d',
 * Z = e
 *
 * The predicates make_query/4, make_spec/4 and make_uri/4 do a minimal
 * encoding. For the parameter name and the parameter value the characters
 * '#%=&\' will be encoded. For the hash the characters '%\' will be
 * encoded. For the spec the characters '?#%\' will be encoded. If used
 * in the other direction the predicates will perform decoding of the
 * corresponding components.
 *
 * An URI is relative when it neither contains a scheme nor an
 * authority, and if the path is relative. The predicate is_relative_uri/1
 * checks whether an URI is relative. The predicate follow_uri/3 can be
 * used to resolve and relativize URIs. Contrary to the Java URL class,
 * it is agnostic to the scheme of the URIs and will allow any scheme.
 * In these predicates the path component is handled by the corresponding
 * routines from system/file.
 *
 * Example:
 * ?- follow_uri('file:/foo/bar/baz?jack#jill', X, 'file:/foo/tip/tap?fix#fox').
 * X = '../tip/tap?fix#fox'
 *
 * The predicates canonical_spec/2 and canonical_uri/2 can be used to
 * canonize specs and URIs. In this predicate the path component is
 * handled by the corresponding routine from system/file. The predicate
 * uri_encode/2 can be used to encode and decode URIs. The predicate
 * uri_encode/2 will percent encode characters above 0x7F. As a result
 * the URI will only contain ASCII. If used in the other direction
 * the predicate will first decode and then minimal encode again.
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

:- package(library(jekpro/frequent/system)).
:- use_package(foreign(matula/util/system)).

:- module(uri, []).

/************************************************************/
/* Query & Uri Assembly                                     */
/************************************************************/

/**
 * make_query(N, V, R, Q):
 * If N, V or R is a variable then the predicate succeeds when N, V and
 * R unify with the first parameter name, the first parameter value and
 * the rest query of the query Q. Otherwise the predicates succeeds when
 * Q unifies with the constructed query.
 */
% make_query(+-Atom, +-Atom, +-Atom, -+Atom)
:- public make_query/4.
make_query(N, V, R, Q) :-
   (  var(N)
   ;  var(V)
   ;  var(R)), !,
   sys_query_name(Q, N),
   sys_query_value(Q, V),
   sys_query_rest(Q, R).
make_query(N, V, R, Q) :-
   sys_query_make(N, V, R, Q).

:- private sys_query_name/2.
:- foreign(sys_query_name/2, 'ForeignUri',
      sysQueryName('String')).

:- private sys_query_value/2.
:- foreign(sys_query_value/2, 'ForeignUri',
      sysQueryValue('String')).

:- private sys_query_rest/2.
:- foreign(sys_query_rest/2, 'ForeignUri',
      sysQueryRest('String')).

:- private sys_query_make/4.
:- foreign(sys_query_make/4, 'ForeignUri',
      sysQueryMake('String','String','String')).

/**
 * make_spec(E, A, P, S):
 * If E, A or P is a variable then the predicate succeeds when E, A and
 * P unify with the scheme, authority and path respectively of the spec S.
 * Otherwise the predicate succeeds when S unifies with the
 * constructed spec.
 */
% make_spec(+-Atom, +-Atom, +-Atom, -+Atom)
:- public make_spec/4.
make_spec(E, A, P, S) :-
   (  var(E)
   ;  var(A)
   ;  var(P)), !,
   sys_spec_scheme(S, E),
   sys_spec_authority(S, A),
   sys_spec_path(S, P).
make_spec(E, A, P, S) :-
   sys_spec_make(E, A, P, S).

:- private sys_spec_scheme/2.
:- foreign(sys_spec_scheme/2, 'ForeignUri',
      sysSpecScheme('String')).

:- private sys_spec_authority/2.
:- foreign(sys_spec_authority/2, 'ForeignUri',
      sysSpecAuthority('String')).

:- private sys_spec_path/2.
:- foreign(sys_spec_path/2, 'ForeignUri',
      sysSpecPath('String')).

:- private sys_spec_make/4.
:- foreign(sys_spec_make/4, 'ForeignUri',
      sysSpecMake('String','String','String')).

/**
 * make_uri(S, Q, H, U):
 * If S, Q or H is a variable then the predicate succeeds when S, Q and
 * H unify with the spec, query and hash respectively of the URI U.
 * Otherwise the predicate succeeds when U unifies with the
 * constructed URI.
 */
% make_uri(+-Atom, +-Atom, +-Atom, -+Atom)
:- public make_uri/4.
make_uri(S, Q, H, U) :-
   (  var(S)
   ;  var(Q)
   ;  var(H)), !,
   sys_uri_spec(U, S),
   sys_uri_query(U, Q),
   sys_uri_hash(U, H).
make_uri(S, Q, H, U) :-
   sys_uri_make(S, Q, H, U).

:- private sys_uri_hash/2.
:- foreign(sys_uri_hash/2, 'ForeignUri',
      sysUriHash('String')).

:- private sys_uri_query/2.
:- foreign(sys_uri_query/2, 'ForeignUri',
      sysUriQuery('String')).

:- private sys_uri_spec/2.
:- foreign(sys_uri_spec/2, 'ForeignUri',
      sysUriSpec('String')).

:- private sys_uri_make/4.
:- foreign(sys_uri_make/4, 'ForeignUri',
      sysUriMake('String','String','String')).

/************************************************************/
/* Uri Following                                            */
/************************************************************/

/**
 * is_relative_uri(U):
 * The predicate succeeds when the URI U is a relative URI.
 */
% is_relative_uri(+Atom)
:- public is_relative_uri/1.
:- foreign(is_relative_uri/1, 'ForeignUri',
      sysUriIsRelative('String')).

/**
 * follow_uri(B, R, A):
 * If R is a variable then the predicate succeeds when R unifies with
 * the relative or absolute URI that leads from B to A. Otherwise the
 * predicate succeeds when A unifies with the URI that results
 * from B by following R.
 */
:- public follow_uri/3.
follow_uri(B, R, A) :-
   var(R), !,
   sys_uri_relative(B, A, R).
follow_uri(B, R, A) :-
   sys_uri_absolute(B, R, A).

:- private sys_uri_absolute/3.
:- foreign(sys_uri_absolute/3, 'ForeignUri',
      sysUriAbsolute('String','String')).

:- private sys_uri_relative/3.
:- foreign(sys_uri_relative/3, 'ForeignUri',
      sysUriRelative('String','String')).

/************************************************************/
/* Canonical Spec & Uri                                     */
/************************************************************/

/**
 * canonical_spec(S, C):
 * The predicate succeeds when C unifies with canonical spec of S.
 */
% canonical_spec(+Atom, -Atom)
:- public canonical_spec/2.
:- foreign(canonical_spec/2, 'ForeignUri',
      sysCanonicalSpec('String')).

/**
 * canonical_uri(U, C):
 * The predicate succeeds when C unifies with canonical URI of U.
 */
% canonical_uri(+Atom, -Atom)
:- public canonical_uri/2.
:- foreign(canonical_uri/2, 'ForeignUri',
      sysCanonicalUri('String')).

/************************************************************/
/* Uri Encoding                                             */
/************************************************************/

/**
 * uri_encode(T, E):
 * If T is a variable then the predicate succeeds when T unifies with
 * the URI decode of E. Otherwise the predicate succeeds when E unifies
 * with the URI encode of T.
 */
% uri_encode(+-Atom, -+Atom)
:- public uri_encode/2.
uri_encode(X, Y) :-
   var(X), !,
   sys_uri_decode(Y, X).
uri_encode(X, Y) :-
   sys_uri_encode(X, Y).

:- private sys_uri_encode/2.
:- foreign(sys_uri_encode/2, 'ForeignUri',
      sysUriEncode('String')).

:- private sys_uri_decode/2.
:- foreign(sys_uri_decode/2, 'ForeignUri',
      sysUriDecode('String')).
