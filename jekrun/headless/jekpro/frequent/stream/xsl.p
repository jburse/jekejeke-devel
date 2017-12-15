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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

:- package(library(jekpro/frequent/stream)).
:- use_package(foreign(matula/util/format)).
:- use_package(foreign(matula/util/transform)).
:- use_package(foreign(java/io)).
:- use_package(foreign(jekpro/frequent/stream)).

:- module(xsl, []).

/*******************************************************************/
/* XML Check                                                       */
/*******************************************************************/

/**
 * schema_new(S):
 * The predicate succeeds in S with a new XSD schema.
 */
% schema_new(-XSDSchema)
:- public schema_new/1.
:- foreign_constructor(schema_new/1, 'XSDSchema', new).

/**
 * schema_digest(S, D):
 * The predicate succeeds in digesting the DOM element D
 * into the XSD schema S.
 */
% schema_digest(+XSDSchema, +DomElement)
:- public schema_digest/2.
:- virtual schema_digest/2.
:- foreign(schema_digest/2, 'XSDSchema', digestElements('DomElement')).

/**
 * data_check(D, S, O):
 * The predicate succeeds in checking the DOM node D versus
 * the XSD schema S with the DOM options O.
 */
% data_check(+DomNode, +XSDSchema, +List)
:- public data_check/3.
:- foreign(data_check/3, 'ForeignSheet',
      sysXmlCheck('DomNode','XSDSchema','Object')).

/**
 * sheet_check(T, O):
 * The predicate succeeds in checking the DOM node T with
 * the XSL options O.
 */
% sheet_check(+DomNode, +List)
:- public sheet_check/2.
:- foreign(sheet_check/2, 'ForeignSheet', sysXslCheck('DomNode','Object')).

/**
 * sheet_transform(T, S, C, O):
 * The predicate succeeds in transforming the DOM node T into
 * the stream S with comment C and the XSL options O.
 */
% sheet_check(+DomNode, +Stream, +Atom, +List)
:- public sheet_transform/4.
:- foreign(sheet_transform/4, 'ForeignSheet',
      sysXslTransform('DomNode','Writer','String','Object')).
