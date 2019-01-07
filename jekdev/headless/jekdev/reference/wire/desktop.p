/**
 * The refinement class for a desktop monitor.
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

:- package(library(jekdev/reference/wire)).

:- module(desktop, []).
:- use_module(monitor).
:- use_module(library(notebook/httpsrv)).
:- reexport(view).

/**
 * dispatch(O, P, A, S):
 * The predicate succeeds in dispatching the request for object
 * O, with path P, with parameter list A and the session S.
 */
% dispatch(+Object, +Spec, +Assoc, +Session)
:- override dispatch/4.
:- public dispatch/4.
dispatch(_, '/layout.html', _, Session) :- !,
   setup_call_cleanup(
      open(Session, write, Response),
      send_text(library(wire/pages/layout), Response),
      close(Response)).
dispatch(Object, Spec, Assoc, Session) :-
   wire/view:dispatch(Object, Spec, Assoc, Session).

% html_target(+Object, +Stream, +Atom)
:- public html_target/3.
html_target(_, Response, Target) :-
   write(Response, ' target="'),
   html_escape(Response, Target),
   write(Response, '"').