/**
 * A transcript of the current session can be written to a file.
 * The transcript captures what is read from the console and
 * written to the console.
 *
 * Example:
 * ?- protocol('session.log').
 * Yes
 * ... do something ..
 * ?- noprotocol.
 *
 * The transcript captures only the console input/output of the
 * thread that is attached to the console where the command has
 * been invoked. Other threads will not be visible as long as
 * they don't input/output to this console as well.
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

:- module(protocol, []).

/***********************************************************************/
/* Input/Output Protocol                                               */
/***********************************************************************/

/**
 * protocol(F):
 * Start transcript of the current session to the file F. If the file
 * already exists the transcript is appended to the existing file.
 * Otherwise a new file is created.
 */
% protocol(+Atom)
:- public protocol/1.
:- sys_notrace protocol/1.
protocol(_) :-
   current_prolog_flag(sys_cur_input, Input),
   stream_property(Input, sys_protocol(Protocol)),
   Protocol \== null,
   throw(error(permission_error(protocol, state, Protocol), _)).
protocol(Name) :-
   open(Name, append, Protocol),
   current_prolog_flag(sys_cur_input, Input),
   set_stream_property(Input, sys_protocol(Protocol)),
   current_prolog_flag(sys_cur_output, Output),
   set_stream_property(Output, sys_protocol(Protocol)),
   current_prolog_flag(sys_cur_error, Error),
   set_stream_property(Error, sys_protocol(Protocol)).
:- set_predicate_property(protocol/1, sys_notrace).

/**
 * noprotocol:
 * Stop transcript of the current session. The current transcript
 * wile is closed.
 */
% noprotocol
:- public noprotocol/0.
:- sys_notrace noprotocol/0.
noprotocol :-
   current_prolog_flag(sys_cur_input, Input),
   stream_property(Input, sys_protocol(Protocol)),
   Protocol == null,
   throw(error(permission_error(protocol, state, Protocol), _)).
noprotocol :-
   current_prolog_flag(sys_cur_input, Input),
   stream_property(Input, sys_protocol(Protocol)),
   close(Protocol),
   set_stream_property(Input, sys_protocol(null)),
   current_prolog_flag(sys_cur_output, Output),
   set_stream_property(Output, sys_protocol(null)),
   current_prolog_flag(sys_cur_error, Error),
   set_stream_property(Error, sys_protocol(null)).