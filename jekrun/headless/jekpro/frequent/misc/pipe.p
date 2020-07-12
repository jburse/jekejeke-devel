/**
 * Pipes allow exchanging messages. Messages are Prolog terms and
 * are copied. An unbounded queue can be created by the predicate
 * pipe_new/1. A bounded queue can be created by the predicate
 * pipe_new/2. Pipes need not be explicitly destroyed, they will
 * automatically be reclaimed by the Java GC when not anymore used.
 * Threads waiting for a pipe can be interrupted.
 *
 * Example:
 * ?- pipe_new(1, Q), pipe_put(Q, p(X)), pipe_take(Q, R).
 * Q = 0ra2a372,
 * R = p(_A)
 *
 * The predicates pipe_put/2, pipe_offer/2 and pipe_offer_timeout/3
 * allow sending a message to a bounded queue. The predicates will
 * block, fail or timeout when the bounded queue is full. The
 * predicate pipe_put/2 can also be used for unbounded queues and
 * will never block. The predicates pipe_take/3, pipe_poll/2 and
 * pipe_poll_timeout/3 allow getting a message from a pipe. The
 * predicates will block, fail or timeout when the pipe is empty.
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

:- package(library(jekpro/frequent/misc)).
:- use_package(foreign(jekpro/frequent/misc)).
:- use_package(foreign(matula/util/misc)).
:- use_package(foreign(jekpro/tools/call)).
:- use_package(foreign(jekpro/tools/term)).

:- module(pipe, []).

/**
 * pipe_new(Q):
 * The predicate succeeds for a new unbonded queue Q.
 */
% pipe_new(-Queue)
:- public pipe_new/1.
:- foreign_constructor(pipe_new/1, 'Unbounded', new).

/**
 * pipe_new(M, Q):
 * The predicate succeeds for a new bounded queue Q with maximum size M.
 */
% pipe_new(+Integer, -Queue)
:- public pipe_new/2.
:- foreign_constructor(pipe_new/2, 'Bounded', new(int)).

/**
 * pipe_put(P, O):
 * The predicate succeeds for sending a copy of the term O
 * to the pipe P.
 */
% pipe_put(+Pipe, +Term)
:- public pipe_put/2.
:- foreign(pipe_put/2, 'ForeignPipe',
      sysPipePut('Interpreter', 'InterfacePipe', 'AbstractTerm')).

/**
 * pipe_offer(P, O):
 * The predicate succeeds for sending a copy of the term O to the
 * bounded queue P. Otherwise the predicate fails.
 */
% pipe_offer(+Pipe, +Term)
:- public pipe_offer/2.
:- foreign(pipe_offer/2, 'ForeignPipe',
      sysPipeOffer('Interpreter', 'InterfacePipe', 'AbstractTerm')).

/**
 * pipe_offer_timeout(P, O, T):
 * The predicate succeeds for sending a copy of the term O to the
 * bounded queue P in the timeout T. Otherwise the predicate fails.
 */
% pipe_offer_timeout(+Pipe, +Term, +Integer)
:- public pipe_offer_timeout/3.
:- foreign(pipe_offer_timeout/3, 'ForeignPipe',
      sysPipeOffer('Interpreter', 'InterfacePipe', 'AbstractTerm', int)).

/**
 * pipe_take(P, O):
 * The predicate succeeds for getting a term O form the pipe P.
 */
% pipe_take(+Pipe, -Term)
:- public pipe_take/2.
:- virtual pipe_take/2.
:- foreign(pipe_take/2, 'InterfacePipe', take).

/**
 * pipe_poll(P, O):
 * The predicate succeeds for getting a term O form the pipe P.
 * Otherwise the predicate fails.
 */
% pipe_poll(+Pipe, -Term)
:- public pipe_poll/2.
:- virtual pipe_poll/2.
:- foreign(pipe_poll/2, 'InterfacePipe', poll).

/**
 * pipe_poll_timeout(P, T, O):
 * The predicate succeeds for getting a term O form the pipe P
 * in the timeout T. Otherwise the predicate fails.
 */
% pipe_poll_timeout(+Pipe, +Integer, -Term)
:- public pipe_poll_timeout/3.
:- foreign(pipe_poll_timeout/3, 'ForeignPipe',
      sysPipePoll('InterfacePipe', int)).
