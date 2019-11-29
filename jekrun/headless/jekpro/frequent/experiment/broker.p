/**
 * This module provides a Prolog implementation of the actor model. The
 * realization leans towards Erlang with the same receive, send and spawn
 * semantics. Currently send and spawn are one way so that a client is
 * not notified whether the primitive was performed or not.
 *
 * Currently we only support unreliable UDP transport with up
 * to 4096 bytes. We might add further protocols in the future.
 * Especially a combination with our module "http" and its web sockets
 * is planned which would even allow communication with web browser.
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

:- package(library(jekpro/frequent/experiment)).

:- module(broker, []).

:- use_module(library(misc/socket)).
:- use_module(library(misc/pipe)).
:- use_module(library(misc/lock)).
:- use_module(library(system/thread)).
:- use_module(library(system/domain)).
:- use_module(library(experiment/ref)).
:- use_module(library(basic/random)).

:- public infix(when).
:- op(1105, xfy, when).

:- public when/2.
:- meta_function when(?, 0).
when(_, _) :- throw(error(existence_error(body, when/2), _)).

% server_endpoint(+Atom, +Endpoint, +Counter)
:- private server_endpoint/3.
:- group_local server_endpoint/3.

% actor_queue(+Atom, +Pipe)
:- private actor_queue/2.
:- group_local actor_queue/2.

% actor_deferred(+Atom, +Term)
:- private actor_deferred/2.
:- group_local actor_deferred/2.

/***********************************************************/
/* Message Broker                                          */
/***********************************************************/

/**
 * broker_start(A, N):
 * The predicate succeeds in starting a broker identified by the
 * authority A and numbering actors starting with N.
 */
% broker_start(+Atom, +Integer)
:- public broker_start/2.
broker_start(Authority, Node) :-
   unslotted_new(Lock),
   lock_acquire(Lock),
   thread_new(broker_run(Lock, Authority, Node), Thread),
   thread_start(Thread),
   lock_acquire(Lock),
   spawn_setup.

/**
 * broker_stop:
 * The predicate succeeds in stopping the current broker.
 */
% broker_stop
:- public broker_stop/0.
broker_stop :-
   server_endpoint(Authority, Endpoint, _),
   term_atom(end, Atom),
   atom_block(Atom, Block, [encoding('utf-8')]),
   make_authority(_, Host, Port, Authority),
   endpoint_send(Endpoint, Block, Host, Port),
   spawn_cleanup.

% broker_run(+Lock, +Atom, +Integer)
:- private broker_run/3.
broker_run(Lock, Authority, Node) :-
   setup_call_cleanup(
      (broker_setup(Authority, Node), lock_release(Lock)),
      broker_server(Authority),
      broker_cleanup(Authority)).

% broker_setup(+Atom, +Integer)
:- private broker_setup/2.
broker_setup(Authority, Node) :-
   make_authority(_, Host, Port, Authority),
   endpoint_new(Host, Port, Endpoint),
   counter_new(Node, Counter),
   assertz(server_endpoint(Authority, Endpoint, Counter)).

% broker_cleanup(+Atom)
:- private broker_cleanup/1.
broker_cleanup(Authority) :-
   retract(server_endpoint(Authority, Endpoint, _)),
   close(Endpoint).

% broker_server(+Atom)
:- private broker_server/1.
broker_server(Authority) :-
   server_endpoint(Authority, Endpoint, _),
   repeat,
   endpoint_receive(Endpoint, Block),
   atom_block(Atom, Block, [encoding('utf-8')]),
   term_atom(Term, Atom),
   (  Term = end -> !
   ;  Term = send(Name, Message)
   -> actor_queue(Name, Queue),
      pipe_put(Queue, Message),
      fail
   ;  Term = spawn(Name, Goal)
   -> thread_new(spawn_safe(Goal), Thread),
      set_thread_flag(Thread, sys_thread_name, Name),
      thread_start(Thread),
      fail; fail).

/***********************************************************/
/* Actor Primitives                                        */
/***********************************************************/

/**
 * spawn(A, G, P):
 * The predicate succeeds in P with the actor path of a new actor on
 * the authority A running the goal G.
 */
% spawn(+Atom, +Goal, -Pid)
:- public spawn/3.
:- meta_predicate spawn(?, 0, ?).
spawn(Authority, Goal, pid(Authority, Name)) :-         /* local run */
   server_endpoint(Authority, _, Counter), !,
   spawn_name(Counter, Name),
   thread_new(spawn_safe(Goal), Thread),
   set_thread_flag(Thread, sys_thread_name, Name),
   thread_start(Thread).
spawn(Authority, Goal, pid(Authority, Name)) :-         /* remote run */
   server_endpoint(_, Endpoint, Counter),
   spawn_name(Counter, Name),
   term_atom(spawn(Name, Goal), Atom),
   atom_block(Atom, Block, [encoding('utf-8')]),
   make_authority(_, Host, Port, Authority),
   endpoint_send(Endpoint, Block, Host, Port).

% spawn_name(+Counter, -Atom)
:- private spawn_name/2.
spawn_name(Counter, Name) :-
   counter_next(Counter, Number),
   atom_number(Atom, Number),
   atom_concat('Actor-', Atom, Name).

% spawn_safe(+Goal)
:- private spawn_safe/1.
:- meta_predicate spawn_safe(0).
spawn_safe(Goal) :-
   setup_call_cleanup(
      spawn_setup,
      Goal,
      spawn_cleanup).

% spawn_setup
:- private spawn_setup/0.
spawn_setup :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   pipe_new(Queue),
   assertz(actor_queue(Name, Queue)).

% spawn_cleanup
:- private spawn_cleanup/0.
spawn_cleanup :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   retract(actor_queue(Name, _)),
   retractall(actor_deferred(Name, _)).

/**
 * self(P):
 * The predicate succeeds in P with actor path of the current actor.
 */
% self(-Pid)
:- public self/1.
self(pid(Authority, Name)) :-
   server_endpoint(Authority, _, _),
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name).

/* send(P, M):
 * The predicate succeeds in sending the message M to the
 * actor identified by the actor path P. Sending is non-blocking
 * and succeeds immediately.
 */
% send(+Pid, +Term)
:- public send/2.
send(pid(Authority, Name), Message) :-                  /* local send */
   server_endpoint(Authority, _, _), !,
   actor_queue(Name, Queue),
   pipe_put(Queue, Message).
send(pid(Authority, Name), Message) :-                  /* remote send */
   server_endpoint(_, Endpoint, _),
   term_atom(send(Name, Message), Atom),
   atom_block(Atom, Block, [encoding('utf-8')]),
   make_authority(_, Host, Port, Authority),
   endpoint_send(Endpoint, Block, Host, Port).

/**
 * receive(L, M):
 * receive(L, M, T):
 * The predicate succeeds in M with the message that the current
 * actor received and that matches the pattern and guard disjunction
 * L. Receiving is blocking and succeeds only when a matching
 * message has arrived. The ternary predicate takes an additional
 * timeout parameter.
 */
% receive(+Assoc, -Term)
:- public receive/2.
receive(PatternGuards, Selected) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   clause_ref(actor_deferred(Name, Selected), true, Ref),
   receive_member(SelectedGuard, PatternGuards),
   receive_check(SelectedGuard, Selected), !,
   erase_ref(Ref).
receive(PatternGuards, Selected) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   actor_queue(Name, Queue),
   repeat,
   pipe_take(Queue, Selected),
   (  receive_member(SelectedGuard, PatternGuards),
      receive_check(SelectedGuard, Selected), !
   ;  assertz(actor_deferred(Name, Selected)),
      fail).

% receive(+Assoc, -Term, +Integer)
:- public receive/3.
receive(PatternGuards, Selected, _) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   clause_ref(actor_deferred(Name, Selected), true, Ref),
   receive_member(SelectedGuard, PatternGuards),
   receive_check(SelectedGuard, Selected), !,
   erase_ref(Ref).
receive(PatternGuards, Selected, Timeout) :-
   thread_current(Thread),
   current_thread_flag(Thread, sys_thread_name, Name),
   actor_queue(Name, Queue),
   repeat,
   (pipe_poll(Queue, Timeout, Selected)
-> (  receive_member(SelectedGuard, PatternGuards),
      receive_check(SelectedGuard, Selected), !
   ;  assertz(actor_deferred(Name, Selected)),
      fail); !, fail).

% receive_member(-Term, +Assoc)
:- private receive_member/2.
receive_member(Term, Assoc) :-
   nonvar(Assoc),
   Assoc = (Term; _).
receive_member(Term, Assoc) :-
   nonvar(Assoc),
   Assoc = (_; Assoc2), !,
   receive_member(Term, Assoc2).
receive_member(Term, Term).

% receive_check(+Pair, -Term)
:- private receive_check/2.
receive_check(SelectedGuard, Selected) :-
   nonvar(SelectedGuard),
   SelectedGuard = (Selected when Guard), !,
   Guard.
receive_check(Selected, Selected).
