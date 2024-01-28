# Spinlock

[![CI build status](https://github.com/farhadi/spinlock/workflows/CI/badge.svg)](https://github.com/farhadi/spinlock/actions?query=workflow%3ACI)
[![codecov](https://codecov.io/gh/farhadi/spinlock/branch/main/graph/badge.svg)](https://codecov.io/gh/farhadi/spinlock)
[![Hex docs](http://img.shields.io/badge/hex.pm-docs-green.svg?style=flat)](https://hexdocs.pm/spinlock)
[![Hex Version](http://img.shields.io/hexpm/v/spinlock.svg?style=flat)](https://hex.pm/packages/spinlock)
[![License](http://img.shields.io/hexpm/l/spinlock.svg?style=flat)](https://github.com/farhadi/spinlock/blob/master/LICENSE)

Spinlock implemented using [atomics](https://erlang.org/doc/man/atomics.html) for Erlang and Elixir.

## Introduction

In software engineering, locks and mutexes are usually used to prevent race conditions and
data corruption that can occur when multiple threads try to access shared data concurrently.
However, in Erlang, we rarely need such locking mechanisms. This is due to the fact that
concurrency in Erlang is based on actor model, and each process has its own separate heap
and does not share memory with other processes. This model inherently avoids many of the
concurrency issues associated with shared memory and locking mechanisms.

In erlang shared resources are typically managed through serialization of access requests
using a single gen_server process to handle all requests for a resource. It's also possible
to replicate mutex behavior using a gen_server process (see [sleeplocks](https://github.com/whitfin/sleeplocks)
as an example of a lock implementation using gen_servers). Generally, the performance of
such locks are good enough for most use cases. However, in some critical situations where
submicrosecond latency is a requirement, relying on a gen_server based lock may not be the
best choice. In such critical cases Spinlocks are used.

Spinlocks are a low-level synchronization mechanism typically used in systems programming,
such as in operating system kernels or in scenarios where you need very fast, lightweight locks.
They are called "spinlocks" because they cause a thread attempting to acquire the lock to "spin"
in a loop while repeatedly checking if the lock is available. Using this technique we can
achieve extremely fast lock acquisitions at the expense of CPU cycles spent in busy-waiting.

## Implementation

This is a Spinlock implementation using atomics for Erlang and Elixir. It is slightly different
from other typical spinlock implementations where the lock state alternates between locked and
unlocked via an atomic operation. Here, a lock consists of two atomic integers: one tracks
the number of requests to acquire the lock, and the other holds the number of releases.

This approach offers several advantages over conventional spinlock implementations. Firstly,
lock acquisition is ordered, meaning locks are acquired in the same sequence they are requested.
This feature ensures consistent and predictable latency. Secondly, it tracks the number of
processes concurrently busy-waiting to acquire a lock. By allowing only the next process
to busy-wait in a tight loop and interrupting spinning for the others with `erlang:yield/0`,
this method reduces spinlock starvation and offers other processes more opportunities to run.

In this implementation, attempting to acquire a lock in a locked state will forcibly release
the lock after a configurable number of attempts, under the assumption that the process owning
the lock has already terminated.

## Use Cases

Use it with caution due to its busy-waiting nature. If the lock is held for extended periods,
it could waste CPU cycles, adversely affecting overall system performance.

The rule of thumb to decide whether to use it or not is: "Employ it only if the operation
requiring the lock is a submicrosecond task, and if there's more than a 99% chance that the
lock can be acquired without busy-waiting."

A typical use case in Erlang is implementing transactions for data structures built on top of
atomic arrays (e.g. [Cuckoo Filter](http://github.com/farhadi/cuckoo_filter)).
