-module(spinlock_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").

-record(spinlock, {
    ref :: atomics:atomics_ref(),
    max_retry :: pos_integer()
}).

wait_for_status(Lock, ExpectedStatus, 0) ->
    ?assertEqual(ExpectedStatus, spinlock:status(Lock));
wait_for_status(Lock, ExpectedStatus, Retry) ->
    case spinlock:status(Lock) of
        ExpectedStatus -> ok;
        _ -> wait_for_status(Lock, ExpectedStatus, Retry - 1)
    end.

receive_msg() ->
    receive
        Message -> Message
    after 1000 ->
        error(timeout)
    end.

all() ->
    [
        new,
        lock_status,
        waiting_status,
        concurrent_acquire,
        acquire_timeout,
        invalid_lock_id,
        invalid_lock_state,
        transaction
    ].

new(_Config) ->
    Lock = spinlock:new(),
    ?assertMatch(#spinlock{max_retry = 100_000}, Lock),
    Lock2 = spinlock:new([{max_retry, 1000}]),
    ?assertMatch(#spinlock{max_retry = 1000}, Lock2),
    Ref = atomics:new(10, []),
    Lock3 = spinlock:new([{atomics_ref, Ref}]),
    ?assertMatch(#spinlock{ref = Ref}, Lock3).

lock_status(_Config) ->
    Lock = spinlock:new(),
    ?assertEqual(#{is_locked => false, released => 0, waiting => 0}, spinlock:status(Lock)),
    LockId = spinlock:acquire(Lock),
    ?assertEqual(1, LockId),
    ?assertEqual(#{is_locked => true, released => 0, waiting => 0}, spinlock:status(Lock)),
    ?assertEqual(ok, spinlock:release(Lock, LockId)),
    ?assertEqual(#{is_locked => false, released => 1, waiting => 0}, spinlock:status(Lock)),
    ?assertEqual({error, already_released}, spinlock:release(Lock, LockId)),
    ?assertEqual(#{is_locked => false, released => 1, waiting => 0}, spinlock:status(Lock)).

waiting_status(_Config) ->
    Lock = spinlock:new(),
    LockId = spinlock:acquire(Lock),
    ?assertEqual(1, LockId),
    spawn(fun() ->
        LockId2 = spinlock:acquire(Lock),
        ?assertEqual(2, LockId2),
        ?assertEqual(ok, spinlock:release(Lock, LockId2))
    end),
    wait_for_status(Lock, #{is_locked => true, released => 0, waiting => 1}, 1000),
    ?assertEqual(ok, spinlock:release(Lock, LockId)),
    wait_for_status(Lock, #{is_locked => false, released => 2, waiting => 0}, 1000).

concurrent_acquire(_Config) ->
    Lock = spinlock:new(),
    Pid = self(),
    Counter = atomics:new(1, []),
    [
        spawn(fun() ->
            LockId = spinlock:acquire(Lock),
            I = atomics:get(Counter, 1),
            atomics:compare_exchange(Counter, 1, I, I + 1),
            ?assertEqual(ok, spinlock:release(Lock, LockId)),
            Pid ! ok
        end)
     || _ <- lists:seq(1, 1000)
    ],
    [receive_msg() || _ <- lists:seq(1, 1000)],
    ?assertEqual(1000, atomics:get(Counter, 1)),
    ?assertEqual(#{is_locked => false, released => 1000, waiting => 0}, spinlock:status(Lock)).

acquire_timeout(_Config) ->
    Lock = spinlock:new(),
    ?assertEqual(#{is_locked => false, released => 0, waiting => 0}, spinlock:status(Lock)),
    LockId = spinlock:acquire(Lock),
    LockId2 = spinlock:acquire(Lock),
    ?assertEqual(2, LockId2),
    ?assertEqual(#{is_locked => true, released => 1, waiting => 0}, spinlock:status(Lock)),
    ?assertEqual({error, already_released}, spinlock:release(Lock, LockId)),
    ?assertEqual(ok, spinlock:release(Lock, LockId2)),
    ?assertEqual(#{is_locked => false, released => 2, waiting => 0}, spinlock:status(Lock)).

invalid_lock_id(_Config) ->
    Lock = spinlock:new(),
    ?assertEqual({error, invalid_lock_id}, spinlock:release(Lock, 2)).

invalid_lock_state(_Config) ->
    Lock = #spinlock{ref = Ref} = spinlock:new(),
    atomics:put(Ref, 2, 10),
    ?assertError(invalid_lock_state, spinlock:acquire(Lock)).

transaction(_Config) ->
    Lock = spinlock:new(),
    spinlock:transaction(Lock, fun() ->
        ?assertEqual(#{is_locked => true, released => 0, waiting => 0}, spinlock:status(Lock))
    end),
    ?assertEqual(#{is_locked => false, released => 1, waiting => 0}, spinlock:status(Lock)),
    ?assertError(failed, spinlock:transaction(Lock, fun() -> error(failed) end)),
    ?assertEqual(#{is_locked => false, released => 2, waiting => 0}, spinlock:status(Lock)).
