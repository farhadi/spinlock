-module(spinlock).

-export([
    new/0, new/1,
    acquire/1,
    release/2,
    transaction/2,
    status/1
]).

-record(spinlock, {
    ref :: atomics:atomics_ref(),
    max_retry :: pos_integer()
}).

-type spinlock() :: #spinlock{}.
-type option() :: {max_retry, pos_integer()} | {atomics_ref, atomics:atomics_ref()}.
-type lock_id() :: pos_integer().

-export_type([spinlock/0]).

-spec new() -> #spinlock{}.
new() ->
    new([]).

-spec new([option()]) -> #spinlock{}.
new(Options) ->
    MaxRetry = proplists:get_value(max_retry, Options, 100_000),
    is_integer(MaxRetry) andalso MaxRetry > 0 orelse error(badarg),
    Ref = case proplists:get_value(atomics_ref, Options) of
        undefined -> atomics:new(2, [{signed, false}]);
        AtomicsRef -> AtomicsRef
    end,
    #spinlock{
        ref = Ref,
        max_retry = MaxRetry
    }.

-spec acquire(#spinlock{}) -> lock_id().
acquire(Lock = #spinlock{ref = Ref}) ->
    LockId = atomics:add_get(Ref, 1, 1),
    spin(Lock, LockId, undefined, 0).

-spec release(#spinlock{}, lock_id()) -> ok | {error, already_released | invalid_lock_id}.
release(#spinlock{ref = Ref}, LockId) ->
    case atomics:compare_exchange(Ref, 2, LockId - 1, LockId) of
        ok -> ok;
        Id when Id >= LockId -> {error, already_released};
        _ -> {error, invalid_lock_id}
    end.

-spec transaction(#spinlock{}, fun(() -> any())) -> any().
transaction(Lock, Fun) ->
    LockId = acquire(Lock),
    try Fun() of
        Res -> Res
    after
        release(Lock, LockId)
    end.

-spec status(#spinlock{}) ->
    #{released => non_neg_integer(), is_locked => boolean(), waiting => non_neg_integer()}.
status(#spinlock{ref = Ref}) ->
    Released = atomics:get(Ref, 2),
    Total = atomics:get(Ref, 1),
    #{
        released => Released,
        is_locked => Total > Released,
        waiting => max(Total - Released - 1, 0)
    }.

spin(Lock = #spinlock{ref = Ref, max_retry = MaxRetry}, LockId, Released, MaxRetry) ->
    atomics:compare_exchange(Ref, 2, Released, Released + 1),
    spin(Lock, LockId, Released, 0);
spin(Lock = #spinlock{ref = Ref}, LockId, LastReleased, Retry) ->
    case atomics:get(Ref, 2) of
        LastReleased ->
            LastReleased < LockId - 2 andalso erlang:yield(),
            spin(Lock, LockId, LastReleased, Retry + 1);
        Released when Released >= LockId - 1 ->
            LockId;
        Released ->
            Released < LockId - 2 andalso erlang:yield(),
            spin(Lock, LockId, Released, 0)
    end.
