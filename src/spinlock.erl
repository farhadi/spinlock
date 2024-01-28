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

%% @equiv new([])
-spec new() -> #spinlock{}.
new() ->
    new([]).

%% @doc Creates a new spinlock instance with the given options.
%%
%% Possible options are:
%% <ul>
%% <li>`{max_retry, MaxRetry}'
%% <p>The lock is forecibly released after `MaxRetry' number of attempts.</p>
%% </li>
%% <li>`{atomics_ref, AtomicsRef}'
%% <p>Uses the first two index of the given atomics array to store the state of the lock.
%% If you want to use spinlock to implement transactions for an atomics array, you can use this
%% option to avoid creating an extra atomics array.</p>
%% </li>
%% </ul>
-spec new(Options :: [option()]) -> #spinlock{}.
new(Options) ->
    MaxRetry = proplists:get_value(max_retry, Options, 100_000),
    is_integer(MaxRetry) andalso MaxRetry > 0 orelse error(badarg),
    Ref =
        case proplists:get_value(atomics_ref, Options) of
            undefined -> atomics:new(2, [{signed, false}]);
            AtomicsRef -> AtomicsRef
        end,
    #spinlock{
        ref = Ref,
        max_retry = MaxRetry
    }.

%% @doc Acquires a lock for the current process.
%%
%% This will busy-wait until a lock can be acquired, or a maximum configured number
%% of attemps is reached. Returned `lock_id' is used to release the lock later.
-spec acquire(Lock :: #spinlock{}) -> lock_id().
acquire(Lock = #spinlock{ref = Ref}) ->
    LockId = atomics:add_get(Ref, 1, 1),
    spin(Lock, LockId - 1, undefined, 0).

%% @doc Releases an already acquired lock.
%%
%% This will release the lock for the given LockId.
%% Returns `ok' if the lock is successfully released. Otherwise returns
%% `{error, already_released}' if the lock is already released
%% or `{error, invalid_lock_state}' if the given LockId has never been acquired.
-spec release(Lock :: #spinlock{}, LockId :: lock_id()) ->
    ok | {error, already_released | invalid_lock_id}.
release(#spinlock{ref = Ref}, LockId) ->
    case atomics:compare_exchange(Ref, 2, LockId - 1, LockId) of
        ok -> ok;
        Id when Id >= LockId -> {error, already_released};
        _ -> {error, invalid_lock_id}
    end.

%% @doc Executes the given function in a transaction.
%%
%% Acquires the lock, executes the given function, and releases the lock when the function has returend.
%% The lock is released even if the function fails with an exception.
-spec transaction(Lock :: #spinlock{}, Fun :: fun(() -> any())) -> any().
transaction(Lock, Fun) ->
    LockId = acquire(Lock),
    try Fun() of
        Res -> Res
    after
        release(Lock, LockId)
    end.

%% @doc Returns the status of the given lock.
%%
%% You can use this function for debugging purposes, to check the current status of the lock,
%% and to see how many process are waiting to acquire the lock.
-spec status(Lock :: #spinlock{}) ->
    #{released => non_neg_integer(), is_locked => boolean(), waiting => non_neg_integer()}.
status(#spinlock{ref = Ref}) ->
    Released = atomics:get(Ref, 2),
    Total = atomics:get(Ref, 1),
    #{
        is_locked => Total > Released,
        released => Released,
        waiting => max(Total - Released - 1, 0)
    }.

%%%-------------------------------------------------------------------
%% Internal functions
%%%-------------------------------------------------------------------

spin(Lock = #spinlock{ref = Ref, max_retry = MaxRetry}, ExpectedId, ReleasedId, MaxRetry) ->
    atomics:compare_exchange(Ref, 2, ReleasedId, ReleasedId + 1),
    spin(Lock, ExpectedId, ReleasedId, 0);
spin(Lock = #spinlock{ref = Ref}, ExpectedId, LastReleasedId, Retry) ->
    case atomics:get(Ref, 2) of
        LastReleasedId ->
            LastReleasedId < ExpectedId - 1 andalso erlang:yield(),
            spin(Lock, ExpectedId, LastReleasedId, Retry + 1);
        ExpectedId ->
            ExpectedId + 1;
        ReleasedId when ReleasedId < ExpectedId ->
            spin(Lock, ExpectedId, ReleasedId, 0);
        _ ->
            error(invalid_lock_state)
    end.
