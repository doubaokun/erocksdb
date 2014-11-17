-module(erocksdb_srv).

-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([stop/0, terminate/2]).
-export([put/2, get/1, del/1]).

-define(SERVER, ?MODULE).

put(Key, Value) ->
  gen_server:call(?SERVER, {put, Key, Value}).

get(Key) ->
  gen_server:call(?SERVER, {get, Key}).

del(Key) ->
  gen_server:call(?SERVER, {del, Key}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, stop).

init(State) ->
  rocksdb:init(),
  rocksdb:open("./db"),
  erlang:bump_reductions(2000),
  {ok, init}.

handle_call({get, Key}, _From, State) ->
  Value = rocksdb:get(Key),
  %erlang:bump_reductions(2000),
  {reply, Value, get};
handle_call({put, Key, Value}, _From, State) ->
  rocksdb:put(Key, Value),
  %erlang:bump_reductions(2000),
  {reply, ok, State};
handle_call({del, Key}, _From, State) ->
  rocksdb:del(Key),
  %erlang:bump_reductions(2000),
  {reply, ok, State}.

handle_cast(Req, State) ->
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Req, State) ->
  {noreply, State}.

handle_info(Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok;
terminate(shutdown, _State) ->
  ok;
terminate({shutdown, _Reason}, _State) ->
  ok;
terminate(_Reason, _State) ->
  ok.
