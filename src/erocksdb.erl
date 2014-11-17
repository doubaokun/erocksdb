-module(erocksdb).

-export([start/0, stop/0]).
-export([put/2, get/1, del/1, test/0]).

-define(SERVER, erocksdb_srv).

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

start() ->
  ok = application:start(?MODULE).

stop() ->
  ok = application:stop(?MODULE).

put(Key, Value) ->
  gen_server:call(?SERVER, {put, Key, Value}).

get(Key) ->
  gen_server:call(?SERVER, {get, Key}).

del(Key) ->
  gen_server:call(?SERVER, {del, Key}).


test() ->
	start(),
	register(erocksdb_test, self()),
	for(1, 100, fun() -> spawn( fun() -> writen(10000) end) end ),
	loop(100).

loop(0) ->
	io:format("All done~n");
loop(N) ->
	receive
		{From, done} ->
			io:format("~w done~n", [100 - N + 1]),
			loop(N-1)
	end.

writen(0) ->
	erocksdb_test ! {self(), done};
writen(N) ->
	erocksdb:put("a" ++ integer_to_list(N), integer_to_list(N)),
	writen(N-1).

for( N, N, F )
        -> [F()];
for( I, N, F ) ->
        io:format("process created: ~w~n", [I] ),
        [F() | for(I+1, N, F)].
