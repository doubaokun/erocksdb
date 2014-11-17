-module(rocksdb).

-export([init/0, close/0, open/1, put/2, get/1, delete/1, test/0]).

test() ->
	rocksdb:init(),
	rocksdb:open("./db1"),
	erlang:bump_reductions(2000),
	rocksdb:put("a", "b"),
	rocksdb:get("a"),
	register(erocksdb_test, self()),
	for(1, 100, fun() -> spawn( fun() -> writen(10000) end) end ),
	loop(100),
	rocksdb:close().

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
	erlang:bump_reductions(2000),
	rocksdb:put("a" ++ integer_to_list(N), integer_to_list(N)),
	writen(N-1).

for( N, N, F )
        -> [F()];
for( I, N, F ) ->
        io:format("process created: ~w~n", [I] ),
        [F() | for(I+1, N, F)].


init() ->
      erlang:load_nif("./priv/rocksdb", 0).

close() ->
      "NIF library not loaded".

open(_Key) ->
      "NIF library not loaded".

put(_Key, _Value) ->
      "NIF library not loaded".

get(_Key) ->
      "NIF library not loaded".

delete(_Key) ->
      "NIF library not loaded".
