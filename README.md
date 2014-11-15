ERocksDB
=============

Erlang nif experiment.


### Play it

	rebar clean compile -v
	./start.sh
	erocksdb:put("k1", "v1").
	erocksdb:get("k1").
	erocksdb:del("k1").
	erocksdb:get("k1").
