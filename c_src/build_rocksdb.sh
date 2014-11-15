if [ ! -d c_src/rocksdb ]; then
git clone https://github.com/facebook/rocksdb c_src/rocksdb && cd c_src/rocksdb && make static_lib && cd ../../
fi