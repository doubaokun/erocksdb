#include <cstdio>
#include <string>

#include "rocksdb/db.h"
#include "rocksdb/slice.h"
#include "rocksdb/options.h"

extern "C" {

#include "erl_nif.h"

}

using namespace rocksdb;

DB* db;

#define MAX_DB_PATH_NAME_LENGTH 2048

ERL_NIF_TERM open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(db != NULL) {
    delete db;
  }
  
  char path[MAX_DB_PATH_NAME_LENGTH];
  memset(path, 0, MAX_DB_PATH_NAME_LENGTH);
  if(!enif_get_string(env, argv[0], path, MAX_DB_PATH_NAME_LENGTH + 1, ERL_NIF_LATIN1)) {
      return enif_make_badarg(env);
  }

  std::string str(path);

  Options options;
  // Optimize RocksDB. This is the easiest way to get RocksDB to perform well
  options.IncreaseParallelism();
  options.OptimizeLevelStyleCompaction();
  // create the DB if it's not already present
  options.create_if_missing = true;

  // open DB
  Status s = DB::Open(options, path, &db);
    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(NULL == db) {
    return enif_make_badarg(env);
  }
  char key[MAX_DB_PATH_NAME_LENGTH];
  memset(key, 0, MAX_DB_PATH_NAME_LENGTH);
  if(!enif_get_string(env, argv[0], key, MAX_DB_PATH_NAME_LENGTH + 1, ERL_NIF_LATIN1)) {
      return enif_make_badarg(env);
  }
  char value[MAX_DB_PATH_NAME_LENGTH];
  memset(value, 0, MAX_DB_PATH_NAME_LENGTH);
  if(!enif_get_string(env, argv[1], value, MAX_DB_PATH_NAME_LENGTH + 1, ERL_NIF_LATIN1)) {
      return enif_make_badarg(env);
  }
    Status s = db->Put(WriteOptions(), key, value);
    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(NULL == db) {
    return enif_make_badarg(env);
  }
  char key[MAX_DB_PATH_NAME_LENGTH];
  memset(key, 0, MAX_DB_PATH_NAME_LENGTH);
  if(!enif_get_string(env, argv[0], key, MAX_DB_PATH_NAME_LENGTH + 1, ERL_NIF_LATIN1)) {
      return enif_make_badarg(env);
  }
  std::string value;
  // get value
  Status s = db->Get(ReadOptions(), key, &value);

  char* r = new char[value.length()+1];
  strcpy(r, value.c_str());

//   assert(s.ok());
    return enif_make_string(env, r, ERL_NIF_LATIN1);
}

ERL_NIF_TERM rocks_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(NULL == db) {
    return enif_make_badarg(env);
  }
  char key[MAX_DB_PATH_NAME_LENGTH];
  memset(key, 0, MAX_DB_PATH_NAME_LENGTH);
  if(!enif_get_string(env, argv[0], key, MAX_DB_PATH_NAME_LENGTH + 1, ERL_NIF_LATIN1)) {
      return enif_make_badarg(env);
  }
  // get value
  Status s = db->Delete(WriteOptions(), key);
    return enif_make_atom(env, "ok");
}


ERL_NIF_TERM close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    delete db;
    db = NULL;
    return enif_make_atom(env, "ok");
}

extern "C" {

static ErlNifFunc nif_funcs[] =
{
    {"close", 0, close},
    {"open", 1, open},
    {"put", 2, put},
    {"get", 1, get},
    {"delete", 1, rocks_delete}
};

ERL_NIF_INIT(rocksdb,nif_funcs,NULL,NULL,NULL,NULL)

}








