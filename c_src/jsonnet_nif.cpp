extern "C" {
    #include "erl_nif.h"
    #include <stdio.h>
    #include <string.h>
    #include "libjsonnet.h"
    
    ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom);
    ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg);
}

#define MAXBUFLEN 1024

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static ERL_NIF_TERM
evaluateFile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char buffer[MAXBUFLEN];
    int error;
    char *output;
    struct JsonnetVm *vm;    
	
    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    (void)memset(&buffer, '\0', sizeof(buffer));

    if (enif_get_string(env, argv[0], buffer, sizeof(buffer), ERL_NIF_LATIN1) < 1)
    {	    
        return enif_make_badarg(env);
    }

    vm = jsonnet_make();
    output = jsonnet_evaluate_file(vm, buffer, &error);
    if (error) {
        jsonnet_realloc(vm, output, 0);
        jsonnet_destroy(vm);

	return mk_error(env, "failed");
    }

    return enif_make_string(env, output, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM
evaluateSnippet(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char buffer[MAXBUFLEN];
    int error;
    char *output;
    struct JsonnetVm *vm;    
	
    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    (void)memset(&buffer, '\0', sizeof(buffer));

    if (enif_get_string(env, argv[0], buffer, sizeof(buffer), ERL_NIF_LATIN1) < 1)
    {	    
        return enif_make_badarg(env);
    }

    vm = jsonnet_make();
    output = jsonnet_evaluate_snippet(vm, "snippet", buffer, &error);
    if (error) {
        jsonnet_realloc(vm, output, 0);
        jsonnet_destroy(vm);

	return mk_error(env, "failed");
    }

    return enif_make_string(env, output, ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] = {
    {"evaluateFile", 1, evaluateFile, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"evaluateSnippet", 1, evaluateSnippet, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

extern "C" {
    ERL_NIF_INIT(jsonnet, nif_funcs, NULL, NULL, NULL, NULL)
}
