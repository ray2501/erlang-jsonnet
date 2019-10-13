extern "C" {
    #include "erl_nif.h"
    #include <stdio.h>
    #include <string.h>
    #include "libjsonnet.h"
    
    ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom);
    ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg);
}

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
    char *buffer = NULL;
    int error = 0;
    char *output = NULL;
    struct JsonnetVm *vm = NULL;
    unsigned int length = 0;
	
    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_list_length(env, argv[0],&length)) {
        return enif_make_badarg(env);
    }

    buffer = (char *) malloc(sizeof(char) * length + 1);
    if(!buffer) {
	return mk_error(env, "no_memory");
    }

    (void)memset(buffer, '\0', length + 1);

    if (enif_get_string(env, argv[0], buffer, length + 1, ERL_NIF_LATIN1) < 1)
    {
        if(buffer) free(buffer);
        return enif_make_badarg(env);
    }

    vm = jsonnet_make();
    output = jsonnet_evaluate_file(vm, buffer, &error);
    if (error) {
        jsonnet_realloc(vm, output, 0);
        jsonnet_destroy(vm);

        if(buffer) free(buffer);
        return mk_error(env, "failed");
    }

    if(buffer) free(buffer);
    return enif_make_string(env, output, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM
evaluateSnippet(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char *buffer = NULL;
    int error = 0;
    char *output = NULL;
    struct JsonnetVm *vm = NULL;
    unsigned int length = 0;
	
    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_list_length(env, argv[0],&length)) {
        return enif_make_badarg(env);
    }

    buffer = (char *) malloc(sizeof(char) * length + 1);
    if(!buffer) {
        return mk_error(env, "no_memory");
    }

    (void)memset(buffer, '\0', length + 1);

    if (enif_get_string(env, argv[0], buffer, length + 1, ERL_NIF_LATIN1) < 1)
    {	    
        if(buffer) free(buffer);
        return enif_make_badarg(env);
    }

    vm = jsonnet_make();
    output = jsonnet_evaluate_snippet(vm, "snippet", buffer, &error);
    if (error) {
        jsonnet_realloc(vm, output, 0);
        jsonnet_destroy(vm);

        if(buffer) free(buffer);
	return mk_error(env, "failed");
    }

    if(buffer) free(buffer);
    return enif_make_string(env, output, ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] = {
    {"evaluateFile", 1, evaluateFile, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"evaluateSnippet", 1, evaluateSnippet, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

extern "C" {
    ERL_NIF_INIT(jsonnet, nif_funcs, NULL, NULL, NULL, NULL)
}
