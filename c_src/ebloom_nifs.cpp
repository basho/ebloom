// -------------------------------------------------------------------
// 
// Copyright (c) 2010 Basho Technologies, Inc.  All Rights Reserved.
// 
// This file is provided to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file
// except in compliance with the License.  You may obtain
// a copy of the License at
// 
//    http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
// 
// -------------------------------------------------------------------

#include "ebloom_nifs.h"
#include "erl_nif_compat.h"
#include "bloom_filter.hpp"

static ErlNifResourceType* BLOOM_FILTER_RESOURCE;

typedef struct
{
    bloom_filter* filter;
} bhandle;

extern "C"
{
    ERL_NIF_TERM ebloom_new_filter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM ebloom_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM ebloom_contains(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    ERL_NIF_TERM ebloom_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    ERL_NIF_TERM ebloom_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM ebloom_elements(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM ebloom_effective_fpp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    ERL_NIF_TERM ebloom_filter_intersect(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM ebloom_filter_union(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM ebloom_filter_difference(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    ERL_NIF_TERM ebloom_serialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM ebloom_deserialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    void ebloom_filter_dtor(ErlNifEnv* env, void* arg);

    int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

    static ErlNifFunc nif_funcs[] =
    {
        {"new",           3, ebloom_new_filter},
        {"insert",        2, ebloom_insert},
        {"contains",      2, ebloom_contains},
        {"clear",         1, ebloom_clear},
        {"size",          1, ebloom_size},
        {"elements",      1, ebloom_elements},
        {"effective_fpp", 1, ebloom_effective_fpp},
        {"intersect",     2, ebloom_filter_intersect},
        {"union",         2, ebloom_filter_union},
        {"difference",    2, ebloom_filter_difference},
        {"serialize",     1, ebloom_serialize},
        {"deserialize",   1, ebloom_deserialize}
    };

    ERL_NIF_INIT(ebloom, nif_funcs, &on_load, NULL, NULL, NULL);
};

ERL_NIF_TERM ebloom_new_filter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long predicted_element_count;
    double false_positive_probability;
    long random_seed;

    if (enif_get_long(env, argv[0], &predicted_element_count) &&
        enif_get_double(env, argv[1], &false_positive_probability) &&
        enif_get_long(env, argv[2], &random_seed))
    {
        bhandle* handle = (bhandle*)enif_alloc_resource_compat(env, BLOOM_FILTER_RESOURCE,
                                                        sizeof(bhandle));
        handle->filter = new bloom_filter(predicted_element_count,
                                          false_positive_probability,
                                          random_seed);
        ERL_NIF_TERM result = enif_make_resource(env, handle);
        enif_release_resource_compat(env, handle);
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
    }
    else
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM ebloom_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data;
    bhandle* handle;
    if (enif_get_resource(env, argv[0], BLOOM_FILTER_RESOURCE, (void**)&handle) &&
        enif_inspect_binary(env, argv[1], &data))
    {
        handle->filter->insert(data.data, data.size);
        return enif_make_atom(env, "ok");
    }
    else
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM ebloom_contains(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data;
    bhandle* handle;
    if (enif_get_resource(env, argv[0], BLOOM_FILTER_RESOURCE, (void**)&handle) &&
        enif_inspect_binary(env, argv[1], &data))
    {
        if (handle->filter->contains(data.data, data.size))
        {
            return enif_make_atom(env, "true");
        }
        else
        {
            return enif_make_atom(env, "false");
        }
    }
    else
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM ebloom_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bhandle* handle;
    if (enif_get_resource(env, argv[0], BLOOM_FILTER_RESOURCE, (void**)&handle))
    {
        handle->filter->clear();
        return enif_make_atom(env, "ok");
    }
    else
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM ebloom_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bhandle* handle;
    if (enif_get_resource(env, argv[0], BLOOM_FILTER_RESOURCE, (void**)&handle))
    {
        long result = handle->filter->size();
        return enif_make_long(env, result);
    }
    else
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM ebloom_elements(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bhandle* handle;
    if (enif_get_resource(env, argv[0], BLOOM_FILTER_RESOURCE, (void**)&handle))
    {
        long result = handle->filter->element_count();
        return enif_make_long(env, result);
    }
    else
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM ebloom_effective_fpp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bhandle* handle;
    if (enif_get_resource(env, argv[0], BLOOM_FILTER_RESOURCE, (void**)&handle))
    {
        double result = handle->filter->effective_fpp();
        return enif_make_double(env, result);
    }
    else
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM ebloom_filter_intersect(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bhandle* handle;
    bhandle* handle2;
    if (enif_get_resource(env, argv[0], BLOOM_FILTER_RESOURCE, (void**)&handle) &&
        enif_get_resource(env, argv[1], BLOOM_FILTER_RESOURCE, (void**)&handle2))
    {
        *(handle->filter) &= *(handle2->filter);
        return enif_make_atom(env, "ok");
    }
    else
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM ebloom_filter_union(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bhandle* handle;
    bhandle* handle2;
    if (enif_get_resource(env, argv[0], BLOOM_FILTER_RESOURCE, (void**)&handle) &&
        enif_get_resource(env, argv[1], BLOOM_FILTER_RESOURCE, (void**)&handle2))
    {
        *(handle->filter) |= *(handle2->filter);
        return enif_make_atom(env, "ok");
    }
    else
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM ebloom_filter_difference(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bhandle* handle;
    bhandle* handle2;
    if (enif_get_resource(env, argv[0], BLOOM_FILTER_RESOURCE, (void**)&handle) &&
        enif_get_resource(env, argv[1], BLOOM_FILTER_RESOURCE, (void**)&handle2))
    {
        *(handle->filter) ^= *(handle2->filter);
        return enif_make_atom(env, "ok");
    }
    else
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM ebloom_serialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bhandle* handle;
    if (enif_get_resource(env, argv[0], BLOOM_FILTER_RESOURCE, (void**)&handle))
    {
        ErlNifBinary bin;
        enif_alloc_binary_compat(env, handle->filter->serialized_size(), &bin);
        handle->filter->serialize(&bin.data, (unsigned int *)&bin.size);
        return enif_make_binary(env, &bin);
    }
    else
    {
        return enif_make_badarg(env);
    }
}

ERL_NIF_TERM ebloom_deserialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (enif_inspect_binary(env, argv[0], &bin))
    {
        bhandle* handle = (bhandle*)enif_alloc_resource_compat(env, BLOOM_FILTER_RESOURCE,
                                                        sizeof(bhandle));
        handle->filter = bloom_filter::deserialize(bin.data, bin.size);
        ERL_NIF_TERM result = enif_make_resource(env, handle);
        enif_release_resource_compat(env, handle);
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
    }
    else
    {
        return enif_make_badarg(env);
    }
}

void ebloom_filter_dtor(ErlNifEnv* env, void* arg)
{
    bhandle* handle = (bhandle*)arg;
    delete handle->filter;
}

int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    BLOOM_FILTER_RESOURCE = enif_open_resource_type_compat(env, "bloom_filter_resource",
                                                    &ebloom_filter_dtor,
                                                    flags,
                                                    0);
    return 0;
}



