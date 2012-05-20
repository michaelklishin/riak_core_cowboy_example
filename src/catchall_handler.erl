-module(catchall_handler).
-behavior(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).


%%
%% API
%%

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_http_req:method(Req),
    {Path, Req3} = cowboy_http_req:path(Req2),

    Id = mkid(Method, Path),
    Hash = riak_core_util:chash_key({Path, Id}),
    Index = case riak_core_apl:get_primary_apl(Hash, 1, hello_world) of
                [{Idx, _Type}] -> Idx;
                _ -> {0, node()}
            end,
    lager:debug("Dispatching to ~p", [Index]),

    Result = case riak_core_vnode_master:sync_spawn_command(Index, {Method, Path, Req3}, hello_world_vnode_master) of
                 {ok, R} ->
                     R;
                 {error, Reason} ->
                     {500, [], Reason};
                 Unhandled ->
                     lager:warning("Unhandled reply: ~p~n", [Unhandled]),
                     {500, [], <<"Unhandled reply">>}
             end,
    {_N, Name} = Index,
    {Status, Headers, Body} = Result,

    {ok, Res} = cowboy_http_req:reply(Status,
                                      normalize_headers(Headers) ++
                                          [{<<"X-Handling-Node">>, atom_to_list(Name)}],
                                      Body,
                                      Req3),
    {ok, Res, State}.


terminate(_Req, _State) ->
    ok.


mkid(Method, Resource) ->
    {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
    {_,_,NowPart} = now(),
    Id = erlang:phash2([Y,Mo,D,H,Mi,S,Method,Resource,NowPart]),
    io_lib:format("~p", [Id]).

normalize_headers(L) ->
    [{list_to_binary(capitalize(K)), list_to_binary(V)} || {K, V} <- L].

capitalize([F|Rest]) ->
    [string:to_upper(F) | string:to_lower(Rest)].
