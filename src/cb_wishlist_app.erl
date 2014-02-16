-module(cb_wishlist_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(C_ACCEPTORS, 100).
-include("shared_definitions.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Routes = routes(),
    Dispatch = cowboy_router:compile(Routes),
    Port = port(),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _} = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
    {ok, _} = cberl:start_link(?CB_POOL, 5, cb_pool(), "", "", cb_bucket()),
    cb_wishlist_sup:start_link().

stop(_State) ->
    ok.


%% ===================================================================
%% Internal
%% ===================================================================
routes() ->
    [
     {'_', [
            {"/", cowboy_static, {file, "priv/index.html"}},
            {"/wishes", wishlist_resource, []},
            {"/assets/[...]", cowboy_static, {dir, "priv", [{mimetypes, cow_mimetypes, all}]}}
           ]}
    ].

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.

cb_pool() ->
    case os:getenv("COUCHBASE_SERVER") of
        false ->
            {ok, Server} = application:get_env(couchbase_server),
            Server;
        Other -> Other
    end.

cb_bucket() ->
    case os:getenv("COUCHBASE_BUCKET") of
        false ->
            {ok, Bucket} = application:get_env(couchbase_bucket),
            Bucket;
        Other -> Other
    end.

