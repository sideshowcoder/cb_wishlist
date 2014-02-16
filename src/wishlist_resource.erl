-module(wishlist_resource).

-export([init/3,
         content_types_provided/2,
         get_json/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, get_json}
     ], Req, State}.

get_json(Req, State) ->
    Page = case cowboy_req:qs_val(<<"page">>, Req) of
               {undefined, _} -> 1;
               {QsVal, _} -> list_to_integer(binary_to_list(QsVal))
           end,
    Body = wishlist:page(Page),
    {Body, Req, State}.

