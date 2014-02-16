-module(wishlist).
-include("shared_definitions.hrl").

-export([
         create_or_update/1,
         find/1,
         page/1
        ]).

-record(wish, {title=list_to_binary(""), description=list_to_binary(""), id}).

%% ===================================================================
%% API
%% ===================================================================

create_or_update(Json) ->
    save(from_json(Json)).

find(Id) ->
    to_json(load(Id)).

page(Page) ->
    case page_elements(Page) of
        Wishes when length(Wishes) > 1 ->
            jiffy:encode({[{wishes, lists:map(fun to_external_plural/1, Wishes)}]});
        _ ->
            jiffy:encode({[{error, <<"no more wishes">>}]})
    end.


%% ===================================================================
%% Internal
%% ===================================================================

page_elements(Page) ->
    {ok, {_, Documents}} = cberl:view(?CB_POOL, "wishlist", "wishlist", [{stale, false}]),
    Pages = split_list(?ELEMENTS_PER_PAGE, Documents),
    lists:map(fun(E) ->
                      [{<<"id">>, Id}, _, _] = E,
                      load(Id)
              end, lists:nth(Page, Pages)).

save(Wish) ->
    cberl:set(?CB_POOL, Wish#wish.id, 0, to_stored(Wish)).

load(Id) ->
    {_, _, Value} = cberl:get(?CB_POOL, Id),
    from_stored(Value).

to_stored(Wish) ->
    {[
      {<<"title">>, Wish#wish.title},
      {<<"description">>, Wish#wish.description},
      {<<"id">>, Wish#wish.id},
      {<<"type">>, <<"wish">>}
    ]}.

to_external_single(Wish) ->
    {[
      {<<"wish">>,
       {[{<<"title">>, Wish#wish.title},
        {<<"description">>, Wish#wish.description},
        {<<"id">>, Wish#wish.id}]}}
     ]}.

to_external_plural(Wish) ->
    {[
      {<<"title">>, Wish#wish.title},
      {<<"description">>, Wish#wish.description},
      {<<"id">>, Wish#wish.id}
    ]}.



from_stored(Data) ->
    {[
      {<<"title">>, Title},
      {<<"description">>, Description},
      {<<"id">>, Id},
      {<<"type">>, <<"wish">>}
     ]} = Data,
    #wish{title=Title,
          description=Description,
          id=Id}.

from_json(Json) ->
    {[{<<"wish">>,
       {[{<<"title">>, Title},
          {<<"description">>, Description},
          {<<"id">>, Id}
        ]}}]} = jiffy:decode(Json),
    #wish{title=Title,
          description=Description,
          id=Id}.

to_json(Wish) ->
    jiffy:encode(to_external_single(Wish)).

split_list(Max, List) ->
    RevList = element(1, lists:foldl( fun
                            (E, {[Buff|Acc], C}) when C < Max -> {[[E|Buff]|Acc], C+1};
                            (E, {[Buff|Acc], _}) -> {[[E],Buff|Acc], 1};
                            (E, {[], _}) -> {[[E]], 1}
                        end, {[], 0}, List)),
    lists:foldl(fun(E, Acc) -> [lists:reverse(E)|Acc] end, [], RevList).



%% ===================================================================
%% Tests
%% ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

wishlist_test_() ->
    [{foreach, fun setup/0, fun teardown/1,
      [
       fun test_create_or_update/0,
       fun test_page_as_json/0
      ]}].

setup() ->
    {ok, _} = cberl:start_link(?CB_POOL, 5, "localhost", "", "", "cb_wishlist"),
    Wish1 = #wish{title=list_to_binary("foo"), description=list_to_binary("bar"), id=list_to_binary("1")},
    Wish2 = #wish{title=list_to_binary("foo"), description=list_to_binary("bar"), id=list_to_binary("2")},
    Wish3 = #wish{title=list_to_binary("foo"), description=list_to_binary("bar"), id=list_to_binary("3")},
    Wish4 = #wish{title=list_to_binary("foo"), description=list_to_binary("bar"), id=list_to_binary("4")},
    Wish5 = #wish{title=list_to_binary("foo"), description=list_to_binary("bar"), id=list_to_binary("5")},
    Wish6 = #wish{title=list_to_binary("foo"), description=list_to_binary("bar"), id=list_to_binary("6")},
    save(Wish1),
    save(Wish2),
    save(Wish3),
    save(Wish4),
    save(Wish5),
    save(Wish6),
    ok.

teardown(_) ->
    cberl:stop(?CB_POOL).

test_create_or_update() ->
    Json = <<"{\"wish\":{\"title\":\"A wish\",\"description\":\"A description\",\"id\":\"123\"}}">>,
    create_or_update(Json),
    WishFromDB = find(<<"123">>),
    ?assertMatch(Json, WishFromDB).

test_page_as_json() ->
    Wish = {[
             {<<"title">>,<<"foo">>},
             {<<"description">>,<<"bar">>},
             {<<"id">>,<<"1">>}
            ]},
    {[{<<"wishes">>, Wishes}]} = jiffy:decode(page(1)),
    [WishFromPage|_] = Wishes,
    ?assertEqual(Wish, WishFromPage),
    ?assertEqual(5, length(Wishes)).

-endif.
