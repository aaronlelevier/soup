%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 30. May 2021 5:43 AM
%%%-------------------------------------------------------------------
-module(soup).
-author("Aaron Lelevier").
-vsn(1.0).
-include("records.hrl").
-export([]).
%% debug
-compile(export_all).


%% Macros

-ifdef(debug_flag).
-define(LOG, fun(X) -> io:format("Mod:~p Line:~p ~p~n", [?MODULE, ?LINE, X]) end).
-else.
-define(LOG(X), void).
-endif.


%%%===================================================================
%%% API
%%%===================================================================

%% @doc title/1 -> Returns the html page title
-spec title(tuple()) -> #dom{} | no_match.

title(Elem) ->
  Name = <<"title">>,
  case find(Elem, Name, match_single([{type, name}])) of
    {Name, Attrs, [Content | []]} ->
      #dom{name = Name, attrs = Attrs, string = Content};
    _ ->
      no_match
  end.

%% @doc p/1 -> Returns the first paragraph found
-spec p(tuple()) -> #dom{} | no_match.

p(Elem) ->
  Name = <<"p">>,
  case find(Elem, Name, match_single([{type, name}])) of
    {Name, Attrs, [Content | []] = C} ->
      ?LOG(C),
      case is_tuple(Content) of
        true ->
          {_, _, ContentList} = Content,
          Content2 = lists:nth(1, ContentList);
        _ ->
          Content2 = Content
      end,
      #dom{name = Name, attrs = Attrs, string = Content2};
    _ ->
      no_match
  end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc find/2 -> Returns the 3 item mochiweb tuple of the
%% found element
-spec find(tuple(), binary(), #match_spec{}) -> tuple() | no_match.

find({Name, _Attrs, _Content} = Elem, Name, #match_spec{type=name}) ->
  ?LOG({Elem, Name}),
  Elem;

find({_OtherName, _Attrs, Content} = Elem, Name, MatchSpec)
    when is_tuple(Content) orelse is_list(Content) ->
  ?LOG({Elem, Name}),
  find(Content, Name, MatchSpec);

find({_OtherName, _Attrs, _Content} = Elem, Name, _MatchSpec) ->
  ?LOG({Elem, Name}),
  no_match;

find([H | T] = Elem, Name, MatchSpec) ->
  ?LOG({Elem, Name}),
  case find(H, Name, MatchSpec) of
    no_match ->
      find(T, Name, MatchSpec);
    Match ->
      Match
  end;

find(_ = Elem, Name, _MatchSpec) ->
  ?LOG({Elem, Name}),
  no_match.


%% Match Specs - specify the matching behaviour

match_single([{type, Type} | _Options]) -> #match_spec{
  behaviour = single,
  type = Type
}.

match_all() -> #match_spec{behaviour = all, data = #{acc => []}}.


%% Testing

path() ->
  filename:join(
    os:getenv("HOME"),
    "Documents/erlang/soup/test/data/example.html"
  ).

bytes() ->
  {ok, Bytes} = file:read_file(path()),
  Bytes.

tree() ->
  mochiweb_html:parse(bytes()).
