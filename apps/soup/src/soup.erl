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

%% API

%% @doc find/2 -> Returns the 3 item mochiweb tuple of the
%% found element
-spec find(tuple(), binary()) -> tuple() | no_match.

find({Name, _Attrs, _Content} = Elem, Name) ->
  ?LOG({Elem, Name}),
  Elem;

find({_OtherName, _Attrs, Content} = Elem, Name) when is_tuple(Content) orelse is_list(Content) ->
  ?LOG({Elem, Name}),
  find(Content, Name);

find({_OtherName, _Attrs, _Content} = Elem, Name) ->
  ?LOG({Elem, Name}),
  no_match;

find([H | T] = Elem, Name) ->
  ?LOG({Elem, Name}),
  case find(H, Name) of
    no_match ->
      find(T, Name);
    Match ->
      Match
  end;

find(_ = Elem, Name) ->
  ?LOG({Elem, Name}),
  no_match.

%% @doc title/2 -> Returns the html page title
-spec title(binary()) -> #dom{} | no_match.

title(Elem) ->
  Name = <<"title">>,
  case find(Elem, Name) of
    {<<"title">>, [], [Content|[]]} ->
      #dom{name = Name, string = Content};
    _ ->
      no_match
  end.

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

