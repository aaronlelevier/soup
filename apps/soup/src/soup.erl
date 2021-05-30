%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 30. May 2021 5:43 AM
%%%-------------------------------------------------------------------
-module(soup).
-author("Aaron Lelevier").
-vsn(1.0).

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

