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

title(Tree) ->
  Name = <<"title">>,
  case find(Tree, Name, match_single([{type, name}])) of
    {Name, Attrs, Content} ->
      #dom{name = Name, attrs = Attrs, content = Content};
    _ ->
      no_match
  end.

%% @doc p/1 -> Returns the first paragraph found
-spec p(tuple()) -> #dom{} | no_match.

p(Tree) ->
  Name = <<"p">>,
  case find(Tree, Name, match_single([{type, name}])) of
    {Name, Attrs, Content} ->
      #dom{name = Name, attrs = Attrs, content = Content};
    _ ->
      no_match
  end.

%% @doc find/2 -> Returns the first element in the tree
%% that matches based on the attr
-spec find(tuple(), {binary(), binary()}) -> #dom{} | no_match.

find(Tree, Attr) ->
  ?LOG({Tree, Attr}),
  case find(Tree, void, match_single([{type, id}, {attr, Attr}])) of
    no_match ->
      no_match;
    {Name, Attrs, Content} ->
      #dom{name = Name, attrs = Attrs, content = Content}
  end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc find/2 -> Returns the 3 item mochiweb tuple of the
%% found element
-spec find(tuple(), binary(), #match_spec{}) -> tuple() | no_match.

find({Name, _Attrs, _Content} = Elem, Name, #match_spec{type = name}) ->
  ?LOG({Elem, Name}),
  Elem;

find({_Name, Attrs, Content} = Elem, Name,
  #match_spec{type = id, attr = Attr} = MatchSpec) ->
  ?LOG({Elem, Name, MatchSpec}),
  case lists:member(Attr, Attrs) of
    true ->
      Elem;
    _ ->
      find(Content, Name, MatchSpec)
  end;

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

match_single([{type, Type} | Options]) -> #match_spec{
  behaviour = single,
  type = Type,
  attr = proplists:get_value(attr, Options, undefined)
}.

%% TODO: add a find_all/2 and use this #match_spec{} with it
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
