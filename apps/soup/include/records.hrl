%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 30. May 2021 7:10 AM
%%%-------------------------------------------------------------------
-author("Aaron Lelevier").

-record(dom, {
  name :: binary(),
  attrs :: [{atom(), binary()}],
  string :: binary()
}).
