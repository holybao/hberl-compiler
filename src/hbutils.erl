%%%-------------------------------------------------------------------
%%% author    libao he (holybao@hotmail.com & blog.holybao.com)
%%%-------------------------------------------------------------------
-module(hbutils).
-export([format2string/2,format2binary/2]).

%% Utils
format2string(Format,Args) when is_list(Args)->
    lists:flatten(io_lib:format(Format,Args));
format2string(Format,Arg) ->
    lists:flatten(io_lib:format(Format,[Arg])).

format2binary(Format,Args) when is_list(Args)->
    list_to_binary(io_lib:format(Format,Args));
format2binary(Format,Arg) ->
    list_to_binary(io_lib:format(Format,[Arg])).
    