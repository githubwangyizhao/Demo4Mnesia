
-module(online_util).

-include("online.hrl").
-export([add/2, find/1, del/2, select/1]).

add(Who, Pid) ->
    F = fun() ->
            New = #chat{user_name = Who, pid = Pid},
            mnesia:write(New)
        end,
    mnesia:transaction(F).

del(Who, Pid) ->
    Ori = {chat, Who, Pid},
    F = fun() ->
            mnesia:delete_object(Ori)
        end,
    mnesia:transaction(F).

find(Who) ->
    F = fun() ->
            io:format("find result, ~p~n", [mnesia:read({chat, Who})])
        end,
    mnesia:transaction(F).

select(Who) ->
    F = fun() ->
            Chat = #chat{user_name = Who, pid = '$1', _ = '_'},
            io:format("find result, ~p~n", [mnesia:select(chat, [{Chat, [], ['$1']}])])
        end,
    mnesia:transaction(F).