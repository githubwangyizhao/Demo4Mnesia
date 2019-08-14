
-module(online_init).

-include("../include/online.hrl").
-export([init/0]).

init() ->
    mnesia:create_table(chat, [
        {ram_copies, []},
        {type, bag},
        {attributes, record_info(fields, chat)}
    ]).
