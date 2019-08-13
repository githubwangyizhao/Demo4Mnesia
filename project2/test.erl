
-module(test).

-include_lib("stdlib/include/qlc.hrl").
-include("def.hrl").

-export([create_table/0, del_table/0]).
-export([insert_or_update/2]).
-export([select/0, qlc_select/0, select_part/0, qlc_select_part/0]).
-export([select_where/1, qlc_select_where/1, select_where2/1, select_where3/1]).
-export([qlc_order_by/0, qlc_order_by2/0]).
-export([qlc_join/1]).
-export([select_limit/1, qlc_select_limit/1]).
-export([select_count/0]).
-export([delete/0]).

%%===============================================
%%  Create Table/Delete Table操作
%%  create table y_account (
%%          id int,
%%          account varchar(50),
%%          password varchar(50),
%%          primary key(id)
%%      );
%%===============================================
create_table() ->
    mnesia:create_table(y_account, [
        {ram_copies, [node()]},
        {type, set},        %% set表示id作为主键，不予许id重复。如果改为bag，id可以重复，但整条记录不能重复。
        {attributes, record_info(fields, y_account)}
    ]),
    mnesia:create_table(y_info, [{ram_copies, [node()]}, {type, set}, {attributes, record_info(fields, y_info)}]).

del_table() ->
    mnesia:delete_table(y_account).

%%===============================================
%%  Insert / Update 操作
%%  mnesia是根据主键去更新记录的，如果主键不存在则插入
%%  insert into y_account (id, account, password)
%%              values(5, "xiaohong", "123")
%%              on duplicate key update account="xiaohong", password="123";
%%===============================================
insert_or_update(Account, Pwd) ->
    SeqId =
        case get(seq_id) of
            undefined ->
                put(seq_id, 1),
                1;
            OriSeqId ->
                put(seq_id, OriSeqId + 1),
                OriSeqId + 1
        end,
    F = fun() ->
            Acc = #y_account{id = SeqId, account=Account, password=Pwd},
            mnesia:write(Acc),
            Info = #y_info{id = SeqId, birthday = calendar:now_to_datetime(now()), nickname = "nickname_" ++ Account, sex = rand:uniform(2)},
            mnesia:write(Info)
        end,
    mnesia:transaction(F).

%%===============================================
%%  Select 查询全部字段
%%  select * from y_account
%%===============================================
select() ->
    F = fun() ->
            MatchHead = #y_account{ _ = '_'},
            Guard = [],
            Result = ['$_'],
            mnesia:select(y_account, [{MatchHead, Guard, Result}])
        end,
    mnesia:transaction(F).

qlc_select() ->
    F = fun() ->
            Q = qlc:q([E || E <- mnesia:table(y_account)]),
            qlc:e(Q)
        end,
    mnesia:transaction(F).

%%===============================================
%%  Select 查询部分字段
%%  select id, account from y_account
%%===============================================
select_part() ->
    F = fun() ->
            MatchHead = #y_account{id = '$1', account = '$2', _ = '_' },
            Guard = [],
            Result = ['$$'],
            mnesia:select(y_account, [{MatchHead, Guard, Result}])
        end,
    mnesia:transaction(F).

qlc_select_part() ->
    F = fun() ->
            Q = qlc:q([[E#y_account.id, E#y_account.account] || E <- mnesia:table(y_account)]),
            qlc:e(Q)
        end,
    mnesia:transaction(F).

%%===============================================
%%  Where 查询
%%  select account from y_account where id>N
%%===============================================
select_where(N) ->
    F = fun() ->
            MatchHead = #y_account{id = '$1', account = '$2', _ = '_'},
            Guard = [{'>', '$1', N}],
            Result = ['$2'],
            mnesia:select(y_account, [{MatchHead, Guard, Result}])
        end,
    mnesia:transaction(F).

qlc_select_where(N) ->
    F = fun() ->
            Q = qlc:q([E#y_account.account || E <- mnesia:table(y_account), E#y_account.id > N]),
            qlc:e(Q)
        end,
    mnesia:transaction(F).

%%===============================================
%%  按主键 key=X 查询
%%  select * from y_account where id=N
%%===============================================
select_where2(Id) ->
    F = fun() ->
            mnesia:read({y_account, Id})
        end,
    mnesia:transaction(F).

%%===============================================
%%  按非主键 key=X 查询
%%  select * from y_account where account='xiaomin'
%%===============================================
select_where3(Account) ->
    F = fun() ->
            MatchHead = #y_account{id = '_', account = Account, password = '_'},
            Guard = [],
            Result = ['$_'],
            mnesia:select(y_account, [{MatchHead, Guard, Result}])
        end,
    mnesia:transaction(F).

%%===============================================
%%  Order By 查询
%%  select * from y_account order by id asc
%%===============================================
qlc_order_by() ->
    F = fun() ->
            Q = qlc:q([E || E <- mnesia:table(y_account)]),
            qlc:e(qlc:keysort(2, Q, [{order, ascending}]))
        end,
    mnesia:transaction(F).

qlc_order_by2() ->
    F = fun() ->
            Q = qlc:q([E || E <- mnesia:table(y_account)]),
            Order = fun(A, B) ->
                        B#y_account.id > A#y_account.id
                    end,
            qlc:e(qlc:sort(Q, [{order, Order}]))
        end,
    mnesia:transaction(F).

%%===============================================
%%  Join 关联表查询
%%  select y_info.* from y_account
%%  join y_info on (y_account.id = y_info.id)
%%  where y_account.account = 'xiaomin'
%%===============================================
qlc_join(Account) ->
    F = fun() ->
            Q = qlc:q([Y || X <- mnesia:table(y_account),
                X#y_account.account =:= Account,
                Y <- mnesia:table(y_info),
                X#y_account.id =:= Y#y_info.id
            ]),
            qlc:e(Q)
        end,
    mnesia:transaction(F).

%%===============================================
%%  Limit 查询
%%  select * from y_account limit N
%%===============================================
select_limit(N) ->
    F = fun() ->
            MatchHead = #y_account{ _ = '_' },
            mnesia:select(y_account, [{MatchHead, [], ['$_']}], N, none)
        end,
    mnesia:transaction(F).

qlc_select_limit(N) ->
    F = fun() ->
            Q = qlc:q([E || E <- mnesia:table(y_account)]),
            QC = qlc:cursor(Q),
            qlc:next_answers(QC, N)
        end,
    mnesia:transaction(F).

%%===============================================
%%  Select count(*) 查询
%%  select count(*) from y_account
%%===============================================
select_count() ->
    F = fun() ->
            mnesia:table_info(y_account, size)
        end,
    mnesia:transaction(F).

%%===============================================
%%  Delete 查询
%%  delete from y_account where id=5
%%===============================================
delete() ->
    F = fun() ->
            mnesia:delete({y_account, 5})
        end,
    mnesia:transaction(F).

