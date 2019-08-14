用于mnesia数据库的学习用例。

==***用例1***== 【/project1】
说明：mnesia启动为内存存储模式，涉及集群部署。 
1.运行用例中的project1/run.bat文件。启动节点aa、bb和cc
2.第一次启动mnesia的时候，在aa或bb点，输入：
    mnesia:create_schema(['aa@127.0.0.1', 'bb@127.0.0.1']).
    返回ok
    1).schema只能创建一次
    2).实际上，每个节点毒保存一份schema拷贝
    3).在aa和bb两节点上分别输入：
        mnesia:start().
        返回ok
    4).初始化表，在aa或者bb节点输入：
        online_init:init().
    5).查看表信息，在aa和bb点分别输入:
        mnesia:info().
3.验证==>集群中的增、删、查操作
    1).在aa点输入：
        online_util:add("bob", "im").      
    2).在bb点输入：
        online_util:find("bob").
    3).在bb点输入：
        online_util:add("bob", "hello").
    4).在aa点输入：              
        online_util:find("bob").
    5).在aa点输入：
        online_util:del("bob", "hello").
    6).在bb点输入：
        online_util:select("bob").
4.验证==>宕机处理        
    1).aa点输入:
        halt(). 
        宕掉aa点
    2).bb点输入:    
        online_util:select("bob").        
       [可证明：mnesia数据库依旧存在。]
    3).运行用例中的project1/nodes/aa/start_aa_nodes.bat文件, 重启aa, 输入：       
        mnesia:start().
        online_util:select("bob").
       [可证明：mnesia数据库，从bb点同步到aa点，数据依旧存在。]
5.给已有的mnesia集群添加节点cc
	1).	cc点输入：
		mnesia:start().
		online_util:join_cluster_mnesia('aa@127.0.0.1').
		online_util:find("bob"). 
		[可证明：cc已加入集群。]

==***用例2***== 【/project2】      
说明：Mnesia数据库如何实现SQL查询。
    实现select/insert/update/where/order by/join/limit/delete等SQL操作。
1.运行用例中的project2/run.bat文件。启动mnesia_test@localhost节点
2. 1)启动mnesia:
        mnesia:start().
   2)test:create_table().
   3)test:insert_or_update("account1", "password123")
   4)test:select().
   5)test:qlc_select().
   6)test:select_part().
   7)test:qlc_select_part().
   8)test:select_where(1).
   9)test:qlc_select_where(1).
   10)test:select_where2(1).
   11)test:select_where3(1).
   12)test:qlc_order_by().
   13)test:qlc_order_by2().
   14)test:qlc_join(1).
   15)test:select_limit(1).
   16)test:qlc_select_limit(1).
   17)test:select_count(1).
   18)test:delete(1).