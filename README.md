用于mnesia数据库的学习用例。mnesia启动为内存存储模式，涉及集群部署。 

1.运行用例中的run.bat文件。启动两个临时节点aa和bb
2.第一次启动mnesia的时候，在aa或bb点，输入：
    mnesia:create_schema(['aa@localhost', 'bb@localhost']),
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
       [需验证：mnesia数据库依旧存在。]
    3).运行用例中的restart_aa.bat文件, 重启aa, 输入：       
        mnesia:start().
        online_util:select("bob").
       [需验证：mnesia数据库，从bb点同步到aa点，数据依旧存在。]