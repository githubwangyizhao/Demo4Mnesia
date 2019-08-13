
cd %~dp0

for %%I in (*.erl) do start /wait erlc %%I

if not exist ".\aa" (md aa)
if not exist ".\bb" (md bb)

for %%I in (*.beam) do ( 
	copy %%I "aa\" 
	copy %%I "bb\" 
)
for %%I in (*.hrl) do ( 
	copy %%I "aa\" 
	copy %%I "bb\"
)

if errorlevel 1 goto error_end
if errorlevel 0 goto normal_end

:error_end
echo "执行错误"
pause

:normal_end
for %%I in (*.beam) do del %%I
cd aa 
start werl -sname aa -mnesia dir "online"
cd ../bb
start werl -sname bb -mnesia dir "online"
echo "执行成功"
pause