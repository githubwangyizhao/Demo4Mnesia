
@echo off
set base_path=%~dp0
set src_path=%~dp0\src
set nodes_path=%~dp0\nodes
set nodes=%~dp0\nodes.txt

if not exist %nodes_path% mkdir %nodes_path%
cd %nodes_path%
for	/f "skip=1 delims=" %%I in (%nodes%) do (
	if not exist %%I mkdir %%I
)

cd %src_path%
for %%f in (*.erl) do start /wait erlc %%f
for %%f in (*.beam) do ( 
	for	/f "skip=1 delims=" %%d in (%nodes%) do xcopy %src_path%\%%f %nodes_path%\%%d
)

if errorlevel 1 goto error_end
if errorlevel 0 goto normal_end

:error_end
echo "执行错误"
pause

:normal_end
for %%f in (*.beam) do del %%f
cd %nodes_path%
for	/f "skip=1 delims=" %%I in (%nodes%) do (
	cd %%I
	echo cd %nodes_path%\%%I > start_%%I_node.bat 
	echo start werl -name %%I@127.0.0.1 -mnesia dir "mnesia_db" >> start_%%I_node.bat 
	start cmd /c start_%%I_node.bat
	cd ..
)
echo "执行成功"
pause