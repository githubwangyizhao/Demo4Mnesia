
cd %~dp0

if not exist ".\ebin" md ebin
for %%I in (*.erl) do start /wait erlc -o ".\ebin" %%I
for %%I in (*.hrl) do copy %%I ".\ebin"

cd ebin
start werl -sname mnesia_test

pause