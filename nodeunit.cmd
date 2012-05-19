if "%NODE_PATH%" == "" set NODE_PATH=%~dp0lib
node node_modules\nodeunit\bin\nodeunit %*
