
Smalltalk load: 'RemoteServer.st'.
Smalltalk load: 'Console.st'.

console := Console new.

server := RemoteServer new: [ :client | 
    client napply: 'on' with: { 'error'. [ :err | console write: 'error' with: err ] toFunction }.
    client napply: 'on' with: { 'close'. [ console write: 'client close' ] toFunction }.
    client napply: 'write' with: { 'Hello, client. I''m the server' }.
    ].

server listen: 3000.

Console new write: 'Server listening at port 3000'.

