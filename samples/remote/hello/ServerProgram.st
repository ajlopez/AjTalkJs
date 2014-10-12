
Smalltalk load: 'RemoteServer.st'.
Smalltalk load: 'Console.st'.

console := Console new.

server := RemoteServer new: [ :client | 
    client on: 'error' with: [ :err | console write: 'error' with: err ] asFunction.
    client on: 'close' with: [ console write: 'client close' ] asFunction.
    client write: 'Hello, client. I''m the server'.
    ].

server listen: 3000.

Console new write: 'Server listening at port 3000'.

