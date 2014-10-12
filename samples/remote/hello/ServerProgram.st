
Smalltalk load: 'RemoteServer.st'.

server := RemoteServer new: [ :client | 
    client on: 'error' with: [ :err | Global console log: 'error' with: err ] asFunction.
    client on: 'close' with: [ Global console log: 'client close' ] asFunction.
    client write: 'Hello, client. I''m the server'.
    ].

server listen: 3000.

Global console log: 'Server listening at port 3000'.

