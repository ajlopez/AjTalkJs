# Web Server Sample

Run with

```
node run WebServer.st
```

The Smalltalk script is:

```smalltalk
http := Smalltalk require: 'http'.

server := http napply: 'createServer' with: { [ :req :res |
    res napply: 'end' with: { '<h1>Hello, world</h1>' }
    ] asFunction }.
    
server napply: 'listen' with: { 3000 }.

console := Global nat: 'console'.
console napply: 'log' with: { 'Server started at http://localhost:3000' }.
```
