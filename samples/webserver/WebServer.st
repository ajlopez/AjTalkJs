
http := Smalltalk require: 'http'.

server := http napply: 'createServer' with: { [ :req :res |
    res napply: 'end' with: { '<h1>Hello, world</h1>' }
    ] asFunction }.
    
server napply: 'listen' with: { 3000 }.


