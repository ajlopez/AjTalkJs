
http := Smalltalk require: 'http'.

server := http napply: 'createServer' with: { [ :req :res |
    res napply: 'end' with: { '<h1>Hello, world</h1>' }
    ] toFunction }.
    
server napply: 'listen' with: { 3000 }.


