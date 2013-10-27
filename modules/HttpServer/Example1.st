
server := HttpServer new.

server do: [ :req :res | res napply: 'end' with: { '<h1>Hello, world</h1' } ].
server listen: 3000.

