
"Module dependencies"

console := Global nat: 'console'.
express := Smalltalk require: 'express'.
http := Smalltalk require: 'http'.
ejslocals := Smalltalk require: 'ejs-locals'.
 
app := express value.

app napply: 'engine' with: { 'ejs'. ejslocals }.

"all environments"

app napply: 'set' with: { 'port'. 3000 }.
app napply: 'set' with: { 'views'. './views' }.
app napply: 'set' with: { 'view engine'. 'ejs' }.
app napply: 'use' with: { express napply: 'favicon' }.
app napply: 'use' with: { express napply: 'logger' with: { 'dev' } }.
app napply: 'use' with: { express napply: 'bodyParser' }.
app napply: 'use' with: { express napply: 'methodOverride' }.
app napply: 'use' with: { express napply: 'cookieParser' with: { 'your secret here' } }.
app napply: 'use' with: { express napply: 'session' }.
app napply: 'use' with: { app nat: 'router' }.
app napply: 'use' with: { express napply: 'static' with: { './public' } }.

"development only"
'development' = (app napply: 'get' with: { 'env' })
    ifTrue: [
      app napply: 'use' with: { express napply: 'errorHandler' }  
    ].

index := [ :request :response |
    | model |
    model := NativeObject new.
    model nat: 'title' put:'Bootstrap Sample'.
    response napply: 'render' with: { 'index'. model }
] toFunction.  

app napply: 'get' with: { '/'. index }.

server := http napply: 'createServer' with: { app }.
server napply: 'listen' with: { 3000. [ console napply: 'log' with: { 'Express server listening on port 3000' } ] toFunction }.

