
"Module dependencies"

express := Smalltalk require: 'express'.
http := Smalltalk require: 'http'.
ejslocals := Smalltalk require: 'ejs-locals'.

app := express value.

console := Global nat: 'console'.

console napply: 'log' with: { 'value' }.
"console napply: 'log' with: { 'ejslocals', ejslocals }."
"app napply: 'engine' with: { 'ejs', ejslocals }."

console napply: 'log' with: { 'ejs' }.

"all environments"

app napply: 'set' with: { 'port', 3000 }.
app napply: 'set' with: { 'views', './views' }.
app napply: 'set' with: { 'view engine', 'ejs' }.
console napply: 'log' with: { 'sets' }.
app napply: 'use' with: { express napply: 'favicon' }.
console napply: 'log' with: { 'use1' }.
app napply: 'use' with: { express napply: 'logger' with: { 'dev' } }.
console napply: 'log' with: { 'use2' }.
app napply: 'use' with: { express napply: 'bodyParser' }.
console napply: 'log' with: { 'use3' }.
app napply: 'use' with: { express napply: 'methodOverride' }.
console napply: 'log' with: { 'use4' }.
app napply: 'use' with: { express napply: 'cookieParser' with: { 'your secret here' } }.
console napply: 'log' with: { 'use5' }.
app napply: 'use' with: { express napply: 'session' }.
console napply: 'log' with: { 'use6' }.
app napply: 'use' with: { app nat: 'router' }.
console napply: 'log' with: { 'use7' }.
app napply: 'use' with: { express napply: 'static' with: { './public' } }.
console napply: 'log' with: { 'use' }.

"development only"
'development' = (app napply: 'get' with: { 'env' })
    ifTrue: [
      app napply: 'use' with: { express napply: 'errorHandler' }  
    ].

index := [ :request :response |
    | model |
    model := NativeObject new.
    model nat: 'title' put:'Bootstrap Sample'.
    response napply: 'render' with: { 'index', model }
] toFunction.  

app napply: 'get' with: { '/', index }.

server := http napply: 'createServer' with: { app }.
server napply: 'listen' with: { 3000 ", [ console napply: 'log' with: { ' Express server listening on port 3000' } ] toFunction "}.

