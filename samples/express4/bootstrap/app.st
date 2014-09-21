
"Module dependencies"

console := Global nat: 'console'.
express := Smalltalk require: 'express'.
path := Smalltalk require: 'path'.
favicon := Smalltalk require: 'static-favicon'.
logger := Smalltalk require: 'morgan'.
cookieParser := Smalltalk require: 'cookie-parser'.
bodyParser := Smalltalk require: 'body-parser'.

app := express value.

"all environments"

app set: 'views' with: './views'.
app set: 'view engine' with: 'ejs'.
app use: favicon value.
app use: (logger value: 'dev').
app use: bodyParser json.
app use: bodyParser urlencoded.
app use: cookieParser value.
app use: (express static: './public').

"index"

index := [ :request :response |
    | model |
    console log: 'get index'.
    model := NativeObject new.
    model nat: 'title' put:'Bootstrap Sample'.
    response render: 'index' with: model
] asFunction.

app napply: 'get' with: { '/'. index }.
 
"catch 404 and forward to error handler"
app napply: 'use' with: { [ :req :res :next | |err| 
    err := Error nnew.
    err nat: 'status' put: 404.
    next value: err.
    ] asFunction }.

"error handlers"

"development error handlers
 will print stack trace"
'development' = (app napply: 'get' with: { 'env' })
    ifTrue: [
      app napply: 'use' with: { 
        [ :err :req :res :next |
            | model |
            model := NativeObject new.
            model nat: 'message' put: (err nat: message).
            model nat: 'title' put: 'error'.
            model nat: 'error' put: err.
            res napply: 'status' with: { ( (err nat: status) ifTrue: [err nat: status] ifFalse: [500] ) }.            
            res napply: 'render' with: { 'error', model }.
        ] asFunction
      }
    ].

"production error handler
 no stacktraces leaked for user"
app napply: 'use' with: { 
    [ :err :req :res :next |
        | model |
        model := NativeObject new.
        model nat: 'message' put: (err nat: message).
        model nat: 'error' put: NativeObject new.
        res napply: 'status' with: { ((err nat: status) ifTrue: [err nat: status] ifFalse: [500] ) }.            
        res napply: 'render' with: { 'error', model }.
    ] asFunction
}.

"start"

app napply: 'set' with: { 'port'. 3000 }.

app napply: 'listen' with: {
    (app napply: 'get' with: { 'port' }).
    [ console napply: 'log' with: { 'Express server listening on port'. app napply: 'get' with: { 'port' } } ] asFunction
}.
