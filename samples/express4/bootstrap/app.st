
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

app napply: 'set' with: { 'views'. './views' }.
app napply: 'set' with: { 'view engine'. 'ejs' }.
app napply: 'use' with: { favicon value }.
app napply: 'use' with: { logger value: 'dev' }.
app napply: 'use' with: { bodyParser napply: 'json' }.
app napply: 'use' with: { bodyParser napply: 'urlencoded' }.
app napply: 'use' with: { cookieParser value }.
app napply: 'use' with: { express napply: 'static' with: { './public' } }.

"catch 404 and forward to error handler"
app napply: 'use' with: { [ :req :res :next | |err| 
    err := Error new.
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
}  
 
