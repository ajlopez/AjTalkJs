
"Module dependencies"

express := Smalltalk require: 'express'.
http := Smalltalk require: 'http'.
ejslocals := Smalltalk require: 'ejs-locals'.

"Load"

Smalltalk load: 'Repository.st'.

console := Global nat: 'console'.

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
    model nat: 'title' put:'My Company Sample'.
    response napply: 'render' with: { 'index'. model }
] asFunction.  

customerrepo := Repository new.
supplierrepo := Repository new.

customerlist := [ :request :response |
    | model |
    model := NativeObject new.
    model nat: 'title' put:'Customers'.
    model nat: 'items' put: customerrepo getItems.
    response napply: 'render' with: { 'customerlist'. model }
] asFunction.  

customernew := [ :request :response |
    | model |
    model := NativeObject new.
    model nat: 'title' put:'New Customer'.
    response napply: 'render' with: { 'customernew'. model }
] asFunction.

customerinsert := [ :request :response |
    | customer |
    customer := NativeObject new.
    customer nat: 'name' put: (request napply: 'param' with: { 'name' }).
    customer nat: 'address' put: (request napply: 'param' with: { 'address' }).
    customerrepo add: customer.
    index value: request with: response.
] asFunction.

customerview := [ :request :response |
    | model customer |
    customer := customerrepo getById: ((request nat: 'params') nat: 'id').
    model := NativeObject new.
    model nat: 'title' put: ' Customer'.
    model nat: 'item' put: customer.
    response napply: 'render' with: { 'customerview'. model }
] asFunction.

supplierlist := [ :request :response |
    | model |
    model := NativeObject new.
    model nat: 'title' put:'Suppliers'.
    model nat: 'items' put: supplierrepo getItems.
    response napply: 'render' with: { 'supplierlist'. model }
] asFunction.  

suppliernew := [ :request :response |
    | model |
    model := NativeObject new.
    model nat: 'title' put:'New Supplier'.
    response napply: 'render' with: { 'suppliernew'. model }
] asFunction.  

supplierinsert := [ :request :response |
    | supplier |
    supplier := NativeObject new.
    supplier nat: 'name' put: (request napply: 'param' with: { 'name' }).
    supplier nat: 'address' put: (request napply: 'param' with: { 'address' }).
    supplierrepo add: supplier.
    index value: request with: response.
] asFunction.

supplierview := [ :request :response |
    | model supplier |
    supplier := supplierrepo getById: ((request nat: 'params') nat: 'id').
    model := NativeObject new.
    model nat: 'title' put: ' supplier'.
    model nat: 'item' put: supplier.
    response napply: 'render' with: { 'supplierview'. model }
] asFunction.

"Routing"

app napply: 'get' with: { '/'. index }.
app napply: 'get' with: { '/customers'. customerlist }.
app napply: 'get' with: { '/customers/new'. customernew }.
app napply: 'post' with: { '/customers/new'. customerinsert }.
app napply: 'get' with: { '/customers/:id'. customerview }.
app napply: 'get' with: { '/suppliers'. supplierlist }.
app napply: 'get' with: { '/suppliers/new'. suppliernew }.
app napply: 'post' with: { '/suppliers/new'. supplierinsert }.
app napply: 'get' with: { '/suppliers/:id'. supplierview }.

server := http napply: 'createServer' with: { app }.
server napply: 'listen' with: { 3000. [ console napply: 'log' with: { 'Express server listening on port 3000' } ] asFunction }.

