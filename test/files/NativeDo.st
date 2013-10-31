
a := NativeObject new.
a nat: 'name' put: 'Adam'.
a nat: 'age' put: 800.

result := NativeArray new.

a ndo: [ :item | result napply: 'push' with: { item }].

