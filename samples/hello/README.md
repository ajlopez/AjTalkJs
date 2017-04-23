# Hello sample

Sample to use the native console:

```smalltalk
console := Global nat: 'console'.
console napply: 'log' with: { 'Hello, world' }.
```

The `nat:` method get the JavaScript property. The `napply:with:` method calls the native method using the arguments.

So, the above code is equivalent to:

```
global.console.log('Hello, world');
```

Run with

```
node run hello.st
```
