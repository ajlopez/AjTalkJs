# AjTalkJs

Smalltalk bytecode Virtual Machine, written in Javascript. It can compile Smalltalk fileOut. Work in Progress.

See my other project (in C#): https://github.com/ajlopez/AjTalk

Blog posts about AjTalk implementations: http://ajlopez.wordpress.com/category/ajtalk

## Simple Tests

I wrote simple test using node built-in assert module

```
node tests\tests
```

In Windows, you have a command file:

```
tests.cmd
```

## NodeUnit Tests

Install NodeUnit using npm from project folder

```
npm install nodeunit
```

Then run all tests in test folder

```
nodeunit test
```

(In Windows, there is a nodeunit.cmd that calls the nodeunit script)


