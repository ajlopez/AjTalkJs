Object subclass: #Host
	instanceVariableNames: 'server'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Host'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Host class
	instanceVariableNames: ''!

!Host class methodsFor: 'creation' stamp: 'ajlopez 10/12/2014 06:19'!
new
    | newserver simpleremote |
    
    newserver := self basicNew.
    simpleremote := Smalltalk require: 'simpleremote'.
    newserver server: (simpleremote createRemoteServer: Smalltalk).
    ^newserver.
! !

!Host methodsFor: 'accessing' stamp: 'ajlopez 10/12/2014 06:19'!
server: aServer
    server := aServer.
! !

!Host methodsFor: 'operation' stamp: 'ajlopez 10/12/2014 06:19'!
listen: aPort
    server listen: aPort.
! !

