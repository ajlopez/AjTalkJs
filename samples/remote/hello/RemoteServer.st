Object subclass: #RemoteServer
	instanceVariableNames: 'server'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'RemoteServer'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

RemoteServer class
	instanceVariableNames: ''!

!RemoteServer class methodsFor: 'creation' stamp: 'ajlopez 10/29/2013 06:19'!
new: aBlock
    | newserver simplemessages |
    
    newserver := self basicNew.
    simplemessages := Smalltalk require: 'simplemessages'.
    newserver server: (simplemessages napply: 'createServer' with: { aBlock asFunction }).
    ^newserver.
! !

!RemoteServer methodsFor: 'accessing' stamp: 'ajlopez 10/29/2013 06:19'!
server: aServer
    server := aServer.
! !

!RemoteServer methodsFor: 'operation' stamp: 'ajlopez 10/29/2013 06:19'!
listen: aPort
    server napply: 'listen' with: { aPort }.
! !

