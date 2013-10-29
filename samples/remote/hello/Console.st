Object subclass: #Console
	instanceVariableNames: 'console'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Console'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Console class
	instanceVariableNames: ''!

!Console class methodsFor: 'creation' stamp: 'ajlopez 10/29/2013 06:19'!
new
    | newconsole |
    
    newconsole := self basicNew.
    newconsole console: (Global nat: 'console').
    ^newconsole.
! !

!Console methodsFor: 'accessing' stamp: 'ajlopez 10/29/2013 06:19'!
console: aConsole
    console := aConsole.
! !

!Console methodsFor: 'writing' stamp: 'ajlopez 10/29/2013 06:19'!
write: message
    console napply: 'log' with: { message }.
! !

!Console methodsFor: 'writing' stamp: 'ajlopez 10/29/2013 06:19'!
write: message with: message2
    console napply: 'log' with: { message. message2 }.
! !

!Console methodsFor: 'writing' stamp: 'ajlopez 10/29/2013 06:19'!
dump: anObject
    console napply: 'dir' with: { anObject }.
! !
