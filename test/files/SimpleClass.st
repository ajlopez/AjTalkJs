Object subclass: #SimpleClass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests'!

!SimpleClass methodsFor: 'acesss' stamp: 'ajlopez 10/27/2013 20:06'!
one
    ^1.
! !

SimpleClass subclass: #SimpleSubclass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests'!

!SimpleSubclass methodsFor: 'acesss' stamp: 'ajlopez 10/27/2013 20:06'!
two
    ^super one + 1.
! !    
