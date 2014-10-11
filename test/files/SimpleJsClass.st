Object subclass: #SimpleJsClass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests'!

!SimpleJsClass methodsFor: 'acesss' stamp: 'ajlopez 10/27/2013 20:06'!
one
    "js: return 1;".
! !

!SimpleJsClass methodsFor: 'acesss' stamp: 'ajlopez 10/27/2013 20:06'!
inc: aNumber
    "js: return aNumber+1;".
! !

SimpleJsClass subclass: #SimpleJsSubclass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests'!

!SimpleJsSubclass methodsFor: 'acesss' stamp: 'ajlopez 10/27/2013 20:06'!
two
    ^super one + 1.
! !    
