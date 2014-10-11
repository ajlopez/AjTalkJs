Object subclass: #SimpleProxy
	instanceVariableNames: 'object'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests'!

!SimpleProxy class methodsFor: 'creation' stamp: 'ajlopez 10/11/2014 17:36'!
new: anObject
    | proxy |
    proxy := self basicNew.
    proxy object: anObject.
    ^proxy.
! !    

!SimpleProxy methodsFor: 'acesss' stamp: 'ajlopez 10/11/2014 17:36'!
object
    ^object.
! !

!SimpleProxy methodsFor: 'setters' stamp: '10/11/2014 17:36'!
object: anObject
    object := anObject.
! !

!SimpleProxy methodsFor: 'acesss' stamp: 'ajlopez 10/11/2014 17:38'!
doesNotUnderstand: aMessage
    ^object perform: aMessage selector with: aMessage arguments.
! !
