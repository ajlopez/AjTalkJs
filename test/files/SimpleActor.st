Object subclass: #SimpleActor
	instanceVariableNames: 'object'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests'!

!SimpleActor class methodsFor: 'creation' stamp: 'ajlopez 10/11/2014 18:36'!
new: anObject
    | proxy |
    proxy := self basicNew.
    proxy object: anObject.
    ^proxy.
! !    

!SimpleActor methodsFor: 'acesss' stamp: 'ajlopez 10/11/2014 18:36'!
object
    ^object.
! !

!SimpleActor methodsFor: 'setters' stamp: '10/11/2014 18:36'!
object: anObject
    object := anObject.
! !

!SimpleActor methodsFor: 'acesss' stamp: 'ajlopez 10/11/2014 18:38'!
doesNotUnderstand: aMessage
    ^object perform: aMessage selector with: aMessage arguments delayed: true.
! !
