Object subclass: #SimplePoint
	instanceVariableNames: 'x y'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests'!

!SimplePoint class methodsFor: 'creation' stamp: 'ajlopez 10/27/2013 20:06'!
new
    | point |
    point := self basicNew.
    point x: 0.
    point y: 0.
    ^point.
! !    

!SimplePoint methodsFor: 'acesss' stamp: 'ajlopez 10/27/2013 20:06'!
x
    ^x.
! !

!SimplePoint methodsFor: 'acesss' stamp: 'ajlopez 10/27/2013 20:06'!
y
    ^y.
! !    

!SimplePoint methodsFor: 'setters' stamp: 'ajlopez 10/27/2013 20:06'!
x: aValue
    x := aValue.
! !

!SimplePoint methodsFor: 'setters' stamp: 'ajlopez 10/27/2013 20:06'!
y: aValue
    y := aValue.
! !    

