'From Pharo1.3 of 16 June 2011 [Latest update: #13315] on 29 October 2011 at 3:52:13 pm'!
Object subclass: #HtmlHelloPage
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AjTalk'!
!HtmlHelloPage commentStamp: '<historical>' prior: 0!
A simple hello page.!


!HtmlHelloPage methodsFor: 'render' stamp: 'ajl 10/31/2011 11:01'!
render: html
	html h1: 'Hello, world'! !
