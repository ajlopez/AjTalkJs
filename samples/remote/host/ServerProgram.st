
Smalltalk load: 'Host.st'.

server := Host new.

server listen: 3000.

Global console log: 'Server listening at port 3000'.

