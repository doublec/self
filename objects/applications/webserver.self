 '$Revision:$'
 '
Copyright 1992-2011 AUTHORS.
See the legal/LICENSE file for license information and legal/AUTHORS for authors.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         webserver = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules webserver.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision:$'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> () From: ( | {
         'Category: applications\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         webserver <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         about = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'about' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver about.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'about' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         help = '
This is the webserver for Self 4.3.

Register a servlet then start up.

As an example

webserver registerServlet: (webserver exampleServlets fileServlet copy) at: .
webserver start.

fileServlet has a baseDirectory you may wish to set.

When finished, webserver stop.



'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'about' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         shortName = 'A Self Webserver'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'about' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         version = 1.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         deregisterServletAt: u = ( |
            | 
            private removeServletAt: s).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         exampleServlets = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         fileServlet = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets fileServlet.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'~/Sites/\')'
        
         baseDirectory <- '~/Sites/'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         mimeTypes <- bootstrap setObjectAnnotationOf: ( [|d|
	d: dictionary copyRemoveAll.
	d at: ('html') Put: ('text/html').
	d at: ('jpg') Put: ('image/jpg').
	d at: ('mp4') Put: ('video/mp4').
	d at: ('ogg') Put: ('video/ogg').
	d at: ('ogv') Put: ('video/ogg').
	d at: ('webm') Put: ('video/webm').
] value) From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets fileServlet mimeTypes.

CopyDowns:
globals set. copy 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets fileServlet parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         fileExtension: fn = ( |
            | 
            fn findLast: [|:v. :k| v = '.']
               IfPresent: [|:v. :k| fn copyFrom: k+1]
               IfAbsent: [nil]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         filenameFromUrl: u = ( |
             fn.
            | 
            (u = '') ifTrue: [fn: 'index.html'] False: [fn: u]. 
            (fn last = '/') ifTrue: [fn: fn, 'index.html'].
            os_file expand: baseDirectory, fn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handleUrl: u = ( |
             f.
             fn.
             r.
            | 
            fn: filenameFromUrl: u.
            (isUnderBaseDirectory: fn) ifFalse: [^ 'Not Found:', fn].
            f: os_file deadCopy openForReading: fn IfFail: [^ 'Not Found:', fn].
            r: f contents.
            f close.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handleUrl: u Server: webserver Socket: socket = ( |
             sc.
            | 
            sc: statusCode: u.
            (sc = 200 ) ifTrue: [ | fn. contents |
              fn: filenameFromUrl: u.
              contents: handleUrl: u.
              socket writeLine: 'HTTP/1.1 200 OK'.
              socket writeLine: 'Content-Length: ',contents size asString.
              socket writeLine: 'Content-Type: ',(mimeTypes at: (fileExtension: fn) uncapitalizeAll IfAbsent: ['application/octet-stream']).
              socket writeLine: ''.
              socket writeLine: contents
            ] False: [
              socket writeLine: 'HTTP/1.1 ',sc asString,' Error'.
              socket writeLine: ''.
              socket writeLine: 'Error'
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         isUnderBaseDirectory: fn = ( |
            | 
            (os_file expand: baseDirectory) isPrefixOf: fn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         statusCode: u = ( |
             fn.
            | 
            fn: filenameFromUrl: u.
            (isUnderBaseDirectory: fn) ifTrue: [
              (os_file existsAndIsReadable: fn) ifTrue: [ 200] False: [ 404 ]
            ] False: [ 400 ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         lobbyBrowserServlet = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets lobbyBrowserServlet.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets lobbyBrowserServlet parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         appropriateSigilFor: slot = ( |
            | 
            ( slot mirror at: (slot name, ':') IfAbsent: [^ ' = '] ) 
              value = mirrors assignment ifTrue: [^ ' &larr; '].
            ' = ').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         getMirrorAtPath: p = ( |
             o.
            | 
            o: reflect: lobby.
            p do: [|:n| o:(o at: n) contents].
            o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handleUrl: u = ( |
             o.
             r.
             t.
            | 
            u = '' 
              ifTrue: [o: reflect: lobby]
              False: [
                o: getMirrorAtPath: 
                  u asTokensSeparatedByCharactersSatisfying: [|:c | c = '/']].
            htmlFor: o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handleUrl: u Server: webserver Socket: socket = ( |
            | 
            socket writeLine: 'HTTP/1.1 200 OK'.
            socket writeLine: ''.
            socket writeLine: handleUrl: u).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         htmlFor: o = ( |
             r.
            | 

            r: '<html><body><h1>', (o evalName), '</h1><p>'.
            r: r, '<i>', (o comment), '</i><p>'.
            r: r, 'CopyDowns: ', (o copyDowns printString), '<p>'.
            r: r, 'CreatorPath: ', (o creatorPath printString), '<p>'.
            r: r, 'ModuleNames: ', (o moduleNames printString), '<p>'.

            r: r, 'Parents:<ul>'.
            o parentsDo: [|:i| r: r, htmlForSlot: i].
            r: r, '</ul>Slots:<ul>'.
            o withoutAssignmentSlotsDo: [|:i| 
                i isParent ifFalse: [r: r, htmlForSlot: i]].

            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         htmlForSlot: s = ( |
            | 
            '<li><a href="', 
             s key, '/">', 
             s key, '</a>', (appropriateSigilFor: s),
             s value safeName, '</li>').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         isRunning = ( |
            | 
            private continue).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         private = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'private' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver private.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'private' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (false)'
        
         continue <- bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'private' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         debug = bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'private' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handleError: io Code: code Reason: reason = ( |
            | 
            io writeLine: 'HTTP/1.1 ',code asString,' ',reason.
            io writeLine: ''.
            io writeLine: '<html><head><title>',reason,'</title></head>'.
            io writeLine: '<body><p>',reason,'</p></body></html>').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'private' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handleRequest: io = ( |
             url.
            | 
            [|i|
              i: io readLine. 
              i print.
              (i matchesPattern: 'GET *') ifTrue: [
                url: i copyFrom: 5 UpTo: i size - 10].
              i first asByte = 13] whileFalse: [].
            handleUrl: url Server: self Socket: io.
            io close.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'private' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handleUrl: url Server: webserver Socket: socket = ( |
             s.
            | 
            servlets keys do: [|:k| 
              (k isPrefixOf: url) || (k = '') ifTrue: [
                ^ (servlets at: k) handleUrl: (url copyFrom: k size UpTo: url size) Server: self Socket: socket]].
            handleError: socket Code: 404 Reason: 'Servlet Not Found').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'private' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'private' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         registerServlet: s At: urlStub = ( |
            | 
            servlets isNil ifTrue: [servlets: dictionary copy].
            servlets at: urlStub Put: s.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'private' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         removeServletAt: u = ( |
            | 
            servlets removeKey: u.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'private' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (os_file deadCopy)'
        
         serverSocket <- os_file deadCopy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'private' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (dictionary copy)'
        
         servlets <- dictionary copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'private' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         start = ( |
            | startOnPort: 8080).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'private' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: private'
        
         startOnPort: port = ( |
            | 
            continue: true.
            serverSocket closeIfFail: [].
            serverSocket:
              os_file
                openTCPListenerOnPort: port      "HARDWIRED!"
                IfFail: [|:e|
                     (e matchesPattern: '*UNKNOWN 125*')
                  || [e matchesPattern: '*EADDRINUSE*']
                     ifTrue: [error: e, '\n',
                                'Warning: couldn\'t start the rself server process.\n',
                                'The port (', port printString, ') is already in use, ',
                                'probably by another Self server.\n\n',
                                'If you wish to start it, kill the other one off and\n',
                                'evaluate "socketServer start"'.
                              ^self]
                      False: [^error: 'Couldn\'t start self server: ', e]].
            'Server started.' printLine.
            [continue] whileTrue: [| io <- unixGlobals os_file. |
            serverSocket initialize: 'server socket'.
                io: serverSocket acceptConnection.
                (message copy receiver: self 
                              Selector: 'handleRequest:' 
                                  With: io) fork resume.
            ].
            serverSocket close.
            'Server terminated.' printLine.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'private' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         stop = ( |
            | 
            debug ifTrue: ['forcing a stop' printLine].
            serverSocket closeIfFail: [].
            continue: false.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         registerServlet: s At: u = ( |
            | 
            private registerServlet: s At: u).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         start = ( |
            | 
            private start).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         startOnPort: port = ( |
            | private startOnPort: port).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         stop = ( |
            | 
            private stop).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'collection' -> () From: ( | {
         'Category: searching\x7fModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         findLast: eb IfPresent: fb IfAbsent: fail = ( |
             lastKey.
             lastValue.
            | 
            do: [|:v. :k|
                (eb value: v With: k) ifTrue: [ lastValue: v. lastKey: k]
            ].
            lastKey isNil ifFalse: [ ^fb value: lastValue With: lastKey ].
            fail value).
        } | ) 



 '-- Side effects'

 globals modules webserver postFileIn
