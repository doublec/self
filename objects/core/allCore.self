 '30.8.0-prerelease2'
 '
Copyright 1992-2014 AUTHORS.
See the legal/LICENSE file for license information and legal/AUTHORS for authors.
'
["preFileIn" self] value


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: allCore InitialContents: FollowSlot'
        
         allCore = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'allCore' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'allCore' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules allCore.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'allCore' -> () From: ( | {
         'ModuleInfo: Module: allCore InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'core'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'allCore' -> () From: ( | {
         'ModuleInfo: Module: allCore InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'allCore' -> () From: ( | {
         'ModuleInfo: Module: allCore InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'allCore' -> () From: ( | {
         'ModuleInfo: Module: allCore InitialContents: InitializeToExpression: (\'30.8.0-prerelease2\')\x7fVisibility: public'
        
         revision <- '30.8.0-prerelease2'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'allCore' -> () From: ( | {
         'ModuleInfo: Module: allCore InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- 'systemStructure
coreObjects
processesAndIO
programmingSupport
ttySupport
defaultPreferences
debugger
prompt
int32and64
systemLog
'.
        } | ) 



 '-- Sub parts'

 bootstrap read: 'systemStructure' From: 'core'
 bootstrap read: 'coreObjects' From: 'core'
 bootstrap read: 'processesAndIO' From: 'core'
 bootstrap read: 'programmingSupport' From: 'core'
 bootstrap read: 'ttySupport' From: 'core'
 bootstrap read: 'defaultPreferences' From: 'core'
 bootstrap read: 'debugger' From: 'core'
 bootstrap read: 'prompt' From: 'core'
 bootstrap read: 'int32and64' From: 'core'
 bootstrap read: 'systemLog' From: 'core'



 '-- Side effects'

 globals modules allCore postFileIn
