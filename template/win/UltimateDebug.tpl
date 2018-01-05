#TEMPLATE(UltimateDebug,'UltimateDebug Template (v17.09.06.1)'),FAMILY('ABC'),FAMILY('Clarion'),FAMILY('cw20')
#!-----------------------------------------------------------------------------------------------------
#SYSTEM
#EQUATE(%CLSkelTPLVersion,'17.09.06.1, Released 2017-09-06')
#!
#! SystemIdle (Global Extension)
#!
#EXTENSION(UltimateDebugGlobal, 'UltimateDebug (Global Extension)'), APPLICATION(ProcedureInfoExtension(UltimateDebugGlobal))
#PREPARE
  #INSERT(%CalcFamily, %CLSkelFamily)
#ENDPREPARE
#!
#!#BOXED('Information')
#!#INSERT(%CopyrightInfo)
#!#ENDBOXED
#!
#DISPLAY
#PROMPT('Disable Ultimate Debug template',CHECK),%CLSkelAppDisable,AT(10),DEFAULT(0)
#DISPLAY
#!-------------------------------------------------------------------------
#! RA.2014.03.28 - Added Debuger code generation options prompts.
#! Ties in with the additional templates for mass debugging procedures.
#!-------------------------------------------------------------------------
#BUTTON('Ultimate Debug &Generation Options'),PROP(PROP:FontColor,7B0012H),PROP(PROP:FontStyle,400),AT(,,180,20)
#DISPLAY('UltimateDebug'),AT(10,0),PROP(PROP:FontStyle,700),PROP(PROP:FontName,'Tahoma')
#DISPLAY('Version ' & %CLSkelTPLVersion),AT(10,10),PROP(PROP:FontStyle,700),PROP(PROP:FontName,'Tahoma')
#DISPLAY('')
#DISPLAY('')
#DISPLAY('')
#DISPLAY('')
#SHEET,AT(,,288),HSCROLL
#TAB('General')
  #BOXED(' Ultimate Debug Generation Options '),AT(,,280)
    #ENABLE(~%CLSkelAppDisable)
      #BOXED(' Application Generation Options ')
        #DISPLAY('')
        #PROMPT('Generate global object?',CHECK),%gGenGlobalObject,DEFAULT(1),AT(10)
        #!ENABLE(%gGenGlobalObject)
        #!ENDENABLE     
        #DISPLAY('')
        #PROMPT('Generate procedure-level objects?',CHECK),%gGenProcedureLevelObject,DEFAULT(1),AT(10)
        #!ENABLE(%gGenProcedureLevelObject)
        #!ENDENABLE             
        #DISPLAY('')
        #PROMPT('Create global information procedure? ',CHECK),%gDumpTpl,DEFAULT(0),AT(10)
        #ENABLE(%gDumpTpl)
          #PROMPT('Dump global information? '          ,CHECK),%zDumpTpl,DEFAULT(0),AT(25)
        #ENDENABLE
        #DISPLAY('')
        #PROMPT('Create global variables procedure? '  ,CHECK),%gDumpVar,DEFAULT(0),AT(10)
        #ENABLE(%gDumpVar)
          #PROMPT('Dump global variables? '            ,CHECK),%zDumpVar,DEFAULT(0),AT(25)
        #ENDENABLE
        #PROMPT('Enable LineWrap (# of Chars)',@N_3),%LineWrap,DEFAULT(0)
        #DISPLAY('')
      #ENDBOXED
    #ENDENABLE
  #ENDBOXED
  #!#DISPLAY('Copyright © 1999-2014 by Roberto Artigas')
#ENDTAB
#TAB('Proc Info')
  #BOXED(''),AT(,,280)
    #DISPLAY('')
    #ENABLE(~%CLSkelAppDisable)
      #PROMPT('Disable This Feature',CHECK),%DisableProcedureDebug,AT(10),DEFAULT(0)
      #ENABLE(~%DisableProcedureDebug)
          #PROMPT ('Alert Key:',KEYCODE),%UDProcAlert,DEFAULT('CtrlShiftP'),AT(100,,180) 
          #PROMPT ('Report Keystate:',@S200),%UDReportKeyState,DEFAULT('KeyStateUD:Shift'),AT(100,,180)
          #DISPLAY('')
          #DISPLAY('Available Keystates are:')
          #DISPLAY('KeyStateUD:Shift')                         
          #DISPLAY('KeyStateUD:Ctrl')                          
          #DISPLAY('KeyStateUD:Alt') 
          #DISPLAY('')
          #DISPLAY('You can use combinations by adding them.')
          #DISPLAY('Example:')
          #DISPLAY('KeyStateUD:Shift + KeyStateUD:Ctrl')   
          #DISPLAY('')
          #DISPLAY('For continuous display of Procedure Info in your Debug View window')                       
          #PROMPT (' Display Entry and Exit into procedure',CHECK),%UDEntryExit,AT(10),DEFAULT(1)
      #ENDENABLE
    #ENDENABLE
    #DISPLAY('')
  #ENDBOXED                       
#ENDTAB
#INSERT(%TabPurpose1) 
#INSERT(%TabInstructions1)
#INSERT(%TabLimitations1)
#INSERT(%TabTesting1)
#ENDSHEET
#ENDBUTTON
#!-------------------------------------------------------------------------
#DISPLAY
#SHEET,AT(,,288),HSCROLL
#TAB('General')
  #DISPLAY
  #PROMPT('Global object:',@S40),%CLSkelGlobalClass,AT(90,,95,10),REQ,DEFAULT('UD')
  #PROMPT('Procedure object:',@S40),%CLSkelProcedureClass,AT(90,,95,10),REQ,DEFAULT('UDP')  
  #PROMPT('This is part of a Multi-DLL program',CHECK),%CLSkelMultiDLL,AT(90),DEFAULT(0)
  #ENABLE(%CLSkelMultiDLL=1),CLEAR
    #PROMPT('Declaration:',DROP('Declared in another App[0]|Declared in this app[1]')),%CLSkelMultiDLLData,DEFAULT(0),AT(90,,95,10)
  #ENDENABLE
  #BOXED('Settings'),WHERE(%ProgramExtension = 'EXE')
      #ENABLE(%ProgramExtension = 'EXE')
          #PROMPT('Prefix:',@S20),%CLDebugPrefix,DEFAULT('!')
          #PROMPT('Turn Debug Off Variable:',@S20),%DebugOffVariable,DEFAULT(0)
          #PROMPT('Save To File Flag:',@S40),%SaveToFileVariable,DEFAULT(0)
          #PROMPT('Log File Name:',@S120),%CLLogFileName,DEFAULT('DebugLog.txt') 
          #PROMPT('Do Not Split Lines',CHECK),%DoNotSplitLines,DEFAULT(1)  
          #ENABLE(%DoNotSplitLines = 0)
            #PROMPT('  Characters Per Line:',@N3),%LineWrap,DEFAULT(80)
          #ENDENABLE
      #ENDENABLE
  #ENDBOXED
  #INSERT(%TabCopyright)
#ENDTAB 
#INSERT(%TabPurpose)
#INSERT(%TabInstructions)
#INSERT(%TabContributors)
#INSERT(%TabClarionVer)
#INSERT(%TabTesting)
#ENDSHEET
#!
#!-------------------------------------------------------------------------
#!-------------------------------------------------------------------------
#ATSTART
  #DECLARE(%CLSkelDataExternal)
  #DECLARE(%CLSkelProcedureClassProfile)
  #SET(%CLSkelProcedureClassProfile,%CLSkelProcedureClass & 'Profile')
  #IF(%CLSkelMultiDLL=1 AND %CLSkelMultiDLLData=0)
    #SET(%CLSkelDataExternal,',EXTERNAL,DLL(dll_mode)')
  #ENDIF
  #DECLARE (%ListFile),Multi,Unique
  #IF(varexists(%DefaultExport)<>1)
    #Declare(%DefaultExport)
  #EndIf
  #If(%DefaultGenerate=1 or %DefaultExport=1)
    #FOR(%File)
      #ADD (%ListFile,%File)
    #ENDFOR
  #ELSE
    #FOR(%File)
      #If(%OverrideGenerate=1)
        #ADD (%ListFile,%File)
      #EndIf
      #If (%ProgramExtension='EXE')
        #IF ((Instring('CONFIG,',upper(%FileUserOptions),1,1) <> 0) or (Instring(',CONFIG',upper(%FileUserOptions),1,1) <> 0) or (upper(%FileUserOptions) = 'CONFIG'))
          #ADD (%ListFile,%File)
        #ENDIF
      #ENDIf
    #ENDFOR
    #FOR (%Procedure)    
      #ADD (%ListFile,%Primary)
      #FOR(%Secondary)
        #ADD (%ListFile,%Secondary)
      #ENDFOR
      #FOR(%OtherFiles)
        #ADD (%ListFile,%OtherFiles)
      #ENDFOR
      #FOR(%ActiveTemplate)
        #FOR(%ActiveTemplateInstance)
          #ADD (%ListFile,%Primary)
          #FOR(%Secondary)
            #ADD (%ListFile,%Secondary)
          #ENDFOR
        #ENDFOR
      #ENDFOR
    #ENDFOR
    #LOOP,Times(10)
      #FOR (%ListFile)
        #FIX(%File,%ListFile)
        #FOR(%Relation)
           #IF((%FileRelationType='1:MANY') and ((%RelationConstraintUpdate <> '') or (%RelationConstraintDelete<> '')))
             #ADD (%ListFile,%Relation)
           #ENDIF
        #ENDFOR
      #ENDFOR
    #ENDLOOP
  #ENDIF
#ENDAT

#AT(%AfterGlobalIncludes),WHERE(~%CLSkelAppDisable)
  INCLUDE('UltimateDebug.INC'),ONCE 
#ENDAT

#AT(%CustomGlobalDeclarations),WHERE(~%CLSkelAppDisable)
  #INSERT(%CalcFamily, %CLSkelFamily)
  #IF(%CLSkelFamily='LEGACY')
  #PROJECT('UltimateDebug.CLW')
  #ENDIF
#ENDAT

#AT(%GlobalData),WHERE(~%CLSkelAppDisable)
CLSkel_TplVersion    CSTRING('v%CLSkelTPLVersion')%CLSkelDataExternal
 #IF(%CLSkelMultiDLL=1 AND %CLSkelMultiDLLData=0)  
udb_Settings                GROUP,PRE(udb_Settings),external,dll(1)
DebugOff                            BOOL(FALSE)
DebugPrefix                         STRING(20)
SaveToFile                          BOOL(FALSE)
ASCIIFileName                       STRING(100)
DebugNoCR                           BYTE(FALSE)
LineWrap                            BYTE(TRUE)
ModuleName                          STRING(100)
AppName                             STRING(100)
Modified                            STRING(26)
                            END
 
 #ELSE
udb_Settings                GROUP,PRE(udb_Settings)
DebugOff                            BOOL(FALSE)
DebugPrefix                         STRING(20)
SaveToFile                          BOOL(FALSE)
ASCIIFileName                       STRING(100)
DebugNoCR                           BYTE(FALSE)
LineWrap                            BYTE(TRUE)
ModuleName                          STRING(100)
AppName                             STRING(100)
Modified                            STRING(26)
                            END
 #ENDIF 
#IF(%gGenGlobalObject=1)
#INSERT(%DeclareGlobalClass,%CLSkelDataExternal) 
#ENDIF 
#ENDAT

#AT(%DLLExportList),WHERE(%CLSkelMultiDLL=1 AND %CLSkelMultiDLLData=1 AND ~%CLSkelAppDisable)
  $CLSkel_TplVersion  @?
  TYPE$udb_Settings     @?
  $udb_Settings     @?

#ENDAT

#AT(%ProgramSetup),WHERE(~%CLSkelAppDisable),PRIORITY(0001)
#COMMENT(90)
#IF(%ProgramExtension = 'EXE')
  udb_Settings.DebugOff      =  %DebugOffVariable
  udb_Settings.DebugPrefix   =  '%CLDebugPrefix'
  udb_Settings.SaveToFile    =  %SaveToFileVariable
  udb_Settings.ASCIIFileName =  '%CLLogFileName'
  udb_Settings.DebugNoCR     =  %DoNotSplitLines
  udb_Settings.LineWrap      =  %LineWrap
  %CLSkelGlobalClass.INIT('global',udb_settings)
#ENDIF
  #IF(%gDumpTpl AND %zDumpTpl)                                                #! Dump GLOBAL information - end
DebugABCGlobalInformation_%Application()      #<! Dump GLOBAL information
  #END                                                                        #! Dump GLOBAL information - begin
  #IF(%gDumpVar AND %zDumpVar)                                                #! Dump GLOBAL data - begin
DebugABCGlobalVariables_%Application()        #<! Dump GLOBAL variables
  #ENDIF                                                                      #! Dump GLOBAL data - end
#COMMENT(60)
#ENDAT
#!-------------------------------------------------------------------------
#! RA.2010.12.10 - Additional procedures to dump basic information.
#! Nice to know where everything is and what global templates are being used.
#!-------------------------------------------------------------------------
#AT (%GlobalMap),PRIORITY(9000)
#!  #IF(~%CLSkelAppDisable)
    #INDENT(-5)

#COMMENT(90)
DebugABCGlobalInformation_%Application PROCEDURE() #<! DEBUG Prototype
DebugABCGlobalVariables_%Application PROCEDURE() #<! DEBUG Prototype
#COMMENT(60)

    #INDENT(+5)
#!  #ENDIF
#ENDAT
#!-------------------------------------------------------------------------
#AT(%ProgramProcedures),WHERE(~%CLSkelAppDisable),PRIORITY(9000)
 
!BOE: DEBUG Global
DebugABCGlobalInformation_%Application PROCEDURE()
#IF(~%gGenProcedureLevelObject)
#INSERT(%DeclareClass)
#ENDIF
                     
  CODE
  
  %CLSkelProcedureClass.Init('DebugABCGlobalInformation_%Application',udb_Settings)
    
  
 #IF(~%CLSkelAppDisable)
 #IF(%gGenProcedureLevelObject = 1)
  #IF(%gDumpTpl)
  %CLSkelProcedureClass.Debug('----------------> APPLICATION INFORMATION')
  %CLSkelProcedureClass.Debug('Information Generated on: '& FORMAT(TODAY(),@D010) & ' - ' & FORMAT(CLOCK(),@T04))
    #IF(%ProgramExtension = 'EXE')
  %CLSkelProcedureClass.Debug('CW Version: Lib ' & system{prop:libversion} & ' Exe ' & system{prop:exeversion} & '')
    #ENDIF
  %CLSkelProcedureClass.Debug('Application Name: %Application ')
    #IF (%ApplicationDebug = %True)
  %CLSkelProcedureClass.Debug('Compiled in DEBUG mode.')
    #ENDIF
    #IF (%ApplicationLocalLibrary = %TRUE)
  %CLSkelProcedureClass.Debug('Compiled with LOCAL option.')
    #ENDIF
    #IF (%Target32 = %True)
  %CLSkelProcedureClass.Debug('Application is 32 bits.')
    #ELSE
  %CLSkelProcedureClass.Debug('Application is 16 bits.')
    #ENDIF
  %CLSkelProcedureClass.Debug('First procedure: %FirstProcedure')
  %CLSkelProcedureClass.Debug('Program Extension: %ProgramExtension')
  %CLSkelProcedureClass.Debug('Dictionary Name: %DictionaryFile')
  %CLSkelProcedureClass.Debug('Installation Path: ' & LONGPATH(PATH()))
    #IF(ITEMS(%ApplicationTemplate))
  %CLSkelProcedureClass.Debug('----------------> GLOBAL TEMPLATES')
      #FOR(%ApplicationTemplate)
  %CLSkelProcedureClass.Debug('Global Templates: %ApplicationTemplate ')
      #ENDFOR
    #ENDIF
  %CLSkelProcedureClass.Debug('----------------> ')
  #ENDIF
 #ENDIF 
 #ENDIF
  RETURN

DebugABCGlobalVariables_%Application PROCEDURE()

#INSERT(%DeclareClass)

  CODE
  
  %CLSkelProcedureClass.Init('DebugABCGlobalVariables_%Application',udb_Settings)
  
 #IF(~%CLSkelAppDisable)
  #IF(%gDumpVar)
    #DECLARE (%Prefix)
    #DECLARE (%VarName)
    #DECLARE (%PrefixStart)
    #DECLARE (%PrefixEnd)
    #DECLARE (%DataStmt)
  %CLSkelProcedureClass.Debug('----------------> GLOBAL VARIABLES')
    #FOR(%GlobalData)
      #SET(%DataStmt,QUOTE(%GlobalDataStatement))
      #IF (INSTRING('QUEUE',%GlobalDataStatement,1,1) OR INSTRING('GROUP',%GlobalDataStatement,1,1))
        #SET(%PrefixStart,INSTRING('PRE(',%GlobalDataStatement,1,1)+4)
        #SET(%PrefixEnd  ,INSTRING(')',%GlobalDataStatement,1,%PrefixStart))
        #IF (%PrefixStart)
          #SET(%Prefix,SUB(%GlobalDataStatement, %PrefixStart, %PrefixEnd-%PrefixStart) & ':')
          #IF (LEN(%Prefix) = 1)
            #SET(%Prefix,'')
          #ENDIF
        #ENDIF
  %CLSkelProcedureClass.Debug('Only the active record of a group or queue is displayed.')
#!  %CLSkelProcedureClass.Debug('Global data: %[23]GlobalData %[17]DataStmt')  ! & ' Records: ' & RECORDS(%GlobalData))
  %CLSkelProcedureClass.Debug('Global data: %[23]GlobalData %[17]DataStmt')
      #ELSE
        #IF (INSTRING('END',%GlobalDataStatement,1,1) OR INSTRING('FILE',%GlobalDataStatement,1,1))
          #SET (%Prefix,'')
  %CLSkelProcedureClass.Debug('Global Data: %[23]GlobalData %[17]DataStmt')
        #ELSE
	  #! RA.2014.04.19 - No ARRAYS are allowed or supported. 
	  #IF(INSTRING(',DIM',UPPER(%DataStmt))>0)
  %CLSkelProcedureClass.Debug('Global Data: %[23]GlobalData is an ARRAY variable and NOT SUPPORTED.')
	  #ELSIF(INSTRING('&',UPPER(%DataStmt))>0) #! RA.2014.04.27 - Reference variable
  %CLSkelProcedureClass.Debug('Global Data: %[23]GlobalData is a REFERENCE variable and NOT SUPPORTED.')          
          #ELSE  
  %CLSkelProcedureClass.Debug('Global Data: %[23]GlobalData %[17]DataStmt Value: ''' & CLIP(%Prefix%GlobalData) & '''')
          #ENDIF
	  #! RA.2014.04.19 - No ARRAYS are allowed or supported. 
        #ENDIF
      #ENDIF
    #ENDFOR
  %CLSkelProcedureClass.Debug('---------------->')
  #ENDIF
 #ENDIF
  RETURN
!EOE: DEBUG Global

#ENDAT

#AT(%LocalDataAfterClasses),WHERE(%ProcStillNeedsUltDB()),PRIORITY(100),DESCRIPTION('UltimateDebugger Object')
#IF(%gGenProcedureLevelObject = 1)
#INSERT(%DeclareClass)
#ENDIF
#ENDAT
!
#AT(%DataSection),PRIORITY(100),WHERE(%ProcStillNeedsUltDB()),PRIORITY(100),DESCRIPTION('UltimateDebugger Object')
#IF(%gGenProcedureLevelObject = 1)
#INSERT(%DeclareClass)
#ENDIF
#ENDAT
!
#AT(%DataSectionBeforeWindow),WHERE(%ProcStillNeedsUltDB()),PRIORITY(100),DESCRIPTION('UltimateDebugger Object')
#IF(%gGenProcedureLevelObject = 1)
#INSERT(%DeclareClass)
#ENDIF
#ENDAT
!
#AT(%DeclarationSection),WHERE(%ProcStillNeedsUltDB()),PRIORITY(100),DESCRIPTION('UltimateDebugger Object')
#IF(%gGenProcedureLevelObject = 1)
#INSERT(%DeclareClass)
#ENDIF
#ENDAT


#!---------------------------------------------------------------------
#AT(%ProcessedCode),WHERE(~%CLSkelAppDisable),PRIORITY(1) 
#!IF(%CLSkelFamily = 'LEGACY')
    #INSERT(%ShowEnteringTheProcedure)
#!ENDIF
#ENDAT
#!---------------------------------------------------------------------
#AT(%ProcedureSetup),WHERE(~%CLSkelAppDisable),PRIORITY(1)
  #IF(%CLSkelFamily = 'LEGACY' AND (%ProcedureTemplate = 'UnivProcess' OR %ProcedureTemplate ='UnivReport' OR %ProcedureTemplate = 'UnivAbcReport'))
    #INSERT(%ShowEnteringTheProcedure)
  #ENDIF                       
#ENDAT
#!---------------------------------------------------------------------
#AT(%ProcedureInitialize),WHERE(~%CLSkelAppDisable),PRIORITY(0010)
  #IF(%CLSkelFamily = 'LEGACY')
    #INSERT(%ShowEnteringTheProcedure)
  #ENDIF
#ENDAT
#!---------------------------------------------------------------------
#! RA.2014.06.06 - ABC Support
#AT(%WindowManagerMethodCodeSection,'Init','(),BYTE'),WHERE(~%CLSkelAppDisable),PRIORITY(0010)
  #INSERT(%ShowEnteringTheProcedure)
#ENDAT
#!---------------------------------------------------------------------
#AT(%AfterWindowOpening),WHERE(~%CLSkelAppDisable),PRIORITY(0005)
 #IF(~%CLSkelAppDisable AND ~%DisableProcedureDebug)
  #IF (%Window <> '')
%Window{Prop:Alrt,255} = %UDProcAlert
  #ENDIF
 #ENDIF
#ENDAT         
#!---------------------------------------------------------------------
#AT(%AcceptLoopBeforeEventHandling),WHERE(~%CLSkelAppDisable),PRIORITY(6320) 
#IF(~%CLSkelAppDisable AND ~%DisableProcedureDebug)  
 #IF(%CLSkelFamily = 'LEGACY')
  #IF ((%ProcedureTemplate <> 'Report') AND (%ProcedureTemplate <> 'UnivProcess') AND (%ProcedureTemplate <> 'UnivReport') AND (%ProcedureTemplate <> 'UnivAbcReport') AND (%ProcedureTemplate <> 'QueueProcess') AND (%ProcedureTemplate <> 'ProcessQueue') AND (%ProcedureTemplate <> 'Process')AND (%ProcedureTemplate <> 'Source'))
   IF KEYCODE()=%UDProcAlert  
  #INSERT(%ShowTheProcedureInfo)
     CYCLE
   END
  #ENDIF
 #ENDIF
#ENDIF
#ENDAT 
#!---------------------------------------------------------------------
#! RA.2014.06.06 - ABC support
#!---------------------------------------------------------------------
#AT(%WindowManagerMethodCodeSection,'TakeEvent','(),BYTE'),WHERE(~%CLSkelAppDisable),PRIORITY(6320)
#IF(~%CLSkelAppDisable AND ~%DisableProcedureDebug)
 #IF(%CLSkelFamily = 'ABC')
  #IF ((%ProcedureTemplate <> 'Report') AND (%ProcedureTemplate <> 'UnivProcess') AND (%ProcedureTemplate <> 'UnivReport') AND (%ProcedureTemplate <> 'UnivAbcReport') AND (%ProcedureTemplate <> 'QueueProcess') AND (%ProcedureTemplate <> 'ProcessQueue') AND (%ProcedureTemplate <> 'Process')AND (%ProcedureTemplate <> 'Source'))
   IF KEYCODE()=%UDProcAlert AND EVENT() = Event:PreAlertKey
     CYCLE
   END
   IF KEYCODE()=%UDProcAlert  
  #INSERT(%ShowTheProcedureInfo)
     CYCLE
   END
  #ENDIF
 #ENDIF
#ENDIF
#ENDAT
#!---------------------------------------------------------------------
#!#AT(%ProcessedCode),PRIORITY(9990) 
#!AT(%ProcessedCode),PRIORITY(9990)
#!IF(%CLSkelFamily = 'LEGACY')
  #!INSERT(%ShowLeavingTheProcedure)
 #!ENDIF
 #!ENDAT
#!
#AT(%EndOfProcedure),WHERE(~%CLSkelAppDisable),PRIORITY(9990)
#IF(%CLSkelFamily = 'LEGACY')
  #!INSERT(%ShowLeavingTheProcedure)
#!
 #IF ((%ProcedureTemplate <> 'Report') AND (%ProcedureTemplate <> 'UnivProcess') AND (%ProcedureTemplate <> 'UnivReport') AND (%ProcedureTemplate <> 'UnivAbcReport') AND (%ProcedureTemplate <> 'QueueProcess') AND (%ProcedureTemplate <> 'ProcessQueue') AND (%ProcedureTemplate <> 'Process')AND (%ProcedureTemplate <> 'Source'))
 #ELSE
IF BAND(Keystate(),%UDReportKeyState) 
  #INSERT(%ShowTheProcedureInfo)
END
 #ENDIF
#ENDIF
#ENDAT
#!---------------------------------------------------------------------
#! RA.2014.06.06 - ABC Support
#AT(%WindowManagerMethodCodeSection,'Kill','(),BYTE'),WHERE(~%CLSkelAppDisable),PRIORITY(9990)
 #!INSERT(%ShowLeavingTheProcedure)
#!
 #IF ((%ProcedureTemplate <> 'Report') AND (%ProcedureTemplate <> 'UnivProcess') AND (%ProcedureTemplate <> 'UnivReport') AND (%ProcedureTemplate <> 'UnivAbcReport') AND (%ProcedureTemplate <> 'QueueProcess') AND (%ProcedureTemplate <> 'ProcessQueue') AND (%ProcedureTemplate <> 'Process')AND (%ProcedureTemplate <> 'Source'))
 #ELSE
IF BAND(Keystate(),%UDReportKeyState) 
  #INSERT(%ShowTheProcedureInfo)
END
 #ENDIF
#ENDAT
#!-------------------------------------------------------------------------
#INCLUDE('UltimateDebug.TPW')
#INCLUDE('UltimateDebug2.TPW') #! RA.2014.03.28 - Global Debug ALL procedures
#INCLUDE('UltimateDebug3.TPW') #! RA.2014.04.12 - Tracing Source (*.CLW) files
#INCLUDE('UltimateDebug4.TPW') #! RA.2014.06.04 - John Hickey's procedure track
#!------------------------------------------------------------------------- 
#!------------------------------------------------------------------------- 
#!*****************************************************************************
#GROUP(%ProcStillNeedsUltDB),AUTO  
#IF(~%CLSkelAppDisable)
  #EQUATE(%CustomFlag, 'UltDB|'& %Procedure)
  #IF(~INLIST(%CustomFlag, %CustomFlags))
    #ADD(%CustomFlags, %CustomFlag)
    #RETURN(%True)
  #ENDIF
#ENDIF  
  #RETURN(%False)
#!*****************************************************************************
#GROUP(%DeclareClass,%ExternalAttr='')  
#IF(~%CLSkelAppDisable)
#IF(%gGenProcedureLevelObject = 1)
%[20]CLSkelProcedureClass UltimateDebug%ExternalAttr
#ENDIF
#ENDIF
#!*****************************************************************************
#GROUP(%DeclareGlobalClass,%ExternalAttr='')  
#IF(~%CLSkelAppDisable)
#IF(%gGenGlobalObject = 1)
%[20]CLSkelGlobalClass UltimateDebug%ExternalAttr
#ENDIF
#ENDIF
#!*****************************************************************************
