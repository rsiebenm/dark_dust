;*****************************************************************************************************
;+
; NAME:
;       ProgramRootDir
;
; PURPOSE:
;
;       The purpose of this function is to provide a portable way of finding
;       the root directory of a program distribution. It assumes that all
;       data files, utility files, etc. are in sub-directories of the directory where
;       the program files reside. There is no explicit rule specifying where the program
;       file directory must reside on the computer.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       ONEUP:  Set this keyword if you want to start your search one directory
;               *above* where your source program resides (i.e., "../Source").
;               This allows you, for example, to put your source files in a Source
;               directory that it at the same level as your Data directory, Utility
;               directory, etc. See the example below.
;
; EXAMPLE:
;
;       Assume that your application files (and source programs) reside in this root directory:
;
;           ../app
;
;       You have placed a DATA directory immediately under the APP directiory, and a RESOURCES
;       directory immedately under the DATA directory. Your directory structure looks like this:
;
;           ../app                    ; Contains your application and source (*.pro) files.
;           ../app/data               ; Contains your application data files.
;           ...app/data/resources     ; Contains your application resource files.
;
;       The end user can install the APP directory wherever he or she likes. In your
;       program, you will identify the DATA and RESOURCES directory like this:
;
;            ; Get icon image in resources directory.
;            filename = Filepath(Root_Dir=ProgramRootDir(), Subdirectory=['data','resources'], 'myicon.tif')
;
;            ; Get default image in data directory.
;            filename = Filepath(Root_Dir=ProgramRootDir(), Subdirectory='data', 'ctscan.tif')
;
;       Alternatively, you might set up an application directory structure like this:
;
;           ../app                    ; Contains your application files.
;           ../app/source             ; Contains your application source (*.pro) files.
;           ../app/data               ; Contains your application data files.
;           ...app/data/resources     ; Contains your application resource files.
;
;       In this case, you would use the ONEUP keyword to find your data and resource files, like this:
;
;            ; Get icon image in resources directory.
;            filename = Filepath(Root_Dir=ProgramRootDir(/ONEUP), Subdirectory=['data','resources'], 'myicon.tif')
;
;            ; Get default image in data directory.
;            filename = Filepath(Root_Dir=ProgramRootDir(/ONEUP), Subdirectory='data', 'ctscan.tif')
;
;
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, 23 November 2003. Based on program SOURCEROOT, written by
;         Jim Pendleton at RSI (http://www.rsinc.com/codebank/search.asp?FID=35).
;       Added ONEUP keyword. 10 December 2003. DWF.
;-
;*****************************************************************************************************
FUNCTION ProgramRootDir, OneUp=oneup

   ; Return to caller on an error.
   On_Error, 2

   ; Get the current call stack.
   Help, Calls=callStack

   ; Get the name of the calling routine.
   callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]

   ; We don't know if the calling routine is a procedure or a function,
   ; and we don't have a way to get information without knowing this. So,
   ; we are going to try first to see if it is a procedure. If not, we
   ; will try it as a function. Unfortunately, if it is *not* a procedure,
   ; we will cause an error. We have to catch that and handle it silently.

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      Message, /Reset
      thisRoutine = Routine_Info(callingRoutine, /Functions, /Source)
   ENDIF

   IF N_Elements(thisRoutine) EQ 0 THEN $
      thisRoutine = Routine_Info(callingRoutine, /Source)

   ; If there are no path separators, you are here.
   IF ( StrPos(thisRoutine.Path, Path_Sep()) ) EQ -1 THEN BEGIN
      CD, Current=thisDir
      sourcePath = FilePath(thisRoutine.Path, Root_Dir=thisDir)
   ENDIF ELSE BEGIN
      sourcePath = thisRoutine.Path
   ENDELSE

   ; Strip the root directory off the source path.
   root = StrMid(sourcePath, 0, StrPos(sourcePath, Path_Sep(), /Reverse_Search) + 1)

   ; If ONEUP is set, then climb up the root directory by one directory. This will
   ; be the *second* path separator, since the root directory has a path separator
   ; as its end.
   IF Keyword_Set(oneup) THEN BEGIN
      i = Where( Byte(root) EQ (Byte(Path_Sep()))[0], count)

      IF count GE 2 THEN BEGIN
         sourcePath = StrMid(root, 0, StrLen(root)-1)
         root = StrMid(sourcePath, 0, StrPos(sourcePath, Path_Sep(), /Reverse_Search) + 1)
      ENDIF
   ENDIF

   RETURN, root
END

