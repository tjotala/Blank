;====================================================================
;
; BLANK v0.00
;
; Copyright (c)1994 Tapani J. Otala
; All rights reserved
;
; Usage:
;       [d:][path]BLANK [flags]
;
; Flags:
;       /E[:M|C]        Enable
;
;       /D[:M|C]        Disable
;
;       /U              Unload
;
;       /V              Verbose
;
;       /?, /H          Show brief help message
;
;====================================================================
ideal
p286
smart
warn
jumps
locals
include "blank.inc"
;--------------------------------------------------------------------
;
; Initialized Data
;
;--------------------------------------------------------------------
segment ResData
;
; Current status
;
Status StatusFlags ?
ends

segment InitData
;
; Entry point of the API functions
;
ApiEntryPoint dd ApiHandler
ends
;--------------------------------------------------------------------
;
; Constant Data
;
;--------------------------------------------------------------------
segment ResData
;
; Old interrupt vectors
;
OldInt1CHandler dd ?
;
; Timeout counter
;
Timeout dw DefTimeout
TimeoutSeed dw DefTimeout
;
; Dispatch table for API functions
;
ApiTable dw\
  _ApiGetVersion,\
  _ApiEnable,\
  _ApiDisable,\
  _ApiUnload
ends

segment InitData
;
; Messages
;
GreetMsg db\
  'BLANK v',\
  VerFileMajor + '0',\
  (VerFileMinor / 10) + '0',\
  (VerFileMinor mod 10) + '0',\
  VerFileStage,13,10,\
  'Copyright (c)1994 Tapani J. Otala. All rights reserved.',13,10,\
  13,10,\
  0
         
BadDosVerMsg db\
  'Incorrect DOS version, aborted.',13,10,\
  0

BadSyntaxMsg db\
  'Bad command line syntax.',13,10,10
HelpMsg db\
  'Usage: BLANK [flags]',13,10,\
  'Flags: /E[:M|C]     Enable',13,10,\
  '       /D[:M|C]     Disable',13,10,\
  '       /U           Unload',13,10,\
  '       /V           Verbose',13,10,\
  '       /?, /H       Show this help message',13,10,\
  13,10,\
  0

InstallMsg db\
  'Installed.',13,10,\
  0
;
; Command line flags
;
Flags CmdLineFlag\
  <'E',_FlagEnable>,\
  <'D',_FlagDisable>,\
  <'U',_FlagUnload>,\
  <'V',_FlagVerbose>,\
  <'?',_FlagHelp>,\
  <'H',_FlagHelp>,\
  <0,_FlagBadSyntax>
;
; Arguments for some flags
;
MonoDisplayChar db 'M'
ColorDisplayChar db 'C'
ends
;--------------------------------------------------------------------
;
; Uninitialized Data
;
;--------------------------------------------------------------------
segment InitData
;
; PSP segment address
;
PspSeg dw ?
;
; Country information
;
CountryInfo DosCountryInfo ?
ends
;--------------------------------------------------------------------
;
; Stack
;
;--------------------------------------------------------------------
segment InitStack
  dw 256 dup(?)
ends
;--------------------------------------------------------------------
;
; Code
;
;--------------------------------------------------------------------
segment ResCode
assume cs:ResCode,ds:nothing,es:nothing,ss:nothing
;
; Int1CHandler
; 
; Purpose:
;       This function is linked to the timer tick interrupt 1Ch.  It
;       is invoked 18.2 times per second; its purpose is to update the
;       timeout counter.
; In:
;       none
; Out:
;       none, all registers and flags must be preserved
;
proc Int1CHandler far
        push ax ds
        mov ax,DGROUP
        mov ds,ax
        assume ds:DGROUP
        cmp [Timeout],0
        je @@Exit
        dec [Timeout]
        jnz @@Exit
@@Exit:
        pop ds ax
        assume ds:nothing
        jmp [OldInt1CHandler]
endp
;
; ApiHandler
; 
; Purpose:
;       Handles API calls
; In:
;       Func = function number
;       additional parameters on stack depending on function
; Out:
;       cf = set if error:
;               al = error code
;       cf = cleared if no error
;               register values depending on function
;
cproc ApiHandler far
arg @@Func:word
uses ds
        mov ax,DGROUP
        mov ds,ax
        assume ds:DGROUP
        mov bx,[@@Func]
        cmp bx,ApiLast
        jae @@BadFunction
        shl bx,1
        jmp [ApiTable + bx]
@@BadFunction:
        mov ax,ErrorBadFunction
        stc
        ret
        assume ds:nothing
endp
;
; _ApiGetVersion
;
; Purpose:
;       Returns the version number
; In:
;       none
; Out:
;       ax = API version number; al = minor, ah = major
;       dx = file version number; dl = minor, dh = major
;       cf = cleared
;
cproc _ApiGetVersion far
        assume ds:DGROUP
        mov ax,(VerApiMajor shl 8) or VerApiMinor
        mov dx,(VerFileMajor shl 8) or VerFileMinor
        clc
        ret
endp
;
; _ApiEnable
;
; Purpose:
;       Enables the screen saver
; In:
;       Display = ID(s) of the displays to enable
; Out:
;       cf = cleared
;
cproc _ApiEnable far
arg @@Display:byte
        assume ds:DGROUP
        mov al,[@@Display]
        setflag [Status],al
        clc
        ret
endp
;
; _ApiDisable
;
; Purpose:
;       Disables the screen saver
; In:
;       none
; Out:
;       cf = cleared
;
cproc _ApiDisable far
arg @@Display:byte
        assume ds:DGROUP
        mov al,[@@Display]
        not al
        maskflag [Status],al
        clc
        ret
endp
;
; _ApiUnload
;
; Purpose:
;       Unloads the screen saver
; In:
;       none
; Out:
;       cf = cleared
;
cproc _ApiUnload far
        assume ds:DGROUP
        clc
        ret
endp
ends

segment InitCode
assume cs:InitCode,ds:DGROUP,es:nothing,ss:DGROUP
;
; CharToUpper
; 
; Purpose:
;       Converts a character to uppercase.
; In:
;       Char = character to convert
; Out:
;       al = character converted to uppercase
;
pproc CharToUpper near
arg @@Char:byte
        mov al,[@@Char]
        cmp al,'a'
        jb @@Ok
        cmp al,'z'
        jbe @@ConvertLow
        cmp al,128
        jb @@Ok
        call [CountryInfo.CaseMapFunc]
        jmp @@Ok
@@ConvertLow:
        sub al,'a' - 'A'
@@Ok:
        ret
endp
;
; DisplayString
; 
; Purpose:
;       Displays a string on the standard output.
; In:
;       String -> string to display
; Out:
;       none
;
pproc DisplayString near
arg @@String:far ptr byte
uses ds,di
        cld
        les di,[@@String]
        sub al,al
        mov cx,-1
        repnz scasb
        not cx
        dec cx
        lds dx,[@@String]
        mov bx,1
        doscall DosWriteToFile
        ret
endp
;
; ParseCmdLine
; 
; Purpose:
;       Parses a command line.
; In:
;       CmdLine -> command line to parse
; Out:
;       cf = set if error:
;               ax = error code
;       cf = cleared if no error
;
pproc ParseCmdLine near
arg @@CmdLine:far ptr byte
uses si
;
; Begin parsing loop
;
@@BeginLoop:
        cld
        les si,[@@CmdLine]
;
; Skip until something tangible
;
@@Loop:
        seges lodsb
irp SkipChar,<' ',9>        
        cmp al,SkipChar
        je @@Loop
endm
        or al,al
        jz @@Exit
        cmp al,13
        je @@Exit

irp FlagPrefix,<'/','-'>
        cmp al,FlagPrefix
        je @@Flag
endm
        mov al,ErrorBadSyntax
        stc
        jmp @@Exit
;
; Parse a flag
;
@@Flag:
        seges lodsb
        mov [word low @@CmdLine],si
        call CharToUpper pascal,ax
        lea si,[Flags]
@@FlagLoop:
        mov ah,[(CmdLineFlag ds:si).Flag]
        or ah,ah
        jz @@FlagFound
        cmp al,ah
        je @@FlagFound
        add si,size CmdLineFlag
        jmp @@FlagLoop
@@FlagFound:
        call [(CmdLineFlag ds:si).Parser] pascal,[@@CmdLine]
        mov [word low @@CmdLine],ax
        mov [word high @@CmdLine],dx
        jnc @@BeginLoop
@@Exit:
        ret
endp
;
; GetDisplayArg
; 
; Purpose:
;       Gets the display ID, if any
; In:
;       Args -> arguments
; Out:
;       cf = set if error:
;               al = error code
;       cf = cleared if no error:
;               dx:ax = arguments
;
pproc GetDisplayArg near
arg @@Args:far ptr byte
        mov cx,mask SfMonoEnabled or mask SfColorEnabled
        les bx,[@@Args]
        mov al,[es:bx]
irp ArgSep,<':','='>
        cmp al,ArgSep
        je @@Found
endm
        clc
        jmp @@Exit
@@Found:
        inc bx
        mov al,[es:bx]
        call CharToUpper pascal,ax
        mov cx,mask SfMonoEnabled
        cmp al,[MonoDisplayChar]
        je @@Ok
        mov cx,mask SfColorEnabled
        cmp al,[ColorDisplayChar]
        jne @@BadParameter
@@Ok:
        mov dx,es
        mov ax,bx
        inc ax
        jmp @@Exit
@@BadParameter:        
        mov ax,ErrorBadParameter
        stc
@@Exit:
        ret
endp
;
; _FlagEnable
; 
; Purpose:
;       Enables the screen saver
; In:
;       Args -> arguments
; Out:
;       cf = set if error:
;               al = error code
;       cf = cleared if no error:
;               dx:ax = arguments
;
pproc _FlagEnable near
arg @@Args:far ptr byte
        call GetDisplayArg pascal,[@@Args]
        jc @@Exit
        mov [word low @@Args],ax
        call [ApiEntryPoint] c,ApiEnable,cx
        jc @@Exit
        les ax,[@@Args]
        mov dx,es
@@Exit:        
        ret
endp
;
; _FlagDisable
; 
; Purpose:
;       Disables the screen saver
; In:
;       Args -> arguments
; Out:
;       cf = set if error:
;               al = error code
;       cf = cleared if no error:
;               dx:ax = arguments
;
pproc _FlagDisable near
arg @@Args:far ptr byte
        call GetDisplayArg pascal,[@@Args]
        jc @@Exit
        mov [word low @@Args],ax
        call [ApiEntryPoint] c,ApiDisable,cx
        jc @@Exit
        les ax,[@@Args]
        mov dx,es
@@Exit:        
        ret
endp
;
; _FlagUnload
; 
; Purpose:
;       Unloads the resident portion.
; In:
;       Args -> arguments
; Out:
;       cf = set if error:
;               al = error code
;       cf = cleared if no error:
;               dx:ax = arguments
;
pproc _FlagUnload near
arg @@Args:far ptr byte
        call [ApiEntryPoint] c,ApiUnload
        jc @@Exit
        les ax,[@@Args]
        mov dx,es
@@Exit:        
        ret
endp
;
; _FlagVerbose
; 
; Purpose:
;       Sets the verbose flag.
; In:
;       Args -> arguments
; Out:
;       cf = set if error:
;               al = error code
;       cf = cleared if no error:
;               dx:ax = arguments
;
pproc _FlagVerbose near
arg @@Args:far ptr byte
        setflag [Status],SfVerbose
        les ax,[@@Args]
        mov dx,es
        clc
        ret
endp
;
; _FlagHelp
; 
; Purpose:
;       Shows the help text.
; In:
;       Args -> arguments
; Out:
;       cf = set if error:
;               al = error code
;       cf = cleared if no error:
;               dx:ax = arguments
;
pproc _FlagHelp near
arg @@Args:far ptr byte
        call DisplayString pascal,ds offset HelpMsg
        mov ax,ErrorOk
        stc
        ret
endp
;
; _FlagBadSyntax
; 
; Purpose:
;       Shows the bad syntax text.
; In:
;       Args -> arguments
; Out:
;       cf = set if error:
;               al = error code
;       cf = cleared if no error:
;               dx:ax = arguments
;
pproc _FlagBadSyntax near
arg @@Args:far ptr byte
        call DisplayString pascal,ds offset BadSyntaxMsg
        mov ax,ErrorBadSyntax
        stc
        ret
endp
;
; Main
;
; Purpose:
;       Called by DOS when the application begins execution.
; In:
;       ds, es = PSP segment address
;       ss:sp -> initial stack
; Out:
;       none, terminates via DOS function call 0x4c or 0x31
;
pproc Main far
        mov ax,DGROUP
        mov ds,ax
        mov [PspSeg],es                 ; save PSP address for future use
;
; Greet user
;
        call DisplayString pascal,ds offset GreetMsg
;
; Investigate environment
;
        doscall DosGetVersion
        xchg al,ah
        cmp ax,(3 shl 8) + 30           ; at least 3.3?
        jae @@DosOk
        call DisplayString pascal,ds offset BadDosVerMsg
        int 20h
@@DosOk:        
        lea dx,[CountryInfo]
        doscall DosGetCurrentCountryInfo
;
; Parse command line
;
        call ParseCmdLine pascal,es 81h
        jc @@Exit

;
; Remain resident
;
        call DisplayString pascal,ds offset InstallMsg
        mov dx,InitData
        sub dx,ResCode
        add dx,256 / 16                 ; add PSP size
        mov al,ErrorOk
        doscall DosTerminateAndStayResident
@@Exit:
        doscall DosTerminate
endp
ends
end Main

