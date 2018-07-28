;
; Set up segmentation
;
; Do this manually instead of using handy TASM features, so we'll
; have more freedom with the segmentation.  Anything beyond RESIDENTDATA
; will be discarded at the time this program goes resident (if at all).
;
segment ResCode word public 'RESIDENTCODE'
ends
segment ResData word public 'RESIDENTDATA'
ends
segment InitData word public 'INITDATA'
ends
segment InitStack para stack 'STACK'
ends
segment InitCode word public 'INITCODE'
ends
group DGROUP ResData,InitData,InitStack
;--------------------------------------------------------------------
;
; Constants
;
;--------------------------------------------------------------------
;
; File Versions
;
VerFileMajor = 0
VerFileMinor = 0
VerFileStage equ 'alpha'
;
; API Versions
;
VerApiMajor = 0
VerApiMinor = 0
;
; Error Codes
;
enum Errors {
  ErrorOk = 0,
  ErrorBadSyntax,
  ErrorBadFunction,
  ErrorBadParameter
}
;
; API Functions
;
enum ApiFunctions {
  ApiGetVersion = 0,
  ApiEnable,
  ApiDisable,
  ApiUnload,
  ApiLast
}
;
; Defaults
;
DefTimeout = 2 * 1092                   ; 2 minutes
;
; DOS Date Formats
;
enum DosDateFormats {
  DdfMonthDayYear = 0,
  DdfDayMonthYear = 1
  DdfYearMonthDay = 2
}
;
; DOS Functions
;
enum DosFunctions {
  DosGetVersion = 30h,
  DosTerminateAndStayResident = 31h,
  DosGetCurrentCountryInfo = 3800h,
  DosWriteToFile = 40h,
  DosTerminate = 4ch
}
;--------------------------------------------------------------------
;
; Data Types
;
;--------------------------------------------------------------------
;
; Status Flag
;
record StatusFlags {
  SfMonoPresent:1=0,
  SfColorPresent:1=0,
  SfMonoEnabled:1=1,
  SfColorEnabled:1=1,
  SfVerbose:1=0
}
;
; Command Line Flag
;
struc CmdLineFlag
  Flag db ?                             ; flag character
  Parser dw ?                           ; -> parser function
ends
;
; DOS Currency Format
;
record DosCurrencyFormat {
  DcfSuffix:1=?,
  DcfNumSpaces:1=?,
  DcfReplaceDecimalPoint:1=?
}
;
; DOS Time Format
;
record DosTimeFormat {
  Dtf24Hour:1=?
}
;
; DOS Country Information (2.11+)
;
struc DosCountryInfo
  DateFormat dw ?                       ; date format
  CurrencySep db 5 dup(?)               ; currency separator
  ThousandsSep db 2 dup(?)              ; thousands separator
  DecimalSep db 2 dup(?)                ; decimal separator
  DateSep db 2 dup(?)                   ; date separator
  TimeSep db 2 dup(?)                   ; time separator
  CurrencyFormat DosCurrencyFormat ?    ; currency format
  DecimalDigits db ?                    ; # of decimal digits in currency
  TimeFormat DosTimeFormat ?            ; time format
  CaseMapFunc dd ?                      ; -> case map function
  ListSep db 2 dup(?)                   ; list separator
  Reserved db 10 dup(?)
ends
;--------------------------------------------------------------------
;
; Macros
;
;--------------------------------------------------------------------
;
; cproc
;
; Purpose:
;       Begin a C-callable procedure.  This is a macro to avoid
;       differences between TASM versions pre-3.2x post-3.2x.
; In:
;       Name = name of the procedure
;       Dist = distance (near or far)
;
macro cproc Name:req,Dist:req
  if ??version ge 320h
    proc Name c Dist
  else
    proc c Name dist
  endif  
endm
;
; pproc
;
; Purpose:
;       Begin a Pascal-callable procedure.  This is a macro to avoid
;       differences between TASM versions pre-3.2x post-3.2x.
; In:
;       Name = name of the procedure
;       Dist = distance (near or far)
;
macro pproc Name:req,Dist:req
  if ??version ge 320h
    proc Name pascal Dist
  else
    proc pascal Name dist
  endif  
endm
;
; doscall
;
; Purpose:
;       Executes a DOS function.
; In:
;       Func = function #
;
macro doscall Func:req
  ifnb <Func>
    if (Func) gt 0ffh
        mov ax,Func
    else
        mov ah,Func
    endif
  endif
        int 21h
endm
;
; mpxcall
;
; Purpose:
;       Executes a multiplex function.
; In:
;       Func = function #
;
macro mpxcall Func:req
  ifnb <Func>
    if (Func) gt 0ffh
        mov ax,Func
    else
        mov ah,Func
    endif
  endif
        int 2fh
endm

