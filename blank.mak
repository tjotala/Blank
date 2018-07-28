!if !$d(DEBUG)
DEBUG = 0
!endif

!if !$d(BC)
BC = c:\l\tasm40\bin
!endif
AS = $(BC)\tasm
EL = $(BC)\tlink

ASFLAGS = /m5 /ml /p
ELFLAGS = /x
!if $(DEBUG)
ASFLAGS = $(ASFLAGS) /la /zi
ELFLAGS = $(ELFLAGS) /l /s /v
!endif

all : blank.exe

blank.exe : blank.obj
  $(EL) $(ELFLAGS) blank,$<

blank.obj : blank.asm blank.inc
  $(AS) {$(ASFLAGS) blank,$<;}

