; Important note: dasm requires at least one space before the following commands:
; processor, seg, seg.u, org
; Also, compiled games have to have the ".bin" file extension on them to run in Stella.

; Defines the processor type and includes the standard Atari header files.
    processor 6502          ; Defines processor type being used for the assembler.
    include "vcs.h"         ; Includes the vcs.h header file.
    include "macro.h"       ; Includes the macro.h header file.

; Create space for variables in memory.
    seg.u Variables         ; Defines uninitialized segment named Variables.
    org $80                 ; Set origin of segment at beginning of RAM.

; This is just here so I remember how to do variable declaration.
; DataByte .byte ; declare 8-bit value
; DataWord .word ; declare 16-bit value
; DataArray ds 20 ; reserve 20 bytes for array

; Creates segment for the main program.
    seg Program             ; Defines the initialized code segment of the program.
    org $F000               ; Set origin of segment at beginning of cartridge ROM.

; Creates a subroutine for all initialization details.
Initialize                  ; Defines the Initialize subroutine.
    CLEAN_START             ; Calls the CLEAN_START macro from macro.h.

    JMP Initialize          ; Loop.

    org $FFFC               ; Fills ROM out to exactly 4K in size and tell it where to start the program.
    .word Initialize        ; Reset pointer at $FFFC
    .word Initialize        ; Interrupt pointer at $FFFE (unused but apparently a good idea).

; EOF
