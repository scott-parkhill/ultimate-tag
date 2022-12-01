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

PlayerIt        .byte       ; 0 for it, not zero for not it.
PlayerDirection .byte       ; 0 for left, not zero for right.
NPCDirection    .byte       ; 0 for left, not zero for right.

; Creates segment for the main program.
    seg Program             ; Defines the initialized code segment of the program.
    org $F000               ; Set origin of segment at beginning of cartridge ROM.

; Creates a subroutine for all initialization details.
Initialize                  ; Defines the Initialize subroutine.
    CLEAN_START             ; Calls the CLEAN_START macro from macro.h.

    ; Accumulator starts at 0 from the CLEAN_START macro.
    STA NPCDirection        ; Sets the NPC facing left.
    STA PlayerIt            ; Sets the player to it.
    JSR SetItColours        ; Go to subroutine to set the colours for who is it.

    LDA #$9F                ; Loads background colour into the accumulator.
    STA COLUBK              ; Sets the background colour register in the TIA.

    LDA #2                  ; Sets the binary value #%0000_0010, which will turn on VBLANK and VSYNC.
    STA PlayerDirection     ; Sets the player direction to facing right.
    STA VBLANK              ; Sets the register bit value for VBLANK to ON.


; Fall into FrameStart from the Initialize subroutine.
; This subroutine controls setting the VSYNC and VBLANK and handles the
; three mandatory scanlines required for VSYNC. 
FrameStart
    STA VSYNC               ; Sets the register bit value for VSYNC to ON.
                            ; #2 guaranteed to be in register at this point from the Initialize and OverscanPeriod sections.
                            ; VBLANK is set to ON in the Initialize and OverscanPeriod sections.

    ; Outputs the three lines of the VSYNC signal required by the NTSC standard.
    STA WSYNC               ; Strobe the WSYNC register, i.e. signal the CPU to wait for the current
                            ; scanline to finish printing to screen before continuing operations.
    STA WSYNC               ; Strobe.
    STA WSYNC               ; Strobe.
    LDA #0                  ; Put zero in accumulator so VSYNC can be turned off.
    STA VSYNC               ; Turn off VYSNC.
    ; We now fall into the vertical blank period.

; Handles the 37 lines of vertical blank. 76 CPU clock cycles per VBLANK period.
VBlankPeriod
    LDX #37                 ; Store the value 37 into X to decrement down for each line of VBLANK.
                            ; Counting down is better because you save an instruction by being able
                            ; to use the zero bit in the processor status register instead of having
                            ; to do a CMP operation.

VBlankLoop
    STA WSYNC               ; Strobe.
    DEX                     ; Decrement the X register containing our line counter.
    BNE VBlankLoop          ; Loop until VBLANK is complete (i.e. until X == 0).

    ; Turn off VBLANK.
    STX VBLANK              ; Stores X into VBLANK since we know X is zero at this point in the code.

    ; Now fall into the portion with HBLANKS and visible output to TV.

; Handles the 192 scanlines of visible frame, as well as the intervening
; horizontal blanks. 22 CPU cycles of HBLANK followed by 54 cycles of
; drawing to the screen
VisibleFrame
    LDX #192                ; Have X be the line counter for the 192 scanlines of visible frame.

; Set registers for the TIA within 22 CPU cycles of HBLANK before it starts printing to the screen.
HBlankPeriod

; Do logic operations within 54 CPU cycles while the TIA is printing the image to the screen.
PrintingPeriod
    STA WSYNC               ; Strobe.
    DEX                     ; Decrement our line counter.
    BNE HBlankPeriod        ; Wait for the printing period to be done, then start again.

; Handles the 30 lines of overscan, 76 clock cycles each.
OverscanPeriod
    LDA #2                  ; Put 2 into accumulator again in order to switch on VBLANK.
    STA VBLANK              ; Turn on register indicating vertical blank.
    JSR SetItColours        ; Set the colours for who is it during the first overscan period.

    LDX #30                 ; Make X the line counter for the 30 lines of overscan.

OverscanLoop
    STA WSYNC               ; Strobe.
    DEX                     ; Decrement the line counter.
    BNE OverscanLoop        ; Repeat until X = 0.

    JMP FrameStart          ; When the overscan period is complete, start the next frame.

; Subroutine to set the colours for who is it.
SetItColours
    LDA #PlayerIt           ; Load the PlayerIt value into the accumulator.
    BNE ComputerRed         ; Go to ComputerRed if the value is non-zero.

PlayerRed
    STA COLUP1              ; Accumulator is zero at this point; set NPC to black.
    LDA #$32                ; Load red into accumulator..
    STA COLUP0              ; Set player colour register to red.
    RTS                     ; Return.

ComputerRed
    LDA #$32                ; Load red into accumulator.
    STA COLUP1              ; Set NCP to red.
    LDA #0                  ; Load black into accumulator.
    STA COLUP0              ; Set player to black.
    RTS                     ; Return.

; Defines the bitmap for a left-facing sprite.
LeftFacingSprite
    .byte #%00000000
    .byte #%11101110
    .byte #%00100010
    .byte #%00100010
    .byte #%00100010
    .byte #%00110100
    .byte #%00001000
    .byte #%00001000
    .byte #%00111110
    .byte #%01001001
    .byte #%10001001
    .byte #%10001001
    .byte #%00011100
    .byte #%00100010
    .byte #%00100010
    .byte #%00011100

; Defines the bitmap for a right-facing sprite.
RightFacingSprite
    .byte #%00000000
    .byte #%01110111
    .byte #%01000100
    .byte #%01000100
    .byte #%01000100
    .byte #%00101100
    .byte #%00010000
    .byte #%00010000
    .byte #%01111100
    .byte #%10010010
    .byte #%10010001
    .byte #%10010001
    .byte #%00111000
    .byte #%01000100
    .byte #%01000100
    .byte #%00111000

; Sets the ROM's capacity to exactly 4K, i.e. $F000 through $FFFF, and tells it where to start execution.
    org $FFFC               ; Set origin as $FFFC, the reset pointer, to tell where to begin execution.
    .word Initialize        ; Reset pointer at $FFFC, set to Initialize.
    .word Initialize        ; Interrupt pointer at $FFFE (unused but apparently a good idea to set anyway).

; EOF
