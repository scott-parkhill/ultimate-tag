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

PlayerIt            .byte   ; 0000 0000 for it, and 1111 1111 for not it.
PowerupTimer        .byte   ; 1 for timer set, 0 for not.
PowerupCooldown     .byte   ; 1 for timer set, 0 for not.

PlayerDirection     .byte   ; 0 for left, 8 for right (the value to set the reflection bit).
PrintPlayerSprite   .byte   ; 0 for don't print, anything else for print.
PlayerSpriteCount   .byte   ; Memory offset for printing sprites; 16 bit sprites -> 15 as the offset.
PlayerSpriteMap     .byte   ; The bitmap level for the player.
PlayerX             .byte   ; The player's X coordinate. Coordinates are given with the origin at the bottom left of the screen.
PlayerY             .byte   ; The player's Y coordinate.
PlayerPowerup       .byte   ; The player's powerup status. 0 = no powerup, 1 = 1.25x powerup, 2 = 2x powerup.

NpcDirection        .byte   ; 0 for left, 8 for right (the value to set the reflection bit).
PrintNpcSprite      .byte   ; 0 for don't print, anything else for print.
NpcSpriteCount      .byte   ; Memory offset for printing sprites; 16 bit sprites -> 15 as the offset.
NpcSpriteMap        .byte   ; The bitmap level for the Npc.
NpcX                .byte   ; The Npc's X coordinate.
NpcY                .byte   ; The Npc's Y coordinate.
NpcPowerup          .byte   ; The Npc's powerup status.

Sprite              ds 16   ; Array for the sprite data. This is needed in order to save on clock cycles by having the sprite zero-page indexed.

; This logic is a bit weird but done so that branching is on positive. Basically, -1 to 8 = 10, etc.
HalfSecondCounter   .byte   ; Initializes to 29 and re-initializes to 28, as there are 60 frames a second. When zero, half a second has passed.
SecondCounter       .byte   ; Initializes to 59 and re-initializes to 58, as there are 60 frames a second. When zero, a second has passed.
TenSecondCounter    .byte   ; Initializes to 9 and re-initializes to 8. 1 is deducted each second. When zero, 10 seconds have passed.

; Creates segment for the main program.
    seg Program             ; Defines the initialized code segment of the program.
    org $F000               ; Set origin of segment at beginning of cartridge ROM.

; Creates a section for all initialization details.
Initialize                  ; Defines the Initialize section.
    CLEAN_START             ; Calls the CLEAN_START macro from macro.h.

    ; Accumulator starts at 0 from the CLEAN_START macro.
    STA SWACNT              ; Set joystick register for reading.
    STA PlayerIt            ; Sets the player to it.
    STA NpcDirection        ; Sets the Npc facing left.
    STA PrintPlayerSprite   ; Set to don't print.
    STA PrintNpcSprite      ; Set to don't print.
    STA PlayerSpriteMap     ; Set to zeros.
    STA NpcSpriteMap        ; Set to zeros.
    JSR SetItColours        ; Go to subroutine to set the colours for who is it.

    LDX #104                ; Sets X so the sprites sits halfway up the screen.
    STX PlayerY             ; Set player to be halfway up the screen.
    STX NpcY                ; Set NPC to be halfway up the screen.

    LDY #15                 ; Load 15 into Y, the offset for the SpriteData.

; Loads the sprite into RAM. This is done in order to properly "race the beam" in the HBlankLoop.
; Loading the sprite data during the printing period resulted in too many clock cycles used when
; both the player and NPC shared any horizontal spaces (i.e. were printed on the same scanline ever).
; Having it zero-page indexed saves on load clock cycles which makes me race the beam just in time.
SpriteLoop
    LDA SpriteData,Y        ; Load the sprite data at the offset into accumulator.
    STA Sprite,Y            ; Save the sprite data into RAM at the offset amount.
    DEY                     ; Decrease Y.
    BPL SpriteLoop          ; Repeat loop until Y is negative.
    
    LDA #8
    STA PlayerDirection     ; Sets the player direction to facing right.

    LDA #$9F                ; Loads background colour (light blue) into the accumulator.
    STA COLUBK              ; Sets the background colour register in the TIA.

    LDA #2                  ; Sets the binary value #%0000_0010, which will turn on VBLANK and VSYNC.
    STA VBLANK              ; Sets the register bit value for VBLANK to ON.

; Fall into FrameStart from the Initialize section.
; This section controls setting the VSYNC and VBLANK and handles the
; three mandatory scanlines required for VSYNC. 
FrameStart
    LDA #2                  ; Sets accumulator to 2 in order to set the VSYNC bit.
    STA VSYNC               ; Sets the register bit value for VSYNC to ON.

    ; Outputs the three lines of the VSYNC signal required by the NTSC standard.
    STA WSYNC               ; Strobe the WSYNC register, i.e. signal the CPU to wait for the current
                            ; scanline to finish printing to screen before continuing operations.
    STA WSYNC               ; Strobe.
    STA WSYNC               ; Strobe.
    LDA #0                  ; Put zero in accumulator so VSYNC can be turned off.
    STA VSYNC               ; Turn off VYSNC.
    ; We now fall into the vertical blank period.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VBLANK

; Handles the 37 lines of vertical blank. 76 CPU clock cycles per VBLANK period.
VBlankPeriod

    STA GRP0                ; Reset player graphics register. Accumulator is zero here.
    STA GRP1                ; Reset Npc graphics register.

    LDX PlayerDirection     ; Load the player direction into X.
    STX REFP0               ; Save player direction in reflection register.

    LDX NpcDirection        ; Load the Npc direction into X.
    STX REFP1               ; Save the Npc direction into the reflection register.
                 
    LDA #15                 ; Put 15 into accumulator in order to reset player sprite height counts.
    STA PlayerSpriteCount   ; Reset sprite memory offset.
    STA NpcSpriteCount      ; Reset sprite memory offset.

    LDX #192                ; Sets X to the number of lines for the visible frame.

    STA WSYNC               ; Wait for next scanline.

; These two sections (SetPrintPlayerVblank and SetPrintNpcVblank) are so that if the sprites are at the top of the screen,
; their bitmap line value is read into the graphics register now. Otherwise, their heads would be cut off on the very top line
; because of the way that the graphics registers are set in the visible frame part (as done during the HBLANK period).
SetPrintPlayerVblank        
    CPX PlayerY             ; Compare player's Y position with the line number in X.
    BNE SetPrintNpcVblank   ; If the top of the sprite is not inline with current line being printed, skip.
    STX PrintPlayerSprite   ; Set print player sprite to true.
    LDY PlayerSpriteCount   ; Get the player sprite count.
    LDA Sprite,Y            ; Get the bitmap line.
    STA PlayerSpriteMap     ; Save the line to print.
    DEY                     ; Decrement the height count.
    STY PlayerSpriteCount   ; Save the height count.

SetPrintNpcVblank
    STA WSYNC               ; Strobe.
    CPX NpcY                ; Compare NPC's Y position with the line number in X.
    BNE SkipSetNpcPrint     ; If player not at the top of the screen, skip.
    STX PrintNpcSprite      ; Set print NPC sprite to true.
    LDY NpcSpriteCount      ; Get the NPC sprite count.
    LDA Sprite,Y            ; Get the bitmap line.
    STA NpcSpriteMap        ; Save the line to print.
    DEY                     ; Decrement the height counter.
    STY NpcSpriteCount      ; Save the height count.

SkipSetNpcPrint
    STA WSYNC               ; Strobe.

    LDX #34                 ; Store the value 37 into X to decrement down for each line of VBLANK.
                            ; Counting down is better because you save an instruction by being able
                            ; to use the zero bit in the processor status register instead of having
                            ; to do a CMP operation.
                            ; Minus 3 for three WYSNCs in the code.

; With all logic complete, loop until the visible frame portion of the code.
VBlankLoop
    STA WSYNC               ; Strobe.
    DEX                     ; Decrement the X register containing our line counter.
    BNE VBlankLoop          ; Loop until VBLANK is complete (i.e. until X == 0).

    ; Turn off VBLANK.
    STX VBLANK              ; Stores X into VBLANK since we know X is zero at this point in the code.

    ; Now fall into the portion with HBLANKS and visible output to TV.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VISIBLE FRAME

; Handles the 192 scanlines of visible frame, as well as the intervening
; horizontal blanks. 22 CPU cycles of HBLANK followed by 54 cycles of
; drawing to the screen
HBlankInitialize
    LDX #192                ; Have X be the line counter for the 192 scanlines of visible frame. One less because of BMI operation.
                            ; This offset of 1 is to compute the next line of sprites to print.

; Set registers for the TIA within 22 CPU cycles of HBLANK before it starts printing to the screen.
HBlankLoop

    LDA PlayerSpriteMap     ; Load the bitmap line to print for the player.
    STA GRP0                ; Set the graphics register to print that line.

    LDA NpcSpriteMap        ; Load the bitmap line to print the NPC.
    STA GRP1                ; Set the graphics register to print that line.

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Player Sprite Printing
StartSetPlayer
    DEX                     ; Reduce X for the current comparisons against the Y values.
    LDA PrintPlayerSprite   ; Load whether we are currently printing the player's sprite.
    BNE SetPlayerMap        ; If we ARE printing the player, skip to that part. Otherwise, fall through.

SetPrintPlayer        
    CPX PlayerY             ; Compare player's Y position with the line number in X.
    BNE StartSetNpc         ; If the top of the sprite is not inline with current line being printed, skip.
    STX PrintPlayerSprite   ; Set print player sprite to true.

SetPlayerMap
    LDY PlayerSpriteCount   ; Load the player sprite's current memory offset.
    LDA Sprite,Y            ; Load the sprite's bitmap line into the accumulator.
    STA PlayerSpriteMap     ; Save the player's bitmap line to the player's sprite map variable.
    DEY                     ; Decrement the offset for the sprite (as it is upside-down).
    BMI PlayerFinished      ; If the offset is now negative, branch to turn off sprite printing.
    STY PlayerSpriteCount   ; Store the new sprite offset.
    JMP StartSetNpc         ; Jump to start setting up values for the NPC.

PlayerFinished
    INY                     ; Y right now is -1, so increment it to zero.
    STY PrintPlayerSprite   ; Turn off player sprite printing.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NPC Sprite Printing
StartSetNpc
    LDA PrintNpcSprite      ; Load whether we are currently printing the npc's sprite.
    BNE SetNpcMap           ; If we ARE printing the npc, skip to that part. Otherwise, fall through.

SetPrintNpc        
    CPX NpcY                ; Compare player's Y position with the line number in X.
    BNE SpriteSettingDone   ; If the top of the sprite is not inline with current line being printed, skip.
    STX PrintNpcSprite      ; Set print player sprite to true.

SetNpcMap
    LDY NpcSpriteCount      ; Load the player sprite's current memory offset.
    LDA Sprite,Y            ; Load the sprite's bitmap line into the accumulator.
    STA NpcSpriteMap        ; Save the player's bitmap line to the player's sprite map variable.
    DEY                     ; Decrement the offset for the sprite (as it is upside-down).
    BMI NpcFinished         ; If the offset is now negative, branch to turn off sprite printing.
    STY NpcSpriteCount      ; Store the new sprite offset.
    JMP SpriteSettingDone   ; Jump to start setting up values for the NPC.

; TODO
NpcFinished
    INY                     ; Y right now is -1, so increment it to zero.
    STY PrintNpcSprite      ; Turn off player sprite printing.

SpriteSettingDone
    INX                     ; Increment X back up to it's actual current line number.

; Do logic operations within 54 CPU cycles while the TIA is printing the image to the screen.
PrintingPeriod
    STA WSYNC               ; Strobe.
    DEX                     ; Decrement our line counter.
    BNE HBlankLoop          ; Wait for the printing period to be done, then start again.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OVERSCAN

; Handles the 30 lines of overscan, 76 clock cycles each.
OverscanPeriod
    LDA #2                  ; Put 2 into accumulator again in order to switch on VBLANK.
    STA VBLANK              ; Turn on register indicating vertical blank.

    JSR PlayerCollisions    ; Check the player collisions and alter who is it as needed.
    STA CXCLR               ; Clear collision latches.
    STA WSYNC               ; Strobe.

    JSR SetItColours        ; Set the colours for who is it during the first overscan period.
    STA WSYNC               ; Strobe.

    JSR SetPlayerPosition   ; Call subroutine to change the player's position based on joystick inputs.
    STA WSYNC               ; Strobe.

    JSR UpdateTimeCounters  ; Update the time counters. This should be the last call in the overscan period logic.
    STA WSYNC               ; Strobe.

    LDX #26                 ; Make X the line counter for the 30 lines of overscan (minus the four already used).

OverscanLoop
    STA WSYNC               ; Strobe.
    DEX                     ; Decrement the line counter.
    BNE OverscanLoop        ; Repeat until X = 0.

    JMP FrameStart          ; When the overscan period is complete, start the next frame.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SUBROUTINES

; Subroutine to set the colours for who is it. Manipulates accumulator.
SetItColours
    LDA PlayerIt            ; Load the PlayerIt value into the accumulator.
    BNE NpcRed              ; Go to NpcRed if the value is non-zero.

PlayerRed
    STA COLUP1              ; Accumulator is zero at this point; set Npc to black.
    LDA #$32                ; Load red into accumulator.
    STA COLUP0              ; Set player colour register to red.
    RTS                     ; Return.

NpcRed
    LDA #$32                ; Load red into accumulator.
    STA COLUP1              ; Set NCP to red.
    LDA #0                  ; Load black into accumulator.
    STA COLUP0              ; Set player to black.
    RTS                     ; Return.


; Subroutine to update the time counters.
UpdateTimeCounters
    DEC HalfSecondCounter   ; Decrement the half second counter.
    BPL UpdateSecondCounter ; Branch to updating the seconds counter if half second counter is not zero.

ResetHalfSecondCounter
    LDA #28                 ; Load 30 into accumulator.
    STA HalfSecondCounter   ; Save 30 into the half second counter.

UpdateSecondCounter
    DEC SecondCounter       ; Decrement the seconds counter.
    BPL ExitUpdateCounters  ; If a second hasn't passed yet, exit the subroutine.

ResetSecondCounter
    LDA #58                 ; Load 60 into accumulator.
    STA SecondCounter       ; Save 60 into the second counter.
    DEC TenSecondCounter    ; Decrement the 10 second counter as a second has passed.
    BPL ExitUpdateCounters  ; Exit the subroutine if the ten secound counter is not yet zero.

ResetTenSecondCounter
    LDA #8                  ; Load 10 into the accumulator.
    STA TenSecondCounter    ; Save 10 into the ten second counter.

ExitUpdateCounters
    RTS                     ; Return from the subroutine.


; Subroutine to change who is it based on NPC and player collisions.
; Note that PlayerIt will be 0000 0000 when the player is It, and 1111 1111 otherwise.
PlayerCollisions
    BIT CXPPMM              ; The bit operation sets the N flag from the initial, un-ANDed value of memory bit 7, which represents player-player collisions.
    BPL ExitPlayerCollisions; If no collision occured, exit.
    LDA #%11111111          ; Loads 1111 1111 into the accumulator.
    EOR PlayerIt            ; Flips the bits in the PlayerIt variable.
    STA PlayerIt            ; Store the value.

ExitPlayerCollisions
    RTS


; Subroutine to get joystick input and appropriately adjust player position.
SetPlayerPosition
    ; This subroutine is a bit hacky to save on instructions. The joystick is read as follows:
    ; Bit 4 = up, bit 5 = down, bit 6 = left, bit 7 = right. Note that a bit being UNSET means movement occured.
    ; This means for left and right we can do bit comparisons since the N and V flags are set from the memory value.
    ; Therefore if bit 6 is set (flag V), the player has NOT moved left. If bit 7 is set (flag N), the player has NOT moved right.

CheckPlayerLeft
    BIT SWCHA               ; Bit compare with accumulator. This is just to capture the overflow flag as described above.
    BVS CheckPlayerRight    ; If the overflow flag is set, skip to checking movement right as described above.
    DEC PlayerX             ; Otherwise, the player has moved left so decrement X.
    ; TODO add in logic to check boundaries??

CheckPlayerRight
    BIT SWCHA               ; Bit compare with accumulator. This is just to capture the negative flag as described above.
    BMI CheckPlayerUp       ; If the bit is set, skip to checking up.
    INC PlayerX             ; Otherwise, the player has moved right so increment X.

CheckPlayerUp
    LDA #%00010000          ; Loads mask into accumulator.
    BIT SWCHA               ; Compare with accumulator.
    BNE CheckPlayerDown     ; If up hasn't been pressed, skip to down.
    INC PlayerY             ; Otherwise, increment player's Y value.

CheckPlayerDown
    ASL                     ; Shift the bit over in the accumulator so that the mask is 0010 0000.
    BIT SWCHA               ; Bit compare with accumulator.
    BNE ExitPlayerPosition  ; If down hasn't been pressed, exit the subroutine.
    DEC PlayerY             ; Otherwise, player has moved down so decrement Y value.

ExitPlayerPosition
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SPRITES

; Defines the bitmap for a left-facing sprite. 
; The sprite is stored upside-down so height can be decremented to zero.
SpriteData
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

; Sets the ROM's capacity to exactly 4K, i.e. $F000 through $FFFF, and tells it where to start execution.
    org $FFFC               ; Set origin as $FFFC, the reset pointer, to tell where to begin execution.
    .word Initialize        ; Reset pointer at $FFFC, set to Initialize.
    .word Initialize        ; Interrupt pointer at $FFFE (unused but apparently a good idea to set anyway).

; EOF
