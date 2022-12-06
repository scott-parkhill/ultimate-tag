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

; Variables for who is it and the height in pixels of the player sprite.
PlayerIt            .byte   ; 0000 0000 for it, and 1111 1111 for not it.
NpcDelay            .byte   ; $00 for no delay, $FF for delay.
NpcDelayCounter     .byte   ; Initializes to 59, when it hits zero the delay ends (as there are 60 frames a second).
SpriteHeight        .byte   ; Saves the sprite's height.
WindowTop           .byte   ; Defines the top edge of the screen.
WindowRight         .byte   ; Defines the right edge of the screen.
WindowBottom        .byte   ; Defines the bottom edge of the screen.
WindowLeft          .byte   ; Defines the left edge of the screen.

; Player and NPC coordinates.
PlayerX             .byte   ; The player's X coordinate. Coordinates are given with the origin at the bottom left of the screen.
PlayerY             .byte   ; The player's Y coordinate.
PlayerPreviousX     .byte   ; For handling collisions.
PlayerPreviousY     .byte   ; For handling collisions.
NpcX                .byte   ; The Npc's X coordinate.
NpcY                .byte   ; The Npc's Y coordinate.
NpcPreviousX        .byte   ; For handling collisions.
NpcPreviousY        .byte   ; For handling collisions.

; Powerups.
PowerupTimer        .byte   ; 3 second powerup timer.
PlayerPowerup       .byte   ; The player's powerup status. 0 = no powerup, 1 = 2x powerup.
NpcPowerup          .byte   ; The Npc's powerup status. 0 = no powerup, 1 = 2x powerup.

; Player sprite information.
PlayerDirection     .byte   ; 0 for left, 8 for right (the value to set the reflection bit).
PrintPlayerSprite   .byte   ; 0 for don't print, anything else for print.
PlayerSpriteCount   .byte   ; Memory offset for printing sprites; 16 bit sprites -> 15 as the offset.
PlayerSpriteMap     .byte   ; The bitmap level for the player.

; NPC sprite information.
NpcDirection        .byte   ; 0 for left, 8 for right (the value to set the reflection bit).
PrintNpcSprite      .byte   ; 0 for don't print, anything else for print.
NpcSpriteCount      .byte   ; Memory offset for printing sprites; 16 bit sprites -> 15 as the offset.
NpcSpriteMap        .byte   ; The bitmap level for the Npc.

; Array to store the sprite data into memory.
Sprite              ds 16   ; Array for the sprite data. This is needed in order to save on clock cycles by having the sprite zero-page indexed.

; This logic is a bit weird but done so that branching is on positive. Basically, -1 to 8 = 10, etc.
SecondCounter       .byte   ; Initializes to 59 and re-initializes to 58, as there are 60 frames a second. When zero, a second has passed.

; Creates segment for the main program.
    seg Program             ; Defines the initialized code segment of the program.
    org $F000               ; Set origin of segment at beginning of cartridge ROM.

; Creates a section for all initialization details.
Initialize                  ; Defines the Initialize section.
    CLEAN_START             ; Calls the CLEAN_START macro from macro.h.

    ; Accumulator starts at 0 from the CLEAN_START macro.
    STA SWACNT              ; Set joystick register for reading.
    STA NpcDirection        ; Sets the Npc facing left.
    STA PrintPlayerSprite   ; Set to don't print.
    STA PrintNpcSprite      ; Set to don't print.
    STA PlayerSpriteMap     ; Set to zeros.
    STA NpcSpriteMap        ; Set to zeros.
    STA NpcPowerup          ; Set NPC powerup to off.
    JSR SetItColours        ; Go to subroutine to set the colours for who is it.

    LDA #3                  ; Set the powerup timer.
    STA PowerupTimer        ; Save the powerup timer.

    LDA #173                ; Set window top edge.
    STA WindowTop           ; Save window top edge.
    LDA #145                ; Set window right edge.
    STA WindowRight         ; Save window right edge.
    LDA #4                  ; Set window bottom edge.
    STA WindowBottom        ; Save window bottom edge.
    LDA #0                  ; Set window left edge.
    STA WindowLeft          ; Save window left edge.
    STA NpcDelay            ; Set NPC to no delay.

    LDA #$FF                ; Load the "npc it" value into the accumulator.
    STA PlayerIt            ; Initialize the NPC to "it".
    STA PlayerPowerup       ; Set player powerup to on.

    LDX #96                 ; Sets X so the sprites sits halfway up the screen.
    STX PlayerY             ; Set player to be halfway up the screen.
    STX NpcY                ; Set NPC to be halfway up the screen.

    LDX #20                 ; Loads X with 10, the starting x coordinate for the player.
    STX PlayerX             ; Saves the X value into PlayerX.

; 149 is right edge of the screen.
    LDX #139                ; Loads X with 139, the starting x coordinate for the NPC.
    STX NpcX                ; Saves the X value into NpcX.

    LDY #16                 ; Load 16 into Y, to store the sprite height.
    STY SpriteHeight        ; Saves the value 16 into the SpriteHeight variable.

; Loads the sprite into RAM. This is done in order to properly "race the beam" in the HBlankLoop.
; Loading the sprite data during the printing period resulted in too many clock cycles used when
; both the player and NPC shared any horizontal spaces (i.e. were printed on the same scanline ever).
; Having it zero-page indexed saves on load clock cycles which makes me race the beam just in time.
    LDY #15                 ; Load 15, the sprite offset, into Y.
SpriteLoop
    LDA SpriteData,Y        ; Load the sprite data at the offset into accumulator.
    STA Sprite,Y            ; Save the sprite data into RAM at the offset amount.
    DEY                     ; Decrease Y.
    BPL SpriteLoop          ; Repeat loop until Y is negative.
    
    LDA #8
    STA PlayerDirection     ; Sets the player direction to facing right.

    LDA #$9F                ; Loads background colour (light blue) into the accumulator.
    STA COLUBK              ; Sets the background colour register in the TIA.

    LDA #$74                ; Load dark blue into register.
    STA COLUPF              ; Set the playfield (here just the ball) to dark blue.

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
    LDA #38                 ; Value used to skip a bunch of scanlines using the formula FLOOR((N*76+13)/64). Want to skip 32 scanlines here.
    STA WSYNC               ; Guarantee start of scanline.
    STA TIM64T              ; Set the timer to skip the scanlines.

    STA GRP0                ; Reset player graphics register. Accumulator is zero here.
    STA GRP1                ; Reset Npc graphics register.

    LDX PlayerDirection     ; Load the player direction into X.
    STX REFP0               ; Save player direction in reflection register.

    LDX NpcDirection        ; Load the Npc direction into X.
    STX REFP1               ; Save the Npc direction into the reflection register.
                 
    LDA #15                 ; Put 15 into accumulator in order to reset player sprite height counts.
    STA PlayerSpriteCount   ; Reset sprite memory offset.
    STA NpcSpriteCount      ; Reset sprite memory offset.

    LDX #0                  ; Load player into X.
    LDA PlayerX             ; Load player X coordinate into accumulator.
    JSR SetHorizontalPosition   ; Set the horizontal position of the player.
    ; WSYNC occurs.

    LDX #1                  ; Load the NPC into X.
    LDA NpcX                ; Load the NPC X coordinate into accumulator.
    JSR SetHorizontalPosition   ; Set the horizontal position of the NPC.
    ; WSYNC occurs.

    LDX #192                ; Sets X to the number of lines for the visible frame.
VblankSetPlayerMap
    TXA                     ; Transfer X, the current line, to accumulator.
    SEC                     ; Set the carry bit in order to do subtraction. Subtraction expects the carry bit to be set.
    SBC PlayerY             ; Subtract the player's Y position from the line number in the accumulator to get a local coordinate.
    CMP SpriteHeight        ; Compare this value with the height of the sprite.
    BCC VblankSavePlayerSprite  ; Branch on carry clear. This works because carry clear on CMP is accumulator < SpriteHeight (both C and Z bits clear in this case, so also not zero).
    LDA #0                  ; Otherwise, we're not in the sprite so set the accumulator to zero to reset the graphics register.

VblankSavePlayerSprite
    TAY                     ; Transfer the accumulator into Y.
    LDA Sprite,Y            ; Load the sprite bitmap line value into the accumulator using the offset that was computed.
    STA PlayerSpriteMap     ; Save this bitmap line to be printed at the beginning of the HBLANK.

VblankSetNpcMap
    TXA                     ; Transfer X, the current line, to accumulator.
    SEC                     ; Set the carry bit in order to do subtraction. Subtraction expects the carry bit to be set.
    SBC NpcY                ; Subtract the NPC's Y position from the line number in the accumulator to get a local coordinate.
    CMP SpriteHeight        ; Compare this value with the height of the sprite.
    BCC VblankSaveNpcSprite ; Branch on carry clear. This works because carry clear on CMP is accumulator < SpriteHeight (both C and Z bits clear in this case, so also not zero).
    LDA #0                  ; Otherwise, we're not in the sprite so set the accumulator to zero to reset the graphics register.

VblankSaveNpcSprite
    TAY                     ; Transfer the accumulator into Y.
    LDA Sprite,Y            ; Load the sprite bitmap line value into the accumulator using the offset that was computed.
    STA NpcSpriteMap        ; Save this bitmap line to be printed at the beginning of the HBLANK.

VblankWaitForTimer
    LDA INTIM               ; Load the timer value.
    BNE VblankWaitForTimer  ; Wait until the timer is set to zero.

; Finish setting up the sprites for the first HBLANK, so strobe WSYNC then loop.
    STA WSYNC               ; Strobe.
    STA HMOVE               ; Apply the fine horizontal offsets.
    
    ; Turn off VBLANK.
    LDX #0
    STA WSYNC
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

SetPlayerMap
    TXA                     ; Transfer X, the current line, to accumulator.
    SEC                     ; Set the carry bit in order to do subtraction. Subtraction expects the carry bit to be set.
    SBC PlayerY             ; Subtract the player's Y position from the line number in the accumulator to get a local coordinate.
    CMP SpriteHeight        ; Compare this value with the height of the sprite.
    BCC SavePlayerSprite    ; Branch on carry clear. This works because carry clear on CMP is accumulator < SpriteHeight (both C and Z bits clear in this case, so also not zero).
    LDA #0                  ; Otherwise, we're not in the sprite so set the accumulator to zero to reset the graphics register.

SavePlayerSprite
    TAY                     ; Transfer the accumulator into Y.
    LDA Sprite,Y            ; Load the sprite bitmap line value into the accumulator using the offset that was computed.
    STA PlayerSpriteMap     ; Save this bitmap line to be printed at the beginning of the HBLANK.

SetNpcMap
    TXA                     ; Transfer X, the current line, to accumulator.
    SEC                     ; Set the carry bit in order to do subtraction. Subtraction expects the carry bit to be set.
    SBC NpcY                ; Subtract the NPC's Y position from the line number in the accumulator to get a local coordinate.
    CMP SpriteHeight        ; Compare this value with the height of the sprite.
    BCC SaveNpcSprite       ; Branch on carry clear. This works because carry clear on CMP is accumulator < SpriteHeight (both C and Z bits clear in this case, so also not zero).
    LDA #0                  ; Otherwise, we're not in the sprite so set the accumulator to zero to reset the graphics register.

SaveNpcSprite
    TAY                     ; Transfer the accumulator into Y.
    LDA Sprite,Y            ; Load the sprite bitmap line value into the accumulator using the offset that was computed.
    STA NpcSpriteMap        ; Save this bitmap line to be printed at the beginning of the HBLANK.

; Do logic operations within 54 CPU cycles while the TIA is printing the image to the screen.
PrintingPeriod
    STA WSYNC               ; Strobe.
    DEX                     ; Decrement our line counter.
    BNE HBlankLoop          ; Wait for the printing period to be done, then start again.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OVERSCAN

; Handles the 30 lines of overscan, 76 clock cycles each.
OverscanPeriod
    LDA #33                 ; Timer number. FLOOR((N*76+13)/64). Want to skip 28 WSYNCs.
    STA WSYNC               ; Strobe.
    STA TIM64T              ; Set timer.

    LDA #2                  ; Put 2 into accumulator again in order to switch on VBLANK.
    STA VBLANK              ; Turn on register indicating vertical blank.

    JSR PlayerCollisions    ; Check the player collisions and alter who is it as needed.
    STA CXCLR               ; Clear collision latches.

    JSR SetItColours        ; Set the colours for who is it during the first overscan period.
    JSR SetPlayerPosition   ; Call subroutine to change the player's position based on joystick inputs.
    JSR ExecuteNpcAi        ; Run the NPC's AI.
    JSR UpdateTimeCounters  ; Update the time counters. This should be the last call in the overscan period logic.

OverscanWaitForTimer
    LDA INTIM               ; Load timer value into accumulator.
    BNE OverscanWaitForTimer; Loop while time remains.
    STA WSYNC               ; End overscan period.

    JMP FrameStart          ; When the overscan period is complete, start the next frame.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SUBROUTINES

; Subroutine to set the colours for who is it. Manipulates accumulator.
SetItColours subroutine
    LDA PlayerIt            ; Load the PlayerIt value into the accumulator.
    BNE .NpcRed              ; Go to NpcRed if the value is non-zero.

.PlayerRed
    STA COLUP1              ; Accumulator is zero at this point; set Npc to black.
    LDA #$32                ; Load red into accumulator.
    STA COLUP0              ; Set player colour register to red.
    RTS                     ; Return.

.NpcRed
    LDA #$32                ; Load red into accumulator.
    STA COLUP1              ; Set NCP to red.
    LDA #0                  ; Load black into accumulator.
    STA COLUP0              ; Set player to black.
    RTS                     ; Return.


; Subroutine to update the time counters.
UpdateTimeCounters subroutine

    DEC NpcDelayCounter     ; Update the NPC delay counter.
    DEC SecondCounter       ; Decrement the seconds counter.
    BPL .Exit               ; If a second hasn't passed yet, exit the subroutine.

; Resets the second counter and decrements tens counter and powerup counter.
    LDA #58                 ; Load 60 into accumulator.
    STA SecondCounter       ; Save 60 into the second counter.

    DEC PowerupTimer        ; Decrement the powerup counter.
    BNE .Exit               ; If timer hasn't reached zero, branch.

    ; LDA PlayerPowerup
    ; STA NpcPowerup
    LDA #$FF                ; Load $FF into accumulator.
    EOR PlayerPowerup       ; Flip player powerup.
    STA PlayerPowerup       ; Store player powerup.

    LDA #3                  ; Put 3 into accumulator.
    STA PowerupTimer        ; Store in powerup timer.

.Exit
    RTS                     ; Return from the subroutine.


; Subroutine to change who is it based on NPC and player collisions.
; Note that PlayerIt will be 0000 0000 when the player is It, and 1111 1111 otherwise.
PlayerCollisions subroutine

    BIT CXPPMM              ; The bit operation sets the N flag from the initial, un-ANDed value of memory bit 7, which represents player-player collisions.
    BPL .Exit               ; If no collision occured, exit.

    LDA NpcDelay            ; Loads whether the delay is occuring right now from the NPC being it.
    BNE .SkipSetDelay       ; If delay is occuring, then just reset the positions to previous.
                            ; Otherwise, switch who is it.    

    LDA #%11111111          ; Loads 1111 1111 into the accumulator.
    EOR PlayerIt            ; Flips the bits in the PlayerIt variable.
    STA PlayerIt            ; Store the value.

    BEQ .SkipSetDelay       ; If the value is $00 then the player is it so don't set NPC delay.
    LDA NpcDelay            ; Load the NPC delay value.
    EOR #$FF                ; Switch it.
    STA NpcDelay            ; Save it.
    LDA #59                 ; Set accumulator to 59.
    STA NpcDelayCounter     ; Save into counter.

.SkipSetDelay
    LDA PlayerPreviousX     ; Get player previous x value.
    STA PlayerX             ; Save it in player value.
    LDA PlayerPreviousY     ; Get previous Y.
    STA PlayerY             ; Store in player value.

    LDA NpcPreviousX        ; Get NPC previous x value.
    STA NpcX                ; Save it.
    LDA NpcPreviousY        ; Get NPC previous y value.
    STA NpcY                ; Save it.

.Exit
    RTS                     ; Exit subroutine.


; Subroutine to get joystick input and appropriately adjust player position.
SetPlayerPosition subroutine
    ; This subroutine is a bit hacky to save on instructions. The joystick is read as follows:
    ; Bit 4 = up, bit 5 = down, bit 6 = left, bit 7 = right. Note that a bit being UNSET means movement occured.
    ; This means for left and right we can do bit comparisons since the N and V flags are set from the memory value.
    ; Therefore if bit 6 is set (flag V), the player has NOT moved left. If bit 7 is set (flag N), the player has NOT moved right.

    LDA PlayerX             ; Load player X.
    STA PlayerPreviousX     ; Save player x.
    LDA PlayerY             ; Load player Y.
    STA PlayerPreviousY     ; Save player y.

.CheckPlayerLeft
    LDA WindowLeft          ; Get left edge of screen.
    SEC                     ; Set carry bit for subtraction.
    SBC PlayerX             ; Subtract from the player X value.
    BEQ .CheckPlayerRight   ; If at left side of the window, skip left checks.

    BIT SWCHA               ; Bit compare with accumulator. This is just to capture the overflow flag as described above.
    BVS .CheckPlayerRight   ; If the overflow flag is set, skip to checking movement right as described above.
    DEC PlayerX             ; Otherwise, the player has moved left so decrement X.
    LDA #0                  ; Load 0 into accumulator for "left".
    STA PlayerDirection     ; Save into player direction.

    LDA WindowLeft          ; Load window left.
    SEC                     ; Set carry bit for subtraction.
    SBC PlayerX             ; Get if in the window still.
    BEQ .CheckPlayerUp      ; If at the window's edge, skip powerup check.

    LDA PlayerPowerup       ; Load powerup data.
    BEQ .CheckPlayerUp      ; Continue if no powerup.
    DEC PlayerX             ; Double speed otherwise.
    JMP .CheckPlayerUp      ; If the player has gone left then they can't go right.

.CheckPlayerRight
    LDA WindowRight         ; Load WindowRight into accumulator.
    SEC
    SBC PlayerX
    BEQ .CheckPlayerUp      ; If against the right edge of screen, skip moving right.

    BIT SWCHA               ; Bit compare with accumulator. This is just to capture the negative flag as described above.
    BMI .CheckPlayerUp      ; If the bit is set, skip to checking up.
    INC PlayerX             ; Otherwise, the player has moved right so increment X.
    LDA #8                  ; Load 8 into accumulator for "right".
    STA PlayerDirection     ; Save into player direction.

    LDA WindowRight         ; Get window right edge.
    SEC                     ; Set carry bit.
    SBC PlayerX             ; See if player is at window's edge.
    BEQ .CheckPlayerUp      ; If at window's edge, skip powerup check.

    LDA PlayerPowerup       ; Load powerup data.
    BEQ .CheckPlayerUp      ; Continue if no powerup.
    INC PlayerX             ; Double speed otherwise.

.CheckPlayerUp
    LDA WindowTop           ; Load window height.
    SEC
    SBC PlayerY
    BEQ .CheckPlayerDown    ; Skip up if at top of screen.

    LDA #%00010000          ; Loads mask into accumulator.
    BIT SWCHA               ; Compare with accumulator.
    BNE .CheckPlayerDown    ; If up hasn't been pressed, skip to down.
    INC PlayerY             ; Otherwise, increment player's Y value.

    LDA WindowTop           ; Load window top edge.
    SEC                     ; Set carry bit.
    SBC PlayerY             ; See if on the edge.
    BEQ .Exit               ; If on edge, skip powerup check.

    LDA PlayerPowerup       ; Load powerup data.
    BEQ .Exit               ; Exit if no powerup.
    INC PlayerY             ; Otherwise, double speed.
    RTS                     ; If the player has gone up then they can't go down.

.CheckPlayerDown
    LDA WindowBottom        ; Get bottom edge of screen.
    SEC
    SBC PlayerY
    BEQ .Exit               ; Exit if at bottom of screen already.

    LDA #%00100000          ; Set mask.
    BIT SWCHA               ; Bit compare with accumulator.
    BNE .Exit               ; If down hasn't been pressed, exit the subroutine.
    DEC PlayerY             ; Otherwise, player has moved down so decrement Y value.

    LDA WindowBottom        ; Get bottom edge of screen.
    SEC                     ; Set carry bit.
    SBC PlayerY             ; Get if on edge of screen.
    BEQ .Exit               ; If on edge, skip powerup check.

    LDA PlayerPowerup       ; Load powerup data.
    BEQ .Exit               ; Exit if no powerup is on.
    DEC PlayerY             ; Otherwise, double speed.

.Exit
    RTS


; This subroutine sets the horizontal position of a given sprite. This should be called in the VBLANK section.
; Offsets are based on the register positions defined in vcs.h.
;
; A = 0: player 0
; A = 1: player 1
; A = 2: missile 0
; A = 3: missile 1
; A = 4: ball
;
; Set the horizontal coordinate of the sprite in the X register. Set the offset for saving into accumulator.
SetHorizontalPosition subroutine
    STA WSYNC               ; Wait for start of the new scanline so proper positioning information can be set.
    SEC                     ; Set the carry flag in order to do subtraction.

.DivideLoop
    SBC #15                 ; Subtract 15.
    BCS .DivideLoop         ; Branch until negative.

    EOR #7                  ; Calculate the sprite's fine offset. This is because a normal loop would only be able to set it in increments of 5, so it would be blocky.
    ASL                     ; Arithmetic shift left.
    ASL                     ; Arithmetic shift left.
    ASL                     ; Arithmetic shift left.
    ASL                     ; Shift to get the sprite fine offset.
    STA HMP0,X              ; Set the fine offset for the sprite.
    STA RESP0,X             ; Reset the sprite to the given coarse position.

    RTS                     ; Return from subroutine.


; The subroutine that does all the calculations for NPC logic.
ExecuteNpcAi subroutine
.CheckHorizontalMovement
    ; Save previous NPC coordinate values.
    LDA NpcX                ; Load NpcX into accumulator.
    STA NpcPreviousX        ; Store X value in previous.
    LDA NpcY                ; Load y coord into accumulator.
    STA NpcPreviousY        ; Store into previous.

    LDA NpcDelay            ; Load whether the NPC should be delayed or not.
    BEQ .NormalMovement     ; If no delay is set, make the NPC move.
    LDA NpcDelayCounter     ; Load the delay counter.
    BMI .RemoveDelay        ; If a second has passed, then remove the delay.
    RTS                     ; Otherwise, exit the routine so the NPC doesn't move.

.RemoveDelay
    LDA NpcDelay            ; Load the NPC delay.    
    EOR #$FF                ; Otherwise, the delay is over so turn off the delay switch.
    STA NpcDelay            ; Store the updated delay status.
                            ; Then continue with movements.

.NormalMovement
    LDA PlayerX             ; Load the player x-coordinate into A.
    SEC                     ; Set the carry bit for subtraction.
    SBC NpcX                ; Subtract X from the accumulator.
    BPL .NpcMoveRight       ; Branch to right movement if positive or zero.
                            ; Otherwise, handle running left.

.HandleRunLeft
    LDA PlayerIt            ; Load player it.
    BEQ .NpcMoveRight       ; If the NPC is to run left but is it, then run right.
    JMP .NpcMoveLeft        ; Otherwise, run left as intended.

.HandleRunRight
    LDA PlayerIt            ; Load player it into accumulator.
    BEQ .NpcMoveLeft        ; If the NPC is to run right but is it, then run left.
                            ; Otherwise, run right.

.NpcMoveRight
    LDA WindowRight         ; Load window width into accumulator.
    SEC
    SBC NpcX
    BEQ .CheckVerticalMovement  ; If the sprite is at the window's edge, continue.
    
    INC NpcX                ; Run right.
    LDA #8                  ; Load 8 into accumulator for "right".
    STA NpcDirection     ; Save into npc direction.

    LDA WindowRight
    SEC
    SBC NpcX
    BEQ .CheckVerticalMovement  ; Skip powerup check if at window width.

    LDA NpcPowerup          ; Load powerup data.
    BEQ .CheckVerticalMovement  ; Continue if no powerup.
    INC NpcX                ; Otherwise, double movement.

.NpcMoveLeft
    LDA WindowLeft          ; Get left edge of screen.
    SEC
    SBC NpcX
    BEQ .CheckVerticalMovement  ; If at window edge, skip to vertical movement.

    DEC NpcX                ; Run left.
    LDA #0                  ; Load 0 into accumulator for "left".
    STA NpcDirection     ; Save into npc direction.

    LDA WindowLeft
    SEC
    SBC NpcX
    BEQ .CheckVerticalMovement  ; If at window edge, skip powerup and go to vertical movement.

    LDA NpcPowerup          ; Load powerup data.
    BEQ .CheckVerticalMovement  ; If no powerup, continue.
    DEC NpcX                ; Otherwise, double movement.
                            ; Fall into checking vertical movement.

.CheckVerticalMovement
    LDA PlayerY             ; Load the player's Y coordinate into accumulator.
    SEC                     ; Set carry bit for subtraction.
    SBC NpcY                ; Subtract NPC's position from the player's.
    BPL .HandleRunUp        ; If the value is positive, player is above the NPC.
                            ; Otherwise, fall into handling run down.

.HandleRunDown
    LDA PlayerIt            ; Load the player it status.
    BEQ .NpcMoveUp          ; If the npc is supposed to move down but is it, move up instead.
    JMP .NpcMoveDown        ; Otherwise, move down as intended.

.HandleRunUp
    LDA PlayerIt            ; Load the player it status.
    BEQ .NpcMoveDown        ; If the npc is supposed to run up but is it, run down instead.
                            ; Otherwise, fall into moving up.
.NpcMoveUp
    LDA WindowTop        ; Load window height into accumulator.
    SEC
    SBC NpcY
    BEQ .Exit               ; If already at the top, exit.

    INC NpcY                ; Otherwise, increase the y coordinate.
    LDA WindowTop
    SEC
    SBC NpcY
    BEQ .Exit               ; If at top, don't check powerup status, just exit.

    LDA NpcPowerup          ; Load powerup data.
    BEQ .Exit               ; If no powerup, exit.
    INC NpcY                ; Otherwise, double the speed.
    JMP .Exit               ; Then exit.

.NpcMoveDown
    LDA WindowBottom        ; Get bottom of screen.
    SEC
    SBC NpcY
    BEQ .Exit               ; If at bottom of screen, exit.
    
    DEC NpcY                ; Otherwise, move the NPC down.
    LDA WindowBottom
    SEC
    SBC NpcY
    BEQ .Exit               ; If at bottom, exit.

    LDA NpcPowerup          ; Otherwise load powerup data.
    BEQ .Exit               ; If no powerup, exit.
    DEC NpcY                ; Otherwise, double movement speed.
                            ; Fall into exit.

.Exit
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
