; =====================================================================
;  ICE 0.51  --  recovered source: assembler helpers (segment 0013 and
;  DOSXIT at 001C:0002 of ICE.EXE).  MASM 5.x, large model, called from
;  MS FORTRAN 4.x (arguments passed by far reference, the callee pops).
;  Byte-for-byte equivalent to the code in the executable.  SETCUR and
;  CLRWIN are linked but never called by the program.
;  (scrio.c is the stand-in used for the gfortran build.)
; =====================================================================

SCRIO_TEXT SEGMENT BYTE PUBLIC 'CODE'
        ASSUME  CS:SCRIO_TEXT

        PUBLIC  SETCUR, GETKEY, CLRWIN, CUROFF, CURON

; ---------------------------------------------------------------------
;  CALL SETCUR (ICOL, IROW)        [0013:000A]  INTEGER*1 column, row
;  BIOS INT 10h function 02h, page 0
; ---------------------------------------------------------------------
SETCUR  PROC    FAR
        push    bp
        mov     bp, sp
        les     bx, [bp+10]             ; ICOL
        mov     dl, es:[bx]
        les     bx, [bp+6]              ; IROW
        mov     dh, es:[bx]
        mov     ah, 2
        xor     bx, bx
        int     10h
        mov     sp, bp
        pop     bp
        ret     8
SETCUR  ENDP

; ---------------------------------------------------------------------
;  CALL GETKEY (IASC, ISCAN)       [0013:0025]  wait for a key; ASCII
;  code and scan code returned as INTEGER*2 (low byte, high byte 0)
;  BIOS INT 16h function 00h
; ---------------------------------------------------------------------
GETKEY  PROC    FAR
        push    bp
        mov     bp, sp
        xor     ah, ah
        int     16h
        les     bx, [bp+10]             ; IASC
        mov     es:[bx], al
        mov     byte ptr es:[bx+1], 0
        les     bx, [bp+6]              ; ISCAN
        mov     es:[bx], ah
        mov     byte ptr es:[bx+1], 0
        pop     bp
        ret     8
GETKEY  ENDP

; ---------------------------------------------------------------------
;  CALL CLRWIN (IL, IT, IR, IB)    [0013:0046]  blank a window
;  (attribute 0Fh) and put the cursor at its upper left corner.
;  BIOS INT 10h functions 06h and 02h.
;  NB: the original returns with RET 8 although it takes four
;  arguments (16 bytes) -- harmless only because it is never called.
; ---------------------------------------------------------------------
CLRWIN  PROC    FAR
        push    bp
        mov     bp, sp
        les     bx, [bp+18]             ; IL  left column
        mov     cl, es:[bx]
        les     bx, [bp+14]             ; IT  top row
        mov     ch, es:[bx]
        les     bx, [bp+10]             ; IR  right column
        mov     dl, es:[bx]
        les     bx, [bp+6]              ; IB  bottom row
        mov     dh, es:[bx]
        mov     ax, 0600h
        mov     bh, 0Fh
        int     10h
        mov     ah, 2
        xor     bh, bh
        mov     dh, ch
        mov     dl, cl
        int     10h
        mov     sp, bp
        pop     bp
        ret     8
CLRWIN  ENDP

; ---------------------------------------------------------------------
;  CALL CUROFF   [0013:0078]   hide the cursor (start line 20h)
;  CALL CURON    [0013:0085]   cursor lines 0..7
; ---------------------------------------------------------------------
CUROFF  PROC    FAR
        push    bp
        push    ds
        mov     ah, 1
        mov     cl, 0
        mov     ch, 20h
        int     10h
        pop     ds
        pop     bp
        ret
CUROFF  ENDP

CURON   PROC    FAR
        push    bp
        push    ds
        mov     ah, 1
        mov     cl, 7
        mov     ch, 0
        int     10h
        pop     ds
        pop     bp
        ret
CURON   ENDP

SCRIO_TEXT ENDS

; ---------------------------------------------------------------------
;  CALL DOSXIT   [001C:0002]   terminate with return code 0
; ---------------------------------------------------------------------
DOSX_TEXT SEGMENT BYTE PUBLIC 'CODE'
        ASSUME  CS:DOSX_TEXT
        PUBLIC  DOSXIT
DOSXIT  PROC    FAR
        push    bp
        mov     ax, 4C00h
        int     21h
        pop     bp
        ret
DOSXIT  ENDP
DOSX_TEXT ENDS

        END
