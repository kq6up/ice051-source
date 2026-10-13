; Replacement for ICE 0.51 SUBROUTINE DELTAT(YEAR,DT)  (seg 001C:3480, MS FORTRAN far/pascal conv.)
; DT = linear interpolation in a 2-year table of Delta-T (0.01 s units), 1800.0 .. 2050.0,
;      linear extrapolation outside the table.
; YEAR arrives as IY + IM/12 + ID/365.25 (caller patched); Y0 removes the 1/12 + 1/365.25 bias.
        bits 16
        org 0x3480
Y0      equ 0x43a0          ; DGROUP: 1800 + 1/12 + 1/365.25
HALF    equ 0x43a8          ; DGROUP: 0.5
CENTI   equ 0x43b0          ; DGROUP: 0.01
deltat:
        push bp
        mov  bp, sp
        sub  sp, 0x0c
        les  bx, [bp+0x0a]          ; YEAR
        wait
        fld  qword [es:bx]
        wait
        fsub qword [Y0]
        wait
        fmul qword [HALF]           ; X = (Y-Y0)/2
        wait
        fld  st0
        wait
        fsub qword [HALF]           ; X-0.5
        wait
        fistp dword [bp-4]          ; I = nint(X-0.5) = floor(X)
        nop
        wait
        mov  ax, [bp-4]
        mov  dx, [bp-2]
        or   dx, dx                 ; clamp I to 0 .. 124
        jl   .lo
        jg   .hi
        cmp  ax, 124
        jbe  .ok
.hi:    mov  ax, 124
        jmp  short .ok
.lo:    xor  ax, ax
.ok:    mov  [bp-4], ax
        mov  word [bp-2], 0
        mov  bx, ax
        shl  bx, 1
        mov  ax, [cs:bx+table]      ; T(I)
        mov  cx, [cs:bx+table+2]
        sub  cx, ax                 ; T(I+1)-T(I)
        cwd
        mov  [bp-8], ax
        mov  [bp-6], dx
        mov  ax, cx
        cwd
        mov  [bp-0x0c], ax
        mov  [bp-0x0a], dx
        wait
        fild dword [bp-4]           ; I , X
        wait
        fsubp st1, st0              ; F = X - I
        wait
        fild dword [bp-0x0c]
        wait
        fmulp st1, st0              ; F*(T(I+1)-T(I))
        wait
        fiadd dword [bp-8]          ; + T(I)
        wait
        fmul qword [CENTI]          ; -> seconds
        les  bx, [bp+6]             ; DT
        wait
        fstp qword [es:bx]
        nop
        wait
        mov  sp, bp
        pop  bp
        retf 8
        align 2, db 0
table:  ; Delta-T at 1800.0, 1802.0, ... 2050.0 in units of 0.01 s
        dw 1400, 1370, 1310, 1270, 1250, 1250, 1250, 1250, 1250, 1250
        dw 1230, 1200, 1140, 1060, 960, 860, 750, 660, 600, 570
        dw 560, 570, 590, 620, 650, 680, 710, 730, 750, 770
        dw 780, 788, 754, 640, 541, 292, 161, -128, -324, -454
        dw -511, -542, -546, -579, -564, -566, -601, -664, -647, -576
        dw -374, -154, 264, 537, 775, 1046, 1336, 1601, 1824, 2025
        dw 2116, 2241, 2349, 2386, 2434, 2402, 2387, 2386, 2373, 2396
        dw 2433, 2530, 2624, 2728, 2825, 2915, 2997, 3072, 3135, 3218
        dw 3315, 3400, 3503, 3654, 3829, 4018, 4223, 4448, 4646, 4853
        dw 5054, 5217, 5379, 5487, 5582, 5686, 5831, 5998, 6163, 6297
        dw 6383, 6430, 6457, 6484, 6546, 6607, 6660, 6728, 6810, 6897
        dw 6936, 6929, 6918, 6911, 6939, 6969, 7002, 7038, 7076, 7117
        dw 7160, 7205, 7253, 7304, 7356, 7412
end_:
