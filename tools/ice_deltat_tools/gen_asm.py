import os
here=os.path.dirname(os.path.abspath(__file__))
grid=[(int(l.split()[0]),float(l.split()[1])) for l in open(os.path.join(here,'data/dt_grid.txt')) if not l.startswith('#')]
assert grid[0][0]==1800 and grid[-1][0]==2050 and len(grid)==126
cs=[round(v*100) for y,v in grid]
NMAX=len(cs)-2   # highest usable interval index
tab='\n'.join('        dw '+', '.join(str(x) for x in cs[i:i+10]) for i in range(0,len(cs),10))
asm=f"""; Replacement for ICE 0.51 SUBROUTINE DELTAT(YEAR,DT)  (seg 001C:3480, MS FORTRAN far/pascal conv.)
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
        or   dx, dx                 ; clamp I to 0 .. {NMAX}
        jl   .lo
        jg   .hi
        cmp  ax, {NMAX}
        jbe  .ok
.hi:    mov  ax, {NMAX}
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
{tab}
end_:
"""
open(os.path.join(here,'deltat_new.asm'),'w').write(asm)
