; Source equivalent of drive_c/DELAY.COM, a test-harness-only helper.
; Wait two seconds for pending AUTOTYPE key-release events before exit.
CODE SEGMENT
        ASSUME CS:CODE
        ORG 100H
START:  MOV AH,0
        INT 1AH
        MOV BX,DX
AGAIN:  MOV AH,0
        INT 1AH
        SUB DX,BX
        CMP DX,36
        JB AGAIN
        MOV AX,4C00H
        INT 21H
CODE ENDS
        END START
