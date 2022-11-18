;Emilio Buffey

MyStack SEGMENT STACK                                                         ;Stack segment
	DW 256(?)                               ;stack for interrupt handling
MyStack ENDS
;------------------------------------------------------------------------------Data segment 
MyData SEGMENT
	CRTMemSeg EQU 0B800h                    ;segment for video RAM

	BackSpace EQU 8            ;backspace character
	EscapeKey EQU 1bh          ;escape character
	F1Key EQU 3B00h            ;F1 character
	RightArrow EQU 26          ;right arrow character
	LeftArrow EQU  27          ;left arrow character
    	typedCharPos DW 640        ;screen position of typed characters
    	charMatch DB 0
    	
	S0 DB "How are you today.",0
	S1 DB "I am fine.",0
	S2 DB "I am hungry.",0
	S3 DB "Terminate the program.",0
	S4 DB "Do you like long walks on the beach.",0
	S5 DB "You don't like cats.",0
	S6 DB "Green beans are green.",0
	S7 DB "Delete the character to the left.",0
	S8 DB "Food is very good.",0
	S9 DB "Larue is tall.",0

	sentences DW S0,S1,S2,S3,S4,S5,S6,S7,S8,S9    	
    	
    	startTimer DB 0            ;starts the timer if it gets a 1
    	decimal DB 1               ;if decimal is needed
    	decimalTicks DW 0          ;stores a decimal for the timer
    	ticks DB 0                 ;number of ticks  	
	
MyData ENDS
;------------------------------------------------------------------------------Code segment
MyCode SEGMENT
	ASSUME CS:MyCode, DS:MyData
Main PROC                                       ;main proc
	
	MOV AX, CRTMemSeg           ;make ES segment register address video memory
	MOV ES, AX
	MOV AX, MyData              ;Makes DS address our data segment
	MOV DS, AX	
	MOV DL, 0
	
startOver:
	CALL ClearScreen
	CALL copySentenceToScreen
	CALL setupCursor
	CALL getKeys

theEnd:
	MOV AH, 4Ch                 ;These instructions use a DOS interrupt to relase
	INT 21h                     ;the memory for the program then return it 
Main ENDP
;------------------------------------------------------------------------------
copySentenceToScreen PROC
	;on entry [SI] points to beginning of string
	;ES:[DI] points to the starting positon on the screen
	;on exit no registers change

	PUSH AX BX DX SI DI    
	MOV DI, 160 * 2 
	MOV AH, 00h		
	INT 1Ah			;get timer ticks
	MOV AX, DX		
	MOV DX, 0               ;clearing DX
	MOV BX, 10
	DIV BX			;remainder is DX
	ADD DX, DX		
	LEA SI, sentences	;points to the address of S0
	ADD SI, DX
	MOV SI, [SI]

sentenceToScreenLoop:
	MOV AX, [SI]             ;moves character into SI
	MOV AH, 00001111b
	MOV ES:[DI], AX          ;displays character on screen
	ADD DI, 2                ;screen position moves over one to the right
	INC SI                   ;next position in the sentence
	CMP AL, 0                ;see is CX is zero
	JNZ sentenceToScreenLoop
	ADD DI, 160
	POP DI SI DX BX AX
	
	RET                      ;RETURN
copySentenceToScreen ENDP
;------------------------------------------------------------------------------
setupCursor PROC            ;Sets up the cursor 
   	;on entry,
   	;on exit,
	PUSH AX BX
	MOV DH, 4       ;what column
	MOV BH, 0       ;what page
	MOV AH, 02h     ;get cursor position
	INT 10h
	POP BX AX

	RET	        ;RETURN
setupCursor ENDP
;------------------------------------------------------------------------------
escape PROC               ;when escape key is clicked the program is terminated
	;on entry,
   	;on exit,
	MOV AH, 4Ch
	INT 21h	
	
	RET           ;RETURN
escape ENDP
;------------------------------------------------------------------------------
F1 PROC                      ;when F1 is clicked                     
	;on entry,
   	;on exit,
	MOV startTimer, 0
	MOV ticks, 0
	MOV typedCharPos, 640
	MOV Decimal, 1
	MOV decimalTicks, 0
	MOV AX, 0
	MOV BX, 0
	MOV CX, 0
	MOV DX, 0
	MOV SI, 0
	MOV DI, 0

	RET                 ;RETURN
F1 ENDP
;------------------------------------------------------------------------------
getRightArrow PROC
	;on entry,
   	;on exit,
	INC DL               ;move cursor up a space
	CALL setUpCursor
	ADD typedCharPos, 2  ;get character position
	MOV DI, typedCharPos 
	MOV ES:[DI], AX      

	RET                  ;RETURN
getRightArrow ENDP
;------------------------------------------------------------------------------
getLeftArrow PROC
	;on entry,
   	;on exit,

	DEC DL               ;move cursor back a space
	CALL setUpCursor
	SUB typedCharPos, 2  ;gets character position
	MOV DI, typedCharPos 
	MOV ES:[DI], AX      
	
	RET                  ;RETURN
getLeftArrow ENDP
;------------------------------------------------------------------------------
ClearScreen PROC
	;on entry,
   	;on exit,
	MOV CX, 2000         ;position of typed chars
keepClearing:
	MOV AH, 00001111b   ;black screen
	MOV AL, ' '         ;AL becomes a space
	MOV ES:[DI], AX     ;displays 
	ADD DI, 2           ;gets next char
	DEC CX              
	CMP CX, 0           ;is CX zero
	JNZ keepClearing    ;keep going if not
	
	RET                 ;RETURN
ClearScreen ENDP
;----------------------------------------------------------------------------- 
getBackSpace PROC
	;on entry,
   	;on exit,
 	DEC DL               ;decrements cursor
 	CALL setUpCursor     ;gets cursor
 	SUB typedCharPos, 2  ;gets character position
 	MOV DI, typedCharPos 
	MOV AH, 00001111b
	MOV AL, ' '          ;replaces character with space
	MOV ES:[DI], AX      ;displays
	
	RET                  ;RETURN
getBackSpace ENDP
;------------------------------------------------------------------------------
getTime PROC
	;on entry,
   	;on exit,
        PUSH AX CX DX

        CMP startTimer, 1        ;Start timer if 1
        JNE notTime              ;If not, do not start timer
        CMP ticks, 0             ;checks to see if started
        JNE keepGoing            ;If so,count ticks 
        MOV AH, 00h              ;Else, get the number of timer ticks
        INT 1Ah
        MOV decimalTicks, DX     ;Move DX into decimalTicks
        INC ticks                ;Increment the ticks
        
keepGoing:
        MOV AH, 00h              ;Get number of ticks
        INT 1Ah
        SUB DX, decimalTicks     ;stores  DX minus decimalTicks
        PUSH AX BX DI            ;Push registers AX BX DI again
        MOV BX, DX               ;Move DX into BX
        MOV AX, 55               ;Move 55 into AL
        MUL BX                   ;DX:AX := AX * BX, AX := AL * BL
        MOV BX, 100              ;Move 100 into BL 
        DIV BX                   ;Divide BX
        MOV DI, ((160)+80)       ;Display timer 
    
        CALL convertToAscii      ;convert the timer into Ascii
        POP DI BX AX
    
    notTime:    
        POP DX CX AX
        
        RET                      ;RETURN
getTime ENDP
;------------------------------------------------------------------------------
convertToAscii PROC 
	;on entry,
   	;on exit,
        PUSH BX DX                  
    
convert:
        MOV DX, 0                  ;Clear DX
        MOV BX, 10                 ;Move 10 into BX
        DIV BX                     ;DX = remainder                          								 
        ADD DL, BYTE PTR '0'       ;Convert DL to ASCII
        MOV ES:[DI], DL            ;Puts it on the screen
        SUB DI, 2                  ;Move to previous screen position
        CMP Decimal, 1             ;sees if there is a decimal
        JNE noDecimal              ;If not, jump to noDecimal
        CMP DL, 9                  ;Is DL greater than 9
        JLE noDecimal              ;If not, noDecimal
        MOV ES:[DI], BYTE PTR '.'  ;Else, put decimal on screen
        SUB DI, 2                  ;go to previous screen position
        DEC Decimal                ;Decrement the decimal
noDecimal:    
        CMP AX, 0                  ;is anything AX
        JNE convert                ;yes, loop back to convert
        MOV Decimal, 1             ;Else, make it 1        
        POP DX BX
        
        RET                        ;RETURN
convertToAscii ENDP
;------------------------------------------------------------------------------
getKeys PROC                ;get the keys from the keyboard
	;on entry,
   	;on exit,
viewKey:
	CALL getTime          
	MOV AH, 11h          ;look for key in buffer
	INT 16h
	JZ notKey
	JMP keyFound
	
keyFound:
	MOV startTimer, 1    ;starts the timer
	MOV AH, 10h          ;reads key from buffer
	INT 16h
	
	CMP AL, EscapeKey    ;was escape clicked
	JE doEscape          ;yes, go to  doEscape
	CMP AL, BackSpace    ;was backspace clicked 
	JE doBackSpace       ;yes, go to doBackSpace 
	CMP AX, F1Key        ;was f1 clicked
	JE doF1              ;yes, go to doF1
	CMP AL, RightArrow   ;was right arrow clicked
	JE doRightArrow      ;yes, go to doRightArrow
	CMP AL, LeftArrow    ;was left arrow clicked
	JE doLeftArrow       ;yes, go to doLeftArrow
	CMP AL, 32           ;is it a valid character
	JL notKey
	CMP AL, 127          ;is it a valid character
	JG notKey
	MOV DI, typedCharPos
	MOV ES:[DI], AL      ;display character on the screen
	ADD typedCharPos, 2  ;next character
	INC DL               ;move cursor
	CALL setUpCursor
	JMP viewKey
	
doEscape:
	CALL escape   
	JMP theEnd

doBackSpace:
	CALL getBackSpace
	JMP keyFound         ;keep going

doF1:
	CALL F1              
	JMP startOver        ;go to above the CALLS
	
doRightArrow:
	CALL getRightArrow
	JMP keyFound         ;keep going
	
doLeftArrow:
	CALL getLeftArrow
	JMP keyFound         ;keep going
	
notKey:
	JMP viewKey 
	
	RET                  ;RETURN
getKeys ENDP             
;------------------------------------------------------------------------------	
compareStrings PROC
	

compareStrings ENDP
;------------------------------------------------------------------------------	
MyCode ENDS
;------------------------------------------------------------------------------End of soure code
END Main