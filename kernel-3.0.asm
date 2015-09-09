;##############################################################
;###    Name       : D.A.U.Nanayakkara                      ###
;###    Index No   : 090342F                                ###
;###    Assignment : 01                                     ###
;###                                                        ###
;##############################################################
;*****************start of the kernel code*********************
[org 0x000]
[bits 16]

[SEGMENT .text]

;START #####################################################
    mov ax, 0x0100			;location where kernel is loaded
    mov ds, ax
    mov es, ax
    
    cli
    mov ss, ax				;stack segment
    mov sp, 0xFFFF			;stack pointer at 64k limit
    sti

    push dx
    push es
    xor ax, ax
    mov es, ax
    cli
    mov word [es:0x21*4], _int0x21	; setup interrupt service
    mov [es:0x21*4+2], cs
    sti
    pop es
    pop dx

    mov si, strWelcomeMsg   ; load message
    mov al, 0x01            ; request sub-service 0x01
    int 0x21
    
    call _display_endl
    mov si, strMessage      ; load message
    mov al, 0x01            ; request sub-service 0x01
    int 0x21

	call _shell				; call the shell
    
    int 0x19                ; reboot
;END #######################################################

_int0x21:
    _int0x21_ser0x01:       ;service 0x01
    cmp al, 0x01            ;see if service 0x01 wanted
    jne _int0x21_end        ;goto next check (now it is end)
    
	_int0x21_ser0x01_start:
    lodsb                   ; load next character
    or  al, al              ; test for NUL character
    jz  _int0x21_ser0x01_end
    mov ah, 0x0E            ; BIOS teletype
    mov bh, 0x00            ; display page 0
    mov bl, 0x07            ; text attribute
    int 0x10                ; invoke BIOS
    jmp _int0x21_ser0x01_start
    _int0x21_ser0x01_end:
    jmp _int0x21_end

    _int0x21_end:
    iret

_shell:
	_shell_begin:
	;move to next line
	call _display_endl

	;display prompt
	call _display_prompt

	;get user command
	call _get_command
	
	;split command into components
	call _split_cmd

	;check command & perform action

	; empty command
	_cmd_none:		
	mov si, strCmd0
	cmp BYTE [si], 0x00
	jne	_cmd_ver		        ;next command
	jmp _cmd_done
	
	; display version
	_cmd_ver:		
	mov si, strCmd0
	mov di, cmdVer
	mov cx, 4
	repe	cmpsb
	jne	_cmd_help		        ;next command
	
	call _display_endl
	mov si, strOsName		    ;display OS name
	mov al, 0x01
    int 0x21
	call _display_space
	mov si, txtVersion		    ;display version
	mov al, 0x01
    int 0x21
	call _display_space

	mov si, strMajorVer		
	mov al, 0x01
    int 0x21
	mov si, strMinorVer
	mov al, 0x01
    int 0x21
	jmp _cmd_done
;===============================================================================	
	
	;help
	
_cmd_help:	
	mov si, strCmd0
	mov di, cmdHelp
	mov cx, 4
	repe	cmpsb
	jne	_cmd_sysinfo		        ;if not equal jump to exit
	   
	call _display_endl		
	mov si, cmdVer		        
	mov al, 0x01
	int 0x21
	
	call _display_endl		
	mov si, cmdSysinfo		    
	mov al, 0x01
	int 0x21
	
	call _display_endl		
	mov si, cmdExit		        
	mov al, 0x01
	int 0x21
	
	call _display_endl		
	mov si, cmdHelp             
	mov al, 0x01
	int 0x21
	
	jmp _cmd_done
	
;===============================================================================
;--------------------------CODE to print System Details-------------------------
;-------------------------------------------------------------------------------
;===============================================================================
      
_cmd_sysinfo:
	mov si, strCmd0
	mov di, cmdSysinfo
	mov cx, 4
	repe	cmpsb
	jne	_cmd_exit		        ;if not equal jump to exit

    call _display_endl		
	mov si, details_header		;Display the system info header
	mov al, 0x01
	int 0x21
    
    call _display_endl	
    call _display_endl	

;###############################################################################	    
    
    ;CPU details 
    push eax
    push ebx
    push ecx
    push edx
    push si
    push edi
    
               
    mov si, cpuinfo
    mov al, 0x01
    int 0x21
   
    xor eax, eax	                ; when eax = 0 cpuid gives the vendor id 
    cpuid                           ; call cpuid 
    mov edi, tmp                    ; starting memory location of temporary storage is stored in edi
    mov [edi], ebx              
    add edi, 0x4 
    mov [edi], edx
    add edi, 0x4
    mov [edi], ecx
    add edi, 0x04
    mov BYTE [edi], 0x00
    
    mov si, tmp
    mov al, 0x01
    int 0x21
    
    call _display_endl	
    mov si, blank                   ; blank line is printed 
    mov al, 0x01
    int 0x21
   
    mov eax, 0x80000000             ; checks whether the cpu suports the brand string
    cpuid
    cmp eax, 0x80000004         
    jge _cpu_brand_string           ; if supports print the brand string
    jmp _floppy                     ; if not check floppy drives
    
    mov si, brandString
    mov al, 0x01
    int 0x21  
    _cpu_brand_string:              ; prints the brand string
    mov eax, 0x80000002
    call _brand_string
    mov eax, 0x80000003
    call _brand_string
    mov eax, 0x80000004
    call _brand_string   
    
    call _cache                     ; prints cache info
    call _address_space             ; prints cpus virtual and physical address size
    
    pop edi
    pop si
    pop edx
    pop ecx
    pop ebx
    pop eax
    
;###############################################################################
    
_floppy:                            ; floppy detection
    xor eax, eax    
    int 0x11
    push ax                         ; saves data on ax on stack
    and ax, 0x01
    cmp ax, 0x01
    jne _no_floppy                  ; if no floppy print no floppy
    
    call _display_endl	
    mov si, hasFloppy
    mov al, 0x01
    int 0x21
    
    pop ax                          ; retrieve ax 
    push ax                         ; save ax
    
    shr al, 6
    and al, 0x03
    add al, 49
    
    mov ah, 0x0e
    int 0x10
    jmp _vga_mode
    
    _no_floppy:
    call _display_endl	
    mov si, noFloppy
    mov al, 0x01
    int 0x21
    jmp _vga_mode
    
    
;###############################################################################    
    
_vga_mode:
    
    call _display_endl	
    pop ax
    push ax
    mov al, bl
    shr al, 4
    and al, 0x3
    cmp al, 0x00
    jne _vga_1
     
    mov si, vga0                ; EGA, VGA, or PGA
    mov al, 0x01
    int 0x21
    jmp _PS_device
    
    _vga_1:
    cmp al, 0x01
    jne _vga_2
    
     
    mov si, vga1                ; 40x25 color.
    mov al, 0x01
    int 0x21
    jmp _PS_device
    
    _vga_2:
    cmp al, 0x02
    jne _vga_3
     
    mov si, vga2                ; 80x25 color.
    mov al, 0x01
    int 0x21
    jmp _PS_device 

    _vga_3:        
    mov si, vga3                ; 80x25 monochrome
    mov al, 0x01
    int 0x21
    
    
;###############################################################################
_PS_device:
    call _display_endl	
    
    mov si, psDevice
    mov al, 0x01
    int 0x21
    
    pop ax
    mov cx, ax
    
    shr al, 2
    and al, 0x01
    cmp al, 0x01
    jne _no_PS
    
    push cx
    
    mov si, installed
    mov al, 0x01
    int 0x21 
    
    pop ax
    
    shr al, 3           ; check whether PS device is in use
    and al, 0x01
    cmp al, 0x01
    jne _serial_port    ; if not jump to serial port
    
    mov si, unused
    mov al, 0x01
    int 0x21 
    jmp _serial_port
    
    _no_PS:
    mov si, notInstalled    
    mov al, 0x01
    int 0x21 
    

;###############################################################################
_serial_port: 
        
    call _display_endl	
    mov si, serial
    mov al, 0x01
    int 0x21
    
    xor eax, eax    
    int 0x11
       
    shr al, 9
    and al, 0x07
    add al, 48

    mov ah, 0x0e
    int 0x10   
          

;###############################################################################

_parallel_port:

    call _display_endl	
    mov si, parallel
    mov al, 0x01
    int 0x21
    
    xor eax, eax    
    int 0x11
   
    shr al, 14
    and al, 0x03
    add al, 48

    mov ah, 0x0e
    int 0x10    
        
    
;###############################################################################
     
_ram:
    
    call _display_endl	
    push si 
    push ax
    mov si, ram
    mov al, 0x01
    int 0x21    
  
    pop ax
    pop si
  
    xor cx, cx
    xor dx, dx
    mov eax, 0xe801
    int 0x15
    cmp cx, 0x00            ; if cx ix zero get the value from ax bx
    jne mem_check
    
    mov cx, ax
    mov dx, bx
    
    mem_check:    
    push cx                 ; save value of cx 
    shr cx, 6               ; divide by 64
    add dx, cx  
    shr dx, 4               ; to make the value in MB (dx * 2^6)/ 2^10
    add dx, 0x01            ; cx always gives one MB short(add one)
    call _hex2dec
    
    push si 
    push ax
  
    mov si, MBtag     
    mov al, 0x01
    int 0x21    
  
    pop ax
    pop si 
       
    ;call _cmd_done
;===============================================================================
_hardDrives:  
    
    call _display_endl	
    mov si, HardDrives     
    mov al, 0x01
    int 0x21    
    push ds
    mov ax, 0x0040
    mov ds,ax
    mov al, [ds:0x0075]             ; read 40:75 from memory (number of hard drives)
    pop ds
    add al, 48                      ; add 48 to get ASCII char
    mov ah, 0x0e
    int 0x10
  
    jmp _cmd_done
    
       
;===============================================================================
;---------------------------END of SYSTEM DETAILS-------------------------------
;-------------------------------------------------------------------------------
;===============================================================================

	; exit shell
	_cmd_exit:		
	mov si, strCmd0
	mov di, cmdExit
	mov cx, 5
	repe	cmpsb
	jne	_cmd_unknown		    ;next command

	je _shell_end			    ;exit from shell

	_cmd_unknown:
	call _display_endl
	mov si, msgUnknownCmd		;unknown command
	mov al, 0x01
    int 0x21

	_cmd_done:

	;call _display_endl
	jmp _shell_begin
	
	_shell_end:
	ret


;===============================================================================
;--------------------------------CPUID------------------------------------------
;===============================================================================

    ;fucnction to get cpu brand string

_brand_string:
    
    
    cpuid                       ; calls for cpu id
    mov edi, tmp                ; get the starting address of tmp to EDI
    mov [edi], eax              ; copy EAX to tmp
    add edi, 0x4                ; move EDI pointer 4 bytes into the tmp
    mov [edi], ebx              
    add edi, 0x4 
    mov [edi], ecx
    add edi, 0x4
    mov [edi], edx
    add edi, 0x04
    mov BYTE [edi], 0x00        ; append the end of line charachter to the tmp
    
    mov si, tmp                 ; print the string in tmp
    mov al, 0x01
    int 0x21    
        
    ret 
 ; function to get L2 cache info   
_cache:
    push ecx
    push ebx
    call _display_endl
    mov si, cache
    mov al, 0x01
    int 0x21
    
    mov eax, 0x80000006
    cpuid
    
    mov ebx, ecx
    and ebx, 0xFF               ; low byte stores the L2 cache line size in bytes
    mov dx, bx
    call _hex2dec
    
    
    mov ebx, ecx
    shr ebx, 12
    and ebx, 0x0F
    cmp ebx, 0
   ; jne _DirMap 
            
    pop ebx
    pop ecx
    
    ret
  ; function to get cpu supported virtual and physical address space  
 _address_space:
  
    push eax
    push ebx
 
    call _display_endl
    mov si, virtual
    mov al, 0x01
    int 0x21
        
    mov eax, 0x80000008
    cpuid
    mov ebx, eax
    shr ebx, 8
    and ebx, 0xFF
    mov dx, bx
    call _hex2dec
    
    push ax    
    call _display_endl
    mov si, physical
    mov al, 0x01
    int 0x21
    pop ax
    
    mov ebx, eax
    and ebx, 0xFF
    mov dx, bx
    call _hex2dec
        
    pop ebx
    pop eax
    ret
    
    
    
;===============================================================================
;-------------------------hex to decimal conversion-----------------------------
;===============================================================================


_hex2dec:                        ; input is DX

	push ax                     ; save AX
	push bx                     ; save CX
	push cx                     ; save DX
	push si                     ; save SI
	mov ax,dx                   ; copy number into AX
	mov si,10                   ; SI will be our divisor
	xor cx,cx                   ; clear CX

non_zero:

	xor dx,dx                   ; clear XX
	div si                      ; divide by 10
	push dx                     ; push number onto the stack
	inc cx                      ; increment CX to do it more times
	or ax,ax                    ; end of the number?
	jne non_zero                 
	
write_digits:

	pop dx                      ; get the digit off DX
	add dl,30h                  ; add 30h to it to make it decimal (ASCII)
	
	mov al, dl                  ; print character
	mov ah, 0x0e
    int 0x10         
    ;call print_char            ; print number
	loop write_digits           ; print all the numbers

   
	pop si                      ; restore SI
	pop cx                      ; restore DX
	pop bx                      ; restore CX
	pop ax                      ; restore AX
	ret                         
	

;===============================================================================

_get_command:
	;initiate count
	mov BYTE [cmdChrCnt], 0x00
	mov di, strUserCmd

	_get_cmd_start:
	mov ah, 0x10		        ;get character
	int 0x16

	cmp al, 0x00		        ;check if extended key
	je _extended_key
	cmp al, 0xE0		        ;check if new extended key
	je _extended_key

	cmp al, 0x08		        ;check if backspace pressed
	je _backspace_key

	cmp al, 0x0D		        ;check if Enter pressed
	je _enter_key

	mov bh, [cmdMaxLen]		    ;check if maxlen reached
	mov bl, [cmdChrCnt]
	cmp bh, bl
	je	_get_cmd_start

	;add char to buffer, display it and start again
	mov [di], al			    ;add char to buffer
	inc di					    ;increment buffer pointer
	inc BYTE [cmdChrCnt]	    ;inc count

	mov ah, 0x0E			    ;display character
	mov bl, 0x07
	int 0x10
	jmp	_get_cmd_start

	_extended_key:			    ;extended key - do nothing now
	jmp _get_cmd_start

	_backspace_key:
	mov bh, 0x00			    ;check if count = 0
	mov bl, [cmdChrCnt]
	cmp bh, bl
	je	_get_cmd_start		    ;yes, do nothing
	
	dec BYTE [cmdChrCnt]	    ;dec count
	dec di

	;check if beginning of line
	mov	ah, 0x03		        ;read cursor position
	mov bh, 0x00
	int 0x10

	cmp dl, 0x00
	jne	_move_back
	dec dh
	mov dl, 79
	mov ah, 0x02
	int 0x10

	mov ah, 0x09		        ; display without moving cursor
	mov al, ' '
    mov bh, 0x00
    mov bl, 0x07
	mov cx, 1			        ; times to display
    int 0x10
	jmp _get_cmd_start

	_move_back:
	mov ah, 0x0E		        ; BIOS teletype acts on backspace!
    mov bh, 0x00
    mov bl, 0x07
    int 0x10
	mov ah, 0x09		        ; display without moving cursor
	mov al, ' '
    mov bh, 0x00
    mov bl, 0x07
	mov cx, 1			        ; times to display
    int 0x10
	jmp _get_cmd_start

	_enter_key:
	mov BYTE [di], 0x00
	ret

_split_cmd:
	;adjust si/di
	mov si, strUserCmd
	;mov di, strCmd0

	;move blanks
	_split_mb0_start:
	cmp BYTE [si], 0x20
	je _split_mb0_nb
	jmp _split_mb0_end

	_split_mb0_nb:
	inc si
	jmp _split_mb0_start

	_split_mb0_end:
	mov di, strCmd0

	_split_1_start:			    ;get first string
	cmp BYTE [si], 0x20
	je _split_1_end
	cmp BYTE [si], 0x00
	je _split_1_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_1_start

	_split_1_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb1_start:
	cmp BYTE [si], 0x20
	je _split_mb1_nb
	jmp _split_mb1_end

	_split_mb1_nb:
	inc si
	jmp _split_mb1_start

	_split_mb1_end:
	mov di, strCmd1

	_split_2_start:			    ;get second string
	cmp BYTE [si], 0x20
	je _split_2_end
	cmp BYTE [si], 0x00
	je _split_2_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_2_start

	_split_2_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb2_start:
	cmp BYTE [si], 0x20
	je _split_mb2_nb
	jmp _split_mb2_end

	_split_mb2_nb:
	inc si
	jmp _split_mb2_start

	_split_mb2_end:
	mov di, strCmd2

	_split_3_start:			    ;get third string
	cmp BYTE [si], 0x20
	je _split_3_end
	cmp BYTE [si], 0x00
	je _split_3_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_3_start

	_split_3_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb3_start:
	cmp BYTE [si], 0x20
	je _split_mb3_nb
	jmp _split_mb3_end

	_split_mb3_nb:
	inc si
	jmp _split_mb3_start

	_split_mb3_end:
	mov di, strCmd3

	_split_4_start:			    ;get fourth string
	cmp BYTE [si], 0x20
	je _split_4_end
	cmp BYTE [si], 0x00
	je _split_4_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_4_start

	_split_4_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb4_start:
	cmp BYTE [si], 0x20
	je _split_mb4_nb
	jmp _split_mb4_end

	_split_mb4_nb:
	inc si
	jmp _split_mb4_start

	_split_mb4_end:
	mov di, strCmd4

	_split_5_start:			    ;get last string
	cmp BYTE [si], 0x20
	je _split_5_end
	cmp BYTE [si], 0x00
	je _split_5_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_5_start

	_split_5_end:
	mov BYTE [di], 0x00

	ret

_display_space:
    push ax
    push bx                                 ; save registers   
    
	mov ah, 0x0E                            ; BIOS teletype
	mov al, 0x20
    mov bh, 0x00                            ; display page 0
    mov bl, 0x07                            ; text attribute
    int 0x10                                ; invoke BIOS
	
	pop bx
	pop ax
	
	ret

_display_endl:

    push ax
    push bx
	mov ah, 0x0E		; BIOS teletype acts on newline!
    mov al, 0x0D
	mov bh, 0x00
    mov bl, 0x07
    int 0x10
	mov ah, 0x0E		; BIOS teletype acts on linefeed!
    mov al, 0x0A
	mov bh, 0x00
    mov bl, 0x07
    int 0x10
    
    pop bx
    pop ax
	ret

_display_prompt:

    push si
    push ax
	mov si, strPrompt
	mov al, 0x01
	int 0x21
	
	pop ax
	pop si
	ret

[SEGMENT .data]
    strWelcomeMsg   db  "Welcome to JOSH Ver 0.04", 0x00
    strMessage      db  "Type help to get the list of commands", 0x00
	strPrompt		db	"JOSH>>", 0x00
	cmdMaxLen		db	255			;maximum length of commands
    details_header  db  "=================== JOSH System Information ===================", 0x00
	
	
	strOsName		db	"JOSH", 0x00	;OS details
	strMajorVer		db	"0", 0x00
	strMinorVer		db	".04", 0x00
    cpuinfo         db  "CPU Details        : ", 0x00
    blank           db  "                     ", 0x00
    cache           db  "L2 cache           : ", 0x00
    brandString     db  "Brand String       : ", 0x00
    virtual         db  "Virtual Address size (bits)  : ", 0x00
    physical        db  "Physical Address size (bits) : ", 0x00
    
    hasFloppy       db  "Floppy drive(s)    : ", 0x00
    noFloppy        db  "Floppy drive(s)    : Not present", 0x00

    vga0            db  "VGA Mode           : EGA, VGA, or PGA ", 0x00
    vga1            db  "VGA Mode           : 40x25 color", 0x00
    vga2            db  "VGA Mode           : 80x25 color", 0x00
    vga3            db  "VGA Mode           : 80x25 monochrome", 0x00

    psDevice        db  "Pointing Device    : ", 0x00
    installed       db  "Installed", 0x00 
    notInstalled    db  "Not Installed",0x00
    unused          db  " [Unused]", 0x00

    serial          db  "Serial port(s)     : ", 0x00
    parallel        db  "parallel port(s)   : ", 0x00

    ram             db  "RAM                : ", 0x00
    error           db  "Error! ", 0x00
    MBtag           db  " MB", 0x00

    HardDrives      db  "HardDrive(s)       : ", 0x00

    divider         db  10, 0x00  

	cmdVer			db	"ver", 0x00		; internal commands
	cmdExit			db	"exit", 0x00
	cmdSysinfo      db  "sysinfo", 0x00
    cmdHelp         db  "help", 0x00
    

	txtVersion		db	"version", 0x00	;messages and other strings
	msgUnknownCmd	db	"Unknown command or bad file name! Type help to get the command list", 0x00

[SEGMENT .bss]
	strUserCmd	resb	256		;buffer for user commands
	cmdChrCnt	resb	1		;count of characters
	strCmd0		resb	256		;buffers for the command components
	strCmd1		resb	256
	strCmd2		resb	256
	strCmd3		resb	256
	strCmd4		resb	256
    tmp         resb    256     ;temp buffer to store strings

;********************end of the kernel code********************
