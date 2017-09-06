org 07c00h

BaseOfStack equ 07c00h

%include "load.inc"

    jmp short LABEL_START
    nop

%include "fat12hdr.inc"

LABEL_START:
    mov ax, cs
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, BaseOfStack

    ;清屏
    mov ax, 0600h
    mov bx, 0700h
    mov cx, 0
    mov dx, 184fh
    int 10h

    ;显示"Booting  "
    mov dh, 0
    call DispStr

    ;软驱复位
    xor ah, ah
    xor dl, dl
    int 13h

    ;下面再A盘根目录寻找LODER.BIN
    mov word [wSectorNo], SectorNoOfRootDirectory
LABEL_SEARCH_IN_ROOT_DIR_BEGIN:
    cmp word[wRootDirSizeForLoop], 0            ;判断根目录是否已经读完
    jz LABEL_NO_LOADERBIN                       ;如果读完表示没找到loader.bin
    dec word [wRootDirSizeForLoop]
    mov ax, BaseOfLoader
    mov es, ax
    mov bx, OffsetOfLoader
    mov ax, [wSectorNo]
    mov cl, 1
    call ReadSector

    mov si, LoaderFileName
    mov di, OffsetOfLoader
    cld
    mov dx, 10h                         ;一个扇区有16个条目
LABEL_SEARCH_FOR_LOADERBIN:
    cmp dx, 0
    jz LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR
    dec dx
    mov cx, 11                          ;文件名长度11字节
LABEL_CMP_FILENAME:
    cmp cx, 0
    jz LABEL_FILENAME_FOUND
    dec cx
    lodsb
    cmp al, byte [es:di]
    jnz LABEL_DIFFERENT
    inc di
    jmp LABEL_CMP_FILENAME
LABEL_DIFFERENT:
    and di, 0ffe0h
    add di, 20h
    mov si, LoaderFileName
    jmp LABEL_SEARCH_FOR_LOADERBIN

LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR:
    add word [wSectorNo], 1
    jmp LABEL_SEARCH_IN_ROOT_DIR_BEGIN

LABEL_NO_LOADERBIN:
    mov dh, 2
    call DispStr
    jmp $

LABEL_FILENAME_FOUND:
    mov ax, RootDirSectors
    and di, 0ffe0h
    add di, 01Ah                ;跳到记录条目对应的开始簇号的位置
    mov cx, word [es:di]
    push cx
    add cx, ax
    add cx, DeltaSectorNo
    mov ax, BaseOfLoader
    mov es, ax
    mov bx, OffsetOfLoader
    mov ax, cx

LABEL_GOON_LOADING_FILE:
    push ax
    push bx
    mov ah, 0eh
    mov al, '.'
    mov bl, 0fh
    int 10h
    pop bx
    pop ax

    mov cl, 1
    call ReadSector
    pop ax
    call GetFATEntry
    cmp ax, 0fffh
    jz LABEL_FILE_LOADED
    push ax
    mov dx, RootDirSectors
    add ax, dx
    add ax, DeltaSectorNo
    add bx, [BPB_BytePerSec]
    jmp LABEL_GOON_LOADING_FILE
LABEL_FILE_LOADED:
    mov dh, 1
    call DispStr

    jmp BaseOfLoader:OffsetOfLoader
;============================================================================
;变量
;----------------------------------------------------------------------------
wRootDirSizeForLoop	dw	RootDirSectors	; Root Directory 占用的扇区数, 在循环中会递减至零.
wSectorNo		dw	0		; 要读取的扇区号
bOdd			db	0		; 奇数还是偶数

;============================================================================
;字符串
;----------------------------------------------------------------------------
LoaderFileName		db	"LOADER  BIN", 0	; LOADER.BIN 之文件名
; 为简化代码, 下面每个字符串的长度均为 MessageLength
MessageLength		equ	9
BootMessage:		db	"Booting  "; 9字节, 不够则用空格补齐. 序号 0
Message1		    db	"Ready.   "; 9字节, 不够则用空格补齐. 序号 1
Message2		    db	"No LOADER"; 9字节, 不够则用空格补齐. 序号 2
;============================================================================

;----------------------------------------------------------------------------
; 函数名: DispStr
;----------------------------------------------------------------------------
; 作用:
;	显示一个字符串, 函数开始时 dh 中应该是字符串序号(0-based)
DispStr:
    mov ax, MessageLength
    mul dh
    add ax, BootMessage
    mov bp, ax
    mov ax, ds
    mov es, ax
    mov cs, MessageLength
    mov ax, 1301h
    mov bx, 0007h
    mov dl, 0
    int 10h
    ret

;----------------------------------------------------------------------------
; 函数名: ReadSector
;----------------------------------------------------------------------------
; 作用:
;	从第 ax 个 Sector 开始, 将 cl 个 Sector 读入 es:bx 中
ReadSector:
	; -----------------------------------------------------------------------
	; 怎样由扇区号求扇区在磁盘中的位置 (扇区号 -> 柱面号, 起始扇区, 磁头号)
	; -----------------------------------------------------------------------
	; 设扇区号为 x
	;                          ┌ 柱面号 = y >> 1
	;       x           ┌ 商 y ┤
	; -------------- => ┤      └ 磁头号 = y & 1
	;  每磁道扇区数      │
	;                   └ 余 z => 起始扇区号 = z + 1
    push bp
    mov bp, sp
    sub sp, 2

    mov byte [bp-2], cl
    push bx
    mov bl, [BPB_SecPerTrk]
    div bl                          ;余数保存在ah，商保存在al
    inc ah
    mov cl, ah                      ;cl=起始扇区数
    mov dh, al
    shr al, 1                       ;除以BPB_NumHeads=2
    mov ch, al                      ;ch=柱面号
    and dh, 1                       ;dh=磁头号
    pop bx

    mov dl, [BS_DrvNum]             ;驱动器号
.GoOnReading:
    mov ah, 2
    mov al, byte[bp-2]              ;al=要读取扇区数
    int 13h
    jc .GoOnReading

    add sp, 2
    pop bp

    ret


;----------------------------------------------------------------------------
; 函数名: GetFATEntry
;----------------------------------------------------------------------------
; 作用:
;	找到序号为 ax 的 Sector 在 FAT 中的条目, 结果放在 ax 中
;	需要注意的是, 中间需要读 FAT 的扇区到 es:bx 处, 所以函数一开始保存了 es 和 bx
GetFATEntry:
	push	es
	push	bx
	push	ax
	mov	ax, BaseOfLoader	; ┓
	sub	ax, 0100h		; ┣ 在 BaseOfLoader 后面留出 4K 空间用于存放 FAT
	mov	es, ax			; ┛
	pop	ax
	mov	byte [bOdd], 0
	mov	bx, 3
	mul	bx			; dx:ax = ax * 3
	mov	bx, 2
	div	bx			; dx:ax / 2  ==>  ax <- 商, dx <- 余数
	cmp	dx, 0
	jz	LABEL_EVEN
	mov	byte [bOdd], 1
LABEL_EVEN:;偶数
	xor	dx, dx			; 现在 ax 中是 FATEntry 在 FAT 中的偏移量. 下面来计算 FATEntry 在哪个扇区中(FAT占用不止一个扇区)
	mov	bx, [BPB_BytsPerSec]
	div	bx			; dx:ax / BPB_BytsPerSec  ==>	ax <- 商   (FATEntry 所在的扇区相对于 FAT 来说的扇区号)
					;				dx <- 余数 (FATEntry 在扇区内的偏移)。
	push	dx
	mov	bx, 0			; bx <- 0	于是, es:bx = (BaseOfLoader - 100):00 = (BaseOfLoader - 100) * 10h
	add	ax, SectorNoOfFAT1	; 此句执行之后的 ax 就是 FATEntry 所在的扇区号
	mov	cl, 2
	call	ReadSector		; 读取 FATEntry 所在的扇区, 一次读两个, 避免在边界发生错误, 因为一个 FATEntry 可能跨越两个扇区
	pop	dx
	add	bx, dx
	mov	ax, [es:bx]
	cmp	byte [bOdd], 1
	jnz	LABEL_EVEN_2
	shr	ax, 4
LABEL_EVEN_2:
	and	ax, 0FFFh

LABEL_GET_FAT_ENRY_OK:

	pop	bx
	pop	es
	ret
;----------------------------------------------------------------------------

times 	510-($-$$)	db	0	; 填充剩下的空间，使生成的二进制代码恰好为512字节
dw 	0xaa55				; 结束标志
