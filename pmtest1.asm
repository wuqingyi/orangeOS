%include "pm.inc"

org 07c00h
jmp LABEL_BEGIN

[SECTION .gdt]
    LABEL_GDT: Descriptor           0,          0,                  0
    LABEL_DESC_CODE32: Descriptor   0,          SegCode32Len - 1,   DA_C + DA_32
    LABEL_DESC_VIDEO: Descriptor         0B8000h,     0FFFFh,             DA_DRW
    GdtLen equ $ - LABEL_GDT
    GdtPtr  dw GdtLen - 1
            dd 0

    SelectorCode32  equ LABEL_DESC_CODE32 - LABEL_GDT
    SelectorVideo   equ LABEL_DESC_VIDEO - LABEL_GDT

[SECTION .s16]
[BITS 16]
LABEL_BEGIN:
    mov ax, cs
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0100h

    ;初始化32位代码段
    xor eax, eax
    mov ax, cs
    shl eax, 4
    add eax, LABEL_SEG_CODE32
    mov word [LABEL_DESC_CODE32 + 2], ax
    shr eax, 16
    mov byte [LABEL_DESC_CODE32 + 4], al
    mov byte [LABEL_DESC_CODE32 + 7], ah

    ;为加载GDTR做准备
    xor eax, eax
    mov ax, ds
    shl eax, 4
    add eax, LABEL_GDT
    mov dword [GdtPtr + 2], eax

    ;加载GDTR
    lgdt [GdtPtr]

    ;关中断
    cli

    ;打开A20
    in al, 92h
    or al, 00000010b
    out 92h, al

    ;准备切换到保护模式
    mov eax, cr0
    or eax, 1
    mov cr0, eax

    ;进入保护模式
    jmp dword SelectorCode32:0

[SECTION .s32]
[BITS 32]
LABEL_SEG_CODE32:
    mov ax, SelectorVideo
    mov gs, ax
    mov edi, (80 * 11 + 79) * 2
    mov ah, 0Ch
    mov al, 'P'
    mov [gs:edi], ax

    jmp $

    SegCode32Len equ $ - LABEL_SEG_CODE32