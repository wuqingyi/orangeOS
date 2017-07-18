pmtest1.bin:pmtest1.asm
    nasm pmtest1.asm -o $@
    dd if=pmtest1.bin of=a.img bs=512 count=1 conv=notrunc