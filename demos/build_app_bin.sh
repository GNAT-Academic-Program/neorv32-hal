alr clean
alr build
riscv64-elf-objcopy -O binary bin/bios bin/bios.bin
truncate -s %4 bin/bios.bin
image_gen -app_bin bin/bios.bin bin/bios.exes