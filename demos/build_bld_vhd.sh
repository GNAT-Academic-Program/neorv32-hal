alr clean
alr build
riscv64-elf-objcopy -O binary bin/bios bin/bios.bin
truncate -s %4 bin/bios.bin
image_gen -bld_vhd bin/bios.bin ../../neorv32/rtl/core/neorv32_bootloader_image.vhd