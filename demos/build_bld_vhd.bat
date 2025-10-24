@echo off
REM === Clean and rebuild Ada project ===
alr clean
if errorlevel 1 exit /b 1

alr build
if errorlevel 1 exit /b 1

REM === Convert ELF to raw binary ===
riscv64-elf-objcopy -O binary bin\bios bin\bios.bin
if errorlevel 1 exit /b 1

REM === Pad binary to multiple of 4 bytes ===
for %%F in (bin\bios.bin) do (
    set /a size=%%~zF
)
set /a rem=%size% %% 4
if %rem% neq 0 (
    set /a pad=4 - %rem%
    fsutil file createnew bin\pad.tmp %pad% >nul
    copy /b bin\bios.bin+bin\pad.tmp bin\bios.bin >nul
    del bin\pad.tmp
)

REM === Generate VHDL image ===
image_gen -bld_vhd bin\bios.bin ../../neorv32/rtl/core/neorv32_bootloader_image.vhd
if errorlevel 1 exit /b 1

echo.
echo Build and image generation complete!
pause