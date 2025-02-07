# neorv32_hal

A blog post explaining this project can be found here: [Open Source Ada from Gateware to Application](https://blog.adacore.com/open-source-ada-from-gateware-to-application)

<p align="center">
  <img src="https://github.com/GNAT-Academic-Program/neorv32_hal/blob/main/neorv32_ada.png?raw=true" alt="Header Image" style="width: 65%; height: auto;">
</p>

## Build the neorv32 Bitstream (for ULX3S dev board)

1. **Install Docker (if not already installed):**
   ```bash
   sudo apt update
   sudo apt install -y docker.io
   sudo systemctl enable --now docker
   ```
2. **Create a working directory:**
   ```bash
   cd ~ && mkdir neorv32_workspace && cd neorv32_workspace
   ```
3. **Pull the `neorv32/impl` container (includes all the required open-source tools):**
   ```bash
   sudo docker pull ghcr.io/stnolting/neorv32/impl:latest
   ```
4. **Run the container, mapping your current folder to `/workspace`:**
   ```bash
   sudo docker run --rm -it -v "$(pwd)":/workspace ghcr.io/stnolting/neorv32/impl bash
   cd /workspace
   ```
5. **Clone `neorv32-setups` (with submodules):**
   ```bash
   git clone --recurse-submodules https://github.com/stnolting/neorv32-setups.git
   ```
6. **(IMPORTANT): Modify the top-level VHDL file to adjust memory sizes for our BIOS firmware**  
   ```bash
   apt-get update && apt-get install nano
   nano neorv32-setups/osflow/board_tops/neorv32_ULX3S_BoardTop_MinimalBoot.vhd
   ```  
   Change the IMEM and DMEM lines to:  
   ```
   MEM_INT_IMEM_SIZE => 32*1024,
   MEM_INT_DMEM_SIZE => 16*1024
   ```

7. **Build the default SoC with minimal boot:**  
   ```bash
   cd neorv32-setups/osflow
   make BOARD=ULX3S MinimalBoot
   ```

8. **Quit the container:**  
   ```bash
   exit
   ```

---

## Program Your ULX3S FPGA Board Using Fujprog

1. **Install dependencies:**
   ```bash
   sudo apt install libftdi1-dev libusb-dev cmake make build-essential
   ```
2. **Clone fujprog:**
   ```bash
   cd ~/neorv32_workspace
   git clone https://github.com/kost/fujprog
   cd fujprog
   ```
3. **Build and install fujprog:**
   ```bash
   mkdir build && cd build
   cmake ..
   make
   sudo make install
   ```
4. **Program your board (adjust the path to your bitstream if needed):**
   ```bash
   fujprog ../../neorv32-setups/osflow/neorv32_ULX3S_MinimalBoot.bit
   ```

Now the neorv32 SoC is on your ULX3S dev board. Next, build and prepare the BIOS demo to familiarize yourself with the workflow.

---

## Set Up the neorv32 Bootloader Tools

1. **Compile `image_gen` (bundled with neorv32):**
   ```bash
   cd ~/neorv32_workspace/neorv32-setups/neorv32/sw/image_gen
   gcc image_gen.c -o image_gen
   sudo cp image_gen /usr/local/bin/
   ```
   > **Note:** If you encounter a permission error while compiling, ensure you’re in a directory where you have write access or use `sudo` appropriately.

2. **Ensure `riscv64-elf-objcopy` is in your `$PATH`:**  
   This tool is provided when Alire installs the RISC-V cross-compiler. Check its location with:
   ```bash
   which riscv64-elf-objcopy
   ```
   If nothing is returned, complete the **Build the Ada Demos Firmware (BIOS)** section first—Alire will install the entire RISC-V toolchain. Then add the toolchain’s directory (e.g., `~/.local/share/alire/toolchains/gnat_riscv64_elf_[version]/bin`) to your PATH:
   ```bash
   export PATH="$PATH:~/.local/share/alire/toolchains/gnat_riscv64_elf_[version]/bin"
   ```
   To make this change permanent, add that line to your `~/.bashrc` (or equivalent) and reload:
   ```bash
   source ~/.bashrc
   ```

---

## Build the Ada Demos Firmware (BIOS)

1. **Move back to the root of your workspace:**
   ```bash
   cd ~/neorv32_workspace
   ```
2. **Clone the repository from GitHub:**
   ```bash
   git clone https://github.com/GNAT-Academic-Program/neorv32_hal
   ```
   *(Alternatively, once it’s on Alire:)*
   ```bash
   alr get neorv32_hal
   ```
3. **Build using Alire:**
   ```bash
   cd neorv32_hal/demos
   alr build
   ```
4. **Convert and package the firmware:**
   ```bash
   riscv64-elf-objcopy -O binary bin/bios bin/bios.bin
   image_gen -app_bin bin/bios.bin bin/bios.exe
   ```

---

## Upload the Firmware to the neorv32 Bootloader

1. **Install a serial terminal for raw file transfers (e.g., gtkterm):**
   ```bash
   sudo apt install gtkterm
   ```
2. **Open gtkterm:**
   ```bash
   gtkterm --port /dev/ttyUSB0 --speed 19200
   ```
3. **Configure gtkterm:**
   - In the **Configuration** menu, set `CR LF auto`.
4. **Upload the firmware:**
   - At the `CMD:>` prompt in gtkterm, type `u`, then press **Ctrl+Shift+R**.
   - Select `bios.exe` and wait for the upload to complete.
   - At `CMD:>`, type `e`.

And voila!

---

## Use `neorv32_hal` in Your Project

1. **Add `neorv32_hal` as a dependency in your `alire.toml`:**
   ```toml
   [[depends-on]]
   neorv32_hal = "*"
   ```
2. **Modify your `your_project.gpr`:**
   ```ada
   with "bare_runtime.gpr";
   with "neorv32_hal.gpr";

   project Your_Project is
      for Target use "riscv64-elf";
      for Runtime ("Ada") use Bare_Runtime'Runtime ("Ada");

      package Linker is
         for Switches ("Ada") use ("-T", neorv32_Hal'Project_Dir & "/src/link.ld");
      end Linker;
   end Your_Project;
   ```

That’s it! You now have everything you need to generate the bitstream, program your FPGA, and start developing neorv32-based Ada projects. Happy hacking! 🚀
