with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with neorv32.UART0;
with neorv32;
with Uart0;
with MPU6050Driver; use MPU6050Driver;
with SPI; use SPI;
with Sysinfo;

package body Bios_Core is

   type Cmd_T is (Echo, Infos, Reload, Help, Unknown, SPI_Demo);
   Cmd : Cmd_T := Unknown;

   Cyan      : constant String := ASCII.ESC & "[38;2;0;255;255m";
   Blue      : constant String := ASCII.ESC & "[38;2;85;170;255m";
   Purple    : constant String := ASCII.ESC & "[38;2;170;85;255m";
   Pink      : constant String := ASCII.ESC & "[38;2;255;0;255m";
   Green     : constant String := ASCII.ESC & "[38;2;57;255;20m";

   Reset   : constant String := ASCII.ESC & "[0m";

   procedure Neorv32_Ada_Art is
   begin
      Put_Line (Cyan   & " _______                          ______ ______ " & Reset);
      Put_Line (Blue   & "|    |  |.-----.-----.----.--.--.|__    |__    |" & Reset);
      Put_Line (Purple & "|       ||  -__|  _  |   _|  |  ||__    |  " & Reset & Green & " ___" & Reset & Purple & "|" & Reset & Green & "_   _     ");
      Put_Line (Pink   & "|__|____||_____|_____|__|  \___/ |______|__" & Reset & Green & "|  _  |_| |___ ");
      Put_Line (         "                                           " & Reset & Green & "|     | . | .'|");
      Put_Line (         "                                           " & Reset & Green & "|__|__|___|__,|" & Reset);
   end Neorv32_Ada_Art;

   procedure Show_Choice_Prompt is
   begin
      Put (Pink & "Enter your choice >" & Reset);
   end Show_Choice_Prompt;

   procedure Show_Menu is
   begin
      Put_Line ("========= Available Commands =========");
      Put_Line (" e: Echo your input");
      Put_Line (" i: System Infos.");
      Put_Line (" h: Help on commands.");
      Put_Line (" r: Reload the program.");
      Put_Line (" s: start SPI demo.");
      Put_Line ("======================================");
      Show_Choice_Prompt;
   end Show_Menu;

   procedure Show_Input_Prompt is
   begin
      New_Line;
      Put (Pink & ">" & Reset);
   end Show_Input_Prompt;

   procedure Show_Welcome is
   begin
      New_Line;
      Neorv32_Ada_Art;
      Show_Menu;
   end Show_Welcome;

   procedure Show_Infos is
   begin
      New_Line;
      Put_Line ("Main Clock Frequency: " & Sysinfo.Clk'Image);
      Show_Choice_Prompt;
   end Show_Infos;

   procedure Show_Unknown_Command is
   begin
      New_Line;
      Put_Line ("Unknown command! 'h' for help.");
      Show_Choice_Prompt;
   end Show_Unknown_Command;

   procedure Parse_Cmd (Hart : Harts_T; Trap_Code : Trap_Code_T) is
      pragma Unreferenced (Hart);
      pragma Unreferenced (Trap_Code);
      use neorv32.UART0;
      use neorv32;
      Choice : Character;
      Command : Cmd_T := Unknown;
      SPI_Enabled : Bit;
   begin
      loop
         if UART0_Periph.CTRL.UART_CTRL_RX_NEMPTY = 1 then
            Choice := Uart0.Read_RX;
            if Cmd = Echo then
               Put_Line (Choice'Image);
               Cmd := Unknown;
               Show_Choice_Prompt;
            else
               Put (Choice);
               for C in Cmd_T'Range loop
                  declare
                     C_Str : constant String := C'Image;
                  begin
                     if Choice = To_Lower (C_Str (1)) then
                        Command := C;
                        exit;
                     end if;
                  end;
               end loop;
               case Command is
                  when Echo =>
                     Show_Input_Prompt;
                     Cmd := Echo;
                  when Infos =>
                     Show_Infos;
                  when Reload =>
                     New_Line;
                     Show_Welcome;
                  when Help =>
                     New_Line;
                     Show_Menu;
                  when SPI_Demo =>
                     New_Line;
                     Put_Line ("Starting SPI demo...");
                     SPI_Enabled := SPI.Check_Enabled;
                     if SPI_Enabled = 1 then
                        Put_Line ("SPI peripheral is enabled.");
                        
                        Init_MPU6050;
                        Put_Line (Read_MPU6050 (16#75#)'Image);
                     else
                        Put_Line ("SPI peripheral not enabled!");
                     end if;
                  when others =>
                     Show_Unknown_Command;
               end case;
            end if;
            exit;
         end if;
      end loop;
   end Parse_Cmd;

   procedure Exit_Handler is
   begin
      loop
         null;  -- Stay in an infinite loop
      end loop;
   end Exit_Handler;

end Bios_Core;