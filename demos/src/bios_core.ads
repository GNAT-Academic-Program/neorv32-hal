with Interrupts; use Interrupts;

package Bios_Core is
   procedure Show_Welcome;
   procedure Parse_Cmd (Hart : Harts_T; Trap_Code : Trap_Code_T);
   procedure Exit_Handler with
     Export, Convention => C, External_Name => "__gnat_exit";
end Bios_Core;