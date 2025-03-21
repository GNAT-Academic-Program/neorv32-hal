with Bios_Core;
with Interrupts;
with Uart0;
with Random;

procedure Bios is
begin
   Interrupts.Init;
   Interrupts.Install_Uart0_Rx_Interrupt_Handler
      (0, Bios_Core.Parse_Cmd'Access);
   --Uart0.Init (2000000);
   Uart0.Init (19200);
   Random.Init;
   Interrupts.Global_Machine_Interrupt_Enable;
   Bios_Core.Show_Welcome;
   loop
      null;
   end loop;
end Bios;
