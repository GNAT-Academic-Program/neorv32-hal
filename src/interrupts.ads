package Interrupts is

   procedure Init;

   subtype Harts_T is Natural range 0 .. 1;
   type Trap_Code_T is (
      Instruction_Misaligned,
      Instruction_Access,
      Instruction_Illegal,
      Breakpoint,
      Load_Misaligned,
      Load_Access,
      Store_Misaligned,
      Store_Access,
      User_Environment_Call,
      Machine_Environment_Call,
      Machine_Software_Interrupt,
      Machine_Timer_Interrupt,
      Machine_External_Interrupt,
      Fast_Interrupt_0,
      Fast_Interrupt_1,
      Fast_Interrupt_2,
      Fast_Interrupt_3,
      Fast_Interrupt_4,
      Fast_Interrupt_5,
      Fast_Interrupt_6,
      Fast_Interrupt_7,
      Fast_Interrupt_8,
      Fast_Interrupt_9,
      Fast_Interrupt_10,
      Fast_Interrupt_11,
      Fast_Interrupt_12,
      Fast_Interrupt_13,
      Fast_Interrupt_14,
      Fast_Interrupt_15,
      Default_Trap
   );

      for Trap_Code_T use (
      Instruction_Misaligned     => 16#00000000#,
      Instruction_Access         => 16#00000001#,
      Instruction_Illegal        => 16#00000002#,
      Breakpoint                 => 16#00000003#,
      Load_Misaligned            => 16#00000004#,
      Load_Access                => 16#00000005#,
      Store_Misaligned           => 16#00000006#,
      Store_Access               => 16#00000007#,
      User_Environment_Call      => 16#00000008#,
      Machine_Environment_Call   => 16#0000000B#,
      Machine_Software_Interrupt => 16#80000003#,
      Machine_Timer_Interrupt    => 16#80000007#,
      Machine_External_Interrupt => 16#8000000B#,
      Fast_Interrupt_0           => 16#80000010#,
      Fast_Interrupt_1           => 16#80000011#,
      Fast_Interrupt_2           => 16#80000012#,
      Fast_Interrupt_3           => 16#80000013#,
      Fast_Interrupt_4           => 16#80000014#,
      Fast_Interrupt_5           => 16#80000015#,
      Fast_Interrupt_6           => 16#80000016#,
      Fast_Interrupt_7           => 16#80000017#,
      Fast_Interrupt_8           => 16#80000018#,
      Fast_Interrupt_9           => 16#80000019#,
      Fast_Interrupt_10          => 16#8000001A#,
      Fast_Interrupt_11          => 16#8000001B#,
      Fast_Interrupt_12          => 16#8000001C#,
      Fast_Interrupt_13          => 16#8000001D#,
      Fast_Interrupt_14          => 16#8000001E#,
      Fast_Interrupt_15          => 16#8000001F#,
      Default_Trap               => 16#FFFFFFFF#
   );

   type Interrupt_Handler is access
      procedure (Hart : Harts_T; Trap_Code : Trap_Code_T);
   procedure Install_Uart0_Rx_Interrupt_Handler
      (Hart : Harts_T; Handler : Interrupt_Handler);

   procedure Global_Machine_Interrupt_Enable;

end Interrupts;