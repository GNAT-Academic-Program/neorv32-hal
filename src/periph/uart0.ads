package Uart0 is
   procedure Echo_Uart_RX;
   procedure Init (Baud_Rate : Natural);
   function Read_RX return Character;
end Uart0;