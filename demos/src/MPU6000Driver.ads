with neorv32; use neorv32;

package MPU6000Driver is
   procedure Init_MPU6000;
   function Read_MPU6000 (Reg_Address : Byte) return Byte;
end MPU6000Driver;