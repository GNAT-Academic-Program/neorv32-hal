with neorv32; use neorv32;

package MPU6050Driver is
   procedure Init_MPU6050;
   function Read_MPU6050 (Reg_Address : Byte) return Byte;
end MPU6050Driver;