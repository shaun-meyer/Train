--Locomotivation
--James Vannordstrand
with Common_Units; use Common_Units;
with Ada.Unchecked_Conversion;
with Port_IO;
use type Port_IO.Address_Range;
with System;
with Interfaces;

package body ADC is

   BASE : constant Port_IO.Address_Range := 16#260#;

   type CSR_Rec is  -- Type for the control status register
      record
         Channel   : Channel_Number;
         EOC       : Boolean;
      end record;

   for CSR_Rec use
      record
         Channel    at 0 range 0 .. 2;
         EOC        at 0 range 7 .. 7;
      end record;

   for CSR_Rec'Size use 8;
   for CSR_Rec'Bit_Order use System.Low_Order_First;

   Control_Status_Register : CSR_Rec;
   pragma Volatile (Control_Status_Register);


   function To_Register is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte,
      Target => CSR_Rec);

   function To_Byte is new Ada.Unchecked_Conversion
     (Source => CSR_Rec,
      Target => Port_IO.Byte);

   ---------------------------------------------
   protected type Semaphore (Initial_Value : Natural) is
      procedure Signal;
      entry Wait;
   private
      Count : Natural := Initial_Value;
   end Semaphore;

   protected body Semaphore is
      procedure Signal is
      begin
         Count := Count + 1;
      end Signal;

      entry Wait when Count > 0 is
      begin
         Count := Count - 1;
      end Wait;
   end Semaphore;
   ---------------------------------------------------------
   My_Semaphore : Semaphore (Initial_Value => 1);

   procedure Read (Channel : in     Channel_Number;
                   Value   :    out Output_Volts) is
      Out_Byte     : Port_IO.Byte;
      In_Word      : Port_IO.Word;
      Bit_Value    : Interfaces.Unsigned_16;
   begin
      My_Semaphore.Wait;
      Control_Status_Register := (Channel => Channel,
                                  EOC     => True);
      Out_Byte := To_Byte (Control_Status_Register);
      for secondVal in 1 .. 2 loop
         Port_IO.Out_Byte (Address => BASE + 2,
                        Data    => Out_Byte);
         Port_IO.Out_Byte (Address => BASE + 1,
                        Data    => 0);

      -- Wait until the conversion is complete
         loop
            delay 0.000025;
            Out_Byte := Port_IO.In_Byte (Address => BASE + 2);
            Control_Status_Register := To_Register (Out_Byte);
            exit when not Control_Status_Register.EOC;
         end loop;

         In_Word := Port_IO.In_Word (Address => BASE);
         Bit_Value := Interfaces.Shift_Right (Value => Interfaces.Unsigned_16
                                              (In_Word),
                                              Amount => 4);
      end loop;
      -- Convert whole number data to a fixed point voltage
      Value := Volts (Bit_Value) / 409.5 - 5.0;
      My_Semaphore.Signal;
   end Read;


end ADC;
