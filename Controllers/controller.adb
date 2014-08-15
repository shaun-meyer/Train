-- Locomotivation
-- Shaun Meyer

with Ada.Unchecked_Conversion;
with Port_IO; use type Port_IO.Address_Range;
with Common_Units; use type Common_Units.Volts;
with System;
with ADC;
package body Controller is

   Base_Address : constant Port_IO.Address_Range := 16#240#;
   Controller_Port_A : constant Port_IO.Address_Range := Base_Address + 12;
   Controller_Port_B : constant Port_IO.Address_Range := Base_Address + 13;
   Controller_Port_C : constant Port_IO.Address_Range := Base_Address + 14;

   type Hand_Controller is
      record
         Red_Button       : Button_Type;
         Black_Button     : Button_Type;
         Direction_Switch : FB_Toggle_Type;
         Turn_Switch      : LRC_Toggle_Type;
      end record;

   for Hand_Controller use
      record
         Red_Button       at 0 range 0 .. 0;
         Black_Button     at 0 range 1 .. 1;
         Direction_Switch at 0 range 2 .. 2;
         Turn_Switch      at 0 range 3 .. 4;
      end record;
   --Specify the bit position per byte for each message from the hand controller

   for Hand_Controller'Size use 8;
   for Hand_Controller'Bit_Order use System.Low_Order_First;

   function To_Register is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte,
      Target => Hand_Controller);

   procedure Get (Controller       : in Controller_ID;
                  Red_Button       : out Button_Type;
                  Black_Button     : out Button_Type;
                  Direction_Switch : out FB_Toggle_Type;
                  Turn_Switch      : out LRC_Toggle_Type) is

      Input_Byte : Port_IO.Byte;
      Output_Controller : Hand_Controller;

   begin
      case Controller is
         when A =>
            Input_Byte := Port_IO.In_Byte (Address => Controller_Port_A);
         when B =>
            Input_Byte := Port_IO.In_Byte (Address => Controller_Port_B);
         when C =>
            Input_Byte := Port_IO.In_Byte (Address => Controller_Port_C);
      end case;

      Output_Controller := To_Register (Input_Byte);
      Red_Button := Output_Controller.Red_Button;
      Black_Button := Output_Controller.Black_Button;
      Direction_Switch := Output_Controller.Direction_Switch;
      Turn_Switch := Output_Controller.Turn_Switch;

   end Get;

   procedure Get (Controller : in Controller_ID;
                    Knob     : out Common_Units.Percent) is
      Out_Volts : ADC.Output_Volts;
   begin
      case Controller is
         when A =>
            ADC.Read (Channel => ADC.Channel_Number (0),
                      Value   => Out_Volts);
         when B =>
            ADC.Read (Channel => ADC.Channel_Number (1),
                      Value   => Out_Volts);
         when C =>
            ADC.Read (Channel => ADC.Channel_Number (2),
                      Value   => Out_Volts);
      end case;

      -- Take absolute value of voltage to account for improper fluctuation
      -- below 0
      Knob := Common_Units.Percent (Float (abs (Out_Volts)) / 5.0 * 100.0);
   end Get;


end Controller;


