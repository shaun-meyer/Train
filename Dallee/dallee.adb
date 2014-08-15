-- Locomotivation
-- Shaun Meyer

with Port_IO; use type Port_IO.Address_Range;
with Ada.Unchecked_Conversion;
package body Dallee is

   BASE : constant Port_IO.Address_Range := 16#260#;
   Digital_IO_Address : constant Port_IO.Address_Range := BASE + 3;

   type Sound_Array is array (Dallee_Sound_Units, Sound_Type) of State;
   for  Sound_Array'Size use 8;
   for  Sound_Array'Component_Size use 1;

   Dallee_Sound_Array : Sound_Array;

   function To_Byte is new Ada.Unchecked_Conversion
     (Source => Sound_Array,
      Target => Port_IO.Byte);

   function To_Array is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte,
      Target => Sound_Array);

--------------------------------------------------------------------
   procedure Set_Sound (Changed_State : in State;
                        Sound_Unit    : in Dallee_Sound_Units;
                        Sound         : in Sound_Type) is
   begin
      Dallee_Sound_Array (Sound_Unit, Sound) := Changed_State;
      Port_IO.Out_Byte (Address => Digital_IO_Address,
                        Data    => To_Byte (Dallee_Sound_Array));
   end Set_Sound;
--------------------------------------------------------------------

begin
   Dallee_Sound_Array := To_Array (2#00000000#);
   Port_IO.Out_Byte (Address => Digital_IO_Address,
                     Data    => To_Byte (Dallee_Sound_Array));
end Dallee;
