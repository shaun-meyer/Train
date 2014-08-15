--Locomotivation
--Mike Miller

with Layout;
use Layout;
with Port_IO;
use type Port_IO.Address_Range;
with Ada.Unchecked_Conversion;

package body Motors is

   High_Output_Byte       : constant Port_IO.Byte := 2#10000010#;
   Low_Output_Byte        : constant Port_IO.Byte := 2#10011001#;
   Initialization_To_Left : constant Port_IO.Byte := 2#00000000#;

   type Board_Address_Array is array (0 .. 1) of Port_IO.Address_Range;
   Board_Address : constant Board_Address_Array := (16#220#, 16#228#);

----------------------------------------------------------------------------


   type Data_Array is array (0 .. 7) of State;
   for  Data_Array'Size use 8;
   for  Data_Array'Component_Size use 1;

   type Input_Array is array (0 .. 7) of Boolean;
   for  Input_Array'Size use 8;
   for  Input_Array'Component_Size use 1;

   function To_Byte is new Ada.Unchecked_Conversion
     (Source => Data_Array,
      Target => Port_IO.Byte);

   function To_Rec is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte,
      Target => Data_Array);

   function To_Rec is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte,
      Target => Input_Array);

   -----------------------------------------------------------------------------
   protected Write_Function is
      procedure Write (Address : in Port_IO.Address_Range;
                       Data    : in Port_IO.Byte);
      procedure Set (Motor     : in Layout.Turnout_ID;
                     Direction : in State);
   end Write_Function;

   protected body Write_Function is
      procedure Write (Address : in Port_IO.Address_Range;
                       Data    : in Port_IO.Byte) is
      begin
         Port_IO.Out_Byte (Address => Address,
                           Data    => Data);
      end Write;

      procedure Set (Motor     : in Layout.Turnout_ID;

                     Direction : in State) is
         Board_Index      : constant Natural := Natural (Motor) / 25;

         Turnout_Location : Layout.Turnout_ID;
         Turnout_Index    : Integer;
         Turnout_Position : Data_Array;
         Offset           : Port_IO.Address_Range;

      begin
         if Motor <= 24 then
            Turnout_Location := Motor;
         else
            Turnout_Location := Motor - 24;
         end if;
         Turnout_Index := (Integer (Turnout_Location) - 1) rem 8;

         Offset := Port_IO.Address_Range
           ((((Integer (Turnout_Location)  - 1) / 8) + 1)* 2 - 1);
         --Calculation: Get a number from 1 - 6, 1 - 3 is high, 4 - 6 is low.

         --This changes values so the port offset is correct
         if Offset < 4 then
            Offset := Offset + 3;
         else
            Offset := Offset - 4;
         end if;

         Turnout_Position := To_Rec (Port_IO.In_Byte
                             (Address => Board_Address (Board_Index) + Offset));

         Turnout_Position (Turnout_Index) := Direction;

         Write_Function.Write (Address => Board_Address (Board_Index) + Offset,
                               Data    => To_Byte (Turnout_Position));
      end Set;
   end Write_Function;
------------------------------------------------------------------------------

   procedure Set (Motor     : in Layout.Turnout_ID;
                  Direction : in State) is
   begin
      Write_Function.Set (Motor     => Motor,
                          Direction => Direction);
   end Set;
---------------------------------------------------------------------------

   function In_Position (Motor : in Layout.Turnout_ID) return Boolean is

      Board_Index      : constant Natural := Natural (Motor) / 25;

      Turnout_Location : Layout.Turnout_ID;
      Turnout_Index    : Integer;
      Offset           : Port_IO.Address_Range;
      Turnout_Position : Input_Array;


   begin
      if Motor <= 24 then
         Turnout_Location := Motor;
      else
         Turnout_Location := Motor - 24; -- set turnout location for 2nd board
      end if;
      Turnout_Index := (Integer (Turnout_Location) - 1) rem 8;

      Offset := Port_IO.Address_Range
        ((((Integer (Turnout_Location)  - 1) / 8) + 1)* 2);
      --Calculation: Get a number from 1 - 6, 1 - 3 is high, 4 - 6 is low.

      --This changes values so the port offset is correct
      if Offset < 4 then
         Offset := Offset + 3;
      else
         Offset := Offset - 4;
      end if;
      Turnout_Position := To_Rec (Port_IO.In_Byte
         (Address => Board_Address (Board_Index) + Offset));

      return Turnout_Position (Turnout_Index);

   end In_Position;
----------------------------------------------------------------------------

begin
   -- set Board A & C High and B Low to Output Bit and
   Port_IO.Out_Byte (Address => Board_Address (0) + 3,
                     Data    => Low_Output_Byte);
   Port_IO.Out_Byte (Address => Board_Address (1) + 3,
                     Data    => Low_Output_Byte);
   Port_IO.Out_Byte (Address => Board_Address (0) + 7,
                     Data    => High_Output_Byte);
   Port_IO.Out_Byte (Address => Board_Address (1) + 7,
                     Data    => High_Output_Byte);

   -- initialized all turnouts to left for Board_0
   Port_IO.Out_Byte (Address => Board_Address (0) + 1,
                     Data    => Initialization_To_Left);
   Port_IO.Out_Byte (Address => Board_Address (0) + 4,
                     Data    => Initialization_To_Left);
   Port_IO.Out_Byte (Address => Board_Address (0) + 6,
                     Data    => Initialization_To_Left);

   -- initialized all turnouts to left for Board_1
   Port_IO.Out_Byte (Address => Board_Address (1) + 1,
                     Data    => Initialization_To_Left);
   Port_IO.Out_Byte (Address => Board_Address (1) + 4,
                     Data    => Initialization_To_Left);
   Port_IO.Out_Byte (Address => Board_Address (1) + 6,
                     Data    => Initialization_To_Left);
end Motors;
