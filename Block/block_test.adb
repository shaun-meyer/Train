-- locomotivation
-- Mike Miller
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
with Cabs;
with Layout;
with Trains;
with Block;
with System;
with Port_IO; use type Port_IO.Address_Range;
with Common_Units; use Common_Units;
use Layout;

procedure Block_Test is
   package Block_Polarity_IO is new Ada.Text_IO.Enumeration_IO
     					(Enum => Layout.Block_Polarity);
   package Block_IO is new Ada.Text_IO.Integer_IO (Layout.Block_ID);
   package Cabs_IO is new Ada.Text_IO.Integer_IO (Cabs.Cab_ID);
   package Train_IO is new Ada.Text_IO.Integer_IO (Trains.Train_ID);
   package Request_IO is new Ada.Text_IO.Integer_IO (Num => Trains.Request_ID);

   type Power_Record is
      record
         Polarity  : Layout.Block_Polarity;
         Cab       : Cabs.Cab_ID;
      end record;

   for Power_Record use
      record
         Cab         at 0 range 0 .. 2;
         Polarity    at 0 range 3 .. 3;
      end record;
   for Power_Record'Bit_Order use System.Low_Order_First;

   type Power_Array_Type is array (1 .. 2) of Power_Record;
   for  Power_Array_Type'Size use 8;
   for  Power_Array_Type'Component_Size use 4;

   function To_Power_Array is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte,
      Target => Power_Array_Type);

   type Board_Address_Array is array (0 .. 3) of Port_IO.Address_Range;
   Board_Address : constant Board_Address_Array := (16#200#, 16#208#, 16#210#,
                                                    16#218#);

   procedure Test_Connect_Cab is
      Block_ID : Layout.Block_ID;
      Cab_ID   : Cabs.Cab_ID;
      Polarity : Layout.Block_Polarity;
      Modified_Blocks : Power_Array_Type;
      Offset          : Port_IO.Address_Range;
      Percent         : Common_Units.Percent;

   begin
      Ada.Text_IO.Put ("Enter Cab_ID :");
      Cabs_IO.Get (Cab_ID);
      Ada.Text_IO.Put ("Enter Polarity (Normal or Reversed) :");
      Block_Polarity_IO.Get (Polarity);
      Ada.Text_IO.Put ("Enter Block_ID :");
      Block_IO.Get (Block_ID);
      Block.Connect_Cab (Cab      => Cab_ID,
                         Polarity => Polarity,
                         Block    => Block_ID);

      Offset := Port_IO.Address_Range
        (((Integer (Block_ID) - 1) rem 12) / 2);
      if Integer (Offset) > 2 then
         Offset := Offset + 1;
      end if;

      Modified_Blocks := To_Power_Array
        (Port_IO.In_Byte
        (Address =>  Board_Address ((Integer (Block_ID) - 1) / 12) + Offset));
      if Integer (Block_ID) mod 2 = 1 then
         Block_Polarity_IO.Put (Modified_Blocks (1).Polarity);
         Cabs_IO.Put (Modified_Blocks (1).Cab);
         Ada.Text_IO.New_Line;
      else
         Block_Polarity_IO.Put (Modified_Blocks (2).Polarity);
         Cabs_IO.Put (Modified_Blocks (2).Cab);
         Ada.Text_IO.New_Line;
      end if;
      Cabs.Set_Limit (Cab   => Cab_ID,
                      Value => 100);
      Cabs.Set (Cab   => Cab_ID,
                Value => 50);
      Cabs.Get (Cab   => Cab_ID,
                Value => Percent);
      Ada.Text_IO.Put_Line (Common_Units.Percent'Image (Percent));
      delay 3.0;
      Cabs.Set (Cab   => Cab_ID,
                Value => 100);
      Cabs.Get (Cab   => Cab_ID,
                Value => Percent);
      Ada.Text_IO.Put_Line (Common_Units.Percent'Image (Percent));

      delay 3.0;
   end Test_Connect_Cab;

   procedure Test_Reserve is
      Success  : Boolean;
      Train_ID : Trains.Train_ID;
      Block_ID : Layout.Block_ID;

   begin
      Ada.Text_IO.Put ("Enter Train_ID :");
      Train_IO.Get (Train_ID);
      Ada.Text_IO.Put ("Enter Block_ID :");
      Block_IO.Get (Block_ID);
      Block.Reserve (Train_ID => Train_ID,
                     Block    => Block_ID,
                     Success  => Success);
      Ada.Text_IO.Put ("The Reserve Returned: ");
      Ada.Text_IO.Put_Line (Boolean'Image (Success));
   end Test_Reserve;

   procedure Test_UnReserve is
      Block_ID : Layout.Block_ID;
      Request_ID : Trains.Request_ID;
   begin
      Ada.Text_IO.Put ("Enter Block_ID :");
      Block_IO.Get (Block_ID);
      Ada.Text_IO.Put ("Enter Request_ID :");
      Request_IO.Get (Request_ID);
      Block.UnReserve (Block     => Block_ID,
                       Requestor => Request_ID);
   end Test_UnReserve;

   procedure Test_Get_Polarity is
      Block_Number : Integer;
      Polarity     : Layout.Block_Polarity;
      Cab          : Cabs.Cab_ID;
   begin
      Ada.Text_IO.Put
        ("Enter the block you would like to get the polarity of :");
      Ada.Integer_Text_IO.Get (Block_Number);
      Ada.Text_IO.New_Line;
      Block.Get_Polarity (Block    => Layout.Block_ID (Block_Number),
                          Polarity => Polarity,
                          Cab      => Cab);
      Ada.Text_IO.Put ("The Cab is : ");
      Cabs_IO.Put (Cab);
      Ada.Text_IO.New_Line;
      if Polarity = Layout.Normal then
         Ada.Text_IO.Put_Line ("NORMAL");
      elsif Polarity = Layout.Reversed then
         Ada.Text_IO.Put_Line ("REVERSED");
      else
         Ada.Text_IO.Put_Line ("ERROR");
      end if;
   end Test_Get_Polarity;

   Check : Positive;
begin
   loop
      Ada.Text_IO.Put ("test Connect_Cab (1) test Reserve (2) UnReserve(3):");
      Ada.Integer_Text_IO.Get (Check);
      if Check = 1 then
         Test_Connect_Cab;
         Test_Get_Polarity;
      elsif Check = 2 then
         Test_Reserve;
      elsif Check = 3 then
         Test_UnReserve;
      end if;
   end loop;
end Block_Test;
