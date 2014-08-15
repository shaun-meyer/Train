-- Locomotivation
-- Shaun Meyer
with System;
with Ada.Unchecked_Conversion;
with DoubleTalk;
with Trains;
use type Trains.Train_ID;

package body Block is
   Initialization_To_Output : constant Port_IO.Byte := 16#80#;

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

   type Reserved_Record is
      record
         Reserved_Count : Natural := 0;
         Train_ID       : Trains.Train_ID;
      end record;

   type Power_Array_Type is array (1 .. 2) of Power_Record;
   for  Power_Array_Type'Size use 8;
   for  Power_Array_Type'Component_Size use 4;
   --This is for reading a byte from the hardware, which contains 2 records

   type Reserved_Array_Type is array (Layout.Block_ID) of Reserved_Record;
   --Keeps track of blocks reservations and trains ID associated with
   Reserved_Array : Reserved_Array_Type;

   type Board_Address_Array is array (0 .. 3) of Port_IO.Address_Range;
   Board_Address : constant Board_Address_Array := (16#200#, 16#208#, 16#210#,
                                                    16#218#);
   function To_Byte is new Ada.Unchecked_Conversion
     (Source => Power_Array_Type,
      Target => Port_IO.Byte);

   function To_Power_Array is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte,
      Target => Power_Array_Type);

-------------------------------------------------------------------------------
   protected Block_Procedures is
      procedure Reserve (Train_ID : in Trains.Train_ID;
                         Block    : in Layout.Block_ID;
                         Success  : out Boolean);
      procedure Read_Write (Address  : in Port_IO.Address_Range;
                            Cab   : in Cabs.Cab_ID;
                            Block    : in Layout.Block_ID;
                            Polarity : in Layout.Block_Polarity);

   end Block_Procedures;
-------------------------------------------------------------------------------
   protected body Block_Procedures is
      procedure Reserve (Train_ID : in Trains.Train_ID;
                         Block    : in Layout.Block_ID;
                         Success  : out Boolean) is
      begin
         if Reserved_Array (Block).Reserved_Count /= 0 then
            if Reserved_Array (Block).Train_ID = Train_ID then
               Reserved_Array (Block).Reserved_Count :=
                 Reserved_Array (Block).Reserved_Count + 1;
               Success := True;
            else
               Success := False;
            end if;
         else
            Reserved_Array (Block).Reserved_Count :=
              Reserved_Array (Block).Reserved_Count + 1;
            Reserved_Array (Block).Train_ID := Train_ID;
            Success := True;
         end if;
      end Reserve;
-------------------------------------------------------------------------------
      procedure Read_Write (Address  : in Port_IO.Address_Range;
                            Cab      : in Cabs.Cab_ID;
                            Block    : in Layout.Block_ID;
                            Polarity : in Layout.Block_Polarity) is
         Modified_Blocks : Power_Array_Type;

      begin
         Modified_Blocks := To_Power_Array (Port_IO.In_Byte (Address));

         if Integer (Block) rem 2 = 1 then
            Modified_Blocks (2).Cab := Cab;
            Modified_Blocks (2).Polarity := Polarity;
         else
            Modified_Blocks (1).Cab := Cab;
            Modified_Blocks (1).Polarity := Polarity;
         end if;
         Port_IO.Out_Byte (Address => Address,
                           Data    => To_Byte (Modified_Blocks));
      end Read_Write;

   end Block_Procedures;

-------------------------------------------------------------------------------
   procedure Reserve (Train_ID : in Trains.Train_ID;
                      Block    : in Layout.Block_ID;
                      Success  : out Boolean) is
   begin
      Block_Procedures.Reserve (Train_ID => Train_ID,
                                Block    => Block,
                                Success  => Success);
   end Reserve;
-------------------------------------------------------------------------------
   procedure Connect_Cab (Cab      : in Cabs.Cab_ID;
                          Polarity : in Layout.Block_Polarity;
                          Block    : in Layout.Block_ID) is
      Offset : Port_IO.Address_Range;
   begin
      Offset := Port_IO.Address_Range
        (((Integer (Block) - 1) rem 12)  / 2);
      if Integer (Offset) > 2 then
         Offset := Offset + 1;
      end if;
      Block_Procedures.Read_Write
        (Address  => Board_Address ((Integer (Block) - 1) / 12) + Offset,
         Cab      => Cab,
         Block    => Block,
         Polarity => Polarity);
   end Connect_Cab;
-------------------------------------------------------------------------------
   procedure UnReserve (Block      : in Layout.Block_ID;
                        Requestor  : in Trains.Request_ID) is
      Offset : Port_IO.Address_Range;
   begin
      if Reserved_Array (Block).Reserved_Count > 0 then
         if Requestor = 0 then
            Reserved_Array (Block).Reserved_Count := 0;
         else
            Reserved_Array (Block).Reserved_Count
              := Reserved_Array (Block).Reserved_Count - 1;
         end if;
      else
         DoubleTalk.Speak
           (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String ("Block" &
              " is already unreserved."),
            Voice  => DoubleTalk.Vader);
      end if;

      if Reserved_Array (Block).Reserved_Count = 0 then
         Offset := Port_IO.Address_Range
           (((Integer (Block) - 1) rem 12)  / 2);
         if Integer (Offset) > 2 then
            Offset := Offset + 1;
         end if;
         Block_Procedures.Read_Write
           (Address  => Board_Address ((Integer (Block) - 1) / 12) + Offset,
            Cab      => 0,
            Block    => Block,
            Polarity => Layout.Normal);
      end if;
   end UnReserve;
-------------------------------------------------------------------------------
   procedure Get_Polarity (Block    : in Layout.Block_ID;
                           Polarity : out Layout.Block_Polarity;
                           Cab      : out Cabs.Cab_ID) is
      Offset : Port_IO.Address_Range;
      Blocks : Power_Array_Type;

   begin
      Offset := Port_IO.Address_Range
        (((Integer (Block) - 1) rem 12)  / 2);
      if Integer (Offset) > 2 then
         Offset := Offset + 1;
      end if;

      Blocks := To_Power_Array (Port_IO.In_Byte (
        Board_Address ((Integer (Block) - 1) / 12) + Offset));

      if Integer (Block) rem 2 = 1 then
         Polarity := Blocks (2).Polarity;
         Cab := Blocks (2).Cab;
      else
         Polarity := Blocks (1).Polarity;
         Cab := Blocks (1).Cab;
      end if;
   end Get_Polarity;

   procedure Get_Reserve_Count (Block : in Layout.Block_ID;
                                ID    : in Trains.Train_ID;
                                Count : out Natural) is
   begin
      if Reserved_Array (Block).Train_ID = ID then
         Count := Reserved_Array (Block).Reserved_Count;
      else
         Count := 0;
      end if;
   end Get_Reserve_Count;

   procedure UnReserve_All is
   begin
      for ID in Layout.Block_ID'Range loop
         Reserved_Array (ID).Reserved_Count := 0;
         Connect_Cab (Cab      => 0,
                      Polarity => Layout.Normal,
                      Block    => ID);
      end loop;
   end UnReserve_All;

begin
   for Index in Board_Address'Range loop
      Port_IO.Out_Byte (Address => Board_Address (Index) + 3,
                     Data    => Initialization_To_Output);
      Port_IO.Out_Byte (Address => Board_Address (Index) + 7,
                     Data    => Initialization_To_Output);
      Port_IO.Out_Byte (Address => Board_Address (Index),
                        Data    => 0);
      Port_IO.Out_Byte (Address => Board_Address (Index) + 1,
                        Data    => 0);
      Port_IO.Out_Byte (Address => Board_Address (Index) + 2,
                        Data    => 0);
      Port_IO.Out_Byte (Address => Board_Address (Index) + 4,
                        Data    => 0);
      Port_IO.Out_Byte (Address => Board_Address (Index) + 5,
                        Data    => 0);
      Port_IO.Out_Byte (Address => Board_Address (Index) + 6,
                        Data    => 0);
   end loop;

end Block;
