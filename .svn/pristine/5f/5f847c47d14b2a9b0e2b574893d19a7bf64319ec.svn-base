-- locomotivation
-- Mike Miller
with Ada.Text_IO;
with Layout; use Layout;
with Ada.Integer_Text_IO;

procedure Test_Layout is
   package Block_Polarity_IO is new Ada.Text_IO.Enumeration_IO
     					(Enum => Layout.Block_Polarity);
   package Terminator_Type_IO is new Ada.Text_IO.Enumeration_IO
     					(Enum => Layout.Terminator_Type);
   package Turnout_Direction_IO is new Ada.Text_IO.Enumeration_IO
     					(Enum => Layout.Turnout_Direction);
   package Boolean_IO is new Ada.Text_IO.Enumeration_IO (Enum => Boolean);
   package Block_IO is new Ada.Text_IO.Integer_IO (Layout.Block_ID);
   package Turnout_IO is new Ada.Text_IO.Integer_IO (Layout.Turnout_ID);
   package Hall_ID_IO is new Ada.Text_IO.Integer_IO (Layout.Hall_ID);

   ----------------------------------------------------------------------------

   procedure Next_Component_Test (File_Name   : in String) is
      Actual_Terminator   : Layout.Terminator_Type;
      Block_Number        : Layout.Block_ID;
      Direction           : Layout.Block_Polarity;
      Terminator          : Layout.Terminator_Type;
      File                : Ada.Text_IO.File_Type;

   begin
      -- next Track Component
      -- test for what track component follows a block
      --there are 80 tests for this
      Ada.Text_IO.Put_Line ("Retrieve_Terminator Test");
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => File_Name);
      -- read data in from retrieveTrackComponent.txt
      for Tests in 1 .. 80 loop
         Block_IO.Get (Item => Block_Number,
                       File => File);
         Block_Polarity_IO.Get (Item => Direction,
                                File => File);
         Terminator_Type_IO.Get (Item => Terminator,
                                 File => File);
         Actual_Terminator := Layout.Retrieve_Terminator
           (Block     => Block_Number,
            Direction => Direction);
         -- output Fail Data
         if Actual_Terminator /= Terminator then
            Ada.Text_IO.Put ("Test Data: Block_Number = ");
            Block_IO.Put (Block_Number);
            Ada.Text_IO.Put (" Polarity = ");
            Block_Polarity_IO.Put (Direction);
            Ada.Text_IO.Put (" Terminator_Type = ");
            Terminator_Type_IO.Put (Terminator);
            Ada.Text_IO.Put_Line (" test failed");
         end if;
      end loop;
      Ada.Text_IO.Close (File => File);
      Ada.Text_IO.Put_Line ("end of Retrieve_Terminator Test");
   end Next_Component_Test;

-------------------------------------------------------------------------------
   procedure Retrieve_Turnout_ID_Test (File_Name   : in String) is
      Block_Number       : Layout.Block_ID;
      Direction          : Layout.Block_Polarity;
      Turnout            : Layout.Turnout_ID;
      Actual_Turnout     : Layout.Turnout_ID;
      File               : Ada.Text_IO.File_Type;

   begin
      -- retrieve Turnout_ID
      -- test Turnout_ID is returned
      --there are 23 tests for this
      Ada.Text_IO.Put_Line ("Retrieve_Turnout_ID Test");
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => File_Name);
      -- read data in from retrieveTurnoutNumber.csv
      for Tests in 1 .. 23 loop
         Block_IO.Get (Item => Block_Number,
                       File => File);
         Block_Polarity_IO.Get (Item => Direction,
                                 File => File);
         Turnout_IO.Get (Item => Turnout,
                         File => File);
         Actual_Turnout := Layout.Retrieve_Turnout_ID (Block => Block_Number,
                                                  Direction => Direction);
         -- output fail data
         if Actual_Turnout /= Turnout then
            Ada.Text_IO.Put ("Test Data: Block_Number = ");
            Block_IO.Put (Block_Number);
            Ada.Text_IO.Put (" Polarity = ");
            Block_Polarity_IO.Put (Direction);
            Ada.Text_IO.Put (" Turnout = ");
            Turnout_IO.Put (Turnout);
            Ada.Text_IO.Put_Line ("test failed");
         end if;
      end loop;
      Ada.Text_IO.Close (File => File);
      Ada.Text_IO.Put_Line ("end of Retrieve_Turnout_ID Test");
   end Retrieve_Turnout_ID_Test;
-------------------------------------------------------------------------------
   procedure Next_Block_ID_Test (File_Name   : in String) is
      Block_Number       : Layout.Block_ID;
      Next_Block_Number  : Layout.Block_ID;
      Direction          : Layout.Block_Polarity;
      Actual_Next_Block  : Layout.Block_ID;
      File               : Ada.Text_IO.File_Type;

   begin
      -- retrieve Next_Block_ID
      -- test which block_id is returned
      --there are 56 tests for this
      Ada.Text_IO.Put_Line ("Retrieve_Next_Block_ID Test");
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => File_Name);
      -- read data in from retrieveBlockNumber.txt
      for Tests in 1 .. 56 loop
         Block_IO.Get (Item => Block_Number,
                       File => File);
         Block_Polarity_IO.Get (Item => Direction,
                                File => File);
         Block_IO.Get (Item => Next_Block_Number,
                       File => File);
         Actual_Next_Block := Layout.Retrieve_Next_Block_ID
        	(Block     => Block_Number,
                 Direction => Direction);
         -- output fail data
         if Actual_Next_Block /= Next_Block_Number then
            Ada.Text_IO.Put ("Test Data: Block_Number = ");
            Block_IO.Put (Block_Number);
            Ada.Text_IO.Put (" Polarity = ");
            Block_Polarity_IO.Put (Direction);
            Ada.Text_IO.Put (" Next_Block_Number = ");
            Block_IO.Put (Next_Block_Number);
            Ada.Text_IO.Put (" ");
            Block_IO.Put (Actual_Next_Block);
            Ada.Text_IO.Put_Line ("test failed");
         end if;
      end loop;
      Ada.Text_IO.Close (File => File);
      Ada.Text_IO.Put_Line ("end of Retrieve_Next_Block_ID Test");
   end Next_Block_ID_Test;
--------------------------------------------------------------------------------
   procedure Turnout_Block_Test (File_Name   : in String) is
      Way                : Layout.Turnout_Direction;
      Turnout            : Layout.Turnout_ID;
      Next_Block_Number  : Layout.Block_ID;
      Actual_Next_Block  : Layout.Block_ID;
      File               : Ada.Text_IO.File_Type;

   begin
      -- retrieve Turnout_Block
      -- test which block_id is returned
      --there are 52 tests for this
      Ada.Text_IO.Put_Line ("Retrieve_Turnout_Block_ID Test");
      Ada.Text_IO.Open (File => File,
                    	Mode => Ada.Text_IO.In_File,
                    	Name => File_Name);
      -- read data from retrieveTurnoutBlocks.txt
      for Tests in 1 .. 52 loop
         Turnout_Direction_IO.Get (Item => Way,
                                   File => File);
         Turnout_IO.Get (Item => Turnout,
                         File => File);
         Block_IO.Get (Item => Next_Block_Number,
                 File => File);
         Actual_Next_Block := Layout.Retrieve_Turnout_Block_ID
           (Direction => Way,
            Turnout   => Turnout);
         -- output fail data
         if Actual_Next_Block /= Next_Block_Number then
            Ada.Text_IO.Put ("Test Data: Way = ");
            Turnout_Direction_IO.Put (Way);
            Ada.Text_IO.Put (" Turnout = ");
            Turnout_IO.Put (Turnout);
            Ada.Text_IO.Put (" Next_Block_Number = ");
            Block_IO.Put (Next_Block_Number);
            Ada.Text_IO.Put_Line (" test failed");
         end if;
      end loop;
      Ada.Text_IO.Close (File => File);
      Ada.Text_IO.Put_Line ("end of Retrieve_Turnout_Block_ID Test");
   end Turnout_Block_Test;
--------------------------------------------------------------------------------
   procedure Reversing_Point_Test (File_Name   : in String) is
      Hall_Sensor_ID      : Layout.Hall_ID;
      Actual_Boolean      : Boolean;
      Test_Boolean        : Boolean;
      File                : Ada.Text_IO.File_Type;

   begin
      -- retrieve reversing point
      -- test if a hallid is a reversing point
      --there are 51 tests for this
      Ada.Text_IO.Put_Line ("Is_Reversing_Point Test");
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => File_Name);
      -- read data from retrieveReversingPoint.txt
      for Tests in 1 .. 51 loop
         Hall_ID_IO.Get (Item => Hall_Sensor_ID,
                   File => File);
         Boolean_IO.Get (Item => Test_Boolean,
                   File => File);
         Actual_Boolean := Layout.Is_Reversing_Point (Sensor => Hall_Sensor_ID);
         -- output fail data
         if Actual_Boolean /= Test_Boolean then
            Ada.Text_IO.Put ("Test Data: Hall_Sensor_ID = ");
            Hall_ID_IO.Put (Hall_Sensor_ID);
            Ada.Text_IO.Put (" TestBoolean = ");
            Boolean_IO.Put (Test_Boolean);
            Ada.Text_IO.Put_Line ("test failed");
         end if;
      end loop;
      Ada.Text_IO.Close (File => File);
      Ada.Text_IO.Put_Line ("end of Is_Reversing_Point Test");
   end Reversing_Point_Test;
-------------------------------------------------------------------------------
   procedure Hall_ID_Test (File_Name   : in String) is
      Hall_Sensor_ID     : Layout.Hall_ID;
      Actual_Hall_ID     : Layout.Hall_ID;
      Next_Block_Number  : Layout.Block_ID;
      Block_Number       : Layout.Block_ID;
      File               : Ada.Text_IO.File_Type;

   begin
      -- retrieve Hall ID
      -- tests which hall id is connecting 2 blocks
      --there are 51 tests for this
      Ada.Text_IO.Put_Line ("Retrieve_Hall_ID Test");
      Ada.Text_IO.Open (File => File,
                    Mode => Ada.Text_IO.In_File,
                    Name => File_Name);
      -- read data from retrieveHallId.txt
      for Tests in 1 .. 51 loop
         Block_IO.Get (Item => Block_Number,
                 File => File);
         Block_IO.Get (Item => Next_Block_Number,
                 File => File);
         Hall_ID_IO.Get (Item => Hall_Sensor_ID,
                   File => File);
         Actual_Hall_ID := Layout.Retrieve_Hall_ID
           (Current_Block => Block_Number,
            Next_Block    => Next_Block_Number);
         -- display Fail Data
         if Actual_Hall_ID /= Hall_Sensor_ID then
            Ada.Text_IO.Put ("Test Data: Block_Number = ");
            Block_IO.Put (Block_Number);
            Ada.Text_IO.Put (" Next_Block_Number = ");
            Block_IO.Put (Next_Block_Number);
            Ada.Text_IO.Put (" Hall_Sensor_ID = ");
            Hall_ID_IO.Put (Hall_Sensor_ID);
            Ada.Text_IO.Put_Line ("test failed");
         end if;
      end loop;
      Ada.Text_IO.Close (File => File);
      Ada.Text_IO.Put_Line ("end of Retrieve_Hall_ID Test");
   end Hall_ID_Test;
   ----------------------------------------------------------------------------
   --            _                __                  _   _
   --           | |              / _|                | | (_)
   --   _____  __ |_ _ __ __ _  | |_ _   _ _ __   ___| |_ _  ___  _ __  ___
   --  / _ \ \/ / __| '__/ _` | |  _| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
   -- |  __/>  <| |_| | | (_| | | | | |_| | | | | (__| |_| | (_) | | | \__ \
   -- \___/_/\_\\__|_|  \__,_| |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
   ----------------------------------------------------------------------------
   procedure Opposite_Polarity (Total_Test : Integer) is
      Direction : Layout.Block_Polarity;

   begin
      for Count in 1 .. Total_Test loop
         Ada.Text_IO.Put ("enter Block_Polarity (Normal or Reversed): ");
         Block_Polarity_IO.Get (Direction);
         Ada.Text_IO.Put ("the opposite is : ");
         Block_Polarity_IO.Put (Layout.Opposite (Direction));
         Ada.Text_IO.New_Line;
      end loop;
   end Opposite_Polarity;
   ----------------------------------------------------------------------------
   procedure Opposite_Turnout_Test (Total_Test : Integer) is
      Direction : Layout.Turnout_Direction;

   begin
      for Count in 1 .. Total_Test loop
         Ada.Text_IO.Put ("enter Turnout_Direction (Left or Right): ");
         Turnout_Direction_IO.Get (Direction);
         Ada.Text_IO.Put ("the opposite is : ");
         Turnout_Direction_IO.Put (Layout.Opposite (Direction));
         Ada.Text_IO.New_Line;
      end loop;
   end Opposite_Turnout_Test;
   ----------------------------------------------------------------------------
   procedure Get_Block_IDs_With_Hall_Sensor_Test (Total_Test : Integer) is
      Hall_Sensor_ID     : Layout.Hall_ID;
      Block_Number       : Layout.Block_ID;
      Next_Block_Number  : Layout.Block_ID;
   begin
      for Count in 1 .. Total_Test loop
         Ada.Text_IO.Put ("enter a Hall_Sensor_ID : ");
         Hall_ID_IO.Get (Hall_Sensor_ID);
         Get_Block_IDs_With_Hall_Sensor (ID        => Hall_Sensor_ID,
                                        First_ID  => Block_Number,
                                        Second_ID => Next_Block_Number);
         Ada.Text_IO.Put_Line ("the two conected Blocks");
         Ada.Text_IO.Put ("block 1");
         Block_IO.Put (Block_Number);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("block 2");
         Block_IO.Put (Next_Block_Number);
         Ada.Text_IO.New_Line;
      end loop;
   end Get_Block_IDs_With_Hall_Sensor_Test;
----------------------------------------------------------------------------
   procedure Is_Forced_Turnout_Test (Total_Test : Integer) is
      Block_Number       : Layout.Block_ID;
      Polarity           : Layout.Block_Polarity;
      Force              : Boolean;
   begin
      for Count in 1 .. Total_Test loop
         Ada.Text_IO.Put ("enter a Block_Number : ");
         Block_IO.Get (Block_Number);
         Ada.Text_IO.Put ("enter a Polarity (Normal or Reversed) :");
         Block_Polarity_IO.Get (Polarity);
         Is_Forced_Turnout (ID       => Block_Number,
                            Polarity => Polarity,
                            Forced   => Force);
         if Force then
            Ada.Text_IO.Put_Line ("this has a forced Turnout");
         else
            Ada.Text_IO.Put_Line ("this does not have a forced Turnout");
         end if;
      end loop;
   end Is_Forced_Turnout_Test;
----------------------------------------------------------------------------
   procedure Change_Forced_Turnout_Test (Total_Test : Integer) is
      Block_Number       : Layout.Block_ID;
      Polarity           : Layout.Block_Polarity;
      Turnout            : Layout.Turnout_ID;
      Way                : Layout.Turnout_Direction;
   begin
      for Count in 1 .. Total_Test loop
         Ada.Text_IO.Put ("enter a Block_Number : ");
         Block_IO.Get (Block_Number);
         Ada.Text_IO.Put ("enter a Polarity (Normal or Reversed) :");
         Block_Polarity_IO.Get (Polarity);
         Set_Forced_Turnout (Block     => Block_Number,
                                Polarity  => Polarity,
                                Turnout   => Turnout,
                                Direction => Way);
         Ada.Text_IO.Put ("Turnout ID : ");
         Turnout_IO.Put (Turnout);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("Direction : ");
         Turnout_Direction_IO.Put (Way);
	 Ada.Text_IO.New_Line;
      end loop;
   end Change_Forced_Turnout_Test;
----------------------------------------------------------------------------
   procedure Is_Joint_Turnout_Test (Total_Test : Integer) is
      Way                : Layout.Turnout_Direction;
      Turnout            : Layout.Turnout_ID;
   begin
      for Count in 1 .. Total_Test loop
         Ada.Text_IO.Put ("Enter Turnout ID : ");
         Turnout_IO.Get (Turnout);
         Ada.Text_IO.Put ("Enter Direction (Left or Right): ");
         Turnout_Direction_IO.Get (Way);
         if Is_Joint_Turnout (Direction => Way,
                              In_Turnout   => Turnout)  then
            Ada.Text_IO.Put_Line ("this is a joint turnout");
         else
            Ada.Text_IO.Put_Line ("this is not a joint turnout");
         end if;
      end loop;

   end Is_Joint_Turnout_Test;
----------------------------------------------------------------------------
   procedure Retrieve_Joint_Turnout_Test (Total_Test : Integer) is
      Way                : Layout.Turnout_Direction;
      Turnout            : Layout.Turnout_ID;
      Joint              : Layout.Turnout_ID;
   begin
      for Count in 1 .. Total_Test loop
         Ada.Text_IO.Put ("Enter Turnout ID : ");
         Turnout_IO.Get (Turnout);
         Ada.Text_IO.Put ("Enter Direction : ");
         Turnout_Direction_IO.Get (Way);
         Retrieve_Joint_Turnout (Direction => Way,
                                 In_Turnout   => Turnout,
                                 Joint     => Joint);
         Ada.Text_IO.Put ("Joint Turnout ID : ");
         Turnout_IO.Put (Joint);
         Ada.Text_IO.New_Line;
      end loop;
   end Retrieve_Joint_Turnout_Test;
----------------------------------------------------------------------------
   procedure Retrieve_Cross_Blocks_Test (Total_Test : Integer) is
      Block_Number       : Layout.Block_ID;
      List               : Layout.Block_List (1);
   begin
      for Count in 1 .. Total_Test loop
         Ada.Text_IO.Put ("Enter Block ID : ");
         Block_IO.Get (Block_Number);
         Retrieve_Cross_Blocks (Block        => Block_Number,
                                Cross_Blocks => List);
	 Ada.Text_IO.Put ("Crossing Blocks : ");
         for item in List.Items'First .. List.Size loop
            Block_IO.Put (List.Items (item));
            Ada.Text_IO.New_Line;
         end loop;
      end loop;
   end Retrieve_Cross_Blocks_Test;
----------------------------------------------------------------------------
   procedure Next_Choice_Turnout_Test  (Total_Test : Integer) is
      Block_Number       : Layout.Block_ID;
      Polarity           : Layout.Block_Polarity;
      Turnout            : Layout.Turnout_ID;

   begin
      for Count in 1 .. Total_Test loop
         Ada.Text_IO.Put ("Enter Block ID : ");
         Block_IO.Get (Block_Number);
         Ada.Text_IO.Put ("Enter Polarity (Normal or Reversed) : ");
         Block_Polarity_IO.Get (Polarity);
         Next_Choice_Turnout (Block    => Block_Number,
                              Polarity => Polarity,
                              Out_Turnout  => Turnout);
         Ada.Text_IO.Put ("Next Turnout ID : ");
         Turnout_IO.Put (Turnout);
         Ada.Text_IO.New_Line;
      end loop;
   end Next_Choice_Turnout_Test;
   ---------------------------------------------------------------------------
   Total_Test : Integer;
begin
   Retrieve_Turnout_ID_Test ("dataTests/retrieveTurnoutNumber.csv");
   Next_Block_ID_Test ("dataTests/retrieveBlockNumber.txt");
   Next_Component_Test ("dataTests/retrieveTrackComponent.txt");
   Turnout_Block_Test ("dataTests/retrieveTurnoutBlocks.txt");
   Hall_ID_Test ("dataTests/retrieveHallId.txt");
   Reversing_Point_Test ("dataTests/retrieveReversingPoint.txt");


   -----------------------------------------------------------------------------
   --            _                __                  _   _
   --           | |              / _|                | | (_)
   --   _____  __ |_ _ __ __ _  | |_ _   _ _ __   ___| |_ _  ___  _ __  ___
   --  / _ \ \/ / __| '__/ _` | |  _| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
   -- |  __/>  <| |_| | | (_| | | | | |_| | | | | (__| |_| | (_) | | | \__ \
   -- \___/_/\_\\__|_|  \__,_| |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
   -----------------------------------------------------------------------------

   Ada.Text_IO.Put ("how many times to test Opposite_Polarity: ");
   Ada.Integer_Text_IO.Get (Total_Test);
   Opposite_Polarity (Total_Test);

   Ada.Text_IO.Put ("how many times to test Opposite_Turnout: ");
   Ada.Integer_Text_IO.Get (Total_Test);
   Opposite_Turnout_Test (Total_Test);

   Ada.Text_IO.Put ("how many times to test Get_Block_IDs_With_Hall_Sensor: ");
   Ada.Integer_Text_IO.Get (Total_Test);
   Get_Block_IDs_With_Hall_Sensor_Test (Total_Test);

   Ada.Text_IO.Put ("how many times to test Is_Forced_Turnout: ");
   Ada.Integer_Text_IO.Get (Total_Test);
   Is_Forced_Turnout_Test (Total_Test);

   Ada.Text_IO.Put ("how many times to test Change_Forced_Turnout: ");
   Ada.Integer_Text_IO.Get (Total_Test);
   Change_Forced_Turnout_Test (Total_Test);

   Ada.Text_IO.Put ("how many times to test Is_Joint_Turnout: ");
   Ada.Integer_Text_IO.Get (Total_Test);
   Is_Joint_Turnout_Test (Total_Test);

   Ada.Text_IO.Put ("how many times to test Retrieve_Joint_Turnout: ");
   Ada.Integer_Text_IO.Get (Total_Test);
   Retrieve_Joint_Turnout_Test (Total_Test);

   Ada.Text_IO.Put ("how many times to test Retrieve_Cross_Blocks: ");
   Ada.Integer_Text_IO.Get (Total_Test);
   Retrieve_Cross_Blocks_Test (Total_Test);

   Ada.Text_IO.Put ("how many times to test Next_Choice_Turnout: ");
   Ada.Integer_Text_IO.Get (Total_Test);
   Next_Choice_Turnout_Test (Total_Test);

end Test_Layout;

-------------------------------------------------------------------------------
