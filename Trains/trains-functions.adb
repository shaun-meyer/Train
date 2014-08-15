--locomotivation
--Mike Miller
with Layout; use Layout;
with DoubleTalk; use DoubleTalk;
with Dallee; use Dallee;
with Sound; use Sound;
with Block;
with Turnouts;
with Display;

package body Trains.Functions is

   type Train_Rec is
      record
         Enabled            : Boolean := False;
         ID                 : Train_ID;
         Cab                : Cabs.Cab_ID;
         Block_List         : Layout.Block_List (6);
         Stop_Record        : Stop_Rec;
         Speed              : Common_Units.Percent := 0;
         Direction          : Direction_Type := Forward;
         Block_Reserve_Fail : Layout.Block_ID;
         Train_Info         : Locomotives.Loco_Rec;
      end record;

   type Trains_Array is array (Train_ID) of Train_Rec;
   The_Trains : Trains_Array;

   ---------------------------------------------------------------------------
   --helper functions
   ---------------------------------------------------------------------------
   procedure Get_Next_Block_Polarity (Current_Block : in Layout.Block_ID;
                                      Hall          : in Halls.Hall_ID;
                                      Polarity   : out Layout.Block_Polarity) is
   -- Helper procedure to get the polarity of the next block
   -- Useful for determining how the next block should be powered
      Current_Block_Polarity : Layout.Block_Polarity;
      Next_Block_Cab         : Cabs.Cab_ID;
   begin
      Block.Get_Polarity (Block    => Current_Block,
                          Polarity => Current_Block_Polarity,
                          Cab      => Next_Block_Cab);
      if Layout.Is_Reversing_Point (Sensor => Hall) then
         Polarity := Layout.Opposite (Current_Block_Polarity);
      else
         Polarity := Current_Block_Polarity;
      end if;
   end Get_Next_Block_Polarity;
   ----------------------------------------------------------------------------
   procedure Convert_To_Smart_Throttle (ID : in Train_ID;
                                       Percent : in out Common_Units.Percent) is
   -- changes the Percent given to a given percent depending on the
   -- minimum throttle of a certain train
      Minimum : constant Float
        := Float (The_Trains (ID).Train_Info.Minimum_Throttle);
   begin
      if Percent <= 10 then
         Percent := Common_Units.Percent ((Minimum / 10.0) * Float (Percent));
      else
         Percent := Common_Units.Percent ((Minimum / 10.0) * 10.0 +
                                          (100.0 - Minimum) / 90.0 *
                                          (Float (Percent) - 10.0));
      end if;
   end Convert_To_Smart_Throttle;
   -----------------------------------------------------------------------------
   procedure Ready (ID : in Train_ID) is
   -- Helper procedure to see if the train is able to start
   -- checks stop reasons to see if the train is ready to be powered
      Go : Boolean := True;
   begin
      for Index in The_Trains (ID).Stop_Record.Reasons'Range loop
         Go := not The_Trains (ID).Stop_Record.Reasons (Index) and Go;
      end loop;

      if Go then
         Cabs.Set_Limit (Cab   => The_Trains (ID).Cab,
                         Value => 100);
         Convert_To_Smart_Throttle (ID      => ID,
                                    Percent => The_Trains (ID).Speed);
         Cabs.Set (Cab   => The_Trains (ID).Cab,
                   Value => The_Trains (ID).Speed);
      end if;
   end Ready;

   procedure Task_Check_Forced (ID       : in Train_ID;
                                Polarity : in Layout.Block_Polarity) is
   -- this procedures finds if there is a forced turnout after recovering
   -- from a fail reserve
      Terminator : Layout.Terminator_Type;
      Turnout    : Layout.Turnout_ID;
      Left       : Layout.Block_ID;
      Right      : Layout.Block_ID;
   begin
      Terminator := Layout.Retrieve_Terminator
        (Block     => The_Trains (ID).Block_Reserve_Fail,
         Direction => Polarity);
      if Terminator = Layout.Turnout then
         Turnout := Layout.Retrieve_Turnout_ID
           (Block     => The_Trains (ID).Block_Reserve_Fail,
            Direction => Polarity);
         Left := Layout.Retrieve_Turnout_Block_ID
           (Direction => Layout.Left,
            Turnout   => Turnout);
         Right := Layout.Retrieve_Turnout_Block_ID
           (Direction => Layout.Right,
            Turnout   => Turnout);
         if Left = The_Trains (ID).Block_List.Items
           (The_Trains (ID).Block_List.Size) then
            Turnouts.Set (Requestor => ID,
                          Turnout   => Turnout,
                          Direction => Layout.Left);
         elsif Right = The_Trains (ID).Block_List.Items
           (The_Trains (ID).Block_List.Size) then
            Turnouts.Set (Requestor => ID,
                          Turnout   => Turnout,
                          Direction => Layout.Right);
         end if;
      end if;
   end Task_Check_Forced;
   -----------------------------------------------------------------------------
   procedure Reverse_Block_List (Blocks : in out Layout.Block_List) is
   -- this takes in a  block list and switches the positions of each
   -- element in the list from the first item to the size of the list
      New_Blocks : Layout.Block_List (6);
      Counter    : Natural := 1;
   begin
      New_Blocks.Size := Blocks.Size;
      for Block in reverse 1 .. Blocks.Size loop
         New_Blocks.Items (Counter) := Blocks.Items (Block);
         Counter := Counter + 1;
      end loop;
      Blocks := New_Blocks;
   end Reverse_Block_List;

   protected type Reserver is
      entry     Reserve_Failed (ID      : in Train_ID;
                                Success : out Boolean);
      procedure Change_Success (New_State : Boolean);
   private
      IS_Reserved : Boolean := True;
   end Reserver;

   protected body Reserver is
      entry Reserve_Failed (ID      : in Train_ID;
                            Success : out Boolean)
        when not IS_Reserved is
      begin
         Block.Reserve (Train_ID => ID,
                        Block    => The_Trains (ID).Block_Reserve_Fail,
                        Success  => IS_Reserved);
         Success := IS_Reserved;
      end Reserve_Failed;
      procedure Change_Success (New_State : Boolean) is
      begin
         IS_Reserved := New_State;
      end Change_Success;
   end Reserver;
   type Reserver_Array_Type is array (Train_ID) of Reserver;
   Reserver_Array : Reserver_Array_Type;
   -----------------------------------------------------------------------------
   --Task
   -----------------------------------------------------------------------------
   -- this will check for any reservation fail and loop till the block is able
   -- to be reserved or the train or turnout are switched
   task type Train_Task is
      entry Assign_ID (Assigned_ID : Train_ID);
   end Train_Task;
   task body Train_Task is
      ID      : Train_ID;
      Success : Boolean;
   begin
      accept Assign_ID (Assigned_ID : Train_ID) do
         ID := Assigned_ID;
      end Assign_ID;

      loop
         select
            accept Assign_ID (Assigned_ID : Train_ID) do
               ID := Assigned_ID;
            end Assign_ID;
         else
            if The_Trains (ID).Stop_Record.Reasons
              (Reservation_Failure) then
               Reserver_Array (ID).Reserve_Failed
                 (ID         => ID,
                  Success    => Success);
               if Success then
                  The_Trains (ID).Stop_Record.Reasons
                    (Reservation_Failure) := False;
                  Display.Put (Train  => ID,
                               Status => The_Trains (ID).Stop_Record);
                  -- check for forced.
                  Task_Check_Forced (ID       => ID,
                                     Polarity => Layout.Normal);
                  Task_Check_Forced (ID       => ID,
                                     Polarity => Layout.Reversed);
                  Ready (ID);
               end if;
            end if;
            delay 1.0;
         end select;
      end loop;
   end Train_Task;

   type Train_Task_Array is array (Train_ID) of Train_Task;
   Train_Tasks : Train_Task_Array;

   -----------------------------------------------------------------------------
   --Helper Functions
   -----------------------------------------------------------------------------
   procedure Reserve (ID              : in Train_ID;
                      Next_Reserve    : in Layout.Block_ID;
                      Reserve_Success : out Boolean) is
   --tries to reserve a given block and returns if there was a success
      Cross_Blocks : Layout.Block_List (1);
      Cross_Block_Success : Boolean := True;
      Next_Block_Reserve_Success : Boolean;
   begin
      Layout.Retrieve_Cross_Blocks (Block        => Next_Reserve,
                                    Cross_Blocks => Cross_Blocks);
      Block.Reserve (Train_ID => ID,
                     Block    => Next_Reserve,
                     Success  => Next_Block_Reserve_Success);
      if not Next_Block_Reserve_Success then
         DoubleTalk.Speak (Phrase => Phrase_Strings.To_Bounded_String (
                           "Locomotivation has halted train " &
                             Trains.Train_ID'Image (ID) &
                             "Because it Cannot Reserve Block" &
                           Layout.Block_ID'Image (Next_Reserve)),
                           Voice  => DoubleTalk.Vader);
         The_Trains (ID).Stop_Record.Reasons (Reservation_Failure) := True;
         The_Trains (ID).Stop_Record.Block := Next_Reserve;
         Display.Put (Train  => ID,
                      Status => The_Trains (ID).Stop_Record);
         Cabs.Set_Limit (Cab   => The_Trains (ID).Cab,
                         Value => 0);
         The_Trains (ID).Block_Reserve_Fail := Next_Reserve;
         Reserver_Array (ID).Change_Success (False);
      else
         The_Trains (ID).Stop_Record.Reasons (Reservation_Failure) := False;
         Display.Put (Train  => ID,
                      Status => The_Trains (ID).Stop_Record);
      end if;
      if Cross_Blocks.Size > 0  and Next_Block_Reserve_Success then
         Block.Reserve (Train_ID => ID,
                        Block    => Cross_Blocks.Items (1),
                        Success  => Cross_Block_Success);
         if not Cross_Block_Success then
            The_Trains (ID).Stop_Record.Reasons (Reservation_Failure) := True;
            The_Trains (ID).Stop_Record.Block := Cross_Blocks.Items (1);
            Display.Put (Train  => ID,
                         Status => The_Trains (ID).Stop_Record);
            Cabs.Set_Limit (Cab   => The_Trains (ID).Cab,
                            Value => 0);
            The_Trains (ID).Block_Reserve_Fail := Next_Reserve;
            Reserver_Array (ID).Change_Success (False);
         else
            The_Trains (ID).Stop_Record.Reasons (Reservation_Failure) := False;
            Display.Put (Train  => ID,
                         Status => The_Trains (ID).Stop_Record);
         end if;
      end if;
      Reserve_Success := Cross_Block_Success and Next_Block_Reserve_Success;
   end Reserve;

   function Convert_Block_List (ID         : in Train_ID;
                                Old_Blocks : in Layout.Search.Block_List)
                                return Layout.Block_List is
   -- changes a Layout.Search.Block_List to a Layout.Block_list
   -- the dif is Layout.Block_List doesn't hold a polarity for a block
      New_Blocks      : Layout.Block_List (6);
      Reserve_Success : Boolean;
   begin
      New_Blocks.Size := Old_Blocks.Size;
      for Current_Block in 1 .. Old_Blocks.Size loop
         Reserve (ID           => ID,
                  Next_Reserve    => Old_Blocks.Items (Current_Block).Block,
                  Reserve_Success => Reserve_Success);
         Block.Connect_Cab (Cab      => The_Trains (ID).Cab,
                            Polarity => Old_Blocks.Items
                              (Current_Block).Direction,
                            Block    => Old_Blocks.Items (Current_Block).Block);
         New_Blocks.Items (Current_Block) :=
           Old_Blocks.Items (Current_Block).Block;
      end loop;
      return New_Blocks;
   end Convert_Block_List;
   ---------------------------------------------------------------------------
   procedure Reserve_Next_Block (ID        : Train_ID;
                                 Block     : Layout.Block_ID;
                                 Direction : Layout.Block_Polarity) is
   -- Reserves block and any cross blocks
      Next_Terminator    : Layout.Terminator_Type;
      Next_Reserve_Block : Layout.Block_ID;
      Turnout_ID         : Layout.Turnout_ID;
      Turnout_Direction  : Layout.Turnout_Direction;
      Joint_Turnout      : Layout.Turnout_ID;
      Forced             : Boolean;
      Reserve_Success    : Boolean;
   begin
      Next_Terminator := Layout.Retrieve_Terminator
        (Block     => Block,
         Direction => Direction);
      if Next_Terminator = Layout.Block then
         Next_Reserve_Block := Layout.Retrieve_Next_Block_ID
           (Block     => Block,
            Direction => Direction);
         Reserve (ID              => ID,
                  Next_Reserve    => Next_Reserve_Block,
                  Reserve_Success => Reserve_Success);
         if Reserve_Success then
            Layout.Is_Forced_Turnout (ID       => Block,
                                      Polarity => Direction,
                                      Forced   => Forced);
            if Forced then
               Layout.Set_Forced_Turnout (Block     => Block,
                                          Polarity  => Direction,
                                          Turnout   => Turnout_ID,
                                          Direction => Turnout_Direction);
               Turnouts.Set (Requestor => ID,
                             Turnout   => Turnout_ID,
                             Direction => Turnout_Direction);
            end if;
         end if;
      elsif Next_Terminator = Layout.Turnout then
         Turnout_ID := Layout.Retrieve_Turnout_ID
           (Block     => Block,
            Direction => Direction);
         Turnout_Direction := Turnouts.Direction_Of (Turnout_ID);
         Next_Reserve_Block := Layout.Retrieve_Turnout_Block_ID
           (Direction => Turnout_Direction,
            Turnout   => Turnout_ID);
         Reserve (ID              => ID,
                  Next_Reserve    => Next_Reserve_Block,
                  Reserve_Success => Reserve_Success);
         if Reserve_Success then
            if Layout.Is_Joint_Turnout (Direction  => Turnout_Direction,
                                        In_Turnout => Turnout_ID) then
               Layout.Retrieve_Joint_Turnout (Direction  => Turnout_Direction,
                                              In_Turnout => Turnout_ID,
                                              Joint      => Joint_Turnout);
               Turnouts.Set (Requestor => ID,
                             Turnout   => Joint_Turnout,
                             Direction => Turnout_Direction);
            end if;
         end if;
      end if;
   end Reserve_Next_Block;
   -----------------------------------------------------------------------------
   procedure UnReserve_Next_Block (ID            : Train_ID;
                                   Current_Block : Layout.Block_ID;
                                   Direction     : Layout.Block_Polarity) is
   -- unreserves when changing direction to free previous next block
      Next_Terminator   : Layout.Terminator_Type;
      UnReserve_Block   : Layout.Block_ID;
      Turnout_ID        : Layout.Turnout_ID;
      Turnout_Direction : Layout.Turnout_Direction;
   begin
      Next_Terminator := Layout.Retrieve_Terminator
        (Block     => Current_Block,
         Direction => Direction);
      if Next_Terminator = Layout.Block and not
        The_Trains (ID).Stop_Record.Reasons (Reservation_Failure) then
         UnReserve_Block := Layout.Retrieve_Next_Block_ID
           (Block     => Current_Block,
            Direction => Direction);
         Block.UnReserve (Block     => UnReserve_Block,
                          Requestor => ID);
      elsif Next_Terminator = Layout.Turnout and not
        The_Trains (ID).Stop_Record.Reasons (Reservation_Failure) then
         Turnout_ID := Layout.Retrieve_Turnout_ID
           (Block     => Current_Block,
            Direction => Direction);
         Turnout_Direction := Turnouts.Direction_Of (Turnout_ID);
         UnReserve_Block := Layout.Retrieve_Turnout_Block_ID
           (Direction => Turnout_Direction,
            Turnout   => Turnout_ID);
         Block.UnReserve (Block     => UnReserve_Block,
                          Requestor => ID);
      end if;
   end UnReserve_Next_Block;
   -----------------------------------------------------------------------------

   procedure Locomotive_Trigger (ID              : in Train_ID;
                                 Powered_Block   : in Layout.Block_ID;
                                 UnPowered_Block : in Layout.Block_ID;
                                 Hall            : in Layout.Hall_ID) is
   -- takes in what block it needs to power next as the unpowered block
   -- connects then unpowered block to the cab and reserves the next block
      Next_Polarity : Layout.Block_Polarity;
   begin
      The_Trains (ID).Block_List.Size := The_Trains (ID).Block_List.Size + 1;
      if The_Trains (ID).Block_List.Size = 6 then
         DoubleTalk.Speak (Phrase => Phrase_Strings.To_Bounded_String (
                           "Locomotivation Halted train " &
                             Trains.Train_ID'Image (ID) &
                             "Because it has Lost cars"),
                           Voice  => DoubleTalk.Vader);
         The_Trains (ID).Stop_Record.Reasons (Lost_Caboose) := True;
         The_Trains (ID).Stop_Record.Reasons (Dispatcher_Request) := True;
         Display.Put (Train  => ID,
                      Status => The_Trains (ID).Stop_Record);
         Cabs.Set_Limit (Cab   => The_Trains (ID).Cab,
                         Value => 0);
      end if;
      The_Trains (ID).Block_List.Items (The_Trains (ID).Block_List.Size) :=
        UnPowered_Block;

      Display.Put (Train  => ID,
                   Blocks => The_Trains (ID).Block_List.Items
                   (1 .. The_Trains (ID).Block_List.Size));

      Get_Next_Block_Polarity (Current_Block => Powered_Block,
                               Hall          => Hall,
                               Polarity      => Next_Polarity);
      Block.Connect_Cab (Cab      => The_Trains (ID).Cab,
                         Polarity => Next_Polarity,
                         Block    => UnPowered_Block);
      Reserve_Next_Block (ID        => ID,
                          Block     => UnPowered_Block,
                          Direction => Next_Polarity);
   end Locomotive_Trigger;
   ----------------------------------------------------------------------------
   --Protected Procedures
   ----------------------------------------------------------------------------
   protected Train_Calls is
      procedure Enable_Train (ID       : in Train_ID;
                              Cab      : in Cabs.Cab_ID;
                              Blocks   : in Layout.Search.Block_List;
                              Starting_Turnouts : in Layout.Search.Turnout_List;
                              Info     : in Locomotives.Loco_Rec);
      procedure Is_Triggered (Hall   : Halls.Hall_ID);
      procedure Throttle (ID      : in Train_ID;
                          Percent : in Common_Units.Percent);
      procedure Set_Turnout (ID        : in Train_ID;
                             Direction : in Layout.Turnout_Direction);
      procedure Set_Direction (ID        : in Train_ID;
                               Direction : in Direction_Type);
      procedure Horn (ID : in Train_ID);
      procedure Engineer_Stop (ID : in Train_ID);
      procedure Dispatcher_Request_Stop (ID : in Train_ID);
      procedure Dispatcher_Request_Go (ID : in Train_ID);
      procedure Failure_Callback (Requestor : in Trains.Request_ID;
                                  Turnout   : in Layout.Turnout_ID);
      procedure Recovery_Callback (Turnout : in Layout.Turnout_ID);
   end Train_Calls;
   -----------------------------------------------------------------------------
   protected body Train_Calls is
      procedure Enable_Train (ID       : in Train_ID;
                              Cab      : in Cabs.Cab_ID;
                              Blocks   : in Layout.Search.Block_List;
                              Starting_Turnouts : in Layout.Search.Turnout_List;
                              Info     : in Locomotives.Loco_Rec) is
      begin
         The_Trains (ID).Cab := Cab;
         The_Trains (ID).ID := ID;
         The_Trains (ID).Train_Info := Info;
         for Turnouts_Index in 1 .. Starting_Turnouts.Size loop
            Turnouts.Set (Requestor => ID,
                          Turnout   => Starting_Turnouts.Items
                            (Turnouts_Index).Turnout,
                          Direction => Starting_Turnouts.Items
                            (Turnouts_Index).Direction);
         end loop;
         The_Trains (ID).Enabled := True;

         The_Trains (ID).Stop_Record.Reasons (Dispatcher_Request) := True;
         Display.Put (Train  => ID,
                      Status => The_Trains (ID).Stop_Record);
         -- Moving the blocks into a List of the Correct size
         The_Trains (ID).Block_List := Convert_Block_List (ID         => ID,
                                                          Old_Blocks => Blocks);
         Display.Put (Train  => ID,
                      Blocks => The_Trains (ID).Block_List.Items
                      (1 .. The_Trains (ID).Block_List.Size));
         Reserve_Next_Block (ID        => ID,
                             Block     => Blocks.Items (Blocks.Size).Block,
                             Direction => Blocks.Items (Blocks.Size).Direction);
      end Enable_Train;
      --------------------------------------------------------------------------
      procedure Is_Triggered (Hall   : Halls.Hall_ID) is

         Block_One          : Layout.Block_ID;
         Block_Two          : Layout.Block_ID;
         Block_One_Cab      : Cabs.Cab_ID;
         Block_Two_Cab      : Cabs.Cab_ID;
         Block_One_Polarity : Layout.Block_Polarity;
         Block_Two_Polarity : Layout.Block_Polarity;
         Current_Train_ID   : Train_ID;
         Caboose_Block      : Layout.Block_ID;
         Cross_Blocks       : Layout.Block_List (1);

      begin
         -- Figure out what blocks the ID that was triggered lies between
         Layout.Get_Block_IDs_With_Hall_Sensor (ID        => Hall,
                                                First_ID  => Block_One,
                                                Second_ID => Block_Two);

         Block.Get_Polarity (Block    => Block_One,
                             Polarity => Block_One_Polarity,
                             Cab      => Block_One_Cab);
         Block.Get_Polarity (Block    => Block_Two,
                             Polarity => Block_Two_Polarity,
                             Cab      => Block_Two_Cab);

         --figure out which train_id  to use
         for Train in The_Trains'Range loop
            if The_Trains (Train).Cab = Block_One_Cab  and
              Block_One_Cab /= 0 then
               Current_Train_ID := Train;
            elsif The_Trains (Train).Cab = Block_Two_Cab and
              Block_Two_Cab /= 0 then
               Current_Train_ID := Train;
            end if;
         end loop;

         if Block_One_Cab = Block_Two_Cab then --back end/Caboose
            if Block_One_Cab = 0 or Block_One_Cab = 7 then
               DoubleTalk.Speak (Phrase => Phrase_Strings.To_Bounded_String (
                            "A Sensor was triggered when no train was near it"),
                                 Voice  => DoubleTalk.Vader);
            else
               if The_Trains (Current_Train_ID).Block_List.Items (1) =
                 Block_One then
                  Caboose_Block := Block_One;
               else
                  Caboose_Block := Block_Two;
               end if;
               Block.UnReserve (Block     => Caboose_Block,
                                Requestor => Current_Train_ID);

               Layout.Retrieve_Cross_Blocks (Block        => Caboose_Block,
                                             Cross_Blocks => Cross_Blocks);
               if Cross_Blocks.Size > 0 then
                  Block.UnReserve (Block     => Cross_Blocks.Items (1),
                                   Requestor => Current_Train_ID);
               end if;

               The_Trains (Current_Train_ID).Block_List.Items
                 (1 .. The_Trains (Current_Train_ID).Block_List.Size - 1) :=
                 The_Trains (Current_Train_ID).Block_List.Items
                 (2 .. The_Trains (Current_Train_ID).Block_List.Size);
               The_Trains (Current_Train_ID).Block_List.Size :=
                 The_Trains (Current_Train_ID).Block_List.Size - 1;

               Display.Put (Train  => Current_Train_ID,
                            Blocks => The_Trains
                              (Current_Train_ID).Block_List.Items
                              (1 .. The_Trains
                                 (Current_Train_ID).Block_List.Size));

               if The_Trains (Current_Train_ID).Stop_Record.Reasons
                 (Lost_Caboose) then
                  The_Trains (Current_Train_ID).Stop_Record.Reasons
                    (Lost_Caboose) := False;
                  Display.Put (Train  => Current_Train_ID,
                               Status => The_Trains
                                 (Current_Train_ID).Stop_Record);
               end if;
            end if;
         else --front end / Loco
            if Block_One_Cab = 0 then
               Locomotive_Trigger (ID              => Current_Train_ID,
                                   Powered_Block   => Block_Two,
                                   UnPowered_Block => Block_One,
                                   Hall            => Hall);
            else
               Locomotive_Trigger (ID              => Current_Train_ID,
                                   Powered_Block   => Block_One,
                                   UnPowered_Block => Block_Two,
                                   Hall            => Hall);
            end if;
         end if;
      end Is_Triggered;
      --------------------------------------------------------------------------
      procedure Throttle (ID      : in Train_ID;
                          Percent : in Common_Units.Percent) is
         Current_Speed  : Common_Units.Percent;
      begin
         Current_Speed := Percent;
         Convert_To_Smart_Throttle (ID      => ID,
                                    Percent => Current_Speed);
         Cabs.Set (Cab   => The_Trains (ID).Cab,
                   Value => Current_Speed);
         Cabs.Get (Cab   => The_Trains (ID).Cab,
                   Value => Current_Speed);
         The_Trains (ID).Speed := Current_Speed;
         if Current_Speed > 0 then
            Display.Put (Train    => ID,
                         Throttle => Percent);
         else
            Display.Put (Train    => ID,
                         Throttle => 0);
         end if;
      end Throttle;
      --------------------------------------------------------------------------
      procedure Set_Turnout (ID        : in Train_ID;
                             Direction : in Layout.Turnout_Direction) is
         Turnout                    : Layout.Turnout_ID;
         Forced_Turnout             : Layout.Turnout_ID;
         Polarity                   : Layout.Block_Polarity;
         Next_Block_Cab             : Cabs.Cab_ID;
         Common_Limb_Joint          : Layout.Block_ID;
         Common_Limb_Choice         : Layout.Block_ID;
         Success                    : Boolean;
         Reservation_Fail_Reserve   : Layout.Block_ID;
         Unreserve_Block            : Layout.Block_ID;
         Count                      : Natural;
         Front_Block                : constant Layout.Block_ID
           := The_Trains (ID).Block_List.Items
           (The_Trains (ID).Block_List.Size);

      begin
         Block.Get_Polarity (Block    => Front_Block,
                             Polarity => Polarity,
                             Cab      => Next_Block_Cab);
         Layout.Next_Choice_Turnout (Block       => Front_Block,
                                     Polarity    => Polarity,
                                     Out_Turnout => Turnout);
         Common_Limb_Choice :=
           Layout.Retrieve_Turnout_Block_ID (Direction => Common,
                                             Turnout   => Turnout);
         Block.Reserve (Train_ID => ID,
                        Block    => Common_Limb_Choice,
                        Success  => Success);
         -- this is an unreserve if a turnoiut is next to keep from powering/
         -- reserving unneeded block
         if Common_Limb_Choice = The_Trains (ID).Block_List.Items
           (The_Trains (ID).Block_List.Size) then
            Unreserve_Block := Layout.Retrieve_Turnout_Block_ID
              (Direction => Layout.Opposite (Direction),
               Turnout   => Turnout);
            Block.Get_Reserve_Count (Block => Unreserve_Block,
                                     ID    => ID,
                                     Count => Count);
            if Count > 0 then
               Block.UnReserve (Block     => Unreserve_Block,
                                Requestor => ID);
            end if;
         end if;
         if Success then
            if Common_Limb_Choice = The_Trains (ID).Block_List.Items
              (The_Trains (ID).Block_List.Size) then
               The_Trains (ID).Stop_Record.Reasons
                 (Reservation_Failure) := False;
               Display.Put (Train  => ID,
                            Status => The_Trains (ID).Stop_Record);
               Reserver_Array (ID).Change_Success (True);
               Ready (ID);
            end if;
            if Layout.Is_Joint_Turnout (Direction  => Direction,
                                        In_Turnout => Turnout) then
               Layout.Retrieve_Joint_Turnout (Direction  => Direction,
                                              In_Turnout => Turnout,
                                              Joint      => Forced_Turnout);
               Common_Limb_Joint :=
                 Layout.Retrieve_Turnout_Block_ID (Direction => Common,
                                                   Turnout   => Forced_Turnout);
               Block.Reserve (Train_ID => ID,
                              Block    => Common_Limb_Joint,
                              Success  => Success);
               if Success then
                  Turnouts.Set (Requestor => ID,
                                Turnout   => Turnout,
                                Direction => Direction);
                  Turnouts.Set (Requestor => ID,
                                Turnout   => Forced_Turnout,
                                Direction => Direction);
                  Block.UnReserve (Block     => Common_Limb_Joint,
                                   Requestor => ID);
               end if;
            else
               Turnouts.Set (Requestor => ID,
                             Turnout   => Turnout,
                             Direction => Direction);
            end if;
            Block.UnReserve (Block     => Common_Limb_Choice,
                             Requestor => ID);
         end if;

         if Common_Limb_Choice = The_Trains (ID).Block_List.Items
           (The_Trains (ID).Block_List.Size) and not
           The_Trains (ID).Stop_Record.Reasons (Reservation_Failure) then
            Reservation_Fail_Reserve := Layout.Retrieve_Turnout_Block_ID
              (Direction => Direction,
               Turnout   => Turnout);
            Reserve (ID              => ID,
                     Next_Reserve    => Reservation_Fail_Reserve,
                     Reserve_Success => Success);
            if Success then
               Reserver_Array (ID).Change_Success (True);
               Ready (ID);
            end if;
         end if;
      end Set_Turnout;
      --------------------------------------------------------------------------
      procedure Set_Direction (ID        : in Train_ID;
                               Direction : in Direction_Type) is
         Front_Polarity : Layout.Block_Polarity;
         Polarity       : Layout.Block_Polarity;
         Percent        : Common_Units.Percent;
         Sound_Unit     : Dallee.Dallee_Sound_Units;
         Next_Block_Cab : Cabs.Cab_ID;
      begin
         if The_Trains (ID).Direction /= Direction then
            for Unit in The_Trains'Range loop
               if ID = Unit then
                  Sound_Unit := Sound_Units (Unit);
               end if;
            end loop;
            if Direction = Trains.Backward then
               Sound.Bell_On (Sound_Unit);
            else
               Sound.Bell_Off (Sound_Unit);
            end if;
            Cabs.Get (Cab   => The_Trains (ID).Cab,
                      Value => Percent);
            The_Trains (ID).Direction := Direction;
            Cabs.Set_Limit (Cab   => The_Trains (ID).Cab,
                            Value => 0);
            Block.Get_Polarity
              (Block  => The_Trains (ID).Block_List.Items
               (The_Trains (ID).Block_List.Size),
               Polarity => Front_Polarity,
               Cab      => Next_Block_Cab);
            for Index in 1 .. The_Trains (ID).Block_List.Size loop
               Block.Get_Polarity
                 (Block    => The_Trains (ID).Block_List.Items (Index),
                  Polarity => Polarity,
                  Cab      => Next_Block_Cab);
               Block.Connect_Cab
                 (Cab      => The_Trains (ID).Cab,
                  Polarity => Layout.Opposite (Polarity),
                  Block    => The_Trains (ID).Block_List.Items (Index));
            end loop;
            UnReserve_Next_Block (ID            => ID,
                                  Current_Block => The_Trains
                                    (ID).Block_List.Items
                                  (The_Trains (ID).Block_List.Size),
                                  Direction     => Front_Polarity);
            Reverse_Block_List (The_Trains (ID).Block_List);
            Block.Get_Polarity
              (Block  => The_Trains (ID).Block_List.Items
               (The_Trains (ID).Block_List.Size),
               Polarity => Front_Polarity,
               Cab      => Next_Block_Cab);
            Reserve_Next_Block (ID        => ID,
                                Block     => The_Trains (ID).Block_List.Items
                                (The_Trains (ID).Block_List.Size),
                                Direction => Front_Polarity);
            Ready (ID);
         end if;
      end Set_Direction;
      --------------------------------------------------------------------------
      procedure Horn (ID : in Train_ID) is
         Percent_Limit : Common_Units.Percent;
         Percent_Speed : Common_Units.Percent;
         Sound_Unit    : Dallee.Dallee_Sound_Units;
      begin
         Cabs.Get_Limit (Cab   => The_Trains (ID).Cab,
                         Value => Percent_Limit);
         Cabs.Get (Cab   => The_Trains (ID).Cab,
                   Value => Percent_Speed);

         for Unit in The_Trains'Range loop
            if ID = Unit then
               Sound_Unit := Sound_Units (Unit);
            end if;
         end loop;
         if Percent_Speed < (The_Trains (ID).Train_Info.Minimum_Throttle / 2)
           or Percent_Limit = 0 then
            Sound.Sound_Horn (Unit   => Sound_Unit,
                              Signal => Sound.Start);
         else
            Sound.Sound_Horn (Unit   => Sound_Unit,
                              Signal => Sound.Approaching_Highway);
         end if;
      end Horn;
      --------------------------------------------------------------------------
      --Stops/GO for train
      -- Go's will need a check to make sure it's okay to release the train.

      procedure Engineer_Stop (ID : in Train_ID) is
      begin
         DoubleTalk.Speak (Phrase => Phrase_Strings.To_Bounded_String (
                           "Engineer " &
                             Trains.Train_ID'Image (ID) &
                             " Request an emergency stop"),
                           Voice  => DoubleTalk.Vader);
         Cabs.Set_Limit (Cab   => The_Trains (ID).Cab,
                         Value => 0);
         The_Trains (ID).Stop_Record.Reasons (Dispatcher_Request) := True;
         Display.Put (Train  => ID,
                      Status => The_Trains (ID).Stop_Record);
      end Engineer_Stop;
      --------------------------------------------------------------------------
      procedure Dispatcher_Request_Stop (ID : in Train_ID) is
      begin
         DoubleTalk.Speak (Phrase => Phrase_Strings.To_Bounded_String (
                           "Dispatcher Request an Emergency Stop for train " &
                             Trains.Train_ID'Image (ID)),
                           Voice  => DoubleTalk.Vader);
         Cabs.Set_Limit (Cab   => The_Trains (ID).Cab,
                         Value => 0);

         The_Trains (ID).Stop_Record.Reasons (Dispatcher_Request) := True;
         Display.Put (Train  => ID,
                      Status => The_Trains (ID).Stop_Record);
      end Dispatcher_Request_Stop;
      --------------------------------------------------------------------------
      procedure Dispatcher_Request_Go (ID : in Train_ID) is
      begin
         DoubleTalk.Speak (Phrase => Phrase_Strings.To_Bounded_String (
                           "The Dispatcher has given permission for train " &
                             Trains.Train_ID'Image (ID) &
                             " to start"),
                           Voice  => DoubleTalk.Vader);
         The_Trains (ID).Stop_Record.Reasons (Dispatcher_Request) := False;
         Display.Put (Train  => ID,
                      Status => The_Trains (ID).Stop_Record);
         Ready (ID);
      end Dispatcher_Request_Go;
      --------------------------------------------------------------------------
      procedure Failure_Callback (Requestor : in Trains.Request_ID;
                                  Turnout   : in Layout.Turnout_ID) is
      begin
         if Requestor /= 0 then
            DoubleTalk.Speak (Phrase => Phrase_Strings.To_Bounded_String (
                              "Locomotivation Halted train " &
                              Trains.Request_ID'Image (Requestor) &
                              "Because Turnout " &
                              Layout.Turnout_ID'Image (Turnout) &
                              "Failed"),
                           Voice  => DoubleTalk.Vader);
            Cabs.Set_Limit (Cab   => The_Trains (Requestor).Cab,
                            Value => 0);
            The_Trains (Requestor).Stop_Record.Reasons
              (Turnout_Failure) := True;
            The_Trains (Requestor).Stop_Record.Turnouts (Turnout) := True;
            Display.Put (Train  => Requestor,
                         Status => The_Trains (Requestor).Stop_Record);
         else
            DoubleTalk.Speak (Phrase => Phrase_Strings.To_Bounded_String (
                             "Locomotivation Halted All Trains Because Turnout "
                              & Layout.Turnout_ID'Image (Turnout) &
                              "Failed"),
                              Voice  => DoubleTalk.Vader);

            for Train in Trains_Array'Range loop
               Cabs.Set_Limit (Cab   => The_Trains (Train).Cab,
                               Value => 0);

               The_Trains (Train).Stop_Record.Reasons
                 (Dispatcher_Request) := True;
               Display.Put (Train  => Train,
                            Status => The_Trains (Train).Stop_Record);
            end loop;
         end if;
      end Failure_Callback;
      --------------------------------------------------------------------------
      procedure Recovery_Callback (Turnout : in Layout.Turnout_ID) is
      begin
         for Train in Trains_Array'Range loop
            if The_Trains (Train).Stop_Record.Turnouts (Turnout) then

               The_Trains (Train).Stop_Record.Reasons
                 (Turnout_Failure) := False;
               Display.Put (Train  => Train,
                            Status => The_Trains (Train).Stop_Record);
               Ready (Train);
            end if;
         end loop;
      end Recovery_Callback;
   end Train_Calls;
   -----------------------------------------------------------------------------
   --Train procedures
   -----------------------------------------------------------------------------

   procedure Enable_Train (ID     : in Train_ID;
                           Cab    : in Cabs.Cab_ID;
                           Blocks : in Layout.Search.Block_List;
                           Starting_Turnouts : in Layout.Search.Turnout_List;
                           Info   : in Locomotives.Loco_Rec) is
   begin
      Train_Tasks (ID).Assign_ID (ID);

      Train_Calls.Enable_Train (ID     => ID,
                                Cab    => Cab,
                                Blocks => Blocks,
                                Starting_Turnouts => Starting_Turnouts,
                                Info   => Info);
   end Enable_Train;

   procedure Is_Triggered (Hall   : Halls.Hall_ID) is
   begin
      Train_Calls.Is_Triggered (Hall);
   end Is_Triggered;

   procedure Throttle (ID      : in Train_ID;
                       Percent : in Common_Units.Percent) is
   begin
      if The_Trains (ID).Enabled then
         Train_Calls.Throttle (ID      => ID,
                               Percent => Percent);
      end if;
   end Throttle;

   procedure Set_Turnout (ID        : in Train_ID;
                          Direction : in Layout.Turnout_Direction) is
   begin
      if The_Trains (ID).Enabled then
         Train_Calls.Set_Turnout (ID        => ID,
                                  Direction => Direction);
      end if;
   end Set_Turnout;

   procedure Set_Direction (ID        : in Train_ID;
                            Direction : in Direction_Type) is
   begin
      if The_Trains (ID).Enabled then
         Train_Calls.Set_Direction (ID        => ID,
                                    Direction => Direction);
      end if;
   end Set_Direction;

   procedure Horn (ID : in Train_ID) is
   begin
      if The_Trains (ID).Enabled then
         Train_Calls.Horn (ID);
      end if;
   end Horn;

   procedure Engineer_Stop (ID : in Train_ID) is
   begin
      if The_Trains (ID).Enabled then
         Train_Calls.Engineer_Stop (ID);
      end if;
   end Engineer_Stop;

   procedure Dispatcher_Request_Stop (ID : in Train_ID) is
   begin
      if The_Trains (ID).Enabled then
         Train_Calls.Dispatcher_Request_Stop (ID);
      end if;
   end Dispatcher_Request_Stop;

   procedure Dispatcher_Request_Go (ID : in Train_ID) is
   begin
      if The_Trains (ID).Enabled then
         Train_Calls.Dispatcher_Request_Go (ID);
      end if;
   end Dispatcher_Request_Go;

   procedure Failure_Callback (Requestor : in Trains.Request_ID;
                               Turnout   : in Layout.Turnout_ID) is
   begin
      Train_Calls.Failure_Callback (Requestor => Requestor,
                                    Turnout   => Turnout);
   end Failure_Callback;

   procedure Recovery_Callback (Turnout : in Layout.Turnout_ID) is
   begin
      Train_Calls.Recovery_Callback (Turnout);
   end Recovery_Callback;

   procedure Disable is
   begin
      for ID in Train_ID'Range loop
         The_Trains (ID).Enabled := False;
         The_Trains (ID).Stop_Record.Reasons := (False, False, False, False);
         The_Trains (ID).Speed := 0;
      end loop;
      Block.UnReserve_All;

   end Disable;

   -----------------------------------------------------------------------------


   Triggered_Callback : Halls.Callback_Ptr;
   Fail_Call : Turnouts.Failure_Ptr;
   Recovery_Call : Turnouts.Recover_Ptr;

begin
   Fail_Call := Failure_Callback'Access;
   Recovery_Call := Recovery_Callback'Access;
   Triggered_Callback := Is_Triggered'Access;
   Turnouts.Set_Failure_Callback (Fail_Call);
   Turnouts.Set_Recovery_Callback (Recovery_Call);
   Halls.Initialize;
   Halls.Enable (Callback => Triggered_Callback);
end Trains.Functions;
