--Locomotivation
--Shaun Meyer

with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Common_Units;
with Console_Management;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Handling;
with Ada.IO_Exceptions;
with Layout; use type Layout.Block_ID;
with Layout.Search;
with Locomotives;
with Command;
with Engineers;
with Engineers.Functions;
with Trains;
with Trains.Functions;
with Cabs;
with Controller;
with Turnouts;
with Display;
with Block;
with DoubleTalk;
use type Command.Command_Type;
with Motors;
with Ada.Strings.Fixed;

procedure Dispatcher is

   type Question_Number is (First, Second, Third);

   --Initialize number of trains to 3 to avoid null pointers / constraint errors
   Number_Of_Trains  : Integer := 3;

   type Train_Rec is
      record
         Info               : Locomotives.Loco_Rec;
         Loco               : Integer;
         Caboose            : Integer;
         Blocks             : Layout.Search.Block_List (3);
         Turnouts           : Layout.Search.Turnout_List (4);
         Cab                : Cabs.Cab_ID;
         Previously_Enabled : Boolean := False;
      end record;
   type Train_Array is array (Positive range <>) of Train_Rec;

   type Train_List is
      record
         Size : Natural := 0;
         Items : Train_Array (1 .. Number_Of_Trains);
      end record;
   The_Trains : Train_List;

   package Block_IO is new Ada.Text_IO.Integer_IO (Num => Layout.Block_ID);

--------------------------------------------------------------------------------
   procedure Validate_Letter_Input (Letter   :    out Character;
                                    Question : in     Question_Number) is
      -- This procedure validates a character input based on which question
      --   the character is entered in response to
      -- Preconditions : None
      -- Postconditions: Sends a valid, uppercase letter as an out parameter

      Continue : Boolean := False;
      -- Using Continue flag to avoid having multiple exit statements in loop

   begin
      --Loops until valid
      --each loop checks validity
      loop
         --First question has results Y, N, or Q
         --Second question has results Y, N, Q, or R
         --Third question has results R or N
         Ada.Text_IO.Get (Letter);
         Letter := Ada.Characters.Handling.To_Upper (Letter); --uppercase it
         if (Letter = 'Y' or Letter = 'N' or Letter = 'Q')
            and Question = First then
            Continue := True;

            --Changing N to R to simplify parsing later
            -- Equivalent statements
            if Letter = 'N' then
               Letter := 'R';
            end if;

         elsif (Letter = 'Y' or Letter = 'N' or Letter = 'Q' or Letter = 'R')
            and Question = Second then
            Continue := True;
         elsif (Letter = 'N' or Letter = 'R') and Question = Third then
            Continue := True;
         end if;

         exit when Continue;

         --If it didn't exit user entered bad input
         if Question = First then
            Ada.Text_IO.Put_Line ("Not a Valid Response. Please enter" &
                                  " 'Y', 'N', or 'Q'");
         elsif Question = Second then
            Ada.Text_IO.Put_Line ("Not a Valid Response. " &
                                    "Please enter 'Y', 'N', 'R' or 'Q'");
         else
            Ada.Text_IO.Put_Line ("Not a Valid Response. " &
                                    "Please enter 'N' or 'R'");
         end if;
      end loop;
   end Validate_Letter_Input;
--------------------------------------------------------------------------------
   procedure Validate_Integer_Input (User_Instruction : in     String;
                                     Low_End          : in     Integer;
                                     High_End         : in     Integer;
                                     Input_Value      :    out Integer) is
      -- This procedure validates an integer input based on which question
      --   the integer is entered in response to
      -- Preconditions : None
      -- Postconditions: Sends a validated integer as an out parameter
   begin
      loop
         --While the input value is outside of the boundaries, print a usage
         -- statement. If the input value is within the boundaries, send it out
         Ada.Text_IO.Put_Line (User_Instruction);
         begin
            Ada.Integer_Text_IO.Get (Input_Value);

            if Low_End <= Input_Value and High_End >= Input_Value then
               exit;
            else
               Ada.Text_IO.Put ("Input valid integer between ");
               Ada.Integer_Text_IO.Put (Low_End, 1);
               Ada.Text_IO.Put (" and ");
               Ada.Integer_Text_IO.Put (High_End, 1);
               Ada.Text_IO.Put_Line (" (inclusive).");
            end if;

         exception
            when Ada.IO_Exceptions.Data_Error =>
               Ada.Text_IO.Put_Line ("Enter an integer.");
               Ada.Text_IO.Skip_Line;
         end;
      end loop;
      Ada.Text_IO.New_Line;
   end Validate_Integer_Input;
--------------------------------------------------------------------------------
   procedure Dispatcher_Startup (Number_Of_Trains : out Integer) is
      -- This procedure starts the dispatcher by asking how many trains to run
      -- Preconditions : None
      -- Postconditions: Send the number of trains as an out parameter
   begin
      Validate_Integer_Input
        (User_Instruction => "How many trains do you wish to run? (1, 2, or 3)",
         Low_End          => 1,
         High_End         => 3,
         Input_Value      => Number_Of_Trains);

   end Dispatcher_Startup;
--------------------------------------------------------------------------------
   procedure Get_Other_Train_Data (New_Train_Type : out Locomotives.Loco_Rec) is
      -- This procedure deals with the situation where the user specifies
      --    a train that is not of the 10 originally supplied types
      -- Preconditions  : None
      -- Postconditions : A new loco_rec instance is created and passed out

      Road_Name    : String (1 .. 20);
      Name_Length  : Integer;
      Model        : String (1 .. 15);
      Model_Length : Integer;
      Number       : String (1 .. 5);
      Num_Length   : Integer;
      Train_Name   : Locomotives.Loco_String;
      Min_Throttle : Common_Units.Percent;
   begin
      Ada.Text_IO.New_Line;

      loop
         begin
            Ada.Text_IO.Put_Line ("What is the train's road name?  ");
            Ada.Text_IO.Skip_Line;
            Ada.Text_IO.Get_Line (Item => Road_Name,
                                  Last => Name_Length);
            Ada.Text_IO.Put_Line ("What is the train's model?  ");
            Ada.Text_IO.Get_Line (Item => Model,
                                  Last => Model_Length);
            Ada.Text_IO.Skip_Line;
            Ada.Text_IO.Put_Line ("What is the train's number?  ");
            Ada.Text_IO.Get_Line (Item => Number,
                                  Last => Num_Length);
            exit;
         exception
            when Constraint_Error =>
               Ada.Text_IO.Put_Line ("Max lengths for each request: ");
               Ada.Text_IO.Put_Line ("Name - 20. Model - 15. Number - 5.");
         end;
      end loop;
      Ada.Strings.Fixed.Move (Source  => (Road_Name (1 .. Name_Length) & " " &
                                         Model (1 .. Model_Length) & " " &
                                         Number (1 .. Num_Length)) & " ",
                              Target  => Train_Name,
                              Pad     => Ada.Strings.Space);
      Validate_Integer_Input (User_Instruction => "What is the minimum " &
                                "percent throttle the train requires to move? ",
                              Low_End          => 0,
                              High_End         => 100,
                              Input_Value      =>
                                Common_Units.Percent (Min_Throttle));
      New_Train_Type := (Name             => Train_Name,
                         Minimum_Throttle => Min_Throttle);
   end Get_Other_Train_Data;
--------------------------------------------------------------------------------
   procedure Confirm_Train_Data
     (Train_Number           : in Integer;
      Blocks_Beneath         : in Layout.Search.Block_List) is
      -- This procedure displays the current train's data for confirmation
      -- Preconditions  : None
      -- Postconditions : The train's current data is displayed on screen
   begin
      Ada.Text_IO.Put_Line ("                Train #" &
                              Integer'Image (Train_Number));
      Ada.Text_IO.Put_Line ("            Confirmation of Information");

      Ada.Text_IO.Put (The_Trains.Items (Train_Number).Info.Name);
      Ada.Text_IO.New_Line (2);

      Ada.Text_IO.Put ("Locomotive on block");
      Ada.Integer_Text_IO.Put (The_Trains.Items (Train_Number).Loco);
      Ada.Text_IO.New_Line (1);
      Ada.Text_IO.Put ("Caboose    on block");
      Ada.Integer_Text_IO.Put (The_Trains.Items (Train_Number).Caboose);
      Ada.Text_IO.New_Line (1);
      Ada.Text_IO.Put ("Train occupies blocks");
      for Count in Blocks_Beneath.Items'First .. Blocks_Beneath.Size loop
         Block_IO.Put (Item => Blocks_Beneath.Items (Count).Block);
      end loop;
      Ada.Text_IO.New_Line (2);
   end Confirm_Train_Data;
--------------------------------------------------------------------------------
   procedure Check_Block_Lists (Train   : in Integer;
                                Block   : in Layout.Block_ID;
                                Success : out Boolean) is
   begin
      Success := True;
      for Current_Train in 1 .. Train - 1 loop
         for Count in 1 .. The_Trains.Items (Current_Train).Blocks.Size loop
            if Block =
              The_Trains.Items (Current_Train).Blocks.Items (Count).Block then
               Success := False;
            end if;
         end loop;
      end loop;
   end Check_Block_Lists;
--------------------------------------------------------------------------------
   procedure Retrieve_Train_Data
     (Train_Number   : in  Integer;
      Found_Success  : out Boolean) is
      -- This procedure gathers all relevant information for the train,
      -- including the train type name and the starting blocks of loco & caboose
      -- Preconditions : None
      -- Postconditions: Sends the result of running the layout search and a
      --   list of the trains as out parameters, as well as setting the data
      --   retrieved as input to the specified train

      Train_Type              : Integer;
      Train_Starting_Block    : Integer;
      Caboose_Starting_Block  : Integer;
      Blocks_Beneath          : Layout.Search.Block_List (3);
      Turnouts_Beneath        : Layout.Search.Turnout_List (4);
      User_Input              : Character;

   begin
      Blocks_Beneath.Size := 0;
      Turnouts_Beneath.Size := 0;

      if not The_Trains.Items (Train_Number).Previously_Enabled then
         -- If this is the first time the system is set up, ask for all info
         -- Present which trains are available for use
         Ada.Text_IO.Put_Line ("The following locomotives are available");
         Ada.Text_IO.New_Line (2);
         Ada.Text_IO.Put_Line ("   Road Name            Model          Number");
         Ada.Text_IO.New_Line;
         for Item in Locomotives.Available_Locos'Range loop
            Ada.Integer_Text_IO.Put (Item  => Item,
                                     Width => 2);
            Ada.Text_IO.Put (" ");
            Ada.Text_IO.Put (Locomotives.Available_Locos (Item).Name);
            Ada.Text_IO.New_Line;
         end loop;
         Ada.Integer_Text_IO.Put
           (Item  => Locomotives.Available_Locos'Length + 1,
            Width => 2);
         Ada.Text_IO.Put (" Other");
         Ada.Text_IO.New_Line (1);

         -- Retrieve which train model will be used for this train object
         Validate_Integer_Input
           (User_Instruction => "Enter the line number from the above table" &
              " of the locomotive pulling train #" &
              Integer'Image (Train_Number),
            Low_End          => 1,
            High_End         => Locomotives.Available_Locos'Length + 1,
            Input_Value      => Train_Type);

         -- If the selected type is other, a separate way of persisting the data
         --   needs to be executed
         if Train_Type = Locomotives.Available_Locos'Length + 1 then
            Get_Other_Train_Data
              (New_Train_Type => The_Trains.Items (Train_Number).Info);
         else
            The_Trains.Items (Train_Number).Info.Name :=
              Locomotives.Available_Locos (Train_Type).Name;
            The_Trains.Items (Train_Number).Info.Minimum_Throttle :=
              Locomotives.Available_Locos (Train_Type).Minimum_Throttle;
         end if;

         -- Retrieve the starting block for the loco
         Validate_Integer_Input
           (User_Instruction => "On which block is the locomotive" &
              " pulling train" &
              Integer'Image (Train_Number) & " located?",
            Low_End          => 1,
            High_End         => 40,
            Input_Value      => Train_Starting_Block);

         The_Trains.Items (Train_Number).Loco := Train_Starting_Block;

         -- Retrieve the starting block for the caboose
         Validate_Integer_Input
           (User_Instruction => "On which block is the caboose (or " &
              "observation car) for train" &
              Integer'Image (Train_Number) & " located?",
            Low_End          => 1,
            High_End         => 40,
            Input_Value      => Caboose_Starting_Block);

         The_Trains.Items (Train_Number).Caboose := Caboose_Starting_Block;

         if Caboose_Starting_Block = Train_Starting_Block then
            Ada.Text_IO.Put_Line ("What is the direction of the locomotive " &
                                    "on the block?");
            Ada.Text_IO.Put_Line ("        N = Normal");
            Ada.Text_IO.Put_Line ("        R = Reversed");
            Validate_Letter_Input (Letter   => User_Input,
                                   Question => Third);

            Blocks_Beneath.Size := 1;
            Blocks_Beneath.Items (1).Block :=
              Layout.Block_ID (Train_Starting_Block);
            if User_Input = 'N' then
               Blocks_Beneath.Items (1).Direction := Layout.Normal;
            else
               Blocks_Beneath.Items (1).Direction := Layout.Reversed;
            end if;
            Found_Success := True;

            -- Run the layout search to see if the train has a valid placement
         else
            Layout.Search.Blocks_Beneath
              (Loco     => Layout.Block_ID (Train_Starting_Block),
               Caboose  => Layout.Block_ID (Caboose_Starting_Block),
               Blocks   => Blocks_Beneath,
               Turnouts => Turnouts_Beneath,
               Success  => Found_Success);

            for Each_Block in 1 .. Blocks_Beneath.Size loop
               Check_Block_Lists
                 (Train   => Train_Number,
                  Block   => Blocks_Beneath.Items (Each_Block).Block,
                  Success => Found_Success);
            end loop;
         end if;
      else
         -- If this isn't the first time the system is set up, ask if the old
         --    train data stored is still correct
         Found_Success := True;
      end if;

      if Found_Success and not
        The_Trains.Items (Train_Number).Previously_Enabled then
         The_Trains.Items (Train_Number).Blocks := Blocks_Beneath;
         The_Trains.Items (Train_Number).Turnouts := Turnouts_Beneath;
      end if;


   end Retrieve_Train_Data;
--------------------------------------------------------------------------------
   procedure Dispatcher_Data_Retrieval (Number_Of_Trains : in Integer;
                                        User_Response    : out Character) is
      -- This procedure acts as a wrapper for the data retrieval procedure,
      --  ensuring that the process is able to terminate and reinitialize
      --  successfully.
      -- Preconditions : None
      -- Postconditions: All data for the trains is gathered properly, and user
      --  commands are followed

      Success      : Boolean;
      Train_Number : Integer := 1;

   begin
      User_Response := 'Y';

      The_Trains.Size := Number_Of_Trains;

      while Train_Number <= Number_Of_Trains loop

         Retrieve_Train_Data (Train_Number  => Train_Number,
                              Found_Success => Success);

         while not Success loop

            Ada.Text_IO.Put_Line ("The maximum number of blocks beneath a train"
                                  & " is 3. The minimum is 1.");
            Ada.Text_IO.Put_Line ("There is either an invalid number of blocks"
                                  & "beneath the train, or the location " &
                                    "information for this train is not valid " &
                                 "because it conflicts with another train.");
            Ada.Text_IO.New_Line (2);

            Ada.Text_IO.Put_Line ("Would you like to reenter the information?");
            Ada.Text_IO.Put_Line ("    Y = yes, I wish to reenter" &
                                    " the train's location.");
            Ada.Text_IO.Put_Line ("    N = no, I wish to restart " &
                                    "set up from the beginning.");
            Ada.Text_IO.Put_Line ("    Q = no, I wish to terminate" &
                                    " this operating session.");

            Validate_Letter_Input (Letter   => User_Response,
                                   Question => First);

            if User_Response = 'Y' then
               Retrieve_Train_Data (Train_Number  => Train_Number,
                                    Found_Success => Success);
            else
               Success := True;  --Making it leave the loop
            end if;

         end loop;

         exit when User_Response = 'R' or User_Response = 'Q';

         Confirm_Train_Data (Train_Number           => Train_Number,
                             Blocks_Beneath         => The_Trains.Items
                               (Train_Number).Blocks);

         Ada.Text_IO.Put_Line ("Is this information correct? " &
                                 "Enter one of the following : ");
         Ada.Text_IO.Put_Line ("    Y = yes, the information" &
                                 " for this train is correct.");
         Ada.Text_IO.Put_Line ("    N = no, I wish to enter " &
                                 "different information for this train.");
         Ada.Text_IO.Put_Line ("    R = no, I wish to restart " &
                                 "setting up from the beginning.");
         Ada.Text_IO.Put_Line ("    Q = no, I wish to terminate " &
                                 "this operating session.");

         Validate_Letter_Input (Letter   => User_Response,
                                Question => Second);

         exit when User_Response = 'R' or User_Response = 'Q';

         if User_Response = 'Y' then
            The_Trains.Items (Train_Number).Cab := Cabs.Cab_ID (Train_Number);

            Engineers.Functions.Enable_Engineer
              (Engineer     => Engineers.Engineer_ID (Train_Number),
               Train        => Trains.Train_ID (Train_Number),
               Min_Throttle => The_Trains.Items
                 (Train_Number).Info.Minimum_Throttle,
               Control_ID   => Controller.Controller_ID'Val (Train_Number - 1));
            Train_Number := Train_Number + 1;
         elsif User_Response = 'N' then
            The_Trains.Items (Train_Number).Previously_Enabled := False;
         end if;

      end loop;
   end Dispatcher_Data_Retrieval;
--------------------------------------------------------------------------------
   procedure Enable_Trains (Train_Number : Integer;
                            The_Trains   : Train_List) is
      -- This procedure will enable the train object specified by train number
      -- Preconditions  : None
      -- Postconditions : Train number [Train_Number] is enabled
   begin
      Trains.Functions.Enable_Train
        (ID                => Trains.Train_ID (Train_Number),
         Cab               => The_Trains.Items (Train_Number).Cab,
         Blocks            => The_Trains.Items (Train_Number).Blocks,
         Starting_Turnouts => The_Trains.Items (Train_Number).Turnouts,
         Info              => The_Trains.Items (Train_Number).Info);
   end Enable_Trains;
--------------------------------------------------------------------------------
   procedure Fill_Display is
      -- This procedure puts all necessary preliminary data to the display
      -- Preconditions  : None
      -- Postconditions : Startup data for the layout is sent to the screen
   begin
      for Index in 1 .. The_Trains.Size loop
         Display.Put (Train => Trains.Request_ID (Index),
                      Name  => The_Trains.Items (Index).Info.Name);
         Display.Put (Train => Trains.Request_ID (Index),
                      Skill => Engineers.Novice);
         Display.Put (Train     => Trains.Request_ID (Index),
                      Direction => Trains.Forward);
         Display.Put (Train    => Trains.Request_ID (Index),
                      Throttle => 0);
      end loop;

      for Index in 1 .. Layout.Turnout_ID'Last loop
         Display.Put (Turnout   => Index,
                      Direction => Turnouts.Direction_Of (Turnout => Index),
                      Moving    => False);
      end loop;
   end Fill_Display;
--------------------------------------------------------------------------------
   procedure Setup is
      -- This procedure implements the Dispatcher's setup phase
      -- This includes calling procedures to gather number of trains and
      --   information about where and what the trains are
      -- It will then call a procedure to enable all active trains
      -- Preconditions  : None
      -- Postconditions : Train data is gathered and stored, trains are enabled,
      --    and the console is prepared for the "controlling the program" state
      User_Response : Character;

   begin
      Console_Management.Set_Cursor (20, 20);

      Ada.Text_IO.New_Line (30);
      Console_Management.Set_Cooked_Mode;
      Console_Management.Enable_Echo;
      loop
         Dispatcher_Startup (Number_Of_Trains => Number_Of_Trains);
         Dispatcher_Data_Retrieval (Number_Of_Trains => Number_Of_Trains,
                                    User_Response    => User_Response);
         exit when User_Response /= 'R';
      end loop;
      if User_Response /= 'Q' then
         Console_Management.Clear_Screen;
         Display.Enable;
         Fill_Display;
         for Index in 1 .. Number_Of_Trains loop
            Enable_Trains (Train_Number => Index,
                           The_Trains   => The_Trains);
         end loop;
      else
         Ada.Text_IO.Put_Line ("Hit 'Q' to again to quit.");
      end if;
      Console_Management.Set_Raw_Mode;
      Console_Management.Disable_Echo;
   end Setup;
--------------------------------------------------------------------------------
   procedure Handle_Command (Input_Command : in Command.Command_Rec) is
      -- This procedure parses and executes commands supplied by the user
      -- Preconditions  : None
      -- Postcondition  : Proper command is executed
      Successful_Shutdown : Boolean := True;
   begin
      if Input_Command.Which = Command.Restart then
         for Index in 1 .. Number_Of_Trains loop
            Engineers.Functions.Disable_Engineer
              (Engineer => Engineers.Engineer_ID (Index));
            Trains.Functions.Dispatcher_Request_Stop
              (ID => Trains.Train_ID (Index));
            The_Trains.Items (Index).Previously_Enabled := True;
         end loop;
         Trains.Functions.Disable;
         delay 1.0;
         Display.Disable;
         Console_Management.Clear_Screen;

      elsif Input_Command.Which = Command.Quit then
         Display.Disable;
         Console_Management.Clear_Screen;
         Turnouts.Shut_Down;
         for Turnout in Layout.Turnout_ID'Range loop
            Successful_Shutdown := Successful_Shutdown and Motors.In_Position
              (Turnout);
         end loop;
         if Successful_Shutdown then
            Console_Management.Enable_Echo;
            Console_Management.Set_Cursor (Row => 12,
                                           Column => 39);
            Ada.Text_IO.New_Line (5);
            Ada.Text_IO.Put_Line ("Safe to power off.");
         end if;
      elsif Input_Command.Which = Command.Go then
         Trains.Functions.Dispatcher_Request_Go (ID => Input_Command.Train);
      elsif Input_Command.Which = Command.Stop then
         Trains.Functions.Dispatcher_Request_Stop (ID => Input_Command.Train);
      elsif Input_Command.Which = Command.Stop_All then
         for Index in 1 .. Number_Of_Trains loop
            Trains.Functions.Dispatcher_Request_Stop
              (ID => Trains.Train_ID (Index));
         end loop;
      elsif Input_Command.Which = Command.Left then
         Turnouts.Set (Requestor => 0,
                       Turnout   => Input_Command.Turnout,
                       Direction => Layout.Left);
      elsif Input_Command.Which = Command.Right then
         Turnouts.Set (Requestor => 0,
                       Turnout   => Input_Command.Turnout,
                       Direction => Layout.Right);
      elsif Input_Command.Which = Command.Free then
         Block.UnReserve (Block     => Input_Command.Block,
                          Requestor => 0);
      elsif Input_Command.Which = Command.Skill then
         Engineers.Functions.Toggle_Skill (Engineer => Input_Command.Engineer);
         Display.Put (Train => Trains.Train_ID (Input_Command.Engineer),
                      Skill => Engineers.Functions.Get_Skill
                        (Engineer => Input_Command.Engineer));
      else -- command = Error
         DoubleTalk.Speak
           (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String ("Dispatcher "
              & "has entered an invalid command."),
            Voice  => DoubleTalk.Vader);
      end if;
   end Handle_Command;
--------------------------------------------------------------------------------
   User_Command  : Command.Command_Rec;

begin
   DoubleTalk.Speak
     (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String ("Flying " &
        "Scotsman is ready to begin an operating session."),
      Voice  => DoubleTalk.Vader);
   loop
      Setup;
      loop
         Command.Get (Command => User_Command);
         Handle_Command (Input_Command => User_Command);
         exit when User_Command.Which = Command.Quit or
           User_Command.Which = Command.Restart;
      end loop;
      exit when User_Command.Which = Command.Quit;
   end loop;
   Display.Disable;
   Console_Management.Clear_Screen;
   DoubleTalk.Speak
     (Phrase => DoubleTalk.Phrase_Strings.To_Bounded_String ("Flying " &
     "Scotsman has shut down"),
      Voice  => DoubleTalk.Vader);
end Dispatcher;
