--  Locomotivation
--  James Vannordstrand
with Console_Management;
with Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Command is
   type State_Type is (Waiting_First_Char, Waiting_Second_Char,
                       Waiting_Third_Char, Basic_Validation,
                       Compound_Validation);


   procedure Handle_Compound_Validation (Command_Number : in Integer;
                                         Char           : in Character;
                                         Command        : out Command_Rec) is
   begin
      Console_Management.Set_Raw_Mode;
      case Char is
         when 'S' | 's' =>
            if Command_Number > 0 and
              Command_Number < 4 then
               Command := (Which => Stop,
                           Train => Trains.Request_ID (Command_Number));
            else
               Command := (Which => Error);
            end if;

         when 'G' | 'g' =>
            if Command_Number > 0 and
              Command_Number < 4 then
               Command := (Which => Go,
                           Train => Trains.Request_ID (Command_Number));
            else
               Command := (Which => Error);
            end if;

         when 'L' | 'l' =>
            if Command_Number > 0 and
              Command_Number < 27 then
               Command := (Which   => Left,
                           Turnout => Layout.Turnout_ID
                             (Command_Number));
            else
               Command := (Which => Error);
            end if;

         when 'R' | 'r' =>
            if Command_Number > 0 and
              Command_Number < 27 then
               Command := (Which   => Right,
                           Turnout => Layout.Turnout_ID
                             (Command_Number));
            else
               Command := (Which => Error);
            end if;

         when 'F' | 'f' =>
            if Command_Number > 0 and
              Command_Number < 41 then
               Command := (Which => Free,
                           Block => Layout.Block_ID (Command_Number));
            else
               Command := (Which => Error);
            end if;

         when 'E' | 'e' =>
            if Command_Number > 0 and
              Command_Number < 4 then
               Command := (Which    => Skill,
                        Engineer => Engineers.Engineer_ID (Command_Number));
            else
               Command := (Which => Error);
            end if;
            -- Toggle engineer skill level for train
         when others =>
            Command := (Which => Error);
      end case;
   end Handle_Compound_Validation;

   procedure Retrieve_Command (State   : in out State_Type;
                               Command : out Command_Rec) is
      --This procedure will retrieve the command given, and depending on the
      -- current state, handles the command
      --Preconditions  : None
      --Postconditions : A command is sent as the out parameter

      Char : Character;
      Response : String (1 .. 3);
      Command_Number : Integer;
      Response_Size : Integer := 0;

   begin
      loop
         case State is
            when Waiting_First_Char =>
               Ada.Text_IO.Get (Item => Char);
               Response (1) := Char;
               Response_Size := Response_Size + 1;
               if Is_Digit (Item => Char) then
                  State := Waiting_Second_Char;
               else
                  State := Basic_Validation;
               end if;

            when Waiting_Second_Char =>
               Ada.Text_IO.Get (Item => Char);
               Response (2) := Char;
               Response_Size := Response_Size + 1;
               if Is_Digit (Item => Char) then
                  State := Waiting_Third_Char;
               else
                  State := Compound_Validation;
               end if;

            when Waiting_Third_Char =>
               Ada.Text_IO.Get (Item => Char);
               if not Is_Digit (Item => Char) then
                  Response (3) := Char;
                  Response_Size := Response_Size + 1;
                  State := Compound_Validation;
               else
                  Command := (Which => Error);
                  State := Waiting_First_Char;
                  exit;
               end if;

            when Basic_Validation =>
               case Response (1) is
                  when 'R' | 'r' =>
                     Command := (Which => Restart);
                  when 'Q' | 'q' =>
                     Command := (Which => Quit);
                  when ' ' =>
                     Command := (Which => Stop_All);
                  when others =>
                     Command := (Which => Error);
               end case;
               exit;

            when Compound_Validation =>
               Command_Number := Integer'Value
                 (Response (1 .. Response_Size - 1));
               -- Grab the numeric part of the response

               Handle_Compound_Validation (Command_Number => Command_Number,
                                     Char           => Response (Response_Size),
                                           Command        => Command);
               exit;
         end case;
      end loop;
   end Retrieve_Command;

   procedure Get (Command : out Command_Rec) is
      State : State_Type := Waiting_First_Char;
   begin
      Retrieve_Command (State   => State,
                        Command => Command);
   end Get;
-- space bar stops all trains
-- Ng starts train N (n = 1..3)
-- Ns stops train N (n = 1..3)
-- Nr turns turnout right (N = 1..26)
-- Nl turns turnout left (N = 1..26)
-- nf free all reservations on block n (n = 1..40)
-- ne set engineer skill lvl (n = 1..3)
-- r reinitialize system hults all trains, displays welcome screen
end Command;
