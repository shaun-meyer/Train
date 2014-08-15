-- Locomotivation
-- James Vannordstrand

with Controller;  use Controller;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Layout;
with Trains.Functions;
with Display;
with Cabs;
with DoubleTalk; use DoubleTalk;

package body Engineers.Functions is
   type Enable_Array is array (Engineer_ID) of Boolean;
   Enabled : Enable_Array := (others => False);

   type Controller_Status_Rec is
      record
         Stop_Button     : Button_Type;
         Horn_Button     : Button_Type;
         FB_Switch       : FB_Toggle_Type;
         LRC_Switch      : LRC_Toggle_Type;
         Knob            : Common_Units.Percent;
      end record;

   type Engineer_Skill_Array is array (Engineer_ID) of Skill_Level;
   Skills : Engineer_Skill_Array := (others => Novice);

   task type Engineer_Task is
      entry Set_Enabled (Engineer     : in Engineer_ID;
                         Train_Num    : in Trains.Train_ID;
                         Min_Throttle : in Common_Units.Percent;
                         Control      : in Controller_ID);
      entry Set_Disabled (Engineer  : in Engineer_ID);
      entry Change_Skill;
   end Engineer_Task;

   DELAY_TIME : constant Duration := 0.1;

   task body Engineer_Task is
      Skill            : Skill_Level;
      Previous_Status  : Controller_Status_Rec;
      Stop_Button      : Button_Type;
      Horn_Button      : Button_Type;
      FB_Switch        : FB_Toggle_Type;
      LRC_Switch       : LRC_Toggle_Type;
      Knob             : Common_Units.Percent;
      Train            : Trains.Train_ID;
      Hand_Controller  : Controller_ID;
      ID               : Engineer_ID;
      Wait_Time        : Duration := 0.0;
      Turnout_Time     : Duration := 3.0;

      Cab_Limit        : Common_Units.Percent;
      Current_Speed    : Common_Units.Percent;
      Minimum_Throttle : Common_Units.Percent;

      procedure Call_Turnout_With_Skill (ID        : in Trains.Train_ID;
                                         Skill     : in Skill_Level;
                                         Direction : in LRC_Toggle_Type) is
         -- Calls the proper Set_Turnout function based on the engineer's skill
      begin
         Cabs.Get (Cab   => Cabs.Cab_ID (ID),
                   Value => Current_Speed);
         if Skill = Novice then
            if Current_Speed < (Minimum_Throttle / 2) then
               if Direction = Controller.Left then
                  Trains.Functions.Set_Turnout (ID        => ID,
                                                Direction => Layout.Left);
               else
                  Trains.Functions.Set_Turnout (ID        => ID,
                                                Direction => Layout.Right);
               end if;
            end if;
         else
            if Direction = Controller.Left then
               Trains.Functions.Set_Turnout (ID        => ID,
                                                    Direction => Layout.Left);
            else
               Trains.Functions.Set_Turnout (ID        => ID,
                                                    Direction => Layout.Right);
            end if;
         end if;
      end Call_Turnout_With_Skill;

   begin
      -- Set all values in status rec to their default values
      Previous_Status :=
        (Stop_Button => Up,
         Horn_Button => Up,
         FB_Switch   => Forward,
         LRC_Switch  => Center,
         Knob        => 0);

      loop
         select

            accept Set_Enabled (Engineer     : Engineer_ID;
                                Train_Num    : Trains.Train_ID;
                                Min_Throttle : Common_Units.Percent;
                                Control      : Controller_ID) do
               ID                 := Engineer;
               Skill              := Novice;
               Train              := Train_Num;
               Hand_Controller    := Control;
               Enabled (Engineer) := True;
               Minimum_Throttle   := Min_Throttle;

            end Set_Enabled;

         or
            accept Set_Disabled (Engineer : in Engineer_ID) do
               Enabled (Engineer) := False;
            end Set_Disabled;

         else
            if  Enabled (ID) then
               select
                  accept Change_Skill do

                     if Skill = Novice then
                        Skill := Expert;
                     else
                        Skill := Novice;
                     end if;
                     Skills (ID) := Skill;
                  end Change_Skill;

               else
                  Get (Controller       => Hand_Controller,
                       Red_Button       => Stop_Button,
                       Black_Button     => Horn_Button,
                       Direction_Switch => FB_Switch,
                       Turn_Switch      => LRC_Switch);

                  if Stop_Button /= Previous_Status.Stop_Button
                    and Stop_Button = Down then
                     Trains.Functions.Engineer_Stop (ID => Train);
                  end if;

                  if Horn_Button /= Previous_Status.Horn_Button
                    and Horn_Button = Down then
                     Trains.Functions.Horn (ID => Train);
                  end if;
                  Cabs.Get (Cab   => Cabs.Cab_ID (Train),
                            Value => Current_Speed);
                  if FB_Switch /= Previous_Status.FB_Switch and
                    Current_Speed < (Minimum_Throttle / 2)
                    and Wait_Time >= 3.0 then
                     if FB_Switch = Controller.Forward then
                        Trains.Functions.Set_Direction
                          (ID        => Train,
                           Direction => Trains.Forward);
                        Display.Put (Train     => Train,
                                     Direction => Trains.Forward);
                     else
                        Trains.Functions.Set_Direction
                          (ID        => Train,
                           Direction => Trains.Backward);
                        Display.Put (Train     => Train,
                                     Direction => Trains.Backward);
                     end if;

                  elsif Wait_Time < 3.0
                    and FB_Switch /= Previous_Status.FB_Switch then
                     DoubleTalk.Speak (Phrase =>
      Phrase_Strings.To_Bounded_String ("Engineer " & Engineer_ID'Image (ID)
                  & " has attempted to change direction while train is moving"),
                                       Voice  => DoubleTalk.Vader);
                  end if;

                  if LRC_Switch /= Previous_Status.LRC_Switch
                    and LRC_Switch /= Center and Wait_Time >= 3.0 and
                    Turnout_Time >= 3.0 then
                     Turnout_Time := 0.0;
                     Call_Turnout_With_Skill (ID        => Train,
                                              Skill     => Skill,
                                              Direction => LRC_Switch);
                  elsif Skill = Engineers.Novice  and Wait_Time < 3.0 and
                    LRC_Switch /= Previous_Status.LRC_Switch and
                    LRC_Switch /= Controller.Center then
                     DoubleTalk.Speak (Phrase =>
      Phrase_Strings.To_Bounded_String ("Engineer " & Engineer_ID'Image (ID)
                  & " has attempted to change turnout while moving"),
                                       Voice  => DoubleTalk.Vader);
                  end if;

                  Controller.Get (Controller => Hand_Controller,
                                  Knob       => Knob);

                  if Knob /= Previous_Status.Knob then
                     Trains.Functions.Throttle (ID      => Train,
                                                Percent => Knob);
                  end if;

                  Previous_Status :=
                    (Stop_Button => Stop_Button,
                     Horn_Button => Horn_Button,
                     FB_Switch   => FB_Switch,
                     LRC_Switch  => LRC_Switch,
                     Knob        => Knob);
               end select;
            end if;
         end select;
         delay DELAY_TIME;

         Cabs.Get_Limit (Cab   => Cabs.Cab_ID (Train),
                         Value => Cab_Limit);

         -- Timing how long a train has been stopped following turnout request
         if Knob < (Minimum_Throttle / 2) or Cab_Limit = 0 then
            Wait_Time := Wait_Time + DELAY_TIME;
            Turnout_Time := Turnout_Time + DELAY_TIME;

         else
            Wait_Time := 0.0;
         end if;
      end loop;
   exception
      when Except : others =>
         Put_Line
           (File => Standard_Error,
            Item => "Engineer Task Bad_Logic died with the exception " &
              Ada.Exceptions.Exception_Name (Except));

   end Engineer_Task;

   type Engineer_Array is array (Engineer_ID) of Engineer_Task;
   Engineer_Tasks : Engineer_Array;

   procedure Enable_Engineer (Engineer     : in Engineer_ID;
                              Train        : in Trains.Train_ID;
                              Min_Throttle : in Common_Units.Percent;
                              Control_ID   : in Controller_ID) is
   begin
      Engineer_Tasks (Engineer).Set_Enabled (Engineer   => Engineer,
                                             Train_Num  => Train,
                                             Min_Throttle => Min_Throttle,
                                             Control    => Control_ID);
   end Enable_Engineer;

   procedure Disable_Engineer (Engineer  : in Engineer_ID) is
   begin
      Engineer_Tasks (Engineer).Set_Disabled (Engineer => Engineer);
   end Disable_Engineer;

   procedure Toggle_Skill (Engineer : in Engineer_ID) is
   begin
      if Enabled (Engineer) then
         Engineer_Tasks (Engineer).Change_Skill;
      end if;
   end Toggle_Skill;

   function Get_Skill (Engineer : in Engineer_ID) return Skill_Level is
   begin
      return Skills (Engineer);
   end Get_Skill;

end Engineers.Functions;
