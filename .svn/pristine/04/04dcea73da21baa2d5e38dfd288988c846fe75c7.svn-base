-- Locomotivation
-- James Vannordstrand

with Ada.Text_IO;
with MaRTE_OS;
pragma Warnings (off, MaRTE_OS);
with Controller;
with Common_Units;

procedure Controller_Test is

   package Controller_IO is new Ada.Text_IO.Enumeration_IO
     (Enum => Controller.Controller_ID);

   package FB_Toggle_IO is new Ada.Text_IO.Enumeration_IO
     (Enum => Controller.FB_Toggle_Type);

   package LRC_Toggle_IO is new Ada.Text_IO.Enumeration_IO
     (Enum => Controller.LRC_Toggle_Type);

   package Button_IO is new Ada.Text_IO.Enumeration_IO
     (Enum => Controller.Button_Type);

   package Percent_IO is new Ada.Text_IO.Integer_IO
     (Num => Common_Units.Percent);

   Red_Button       : Controller.Button_Type;
   Black_Button     : Controller.Button_Type;
   Direction_Switch : Controller.FB_Toggle_Type;
   Turn_Switch      : Controller.LRC_Toggle_Type;
   Knob_Percent     : Common_Units.Percent;

begin

   -- infinite loop to check state of controller buttons and levers
   -- each iteration displays the current state
   loop
      Ada.Text_IO.Put ("Press Enter Continue.");
      Ada.Text_IO.Skip_Line;
      Ada.Text_IO.Put_Line ("-----------------------------");
      for ID in Controller.Controller_ID'Range loop
         Controller.Get (Controller       => ID,
                         Red_Button       => Red_Button,
                         Black_Button     => Black_Button,
                         Direction_Switch => Direction_Switch,
                         Turn_Switch      => Turn_Switch);

         Controller.Get (Controller => ID,
                           Knob       => Knob_Percent);

         Ada.Text_IO.Put ("Controller_ID : ");
         Controller_IO.Put (ID);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("Red Button : ");
         Button_IO.Put (Red_Button);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("Black Button : ");
         Button_IO.Put (Black_Button);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("Direction Switch : ");
         FB_Toggle_IO.Put (Direction_Switch);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("Turn Switch : ");
         LRC_Toggle_IO.Put (Turn_Switch);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("Knob Percentage : ");
         Percent_IO.Put (Knob_Percent);
         Ada.Text_IO.New_Line;

      end loop;
      Ada.Text_IO.Put_Line ("-----------------------------");
   end loop;

end Controller_Test;
