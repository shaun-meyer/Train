--Locomotivation
--Mike Miller

with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Ada.Text_IO;
with Controller;   use type Controller.Button_Type;
with Ada.Integer_Text_IO;
procedure Button_Press_Count is

   type Button_Rec is
      record
         Current : Controller.Button_Type;
         Last    : Controller.Button_Type;
      end record;

   Black_Button_Pushes_Total : Natural;
   Red_Button                : Button_Rec;
   Black_Button              : Button_Rec;
   Direction_Swith           : Controller.FB_Toggle_Type;
   Turn_Switch               : Controller.LRC_Toggle_Type;

begin

   Black_Button_Pushes_Total := 0;
   Black_Button.Last         := Controller.Up;
   Red_Button.Last           := Controller.Up;

   --Infinite loop getting button input and incrementing/reseting count
   --Each iteration gets one button press of either color
   loop
      Controller.Get (Controller       => Controller.B,
                      Red_Button       => Red_Button.Current,
                      Black_Button     => Black_Button.Current,
                      Direction_Switch => Direction_Swith,
                      Turn_Switch      => Turn_Switch);
      if Black_Button.Current = Controller.Down and
         Black_Button.Last = Controller.Up then
         Black_Button_Pushes_Total := Black_Button_Pushes_Total + 1;
         Black_Button.Last := Controller.Down;
      elsif Black_Button.Current = Controller.Up then
         Black_Button.Last := Controller.Up;
      end if;

      if Red_Button.Current = Controller.Down and
         Red_Button.Last = Controller.Up then
         Ada.Text_IO.Put (Item => "Black Button Total Pushes: ");
         Ada.Integer_Text_IO.Put (Item => Black_Button_Pushes_Total);
         Ada.Text_IO.New_Line;
         Black_Button_Pushes_Total := 0;
         Red_Button.Last := Controller.Down;
      elsif Red_Button.Current = Controller.Up then
         Red_Button.Last := Controller.Up;
      end if;

      --To stop misreads from switch bounce
      delay 0.05;
   end loop;
end Button_Press_Count;
