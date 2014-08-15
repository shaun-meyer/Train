-- locomotivation
-- Mike Miller
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Ada.Text_IO;
with Controller;
with Engineers;
with Engineers.Functions;
with Trains;
procedure Engineers_Test is
   package Engineer_IO is new Ada.Text_IO.Integer_IO (Engineers.Engineer_ID);
   package Train_IO is new Ada.Text_IO.Integer_IO (Trains.Train_ID);
   package Controller_IO is new Ada.Text_IO.Enumeration_IO
      (Controller.Controller_ID);

   Choice     : Character;
   Engineer   : Engineers.Engineer_ID;
   Train      : Trains.Train_ID;
   Controll : Controller.Controller_ID;
begin
   loop
      Ada.Text_IO.Put ("(T)oggle_Skill (E)nable_Engineer (D)isable_Engineer :");
      Ada.Text_IO.Get (Choice);
      if Choice = 'T' then
         Ada.Text_IO.Put ("Enter Engineer_ID : ");
         Engineer_IO.Get (Engineer);
         Engineers.Functions.Toggle_Skill (Engineer);
      elsif Choice = 'E' then
         Ada.Text_IO.Put ("Enter Engineer_ID : ");
         Engineer_IO.Get (Engineer);
         Ada.Text_IO.Put ("Enter Train_ID : ");
         Train_IO.Get (Train);
         Ada.Text_IO.Put ("Enter Controller_ID : ");
         Controller_IO.Get (Controll);

         Engineers.Functions.Enable_Engineer (Engineer   => Engineer,
                                    Train      => Train,
                                    Control_ID => Controll);
      elsif Choice = 'D' then
         Ada.Text_IO.Put ("Enter Engineer_ID : ");
         Engineer_IO.Get (Engineer);
         Engineers.Functions.Disable_Engineer (Engineer);
      end if;
   end loop;

end Engineers_Test;
