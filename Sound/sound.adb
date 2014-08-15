with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
-- Locomotivation
--Mike Miller

package  body Sound is
   SHORT : constant Duration := 1.0;
   LONG  : constant Duration := 3.0;
   NULL_SOUND : constant Duration := 0.0;
   SILENCE_BETWEEN_BLASTS : constant Duration := 0.5;


   type Time is array (1 .. 4) of Duration;

   type Sound_List is
      record
         Size : Natural := 0;
         Items : Time;
      end record;

   type Signal is array (Horn_Signal) of Sound_List;
   Sound_Meaning : constant Signal := (
       Stop => (Size => 1, Items => (1 => SHORT, others => NULL_SOUND)),
       Approaching_Station =>
         (Size => 1, Items => (1 => LONG, others => NULL_SOUND)),
       Warning =>
         (Size => 2, Items => (1 => LONG, 2 => SHORT, others => NULL_SOUND)),
       Start =>
         (Size => 2, Items => (1 => LONG, 2 => LONG, others => NULL_SOUND)),
       Approaching_Highway =>
         (Size => 4, Items => (1 => LONG, 2 => LONG, 3 => SHORT, 4 => LONG)),
       Approaching_Crossing =>
         (Size => 3,
          Items => (1 => LONG, 2 => LONG, 3 => SHORT, others => NULL_SOUND)),
       Livestock_On_Track =>
         (Size => 4,
          Items => (1 => SHORT, 2 => SHORT, 3 => SHORT, 4 => SHORT)));
-------------------------------------------------------------------------------
   protected Sound_Functions is
      procedure Protected_Write (State : in Dallee.State;
                                 Unit  : in Dallee.Dallee_Sound_Units;
                                 Sound : in Dallee.Sound_Type);
   end Sound_Functions;

   protected body Sound_Functions is
      procedure Protected_Write (State : in Dallee.State;
                                 Unit  : in Dallee.Dallee_Sound_Units;
                                 Sound : in Dallee.Sound_Type) is
      begin
         Dallee.Set_Sound (Changed_State => State,
                           Sound_Unit    => Unit,
                           Sound         => Sound);
      end Protected_Write;

   end Sound_Functions;
-------------------------------------------------------------------------------
   procedure Signal_Duration (Unit         : in Dallee.Dallee_Sound_Units;
                              Sound_Length : in Duration) is
   begin
      Sound_Functions.Protected_Write (State => Dallee.On,
                                       Unit  => Unit,
                                       Sound => Dallee.Horn);
      delay Sound_Length;
      Sound_Functions.Protected_Write (State => Dallee.Off,
                                       Unit  => Unit,
                                       Sound => Dallee.Horn);
   end Signal_Duration;

   task type Horn_Task is
      entry Assign_ID (Assigned_ID : in Dallee.Dallee_Sound_Units);
      entry Signal    (Signal      : in Horn_Signal);
   end Horn_Task;

   task body Horn_Task is
      My_ID : Dallee.Dallee_Sound_Units;
      Current_Signal : Horn_Signal;
   begin
      accept Assign_ID (Assigned_ID : Dallee.Dallee_Sound_Units) do
         My_ID := Assigned_ID;
      end Assign_ID;
      loop
         accept Signal (Signal : Horn_Signal) do
            Current_Signal := Signal;
         end Signal;

            for Time in 1 .. Sound_Meaning (Current_Signal).Size loop
               Signal_Duration
                  (Unit         => My_ID,
                   Sound_Length => Sound_Meaning (Current_Signal).Items (Time));
               delay SILENCE_BETWEEN_BLASTS;
            end loop;
      end loop;
   exception
      when Except : others =>
         Put_Line
           (File => Standard_Error,
            Item => "Sound Task Bad_Logic died with the exception " &
              Ada.Exceptions.Exception_Name (Except));
   end Horn_Task;

   type Horn_Task_Array is array (Dallee.Dallee_Sound_Units) of Horn_Task;
   Horns : Horn_Task_Array;
-------------------------------------------------------------------------------

   procedure Sound_Horn (Unit   : in Dallee.Dallee_Sound_Units;
                         Signal : in Horn_Signal) is
   begin
      select
         Horns (Unit).Signal (Signal);
      else
         null;
      end select;
   end Sound_Horn;
-------------------------------------------------------------------------------
   procedure Bell_On (Unit : in Dallee.Dallee_Sound_Units) is
   begin
      Sound_Functions.Protected_Write (State => Dallee.On,
                                       Unit  => Unit,
                                       Sound => Dallee.Bell);
   end Bell_On;
-------------------------------------------------------------------------------

   procedure Bell_Off (Unit : in Dallee.Dallee_Sound_Units) is
   begin
      Sound_Functions.Protected_Write (State => Dallee.Off,
                                       Unit  => Unit,
                                       Sound => Dallee.Bell);
   end Bell_Off;
-------------------------------------------------------------------------------

   --Initialization Code
begin
   for Index in Horn_Task_Array'Range loop
      Horns (Index).Assign_ID (Index);
   end loop;
end Sound;
