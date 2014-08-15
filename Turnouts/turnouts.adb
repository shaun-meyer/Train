-- Locomotivation
-- Shaun Meyer
with Motors;
with Layout;
use Layout;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

package body Turnouts is
   type Turnouts_Array is array (Layout.Turnout_ID) of Status_Rec;
   Turnouts : Turnouts_Array;

   Failure_Callback  : Failure_Ptr; --Stop Train
   Recovery_Callback : Recover_Ptr; --Start Train
   Change_Callback   : Change_Ptr; --display info

   task type Turnout_Task is
      entry Set    (Requestor : in Trains.Request_ID;
                    Turnout   : in Layout.Turnout_ID;
                    Direction : in Layout.Turnout_Direction);
   end Turnout_Task;

   task body Turnout_Task is
      In_Progress : Boolean := False;
      Train_ID    : Trains.Request_ID;
      Turnout_To_Set : Layout.Turnout_ID;
      Direction_To_Set : Layout.Turnout_Direction;
   begin
      loop
         accept Set (Requestor : in Trains.Request_ID;
                     Turnout   : in Layout.Turnout_ID;
                     Direction : in Layout.Turnout_Direction) do
            Direction_To_Set := Direction;
            if not In_Progress then
               In_Progress := True;
               Train_ID := Requestor;
               Turnout_To_Set := Turnout;
            end if;
         end Set;

         if In_Progress then
            Turnouts (Turnout_To_Set).Desired := Direction_To_Set;
            Turnouts (Turnout_To_Set).Current := Direction_To_Set;
            Turnouts (Turnout_To_Set).Moving := True;

            Change_Callback.all (Turnout   => Turnout_To_Set,
                                 Direction => Direction_To_Set,
                                 Moving    => Turnouts (Turnout_To_Set).Moving);

            if Direction_To_Set = Layout.Left then
               Motors.Set (Motor     => Turnout_To_Set,
                           Direction => Motors.Left);
            else
               Motors.Set (Motor     => Turnout_To_Set,
                           Direction => Motors.Right);
            end if;

            delay until Ada.Real_Time.Clock + Time_Limit;

            if Motors.In_Position (Turnout_To_Set) then
               Turnouts (Turnout_To_Set).Moving := False;
               Change_Callback.all (Turnout => Turnout_To_Set,
                                    Direction => Direction_To_Set,
                                    Moving => Turnouts (Turnout_To_Set).Moving);
            else
               Failure_Callback.all (Requestor => Train_ID,
                                     Turnout   => Turnout_To_Set);
               while not Motors.In_Position (Turnout_To_Set) or
                 Direction_To_Set /= Turnouts (Turnout_To_Set).Current loop

                  Turnouts (Turnout_To_Set).Current :=
                    Layout.Opposite (Turnouts (Turnout_To_Set).Current);
                  Change_Callback.all (Turnout   => Turnout_To_Set,
                                 Direction => Turnouts (Turnout_To_Set).Current,
                                 Moving    => Turnouts (Turnout_To_Set).Moving);

                  if Turnouts (Turnout_To_Set).Current = Layout.Left then
                     Motors.Set (Motor     => Turnout_To_Set,
                                 Direction => Motors.Left);
                  else
                     Motors.Set (Motor     => Turnout_To_Set,
                                 Direction => Motors.Right);
                  end if;

                  delay until Ada.Real_Time.Clock + Time_Limit;
               end loop;

               Turnouts (Turnout_To_Set).Moving := False;
               Change_Callback.all (Turnout   => Turnout_To_Set,
                                    Direction => Direction_To_Set,
                                 Moving    => Turnouts (Turnout_To_Set).Moving);
               Recovery_Callback.all (Turnout_To_Set);
            end if;
            In_Progress := False;
         end if;
      end loop;
   exception
      when Except : others =>
         Put_Line
           (File => Standard_Error,
            Item => "Turnout Task Bad_Logic died with the exception " &
              Ada.Exceptions.Exception_Name (Except));
   end Turnout_Task;

   type Turnout_Task_Array is array (Layout.Turnout_ID) of Turnout_Task;
   Turnout_Tasks : Turnout_Task_Array;
   -----------------------------------------------------------------------------
   procedure Set_Failure_Callback (To : in Failure_Ptr) is
   begin
      Failure_Callback := To;
   end Set_Failure_Callback;
   -----------------------------------------------------------------------------
   procedure Set_Recovery_Callback (To : in Recover_Ptr) is
   begin
      Recovery_Callback := To;
   end Set_Recovery_Callback;
   -----------------------------------------------------------------------------
   procedure Set_Change_Callback (To : in Change_Ptr) is
   begin
      Change_Callback := To;
   end Set_Change_Callback;
   -----------------------------------------------------------------------------
   procedure Set (Requestor : in Trains.Request_ID;
                  Turnout   : in Layout.Turnout_ID;
                  Direction : in Layout.Turnout_Direction) is
   begin
      Turnout_Tasks (Turnout).Set (Requestor => Requestor,
                                   Turnout   => Turnout,
                                   Direction => Direction);
   end Set;
   -----------------------------------------------------------------------------
   function Status (Turnout : in  Layout.Turnout_ID) return Status_Rec is
   begin
      return Turnouts (Turnout);
   end Status;

   function Direction_Of (Turnout : in Layout.Turnout_ID)
                          return Layout.Turnout_Direction is
   begin
      return Turnouts (Turnout).Desired;
   end Direction_Of;
   -----------------------------------------------------------------------------
   procedure Shut_Down is
   begin
      for Turnout in Layout.Turnout_ID'Range loop
         if (Turnout mod 5) = 0 then
            delay until Ada.Real_Time.Clock + Time_Limit;
         end if;
         Motors.Set (Motor     => Turnout,
                     Direction => Motors.Left);

      end loop;
   end Shut_Down;

end Turnouts;
