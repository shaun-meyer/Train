-- locomotivation
-- James Vannordstrand
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Sound; use Sound;
with Dallee;

procedure Sound_Test is
   package Dallee_Sound_Units_IO is new Ada.Text_IO.Integer_IO
     (Dallee.Dallee_Sound_Units);

   package Signal_IO is new Ada.Text_IO.Enumeration_IO
     (Sound.Horn_Signal);

-----------------------------------------------------------------------
   task type Concurrent_Horns (Unit   : Dallee.Dallee_Sound_Units;
                              Signal  : Sound.Horn_Signal);
   task body Concurrent_Horns is

   begin
      Sound.Sound_Horn (Unit   => Unit,
                        Signal => Signal);
   end Concurrent_Horns;
-----------------------------------------------------------------------
   type Horn_Ptr is access Concurrent_Horns;

   Horn_Task     : Horn_Ptr;
   Sound_Unit    : Dallee.Dallee_Sound_Units;
   Chosen_Signal : Sound.Horn_Signal;
   Concurrent    : Positive;
   Bell		 : Positive;

begin
   Ada.Text_IO.Put ("Concurrent? Yes(1) or No(2) ");
   Ada.Integer_Text_IO.Get (Concurrent);
   if Concurrent = 1 then
      loop
         Ada.Text_IO.Put ("Enter Sound_Unit (Horn): ");
         Dallee_Sound_Units_IO.Get (Sound_Unit);
         Ada.Text_IO.Put ("Enter Horn_Signal : ");
         Signal_IO.Get (Chosen_Signal);
         Horn_Task := new Concurrent_Horns (Unit   => Sound_Unit,
                                            Signal => Chosen_Signal);
      end loop;
   else
      loop
         Ada.Text_IO.Put ("Enter Sound_Unit (Horn): ");
         Dallee_Sound_Units_IO.Get (Sound_Unit);
         Ada.Text_IO.Put ("Enter Horn_Signal : ");
         Signal_IO.Get (Chosen_Signal);
         Sound.Sound_Horn (Unit   => Sound_Unit,
                           Signal => Chosen_Signal);

         Ada.Text_IO.Put ("Enter Sound_Unit (Bell) : ");
         Dallee_Sound_Units_IO.Get (Sound_Unit);
         Ada.Text_IO.Put ("Sound On(1) or Off(2) ");
         Ada.Integer_Text_IO.Get (Bell);
         if Bell = 1 then
            Sound.Bell_On (Unit   => Sound_Unit);
         else
            Sound.Bell_Off (Unit   => Sound_Unit);
         end if;
      end loop;
   end if;
end Sound_Test;
