-- Locomotivation
-- Mike Miller
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with DoubleTalk;
use DoubleTalk;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
procedure Doubletalk_Test is
begin
   -- validate that all voices are correct
   for Voice in DoubleTalk.Voice_Range loop

      Speak (Phrase => Phrase_Strings.To_Bounded_String ("This is " &
               DoubleTalk.Voice_Range'Image (Voice)),
             Voice  => Voice);
      Ada.Text_IO.Put_Line (DoubleTalk.Voice_Range'Image (Voice));
      Delay 2.0
   end loop;
   -- over load queue to make sure correct position is dequeued
   for Voice in DoubleTalk.Voice_Range loop

      Speak (Phrase => Phrase_Strings.To_Bounded_String ("This is " &
               DoubleTalk.Voice_Range'Image (Voice)),
             Voice  => Voice);
   end loop;
end Doubletalk_Test;
