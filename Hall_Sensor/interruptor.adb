with Ada.Text_IO;
with DoubleTalk;
use DoubleTalk;

package body Interruptor is
   package Hall_IO is new Ada.Text_IO.Integer_IO (Num => Halls.Hall_ID);
   procedure Print_Interrupt (ID : in Halls.Hall_ID) is
   begin
      Ada.Text_IO.Put ("INTERRUPTING YOUR PROGRAM WITH ID #");
      Hall_IO.Put (ID);
      DoubleTalk.Speak (Phrase => Phrase_Strings.To_Bounded_String (
                        "INTERRUPTING YOUR PROGRAM WITH ID"
                        & Integer'Image (Integer (ID))),
                        Voice  => Vader);
      Ada.Text_IO.New_Line;
   end Print_Interrupt;
end Interruptor;
