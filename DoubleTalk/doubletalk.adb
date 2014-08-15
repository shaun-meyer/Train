--Locomotivation
--James Vannordstrand
with Bounded_Queue;
with Port_IO;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

package body DoubleTalk is

   type Phrase_Rec is
      record
         Phrase : Phrase_Strings.Bounded_String;
         Voice  : Voice_Range;
      end record;

   package Phrase_Queue is new Bounded_Queue
     (Element_Type => Phrase_Rec);

   BASE    : constant Port_IO.Address_Range := 16#31F#;
   CONTROL : constant Character := Character'Val (0);
   COMMAND : constant Character := Character'Val (1);

--     type Phrase_Rec is
--        record
--           Phrase : Phrase_Strings.Bounded_String;
--           Voice  : Voice_Range;
--        end record;

   type TTSD_Rec is  -- Type for the TTD data register
      record
         Voice : Voice_Range;
         Char  : Character;
      end record;

   type TTSS_Array is array (0 .. 7) of Boolean;
   --Type for the TTD status register
   for TTSS_Array'Size use 8;
   for TTSS_Array'Component_Size use 1;

   type Voice_Array is array (0 .. 7) of Character;
   Voice_Characters : constant Voice_Array := ('0', '1', '2', '3', '4', '5',
                                               '6', '7');

   function To_Array is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte,
      Target => TTSS_Array);
   --------------------------------------------------
   procedure Write_Character (Char : in Character) is
      TTSS : TTSS_Array;
   begin
      loop
         delay 0.000025;
         TTSS := To_Array (Port_IO.In_Byte (Address => BASE));
         exit when TTSS (4);
      end loop;
      Port_IO.Out_Byte (Address => BASE,
                        Data    => Port_IO.Byte (Character'Pos (Char)));
   end Write_Character;
   --------------------------------------------------
   procedure Change_Voice (Voice : in Voice_Range) is
   begin
      Write_Character (COMMAND);
      Write_Character (Voice_Characters (Voice_Range'Pos (Voice)));
      Write_Character ('O');
   end Change_Voice;
   --------------------------------------------------
   protected type Bounded_Buffer is
      entry     Remove_Phrase (Phrase : out Phrase_Rec);
      procedure Add_Phrase    (Phrase : in  Phrase_Rec);
   private
      Phrase_Buffer : Phrase_Queue.Queue_Type (Buffer_Size);
   end Bounded_Buffer;

   protected body Bounded_Buffer is
      entry Remove_Phrase (Phrase : out Phrase_Rec)
        when not Phrase_Queue.Empty (Phrase_Buffer) is

      begin
         Phrase_Queue.Dequeue (Queue => Phrase_Buffer,
                               Item  => Phrase);
      end Remove_Phrase;

      procedure Add_Phrase (Phrase : in Phrase_Rec) is
	 Phrase_Garbage : Phrase_Rec;
      begin
         if Phrase_Queue.Full (Phrase_Buffer) then
            Phrase_Queue.Dequeue (Queue => Phrase_Buffer,
                                  Item  => Phrase_Garbage);
         end if;
         Phrase_Queue.Enqueue (Queue => Phrase_Buffer,
                               Item  => Phrase);
      end Add_Phrase;

   end Bounded_Buffer;

   The_Buffer : Bounded_Buffer;

   --------------------------------------------------
   --procedure consumer will call to send char to doubletalk
   --------------------------------------------------
   task Consumer;

   task body Consumer is

      Phrase_Record : Phrase_Rec;
      TTSD          : TTSD_Rec;

   begin
      loop
         The_Buffer.Remove_Phrase (Phrase => Phrase_Record);
         Change_Voice (Voice => Phrase_Record.Voice);
         for Char_Index in 1 .. Phrase_Strings.Length (Phrase_Record.Phrase)
         loop
            TTSD.Char := Phrase_Strings.Element (Source => Phrase_Record.Phrase,
                                                 Index  => Char_Index);
            TTSD.Voice := Phrase_Record.Voice;

            Write_Character (TTSD.Char);
         end loop;
         delay 1.0;
         Write_Character (CONTROL);
      end loop;
   exception
      when Except : others =>
         Put_Line
           (File => Standard_Error,
            Item => "Task Bad_Logic died with the exception " &
              Ada.Exceptions.Exception_Name (Except));
   end Consumer;

   --------------------------------------------------

   procedure Speak (Phrase : in Phrase_Strings.Bounded_String;
                    Voice  : in Voice_Range) is
      Phrase_Record : constant Phrase_Rec := (Phrase => Phrase, Voice => Voice);
   begin
      The_Buffer.Add_Phrase (Phrase_Record);
   end Speak;
end DoubleTalk;
