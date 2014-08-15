--Locomotivation
--Shaun Meyer
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Layout;
with Layout.Search; use type Layout.Search.Turnout_Array;

procedure Test_Layout_Search is
   package Block_IO is new Ada.Text_IO.Integer_IO (Layout.Block_ID);
   package Turnout_IO is new Ada.Text_IO.Integer_IO (Layout.Turnout_ID);
   package Polarity_IO is new Ada.Text_IO.Enumeration_IO (Enum =>
      Layout.Block_Polarity);
   package Turn_Choice_IO is new Ada.Text_IO.Enumeration_IO (Enum =>
      Layout.Turn_Choice);

   Loco               :  Layout.Block_ID;
   Caboose            :  Layout.Block_ID;
   Blocks_Found       :  Layout.Search.Block_List (3);
   Turnouts_Found     :  Layout.Search.Turnout_List (10);
   Success            :  Boolean;
   Continue           :  Character;

begin

   loop

      Ada.Text_IO.Put ("Enter Locomotive - ");
      Block_IO.Get (Loco);
      Ada.Text_IO.Put ("Enter Caboose - ");
      Block_IO.Get (Caboose);

      Layout.Search.Blocks_Beneath (Loco => Loco,
                                    Caboose => Caboose,
                                    Blocks => Blocks_Found,
                                    Turnouts => Turnouts_Found,
                                    Success => Success);

      if Success then

         Ada.Text_IO.Put_Line ("Block List");
         for Block in 1 .. Blocks_Found.Size loop
            Ada.Integer_Text_IO.Put (Block);
            Ada.Text_IO.Put (" Block Number : ");
            Block_IO.Put (Blocks_Found.Items (Block).Block);
            Ada.Text_IO.Put (" Direction : ");
            Polarity_IO.Put (Blocks_Found.Items (Block).Direction);
            Ada.Text_IO.New_Line;
         end loop;

         Ada.Text_IO.Put_Line ("Turnout List");
         for Turnout in 1 .. Turnouts_Found.Size loop
            Ada.Integer_Text_IO.Put (Turnout);
            Ada.Text_IO.Put (" Turnout Number : ");
            Turnout_IO.Put (Turnouts_Found.Items (Turnout).Turnout);
            Ada.Text_IO.Put (" Direction : ");
            Turn_Choice_IO.Put (Turnouts_Found.Items (Turnout).Direction);
            Ada.Text_IO.New_Line;
         end loop;
      else
         Ada.Text_IO.Put_Line ("Search Failed");
      end if;

      Ada.Text_IO.Put ("Continue Testing? (Y) - ");
      Ada.Text_IO.Get (Continue);
      exit when Continue /= 'Y';
      Blocks_Found.Size := 0;
      Turnouts_Found.Size := 0;

   end loop;

end Test_Layout_Search;
