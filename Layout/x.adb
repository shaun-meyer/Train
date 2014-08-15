with Ada.Text_IO;
with Ada.Integer_Text_IO;
procedure X is
   x: String (1 .. 3);
   y: Integer;

begin
   x (1) := '1';
   x (2) := '2';
   x (3) := '!';
   Ada.Text_IO.Put_Line (Item => x);

   y := Integer'Value( x (1 .. 2));
   Ada.Integer_Text_IO.Put (y);
end X;
