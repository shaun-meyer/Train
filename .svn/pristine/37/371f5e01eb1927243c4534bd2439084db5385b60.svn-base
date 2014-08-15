generic
   type Element_Type is private;
package Bounded_Queue is
-- Final version, encapsulated generic queue

   type Queue_Type (Max_Size : Positive) is limited private;

   Overflow  : exception;
   Underflow : exception;

   procedure Clear (Queue : in out Queue_Type);

   procedure Enqueue (Queue : in out Queue_Type;
                      Item  : in     Element_Type);
   -- Overflow is raised on attempt to Enqueue an element onto
   --          a full queue.  Queue is unchanged.

   procedure Dequeue (Queue : in out Queue_Type;
                      Item  :    out Element_Type);
   -- Underflow is raised on attempt to dequeue an element from
   --           an empty Queue.  Queue remains empty.

   function Full (Queue : in Queue_Type) return Boolean;

   function Empty (Queue : in Queue_Type) return Boolean;

private

   type Queue_Array is array (Positive range <>) of Element_Type;
   type Queue_Type (Max_Size : Positive) is
      record
        Count : Natural  := 0;         -- Number of items in the queue
        Front : Positive := 1;         -- Index of the first item in the queue
        Rear  : Positive := Max_Size;  -- Index of the last item in the queue
        Items : Queue_Array (1 .. Max_Size);  -- The element array
      end record;

end Bounded_Queue;
