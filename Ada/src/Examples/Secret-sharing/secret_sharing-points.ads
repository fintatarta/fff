package Secret_Sharing.Points is
   type Point_Type is
      record
         X, Y : Secret_Type;
      end record;

   type Point_Array is array (Positive range <>) of Point_Type;

   function Image (P : Point_Type) return String;

   function Parse (S : String) return Point_Type;
end Secret_Sharing.Points;
