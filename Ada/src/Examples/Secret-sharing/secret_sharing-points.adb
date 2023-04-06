pragma Ada_2012;
with Ada.Strings.Fixed;

package body Secret_Sharing.Points is
   Conversion_Table : constant String := "0123456789abcdef";

   Secret_Image_Size : constant Natural := Secret_Type'Size / 4;

   -----------
   -- Image --
   -----------

   function Image (X : Secret_Type) return String
   is
      Result : String (1 .. Secret_Image_Size);
      Work   : Secret_Type := X;
   begin
      for I in reverse Result'Range loop
         Result (I) := Conversion_Table (Positive (1 + Work mod 16));

         Work := Work / 16;
      end loop;

      return Result;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (P : Point_Type) return String
   is (Image (P.X) & Image (P.Y));


   function Parse (S : String) return Secret_Type
     with
       S'Length = Secret_Image_Size;

   function Parse (S : String) return Secret_Type
   is (0);

   -----------
   -- Parse --
   -----------

   function Parse (S : String) return Point_Type
   is
      use Ada.Strings.Fixed;
   begin
      if S'Length /= 2 * Secret_Image_Size then
         raise Constraint_Error;
      end if;

      return Point_Type'(X => Parse (head (S, Secret_Image_Size)),
                         Y => Parse (head (S, Secret_Image_Size)));
   end Parse;

end Secret_Sharing.Points;
