pragma Ada_2012;
with Ada.Strings.Fixed;

package body Secret_Sharing.Points is
   Conversion_Table : constant String := "0123456789abcdef";

   function Image (X : Secret_Type) return String
     with
       Post =>
         Image'Result'Length = Secret_Image_Size and
         Is_Hexadecimal_String (Image'Result);


   function Parse (S : String) return Secret_Type
     with
       Pre =>
         S'Length = Secret_Image_Size and
         Is_Hexadecimal_String (S);

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
   is
      pragma Warnings (Off, "lower bound test optimized away");

      function To_Int (C : Character) return Secret_Type
        with
          Pre => Characters.Handling.Is_Hexadecimal_Digit (C),
          Post => To_Int'Result in 0 .. 15;


      function To_Int (C : Character) return Secret_Type
      is (case C is
             when '0' .. '9' =>
                Secret_Type (Character'Pos (C)-Character'Pos ('0')),

             when 'a' .. 'f' =>
                Secret_Type (Character'Pos (C)-Character'Pos ('a')+10),

             when 'A' .. 'F' =>
                Secret_Type (Character'Pos (C)-Character'Pos ('A')+10),

             when others     =>
                raise Constraint_Error);

   begin
      return Result : Secret_Type := 0 do

         for I in S'Range loop
            Result := 16 * Result + To_Int (S (I));
         end loop;

      end return;
   end Parse;

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

      return Point_Type'(X => Parse (Head (S, Secret_Image_Size)),
                         Y => Parse (Head (S, Secret_Image_Size)));
   end Parse;

end Secret_Sharing.Points;
