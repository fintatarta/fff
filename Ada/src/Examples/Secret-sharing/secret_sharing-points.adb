pragma Ada_2012;

pragma Warnings (Off, "no entities of ""Ada.Text_IO"" are referenced");
with Ada.Text_IO;          use Ada.Text_IO;

with Ada.Strings.Fixed;    use ada.Strings;
with Ada.Strings.Maps;

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
         Result (I) := Conversion_Table (Positive (1 + (Work mod 16)));

         Work := Work / 16;
      end loop;

      --  Put_Line (Result & ":" & X'Image);

      return Result;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (P : Point_Type) return String
   is (Trim (Image (P.X) & Image (P.Y)));

   --     T : constant String := Image (P.X) & Image (P.Y);
   --     Q : constant Point_Type := Parse (T);
   --  begin
   --     Put_Line ("[" & T & "]" & T'Length'Image & Secret_Image_Size'Image);
   --     Put_Line (Q.X'Image & Q.Y'Image);
   --     Put_Line (p.X'Image & p.Y'Image);
   --
   --
   --     return Image (P.X) & Image (P.Y);
   --  end Image;


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
      Full : constant String := Expand (S);
   begin
      pragma Assert (Is_Full_Image (Full));

      return Point_Type'(X => Parse (Head (Full, Secret_Image_Size)),
                         Y => Parse (Tail (Full, Secret_Image_Size)));
   end Parse;

   function Trim (S : String) return String
   is (Fixed.Trim (Source => S,
                   Left   => Maps.To_Set ('0'),
                   Right  => Maps.Null_Set));

   function Expand (S : String) return String
   is
      use Ada.Strings.Fixed;

      Padding : constant String := (2 * Secret_Image_Size - S'Length) * '0';
   begin
      return Padding & S;
   end Expand;
end Secret_Sharing.Points;
