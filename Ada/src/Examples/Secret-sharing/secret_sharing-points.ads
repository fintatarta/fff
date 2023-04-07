with Ada.Characters.Handling;
with Ada.Containers.Vectors;


use Ada;
package Secret_Sharing.Points is
   type Point_Type is
      record
         X, Y : Secret_Type;
      end record;

   package Point_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Point_Type);

   subtype Point_Array is Point_Vectors.Vector;

   function Is_Valid_Image (S : String) return Boolean;

   function Image (P : Point_Type) return String
     with
       Post =>
         Is_Valid_Image (Image'Result) and
         Parse (Image'Result) = P;

   function Parse (S : String) return Point_Type
     with
       Pre => Is_Valid_Image (S);
private
   Secret_Image_Size : constant Natural := Secret_Type'Size / 4;

   function Is_Hexadecimal_String (S : String) return Boolean
   is (for all C of S => Characters.Handling.Is_Hexadecimal_Digit (C));

   function Is_Valid_Image (S : String) return Boolean
   is (S'Length = 2 * Secret_Image_Size and Is_Hexadecimal_String (S));
end Secret_Sharing.Points;