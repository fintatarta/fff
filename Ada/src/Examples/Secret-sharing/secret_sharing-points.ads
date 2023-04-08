with Ada.Characters.Handling;
with Ada.Containers.Vectors;

--
--  A "point" is a point on the polynomial curve used in the Shamir
--  algorithm.  Nothing special here, a point is just a pair of coordinates.
--  This package is here mostly to define the type and provide some
--  conversion point <--> text representation
--

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
   is (S'Length <= 2 * Secret_Image_Size and Is_Hexadecimal_String (S));

   function Is_Full_Image (S : String) return Boolean
   is (S'Length = 2 * Secret_Image_Size and Is_Hexadecimal_String (S));

   function Trim (S : String) return String
     with
       Pre => Is_Full_Image (S);

   function Expand (S : String) return String
     with
       Post =>
         Is_Full_Image (Expand'Result) and
         (if Is_Full_Image (S) then Expand'Result = S);

end Secret_Sharing.Points;
