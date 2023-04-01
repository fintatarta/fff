with Ada.Numerics.Big_Numbers.Big_Reals;
with Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Text_IO; use Ada.Text_IO;

with Generic_Matrices;
with Generic_Lup;

procedure Test_Matrices is
   use Ada.Numerics.Big_Numbers.Big_Reals;
   use Ada.Numerics.Big_Numbers.Big_Integers;

   One : constant Big_Real := 1.0;

   Zero : constant Big_Real := 0.0;

   function Inv (X : Big_Real) return Big_Real
   is (One / X);
   --
   --  function Is_Unit (X : Big_Real) return Boolean
   --  is (X /= Zero);

   package Mtx is
     new Generic_Matrices (Ring_Type => Big_Real,
                           Ring_Zero => Zero,
                           Ring_One  => One);

   use Mtx;

   package My_LUP is
     new Generic_LUP (Field_Type => Big_Real,
                      Zero       => Zero,
                      One        => One,
                      Matrices   => Mtx);

   use My_LUP;

   function Image (X : Big_Real) return String
   is (if Numerator (X) = 0 then
          "0"
       elsif Denominator (X) = 1 then
          To_String (Numerator (X))
       else
          To_Quotient_String (X));


   A       : constant Matrix := To_Matrix ((1 => (1.0, 2.0),
                                            2 => (3.0, 4.0)));
   B       : Matrix;
   U, L, P : Matrix;

   X       : constant Matrix := To_Matrix ((1 => (1.0, 2.0, 3.0),
                                            2 => (2.0, 3.5, 0.5),
                                            3 => (5.0, 1.0, 1.0)));
begin
   B := A * A - A;

   Register_Printer (Image'Access);

   Put_Line (To_String (Trace (A)));
   Put_Line (To_String (B, Image'Access));
   Put_Line (To_String (Flip_Lr (B), Image'Access));
   Put_Line (To_String (Flip_Ud (B), Image'Access));
   Put_Line (To_String (Transpose (B), Image'Access));
   Put_Line (To_String (Reverse_Identity (4), Image'Access));
   Put_Line (To_String (Identity (4, 5), Image'Access));
   Put_Line (To_String (Mtx.Zero (B)));

   LUP (X => x,
        L => L,
        U => U,
        P => P);

   Put_Line (To_String (L));
   Put_Line (To_String (U));
   Put_Line (To_String (P));

   Put_Line (Image (Determinant (X)));

   Put_Line (To_String(Inverse (X)));
end Test_Matrices;
