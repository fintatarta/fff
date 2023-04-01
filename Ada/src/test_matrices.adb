with Ada.Numerics.Big_Numbers.Big_Reals;
with Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Text_IO; use Ada.Text_IO;

with Generic_Matrices;
with Generic_Lup;
with Modular_Polynomials;

with Ring_Mod_N;
with Generic_Polynomials;

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


   package My_LUP is
     new Generic_LUP (Field_Type => Big_Real,
                      Zero       => Zero,
                      One        => One,
                      Matrices   => Mtx);

   use Mtx, My_LUP;

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

   C       : constant Matrix := To_Matrix ((1 => (1 => 1.0),
                                            2 => (1 => 1.0),
                                            3 => (1 => 1.0)));
   type Zorro is mod 17;

   package R is new Ring_Mod_N (Zorro);
   use R;


   package Mtx_Mod is
     new Generic_Matrices (Ring_Type => Zorro,
                           Ring_Zero => 0,
                           Ring_One  => 1);

   package Lud_Mod is
     new Generic_LUP (Field_Type => Zorro,
                      Zero       => 0,
                      One        => 1,
                      Matrices   => Mtx_Mod);

   package Poly_R is
     new Generic_Polynomials (Field_Type => Zorro,
                              Field_Zero => 0,
                              Field_One  => 1,
                              Inv        => R.Inv);
   package Mod_Poly is
     new Modular_Polynomials (Field_Type => Zorro,
                              Field_Zero => 0,
                              Field_One  => 1,
                              Inv        => R.Inv,
                              Poly       => Poly_R,
                              Modulus    => Poly_R.To_Polynomial ((1, 2, 3)));
   use Mod_Poly;

   package Mtx_Poly_Mod is
     new Generic_Matrices (Ring_Type => Polynomial_Mod,
                           Ring_Zero => Mod_Poly.Zero,
                           Ring_One  => Mod_Poly.One);

   G       : constant Zorro := 4;
   W       : Zorro;

   Q : constant Mtx_Mod.Matrix := Mtx_Mod.To_Matrix ((1 => (1, 2, 3),
                                                      2 => (3, 7, 9),
                                                      3 => (4, 4, 6)));

   Qu, Ql, Qp : Mtx_Mod.Matrix;

   function Image (X : Zorro) return String
   is (X'Image);
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

   LUP (X => X,
        L => L,
        U => U,
        P => P);

   Put_Line (To_String (L));
   Put_Line (To_String (U));
   Put_Line (To_String (P));

   Put_Line (Image (Determinant (X)));

   Put_Line (To_String (Solve_Linear_System (X, C)));
   Put_Line (To_String (To_Real (2) * (Determinant (X) * Inverse (X))));

   W := 3 / G;

   Put_Line (W'Image & ", " & Zorro'Image (W * G) & "=3?");

   Mtx_Mod.Register_Printer (Image'Access);

   Lud_Mod.LUP (X => Q,
                L => Ql,
                U => Qu,
                P => Qp);
   Put_Line (Mtx_Mod.To_String (Ql));
   Put_Line (Mtx_Mod.To_String (Qu));
   Put_Line (Mtx_Mod.To_String (Qp));

   Put_Line (Mtx_Mod.To_String (Lud_Mod.Inverse (Q)));
end Test_Matrices;
