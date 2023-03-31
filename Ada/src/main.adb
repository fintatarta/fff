with Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals;

with Ada.Text_IO; use Ada.Text_IO;
with Generic_Polynomials;
with Generic_GCD;

procedure Main is
   use Ada.Numerics.Big_Numbers.Big_Reals;
   use Ada.Numerics.Big_Numbers;
   use type Ada.Numerics.Big_Numbers.Big_Integers.Big_Integer;

   One : constant Big_Real := To_Real (1);

   Zero : constant Big_Real := To_Real (0);

   function Inv (X : Big_Real) return Big_Real
   is (One / X);

   function To_Quotient (X : Big_Real) return String
   is (if Numerator (X) = 0 then
          "0"
       elsif Denominator (X) = 1 then
          Big_Integers.To_String (Numerator (X))
       else
          To_Quotient_String (X));

   function Image (X : Big_Real) return String
   is (To_Quotient (X));

   function Eq (X, Y : Float) return Boolean
   is
   begin
      return abs (X - Y) <= Float'Max (abs X, abs Y) * 1.0e-6;
   end Eq;
   pragma Unreferenced (Eq);

   package P_Float is
     new Generic_Polynomials (Field_Type => Big_Real,
                              Field_Zero => Zero,
                              Field_One  => One);

   use P_Float;

   function Image (X : Polynomial) return String
   is (Image (X, Image'Access));

   function Degree (P : Polynomial) return Integer
   is (if P.Is_Zero then -1 else To_Integer (P.Degree));

   package P_Gcd is
     new Generic_GCD (Euclidean_Ring => Polynomial,
                      Zero           => P_Float.Zero,
                      One            => P_Float.One,
                      "/"            => Div);

   P : constant Polynomial := To_Polynomial ((1.0, 2.0, 3.0));

   Q : constant Polynomial := To_Polynomial ((0.0, 1.0, -3.0, 4.2));

   A, B : Polynomial;

begin
   Put_Line (Image (P * P, Image'Access));
   Put_Line (Image (P + Q, Image'Access));

   Div_Mod (Num       => Q,
            Den       => P,
            Quotient  => A,
            Remainder => B);

   Put_Line ("Q=" & Image (A) & "; Rem=" & Image (B));
   Put_Line ("num=" & Image (Q) & "; den*q+rem = " & Image (P * A + B));
   Div_Mod (Num       => P,
            Den       => Q,
            Quotient  => A,
            Remainder => B);

   Put_Line ("Q=" & Image (A) & "; Rem=" & Image (B));


   Div_Mod (Num       => P,
            Den       => P,
            Quotient  => A,
            Remainder => B);

   Put_Line ("Q=" & Image (A) & "; Rem=" & Image (B));

   Put_Line (To_String (P (4.5)));

   Put_Line (Image (P_Gcd.GCD (A => To_Polynomial ((1.0, 2.0, 1.0)),
                               B => To_Polynomial ((1.0, 3.0, 2.0)))));

   Put_Line (Image (P_Gcd.Inv_Mod (x => To_Polynomial ((1.0, 2.0)),
                               modulus => To_Polynomial ((1.0, 2.0, 1.0)))));

end Main;
