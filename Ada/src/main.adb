with Ada.Text_IO; use Ada.Text_IO;
with Generic_Polynomials;

procedure Main is
   function Inv (X : Float) return Float
   is (1.0 / X);

   function Image (X : Float) return String
   is (Float'Image (X));

   function Eq (X, Y : Float) return Boolean
   is
   begin
      --  Put_Line (X'Image & "=" & Y'Image & "dif=" & Float'Image (abs (X - Y)));

      return abs (X - Y) <= Float'Max (abs X, abs Y) * 1.0e-6;
   end Eq;

   package P_Float is
     new Generic_Polynomials (Field_Type => Float,
                              Field_Zero => 0.0,
                              Field_One  => 1.0,
                              "="        => Eq);

   use P_Float;

   function Image (X : Polynomial) return String
   is (Image (X, Image'Access));

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
end Main;
