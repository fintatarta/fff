pragma Ada_2012;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body Generic_Polynomials is

   -------------------
   -- To_Polynomial --
   -------------------

   function To_Polynomial (C : Coefficient_Array) return Polynomial is
      use Ada.Containers;
   begin
      return Result : Polynomial do
         Result.Coeffs.Set_Length (Count_Type (C'Last + 1));

         for I in C'Range loop
            Result.Coeffs (I) := C (I);
         end loop;

         Normalize (Result);
      end return;
   end To_Polynomial;


   -----------
   -- Value --
   -----------

   function Value (P : Polynomial; N : Natural) return Field_Type
   is
   begin
      if N > P.Coeffs.Last_Index then
         return Field_Zero;
      else
         return P.Coeffs (N);
      end if;
   end Value;

   -----------
   -- Value --
   -----------

   function Value (P : Polynomial; X : Field_Type) return Field_Type
   is
      use Field_Vectors;
   begin
      return Accumulator : Field_Type := Field_Zero do

         for Pos in reverse P.Coeffs.Iterate loop
            Accumulator := Accumulator * X + Element (Pos);
         end loop;

      end return;
   end Value;

   ---------
   -- "+" --
   ---------

   function "+" (X, Y : Polynomial) return Polynomial is
   begin
      if X = Zero then
         return Y;

      elsif Y = Zero then
         return X;

      else
         declare
            Degree_Result : constant Natural :=
                              Natural'Max (Degree (X), Degree (Y));

            Result        : Polynomial;
         begin
            for I in 0 .. Degree_Result loop
               Result.Coeffs.Append (X (I) + Y (I));
            end loop;

            return Result;
         end;
      end if;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (X : Polynomial) return Polynomial is
   begin
      return Result : Polynomial do
         for Element of X.Coeffs loop
            Result.Coeffs.Append (-Element);
         end loop;
      end return;
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (X, Y : Polynomial) return Polynomial
   is (X + (-Y));

   function "*" (C : Field_Type; P : Polynomial) return Polynomial
   is
   begin
      return Result : Polynomial := P do
         for Pos in Result.Coeffs.Iterate loop
            Result.Coeffs (Pos) := C * Result.Coeffs (Pos);
         end loop;
      end return;
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (X, Y : Polynomial) return Polynomial is
   begin
      return Result : Polynomial := Zero do

         declare
            Working_Y : Polynomial := Y;
         begin
            for N in 0 .. X.Degree loop
               pragma Assert (Working_Y = Shift (Y, N));

               Result := Result + X (N) * Working_Y;

               Working_Y.Shift;
            end loop;
         end;

      end return;
   end "*";

   -------------
   -- Is_Unit --
   -------------

   function Is_Unit (X : Polynomial) return Boolean
   is (X.Degree = 0 and then X.Coeffs.First_Element /= Field_Zero);

   ---------
   -- Inv --
   ---------

   function Inv (X : Polynomial) return Polynomial is
   begin
      if not X.Is_Unit then
         raise Constraint_Error;
      else
         return To_Polynomial (Inv (X (0)));
      end if;
   end Inv;

   -----------
   -- "mod" --
   -----------

   function "mod" (X, Y : Polynomial) return Polynomial is
      Q, R : Polynomial;
   begin
      Div_Mod (num       => X,
               Den       => Y,
               Quotient  => Q,
               Remainder => R);

      return R;
   end "mod";

   ---------
   -- Div --
   ---------

   function Div (X, Y : Polynomial) return Polynomial is
      Q, R : Polynomial;
   begin
      Div_Mod (num       => X,
               Den       => Y,
               Quotient  => Q,
               Remainder => R);

      return Q;
   end Div;

   function Leading (X : Polynomial) return Field_Type
   is (X (X.Degree));

   function Monomial (C : Field_Type; Exponent : Natural := 1) return Polynomial
   is
      use Ada.Containers;
      use Field_Vectors;
   begin
      return Result : Polynomial :=
        (Coeffs => To_Vector (New_Item => Field_Zero,
                              Length   => Count_Type(Exponent + 1)))
      do
         Result.Coeffs (Exponent) := C;
      end return;
   end Monomial;
   -------------
   -- Div_Mod --
   -------------

   procedure Div_Mod
     (Num, Den : Polynomial; Quotient : out Polynomial; Remainder : out Polynomial)
   is
      Working_Num : Polynomial := Num;
      K           : constant Field_Type := Inv (Leading (Den));
      C           : Polynomial;
   begin
      Quotient := Zero;

      while Working_Num.Degree >= Quotient.Degree loop
         pragma Loop_Variant (Decreases => Working_Num.Degree);

         C := Monomial (Leading (Working_Num) * K, Working_Num.Degree - Den.Degree);

         Quotient := Quotient + C;

         Working_Num := Working_Num - C * Den;
      end loop;

      Remainder := Working_Num;
   end Div_Mod;

   -----------
   -- Shift --
   -----------

   function Shift (P : Polynomial; Amount : Natural := 1) return Polynomial
   is
   begin
      return Result : Polynomial := P do
         Result.Shift (Amount);
      end return;
   end Shift;

   -----------
   -- Shift --
   -----------

   procedure Shift (P : in out Polynomial; Amount : Natural := 1)
   is
      use Ada.Containers;

      use Field_Vectors;
   begin
      if Amount > 0 then
         P.Coeffs.Insert_Vector
           (Before   => P.Coeffs.First_Index,
            New_Item => To_Vector (Field_Zero, Count_Type (Amount)));
      end if;
   end Shift;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (X : Polynomial) return Polynomial
   is
   begin
      return Result : Polynomial := X do
         Normalize (Result);
      end return;
   end Normalize;

   ------------
   -- Degree --
   ------------

   function Degree (X : Polynomial) return Natural
   is (X.Normalize.Coeffs.Last_Index);

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize (X : in out Polynomial) is
   begin
      while X.Coeffs.Length > 1 and X.Coeffs.Last_Element = Field_Zero loop
         X.Coeffs.Delete_Last;
      end loop;
   end Normalize;

    function Image
     (X                 : Polynomial;
      Field_Image       : access function (X : Field_Type) return String;
      Var_Name          : Character := 'x';
      Exponent_Operator : Character := '^')
      return String
   is
      Accumulator : Unbounded_String := Null_Unbounded_String;
   begin
      for N in 0 .. X.Degree loop
         Accumulator := Accumulator & Field_Image (X (N));

         if N /= 0 then
            Accumulator := Accumulator & ' ' & Var_Name;
         end if;

         if N > 1 then
            Accumulator := Accumulator & Exponent_Operator & Integer'Image (N);
         end if;

         if N < X.Degree then
            Accumulator := Accumulator & "+";
         end if;
      end loop;

      return To_String (Accumulator);
   end Image;
end Generic_Polynomials;
