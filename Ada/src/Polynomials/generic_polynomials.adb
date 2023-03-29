pragma Ada_2012;
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

   -----------------
   -- Coefficient --
   -----------------

   function Coefficient (P : Polynomial; N : Natural) return Field_Type
   is
   begin
      if N > P.Coeffs.Last_Index then
         return Field_Zero;
      else
         return P.Coeffs (N);
      end if;
   end Coefficient;

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

   ---------
   -- "*" --
   ---------

   function "*" (X, Y : Polynomial) return Polynomial is
      Degree_Result : constant Natural := X.Degree + Y.Degree;
      Accumulator   : Field_Type;
   begin
      return Result : Polynomial do

         for N in 0 .. Degree_Result loop
            Accumulator := Field_Zero;

            for K in 0 .. N loop
               Accumulator := Accumulator + X (K) * Y (N - K);
            end loop;

            Result.Coeffs.Append (Accumulator);
         end loop;

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
      Div_Mod (X         => X,
               Y         => Y,
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
      Div_Mod (X         => X,
               Y         => Y,
               Quotient  => Q,
               Remainder => R);

      return Q;
   end Div;

   -------------
   -- Div_Mod --
   -------------

   procedure Div_Mod
     (X, Y : Polynomial; Quotient : out Polynomial; Remainder : out Polynomial)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Div_Mod unimplemented");
      raise Program_Error with "Unimplemented procedure Div_Mod";
   end Div_Mod;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (X : Polynomial) return Polynomial
   is
   begin
      return Result : Polynomial := X do
         Normalize(Result);
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

end Generic_Polynomials;
