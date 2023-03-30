pragma Ada_2012;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
package body Generic_Polynomials is

   function Constant_Part (X : Polynomial) return Field_Type
   is (X (0));

   function "<" (X, Y : Polynomial_Degree) return Boolean
   is (if X = Minus_Infinity then
          y /= Minus_Infinity

       elsif Y = Minus_Infinity then
          False

       else
          Integer (X) < Integer (Y));

   function "+" (X, Y : Polynomial_Degree) return Polynomial_Degree
   is (if X = Minus_Infinity or Y = Minus_Infinity then
          Minus_Infinity
       else
          Polynomial_Degree(natural(x)+Natural(y)));

   function To_Degree (D : Natural) return Polynomial_Degree
   is (Polynomial_Degree (D));

   function To_Integer (D : Polynomial_Degree) return Natural
   is (if D = Minus_Infinity then
          raise Constraint_Error
       else
          Natural (D));

   function Is_Zero (X : Polynomial) return Boolean
   is (X.Is_A_Constant and then X.Coeffs.First_Element = Field_Zero);
   -------------------
   -- To_Polynomial --
   -------------------

   function To_Polynomial (C : Coefficient_Array) return Polynomial is
      use Ada.Containers;
   begin
      return Result : Polynomial do
         Result.Coeffs.Set_Length (Count_Type (C'Last + 1));

         for I in C'Range loop
            Result.Coeffs (Exponent_Type (I)) := C (I);
         end loop;

         Normalize (Result);
      end return;
   end To_Polynomial;


   -----------
   -- Value --
   -----------

   function Value (P : Polynomial; N : Exponent_Type) return Field_Type
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
      if X.Is_Zero then
         return Y;

      elsif Y.Is_Zero then
         return X;

      else
         declare
            use Field_Vectors;
            use Ada.Containers;

            Max_Power : constant Exponent_Type :=
                          To_Exponent (Max (X.Degree, Y.Degree));

            Result        : Polynomial :=
                              (Coeffs => To_Vector (New_Item => Field_Zero,
                                                    Length   => Count_Type (Max_Power + 1)));
         begin
            pragma Assert (Max_Power >= 0);

            for I in 0 .. Max_Power loop
               Result.Coeffs (I) := X (I) + Y (I);

               --  Put_Line ("$$$" & Result.Leading_Power'image);
            end loop;

            Normalize (Result);
            --  Put_Line ("$$$o" & Result.Leading_Power'Image);
            return Result;
         end;
      end if;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (X : Polynomial) return Polynomial is
   begin
      return Result : Polynomial := X do
         for I in 0 .. Result.Leading_Power loop
            Result.Coeffs (I) := -X (I);
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

         Normalize (Result);
      end return;
   end "*";

   ---------
   -- "*" --
   ---------

   ---------
   -- "*" --
   ---------

   function "*" (X, Y : Polynomial) return Polynomial is
   begin
      if X.Is_Zero or Y.Is_Zero then
         return Zero;
      end if;

      return Result : Polynomial := Zero do

         declare
            Working_Y : Polynomial := Y;
         begin
            for N in 0 .. To_Exponent (X.Degree) loop
               pragma Assert (Working_Y = Shift (Y, Natural (N)));

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
   is (X.Is_A_Constant and then x.Coeffs.First_Element /= Field_Zero);

   ---------
   -- Inv --
   ---------

   function Inv (X : Polynomial) return Polynomial is
   begin
      if not X.Is_Unit then
         raise Constraint_Error;
      else
         return To_Polynomial (Inv (X.Coeffs.First_Element));
      end if;
   end Inv;

   -----------
   -- "mod" --
   -----------

   function "mod" (X, Y : Polynomial) return Polynomial is
      Q, R : Polynomial;
   begin
      Div_Mod (Num       => X,
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
      Div_Mod (Num       => X,
               Den       => Y,
               Quotient  => Q,
               Remainder => R);

      return Q;
   end Div;

   function Leading (X : Polynomial) return Field_Type
   is (X.Coeffs.Last_Element);

   function Monomial (C        : Field_Type;
                      Exponent : Exponent_Type)
                      return Polynomial
   is
      use Ada.Containers;
      use Field_Vectors;
   begin
      return Result : Polynomial :=
        (Coeffs => To_Vector (New_Item => Field_Zero,
                              Length   => Count_Type (Exponent + 1)))
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

      while Working_Num.Degree >= Den.Degree loop
         Put_Line (Working_Num.Leading_Power'Image);
         pragma Loop_Variant (Decreases => Working_Num.Leading_Power);

         C := Monomial (Leading (Working_Num) * K,
                        Working_Num.Leading_Power - Den.Leading_Power);

         pragma Assert (Degree (C * Den) = Working_Num.Degree);

         Quotient := Quotient + C;

         Working_Num := Working_Num - C * Den;
         Put_Line (">>>>" & Working_Num.Leading_Power'Image);

      end loop;

      Put_Line ("%" & Working_Num.Leading_Power'Image & Den.Degree'Image);

      Remainder := Working_Num;
   end Div_Mod;


   function "=" (X, Y : Polynomial) return Boolean
   is
   begin
      return X.Leading_Power = Y.Leading_Power
        and then (for all I in 0 .. X.Leading_Power => X (I) = Y (I));
   end "=";
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

   function Degree (X : Polynomial) return Polynomial_Degree
   is (if X.Coeffs.Last_Index = 0 and then X (0) = Field_Zero then
          Minus_Infinity
       else
          To_Degree (Integer (X.Coeffs.Last_Index)));

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize (X : in out Polynomial) is
   begin
      while X.Coeffs.Length > 1 and X.Coeffs.Last_Element = Field_Zero loop
         X.Coeffs.Delete_Last;
      end loop;
   end Normalize;


   function Is_A_Constant (X : Polynomial) return Boolean
   is (X.Coeffs.Length = 1);

   function Image
     (X                 : Polynomial;
      Field_Image       : access function (X : Field_Type) return String;
      Var_Name          : Character := 'x';
      Exponent_Operator : Character := '^')
      return String
   is
      Accumulator : Unbounded_String := Null_Unbounded_String;
   begin
      if X.Is_A_Constant then
         return Field_Image (X (0));
      end if;

      for N in 0 .. To_Exponent (X.Degree) loop
         if X (N) /= Field_Zero then
            if Accumulator /= Null_Unbounded_String then
               Accumulator := Accumulator & "+";
            end if;

            if N = 0 or X (N) /= Field_One then
               Accumulator := Accumulator & Field_Image (X (N));
            end if;

            if N /= 0 then
               Accumulator := Accumulator & ' ' & Var_Name;
            end if;

            if N > 1 then
               Accumulator := Accumulator & Exponent_Operator & Exponent_Type'Image (N);
            end if;
         end if;
      end loop;

      return To_String (Accumulator);
   end Image;
end Generic_Polynomials;
