pragma Ada_2012;
package body Generic_LUP is

   function Transpose (X : Matrix) return Matrix
     with
       Post =>
         X'First (1) = Transpose'Result'First (2) and
         X'Last (1)  = Transpose'Result'Last (2) and
         X'First (2) = Transpose'Result'First (1) and
         X'Last (2)  = Transpose'Result'Last (1);

   function Transpose (X : Matrix) return Matrix
   is
      Result : Matrix (X'Range (2), X'Range (1));
   begin
      for Row in X'Range (1) loop
         for Col in X'Range (2) loop
            Result (Col, Row) := X (Row, Col);
         end loop;
      end loop;

      return Result;
   end Transpose;

   --------------------
   -- Is_Permutation --
   --------------------

   function Is_Permutation (X : Matrix) return Boolean is
      Found_In_Row : Boolean;
      Found        : array (X'Range (1)) of Boolean := (others => False);
   begin
      for Row in X'Range (1) loop
         Found_In_Row := False;

         for Col in X'Range (2) loop
            if X (Row, Col) = One then
               if Found_In_Row or Found (Col) then
                  return False;
               end if;

               Found_In_Row := True;
               Found (Col) := True;

            elsif X (Row, Col) /= Zero then
               return False;
            end if;
         end loop;
      end loop;

      return (for all I in Found'Range => Found (I));
   end Is_Permutation;

   -------------------------
   -- Is_Lower_Triangular --
   -------------------------

   function Is_Lower_Triangular (X : Matrix) return Boolean is
   begin
      return (for all Row in X'Range (1)  =>
                (for all Col in Index_Type'Succ (Row) .. X'Last (2) => X (Row, Col) = Zero));
   end Is_Lower_Triangular;

   -------------------------
   -- Is_Upper_Triangular --
   -------------------------

   function Is_Upper_Triangular (X : Matrix) return Boolean is
   begin
      return (for all Col in X'Range (2)  =>
                (for all Row in Index_Type'Succ (Col) .. X'Last (1) => X (Row, Col) = Zero));
   end Is_Upper_Triangular;

   -----------------------
   -- Has_Unit_Diagonal --
   -----------------------

   function Has_Unit_Diagonal (X : Matrix) return Boolean is
   begin
      if not Is_Square (X) then
         raise Constraint_Error;
      end if;

      return (for all Row in X'Range (1) => X (Row, Row) = One);
   end Has_Unit_Diagonal;

   ---------
   -- "*" --
   ---------

   function "*" (X, Y : Matrix) return Matrix is

      Accumulator : Field_Type;
   begin
      return Result : Matrix (X'Range (1), Y'Range (2)) do
         for Row in Result'Range (1) loop
            for Col in Result'Range (2) loop
               Accumulator := Zero;

               for Internal in X'Range (2) loop
                  Accumulator := Accumulator + X (Row, Internal) * Y (Internal, Col);
               end loop;

               Result (Row, Col) := Accumulator;
            end loop;
         end loop;
      end return;
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (X : Matrix; Y : Vector) return Vector is
      Accumulator : Field_Type;
   begin
      return Result : Vector (X'Range (1)) do
         for Row in Result'Range loop
            Accumulator := Zero;

            for Internal in X'Range (2) loop
               Accumulator := Accumulator + X (Row, Internal) * Y (Internal);
            end loop;

            Result (Row) := Accumulator;
         end loop;
      end return;
   end "*";

   ---------
   -- LUP --
   ---------

   procedure LUP (X : Matrix; L : out Matrix; U : out Matrix; P : out Matrix)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "LUP unimplemented");
      raise Program_Error with "Unimplemented procedure LUP";
   end LUP;

   -----------------
   -- Determinant --
   -----------------

   function Determinant (X : Matrix) return Field_Type is
      U : Matrix (X'Range (1), X'Range (2));
      L : Matrix (X'Range (1), X'Range (2));
      P : Matrix (X'Range (1), X'Range (2));

      Result : Field_Type;
   begin
      LUP (X => X,
           L => L,
           U => U,
           P => P);

      Result := One;

      for Row in U'Range (1) loop
         Result := Result * U (Row, Row);
      end loop;

      return Result;
   end Determinant;

   -------------------------
   -- Solve_Linear_System --
   -------------------------

   function Solve_Linear_System (A : Matrix; B : Vector) return Vector is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Solve_Linear_System unimplemented");
      return
      raise Program_Error with "Unimplemented function Solve_Linear_System";
   end Solve_Linear_System;

end Generic_LUP;
