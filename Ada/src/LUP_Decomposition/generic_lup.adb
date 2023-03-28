pragma Ada_2012;
with Generic_LUP.Actions;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package body Generic_LUP is



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
      package Acts is new Actions;
      use Acts;

      package Action_Lists is
        new Ada.Containers.Indefinite_Doubly_Linked_Lists (Action_Type);

      procedure Bring_Non_Zero_On_Diagonal
        (Object : in out Matrix;
         Col    : Index_Type;
         Applied_Actions : in out Action_Lists.List)
      is
      begin
         for Row in Col .. Object'Last (1) loop
            if Object (Row, Col) /= Zero then
               declare
                  Action : constant Acts.Action_Type :=
                             Acts.Swap_Rows (R   => Row,
                                             S   => Col,
                                             Rng => Row_Range (Object));
               begin
                  Object := Action * Object;
                  Applied_Actions.Append (Action);
                  exit;
               end;
            end if;
         end loop;
      end Bring_Non_Zero_On_Diagonal;

      function Lower_Half (Object : Matrix;
                           Col    : Index_Type)
                           return Vector
      is

      begin
         return Result : Vector (Index_Type'Succ (Col) .. Object'Last (1)) do
            for Row in Result'Range loop
               Result (Row) := Object (Row, Col);
            end loop;
         end return;
      end Lower_Half;


      function "*" (X : Field_Type; V : Vector) return Vector
      is
      begin
         return Result : Vector (V'Range) do
            for I in Result'Range loop
               Result (I) := X * V (I);
            end loop;
         end return;
      end "*";


      Applied_Actions : Action_Lists.List;

      Rng : constant Index_Range := Row_Range (X);
   begin
      if X'Length (1) = 0 then
         -- This makes no sense
         raise Constraint_Error;
      end if;

      if X'Length (1) = 1 then
         -- Handle separately this simple and degenerate case
         U := X;
         L (L'First (1), L'First (2)) := One;
         P := L;
      end if;

      declare
         Xx : Matrix := X;
      begin
         for Col in XX'First (2) .. Index_Type'Pred (XX'Last (2)) loop
            if XX (Col, Col) = Zero then
               Bring_Non_Zero_On_Diagonal (XX, Col, Applied_Actions);

               if XX (Col, Col) = Zero then
                  raise Singular_Matrix;
               end if;
            end if;

            pragma Assert (XX (Col, Col) /= Zero);

            declare
               Coeff : constant Vector :=
                         (-Inv (XX (Col, Col))) * Lower_Half (XX, Col);

               Action : constant Acts.Action_Type :=
                         Acts.Add_Rows (Src   => Col,
                                        Coeff => Coeff,
                                        Rng   => Rng);
            begin
               Xx := Action * Xx;
               Applied_Actions.Append (Action);
            end;

         end loop;
      end;
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
