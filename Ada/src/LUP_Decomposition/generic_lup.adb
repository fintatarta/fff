pragma Ada_2012;
with Generic_LUP.Actions;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package body Generic_LUP is

   ---------------
   -- Row_Range --
   ---------------

   function Row_Range (X : Matrix) return Index_Range
   is (Index_Range'(First => X'First (1),
                    Last  => X'Last (1)));


   function First (X : Index_Range) return Index_Type
   is (X.First);

   function Last  (X : Index_Range) return Index_Type
   is (X.Last);

   -----------
   -- Is_In --
   -----------

   function Is_In (X : Index_Type; R : Index_Range) return Boolean
   is (X in R.First .. R.Last);


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

   --------------
   -- Identity --
   --------------

   function Identity (Rng : Index_Range) return Matrix
   is
   begin
      return I : Matrix (Rng.First .. Rng.Last, Rng.First .. Rng.Last) :=
        (others => (others => Zero)) do

         for Row in I'Range (1) loop
            I (Row, Row) := One;
         end loop;
      end return;
   end Identity;


   --------------
   -- Identity --
   --------------

   function Identity (Template : Matrix) return Matrix
   is (Identity (Row_Range (Template)));

   ----------------------
   -- Reverse_Identity --
   ----------------------

   function Reverse_Identity (Template : Matrix) return Matrix
   is
      Rng : constant Index_Range := Row_Range (Template);
      Cursor : Index_Type;
   begin
      return I : Matrix (Rng.First .. Rng.Last, Rng.First .. Rng.Last) :=
        (others => (others => Zero)) do

         Cursor := Rng.Last;

         for Row in I'Range (1) loop
            I (Row, Cursor) := One;

            if Cursor /= Index_Type'First then
               Cursor := Index_Type'Pred (Cursor);
            end if;
         end loop;
      end return;
   end Reverse_Identity;


   ---------
   -- LUP --
   ---------

   procedure LUP (X : Matrix; L : out Matrix; U : out Matrix; P : out Matrix)
   is
      package Acts is new Actions;
      use Acts;

      package Action_Lists is
        new Ada.Containers.Indefinite_Doubly_Linked_Lists (Action_Type);




      function "*" (X : Field_Type; V : Vector) return Vector
      is
      begin
         return Result : Vector (V'Range) do
            for I in Result'Range loop
               Result (I) := X * V (I);
            end loop;
         end return;
      end "*";

      procedure To_Upper_Triangular (U               : in out Matrix;
                                     Applied_Actions : in out Action_Lists.List)
        with
          Post => Is_Upper_Triangular (U);

      procedure To_Upper_Triangular (U               : in out Matrix;
                                     Applied_Actions : in out Action_Lists.List)
      is
         --------------------------------
         -- Bring_Non_Zero_On_Diagonal --
         --------------------------------

         procedure Bring_Non_Zero_On_Diagonal
           (Object          : in out Matrix;
            Col             : Index_Type;
            Applied_Actions : in out Action_Lists.List)
         is
         begin
            if Object (Col, Col) /= Zero then
               return;
            end if;

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

         -----------------------
         -- Lower_Half_Column --
         -----------------------

         function Lower_Half_Column (Object : Matrix;
                                     Col    : Index_Type)
                                     return Vector
         is

         begin
            return Result : Vector (Index_Type'Succ (Col) .. Object'Last (1)) do
               for Row in Result'Range loop
                  Result (Row) := Object (Row, Col);
               end loop;
            end return;
         end Lower_Half_Column;

      begin
         for Col in U'First (2) .. Index_Type'Pred (U'Last (2)) loop
            Bring_Non_Zero_On_Diagonal (U, Col, Applied_Actions);

            if U (Col, Col) = Zero then
               raise Singular_Matrix;
            end if;

            pragma Assert (U (Col, Col) /= Zero);

            declare
               Q : constant Field_Type := -Inv (U (Col, Col));

               Coeff : constant Vector := Q * Lower_Half_Column (U, Col);

               Action : constant Acts.Action_Type :=
                          Acts.Add_Rows (Src   => Col,
                                         Coeff => Coeff,
                                         Rng   => Row_Range (U));
            begin
               U := Action * U;
               Applied_Actions.Append (Action);
            end;

         end loop;
      end To_Upper_Triangular;

      procedure De_Intertwine_Actions
        (L               : out Matrix;
         P               : out Matrix;
         Applied_Actions : in out Action_Lists.List;
         Rng             : Index_Range)
        with
          Post =>
            Row_Range (L) = Rng and
            Row_Range (P) = Rng and
            Is_Lower_Triangular (L) and
            Has_Unit_Diagonal (L) and
            Is_Permutation (P) and
            Applied_Actions.Is_Empty;

      procedure De_Intertwine_Actions
        (L               : out Matrix;
         P               : out Matrix;
         Applied_Actions : in out Action_Lists.List;
         Rng             : Index_Range)
      is
         use Acts;
      begin
         L := Identity (Rng);
         P := Identity (Rng);

         while not Applied_Actions.Is_Empty loop
            pragma Loop_Variant (Decreases => Applied_Actions.Length);

            declare
               Action : constant Acts.Action_Type := Applied_Actions.Last_Element;
            begin
               Applied_Actions.Delete_Last;

               if not Is_Permutation (Action) then
                  L := Inv (Action) * L;

               else
                  if Applied_Actions.Is_Empty then
                     P := Acts.To_Matrix (Action);
                     return;

                  else
                     declare
                        Next_Action : constant Acts.Action_Type :=
                                        Applied_Actions.Last_Element;
                     begin
                        if Acts.Is_Permutation (Next_Action) then
                           Applied_Actions.Append (Action * Next_Action);
                        else
                           L := Inv (Commute (Next_Action, Action)) * L;
                           Applied_Actions.Append (Action);
                        end if;
                     end;
                  end if;
               end if;
            end;

            raise Program_Error;
         end loop;
      end De_Intertwine_Actions;

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
         L := Identity (X);
         P := Identity (X);

         return;
      end if;

      U := X;

      To_Upper_Triangular (U, Applied_Actions);

      De_Intertwine_Actions (L, P, Applied_Actions, Row_Range (X));
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

   ------------
   -- Column --
   ------------

   function Column (A : Matrix; Col : Index_Type) return Vector
   is
   begin

      return Result : Vector (A'Range (2)) do
         for Row in Result'Range loop
            Result (Row) := A (Row, Col);
         end loop;
      end return;

   end Column;

   ---------
   -- "*" --
   ---------

   function "*" (C : Field_Type; V : Vector) return Vector
   is
   begin

      return Result : Vector (V'Range) do
         for Row in Result'Range loop
            Result (Row) := C * V (Row);
         end loop;
      end return;
   end "*";

   ---------
   -- "-" --
   ---------
   function "-" (X, Y : Vector) return Vector
     with
       Pre => Have_Equal_Size (X, Y);

   function "-" (X, Y : Vector) return Vector
   is
   begin
      if not Have_Equal_Size (X, Y) then
         raise Constraint_Error;
      end if;

      return Result : Vector (X'Range) do
         for Row in Result'Range loop
            Result (Row) := X (Row)-Y (Row);
         end loop;
      end return;

   end "-";

   ---------------------------
   -- Solve_Upper_Triangual --
   ---------------------------

   function Solve_Upper_Triangual (U : Matrix; B : Vector) return Vector
   is
      X         : Vector (B'Range);
      Working_B : Vector := B;
   begin
      pragma Assert (Is_Upper_Triangular (U));

      for I in reverse X'Range loop
         if U (I, I) = Zero then
            raise Singular_Matrix;
         end if;

         X (I) := Working_B (I) / U (I, I);

         Working_B := Working_B - X (I) * Column (U, I);
      end loop;

      return X;
   end Solve_Upper_Triangual;

   -------------------------
   -- Solve_Linear_System --
   -------------------------

   -------------------------
   -- Solve_Linear_System --
   -------------------------

   function Solve_Linear_System (A : Matrix; B : Vector) return Vector is
      U : Matrix (A'Range (1), A'Range (2));
      L : Matrix (A'Range (1), A'Range (2));
      P : Matrix (A'Range (1), A'Range (2));
   begin
      --
      --  We want to solve A*x = b.  We decompose A as P*A = L*U and the
      --  system becomes
      --
      --       P*b = c = P*A*x = L*U*x
      --
      --  Write L = J*V*J, with V upper triangualr, and rewrite the system as
      --
      --       J*P*b = V*J*U*x = V*y
      --       y = J*U*x  <==> J*y = U*x
      --
      --  Therefore, the solution can be written as
      --
      --      y = Inv(V)*(J*P*b),
      --      x = Inv(U)*(J*y)
      --
      -- where Inv(V)*(J*P*b) and Inv(U)*(J*y) can be computed using
      -- Solve_Upper_Trianuglar
      --
      LUP (X => A,
           L => L,
           U => U,
           P => P);

      declare
         J : constant Matrix := Reverse_Identity (U);
         Y : constant Vector := Solve_Upper_Triangual (J * L * J, J * P * B);
      begin
         return Solve_Upper_Triangual (U, J * Y);
      end;
   end Solve_Linear_System;


end Generic_LUP;
