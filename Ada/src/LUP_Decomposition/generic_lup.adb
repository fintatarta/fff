pragma Ada_2012;
with Generic_LUP.Actions;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package body Generic_LUP is

   ---------------
   -- Row_Range --
   ---------------

   function Row_Range (X : Matrix) return Index_Range
   is (Index_Range'(First => 1,
                    Last  => X.N_Rows));


   function First (X : Index_Range) return Index_Type
   is (X.First);

   function Last  (X : Index_Range) return Index_Type
   is (X.Last);

   -----------
   -- Is_In --
   -----------

   function Is_In (X : Index_Type; R : Index_Range) return Boolean
   is (X in R.First .. R.Last);



   --------------------
   -- Is_Permutation --
   --------------------

   function Is_Permutation (X : Matrix) return Boolean
   is
      Found_In_Row : Boolean;
   begin
      if not Is_Square (X) then
         return False;
      end if;

      declare
         Found        : array (1 .. X.N_Rows) of Boolean := (others => False);
      begin
         for Row in 1 .. X.N_Rows loop
            Found_In_Row := False;

            for Col in 1 .. X.N_Cols loop
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
      end;
   end Is_Permutation;

   -------------------------
   -- Is_Lower_Triangular --
   -------------------------

   function Is_Lower_Triangular (X : Matrix) return Boolean is
   begin
      return (for all Row in 1 .. X.N_Rows  =>
                (for all Col in Row + 1 .. X.N_Cols => X (Row, Col) = Zero));
   end Is_Lower_Triangular;

   -------------------------
   -- Is_Upper_Triangular --
   -------------------------

   function Is_Upper_Triangular (X : Matrix) return Boolean is
   begin
      return (for all Col in 1 .. X.N_Cols  =>
                (for all Row in Col + 1 .. X.N_Rows =>
                   X (Row, Col) = Zero));
   end Is_Upper_Triangular;

   -----------------------
   -- Has_Unit_Diagonal --
   -----------------------

   function Has_Unit_Diagonal (X : Matrix) return Boolean is
   begin
      if not Is_Square (X) then
         raise Constraint_Error;
      end if;

      return (for all Row in 1 .. X.N_Rows => X (Row, Row) = One);
   end Has_Unit_Diagonal;


   ----------------------
   -- Reverse_Identity --
   ----------------------


   ---------
   -- LUP --
   ---------

   procedure LUP (X : Matrix; L : out Matrix; U : out Matrix; P : out Matrix)
   is
      package Acts is new Actions;
      use Acts;

      package Action_Lists is
        new Ada.Containers.Indefinite_Doubly_Linked_Lists (Action_Type);




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

            for Row in Col .. Object.N_Rows loop
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

         function Lower_Half_Column (Object : Matrix;
                                     Col    : Index_Type)
                                     return Matrix
           with
             Pre => Col < Object.N_Cols,
             Post =>
               Lower_Half_Column'Result.Is_Vector and
               Lower_Half_Column'Result.Length = Object.N_Rows - Col;

         -----------------------
         -- Lower_Half_Column --
         -----------------------

         function Lower_Half_Column (Object : Matrix;
                                     Col    : Index_Type)
                                     return Matrix
         is

         begin
            return Result : Matrices.Matrix := Matrices.Zero (Object.N_Rows - Col, 1) do
               for Row in 1 .. Result.N_Rows loop
                  Result (Row) := Object (Row + Col, Col);
               end loop;
            end return;
         end Lower_Half_Column;

      begin
         for Col in 1 .. U.N_Cols - 1 loop
            Bring_Non_Zero_On_Diagonal (U, Col, Applied_Actions);

            if U (Col, Col) = Zero then
               raise Singular_Matrix;
            end if;

            pragma Assert (U (Col, Col) /= Zero);

            declare
               Q : constant Field_Type := -Inv (U (Col, Col));

               Coeff : constant Matrix := Q * Lower_Half_Column (U, Col);

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
         U               : Matrix)
        with
          Post =>
            Have_Equal_Size (L, U)  and
            Have_Equal_Size (P, U)  and
            Is_Lower_Triangular (L) and
            Has_Unit_Diagonal (L) and
            Is_Permutation (P) and
            Applied_Actions.Is_Empty;

      procedure De_Intertwine_Actions
        (L               : out Matrix;
         P               : out Matrix;
         Applied_Actions : in out Action_Lists.List;
         U               : Matrix)
      is
      begin
         L := Identity (U);
         P := Identity (U);

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
         end loop;
      end De_Intertwine_Actions;

      Applied_Actions : Action_Lists.List;

   begin
      if not Is_Square (X) then
         raise Constraint_Error;
      end if;

      if X.N_Rows = 1 then
         -- Handle separately this simple and degenerate case
         U := X;
         L := Identity (X);
         P := Identity (X);

         return;
      end if;

      U := X;

      To_Upper_Triangular (U, Applied_Actions);

      De_Intertwine_Actions (L, P, Applied_Actions, X);
   end LUP;

   -----------------
   -- Determinant --
   -----------------

   function Determinant (X : Matrix) return Field_Type is
      U : Matrix;
      L : Matrix;
      P : Matrix;

      Result : Field_Type;
   begin
      LUP (X => X,
           L => L,
           U => U,
           P => P);

      Result := One;

      for Row in 1 .. U.N_Rows loop
         Result := Result * U (Row, Row);
      end loop;

      return Result;
   end Determinant;
   --
   --  ------------
   --  -- Column --
   --  ------------
   --
   --  function Column (A : Matrix; Col : Index_Type) return Vector
   --  is
   --  begin
   --
   --     return Result : Matrix := Matrices.zero do
   --        for Row in Result'Range loop
   --           Result (Row) := A (Row, Col);
   --        end loop;
   --     end return;
   --
   --  end Column;
   --
   --  ---------
   --  -- "*" --
   --  ---------
   --
   --  function "*" (C : Field_Type; V : Vector) return Vector
   --  is
   --  begin
   --
   --     return Result : Vector (V'Range) do
   --        for Row in Result'Range loop
   --           Result (Row) := C * V (Row);
   --        end loop;
   --     end return;
   --  end "*";
   --
   --  ---------
   --  -- "-" --
   --  ---------
   --  function "-" (X, Y : Vector) return Vector
   --    with
   --      Pre => Have_Equal_Size (X, Y);
   --
   --  function "-" (X, Y : Vector) return Vector
   --  is
   --  begin
   --     if not Have_Equal_Size (X, Y) then
   --        raise Constraint_Error;
   --     end if;
   --
   --     return Result : Vector (X'Range) do
   --        for Row in Result'Range loop
   --           Result (Row) := X (Row)-Y (Row);
   --        end loop;
   --     end return;
   --
   --  end "-";
   --
   ---------------------------
   -- Solve_Upper_Triangual --
   ---------------------------

   function Solve_Upper_Triangular (U : Matrix; B : Matrix) return Matrix
   is
   begin
      pragma Assert (Is_Upper_Triangular (U));

      return X : Matrix := Matrices.Zero (B) do

         declare
            Working_B : Matrix := B;
         begin
            for I in reverse 1 .. X.Length loop
               if U (I, I) = Zero then
                  raise Singular_Matrix;
               end if;

               X (I) := Working_B (I) / U (I, I);

               Working_B := Working_B - X (I) * U.Column (I);
            end loop;
         end;
      end return;
   end Solve_Upper_Triangular;

   ----------------------------
   -- Solve_Lower_Triangular --
   ----------------------------

   function Solve_Lower_Triangular (L : Matrix; B : Matrix) return Matrix
   is
      J : constant Matrix := Reverse_Identity (L);
   begin
      --
      --  We want to solve the linear system
      --
      --        L * x = B
      --
      --  where L is lower triangular.  Write L = J*U*J where U is upper
      --  triangular.  The system above is equivalent to
      --
      --        J * U * J * x = B <=> U * y = J*B
      --
      --  where y = J*x.  I can find y by solving an upper triangular system
      --  with U=J*L*J and J*B
      --
      --
      declare
         U : constant Matrix := J * L * J;
         Y : constant Matrix := Solve_Upper_Triangular (U, J * B);
      begin
         return J * Y;
      end;
   end Solve_Lower_Triangular;

   -------------------------
   -- Solve_Linear_System --
   -------------------------

   function Solve_Linear_System (A : Matrix; B : Matrix) return Matrix is
      U : Matrix;
      L : Matrix;
      P : Matrix;
   begin
      --
      --  We want to solve A*x = b.  We decompose A as P*A = L*U and the
      --  system becomes
      --
      --       P*b = c = P*A*x = L*U*x
      --
      LUP (X => A,
           L => L,
           U => U,
           P => P);

      return Solve_Upper_Triangular (U, Solve_Lower_Triangular (L, P * B));
   end Solve_Linear_System;

   ------------------------------
   -- Upper_Triangular_Inverse --
   ------------------------------

   function Upper_Triangular_Inverse (U : Matrix) return Matrix
   is
   begin
      raise Program_Error;
      return Empty_Matrix;
   end Upper_Triangular_Inverse;

   ------------------------------
   -- Lower_Triangular_Inverse --
   ------------------------------

   function Lower_Triangular_Inverse (L : Matrix) return Matrix
   is
      U : constant Matrix := Flip_Lr (Flip_Ud (L));
   begin
      return Flip_Lr (Flip_Ud (Upper_Triangular_Inverse (U)));
   end Lower_Triangular_Inverse;

   -------------
   -- Inverse --
   -------------

   function Inverse (X : Matrix) return Matrix
   is
      U, L, P : Matrix;
   begin
      LUP (X => X,
           L => L,
           U => U,
           P => P);

      return Upper_Triangular_Inverse (U) * Lower_Triangular_Inverse (L) * P;
   end Inverse;


end Generic_LUP;
