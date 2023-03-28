pragma Ada_2012;
package body Generic_Lup.Actions is


   --------------------
   -- Is_Permutation --
   --------------------

   function Is_Permutation (X : Action_Type) return Boolean
   is (X.Class = Permutation);


   ---------------------
   -- Have_Equal_Size --
   ---------------------

   function Have_Equal_Size (X, Y : Action_Type) return Boolean
   is (X.First = Y.First and X.Last = Y.Last);


   ---------------
   -- Swap_Rows --
   ---------------

   function Swap_Rows (R, S : Index_Type; Rng : Index_Range) return Action_Type
   is
   begin
      return Swap : Action_Type := Action_Type'(Class => Permutation,
                                                First => Rng.First,
                                                Last  => Rng.Last,
                                                Mtx   => Identity (Rng))
      do
         Swap.Mtx (R, S) := One;
         Swap.Mtx (S, R) := One;
         Swap.Mtx (R, R) := Zero;
         Swap.Mtx (S, S) := Zero;
      end return;
   end Swap_Rows;

   --------------
   -- Add_Rows --
   --------------

   function Add_Rows
     (Src : Index_Type; Coeff : Vector; Rng : Index_Range) return Action_Type
   is

   begin
      return S : Action_Type := Action_Type'(Class => Sum,
                                             First => Rng.First,
                                             Last  => Rng.Last,
                                             Mtx   => Identity (Rng),
                                             Src   => Src)
      do
         for Row in Index_Type'Succ (Src) .. s.Mtx'Last (1) loop
            S.Mtx (Row, Src) := Coeff (Row);
         end loop;
      end return;
   end Add_Rows;

   ---------
   -- Inv --
   ---------

   function Inv (X : Action_Type) return Action_Type is

   begin
      -- Initialize the result with X itself, they will differ only in Mtx
      return Result  : Action_Type := X  do
         case Result.Class is
            when Permutation =>
               Result.Mtx := Transpose (Result.Mtx);

            when Sum =>
               for Row in Index_Type'Succ (Result.Src) .. Result.Mtx'Last (1) loop
                  Result.Mtx (Row, Result.Src) := - Result.Mtx (Row, Result.Src);
               end loop;
         end case;
      end return;
   end Inv;

   -------------
   -- Commute --
   -------------

   function Commute (S : Action_Type; P : Action_Type) return Action_Type is
   begin
      if not (S.Class = Sum and P.Class = Permutation) then
         raise Constraint_Error;
      end if;

      return Result : Action_Type := S do
         Result.Mtx := P.Mtx * S.Mtx * Transpose (P.Mtx);
      end return;
   end Commute;

   ---------
   -- "*" --
   ---------

   function "*" (Action : Action_Type; Object : Matrix) return Matrix
   is (Action.Mtx * Object);

   ---------
   -- "*" --
   ---------

   function "*" (X, Y : Action_Type) return Action_Type is
   begin
      if not (X.Class = Permutation and Y.Class = Permutation) then
         raise Constraint_Error;
      end if;

      return Result : Action_Type := X do
         Result.Mtx := X.Mtx * Y.Mtx;
      end return;
   end "*";

   ---------------
   -- To_Matrix --
   ---------------

   function To_Matrix (X : Action_Type) return Matrix
   is (X.Mtx);
end Generic_Lup.Actions;
