
private
generic
package Generic_Lup.Actions is

   type Action_Type (<>) is private;

   function Is_Permutation (X : Action_Type) return Boolean;

   function Have_Equal_Size (X, Y : Action_Type) return Boolean;

   function Swap_Rows (R, S : Index_Type; Rng : Index_Range) return Action_Type
     with
       Pre =>
         Is_In (R, Rng) and Is_In (S, Rng),
       Post =>
         Is_Permutation (Swap_Rows'Result);

   function Add_Rows (Src   : Index_Type;
                      Coeff : Vector;
                      Rng   : Index_Range) return Action_Type
     with
       Pre =>
         Is_In (Src, Rng) and then
         Coeff'First = Index_Type'Succ (Src) and then
         Coeff'Last = Last (Rng),
         Post =>
           not Is_Permutation (Add_Rows'Result);

   function Inv (X : Action_Type) return Action_Type
     with
       Post => Is_Permutation (Inv'Result) = Is_Permutation (X);

   function Commute (S : Action_Type; P : Action_Type) return Action_Type
     with
       Pre => not Is_Permutation (S) and Is_Permutation (P);

   function "*" (Action : Action_Type; Object : Matrix) return Matrix;

   function "*" (X, Y : Action_Type) return Action_Type
     with
       Pre =>
         Is_Permutation (X) and
         Is_Permutation (Y) and
         Have_Equal_Size (X, Y),
       Post =>
           Is_Permutation ("*"'Result) and
           Have_Equal_Size (X, "*"'Result);

   function To_Matrix (X : Action_Type) return Matrix;

private

   type Action_Class is (Permutation, Sum);

   type Action_Type (Class : Action_Class;
                     First : Index_Type;
                     Last  : Index_Type) is
      record
         Mtx : Matrix (First .. Last, First .. Last);

         case Class is
            when Permutation =>
               null;

            when Sum =>
               Src : Index_Type;
         end case;
      end record;


end Generic_Lup.Actions;
