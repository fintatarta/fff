with Generic_Matrices;

generic
   type Field_Type is private;

   Zero : Field_Type;
   One  : Field_Type;

   with function "+" (X, Y : Field_Type) return Field_Type is <>;
   with function "-" (X : Field_Type) return Field_Type is <>;
   with function "*" (X, Y : Field_Type) return Field_Type is <>;
   with function Inv (X : Field_Type) return Field_Type is <>;
   with function Is_Unit (X : Field_Type) return Boolean is <>;

   with package Matrices is new Generic_Matrices (Ring_Type => Field_Type,
                                                  Ring_Zero => Zero,
                                                  Ring_One  => One);
package Generic_LUP is
   use Matrices;

   subtype Index_Type is Positive;

   type Index_Range is private;

   function Row_Range (X : Matrices.Matrix) return Index_Range;

   function First (X : Index_Range) return Index_Type;
   function Last  (X : Index_Range) return Index_Type;

   function Is_In (X : Index_Type; R : Index_Range) return Boolean;


   function Is_Square (A : Matrices.Matrix) return Boolean
   is (A.N_Rows = A.N_Cols)
     with Ghost;

   function Have_Equal_Size (A, B : Matrix) return Boolean
   is (A.N_Rows = B.N_Rows and A.N_Cols = B.N_Cols)
     with Ghost;

   function Is_Permutation (X : Matrix) return Boolean
     with
       Pre => Is_Square (X),
       Ghost;

   function Is_Lower_Triangular (X : Matrix) return Boolean
     with
       Pre => Is_Square (X),
       Ghost;

   function Is_Upper_Triangular (X : Matrix) return Boolean
     with
       Pre => Is_Square (X),
       Ghost;

   function Has_Unit_Diagonal (X : Matrix) return Boolean
     with
       Pre => Is_Square (X),
       Ghost;



   procedure LUP (X : Matrix;
                  L : out Matrix;
                  U : out Matrix;
                  P : out Matrix)
     with
       Pre =>
         Is_Square (X),

         Post =>
           Have_Equal_Size (X, L) and
           Have_Equal_Size (X, U) and
           Have_Equal_Size (X, P) and
           P * X = L * U and
           Is_Permutation (P) and
           Is_Lower_Triangular (L) and
           Has_Unit_Diagonal (L) and
           Is_Upper_Triangular (U);

   function Determinant (X : Matrix) return Field_Type
     with
       Pre => Is_Square (X);

   function Solve_Upper_Triangual (U : Matrix; B : Matrix) return Matrix
     with
       Pre =>
         B.Is_Column_Vector and
         U.N_Cols = B.N_Rows and
         Is_Square (U) and
         Is_Upper_Triangular (U) and
         (for all I in 1 .. U.N_Rows  => U (I, I) /= Zero),
         Post =>
           Have_Equal_Size (B, Solve_Upper_Triangual'Result) and
           U * Solve_Upper_Triangual'Result = B;


   function Solve_Linear_System (A : Matrix; B : Matrix) return Matrix
     with
       Pre =>
         B.Is_Column_Vector  and
         A.N_Cols = B.N_Rows and
         (Is_Square (A) and then Determinant (A) /= Zero),
         Post =>
           Have_Equal_Size (B, Solve_Linear_System'Result) and
           A * Solve_Linear_System'Result = B;

   Singular_Matrix : exception;

private
   type Index_Range is
      record
         First : Index_Type;
         Last  : Index_Type;
      end record
     with
       Type_Invariant => (First <= Last);

   function "-" (X, Y : Field_Type) return Field_Type
   is (X + (-Y));

   function "/" (X, Y : Field_Type) return Field_Type
   is (X * Inv (Y));

end Generic_LUP;
