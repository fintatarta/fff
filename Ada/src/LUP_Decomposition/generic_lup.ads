with Generic_Matrices;

--
-- This package implements the LUP decomposition for matrices with
-- entries in a field.  The LUP decomposition, given a matrix A, it finds
-- matrices L, U and P such that
--
--          L * U = P * A
--
-- where
--
-- * L is lower triangular
-- * U is upper triangular
-- * P is a permutation matrix
--
-- Currently this implementation works only for A square and invertible.
-- A future version  will maybe address the more general case.
--
generic
   type Field_Type is private;  -- Type of matrix elements

   --
   -- The usual ingredients for a field: neutral elements, sum,
   -- product and their inverses
   --
   Zero : Field_Type;
   One  : Field_Type;

   with function "+" (X, Y : Field_Type) return Field_Type is <>;
   with function "-" (X : Field_Type) return Field_Type is <>;
   with function "*" (X, Y : Field_Type) return Field_Type is <>;
   with function Inv (X : Field_Type) return Field_Type is <>;

   --
   -- The matrix we work with is from the package Generic_Matrices
   --
   with package Matrices is new Generic_Matrices (Ring_Type => Field_Type,
                                                  Ring_Zero => Zero,
                                                  Ring_One  => One);
package Generic_LUP is
   use Matrices;

   procedure LUP (X           : Matrix;
                  L           : out Matrix;
                  U           : out Matrix;
                  P           : out Matrix;
                  Is_Singular : out Boolean)
     with
       Pre =>
         Is_Square (X),

         Post =>
           Is_Singular or else
           (
              Have_Equal_Size (X, L) and
                Have_Equal_Size (X, U) and
              Have_Equal_Size (X, P) and
                Is_Permutation (P) and
              Is_Lower_Triangular (L) and
                Has_Unit_Diagonal (L) and
              Is_Upper_Triangular (U) and
                P * X = L * U
           );

   --
   -- Almost syntactic sugar: get the determinant of X via
   -- the LUP decomposition
   --
   function Determinant (X : Matrix) return Field_Type
     with
       Pre => Is_Square (X);


   --
   -- Compute the inverse of X using the LUP decomposition.
   -- Raise Singular_Matrix if X is singular
   --
   function Inverse (X : Matrix) return Matrix
     with
       Pre => Is_Square (X),
       Post =>
         Have_Equal_Size (X, Inverse'Result) and
         X * Inverse'Result = Identity (X);

   --
   -- Compute the inverse of X using the LUP decomposition.
   -- Set Is_Singular to True if X has no inverse and raises no exception
   --
   procedure Inverse (X           : Matrix;
                      Inv         : out Matrix;
                      Is_Singular : out Boolean)
     with
       Pre => Is_Square (X),
       Post =>
         Is_Singular or else
         (Have_Equal_Size (X, Inv) and X * Inv = Identity (X));

   --
   -- Solve linear system A*x = B.  It is more efficient than computing
   -- Inverse(A)*B.
   -- Raise Singular_Matrix if A is Singular
   --
   function Solve_Linear_System (A : Matrix; B : Matrix) return Matrix
     with
       Pre =>
         B.Is_Column_Vector  and
         A.N_Cols = B.N_Rows and
         (Is_Square (A) and then Determinant (A) /= Zero),
         Post =>
           Have_Equal_Size (B, Solve_Linear_System'Result) and
           A * Solve_Linear_System'Result = B;

   --
   -- Solve linear system A*x = B
   -- Set Is_Singular to True if A has no inverse and raises no exception
   --
   procedure Solve_Linear_System (A           : Matrix;
                                  B           : Matrix;
                                  Solution    : out Matrix;
                                  Is_Singular : out Boolean)
     with
       Pre =>
         B.Is_Column_Vector  and
         A.N_Cols = B.N_Rows and
         Is_Square (A),
         Post =>
           Is_Singular or else
           (Have_Equal_Size (B, Solution) and A * Solution = B);


   --
   --  Solve system U*x=B where U is upper triangular.  This is used
   --  internally by Solve_Linear_System, but we export it nevertheless
   --  since it can be useful
   --
   function Solve_Upper_Triangular (U : Matrix; B : Matrix) return Matrix
     with
       Pre =>
         B.Is_Column_Vector and
         U.N_Cols = B.N_Rows and
         Is_Square (U) and
         Is_Upper_Triangular (U) and
         (for all I in 1 .. U.N_Rows  => U (I, I) /= Zero),
         Post =>
           Have_Equal_Size (B, Solve_Upper_Triangular'Result) and
           U * Solve_Upper_Triangular'Result = B;

   --
   --  Solve system L*x=B where L is lower triangular.  This is used
   --  internally by Solve_Linear_System, but we export it nevertheless
   --  since it can be useful
   --
   function Solve_Lower_Triangular (L : Matrix; B : Matrix) return Matrix
     with
       Pre =>
         B.Is_Column_Vector and
         L.N_Cols = B.N_Rows and
         Is_Square (L) and
         Is_Lower_Triangular (L) and
         (for all I in 1 .. L.N_Rows  => L (I, I) /= Zero),
         Post =>
           Have_Equal_Size (B, Solve_Lower_Triangular'Result) and
           L * Solve_Lower_Triangular'Result = B;

   --
   --  Compute the inverse of an upper triangular matrix.  This is used
   --  internally by Inverse, but we export it nevertheless
   --  since it can be useful
   --
   function Upper_Triangular_Inverse (U : Matrix) return Matrix
     with
       Pre => Is_Square (U) and
       Is_Upper_Triangular (U) and
       (for all I in 1 .. U.N_Rows  => U (I, I) /= Zero),
       Post =>
         Have_Equal_Size (U, Upper_Triangular_Inverse'Result) and
         U * Upper_Triangular_Inverse'Result = Identity (U);

   --
   --  Compute the inverse of a lower triangular matrix.  This is used
   --  internally by Inverse, but we export it nevertheless
   --  since it can be useful
   --
   function Lower_Triangular_Inverse (L : Matrix) return Matrix
     with
       Pre => Is_Square (L) and
       Is_Lower_Triangular (L) and
       (for all I in 1 .. L.N_Rows  => L (I, I) /= Zero),
       Post =>
         Have_Equal_Size (L, Lower_Triangular_Inverse'Result) and
         L * Lower_Triangular_Inverse'Result = Identity (L);



   Singular_Matrix : exception;

   -- Functions used in contracts

   function Is_Square (A : Matrices.Matrix) return Boolean
   is (A.N_Rows = A.N_Cols);

   function Have_Equal_Size (A, B : Matrix) return Boolean
   is (A.N_Rows = B.N_Rows and A.N_Cols = B.N_Cols);

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

private
   type Index_Range is
      record
         First : Index_Type;
         Last  : Index_Type;
      end record;

   function Row_Range (X : Matrices.Matrix) return Index_Range;

   function First (X : Index_Range) return Index_Type;
   function Last  (X : Index_Range) return Index_Type;

   function Is_In (X : Index_Type; R : Index_Range) return Boolean;


   function "-" (X, Y : Field_Type) return Field_Type
   is (X + (-Y));

   function "/" (X, Y : Field_Type) return Field_Type
   is (X * Inv (Y));

end Generic_LUP;
