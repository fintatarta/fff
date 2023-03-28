generic
   type Field_Type is private;

   Zero : Field_Type;
   One  : Field_Type;

   with function "+" (X, Y : Field_Type) return Field_Type is <>;
   with function "-" (X : Field_Type) return Field_Type is <>;
   with function "*" (X, Y : Field_Type) return Field_Type is <>;
   with function Inv (X : Field_Type) return Field_Type is <>;

   type Index_Type is (<>);

   type Matrix is array (Index_Type range <>, Index_Type range <>) of Field_Type;
   type Vector is array (Index_Type range <>) of Field_Type;
package Generic_LUP is
   function Is_Square (A : Matrix) return Boolean
   is (A'First (1) = A'First (2) and A'Last (1) = A'Last (2))
     with Ghost;

   function Have_Equal_Size (A, B : Matrix) return Boolean
   is (A'First (1) = B'First (1) and
         A'Last (1) = B'Last (1) and
         A'First (2) = B'First (2) and
         A'Last (2) = B'Last (2))
     with Ghost;

   function Have_Equal_Size (A, B : Vector) return Boolean
   is (A'First = B'First and A'Last = B'Last)
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

   function "*" (X, Y : Matrix) return Matrix
     with
       Pre =>
         X'First (2) = Y'First (1) and
         X'Last (2) = Y'Last (1),
         Post =>
           X'First (1) = "*"'Result'First (1) and
           X'Last (1) = "*"'Result'Last (1) and
           Y'First (2) = "*"'Result'First (2) and
           Y'Last (2) = "*"'Result'Last (2);

   function "*" (X : Matrix; Y : Vector) return Vector
     with
       Pre =>
         X'First (2) = Y'First and
         X'Last (2) = Y'Last,
         Post =>
           X'First (1) = "*"'Result'First and
           X'Last (1) = "*"'Result'Last;

   function Transpose (X : Matrix) return Matrix
     with
       Post =>
         X'First (1) = Transpose'Result'First (2) and
         X'Last (1)  = Transpose'Result'Last (2) and
         X'First (2) = Transpose'Result'First (1) and
         X'Last (2)  = Transpose'Result'Last (1);


   procedure LUP (X : Matrix;
                  L : out Matrix;
                  U : out Matrix;
                  P : out Matrix)
     with
       Pre =>
         Is_Square (X) and
         Have_Equal_Size (X, L) and
         Have_Equal_Size (X, U) and
         Have_Equal_Size (X, P),

         Post =>
           P * X = L * U and
           Is_Permutation (P) and
           Is_Lower_Triangular (L) and
           Has_Unit_Diagonal (L) and
           Is_Upper_Triangular (U);

   function Determinant (X : Matrix) return Field_Type
     with
       Pre => Is_Square (X);

   function Solve_Linear_System (A : Matrix; B : Vector) return Vector
     with
       Pre =>
         (Is_Square (A) and then Determinant (A) /= Zero) and
         A'First (1) = B'First (1) and
         A'Last (1) = B'Last (1),
         Post =>
           Have_Equal_Size (B, Solve_Linear_System'Result) and
           A * Solve_Linear_System'Result = B;
end Generic_LUP;
