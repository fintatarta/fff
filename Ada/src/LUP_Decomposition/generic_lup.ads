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
   type Index_Range is private;

   function Row_Range (X : Matrix) return Index_Range
     with
       Post =>
         X'First (1) = First (Row_Range'Result) and
         X'Last (1) = Last (Row_Range'Result);

   function First (X : Index_Range) return Index_Type;
   function Last  (X : Index_Range) return Index_Type;

   function Is_In (X : Index_Type; R : Index_Range) return Boolean;


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
   is (A'First = B'First and A'Last = B'Last);

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

   function Identity (Rng : Index_Range) return Matrix
     with
       Post =>
         Row_Range (Identity'Result) = Rng and
         Is_Square (Identity'Result);


   function Identity (Template : Matrix) return Matrix
     with
       Pre =>
         Is_Square (Template),
         Post =>
           Row_Range (Identity'Result) = Row_Range (Template) and
           Is_Square (Identity'Result);


   procedure LUP (X : Matrix;
                  L : out Matrix;
                  U : out Matrix;
                  P : out Matrix)
     with
       Pre =>
         X'Length (1) > 0 and
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

   function Solve_Upper_Triangual (U : Matrix; B : Vector) return Vector
     with
       Pre =>
         Is_Square (U) and
         Is_Upper_Triangular (U) and
         (for all I in U'Range (1) => U (I, I) /= Zero) and
         U'First (1) = B'First (1) and
         U'Last (1) = B'Last (1),

         Post =>
           Have_Equal_Size (B, Solve_Upper_Triangual'Result) and
           U * Solve_Upper_Triangual'Result = B;


   function Solve_Linear_System (A : Matrix; B : Vector) return Vector
     with
       Pre =>
         (Is_Square (A) and then Determinant (A) /= Zero) and
         A'First (1) = B'First (1) and
         A'Last (1) = B'Last (1),
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
