with Ada.Containers.Vectors;

generic
   type Ring_Type is private;

   Ring_Zero : Ring_Type;
   Ring_One  : Ring_Type;

   with function "+" (X, Y : Ring_Type) return Ring_Type is <>;
   with function "-" (X : Ring_Type) return Ring_Type is <>;
   with function "*" (X, Y : Ring_Type) return Ring_Type is <>;

   with function Is_Unit (X : Ring_Type) return Boolean is <>;
   with function Inv (X : Ring_Type) return Ring_Type is <>;
package Generic_Matrices is
   type Matrix is tagged private
     with
       Constant_Indexing => Value,
       Variable_Indexing => Reference;

   subtype Index_Type is Positive;

   function N_Rows (X : Matrix) return Positive;
   function N_Cols (X : Matrix) return Positive;

   subtype Column_Vector is Matrix
     with
       Dynamic_Predicate => Column_Vector.N_Cols = 1;

   subtype Row_Vector is Matrix
     with
       Dynamic_Predicate => Row_Vector.N_Rows = 1;

   subtype Square_Matrix is Matrix
     with
       Dynamic_Predicate => Square_Matrix.N_Rows = Square_Matrix.N_Cols;

   function Is_Vector (X : Matrix) return Boolean
   is (X.N_Rows = 1 or X.N_Cols = 1);

   function Is_Column_Vector (X : Matrix) return Boolean
   is (X.N_Cols = 1);

   function Is_Row_Vector (X : Matrix) return Boolean
   is (X.N_Rows = 1);

   function Length (X : Matrix) return Positive
   is (Positive'Max (X.N_Rows, X.N_Cols));

   function Value (X : Matrix; N : Index_Type) return Ring_Type
     with
       Pre => X.Is_Vector and X.Length >= N;

   function Value (X : Matrix; Row, Col : Index_Type) return Ring_Type
     with
       Pre => Row <= X.N_Rows and Col <= X.N_Cols;

   type Reference_Type (Element : not null access Ring_Type) is private
     with Implicit_Dereference => Element;

   function Reference (X : aliased in out Matrix;
                       N : in Index_Type)
                       return Reference_Type
     with
       Pre => X.Is_Vector and N <= X.Length;

   function Reference (X        : aliased in out Matrix;
                       Row, Col : in Index_Type)
                       return Reference_Type
     with
       Pre => Row <= X.N_Rows and Col <= X.N_Cols;

   function Zero (N : Index_Type) return Matrix
     with
       Post =>
         Zero'Result.N_Rows = N and
         Zero'Result.N_Cols = N;

   function Zero (N_Rows, N_Cols : Index_Type) return Matrix
     with
       Post =>
         Zero'Result.N_Rows = N_Rows and
         Zero'Result.N_Cols = N_Cols;

   function Zero (X : Matrix) return Matrix
     with
       Post =>
         Zero'Result.N_Rows = X.N_Rows and
         Zero'Result.N_Cols = X.N_Cols;

   function Identity (N : Index_Type) return Matrix
     with
       Post =>
         Identity'Result.N_Rows = N and
         Identity'Result.N_Cols = N;

   function Identity (X : Matrix) return Matrix
     with
       Post =>
         Identity'Result.N_Rows = X.N_Rows and
         Identity'Result.N_Cols = X.N_Cols;


   function Identity (N_Rows, N_Cols : Index_Type) return Matrix
     with
       Post =>
         Identity'Result.N_Rows = N_Rows and
         Identity'Result.N_Cols = N_Cols;

   function Reverse_Identity (N : Index_Type) return Matrix
     with
       Post =>
         Reverse_Identity'Result.N_Rows = N and
         Reverse_Identity'Result.N_Cols = N;

   function Reverse_Identity (X : Matrix) return Matrix
     with
       Post =>
         Reverse_Identity'Result.N_Rows = X.N_Rows and
         Reverse_Identity'Result.N_Cols = X.N_Cols;


   function Reverse_Identity (N_Rows, N_Cols : Index_Type) return Matrix
     with
       Post =>
         Reverse_Identity'Result.N_Rows = N_Rows and
         Reverse_Identity'Result.N_Cols = N_Cols;

   function Flip_Lr(X: Matrix) return Matrix
     with
       Post =>
         Flip_Lr'Result.N_Rows = X.N_Rows and
         Flip_Lr'Result.N_Cols = X.N_Cols;

   function Flip_Ud (X : Matrix) return Matrix
     with
       Post =>
         Flip_Ud'Result.N_Rows = X.N_Rows and
         Flip_Ud'Result.N_Cols = X.N_Cols;

   function Transpose (X : Matrix) return Matrix
     with
       Post =>
         Transpose'Result.N_Rows = X.N_Cols and
         Transpose'Result.N_Cols = X.N_Rows;

   function Trace (X : Matrix) return Ring_Type;



   function "+" (X, Y : Matrix) return Matrix
     with
       Pre =>
         X.N_Rows = Y.N_Rows and
         X.N_Cols = Y.N_Cols,
         Post =>
           X.N_Rows = "+"'Result.N_Rows and
           X.N_Cols = "+"'Result.N_Cols;

   function "-" (X, Y : Matrix) return Matrix
     with
       Pre =>
         X.N_Rows = Y.N_Rows and
         X.N_Cols = Y.N_Cols,
         Post =>
           X.N_Rows = "-"'Result.N_Rows and
           X.N_Cols = "-"'Result.N_Cols;

   function "-" (X : Matrix) return Matrix
     with
       Post =>
         X.N_Rows = "-"'Result.N_Rows and
         X.N_Cols = "-"'Result.N_Cols;

   function "*" (X, Y : Matrix) return Matrix
     with
       Pre => X.N_Cols = Y.N_Rows,
       Post =>
         X.N_Rows = "*"'Result.N_Rows and
         Y.N_Cols = "*"'Result.N_Cols;

   function "*" (C : Ring_Type; X : Matrix) return Matrix
     with
       Post =>
         X.N_Rows = "*"'Result.N_Rows and
         X.N_Cols = "*"'Result.N_Cols;

   function "**" (X : Matrix; Exponent : Natural) return Matrix
     with
       Pre =>
         X.N_Rows = X.N_Cols,
         Post =>
           X.N_Rows = "**"'Result.N_Rows and
           X.N_Cols = "**"'Result.N_Cols;

   function Row (X : Matrix; R : Index_Type) return Matrix
     with
       Pre => R <= X.N_Rows,
       Post => Row'Result.N_Rows = 1 and Row'Result.N_Cols = X.N_Cols;

   function Column (X : Matrix; C : Index_Type) return Matrix
     with
       Pre => C <= X.N_Cols,
       Post => Column'Result.N_Cols = 1 and Column'Result.N_Rows = X.N_Rows;

   function Scalar_Product (X, Y : Matrix) return Ring_Type
     with
       Pre => X.Is_Vector and Y.Is_Vector and X.Length = Y.Length;

private
   use Ada.Containers;

   --  function Create (N_Row, N_Cols : Positive) return Matrix
   --    with
   --      Post =>
   --        Create'Result.N_Rows = N_Rows and
   --        Create'Result.N_Cols = N_Cols;

   package Ring_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Index_Type,
                                 Element_Type => Ring_Type);

   type Matrix is tagged
      record
         N_Rows : Positive;
         N_Cols : Positive;
         Data   : Ring_Vectors.Vector;
      end record
     with
       Type_Invariant => Data.Length = Count_Type (N_Rows * N_Cols);

   function To_Index (M : Matrix; Row, Col : Index_Type) return Index_Type
   is (Row + (Col - Index_Type'First) * M.N_Rows);

   type Reference_Type (Element : not null access Ring_Type) is null record;

end Generic_Matrices;
