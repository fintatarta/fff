pragma Ada_2012;
package body Generic_Matrices is

   function "-" (X, Y : Ring_Type) return Ring_Type
   is (X + (-Y));

   ------------
   -- N_Rows --
   ------------

   function N_Rows (X : Matrix) return Positive
   is (X.N_Rows);

   ------------
   -- N_Cols --
   ------------

   function N_Cols (X : Matrix) return Positive
   is (X.N_Cols);


   -----------
   -- Value --
   -----------

   function Value (X : Matrix; N : Index_Type) return Ring_Type is
   begin
      if not X.Is_Vector or N > X.Length then
         raise Constraint_Error;
      end if;

      return X.Data (N);
   end Value;

   -----------
   -- Value --
   -----------

   function Value (X : Matrix; Row, Col : Index_Type) return Ring_Type is
   begin
      if Row > X.N_Rows or Col > X.N_Cols then
         raise Constraint_Error;
      end if;

      return X.Data (To_Index (X, Row, Col));
   end Value;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (X : aliased in out Matrix; N : in Index_Type) return Reference_Type
   is
   begin
      if not X.Is_Vector or N > X.Length then
         raise Constraint_Error;
      end if;

      return Reference_Type'
        (Element => Ring_Vectors.Reference (X.Data, N).Element);
   end Reference;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (X : aliased in out Matrix; Row, Col : in Index_Type)
      return Reference_Type
   is
      use Ring_Vectors;
   begin
      if Row > X.N_Rows or Col > X.N_Cols then
         raise Constraint_Error;
      end if;
      return Reference_Type'
        (Element => Reference (X.Data, To_Index (X, Row, Col)).Element);
   end Reference;

   ----------
   -- Zero --
   ----------

   function Zero (N : Index_Type) return Matrix
   is (Zero (N, N));

   ----------
   -- Zero --
   ----------

   function Zero (X : Matrix) return Matrix
   is (Zero (N_Rows => X.N_Rows,
             N_Cols => X.N_Cols));

   ----------
   -- Zero --
   ----------

   function Zero (N_Rows, N_Cols : Index_Type) return Matrix
   is
      use Ring_Vectors;
   begin
      return Matrix'(N_Rows => N_Rows,
                     N_Cols => N_Cols,
                     Data   =>
                       To_Vector (New_Item => Ring_Zero,
                                  Length   => Count_Type (N_Rows * N_Cols)));
   end Zero;

   --------------
   -- Identity --
   --------------

   function Identity (N : Index_Type) return Matrix
   is (Identity (N, N));

   --------------
   -- Identity --
   --------------

   function Identity (X : Matrix) return Matrix
   is (Identity (N_Rows => X.N_Rows,
                 N_Cols => X.N_Cols));

   --------------
   -- Identity --
   --------------

   function Identity (N_Rows, N_Cols : Index_Type) return Matrix is
   begin
      return Result : Matrix := Zero (N_Rows => N_Rows, N_Cols => N_Cols) do
         for I in Index_Type'First .. Index_Type'Min (N_Rows, N_Cols) loop
            Result (I, I) := Ring_One;
         end loop;
      end return;
   end Identity;

   ----------------------
   -- Reverse_Identity --
   ----------------------

   function Reverse_Identity (N : Index_Type) return Matrix
   is (Reverse_Identity (N, N));

   ----------------------
   -- Reverse_Identity --
   ----------------------

   function Reverse_Identity (X : Matrix) return Matrix
   is (Reverse_Identity (N_Rows => X.N_Rows,
                         N_Cols => X.N_Cols));


   ----------------------
   -- Reverse_Identity --
   ----------------------

   function Reverse_Identity (N_Rows, N_Cols : Index_Type) return Matrix
   is (Flip_Lr (Identity (N_Rows => N_Rows, N_Cols => N_Cols)));

   -------------
   -- Flip_Lr --
   -------------

   function Flip_Lr (X : Matrix) return Matrix
   is
   begin
      return Result : Matrix := Zero (X) do
         for Row in 1 .. Result.N_Rows loop
            for Col in 1 .. Result.N_Cols loop
               Result (Row, Col) := X (Row, X.N_Cols + 1 - Col);
            end loop;
         end loop;
      end return;
   end Flip_Lr;

   -------------
   -- Flip_Ud --
   -------------

   function Flip_Ud (X : Matrix) return Matrix
   is
   begin
      return Result : Matrix := Zero (X) do

         for Row in 1 .. Result.N_Rows loop
            for Col in 1 .. Result.N_Cols loop
               Result (Row, Col) := X (X.N_Rows + 1 - Row, Col);
            end loop;
         end loop;

      end return;
   end Flip_Ud;

   ---------------
   -- Transpose --
   ---------------

   function Transpose (X : Matrix) return Matrix
   is
   begin
      return Result : Matrix := Zero (N_Rows => X.N_Cols,
                                      N_Cols => X.N_Rows) do

         for Row in 1 .. Result.N_Rows loop
            for Col in 1 .. Result.N_Cols loop
               Result (Row, Col) := X (Col, Row);
            end loop;
         end loop;

      end return;
   end Transpose;

   -----------
   -- Trace --
   -----------

   function Trace (X : Matrix) return Ring_Type
   is
   begin
      return Accumulator : Ring_Type := Ring_Zero do

         for I in 1 .. Index_Type'Min (X.N_Rows, X.N_Cols) loop
            Accumulator := Accumulator + X (I, I);
         end loop;

      end return;
   end Trace;

   ---------
   -- "+" --
   ---------

   function "+" (X, Y : Matrix) return Matrix is
   begin
      if not (X.N_Rows = Y.N_Rows and X.N_Cols = Y.N_Cols) then
         raise Constraint_Error;
      end if;

      pragma Assert (X.Data.First_Index = Y.Data.First_Index);
      pragma Assert (X.Data.Last_Index = Y.Data.Last_Index);

      return Result : Matrix := Zero (X) do
         pragma Assert (X.Data.First_Index = Result.Data.First_Index);
         pragma Assert (X.Data.Last_Index = Result.Data.Last_Index);

         for I in X.Data.First_Index .. X.Data.Last_Index loop
            Result.Data (I) := X.Data (I) + Y.Data (I);
         end loop;
      end return;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (X, Y : Matrix) return Matrix is
   begin
      if not (X.N_Rows = Y.N_Rows and X.N_Cols = Y.N_Cols) then
         raise Constraint_Error;
      end if;

      pragma Assert (X.Data.First_Index = Y.Data.First_Index);
      pragma Assert (X.Data.Last_Index = Y.Data.Last_Index);

      return Result : Matrix := Zero (X) do
         pragma Assert (X.Data.First_Index = Result.Data.First_Index);
         pragma Assert (X.Data.Last_Index = Result.Data.Last_Index);

         for I in X.Data.First_Index .. X.Data.Last_Index loop
            Result.Data (I) := X.Data (I) - Y.Data (I);
         end loop;
      end return;
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (X : Matrix) return Matrix is
   begin
      return Result : Matrix := Zero (X) do
         pragma Assert (X.Data.First_Index = Result.Data.First_Index);
         pragma Assert (X.Data.Last_Index = Result.Data.Last_Index);

         for I in X.Data.First_Index .. X.Data.Last_Index loop
            Result.Data (I) := - X.Data (I);
         end loop;
      end return;
   end "-";

   ---------
   -- Row --
   ---------

   function Row (X : Matrix; R : Index_Type) return Matrix
   is
   begin
      return Result : Matrix := Zero (N_Rows => 1,
                                      N_Cols => X.N_Cols) do
         for I in 1 .. X.N_Cols loop
            Result.Data (I) := X (R, I);
         end loop;
      end return;
   end Row;

   ------------
   -- Column --
   ------------

   function Column (X : Matrix; C : Index_Type) return Matrix
   is
   begin
      return Result : Matrix := Zero (N_Rows => X.N_Rows,
                                      N_Cols => 1) do
         for I in 1 .. X.N_Rows loop
            Result.Data (I) := X (I, C);
         end loop;
      end return;
   end Column;


   --------------------
   -- Scalar_Product --
   --------------------

   function Scalar_Product (X, Y : Matrix) return Ring_Type
   is

   begin
      if not (X.Is_Vector and Y.Is_Vector and X.Length = Y.Length) then
         raise Constraint_Error;
      end if;

      return  Accumulator : Ring_Type := Ring_Zero do
         for I in 1 .. X.Length loop
            Accumulator := Accumulator + X.Data (I) * Y.Data (I);
         end loop;
      end return;
   end Scalar_Product;


   ---------
   -- "*" --
   ---------

   function "*" (X, Y : Matrix) return Matrix is

   begin
      if X.N_Cols /= Y.N_Rows then
         raise Constraint_Error;
      end if;

      return Result : Matrix := Zero (N_Rows => X.N_Rows,
                                      N_Cols => Y.N_Cols) do

         for R in 1 .. Result.N_Rows loop
            for C in 1 .. Result.N_Cols loop
               Result (R, C) := Scalar_Product (Row (X, R), Column (Y, C));
            end loop;
         end loop;

      end return;
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (C : Ring_Type; X : Matrix) return Matrix is
   begin
      return Result : Matrix := Zero (X) do
         pragma Assert (X.Data.First_Index = Result.Data.First_Index);
         pragma Assert (X.Data.Last_Index = Result.Data.Last_Index);

         for I in X.Data.First_Index .. X.Data.Last_Index loop
            Result.Data (I) := C * X.Data (I);
         end loop;
      end return;
   end "*";

   ----------
   -- "**" --
   ----------

   function "**" (X : Matrix; Exponent : Natural) return Matrix is
   begin
      return Result : Matrix := Identity (X) do
         for I in 1 .. Exponent loop
            Result := Result * X;
         end loop;
      end return;
   end "**";

end Generic_Matrices;
