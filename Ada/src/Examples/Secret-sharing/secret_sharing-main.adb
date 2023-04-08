with Ada.Numerics.Discrete_Random;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;

with Secret_Sharing.Configuration;
with Secret_Sharing.Points;

with Generic_Polynomials;

--
--  Main procedure.  In Ada any procedure can play the role of the main.
--  The language standard delegates to the development environment how
--  the main is specified; in the specific case of GNAT compiler, it is
--  specified in the project file.
--
procedure Secret_Sharing.Main is
   use Ada.Command_Line;
   use Field;

   package Poly is
     new Generic_Polynomials (Field_Type => Galois,
                              Field_Zero => Zero,
                              Field_One  => One);


   function Encode_Secret (Secret    : Secret_Type;
                           N_Pieces  : Positive;
                           Threshold : Positive)
                           return Points.Point_Array
     with
       Pre  => Threshold <= N_Pieces,
       Post => Natural (Encode_Secret'Result.Length) = N_Pieces;

   function Encode_Secret (Secret    : Secret_Type;
                           N_Pieces  : Positive;
                           Threshold : Positive)
                           return Points.Point_Array
   is
      use Points;

      use type Poly.Polynomial_Degree;

      --
      -- Remember the procedure for encoding the secret
      --
      -- 1. Get a random polynomial P(x) = a_0 + a_1 x + a_2 x^2 + ...
      --    of degree Threshold-1 where a_0 is the secret and the other
      --    coefficients are randomly chosen
      --
      -- 2. Choose x_1, x_2, ..., x_N, with N=N_pieces and compute y_i=P(x_i)
      --    x_i does not need to be secret and it can be anything, for example,
      --    one can choose x_i=i
      --
      -- 3. Every pair (x_i, y_i) is a generated point
      --

      -- ------

      --
      -- Get a random polynomial with C0 as the coefficient of x^0
      --
      function Random_Polynomial (C0     : Secret_Type;
                                  Degree : Natural)
                                  return Poly.Polynomial
        with
          Post =>
            Random_Polynomial'Result.Degree = Poly.To_Degree (Degree) and
            Random_Polynomial'Result (Poly.Exponent_Type'(0)) = To_Galois (C0);

      function Random_Polynomial (C0     : Secret_Type;
                                  Degree : Natural)
                                  return Poly.Polynomial
      is
         --
         --  NOTE: The default random generator of Ada can be good
         --  for simulations and other stuff, but it is most probably
         --  a bad idea to use it for security purposes.  However, this
         --  program is for demonstration purposes and we can use it.  Just
         --  be aware that in a real context you should replace this with
         --  a cryptographically strong generator.  Everything else should
         --  work just fine.
         --
         package Rnd is
           new Ada.Numerics.Discrete_Random (Secret_Type);

         Random_Generator : Rnd.Generator;

         function Random_Coefficient return Galois
         is (To_Galois (Rnd.Random (Random_Generator)));

         Coeffs : Poly.Coefficient_Array (0 .. Degree);
      begin
         Rnd.Reset (Random_Generator);

         for I in Coeffs'Range loop
            Coeffs (I) :=
              (if I = 0 then  To_Galois (C0) else Random_Coefficient);
         end loop;


         return Poly.To_Polynomial (Coeffs);
      end Random_Polynomial;

      P : constant Poly.Polynomial :=
            Random_Polynomial (C0     => Secret,
                               Degree => Threshold - 1);
   begin

      return Fragments : Point_Array do

         for I in 1 .. N_Pieces loop
            declare
               S : constant Secret_Type := Secret_Type (I);
            begin
               Fragments.Append (Point_Type'(X => S,
                                             Y => To_Secret (P (To_Galois (S)))));
            end;
         end loop;

      end return;
   end Encode_Secret;

   function Decode_Secret (Pieces : Points.Point_Array) return Secret_Type
   is
      use Mtx;
      --
      --  The reconstruction is done by solving the system
      --
      --    | 1 x_1 x_1^2 ... x_1^(N-1) | |  s_0  |   |  y_1  |
      --    | 1 x_2 x_2^2 ... x_2^(N-1) | |  s_1  |   |  y_2  |
      --    | :  :    :           :     | |   :   | = |   :   |
      --    | 1 x_N x_N^2 ... x_N^(N-1) | | s_N-1 |   |  y_N  |
      --
      -- where (x_n, y_n) are the coordinates of the n-th point.  The
      -- secret is a_0, the first component of the result
      --

      --
      -- Fill a row of the Vandermonde matrix
      --
      procedure Fill_Row (Vandermonde : in out Matrix;
                          Row         : Positive;
                          R           : Galois)
      is
      begin
         Vandermonde (Row, 1) := Field.One;

         for Col in 2 .. Vandermonde.N_Cols loop
            Vandermonde (Row, Col) := Vandermonde (Row, Col - 1) * R;
         end loop;
      end Fill_Row;

      Vandermonde : Matrix := Mtx.Zero (Natural (Pieces.Length));

      Known_Part  : Matrix := Mtx.Zero (N_Rows => Vandermonde.N_Cols,
                                        N_Cols => 1);
   begin
      for Row in 1 .. Vandermonde.N_Rows loop

         Known_Part (Row) := To_Galois (Pieces (Row).Y);

         Fill_Row (Vandermonde, Row, To_Galois (Pieces (Row).X));

      end loop;

      declare
         S           : Matrix;
         Is_Singular : Boolean;
      begin
         Lup.Solve_Linear_System (A           => Vandermonde,
                                  B           => Known_Part,
                                  Solution    => S,
                                  Is_Singular => Is_Singular);

         if Is_Singular then
            raise Lup.Singular_Matrix;
         end if;

         --
         -- That's all folks!!
         --
         return To_Secret (S (1));
      end;
   end Decode_Secret;
begin
   -- Be pessimistic, we will set Success at the end
   Set_Exit_Status (Failure);

   Configuration.Parse_Command_Line;

   Mtx.Register_Printer (Field.Image'Access);

   case Configuration.Action_Required is

      when Configuration.Help =>
         Put_Line (Standard_Error, Configuration.Help_Text);

      when Configuration.Encode =>
         declare
            Fragments : constant Points.Point_Array :=
                          Encode_Secret (Secret    => Configuration.Secret,
                                         N_Pieces  => Configuration.N_Pieces,
                                         Threshold => Configuration.Threshold);
         begin
            for Fragment of Fragments loop
               Put_Line (Points.Image (Fragment));
            end loop;
         end;

      when Configuration.Decode =>
         Put_Line
           (Secret_Type'Image (Decode_Secret (Configuration.Pieces_Provided)));

   end case;

   Set_Exit_Status (Success);

exception
   when Lup.Singular_Matrix =>
      Put_Line (Standard_Error, "Matrix is singular.  Check your data");

   when E : Configuration.Bad_Command_Line =>
      Put_Line (Standard_Error, Ada.Exceptions.Exception_Message (E));

      Put (Standard_Error, Configuration.Help_Text);
end Secret_Sharing.Main;
