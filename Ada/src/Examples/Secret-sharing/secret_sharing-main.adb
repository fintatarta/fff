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

         coeffs : Poly.Coefficient_Array (0 .. Degree);
      begin
         --  Put_Line ("<< 88");

         Rnd.Reset (Random_Generator);
         --  Put_Line ("<< 77");

         for I in Coeffs'Range loop
            --  Put_Line ("<< 66" & I'Image);

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
               Fragments.append(Point_Type'(X => S,
                                            Y => To_Secret (P (To_Galois (S)))));
            end;
         end loop;

      end return;
   end Encode_Secret;
begin
   -- Be pessimistic, we will set Success at the end
   Set_Exit_Status (Failure);

   Configuration.Initialize;

   case Configuration.Action_Required is
      when Configuration.Encode =>
         declare
            Fragments : constant Points.Point_Array :=
                          Encode_Secret (Secret    => Configuration.Secret,
                                         N_Pieces  => Configuration.N_Pieces,
                                         Threshold => Configuration.Threshold);
         begin
            for Fragment of Fragments loop
               Put_Line (Points.Trim (Points.Image (Fragment)));
            end loop;
         end;

      when Configuration.Decode =>
         raise Program_Error with "Unimplemented";

      when Configuration.Help =>
         Put_Line (Standard_Error, Configuration.Help_Text);

   end case;

   Set_Exit_Status (Success);

exception
   when E : Configuration.Bad_Command_Line =>
      Put_Line (Ada.Exceptions.Exception_Message (E));

      Put (Standard_Error, Configuration.Help_Text);
end Secret_Sharing.Main;
