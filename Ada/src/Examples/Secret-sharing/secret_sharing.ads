with Gf_2p_Varsize;
with Generic_Matrices;
with Generic_LUP;

--
--  Root package under which the whole application is organized.
--  Why having the whole application (main included) under a single
--  package?  This is not mandatory, but personally I find it a convenient
--  structure.  For example, the package implementing the finite field used
--  in the code is instantiated here and it is visible by every child unit.
--
package Secret_Sharing is
   type Secret_Type is mod 2 ** 32;

   package Field is
     new Gf_2p_Varsize (Exponent => Secret_Type'Size);

   use type Field.Galois;

   package Mtx is
     new Generic_Matrices (Ring_Type => Field.Galois,
                           Ring_Zero => Field.Zero,
                           Ring_One  => Field.One);

   package LUP is
     new Generic_LUP (Field_Type => field.Galois,
                      Zero       => Field.Zero,
                      One        => Field.One,
                      Inv        => field.Inv,
                      Matrices   => Mtx);
end Secret_Sharing;
