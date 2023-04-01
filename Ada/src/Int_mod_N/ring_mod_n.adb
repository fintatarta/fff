pragma Ada_2012;
with Generic_GCD;
--  with Ada.Text_IO; use Ada.Text_IO;

package body Ring_Mod_N is
   function Degree (X : Integer) return Natural
   is (abs X);

   function Not_Zero (X : Integer) return Boolean
   is (X /= 0);

   package Gcd_Mod_N is
     new Generic_GCD (Euclidean_Ring => Integer,
                      Zero           => 0,
                      One            => 1,
                      Is_Unit        => Not_Zero);
   ---------
   -- Inv --
   ---------

   function Inv (X : Modular_Type) return Modular_Type
   is
      use Gcd_Mod_N;

      T : constant Integer := Inv_Mod (X       => Integer (X),
                                       Modulus => Modular_Type'Modulus);
   begin
      return Modular_Type (T mod Modular_Type'Modulus);
   end Inv;
   -------------
   -- Is_Unit --
   -------------

   function Is_Unit (X : Modular_Type) return Boolean
   is (Gcd_Mod_N.Is_Unit_Mod (X       => Integer (X),
                              Modulus => Modular_Type'Modulus));

end Ring_Mod_N;
