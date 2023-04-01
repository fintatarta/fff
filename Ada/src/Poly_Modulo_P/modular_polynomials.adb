pragma Ada_2012;
with Generic_GCD;

package body Modular_Polynomials is
   function Degree (X : Polynomial) return Natural
   is (To_Integer (Poly.Degree (X)));

   package Poly_Gcd is
     new Generic_GCD (Euclidean_Ring => Polynomial,
                      Zero           => poly.Zero,
                      One            => Poly.One,
                      "/"            => Div);

   -------------
   -- Is_Unit --
   -------------

   function Is_Unit (X : Polynomial_Mod) return Boolean
   is (Poly_Gcd.Is_Unit_Mod (X       => To_Polynomial(x),
                             Modulus => Modulus));
   ---------
   -- Inv --
   ---------

   function Inv (X : Polynomial_Mod) return Polynomial_Mod
   is ((P => Poly_Gcd.Inv_Mod (X => X.P, Modulus => Modulus)));

end Modular_Polynomials;
