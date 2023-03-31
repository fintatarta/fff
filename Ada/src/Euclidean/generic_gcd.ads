
generic
   type Euclidean_Ring is private;

   Zero : Euclidean_Ring;
   One  : Euclidean_Ring;

   with function "+" (X, Y : Euclidean_Ring) return Euclidean_Ring is <>;
   with function "-" (X : Euclidean_Ring) return Euclidean_Ring is <>;
   with function "*" (X, Y : Euclidean_Ring) return Euclidean_Ring is <>;
   with function "mod" (X, Y : Euclidean_Ring) return Euclidean_Ring is <>;
   with function "/" (X, Y : Euclidean_Ring) return Euclidean_Ring is <>;

   with function Degree (X : Euclidean_Ring) return Integer is <>;
package Generic_GCD is
   function GCD (A, B : Euclidean_Ring) return Euclidean_Ring
     with
       Pre =>
         A /= Zero and B /= Zero,
       Post =>
         A mod Gcd'Result = Zero and
         B mod Gcd'Result = Zero;

   procedure GCD (A     : Euclidean_Ring;
                  B     : Euclidean_Ring;
                  Alpha : out Euclidean_Ring;
                  Beta  : out Euclidean_Ring;
                  Gcd   : out Euclidean_Ring)
     with
       Pre =>
         A /= Zero and B /= Zero,
       Post =>
         A mod Gcd = Zero and
         B mod Gcd = Zero and
         A * Alpha + B * Beta = Gcd;
end Generic_GCD;
