
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

   with function Is_Unit (X : Euclidean_Ring) return Boolean is <>;
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

   procedure Inv_Mod (X       : Euclidean_Ring;
                      Modulus : Euclidean_Ring;
                      Inverse : out Euclidean_Ring;
                      Is_Unit : out Boolean)
     with
       Pre => X /= Zero,
       Post =>
         (not Is_Unit) or else
         ((X * Inverse + (- One)) mod Modulus = Zero);

   function Is_Unit_Mod (X       : Euclidean_Ring;
                         Modulus : Euclidean_Ring)
                         return Boolean;


   function Inv_Mod (X       : Euclidean_Ring;
                     Modulus : Euclidean_Ring) return Euclidean_Ring
     with
       Pre =>
         Is_Unit_Mod (X, Modulus),
         Post =>
           (X * Inv_Mod'Result + (- One)) mod Modulus = Zero;


end Generic_GCD;
