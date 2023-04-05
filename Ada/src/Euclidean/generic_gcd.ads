
--
--  This package implements the Euclidean algorithm for computing the
--  Greatest Common Divisor (GCD) in the specific case of Euclidean rings,
--  that is, rings that have a "degree" function compatible with the
--  product.
--
generic
   type Euclidean_Ring is private;

   Zero : Euclidean_Ring;  --  Neutral element of the sum
   One  : Euclidean_Ring;  --  Neutral element of the product

   -- The two operations of a ring
   with function "+" (X, Y : Euclidean_Ring) return Euclidean_Ring is <>;
   with function "*" (X, Y : Euclidean_Ring) return Euclidean_Ring is <>;

   -- Inverse of the sum
   with function "-" (X : Euclidean_Ring) return Euclidean_Ring is <>;

   --
   -- Division, remainder and degree.  They must satisty the following
   -- property: given two ring elements X and Y it must exist Q such
   -- that
   --
   --     X = Q*Y + (X mod Y)
   --
   --     degree(X mod Y) < degree (X)
   --
   -- Intutiively, X mod Y is the remainder of the division of X by Y
   -- and Q is the quotient.
   --
   -- The division X/Y returns Q. It can assume that X mod Y=0
   --
   with function "mod" (X, Y : Euclidean_Ring) return Euclidean_Ring is <>;
   with function "/" (X, Y : Euclidean_Ring) return Euclidean_Ring is <>;
   with function Degree (X : Euclidean_Ring) return Integer is <>;

   --
   -- Return True if X is an unit of the ring, that is, if X has
   -- a multiplicative inverse
   --
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

   --
   -- If X has a multiplicative inverse modulo Modulus, set Is_Unit to
   -- True and Inverse to the inverse of X, that is, Inverse is such that
   --
   --      X * Inverse = One + Q * Modulus
   --
   -- for some Q.
   --
   -- If X has no multiplicative inverse, set Is_Unit to False; the value
   -- of Inverse is unspecified.
   --
   procedure Inv_Mod (X       : Euclidean_Ring;
                      Modulus : Euclidean_Ring;
                      Inverse : out Euclidean_Ring;
                      Is_Unit : out Boolean)
     with
       Pre => X /= Zero,
       Post =>
         (not Is_Unit) or else
         ((X * Inverse + (- One)) mod Modulus = Zero);

   --
   -- Return true if X has an inverse modulo Modulus
   --
   function Is_Unit_Mod (X       : Euclidean_Ring;
                         Modulus : Euclidean_Ring)
                         return Boolean;


   --
   -- Return the multiplicative inverse of X modulo Modulus. If
   -- X has no inverse, Constraint_Error is raised
   --
   function Inv_Mod (X       : Euclidean_Ring;
                     Modulus : Euclidean_Ring) return Euclidean_Ring
     with
       Pre =>
         Is_Unit_Mod (X, Modulus),
         Post =>
           (X * Inv_Mod'Result + (- One)) mod Modulus = Zero;


end Generic_GCD;
