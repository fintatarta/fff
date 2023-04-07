--                              -*- Mode: Ada -*-
--  Filename        : gf_2p.ads
--  Description     : Generic package for GF(2^n), n <= 64
--  Author          : Riccardo Bernardini
--  Created On      : Thu Nov 15 17:01:13 2007
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : <TESTED> for Exp in 1..32
--  Type            : <GENERIC>
--

--
-- Typical usage
--
--    package GF256 is new GF_2p(8);
--    use GF256;
--
--    A, B : Galois;
--

generic
   Exponent : Positive;
   -- To work with GF(2^p) use Exponent=p

   pragma Compile_Time_Error (Exponent > 32,
                              "Exponent cannot be larger than 32");


   Small_Footprint : Boolean := False;
   -- If Small_Footprint is True, use algorithms which are slower, but
   -- require less memory.  If Small_Footprint is False, try to use
   -- the faster algorithm (but fall back to the slower ones if
   -- Exponent > 16)
package Gf_2p_Varsize is
   type Galois is private;

   Zero : constant Galois;
   One  : constant Galois;

   type Size_Type is mod 2 ** 64;

   Size : constant Size_Type := 2 ** Exponent;
   Name : constant String;

   function "+" (Left, Right : Galois) return Galois;
   function "-" (Left, Right : Galois) return Galois;
   function "-" (X : Galois) return Galois;

   function "*" (Left, Right : Galois) return Galois;
   function "/" (Num, Den : Galois) return Galois;
   function Inv (X : Galois) return Galois;

   function Image (X : Galois) return String;
   -- Equivalent to Integer'Image(To_Int(X))

   function Is_Unit (X : Galois) return Boolean;
   -- Return True if X is non-zero

   generic
      type Int_Type is mod <>;
   function To_Int (X : Galois) return Int_Type;

   generic
      type Int_Type is mod <>;
   function To_Galois (X : Int_Type) return Galois;
private
   type Basic_Int is  mod 2 ** 64;
   type Galois is new Basic_Int;

   Zero : constant Galois := 0;
   One  : constant Galois := 1;

   Name : constant String   := "GF(2^" & Integer'Image (Exponent) & ")";
   --  Size : constant Interfaces.Unsigned_64 := 2 ** Exponent;
end Gf_2p_Varsize;
