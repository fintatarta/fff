--
-- This package implements the ring of integers modulo N.
-- Actually, Ada does most of the work here since it has
-- modular integer natively.  This package just provides few functions that
-- are used in the other packages of this library:
--
--  - Inv to compute the multiplicative inverse
--  - Is_Unit
--  - A division operator compatible with Inv
--
-- Indeed, the division for a modular type is just the integer division and
-- it does not take into account the "wrap around" nature of modular types
--
generic
   type Modular_Type is mod <>;
package Ring_Mod_N is
   function Inv (X : Modular_Type) return Modular_Type
     with
       Pre => Is_Unit (X),
       Post => X * Inv'Result = 1;

   function Is_Unit (X : Modular_Type) return Boolean;

   function "/" (X, Y : Modular_Type) return Modular_Type
   is (X * Inv (Y))
     with
       Pre => Is_Unit (Y);
end Ring_Mod_N;
