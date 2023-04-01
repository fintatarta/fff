generic
   type Modular_Type is mod <>;
package Ring_Mod_N is
   function Inv (X : Modular_Type) return Modular_Type
     with
       Pre => Is_Unit (x);

   function Is_Unit (X : Modular_Type) return Boolean;

   function "/" (X, Y : Modular_Type) return Modular_Type
   is (X * Inv (Y))
     with
       Pre => Is_Unit (Y);
end Ring_Mod_N;
