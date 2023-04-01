with Generic_Polynomials;

generic
   type Field_Type is private;

   Field_Zero : Field_Type;
   Field_One  : Field_Type;

   with function "+" (X, Y : Field_Type) return Field_Type is <>;
   with function "-" (X : Field_Type) return Field_Type is <>;
   with function "*" (X, Y : Field_Type) return Field_Type is <>;
   with function Inv (X : Field_Type) return Field_Type is <>;

   with function "=" (X, Y : Field_Type) return Boolean is <>;

   with package Poly is
     new Generic_Polynomials (Field_Type => Field_Type,
                              Field_Zero => Field_Zero,
                              Field_One  => Field_One);

   Modulus : Poly.Polynomial;

package Modular_Polynomials is
   type Polynomial_Mod is  private;

   Zero : constant Polynomial_Mod;
   One : constant Polynomial_Mod;

   function To_Polymod (X : Poly.Polynomial) return Polynomial_Mod;

   function To_Polynomial (X : Polynomial_Mod) return Poly.Polynomial;

   function "+" (X, Y : Polynomial_Mod) return Polynomial_Mod;
   function "-" (X : Polynomial_Mod) return Polynomial_Mod;

   function "-" (X, Y : Polynomial_Mod) return Polynomial_Mod
   is (X + (-Y));

   function "*" (X, Y : Polynomial_Mod) return Polynomial_Mod;

   function Is_Unit (X : Polynomial_Mod) return Boolean;

   function Inv (X : Polynomial_Mod) return Polynomial_Mod
     with
       Pre => Is_Unit (X);

   function "/" (X, Y : Polynomial_Mod) return Polynomial_Mod
   is (X * Inv (Y))
     with
       Pre => Is_Unit (Y);
private
   use Poly;
   type Polynomial_Mod is
      record
         P : Poly.Polynomial;
      end record;



   Zero : constant Polynomial_Mod := (P => Poly.Zero);
   One : constant Polynomial_Mod := (P => Poly.One);

   function To_Polymod (X : Poly.Polynomial) return Polynomial_Mod
   is (Polynomial_Mod'(P => X));

   function To_Polynomial (X : Polynomial_Mod) return Poly.Polynomial
   is (X.P);

   function "+" (X, Y : Polynomial_Mod) return Polynomial_Mod
   is ((P => X.P + Y.P));

   function "-" (X : Polynomial_Mod) return Polynomial_Mod
   is ((P => -X.P));


   function "*" (X, Y : Polynomial_Mod) return Polynomial_Mod
   is ((P => (X.P * Y.P) mod Modulus));


end Modular_Polynomials;
