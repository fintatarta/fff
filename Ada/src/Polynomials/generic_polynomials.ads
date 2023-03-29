with Ada.Containers.Vectors;

generic
   type Field_Type is private;

   Field_Zero : Field_Type;
   Field_One  : Field_Type;

   with function "+" (X, Y : Field_Type) return Field_Type is <>;
   with function "-" (X : Field_Type) return Field_Type is <>;
   with function "*" (X, Y : Field_Type) return Field_Type is <>;
   with function Inv (X : Field_Type) return Field_Type is <>;
package Generic_Polynomials is
   type Polynomial is tagged private
     with Constant_Indexing => Coefficient;

   Zero : constant Polynomial;
   One  : constant Polynomial;

   type Coefficient_Array is array (Natural range <>) of Field_Type;

   function To_Polynomial (C : Field_Type) return Polynomial;

   function To_Polynomial (C : Coefficient_Array) return Polynomial;

   function Coefficient (P : Polynomial; N : Natural) return Field_Type;

   function "+" (X, Y : Polynomial) return Polynomial;
   function "-" (X : Polynomial) return Polynomial;
   function "-" (X, Y : Polynomial) return Polynomial;

   function "*" (X, Y : Polynomial) return Polynomial;
   function Is_Unit (X : Polynomial) return Boolean;
   function Inv (X : Polynomial) return Polynomial
     with
       Pre => Is_Unit (X),
       Post => X * Inv'Result = One;

   function "mod" (X, Y : Polynomial) return Polynomial
     with
       Pre => Y /= Zero,
       Post => "mod"'Result = Zero or else Degree ("mod"'Result) < Degree (Y);

   function Div (X, Y : Polynomial) return Polynomial
     with
       Pre => Y /= Zero;


   procedure Div_Mod (X, Y      : Polynomial;
                      Quotient  : out Polynomial;
                      Remainder : out Polynomial)
     with
       Pre => Y /= Zero,
       Post =>
         (Remainder = Zero or else Degree (Remainder) < Degree (Y)) and
         X = Quotient * Y + Remainder;

   function Degree (X : Polynomial) return Natural
     with
       Pre => X /= Zero;


private
   procedure Normalize (X : in out Polynomial);

   use type Ada.Containers.Count_Type;

   package Field_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Natural,
                                 Element_Type => Field_Type);

   type Polynomial is tagged
      record
         Coeffs : Field_Vectors.Vector;
      end record
     with
       Type_Invariant =>
         not Coeffs.Is_Empty and
         (Coeffs.Length = 1 or else Coeffs.Last_Element /= Field_Zero);

   function To_Polynomial (C : Field_Type) return Polynomial
   is ((Coeffs => Field_Vectors.To_Vector (C, 1)));

   Zero : constant Polynomial := To_Polynomial (Field_Zero);

   One : constant Polynomial := To_Polynomial (Field_One);

   function "-" (X, Y : Field_Type) return Field_Type
   is (X + (-Y));

   function "/" (X, Y : Field_Type) return Field_Type
   is (X * Inv (Y));

end Generic_Polynomials;
