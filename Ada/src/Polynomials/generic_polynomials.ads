with Ada.Containers.Vectors;

generic
   type Field_Type is private;

   Field_Zero : Field_Type;
   Field_One  : Field_Type;

   with function "+" (X, Y : Field_Type) return Field_Type is <>;
   with function "-" (X : Field_Type) return Field_Type is <>;
   with function "*" (X, Y : Field_Type) return Field_Type is <>;
   with function Inv (X : Field_Type) return Field_Type is <>;

   with function "=" (X, Y : Field_Type) return Boolean is <>;

package Generic_Polynomials is
   type Exponent_Type is range 0 .. Integer'Last;

   type Polynomial_Degree is private;

   Minus_Infinity : constant Polynomial_Degree;


   function "<" (X, Y : Polynomial_Degree) return Boolean;
   function ">" (X, Y : Polynomial_Degree) return Boolean
   is (Y < X);

   function "<=" (X, Y : Polynomial_Degree) return Boolean
   is (not (X > Y));

   function ">=" (X, Y : Polynomial_Degree) return Boolean
   is (not (X < Y));

   function Max (X, Y : Polynomial_Degree) return Polynomial_Degree
   is (if X < Y then Y else X);


   function "+" (X, Y : Polynomial_Degree) return Polynomial_Degree;

   function To_Degree (D : Natural) return Polynomial_Degree;

   function To_Integer (D : Polynomial_Degree) return Natural
     with
       Pre => D /= Minus_Infinity;

   function To_Exponent (D : Polynomial_Degree) return Exponent_Type
   is (Exponent_Type (To_Integer (D)))
     with
       Pre => D /= Minus_Infinity;

   type Polynomial is tagged private
     with Constant_Indexing => Value;

   Zero : constant Polynomial;
   One  : constant Polynomial;

   type Coefficient_Array is array (Natural range <>) of Field_Type;

   function To_Polynomial (C : Field_Type) return Polynomial;

   function To_Polynomial (C : Coefficient_Array) return Polynomial;

   function Value (P : Polynomial; N : Exponent_Type) return Field_Type;
   function Value (P : Polynomial; X : Field_Type) return Field_Type;


   function Constant_Part (X : Polynomial) return Field_Type;

   function Leading_Power (X : Polynomial) return Exponent_Type;

   function Leading (X : Polynomial) return Field_Type
     with
       Post => X (X.Leading_Power) = Leading'Result;

   function Monomial (C        : Field_Type;
                      Exponent : Exponent_Type)
                      return Polynomial
     with
       Post =>
         Exponent = To_Exponent (Monomial'Result.Degree) and
         Leading (Monomial'Result) = C;


   function Shift (P : Polynomial; Amount : Natural := 1) return Polynomial
     with
       Post => Shift'Result.Degree = P.Degree + To_Degree (Amount);

   procedure Shift (P : in out Polynomial; Amount : Natural := 1);

   function "+" (X, Y : Polynomial) return Polynomial
     with
       Post => "+"'Result.Degree <= Max (X.Degree, Y.Degree);

   function "-" (X : Polynomial) return Polynomial
     with
       Post => "-"'Result.Degree = X.Degree;

   function "-" (X, Y : Polynomial) return Polynomial
     with
       Post => "-"'Result.Degree <= Max (X.Degree, Y.Degree);

   function "*" (C : Field_Type; P : Polynomial) return Polynomial
     with
       Post => ("*"'Result.Degree =
                (if C /= Field_Zero then P.Degree else Minus_Infinity));


   function "*" (X, Y : Polynomial) return Polynomial
     with
       Post => "*"'Result.Degree = X.Degree + Y.Degree;

   function Is_Unit (X : Polynomial) return Boolean;
   function Inv (X : Polynomial) return Polynomial
     with
       Pre => Is_Unit (X),
       Post => X * Inv'Result = One;

   function Is_Zero (X : Polynomial) return Boolean;

   function Is_A_Constant (X : Polynomial) return Boolean;

   function "mod" (X, Y : Polynomial) return Polynomial
     with
       Pre => Y /= Zero,
       Post => "mod"'Result = Zero or else Degree ("mod"'Result) < Degree (Y);

   function Div (X, Y : Polynomial) return Polynomial
     with
       Pre => Y /= Zero;

   function "=" (X, Y : Polynomial) return Boolean;

   procedure Div_Mod (Num, Den  : Polynomial;
                      Quotient  : out Polynomial;
                      Remainder : out Polynomial)
     with
       Pre => Den /= Zero,
       Post =>
         (Degree (Remainder) < Degree (Den)) and
         Num = Quotient * Den + Remainder;


   function Degree (X : Polynomial) return Polynomial_Degree;

   function Image
     (X                 : Polynomial;
      Field_Image       : access function (X : Field_Type) return String;
      Var_Name          : Character := 'x';
      Exponent_Operator : Character := '^')
      return String;
private
   type Polynomial_Degree is range -1 .. Integer'Last;

   Minus_Infinity : constant Polynomial_Degree := -1;

   procedure Normalize (X : in out Polynomial);

   use type Ada.Containers.Count_Type;

   package Field_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Exponent_Type,
                                 Element_Type => Field_Type);

   type Polynomial is tagged
      record
         Coeffs : Field_Vectors.Vector := Field_Vectors.To_Vector (New_Item => Field_Zero,
                                                                   Length   => 1);
      end record
     with
       Type_Invariant =>
         not Coeffs.Is_Empty and
         (Coeffs.Length = 1 or else Coeffs.Last_Element /= Field_Zero);

   function Leading_Power (X : Polynomial) return Exponent_Type
   is (X.Coeffs.Last_Index);

   function To_Polynomial (C : Field_Type) return Polynomial
   is ((Coeffs => Field_Vectors.To_Vector (C, 1)));

   Zero : constant Polynomial := To_Polynomial (Field_Zero);

   One : constant Polynomial := To_Polynomial (Field_One);

   function "-" (X, Y : Field_Type) return Field_Type
   is (X + (-Y));

   function "/" (X, Y : Field_Type) return Field_Type
   is (X * Inv (Y));

end Generic_Polynomials;
