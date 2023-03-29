# Ada functions for finite fields

Under folder `src/` you will find several Ada packages with functions for working with finite fields.  Most of them are generic packages, parameterized by a type that implements a field or a ring (depending on the package).  For example, the initial part of `Generic_LUP` is

```ada
generic
   type Field_Type is private;

   Zero : Field_Type;
   One  : Field_Type;

   with function "+" (X, Y : Field_Type) return Field_Type is <>;
   with function "-" (X : Field_Type) return Field_Type is <>;
   with function "*" (X, Y : Field_Type) return Field_Type is <>;
   with function Inv (X : Field_Type) return Field_Type is <>;
--
-- Other stuff...
---
package Generic_LUP is 
-- 
-- Still other stuff...
--
```

Note how it is required that `Field_Type` has the neutral elements (`Zero` and `One`), the two operations (`"+"` and `"*"`) together with the inverses (`"-"` and `Inv`).
