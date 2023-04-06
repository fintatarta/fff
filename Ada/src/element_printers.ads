with Ada.Containers.Indefinite_Holders;

--
--  If we want to print some object that depends on an "element type"
--  (say, polynomials with coefficients in a ring, or matrices, ...)
--  we need a way to print the single elements (i.e., the coefficients of
--  the polynomial or the entries of the matrix).
--
--  There are two possible solutions to this problem: a callback (Image-like)
--  that converts an element into a string or a tagged type exporting
--  an Image function.  The former is simpler, the latter is more general
--  and flexible since the printer can have an internal status.
--
--  This package provides the minimal scaffolding to use both solutions.
--
generic
   type Element_Type is private;
package Element_Printers is
   -- Callback type
   type Printer_Function is not null access
     function (El : Element_Type) return String;

   -- Printer interface: only a Image method is required
   type Printer_Interface is interface;

   function Image (Printer : Printer_Interface;
                   Item    : Element_Type)
                   return String
                   is abstract;

   type Printer_Access is access all Printer_Interface'Class;

   --  Holders can be useful to store default printers in packages
   package Printer_Holders is
     new Ada.Containers.Indefinite_Holders (Printer_Interface'Class);

   -- Convert a callback to a printer object
   function Callback (Fun : Printer_Function) return Printer_Interface'Class;

   --  Convert a callback to a printer object and envelope it in an holder.
   --  Just a bit of syntactic sugar
   function Callback (Fun : Printer_Function) return Printer_Holders.Holder;

end Element_Printers;
