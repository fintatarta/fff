pragma Ada_2012;
package body Element_Printers is

   type Callback_Based is
     new Printer_Interface
   with
      record
         Callback : Printer_Function;
      end record;

   function Image (Printer : Callback_Based;
                   Item    : Element_Type)
                   return String;

   function Image (Printer : Callback_Based;
                   Item    : Element_Type)
                   return String
   is (Printer.Callback (Item));

   function Callback (Fun : Printer_Function) return Printer_Interface'Class
   is (Callback_Based'(Callback => Fun));


   --------------
   -- Callback --
   --------------

   function Callback (Fun : Printer_Function) return Printer_Holders.Holder is
   begin
      return Printer_Holders.To_Holder (Callback_Based'(Callback => Fun));
   end Callback;

end Element_Printers;
