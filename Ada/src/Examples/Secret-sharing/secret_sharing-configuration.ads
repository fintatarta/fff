
with Secret_Sharing.Points;

package Secret_Sharing.Configuration is

   --
   --  Command line parsing procedure to be called at the beginning. Why
   --  a specific initialization function? (Ada allows to specify
   --  package initialization code) It is a matter of exception handling.
   --  If the command line is not correct, Bad_Command_Line is raised;
   --  if this were done in the package initialization block, it would not
   --  be possible to catch the exception and give a message to the user.
   --
   --  By calling it explicitely, we can catch the raised exception. In order
   --  to be sure that it is called before the rest of the package is
   --  accessed, we make use of contracts that query an internal "Initialized"
   --  flag.
   --
   procedure Parse_Command_Line
     with
       Pre => not Initialized,
       Post  => Initialized;

   Bad_Command_Line : exception;

   function Initialized return Boolean
     with
       Ghost;  -- Used only in contract


   --
   --  Return a help text for the user.  This could seem a bit out  of place
   --  here, but since this package is the one who knows the actual CLI
   --  syntax, it is reasonable to keep also the help line here.
   --
   function Help_Text return String;


   type Action_Type is (Encode, Decode, Help);

   function Action_Required return Action_Type
     with
       Pre => Initialized;

   -- The secret to be encoded
   function Secret return Secret_Type
     with
       Pre => Initialized and then
       Action_Required = Encode;

   -- The number of fragments to be produced
   function N_Pieces return Positive
     with
       Pre =>
         Initialized and then
         Action_Required = Encode;

   -- The number of  fragment required to reconstruct the secret
   function Threshold return Positive
     with
       Pre =>
         Initialized and then
         Action_Required = Encode;

   -- The list of points provided by the user
   function Pieces_Provided return Points.Point_Array
     with
       Pre =>
         Initialized and then
         Action_Required = Decode;
end Secret_Sharing.Configuration;
