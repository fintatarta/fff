
with Secret_Sharing.Points;

package Secret_Sharing.Configuration is

   --
   --  Initialization procedure to be called at the beginning. Why
   --  a specific initialization function? (Ada allows to specify
   --  package initialization code) It is a matter of exception handling.
   --
   procedure Initialize
     with
       Pre => not Initialized,
       Post  => Initialized;

   Bad_Command_Line : exception;

   function Help_Text return String;

   function Initialized return Boolean
     with
       Ghost;  -- Used only in contract

   type Action_Type is (Encode, Decode, Help);

   function Action_Required return Action_Type
     with
       Pre => Initialized;

   -- The secret to be encoded
   function Secret return Secret_Type
     with
       Pre => Initialized and then Action_Required = Encode;

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

   function Pieces_Provided return Points.Point_Array
     with
       Pre =>
         Initialized and then
         Action_Required = Decode;
end Secret_Sharing.Configuration;
