pragma Ada_2012;
with Ada.Characters.Latin_1;

with Ada.Containers.Indefinite_Holders;
with ada.Command_Line;
with Ada.Characters.Handling;

package body Secret_Sharing.Configuration is
   use ada.Characters.Latin_1;

   type User_Requirement_Type (Action : Action_Type) is
      record
         case Action is
            when Encode =>
               Secret    : Secret_Type;
               N_Pieces  : Positive;
               Threshold : Positive;

            when Decode =>
               Pieces_Provided : Points.Point_Array;

            when Help =>
               null;

         end case;
      end record;

   package Requirement_Holders is
     new Ada.Containers.Indefinite_Holders (User_Requirement_Type);

   User_Requirements : Requirement_Holders.Holder :=
                         Requirement_Holders.Empty_Holder;
   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use Ada.Command_Line;
      use Requirement_Holders;

      procedure Help_Requested
        with
          Post => Initialized and Action_Required = help;

      procedure Parse_Encode_Command
        with
          Post => Initialized and Action_Required = Encode;

      procedure Parse_decode_Command
        with
          Post => Initialized and Action_Required = Decode;

      procedure Parse_Encode_Command is
      begin
         if Argument_Count /= 4 then
            raise Bad_Command_Line
              with "Wrong number of args to encode";
         end if;

         User_Requirements :=
           To_Holder ((Action          => Encode,
                       Secret          => Secret_Type'Value (Argument (4)),
                       N_Pieces        => Positive'Value (Argument (2)),
                       Threshold       => Positive'Value (Argument (3))));

         if User_Requirements.Element.N_Pieces < User_Requirements.Element.Threshold then
            raise Bad_Command_Line with "N. pieces < Threshold";
         end if;
      end Parse_Encode_Command;


      --------------------------
      -- Parse_Decode_Command --
      --------------------------

      procedure Parse_Decode_Command
      is
         Pieces_Provided : Points.Point_Array;
      begin
         if Argument_Count < 3 then
            raise Bad_Command_Line
              with "Wrong number of args to decode";
         end if;

         for I in 2 .. Argument_Count loop
            declare
               Arg : constant String := Points.Expand (Argument (I));
            begin
               if not Points.Is_Valid_Image (Arg) then
               raise Bad_Command_Line
                    with "Bad data point spec '" & Argument (I) & "'";
               end if;

               Pieces_Provided.Append (Points.Parse (Arg));
            end;
         end loop;

         User_Requirements :=
           To_Holder ((Action          => Decode,
                       Pieces_Provided => Pieces_Provided));
      end Parse_Decode_Command;


      procedure Help_Requested is
      begin
         User_Requirements := To_Holder ((Action => Help));
      end Help_Requested;
   begin
      if Argument_Count = 0 then
         Help_Requested;
         return;
      end if;

      declare
         use Ada.Characters.Handling;

         Command : constant String := To_Lower (Argument (1));
      begin
         if Command = "encode" or Command = "enc" or Command = "e" then
            Parse_Encode_Command;
            return;

         elsif Command = "decode" or Command = "dec" or Command = "d" then
            Parse_Decode_Command;
            return;

         elsif Command = "help" or Command = "h" or Command = "?" then
            Help_Requested;
            return;

         else
            raise Bad_Command_Line
              with "Unknown command '" & Command & "'";
         end if;
      end;
   end Initialize;

   ---------------
   -- Help_Text --
   ---------------

   function Help_Text return String
   is ("Usage : main encode n_pieces threshold secret"
       & LF
       & "OR main decode point1 point2 ... pointN");

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean
   is (not User_Requirements.Is_Empty);

   ---------------------
   -- Action_Required --
   ---------------------

   function Action_Required return Action_Type
   is (User_Requirements.Element.action);

   ------------
   -- Secret --
   ------------

   function Secret return Secret_Type
   is (User_Requirements.Element.Secret);

   --------------
   -- N_Pieces --
   --------------

   function N_Pieces return Positive
   is (User_Requirements.Element.N_Pieces);

   ---------------
   -- Threshold --
   ---------------

   function Threshold return Positive
   is (User_Requirements.Element.Threshold);

   ---------------------
   -- Pieces_Provided --
   ---------------------

   function Pieces_Provided return Points.Point_Array
   is (User_Requirements.Element.Pieces_Provided);

end Secret_Sharing.Configuration;
