with Ada.Text_IO; use Ada.Text_IO;
with ada.Command_Line;
with Ada.Exceptions;

with Secret_Sharing.Configuration;
with Secret_Sharing.Points;

--
--  Main procedure.  In Ada any procedure can play the role of the main.
--  The language standard delegates to the development environment how
--  the main is specified; in the specific case of GNAT compiler, it is
--  specified in the project file.
--
procedure Secret_Sharing.Main is
   use Ada.Command_Line;
begin
   -- Be pessimistic, we will set Success at the end
   Set_Exit_Status (Failure);


   Configuration.Initialize;

   case Configuration.Action_Required is
      when Configuration.Encode =>
         declare
            Fragments : constant Points.Point_Array :=
                          Encode_Secret (Secret    => Configuration.Secret,
                                         N_Pieces  => Configuration.N_Pieces,
                                         Threshold => Configuration.Threshold);
         begin
            for Fragment of Fragments loop
               Put_Line (Points.Image (Fragment));
            end loop;
         end;
      when Configuration.Decode =>
         null;
   end case;

exception
   when E : Configuration.Bad_Command_Line =>
      Put_Line (Ada.Exceptions.Exception_Message (E));

      Put (Standard_Error, Configuration.Help_Text);
end Secret_Sharing.Main;
