pragma Ada_2012;
package body Generic_GCD is

   ---------
   -- GCD --
   ---------

   function GCD (A, B : Euclidean_Ring) return Euclidean_Ring is
      G, Alpha, Beta : Euclidean_Ring;
   begin
      Gcd (A     => A,
           B     => B,
           Alpha => Alpha,
           Beta  => Beta,
           Gcd   => G);

      return G;
   end GCD;

   ---------
   -- GCD --
   ---------

   procedure GCD
     (A     : Euclidean_Ring;
      B     : Euclidean_Ring;
      Alpha : out Euclidean_Ring;
      Beta  : out Euclidean_Ring;
      Gcd   : out Euclidean_Ring)
   is
      Status_Matrix : array (1 .. 2, 1 .. 3) of Euclidean_Ring :=
                        (1 => (One, Zero, A),
                         2 => (Zero, One, B));

      procedure Swap_Rows is
         procedure Swap (A, B : in out Euclidean_Ring) is
            Tmp : Euclidean_Ring;
         begin
            Tmp := A;
            A := B;
            B := Tmp;
         end Swap;
      begin
         for Col in Status_Matrix'Range (2) loop
            Swap (Status_Matrix (1, Col),
                  Status_Matrix (2, Col));
         end loop;
      end Swap_Rows;

      procedure Combine_Rows (Coeff : Euclidean_Ring) is
      begin
         for Col in Status_Matrix'Range (2) loop
            Status_Matrix (1, Col) :=
              Status_Matrix (1, Col) + Coeff * Status_Matrix (2, Col);
         end loop;
      end Combine_Rows;
   begin
      if A = Zero or B = Zero then
         raise Constraint_Error;
      end if;

      if Degree (Status_Matrix (1, 3)) < Degree (Status_Matrix (2, 3)) then
         Swap_Rows;
      end if;

      pragma Assert (Degree (Status_Matrix (1, 3)) >= Degree (Status_Matrix (2, 3)));

      while Status_Matrix (2, 3) /= Zero loop
         pragma Loop_Invariant
           (Degree (Status_Matrix (1, 3)) >= Degree (Status_Matrix (2, 3)));

         pragma Loop_Invariant
           (Status_Matrix (1, 1) * A + Status_Matrix (1, 2) * B = Status_Matrix (1, 3));

         pragma Loop_Invariant
           (Status_Matrix (2, 1) * A + Status_Matrix (2, 2) * B = Status_Matrix (2, 3));

         declare
            M : constant Euclidean_Ring :=
                  Status_Matrix (1, 3) mod Status_Matrix (2, 3);

            Quotient : constant Euclidean_Ring :=
                         (Status_Matrix (1, 3) + (-M)) / Status_Matrix (2, 3);
         begin
            pragma Assert (Degree (M) < Degree (Status_Matrix (2, 3)));

            pragma Assert
              (Status_Matrix (1, 3) = Quotient * Status_Matrix (2, 3) + M);

            Combine_Rows (-Quotient);

            pragma Assert (Status_Matrix (1, 3) = M);

            Swap_Rows;
         end;
      end loop;

      pragma Assert
        (Status_Matrix (1, 1) * A + Status_Matrix (1, 2) * B = Status_Matrix (1, 3));

      pragma Assert
        (Status_Matrix (2, 1) * A + Status_Matrix (2, 2) * B = Zero);

      Alpha := Status_Matrix (1, 1);
      Beta := Status_Matrix (1, 2);
      Gcd := Status_Matrix (1, 3);
   end GCD;

end Generic_GCD;
