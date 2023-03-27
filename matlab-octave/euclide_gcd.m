function [gcd, alpha, beta] = euclide_gcd(a,b)
%
%
%     [GCD, ALPHA, BETA] = euclide_gcd(A,B)  
%
% Return (in GCD) the Greatest Common Divisor of A and B and 
% ALPHA and BETA such that GCD=A*ALPHA + B*BETA
%

 %
 % Check the arguments: they must be integer and not null
 %

  if nargin < 2
    help euclide_gcd
    return
  end

  if a == 0        || ...
     b == 0        || ...
     a != round(a) || ...
     b != round(b)
    error('A and B must be non-null integers')
  end

 %
 % Handle the easy case a==b
 % 
  if a == b
    alfa=1;
    beta=0;
    gcd = a;
    
    return
  end

  swap = [0 1
	  1 0];

  status = [1 0 a
	    0 1 b];

 %
 % Force A and B positive
 %
  
  if a < 0
    status = [-1 0
	      0 1] * status;
  end

  if b < 0
    status = [1 0
	      0 -1] * status;
  end

 %
 % Now force A > B
 %
  
  if a < b 
    status = swap * status;
  end

  assert((status(1, 3) > status(2, 3)) && ...
	 (status(2, 3) > 0));

  while status(2, 3) > 0
    m = mod(status(1, 3), status(2, 3));
    q = (status(1, 3) - m) / status(2,3);

    assert (q == round(q));  % Q must be integer

 %
 % Combine the second and first row of status in order to
 % replace status(1,3) with m
 %
    status = [1, -q
	      0,  1] * status;

 %
 % Swap the rows to force status(1, 3) > status(2, 3)
 %
    status = swap * status;

    assert(status(2, 3) == m);
    assert(status(1, 3) > status(2, 3))
    assert(all(status(:, 1:2)*[a;b] == status(:, 3)));
  end

  assert(all(status(:, 1:2)*[a;b] == status(:, 3)));
  assert(status(2,3)==0);

  gcd = status(1,3);
  alpha = status (1, 1);
  beta  = status (1, 2);

  assert(mod(a, gcd)==0 && mod(b, gcd)==0);
  assert(gcd == alpha*a + beta*b);
  
    
