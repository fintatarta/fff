function y = invmod(x, M)
 %
 %  Y=INVMOD(X, M)
 %
 % Return the inverse of X modulo M, that is, Y is such that
 % Y*X mod M = 1.  Y exists if and only if GCD(X,M)=1
 %

  [gcd, alpha, beta] = euclide_gcd(x, M);

  if gcd != 1
    error(sprintf('%d and %d are not mutually prime', x , M));
  end

  assert(alpha * x + beta * M == 1)

  y = mod(alpha, M);

  assert(mod(y * x, M) == 1)
  
  
