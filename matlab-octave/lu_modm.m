function [L, U, P] = lu_modm(A, M)
 %
 %      [L, U, P] = lu_modm(A, M)
 %
% Compute the LUP decomposition in the field of the integers modulo M,
% that is, return L, U and P such that P*A = L*U where P is a
% permutation matrix, L is lower triangular and U upper triangular.  A
% must be square and not singular

  if size(A,1) != size(A,2)
    error('A must be a square matrix')
  end

  N = size(A,1);

  if ! (all(all(A == round(A))) && M == round(M))
    error('Entries of A and M must be integer');
  end

  if M <= 0
    error('M must be positive')
  end

  [U, actions] = to_upper_triangular(A, M)

  L = eye(size(A));
  
  while ~isempty(actions)
    act = actions(end);
    actions(end) = [];

    if ! act.is_permutation
      L = mod(inv_action(act).mtx * L, M);
    else
      if isempty(actions)
	P = act.mtx;
	break
      end
      
      Q = actions(end);
      actions(end) = [];

      if Q.is_permutation
	Q.mtx = act.mtx * Q.mtx;
	actions(end+1) = Q;
      else
	Q.mtx = act.mtx * Q.mtx * inv_action(act).mtx;
	actions(end+1) = act;
	actions(end+1) = Q;
      end
    end
  end

  U = mod(U, M);
  L = mod(L, M);

  assert(all(all(mod(L*U-P*A, M)==0)))
end

function [U, actions] = to_upper_triangular(A, M)
  N = size(A,2);
  U = A;
  actions = [ swap(1,1,N) ];
  
  for col=1:size(U,2)-1
    U = mod(U, M);

    S = force_diagonal_entry_non_null(U, col);

    if ~isempty(S)
      U = S.mtx * U;
      actions(end+1) = S;
    end


    U_inv = invmod(U(col, col), M);

    S = add_rows(col, -U_inv * U(col+1:end, col), N);

    U = S.mtx * U;
    actions(end+1) = S;
  end
end

function S = force_diagonal_entry_non_null(A, col)
  idx = find(A(col:end, col) != 0);
  
  if isempty(idx)
    error('A is singular');
  end

  idx = idx(1);
  
  if idx == 1
    S = [];
  else
    S = swap(col, col+idx-1);
  end
end

function S=swap(r, s, N)
  R = eye(N);
  
  tmp = R(:,r);
  R(:, r) = R(:, s);
  R(:, s) = tmp;

  S = struct('mtx', R, 'is_permutation', 1, 'arg1', r, 'arg2', s);
end

function S=add_rows(src, alpha, N)
  R = eye(N);

  assert(length(alpha) == N - src);

  R((src+1):N, src) = alpha(:);

  S = struct('mtx', R, 'is_permutation', 0, 'arg1', src, 'arg2', alpha);
end

function y = inv_action(act)
  if act.is_permutation
    y = act;
  else
    y = add_rows(act.arg1, -act.arg2, size(act.mtx,1));
  end
end

