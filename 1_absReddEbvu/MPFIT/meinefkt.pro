FUNCTION meinefkt, X, P
  RETURN, P[0] + GAUSS1(X, P[1:3])
END
