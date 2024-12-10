function fktRmean, rl, ru, q


; computes mean radius of MRN dust size distribution r^-q between
; rl = lower radiis, ru = upper radius
;  rm = f(rl, ru, q) using the mittelwertsatz.
; Example: 
;   ru = 4. &  rl = 0.22 &  q  = 2.4 ; => rm= 0.833
;   print, fktRmean(rl,ru,q)
;
  
rm = ((ru^(1-q) - rl^(1-q)) / (ru-rl) / (1-q) )^(-1./q)

return, rm
end
