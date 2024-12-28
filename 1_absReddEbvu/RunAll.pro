;  mp_absreDEbuv, target, Ebv_ref, Vmag=Vmag,  Mv=Mv, /ps
; this computes Av(Ebv) by fitting the reddening curve calling mp_absreDEbvu
; with: 
; INPUT: 
;  target : name of the star 'HD027778'
;  Vmag    : V band mag
;  Ebv_ref : reddening in E(B-V) as provided in the redening curves
;  Mv      : absolute V band brightness of the star
; These input are provided in Table1 of S+24 and stored in  idl saveset:
; in file : './Input/mk_Tab1.xdr'
; Reddening curve E(lambda) normalized to E(B-V) : The data for high
; quality reddening curve sample by Siebenmorgen et al., 2023 Tab.4,
; AA676,132, 2023) are available './Data_Redd2023_AA676_132_Tab4/' and
; binned to unique for wavelengths grid. For stars withnew high
; quality reddening curves whcih are not available store them in
; similar format.
;
; PAH 2175AA bump: The PAH contribution to the extinction bump is a
; secondary parameter. We use default parameters, the inverse center
; wavelength x0=4.6/mu and width gam =1, of the Drude
; parameters. However sometimes the fit quality to the bump is
; slightly improved using those by Gordon+09. These are stored in
; files ./StartParameters/HD*PAH2170.wq'
;
; 
; OUTPUT: plot of reddening curve fit './Result/HD*.pdf' and as
; described in mp_absredd
;
; External:  ./a.crossec_Ebv (compile crossec_Ebv.f)
; Compile:   .r mp_absredEbvu            
; -------------------------------------------------
;


restore,/verb, './Input/mk_Tab1.xdr' 

   
nn    = n_elements(star)

for i = 0, nn-1 do begin

   target  = star(i)

;   target  = 'HD093222'
   
   if star(i) eq target then begin

;  if star(i) eq 'HD129557'  or
;     star(i) eq 'HD146285'  or
;     star(i) eq  'HD315023' then begin
      
   Vmag    = vk(i)
   Ebv_ref = av(i)/Rv(i)
   Mv_mag  = Mv(i)
   Dgaia   = Dp(i)
   
   mp_absreDEbvu, target, Ebv_ref, Vmag=Vmag,  Mv=Mv_mag, /ps
   spawn, 'mv pl_allRedd.pdf ./Result/'+target+'_redd.pdf'


   print, ' ------------------------ ', target
;   ck_abunMgd, target
   

   
 endif

endfor

print
print, ' ================= DONE:  all stars processed ========='
print

end
