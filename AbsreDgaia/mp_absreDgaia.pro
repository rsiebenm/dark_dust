; README of mp_absreDgaia, ps=ps, no_mpfit=no_mpfit
;  
; ----------------------------------------------------------------- 
; IDL (GDL) Program : mp_absreDgaia -
; Fits absolute Reddening curve with cross-section of Dark Dust (DD)
; Model using chi2 fitter mpfit.pro compuitng Av from given
; trigionometric distance Dgaia, V, E(B-V) and Mv of the star.
;
; 1) Externals:
;   a)   ./MPFIT/mpfit.pro : MPFIT IDL fitting package
;   b)    fkt_absredd      : External function of mpfit() -- radii fixed
;   c)   ./a.crossec_Ebv   : Executable of crossec_Ebv.f (called by fkt_absredd*pro)
;   d)   ck_abunMgd        : Check element abundance constraints are hold 
;                            and derive Gas/dust mass ratio.
;
; INPUT:
;   see File './Input/mk_Tab1.xdr', which includes:
;      Name of the stars, and Rv_ref, Av_ref, Ebv_ref from the reddening curves
;      inspected by Siebenmorgen et al,2023, A&A, 676, 132.
;
;      Start parameters (generic) : ./Input/jsm,12fitStart.inp
;      Parameters of dust model read by a.crossec_dd when fitting the
;      reddening curve. One may use default parameters as in
;      crossec_dd.f, (mpfit convergence is accelerated when using
;      'HD*jsm12fit.inp')
; Keywords:
;   ps: plot reddening curve and fit to pl_allRedd.pdf
;   no_mpfit: 1=nofit 0=mpfit is used (default)
;
;
; OUTPUT:
;   './Result/'+ target+'_Kappa.out'    : Abs, sca cross sections K
;   './Result/'+ target+'_PolKappa.out' : Polarization not in use here
;   './Result/'+ target+'_jsm12fit.inp' : Update of 2.2
;   './Result/'+ target+'_PAH2170.wq'   :
;   './Result/'+ target+'_para.xdr'     : 
;   './Result/'+ target+'_message.out'  : Messages of final a.crossec_dd 
;   idl.ps : PS file showing data and fti of the reddening curve
;
; HD*para.xdr: idl saveset which includes the varoious variables such
; as input and best fit model parameters, the relative reddening curve
; E(w-V)/E(B-V) of the star that shall be fit. (Variables: wdata,
; data, with rms error edata; E(B-V)=ebv of the reddening curve paper
; and Rv=Rv_mod of the model fit (returned by a.crossec_Ebv) and
; reduced chi2 (divided by dot).
;
; Parallelization is useful when starting fitting a larger sample.
; The vectorization of the code can be done separating the number
; of stars into different threads. 
;
; Libraries: IDL (GDL start "idl istart" or set:
; !PATH = './AAstron/:' + !PATH
; !PATH = './Coyote/:'  + !PATH
; !PATH = './MPFIT/:'   + !PATH
;
; Example set 
;   target  = 'HD027778' & Vmag = 6.327 &
;   Ebv_ref = 0.391      &  Mv  = -1.52 &  Dgaia   = 210.3
;   mp_absreDgaia,  target, Vmag, Ebv_ref, Mv_mag, Dgaia, /ps
;
; ============================================================
Function fkt_absredd, wdata, para

; help function for mpfit, 9 fit parameters of dark dust (DD) model: 
; 0     1       2        3         4     5     6     7               8
; abuc, abuvsi, abucvgr, abucpahs, qmrn alec, alesi, arad_polmin_aC, arad_polmin_Si
; 1) For alec,alesi, arad_polmin* fixed parameters
; 2) For alec,alesi, arad_polmin* feee  parameters (interpol.)
; ------------------------------------------
;
; Get parain Check which parameter is free or constant
restore, './Input/tmp_parain.xdr'

; Polarisation data
;   readcol, './Data/poldata.tab',  wpd, Int, dInt, Qint, dQint, nQ, $
;         Uint, dUint, nU, Pd, epd,theta, dtheta, skip=0, /sil
;     npd = n_elements(pd)   

;
; reads previous parameters: alec and alesi
readcol, './Input/jsm12fit.inp', /sil, skipline=1, numli=1,  $
         abuc, abusi, abuvsi, abucvgr, abucpahs, $
         qmrn, alec,  alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, arad_Darkmax 
 qmrn    = qmrn(0)  > 2.
 qmrn    = qmrn(0) < 4.
 
abusi          = abusi(0) *1.d0
para           = para     *1d0
abuc           = para(0)  
abuvsi         = para(1) 
abucvgr        = para(2) 
abucpahs       = para(3) 
qmrn           = para(4)
alec           = para(5) 
alesi          = para(6)
arad_polmin_aC = para(7) < alec/1.05
arad_polmin_Si = para(8) < alesi/1.05


;abundance constraints [C]/[Si] < 5.25
if  para(0)        gt 5.25*(abusi + abuvsi) -abucvgr - abucpahs then begin
   print, 'abundance constraint not ok in absredd '
   print, '                          reset para(0) = ', abuc
    abuc    = abuc < 5.25*(abusi + abuvsi) -abucvgr - abucpahs
    para(0) = abuc
    print, '                          to    para(0) = ', para(0)
    
 endif


   x0             =   para(9)
   gam            =   para(10)   



; use org Drude parameters not those form redd fit !
   get_lun, iu
   openw,iu,'./Input/PAH2170.wq'
   printf, iu, 'x0 (mu)    gam : Drude Parameters at 2175AA bump as in reddening papers'
   printf,iu, x0, gam          ; used here considerung PAH 
   close, iu
   free_lun, iu


   
arad_polmax    = arad_polmax(0)
arad_Darkmax   = arad_Darkmax(0)
alec           = alec(0)
alesi          = alesi(0)
arad_polmin_aC = arad_polmin_aC(0)
arad_polmin_Si = arad_polmin_Si(0)

; set up radii:
narad          = 130
rr             = 6.e-7* 1.05^ findgen(narad)
alec           = alec            > min(rr)
alec           = alec            < max(rr)
alesi          = alesi           < max(rr)
alesi          = alesi           > min(rr)
arad_polmin_aC = arad_polmin_aC  > min(rr)
arad_polmin_Si = arad_polmin_Si  > min(rr)
arad_polmin_aC = arad_polmin_aC  < alec/1.05
arad_polmin_Si = arad_polmin_Si  < alesi/1.05

;
para(0) = para(0) 
para(5) = alec        
para(6) = alesi       
para(7) = arad_polmin_aC
para(8) = arad_polmin_Si
; check arad_polmin_* le alesi:
if para(7) ge para(5)/1.049  or para(8) ge para(6)/1.049 then begin
   print, ' arad_polmin_aC, alec '
   print,   arad_polmin_aC, alec
   print, ' arad_polmin_Si, alesi '
   print,   arad_polmin_Si, alesi
   print,   para(5:8)
   stop, ' arad_polmin* gt alec or alesisi ?'
endif

;
; ---------------------------------------------
; alec, alesi as fixed parameters:

 get_lun, iu
  openw,iu,'./Input/jsm12fit.inp'
printf, iu, '  abuc    abusi    avsi   avgr    apah     qmrn  rlec      rlesi     r_pmin_aC r_pmin_Si r_pmax    r_Darkmax '
printf, iu, format='(6f8.3, 6e10.3)', abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, $
        alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, arad_Darkmax 
  close, iu
free_lun, iu

spawn,   './a.crossec_Dgaia >  message.out'

; Reddening curve of model for radii alec, alesi
 readcol, './Output/tau4fit.out', wr, taum, reddm, absredd, skipline=1, /sil


  yfit = [absredd]     ; only redd fit
  
  return, yfit




end
;
; ====================================================================
;

Pro ck_abunMgd, target
;  
; For given star(=target) scale the relative C and Si-grain abundances
; of the different dust model populations by a single factor (:=xgd)
; so that the gas to dust mass ratio matches about standard value
;   1) Mgd ~125
;  and by respecting the following two condistions of the
; total abundance for carbon C and Si in dust grains are hold:
;  1) C :
; aC_min = 69 ppm < aCtd < aC_max = 160.ppm
;  2) Si : aSi_min = 18 ppm < aSitd < asi_max = 42. ppm
;  4) Verify that the Si/C element ratio of
; the total dust holds: aSitd/aCtd < 5.2 is (Siebenmorgen 2023 ;
; Hensley&Draine21 (HD21) If not print out a warning message.
;  
; Input:
;  variables for the given target are read from xdr-file
;  
; Note: The mass are relative (%) to the total dust masss of 1g.  The
; abundances are also relative and shall be scaled (scale) respecting
; the depletion ration (condition 2 and 3 as above ) given by HD21:
; 
; Notation: 
; aCmass,   abuc     : mass and abundance of C in amorphous caron grains
; pahmasss, abucpahs : mass and abundance of C in s = samll PAH
; pahmassb, abucpahb : mass and abundance of C in b = big PAH
; vgrmass,  abucvgr  : mass and abundance of C in very samll graphiyte
; Simass,   abusi    : mass and abundance of Si in amorphous;  silivcate 
; vsimass,  abuvsi   : mass and abundance of Si in v= very small =
;                       nano-silicate particles
  
; ------------- Some additonal information:
; Molecular weights:
; DH21:  Mg1.3 (Fe,Ni)0.3 SiO3.6 (ρ = 3.41 g cm−3)
; print, w_Mg*1.3 + w_Si + w_O * 3.6  + 0.3*w_Fe               ;
; wmolsi(X50A) = 100.4
; wmolvsi = wmolDH21 =  w_Mg*1.3 + w_Si + w_O * 3.6  + 0.3*w_Fe 
; Demyk: 97% X50A + 3% E20
; print, w_Mg + w_Si + 3. *w_O                ; wmolsi(X50A) = 100.4
; print, w_Mg*0.8 + 0.2*w_Fe + w_Si + 3. *w_O ; wmolsi(E20)  = 106.7  
; All Si in Siicate and rest of Elements:
;  Mg- number of Si, Fe - 0.2*Number of Si,  O- 3*number of Si
; -----------------
; N(H+H2) = const. * E(B-V) with const [1d21H/cm2/mag]:
; Copernicus (Bohlin78),      = 5.8 but seee new references
; =====================================================================
;
; Constants 

  
  aC_min     = 60.5  ; minimum total C  abundance 
  aSi_max    = 42.   ; maximum total Si abundance 
  

; absolute abundances (abu_) and moleculate weights (w_) of depleted
; elements of C, O, Al, Mg, S, Ca, Fe, Ni as gives by HD21:

abu_C  =  126.      & w_C  =  12.  ;(=wmolC)
abu_O  =  250.      & w_O   = 16  & w_H2o = 18.
abu_Al =   3.4      & w_Al  = 27.
abu_Mg =   46.      & w_Mg  = 24.3
abu_Si  =  38.      & w_Si  = 28.09
abu_S  =   7.6      & w_S   = 32.06
abu_Ca =   3.2      & w_Ca  = 40.08
abu_Fe =  43.       & w_Fe  = 55.85
abu_Ni =  2.0       & w_Ni  = 58.69


print, '; Using Demyk mix with 5% porosity'
; molecular weight of amorphous silicates
wmolsi  = 100*0.97 + 99.33 * 0.03   

; molecular weight of nominal silicate rgain mix by HD21. Here used
; for nano silicates and Si in dark dust (d) 
wmolvsi = 134.5
;molecular weight of carbon
wmolC   = 12.

; densities for amorphouse silicates, nano-silicates, the latter
; also used for dark dust:
rhsi    = 2.58
rhvsi   = 3.5

; density for amorphouse carbon
rhc     = 1.6

; density for dark dust particles
rhd     = 1.75

; abundance of big PAH: 
abucpahb   = 0.

; abundances ratio in submicro grains (= dark dust) abuc/abusi =
abu_aCSi  = 2.815

; Mass of C and Si in dark dust 
           Vsi      = 0.50 & Vc = 0.30
           Mdark_C  = Vc* rhC    *0.8
           Mdark_Si = Vsi* RHvsi *0.8

; ----------------------------
           print
           print, ' ==== Compute: specific masses and [C]/[H] and [Si]/[H]  and check against absolute abundances  constraints '


; read input parameters using the results after mpfit from xdr file:
           file  = findfile('./Result/'+target+'*.xdr', cou=nf)
  if nf ne 1 then stop, ' check results xdr file'
  restore,     file(0)
  ; convert to absolute reddening curve E(w) for later saving
   data    = data    * ebv_ref
   edata   = edata   * ebv_ref   


; ---------------------------
   mrn_mass  = Mmrn       ; relative dust masses of MRN      particles (nano+ amorphore grains) in 1g of dust
   dark_mass = Mdark      ; relative dust masses of dark dust                                   in 1g of dust
   ndust_l   = Mmrn       ; relative dust column density (ndust) of "l" = lagre grains (=MRN =nano-+amorphous grains)
   ndust_d   = Mdark      ; relative dust column density (ndust of d = dark dust particles
   

; 1) compute relative masses 
; total (t) for amorphous + nano silicates grains
       aSit              = abusi + abuvsi
; total (t) for amorphous carbon + very small graphite + small PAH 
       aCt               = abuc  + abucvgr  + abucpahs

       abuwmol  = aCt*wmolc + abusi*wmolsi  + abuvsi*wmolvsi  
       aCmass   = abuc     * wmolc  /abuwmol    
       pahmasss = abucpahs * wmolc  /abuwmol
       pahmassb = abucpahb * wmolc  /abuwmol
       vgrmass  = abucvgr  * wmolc  /abuwmol
       Simass   = abusi    * wmolsi /abuwmol
       vsimass  = abuvsi   * wmolvsi/abuwmol
       
; total(t) mass of MRN particles =  amorphous C + amorphous silicates
; + very small graphite + very small silicates + PAH (small+big)
       
       tmassMRN =  Simass+ aCmass+ vgrmass+vsimass+ (pahmasss+pahmassb)
       if abs(tmassMRN-1.) gt 1.e-3 then stop, ' wrong tmassMRN', tmassMRN

; mass in MRN (also called large grains "_l") and mass in dark dust ("_d)particles        
        mrn_mass = ndust_l /(ndust_l  + ndust_d)
       dark_mass = ndust_d /(ndust_l  + ndust_d)

       
       aCmass   = aCmass   * mrn_mass
       pahmass  = pahmasss * mrn_mass
       vgrmass  = vgrmass  * mrn_mass
       Simass   = Simass   * mrn_mass
       vsimass  = vsimass  * mrn_mass
       tmassMRN = Simass+ aCmass+ vgrmass+vsimass+ pahmass
       if abs(tmassMRN-mrn_mass) gt 1.e-3 then $
          stop, ' wrong mrn_mass', tmassMRN
       if abs(tmassMRN+dark_mass-1.) gt 1.e-3 then $
          stop, ' wrong tmassMRN', dar_mass, tmassMRN 


; Mass in large grains

       Mmrn     = (wmolc*aCt + abusi * wmolsi + abuvsi   * wmolvsi) 
       Mdark    = Mmrn *  ndust_d /(ndust_l  + ndust_d)
       scale    = Mdark / (Mdark_C + Mdark_Si)


       
; relativ abundances of C und Si in Dark dust (d)
acd  = scale * Mdark_C / wmolc     
aSid = scale * Mdark_Si /wmolvSi   

; relativ abundances of C und Si in total (t) dust model (sum of all dust populations) 
actd   = acT  + acd
asitd  = aSiT + asid

; Rest dust: other dust materials ar_X = "abundance rest of Element X"
; Mg- Anzahl Si, Fe - 0.2*Anzahl Si,  O- 3* Anzahl Si 
           ar_Mg = abu_MG - (asitd -abusi)        - abusi*0.945
           ar_Fe = abu_Fe - (asitd -abusi)*0.3    - abusi*0.03*0.2
           ar_O  = abu_O  - 3.6 * (asitd - abusi) - 3.* abusi
           Mrest = ar_MG * w_Mg + ar_Fe * w_Fe + ar_O*w_O + abu_Al * w_Al + abu_S  * w_S + abu_Ca * w_Ca+ abu_Ni * w_Ni

; Gas/Dust mass ratio:
       Mtot   = Mmrn + Mdark + Mrest
       Mgd    = 1.3e6/Mtot


;  if iscale > 0 then do terartive rescaling of Gas/Dust mass rstio
;  using factor xgd to closely match standard value of Mgd_std=125 as
;  derived by Draine&Hensley2003, Siebenmorgen2003

          
         Mgd_std    = 125.   
         xgd        = Mgd /Mgd_std


         print, ' Iter Mgd       xgd    abuc    abusi   aCtd,   aSitd'
         
   for iter = 0, 11 do begin 
       if abs(xgd -1.) gt 0.005 and acTD gt aC_min and aSitd le aSi_max then begin
              
         xgd       = Mgd /Mgd_std         
         abusi     = abusi      * xgd
         abuvsi    = abuvsi     * xgd
         abucvgr   = abucvgr    * xgd
         abucpahs  = abucpahs   * xgd
         abuc      = abuc       * xgd

         aSit              = abusi + abuvsi
         aCt               = abuc  + abucvgr  + abucpahs
; Mass in amorphous grains
         Mmrn  = (wmolc*aCt + abusi * wmolsi + abuvsi   * wmolvsi) 
         Mdark = Mmrn *  ndust_d /(ndust_l  + ndust_d)
         scale    = Mdark / (Mdark_C + Mdark_Si)

; in Dark dust  relativen Haufigkeiten von C und Si:
         acd  = scale * Mdark_C / wmolc     
         aSid = scale * Mdark_Si /wmolvSi   

; in total dust model relativen Haufigkeiten von C und Si:
         actd   = acT  + acd
         asitd  = aSiT + asid


; Rest dust: other dust materials ar_X = "abundance rest of Element X"
; Mg- Anzahl Si, Fe - 0.2*Anzahl Si, O- 3* Anzahl Si a la
; stochiometrie D&H21, nominal composition: Mg1.3(Fe,Ni)0.3SiO3.6
         
          ar_Mg = abu_MG - (aSitd -abusi)        - abusi*0.945
          ar_Fe = abu_Fe - (aSitd -abusi)*0.3    - abusi*0.03*0.2
          ar_O  = abu_O  - 3.6 * (asitd - abusi) - 3.* abusi
          Mrest = ar_MG * w_Mg + ar_Fe * w_Fe + ar_O*w_O + abu_Al * w_Al + abu_S  * w_S + abu_Ca * w_Ca+ abu_Ni * w_Ni
           
           Mtot = Mmrn + Mdark + Mrest
           Mgd = 1.3e6/Mtot

          print, format='(i5,6f8.2)', iter, Mgd, xgd, abuc, abusi, aCtd, aSitd
      endif 
    endfor  ; end abundance scaling 
           


;
; print, "abundance rest of Elements X=Mg, Fe, O"
if ( ar_MG lt 0 or ar_Fe lt 0 or ar_O lt 0) then print, 'check ar_MG, ar_Fe, ar_O:', ar_MG, ar_Fe, ar_O




; ---------------------------------------------
; alec, alesi as fixed parameters:

 get_lun, iu
  openw,iu,'./Input/jsm12fit.inp'
printf, iu, '  abuc    abusi    avsi   avgr    apah     qmrn  rlec      rlesi     r_pmin_aC r_pmin_Si r_pmax    r_Darkmax '
printf, iu, format='(6f8.3, 6e10.3)', abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, $
        alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled
  close, iu
free_lun, iu


; ===============================
; Save Fit results MPFIT consistent wiht abundances results :
; ===============================


print, ' Star      & C_tot & C_dd  &  C_ac & C_vgr & C_pah & Si_tot& Si_dd & Si_Si & Si_vSi& Mgas/Mdust ' 
print, format='(a10, a3 10(i5,a3), a5)', target, ' & ', nint(actd), ' & ', nint( acd), ' & ', nint( abuc), ' & ', $
       nint( abucvgr), ' & ', nint( abucpahs), ' & ', nint(asitd), ' & ', nint(asid), ' & ', nint( abusi), ' & ', nint( abuvsi), ' & ', nint(Mgd), ' \\'


print, ' in xdr save file =', './Result/'+ target+'_para.xdr' 
print, ' relative reddening E(x-V)/Ebv is saved'
; convert to relaive reddening curve E(w) 
    data = data    / ebv_ref      ; save relative reddening
   edata = edata   / ebv_ref      ; save relative reddening
     save, /xdr, filename='./Result/'+ target+'_para.xdr', target, redchi2, $ 
          abuc, abusi,  abuvsi,    abucvgr,    abucpahs,  acd, asid, actd, asiTD, Mgd, $
          qmrn, alec, alesi, aled,  arad_polmin_aC, arad_polmin_Si, arad_polmax, x0, gam, $
          Mmrn, Mdark, fdark, tauV, Ebv_ref,  Vmag, Mv, Dlum, $
          wredd_org, redd_org, eredd_org, wdata, data, edata, $ 
          w, sac, ssac, sasi, sssi, savgr, ssvgr, savsi, ssvsi, sapah, sad, ssd, st


end



;
; ===================================================================================================
;   Main code : mp_absreDgaia
; ===================================================================================================
;
Pro mp_absreDgaia,  target, Vmag, Ebv_ref, Mv, Dgaia, ps=ps, no_mpfit=no_mpfit
;
; Example of input parameters
;   target  = 'HD027778'
;  Vmag    = 6.327
;  Ebv_ref = 0.391
;  Mv      = -1.52
;  Dgaia   = 210.3
; -----------------------------------------
; no_mpfit >0: do not compute fit to reddening curve just plot result
  if not keyword_set(no_mpfit) then      no_mpfit = 0

window, 0, xsi=700, ysiz=700
   !p.thick=2
   !p.charsize =1.3
   !P.font = 7
 if keyword_set(ps) then begin
    !P.font = 0
    a = PSWINDOW(/cm)
    device, decomposed=0
    set_plot, 'PS'
    device, filename='idl.ps',BITS_PER_PIXEL=8, /color,  $
           XSIZE=a.xsize, YSIZE=a.ysize, XOFFSET=a.xoffset, YOFFSET=a.yoffset
 endif


; no_mpfit >0: do not compute fit to reddening curve just plot result
  if not keyword_set(no_mpfit) then      no_mpfit = 0
  if not keyword_set(Vmag)    then      Vmag     = -999.
  if not keyword_set(Mv)      then      Mv       = -999.

  Dlum              = -999.
     chi2           = -999.
  redchi2           = -999.
  dof               = -999.
  acd               = -999.
  asid              = -999.
  actd              = -999.
  asiTD             = -999.
  Mgd               = -999.


  narad             = 130
  rarad             = 6.e-7 * 1.05^ findgen(narad)  < 3.10d-4


; ================================= INPUT ===============

 
  tauv    = alog(10.)/2.5 * (Vmag - Mv +5. - 5.*alog10(Dgaia))
  Av      = 2.5/alog(10.) * tauv


;
; -----------------------------------------
; Reddening curve E(lambda) normalized to E(B-V) : read wavelengths
; and data (Siebenmorgen et al., 2023 Tab.4, AA676,132, 2023).
; OR read parameters and reddening curve  from idl saveset file fxdr:
;     fxdr = findfile('./StartParameters/'+target+'_para.xdr',   cou=nf)
;                  restore, fxdr

  
  readcol, './Input/waveRedd.grid', wdata, skip=1
  fredd = findfile('./Data_Redd2023_AA676_132_Tab4/'+target+'*.redd', cou=nfr)
  fredd = fredd(0)
     if nfr ge 1 then begin
       readcol, fredd, wredd_org, redd_org, err_up, err_low, skipl=1, /sil
       eredd_org     = abs(err_up-err_low)/2. > 0.05 * redd_org
       redd_org(0)   = -av/ebv_ref ; trigonometric distance (Eq.A1) instead of extrapolated Rv values
       data          = interpol( redd_org, 1./wredd_org, 1./wdata)
       edata         = interpol(eredd_org, 1./wredd_org, 1./wdata)   
;      edata(0)      = data(0) * 0.1   ;  test sys error (marginal impact)
       data(0)       = -av/ebv_ref
    endif else begin
       stop, " *** Provide reddening curve of your target, exampes provided in ./Data_Redd2023_AA676_132_Tab4"
     endelse 

; convert to absolute reddening curve E(w) and E(oo) < E(H,K) 
   data    = data    * ebv_ref
   edata   = edata   * ebv_ref   
   data(0) = data(0)  < min([data(1:2)])
   
; save jsmTaufit.inp read by a.crossec_Dgaia (Note: tauV not Av)
 get_lun, iu
 openw,iu,'./Input/jsmTaufit.inp'
 printf, iu, ' tauV, ebv = '
 printf,iu, -data(0)/1.0875, ebv_ref
 printf,iu, tauV, ebv_ref
 close, iu
 free_lun, iu



; 
; Start mpfit using default startparameters. In case of better guess
; uncomment next lines:
;  finp = findfile('./StartParameters/'+target+'_jsm12fit.inp', cou=nfi)
;  if nfi ne 1 then   stop, 'check  input files =',  finp
;  spawn, 'cp ' +finp(0)  + ' ./Input/jsm12fit.inp'

 
  spawn, 'cp  ./Input/jsm12fitStart.inp  ./Input/jsm12fit.inp' 
         

readcol, './Input/jsm12fit.inp', /sil, skipline=1, numli=1,  $
          abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, alec,  alesi, $
          arad_polmin_aC, arad_polmin_Si, arad_polmax, aled 
  qmrn    = qmrn(0)  > 2.
  qmrn    = qmrn(0)  < 4.
  alec    = alec(0)   < 3.501e-5
  alesi   = alesi(0)  < 3.501e-5
  aled    = aled(0)   < 3.10e-4

;  PAH Drude parameters to file using default or Gordon+09, etc. 
;  fpah = findfile('./StartParameters/'+target+'_PAH2170.wq',   cou=nfp)
;  if nfp ne 1 then   stop, 'check PAH files =', fpah
;  spawn, 'cp ' +fpah(0)   + ' ./Input/PAH2170.wq'
                               x0    = 4.60  & gam    = 1.
 if target eq 'HD093222' then  x0    = 4.57  & gam    = 0.57
 if target eq 'HD146285' then  x0    = 4.57  & gam    = 0.77


   x0org = x0 & gamOrg = gam
   get_lun, iu
   openw,iu,'./Input/PAH2170.wq'
   printf, iu, 'x0 (mu)    gam : Drude Parameters of PAH at 2175AA bump'
   printf,iu, x0,    gam        ; used here considerung PAH
   printf,iu, x0org, gamOrg     ; default
   close, iu
   free_lun, iu
   

 print
 print, ' ============= START target    =   ', target, '      ================ '
 print, ' Input start parameters of dust model used by a.crossec_Dgaia'
 print, ' abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled, x0, gam' 
 print, format='(6f8.3, 8e10.3)', abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, $
        alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled, x0, gam
 
  xtol        = 1d-4
  para        = [abuc(0), abuvsi(0), abucvgr(0), abucpahs(0), qmrn(0),    $
                 alec(0), alesi(0), arad_polmin_aC(0), arad_polmin_Si(0), $
                 x0, gam]  * 1.d0

delvar, parain
parain = replicate({fixed:0, limited:[1,1], limits:[0.D,0.D],mpside:2},11)
parain(0).limits(0) =   1.d0
parain(0).limits(1) = 160.0d0
parain(1).limits(0) =   1.0d
parain(1).limits(1) =  20.0d0
parain(2).limits(0) =   1.0d-2
parain(2).limits(1) =  40.0d0
parain(3).limits(0) =   1.0d-2
parain(3).limits(1) =  40.0d0
parain(4).limits(0) =   2.d0         ; qmrn
parain(4).limits(1) =   4.d0         ; 
parain(5).limits(0) =  min(rarad)    ; alec
parain(5).limits(1) =  2.5d-5
parain(6).limits(0) =  min(rarad)    ; alesi
parain(6).limits(1) =  2.5d-5  ; 
parain(7).limits(0) =  5.0d-6          ; arad_polmin_aC
parain(7).limits(1) =  2.5d-5  ; 
parain(8).limits(0) =  5.0d-6          ; arad_polmin_Si
parain(8).limits(1) =  2.5d-5  ; 
parain(9).limits(0) =  4.3           ; PAH x0
parain(9).limits(1) =  4.9
parain(10).limits(0)=  0.5           ; PAH gam
parain(10).limits(1)=  2.0

; Relative dust abundances are treated so that we can fix one of them
; and set abuSi :=15ppm.
 
print, '  MP2: free: abundance+qmrn, fixed: radii         '
print

   parain.fixed      = 0
   parain(5:8).fixed = 1
   save, filename='./Input/tmp_parain.xdr', parain

  if no_mpfit ge 1 then begin
     res = para
  endif else begin
   stat = -1 
   delvar, res
   res  = MPFITFUN('fkt_absredd', wdata, data, edata, $
                 para, PARINFO=parain, perror=rms_para, status=stat,    $
                 yfit=reddfit, bestnorm=chi2, dof=dof, xtol=xtol)
   redchi2     = chi2/dof
   para           = res
   
   rmsabuc           = rms_para(0)
   rmsabuvsi         = rms_para(1)
   rmsabucvgr        = rms_para(2)
   rmsabucpahs       = rms_para(3)
   rmsqmrn           = rms_para(4)
   rmsalec           = rms_para(5)
   rmsalesi          = rms_para(6)
   rmsarad_polmin_aC = rms_para(7)
   rmsarad_polmin_Si = rms_para(8)
   rmsx0             = rms_para(9)
   rmsgam            = rms_para(10)   
  endelse
  
   para(0)        = ((para(0) > parain(0).limits(0)) < parain(0).limits(1)) 
   para(1)        = ((para(1) > parain(1).limits(0)) < parain(1).limits(1)) 
   para(2)        = ((para(2) > parain(2).limits(0)) < parain(2).limits(1)) 
   para(3)        = ((para(3) > parain(3).limits(0)) < parain(3).limits(1)) 
   para(4)        = ((para(4) > parain(4).limits(0)) < parain(4).limits(1))
   para(5)        = ((para(5) > parain(5).limits(0)) < parain(5).limits(1))
   para(6)        = ((para(6) > parain(6).limits(0)) < parain(6).limits(1)) 
   para(7)        = ((para(7) > parain(7).limits(0)) < parain(7).limits(1))
   para(8)        = ((para(8) > parain(8).limits(0)) < parain(8).limits(1)) 
   para(7)        =   para(7) < para(5)
   para(8)        =   para(8) < para(6)
   para(9)        = ((para(9) > parain( 9).limits(0)) < parain( 9).limits(1))
   para(10)       = ((para(10)> parain(10).limits(0)) < parain(10).limits(1)) 

   abuc           =   para(0)
   abusi          =   abusi(0)*1d0
   abuvsi         =   para(1)
   abucvgr        =   para(2)
   abucpahs        =  para(3)
   qmrn           =   para(4)
   alec           =   para(5)
   alesi          =   para(6)
   arad_polmin_aC =   para(7)
   arad_polmin_Si =   para(8)   
   x0             =   para(9)
   gam            =   para(10)
   

 
get_lun, iu
openw,iu,'./Input/jsm12fit.inp'
printf, iu, '  abuc    abusi    avsi   avgr    apah     qmrn  rlec      rlesi     r_pmin_aC r_pmin_Si r_pmax    r_Darkmax '
printf, iu, format='(6f8.3, 6e10.3)', abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, $
        alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled 
close, iu
free_lun, iu

; use org Drude parameters not those form redd fit !
   get_lun, iu
   openw,iu,'./Input/PAH2170.wq'
   printf, iu, 'x0 (mu)    gam : Drude Parameters of PAH at 2175AA bump'
   printf,iu, x0, gam          ; change if ext papra shall be used
   printf,iu, x0org, gamorg    ; NOT used. default parameters or as in Gordon+09 
   close, iu
   free_lun, iu
   

  print, format='(6f8.3, 8e10.3)', abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, $
        alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled, x0, gam

 print
 print, ' MPfit is done for star = ', target
 print
 redchi2 = chi2/dof         ; reduced chi2
 res     = -9
 spawn,   './a.crossec_Dgaia | tee  message.out'
 spawn, 'grep Mass_mrn message.out', res 
 Mmrn     = float(strmid(res, 43,9))
 Mdark    = float(strmid(res, 55,9))
 fdark    = float(strmid(res, 67,9))
 Mmrn     = Mmrn(0) & Mdark=Mdark(0) & fdark = fdark(0)  
 print, 'star: Mmrn, Mdark, fdark'
 print, format='(a10,3f7.2)', target, Mmrn, Mdark, fdark
 

 
; Save MPFIT results and readig Kappa.out
rdfloat, './Output/Kappa.out', skipline=3, w, sac, ssac, sasi, $
        sssi, savgr, ssvgr, savsi, ssvsi, sapah, sad, ssd, st, /sil
print, ' in xdr save file =', './Result/'+ target+'_para.xdr' 
print, ' relative reddening E(x-V)/Ebv is saved'
   data = data / ebv_ref           ; save relatkive reddening

    save, /xdr, filename='./Result/'+ target+'_para.xdr',  target, redchi2, $ 
     abuc, abusi,  abuvsi,    abucvgr,    abucpahs,  qmrn, alec, alesi, aled, $
     arad_polmin_aC, arad_polmin_Si, arad_polmax, x0, gam, $
     Mmrn, Mdark, fdark, tauV, Ebv_ref, $
     wredd_org, redd_org, eredd_org, wdata, data, edata, $ 
          w, sac, ssac, sasi, sssi, savgr, ssvgr, savsi, ssvsi, sapah, sad, ssd, st
     data = data * ebv_ref           ; absulte reddening


; ---------------------- check  abundances and Mgas/Mdust ----------------------    
; check abundance constraints [C]/[Si] < 5.25
 if  abuc gt 5.25*(abusi + abuvsi) -abucvgr - abucpahs then stop, 'abundance constraint not ok after  mpfit'


   ck_abunMgd, target
 
   restore, './Result/'+ target+'_para.xdr'
   data = data * ebv_ref        ; absulte reddening
   
; -------------------------  PLOT and save  final fit result ---------

 res     = -9
 spawn,   './a.crossec_Dgaia | tee  message.out'
 spawn, 'grep Mass_mrn message.out', res 
 Mmrn     = float(strmid(res, 43,9))
 Mdark    = float(strmid(res, 55,9))
 fdark    = float(strmid(res, 67,9))
 Mmrn     = Mmrn(0) & Mdark=Mdark(0) & fdark = fdark(0)  & 
 print, 'star: Mmrn, Mdark, fdark'
 print, target, Mmrn, Mdark, fdark


; Kappa.out
rdfloat, './Output/Kappa.out', skipline=3, w, sac, ssac, sasi, $
        sssi, savgr, ssvgr, savsi, ssvsi, sapah, sad, ssd, st, /sil
  w   = w*1e4
  kvis  = (where(w le 0.548))(0) & kblue  = (where(w le 0.445))(0)
  Rv_mod      = st(kvis) / (st(kblue) - st(kvis))
 Ebv_mod      = 2.5/alog(10.) * (st(kblue) - st(kvis))
  Av_mod      = Rv_mod * Ebv_mod
 absredd_mod  = 2.5/alog(10.) * (st/st(kvis) -1.) * st(kvis) 

 print, ' w(kvis), w(kblue)     : ',  w(kvis), w(kblue)
 print, ' Ebv_ref, Ebv_mod, Rv_mod : ', ebv_ref, ebv_mod, Rv_mod


    salec  = strmid(string(nint(alec*1e7)),  5, 7) 
    salesi = strmid(string(nint(alesi*1e7)), 5, 7)
    saled  = strmid(string(nint(aled*1e7)),  4, 7) 
    spc    = strmid(string(nint(arad_polmin_aC*1e7)), 5, 7) 
    spsi   = strmid(string(nint(arad_polmin_si*1e7)), 5, 7) 

    tit    = 'c2=' + strmid(string(nint(100*redchi2)/100.),4,4)   + $
             ' R!DC!N= '  + salec+ ' R!DSi!N= ' + salesi + ' R!DD!N= ' + saled 
    
    yrange = [min([data-2.*edata])-0.2, max(data+edata)+0.2]

 plot, 1./w, absredd_mod, xrange=[-0.1,11.1], yrange=yrange, /xsty, /ysty, $
       xthick=4, ythick=4, /nodata
 
 oplot, 1./w, ((sac+ssac + sasi+sssi)/st(kvis)   -1.)* Av_mod, color=fsc_color('brown')
 oplot, 1./w, ((savsi+ssvsi+ sapah+savgr+ssvgr)/st(kvis) -1.)* Av_mod, color=fsc_color('forest green')
 oplot, 1./w, ((sad+ssd)/st(kvis)     -1.)            * Av_mod, thick=!P.thick+1
 oplot, 1./w, absredd_mod, thick=4, color=fsc_color('magenta')



 oploterror, 1./wdata, data, wdata*0, 0.1*abs(data) > 0.05, /nohat, $
             color=fsc_color('dark gray'), psym=3
 oplot, 1./wdata(1:*), data(1:*),  psym=plotsym(/circle,scale=1.01,color=fsc_color('gray'), /fill)
 oplot, 1./wdata(1:*), data(1:*),  psym=plotsym(/circle,scale=1.01)
; model if tauV from Dgaia:
 oplot, 1./wdata(0) *[1,1], [data(0), data(0)],  psym=plotsym(/box,scale=1.09,color=fsc_color('Charcoal'), /fill)
 oplot, 1./wdata(0) *[1,1], [data(0), data(0)],  psym=plotsym(/box,scale=1.1)

 
 oplot, 1./w, absredd_mod, thick=1, color=fsc_color('magenta')
 xyouts, 1, max([data*0.85]), target, charsize=1.5


 
; ======================
; Save Fit results 
; =================



print, ' in xdr save file =', './Result/'+ target+'_para.xdr' 
print, ' relative reddening E(x-V)/Ebv is saved'

data = data / ebv_ref           ; save relatkive reddening


save, /xdr, filename='./Result/'+ target+'_para.xdr',  target, redchi2, $ 
     abuc, abusi,  abuvsi,    abucvgr,    abucpahs, acd, asid, acTd, asiTD, Mgd, $
     Mmrn, Mdark, fdark, qmrn, alec, alesi, aled, $
     arad_polmin_aC, arad_polmin_Si, arad_polmax, x0, gam, $
     tauV, Ebv_ref,  wredd_org, redd_org, eredd_org, wdata, data, edata, $ 
     w, sac, ssac, sasi, sssi, savgr, ssvgr, savsi, ssvsi, sapah, sad, ssd, st


 
 spawn, 'cp ./Output/Kappa.out    ./Result/'+ target+'_Kappa.out'
 spawn, 'cp ./Output/PolKappa.out ./Result/'+ target+'_PolKappa.out'
 spawn, 'cp ./Input/jsm12fit.inp  ./Result/'+ target+'_jsm12fit.inp'
 spawn, 'cp ./Input/PAH2170.wq    ./Result/'+ target+'_PAH2170.wq'
 spawn, 'mv message.out           ./Result/'+ target+'_message.out'

   


if keyword_set(ps) then begin
   device, /close
   set_plot, 'X'
   device, decomposed=1
   print
   print
   print, ' Result of reddening fit in plot :  pl_allRedd.pdf'
   com =  ' ps2pdf                  idl.ps     pl_allRedd.pdf'
   spawn, com
   print, com + '     ->      DONE'
endif

print
print, ' mp_absreDgaia:        DONE '
print, ' ========================== '
print
end
