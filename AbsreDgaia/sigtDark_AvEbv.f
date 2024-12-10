      subroutine sigtDark_EbvAv

      include "crossec.com"

      
c     Computes the relative mass of nano-MRN and sub-micron dust using
c     E(B-V) and Av(Dgaia): Rel. mass of dust in 1g of aC, Si, pah, vgr,
c     vsi and Dark dust.  Output is the total extinction cross section
c     of larg+nano+dark + Dark dust : sigt* (cm^2/g_dust ISM)
c
        
       abuc_tot = abuc + abucvgr + abucpahs + abucpahb     
       abuwmol  = abuc_tot*wmolc + abusi*wmolsi  + abuvsi*wmolvsi


c
c     Find fdark to previous fdark_old iteration to del < 5%
c
       xnd    = 0.
       xnl    = 0.
       fdark  = max(fdark,0.001)
       fdark  = min(fdark,0.999)       
       del    = 1.
c     ibug   = 2
c     ===================== iterate to find consistent solution of tauV (Eq. A2) and tauV-tauB (Eq.A3)
c
      do iter = 1, 11
        if (del .gt. 0.05) then
          
           fdark_old = fdark
       
       aCmass   = (1.-fdark) * abuc     * wmolc  /abuwmol
       pahmasss = (1.-fdark) * abucpahs * wmolc  /abuwmol
       pahmassb = (1.-fdark) * abucpahb * wmolc  /abuwmol
       vgrmass  = (1.-fdark) * abucvgr  * wmolc  /abuwmol
       Simass   = (1.-fdark) * abusi    * wmolsi /abuwmol
       vsimass  = (1.-fdark) * abuvsi   * wmolvsi /abuwmol
       pahmass = pahmasss + pahmassb

       tmassMRN =  Simass+ aCmass+ vgrmass+vsimass+ (pahmasss+pahmassb)
       tmassDark= fdark/(1.-fdark) * tmassMRN

  
       if(ibug .ge. 2) then
          write(6,*)       '    abusi    abuc_tot abucaC     abugr   abuvsi  abucpahs  abucpahb '
          write(6,'(7f9.1)') abusi, abuc_tot,   abuc,    abucvgr,   abuvsi, abucpahs, abucpahb
          write(6,*)        ' acmass,   Simass,   vgrmass,  vsimass,  pahmass,  tmassDark '
          write(6,'(1p6e10.2)') acmass,   Simass,   vgrmass,  vsimass,  pahmass, tmassDark    
          if(abs(tmassmrn+tmassDark-1.) .gt. 0.001) stop 'tot Mass MRN=Drak ne 1 '
          write(6,'(a30,4f7.2)')  ' Mass of MRN, Dark, Mtot, fDark : ',
     $                              tmassmrn, tmassDark, tmassmrn+tmassDark, fDark
       endif
           

c
c   a2 for geometrical cross section and qrmn already reduced by 1 above
c   n(a) da = a^-(qmrn) da. 
c   en1c    = Proportionalitats-Konstante fur Grossenverteilung
c   ahc(l)  = geometrischer Querschnitt der Graphitkorner mit Radius
c             amrn(l) bez. auf 1 g IM, analog ahsi(l), ahd(l)


               
      en1C = 0. 
      do  L = Lac, Lec 
        en1C = en1C + amrn(L)**(3d0-qmrn)
      end do
      en1C = 3d0/pi4/rhc / en1C    * aCmass

      en1Si = 0.
      do  L = Lasi, Lesi 
        en1Si = en1Si + amrn(L)**(3d0-qmrn)
      end do
      en1Si = 3d0/pi4/rhsi / en1Si * Simass

      
     
      en1d  = 0.
      do  L = Lad, Led 
        en1d = en1d + adark(L)**(3d0-qmrn)
      end do
      en1d  = 3d0/pi4/rhd / en1d   * tmassDark

      do  L = Lac, Lec
        ahc(L) = en1C * pi * amrn(L)**(2d0-qmrn)
      end do

      do  L     = Lasi, Lesi
        ahsi(L)  = en1Si * pi * amrn(L)**(2d0-qmrn)
      end do

      do  L   = Lad, Led
         ahd(L) = en1d * pi * adark(L)**(2d0-qmrn)
c        write(6,'(1p3e9.2)') en1d, adark(l), ahd(l)
      end do


        call locat(adark, lrd, arad_polmin_aC, l)
        fak  = abs(adark(l)   /arad_polmin_aC -1.)
        fak1 = abs(adark(l+1) /arad_polmin_aC -1.)
        if(fak1 .lt. fak .and. l .lt. lrd-1) l  = l+1
        arad_polmin_aC = adark(l)

        call locat(adark, lrd, arad_polmin_Si, l)
        fak  = abs(adark(l)   /arad_polmin_Si -1.)
        fak1 = abs(adark(l+1) /arad_polmin_Si -1.)
        if(fak1 .lt. fak .and. l .lt. lrd-1) l  = l+1
        arad_polmin_Si = adark(l)
        
        call locat(adark, lrd, arad_polmax, l)
        fak   = abs(adark(l)   /arad_polmax -1.)
        fak1  = abs(adark(l+1)/arad_polmax -1.)
        if(fak1 .lt. fak .and. l .lt. lrd-1) l  = l+1
        arad_polmax = min(arad_polmax, adark(led))
        
c       write(6,'(a40,1x,1p2e10.3)') '  Min grain alignment radius (cm) aC, SI : ', arad_polmin_aC, arad_polmin_Si
c       write(6,'(a40,1x,1p2e10.3)') '  Max grain alignment radius (cm) used   : ', arad_polmax

              
      do        l   = Lac, Lec 
        do      k   = 1, mm
        qabs(k)     = qac(L,k)
        qsca(k)     = qsc(L,k)
        if(iblack .eq. 1)   qabs(k) = 1d0
        if(iblack .eq. 1)   qsca(k) = 1d0
        if(qabs(k) .eq.0.) stop 'qabs =0'
        sigs_aC(k)  = sigs_aC(k)  + ahc(L) * qsca(k)
        siga_aC(k)  = siga_aC(k)  + ahc(L) * qabs(k) 
        if(amrn(l).ge.arad_polmin_ac .and. amrn(l).le.arad_polmax) then
         sigp_aC(k)  = sigp_aC(k)  + ahc(L) * qpc(L,k)
         sigcp_aC(k) = sigcp_aC(k) + ahc(L) * qcpc(L,k)
        endif
c        write(71,'(1p3e12.3)') wel(k), amrn(l), ahc(L)*qpc(L,k)
      end do
      end do


      do          l = lasi, lesi
        do        k = 1, mm
        qabs(k)     = qasi(L,K)
        qsca(k)     = qssi(L,K)
        if(iblack .eq. 1)   qabs(k) = 1d0
        if(iblack .eq. 1)   qsca(k) = 1d0
        if(qabs(k) .eq.0.) stop 'qabs =0'
        sigs_Si(k)  = sigs_Si(k)  + ahsi(L) * qsca(k)
        siga_Si(k)  = siga_Si(k)  + ahsi(L) * qabs(k) 
        if(amrn(l) .ge. arad_polmin_Si .and. amrn(l) .le.arad_polmax) then
         sigp_Si(k)  = sigp_Si(k)  + ahsi(L) * qpsi(L,k)
         sigcp_si(k) = sigcp_si(k) + ahsi(L) * qcpsi(L,k)
        endif
      end do
      end do


       do          l = lad, led
        do         k = 1, mm
         qabs(k)     = qad(L,K)
         qsca(k)     = qsd(L,K)
         if(iblack .eq. 1)   qabs(k) = 1d0
         if(iblack .eq. 1)   qsca(k) = 1d0
         if(qabs(k) .eq.0.) stop 'qabs =0'
         sigs_d(k)   = sigs_d(k)  + ahd(L) * qsca(k)
         siga_d(k)   = siga_d(k)  + ahd(L) * qabs(k)
         if(adark(l) .ge. arad_polmin_aC .and. adark(l) .le.arad_polmax) then
          sigp_d(k)  = sigp_d(k)  + ahd(L) * qpd(L,k)
          sigcp_d(k) = sigcp_d(k) + ahd(L) * qcpd(L,k)
         endif
        end do
       end do

c
c ----------------------------------------------------------------------------
c            Optics of  Very Small Grains  (vsg)
c ----------------------------------------------------------------------------
c   abucvgr  = abundance of very small graphite in ppm.
c   abuvsi   = abundance of very small silicates in ppm.
c   material = 0 bedeutet:  vsg sind aus Graphit.
c   material = 1 bedeutet:  vsg sind aus Silicates
c   xatom    = No of C 
      if(ispecvsg .ne. 1) goto 75
      fL        = 0. 
      do  L = Lav , Lev
       fL = fL + avsg(L)**(3d0 - qvsg)
      end do
      fakgr = 3d0*vgrmass /pi4 /rhgr /fl
      faksi = 3d0*vsimass /pi4 /rhvsi / fL
      do  L = Lav, Lev
       ahvgr(L) = fakgr /avsg(l)**qvsg * pi*avsg(L)**2
       if(ibug.ge.2) write(166,'(a6,1p2e11.3)') ' vGr:', avsg(l),  ahvgr(l)
      end do
      do  L = Lav, Lev
       ahvsi(L) = faksi /avsg(l)**qvsg * pi*avsg(L)**2
       if(ibug.ge.2) write(166,'(a6,1p2e11.3)') ' vSi:', avsg(l),  ahvsi(l)
      end do
      do k    = 1, mm
       do  Lrad = Lav, Lev 
        siga_vgr(k) = siga_vgr(k) + ahvgr(Lrad) * qagr(lrad,k)
        sigs_vgr(k) = sigs_vgr(k) + ahvgr(Lrad) * qsgr(lrad,k)
        siga_vsi(k) = siga_vsi(k) + ahvsi(Lrad) * qavsi(lrad,k)
        sigs_vsi(k) = sigs_vsi(k) + ahvsi(Lrad) * qsvsi(lrad,k)
       end do
      end do
 75     continue

c ----------------------------------------------------------------------------
c      Optics of  P A Hs         (PAHs bestehen aus C  ==>  material = 0)
c ----------------------------------------------------------------------------
c hier:   xatom    = No of C  + H atoms
c

        if (ispecpah .eq. 0)   go to 1503
        
c      write(6,*)'***   P A H              ***'
      material  = 0
      zcpah   = zcpahs
      zhpah   = zhpahs
      xatom   = zcpah + zhpah
      abucpah = abucpahs
      arad    = sqrt(zcpah/1.2) * 1d-8
      write(6,*) '      small PAH: zcpah, zhpah'
      write(6,'(16x,2f7.1)')  zcpah, zhpah
      pahmass = (1.-fdark) * abucpah * wmolc  / (abuc_tot*wmolc + abusi *wmolsi  +
     $                                               abuvsi*wmolvsi)
      
      ahpah   = pahmass/(wmolc*zcpah*protm)  * pi*arad**2 
      ahpahs  = ahpah
       if(ibug.gt.1) write(166,'(a6,1p2e11.3)') ' PAH:', arad, ahpah

       call pah_wq

c     Qabs war bislang der WQ wird nun zu einer Effiziens C=pi a^2*Q umdefiniert
c      absorbtion cross section per gramm dust
       do        k  = 1, mm
        qabs(k)     = qabs(k) / pi / arad**2
        siga_pah(k) = qabs(k) * ahpah
        qabspahs(k) = qabs(k)
        siga_pahs(k)= siga_pah(k)
      end do

      if(ibug .gt. 1) then         
         write(6,*) pahmass, arad, ahpah
         write(36,*) ' wel(k), qabs(k), siga_pah(k) '
         write(36,'(1p3e10.2)') (wel(k), qabs(k), siga_pah(k), k=1,mm)
      endif
c
c   Big PAHs: Qabs war bislang der WQ wird nun zu einer Effiziens C=pi a^2*Q umdefiniert
c             absorbtion cross section per gramm dust

      if(zcpahb .gt. 10.) then 
       zcpah   = zcpahb
       zhpah   = zhpahb
       xatom   = zcpah + zhpah
       abucpah = abucpahb
       arad    = sqrt(zcpah/1.2) * 1d-8
            write(6,*) '      PAH cluster: zcpah, zhpah'
            write(6,'(16x,2f7.1)')  zcpah, zhpah
            
       pahmass = (1.-fdark) * abucpah * wmolc  / (abuc_tot*wmolc + abusi *wmolsi  +
     $                                               abuvsi*wmolvsi)
       ahpah   = pahmass/(wmolc*zcpah*protm)  * pi*arad**2 
       ahpahb  = ahpah

        call pah_wq

       do        k  = 1, mm
        qabs(k)     = qabs(k) / pi / arad**2
        siga_pah(k) = qabs(k) * ahpah
        qabspahb(k) = qabs(k)
        siga_pahb(k)= siga_pah(k)
       end do

       if(ibug .gt. 2) then         
         write(6,*) pahmass, arad, ahpah
         write(36,*) ' wel(k), qabs(k), siga_pah(k) '
         write(36,'(1p3e10.2)') (wel(k), qabs(k), siga_pah(k), k=1,mm)
       endif
      endif

 1503 continue
c
c     -------------------------------------------------------------
c     Cross section [cm^2/g-dust] -- Notation: t=total, a_= abs, s_=sca
c     sigt_l               refer to large + nano dust
c     sigt_d=siga_d+sigs_d refer to      dark    dust 
c     sigt total cross section requires computation of column densities
c     Nl, Nd.  The column densities are given by input 2 parameters, the
c     total optical depth tauV and the observed reddening Ebv_obs, where
c     tauV is estimated from the GAIA distance.
c     -------------------------------------------------------------

      

      do k         = 1,mm
       sigt_l(k)   = siga_aC(k)  + sigs_aC(k)   + 
     $               siga_Si(k)  + sigs_Si(k)   +
     $               siga_vgr(k) + sigs_vgr(k)  + 
     $               siga_vsi(k) + sigs_vsi(k)  + 
     $               siga_pahs(k)+ siga_pahb(k)
       sigt_d(k)   = siga_d(k)   + sigs_d(k)

      end do

      sigt   = sigt_l + sigt_d

c
c     open(unit=8, file='./Output/Kappa_AvEbv.out', form='formatted')
c      rewind 8
c      write(8,*) '# Optical depth (Siebenmorgen 2023, A&A 670A,115; SC 2023 doi: 10.48550/arXiv.2311.03310)'
c      write(8,*) '# wel(cm) sa_aC    ss_aC    sa_Si    ss_Si    sa_vgr   ss_vgr   sa_vsi   ss_vsi   s_pahS+B sa_Dark  ss_Dark sigt '
c      write(8,*) '# ------------------------------------------------------------------------------------------------------------- '
c      write(8,'(1p1e9.3,11e9.2, 1e10.3)') (wel(k), 
c     $   siga_aC(k),    sigs_aC(k),  siga_Si(k), sigs_Si(k), 
c     $   siga_vgr(k),  sigs_vgr(k), siga_vsi(k), sigs_vsi(k),
c     $   siga_pahS(k)+siga_pahB(k),   siga_d(k), sigs_d(k), sigt(k), k=1,mm)
c      close(8)
c
       
      tbv     = ebv_obs/1.0857d0  

      slb     = sigt_l(kblue) 
      slv     = sigt_l(kvis)  
      sdb     = sigt_d(kblue) 
      sdv     = sigt_d(kvis)  


c     cross section in V and B could be for dark dust identical hence:
      if (abs(sdb-sdv) .lt. 1.e-4) then
         rvd = sdv * 1e4
      else 
        rvd = sdv / (sdb-sdv)
      endif
        rvl = slv / (slb-slv)
      
      if(sdv .gt. 0.) xnd  = (tauv/sdv - rvl * tbv/sdv) / ( 1.d0 - rvl/rvd) ! nd aus Eq.5 in Eq.6 einsetzen und Rl,d einfuehren OK
      
      xnd  = max(xnd,0.)
      xnl  = tauv/slv - xnd  * sdv/slv   ! Eq,5
      xnl  = max(xnl,0.)

c ! zur Probe      
      xnd1 = tauv/sdv - xnl * slv/sdv    
      if(abs(xnd1/xnd -1.) .gt. 1.d-4 .and. xnd1 .gt. 0.01 ) then
         write(6,*) ' *** Check: cannot be nd ne nd1 ?'
         write(6,'(a30, 1p2e12.3)') ' xnd, xnd1 = ', xnd, xnd1
c        stop ' cannot be nd ne nd1 ?'
      endif

      
      if(ibug .ge. 2) then
       write(6,'(a40, 2f10.3)') ' slv, slv *xnl = ', slv, slv*xnl
       write(6,'(a40, 2f10.3)') ' sdv, sdv *xnd = ', sdv, sdv*xnd
       write(6,'(a40, 2f10.3)') ' slb, slb *xnl = ', slb, slb*xnl
       write(6,'(a40, 2f10.3)') ' sdb, sdb *xnd = ', sdb, sdb*xnd
       write(6,'(a40, 2f10.3)') ' tauv= sdv*xnd,slv*xnl ? ', tauv, slv*xnl+sdv*xnd 
       write(6,'(a40, 1f10.3)') ' obs Ebv/1.086 = tbv = ', tbv
      endif
      

c  optical depth = cross section (sig*)  times column (N*) 
         sigt_l    = sigt_l    * xnl
         siga_aC   = siga_aC   * xnl         
         sigs_aC   = sigs_aC   * xnl
         siga_Si   = siga_Si   * xnl
         sigs_Si   = sigs_Si   * xnl
         siga_vgr  = siga_vgr  * xnl
         sigs_vgr  = sigs_vgr  * xnl
         siga_vsi  = siga_vsi  * xnl
         sigs_vsi  = sigs_vsi  * xnl
         siga_pahS = siga_pahS * xnl
         siga_pahB = siga_pahB * xnl 
         siga_d    = siga_d    * xnd
         sigs_d    = sigs_d    * xnd
         sigt_d    = sigt_d    * xnd
         sigt      = sigt_l    + sigt_d         
         
         fdark     = (xnd/(xnl + xnd)  + fdark_old) / 2.d0

         fdark     = max(fdark,0.001)
         fdark     = min(fdark,0.999)
         del       = abs(fdark/fdark_old - 1.)

         if(fdark .le. 0.009 .and. iter .ge. 2) del =0.
         if(ibug .ge. 2)
     $    write(6,'(a12,i3,a27, 4f9.2)') '   --- iter=',iter,' : Mmrn, Mdark, fdark, del', xnl/(xnl+xnd), xnd/(xnl+xnd), fdark, del
      endif
      enddo   ! end of iterative solution of Eq.A2 and Eq.A3

c
c -------------------------------------
c
c  total-to-slective extinction Rv_mod of the dust model:
c
       sigtV  = sigt(kvis)
       Rv_mod = sigt(kvis) / (sigt(kblue) - sigt(kvis))
       Ebv    = 2.5/alog(10.) * (sigt(kblue) - sigt(kvis))

c
c     check precission in using nl, nd in renormalization of sig*
c
       if (abs(Ebv_obs-Ebv) .gt. 0.01) 
     $     write(6,'(a35, 0p3f10.3)') ' Precision: Ebv_obs, Ebv_mod, err(obs/mod) = ',
     $               Ebv_obs, Ebv, Ebv_obs /Ebv

              
        if (abs((sigt_l(kvis)+sigt_d(kvis)) / tauV -1.) .gt. 0.001)
     $     write(6,'(a30, 1p3e12.3)') '  tauV, xnl*sl+xnd*sd, err = ',  
     $       tauV, sigt_l(kvis)+sigt_d(kvis), 
     $        abs((sigt_l(kvis)+sigt_d(kvis)) / tauV -1.)

       if(abs(tauV/tbv/Rv_mod -1.) .gt. 1e-3) then 
        write(6,'(a40, 2f8.2)')    ' Rv = Rv_mod ?               = ', tauV/tbv, Rv_mod
       endif 



          write(6,*)       '    abusi    abuc_tot abucaC     abugr   abuvsi  abucpahs  abucpahb '
          write(6,'(7f9.1)') abusi, abuc_tot,   abuc,    abucvgr,   abuvsi, abucpahs, abucpahb
          write(6,*)        ' acmass,   Simass,   vgrmass,  vsimass,  pahmass,  tmassDark '
          write(6,'(1p6e10.2)') acmass,   Simass,   vgrmass,  vsimass,  pahmass, tmassDark    
          if(abs(tmassmrn+tmassDark-1.) .gt. 0.001) stop 'tot Mass MRN=Drak ne 1 '
          write(6,'(a40,4f7.2)')  '  Mass of MRN, Dark, Mtot, fDark : ',
     $                              tmassmrn, tmassDark, tmassmrn+tmassDark, fDark
       write(6,'(a40, 1p2e12.3)') ' N_large+vsg,   N_dark       = ', xnl, xnd
       write(6,'(a40, 0p3f8.2)')    ' E(B-V), tauV_tot, tauV_dark = ', ebv, sigtV, sigt_d(kvis)
       write(6,'(a40, 1p3e12.3)') ' Mass_mrn, Mass_dark, fdark  = ', xnl/(xnl + xnd), xnd/(xnl + xnd), fdark
       write(6,'(a40)') '     '


       return
       end
