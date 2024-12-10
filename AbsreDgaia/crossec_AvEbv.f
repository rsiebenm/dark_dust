      program crossec_AvEbv

c     Outputs the dust cross-sections for the three-component model by
c     Siebenmorgen R., 2023, A&A, 670A, 115. The relative masses of the
c     components are derived from input dust abundances and size
c     distributions. The relative mass of submicron-sized grains and the
c     cross-sections (cm^2/gramm-ISM-dust) are computed in the
c     subroutine sigtDark_EbvAv.
c
c
c     Compilation Instructions:
c       gfortran -ffixed-line-length-132     crossec_AvEbv.f sigtDark_AvEbv.f -o a.crossec_Dgaia
c       gfortran -ffixed-line-length-132 -O5 crossec_AvEbv.f sigtDark_AvEbv.f -o a.crossec_Dgaia
c
c     Dust Model:
c     1. Cross-sections:
c      Based on Siebenmorgen R. (2023, A&A, 670A, 115).
c     2. Column density estimation:
c        Follows Eq. (2) and Eq. (3) from Siebenmorgen & Chini (2023, 10.48550/arXiv.2311.03310).
c
c     Dust Components:
c     1) Nano-particles (VSG):
c        - Graphite + PAH (2175 AA bump, far-UV reddening).
c        - Nano-silicates (far-UV contribution).
c     2) Large grains:
c        - Prolate grains with a/b = 2, aligned (IDG alignment).
c        - Optical constants: Demyk et al. (2023) for aSi; Zubko (1996) for aC.
c        - Radii: 6 nm to ~250 nm (Mathis et al., 1977).
c     3) Submicron-sized grains (Dark Dust):
c        - Fluffy spheres (20% aC, 30% Si, 50% vacuum).
c        - Radii: 250 nm to ~3 microns.
c
c     Input (./Input/):
c       - jsm12fit.inp:
c         Dust parameters for fitting reddening curves:
c           - Abundances: abuc, abusi, abuvsi, abucvgr, abucpahs.
c           - Sizes: qmrn, alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled.
c       - w12_vv2_Ralf_283wave.dat:
c         Wavelength grid for the dust model.
c       - jsmTaufit.inp:
c         Includes tauV and Ebv_obs for equations (2) and (3).
c       - d.Q* files:
c         Grain efficiencies Q for various components.
c
c     Output (./Output/):
c       - Kappa.out:
c         Optical properties for absorption (sa_*) and scattering (ss_*).
c       - Kappa4fit.out:
c         Optical properties for observed wavelength ranges.
c       - PolKappa.out:
c         Polarization cross-sections (sigp_*). Dark dust: here: sigp_dark = 0.
c       - tau4fit.out:
c         Extinction and reddening curves (normalized and absolute).
c
c     Cross-Section Notation:
c     - Cross-sections (K) are in cm^2/g-dust and computed for each component.
c     - Converted to optical depth by multiplying by Nl, Nd.
c
c     Version History
c       - 26.11.2024: this version      
c       - 24.04.2023: Unified SpT (luminosity) with GAIA (parallax) distances.
c       - 20.09.2022: Updated optics (n, k) using Demyk+22 for X50A+E20R silicates.
c       - 04.12.2021: Included dark dust model.
c       - 1991: Original version (Siebenmorgen, PhD; Siebenmorgen & Kruegel, 1992, A&A).
c      For issues please contact me as author
c      Greetings, Ralf Siebenmorgen
c     ------------------------------------------------------------------
c
      parameter(nfo=283)
      include "crossec.com"

      dimension welo(nfo)
      dimension qoac(lr,nfo), qosc(lr,nfo),  goc(lr,nfo), 
     $          qoasi(lrsi,nfo), qossi(lrsi,nfo), gosi(lrsi,nfo), 
     $          qoad(lrd,nfo), qosd(lrd,nfo), god(lrd,nfo)
      dimension qopd(lrd,nfo), qocpd(lrd,nfo)
      dimension qopc(lr,nfo),  qopsi(lrsi,nfo), qocpc(lr,nfo), qocpsi(lrsi,nfo)
      dimension qoavsi(lrv,nfo), qosvsi(lrv,nfo), govsi(lrv,nfo), 
     $          qoagr(lrv,nfo),  qosgr(lrv,nfo),  gogr(lrv,nfo)
      dimension qoabspahs(nfo), qoabspahb(nfo)

c auf wel(nf) werden cross section interpliert:
      dimension welv(nf), weld(nf), welov(nf)
      
      dimension emi_pahs(nf), emi_pahb(nf), emi_vgr(nf),  emi_vsi(nf),  emi_d(nf)

      dimension emip_c(nf), emip_si(nf), emip_d(nf), emip_t(nf)
      
      dimension dummy(nf), idummy(20), seVolt(nf)
      character*80 cdumSi, cdumaC, cdumD
      
      real*8 ALMBDA,Rv,OMEGA, CMEXT,CMPOL,CMCPL,CMSCA
      real*8 Ebv, Ebv_obs, Rv_mod, Rv_obs
      real*8 md_aCSi, md_aC, md_Si, abud_acSi


c ----------------------------------------------------------------------------
c   Universelle Konstanten und Parameter wmolsi_EK= 168


      data hwirk, clicht, protm, boltz, sigma, eVolt, tbb, Grav, Lsun, Msun / 
     $ 6.6262d-27, 2.997925d10, 1.672d-24, 1.38062d-16, 5.669563d-5,
     $ 1.602d-12,2.9d0, 6.673d-8, 3.846d33, 1.989d33 /

      data fevap / 1d-8 /
c
      data rhsi,  rhvsi, wmolsi,  wmolvsi /
     $     3.4d0, 3.5d0, 134.5d0, 134.5d0 /

      data rhsiE10, rhsiE20, rhsiE30, rhsiE40, rhSiX /
     $     2.8d0,   2.9d0,   3.0d0,   3.1d0, 2.7d0   /

      data wmolSiE10, wmolSiE20, wmolSiE30, wmolSiE40 /
     $     99.86d0,   99.33d0,   99.79d0,   99.26d0   /

      data wmolSiX35, wmolSiX40, wmolSiX50 /
     $     141.d0,    121.d0,    100.d0    /   
      
      data rhc, rhgr, wmolc /  1.6d0, 2.24d0, 12d0 /
c
      
      Eb      = 5.              ! Bindungsenergie of PAH (eV  )
      pi      = 4d0 * atan(1d0)
      pi4     = 4d0 * pi
      au      = 1.496d13
      Msun    = 1.989d33
      Grav    = 6.673d-8



      print*, ' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      print*, '  --> crossec_dd              START                            ++++++++++' 
         amrn  = 0
         adark = 0


         qasi  = 0
         qssi  = 0
         qpsi  = 0
         qcpsi = 0

         
         qac   = 0
         qsc   = 0
         qpc   = 0
         qcpc  = 0

         qad   = 0
         qsd   = 0         
         qpd   = 0
         qcpd  = 0
         
      xnl      = 0.d0
      xnd      = 0.d0
      qabspahs = 0
      qabspahb = 0

      sigt     = 0.
      sigt_l   = 0.
      
      sigt_d   = 0.
      siga_d   = 0.
      sigs_d   = 0.
      sigp_d   = 0.
      sigcp_d  = 0.
      
      siga_ac  = 0.
      siga_si  = 0.
      sigp_ac  = 0.
      sigp_si  = 0.
      sigcp_ac = 0.
      sigcp_si = 0.
      sigs_ac  = 0.
      sigs_si  = 0.

      siga_vgr = 0.
      sigs_vgr = 0.
      siga_vsi = 0.
      sigs_vsi = 0.
      siga_pah = 0.
      siga_pahs= 0.
      siga_pahb= 0.
      
c
c ----------------------------------------------------------------------------
c    Default parameter:

         impfit   = 1             ! 0
         ibug     = 1
         iblack   = 0
         igasabs    = 0
         nnTvsg   = 200
         fakisrf  = 1.
         ispecvsg = 1
         ispecpah = 1
         lav      = 1
         lev      = 4

         alac   = 6.1E-7
         alec   = 2.5E-5
         alasi  = alac
         alesi  = 2.5E-5
         aled   = 3.1E-4
         qmrn   = 3.
         qvsg   = 3.
         
         arad_polmin_aC = 1.2356e-05
         arad_polmin_Si = 7.5857e-06 
         arad_polmax    = aled
         abuc           = 86.23 
         abusi          = 15.
         abucvgr        = 6.4
         abuvsi         = 13.19
         abucpahs       = 10.
         abucpahb       = 0.

         fDark          = 0.5d0 ! fraction of dust mass in Dark dust
         zcpahs         = 30
         zcpahb         = 0 
         hydpahs        = 0.4
         hydpahb        = 0.2
         tauV           = 1.72  ! depends on Star see ./Input/jsmTaufit.inp
         ebv_obs        = 0.32
         rdust          = 1.5d12
         totlum         = 3.85d33
         tstar          = 6000.
         powl           = 0.

c ------------------------------------------------------------------       
c   For  mpfit: read input parameters from ./Input/jsm12fit.inp and ./Input/jsmTaufit.inp
c
         if(impfit .eq. 1 .and. abucpahb .ne. 0.) 
     $     stop ' Enter in jsmDD.inp for mpfit=1:  abupahb = 0 '
               
      if(impfit .ne. 0) then 
       open(unit=20, file='./Input/jsm12fit.inp', form='formatted')
       rewind 20
       read(20,500) (idummy(i),i=1,20)
       read(20,*) abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, alec, alesi,
     $            arad_polmin_aC, arad_polmin_Si, arad_polmax, aled
       close(20)

 500   format(20a4)
       open(unit=20, file='./Input/jsmTaufit.inp', form='formatted')
       rewind 20
       read(20,500) (idummy(i),i=1,20)
       read(20,*) tauV, Ebv_obs
       close(20)
       
       qvsg   = qmrn
       
       if (alesi .gt.  3.442e-5) then
         write(6,*) ' *** Warnning: set to max radius alesi = 3.442e-5'
         alesi = 3.442e-5         
       endif
      
       write(6,*) ' '
       write(6,*) '** Reading 8 input parameters from file jsm12fit.inp'
       write(6,*) '** Reading 2 input parameters from file jsmTaufit.inp'
       Rv     = 1.0857 * tauV/ebv_obs
       write(6,'(a41,1x, 3f8.2)')   ' ** Rv(Av), tauV, ebv_obs= ', Rv, tauV, ebv_obs
       write(6,'(a41,1x,f10.2)')    ' ** abuc                 = ', abuc
       write(6,'(a41,1x,f10.2)')    ' ** abusi                = ', abusi
       write(6,'(a41,1x,f10.2)')    ' ** abuvsi               = ', abuvsi
       write(6,'(a41,1x,f10.2)')    ' ** abucvgr              = ', abucvgr
       write(6,'(a41,1x,f10.2)')    ' ** abucpah              = ', abucpahs
       write(6,'(a41,1x,f10.2)')    ' ** fDark                = ', fDark
       write(6,'(a41,1x,f10.2)')    ' ** qmrn                 = ', qmrn       
c       write(6,'(a41,1x,1p1e10.2)') ' ** alec                 = ', alec
c       write(6,'(a41,1x,1p1e10.2)') ' ** alesi                = ', alesi
c       write(6,'(a41,1x,1p1e10.2)') ' ** arad_polmin_aC       = ', arad_polmin_aC
c       write(6,'(a41,1x,1p1e10.2)') ' ** arad_polmin_Si       = ', arad_polmin_Si       
c       write(6,'(a41,1x,1p1e10.2)') ' ** arad_polmax  (na)   = ', arad_polmax
c       write(6,'(a41,1x,1p1e10.2)') ' ** arad_Darkmax         = ', aled
       if(arad_polmin_aC .ge. arad_polmax .or. arad_polmin_Si .ge. arad_polmax)
     $                     stop 'check arad_polmin/max '
       end if 


      qmrn   = qmrn - 1d0
      qvsg   = qvsg - 1d0
      zhpahs = hydpahs * zcpahs
      zhpahb = hydpahb * zcpahb

      if (ispecvsg .eq. 0.)  then
        abucvgr = 0.
        abuvsi  = 0.
       end if

       if (ispecpah .eq. 0.)  then
        abucpahs = 0.
        abucpahb = 0.
       end if

       if (zcpahb .eq. 0.)  then
        abucpahb = 0.
       end if

c ---------------------
c Density rhc, rhsi,rhd:
c
           por = 0.05
           rhsi    = rhsiX    *0.97/(1.+por) + rhsiE20  *0.03/(1.+por)
           wmolsi  = wmolsiX50*0.97 + wmolsiE20*0.03
           rhc     = rhc/(1.+por)
           write(6,*)                 '  *** 5% vaccum and mix Demyk+22 X50A(92.2%)+E20R(2.8%) per volumne'
           write(6,'(a40,1f7.2)')     '  *** porosity of aC and SI Mix  por = ', por
           write(6,'(a40, 1p4e10.2)') '  *** rhc, rhgr, rhsi, wmolsi        = ', rhc, rhgr, rhsi, wmolsi
           open(unit=3, file='./Input/d.QSi_X922E028V005_idg60', form='formatted')
           close(4)
           open(unit=4, file='./Input/d.QellipaC', form='formatted')
           open(unit=7, file='./Input/d.QellipDark', form='formatted')
           rewind(3)
           rewind(4)
           rewind(7)

c   DARK dust : relative weights  in 1g Staub: aC, Si, Eis'
c   geweis = Masse(Eis) / Masse(aC+Si)  = 1d-6 (hier ohne Eis)
c   volvac = Volumenanteil des Vakuum bezogen auf gesamt Volumen (from input) 
c   Volume of porous Dark dust grain: Vd = Vsi+Vc+Vvac. 
c   Mean density of porous Dark dust grain in 1 gramm of mass is : 
c   rhd    = total mass porous grain / Vd
c
        Vsi  = 0.5
        Vc   = 0.3
        Vvac = 0.2
        Veis = 0.
        write(6,*) ' Relative volume in Dark dust  :'
        write(6,*) '      aC        Si        Eis       Vac       total'
        write(6,'(1x,5f10.2)') vc, vsi, veis, vvac, (Vc+Vsi+Veis+Vvac)

c     mass in 1 gramm of dust in aC and Si sub-particles;
        md_ac     = (Vc*rhc)   /(Vc*rhc + Vsi*rhsi)
        md_Si     = (Vsi*rhsi) /(Vc*rhc + Vsi*rhsi)
        md_aCSi   = md_aC / md_Si
        abud_acSi = md_aCSi *wmolsi/wmolc

        Vd   = Vc + VSi + Vvac
        rhd  = (Vc*rhc  + VSi*rhsi)/ Vd ! density of porous grain
        write(6,'(a35, f7.2)')  '   Density of porous Dark dust = ', rhd
       
c
c     --------------------------------------------------------------------------------------------
c     Optics of dust - read Q's and interpolate to frequency grid of MRN, dark dust and vsgr, vsi
c     --------------------------------------------------------------------------------------------
c
c        MRN - Teilchen : Wellenlangen, Absorptions- und Streueffizienzen 
c   von File         d.QellipaC, d.QellipSi der ellipsoiden 

           print*,    '    ***     open:   d.QSi_X922E028V005_idg60 '
           print*,    '    ***     open:   d.QellipaC'
           print*,    '    ***     open:   d.QellipDark'          
       do j = 1,12
              read (3,'(a80)') cdumSi
              read (4,'(a80)') cdumaC
              read (7,'(a80)') cdumD
c             write(6,'(i5, a80)') j, cdumSI
c             write(6,'(i5, a80)') j, cdumaC
          if (j .ne. 2 .and. j .ne. 4 .and. j .ne. 11 .and. j.lt.13) then
          if(cdumSi .ne. cdumaC .or. cdumSi .ne. cdumd) then
              write(6,*) ' in line  j = ', j
              write(6,'(a80)') cdumSI
              write(6,'(a80)') cdumaC
              write(6,'(a80)') cdumD
              stop ' check header in d.Qellip* files'
          endif
          endif
       enddo
       
              read (3,'(a80)') cdumSi
              write(6,'(a80)') cdumSI
              read (4,'(a80)') cdumaC
              write(6,'(a80)') cdumaC
              read (7,'(a80)') cdumD
              write(6,'(a80)') cdumD
              read (3,'(a80)') cdumSi
              read (4,'(a80)') cdumaC
              read (7,'(a80)') cdumaD
c             write(6,*) 'k=0: Prolate'
c             write(6,*) 'k=1: Oblate' 

       do k = 1, nfo

c     prolate silicates
        do l = 1, lrsi
          read(3,*) welo(k), amrn(l),OMEGA, qoasi(l,k), qopsi(l,k),
     $          qocpsi(l,k), qossi(l,k)
          qoasi(l,k) =  qoasi(l,k) -  qossi(l,k) ! in d.q ist extinction gespeichert

          if(qoasi(l,k) .le. 0) then
           print*, 'l,k =', l, k
           print*, "welo(k), amrn(l),OMEGA, qoasi(l,k), qopsi(l,k), qocpsi(l,k), qossi(l,k)"
           write(6,'(1p7e10.2,a5)') welo(k), amrn(l),OMEGA, qoasi(l,k),
     $          qopsi(l,k), qocpsi(l,k), qossi(l,k), ' Si'
           stop  " *** Check d.QellipSi Q < 0"
          end if
         end do 

c     prolate aC: ! Anzahl von aC grain radii
        do l = 1, lr 
          read(4,*) wdum,   amrn(l) ,OMEGA, qoac(l,k),  qopc(l,k),  qocpc(l,k),  qosc(l,k)
c        write(6,'(1p7e10.2,a5)') wdum, amrn(l),OMEGA, qoac(l,k), qopc(l,k),
c        qocpc(l,k), qosc(l,k), ' aC'
         qoac(l,k)  =  qoac(l,k)  -  qosc(l,k)   ! in d.q ist extinction
                                              ! gespeichert
         if(qoac(l,k) .le. 0) then
          write(6,*) "l,k = ", l, k
          write(6,*) "wdum,   amrn(l),OMEGA, qoac(l,k),  qopc(l,k),  qocpc(l,k),  qosc(l,k), aC "
          write(6,'(1p7e10.2,a5)') wdum, amrn(l),   OMEGA, qoac(l,k),  qopc(l,k),  qocpc(l,k),  qosc(l,k), ' aC'
          stop  "** qoac  = 0 in d.Qellip* "
         end if
        end do


c     Dark dust : r<1mu prolate ; r>1mu spherical
        do l = 1, lrd ! Anzahl von dark dust grain radii
          read(7,*) weld(k),  adark(l), OMEGA, qoad(l,k), qopd(l,k), qocpd(l,k),  qosd(l,k)
          qoad(l,k)  =    qoad(l,k)  -  qosd(l,k) ! in d.q ist extinction gespeichert
          if(qoad(l,k) .le. 0 ) then
          write(6,*) "l,k = ", l, k

          write(6,*)  ' weld,    adark     OMEGA    qoad       qopd      qocpd     qosd    : dark '
          write(6,'(1p7e10.2,a5)') weld(k), adark(l), OMEGA, qoad(l,k),  qopd(l,k),  qocpd(l,k),  qosd(l,k), ' dark'
c          stop  ' ** qoad  = 0 in d.Qellip* '
         end if
        end do

        if(welo(k) .ne. weld(k)) then
         print*, "k, welo(k), weld(k) "
         print*,  k, welo(k), weld(k), amrn, "  " 
         stop  "** wrong wavelengths in files: d.Qellip* "
       endif

       end do
c
c set polarisation efficiencies q = abs(q)
c
       
         qopc  = abs(qopc)
         qopsi = abs(qopsi)
         qopd  = abs(qopd)

         qocpc  = abs(qocpc)
         qocpsi = abs(qocpsi)
         qocpd  = abs(qocpd)
       
c test:
c         call locat(weld, nfo, 0.55d-4, kvisd)
c         write(6,'(a20,f12.3,a30)') '    weld(kvisd) (mu) = ', weld(kvisd)*1e4, ' in d.Qmie_dark'
       close(3)
       close(4)
       close(7)

c
c ------------------------------------------------------ 
c  Wavelength grid of dust model use file: ./Input/w12_vv2_Ralf_283wave.dat    
c  until here old wavelengths as in d.,q files and now interpolate to
c  wavelength (cm) grid of dust model
c
       mm      = nf
       mm1     = mm-1
       if(mm .ne. 283) stop ' mm ne 283'
       open(unit=3, file='./Input/w12_vv2_Ralf_283wave.dat', form='formatted')
       rewind 3
       read (3,'(a80)') cdum
       read(3,*) (wel(k), k=1,mm)
       close(unit=3)


c Interpolate to new wavelenght grid for ellipsoids
       do  k = 1, mm
        if(welo(k) .ne. wel(k) .and. k.ne.242) then
         print*, 'wel(k), welo(k), k'
         print*, k, wel(k), welo(k)
         stop 'shall be the same MRN:'
        end if
        call locat(welo, nfo, wel(k), j)
        if(j .lt. 1)  stop ' MRN: wel-Gitter locat : check j<1 ?'  
        if(j .ge. nfo) j = nfo -1
           if(j .ge. nfo .and. (wel(k) .ne. welo(j)))  then
            print*, welo(1), welo(nfo), wel(k), k
            stop ' wel Gitter locat j=mm : check ?'  
           end if

         fak       = (wel(k) - welo(j)) / (welo(j+1) - welo(j))
        do l       = 1, lrsi ! Anzahl MRN Si grain radii: lrsi= 84 
         qasi(l,k) = qoasi(l,j) + (qoasi(l,j+1) - qoasi(l,j)) * fak
         qpsi(l,k) = qopsi(l,j) + (qopsi(l,j+1) - qopsi(l,j)) * fak
         qcpsi(l,k)= qocpsi(l,j)+ (qocpsi(l,j+1)- qocpsi(l,j))* fak
         qssi(l,k) = qossi(l,j) + (qossi(l,j+1) - qossi(l,j)) * fak
      enddo


      
        do l       = 1, lr ! Anzahl MRN aC grain radii: lr = 100
         qac(l,k)  = qoac(l,j)  + (qoac(l,j+1)  - qoac(l,j))  * fak
         qpc(l,k)  = qopc(l,j)  + (qopc(l,j+1)  - qopc(l,j))  * fak
         qcpc(l,k) = qocpc(l,j) + (qocpc(l,j+1) - qocpc(l,j)) * fak
         qsc(l,k)  = qosc(l,j)  + (qosc(l,j+1)  - qosc(l,j))  * fak
        enddo
      enddo

c
c 
      do k = 1, nfo
           do l = 1, lrsi
              if(ibug.gt.2 .and. wel(k)  .le. 1.06d-5 .and. wel(k)  .ge.
     $             1.04d-5 .and.amrn(l) .le. 3.30e-5  .and. amrn(l) .ge.
     $             3.70d-5) then
                   write(6,'(1p7e10.2,a5)') wel(k), amrn(l), OMEGA, 
     $             qasi(l,k), qpsi(l,k), qcpsi(l,k), qssi(l,k), ' Si'
            end if
       end do
      end do


c
c     Interpolate to wavelenght grid for Dark dust
c
         if( (abs(weld(1)/wel(1)-1.) .ge. 1.e-3) .or. (abs(weld(nf)/wel(nf)-1.) .ge. 1.e-3)) then
          print*, '  wel(1), weld(1), wel(nf),weld (nf)', wel(1), weld(1), wel(nf), weld(nf)
          stop ' check welgrids:'
         endif
       do  k = 1, mm
         if(abs(weld(k)-wel(k)) .gt. 1.e-4 .and. k.ne.242) then
                 print*, wel(k), weld(k), k
                 stop 'shall be the same DARK'
         end if
        call locat(weld, nfo, wel(k), j)
        if(j .ge. nfo) j = nfo -1
        if(j .le. 1)   j = 1

c     check weld(j) after locat:
        if(wel(k) .ge. weld(j)*(1.+1d-7) ) then
            print*, ' k,   j,  weld(j), wel(k), weld(j+1)'
            stop ' check wavelength grid: 1 term weld(j+1) < wel(k) <= weld(j)'
        endif 
        if(wel(k) .lt.  weld(j+1)/(1.+1d-7) ) then
            print*, ' k,   j,  weld(j), wel(k), weld(j+1)'
            print*, k, j,  weld(j), wel(k), weld(j+1)
            stop ' check wavelength grid: 2 term wel(k) <= weld(j)'
        endif 
        if(ibug .ge. 2 .and. weld(j+1).gt.wel(k)+1e-4) write(6,'(a28,2i5,1p3e9.2)') 
     $    ' weld(j) <=wel(k) < weld(j+1)', k, j, weld(j), wel(k), weld(j+1)

c ready to interpol dark dust q's:        
         fak        = (wel(k) - weld(j)) / (weld(j+1) - weld(j))
         do l       = 1, lrd ! Anzahl dark dust grain radii
          qad(l,k)  = qoad (l,j)  + (qoad( l,j+1) - qoad (l,j))  * fak
          qsd(l,k)  = qosd (l,j)  + (qosd( l,j+1) - qosd (l,j))  * fak
          qpd(l,k)  = qopd (l,j)  + (qopd( l,j+1) - qopd (l,j))  * fak
         qcpd(l,k)  = qocpd(l,j)  + (qocpd(l,j+1) - qocpd(l,j))  * fak
         gd( l,k)   = god  (l,j)  + (god ( l,j+1) - god  (l,j))  * fak
        enddo      
       enddo

c    for testing :
       if(ibug .gt. 2) then
          print*, arad_polmin_aC, arad_polmin_Si, arad_polmax, adark(80), weld(kvisd)
          do k = 1,nf
          do l = 1,lrd       
             if (adark(l) .ge. arad_polmin_aC .and. adark(l) .le. arad_polmax .and. weld(k) .ge. 0.3d-4) then
              write(16,'(1p5e10.2, 2i4)') weld(k), adark(l), qoad(l,k), qsd(l,k),  qpd(l,k),  l,k
              write(17,'(1p5e10.2, 2i4)') weld(k), adark(l), qoad(l,k), qosd(l,k), qopd(l,k), l,k
              if (weld(k) .eq.weld(kvisd) ) then
                 write( 6,'(1p5e10.2, 2i4)') weld(k), adark(l), qoad(l,k), qsd(l,k),  qpd(l,k),  l,k
                endif
              endif
           end do
           end do
        end if                    ! end  testen

c   Nano grains: Graphit + silicates -Wellenlangen, Absorptions- und Streueffizienzen from file d.Qmie_* 
c      print*, ' vsg:  Graphite'
       open(unit=3, file='./Input/d.Qmie_vGr', form='formatted')
            read (3,'(a80)') cdumd
            write(6,'(a80)') cdumd
            read (3,'(a80)') cdumd
         do   k = 1, nfo
         do   l = 1, lrv
          read(3,*) welv(k),avsg(l), qoagr(L,k), qosgr(L,k), gogr(L,k), x,xm1
         end do
         end do
         call locat(welv, nfo, 0.55d-4, kvisv)
          close (3) 

c      print*, ' vsg: Silicates'
       open(unit=3, file='./Input/d.Qmie_vSi', form='formatted')
            read (3,'(a80)') cdumd
            write(6,'(a80)') cdumd
            read (3,'(a80)') cdumd
         do   k = 1, nfo
         do   l = 1, lrv
          read(3,*) wdum, adum, qoavsi(L,k), qosvsi(L,k), govsi(L,k), x,xm1

          if(abs(wdum-welv(k)) .ge. 1e-3) then
                      write(6,'(2i4, 1p2e10.2)') k, l, wdum, welv(k)
                      stop ' welv(k) ne wdum: check vsg wel'
          endif

          if(abs(adum-avsg(l)) .ge. 1e-3) then
                      write(6,'(2i4, 1p2e10.2)') k, l, wdum, welv(k)
                      stop ' avsg(l) ne adum: check vsg radii'
          endif

         end do
         end do

         close (3) 

c test wavelength grid same for MRN and VSG:
      do k =1,nfo
       if(abs((welv(k)-welo(k))/welo(k)) .ge. 0.001) then
         print*, k, welo(k), welv(k), abs((welv(k)-welo(k))/welo(k))
         stop ' wrong VSG wavelengths'
       end if
       end do
c test: ok
c       write(6,'(a18,f6.3,a30)') '  welv(kvisv) (mu) = ', weld(kvisv)*1e4, 
c     $                           ' in d.Qmie_vGr'
      
c -------------------------------------------
c Interpolate to new wavelength grid
c
      do  k = 1, mm
        call locat(welv, nfo, wel(k), j)
        if(j .lt. 1)  stop 'wel-Gitter locat : check j<1 ?'  
        if(j .ge. nfo) j = nfo -1
           if(j .ge. nfo .and. (wel(k) .ne. welv(j)))  then
            print*, welv(1), welv(nfo), wel(k), k
            stop ' wel Gitter locat j=mm : check ?'  
           end if
         fak       = (wel(k) - welv(j)) / (welv(j+1) - welv(j))
      do l       = 1, lrv! Anzahl der VSG radii
         qagr(L,k)  = qoagr(L,j)  + (qoagr(l,j+1) - qoagr(l,j)) * fak
         qsgr(L,k)  = qosgr(L,j)  + (qosgr(l,j+1) - qosgr(l,j)) * fak
         qavsi(L,k) = qoavsi(L,j) + (qoavsi(l,j+1)- qoavsi(l,j))* fak
         qsvsi(L,k) = qosvsi(L,j) + (qosvsi(l,j+1)- qosvsi(l,j))* fak
      enddo
      enddo

c      write(65,'(1p2e10.2)') (wel(k), qagr(1,k), k=1,mm)
c      write(66,'(1p2e10.2)') (wel(k), qagr(1,k), k=1,mm)
c      print*, 'Interpol VSG done'
c      stop
c
c X-rays  : For nano-grains reduction of grain absorbtion efficiencies
c as calculated by Mie similar to Fig.5 of Smith and Dwek (97). Grain of
c radius arad and photon energie E > E_mie (keV) => qabs reduced ~1/fr.
c
      if (wel(mm) .le. 100d-8) 
     $ print*, '*** Reduction of abs efficiencies ~1/nu for wel < 136AA' 
      do l   = 1, lrv
        arad  = avsg(l)
        do  k = 1, mm
         if(arad .le.  10d-8)                        E_mie = 0.1
         if(arad .le.  50d-8 .and. arad .gt. 10d-8)  E_mie = 0.4 
         if(arad .le. 300d-8 .and. arad .gt. 50d-8)  E_mie = 1.
         if(arad .le. 1d-5   .and. arad .gt. 300d-8) E_mie = 2.
         if(arad .le. 2d-5   .and. arad .gt. 1d-5)   E_mie = 4.
         if(arad .le. 1d-4   .and. arad .gt. 2d-5)   E_mie = 7.
         if(arad .gt. 1d-4)  E_mie = 10.
         w_mie = clicht/(1d3*E_mie*eVolt/hwirk)
         call locat(wel, mm, w_mie, iX)
         if(wel(k) .lt. wel(iX)) then
          qagr(l,k)  = qagr(l,k)  * wel(k)/wel(iX)
          qavsi(l,k) = qavsi(l,k) * wel(k)/wel(iX)
          print*, wel(k), w_mie, wel(iX), arad
          stop ' verify setting MIE reduction for X-rays (was checked OK)'
         end if
        end do  
        end do  
c
c Chek wavelength settings for KJIVBU bands'
c         print*, 'c Chek wavelength settings for KJIVBU bands'
         call locat(wel, mm, 2.159d-4, k2p3)
         call locat(wel, mm, 1.662d-4, k1p65)
         call locat(wel, mm, 1.235d-4, k1p25)
         call locat(wel, mm, 5.477d-5, kvis)
         call locat(wel, mm, 4.440d-5, kblue)
         call locat(wel, mm, 3.656d-5, kuv)


        if(abs(1.- 2.159d-4/wel(k2p3)) .gt. 1.d-3 .or. 
     $    abs(1.- 1.662d-4/wel(k1p65)) .gt. 1.d-3 .or. 
     $    abs(1.- 1.235d-4/wel(k1p25)) .gt. 1.d-3 .or. 
     $    abs(1.- 5.477d-5/wel(kvis))  .gt. 1.d-3 .or. 
     $    abs(1.- 4.44d-5/wel(kblue))  .gt. 1.d-3)  then 
         print*, wel(k2p3),  2.159d-4, abs(1.- 2.159d-4/wel(k2p3))
         print*, wel(k1p65), 1.662d-4, abs(1.- 1.662d-4/wel(k1p65))
         print*, wel(k1p25), 1.235d-4, abs(1.- 1.235d-4/wel(k1p25))
         print*, wel(kvis),  5.477d-5, abs(1.- 5.477d-5/wel(kvis))
         print*, wel(kblue), 4.353d-5, abs(1.- 4.353d-5/wel(kblue))
         stop ' Chek wavelength settings for KJIVBU bands: still OK ?'
        end if

        if(abs(1.- 3.656d-5/wel(kuv))   .gt. 1.d-3 ) 
     $     write(6,'(a20,1p3e10.2)') ' U band not ok: ', 
     $     wel(kuv),   3.656d-5, abs(1.- 3.656d-5/wel(kuv))

      
       do  10  k = 1, mm      
      fr(k)       = clicht / wel(k)
      fr3(k)      = fr(k)**3
      if( k.le.2)    go to 10
      dfr(k-1)    = 5d-1 * (fr(k) - fr(k-2))
 10   continue
      dfr(1)      = 5d-1 * (fr(2)  - fr(1))
      dfr(mm)     = 5d-1 * (fr(mm) - fr(mm1))
              
c      
c     print*, ' Interpolation of Qs for MRN, VSG, Ellipsoids, Dark dust done'
c     Optics of dust done - Q's are stored
c     =======================================================================     
c      RADII: check grain radii request in input and availabel in d.q files
c        
         call locat(amrn, lr,   alac,  lac)
         call locat(amrn, lr,   alec,  lec)
         call locat(amrn, lrsi, alasi, lasi)
         call locat(amrn, lrsi, alesi, lesi)
         
            fak  = abs(amrn(lec)  /alec -1.)
            fak1 = abs(amrn(lec+1)/alec -1.)
            if(fak1 .lt. fak .and. lec .lt. lr-1) lec  = lec+1

            fak  = abs(amrn(lesi)  /alesi -1.)
            fak1 = abs(amrn(lesi+1)/alesi -1.)
            if(fak1 .lt. fak .and. lesi .lt. lr-1) lesi  = lesi+1
c test OK
c     write(6,'(a40, i5, 1p2e10.2)') ' Radii aC   asked: alac,  alec lec=', lec, alac, alec
c     write(6,'(a40, i5, 1p2e10.2)') ' Radii Si   asked: alasi,     lesi=', lesi, alasi, alesi         

        alac  = amrn(lac)
        alec  = amrn(lec)
        alasi = amrn(lasi)
        alesi = amrn(lesi)
        
        write(6,'(a20, 1p2e10.2,2i5)') ' Radii aC  :  ', amrn(lac),   amrn(lec),  Lac, Lec
        write(6,'(a20, 1p2e10.2,2i5)') ' Radii Si  :  ', amrn(lasi),  amrn(lesi),  Lasi, Lesi


c          alad = 1.d-7      ! for testing is verified correct
           alad = max(amrn(lesi+1), amrn(lec+1)) 
           call locat(adark, lrd, alad, lad)
           call locat(adark, lrd, aled, led)
           fak  = abs(adark(led)  /aled -1.)
           fak1 = abs(adark(led+1)/aled -1.)
           if(fak1 .lt. fak .and. led .lt. lrd-1) led  = led+1
           aled = adark(led)
         write(6,'(a20, 1p2e10.2,2i5)') '  Radii Dark : ', adark(lad),  adark(led),  Lad, Led
         if(led .ge. lrd) print*,       ' *** WARNING: check max grain size in d.q* for Dark Dust'

c
c ====================================================================
c Amount of Dark dust as by Eq.2 and 3 by Siebenmorgen & Chini, 
c     https://ui.adsabs.harvard.edu/abs/2023arXiv231103310S/abstract
c     doi =10.48550/arXiv.2311.03310
c  There are two dust types :
c        a) Large + nano grains in the ISM  with optical depth: tau_l and cross sect sigt_l
c        b) Dark dust as separate component with optical depth: tau_d and cross sect sigt_d



        call sigtDark_EbvAv

c
c =================================================================================
c      
      if(igasabs .eq.0) goto 401
        fak  = 200.d0 /sigt(kvis)
        write(6,*)'   sigt in [cm^2/gramm Gas+Dust ISM]'
        write(6,'(a30,1p1e9.3)')' Norm.sigt(V)=200cm^2/g  = ', fak
        sigt = sigt * fak
 401   continue
c
       
      
      open(unit=8, file='./Output/Kappa.out', form='formatted')
      rewind 8
      write(8,*) '# Optical depth (Siebenmorgen 2023, A&A 670A,115; SC 2023 doi: 10.48550/arXiv.2311.03310)'
      write(8,*) '# wel(cm) sa_aC    ss_aC    sa_Si    ss_Si    sa_vgr   ss_vgr   sa_vsi   ss_vsi   s_pahS+B sa_Dark  ss_Dark sigt '
      write(8,*) '# ------------------------------------------------------------------------------------------------------------- '
      write(8,'(1p1e9.3,11e9.2, 1e10.3)') (wel(k), 
     $   siga_aC(k),    sigs_aC(k),  siga_Si(k), sigs_Si(k), 
     $   siga_vgr(k),  sigs_vgr(k), siga_vsi(k), sigs_vsi(k),
     $   siga_pahS(k)+siga_pahB(k),   siga_d(k), sigs_d(k), sigt(k), k=1,mm)
      close(8)

c
c ------------------------------
c Normalization:
c A(l) = 1.086 * tau 
c A(l) = 1.086 N_dust * Cext 
c P(l) = 1.086 N_dust * Cpol
c => P(l)/A(l) = Cpol(l)/Cext(l) = sigpol(l)/sigt(l)
c ---------------
      open(unit=18, file='./Output/PolKappa.out', form='formatted')
      rewind 18
      write(18,*) '# Polarisation optical depth (Siebenmorgen 2023, A&A 670A,115; SC 2023 doi: 10.48550/arXiv.2311.03310)'
      write(18,*) '# wel         sigp_aC     sigp_Si      sigp_dark    sigt'
      write(18,*) '#----------------------------------------------------------------------------------------'
      write(18,'(1p5e12.3)') (wel(k), sigp_aC(k), sigp_Si(k), sigp_d(k), sigt(k), k=1,mm)
      close(18)

       Rv_mod  =      sigt(kvis)/(sigt(kblue) - sigt(kvis))
       Ebv    = 2.5/alog(10.) * (sigt(kblue) - sigt(kvis))
       
          open(unit=16, file='./Output/tau4fit.out', form='formatted')
          rewind(16)
          write(16,'(a60, 2f9.3)') '#  wel (mu)   tau/tauV  E(w-V)/E(B-V)  E(w-V)| Rv_mod, Ebv =', Rv_mod, Ebv
          write(16,'(1x,1p4e11.3)') wel(1) *1e4, sigt(1)/sigt(kvis),
     $         (sigt(1)/sigt(kvis)-1.)   * Rv_mod, 2.5/alog(10.) * (sigt(1)/sigt(kvis) -1.) * sigt(kvis) 
          write(16,'(1x,1p4e11.3)') wel(k2p3) *1e4, sigt(k2p3)/sigt(kvis),
     $         (sigt(k2p3)/sigt(kvis)-1.)* Rv_mod, 2.5/alog(10.) * (sigt(k2p3)/sigt(kvis) -1.) * sigt(kvis) 
          write(16,'(1x,1p4e11.3)') wel(k1p65)*1e4, sigt(k1p65)/sigt(kvis), 
     $       (sigt(k1p65)/sigt(kvis)-1.) * Rv_mod, 2.5/alog(10.) * (sigt(k1p65)/sigt(kvis) -1.) * sigt(kvis) 
          write(16,'(1x,1p4e11.3)') wel(k1p25)*1e4, sigt(k1p25)/sigt(kvis), 
     $       (sigt(k1p25)/sigt(kvis)-1.) * Rv_mod, 2.5/alog(10.) * (sigt(k1p25)/sigt(kvis) -1.) * sigt(kvis) 
          write(16,'(1x,1p4e11.3)') wel(kvis) *1e4, sigt(kvis)/sigt(kvis), 
     $       (sigt(kvis)/sigt(kvis)-1.)  * Rv_mod, 2.5/alog(10.) * (sigt(kvis)/sigt(kvis) -1.) * sigt(kvis) 
          write(16,'(1x,1p4e11.3)') wel(kblue)*1e4, sigt(kblue)/sigt(kvis), 
     $       (sigt(kblue)/sigt(kvis)-1.) * Rv_mod, 2.5/alog(10.) * (sigt(kblue)/sigt(kvis) -1.) * sigt(kvis) 
          write(16,'(1x,1p4e11.3)') wel(kuv)  *1e4, sigt(kuv)/sigt(kvis), 
     $       (sigt(kuv)/sigt(kvis)-1.)   * Rv_mod, 2.5/alog(10.) * (sigt(kuv)/sigt(kvis) -1.) * sigt(kvis) 
          
c --------------------
c ' optical+FUSE range : 1. > wel > 0.0909, x < 11.0 in tau4fit and Kapp4fit'
c ' optical+ IUE range : 1. > wel > 0.1148, x <  8.7 in tau4fit and Kapp4fit'
          do k = 1, mm  
             if(wel(k).le.0.281d-4 .and. wel(k).ge.0.1249d-4) then
                 write(16,'(1x,1p4e11.3)') wel(k)*1e4, 
     $           sigt(k)/sigt(kvis), (sigt(k)/sigt(kvis)-1.)*Rv_mod, 2.5/alog(10.)*(sigt(k)/sigt(kvis) -1.) * sigt(kvis) 
             endif

                if(wel(k).le.0.119d-4 .and. wel(k).ge.0.09d-4) then
                   write(16,'(1x,1p4e11.3)') wel(k)*1e4, 
     $           sigt(k)/sigt(kvis), (sigt(k)/sigt(kvis)-1.)*Rv_mod, 2.5/alog(10.)*(sigt(k)/sigt(kvis) -1.) * sigt(kvis) 
             endif
          end do
          close(16)

           open(unit=18, file='./Output/Kappa4fit.out', form='formatted')
           rewind(18)
           
      write(18,*) '# Optical depth (Siebenmorgen 2023, A&A 670A,115; SC 2023 doi: 10.48550/arXiv.2311.03310)'
      write(18,*) "# wel(cm) sa_aC    ss_aC    sa_Si    ss_Si    sa_vgr   ss_vgr   sa_vsi   ss_vsi   s_pahS+B sa_Dark  ss_Dark sigt"
      write(18,*) "# --------------------------------------------------------------------------------------------------------------"
             k = 1 ! longest wavelength
           write(18,'(1p1e10.3,11e9.2, 1e10.3)') wel(k), siga_aC(k), sigs_aC(k), siga_Si(k), 
     $    sigs_Si(k), siga_vgr(k), sigs_vgr(k), siga_vsi(k), sigs_vsi(k),
     $    siga_pahS(k)+siga_pahB(k), siga_d(k), sigs_d(k), sigt(k)

             k = k2p3
           write(18,'(1p1e10.3,11e9.2, 1e10.3)') wel(k), siga_aC(k), sigs_aC(k), siga_Si(k), 
     $    sigs_Si(k), siga_vgr(k), sigs_vgr(k), siga_vsi(k), sigs_vsi(k),
     $    siga_pahS(k)+siga_pahB(k), siga_d(k), sigs_d(k), sigt(k)


             k = k1p65
           write(18,'(1p1e10.3,11e9.2, 1e10.3)') wel(k), siga_aC(k), sigs_aC(k), siga_Si(k), 
     $    sigs_Si(k), siga_vgr(k), sigs_vgr(k), siga_vsi(k), sigs_vsi(k),
     $    siga_pahS(k)+siga_pahB(k), siga_d(k), sigs_d(k), sigt(k)


             k = k1p25
           write(18,'(1p1e10.3,11e9.2, 1e10.3)') wel(k), siga_aC(k), sigs_aC(k), siga_Si(k), 
     $    sigs_Si(k), siga_vgr(k), sigs_vgr(k), siga_vsi(k), sigs_vsi(k),
     $    siga_pahS(k)+siga_pahB(k), siga_d(k), sigs_d(k), sigt(k)


             k = kvis
           write(18,'(1p1e10.3,11e9.2, 1e10.3)') wel(k), siga_aC(k), sigs_aC(k), siga_Si(k), 
     $    sigs_Si(k), siga_vgr(k), sigs_vgr(k), siga_vsi(k), sigs_vsi(k),
     $    siga_pahS(k)+siga_pahB(k), siga_d(k), sigs_d(k), sigt(k)


             k = kblue
           write(18,'(1p1e10.3,11e9.2, 1e10.3)') wel(k), siga_aC(k), sigs_aC(k), siga_Si(k), 
     $    sigs_Si(k), siga_vgr(k), sigs_vgr(k), siga_vsi(k), sigs_vsi(k),
     $    siga_pahS(k)+siga_pahB(k), siga_d(k), sigs_d(k), sigt(k)


             k = kuv
           write(18,'(1p1e10.3,11e9.2, 1e10.3)') wel(k), siga_aC(k), sigs_aC(k), siga_Si(k), 
     $    sigs_Si(k), siga_vgr(k), sigs_vgr(k), siga_vsi(k), sigs_vsi(k),
     $    siga_pahS(k)+siga_pahB(k), siga_d(k), sigs_d(k), sigt(k)


         do k = 1, mm  
            if(wel(k).le.0.281d-4 .and. wel(k).ge.0.1249d-4) then 
               write(18,'(1p1e10.3,11e9.2, 1e10.3)') wel(k), siga_aC(k), sigs_aC(k), siga_Si(k), 
     $                  sigs_Si(k), siga_vgr(k), sigs_vgr(k), siga_vsi(k), sigs_vsi(k),
     $                  siga_pahS(k)+siga_pahB(k), siga_d(k), sigs_d(k), sigt(k)
            endif

            if(wel(k).le.0.119d-4 .and. wel(k).ge.0.09d-4) then
               write(18,'(1p1e10.3,11e9.2, 1e10.3)') wel(k), siga_aC(k), sigs_aC(k), siga_Si(k), 
     $                   sigs_Si(k), siga_vgr(k), sigs_vgr(k), siga_vsi(k), sigs_vsi(k),
     $                   siga_pahS(k)+siga_pahB(k), siga_d(k), sigs_d(k), sigt(k)
            endif
          end do
          close(18)


      close(unit=2)
      close(unit=11)
      close(unit=24)
      close(unit=26)

      print*, ' +++                crossec_dd              DONE                     ++++++++++'
      print*, ' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      print*, '        '
      end
c
c ========================================================
c
      subroutine pah_wq
c
c   Berechnet PAH cross sections

      include "crossec.com"
      dimension wel_res(nres), gam_res(nres), area_res(nres), s_res(nres)
      character*80 cdum


c Achtung U band und wel_res bei 0.097 und 0.0772 nicht in neumen wel
c    1    2    3    4    5    6    7    8    9   10   11  12  13  14  15  16  17

c 23.1 21.1 18.2 16.4 15.7 15.1 14.3 13.6 12.7 12.0 11.3 8.6 7.7 7.0 6.2 5.2 3.3
c
c ------------ ISM 
      data wel_res / 23.09d-4,  21.09d-4,  18.19d-4, 16.5d-4, 
     $                15.7d-4,  15.09d-4,  14.29d-4, 13.59d-4, 
     $                12.7d-4,  11.95d-4,  11.28d-4,  8.59d-4, 7.7d-4,
     $                 7.0d-4,    6.2d-4,   5.1d-4,   3.3d-4, 2.175d-5, 
     $                0.97d-5,  0.772d-5  /

c ISM settings:  A&A 561, A82 (2014)
      data gam_res /  10.0D12, 10D12, 10d12,  10.d12,  
     $                5.d12,   4d12,    5d12,   4d12, 
     $                5.d12,   7d12,    6d12,   6d12,  22d12, 
     $                5.d12,  14d12,   20d12,  20d12,  1.8d15, 
     $                3.9d15, 5d15 / 


      data area_res / 2.0d-26, 2.0d-26, 3.0d-26, 5d-26, 
     $                0.3d-26, 0.3d-26, 0.9d-26, 3.7d-26, 
     $                12.d-25, 6.d-25, 30d-25, 2d-25, 3.5d-25,
     $                5.d-26, 1.d-25, 9d-26, 2d-25, 4d-23, 
     $                1d-99, 3.5d-23 /
c
c org:      data area_res / 2.0d-26, 2.0d-25, 3.0d-25, 5d-26, 
c     $                0.3d-26, 0.3d-26, 0.9d-26, 3.7d-26, 
c     $                15.d-25, 6.d-25, 25d-25, 2d-25, 3.5d-25,
c     $                5.d-26, 1.d-25, 2.7d-26, 2d-25, 4d-23, 
c     $                1d-99, 3.5d-23 /
c
c ---------------------------------------------------------------
c original parameters for N1808 as in AA377, 735 2001
c      data gam_res /  10.0D12,  10D12,  10d12,  3.d12,  
c     $                2.0d12,  3d12,  5d12,  4d12, 
c     $                3.5d12,  7d12,  4d12,  6d12,  22d12, 
c     $                5.9d12, 14d12, 20d12, 20d12, 1.8d15 / 
c
c      data area_res / 2.0d-26, 2.0d-26, 1.0d-26, 0.5d-26, 
c     $                0.3d-26, 0.3d-26, 0.9d-26, 3.7d-25, 
c     $                2.8d-25, 12d-25, 3.6d-25,  3.5d-25, 5.5d-25, 
c     $                1.25d-25, 21d-25, 1.1d-26, 1.2d-25,4d-23, 
c     $                1d-99, 3.5d-23 /
c
c ---------------------------------------------------------------

      ievap    = 0
      siga_pah = 0.
      qabs     = 0.
      emis     = 0.



      do i = 1, nres
       call locat(wel, mm, wel_res(i), kres)
c       if (i .eq.18)       kres = kres +1
       ir_res(i)    = kres
c       write(6,'(2i4,1p4e10.3)') i, kres, wel_res(i), 
c     $             wel(kres), wel(kres-1), wel(kres+1)

      end do

      
c Reset PAH parameters using observed Drude Profile at 2170AA bump in
c notation of Gordon et al (2009) using x and width gam2170 we convert to
c notoation by Endrik (using fr(k) , gam_e): 
c  gam_e = gam2170 * sqrt(4.) * pi * 1d4 * clicht
c Standard values to use in PAH2170.wq : 4.6082949   0.95559058
c


       open(unit=24, file='./Input/PAH2170.wq', form='formatted')
       rewind(24) 
       read(24,*) cdum
       read(24,*) x2170, gam2170
       close(24)
       wel2170 = 1.d-4/x2170
        call locat(wel, mm, wel2170, kres)
        ir_res(18) = kres
c       write(6,'(i4,1p5e10.3)') kres, wel2170, wel(ir_res(18)), 
c     $             wel(kres), wel(kres-1), wel(kres+1)
       if(ibug .ge. 2) then
        write(6,*) '  Change 2175AA PAH bump using ./Input/PAH2170.wq'
        write(6,*) '  wel(ir_res(18)), x2170, gam2170,  gam_res(18) '
        write(6,'(1x,0p1f9.4, 2f9.2,1p1e11.3, a35)') wel(ir_res(18))*1d4, 1.d-4/wel(ir_res(18)), gam_res(18)/pi/clicht*1d-4,
     $           gam_res(18), ' old : as in data statement    ' 
        write(6,'(1x,0p1f9.4, 2f9.2,1p1e11.3, a35)') wel2170*1d4, x2170, gam2170, 
     $           gam2170* sqrt(4.)*pi*clicht*1d4, ' used: as in ./Input/PAH2170.wq' 
      endif
      
       gam_res(18)     = gam2170 * sqrt(4.)*pi*clicht*1d4 


c
c ---------------------------------------------------------------------------
c  The following PAH bands depend on No of H atoms

      s_res( 8) = zhpah *gam_res( 8)*clicht*area_res( 8)/wel(ir_res( 8))**2
      s_res( 9) = zhpah *gam_res( 9)*clicht*area_res( 9)/wel(ir_res( 9))**2
      s_res(10) = zhpah *gam_res(10)*clicht*area_res(10)/wel(ir_res(10))**2
      s_res(11) = zhpah *gam_res(11)*clicht*area_res(11)/wel(ir_res(11))**2
      s_res(12) = zhpah *gam_res(12)*clicht*area_res(12)/wel(ir_res(12))**2
      s_res(14) = zhpah *gam_res(14)*clicht*area_res(14)/wel(ir_res(14))**2
      s_res(17) = zhpah *gam_res(17)*clicht*area_res(17)/wel(ir_res(17))**2

c  The following PAH bands depend on No of C atoms

      s_res( 1) = zcpah *gam_res( 1)*clicht*area_res( 1)/wel(ir_res( 1))**2
      s_res( 2) = zcpah *gam_res( 2)*clicht*area_res( 2)/wel(ir_res( 2))**2
      s_res( 3) = zcpah *gam_res( 3)*clicht*area_res( 3)/wel(ir_res( 3))**2
      s_res( 4) = zcpah *gam_res( 4)*clicht*area_res( 4)/wel(ir_res( 4))**2
      s_res( 5) = zcpah *gam_res( 5)*clicht*area_res( 5)/wel(ir_res( 5))**2
      s_res( 6) = zcpah *gam_res( 6)*clicht*area_res( 6)/wel(ir_res( 6))**2
      s_res( 7) = zcpah *gam_res( 7)*clicht*area_res( 7)/wel(ir_res( 7))**2
      s_res(13) = zcpah *gam_res(13)*clicht*area_res(13)/wel(ir_res(13))**2
      s_res(15) = zcpah *gam_res(15)*clicht*area_res(15)/wel(ir_res(15))**2
      s_res(16) = zcpah *gam_res(16)*clicht*area_res(16)/wel(ir_res(16))**2
      s_res(18) = zcpah *gam_res(18)*clicht*area_res(18)/wel(ir_res(18))**2
      s_res(19) = zcpah *gam_res(19)*clicht*area_res(19)/wel(ir_res(19))**2
      s_res(20) = zcpah *gam_res(19)*clicht*area_res(20)/wel(ir_res(20))**2


c      welcut = (1630. + 450. * 1.29 * sqrt(0.4*zcpah) ) * 1d-8
c New welcut folloiwng: Salama, Bakes Alamandola, Tielens, 1996, ApJ458, 621
c
      welcut = 1./(3.804/sqrt(0.4*zcpah) +1.) * 1d-4
      if(welcut .le. 0.55d-4)   welcut = 0.55d-4

c
c ---------------------------------------------------------------------------
c   Abs efficiencies [Schu93]: IR lines + IR continuum + UV
c   UV Querschnit ist: i) zunaechst konst*Nc, dann 
c   ii) ab 912AA ~ zu kleinen aC Koerner und iii) ab 100eV: q=1

c      C_PAH_UV = 9d-18
      C_PAH_UV = 3.d-18    ! (SKB14)

      call locat (wel, mm, 0.44d-4, kb)
c      write(6,'(a14, 1p1e9.2, i5)') '   PAH: wel(B)',  wel(kb), kb
      call locat (wel, mm, 0.167d-4, kuv)
c      write(6,'(a14, 1p1e9.2,i 5)') '   PAH: welcut',  welcut, kuv


      do  72  k = 1, mm

c IR features without 2200 AA bump

       do  j = 1, nres-2
        qabs(k) = qabs(k) + s_res(j) * fr(k)**2 / ( pi**2 * (fr(k)**2- 
     $            fr(ir_res(j))**2)**2 + fr(k)**2 * gam_res(j)**2/4d0)
c        print*, ' nres: ', ir_res(j), s_res(j),  fr(ir_res(j)), gam_res(j)
c        pause
       end do

       if(wel(k) .ge. welcut) then 
c   NIR of ionised PAH (Mattioda et al.2005, apj629,1183;gion=0.5)
         qabs(k)  = qabs(k) +zcpah*3.5/2.*10.**(-19.-1.45*wel(k)*1d4) 
       else

        do  j = nres-2, nres
         qabs(k) = qabs(k) + s_res(j) * fr(k)**2 / ( pi**2 * (fr(k)**2- 
     $            fr(ir_res(j))**2)**2 + fr(k)**2 * gam_res(j)**2/4d0)
        end do
       endif
  72  continue

      if(ibug .ge. 2) then
       write(26,*) 'wel (cm), 1/wel (mic), qpah(k) /C-atom'
       rewind(26)
       do k =1,mm
        if(1.d-4/wel(k) .ge. 1) write(26,'(1p4e11.2)') wel(k), 
     $     1.d-4/wel(k), hwirk*fr(k)/eVolt, qabs(k)/zcpah
       end do
      endif


      return 
      end
c  *********************************************************************

      subroutine locat(xy, n, x, j)

c   xy ist ein geordneter Vektor der Lange n, d.h entweder
c   xy(1) < xy(2) < ... < xy(n)   oder   xy(1) > xy(2) > ... > xy(n).
c   Gegeben die Zahl x. Programm berechnet j, so dass x im Intervall
c   [xy(j), xy(j+1)]  liegt.
c   Falls x < xy(1) < xy(n):     j = 0
c   Falls     xy(1) < xy(n) < x: j = n
c   Falls x > xy(1) > xy(n):     j = 0
c   Falls     xy(1) > xy(n) > x: j = n

      implicit real*8 (a-h, o-z)
      dimension xy(n)
      jlow = 0
      jup  = n + 1

 10   continue
      if(jup-jlow .gt. 1) then
       jm = (jup + jlow) / 2
       if( (xy(n).gt.xy(1) ) .eqv. ( x.gt.xy(jm)) ) then
        jlow = jm
       else
        jup  = jm
       end if
       go to 10
      end if

      j      = jlow

      return
      end


