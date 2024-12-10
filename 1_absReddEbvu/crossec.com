c      parameter (nf=248, nnn=500, nres=20, lrv=5, lr=100)
c      parameter (nf=881, nnn=500, nres=17, lrv=5, lr=12)
c      parameter (nf=223, nnn=500, nres=20, lrv=5, lr=lrd)
c      parameter (nf=283,  nnn=500, nres=20, lrv=5, lr=100) ! for fraine lr=lrsi=100
c

      parameter (nf=283,  nnn=500, nres=20, lrv=5, lr=100, lrsi=84, lrd=201) ! for Demyk and porous aC grains	      
      implicit real*8 (a-h,o-z)
      real*8 j_v, mass, Msun, Lsun

      common /tempmrn_com/ fr(nf), fr3(nf), dfr(nf), qabs(nf), qsca(nf),
     $  emis(nf), j_v(nf)


	      
      common /effi/ qac(lr,nf), qsc(lr,nf), gc(lr,nf), qasi(lrsi,nf), 
     $ qssi(lrsi,nf), gsi(lrsi,nf), qavsi(lr,nf), 
     $ qsgr(lrv,nf), ggr(lrv,nf), qsvsi(lrv,nf), gvsi(lrv,nf), 
     $ qabspahs(nf), qabspahb(nf), qpc(lr,nf),  qpsi(lr,nf), qcpc(lr,nf),  
     $ qcpsi(lr,nf), qpd(lrd,nf),  qcpd(lrd,nf), 
     $ qad(lrd,nf), qsd(lrd,nf), gd(lrd,nf)

      common /vsg_com/ avsg(lrv), qagr(lrv,nf), 
     $ ahvgr(lrv),  ahvsi(lrv), ahpah, ahpahs, ahpahb, 
     $ at(nnn,nnn+1), bt(nnn,nnn), un(nnn), dun(nnn),
     $ dtun(nnn), aconti(nnn), tem(nnn), pw(nnn), xatom


      common /vec_lr/ ahc(lr), amrn(lr), ahsi(lr), adark(lrd), ahd(lrd)
      common /vec_nf/ wel(nf), emi_c(nf), emi_si(nf)
	      
      common /vec_sig/ sigt(nf), sigs(nf), siga_vgr(nf), sigs_vgr(nf), 
     $ siga_vsi(nf), sigs_vsi(nf), 
     $ siga_pah(nf), siga_pahs(nf), siga_pahb(nf), sigas(nf),
     $ siga_aC(nf), sigs_aC(nf), siga_Si(nf), sigs_Si(nf),
     $ sigp_aC(nf), sigcp_aC(nf), sigp_Si(nf), sigcp_Si(nf),
     $ siga_d(nf), sigs_d(nf),  sigt_d(nf), sigp_d(nf),  sigcp_d(nf),
     $ sigt_l(nf)	      

      common /fest/ igasabs, ibug, iblack, mm, mm1, mmold, mm1old,
     $	kvis, kblue, klyc, lac, lec, lasi, lesi, lav, lev, lad, led,
     $	ispecvsg, ispecpah, material, ievap, nnTvsg, ir_res(nres)


      common /const/  pi, pi4, sigma, boltz, clicht, hwirk, protm, 
     $  eVolt, tbb, Grav, Lsun, Msun, AU,
     $	rhc, rhgr, rhsi, rhvsi, rhd, wmolc, wmolsi, wmolvsi,
     $  qmrn, qvsg, sumjv, arad,
     $  zcpahs, zhpahs, zcpahb, zhpahb, zcpah, zhpah, fevap, fakisrf, Tmrn, 
     $  Tevap, zeitphot, zeitfein, sumevap, ratio, totabs, Eb, vper

      common /paraDark/ tauV, fdark, Ebv_obs, xnd, xnl
	      
      common /abu/ abuc, abuc_tot, abusi, abuvsi, abucvgr, abucpah, abucpahs,abucpahb
      common /rdark/ arad_polmin_aC, arad_polmin_Si, arad_polmax, aled	      


      



