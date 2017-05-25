c
c     Program to simulate the ISR/CSR mechanism based on microbunching instability
c     for solar flares
c
c     We assume that the beam of accelerated electrons is divided in 
c     3 different sources when precipitating into the atmosphere:
c
c     1) ISRex: an extended source including a fraction xNISRex = NISRex/Ne 
c        of the total number of electrons.
c
c     NISRex = NISRLOWex + NISRHIGHex
c     NISRLOWex =  xNISRex * NLOW
c     NISRHIGHex = xNISRex * NHIGH
c
c     2) CSRmb: a nearly point source with dimensions given by the characteristic 
c        microbunch lenght scale including a fraction xNCSR = NCSRHIGH/NHIGH of 
c        the total number of high-energy electrons (Etr < E < Emax).
c
c     NCSRHIGH = xNCSR * NHIGH
c
c     3) ISRco: a compact source including the remaining NISRLOWco = NLOW - NISRLOWex 
c        low-energy electrons (Emin < E < Etr) and NISRHIGHco = NHIGH - NISRHIGHex - NCSRHIGH 
c        high-energy electrons (Etr < E < Emax).
c
c     NISRco = NISRLOWco + NISRHIGHco
c     NISRLOWco = NLOW - NISRLOWex = (1 - xNISRex) * NLOW
c     NISRHIGHco = NHIGH - NISRHIGHex - NCSRHIGH = (1 - xNISRex - xNCSR) * NHIGH
c
c     I = [(jISRlow + jISRhigh)/(kISRlow + kISRhigh)]ex 
c         [(jISRlow + jISRhigh)/(kISRlow + kISRhigh)]co
c       + [jCSRhigh/kCSRhigh]
c
c     ISR radiation from high energy electrons can be absorbed only by
c     electrons participating in the ISR process
c
c     CSR radiation from high energy electrons can be absorbed only by
c     electrons participating in the CSR process
c
c     ISRex, ISRco and CSR sources are cylinders with different dimensions
c
c     Different values of B field and plasma density for the compact and extended source
c      
c     The program uses the theory developed in [Ramaty 1969 ApJ 158, 753;
c     Ramaty et al. 1994 ApJ 436, 941] to calculate gyrosynchrotron radiation 
c     from a power-law electron spectrum with isotropic pitch-angle distribution:
c
c     n(E) = A E^(-delta)  (MeV^-1)
c
c     A = 1/ XNORM normalizes the distribution to 1.
c
c     The program requires an input file, gs.inp; the parameters in
c     this file define: 

c     delta        spectral index of electron distribution
c     NTOTAL       total number of electrons
c     bmagco       magnetic field for compact source [gauss] (assumed uniform) 
c     bmagex       magnetic field for extended source [gauss] (assumed uniform)   
c     viewangle    viewing angle (angle between B and the line of sight (0<cs<1)
c                  cs can take on any positive value greater than 0 and less
c                  than 1; the appropriate formulae for cs=0 are given in Ramaty (1969);  
c                  these formulae have not yet been incorporated into the
c                  present routine; it appears that the routine works
c                  well for 0.1<cs<0.95.
c
c     SCsize       source size [arc sec]
c     SCheight     source height [cm]
c     SEsize       source size [arc sec]
c     SEheight     source height [cm]      
c     j1           index to define Emin
c     j2           index to define Emax
c                  define the range of electron energies used in the 
c                  integration, for example
c                  j1=15; j2=80 corresponds to 0.05 - 100 MeV
c                  j1=21; j2=80 corresponds to 0.10 - 100 MeV
c
c     kf           number of output frequencies
c                  defines the highest frequency (in units of gyrofrequency), examples
c                  kf=101 corresponds to ffb=10
c                  kf=111 corresponds to ffb=100
c                  kf=121 corresponds to ffb=1000
c
c     etr          gyro/sync transition energy [MeV]
c                  below etr the full gyrosynchrotron formalism is used; above etr 
c                  the program uses the ultrarelativistic (Ginzburg and Syrovatskii) 
c                  formula; to calculate the total emissions it is sufficient
c                  to take etr about 3 MeV, except for cs close to 1 when etr should 
c                  be larger 
c 
c     npco         plasma density for compact source 
c     npex         plasma density for extended source 
c     ecsr         threshold for microbunching [MeV]
c     xNISRco      fraction of electrons in the compact source participating in the CSR process      
c     xNCSR        fraction of electrons participating in the CSR process
c     tb           microbunch time-width  [s]
c     
c     e1d,e2d ord/xord mode emissivities in erg s^-1 sr^-1 Hz^-1 cm^-3
c     a1d,a2d ord/xord mode abs coefficients in cm^-1
c     phi1,phi1 ord/xord mode flux densities at Earth in SFU
c     rccd and rcd are degrees of polarization calculated 
c     from the emissivities and the flux densities, respectively;
c     All 8 quantities are arrays of size defined by kf.
c
c     As the programs runs, it prints on the screen the current ffb 
c     (frequency over gyrofrequency) at which the calculation is carried 
c     out. The program is very fast at low ffb's; then, depending on
c     ETR it will slow down.

c     The output is written into gs.out.
c============================================================
c
      integer k,kf,j,j1,j2  
c
      parameter (kf=701)    
c
      double precision delta,emin,emax,em,eu,el,de,XNORM
      double precision etr,ecsr,gamma,gtr
c      
      double precision alphaco,bmagco,vbco,vpco,ffpco,npco,vroc  
      double precision alphaex,bmagex,vbex,vpex,ffpex,npex,vrex  
c    
      double precision g1co,g2co,anco,an1co,an2co,ath1co,ath2co
      double precision g1ex,g2ex,anex,an1ex,an2ex,ath1ex,ath2ex
c      
      double precision emfac,abfac  
      double precision PI,DTOR,AU,ARC2CM,M0,C,EC,E0,SFU 
c  
      double precision SCsize,SCheight,SCradius,SCarea
      double precision SCvolume,SCdensity,SComega
c
      double precision SEsize,SEheight,SEradius,SEarea
      double precision SEvolume,SEdensity,SEomega
c
      double precision MBsize,MBheight,MBradius,MBarea
      double precision MBvolume,MBdensity,MBomega
c      
      double precision viewangle,cs,ss
c
      double precision ffbco,ffbex,ff(kf)        
c      
      double precision NTOTAL,NLOW,NHIGH,NCSRHIGH
      double precision xNCSR,xNISRex
      double precision NISRco,NISRLOWco,NISRHIGHco
      double precision NISRex,NISRLOWex,NISRHIGHex
c
      double precision FCSR,tb
      double precision PISRLOWco,PISRHIGHco
      double precision PISRLOWex,PISRHIGHex      
      double precision PCSRHIGH,PCSRHIGH0      
c      
      double precision e1ISRlowco,e2ISRlowco
      double precision a1ISRlowco,a2ISRlowco      
      double precision e1dISRlowco(kf),e2dISRlowco(kf)
      double precision a1dISRlowco(kf),a2dISRlowco(kf)      
      double precision phi1ISRlowco(kf),phi2ISRlowco(kf)     
c
      double precision e1ISRhighco,e2ISRhighco 
      double precision a1ISRhighco,a2ISRhighco     
      double precision e1dISRhighco(kf),e2dISRhighco(kf)
      double precision a1dISRhighco(kf),a2dISRhighco(kf)      
      double precision phi1ISRhighco(kf),phi2ISRhighco(kf)           
c
      double precision e1ISRlowex,e2ISRlowex      
      double precision a1ISRlowex,a2ISRlowex     
      double precision e1dISRlowex(kf),e2dISRlowex(kf)
      double precision a1dISRlowex(kf),a2dISRlowex(kf)      
      double precision phi1ISRlowex(kf),phi2ISRlowex(kf)           
c
      double precision e1ISRhighex,e2ISRhighex      
      double precision a1ISRhighex,a2ISRhighex
      double precision e1dISRhighex(kf),e2dISRhighex(kf)
      double precision a1dISRhighex(kf),a2dISRhighex(kf)      
      double precision phi1ISRhighex(kf),phi2ISRhighex(kf)      
c     
      double precision e1dISRco(kf),e2dISRco(kf)
      double precision a1dISRco(kf),a2dISRco(kf)      
      double precision arg1ISRco,arg2ISRco        
      double precision phi1ISRco(kf),phi2ISRco(kf)
      double precision phitISRco(kf) 
      double precision qISRco(kf),vISRco(kf),scdISRco(kf)
      double precision rccdISRco(kf),rcdISRco(kf) 
c      
      double precision e1dISRex(kf),e2dISRex(kf)
      double precision a1dISRex(kf),a2dISRex(kf)      
      double precision arg1ISRex,arg2ISRex      
      double precision phi1ISRex(kf),phi2ISRex(kf)
      double precision phitISRex(kf) 
      double precision qISRex(kf),vISRex(kf),scdISRex(kf)
      double precision rccdISRex(kf),rcdISRex(kf)        
c
      double precision e1dISR(kf),e2dISR(kf)
      double precision a1dISR(kf),a2dISR(kf)
      double precision phi1ISR(kf),phi2ISR(kf),phitISR(kf) 
      double precision qISR(kf),vISR(kf),scdISR(kf)
      double precision rccdISR(kf),rcdISR(kf)      
c
      double precision e1CSR,e2CSR
      double precision a1CSR,a2CSR 
      double precision e1dCSR(kf),e2dCSR(kf)
      double precision a1dCSR(kf),a2dCSR(kf)      
      double precision arg1CSR,arg2CSR        
      double precision phi1CSR(kf),phi2CSR(kf),phitCSR(kf)
      double precision qCSR(kf),vCSR(kf),scdCSR(kf)
      double precision rccdCSR(kf),rcdCSR(kf)          
c 
      double precision phitLOWco(kf),phitLOWex(kf),phitLOW(kf)
      double precision phitHIGHco(kf),phitHIGHex(kf)
      double precision phitHIGH(kf)            
c
      double precision phi1(kf),phi2(kf),phit(kf) 
      double precision q(kf),v(kf),scd(kf)
      double precision rccd(kf),rcd(kf)            
c 
c------------------------------------------------------------
c
      open(unit=20,file='gyro-27-10-2014.inp'
     &  ,status='unknown')
c      
      open(unit=10,file='gyro-COEF-27-10-2014.out'
     &  ,status='unknown')
c      
      open(unit=11,file='gyroISR-flux-27-10-2014.out'
     &  ,status='unknown')
c      
      open(unit=12,file='gyroCSR-flux-27-10-2014.out'
     &  ,status='unknown')
c      
      open(unit=13,file='gyroLOW-flux-27-10-2014.out'
     &  ,status='unknown')
c      
      open(unit=14,file='gyroHIGH-flux-27-10-2014.out'
     &  ,status='unknown')     
c      
      open(unit=15,file='gyroISRco-flux-27-10-2014.out'
     &  ,status='unknown')
c      
      open(unit=16,file='gyroISRex-flux-27-10-2014.out'
     &  ,status='unknown')          
c      
      open(unit=17,file='gyroTOTAL-flux-27-10-2014.out'
     &  ,status='unknown')     
c      
      open(unit=18,file='gyroTOTAL-pol-27-10-2014.out'
     &  ,status='unknown') 
c 
c------------------------------------------------------------
c      
c     read input
c 
c------------------------------------------------------------
c
      read(20,200)delta        ! spectral index of electron distribution
      read(20,200)NTOTAL       ! total number of electrons
      read(20,200)bmagco       ! magnetic field for compact source [gauss] 
      read(20,200)bmagex       ! magnetic field for extended source [gauss]   
      read(20,200)viewangle    ! viewing angle
      read(20,200)SCsize       ! source size [arc sec]
      read(20,200)SCheight     ! source height [cm]
      read(20,200)SEsize       ! source size [arc sec]
      read(20,200)SEheight     ! source height [cm]      
      read(20,201)j1           ! index to define Emin
      read(20,201)j2           ! index to define Emax      
      read(20,200)etr          ! gyro/sync transition energy [MeV]
      read(20,200)npco         ! plasma density for compact source
      read(20,200)npex         ! plasma density for extended source          
      read(20,200)ecsr         ! threshold for microbunching [MeV]
      read(20,200)xNISRex      ! fraction of electrons in the extended source participating in the ISR process      
      read(20,200)xNCSR        ! fraction of electrons participating in the CSR process
      read(20,200)tb           ! microbunch time-width  [s]
c 
c------------------------------------------------------------
c
c     constants
c 
c------------------------------------------------------------
c
      PI  = 4.0d0*DATAN(1.0D0)
c
      DTOR   = PI/180.0d0                    ! degree --> rad
      AU     = 1.49597870d13                 ! astronomic Unit [cm]
      ARC2CM = (DTOR/3600.0d0)*AU            ! arcsec --> cm in Sun
      M0     = 9.1094d-28                    ! electron mass [g]
      C      = 2.998d10                      ! speed of light [cm/s]
      EC     = 4.803d-10                     ! electron charge [esu]
      E0     = (M0*C*C)/(1.6022d-12)/(1.0d6) ! electron rest energy [MeV]
      SFU    = 1.0d19                        ! erg/s/cm2/Hz --> s.f.u.
c 
c------------------------------------------------------------
c
c     parameters
c 
c------------------------------------------------------------
c
      cs    = dcos(viewangle*DTOR)         ! cos of viewing angle
c
c     verify viewing angle limits
c
         if (cs.lt.0.1d0.or.cs.gt.0.95d0) then
         write(*,*)'viewing angle beyond the limits'
         else
         continue
         endif
c   
      ss    = dsin(viewangle*DTOR)         ! sin of viewing angle
c
      vbco    = 0.5d0/PI*(EC/M0/C)*bmagco          ! gyrofrequency for compact source
      vpco    = EC*dsqrt(npco/PI/M0)               ! plasma frequency for compact source
      ffpco   = vpco/vbco                          ! ratio plasma/gyro frequency for compact source
      alphaco = (3.0d0/2.0d0)/ffpco                ! Razin parameter for compact source
      vrco    = (2.0d0/3.0d0)*(vpco*vpco)/vbco/ss  ! Razin effect cutoff frequency for compact source
c
      vbex    = 0.5d0/PI*(EC/M0/C)*bmagex          ! gyrofrequency for extended source
      vpex    = EC*dsqrt(npex/PI/M0)               ! plasma frequency for extended source
      ffpex   = vpex/vbex                          ! ratio plasma/gyro frequency for extended source
      alphaex = (3.0d0/2.0d0)/ffpex                ! Razin parameter for extended source
      vrex    = (2.0d0/3.0d0)*(vpex*vpex)/vbex/ss  ! Razin effect cutoff frequency for extended source
c
      emfac = (EC*EC*EC)/(M0*C*C)          ! emissivity scale factor
      abfac = 4.0d0*(PI*PI)*EC             ! absorption scale factor
c
      emin=10.0d0**(0.05d0*(j1-1)-2.0d0)   ! minimum electron energy
      emax=10.0d0**(0.05d0*j2-2.0d0)       ! maximum electron energy
c
      SCradius  = SCsize*ARC2CM/2.0d0      ! compact source radius [cm]
c
c   uncomment to set SCheight  = 2.0d0*SCradius
c
c      SCheight  = 2.0d0*SCradius           ! compact source height [cm]      
      SCarea    = PI*SCradius*SCradius     ! compact source area [cm^2]
      SComega   = SCarea/(AU*AU)           ! compact source solid angle [sr]
      SCvolume  = SCarea*SCheight          ! compact source volume [cm^3]
c
      SEradius  = SEsize*ARC2CM/2.0d0      ! extended source radius [cm]
      SEarea    = PI*SEradius*SEradius     ! extended source area [cm^2]
      SEomega   = SEarea/(AU*AU)           ! extended source solid angle [sr]
      SEvolume  = SEarea*SEheight          ! extended source volume [cm^3]
c      
      MBsize    = c*tb                     ! microbunch size [cm]
      MBheight  = c*tb                     ! microbunch height [cm]
      MBradius  = MBsize/2.0d0             ! microbunch radius [cm]
      MBarea    = PI*MBradius*MBradius     ! microbunch area [cm^2]
      MBomega   = MBarea/(AU*AU)           ! microbunch solid angle [sr]
      MBvolume  = MBarea*MBheight          ! microbunch volume [cm^3]
c  
c------------------------------------------------------------
c
c     calculate factor to normalize the electron energy distribution to 1
c 
c------------------------------------------------------------
c
      do j=j1,j2
      el=10.0d0**(0.05d0*(j-1)-2)
      eu=10.0d0**(0.05d0*j-2)
      em=10.0d0**(0.05d0*(j-0.5)-2)
      de=eu-el
      XNORM=XNORM+em**(-delta)*de
      enddo
c
      write(*,*)'NORM=',XNORM
      write(*,*)'Emin=',emin, 'Emax=',emax     
      write(*,*)''      
c 
c------------------------------------------------------------
c
c     calculate the number of low- and high-energy electrons
c 
c------------------------------------------------------------
c
      NLOW=0.0d0
      NHIGH=0.0d0
c      
      do j=j1,j2
      el=10.0d0**(0.05d0*(j-1)-2)
      eu=10.0d0**(0.05d0*j-2)
      em=10.0d0**(0.05d0*(j-0.5)-2)
      de=eu-el
      if(em.lt.ecsr)NLOW = NLOW + (NTOTAL/XNORM)*em**(-delta)*de
      if(em.gt.ecsr)NHIGH = NHIGH + (NTOTAL/XNORM)*em**(-delta)*de      
      enddo
c
      write(*,*)'NLOW=',NLOW
      write(*,*)'NHIGH=',NHIGH
      write(*,*)'NTOTAL=',NLOW + NHIGH 
      write(*,*)''      
c 
c------------------------------------------------------------
c
c     calculate the number of electrons in the extended source participating in the ISR process
c 
c------------------------------------------------------------
c
      NISRLOWex  = xNISRex * NLOW
      NISRHIGHex = xNISRex * NHIGH
      NISRex = NISRLOWex + NISRHIGHex
c 
c------------------------------------------------------------
c
c     calculate the number of electrons participating in the CSR process
c 
c------------------------------------------------------------
c
      NCSRHIGH = xNCSR * NHIGH
c 
c------------------------------------------------------------
c
c     calculate the number of electrons in the compact source participating in the ISR process
c 
c------------------------------------------------------------
c
      NISRLOWco  = (1.0d0 - xNISRex)*NLOW
      NISRHIGHco = (1.0d0 - xNISRex - xNCSR)*NHIGH
      NISRco     = NISRLOWco + NISRHIGHco
c
      write(*,*)'NCSRHIGH=',NCSRHIGH
      write(*,*)''
      write(*,*)'NISRLOWco=',NISRLOWco
      write(*,*)'NISRHIGHco=',NISRHIGHco
      write(*,*)'NISRco=',NISRco  
      write(*,*)''    
      write(*,*)'NISRLOWex=',NISRLOWex
      write(*,*)'NISRHIGHex=',NISRHIGHex
      write(*,*)'NISRex=',NISRex      
      write(*,*)''
      write(*,*)'NISRLOW=',NISRLOWco + NISRLOWex
      write(*,*)'NISRHIGH=',NISRHIGHco + NISRHIGHex
      write(*,*)'NISR=',NISRco + NISRex       
      write(*,*)''      
c 
c------------------------------------------------------------
c
c     calculate densities
c 
c------------------------------------------------------------
c
      MBdensity = NCSRHIGH/MBvolume      ! microbunch density [electrons/cm^3]
      SCdensity = NISRco/SCvolume        ! compact source density [electrons/cm^3]
      SEdensity = NISRex/SEvolume        ! extended source density [electrons/cm^3]      
c 
c------------------------------------------------------------
c
c     start loop on ff
c 
c------------------------------------------------------------
c
      do k=1,kf
c
      ff(k)=10.0d0**(0.01d0*(k-1))*1.0d9
c      
      ffbco=ff(k)/vbco
      ffbex=ff(k)/vbex       
c 
c------------------------------------------------------------
c
c     calculate microbunching form factor and integration weights
c 
c------------------------------------------------------------
c
c    hyperbolic secant profile
c
      FCSR= 1.d0/dcosh(pi*tb*ff(k)/2.0d0)   
c
c    gaussian profile
c
c      FCSR = dexp(-0.5d0*(tb*ff(k))**2.0d0)
c      
      PISRLOWex   = xNISRex*NTOTAL/XNORM
c
      PISRHIGHex  = xNISRex*NTOTAL/XNORM
c      
      PISRLOWco   = (1.0d0 - xNISRex)*NTOTAL/XNORM
c
      PISRHIGHco  = (1.0d0 - xNISRex - xNCSR)*NTOTAL/XNORM
c
      PCSRHIGH    = ((1.0d0 - FCSR) + NCSRHIGH*FCSR)*xNCSR*NTOTAL/XNORM
c
      PCSRHIGH0   = xNCSR*NTOTAL/XNORM     
c 
c------------------------------------------------------------
c
c     calculate refraction indexes
c 
c------------------------------------------------------------
c       
      call refr(ffbco,ffpco,cs,an1co,an2co,ath1co,ath2co)
      call refr(ffbex,ffpex,cs,an1ex,an2ex,ath1ex,ath2ex)      
c 
c------------------------------------------------------------
c
c     start integration over energy
c 
c------------------------------------------------------------
c
      e1ISRlowco=0.0d0
      e2ISRlowco=0.0d0
      a1ISRlowco=0.0d0
      a2ISRlowco=0.0d0
c
      e1ISRhighco=0.0d0
      e2ISRhighco=0.0d0
      a1ISRhighco=0.0d0
      a2ISRhighco=0.0d0
c
      e1ISRlowex=0.0d0
      e2ISRlowex=0.0d0
      a1ISRlowex=0.0d0
      a2ISRlowex=0.0d0
c
      e1ISRhighex=0.0d0
      e2ISRhighex=0.0d0
      a1ISRhighex=0.0d0
      a2ISRhighex=0.0d0
c      
      e1CSR=0.0d0
      e2CSR=0.0d0
      a1CSR=0.0d0
      a2CSR=0.0d0
c 
      do j=j1,j2
      el=10.0d0**(0.05d0*(j-1)-2)
      eu=10.0d0**(0.05d0*j-2)
      em=10.0d0**(0.05d0*(j-0.5)-2)
      de=eu-el
      gamma=em/E0+1.0d0
      gtr=etr/E0+1.0d0
c
      g1co=0.0d0
      g2co=0.0d0
      g1ex=0.0d0
      g2ex=0.0d0      
c
      if(gamma.lt.gtr)then
c
        if(ffbco.gt.ffpco)then
        anco=dsqrt(an1co)
        call gsy(gamma,ffbco,cs,g1co,anco,ath1co)
        endif
c
        if(ffbco.gt.(dsqrt(ffpco**2.0d0+0.25d0)+0.5d0))then
        anco=dsqrt(an2co)
        call gsy(gamma,ffbco,cs,g2co,anco,ath2co)
        endif
c      
        if(ffbex.gt.ffpex)then
        anex=dsqrt(an1ex)
        call gsy(gamma,ffbex,cs,g1ex,anex,ath1ex)
        endif
c 
        if(ffbex.gt.(dsqrt(ffpex**2.0d0+0.25d0)+0.5d0))then
        anex=dsqrt(an2ex)
        call gsy(gamma,ffbex,cs,g2ex,anex,ath2ex)
        endif        
c        
      elseif(gamma.ge.gtr)then
c 
        if(ffbco.gt.(dsqrt(ffpco**2.0d0+0.25d0)+0.5d0))then
        call ssy(gamma,ffbco,cs,g1co,g2co,alphaco)
        endif
c       
        if(ffbex.gt.(dsqrt(ffpex**2.0d0+0.25d0)+0.5d0))then
        call ssy(gamma,ffbex,cs,g1ex,g2ex,alphaex)
        endif
c         
      endif
c      
c------------------------------------------------------------
c
c     calculate the contribution to ISR from low-energy electrons
c 
c------------------------------------------------------------
c 
      if(em.lt.ecsr)then
c
c     compact source
c
      e1ISRlowco=e1ISRlowco+g1co*em**(-delta)*de*bmagco*emfac*PISRLOWco
c
      e2ISRlowco=e2ISRlowco+g2co*em**(-delta)*de*bmagco*emfac*PISRLOWco 
c
      a1ISRlowco=a1ISRlowco+g1co*em**(-delta)*de/ffbco/ffbco/bmagco
     &  *abfac*(delta*gamma*(gamma+1.0d0)+2.0d0*gamma*gamma-1.0d0)
     &  /gamma/(gamma**2.0d0-1.0d0)
     &  *PISRLOWco
c
      a2ISRlowco=a2ISRlowco+g2co*em**(-delta)*de/ffbco/ffbco/bmagco
     &  *abfac*(delta*gamma*(gamma+1.0d0)+2.0d0*gamma*gamma-1.0d0)
     &  /gamma/(gamma**2.0d0-1.0d0)
     &  *PISRLOWco  
c
c     extended source
c
      e1ISRlowex=e1ISRlowex+g1ex*em**(-delta)*de*bmagex*emfac*PISRLOWex
c
      e2ISRlowex=e2ISRlowex+g2ex*em**(-delta)*de*bmagex*emfac*PISRLOWex 
c
      a1ISRlowex=a1ISRlowex+g1ex*em**(-delta)*de/ffbex/ffbex/bmagex
     &  *abfac*(delta*gamma*(gamma+1.0d0)+2.0d0*gamma*gamma-1.0d0)
     &  /gamma/(gamma**2.0d0-1.0d0)
     &  *PISRLOWex
c
      a2ISRlowex=a2ISRlowex+g2ex*em**(-delta)*de/ffbex/ffbex/bmagex
     &  *abfac*(delta*gamma*(gamma+1.0d0)+2.0d0*gamma*gamma-1.0d0)
     &  /gamma/(gamma**2.0d0-1.0d0)
     &  *PISRLOWex  
c
      else
c 
c------------------------------------------------------------
c
c     calculate the contribution to ISR from high-energy
c 
c------------------------------------------------------------
c
c     compact source
c      
      e1ISRhighco=e1ISRhighco+g1co*em**(-delta)*de*bmagco*emfac
     &           *PISRHIGHco
c
      e2ISRhighco=e2ISRhighco+g2co*em**(-delta)*de*bmagco*emfac
     &           *PISRHIGHco
c
      a1ISRhighco=a1ISRhighco+g1co*em**(-delta)*de/ffbco/ffbco/bmagco
     &  *abfac*(delta*gamma*(gamma+1.0d0)+2.0d0*gamma*gamma-1.0d0)
     &  /gamma/(gamma**2.0d0-1.0d0)
     &  *(PISRHIGHco)
c
      a2ISRhighco=a2ISRhighco+g2co*em**(-delta)*de/ffbco/ffbco/bmagco
     &  *abfac*(delta*gamma*(gamma+1.0d0)+2.0d0*gamma*gamma-1.0d0)
     &  /gamma/(gamma**2.0d0-1.0d0)
     &  *(PISRHIGHco)
c
c     extended source
c      
      e1ISRhighex=e1ISRhighex+g1ex*em**(-delta)*de*bmagex*emfac
     &           *PISRHIGHex
c
      e2ISRhighex=e2ISRhighex+g2ex*em**(-delta)*de*bmagex*emfac
     &           *PISRHIGHex
c
      a1ISRhighex=a1ISRhighex+g1ex*em**(-delta)*de/ffbex/ffbex/bmagex
     &  *abfac*(delta*gamma*(gamma+1.0d0)+2.0d0*gamma*gamma-1.0d0)
     &  /gamma/(gamma**2.0d0-1.0d0)
     &  *(PISRHIGHex)
c
      a2ISRhighex=a2ISRhighex+g2ex*em**(-delta)*de/ffbex/ffbex/bmagex
     &  *abfac*(delta*gamma*(gamma+1.0d0)+2.0d0*gamma*gamma-1.0d0)
     &  /gamma/(gamma**2.0d0-1.0d0)
     &  *(PISRHIGHex)
c  
c------------------------------------------------------------
c 
c    calculate the contribution from high-energy electrons to CSR
c 
c------------------------------------------------------------
c      
      e1CSR=e1CSR+g1co*em**(-delta)*de*bmagco*emfac*(PCSRHIGH)
c
      e2CSR=e2CSR+g2co*em**(-delta)*de*bmagco*emfac*(PCSRHIGH)  
c
      a1CSR=a1CSR+g1co*em**(-delta)*de/ffbco/ffbco/bmagco*abfac
     &  *(delta*gamma*(gamma+1.0d0)+2.0d0*gamma*gamma-1.0d0)
     &  /gamma/(gamma**2.0d0-1.0d0)
     &  *(PCSRHIGH0)
c
      a2CSR=a2CSR+g2co*em**(-delta)*de/ffbco/ffbco/bmagco*abfac
     &  *(delta*gamma*(gamma+1.0d0)+2.0d0*gamma*gamma-1.0d0)
     &  /gamma/(gamma**2.0d0-1.0d0)
     &  *(PCSRHIGH0)
c  
      endif       
      enddo
c 
c------------------------------------------------------------
c
c     end of integration over energy
c 
c------------------------------------------------------------
c
c     emissivities and absorption coefficients from low-energy electrons
c 
c------------------------------------------------------------
c
c     compact source
c
      e1dISRlowco(k)=e1ISRlowco/SCvolume
      e2dISRlowco(k)=e2ISRlowco/SCvolume
      a1dISRlowco(k)=a1ISRlowco/SCvolume
      a2dISRlowco(k)=a2ISRlowco/SCvolume
c
c     extended source
c
      e1dISRlowex(k)=e1ISRlowex/SEvolume
      e2dISRlowex(k)=e2ISRlowex/SEvolume
      a1dISRlowex(k)=a1ISRlowex/SEvolume
      a2dISRlowex(k)=a2ISRlowex/SEvolume
c  
c------------------------------------------------------------
c
c     emissivities and absorption coefficients from high-energy electrons
c     participating in the ISR process
c 
c------------------------------------------------------------
c
c     compact source
c
      e1dISRhighco(k)=e1ISRhighco/SCvolume
      e2dISRhighco(k)=e2ISRhighco/SCvolume
      a1dISRhighco(k)=a1ISRhighco/SCvolume
      a2dISRhighco(k)=a2ISRhighco/SCvolume
c 
c     extended source
c
      e1dISRhighex(k)=e1ISRhighex/SEvolume
      e2dISRhighex(k)=e2ISRhighex/SEvolume
      a1dISRhighex(k)=a1ISRhighex/SEvolume
      a2dISRhighex(k)=a2ISRhighex/SEvolume
c 
c------------------------------------------------------------
c
c     emissivities and absorption coefficients from low- and high-energy electrons
c     participating in the ISR process
c 
c------------------------------------------------------------
c
c     compact source
c
      e1dISRco(k)=(e1ISRlowco + e1ISRhighco)/SCvolume
      e2dISRco(k)=(e2ISRlowco + e2ISRhighco)/SCvolume
      a1dISRco(k)=(a1ISRlowco + a1ISRhighco)/SCvolume
      a2dISRco(k)=(a2ISRlowco + a2ISRhighco)/SCvolume
c
c     extended source
c
      e1dISRex(k)=(e1ISRlowex + e1ISRhighex)/SEvolume
      e2dISRex(k)=(e2ISRlowex + e2ISRhighex)/SEvolume
      a1dISRex(k)=(a1ISRlowex + a1ISRhighex)/SEvolume
      a2dISRex(k)=(a2ISRlowex + a2ISRhighex)/SEvolume
c
c     compact + extended sources
c
      e1dISR(k)=e1dISRco(k) + e1dISRex(k)
      e2dISR(k)=e2dISRco(k) + e2dISRex(k)
      a1dISR(k)=a1dISRco(k) + a1dISRex(k)
      a2dISR(k)=a2dISRco(k) + a2dISRex(k)
c  
c------------------------------------------------------------
c
c     emissivities and absorption coefficients from high-energy electrons
c     participating in the CSR process
c 
c------------------------------------------------------------
c     
      e1dCSR(k)=e1CSR/MBvolume
      e2dCSR(k)=e2CSR/MBvolume
      a1dCSR(k)=a1CSR/MBvolume
      a2dCSR(k)=a2CSR/MBvolume
c
c      write(*,*)'ffbco= ',ffbco,'  ffbex= ',ffbex
c 
c------------------------------------------------------------
c
c     calculate ISR polarization
c 
c------------------------------------------------------------
c
c     compact source
c      
      if(e2dISRco(k).gt.0.0d0)then
        rccdISRco(k)=(e2dISRco(k)-e1dISRco(k))/(e2dISRco(k)+e1dISRco(k))
      else
        rccdISRco(k)=0.0d0
      endif
c
c     extended source
c      
      if(e2dISRex(k).gt.0.0d0)then
        rccdISRex(k)=(e2dISRex(k)-e1dISRex(k))/(e2dISRex(k)+e1dISRex(k))
      else
        rccdISRex(k)=0.0d0
      endif
c
c     compact + extended sources
c      
      if(e2dISR(k).gt.0.0d0)then
        rccdISR(k)=(e2dISR(k)-e1dISR(k))/(e2dISR(k)+e1dISR(k))
      else
        rccdISR(k)=0.0d0
      endif
c  
c------------------------------------------------------------
c
c     calculate CSR polarization
c 
c------------------------------------------------------------
c      
      if(e2dCSR(k).gt.0.0d0)then
        rccdCSR(k)=(e2dCSR(k)-e1dCSR(k))/(e2dCSR(k)+e1dCSR(k))
      else
        rccdCSR(k)=0.0d0
      endif
c 
c------------------------------------------------------------
c
c     calculate ISR + CSR polarization
c 
c------------------------------------------------------------
c      
      if((e2dISR(k)+e2dCSR(k)).gt.0.0d0)then
        rccd(k)=((e2dISR(k)+e2dCSR(k))-(e2dISR(k)+e1dCSR(k)))
     $         /((e2dISR(k)+e2dCSR(k))+(e2dISR(k)+e1dCSR(k)))
      else
        rccd(k)=0.0d0
      endif
c 
c------------------------------------------------------------
c
c     calculate flux contributions
c 
c------------------------------------------------------------
c
c     ISR
c 
c------------------------------------------------------------
c
c     compact source
c
      arg1ISRco=a1dISRco(k)*SCheight
c      
      if(arg1ISRco.lt.0.001d0)then
c      
        phi1ISRco(k)=e1dISRco(k)*SCvolume/AU/AU
c        
        phi1ISRlowco(k)=e1dISRlowco(k)*SCvolume/AU/AU
c
        phi1ISRhighco(k)=e1dISRhighco(k)*SCvolume/AU/AU
c        
      else
c      
        phi1ISRco(k)=SComega*e1dISRco(k)/a1dISRco(k)
     &              *(1.0d0-dexp(-arg1ISRco))
c     
        phi1ISRlowco(k)=SComega*e1dISRlowco(k)/a1dISRco(k)
     &              *(1.0d0-dexp(-arg1ISRco))
c     
        phi1ISRhighco(k)=SComega*e1dISRhighco(k)/a1dISRco(k)
     &              *(1.0d0-dexp(-arg1ISRco))
c     
      endif                          
c
      arg2ISRco=a2dISRco(k)*SCheight
c      
      if(arg2ISRco.lt.0.001d0)then
c      
        phi2ISRco(k)=e2dISRco(k)*SCvolume/AU/AU
c        
        phi2ISRlowco(k)=e2dISRlowco(k)*SCvolume/AU/AU
c        
        phi2ISRhighco(k)=e2dISRhighco(k)*SCvolume/AU/AU
c        
      else
c      
        phi2ISRco(k)=SComega*e2dISRco(k)/a2dISRco(k)
     &              *(1.0d0-dexp(-arg2ISRco))
c     
        phi2ISRlowco(k)=SComega*e2dISRlowco(k)/a2dISRco(k)
     &              *(1.0d0-dexp(-arg2ISRco))
c     
        phi2ISRhighco(k)=SComega*e2dISRhighco(k)/a2dISRco(k)
     &              *(1.0d0-dexp(-arg2ISRco))
c     
      endif                                
c
c     extended source
c
      arg1ISRex=a1dISRex(k)*SEheight
c      
      if(arg1ISRex.lt.0.001d0)then
c      
        phi1ISRex(k)=e1dISRex(k)*SEvolume/AU/AU
c        
        phi1ISRlowex(k)=e1dISRlowex(k)*SEvolume/AU/AU
c        
        phi1ISRhighex(k)=e1dISRhighex(k)*SEvolume/AU/AU
c        
      else
c      
        phi1ISRex(k)=SEomega*e1dISRex(k)/a1dISRex(k)
     &              *(1.0d0-dexp(-arg1ISRex))
c     
        phi1ISRlowex(k)=SEomega*e1dISRlowex(k)/a1dISRex(k)
     &              *(1.0d0-dexp(-arg1ISRex))
c     
        phi1ISRhighex(k)=SEomega*e1dISRhighex(k)/a1dISRex(k)
     &              *(1.0d0-dexp(-arg1ISRex))
c     
      endif                          
c
      arg2ISRex=a2dISRex(k)*SEheight
c      
      if(arg2ISRex.lt.0.001d0)then
c      
        phi2ISRex(k)=e2dISRex(k)*SEvolume/AU/AU
c        
        phi2ISRlowex(k)=e2dISRlowex(k)*SEvolume/AU/AU
c        
        phi2ISRhighex(k)=e2dISRhighex(k)*SEvolume/AU/AU
c        
      else
c      
        phi2ISRex(k)=SEomega*e2dISRex(k)/a2dISRex(k)
     &              *(1.0d0-dexp(-arg2ISRex))
c     
        phi2ISRlowex(k)=SEomega*e2dISRlowex(k)/a2dISRex(k)
     &              *(1.0d0-dexp(-arg2ISRex))
c     
        phi2ISRhighex(k)=SEomega*e2dISRhighex(k)/a2dISRex(k)
     &              *(1.0d0-dexp(-arg2ISRex))
c     
      endif                                
c  
c------------------------------------------------------------
c 
c     CSR
c 
c------------------------------------------------------------
c     
      arg1CSR=a1dCSR(k)*MBheight
c   
      if(arg1CSR.lt.0.001d0)then
c      
        phi1CSR(k)=e1dCSR(k)*MBvolume/AU/AU
c        
      else
c      
        phi1CSR(k)=MBomega*e1dCSR(k)/a1dCSR(k)*(1.0d0-dexp(-arg1CSR))
c        
      endif    
c      
      arg2CSR=a2dCSR(k)*MBheight
c     
      if(arg2CSR.lt.0.001d0)then
c      
        phi2CSR(k)=e2dCSR(k)*MBvolume/AU/AU
c        
      else
c      
        phi2CSR(k)=MBomega*e2dCSR(k)/a2dCSR(k)*(1.0d0-dexp(-arg2CSR))
c        
      endif   
c 
c------------------------------------------------------------
c
c     calculate Stokes parameters
c 
c------------------------------------------------------------
c
c     fluxes from electrons participating in ISR and CSR processes
c 
c------------------------------------------------------------
c
      phitISRco(k)=phi1ISRco(k)+phi2ISRco(k)
      phitISRex(k)=phi1ISRex(k)+phi2ISRex(k)      
      phitISR(k)=phitISRco(k)+phitISRex(k)
      phitCSR(k)=phi1CSR(k)+phi2CSR(k)
c 
c------------------------------------------------------------
c
c     fluxes from low- and high-energy electrons
c 
c------------------------------------------------------------
c
      phitLOWco(k)=phi1ISRlowco(k)+phi2ISRlowco(k) 
      phitLOWex(k)=phi1ISRlowex(k)+phi2ISRlowex(k)       
      phitLOW(k)=phitLOWco(k)+phitLOWex(k) 
c
      phitHIGHco(k)=phi1ISRhighco(k)+phi2ISRhighco(k) 
      phitHIGHex(k)=phi1ISRhighex(k)+phi2ISRhighex(k)       
      phitHIGH(k)=phitHIGHco(k)+phitHIGHex(k)+phi1CSR(k)+phi2CSR(k)
c 
c------------------------------------------------------------
c
c     Total flux: ISR + CSR
c 
c------------------------------------------------------------
c
      phi1ISR(k)=phi1ISRco(k)+phi1ISRex(k)
      phi2ISR(k)=phi2ISRco(k)+phi2ISRex(k)          
      phi1(k)=phi1ISR(k)+phi1CSR(k)
      phi2(k)=phi2ISR(k)+phi2CSR(k)
c
      phit(k)=phi1(k)+phi2(k)      
c 
c------------------------------------------------------------
c
c     calculate ISR polarization
c 
c------------------------------------------------------------
c
c     compact source
c      
      qISRco(k)=phi1ISRco(k)*(1.0d0-ath1co**2.0d0)/(1.0d0+ath1co**2.0d0)
     &         +phi2ISRco(k)*(1.0d0-ath2co**2.0d0)/(1.0d0+ath2co**2.0d0)
c
      vISRco(k)=2.0d0*(phi1ISRco(k)*ath1co/(1.0d0+ath1co**2.0d0)
     &         +phi2ISRco(k)*ath2co/(1.0d0+ath2co**2.0d0))
c     
      if(phitISRco(k).gt.0.0d0)then
        rcdISRco(k)=(phi2ISRco(k)-phi1ISRco(k))/phitISRco(k)
        scdISRco(k)=vISRco(k)/abs(vISRco(k))
     &           *dsqrt(qISRco(k)**2.0d0+vISRco(k)**2.0d0)/phitISRco(k)
      else
        rcdISRco(k)=0.0d0
        scdISRco(k)=0.0d0
      endif
c
c     extended source
c      
      qISRex(k)=phi1ISRex(k)*(1.0d0-ath1ex**2.0d0)/(1.0d0+ath1ex**2.0d0)
     &         +phi2ISRex(k)*(1.0d0-ath2ex**2.0d0)/(1.0d0+ath2ex**2.0d0)
c
      vISRex(k)=2.0d0*(phi1ISRex(k)*ath1ex/(1.0d0+ath1ex**2.0d0)
     &         +phi2ISRex(k)*ath2ex/(1.0d0+ath2ex**2.0d0))
c     
      if(phitISRex(k).gt.0.0d0)then
        rcdISRex(k)=(phi2ISRex(k)-phi1ISRex(k))/phitISRex(k)
        scdISRex(k)=vISRex(k)/abs(vISRex(k))
     &           *dsqrt(qISRex(k)**2.0d0+vISRex(k)**2.0d0)/phitISRex(k)
      else
        rcdISRex(k)=0.0d0
        scdISRex(k)=0.0d0
      endif
c
c     compact + extended sources
c      
      qISR(k)=qISRco(k) + qISRex(k)
c
      vISR(k)=vISRco(k) + vISRex(k)
c     
      if(phitISR(k).gt.0.0d0)then
        rcdISR(k)=(phi2ISR(k)-phi1ISR(k))/phitISR(k)
        scdISR(k)=vISR(k)/abs(vISRco(k))
     &           *dsqrt(qISR(k)**2.0d0+vISR(k)**2.0d0)/phitISR(k)
      else
        rcdISR(k)=0.0d0
        scdISR(k)=0.0d0
      endif
c   
c------------------------------------------------------------
c
c     calculate CSR polarization 
c 
c------------------------------------------------------------
c     
      qCSR(k)=phi1CSR(k)*(1.0d0-ath1co**2.0d0)/(1.0d0+ath1co**2.0d0)+
     &        phi2CSR(k)*(1.0d0-ath2co**2.0d0)/(1.0d0+ath2co**2.0d0)
c
      vCSR(k)=2.0d0*(phi1CSR(k)*ath1co/(1.0d0+ath1co**2.0d0)
     &           +phi2CSR(k)*ath2co/(1.0d0+ath2co**2.0d0))
c     
      if(phitCSR(k).gt.0.0d0)then
        rcdCSR(k)=(phi2CSR(k)-phi1CSR(k))/phitCSR(k)
        scdCSR(k)=vCSR(k)/abs(vCSR(k))
     &           *dsqrt(qCSR(k)**2.0d0+vCSR(k)**2.0d0)/phitCSR(k)
      else
        rcdCSR(k)=0.0d0
        scdCSR(k)=0.0d0
      endif   
c 
c------------------------------------------------------------
c
c     calculate total (ISR + CSR) polarization 
c 
c------------------------------------------------------------
c      
      q(k)=qISR(k) + qCSR(k)
c
      v(k)=vISR(k) + vCSR(k)
c 
      if(phit(k).gt.0.0d0)then
        rcd(k)=(phi2(k)-phi1(k))/phit(k)
        scd(k)=v(k)/abs(v(k))*sqrt(q(k)**2.0d0+v(k)**2.0d0)/phit(k)
      else
        rcd(k)=0.0d0
        scd(k)=0.0d0
      endif
c      
      enddo
c 
c------------------------------------------------------------
c      
c     end of loop over ff
c 
c------------------------------------------------------------
c
c     write output
c 
c------------------------------------------------------------
c
      write(10,*)'=================================================='
      write(10,*)''      
      write(10,*)'                 Parameters                       '
      write(10,*)''      
      write(10,*)'=================================================='
c
      write(10,*)''   
      write(10,'(A, F6.1)')   ' delta      = ',delta
      write(10,'(A, F6.2,A)') ' Emin       = ',Emin,' MeV'
      write(10,'(A, F6.2,A)') ' Emax       = ',Emax,' MeV'
      write(10,*)''
      write(10,'(A, F6.1,A)') ' bmagco     = ',bmagco,' gauss' 
      write(10,'(A, F6.1,A)') ' bmagex     = ',bmagex,' gauss'       
      write(10,*)''      
      write(10,'(A, e25.20)') ' NTOTAL     = ',NTOTAL
      write(10,'(A, e25.20)') ' NLOW       = ',NLOW
      write(10,'(A, e25.20)') ' NHIGH      = ',NHIGH
      write(10,*)''      
      write(10,'(A, e25.20)') ' NCSRHIGH   = ',NCSRHIGH
      write(10,*)''      
      write(10,'(A, e25.20)') ' NISRLOWco  = ',NISRLOWco
      write(10,'(A, e25.20)') ' NISRHIGHco = ',NISRHIGHco
      write(10,'(A, e25.20)') ' NISRco     = ',NISRco 
      write(10,*)''     
      write(10,'(A, e25.20)') ' NISRLOWex  = ',NISRLOWex
      write(10,'(A, e25.20)') ' NISRHIGHex = ',NISRHIGHex
      write(10,'(A, e25.20)') ' NISRex     = ',NISRex 
      write(10,*)''      
      write(10,'(A, e25.20)') ' NISRLOW    = ',NISRLOWco + NISRLOWex
      write(10,'(A, e25.20)') ' NISRHIGH   = ',NISRHIGHco + NISRHIGHex       
      write(10,'(A, e25.20)') ' NISR       = ',NISRco + NISRex  
      write(10,*)''   
      write(10,'(A, F6.1,A)') ' etr        = ',etr,' MeV'      
      write(10,'(A, F6.1,A)') ' ecsr       = ',ecsr,' MeV'
      write(10,'(A, f6.1,A)') ' tb         = ',tb/1.e-12,' ps'
      write(10,'(A, F6.1,A)') ' vangle     = ',viewangle,' degrees' 
      write(10,*)''      
      write(10,'(A, e11.3,A)')' SCsize     = ',2.0*SCradius,' cm'
      write(10,'(A, e11.3,A)')' SCheight   = ',SCheight,' cm' 
      write(10,'(A, e11.3,A)')' SEsize     = ',2.0*SEradius,' cm'
      write(10,'(A, e11.3,A)')' SEheight   = ',SEheight,' cm' 
      write(10,*)''      
      write(10,'(A, e11.3,A)')' SCarea     = ',SCarea,' cm^2'
      write(10,'(A, e11.3,A)')' SCvolume   = ',SCvolume,' cm^3' 
      write(10,'(A, e11.3,A)')' SEarea     = ',SEarea,' cm^2'
      write(10,'(A, e11.3,A)')' SEvolume   = ',SEvolume,' cm^3'       
      write(10,*)''    
      write(10,'(A, e11.3,A)')' MBsize     = ',2.0*MBradius,' cm'
      write(10,'(A, e11.3,A)')' MBheight   = ',MBheight,' cm'  
      write(10,*)''  
      write(10,'(A, e11.3,A)')' neCO       = ',SCdensity,' cm^-3'
      write(10,'(A, e11.3,A)')' neEX       = ',SEdensity,' cm^-3'
      write(10,'(A, e11.3,A)')' neMB       = ',MBdensity,' cm^-3' 
      write(10,'(A, e11.3,A)')' npco       = ',npco,' cm^-3' 
      write(10,'(A, e11.3,A)')' npex       = ',npex,' cm^-3'        
      write(10,*)''     
      write(10,'(A, e8.2)')   ' alphaco    = ',alphaco       
      write(10,'(A, f6.2,A)') ' vbco       = ',vbco/1.e9,' GHz'         
      write(10,'(A, f6.2,A)') ' vpco       = ',vpco/1.e9,' GHz'        
      write(10,'(A, f6.2,A)') ' vrco       = ',vrco/1.e9,' GHz' 
      write(10,*)''         
      write(10,'(A, e8.2)')   ' alphaex    = ',alphaex       
      write(10,'(A, f6.2,A)') ' vbex       = ',vbex/1.e9,' GHz'         
      write(10,'(A, f6.2,A)') ' vpex       = ',vpex/1.e9,' GHz'        
      write(10,'(A, f6.2,A)') ' vrex       = ',vrex/1.e9,' GHz'       
      write(10,*)''   
c    
      write(10,*)'=================================================='
      write(10,*)''
      write(10,*)'ISR coefficients - compact source'
      write(10,*)''      
      write(10,*)'=================================================='
      write(10,*)'  f          emissivities      absorption coefficients
     &    pol'
      write(10,*)'  Hz       erg/(cm^3 s^1 sr^1 Hz^1)      cm^-1' 
      write(10,*)''    
      write(10,*)'             o mode     x mode     o mode     x mode'
      write(10,*)''      
c      
      do k=1,kf
      write(10,101)ff(k),e1dISRco(k),e2dISRco(k),
     $             a1dISRco(k),a2dISRco(k),rccdISRco(k),k
      enddo 
c           
      write(10,*)''
      write(10,*)'ISR fluxes (SFU) - compact source'    
      write(10,*)''        
      write(10,*)'=================================================='
      write(10,*)'     f(Hz)        phi_o       phi_x        phi_tot
     &      pol'
      write(10,*)''      
c    
      do k=1,kf
      write(10,102)ff(k),phi1ISRco(k)*SFU+1.0d-23,
     &             phi2ISRco(k)*SFU+1.0d-23,phitISRco(k)*SFU+1.0d-23,
     &             rcdISRco(k),k
      enddo
c    
      write(10,*)'=================================================='
      write(10,*)''
      write(10,*)'ISR coefficients - extended source'
      write(10,*)''      
      write(10,*)'=================================================='
      write(10,*)'  f          emissivities      absorption coefficients
     &    pol'
      write(10,*)'  Hz       erg/(cm^3 s^1 sr^1 Hz^1)      cm^-1' 
      write(10,*)''    
      write(10,*)'             o mode     x mode     o mode     x mode'
      write(10,*)''      
c      
      do k=1,kf
      write(10,101)ff(k),e1dISRex(k),e2dISRex(k),
     $             a1dISRex(k),a2dISRex(k),rccdISRex(k),k
      enddo 
c           
      write(10,*)''
      write(10,*)'ISR fluxes (SFU) - extended source'
      write(10,*)''        
      write(10,*)'=================================================='
      write(10,*)'     f(Hz)        phi_o       phi_x        phi_tot
     &      pol'
      write(10,*)''
c    
      do k=1,kf
      write(10,102)ff(k),phi1ISRex(k)*SFU+1.0d-23,
     &             phi2ISRex(k)*SFU+1.0d-23,phitISRex(k)*SFU+1.0d-23,
     &             rcdISRex(k),k
      enddo
c    
      write(10,*)'=================================================='
      write(10,*)''
      write(10,*)'ISR coefficients - compact + extended sources'
      write(10,*)''      
      write(10,*)'=================================================='
      write(10,*)'  f          emissivities      absorption coefficients
     &    pol'
      write(10,*)'  Hz       erg/(cm^3 s^1 sr^1 Hz^1)      cm^-1' 
      write(10,*)''    
      write(10,*)'             o mode     x mode     o mode     x mode'
      write(10,*)''      
c      
      do k=1,kf
      write(10,101)ff(k),e1dISR(k),e2dISR(k),a1dISR(k),a2dISR(k),
     $             rccdISR(k),k
      enddo 
c           
      write(10,*)''
      write(10,*)'ISR fluxes (SFU) - compact + extended sources'
      write(10,*)''        
      write(10,*)'=================================================='
      write(10,*)'     f(Hz)        phi_o       phi_x        phi_tot
     &      pol'
      write(10,*)''
c    
      do k=1,kf
      write(10,102)ff(k),phi1ISR(k)*SFU+1.0d-23,phi2ISR(k)*SFU+1.0d-23,
     &             phitISR(k)*SFU+1.0d-23,rcdISR(k),k
      enddo
c
      write(10,*)''
      write(10,*)'CSR coefficients'
      write(10,*)''      
      write(10,*)'=================================================='
      write(10,*)'  f          emissivities      absorption coefficients
     &    pol'
      write(10,*)'  Hz       erg/(cm^3 s^1 sr^1 Hz^1)      cm^-1' 
      write(10,*)''    
      write(10,*)'             o mode     x mode     o mode     x mode'
      write(10,*)''      
c      
      do k=1,kf
      write(10,101)ff(k),e1dCSR(k),e2dCSR(k),a1dCSR(k),a2dCSR(k),
     $             rccdCSR(k),k
      enddo 
c           
      write(10,*)''
      write(10,*)'CSR fluxes (SFU)'
      write(10,*)''        
      write(10,*)'=================================================='
      write(10,*)'     f(Hz)        phi_o       phi_x        phi_tot
     &      pol'
      write(10,*)''
c    
      do k=1,kf
      write(10,102)ff(k),phi1CSR(k)*SFU+1.0d-23,phi2CSR(k)*SFU+1.0d-23,
     &             phitCSR(k)*SFU+1.0d-23,rcdCSR(k),k
      enddo
c 
c------------------------------------------------------------
c
c     convert fluxes from erg cm^-2 s^-1 Hz^-1 to W m^-2 s^-1 Hz^-1 and to SFU
c 
c------------------------------------------------------------
c
      do k=1,kf
      if(ff(k).ge.vbco)then   
      write(11,202)ff(k),phitISR(k)*SFU!+1.0d-23
      write(12,202)ff(k),phitCSR(k)*SFU!+1.0d-23
      write(13,202)ff(k),phitLOW(k)*SFU!+1.0d-23      
      write(14,202)ff(k),phitHIGH(k)*SFU!+1.0d-23
      write(15,202)ff(k),phitISRco(k)*SFU!+1.0d-23      
      write(16,202)ff(k),phitISRex(k)*SFU!+1.0d-23      
      write(17,202)ff(k),phit(k)*SFU!+1.0d-23
      write(18,203)ff(k),rccd(k),rcd(k),k    
      else
      continue
      endif  
      enddo
c    
200   format(12x,e15.5)
201   format(12x,i10)        
101   format(e11.3,5e11.3,i5) 
102   format(5e13.3,i5)
202   format(2e15.5) 
203   format(3e13.3,i5)  
c     
      stop
      end
c 
c------------------------------------------------------------
c
c     end of main program
c 
c------------------------------------------------------------
c      
c--------synchrotron routine---------------------------------
c 
c------------------------------------------------------------
c
      subroutine ssy(gamma,ffb,cs,g1,g2,alpha)
      dimension xd(34),fd(34)
      data xd/0.001,0.005,0.01,0.025,0.05,0.075,0.1,0.15,0.2,0.25,
     &        0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.,1.2,1.4,1.6,1.8,2.,2.5,
     &        3.,3.5,4.,4.5,5.,6.,7.,8.,9.,10./
      data fd/0.213,0.358,0.445,0.583,0.702,0.722,0.818,0.874,0.904,
     &        0.917,0.919,0.901,0.872,0.832,0.788,0.742,0.694,0.655,
     &        0.566,0.486,0.414,0.354,0.301,0.2,0.13,0.0845,0.0541,
     &        0.0339,0.0214,0.0085,0.0033,0.0013,0.0005,0.00019/
      ss=sqrt(1.-cs*cs)
      ffc=ffb*2./3./ss/gamma/gamma*
     &    (1.+9./4.*(gamma*gamma-1.)/alpha/alpha/ffb/ffb)**1.5
      if(ffc.le.xd(1))then
      f=2.15*ffc**0.333*(1.-0.844*ffc**0.6666)
      elseif(ffc.gt.xd(34))then
      f=1.253*exp(-ffc)*sqrt(ffc)*(1.+55./72/ffc)
      else 
      call sear(xd,fd,ffc,f)
      endif 
      gtot=0.138*ss*f
      g1=gtot/2.
      g2=gtot/2.
      return
      end
c 
c------------------------------------------------------------
c      
c-----------search routine-----------------------------------
c 
c------------------------------------------------------------
c
      SUBROUTINE SEAR(XC,SGC,X,S)
      DIMENSION XC(34),SGC(34)
      I=1
   12 IF(XC(I).GE.X)GO TO  13
      I=I+1
      GO TO 12
   13 S=(SGC(I)*(X-XC(I-1)  )+SGC(I-1)*(XC(I)   -X ))/(XC(I)-XC(I-1))
      RETURN
      END
c 
c------------------------------------------------------------
c      
c------------gyrosynchrotron routine-------------------------
c 
c------------------------------------------------------------
c
      subroutine gsy(gamma,ffb,cs,g12,an,ath)
      beta=sqrt(gamma*gamma-1.)/gamma
      ss=sqrt(1.-cs*cs)
      ffc=ffb*2./3./ss/gamma/gamma
      if(ffc.ge.20.)then
      g12=0.
      return
      else
      is1=ffb*gamma*(1.-an*beta*cs)+1
      is2=ffb*gamma*(1.+an*beta*cs)
      sum12=0.
      do i=is1,is2
      cphis=(1.-i/ffb/gamma)/beta/cs/an
      sphis=sqrt(1.-cphis*cphis)
      xs=i*an*beta*ss*sphis/(1.-an*beta*cs*cphis)
      if(ffb.gt.50.)then
      xstr=0.8
      if(gamma.gt.5)xstr=0.9
      if(gamma.gt.10)xstr=0.95
      if(gamma.gt.15)xstr=0.96
      if(xs.lt.xstr*i)goto 10
      endif
      if(ffb.gt.250.)then
      xstr=0.9
      if(gamma.gt.5)xstr=0.92
      if(gamma.gt.10)xstr=0.97
      if(gamma.gt.15)xstr=0.98
      if(xs.lt.xstr*i)goto 10
      endif
      call bes(i,xs,b)
      call bespr(i,xs,bpr)
      f12=(-beta*sphis*bpr+ath*(cs/ss/an-beta*cphis/ss)*b)**2
      s12old=sum12
      sum12=sum12+f12
      if(s12old.gt.0.0)then
      if((sum12-s12old)/s12old.lt.1.e-4)goto 11
      endif
c      write(11,100)gamma,i,xs,f12,sum12
100   format(f10.3,i5,3e12.3)
10    continue
      enddo
11    continue
      g12=sum12/beta/2./cs*ffb/(1.+ath**2)
      return
      endif
      end
c 
c------------------------------------------------------------
c      
c-----index of refraction and polarization coefficient-------
c 
c------------------------------------------------------------
c
      subroutine refr(ffb,ffp,cs,an1,an2,ath1,ath2)
      ss=sqrt(1.-cs*cs)
      anum=2.*ffp*ffp*(ffp*ffp-ffb*ffb)
      dnum1=+sqrt(ffb**4*ss**4+4.*ffb**2*(ffp**2-ffb**2)**2*cs**2)-
     &     2.*ffb**2*(ffp**2-ffb**2)-ffb**2*ss**2
      dnum2=-sqrt(ffb**4*ss**4+4.*ffb**2*(ffp**2-ffb**2)**2*cs**2)-
     &     2.*ffb**2*(ffp**2-ffb**2)-ffb**2*ss**2
      an1=1.+anum/dnum1
      an2=1.+anum/dnum2
      aknum=2.*ffb*(ffp*ffp-ffb*ffb)*cs
      dknum1=+sqrt(ffb**4*ss**4+4.*ffb**2*(ffp**2-ffb**2)**2*cs**2)-
     &     ffb**2*ss**2
      dknum2=-sqrt(ffb**4*ss**4+4.*ffb**2*(ffp**2-ffb**2)**2*cs**2)-
     &     ffb**2*ss**2
      ath1=-aknum/dknum1
      ath2=-aknum/dknum2
c      write(12,*)ffb,aknum,dknum1,dknum2
      return
      end
c 
c------------------------------------------------------------
c      
c--------------Bessel Function routines----------------------
c 
c------------------------------------------------------------
c
      subroutine bespr(n,x,bpr)
      n1=n+1
      call bes(n1,x,b1)
      call bes(n,x,b)
      bpr=-b1+n/x*b
      return
      end
      subroutine bes(n,x,b)
      if(n.eq.0)b=bessj0(x)
      if(n.eq.1)b=bessj1(x)
      if(n.ge.2)then
        b=bessj(n,x)
      endif
      return
      end
      FUNCTION BESSJ0(X)
      REAL*8 Y,P1,P2,P3,P4,P5,Q1,Q2,Q3,Q4,Q5,R1,R2,R3,R4,R5,R6,
     *    S1,S2,S3,S4,S5,S6
      DATA P1,P2,P3,P4,P5/1.D0,-.1098628627D-2,.2734510407D-4,
     *    -.2073370639D-5,.2093887211D-6/, Q1,Q2,Q3,Q4,Q5/-.1562499995D-
     *1,
     *    .1430488765D-3,-.6911147651D-5,.7621095161D-6,-.934945152D-7/
      DATA R1,R2,R3,R4,R5,R6/57568490574.D0,-13362590354.D0,651619640.7D
     *0,
     *    -11214424.18D0,77392.33017D0,-184.9052456D0/,
     *    S1,S2,S3,S4,S5,S6/57568490411.D0,1029532985.D0,
     *    9494680.718D0,59272.64853D0,267.8532712D0,1.D0/
      IF(ABS(X).LT.8.)THEN
        Y=X**2
        BESSJ0=(R1+Y*(R2+Y*(R3+Y*(R4+Y*(R5+Y*R6)))))
     *      /(S1+Y*(S2+Y*(S3+Y*(S4+Y*(S5+Y*S6)))))
      ELSE
        AX=ABS(X)
        Z=8./AX
        Y=Z**2
        XX=AX-.785398164
        BESSJ0=SQRT(.636619772/AX)*(COS(XX)*(P1+Y*(P2+Y*(P3+Y*(P4+Y
     *      *P5))))-Z*SIN(XX)*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*Q5)))))
      ENDIF
      RETURN
      END
      FUNCTION BESSJ1(X)
      REAL*8 Y,P1,P2,P3,P4,P5,Q1,Q2,Q3,Q4,Q5,R1,R2,R3,R4,R5,R6,
     *    S1,S2,S3,S4,S5,S6
      DATA R1,R2,R3,R4,R5,R6/72362614232.D0,-7895059235.D0,242396853.1D0
     *,
     *    -2972611.439D0,15704.48260D0,-30.16036606D0/,
     *    S1,S2,S3,S4,S5,S6/144725228442.D0,2300535178.D0,
     *    18583304.74D0,99447.43394D0,376.9991397D0,1.D0/
      DATA P1,P2,P3,P4,P5/1.D0,.183105D-2,-.3516396496D-4,.2457520174D-5
     *,
     *    -.240337019D-6/, Q1,Q2,Q3,Q4,Q5/.04687499995D0,-.2002690873D-3
     *,
     *    .8449199096D-5,-.88228987D-6,.105787412D-6/
      IF(ABS(X).LT.8.)THEN
        Y=X**2
        BESSJ1=X*(R1+Y*(R2+Y*(R3+Y*(R4+Y*(R5+Y*R6)))))
     *      /(S1+Y*(S2+Y*(S3+Y*(S4+Y*(S5+Y*S6)))))
      ELSE
        AX=ABS(X)
        Z=8./AX
        Y=Z**2
        XX=AX-2.356194491
        BESSJ1=SQRT(.636619772/AX)*(COS(XX)*(P1+Y*(P2+Y*(P3+Y*(P4+Y
     *      *P5))))-Z*SIN(XX)*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*Q5)))))
     *      *SIGN(1.,X)
      ENDIF
      RETURN
      END
      FUNCTION BESSJ(N,X)
      PARAMETER (IACC=40,BIGNO=1.E10,BIGNI=1.E-10)
      IF(N.LT.2)PAUSE 'bad argument N in BESSJ'
      TOX=2./X
      IF(X.GT.FLOAT(N))THEN
        BJM=BESSJ0(X)
        BJ=BESSJ1(X)
        DO 11 J=1,N-1
          BJP=J*TOX*BJ-BJM
          BJM=BJ
          BJ=BJP
11      CONTINUE
        BESSJ=BJ
      ELSE
        M=2*((N+INT(SQRT(FLOAT(IACC*N))))/2)
        BESSJ=0.
        JSUM=0
        SUM=0.
        BJP=0.
        BJ=1.
        DO 12 J=M,1,-1
          BJM=J*TOX*BJ-BJP
          BJP=BJ
          BJ=BJM
          IF(ABS(BJ).GT.BIGNO)THEN
            BJ=BJ*BIGNI
            BJP=BJP*BIGNI
            BESSJ=BESSJ*BIGNI
            SUM=SUM*BIGNI
          ENDIF
          IF(JSUM.NE.0)SUM=SUM+BJ
          JSUM=1-JSUM
          IF(J.EQ.N)BESSJ=BJP
12      CONTINUE
        SUM=2.*SUM-BJ
        BESSJ=BESSJ/SUM
      ENDIF
      RETURN
      END

