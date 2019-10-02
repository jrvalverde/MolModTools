
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 frmp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "frmp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "frmp.web"
      subroutine frmp(NBASIS,NOCC,OCC,C,P,PRINT)
      implicit none
      double precision C,cmupib,OCC,one,P,zero
      integer i,ibase,In,Iout,Ipunch,k,mu,NBASIS,NOCC,ntt,nu
      integer PRINT
      dimension C(*),P(*)
      common/io/In,Iout,Ipunch
      data zero/0.0D0/,one/1.0D0/
      
      
      
      
      
      
      
      
      
      
99001 format(' MOLECULAR ORBITAL COEFFICIENTS AT THE BEGINNING OF FRMP')
99002 format(' DENSITY MATRIX AT END OF FRMP.')
99003 format(1H )
      
      
      
      if(PRINT.GE.3)then
      write(Iout,99001)
      call linout(C,NBASIS,1)
      endif
      
      ntt=(NBASIS*(NBASIS+1))/2
      do 100 i=1,ntt
      P(i)=zero
100   continue
      
      ibase=0
      do 200 i=1,NOCC
      k=0
      do 150 mu=1,NBASIS
      cmupib=C(mu+ibase)
      do 120 nu=1,mu
      k=k+1
      P(k)=P(k)+cmupib*C(nu+ibase)
120   continue
150   continue
      ibase=ibase+NBASIS
200   continue
      
      if(OCC.NE.one)then
      do 250 i=1,ntt
      P(i)=OCC*P(i)
250   continue
      endif
      
      
      if(PRINT.GE.3)then
      write(Iout,99002)
      call ltoutd(NBASIS,P,1)
      write(Iout,99003)
      endif
      
      return
      
      end
C* :1 * 
      
