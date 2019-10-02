
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 frmpop"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "frmpop.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "frmpop.web"
      subroutine frmpop(NATOMS,IAN,NBASIS,NADIM,NBDIM,A,B,GAMMA)
      implicit none
      double precision A,B,GAMMA,zero
      integer ia,IAN,In,Iout,Ipunch,irwfci,irwps,len,mu,NADIM,NATOMS,NBA
     &SIS,NBDIM,nu
      dimension IAN(*)
      dimension A(NBDIM,NADIM),B(NBDIM,NBDIM),GAMMA(*)
      common/io/In,Iout,Ipunch
      data irwfci/517/,irwps/534/,zero/0.0D0/
      
      
      
      
      
      
      
      
99001 format(40H  FERMI CONTACT ANALYSIS (ATOMIC UNITS).)
      
      call tquery(irwfci,len)
      if(len.EQ.0)return
      call tquery(irwps,len)
      if(len.EQ.0)return
      write(Iout,99001)
      call tread(irwfci,A,NBDIM,NADIM,NBASIS,NATOMS,0)
      call tread(irwps,B,NBDIM,NBDIM,NBASIS,NBASIS,1)
      do 100 ia=1,NATOMS
      GAMMA(ia)=zero
      do 50 mu=1,NBASIS
      do 20 nu=1,NBASIS
      GAMMA(ia)=GAMMA(ia)+B(mu,nu)*A(mu,ia)*A(nu,ia)
20    continue
50    continue
100   continue
      call atompr(NATOMS,IAN,GAMMA,NATOMS,1,1)
      return
      
      end
C* :1 * 
      
