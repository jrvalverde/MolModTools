
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fill"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fill.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "fill.web"
      subroutine fill(MAXAP3,NATOMS,NOP,MAXOP,T,TRANS,NPERM,A,B)
      implicit none
      double precision A,B,gabs,T,test,Tol2,Toler,TRANS
      integer i,iat,iflag,In,Iout,Ipunch,ixyz,j,kat,lperm,MAXAP3,MAXOP,N
     &ATOMS,NOP,NPERM
      dimension T(3,3),TRANS(3,3,MAXOP),NPERM(MAXAP3,MAXOP)
      dimension A(MAXAP3,3),B(MAXAP3,3)
      dimension lperm(100)
      common/io/In,Iout,Ipunch
      common/tol/Toler,Tol2
      data iflag/1/
      
      
      
      
      
      
      
99001 format(1x,'FILL--  OPERATION LISTS TERMINATED AT ',i3)
      
      
      do 100 iat=1,NATOMS
      do 50 kat=1,NATOMS
      do 20 ixyz=1,3
      test=A(kat,ixyz)-B(iat,ixyz)
      if(gabs(test).GT.Toler)goto 50
20    continue
      lperm(iat)=kat
      goto 100
      
50    continue
100   continue
      
      call filrep(T,lperm,NATOMS)
      
      
      NOP=NOP+1
      if(NOP.GT.MAXOP)then
      NOP=NOP-1
      if(iflag.LT.0)return
      write(Iout,99001)MAXOP
      iflag=-1
      return
      endif
      
      
      do 200 i=1,3
      do 150 j=1,3
      TRANS(i,j,NOP)=T(i,j)
150   continue
200   continue
      
      
      do 300 i=1,NATOMS
      NPERM(i,NOP)=lperm(i)
300   continue
      return
      
      end
C* :1 * 
      
