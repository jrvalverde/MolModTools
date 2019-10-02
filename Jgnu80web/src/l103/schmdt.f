
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 schmdt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "schmdt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "schmdt.web"
      subroutine schmdt(A,IC,NC,NP,RMAX,RMIN,RLIM)
      implicit none
      double precision A,gsqrt,one,r,RLIM,RMAX,RMIN,rr,zero
      integer i,i1,IC,ii,j,k,NC,NP
      dimension A(50,50)
      dimension IC(50)
      data zero,one/0.D0,1.D0/
      
      
99001 format(11(10x,10F12.6/))
99002 format(40I3)
      
      
      i=1
      ii=0
100   rr=zero
      ii=ii+1
      IC(i)=ii
      do 200 j=1,NC
      rr=rr+A(j,i)**2
200   continue
      rr=gsqrt(rr)
      if((NP.LE.1).OR.(rr.GE.RMIN))then
      if((i.LE.1).OR.(rr.LE.RMAX))then
      do 220 j=1,NC
      A(j,i)=A(j,i)/rr
220   continue
      rr=one
      i1=i-1
      if(i1.GE.1)then
      do 240 k=1,i1
      r=zero
      do 225 j=1,NC
      r=r+A(j,i)*A(j,k)
225   continue
      do 230 j=1,NC
      A(j,i)=A(j,i)-A(j,k)*r
230   continue
240   continue
      rr=zero
      do 250 j=1,NC
      rr=rr+A(j,i)**2
250   continue
      rr=gsqrt(rr)
      if(rr.LT.RLIM)goto 300
      do 260 j=1,NC
      A(j,i)=A(j,i)/rr
260   continue
      endif
      if(i.GE.NP)goto 400
      i=i+1
      goto 100
      endif
      endif
      
      
300   NP=NP-1
      if(i.LE.NP)then
      do 350 j=1,NC
      do 320 k=i,NP
      A(j,k)=A(j,k+1)
320   continue
350   continue
      goto 100
      endif
      
400   return
      
      end
C* :1 * 
      
