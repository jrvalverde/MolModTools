
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fmtgen"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fmtgen.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "fmtgen.web"
      subroutine fmtgen(F,T,M,ICK)
      implicit none
      double precision a,approx,b,Cut0s,Cutml,Cutsm,F,F20,F42,F500,fimul
     &t,fiprop,Fmzero,Four,Ga,gabs,gexp,gfloat,gsqrt,Half
      integer i,ICK,In,Iout,Ipunch,ix,M,mm1,notrms
      double precision oldsum,One,Rpitwo,sum,T,Ten,Tenm9,term,texp,Tol,T
     &wo,tx,Zero
      dimension F(M)
      common/io/In,Iout,Ipunch
      common/fmcons/Four,One,Half,Two,Zero,Ten,Tenm9,F20,F42,F500
      common/fm/Ga(15),Rpitwo,Fmzero(15),Tol,Cut0s,Cutsm,Cutml
      equivalence(approx,oldsum)
      
      
99001 format(41H0FAILURE IN FMGEN FOR SMALL T:  IX > 50, /6H IX = ,i3,7H
     &,  T = ,e20.14)
99002 format(37H0FAILURE IN FMGEN FOR INTERMEDIATE T:/6H  T = ,e20.14)
      
      ICK=0
      if(gabs(T).LE.Cut0s)then
      do 50 i=1,M
      F(i)=Fmzero(i)
50    continue
      return
      else
      texp=Zero
      if(gabs(T).LT.Cutml)then
      texp=gexp(-T)
      if(gabs(T).LT.Cutsm)then
      a=gfloat(M-1)+Half
      term=One/a
      sum=term
      do 60 ix=2,200
      a=a+One
      term=term*T/a
      sum=sum+term
      if(gabs(term/sum).LT.Tol)goto 80
60    continue
      write(Iout,99001)ix,T
      stop
      else
      
      a=gfloat(M-1)
      b=a+Half
      a=a-Half
      tx=One/T
      mm1=M-1
      approx=Rpitwo*gsqrt(tx)*(tx**mm1)
      if(mm1.NE.0)then
      do 65 ix=1,mm1
      b=b-One
      approx=approx*b
65    continue
      endif
      fimult=Half*texp*tx
      sum=Zero
      if(fimult.EQ.0)goto 100
      fiprop=fimult/approx
      term=One
      sum=One
      notrms=idint(T)+mm1
      do 70 ix=2,notrms
      term=term*a*tx
      sum=sum+term
      if(gabs(term*fiprop/sum).LE.Tol)goto 100
      a=a-One
70    continue
      write(Iout,99002)T
      ICK=1
      return
      endif
      
80    F(M)=Half*sum*texp
      else
      tx=gfloat(M)-Half
      F(M)=Half*Ga(M)/(T**tx)
      endif
      goto 200
      
100   F(M)=approx-fimult*sum
      endif
200   tx=T+T
      sum=gfloat(M+M-3)
      mm1=M-1
      if(mm1.NE.0)then
      do 250 ix=1,mm1
      F(M-ix)=(tx*F(M-ix+1)+texp)/sum
      sum=sum-Two
250   continue
      endif
      return
      
      end
C* :1 * 
      
