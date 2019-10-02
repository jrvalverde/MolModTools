
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fmtset"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fmtset.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "fmtset.web"
      subroutine fmtset(KOP1,KOP2,KOP3)
      implicit none
      double precision cut,Cut0s,Cutml,Cutsm,f,F20,F42,F500,Fmzero,Four,
     &Ga,gatan,gfloat,gsqrt,Half,One,pi,Rpitwo,t,Ten
      double precision Tenm9,Tol,Two,Zero
      integer i,ick,In,Iout,Ipunch,KOP1,KOP2,KOP3
      dimension f(15)
      common/fm/Ga(15),Rpitwo,Fmzero(15),Tol,Cut0s,Cutsm,Cutml
      common/io/In,Iout,Ipunch
      common/fmcons/Four,One,Half,Two,Zero,Ten,Tenm9,F20,F42,F500
      
      
      
99001 format(21H FROM FMTSET, KOPS = ,3I2,12H AND CUTS = ,3E10.3)
      
      pi=Four*gatan(One)
      Ga(1)=gsqrt(pi)
      Rpitwo=Half*Ga(1)
      
      Tol=Half
      do 100 i=2,15
      Ga(i)=Ga(i-1)*Tol
      Tol=Tol+One
100   continue
      
      Tol=One
      Fmzero(1)=One
      do 200 i=2,15
      Tol=Tol+Two
      Fmzero(i)=One/Tol
200   continue
      
      
      
      Cut0s=Zero
      if(KOP1.NE.0)Cut0s=Ten**(-2*KOP1)
      
      Tol=Tenm9
      Cutsm=Ten
      if(KOP2.NE.0)then
      Tol=Ten**(-6-KOP2)
      t=F20
250   call fmtgen(f,t,1,ick)
      if(ick.NE.0)then
      cut=t+One
      else
      t=t-One
      if(t.GE.Ten)goto 250
      endif
      endif
      
      Cutml=F42
      if(KOP3.NE.0)then
      if(KOP3.NE.7)then
      
      Cutml=gfloat(KOP3)*Four
      else
      Cutml=F500
      endif
      endif
      if((KOP1+KOP2+KOP3).NE.0)write(Iout,99001)KOP1,KOP2,KOP3,Cut0s,Cut
     &sm,Cutml
      return
      
      end
C* :1 * 
      
