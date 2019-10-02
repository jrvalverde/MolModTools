
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 mofi"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "mofi.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "mofi.web"
      subroutine mofi(NATOMS,IAN,C,AMASS,PMOM)
      implicit none
      double precision AMASS,C,ccom,com,e,e2,eigvec,PMOM,t,totwt,wt,x,y,
     &z,zero
      integer i,ia,iaind,IAN,iat,ixyz,NATOMS
      dimension C(*),IAN(*),AMASS(*),PMOM(*)
      dimension com(3),t(6),e(9),e2(18),eigvec(9)
      data zero/0.0D0/
      
      
      
      
      
      
      
      ccom(ixyz,iat)=C(ixyz+3*(iat-1))-com(ixyz)
      
      
      
      com(1)=zero
      com(2)=zero
      com(3)=zero
      
      totwt=zero
      do 100 iat=1,NATOMS
      iaind=3*(iat-1)
      ia=IAN(iat)
      wt=AMASS(ia)
      totwt=totwt+wt
      com(1)=com(1)+wt*C(1+iaind)
      com(2)=com(2)+wt*C(2+iaind)
      com(3)=com(3)+wt*C(3+iaind)
100   continue
      
      com(1)=com(1)/totwt
      com(2)=com(2)/totwt
      com(3)=com(3)/totwt
      
      
      do 200 i=1,6
      t(i)=zero
200   continue
      
      do 300 iat=1,NATOMS
      ia=IAN(iat)
      wt=AMASS(ia)
      x=ccom(1,iat)
      y=ccom(2,iat)
      z=ccom(3,iat)
      t(1)=t(1)+wt*(y*y+z*z)
      t(3)=t(3)+wt*(x*x+z*z)
      t(6)=t(6)+wt*(x*x+y*y)
      t(2)=t(2)-wt*x*y
      t(4)=t(4)-wt*x*z
      t(5)=t(5)-wt*y*z
300   continue
      call diagd(t,eigvec,PMOM,3,e,e2,3,.FALSE.)
      
      return
      
      end
C* :1 * 
      
