
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 redob"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "redob.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "redob.web"
      subroutine redob(NBASIS,NAO,IPRINT)
      implicit none
      double precision C1,C2,C3,Exx,X,Y,Z
      integer i,ij,In,incr,incrm,Iout,IPRINT,Ipunch,j,Jan,jn,jst,lbmax,L
     &bound,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,N10ord
      integer N5ord,N6ord,N7ord,NAO,NBASIS,Nordr,Nshell
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon
      integer Ubound,Ulpure
      integer sconb
      dimension NAO(*)
      dimension incrm(4)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/io/In,Iout,Ipunch
      data incrm/0,0,1,3/
      
      
      
99001 format(' FROM REDOB, CONTENTS OF REVISED AOS:')
99002 format('             CONTENTS OF NAO:')
99003 format(10(2H (,i2,i4,1H)))
      
      incr=0
      do 100 i=1,Nshell
      lbmax=Shellt(i)+1
      sconb=Shellc(i)+1
      jn=Ubound(lbmax)-Lbound(lbmax,sconb)-incrm(lbmax)+1
      jst=Aos(i)+Lbound(lbmax,sconb)-2
      Aos(i)=Aos(i)+incr
      do 50 j=1,jn
      ij=jst+j
      NAO(ij)=ij+incr
50    continue
      incr=incr+incrm(lbmax)
100   continue
      i=Nshell+1
      Aos(i)=NBASIS+1+incr
      
      if(IPRINT.NE.0)then
      write(Iout,99001)
      write(Iout,99003)(i,Aos(i),i=1,Nshell)
      write(Iout,99002)
      write(Iout,99003)(i,NAO(i),i=1,NBASIS)
      endif
      
      return
      
      end
C* :1 * 
      
