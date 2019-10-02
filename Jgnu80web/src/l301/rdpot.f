
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rdpot"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rdpot.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "rdpot.web"
      subroutine rdpot(NVAL,COEF,EXPON,KF,KL,MAX,LSKP,TYPE,CORE)
      implicit none
      double precision COEF,EXPON
      integer ibegin,iend,In,ind,Iout,Ipunch,k,KF,KL,l,lm1,LSKP,MAX,mx1,
     &nbf,NVAL
      integer CORE,title
      double precision TYPE,void
      dimension NVAL(40),COEF(40),EXPON(40),KF(5),KL(5)
      dimension title(15)
      common/io/In,Iout,Ipunch
      data void/6H      /
      
99001 format(15A4)
99002 format(i3)
99003 format(i3,2x,2D15.7)
      
      KF(1)=1
      KL(1)=1
      if(TYPE.EQ.void)then
      
      LSKP=1
      else
      LSKP=0
      mx1=MAX+1
      do 50 l=1,mx1
      read(In,99001)(title(k),k=1,15)
      read(In,99002)nbf
      if(l.NE.1)then
      lm1=l-1
      KF(l)=KL(lm1)+1
      endif
      KL(l)=KF(l)+nbf-1
      ibegin=KF(l)
      iend=KL(l)
      do 20 ind=ibegin,iend
      read(In,99003)NVAL(ind),EXPON(ind),COEF(ind)
20    continue
50    continue
      endif
      return
      
      end
C* :1 * 
      
