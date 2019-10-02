
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 purdf1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "purdf1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "purdf1.web"
      subroutine purdf1(X)
      implicit none
      double precision dx2y2,dz2,f0,f1m,f1p,f2m,f2p,f3m,f3p,Pt5,R1,R2,R3
     &,R3ov2,R4,Root15,Root3,Root5,X,Z1
      double precision Z2,Z3
      integer i,Iend,Imj,incr1,incr2,incr3,incr4,incr5,incr6,incr7,incr8
     &,incr9,indx1,indx2,Inew,Ipurd,Ipurf,Irange,Istart,Itype
      integer j,Jend,jendp,Jnew,Jnktyp,Jrange,jrpure,Jstart,Jtype,Lamax,
     &Lbmax,Lbound,Lentq,Limdum,Lpmax,Maxdum,N10ord,N5ord,N6ord,N7ord
      integer Nordr
      integer Ubound,Ulpure
      dimension X(*)
      common/type/Itype,Jtype,Jnktyp(10)
      common/ipure/Ipurd,Ipurf
      common/limit/Imj,Istart,Jstart,Iend,Jend,Irange,Jrange,Lentq,Limdu
     &m(11)
      common/cfact/Pt5,R3ov2,Root3,Root5,Root15,R1,R2,R3,R4,Z1,Z2,Z3
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/new/Inew,Jnew
      
      
      
      
      
      
      
      
      if(Jtype.LT.2)goto 300
      if(Jtype.EQ.2)then
      
      if(Ipurd.NE.0)goto 300
      
      indx1=5-Jstart+1
      do 50 i=1,Irange
      dz2=X(indx1+2)-Pt5*(X(indx1)+X(indx1+1))
      dx2y2=R3ov2*(X(indx1)-X(indx1+1))
      X(indx1)=dz2
      X(indx1+1)=X(indx1+4)
      X(indx1+2)=X(indx1+5)
      X(indx1+4)=X(indx1+3)
      X(indx1+3)=dx2y2
      indx1=indx1+Jrange
50    continue
      else
      
      
      if(Ipurf.NE.0)goto 300
      
      indx1=0
      do 100 i=1,Irange
      
      f0=X(indx1+3)-R2*(X(indx1+6)+X(indx1+9))
      f1p=R4*(Z1*X(indx1+7)-X(indx1+1)-Z2*X(indx1+4))
      f1m=R4*(Z1*X(indx1+8)-X(indx1+2)-Z2*X(indx1+5))
      f2p=R3*(X(indx1+6)-X(indx1+9))
      f2m=X(indx1+10)
      f3p=R1*(X(indx1+1)-Z3*X(indx1+4))
      f3m=R1*(Z3*X(indx1+5)-X(indx1+2))
      
      X(indx1+1)=f0
      X(indx1+2)=f1p
      X(indx1+3)=f1m
      X(indx1+4)=f2p
      X(indx1+5)=f2m
      X(indx1+6)=f3p
      X(indx1+7)=f3m
      
      indx1=indx1+Jrange
100   continue
      endif
      
      
      
      
      jendp=Ulpure(Lbmax)
      jrpure=jendp-Jstart+1
      
      indx1=0
      indx2=0
      
      do 200 i=1,Irange
      do 150 j=1,jrpure
      X(indx2+j)=X(indx1+j)
150   continue
      indx1=indx1+Jrange
      indx2=indx2+jrpure
200   continue
      
      Jend=jendp
      Jrange=jrpure
      
      
300   if(Itype.LT.2)goto 500
      if(Itype.EQ.2)then
      
      if(Ipurd.NE.0)goto 500
      
      indx1=(5-Istart)*Jrange+1
      incr1=Jrange
      incr2=incr1+Jrange
      incr3=incr2+Jrange
      incr4=incr3+Jrange
      incr5=incr4+Jrange
      
      do 350 j=1,Jrange
      dz2=X(indx1+incr2)-Pt5*(X(indx1)+X(indx1+incr1))
      dx2y2=R3ov2*(X(indx1)-X(indx1+incr1))
      X(indx1)=dz2
      X(indx1+incr1)=X(indx1+incr4)
      X(indx1+incr2)=X(indx1+incr5)
      X(indx1+incr4)=X(indx1+incr3)
      X(indx1+incr3)=dx2y2
      indx1=indx1+1
350   continue
      else
      
      
      if(Ipurf.NE.0)goto 500
      
      indx1=1
      incr1=Jrange
      incr2=incr1+Jrange
      incr3=incr2+Jrange
      incr4=incr3+Jrange
      incr5=incr4+Jrange
      incr6=incr5+Jrange
      incr7=incr6+Jrange
      incr8=incr7+Jrange
      incr9=incr8+Jrange
      
      do 400 j=1,Jrange
      
      f0=X(indx1+incr2)-R2*(X(indx1+incr5)+X(indx1+incr8))
      f1p=R4*(Z1*X(indx1+incr6)-X(indx1)-Z2*X(indx1+incr3))
      f1m=R4*(Z1*X(indx1+incr7)-X(indx1+incr1)-Z2*X(indx1+incr4))
      f2p=R3*(X(indx1+incr5)-X(indx1+incr8))
      f2m=X(indx1+incr9)
      f3p=R1*(X(indx1)-Z3*X(indx1+incr3))
      f3m=R1*(Z3*X(indx1+incr4)-X(indx1+incr1))
      
      X(indx1)=f0
      X(indx1+incr1)=f1p
      X(indx1+incr2)=f1m
      X(indx1+incr3)=f2p
      X(indx1+incr4)=f2m
      X(indx1+incr5)=f3p
      X(indx1+incr6)=f3m
      
      indx1=indx1+1
400   continue
      endif
      
      Iend=Ulpure(Lamax)
      Irange=Iend-Istart+1
      
500   return
      
      end
C* :1 * 
      
