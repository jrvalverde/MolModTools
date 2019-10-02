
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cntprm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cntprm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "cntprm.web"
      subroutine cntprm(NDC,INTC,TQ,TQPRIM)
      implicit none
      double precision Ca,Cb,Cc,cc1,cc2,cc3,Cd,TQ,TQPRIM
      integer i,Idmp,Idump,Iend,Imj,Imk,Imkjml,INTC,Irange,Istart,j,Jend
     &,Jml,Jrange,Jstart,k,Kend,Kml,Krange,Kstart
      integer l,Lamax,Lbmax,Lbound,Lcmax,Ldmax,Lend,Lentq,Lpmax,Lpqmax,L
     &qmax,Lrange,Lstart,N10ord,N5ord,N6ord,N7ord,NDC,Nordr,Numdf
      integer Ubound,Ulpure
      dimension TQ(*),TQPRIM(*)
      common/dump/Idmp,Idump
      common/contr/Ca(20),Cb(20),Cc(20),Cd(20)
      common/limit/Imj,Imk,Jml,Kml,Imkjml,Istart,Jstart,Kstart,Lstart,Ie
     &nd,Jend,Kend,Lend,Irange,Jrange,Krange,Lrange,Lentq,Numdf
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      
      
      
      
      
      INTC=0
      if(NDC.NE.1)then
      do 50 i=Istart,Iend
      cc1=Ca(i)
      if(Imj.EQ.0)Jend=i
      if(Imkjml.EQ.0)Kend=i
      do 20 j=Jstart,Jend
      cc2=cc1*Cb(j)
      do 10 k=Kstart,Kend
      cc3=cc2*Cc(k)
      Lend=Ubound(Ldmax)
      if(Kml.EQ.0)Lend=k
      if(Imkjml+iabs(i-k).EQ.0)Lend=j
      do 5 l=Lstart,Lend
      INTC=INTC+1
      TQ(INTC)=TQ(INTC)+TQPRIM(INTC)*cc3*Cd(l)
5     continue
10    continue
20    continue
50    continue
      else
      
      do 100 i=Istart,Iend
      cc1=Ca(i)
      if(Imj.EQ.0)Jend=i
      if(Imkjml.EQ.0)Kend=i
      do 80 j=Jstart,Jend
      cc2=cc1*Cb(j)
      do 60 k=Kstart,Kend
      cc3=cc2*Cc(k)
      Lend=Ubound(Ldmax)
      if(Kml.EQ.0)Lend=k
      if(Imkjml+iabs(i-k).EQ.0)Lend=j
      do 55 l=Lstart,Lend
      INTC=INTC+1
      TQ(INTC)=TQ(INTC)*cc3*Cd(l)
55    continue
60    continue
80    continue
100   continue
      endif
      
      return
      
      end
C* :1 * 
      
