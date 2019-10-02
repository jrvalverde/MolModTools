
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dfout3"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dfout3.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 35 "dfout3.web"
      subroutine dfout3(TQ1,TQ2,TQ3,DBUF,IBUF2E,DBUF2E,ISH,JSH,KSH,LSH,I
     &OP,D,F)
      implicit none
      double precision C1,C2,C3,DBUF2E,Dumint,Exx,g1,G11,G12,G13,g2,G21,
     &G22,G23,g3,G31,G32,G33,Gint,pt25
      double precision TQ1,TQ2,TQ3,X,Y,Z,Zero
      integer i,IBUF2E,idcout,Idmp,Idump,Iend,Imj,Imk,Imkjml,Irange,iret
     &,Is1,Is2,Is3,ISH,isite1,isite2,isite3,ist,Istart
      integer Istm,j,Jan,Jend,Jml,jp1,Jrange,Js1,Js2,Js3,JSH,jst,Jstart,
     &Jstm,k,Kend,Kml,Krange,Ks1,Ks2
      integer Ks3,KSH,kst,Kstart,Kstm,l,lambda,Lbound,Lend,Lentq,Lrange,
     &LSH,lst,Lstart,Lstm,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp
      integer mu,N10ord,N5ord,N6ord,N7ord,Nfa,Nfb,Nfc,Nfd,Nordr,Nshell,n
     &u,Numdf
      integer IOP(*)
      double precision D(*),F(*)
      integer Shella,Shelln,Shellt,Shellc,shladf,Aos,Aon
      integer Ubound,Ulpure,sigma
      logical DBUF
      dimension TQ1(*),TQ2(*),TQ3(*),IBUF2E(*),DBUF2E(*)
      common/dump/Idmp,Idump
      common/int/Zero,Dumint(12)
      common/site/Is1(10),Js1(10),Ks1(10),Is2(10),Js2(10),Ks2(10),Is3(10
     &),Js3(10),Ks3(10)
      common/limit/Imj,Imk,Jml,Kml,Imkjml,Istart,Jstart,Kstart,Lstart,Ie
     &nd,Jend,Kend,Lend,Irange,Jrange,Krange,Lrange,Lentq,Numdf
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      common/gint/Gint(3,3)
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/nf/Nfa,Nfb,Nfc,Nfd,Istm,Jstm,Kstm,Lstm
      equivalence(G11,Gint(1,1)),(G12,Gint(1,2)),(G13,Gint(1,3))
      equivalence(G21,Gint(2,1)),(G22,Gint(2,2)),(G23,Gint(2,3))
      equivalence(G31,Gint(3,1)),(G32,Gint(3,2)),(G33,Gint(3,3))
      data pt25/0.25D0/
      
      
      
      
      
      
      
      
      
      if(Idump.EQ.11)then
      write(6,99006)ISH,JSH,KSH,LSH
      write(6,99007)Nfa,Nfb,Nfc,Nfd
      write(6,99001)
      write(6,99002)(i,TQ1(i),i=1,Lentq)
      write(6,99002)(i,TQ2(i),i=1,Lentq)
      write(6,99002)(i,TQ3(i),i=1,Lentq)
      write(6,99008)(i,Is1(i),Js1(i),Ks1(i),i=1,10)
      write(6,99009)(i,Is2(i),Js2(i),Ks2(i),i=1,10)
      write(6,99010)(i,Is3(i),Js3(i),Ks3(i),i=1,10)
      endif
      
      
      ist=Aos(ISH)-1
      jst=Aos(JSH)-1
      kst=Aos(KSH)-1
      lst=Aos(LSH)-1
      
      do 100 j=1,Nfb
      nu=jst+Nordr(j+Jstm)
      
      
      do 50 k=1,j
      lambda=kst+Nordr(k+Kstm)
      do 20 i=1,Nfa
      mu=ist+Nordr(i+Istm)
      do 10 l=1,Nfd
      sigma=lst+Nordr(l+Lstm)
      
      
      isite1=Is1(i)+Js1(j)+Ks1(k)+l
      isite2=Is2(i)+Js2(l)+Ks2(j)+k
      isite3=Is3(i)+Js3(k)+Ks3(j)+l
      
      g1=TQ1(isite1)
      g2=TQ2(isite2)
      g3=TQ3(isite3)
      if(Idump.EQ.11)write(6,99003)isite1,isite2,isite3,g1,g2,g3
      
      G31=g3-g2
      G21=g2+g3
      G11=g1-pt25*G21
      
      G32=g1-g3
      G22=g1+g3
      G12=g2-pt25*G22
      
      if(Idump.EQ.11)then
      write(6,99004)i,j,k,l,mu,nu,lambda,sigma,G11,G12,G13
      write(6,99005)G21,G22,G23
      write(6,99005)G31,G32,G33
      endif
      
      call out2e(2,mu,nu,lambda,sigma,Gint,DBUF,IBUF2E,DBUF2E,iret,idcou
     &t,IOP,D,F)
10    continue
20    continue
50    continue
      
      
      jp1=j+1
      if(jp1.LE.Nfc)then
      G12=Zero
      G22=Zero
      G32=Zero
      do 80 k=jp1,Nfc
      lambda=kst+Nordr(k+Kstm)
      do 60 i=1,Nfa
      mu=ist+Nordr(i+Istm)
      do 55 l=1,Nfd
      sigma=lst+Nordr(l+Lstm)
      
      isite1=Is1(i)+Js1(j)+Ks1(k)+l
      isite2=Is2(i)+Js2(l)+Ks2(j)+k
      isite3=Is3(i)+Js3(k)+Ks3(j)+l
      
      g1=TQ1(isite1)
      g2=TQ2(isite2)
      g3=TQ3(isite3)
      if(Idump.EQ.11)write(6,99003)isite1,isite2,isite3,g1,g2,g3
      
      G31=g3-g2
      G21=g2+g3
      G11=g1-pt25*G21
      
      if(Idump.EQ.11)then
      write(6,99004)i,j,k,l,mu,nu,lambda,sigma,G11,G12,G13
      write(6,99005)G21,G22,G23
      write(6,99005)G31,G32,G33
      endif
      
      call out2e(2,mu,nu,lambda,sigma,Gint,DBUF,IBUF2E,DBUF2E,iret,idcou
     &t,IOP,D,F)
55    continue
60    continue
80    continue
      endif
100   continue
      
      return
      
99001 format(' CONTENTS OF TQ1,2,3')
99002 format(7(i5,d13.6))
99003 format(' ISITE,G',3I4,2x,3D20.10)
99004 format(4I2,2x,4I3,3D20.10)
99005 format(10x,12x,3D20.10)
99006 format(' IN DFOUT3, ISH, ETC=',4I3)
99007 format(' NFA,B,C,D=',4I3)
99008 format(' I,J,KS1='/(1x,i2,3I3))
99009 format(' I,J,KS2='/(1x,i2,3I3))
99010 format(' I,J,KS3='/(1x,i2,3I3))
      
      end
C* :1 * 
      
