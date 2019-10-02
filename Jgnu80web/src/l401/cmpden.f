
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cmpden"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cmpden.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "cmpden.web"
      subroutine cmpden(A,B,AA,BB,MD,NB,X,INC,NE)
      implicit none
      double precision A,AA,B,BB,gsqrt,one,root2,root2i,temp,two,X,zero
      integer i,I56d,Ialt,Ibasis,Iblock,Icmp,Icmplt,Idgn,Idon1,Idon2,Idu
     &mp,Iguess,Imix,in,INC,Iobas,Iocmat,Iocore,Iodmat,Iodtot
      integer Iodum,Ioeig,Iogues,Iominc,Iomins,Iominv,Ioproj,Iorthg,Iosc
     &r1,Iosmat,Iosvec,Iosym,Ioteig,iout,Iovmat,ip1,Ipolh,Iprint,Iproj,I
     &scale
      integer Ismear,Itst,Iuhf,j,j1,j2,jmix,k,MD,NB,NE,nmix
      dimension A(MD,MD),B(MD,MD),AA(MD),BB(MD)
      dimension jmix(2,10)
      common/ops401/Iguess,Iproj,Iuhf,Icmp,Ialt,Imix,Idgn,Iscale,Ismear,
     &Iblock,Icmplt,Itst,Ibasis,Ipolh,Idon1,Idon2,Iprint,Idump,I56d
      common/rwf401/Iosmat,Iodmat,Iocmat,Iovmat,Iocore,Iobas,Iodum,Iomin
     &c,Iomins,Iominv,Iodtot,Ioeig,Iogues,Iosym,Ioproj,Iosvec,Ioscr1,Ior
     &thg,Ioteig
      data two/2.0D0/,one/1.0D0/,zero/0.0D0/
      data in,iout/5,6/
      
      
      
      
99001 format(2I3)
99002 format(2I3)
99003 format('  PAIRS OF ORBITALS MIXED BY COMPLEX.')
      
      root2=gsqrt(two)
      root2i=one/root2
      
      if(Imix.EQ.0)then
      
      j1=NE
      j2=NE+1
      do 50 i=1,NB
      A(i,j1)=A(i,j1)*root2i
      A(i,j2)=A(i,j2)*root2i
50    continue
      else
      
      nmix=0
      write(iout,99003)
100   read(in,99001)j1,j2
      if(j1.NE.0)then
      write(iout,99002)j1,j2
      nmix=nmix+1
      jmix(1,nmix)=j1
      jmix(2,nmix)=j2
      
      do 120 i=1,NB
      A(i,j1)=A(i,j1)*root2i
      A(i,j2)=A(i,j2)*root2i
120   continue
      goto 100
      endif
      endif
      
      call twrite(Iocmat+INC,A,MD,MD,NB,NB,0)
      if(Iprint.NE.0)call gesprt(9,A,INC,MD,NB,NB)
      
      do 200 i=1,NB
      do 150 j=1,NB
      B(i,j)=zero
      do 140 k=1,NE
      B(i,j)=B(i,j)+A(j,k)*A(i,k)*X
140   continue
150   continue
200   continue
      
      call twrite(Iodmat+INC,B,MD,MD,NB,NB,1)
      
      call twreig(AA,B,NB,INC,Ioeig)
      if((Iprint-Icmp).GE.2)call gesprt(8,B,INC,MD,NB,NB)
      
      do 300 i=1,NB
      do 250 j=1,NB
      B(i,j)=zero
250   continue
300   continue
      
      if(Imix.EQ.0)then
      
      j1=NE
      j2=NE+1
      do 350 i=1,NB
      B(i,j1)=A(i,j2)
      B(i,j2)=A(i,j1)
350   continue
      else
      do 400 k=1,nmix
      j1=jmix(1,k)
      j2=jmix(2,k)
      do 360 i=1,NB
      B(i,j1)=A(i,j2)
      B(i,j2)=A(i,j1)
360   continue
400   continue
      endif
      
      call twrite(Iocmat+INC+1,B,MD,MD,NB,NB,0)
      if(Iprint.NE.0)call gesprt(9,B,INC+1,MD,NB,NB)
      
      call tread(Iodmat+INC,A,MD,MD,NB,NB,1)
      
      do 500 i=1,NB
      do 450 j=1,NB
      do 420 k=1,NE
      A(i,j)=A(i,j)+B(i,k)*B(j,k)*X
420   continue
450   continue
500   continue
      
      call twrite(Iodmat+INC,A,MD,MD,NB,NB,1)
      if(Iprint.GE.2)call gesprt(8,A,INC,MD,NB,NB)
      call tread(Iocmat+INC,A,MD,MD,NB,NB,0)
      
      do 700 i=1,NB
      ip1=i+1
      do 550 j=ip1,NB
      temp=zero
      do 520 k=1,NE
      
      temp=temp+X*(B(i,k)*A(j,k)-A(i,k)*B(j,k))
520   continue
      AA(j)=temp
550   continue
      do 600 j=ip1,NB
      A(i,j)=AA(j)
600   continue
700   continue
      
      do 800 i=1,NB
      ip1=i+1
      A(i,i)=zero
      do 750 j=ip1,NB
      A(j,i)=-A(i,j)
750   continue
800   continue
      
      call twrite(Iodmat+INC+1,A,MD,MD,NB,NB,1)
      
      return
      
      end
C* :1 * 
      
