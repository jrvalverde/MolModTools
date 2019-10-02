
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 diablo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "diablo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "diablo.web"
      subroutine diablo(NBASIS,E,DD,X)
      implicit none
      double precision A,B,Big,DD,E,ea,ei,Fillab,Four,One,Onept5,Pt5,Sma
     &ll,Three,Two,X,Zero
      integer i,i1,i2,ia,ii,In,ind,Iout,Ipunch,j,Ksm,Kspin,Ksw,Mdim,Mdsq
     &,minprt,mm,Mshifs,Mtt,NBASIS
      integer Nesk,Nest,Nest1,Nse,Nsep,nstb,Ntt,nv
      logical Cmp,Rhf
      dimension E(*),DD(*),X(*)
      common/con503/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      common/maxdm/Mdim,Mtt,Ntt,Mdsq,Mshifs
      common/memry/A(4970),B(4970),Fillab(40060)
      common/io/In,Iout,Ipunch
      
      
      
99001 format(' E(A) - E(I) IS NOT POSITIVE DEFINITE')
      call ilsw(2,21,minprt)
      
      call diagd(A,B,E,Nse,DD,X,Mdim,Cmp)
      
      nv=NBASIS-Nse
      i1=Nse
      do 100 i=1,Nse
      do 50 j=1,nv
      B(i1+j)=Zero
      if(Cmp)B(i1+Mdsq+j)=Zero
50    continue
      i1=i1+Mdim
100   continue
      
      mm=0
      ind=Nse*Nsep/2+Nse
      do 200 i=1,nv
      do 150 j=1,i
      mm=mm+1
      A(mm)=A(ind+j)
      if(Cmp)A(mm+Mtt)=A(ind+Mtt+j)
150   continue
      ind=ind+Nse+i
200   continue
      
      nstb=Nse*Mdim+1
      call diagd(A,B(nstb),E(Nsep),nv,DD,X,Mdim,Cmp)
      
      i2=Nse*Mdim+1
      i1=i2+nv
      i2=i2+NBASIS
      do 300 i=1,nv
      do 250 j=1,nv
      B(i2-j)=B(i1-j)
      if(Cmp)B(i2+Mdsq-j)=B(i1+Mdsq-j)
250   continue
      i1=i1+Mdim
      i2=i2+Mdim
300   continue
      
      i1=Nse*Mdim
      do 400 i=1,nv
      do 350 j=1,Nse
      B(i1+j)=Zero
      if(Cmp)B(i1+j+Mdsq)=Zero
350   continue
      i1=i1+Mdim
400   continue
      
      
      if(minprt.NE.0)return
      do 500 ii=1,Nse
      ei=E(ii)
      do 450 ia=Nsep,NBASIS
      ea=E(ia)
      if(ea.LE.ei)then
      write(Iout,99001)
      return
      endif
      
450   continue
500   continue
      
      return
      
      end
C* :1 * 
      
