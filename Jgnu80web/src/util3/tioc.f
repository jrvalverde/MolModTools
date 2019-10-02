
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tioc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tioc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 49 "tioc.web"
      subroutine tioc(NBASIS,IOOP,IFIL,A,KEY,IRC,IDUMP)
      implicit none
      double precision A
      integer i,i1,icon,icr,icr1,IDUMP,ierr,IFIL,ifile,ifn,ifnr,ifr,imax
     &,In,IOOP,Iout,ip,ip1,Ipunch,IRC
      integer KEY,Ksm,Kspin,Ksw,len,m1,Mdim,Mdsq,mr,Mshifs,Mtt,NBASIS,Ne
     &sk,Nest,Nest1,Nse,Nsep,Ntt
      integer fretln
      logical Cmp,Rhf
      dimension A(*),IFIL(*)
      dimension ifr(2),mr(2)
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      common/maxdm/Mdim,Mtt,Ntt,Mdsq,Mshifs
      common/io/In,Iout,Ipunch
      data fretln/11/
      
99001 format(/1x,10(1H>),' CALL TIOC(',i1,',',i3,', ,',i1,',',i1,')',10x
     &,5x,2(i4,i5,5x))
99002 format(///' FILE  #',i3,' IS NOT INITIALIZED')
      ip=1
      ierr=0
      
      
      ifile=IFIL(1)
      if(KEY.GT.1)then
      
      
      call ilsw(2,1,icon)
      ip=icon/2+1
      ifn=2*(Kspin-1)
      
      do 50 icr=1,ip
      ip1=icr
      icr1=icr-1
      ifnr=IFIL(ifn+icr)
      ifr(icr)=ifnr
      if(IOOP.EQ.2)then
      
      call fileio(fretln,ifnr,len,i,i)
      if(len.EQ.0)goto 200
      if(KEY.GT.3)then
      
      m1=icr1*Mdsq+1
      call tread(ifnr,A(m1),Mdim,Mdim,NBASIS,NBASIS,0)
      else
      m1=icr1*Mtt+1
      call tread(ifnr,A(m1),Mtt,1,Ntt,1,0)
      if(KEY.EQ.3.AND.icr.EQ.ip)call sls(2,A,Mdim,NBASIS)
      endif
      
      elseif(KEY.GT.3)then
      
      m1=icr1*Mdsq+1
      call twrite(ifnr,A(m1),Mdim,Mdim,NBASIS,NBASIS,0)
      else
      m1=icr1*Mtt+1
      if(icr.EQ.1.AND.KEY.EQ.3)call sls(1,A,Mdim,NBASIS)
      call twrite(ifnr,A(m1),Mtt,1,Ntt,1,0)
      if(KEY.EQ.3.AND.icr.EQ.ip)call sls(2,A,Mdim,NBASIS)
      endif
      mr(icr)=m1
50    continue
      elseif(IRC.NE.0)then
      
      
      ierr=1
      goto 100
      else
      ip1=1
      m1=1
      ifr(1)=ifile
      ifnr=ifile
      imax=1
      mr(1)=1
      if(IOOP.EQ.2)then
      
      call fileio(fretln,ifile,len,i,i)
      if(len.EQ.0)goto 200
      call tread(ifile,A,Mdim,Mdim,NBASIS,NBASIS,KEY)
      else
      
      call twrite(ifile,A,Mdim,Mdim,NBASIS,NBASIS,KEY)
      endif
      endif
      
      if(IDUMP.EQ.0)return
100   write(Iout,99001)IOOP,ifile,KEY,IRC,(ifr(i),mr(i),i=1,ip1)
      if(IDUMP.GT.2)then
      i1=KEY+1
      if(i1.EQ.3)then
      
      call dsymm(A,NBASIS)
      elseif(i1.EQ.4)then
      
      if(IOOP.EQ.1)then
      call dsymm(A,NBASIS)
      elseif(IOOP.EQ.2)then
      
      call matout(A,Mdim,Mdim,NBASIS,NBASIS)
      endif
      else
      call matout(A,Mdim,Mdim,NBASIS,NBASIS)
      endif
      endif
      
      if(ierr.NE.0)call lnk1e
      return
      
200   write(Iout,99002)ifnr
      ierr=1
      goto 100
      
      end
C* :1 * 
      
