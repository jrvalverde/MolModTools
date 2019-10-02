
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 integi"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "integi.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "integi.web"
      subroutine integi(X)
      implicit none
      integer i,Ibf,IBUF,ierr,Ifil,IFLAG,In,Iout,Ipunch,IUNIT,KUNIT,l,le
     &n,Len03,Len18,Len19,Len21,Lenbuf,lenext,lenfil
      integer lenrec,loc,LUNIT,NREC,Nrpext,NUMBLK
      real X(*)
      common/io/In,Iout,Ipunch
      common/ibf/Ibf(30)
      common/ntrlen/Len03,Len18,Len19,Len21
      equivalence(Ifil,Ibf(24))
      equivalence(Nrpext,Ibf(12))
      equivalence(Lenbuf,Ibf(29))
      
      
      
      
      
      
      
      entry iwrite(IUNIT,IBUF,X)
      
      loc=(IBUF-1)*Lenbuf+1
      if(len.GE.lenfil)then
      if(IUNIT.EQ.3)lenfil=Len03
      if(IUNIT.EQ.21)lenfil=Len21
      endif
      call ntran(IUNIT,1,Lenbuf,X(loc),ierr)
      if(ierr.NE.-1)goto 200
      len=(Ifil+2)*lenrec
      return
      
      entry iread(IUNIT,IBUF,X)
      
      loc=(IBUF-1)*Lenbuf+1
      call ntran(IUNIT,2,Lenbuf,X(loc),ierr)
      if(ierr.NE.-1)goto 200
      return
      
      entry idef(IUNIT,IFLAG)
      
      call ntran(IUNIT,27,l,l,l)
      call ntran(IUNIT,29,IFLAG,l,l)
      
      entry iwind(IUNIT)
      
      call ntran(IUNIT,10,0,l,l)
      if(IUNIT.EQ.3)lenfil=Len03
      if(IUNIT.EQ.21)lenfil=Len21
      lenext=Nrpext*Lenbuf/128
      lenrec=Lenbuf/128+1
      len=lenrec
      return
      
      entry ipr(IUNIT)
      
      call ntran(IUNIT,29,1,l,l)
      return
      
      entry iwait(IUNIT)
      call ntran(IUNIT,23,0,l,ierr)
      return
      
      entry irel(IUNIT)
      
      return
      
      entry ipos(IUNIT,NUMBLK)
      
      call ntran(IUNIT,6,NUMBLK,l,l)
      return
      
      entry ifile(IUNIT)
      
      return
      
      entry icopy(KUNIT,LUNIT,NREC,X)
      
      do 100 i=1,NREC
      IUNIT=KUNIT
      call ntran(IUNIT,-2,Lenbuf,X,ierr)
      if(ierr.NE.Lenbuf)goto 200
      IUNIT=LUNIT
      call ntran(IUNIT,-1,Lenbuf,X,ierr)
      if(ierr.NE.Lenbuf)goto 200
100   continue
      return
      
200   write(Iout,99001)IUNIT,Lenbuf,ierr
      
99001 format(/1x,'INTEGI  --ERROR--  IUNIT:',i7,' LENBUF:',i7,' IERR:',i
     &7)
      
      call lnk1e
      stop
      
      end
C* :1 * 
      
