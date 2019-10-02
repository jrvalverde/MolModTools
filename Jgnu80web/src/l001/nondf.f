
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nondf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nondf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "nondf.web"
      subroutine nondf
      implicit none
      integer crd,dash,i,icard,imax,In,ioccur,Iout,Ipunch,Jop,k,len,Ll,L
     &nk,ncard,Nchain,ndsh,Nlink,Option,Pad
      integer qparse,res5,result
      dimension icard(80),ncard(100),result(5)
      common/io/In,Iout,Ipunch
      common/option/Option(92)
      common/tmprte/Nchain,Ll,Nlink,Pad,Lnk(200),Jop(50,50)
      data dash/'-'/
      
      
99001 format(2x,19A4)
99002 format(2x,128A1)
      
      read(In,99003)(icard(i),i=1,80)
      
99003 format(80A1)
      
      len=100
      call rdrout(icard,ncard,len)
      if(len.GT.400)then
      write(Iout,99004)
      call lnk1e
      
99004 format(' TO MANY NON DEFAULT OPTIONS')
      endif
      call qpinit(Option,0,1,0)
      ndsh=min0(len,76)
      write(Iout,99002)(dash,i=1,ndsh)
      imax=len/4+1
      write(Iout,99001)(ncard(i),i=1,imax)
      write(Iout,99002)(dash,i=1,ndsh)
      result(1)=0
      result(3)=0
      result(2)=1
      result(4)=0
      result(5)=0
100   i=qparse(result,Option,ncard,len)
      if(i.NE.0)then
      if(i.NE.1)then
      write(Iout,99005)
      
99005 format(' ERROR PARSING NON DEFAULT OPTIONS')
      
      call qperro(ncard,len,Iout)
      call lnk1e
      else
      res5=result(5)
      if(res5.LE.1)then
      ioccur=0
      i=0
      k=0
110   if(mod(Lnk(i+1),10000)/100.NE.result(1))then
      
      if(i.NE.k)then
      ioccur=ioccur+1
      if(res5.NE.0.OR.ioccur.EQ.result(2))then
      crd=mod(Lnk(i),1000000)/10000
      Jop(result(3),crd)=result(4)
      if(res5.EQ.0)goto 100
      endif
      endif
      i=i+1
      k=i
      if(k.LE.Nlink)goto 110
      if(res5.NE.0)goto 100
      else
      i=i+1
      goto 110
      endif
      else
      result(5)=0
      result(2)=1
      goto 100
      endif
      endif
      write(Iout,99006)result(1),result(2)
      
99006 format(' LINK NUMBER ',i10,'(',i2,') UNDEFINED')
      
      call lnk1e
      endif
      return
      
      end
C* :1 * 
      
