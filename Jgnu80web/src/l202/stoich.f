
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 stoich"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "stoich.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "stoich.web"
      subroutine stoich(MAXAP3,IATFLG,IAN,A,NATOMS,LENGTH,IA,IXYZ,LIN)
      implicit none
      double precision A,big,curz,gabs,gmin1,oldz,one,Tol2,Toler,zero,zm
     &in
      integer i,IA,IAN,iat,IATFLG,iatnum,icode,idx,iflag,imin,IXYZ,jdx,L
     &ENGTH,LIN,MAXAP3,NATOMS,num,numfla
      dimension IATFLG(*),IAN(*),A(MAXAP3,3),IA(*)
      dimension idx(104),jdx(92)
      common/tol/Toler,Tol2
      data jdx/6,1,89,47,13,18,33,85,79,5,56,4,83,35,20,48,58,17,27,24,5
     &5,29,66,68,63,9,26,87,31,64,32,2,72,80,67,53,49,77,19,36,57,3,71,1
     &2,25,42,7,11,41,60,10,28,8,76,15,91,82,46,61,59,84,78,88,37,75,45,
     &86,44,16,51,21,34,14,62,50,38,73,65,43,52,90,22,81,69,92,23,74,54,
     &39,70,30,40/
      
      data zero,one,big/0.0D0,1.0D0,100.D0/
      
      
      
      
      
      
      
      if(LIN.EQ.3)then
      
      
      do 50 i=1,92
      idx(i)=jdx(i)
50    continue
      do 100 i=93,104
      idx(i)=i
100   continue
      do 150 iat=1,NATOMS
      if(IAN(iat).EQ.6)return
150   continue
      do 200 i=1,12
      idx(i)=idx(i+2)
200   continue
      idx(13)=6
      do 250 i=14,30
      idx(i)=idx(i+1)
250   continue
      idx(31)=1
      return
      elseif(LIN.GT.0)then
      
      
      call cram(203,IA,LENGTH)
      
      
      numfla=0
      do 300 iat=1,NATOMS
      if(IATFLG(iat).EQ.2)numfla=numfla+1
300   continue
      
      
      oldz=-big
      zmin=big
350   iflag=0
      do 400 iat=1,NATOMS
      if(IATFLG(iat).EQ.2)then
      iflag=1
      curz=A(iat,IXYZ)
      zmin=gmin1(zmin,curz)
      if(gabs(curz-zmin).LT.Toler)imin=iat
      endif
400   continue
      if(iflag.NE.0)then
      if(LIN.EQ.2.AND.numfla.GT.1.AND.dsign(one,oldz).NE.dsign(one,zmin)
     &)call cram(205,IA,LENGTH)
      IATFLG(imin)=1
      icode=IAN(imin)+100
      call cram(icode,IA,LENGTH)
      call cram(1,IA,LENGTH)
      oldz=zmin
      zmin=big
      goto 350
      endif
      else
      call cram(203,IA,LENGTH)
      do 450 i=1,104
      iatnum=idx(i)
      num=0
      do 420 iat=1,NATOMS
      if(IATFLG(iat).EQ.2)then
      if(IAN(iat).EQ.iatnum)then
      num=num+1
      IATFLG(iat)=1
      endif
      endif
420   continue
      if(num.NE.0)then
      icode=iatnum+100
      call cram(icode,IA,LENGTH)
      call cram(num,IA,LENGTH)
      endif
450   continue
      call cram(204,IA,LENGTH)
      call cram(216,IA,LENGTH)
      return
      endif
      
      if(LIN.EQ.2.AND.numfla.GT.1.AND.dsign(one,oldz).LT.zero)call cram(
     &205,IA,LENGTH)
      call cram(204,IA,LENGTH)
      call cram(216,IA,LENGTH)
      return
      
      end
C* :1 * 
      
