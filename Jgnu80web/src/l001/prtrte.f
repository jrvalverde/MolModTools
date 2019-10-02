
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 prtrte"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "prtrte.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "prtrte.web"
      subroutine prtrte(IOUT,LNK,JOP)
      implicit none
      integer alt,buf,crd,i,IOUT,JOP,l,LNK,nb,new,nlink,oalt,ocrd,oov,ov
     &,putone,seg
      dimension JOP(50,50),LNK(200),buf(20)
      data nb/1/
      
      
      ocrd=-1
      oalt=-1
      oov=-1
      nlink=0
      
100   nlink=nlink+1
      l=LNK(nlink)
      if(l.NE.0)then
      if(l.GT.10000)then
      
      alt=l/1000000
      crd=mod(l,1000000)/10000
      ov=mod(l,10000)/100
      seg=mod(l,100)
      
      new=0
      if(crd.NE.ocrd.OR.alt.NE.oalt.OR.ov.NE.oov)new=1
      ocrd=crd
      oalt=alt
      oov=ov
      if(new.EQ.1)then
      
      nb=nb-1
      if(nb.GT.0)call putchr(';',buf,nb)
      if(nb.GT.0)call strout(IOUT,buf,nb,1)
      nb=0
      if(alt.NE.0)then
      call putchr('(',buf,nb)
      call decchr(alt,buf,nb)
      call putchr(')',buf,nb)
      endif
      call decchr(ov,buf,nb)
      call putchr('/',buf,nb)
      
      putone=0
      do 110 i=1,50
      if(JOP(i,crd).NE.0)then
      putone=1
      call decchr(i,buf,nb)
      call putchr('=',buf,nb)
      call decchr(JOP(i,crd),buf,nb)
      call putchr(',',buf,nb)
      endif
110   continue
      if(putone.EQ.1)nb=nb-1
      call putchr('/',buf,nb)
      endif
      
      call decchr(seg,buf,nb)
      call putchr(',',buf,nb)
      else
      
      if(nb.GT.0)nb=nb-1
      call putchr('(',buf,nb)
      call decchr(l,buf,nb)
      call putchr(')',buf,nb)
      call putchr(';',buf,nb)
      endif
      goto 100
      endif
      
      nb=nb-1
      call putchr(';',buf,nb)
      call strout(IOUT,buf,nb,1)
      return
      
      end
C* :1 * 
      
