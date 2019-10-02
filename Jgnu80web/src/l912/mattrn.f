
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 mattrn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "mattrn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "mattrn.web"
      subroutine mattrn(NI,NJ,NK,NL,ISWITC,IBUC1,IBUC2,MAX)
      implicit none
      integer i,i1,i2,iblock,IBUC1,IBUC2,ileft,ind1,index,indexs,indn3,I
     &SWITC,j,j1,jblock,jend,jleft,jmin,jndex,jndexs
      integer k,k1,k2,kblock,kend,kenjen,kleft,kmin,l,l1,l2,leng,lj,lk,l
     &l,MAX,Mdv,NI,nifull,NJ
      integer njfull,njk,njkl,njl,NK,nkfull,nkl,NL,nljend,nlkend,nltot
      double precision V1,V2
      common/v/V1(10000),V2(10000),Mdv
      
      
      
      
      
      
      
      call track('MATTRN')
      
      if(NI*NJ*NK*NL.LE.0)return
      nkl=NK*NL
      njl=NJ*NL
      njk=NJ*NK
      njkl=NJ*nkl
      njfull=MAX/nkl
      nkfull=MAX/njl
      nifull=MAX/njkl
      indn3=1
      if(njkl.LE.MAX)indn3=2
      if(njfull.LE.0.AND.nkfull.LE.0)call lnk1e
      call fileio(2,-IBUC1,0,0,0)
      call fileio(1,-IBUC2,0,0,0)
      
      
      ileft=NI
100   indexs=(NI-ileft)*njkl
      jndexs=(NI-ileft)*njkl
      jmin=NJ
      kmin=NK
      jend=NJ
      kend=NK
      iblock=1
      if(indn3.EQ.2)then
      
      iblock=min0(ileft,nifull)
      leng=iblock*njkl
      call fileio(2,IBUC1,leng,V1,0)
      goto 600
      else
      
      if(ISWITC.EQ.1.OR.ISWITC.EQ.5)then
      elseif(ISWITC.EQ.3.OR.ISWITC.EQ.4)then
      goto 400
      
      elseif(NJ.GE.NK)then
      goto 400
      endif
      
      kleft=NK
      kmin=0
      endif
200   kblock=min0(kleft,nkfull)
      kleft=kleft-kblock
      leng=kblock*NL
      kend=kblock
      i1=indexs+kmin*NL
      i2=1
      do 300 j=1,NJ
      index=i1
      call fileio(2,-IBUC1,leng,V1(i2),index)
      i1=i1+nkl
      i2=i2+leng
300   continue
      goto 600
      
400   jleft=NJ
      jmin=0
500   jblock=min0(jleft,njfull)
      jleft=jleft-jblock
      leng=jblock*nkl
      call fileio(2,IBUC1,leng,V1,0)
      jend=jblock
      
600   nljend=NL*jend
      nlkend=NL*kend
      kenjen=kend*jend
      nltot=kenjen*NL
      
      
      if(ISWITC.EQ.2)then
      
      lj=NL
      lk=nljend
      ll=1
      elseif(ISWITC.EQ.3)then
      
      lj=nlkend
      lk=1
      ll=kend
      elseif(ISWITC.EQ.4)then
      
      lj=kend
      lk=1
      ll=kenjen
      elseif(ISWITC.EQ.5)then
      
      lj=1
      lk=nljend
      ll=jend
      else
      
      lj=1
      lk=jend
      ll=kenjen
      endif
      
      ind1=1
      i1=1
      do 700 i=1,iblock
      j1=i1
      do 650 j=1,jend
      k1=j1
      do 620 k=1,kend
      l1=k1
      do 610 l=1,NL
      V2(l1)=V1(ind1)
      ind1=ind1+1
      l1=l1+ll
610   continue
      k1=k1+lk
620   continue
      j1=j1+lj
650   continue
      i1=i1+njkl
700   continue
      
      
      if(indn3.EQ.2)then
      
      leng=iblock*njkl
      call fileio(1,IBUC2,leng,V2,0)
      goto 1000
      else
      
      if(ISWITC.EQ.2)then
      
      if(NJ.GE.NK)then
      k1=jndexs+jmin*NL
      k2=1
      leng=nljend
      do 710 k=1,NK
      jndex=k1
      call fileio(1,-IBUC2,leng,V2(k2),jndex)
      k1=k1+njl
      k2=k2+leng
710   continue
      goto 900
      endif
      elseif(ISWITC.NE.3.AND.ISWITC.NE.5)then
      
      l1=jndexs+jmin*kmin
      l2=1
      leng=kenjen
      do 720 l=1,NL
      jndex=l1
      call fileio(1,-IBUC2,leng,V2(l2),jndex)
      l1=l1+njk
      l2=l2+leng
720   continue
      goto 800
      endif
      
      leng=kenjen*NL
      call fileio(1,IBUC2,leng,V2,0)
      endif
800   if(ISWITC.NE.3.AND.ISWITC.NE.4)then
      
      kmin=kmin+kblock
      if(kleft.LE.0)goto 1000
      goto 200
      endif
      
900   jmin=jmin+jblock
      if(jleft.GT.0)goto 500
      
1000  ileft=ileft-iblock
      if(ileft.GT.0)goto 100
      
      return
      
      end
C* :1 * 
      
