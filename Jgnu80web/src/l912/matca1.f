
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 matca1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "matca1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "matca1.web"
      subroutine matca1(NIJ,NKL,NMN,IBUC1,IBUC2,IBUC3,ISIGN,IOPT)
      implicit none
      double precision F42,Four,Half,One,Onept5,Ten,Three,thresh,Two,V,Z
     &ero
      integer i,IBUC1,IBUC2,IBUC3,ibucfl,ij,ind1,ind2,ind2s,ind3,ind3s,i
     &ndfl,IOPT,iopt1,ISIGN,iv1,iv2,iv21,iv3,iv31
      integer kl,klbloc,klleft,leng,max1,max2,max3,Mdv,mdv3,mdvlft,mn,mn
     &bloc,mnind,mnleft,nfull,nfull1,nfull2,nfull3,NIJ,nijkl
      integer nijmn,NKL,nklmn,NMN,nsum
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      data thresh/1.D-8/
      
      
      
      
      
      
      
      
      
      
      call track('MATCA1')
      
      if(NIJ*NKL*NMN.LE.0)return
      
      iopt1=IOPT+1
      mdv3=Mdv/3
      ibucfl=0
      nijkl=NIJ*NKL
      nijmn=NIJ*NMN
      nklmn=NKL*NMN
      nsum=nijkl+nijmn+nklmn
      mdvlft=Mdv
      max2=float(nijmn)*float(mdvlft)/float(nsum)
      if(max2.LT.NIJ)max2=NIJ
      if(nijmn.LE.mdv3)max2=nijmn
      mdvlft=mdvlft-max2
      nsum=nsum-nijmn
      max1=float(nijkl)*float(mdvlft)/float(nsum)
      if(max1.LT.NIJ)max1=NIJ
      max3=Mdv-max1-max2
      if(max3.LT.NMN)then
      max3=NMN
      max2=Mdv-max1-max3
      if(max2.LT.NIJ)then
      max2=NIJ
      max1=Mdv-max2-max3
      if(max1.LT.NIJ)call lnk1e
      endif
      endif
      if(max2.GE.nijmn)ibucfl=1
      
      iv1=0
      iv2=max1
      iv3=max1+max2
      iv21=iv2+1
      iv31=iv3+1
      nfull1=max1/NIJ
      nfull2=max2/NIJ
      nfull3=max3/NMN
      nfull=min0(nfull1,nfull3)
      
      call fileio(2,-IBUC1,0,0,0)
      call fileio(2,-IBUC3,0,0,0)
      call fileio(1,-IBUC3,0,0,0)
      klleft=NKL
      
      indfl=0
100   klbloc=min0(klleft,nfull)
      klleft=klleft-klbloc
      leng=klbloc*NIJ
      call fileio(2,IBUC1,leng,V,0)
      leng=klbloc*NMN
      if(iopt1.EQ.2)then
      
      call fileio(2,IBUC3,leng,V(iv31),0)
      else
      
      do 150 i=1,leng
      V(iv3+i)=Zero
150   continue
      endif
      
      mnleft=NMN
      mnind=0
      call fileio(2,-IBUC2,0,0,0)
200   mnbloc=min0(mnleft,nfull2)
      mnleft=mnleft-mnbloc
      leng=mnbloc*NIJ
      if(ibucfl.EQ.0.OR.indfl.EQ.0)call fileio(2,IBUC2,leng,V(iv21),0)
      indfl=1
      
      ind1=0
      ind3s=iv3+mnind
      if(ISIGN.LT.0)then
      
      do 250 kl=1,klbloc
      ind3=ind3s
      ind2s=iv2+1
      do 220 ij=1,NIJ
      ind1=ind1+1
      if(dabs(V(ind1)).GE.thresh)then
      ind2=ind2s
      do 205 mn=1,mnbloc
      V(ind3+mn)=V(ind3+mn)-V(ind1)*V(ind2)
      ind2=ind2+NIJ
205   continue
      endif
      ind2s=ind2s+1
220   continue
      ind3s=ind3s+NMN
250   continue
      else
      do 300 kl=1,klbloc
      ind3=ind3s
      ind2s=iv2+1
      do 260 ij=1,NIJ
      ind1=ind1+1
      if(dabs(V(ind1)).GE.thresh)then
      ind2=ind2s
      do 255 mn=1,mnbloc
      V(ind3+mn)=V(ind3+mn)+V(ind1)*V(ind2)
      ind2=ind2+NIJ
255   continue
      endif
      ind2s=ind2s+1
260   continue
      ind3s=ind3s+NMN
300   continue
      endif
      
      mnind=mnind+mnbloc
      if(mnleft.GT.0)goto 200
      leng=klbloc*NMN
      call fileio(1,IBUC3,leng,V(iv31),0)
      if(klleft.GT.0)goto 100
      return
      
      end
C* :1 * 
      
