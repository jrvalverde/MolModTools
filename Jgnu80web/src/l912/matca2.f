
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 matca2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "matca2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "matca2.web"
      subroutine matca2(NIJ,NKL,NMN,IBUC1,IBUC2,IBUC3,ISIGN,IOPT)
      implicit none
      double precision F42,Four,Half,One,Onept5,Ten,Three,thresh,Two,V,Z
     &ero
      integer i,IBUC1,IBUC2,IBUC3,ibucfl,ij,ijbloc,ijind,ijleft,ind1,ind
     &1s,ind2,ind3,indfl,IOPT,iopt1,ISIGN,iv1,iv2,iv21
      integer iv3,iv31,kl,klbloc,klleft,leng,max1,max2,max3,Mdv,mdv3,mdv
     &lft,mn,nfull,nfull1,nfull2,nfull3,NIJ,nijkl,nijmn
      integer NKL,nklmn,NMN,nsum
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      data thresh/1.D-8/
      
      
      
      
      
      
      
      
      
      
      
      call track('MATCA2')
      
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
      if(max2.LT.NMN)max2=NMN
      if(nijmn.LE.mdv3)max2=nijmn
      mdvlft=mdvlft-max2
      nsum=nsum-nijmn
      max1=float(nijkl)*float(mdvlft)/float(nsum)
      if(max1.LT.NIJ)max1=NIJ
      max3=Mdv-max1-max2
      if(max3.LT.NMN)then
      max3=NMN
      max2=Mdv-max1-max3
      if(max2.LT.NMN)then
      max2=NMN
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
      nfull2=max2/NMN
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
      
      ijleft=NIJ
      ijind=0
      call fileio(2,-IBUC2,0,0,0)
200   ijbloc=min0(ijleft,nfull2)
      ijleft=ijleft-ijbloc
      leng=ijbloc*NMN
      if(ibucfl.EQ.0.OR.indfl.EQ.0)call fileio(2,IBUC2,leng,V(iv21),0)
      indfl=1
      
      ind1s=ijind
      ind3=iv3
      if(ISIGN.LT.0)then
      
      do 250 kl=1,klbloc
      ind1=ind1s
      ind2=iv2
      do 220 ij=1,ijbloc
      ind1=ind1+1
      if(dabs(V(ind1)).GE.thresh)then
      do 205 mn=1,NMN
      V(ind3+mn)=V(ind3+mn)-V(ind1)*V(ind2+mn)
205   continue
      endif
      ind2=ind2+NMN
220   continue
      ind1s=ind1s+NIJ
      ind3=ind3+NMN
250   continue
      else
      do 300 kl=1,klbloc
      ind1=ind1s
      ind2=iv2
      do 260 ij=1,ijbloc
      ind1=ind1+1
      if(dabs(V(ind1)).GE.thresh)then
      do 255 mn=1,NMN
      V(ind3+mn)=V(ind3+mn)+V(ind1)*V(ind2+mn)
255   continue
      endif
      ind2=ind2+NMN
260   continue
      ind1s=ind1s+NIJ
      ind3=ind3+NMN
300   continue
      endif
      
      ijind=ijind+ijbloc
      if(ijleft.GT.0)goto 200
      leng=klbloc*NMN
      call fileio(1,IBUC3,leng,V(iv31),0)
      if(klleft.GT.0)goto 100
      return
      
      end
C* :1 * 
      
