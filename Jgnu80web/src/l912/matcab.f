
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 matcab"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "matcab.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "matcab.web"
      subroutine matcab(NIJ,NKL,NMN,IBUC1,IBUC2,IBUC3,ISIGN,IOPT)
      implicit none
      double precision F42,Four,Half,One,Onept5,Ten,Three,thresh,Two,V,Z
     &ero
      integer i,IBUC1,IBUC2,IBUC3,ibucfl,ij,ijbloc,ijleft,ijmax,ind1,ind
     &2,ind3,IOPT,ISIGN,iv1,iv2,iv21,iv3,iv31,kl
      integer klbloc,klleft,klmax,leng,max1,max2,max3,Mdv,mdv3,mdvlft,mn
     &,nfull,nfull1,nfull2,nfull3,NIJ,nijkl,nijmn,NKL,nklmn
      integer NMN,nsum
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      data thresh/1.D-8/
      
      
      
      
      
      
      
      
      
      
      call track('MATCAB')
      
      if(NIJ*NKL*NMN.LE.0)return
      
      mdv3=Mdv/3
      ibucfl=0
      nijkl=NIJ*NKL
      nijmn=NIJ*NMN
      nklmn=NKL*NMN
      nsum=nijkl+nijmn+nklmn
      mdvlft=Mdv
      max3=float(nklmn)*float(Mdv)/float(nsum)
      if(max3.LT.NMN)max3=NMN
      if(nklmn.LE.mdv3)max3=nklmn
      mdvlft=mdvlft-max3
      nsum=nsum-nklmn
      max1=float(nijkl)*float(mdvlft)/float(nsum)
      if(max1.LT.NKL)max1=NKL
      max2=Mdv-max1-max3
      if(max2.LT.NMN)then
      max2=NMN
      max3=Mdv-max1-max2
      if(max3.LT.NMN)then
      max3=NMN
      max1=Mdv-max2-max3
      if(max1.LT.NKL)call lnk1e
      endif
      endif
      if(max3.GE.nklmn)ibucfl=1
      
      iv1=0
      iv2=max1
      iv3=max1+max2
      iv21=iv2+1
      iv31=iv3+1
      nfull1=max1/NKL
      nfull2=max2/NMN
      nfull3=max3/NMN
      nfull=min0(nfull1,nfull2)
      
      call fileio(2,-IBUC1,0,0,0)
      call fileio(2,-IBUC2,0,0,0)
      ijleft=NIJ
      ijmax=0
100   ijbloc=min0(ijleft,nfull)
      ijleft=ijleft-ijbloc
      leng=ijbloc*NKL
      call fileio(2,IBUC1,leng,V,0)
      leng=ijbloc*NMN
      call fileio(2,IBUC2,leng,V(iv21),0)
      
      call fileio(2,-IBUC3,0,0,0)
      call fileio(1,-IBUC3,0,0,0)
      klleft=NKL
      klmax=0
200   klbloc=min0(klleft,nfull3)
      klleft=klleft-klbloc
      leng=klbloc*NMN
      if(ijmax.LE.0.AND.IOPT.EQ.0)then
      do 250 i=1,leng
      V(iv3+i)=Zero
250   continue
      
      elseif(ibucfl.LE.0.OR.ijmax.LE.0)then
      call fileio(2,IBUC3,leng,V(iv31),0)
      endif
      
      ind3=iv3
      if(ISIGN.LT.0)then
      
      do 300 kl=1,klbloc
      ind2=iv2
      ind1=klmax+kl
      do 260 ij=1,ijbloc
      if(dabs(V(ind1)).GT.thresh)then
      do 255 mn=1,NMN
      V(ind3+mn)=V(ind3+mn)-V(ind1)*V(ind2+mn)
255   continue
      endif
      ind1=ind1+NKL
      ind2=ind2+NMN
260   continue
      ind3=ind3+NMN
300   continue
      else
      do 350 kl=1,klbloc
      ind2=iv2
      ind1=klmax+kl
      do 320 ij=1,ijbloc
      if(dabs(V(ind1)).GT.thresh)then
      do 305 mn=1,NMN
      V(ind3+mn)=V(ind3+mn)+V(ind1)*V(ind2+mn)
305   continue
      endif
      ind1=ind1+NKL
      ind2=ind2+NMN
320   continue
      ind3=ind3+NMN
350   continue
      endif
      
      call fileio(1,IBUC3,leng,V(iv31),0)
      klmax=klmax+klbloc
      if(klleft.GT.0)goto 200
      ijmax=ijmax+ijbloc
      if(ijleft.GT.0)goto 100
      return
      
      end
C* :1 * 
      
