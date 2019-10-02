
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 facab"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "facab.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "facab.web"
      subroutine facab(NA,NB,CAX,CBX,XAB)
      implicit none
      real*8 afac,bic,CAX,caxp,CBX,cbxp,one,XAB,zero
      integer i,ia,ia1,ib,ib1,iend,ind,NA,na1,naind,NB,nb1,nbind
      dimension bic(10),ind(4),XAB(*)
      save bic,ind,zero
      data bic/1.0D0,1.0D0,1.0D0,1.0D0,2.0D0,1.0D0,1.0D0,3.0D0,3.0D0,1.0
     &D0/,ind/1,2,4,7/,zero/0.0D0/,one/1.0D0/
      
      iend=NA+NB+1
      do 100 i=1,iend
      XAB(i)=zero
100   continue
      na1=NA+1
      nb1=NB+1
      naind=ind(na1)
      nbind=ind(nb1)
      do 200 ia1=1,na1
      ia=ia1-1
      if((NA-ia).NE.0)then
      caxp=CAX**(NA-ia)
      else
      caxp=one
      endif
      afac=bic(naind+ia)*caxp
      do 150 ib1=1,nb1
      ib=ib1-1
      if((NB-ib).NE.0)then
      cbxp=CBX**(NB-ib)
      else
      cbxp=one
      endif
      XAB(ia+ib+1)=XAB(ia+ib+1)+afac*bic(nbind+ib)*cbxp
150   continue
200   continue
      return
      end
C* :1 * 
      
