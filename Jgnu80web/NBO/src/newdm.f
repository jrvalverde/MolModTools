
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 newdm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "newdm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "newdm.web"
      subroutine newdm(DM,U,EIG,NDIM,IDEL,LEN,NDEL,ITYPE,NMOOCC,ISPIN)
      implicit none
      double precision DM,EIG,one,onetwo,sum,two,U,zero
      integer i,Iatno,Ibxm,IDEL,ii,ij,iout,ISPIN,ITYPE,j,jj,jout,k,Label
     &,LEN,Locc,MAXATM,MAXBAS,Nbotyp,Nbouni
      integer NDEL,ndelor,NDIM,NMOOCC,nocc,Nrank,ntrunc
      parameter(MAXATM=99,MAXBAS=500)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Iatno(M
     &AXBAS),Ibxm(MAXBAS),Nrank(2*MAXBAS),Locc(2*MAXBAS)
      dimension DM(1),U(NDIM,NDIM),EIG(NDIM),IDEL(LEN)
      data zero/0.0D0/,one/1.0D0/,two/2.0D0/
      onetwo=two
      if(ISPIN.NE.0)onetwo=one
      ntrunc=NDIM
      if(ITYPE.EQ.1)ntrunc=NDIM-NDEL
      call rnkeig(Nrank,EIG,ntrunc,NDIM,Locc)
      nocc=0
      do 100 i=1,ntrunc
      if(Nrank(i).LE.NMOOCC)then
      nocc=nocc+1
      Locc(nocc)=i
      endif
100   continue
      ndelor=NDIM-ntrunc
      
      
      ii=0
      ij=0
      iout=1
      do 200 i=1,NDIM
      if(iout.LE.ndelor)then
      if(i.EQ.IDEL(iout))then
      iout=iout+1
      EIG(i)=zero
      do 110 j=1,i
      ij=ij+1
      DM(ij)=zero
110   continue
      goto 200
      endif
      endif
      ii=ii+1
      jout=1
      jj=0
      do 150 j=1,i
      if(jout.LE.ndelor)then
      if(j.EQ.IDEL(jout))then
      jout=jout+1
      ij=ij+1
      DM(ij)=zero
      goto 150
      endif
      endif
      jj=jj+1
      sum=zero
      do 120 k=1,NMOOCC
      sum=sum+U(ii,Locc(k))*U(jj,Locc(k))
120   continue
      ij=ij+1
      DM(ij)=sum*onetwo
      if(i.EQ.j)EIG(i)=sum*onetwo
150   continue
200   continue
      return
      end
C* :1 * 
      
