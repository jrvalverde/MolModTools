
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 isubst"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "isubst.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 31 "isubst.web"
      integer function isubst(CHAR)
      implicit none
      integer CHAR,i,In,Iout,Ipunch,isave,isub,j,keyi,leni,nch,nchrs,ni
      double precision keyfp,dsub
      dimension CHAR(*)
      logical streqc
      dimension keyfp(2),dsub(2),keyi(108),isub(108),leni(108)
      common/io/In,Iout,Ipunch
      data ni/108/
      data keyi/'H ','HE','LI','BE','B ','C ','N ','O ','F ','NE','NA','
     &MG','AL','SI','P ','S ','CL','AR','K ','CA','SC','TI','V ','CR','M
     &N','FE','CO','NI','CU','ZN','GA','GE','AS','SE','BR','KR','RB','SR
     &','Y ','ZR','NB','MO','TC','RU','RH','PD','AG','CD','IN','SN','SB'
     &,'TE','I ','XE','CS','BA','LA','CE','PR','ND','PM','SM','EU','GD',
     &'TB','DY','HO','ER','TM','YB','LU','HF','TA','W ','RE','OS','IR','
     &PT','AU','HG','TL','PB','BI','PO','AT','RN','FR','RA','AC','TH','P
     &A','U ','NP','PU','AM','CM','BK','CF','ES','FM','MD','NO','LR','KY
     &','BQ','- ','X ','Q '/
      data leni/1,2,2,2,1,1,1,1,1,2,2,2,2,2,1,1,2,2,1,2,2,2,1,13*2,2,2,1
     &,13*2,1,2,19*2,1,12*2,5*2,1,12*2,2,1,1,1/
      data isub/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22
     &,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44
     &,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66
     &,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88
     &,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,0,-1,-1,-1/
      
99001 format(1x,'UNRECOGNIZED ATOMIC SYMBOL:')
      
      do 100 j=1,2
      nchrs=iabs(j-3)
      do 50 i=1,ni
      nch=max0(nchrs,leni(i))
      isave=i
      if(streqc(CHAR,keyi(i),nch))goto 200
50    continue
100   continue
      write(Iout,99001)
      call strout(Iout,CHAR,3,1)
      call lnk1e
      return
      
200   isubst=isub(isave)
      return
      
      end
C* :1 * 
      
