
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 putlbl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "putlbl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "putlbl.web"
      subroutine putlbl(N,LTYPE,MOD)
      implicit none
      integer blank,elemnt,i,iatnam,Irtcrd,Ititle,jmp,Label,lcursr,LTYPE
     &,MOD,N,nelmnt,tcur
      logical sixd
      integer orblab(22)
      dimension iatnam(105),elemnt(8)
      save sixd,lcursr,tcur,nelmnt,elemnt
      common/label/Label(1000),Ititle(100),Irtcrd(100)
      data blank/1H /
      data iatnam/'BQ','H ','HE','LI','BE','B ','C ','N ','O ','F ','NE'
     &,'NA','MG','AL','SI','P ','S ','CL','AR','K ','CA','SC','TI','V ',
     &'CR','MN','FE','CO','NI','CU','ZN','GA','GE','AS','SE','BR','KR','
     &RB','SR','Y ','ZR','NB','MO','TC','RU','RH','PD','AG','CD','IN','S
     &N','SB','TE','I ','XE','CS','BA','LA','CE','PR','ND','PM','SM','EU
     &','GD','TB','DY','HO','ER','TM','YB','LU','HF','TA','W ','RE','OS'
     &,'IR','PT','AU','HG','TL','PB','BI','PO','AT','RN','FR','RA','AC',
     &'TH','PA','U ','NP','PU','AM','CM','BK','CF','ES','FM','MD','NO','
     &LR','KY'/
      data orblab/'S  ','PX ','PY ','PZ ','XX ','YY ','ZZ ','XY ','XZ ',
     &'YZ ','D 0','D+1','D-1','D+2','D-2','F 0','F+1','F-1','F+2','F-2',
     &'F+3','F-3'/
      
      
      
      
      
      
      if(MOD.EQ.-1)then
      
      call tread(2,Label,600,1,600,1,0)
      do 50 i=1,1000
      Label(i)=blank
50    continue
      call ilsw(2,2,i)
      sixd=.TRUE.
      if(i.EQ.0)sixd=.FALSE.
      
      lcursr=0
      tcur=0
      call pad(elemnt,tcur,8,blank)
      nelmnt=0
      return
      
      elseif(MOD.NE.0)then
      
      jmp=LTYPE+1
      if(jmp.EQ.2)then
      
      call applab(elemnt,N,orblab(2),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(3),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(4),Label,lcursr,MOD)
      return
      elseif(jmp.EQ.3)then
      
      if(.NOT.sixd)then
      
      call applab(elemnt,N,orblab(11),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(12),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(13),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(14),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(15),Label,lcursr,MOD)
      return
      else
      call applab(elemnt,N,orblab(5),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(6),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(7),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(8),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(9),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(10),Label,lcursr,MOD)
      return
      endif
      elseif(jmp.NE.4)then
      
      call applab(elemnt,N,orblab(1),Label,lcursr,MOD)
      return
      endif
      else
      nelmnt=nelmnt+1
      tcur=0
      call pad(elemnt,tcur,8,blank)
      tcur=0
      call decchr(nelmnt,elemnt,tcur)
      tcur=4
      call putb(iatnam(LTYPE+1),2,elemnt,tcur)
      return
      endif
      
      call applab(elemnt,N,orblab(16),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(17),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(18),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(19),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(20),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(21),Label,lcursr,MOD)
      call applab(elemnt,N,orblab(22),Label,lcursr,MOD)
      return
      
      end
C* :1 * 
      
