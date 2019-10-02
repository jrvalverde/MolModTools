
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 initfc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "initfc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 32 "initfc.web"
      subroutine initfc(IOP,S,INTVEC,FPVEC,NVAR,ITYPE,XNAME,FRCNST)
      implicit none
      double precision chknam,FPVEC,FRCNST,one,S,XNAME
      integer i,idx,In,INTVEC,IOP,Iout,Ipunch,ititle,ITYPE,nb,nel,NVAR,n
     &wrd
      logical test,streq
      dimension S(50,50),IOP(50),INTVEC(*),FPVEC(*),ITYPE(*),XNAME(*)
      dimension FRCNST(*)
      dimension chknam(100),ititle(20)
      common/io/In,Iout,Ipunch
      data one/1.0D0/
      
99001 format(1H0,'MURTAUGH-SARGENT DOESN''T KNOW HOW TO COMPUTE '/1x,'NU
     &MERICAL SECOND DERIVATIVES, IVAR= ',i5/1x,'TRY COMPUTING THEM WITH
     & L103 AND INPUT THEM TO L105')
99002 format(1x,'FORCE CONSTANT MATRIX READ FROM GUESS FILE--')
99003 format(1x,'ATTEMPT TO READ FORCE CONSTANTS FAILS:'/1x,'MIS-MATCH I
     &N VARIABLE NAMES'/1x,'LENGTH=',i5,' EXPECTED LENGTH=',i5)
99004 format(1x,'NAME STRING READ, FIRST 72 CHARACTERS:')
99005 format(1x,'EXPECTED NAME STRING, FIRST 72 CHARACTERS:')
99006 format(1x,'ATTEMPT TO READ FORCE CONSTANTS FAILS:'/1x,'FILE IS WRO
     &NG LENGTH'/1x,'LENGTH=',i5,' EXPECTED LENGTH=',i5)
      
      
      call ident(S,50,NVAR)
      do 100 i=1,NVAR
      ITYPE(i)=5
100   continue
      if(IOP(10).EQ.3)then
      nel=200
      call binrd(chknam,ititle,16HVARIABLE NAMES  ,nwrd,nb)
      test=nwrd.EQ.nel
      test=test.AND.streq(chknam,XNAME,200)
      write(Iout,99002)
      call strout(Iout,ititle,72,1)
      if(.NOT.(test))then
      write(Iout,99003)nwrd,nel
      write(Iout,99004)
      call strout(Iout,chknam,72,1)
      write(Iout,99005)
      call strout(Iout,XNAME,72,1)
      call lnk1e
      endif
      call binrd(FRCNST,ititle,16HFORCE CONSTANTS ,nwrd,nb)
      nel=NVAR*(NVAR+1)
      test=nwrd.EQ.nel
      if(.NOT.(test))then
      write(Iout,99006)nwrd,nel
      call lnk1e
      endif
      call analfc(NVAR,FRCNST,S)
      do 150 i=1,NVAR
      ITYPE(i)=97
      idx=i*(i+1)/2
      if(INTVEC(i).EQ.0)FPVEC(i)=FRCNST(idx)
150   continue
      endif
      do 200 i=1,NVAR
      if(INTVEC(i).NE.0)then
      if(INTVEC(i).NE.1)then
      write(Iout,99001)i
      call lnk1e
      endif
      ITYPE(i)=1
      S(i,i)=one/FPVEC(i)
      endif
200   continue
      
      return
      
      end
C* :1 * 
      
