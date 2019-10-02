
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 zget"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "zget.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 36 "zget.web"
      subroutine zget(IOP,ICHARG,MULTIP,TOANG)
      implicit none
      double precision Alpha,Anames,Beta,Bl,con,f45,fp,Fpvec,gatan,one,s
     &ymbls,tmpstr,TOANG,tobohr,torad,tstr,Values,zero
      integer i,Ianz,ICHARG,ieof,if1,In,Intvec,IOP,iord,Iout,Ipunch,iuni
     &ts,Iz,j,Lalpha,Lbeta,Lbl,len,lent,MULTIP
      integer namcnt,ncursr,nsymb,Nvar,Nz
      integer maxsym
      logical ok,iv0,streqc
      dimension IOP(*)
      dimension symbls(100),tstr(6),tmpstr(6),namcnt(50)
      common/io/In,Iout,Ipunch
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alpha(50),Beta(50),Lbl(50),La
     &lpha(50),Lbeta(50),Nz,Nvar
      common/zsubst/Anames(50),Values(50),Intvec(50),Fpvec(50)
      data maxsym/50/
      data zero/0.0D0/
      data one/1.0D0/,f45/45.D0/
      
      
      
      
      
99001 format(' Integer Parameters Encountered on Z-MATRIX Card ',i3)
99002 format(' THE MAXIMUM OF ',i4,'  VARIABLES HAS BEEN EXCEEDED')
99003 format(' Too Many Symbols Encountered by SUBROUTINE ZGET.')
99004 format(' UNDEFINED SYMBOL (Bond Length); Record ',i4)
99005 format(' UNDEFINED SYMBOL (Alpha Angle); Record ',i4)
99006 format(' UNDEFINED SYMBOL (Beta Angle); Record ',i4)
99007 format(1x,5x,'VARIABLES')
99008 format(1x,5x,'CONSTANTS')
      
      
      do 100 i=1,maxsym
      nsymb=0
      Lbl(i)=0
      Lalpha(i)=0
      Lbeta(i)=0
      Intvec(i)=0
      Fpvec(i)=zero
      Bl(i)=zero
      Alpha(i)=zero
      Beta(i)=zero
100   continue
      call zsymb(nsymb,IOP,ICHARG,MULTIP,symbls,namcnt)
      
      ok=.TRUE.
      do 200 i=1,Nz
      if(Lbl(i).EQ.2)Lbl(i)=0
      if(Lalpha(i).EQ.2)Lalpha(i)=0
      if(Lbeta(i).EQ.2)Lbeta(i)=0
      if(Lbl(i).EQ.1.OR.Lalpha(i).EQ.1.OR.Lbeta(i).EQ.1)then
      ok=.FALSE.
      write(Iout,99001)i
      endif
200   continue
      if(.NOT.ok)call lnk1e
      
      if(nsymb.NE.0)then
      do 250 i=1,Nz
      Lbl(i)=Lbl(i)*1000
      Lalpha(i)=Lalpha(i)*1000
      Lbeta(i)=Lbeta(i)*1000
250   continue
      
      Nvar=0
      ncursr=0
      write(Iout,99007)
      
300   call ffread(ieof)
      if(ieof.EQ.0)then
      Nvar=Nvar+1
      if(Nvar.GT.maxsym)then
      write(Iout,99002)maxsym
      call lnk1e
      endif
      
      call ffget(tstr,lent,i,fp,if1)
      if(if1.EQ.iord('END'))Nvar=Nvar-1
      if(if1.NE.iord('END'))then
      if(if1.NE.iord('STR'))call fferr('STR',if1)
      call putb(tstr,lent,Anames,ncursr)
      call putdel(2,Anames,ncursr)
      call ffget(tstr,len,i,Values(Nvar),if1)
      if(if1.NE.iord('FP'))call fferr('FP',if1)
      iv0=.FALSE.
      call ffget(tstr,len,Intvec(Nvar),fp,if1)
      if(if1.NE.iord('END'))then
      if(if1.NE.iord('INT'))call fferr('INT',if1)
      iv0=.TRUE.
      call ffget(tstr,len,i,Fpvec(Nvar),if1)
      if(if1.NE.iord('END'))then
      if(if1.NE.iord('FP'))call fferr('FP',if1)
      endif
      endif
      
      call zmatch(ok,symbls,tstr,lent,Values(Nvar),.TRUE.,nsymb)
      
      call prmout(ok,iv0,tstr,lent,Values(Nvar),Intvec(Nvar),Fpvec(Nvar)
     &)
      if(.NOT.ok)call lnk1e
      goto 300
      endif
      endif
      
      if(nsymb.NE.0)then
      write(Iout,99008)
      
320   call ffread(ieof)
      if(ieof.EQ.0)then
      
      call ffget(tstr,lent,i,fp,if1)
      if(if1.NE.iord('END'))then
      if(if1.NE.iord('STR'))call fferr('STR',if1)
      
      call ffget(tstr,lent,i,fp,if1)
      if(if1.NE.iord('FP'))call fferr('FP',if1)
      
      
      call zmatch(ok,symbls,tstr,lent,fp,.FALSE.,nsymb)
      
      call prmout(ok,.FALSE.,tstr,lent,fp,0,0)
      if(.NOT.ok)call lnk1e
      goto 320
      endif
      endif
      
      if(nsymb.NE.0)then
      if(nsymb.LE.0)then
      write(Iout,99003)
      call lnk1e
      endif
      do 330 i=1,Nz
      if(iabs(Lbl(i)).EQ.3000)write(Iout,99004)Bl(i)
      if(iabs(Lalpha(i)).EQ.3000)write(Iout,99005)Alpha(i)
      if(iabs(Lbeta(i)).EQ.3000)write(Iout,99006)Beta(i)
330   continue
      call lnk1e
      endif
      endif
      endif
      
      
      iunits=IOP(20)
      if(iunits.NE.3)then
      tobohr=one
      torad=one
      if(iunits.EQ.0.OR.iunits.EQ.2)tobohr=one/TOANG
      if(iunits.LE.1)torad=gatan(one)/f45
      do 350 i=1,Nz
      Bl(i)=Bl(i)*tobohr
      Alpha(i)=Alpha(i)*torad
      Beta(i)=Beta(i)*torad
350   continue
      if(Nvar.NE.0)then
      do 380 i=1,Nvar
      con=torad
      do 360 j=1,Nz
      if(iabs(Lbl(j)).EQ.i)con=tobohr
360   continue
      Values(i)=Values(i)*con
380   continue
      endif
      endif
      
      if(IOP(31).NE.0)call rdchg(namcnt,Nz,Ianz,IOP)
      return
      
      end
C* :1 * 
      
