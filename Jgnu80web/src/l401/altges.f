      subroutine altges(A,AA,NB,INC)
      implicit none
      double precision A,AA,temp,x
      integer i,ia1,Ialtbf,ieof,if1,In,INC,iord,Iout,Ipunch,Irtcrd,irwlb
     &l,Ititle,j,k,Label,len,lrwlbl,nalt,NB
      integer ntmp
      dimension A(NB,NB),AA(NB)
      integer tmpbuf(24)
      common/label/Label(1000),Ititle(100),Irtcrd(100)
      common/io/In,Iout,Ipunch
      equivalence(Ialtbf,Irtcrd(89))
      data irwlbl/502/,lrwlbl/600/
      
      
99001 format(55H UNEXPECTED END-OF-FILE WHILE READING ORBITAL SWITCHES.)
99002 format(28H PAIRS OF ORBITALS SWITCHED:)
      
      nalt=INC
      ntmp=0
      if(INC.EQ.0)then
      nalt=0
      call tread(irwlbl,Label,lrwlbl,1,lrwlbl,1,0)
      call ffset(0)
      endif
      
100   call ffread(ieof)
      if(ieof.NE.0)write(Iout,99001)
      if(ieof.NE.0)call lnk1e
      
      ia1=0
      call ffget(tmpbuf,len,i,x,if1)
      if(if1.NE.iord('END'))then
      if(if1.NE.iord('INT'))call fferr('INT',if1)
      call ffget(tmpbuf,len,j,x,if1)
      if(if1.NE.iord('INT'))call fferr('INT',if1)
      
      call puti(i,tmpbuf,ntmp)
      call puti(j,tmpbuf,ntmp)
      call putdel(2,tmpbuf,ntmp)
      
      do 150 k=1,NB
      temp=A(k,i)
      A(k,i)=A(k,j)
      A(k,j)=temp
150   continue
      temp=AA(i)
      AA(i)=AA(j)
      AA(j)=temp
      goto 100
      endif
      
      write(Iout,99002)
      if(ntmp.GE.2)call strout(Iout,tmpbuf,ntmp-1,1)
      if(ntmp.GE.1)call putb(tmpbuf,ntmp,Ialtbf,nalt)
      call putdel(3,Ialtbf,nalt)
      
      call twrite(irwlbl,Label,lrwlbl,1,lrwlbl,1,0)
      return
      
      end
      
