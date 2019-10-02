
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 basprt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "basprt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 16 "basprt.web"
      
      subroutine basprt
      implicit none
      double precision ften8,ten,x
      integer I2edsc,I2esf,I5d6d,i6311,Ibasis,iblnk5,Ibmod,Ibpr,id21g,id
     &31g,idee,ieff,Ifbp,igbs,igee,igo,In,Iosc,Iout,ip
      integer Ipt,Ipunch,Irot,isep,isepe,istar,istd,istong,ival,Jpunch,l
     &eft,Llink,losdz,loslp3,losmin,losstv,lp31g,lp41g,mix,nd
      integer nf,ng,Ngic
      integer cursor,string(100)
      dimension istd(16),istong(4),ival(8),lp31g(6),lp41g(6),mix(18)
      dimension i6311(6),igbs(31),iblnk5(5),left(4),isep(5),idee(3)
      dimension ieff(2),ip(3),isepe(4),id31g(4),id21g(4)
      dimension losmin(25),losdz(32),loslp3(31),losstv(26)
      common/io/In,Iout,Ipunch
      common/ifbp/Ifbp
      common/ops301/Ibasis,Ngic,Ipt,I5d6d,Iosc,Ibmod,Ibpr,Llink,I2edsc,I
     &rot,Jpunch,I2esf
      data istar/'*'/
      data igee/'G'/
      data istd/'S','T','A','N','D','A','R','D',' ','B','A','S','I','S',
     &':',' '/
      data istong/'S','T','O','-'/
      data id31g/'-','3','1','G'/
      data id21g/'-','2','1','G'/
      data ival/' ','V','A','L','E','N','C','E'/
      data loslp3/'L','o','s',' ','A','l','a','m','o','s',' ','S','p','l
     &','i','t',' ','V','a','l','e','n','c','e',':','L','P','-','3','1',
     &'G'/
      data losdz/'L','o','s',' ','A','l','a','m','o','s',' ','S','p','l'
     &,'i','t',' ','V','a','l','e','n','c','e',':','L','A','N','L','1','
     &D','Z'/
      data lp31g/'L','P','-','3','1','G'/
      data lp41g/'L','P','-','4','1','G'/
      data mix/'L','P','-','N','1','G',' ','(','N','=','3','/','4',' ','
     &M','I','X',')'/
      data i6311/'6','-','3','1','1','G'/
      data igbs/'G','E','N','E','R','A','L',' ','B','A','S','I','S',':',
     &' ','I','N','P','U','T',' ','F','R','O','M',' ','C','A','R','D','S
     &'/
      data losmin/'L','o','s',' ','A','l','a','m','o','s',' ','M','i','n
     &','i','m','a','l',':','S','T','O','-','3','G'/
      data losstv/'L','o','s',' ','A','l','a','m','o','s',' ','M','i','n
     &','i','m','a','l',':','S','T','O','-','V','A','L'/
      data iblnk5/5*' '/
      data left/'(','S',',',' '/
      data isep/'S','=','P',',',' '/
      data idee/'D',',',' '/
      data ieff/'F',')'/
      data ip/'P',',',' '/
      data isepe/'S','=','P','='/
      data ten/10.0D0/,ften8/1.0D08/
      
      
      
      
      
      
      
99001 format(1x,'UNRECOGNIZED BASIS IN BASPRT, IBASIS=',i10)
99002 format(1x,'UNRECOGNIZED LP-N1G BASIS IN BASPRT, NGIC=',i10)
99003 format(1x,'UNRECOGNIZED SHELL CONSTRAINT IN BASPRT, IOSC=',i10)
99004 format(1x,'MODIFICATION OF INTERNAL BASIS')
99005 format(1x,'NON-STANDARD 2E SCALE FACTOR: ',d8.1)
99006 format(1x,'2-ELECTRON INTEGRAL CUTOFF IS ',d10.3)
      
      
      cursor=0
      Ifbp=0
      if(Ibasis.GE.0.AND.Ibasis.LE.7)then
      
      if(Ibasis.EQ.7)then
      call putstr(igbs,31,string,cursor)
      Ifbp=1
      else
      call putstr(istd,16,string,cursor)
      igo=Ibasis+1
      if(igo.EQ.2)then
      ng=Ngic
      if(ng.EQ.0)ng=4
      call decchr(ng,string,cursor)
      call putstr(id31g,4,string,cursor)
      if(Ipt.GE.1)call puticr(istar,string,cursor)
      if(Ipt.GE.2)call puticr(istar,string,cursor)
      elseif(igo.EQ.3)then
      call putstr(istong,4,string,cursor)
      ng=Ngic
      if(ng.EQ.0)ng=3
      call decchr(ng,string,cursor)
      call puticr(igee,string,cursor)
      if(Ipt.NE.0)call puticr(istar,string,cursor)
      call putstr(ival,8,string,cursor)
      elseif(igo.EQ.4)then
      ng=Ngic
      if(ng.EQ.0)ng=3
      if(ng.EQ.8)ng=1
      if(ng.EQ.1)call putstr(mix,18,string,cursor)
      if(ng.EQ.3)call putstr(lp31g,6,string,cursor)
      if(ng.EQ.4)call putstr(lp41g,6,string,cursor)
      if(ng.NE.1.AND.ng.NE.3.AND.ng.NE.4)then
      write(Iout,99002)Ngic
      call lnk1e
      endif
      if(Ipt.GE.1)call puticr(istar,string,cursor)
      if(Ipt.GE.2)call puticr(istar,string,cursor)
      elseif(igo.EQ.5)then
      call putstr(i6311,6,string,cursor)
      if(Ipt.GE.1)call puticr(istar,string,cursor)
      if(Ipt.GE.2)call puticr(istar,string,cursor)
      elseif(igo.EQ.6)then
      ng=Ngic
      if(ng.EQ.0)ng=3
      call decchr(ng,string,cursor)
      call putstr(id21g,4,string,cursor)
      if(Ipt.GE.1)call puticr(istar,string,cursor)
      if(Ipt.GE.2)call puticr(istar,string,cursor)
      elseif(igo.EQ.7)then
      ng=Ngic
      if(ng.EQ.0)call putstr(losmin,25,string,cursor)
      if(ng.EQ.1)call putstr(losdz,32,string,cursor)
      if(ng.EQ.2)call putstr(losstv,26,string,cursor)
      if(ng.EQ.3)call putstr(loslp3,31,string,cursor)
      else
      call putstr(istong,4,string,cursor)
      ng=Ngic
      if(ng.EQ.0)ng=3
      call decchr(ng,string,cursor)
      call puticr(igee,string,cursor)
      if(Ipt.NE.0)call puticr(istar,string,cursor)
      endif
      endif
      if(Iosc.GE.0.AND.Iosc.LE.2)then
      
      nd=5+I5d6d
      call ilsw(2,16,nf)
      if(nf.EQ.0)nf=7
      if(nf.EQ.1)nf=10
      igo=Iosc+1
      call putstr(iblnk5,5,string,cursor)
      call putstr(left,4,string,cursor)
      if(igo.EQ.2)then
      call putstr(ip,3,string,cursor)
      call decchr(nd,string,cursor)
      call putstr(idee,3,string,cursor)
      call decchr(nf,string,cursor)
      call putstr(ieff,2,string,cursor)
      elseif(igo.EQ.3)then
      call putstr(isepe,4,string,cursor)
      call decchr(nd,string,cursor)
      call putstr(idee,3,string,cursor)
      call decchr(nf,string,cursor)
      call putstr(ieff,2,string,cursor)
      else
      call putstr(isep,5,string,cursor)
      call decchr(nd,string,cursor)
      call putstr(idee,3,string,cursor)
      call decchr(nf,string,cursor)
      call putstr(ieff,2,string,cursor)
      endif
      call strout(Iout,string,cursor,1)
      if(Ibmod.GT.0)then
      write(Iout,99004)
      Ifbp=1
      endif
      
      if(I2esf.GT.0)then
      write(6,*)ten,ften8,I2esf
      x=ften8*(ten**I2esf)
      write(Iout,99005)x
      endif
      
      if(I2edsc.GT.0)then
      x=ten**(-I2edsc)
      write(Iout,99006)x
      endif
      else
      write(Iout,99003)Iosc
      call strout(Iout,string,cursor,1)
      return
      endif
      else
      write(Iout,99001)Ibasis
      return
      endif
      
      
      return
      
      end
C* :1 * 
      
