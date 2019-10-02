
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uhftst"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uhftst.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "uhftst.web"
      subroutine uhftst(NBASIS,IRWE,IRWCA,IRWPA,S)
      implicit none
      integer i,ii,ij,In,Iout,Ipunch,IRWCA,irwcb,IRWE,IRWPA,irwpb,irwps,
     &irwu,j,NBASIS,nbsq,ntt
      double precision one,pt5,S,zero
      dimension S(*)
      common/io/In,Iout,Ipunch
      data irwcb,irwpb,irwu,irwps/526,530,540,534/
      data pt5/0.5D0/,one/1.0D0/,zero/0.0D0/
      
      
      
      
      
99001 format(1x,'>>>>>>>> UHF CONVERSION IN UHFTST')
      
      
      
      
      write(Iout,99001)
      
      
      call tread(IRWE,S,NBASIS,1,NBASIS,1,0)
      call amove(NBASIS,S,S(1+NBASIS))
      call fileio(5,IRWE,0,0,0)
      call twrite(IRWE,S,2*NBASIS,1,2*NBASIS,1,0)
      
      nbsq=NBASIS**2
      call tread(IRWCA,S,nbsq,1,nbsq,1,0)
      call twrite(irwcb,S,nbsq,1,nbsq,1,0)
      
      ntt=(NBASIS*(NBASIS+1))/2
      call tread(IRWPA,S,ntt,1,ntt,1,0)
      call ascale(ntt,pt5,S,S)
      call twrite(IRWPA,S,ntt,1,ntt,1,0)
      call twrite(irwpb,S,ntt,1,ntt,1,0)
      
      call aclear(ntt,S)
      call twrite(irwps,S,ntt,1,ntt,1,0)
      
      do 100 i=1,NBASIS
      do 50 j=1,NBASIS
      ij=(i-1)*NBASIS+j
      S(ij)=zero
50    continue
      ii=(i-1)*NBASIS+i
      S(ii)=one
100   continue
      call twrite(irwu,S,nbsq,1,nbsq,1,0)
      
      call ilsw(1,1,1)
      
      return
      
      end
C* :1 * 
      
