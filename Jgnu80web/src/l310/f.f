
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 f"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "f.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "f.web"
      double precision function f(J,L,M,A,B)
      implicit none
      double precision A,B,Binom,F15,Five,Four,One,term,Three,Two,Zero
      integer i,i0p,ifp,ip,J,L,li,Lind,M,mjmi
      dimension A(*),B(*)
      common/lind/Lind(20)
      common/binom/Binom(28)
      common/con310/Zero,One,Two,Three,Four,Five,F15
      
      
      
      
      
      i0p=max(0,J-M)+1
      ifp=min(J,L)+1
      term=Zero
      
      do 100 ip=i0p,ifp
      i=ip-1
      li=Lind(L+1)+i+1
      mjmi=Lind(M+1)+J-i+1
      term=term+Binom(li)*Binom(mjmi)*A(L-i+1)*B(M+i-J+1)
100   continue
      
      f=term
      return
      
      end
C* :1 * 
      
