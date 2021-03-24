      real function f(x)
      implicit none
      real x 
      f=exp(x)*cos(10.*x) 
      end

      real function g(x,n) 
      implicit none
      real x 
      integer n
      g=(n+1)*x**n
      end
  
      real function rectpoly(n,nn,a,b) 
      implicit none
      real sum1,a,b 
      real h,g 
      integer n,nn,i
      h=(b-a)/nn
      sum1=0.
      do i=1,nn
        sum1=sum1+h*g(((2.*a+((i-1)+i)*h)/2.),n)
      enddo         
      rectpoly=sum1 
      end

      real function rectoscil(nn,a,b) 
      implicit none
      real sum1,a,b 
      real h,f 
      integer nn,i
      h=(b-a)/nn
      sum1=0.
      do i=1,nn
        sum1=sum1+h*f((2.*a+((i-1)+i)*h)/2.) 
      enddo
      rectoscil=sum1 
      return
      end
   
      real function simppoly(n,nn,a,b) 
      implicit none
      real sum1,sum2,a,b 
      real h,g 
      integer n,nn,i
      h=(b-a)/nn
      sum2=0.
      sum1=0.
      do i=1,nn
        sum1=sum1+g((a+(i-1)*h+h/2.),n) 
      enddo
      do i=2,nn
        sum2=sum2+g((a+(i-1)*h),n) 
      enddo         
      simppoly=h*(g(a,n)+g(b,n)+4.*sum1+2.*sum2)/6.
      end

      real function simposcil(nn,a,b) 
      implicit none
      real sum1,sum2,a,b
      real h,f 
      integer nn,i
      h=(b-a)/nn
      sum1=0.
      sum2=0.
      do i=1,nn
        sum1=sum1+f(a+(i-1)*h+h/2.)
      enddo
      do i=2,nn
        sum2=sum2+f(a+(i-1)*h) 
      enddo         
      simposcil=h*(f(a)+f(b)+4.*sum1+2.*sum2)/6. 
      return
      end
      
      real function gausspoly(n,nn,a,b) 
      implicit none
      real sum1,sum2,q,x,a,b 
      real h,g 
      dimension q(4),x(4)
      integer n,nn,j,k
      h=(b-a)/nn
      sum2=0.
      x(1)=-0.8611363
      x(2)=-0.3399810
      x(3)=0.3399810
      x(4)=0.8611363
      q(1)=0.3478548
      q(2)=0.6521451
      q(3)=0.6521451
      q(4)=0.3478548
      do j=1,4
        sum1=0.
        do k=0,nn-1
          sum1=sum1+h*g((a+h*(k)+a+h*(k+1))/2.+x(j)*h/2.,n)
        enddo
        sum2=sum2+q(j)*sum1
      enddo
      gausspoly=sum2/2.
      return
      end

      real function gaussoscil(nn,a,b) 
      implicit none
      real sum1,sum2,q,x,a,b 
      real h,f 
      dimension q(4),x(4)
      integer nn,j,k
      h=(b-a)/nn
      sum2=0.
      x(1)=-0.8611363
      x(2)=-0.3399810
      x(3)=0.3399810
      x(4)=0.8611363
      q(1)=0.3478548
      q(2)=0.6521451
      q(3)=0.6521451
      q(4)=0.3478548
      do j=1,4
        sum1=0.
        do k=0,nn-1
          sum1=sum1+h*f((a+h*k+a+h*(k+1))/2.+x(j)*h/2.) 
        enddo
        sum2=sum2+q(j)*sum1
      enddo
      gaussoscil=sum2/2.
      return
      end
      
      program main
      implicit none
      integer m1,nn,m2,n
      real  a,b,rectpoly,rectoscil,simppoly,simposcil,gausspoly,
     *gaussoscil        
1     print*, 'Select method:'
      print*, '<1> Rectangle'
      print*, '<2> Simpson'
      print*, '<3> Gaus-4'  
      print*, '<4> Exit'   
      read(*,*) m1
      if(m1.eq.4) goto 10     
     
      if(m1.eq.1)then
      print*, 'Method Rectangle'
      print*, 'Enter quantity of segments:'
      read(*,*) nn
      print*, 'Enter begin and end:'
      read(*,*) a,b
2     print*, 'Select:'
      print*, '1) Polynomial'
      print*, '2) Oscillating'
      read(*,*) m2
      if(m2.lt.1.or.m2.gt.2) goto 2
      if(m2.eq.1)then
        print*, 'Enter degree of polynomial:'
        read(*,*)n
        open(10,file='rectpoly.txt',status='unknown',err=5)
        write(10,300) nn,a,b,rectpoly(n,nn,a,b)
        write(*,300) nn,a,b,rectpoly(n,nn,a,b)
        close(10)
      endif
      if(m2.eq.2)then
        open(10,file='rectoscil.txt',status='unknown',err=5)
        write(10,300) nn,a,b,rectoscil(nn,a,b)
        write(*,300) nn,a,b,rectoscil(nn,a,b)
        close(10)            
      endif    
      endif   
         
      if(m1.eq.2)then
      print*, 'Method Simpson'
      print*, 'Enter quantity of segments:'
      read(*,*) nn
      print*, 'Enter begin and end:'
      read(*,*) a,b
3     print*, 'Select:'
      print*, '1) Polynomial'
      print*, '2) Oscillating'
      read(*,*) m2
      if(m2.lt.1.or.m2.gt.2) goto 3
      if(m2.eq.1)then
        print*, 'Enter degree of polynomial:'
        read(*,*)n
        open(10,file='simppoly.txt',status='unknown',err=5)
        write(10,300) nn,a,b,simppoly(n,nn,a,b)
        write(*,300) nn,a,b,simppoly(n,nn,a,b)
        close(10) 
      endif
      if(m2.eq.2)then
        open(10,file='simposcil.txt',status='unknown',err=5)
        write(10,300) nn,a,b,simposcil(nn,a,b)
        write(*,300) nn,a,b,simposcil(nn,a,b)
        close(10)          
      endif    
      endif  
       
      if(m1.eq.3)then
      print*, 'Method Gaus-4'
      print*, 'Enter quantity of segments:'
      read(*,*) nn
      print*, 'Enter begin and end:'
      read(*,*) a,b
4     print*, 'Select:'
      print*,'1) Polynomial'
      print*, '2) Oscillating'
      read(*,*) m2
      if(m2.lt.1.or.m2.gt.2) goto 4
      if(m2.eq.1)then
        print*, 'Enter degree of polynomial:'
        read(*,*) n
        open(10,file='gausspoly.txt',status='unknown',err=5)
        write(10,300) nn,a,b,gausspoly(n,nn,a,b)
        write(*,300) nn,a,b,gausspoly(n,nn,a,b)
        close(10)
      endif
      if(m2.eq.2)then
        open(10,file='gaussoscil.txt',status='unknown',err=5)
        write(10,300) nn,a,b,gaussoscil(nn,a,b)
        write(*,300) nn,a,b,gaussoscil(nn,a,b)
        close(10)        
      endif    
      endif
      
      print*, 'Completed.'
      pause       
      goto 1
300   format('Quantity of segments = 'I8/'a = 'E13.7/'b = 'E13.7/
     ,'Integral = 'E13.7)
301   format('Degree of polynomial = 'I8)
5     print*, 'Error writing file!'
      goto 1
10    end


