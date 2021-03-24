      program main
      implicit none
      integer*4 n,k,readia
      real a
      dimension a(50000000)
      open(2,file='size.txt')
      read(2,*)n
      k = readia(a(1),n)
      close(2)
      call readother(a(n+2),a(n+n+2),a(2*n+2+k),
     ,a(2*(n+k+1)),n,k)
      call mult(a(1), a(n+2), a(n+n+2), a(2*n+2+k),
     ,a(2*(n+k+1)),a(2*(n+k+1)+n), n, k)
      end

      function readia(ia,n)
      integer i,ia
      dimension ia(n+1)
      open(3, file = 'ia.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      do i=1,n + 1
       read(3, rec = i, err = 100) ia(i)
      enddo
      close(3)
      readiad = int(ia(n+1)) - 1
      return
      
  100 pause 'error'
      end

      subroutine readother(di,ja,al,v,n,k)
      integer i,k,n,ja
      real di,al,v
      dimension di(n),ja(k),al(k),v(n)
      open(4, file = 'ja.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      do i=1,k
       read(4, rec = i, err = 100) ja(i)
      end do
      close(4)
      open(4, file = 'di.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      do i=1,n
       read(4, rec = i, err = 100) di(i)
      end do
      close(4)
      open(4, file = 'al.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      do i=1,k
       read(4, rec = i, err = 100) al(i)
      end do
      close(4)
      open(4, file = 'vector.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      do i=1,n
       read(4, rec = i, err = 100) v(i)
      end do
      close(4)
      return
      
  100 pause 'error'
      end

      subroutine mult(ia,di,ja,al,x,y,n,c)
      integer i,k,c,ia,ja
      real di,al,x,y
      dimension ia(n+1),ja(c),di(n),al(c),x(n),y(n)
      do i=1,n
       y(i) = di(i)*x(i)
      enddo
      do i=1,n
       do k=ia(i),ia(i+1)-1
        y(i) = y(i) + al(k)*x(ja(k))
        y(ja(k)) = y(ja(k)) + al(k)*x(i)
       end do
      enddo
      open(5,file='result.txt')
      write(5,*)(y(i),' ',i=1,n)
      close(5)
      return

      end
