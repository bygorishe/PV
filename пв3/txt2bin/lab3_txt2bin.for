      program main
      implicit none
      integer n, k
      open(10, file = 'size.txt', err = 100)
      read(10, *, err = 100) n
      close(10)
      call txt2binint('ia.txt', 'ia.bin', n + 1)
      open(10, file = 'ia.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      read(10, rec = n + 1, err = 100) k
      call txt2binint('ja.txt', 'ja.bin', k - 1)
      call txt2binreal('di.txt', 'di.bin', n)
      call txt2binreal('al.txt' , 'al.bin', k - 1)
      call txt2binreal('vector.txt', 'vector.bin', n)
      return

  100 pause 'error'
      end

      subroutine txt2binreal(txt, bin, n)
      integer i, n
      character *(*) txt, bin
      real a
      dimension a(100000)
      open(11, file = txt, err = 100)
      open(12, file = bin, form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      read(11, *, err = 100) (a(i), i = 1, n)
      do i = 1, n
       write(12, rec = i, err = 100) a(i)
      end do
      close(11)
      close(12)
      return
      
  100 pause 'error'
      end

      subroutine txt2binint(txt, bin, n)
      integer i, n
      character *(*) txt, bin
      integer a
      dimension a(100000)
      open(11, file = txt, err = 100)
      open(12, file = bin, form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      read(11, *, err = 100) (a(i), i = 1, n)
      do i = 1, n
       write(12, rec = i, err = 100) a(i)
      end do

      close(11)
      close(12)
      return

  100 pause 'Error'

      end
