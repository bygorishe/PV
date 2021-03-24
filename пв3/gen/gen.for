      program main
      implicit none
      integer *4 n, m, step, max, i, j, str, col, pos
      open(10, file = 'size.txt', err = 100)
      read(10,*,err=100) n, m
      close(10)
      open(11, file = 'ia.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      open(12, file = 'ja.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      open(13, file = 'di.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      open(14, file = 'al.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      open(15, file = 'vector.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)

      max = n*(n-1)/2
      if (m .gt. max) goto 100
      do i = 1, n
       write(13, rec = i, err = 100) 1.
       write(15, rec = i, err = 100) 1.
      end do
      step = max / m
      col = 1
      pos = 1
      str = 1
      i = 1
      write(11, rec = str, err = 100) pos
      do str = 2, n
       col = str - 1
       write(11, rec = str, err = 100) pos
       if (pos .gt. m) cycle
        do j = i, col, step
         write(14, rec = pos, err = 100) 1.
         write(12, rec = pos, err = 100) j
         pos = pos + 1
         if (j .gt. col) exit
        end do
       i = str + i - col
      end do
      return
      
  100 pause 'error'
      end
