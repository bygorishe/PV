      program main
      integer n, ia, ja
      real al, di
      dimension ia(100000)
      dimension ja(100000), al(100000), di(100000)
      open(10, file = 'size.txt', err = 100)
      read (10, *) n
      open(11, file = 'ia.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      open(12, file = 'ja.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      open(13, file = 'di.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      open(14, file = 'al.bin', form = 'unformatted',
     ,access = 'direct', recl = 4, err = 100)
      do i = 1, n
      read (11, rec = i) ia(i)
      read (13, rec = i) di(i)
      end do
      read(11, rec = n + 1) ia(n + 1)
      do i = 1, ia(n + 1) - 1
       read (12, rec = i) ja(i)
       read (14, rec = i) al(i)
      end do
      call denseMatrix(ia(1), di(1), ja(1), al(1),n, ia(n + 1) - 1)
      return
      
  100 pause 'error'
      end

      subroutine denseMatrix(ia,di,ja,al,n,c)
      integer i,j,n,c,ia,ja
      real di,al,part
      dimension ia(n+1),ja(c),di(n),al(c)
      open(2,file='matrix.txt')
      do i=1,n
       do j=1,n
        if(i .eq. j) then
         write(2,'(f4.2,a1,\)') di(i),' '
        else
         if(i .gt. j) then
          write(2,'(f4.2,a1,\)') part(j,i,ia,
     ,ja,al,n,c),' '
         else
          write(2,'(f4.2,a1,\)') part(i,j,ia,
     ,ja,al,n,c),' '
         endif
        endif
       enddo
       write(2,*)
      enddo
      close(2)
      end

      function part(j,i,ia,ja,al,n,c)
      integer i,j,k,l,n,c,ia,ja
      real al,part
      dimension ia(n+1),ja(c),al(c)
      if(ia(i+1) .ne. ia(i)) then
       k = ia(i+1) - ia(i)
       do l=0,k-1
        if(ja(ia(i)+l) .eq. j) then
         part = al(ia(i)+l)
         return
        end if
       end do
       if(l .eq. k) part = 0.0
      else
       part = 0.0
      end if
      end
