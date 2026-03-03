program day8p2
  implicit none
  integer :: l1,l2
  character (len=15000) image
  character (len=150) layer,processed

  write(*,'(a)')"Advent of Code 2019 day 8, part 2"
  write(*,'(a)')""

  open(10,file="day8in.txt")
  read(10,'(a)') image
  close(10)

  processed=image(1:150)
! Make the message easier to read than 1s and 0s
  do l1=1,150
    if (processed(l1:l1) == '0') processed(l1:l1)=' '
    if (processed(l1:l1) == '1') processed(l1:l1)='X'
  end do

  do l1=151,len(trim(image)),len(layer)
    layer=image(l1:l1+149)

    do l2=1,150
      if (processed(l2:l2) == '2') then
!       Make the message easier to read than 1s and 0s
        if (layer(l2:l2) == '0') processed(l2:l2)=' '
        if (layer(l2:l2) == '1') processed(l2:l2)='X'
      end if
    end do

  end do

  write(*,'(a)')"The message in the image is:"
  write(*,'(a)')""
  do l1=1,150,25
    write(*,'(a25)')processed(l1:l1+24)
  end do

end program day8p2
