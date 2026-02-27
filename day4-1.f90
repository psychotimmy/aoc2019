program day4p1
  implicit none
  integer :: total,low,high,l1,l2,d(6)
  logical :: doublenum,increasing

  write(*,'(a)')"Advent of Code 2019 day 4, part 1"

  open(10,file="day4in.txt")
  read(10,'(i6)') low
  read(10,'(i6)') high
  close(10)

  total=0

! Note - low and high are both 6 digit numbers as per the puzzle instructions
! split current 6 digit number into digits

  do l1=low,high
    d(1)=l1/100000
    d(2)=mod(l1,100000)/10000
    d(3)=mod(l1,10000)/1000
    d(4)=mod(l1,1000)/100
    d(5)=mod(l1,100)/10
    d(6)=mod(l1,10)

! Identify numbers with at least 2 identical consecutive digits
    doublenum=.false.
    do l2=1,5
      if (d(l2) == d(l2+1)) doublenum=.true.
    end do

! Identify numbers where the digits stay the same or increase in size l->r
    increasing=.true.
    do l2=1,5
      if (d(l2) > d(l2+1)) increasing=.false.
    end do

    if ((increasing).and.(doublenum)) total=total+1

  end do

  write(*,'(a)')""
  write(*,'(a,i7)')"Number of passwords in range is ",total

end program day4p1
