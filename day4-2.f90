program day4p2
  implicit none
  integer :: total,low,high,l1,l2,d(6)
  logical :: doublenum,increasing

  write(*,'(a)')"Advent of Code 2019 day 4, part 2"

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

! Identify numbers with at least 2 identical consecutive digits, but not 3+
    doublenum=.false.
    l2=1
    dcheck: do
      if (d(l2) == d(l2+1)) then
        doublenum=.true.
        l2=l2+1
!       Exit with valid double number as we've found it in positions 5 and 6.
        if (l2 == 6) exit dcheck
!       Do we have a triple+ number? If so, reset doublenum false
        do
          if (d(l2) == d(l2+1)) then
            doublenum=.false.
            l2=l2+1
            if (l2 == 6) exit dcheck
          else
!       We just need one valid doublenum anywhere in the number, so exit
!       the dcheck loop if this is the case, otherwise exit inner loop if
!       we haven't checked all of the digits.
            l2=l2+1
            if ((doublenum).or.(l2 ==6)) exit dcheck
            exit
          end if
        end do
      else
!       Evaluate next digit
        l2=l2+1
      end if
!     Exit dcheck at the end of the number
      if (l2 == 6) exit dcheck
    end do dcheck

! Identify numbers where the digits stay the same or increase in size l->r
    increasing=.true.
    do l2=1,5
      if (d(l2) > d(l2+1)) increasing=.false.
    end do

    if ((increasing).and.(doublenum)) total=total+1

  end do

  write(*,'(a)')""
  write(*,'(a,i7)')"Number of passwords in range is ",total

end program day4p2
