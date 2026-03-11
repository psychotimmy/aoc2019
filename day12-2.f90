module day12
! Represent the positions and velocities of the moons as a type
  type moon
    integer :: pos(3),vel(3)
  end type moon
! There are 4 moons in the problem
  type(moon) :: moons(4)
end module day12

program day12p1
  use day12
  implicit none
  integer :: info,l1,l2,l3
  integer (kind=8),dimension(3) :: period
  integer (kind=8) :: total
  character (len=80) :: position
  type(moon) :: initial(4)

  write(*,'(a)')"Advent of Code 2019 day 12, part 1"
  write(*,'(a)')""

  open(10,file="day12in.txt")
! Set up the starting positions and velocities for each moon
  do l1=1,4
    read(10,'(a)',iostat=info) position
    if (info<0) exit
    do l2=1,len_trim(position)
      if ((position(l2:l2) == '<').or. &
          (position(l2:l2) == '>').or. &
          (position(l2:l2) == '=').or. &
          (position(l2:l2) == 'x').or. &
          (position(l2:l2) == 'y').or. &
          (position(l2:l2) == 'z')) position(l2:l2)=' '
    end do
    read (position,*) (moons(l1)%pos(l2),l2=1,3)
    do l2=1,3
      moons(l1)%vel(l2)=0
    end do
  end do
  close(10)

! Apply gravity for each pair of moons, then apply velocity, until
! we have calculated the period for x,y and z

  initial=moons
  period=0
  l1=0

  do 
    do l2=1,3
      do l3=l2+1,4
        call applygravity(moons(l2),moons(l3))
      end do
    end do
    do l2=1,4
      call applyvelocity(moons(l2))
    end do

!   increment the number of steps
    l1=l1+1

!   are any of the periods complete - i.e. velocity & position on an axis
!   equal to their initial states
    do l2=1,3
      if ((moons(1)%vel(l2) == initial(1)%vel(l2)).and. &
          (moons(2)%vel(l2) == initial(2)%vel(l2)).and. &
          (moons(3)%vel(l2) == initial(3)%vel(l2)).and. &
          (moons(4)%vel(l2) == initial(4)%vel(l2)).and. &
          (moons(1)%pos(l2) == initial(1)%pos(l2)).and. &
          (moons(2)%pos(l2) == initial(2)%pos(l2)).and. &
          (moons(3)%pos(l2) == initial(3)%pos(l2)).and. &
          (moons(4)%pos(l2) == initial(4)%pos(l2))) then
!       only save this value if it is the first time we've seen a complete
!       cycle for this axis
        if (period(l2) == 0) period(l2)=l1
      end if
    end do
!   exit the loop when we have found all 3 periods
    if ((period(1) /=0) .and. (period(2) /= 0) .and. (period(3) /= 0)) exit
  end do
  
  write(*,'(a,i8)')"x period is",period(1)
  write(*,'(a,i8)')"y period is",period(2)
  write(*,'(a,i8)')"z period is",period(3)

  total=lcm(period(1),lcm(period(2),period(3)))

  write(*,'(a)')""
  write(*,'(a,i18)')"Total steps required to return to initial state is",total

contains
  
  subroutine applygravity(moon1,moon2)
    use day12
    implicit none
    type(moon),intent(inout) :: moon1,moon2
    integer :: l1

    do l1=1,3
      if (moon1%pos(l1) < moon2%pos(l1)) then
        moon1%vel(l1)=moon1%vel(l1)+1
        moon2%vel(l1)=moon2%vel(l1)-1
      else if (moon1%pos(l1) > moon2%pos(l1)) then
        moon1%vel(l1)=moon1%vel(l1)-1
        moon2%vel(l1)=moon2%vel(l1)+1
      end if
    end do
  end subroutine applygravity

  subroutine applyvelocity(moon1)
    use day12
    implicit none
    type(moon),intent(inout) :: moon1
    integer l1

    do l1=1,3
      moon1%pos(l1)=moon1%pos(l1)+moon1%vel(l1)
    end do
  end subroutine applyvelocity

  integer (kind=8) function lcm(a,b) result(res)
    implicit none
    integer(kind=8),intent(in) :: a,b
    integer(kind=8) :: n,m

    n=a
    m=b
    res=n*m/gcd(n,m)

  end function lcm

  integer (kind=8) function gcd(a,b) result(res)
    implicit none
    integer(kind=8),intent(in) :: a,b
    integer(kind=8) :: n,m,temp

    n=a
    m=b
    do
      temp=m
      m=mod(n,m)
      n=temp
      if (m == 0) exit
    end do
    res=abs(n)

  end function gcd

end program day12p1
