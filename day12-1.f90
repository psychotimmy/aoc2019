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
  integer :: info,l1,l2,l3,pot,kin,total
  character (len=80) :: position

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

! Apply gravity for each pair of moons, then apply velocity, 1000 times

  do l1=1,1000
    do l2=1,3
      do l3=l2+1,4
        call applygravity(moons(l2),moons(l3))
      end do
    end do
    do l2=1,4
      call applyvelocity(moons(l2))
    end do
  end do

! Calculate the total energy in the system
 
  total=0 
  do l1=1,4
    pot=abs(moons(l1)%pos(1))+abs(moons(l1)%pos(2))+abs(moons(l1)%pos(3))
    kin=abs(moons(l1)%vel(1))+abs(moons(l1)%vel(2))+abs(moons(l1)%vel(3))
    total=total+pot*kin
  end do

  write(*,'(a,i8)')"Total energy in the system after 1000 steps is",total

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

end program day12p1
