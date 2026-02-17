program day1p1
  implicit none
  integer(kind=8) :: total
  integer         :: fuelcalc,mass,info

  write(*,*)"Advent of Code 2019 day 1, part 1"
  total=0
  open(10,file="day1in.txt")
  do
    read(10,*,iostat=info) mass
    if (info<0) exit
    total=total+fuelcalc(mass)
  end do
  close(10)
  write(*,*)""
  write(*,*)"Sum of all fuel requirememts is ",total

end program day1p1

function fuelcalc (mass) result(res)
  implicit none
  integer, intent(in) :: mass
  integer             :: res

  res = (mass/3)-2

end function fuelcalc
