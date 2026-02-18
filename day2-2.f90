program day2p2
  implicit none
  integer :: info,intcode(1000),original(1000),noun,verb

  write(*,*)"Advent of Code 2019 day 2, part 2"
! Initialise intcode array to the invalid value -1
  intcode = -1
  open(10,file="day2in.txt")
  do
    read(10,*,iostat=info) intcode
    if (info<0) exit
  end do
  close(10)
! Make a copy of the program
  original=intcode
! Run the program for values of noun and verb from 0 to 99, until
! we find the combination that gives the result 19690720
  nounloop: do noun=0,99
              do verb=0,99
                intcode(2)=noun
                intcode(3)=verb
                call runprogram(1000,intcode,1)
                if (intcode(1)==19690720) exit nounloop
                intcode=original
               end do
             end do nounloop

  write(*,*)""
  write(*,*)"100 * noun + verb is ",100*noun+verb

end program day2p2

subroutine runprogram(n,tape,pos)
  implicit none
  integer,intent(in)  :: n,pos
  integer,intent(out) :: tape(n)
  integer             :: currentpos,opcode,left,right,result

  currentpos=pos
  opcode = tape(currentpos)
  do while (opcode /= 99)
!   Need to add 1 to the positions as our tape array index starts at 1
!   ... but intcode assumes that the first position is 0
    left=tape(currentpos+1)+1
    right=tape(currentpos+2)+1
    result=tape(currentpos+3)+1
    select case (opcode)
!     addition
      case (1)
        tape(result)=tape(left)+tape(right)
!     multiplication
      case (2)
        tape(result)=tape(left)*tape(right)
!     unknown opcode - fatal error
      case default
        stop 8
    end select
    currentpos=currentpos+4
    opcode=tape(currentpos)
  end do

end subroutine runprogram
