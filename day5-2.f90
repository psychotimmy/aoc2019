program day5p2
  implicit none
  integer :: info,intcode(0:1023),result

  write(*,*)"Advent of Code 2019 day 5, part 2"
! Initialise intcode array to the invalid value -1
  intcode = -1
  open(10,file="day5in.txt")
  do
    read(10,*,iostat=info) intcode
    if (info<0) exit
  end do
  close(10)
! Run the program
  write(*,*)""
  call runprogram(0,1023,intcode,0,5,result)
  write(*,*)""
  write(*,*)"The diagnostic code returned is ",result

end program day5p2

subroutine runprogram(m,n,tape,pos,sii,result)
  implicit none
  integer,intent(in)  :: m,n,pos,sii
  integer,intent(out) :: tape(m:n),result
  integer             :: currentpos,opcode,operands(5)
  integer             :: currentmode,mode(5)

  currentpos=pos
  opcode = tape(currentpos)

! Main program loop
! Actual opcode is the last 2 digits of the current tape position

  do
!   Deal with instruction mode for this opcode
!   0 is the default mode = position mode, therefore blank the mode array
    mode = 0

    currentmode=opcode/100
    call getmodes(currentmode,mode)

!   Perform the opcode. Note that modified addresses (results) always
!   have to use position mode (0).
    select case (mod(opcode,100))

!     addition
      case (1)
        call getoperands(m,n,tape,currentpos,mode,operands,2)
        tape(tape(currentpos+3))=operands(1)+operands(2)
        currentpos=currentpos+4

!     multiplication
      case (2)
        call getoperands(m,n,tape,currentpos,mode,operands,2)
        tape(tape(currentpos+3))=operands(1)*operands(2)
        currentpos=currentpos+4

!     single integer input
      case (3)
        tape(tape(currentpos+1))=sii
        write(*,*)"Single integer input is",tape(tape(currentpos+1))
        currentpos=currentpos+2

!     single integer output
      case (4)
        call getoperands(m,n,tape,currentpos,mode,operands,1)
        write(*,*)"Output value is ",operands(1)
        result=operands(1)
        currentpos=currentpos+2

!     jump if true
      case (5)
        call getoperands(m,n,tape,currentpos,mode,operands,2)
        if (operands(1) /= 0) then
          currentpos=operands(2)
        else
          currentpos=currentpos+3
        endif

!     jump if false
      case (6)
        call getoperands(m,n,tape,currentpos,mode,operands,2)
        if (operands(1) == 0) then
         currentpos=operands(2)
       else
         currentpos=currentpos+3
       endif

!     less than
      case (7)
        call getoperands(m,n,tape,currentpos,mode,operands,2)
        if (operands(1) < operands(2)) then
          tape(tape(currentpos+3))=1
        else
          tape(tape(currentpos+3))=0
        end if
        currentpos=currentpos+4

!     equals
      case (8)
        call getoperands(m,n,tape,currentpos,mode,operands,2)
        if (operands(1) == operands(2)) then
          tape(tape(currentpos+3))=1
        else
          tape(tape(currentpos+3))=0
        end if
        currentpos=currentpos+4

!     unknown opcode - fatal error
      case default
        write(*,*)"Illegal opcode ", opcode,currentpos
        stop 8
    end select

!   Get the next opcode from the tape
    opcode=tape(currentpos)
    if (mod(opcode,100) == 99) exit
  end do

end subroutine runprogram

subroutine getoperands(m,n,tape,currentpos,mode,operands,numops)
  implicit none
! Return numops operands

  integer,intent(in)  :: m,n,tape(m:n),currentpos,mode(*),numops
  integer,intent(out) :: operands(*)
  integer             :: l1

  do l1=1,numops
    select case (mode(l1))
!     Position mode
      case (0)
        operands(l1)=tape(tape(currentpos+l1))
!     Immediate mode
      case (1)
        operands(l1)=tape(currentpos+l1)
      case default
        write(*,*)"Illegal instruction mode ",mode(l1)
        stop 8
     end select
  end do

end subroutine getoperands

subroutine getmodes(currentmode,mode)
  implicit none
! Return the addressing mode for each operand from the 
! current opcode currentmode

  integer,intent(inout)  :: currentmode
  integer,intent(out) :: mode(*)
  integer :: l1

! Populate the mode array from the 100's value of the opcode upwards (r -> l)

  l1=1
  do
    if (currentmode == 0) exit
    mode(l1)=mod(currentmode,10)
    currentmode=currentmode/10
    l1=l1+1
  end do

end subroutine getmodes
