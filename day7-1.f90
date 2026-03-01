program day7p1
  implicit none
  integer :: info,intcode(0:1023),prog(0:1023),sii(2),result
  integer :: perms(0:4,120),np,l1,l2,inputsignal,maxthrust

  write(*,'(a)')"Advent of Code 2019 day 7, part 1"
  write(*,'(a)')""
! Initialise intcode array to the invalid value -1
  intcode = -1
  open(10,file="day7in.txt")
  do
    read(10,*,iostat=info) intcode
    if (info<0) exit
  end do
  close(10)
! Calculate the permutations for 0 .. 4 (There are 5! = 120 of these)
! These are all the possible phase sequences.
  np=1
  call genperms(0,4,0,perms,np,120)
! Set the maximum thruster signal to zero
  maxthrust=0
! Run the program 5 times for each phase sequence
  do l1=1,np
!   The initial input signal for each phase sequence is 0
    inputsignal=0
    do l2=0,4
!     The phase comes from the permutations 2d array
!     Set up the program input
      sii(1)=perms(l2,l1)
      sii(2)=inputsignal
!     Take a fresh copy of the intcode program for each run
      prog=intcode
!     Run the program, set the result to be the next inputsignal
      call runprogram(0,1023,prog,0,sii,2,result)
      inputsignal=result
    end do
    if (result > maxthrust) maxthrust=result
  end do

  write(*,'(a)')""
  write(*,'(a,i8)')"Maximum signal to thrusters is ",maxthrust

end program day7p1

recursive subroutine genperms(minval,maxval,position,perms,nperms,maxperms)
  implicit none
  integer,intent(in) :: minval,maxval,position,maxperms
  integer,intent(out) :: perms(minval:maxval,*)
  integer,intent(inout) :: nperms
  integer :: l1

  if (position > maxval) then
    if (nperms < maxperms) then
      nperms=nperms+1
      perms(0:4,nperms)=perms(0:4,nperms-1)
    end if
  else
    do l1=minval,maxval
      if (.not. any(perms(:position-1,nperms) == l1)) then
        perms(position,nperms)=l1
        call genperms(minval,maxval,position+1,perms,nperms,maxperms)
      end if
    end do
  end if 

end subroutine genperms

subroutine runprogram(m,n,tape,pos,sii,ninputs,result)
  implicit none
  integer,intent(in)  :: m,n,pos,ninputs,sii(ninputs)
  integer,intent(out) :: tape(m:n),result
  integer             :: currentpos,opcode,operands(5)
  integer             :: currentmode,mode(5),siin

  currentpos=pos
  opcode = tape(currentpos)
! The input number we're expecting next
  siin = 1

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
        if (siin > ninputs) then
          write(*,*)"Received input number ",siin," but expecting only",ninputs
          stop 8
        end if
        tape(tape(currentpos+1))=sii(siin)
        write(*,*)"Single integer input is",tape(tape(currentpos+1))
        siin=siin+1
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
