program day7p2
  implicit none
  integer :: info,intcode(0:1023),prog(0:4,0:1023),sii(2),result,pos(0:4)
  integer :: perms(0:4,120),np,l1,l2,inputsignal,maxthrust,exitopcode

  write(*,'(a)')"Advent of Code 2019 day 7, part 2"
  write(*,'(a)')""
! Initialise intcode array to the invalid value -1
  intcode = -1
  open(10,file="day7in.txt")
  do
    read(10,*,iostat=info) intcode
    if (info<0) exit
  end do
  close(10)
! Calculate the permutations for 5 .. 9 (There are 5! = 120 of these)
! These are all the possible phase sequences.
  np=1
  call genperms(5,9,0,4,perms,np,120)
! Set the maximum thruster signal to zero
  maxthrust=0
! Run the program until it halts for each phase sequence
  do l1=1,np
    write(*,'(a)')""
    write(*,'(a,i4)')"Phase permutation ",l1
    write(*,'(a)')""
!   The initial input signal for each phase sequence is 0
    inputsignal=0
!   Also flag that this is the first set of inputs for the phase -
!   first time round we have two inputs, but subsequently only 1 until
!   we get an opcode 99.
    do l2=0,4
!     The phase comes from the permutations 2d array for the first
!     cycle through the amplifiers only
!     Set up the program input
!     Take a fresh copy of the intcode program for each amplifier
!     if this is the first run, and start at position 0
      prog(l2,0:1023)=intcode
      pos(l2)=0
      sii(1)=perms(l2,l1)
      sii(2)=inputsignal
!     Run the program, set the result to be the next inputsignal
!     this also saves the state of the amplifier's program for the next run
      call runprogram(0,1023,prog(l2,0:1023),pos(l2),sii,2,result,exitopcode)
      inputsignal=result
    end do
!   Run the program until the last amplifier exits with opcode 99
    l2=0
    do
      sii(1)=inputsignal
      call runprogram(0,1023,prog(l2,0:1023),pos(l2),sii,1,result,exitopcode)
      if ((exitopcode == 99) .and. (l2 == 4)) then
        if (result > maxthrust) maxthrust=result
        exit
      end if
      inputsignal=result
      if (l2 < 4) then
        l2=l2+1
      else
        l2=0
      end if
    end do
  end do

  write(*,'(a)')""
  write(*,'(a,i10)')"Maximum signal to thrusters is ",maxthrust

end program day7p2

recursive subroutine genperms(minval,maxval,position,maxpos,perms,nperms,maxperms)
  implicit none
  integer,intent(in) :: minval,maxval,maxpos,position,maxperms
  integer,intent(out) :: perms(0:maxpos,*)
  integer,intent(inout) :: nperms
  integer :: l1

  if (position > maxpos) then
    if (nperms < maxperms) then
      nperms=nperms+1
      perms(0:4,nperms)=perms(0:4,nperms-1)
    end if
  else
    do l1=minval,maxval
      if (.not. any(perms(:position-1,nperms) == l1)) then
        perms(position,nperms)=l1
        call genperms(minval,maxval,position+1,maxpos,perms,nperms,maxperms)
      end if
    end do
  end if 

end subroutine genperms

subroutine runprogram(m,n,tape,pos,sii,ninputs,result,exitopcode)
  implicit none
  integer,intent(in)    :: m,n,ninputs,sii(ninputs)
  integer,intent(inout) :: tape(m:n),pos
  integer,intent(out)   :: result,exitopcode
  integer               :: opcode,operands(5)
  integer               :: currentmode,mode(5),siin

  opcode = tape(pos)
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

!   Have we got the halt program opcode?
    if (mod(opcode,100) == 99) then
!     Modified for day 7 part 2
      exitopcode=99
      exit
    end if

!   Perform the opcode. Note that modified addresses (results) always
!   have to use position mode (0).
    select case (mod(opcode,100))

!     addition
      case (1)
        call getoperands(m,n,tape,pos,mode,operands,2)
        tape(tape(pos+3))=operands(1)+operands(2)
        pos=pos+4

!     multiplication
      case (2)
        call getoperands(m,n,tape,pos,mode,operands,2)
        tape(tape(pos+3))=operands(1)*operands(2)
        pos=pos+4

!     single integer input
      case (3)
        if (siin > ninputs) then
          write(*,*)"Received input number ",siin," but expecting only",ninputs
          stop 8
        else
          tape(tape(pos+1))=sii(siin)
        end if
        write(*,*)"Single integer input is",tape(tape(pos+1))
        siin=siin+1
        pos=pos+2

!     single integer output
      case (4)
        call getoperands(m,n,tape,pos,mode,operands,1)
        write(*,*)"Output value is ",operands(1)
        result=operands(1)
        pos=pos+2
!       Exit to force a return if we get output (day 7 part 2)
        exitopcode=4
        exit

!     jump if true
      case (5)
        call getoperands(m,n,tape,pos,mode,operands,2)
        if (operands(1) /= 0) then
          pos=operands(2)
        else
          pos=pos+3
        endif

!     jump if false
      case (6)
        call getoperands(m,n,tape,pos,mode,operands,2)
        if (operands(1) == 0) then
         pos=operands(2)
       else
         pos=pos+3
       endif

!     less than
      case (7)
        call getoperands(m,n,tape,pos,mode,operands,2)
        if (operands(1) < operands(2)) then
          tape(tape(pos+3))=1
        else
          tape(tape(pos+3))=0
        end if
        pos=pos+4

!     equals
      case (8)
        call getoperands(m,n,tape,pos,mode,operands,2)
        if (operands(1) == operands(2)) then
          tape(tape(pos+3))=1
        else
          tape(tape(pos+3))=0
        end if
        pos=pos+4

!     unknown opcode - fatal error
      case default
        write(*,*)"Illegal opcode ", opcode," at position",pos
        stop 8
    end select

!   Get the next opcode from the tape
    opcode=tape(pos)

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
