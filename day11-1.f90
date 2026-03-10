program day11p1
  implicit none
  integer :: info
  integer(kind=8) :: intcode(0:32766),prog(0:0,0:32766),sii(2),result,pos(0:0)
  integer(kind=8) :: rel(0:0),exitopcode
  complex,dimension(5000) :: points
  complex                 :: nextpanel
  integer                 :: npanels,direction,l1
  integer(kind=8),dimension(5000) :: colour
  logical                 :: seenpanel

  write(*,'(a)')"Advent of Code 2019 day 11, part 1"
  write(*,'(a)')""
! Initialise intcode array to 0
  intcode = 0
  open(10,file="day11in.txt")
  do
    read(10,*,iostat=info) intcode
    if (info<0) exit
  end do
  close(10)

! All panels are black to start with
  colour=0
! Our first (next) panel is at the origin, 0,0
  npanels=1
  l1=npanels
  points(npanels)=complex(0,0)
  nextpanel=points(npanels)
! Direction stores the way the robot is pointing - 0 to start
! 0 = up, 1 = right, 2 = down, 3 = left
  direction=0

! Initialise the intcode computer
  prog(0,0:)=intcode
  pos(0)=0
  rel(0)=0
! Set the first input value to 0 (robot starts over a black panel, value 0)
  sii(1)=0
  do
    call runprogram(0_8,32766_8,prog(0,0:),pos(0),rel(0),sii(1),1_8,result,exitopcode)
    if (exitopcode == 99) exit
!   Store the current panel's new colour - note - at l1, not npanels
    colour(l1)=result
!   Get the second output value - no further input required
    call runprogram(0_8,32766_8,prog(0,0:),pos(0),rel(0),sii(1),0_8,result,exitopcode)
    if (exitopcode == 99) exit
!   Execute the turn value (0 = left turn, 1 = right turn)
!   Calculate the direction this leaves the robot facing
    if (result == 0) then
      direction = direction-1
      if (direction == -1) direction = 3
    else
      direction = direction+1
      if (direction == 4) direction = 0
    end if
!   Work out where the next panel will be
    select case (direction)
      case (0)
        nextpanel=nextpanel+complex(0,1)
      case (1)
        nextpanel=nextpanel+complex(1,0)
      case (2)
        nextpanel=nextpanel+complex(0,-1)
      case (3)
        nextpanel=nextpanel+complex(-1,0)
    end select
!   If we've not seen this panel before, add it into the points array
    seenpanel=.false.
    do l1=1,npanels
!     If we have seen this panel, the colour will be the next input to
!     the robot's intcode program
      if (nextpanel == points(l1)) then
        seenpanel=.true.
        sii(1)=colour(l1)
        exit
      end if
    end do
    if (.not.seenpanel) then
      npanels=npanels+1
      l1=npanels
      points(l1)=nextpanel
!     The robot input colour will be black if this is the first time
!     the panel has been seen
      sii(1)=0
    end if
  end do

! If the very last panel had not been seen, then we don't count it as it won't
! have been painted

  if (.not.seenpanel) npanels=npanels-1

  write(*,'(a,i4)')"Number of panels painted at least once is ",npanels

end program day11p1

subroutine runprogram(m,n,tape,pos,rel,sii,ninputs,result,exitopcode)
  implicit none
  integer(kind=8),intent(in)    :: m,n,ninputs,sii(ninputs)
  integer(kind=8),intent(inout) :: tape(m:n),pos,rel
  integer(kind=8),intent(out)   :: result,exitopcode
  integer(kind=8)               :: opcode,operands(5)
  integer(kind=8)               :: currentmode,mode(5),siin

  opcode = tape(pos)
! The input number we're expecting next
  siin = 1

! write(*,*)"relative base now ",rel

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
        call getoperands(m,n,tape,pos,mode,operands,2_8,rel)
        if (mode(3) == 0) tape(tape(pos+3))=operands(1)+operands(2)
        if (mode(3) == 2) tape(rel+tape(pos+3))=operands(1)+operands(2)
        exitopcode=1
        pos=pos+4

!     multiplication
      case (2)
        call getoperands(m,n,tape,pos,mode,operands,2_8,rel)
        if (mode(3) == 0) tape(tape(pos+3))=operands(1)*operands(2)
        if (mode(3) == 2) tape(rel+tape(pos+3))=operands(1)*operands(2)
        exitopcode=2
        pos=pos+4

!     single integer input
      case (3)
        if (siin > ninputs) then
          write(*,*)"Received input number ",siin," but expecting only",ninputs
          stop 8
        else
          if (mode(1) == 0) tape(tape(pos+1))=sii(siin)
          if (mode(1) == 2) tape(rel+tape(pos+1))=sii(siin)
        end if
!       write(*,'(a,i3)')"Single integer input is",sii(siin)
        pos=pos+2
        siin=siin+1
        exitopcode=3

!     single integer output
      case (4)
        call getoperands(m,n,tape,pos,mode,operands,1_8,rel)
        result=operands(1)
        pos=pos+2
!       Exit to force a return if we get output (day 7 part 2)
        exitopcode=4
        exit

!     jump if true
      case (5)
        call getoperands(m,n,tape,pos,mode,operands,2_8,rel)
        if (operands(1) /= 0) then
          pos=operands(2)
        else
          pos=pos+3
        endif
        exitopcode=5

!     jump if false
      case (6)
        call getoperands(m,n,tape,pos,mode,operands,2_8,rel)
        if (operands(1) == 0) then
          pos=operands(2)
        else
          pos=pos+3
        endif
        exitopcode=6

!     less than
      case (7)
        call getoperands(m,n,tape,pos,mode,operands,2_8,rel)
        if (operands(1) < operands(2)) then
          if (mode(3) == 0) tape(tape(pos+3))=1
          if (mode(3) == 2) tape(rel+tape(pos+3))=1
        else
          if (mode(3) == 0) tape(tape(pos+3))=0
          if (mode(3) == 2) tape(rel+tape(pos+3))=0
        end if
        exitopcode=7
        pos=pos+4

!     equals
      case (8)
        call getoperands(m,n,tape,pos,mode,operands,2_8,rel)
        if (operands(1) == operands(2)) then
          if (mode(3) == 0) tape(tape(pos+3))=1
          if (mode(3) == 2) tape(rel+tape(pos+3))=1
        else
          if (mode(3) == 0) tape(tape(pos+3))=0
          if (mode(3) == 2) tape(rel+tape(pos+3))=0
        end if
        exitopcode=8
        pos=pos+4

!     relative base adjustment
      case (9)
        call getoperands(m,n,tape,pos,mode,operands,1_8,rel)
        rel=rel+operands(1)
        exitopcode=9
        pos=pos+2

!     unknown opcode - fatal error
      case default
        write(*,*)"Illegal opcode ", opcode," at position",pos
        stop 8
    end select

!   Get the next opcode from the tape
    opcode=tape(pos)

  end do

end subroutine runprogram

subroutine getoperands(m,n,tape,currentpos,mode,operands,numops,rbase)
  implicit none
! Return numops operands

  integer(kind=8),intent(in)  :: m,n,tape(m:n),currentpos,mode(*),numops,rbase
  integer(kind=8),intent(out) :: operands(*)
  integer(kind=8)             :: l1

  do l1=1,numops
    select case (mode(l1))
!     Position mode
      case (0)
        operands(l1)=tape(tape(currentpos+l1))
!     Immediate mode
      case (1)
        operands(l1)=tape(currentpos+l1)
!     Relative mode
      case (2)
        operands(l1)=tape(rbase+tape(currentpos+l1))
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

  integer(kind=8),intent(inout) :: currentmode
  integer(kind=8),intent(out)   :: mode(*)
  integer(kind=8)               :: l1

! Populate the mode array from the 100's value of the opcode upwards (r -> l)

  l1=1
  do
    if (currentmode == 0) exit
    mode(l1)=mod(currentmode,10)
    currentmode=currentmode/10
    l1=l1+1
  end do

end subroutine getmodes
