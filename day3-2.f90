program day3p2
  implicit none
  integer   :: info,ncoords1,ncoords2,l1,l2,ip,md,manhattan,minmd,minsteps,st1,st2,st1prior,stepsnow
  character (len=1500) :: wire1,wire2
  complex :: cwire1(750),cwire2(750),isec,origin

  write(*,'(a)')"Advent of Code 2019 day 3, part 2"
  write(*,'(a)')""
  open(10,file="day3in.txt")
! Two lines to read - one for each wire
  read(10,'(a)',iostat=info) wire1
  read(10,'(a)',iostat=info) wire2
  if (info<0) stop 8
  close(10)
! Split the two wires up into co-ordinates, starting from the origin (0,0)
  call gencoords(wire1,cwire1,ncoords1)
  call gencoords(wire2,cwire2,ncoords2)
! Set the origin point and a large minimum manhattan distance 
! ... and a large mininum steps distance
  origin=cmplx(0,0)
! Set minimum manhattan and steps distances to be huge
  minmd=huge(1)
  minsteps=huge(1)
! loop through each pair of wires to see if they intersect
  st1=0
  do l1=1,ncoords1-1
!   calculate the number of steps taken for the path of wire 1 so far
    st1=st1+abs(int(real(cwire1(l1)))-int(real(cwire1(l1+1)))) &
           +abs(int(aimag(cwire1(l1)))-int(aimag(cwire1(l1+1))))
!   store this value 
    st1prior=st1
    st2=0
    do l2=1,ncoords2-1
      call getintersection(cwire1(l1),cwire1(l1+1),cwire2(l2),cwire2(l2+1),ip,isec)
      if (ip == 0) then
!       No interesection - reset wire 1 back to the prior position and
!       increment wire 2's step distance
        st1=st1prior
        st2=st2+abs(int(real(cwire2(l2)))-int(real(cwire2(l2+1)))) &
               +abs(int(aimag(cwire2(l2)))-int(aimag(cwire2(l2+1))))
      else
!       Intersection - steps on wire 1 is only as far as it
        st1=st1-abs(int(real(isec))-int(real(cwire1(l1+1)))) &
               -abs(int(aimag(isec))-int(aimag(cwire1(l1+1))))
!       Calculate steps on wire 2
        st2=st2+abs(int(real(cwire2(l2)))-int(real(isec))) &
               +abs(int(aimag(cwire2(l2)))-int(aimag(isec)))
!       Add together to get the result
        stepsnow=st1+st2
!       Ignore 0 steps as that's the origin
        if (stepsnow > 0) then
          if (stepsnow < minsteps) minsteps=stepsnow
        end if
      end if
!     ip is set to 1 if there's an intersection point between the two wires
      if (ip == 1) then
        md=manhattan(origin,isec) 
!       Ignore the origin as this is always an intersection!
        if (md > 0) then
          if (md < minmd) minmd=md
          write(*,'(a,2f8.0,a,i5,a,i8)')"intersection at ",isec," distance",md," steps ",stepsnow 
        end if
      end if
    end do
  end do 
  write(*,*)""
  write(*,'(a,i5)')"Part 1: Manhattan distance from central port to nearest intersection is",minmd
  write(*,'(a,i8)')"Part 2: Fewest combined steps to any intersection is",minsteps

end program day3p2

subroutine getintersection(p1,p2,p3,p4,ipoints,intersection)
  implicit none
  complex,intent(in)  :: p1,p2,p3,p4
  complex,intent(out) :: intersection
  integer,intent(out) :: ipoints

  integer :: xi,yi,x1,x2,y1,y2

  ipoints=1
  intersection=(9999999,9999999)

  if (int(real(p1)) == int(real(p2))) then
!   First line is vertical
    if (int(real(p3)) == int(real(p4))) then
!     Second line is vertical
        if (int(real(p1)) == int(real(p3))) then
!         This is the same line ... ignore for this puzzle
          ipoints=0
        else
!         Lines are parallel so don't intersect
          ipoints=0
        end if
     end if
   end if

  if (int(aimag(p1)) == int(aimag(p2))) then
!   First line is horizontal
    if (int(aimag(p3)) == int(aimag(p4))) then
!     Second line is horizontal
        if (int(aimag(p1)) == int(aimag(p3))) then
!         This is the same line ... ignore for this puzzle
          ipoints=0
        else
!         Lines are parallel so don't intersect
          ipoints=0
        end if
     end if
   end if

   if (ipoints == 1) then
!    We have one vertical and one horizontal line
     if (int(real(p1)) == int(real(p2))) then
!      First line is vertical, so second is horizontal
       xi=int(real(p1))
       y1=int(aimag(p1))
       y2=int(aimag(p2))
       yi=int(aimag(p3))
       if (y1<y2) then
         if ((yi >= y1) .and. (yi <= y2)) then
           intersection=cmplx(xi,yi) 
         else
           ipoints=0
         end if
       else
         if ((yi >= y2) .and. (yi <= y1)) then
           intersection=cmplx(xi,yi) 
         else
           ipoints=0
         end if
       end if
!      and now check that any possible intersection is on the x axis too
       if (ipoints /= 0) then
         x1=int(real(p3))
         x2=int(real(p4))
         if (x1<x2) then
           if ((xi >= x1) .and. (xi <=x2)) then
             intersection=cmplx(xi,yi) 
           else
             ipoints=0
           end if
         else
           if ((xi >= x2) .and. (xi <=x1)) then
             intersection=cmplx(xi,yi) 
           else
             ipoints=0
           end if
         end if
       end if
     else
!      First line is horizontal, so second is vertical
       yi=int(aimag(p1))
       x1=int(real(p1))
       x2=int(real(p2))
       xi=int(real(p3))
       if (x1<x2) then
         if ((xi >= x1) .and. (xi <= x2)) then
           intersection=cmplx(xi,yi) 
         else
           ipoints=0
         end if
       else
         if ((xi >= x2) .and. (xi <= x1)) then
           intersection=cmplx(xi,yi) 
         else
           ipoints=0
         end if
       end if
!      and now check that any possible intersection is on the y axis too
       if (ipoints /= 0) then
         y1=int(aimag(p3))
         y2=int(aimag(p4))
         if (y1<y2) then
           if ((yi >= y1) .and. (yi <=y2)) then
             intersection=cmplx(xi,yi) 
           else
             ipoints=0
           end if
         else
           if ((yi >= y2) .and. (yi <=y1)) then
             intersection=cmplx(xi,yi) 
           else
             ipoints=0
           end if
         end if
       end if
     end if
   end if
  
end subroutine getintersection

subroutine gencoords(inwire,cwire,ncoords)
!
! Convert a comma separated string of form R123,L123,U123,D123 etc
! into a set of complex coordinates staring from 0,0
!
  implicit none
  integer,intent(out) :: ncoords
  character (len=1500),intent(in) :: inwire
  complex,intent(out) :: cwire(750)

  character :: direction
  character (len=10) :: sval
  character (len=1500) :: wire
  integer :: val

  wire=inwire
  cwire(1)=cmplx(0,0)
  ncoords=1

  do while ((index(wire,',') /= 0) .or. (wire(1:1) /= ' '))
    ncoords=ncoords+1
    cwire(ncoords)=cwire(ncoords-1)
    direction=wire(1:1)
    if (index(wire,',') /= 0) then
      sval=wire(2:index(wire,',')-1)
    else
!     last direction and value found
      sval=wire(2:index(wire,' ')-1)
    end if
    read(sval,*) val

    select case (direction)
      case ('R')
        cwire(ncoords)=cwire(ncoords)+cmplx(val,0)
      case ('L')
        cwire(ncoords)=cwire(ncoords)-cmplx(val,0)
      case ('U')
        cwire(ncoords)=cwire(ncoords)+cmplx(0,val)
      case ('D')
        cwire(ncoords)=cwire(ncoords)-cmplx(0,val)
      case default
        write(*,*)"Unknown direction ",direction," !!"
        stop 8
    end select

    if (index(wire,',') /= 0) then
      wire=wire(index(wire,',')+1:)
    else
!     just processed the last direction and value
      wire=' '
    end if

  end do

end subroutine gencoords

function manhattan (p1,p2) result(mdist)
  implicit none
  complex :: p1,p2
  integer :: i,j,mdist

  i=iabs(int(real(p1))-int(real(p2)))
  j=iabs(int(aimag(p1))-int(aimag(p2)))
  mdist=i+j

end function manhattan
