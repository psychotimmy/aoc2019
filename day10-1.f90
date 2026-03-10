program day10p1
  implicit none
  integer                      :: info,total,maxtotal,xsize,ysize,x,y
  integer,dimension(0:30,0:30) :: grid
  character (len=30)           :: line

  write(*,'(a)')"Advent of Code 2019 day 10, part 1"
  ysize=0
  open(10,file="day10in.txt")
  do
    read(10,'(a)',iostat=info) line
    if (info<0) exit
    xsize=len(trim(line))
    call addtogrid(line,xsize,ysize,grid)
    ysize=ysize+1
  end do
  close(10)

  maxtotal=0
  total=0

  do y=0,ysize-1
    do x=0,xsize-1
      if (grid(x,y) == 1) then 
        total=countvisible(grid,xsize,ysize,x,y)
      end if
      if (total > maxtotal) maxtotal=total
    end do
  end do

  write(*,'(a)')""
  write(*,'(a,i3)')"Most visible asteroids from any asteroid is ",maxtotal

contains 

  subroutine addtogrid(line,xsize,ysize,grid)
    implicit none
    character (len=30),intent(in) :: line
    integer,intent(in) :: xsize,ysize
    integer,intent(out),dimension(0:30,0:30) :: grid
    integer :: l1

    do l1=0,xsize-1
      if (line(l1+1:l1+1) == '.') then
        grid(l1,ysize)=0
      else
        grid(l1,ysize)=1
      end if
    end do
  end subroutine addtogrid

  integer function countvisible(grid,xsize,ysize,xa,ya) result(result)
    implicit none
    integer,intent(in),dimension(0:,0:)    :: grid
    integer,intent(in)                     :: xsize,ysize,xa,ya
    integer                                :: xb,yb,l1,numasteroids
    real                                   :: theta
    real                                   :: epsilon=0.0001
    logical                                :: unique
    real,dimension(500)                    :: asteroidanglelist

!   grid(xa,ya) assumed to be 1 (current asteroid position) for this
!   function to return correct results

    numasteroids=0
    do yb=0,ysize-1
      do xb=0,xsize-1
        if (grid(xb,yb) == 1) then
!         We have an asteroid to check
          if (.not.((xa == xb) .and. (ya == yb))) then
!           ... and it's not the current asteroid position
!           Calculate the angle between the asteroid at xa,ya and xb,yb
!           (cartesian to polar co-ordinates, but we just need the 
!            angle and we only want to count unique angles)
            theta=atan2(real(yb-ya),real(xb-xa))
!           Only keep one asteroid per angle found - use a suitable epsilon
!           value as we're using real numbers
            unique = .true.
            do l1=1,numasteroids
              if ((asteroidanglelist(l1) >= theta-epsilon) .and. &
                  (asteroidanglelist(l1) <= theta+epsilon)) unique=.false.
            end do
            if (unique) then
              numasteroids=numasteroids+1
              asteroidanglelist(numasteroids)=theta
            end if
         end if
        end if
      end do
    end do

    result=numasteroids

  end function countvisible

end program day10p1
