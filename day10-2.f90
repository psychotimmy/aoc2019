program day10p2
  implicit none
  integer                      :: info,total,maxtotal,xsize,ysize,x,y,x0,y0
  integer                      :: x200,y200
  integer,dimension(0:30,0:30) :: grid
  character (len=30)           :: line

  write(*,'(a)')"Advent of Code 2019 day 10, part 2"
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
      if (total > maxtotal) then 
        maxtotal=total
!       Store the co-ordinates of the best asteroid
        x0=x
        y0=y
      end if
    end do
  end do

  write(*,'(a)')""
  write(*,'(2(a,i2))')"Co-ordinates of best asteroid is ",x0,",",y0
  write(*,'(a,i3)')"Number of visible asteroids from this asteroid is ",maxtotal

  call zap200(grid,xsize,ysize,x0,y0,x200,y200)

  write(*,'(2(a,i2))')"Co-ordinates of 200th asteroid zapped is ",x200,",",y200
  write(*,'(a,i4)')"Which has a value of ",x200*100+y200

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

  subroutine zap200(grid,xsize,ysize,x0,y0,x200,y200)
    implicit none
    integer,intent(in),dimension(0:,0:)    :: grid
    integer,intent(in)                     :: xsize,ysize,x0,y0
    integer,intent(out)                    :: x200,y200
    integer                                :: numasteroids,xb,yb,l1,target
    real                                   :: r,theta
    real                                   :: epsilon=0.0001
    real                                   :: pi=4.0*atan(1.0)
    complex,dimension(500,2)               :: asteroidlist

!   grid(x0,y0) assumed to be 1 (current asteroid position) for this
!   subroutine to return correct results

    numasteroids=0
    do yb=0,ysize-1
      do xb=0,xsize-1
        if (grid(xb,yb) == 1) then
!         We have an asteroid to record
          if (.not.((x0 == xb) .and. (y0 == yb))) then

!           ... and it's not the current asteroid position
!           Cartesian to polar co-ordinates conversion
!           Calculate the distance from xa,ya to xb,yb

            r=sqrt(real((xb-x0)**2+(yb-y0)**2))

!           Calculate the angle between the asteroid at xa,ya and xb,yb
!           and convert to degrees in range 0 - 360 for sorting ease and
!           zapping ease (need to point the laser vertically along decreasing
!           y to start) and then rotate clockwise (increasing angles)

            theta=(atan2(real(yb-y0),real(xb-x0))*(180.0/pi))+180.0

!           ensure the -x, -y quadrant is the last to be considered by adding
!           360 degrees if the asteroid is in that quadrant. We want the
!           asteroids on the vertical axis (90 degrees) to be in the first
!           quadrant, therefore subtract epsilon to ensure this.

            if (theta < 90.0-epsilon) theta=theta+360

!           Add this asteroid to the list, stored as adjusted polar 
!           co-ordinates and their ORIGINAL cartesian co-ordinates

            numasteroids=numasteroids+1
            asteroidlist(numasteroids,1)=complex(r,theta)
            asteroidlist(numasteroids,2)=complex(xb,yb)
         end if
        end if
      end do
    end do

!   Sort the list of asteroids by distance and then by angle

    call insortreal(asteroidlist,numasteroids)
    call insortimag(asteroidlist,numasteroids)
    
!   Our target is the 200th asteroid to be zapped. As this is fewer
!   than the number of asteroids that are already visible we don't
!   have to go through the list of asteroids multiple times.

    target=0
    theta=0.0
    do l1=1,numasteroids
      if (target == 200) exit
      if (theta /= aimag(asteroidlist(l1,1))) then
        target=target+1
        theta=aimag(asteroidlist(l1,1))
!       write(*,*)"zapped ",asteroidlist(l1,2)
      end if
    end do

    x200=int(real(asteroidlist(l1-1,2)))
    y200=int(aimag(asteroidlist(l1-1,2)))

  end subroutine zap200

  subroutine insortimag(list,num)
    implicit none
    complex,dimension(500,2),intent(inout) :: list
    integer,intent(in)                     :: num
    complex,dimension(2)                   :: temp
    integer                                :: j,k
    do j=2,num
      temp(1)=list(j,1)
      temp(2)=list(j,2)
      k=j-1
      do while (aimag(list(k,1)) > aimag(temp(1)))
        list(k+1,1)=list(k,1)
        list(k+1,2)=list(k,2)
        k=k-1
      end do
      list(k+1,1)=temp(1)
      list(k+1,2)=temp(2)
    end do
  end subroutine insortimag

  subroutine insortreal(list,num)
    implicit none
    complex,dimension(500,2),intent(inout) :: list
    integer,intent(in)                     :: num
    complex,dimension(2)                   :: temp
    integer                                :: j,k
    do j=2,num
      temp(1)=list(j,1)
      temp(2)=list(j,2)
      k=j-1
      do while (real(list(k,1)) > real(temp(1)))
        list(k+1,1)=list(k,1)
        list(k+1,2)=list(k,2)
        k=k-1
      end do
      list(k+1,1)=temp(1)
      list(k+1,2)=temp(2)
    end do
  end subroutine insortreal

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

end program day10p2
