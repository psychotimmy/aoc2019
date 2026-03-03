program day8p1
  implicit none
  integer :: l1,l2,layerno,minzeros,zeros,ones,twos,total
  character (len=15000) image
  character (len=150) layer

  write(*,'(a)')"Advent of Code 2019 day 8, part 1"

  open(10,file="day8in.txt")
  read(10,'(a)') image
  close(10)

  minzeros=huge(1)

  do l1=1,len(trim(image)),len(layer)
    zeros=0
    ones=0
    twos=0
    layer=image(l1:l1+149)

    do l2=1,150
      select case (layer(l2:l2))
        case ('0') 
          zeros=zeros+1
        case ('1') 
          ones=ones+1
        case ('2') 
          twos=twos+1
        case default
          write(*,'(a,i3)')"Unexpected digit ",layer(l2:l2)," in layer",l1/150+1
      end select
    end do

    if (zeros < minzeros) then
      layerno=l1/150+1
      minzeros=zeros
      total=ones*twos
    end if
   
  end do

  write(*,'(a)')""
  write(*,'(a,i4)')"Layer with the fewest 0s is number",layerno
  write(*,'(a,i4,a,i6)')"1s multiplied by 2s on layer",layerno," is",total

end program day8p1
