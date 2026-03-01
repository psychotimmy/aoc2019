module day6
! Represent the orbits as a binary tree
  type node
    character(len=3) :: name
    character(len=3) :: left
    character(len=3) :: right
    integer :: lnode
    integer :: rnode
  end type node
  type(node) :: treenode(1200)
end module day6

program day6p2
  use day6
  implicit none
  integer :: numnodes,l1,l2,info,depth,total,parentnode,childnode
  integer :: pathlenyou,pathlensan,path1,path2,shortpath
  integer,dimension(1200) :: pathyou,pathsan,pathtest
  character (len=12) :: map
  character (len=3) :: start
  logical :: nodeexists,pathfound

  write(*,'(a)')"Advent of Code 2019 day 6, part 2"
  write(*,'(a)')""

  open(10,file="day6in.txt")
  numnodes=0
  do
    read(10,'(a)',iostat=info) map
    if (info<0) exit
!   Add each node found into a binary tree structure
!   Deal with the parent node, create if it doesn't exist
    nodeexists=.false.
    do l2=1,numnodes
      if (treenode(l2)%name == map(:index(map,')')-1)) then
        nodeexists=.true.
        parentnode=l2
        exit
      end if
    end do
    if (.not.nodeexists) then
      numnodes=numnodes+1
      parentnode=numnodes
      treenode(numnodes)%name=map(:index(map,')')-1)
      treenode(numnodes)%left='---'
      treenode(numnodes)%lnode=0
      treenode(numnodes)%right='---'
      treenode(numnodes)%rnode=0
    end if
!   Deal with the child node, create if it doesn't exist
    nodeexists=.false.
    do l2=1,numnodes
      if (treenode(l2)%name == trim(map(index(map,')')+1:))) then
        nodeexists=.true.
        childnode=l2
        exit
      end if
    end do
    if (.not.nodeexists) then
      numnodes=numnodes+1
      childnode=numnodes
      treenode(numnodes)%name=trim(map(index(map,')')+1:))
      treenode(numnodes)%left='---'
      treenode(numnodes)%lnode=0
      treenode(numnodes)%right='---'
      treenode(numnodes)%rnode=0
    end if
!   Link the parent to the child
    if (treenode(parentnode)%lnode == 0) then
      treenode(parentnode)%left=trim(map(index(map,')')+1:))
      treenode(parentnode)%lnode=childnode
    else if (treenode(parentnode)%rnode == 0) then
      treenode(parentnode)%right=trim(map(index(map,')')+1:))
      treenode(parentnode)%rnode=childnode
    else
      write(*,'(a)')"Error: the input does not create a binary tree!"
      stop 8
    end if
!   write(*,'(a,i5,3a4,2i5)')"Parent node now ",parentnode,treenode(parentnode)
!   write(*,'(a,i5,3a4,2i5)')"Child node now  ",childnode,treenode(childnode)
  end do
  close(10)

! Calculate the number of direct and indirect orbits around COM

  total=0 
  depth=0
  call numorbits('COM',numnodes,depth,total)
  write(*,'(a,i8)')"Number of direct and indirect orbits is",total
  write(*,'(a)')""

! Find the paths from COM to YOU and COM to SAN

  pathlenyou=0
  pathfound=.false.
  start='COM'
  call pathfinder(start,'YOU',pathfound,numnodes,pathyou,pathlenyou)
  pathlensan=0
  pathfound=.false.
  start='COM'
  call pathfinder(start,'SAN',pathfound,numnodes,pathsan,pathlensan)

! Iterate through all of the possible common nodes between YOU and SAN
! We want the shortest pair of paths from the common node to YOU and SAN
! as this will be the number of orbital transfers required.

  shortpath=huge(1)
  do l1=1,pathlenyou
    do l2=1,pathlensan
      if (pathyou(l1) == pathsan(l2)) then
!       We have a common node, find the lengths of each path and add them
        path1=0
        pathfound=.false.
        start=treenode(pathyou(l1))%name
        call pathfinder(start,'YOU',pathfound,numnodes,pathtest,path1)
        path2=0
        pathfound=.false.
        start=treenode(pathyou(l1))%name
        call pathfinder(start,'SAN',pathfound,numnodes,pathtest,path2)
!       Subtract 2 so we're not counting the last element of the paths 
!       to YOU and SAN as per puzzle instructions
        path1=path1+path2-2
        if (path1 < shortpath) shortpath=path1
      end if
    end do
  end do

  write(*,'(a,i4)')"Fewest orbital transfers from YOU to SAN is",shortpath

end program day6p2

recursive subroutine pathfinder(start,fin,pathfound,nnodes,res,pathlen)
  use day6
  implicit none
  character (len=3),intent(inout) :: start
  character (len=3),intent(in) :: fin
  logical,intent(inout) :: pathfound
  integer,intent(in) :: nnodes
  integer,intent(inout) :: res(*)
  integer,intent(inout) :: pathlen
  integer :: thisnode
  logical :: nodefound

  if (.not.pathfound) then

    nodefound=.false.
    do thisnode=1,nnodes
      if (treenode(thisnode)%name == start) then
        nodefound=.true.
        exit
      end if
    end do

    if (.not.nodefound) then
      write(*,'(3a)')"Error: Node ",start," not found in tree"
      stop 8
    end if

    if ((treenode(thisnode)%name == fin) .and. (.not.pathfound)) then
      pathfound=.true.
    else
      pathlen=pathlen+1
      res(pathlen)=thisnode
      if (treenode(thisnode)%lnode /= treenode(thisnode)%rnode) then
!       We're not at a leaf, so try left and right hand nodes
        if (treenode(thisnode)%lnode > 0) then
          call pathfinder(treenode(thisnode)%left,fin,pathfound,nnodes,res,pathlen)
        end if
        if (treenode(thisnode)%rnode > 0) then
          call pathfinder(treenode(thisnode)%right,fin,pathfound,nnodes,res,pathlen)
        end if
      end if
!     If we're recursing upwards and haven't found fin, decrement pathlen
      if (.not.pathfound) pathlen=pathlen-1
    end if
  end if

end subroutine pathfinder

recursive subroutine numorbits(nodename,nnodes,depth,res)
  use day6
  implicit none
  character (len=3),intent(in) :: nodename
  integer,intent(in) :: nnodes
  integer,intent(inout) :: depth
  integer,intent(inout) :: res
  integer :: thisnode
  logical :: nodefound

  nodefound=.false.
  do thisnode=1,nnodes
    if (treenode(thisnode)%name == nodename) then
      nodefound=.true.
      exit
    end if
  end do

  if (.not.nodefound) then
    write(*,'(3a)')"Error: Node ",nodename," not found in tree"
    stop 8
  end if

! As COM orbits nothing we don't increase the depth if this is the node
! we're currently processing

  if (nodename /= 'COM') then
    depth=depth+1
  end if

! write(*,*)"result is currently ",res
! write(*,*)"depth is currently ",depth

  if (treenode(thisnode)%lnode /= treenode(thisnode)%rnode) then
!   We're not at a leaf, so try left and right hand nodes
    if (treenode(thisnode)%lnode > 0) then
      call numorbits(treenode(thisnode)%left,nnodes,depth,res)
    end if
    if (treenode(thisnode)%rnode > 0) then
      call numorbits(treenode(thisnode)%right,nnodes,depth,res)
    end if
  end if
! We're recursing upwards, so increment res by the depth we're at now
! and then decrement the depth
  res=res+depth
  depth=depth-1

end subroutine numorbits
