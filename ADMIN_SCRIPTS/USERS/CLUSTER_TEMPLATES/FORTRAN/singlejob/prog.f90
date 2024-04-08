program example
implicit none
real*8       :: I,J
character*8  :: date_initial,date_final
character*10 :: time_initial,time_final 

I=10.0D0
J=23.5D0

open(1,file='output',status='unknown')
call DATE_AND_TIME(date_initial,time_initial)
write(1,*) "Initial date: ",date_initial
write(1,*) "Initial time: ",time_initial
write(1,*) I+J
write(1,*)
end

