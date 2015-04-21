       module version_mod
       use IO_tools_mod
       implicit none

       contains

       subroutine exportVersion(dir)
         implicit none
         character(len=*),intent(in) :: dir
         integer :: NewU
         NewU = newAndOpen(dir,'version')
         call writeVersionToFileOrScreen(newU)
         call closeAndMessage(newU,'version',dir)
       end subroutine

       subroutine printVersion()
         implicit none
         call writeVersionToFileOrScreen(6)
       end subroutine

       subroutine writeVersionToFileOrScreen(u)
         implicit none
         integer,intent(in) :: u
         integer*4 today(3), now(3)
         write(u,*) '-------------------------------------'
         write(u,*) ' University of California Los Angeles'
         write(u,*) '              (UCLA)                 '
         write(u,*) ' Magnetohydrodynamic Object-Oriented '
         write(u,*) '    Numberical Simulation (MOONS)    '
         write(u,*) '-------------------------------------'
         write(u,*) 'Charliekawczynski@gmail.com          '
         write(u,*) '-------------------------------------'
         write(u,*) 'MOONS is a finite difference code that'
         write(u,*) 'solves Navier-Stokes and Maxwells'
         write(u,*) 'equations in a 3D rectangular geometry.'
         write(u,*) ''
         write(u,*) 'For documentation, navigate to'
         write(u,*) '/MOONS/__documentation/MOONS.pdf'
         write(u,*) '-------------------------------------'

         ! Source: http://infohost.nmt.edu/tcc/help/lang/fortran/date.html

         call idate(today)   ! today(1)=day, (2)=month, (3)=year
         call itime(now)     ! now(1)=hour, (2)=minute, (3)=second
         write ( u, 1000 )  today(2), today(1), today(3), now
    1000 format ( 'Version: ', i2.2, '/', i2.2, '/', i4.4, '; time ',&
                 i2.2, ':', i2.2, ':', i2.2 )

         write(u,*) '-------------------------------------'
       end subroutine

       ! Source: http://stackoverflow.com/questions/8324326/date-format-in-ddmonyy-in-fortran

       SUBROUTINE get_DDMonYY(date)
         CHARACTER(len=7), INTENT(out) :: date
         CHARACTER(len=2) :: dd
         CHARACTER(len=3) :: mons(12)
         CHARACTER(len=4) :: yyyy
         INTEGER :: values(8)
         mons = ['Jan','Feb','Mar','Apr','May','Jun',&
           'Jul','Aug','Sep','Oct','Nov','Dec']
         CALL DATE_AND_TIME(VALUES=values)
         WRITE(  dd,'(i2)') values(3)
         WRITE(yyyy,'(i4)') values(1)
         date = dd//mons(values(2))//yyyy(3:4)
       END SUBROUTINE get_DDMonYY


       end module