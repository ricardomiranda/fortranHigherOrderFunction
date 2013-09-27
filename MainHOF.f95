!------------------------------------------------------------------------------
!        IST/MARETEC, Water Modelling Group, Mohid modelling system
!------------------------------------------------------------------------------
!
! TITLE         : Higher Order Functions
! DATE          : July 2013
! REVISION      : Ricardo Miranda
! DESCRIPTION   : Example program of Functional Core, Imperative LotkaVolterra according to
!                 Gary Bernhardt
!
!------------------------------------------------------------------------------
!
!This program is free software; you can redistribute it and/or
!modify it under the terms of the GNU General Public License
!version 2, as published by the Free Software Foundation.
!
!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!You should have received a copy of the GNU General Public License
!along with this program; if not, write to the Free Software
!Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
!
!------------------------------------------------------------------------------

program MainHOF

    use ModuleHOF

    implicit none

    type T_MainHOF
        real(8) :: DT, NbrSteps
    end type T_MainHOF

    call Main


    !Subroutines---------------------------------------------------------------

    !Constructor
!    public  :: ConstructMainHOF
!    private ::      ASkQuestions
!    private ::      StartHOF
!    private :: AllocateReplica

    !Selector

    !Modifier
!    public  :: Main
!    private ::      Loop
!    private ::      exampleFunction

    !Destructor
!    public  :: KillMainHOF

    !---------------------------------------------------------------------------

    contains

    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONS

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    subroutine ConstructMainHOF (ObjMainHOF)

        !Arguments---------------------------------------------------------------
        type(T_MainHOF), pointer :: ObjMainHOF

        !------------------------------------------------------------------------

        call AllocateInstance (ObjMainHOF)
        call ASkQuestions     (ObjMainHOF)

    end subroutine ConstructMainHOF

    !--------------------------------------------------------------------------

    subroutine ASkQuestions (ObjMainHOF)

        !Arguments---------------------------------------------------------------
        type(T_MainHOF), pointer :: ObjMainHOF

        !Local-------------------------------------------------------------------

        !------------------------------------------------------------------------

        print*, "What is the time step?"
        read*,  ObjMainHOF%DT

        print*, "How many time steps to compute?"
        read*,  ObjMainHOF%NbrSteps

    end subroutine ASkQuestions

    !--------------------------------------------------------------------------

    function StartHOF ()

        !Local-------------------------------------------------------------------
        integer :: IMIN, IMAX, ILB, IUB
        integer :: JMIN, JMAX, JLB, JUB

        !Return----------------------------------------------------------------
        type (T_Arrays), pointer  :: StartHOF

        !Local-----------------------------------------------------------------
        type (T_Arrays), pointer  :: NewObjHDF

        !----------------------------------------------------------------------

        print*, "IMIN?"
        read*,   IMIN

        print*, "IMAX?"
        read*,   IMAX

        print*, "ILB?"
        read*,   ILB

        print*, "IUB?"
        read*,   IUB

        print*, "JMIN?"
        read*,   JMIN

        print*, "JMAX?"
        read*,   JMAX

        print*, "JLB?"
        read*,   JLB

        print*, "JUB?"
        read*,   JUB

        NewObjHDF => ConstructHOF (IMIN, IMAX, ILB, IUB,                       &
                                   JMIN, JMAX, JLB, JUB)

        StartHOF  => NewObjHDF
    end function StartHOF

    !--------------------------------------------------------------------------

    pure subroutine AllocateInstance (ObjMainHOF)
        !Arguments-------------------------------------------------------------
        type(T_MainHOF), pointer :: ObjMainHOF

        !----------------------------------------------------------------------

        allocate (ObjMainHOF)

    end subroutine AllocateInstance

    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODI

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !--------------------------------------------------------------------------

    subroutine Main

        !Local-----------------------------------------------------------------
        type(T_MainHOF), pointer :: ObjMainHOF
        type(T_Arrays ), pointer :: ObjHOF

        !----------------------------------------------------------------------

        call ConstructMainHOF (ObjMainHOF)

        ObjHOF => StartHOF ()
        call Loop          (ObjMainHOF, ObjHOF, ObjMainHOF%NbrSteps)

    end subroutine Main

    !--------------------------------------------------------------------------

    recursive subroutine Loop (ObjMainHOF, ObjHOF, NbrSteps)

        !Arguments-------------------------------------------------------------
        type(T_MainHOF), pointer :: ObjMainHOF
        type(T_Arrays ), pointer :: ObjHOF
        real(8), intent(IN)      :: NbrSteps

        !Local-----------------------------------------------------------------
        type(T_Arrays ), pointer :: NewObjHOF

        !----------------------------------------------------------------------

cd1 :   if (NbrSteps .LE. 0.0) then
            print*, "Simulation terminated successfully."
            call killMainHOF (ObjMainHOF, ObjHOF)

        else   cd1
            NewObjHOF => CalcResult  (ObjHOF)
            call HOFGarbageCollector (ObjHOF)

            print*, NbrSteps, GetArrayRes(NewObjHOF)
            call Loop (ObjMainHOF, NewObjHOF, NbrSteps - ObjMainHOF%DT)
        end if cd1

    end subroutine Loop

    !--------------------------------------------------------------------------


    !--------------------------------------------------------------------------

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    pure subroutine KillMainHOF (ObjMainHOF, ObjHOF)
        !Arguments-------------------------------------------------------------
        type(T_MainHOF), pointer :: ObjMainHOF
        type(T_Arrays ), pointer :: ObjHOF

        !----------------------------------------------------------------------

        call KillHOF (ObjHOF)

        deallocate   (ObjMainHOF)

    end subroutine KillMainHOF

    !--------------------------------------------------------------------------

end program MainHOF
