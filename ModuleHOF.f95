!------------------------------------------------------------------------------
!        IST/MARETEC, Water Modelling Group, Mohid modelling system
!------------------------------------------------------------------------------
!
! TITLE         : Higher Order Functions
! DATE          : July 2013
! REVISION      : Ricardo Miranda
! DESCRIPTION   : Example program of Functional Core, Imperative Shell according to
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
 
Module ModuleHOF

    use ModuleFL

    implicit none

    !Types---------------------------------------------------------------------
    
    public :: T_Arrays
    type      T_Arrays
        private
        real(8), dimension(:, :),  pointer :: arrayRes
        !Computed every time step
        
        real(8), dimension(:, :),  pointer :: arrayA
        real(8), dimension(:, :),  pointer :: arrayB
        
        integer, pointer :: IMIN, IMAX, ILB, IUB
        integer, pointer :: JMIN, JMAX, JLB, JUB
    end type  T_Arrays

    !--------------------------------------------------------------------------

    private 

    integer :: NULL_INT  =-99999
    real(8) :: NULL_REAL =-99999.99

    !Subroutines---------------------------------------------------------------

    !Constructor
    public  :: ConstructHOF
    private ::      AllocateInstance
    private ::      InitializeValues
    private :: AllocateReplica

    !Selector
    public  :: GetArrayA
    public  :: GetArrayB
    public  :: GetArrayRes
    public  :: GetIMIN
    public  :: GetIMAX
    public  :: GetILB
    public  :: GetIUB
    public  :: GetJMIN
    public  :: GetJMAX
    public  :: GetJLB
    public  :: GetJUB
                         
    !Modifier
    public  :: CalcResult

    !Destructor
    public  :: KillHOF                                                    
    private ::      DeAllocateInstance
    public  :: HOFGarbageCollector
    
    !Interfaces----------------------------------------------------------------

    !--------------------------------------------------------------------------
    
    contains


    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONSTRUCTOR CONS

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    function ConstructHOF(IMIN, IMAX, ILB, IUB,                           &
                          JMIN, JMAX, JLB, JUB) 

        !Arguments---------------------------------------------------------------
        integer, intent(IN) :: IMIN, IMAX, ILB, IUB
        integer, intent(IN) :: JMIN, JMAX, JLB, JUB
        
        !Return------------------------------------------------------------------
        type (T_Arrays), pointer :: ConstructHOF

        !Local-------------------------------------------------------------------
        type (T_Arrays), pointer :: NewObjHOF

        !------------------------------------------------------------------------

        call AllocateInstance(NewObjHOF,                                        &
                              ILB, IUB,                                         &
                              JLB, JUB)
                          
        call InitializeValues(NewObjHOF,                                        &
                              IMIN, IMAX, ILB, IUB,                             &
                              JMIN, JMAX, JLB, JUB)
                         
        ConstructHOF => NewObjHOF

        !----------------------------------------------------------------------

    end function ConstructHOF
 
    !--------------------------------------------------------------------------
     
    subroutine AllocateInstance(NewObjHOF,                                    &
                                ILB, IUB,                                     &
                                JLB, JUB) 

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: NewObjHOF

        integer, intent(IN) :: ILB, IUB
        integer, intent(IN) :: JLB, JUB
                                                    
        !Local-----------------------------------------------------------------

        !Allocates new instance
        allocate (NewObjHOF)
        allocate (NewObjHOF%arrayA  (ILB:IUB,                                   &
                                     JLB:JUB))

        allocate (NewObjHOF%arrayB  (ILB:IUB,                                   &
                                     JLB:JUB))
        
        allocate (NewObjHOF%arrayRes(ILB:IUB,                                   &
                                     JLB:JUB))
        
        allocate (NewObjHOF%IMIN)
        allocate (NewObjHOF%IMAX)
        allocate (NewObjHOF%ILB )
        allocate (NewObjHOF%IUB )
        allocate (NewObjHOF%JMIN)
        allocate (NewObjHOF%JMAX)
        allocate (NewObjHOF%JLB )
        allocate (NewObjHOF%JUB )

    end subroutine AllocateInstance
 
    !--------------------------------------------------------------------------
    
    subroutine InitializeValues(NewObjHOF,                                    &
                                IMIN, IMAX, ILB, IUB,                         &
                                JMIN, JMAX, JLB, JUB) 

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: NewObjHOF

        integer, intent(IN) :: IMIN, IMAX, ILB, IUB
        integer, intent(IN) :: JMIN, JMAX, JLB, JUB
                                                    
        !Local-----------------------------------------------------------------
        integer :: I, J
        
        !----------------------------------------------------------------------

        NewObjHOF%arrayA   = NULL_REAL
        NewObjHOF%arrayB   = NULL_REAL
        NewObjHOF%arrayRes = NULL_REAL

do1:    DO J = JMIN, JMAX
do2:    DO I = IMIN, IMAX
            NewObjHOF%arrayA(I, J) = I * 2.1
            NewObjHOF%arrayB(I, J) = J * 3.4
        ENDDO do2
        ENDDO do1

        NewObjHOF%IMIN = IMIN
        NewObjHOF%IMAX = IMAX
        NewObjHOF%ILB  = ILB
        NewObjHOF%IUB  = IUB
        NewObjHOF%JMIN = JMIN
        NewObjHOF%JMAX = JMAX
        NewObjHOF%JLB  = JLB
        NewObjHOF%JUB  = JUB

    end subroutine InitializeValues

    !--------------------------------------------------------------------------
    
    function AllocateReplica(ObjHOF) 

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer                               :: ObjHOF
                                                    
        !Return----------------------------------------------------------------
        type (T_Arrays), pointer                               :: AllocateReplica

        !Local-----------------------------------------------------------------
        type (T_Arrays), pointer                               :: NewObjHOF

        !-----------------------------------------------------------------------
        
        !Allocates new values
        allocate (NewObjHOF)

        NewObjHOF%arrayA => ObjHOF%arrayA
        NewObjHOF%arrayB => ObjHOF%arrayB
        NewObjHOF%IMIN   => ObjHOF%IMIN
        NewObjHOF%IMAX   => ObjHOF%IMAX
        NewObjHOF%ILB    => ObjHOF%ILB
        NewObjHOF%IUB    => ObjHOF%IUB
        NewObjHOF%JMIN   => ObjHOF%JMIN
        NewObjHOF%JMAX   => ObjHOF%JMAX
        NewObjHOF%JLB    => ObjHOF%JLB
        NewObjHOF%JUB    => ObjHOF%JUB

        AllocateReplica  => NewObjHOF

    end function AllocateReplica

    !--------------------------------------------------------------------------


    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SELECTOR SE

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    

    !--------------------------------------------------------------------------
    
    function GetArrayRes (ObjHOF)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF

        !Return----------------------------------------------------------------
       real(8), dimension(:, :),  pointer :: GetArrayRes

        !----------------------------------------------------------------------

        GetArrayRes => ObjHOF%arrayRes

    end function GetArrayRes
    
    !--------------------------------------------------------------------------
    
    function GetArrayA (ObjHOF)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF

        !Return----------------------------------------------------------------
       real(8), dimension(:, :),  pointer :: GetArrayA

        !----------------------------------------------------------------------

        GetArrayA => ObjHOF%arrayA

    end function GetArrayA
    
    !--------------------------------------------------------------------------
    
    function GetArrayB (ObjHOF)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF

        !Return----------------------------------------------------------------
        real(8), dimension(:, :),  pointer :: GetArrayB

        !----------------------------------------------------------------------

        GetArrayB => ObjHOF%arrayB

    end function GetArrayB
    
    !--------------------------------------------------------------------------
    
    function GetIMAX (ObjHOF)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF

        !Return----------------------------------------------------------------
        integer :: GetIMAX

        !----------------------------------------------------------------------

        GetIMAX = ObjHOF%IMAX

    end function GetIMAX
    
    !--------------------------------------------------------------------------
    
    function GetIMIN (ObjHOF)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF

        !Return----------------------------------------------------------------
        integer :: GetIMIN

        !----------------------------------------------------------------------

        GetIMIN = ObjHOF%IMIN

    end function GetIMIN
    
    !--------------------------------------------------------------------------
    
    function GetILB (ObjHOF)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF

        !Return----------------------------------------------------------------
        integer :: GetILB

        !----------------------------------------------------------------------

        GetILB = ObjHOF%ILB

    end function GetILB
    
    !--------------------------------------------------------------------------
    
    function GetIUB (ObjHOF)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF

        !Return----------------------------------------------------------------
        integer :: GetIUB

        !----------------------------------------------------------------------

        GetIUB = ObjHOF%IUB

    end function GetIUB
    
    !--------------------------------------------------------------------------
    
    function GetJMIN (ObjHOF)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF

        !Return----------------------------------------------------------------
        integer :: GetJMIN

        !----------------------------------------------------------------------

        GetJMIN = ObjHOF%JMIN

    end function GetJMIN
    
    !--------------------------------------------------------------------------
    
    function GetJMAX (ObjHOF)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF

        !Return----------------------------------------------------------------
        integer :: GetJMAX

        !----------------------------------------------------------------------

        GetJMAX = ObjHOF%JMAX

    end function GetJMAX
    
    !--------------------------------------------------------------------------
    
    function GetJLB (ObjHOF)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF

        !Return----------------------------------------------------------------
        integer :: GetJLB

        !----------------------------------------------------------------------

        GetJLB = ObjHOF%JLB

    end function GetJLB
    
    !--------------------------------------------------------------------------
    
    function GetJUB (ObjHOF)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF

        !Return----------------------------------------------------------------
        integer :: GetJUB

        !----------------------------------------------------------------------

        GetJUB = ObjHOF%JUB

    end function GetJUB

    !--------------------------------------------------------------------------
    
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODIFIER MODI

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    function CalcResult(ObjHOF)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF

        !Return------------------------------------------------------------------
        type (T_Arrays), pointer :: CalcResult

        !Local-------------------------------------------------------------------
        type (T_Arrays), pointer :: NewObjHOF

        !----------------------------------------------------------------------

        NewObjHOF          => AllocateReplica(ObjHOF)
        
        NewObjHOF%arrayRes => CalcArraysOp   (exampleFunction,                 &
                                              ObjHOF%arrayA,                   &
                                              ObjHOF%arrayB,                   &
                                              ObjHOF%IMIN, ObjHOF%IMAX,        &
                                              ObjHOF%ILB, ObjHOF%IUB,          &
                                              ObjHOF%JMIN, ObjHOF%JMAX,        &
                                              ObjHOF%JLB, ObjHOF%JUB)
        
        CalcResult         => NewObjHOF

    end function CalcResult

    !--------------------------------------------------------------------------

    pure function exampleFunction (A, B)

        !Arguments-------------------------------------------------------------         
        real(8), intent(IN) :: A, B

        !Local-----------------------------------------------------------------

        !Return----------------------------------------------------------------
        real(8) :: exampleFunction
                                                    
        !----------------------------------------------------------------------

        exampleFunction = A * B + (A + B * SQRT(B)) / 2.2 + A * A - B * B * A * 1.1 - 2.0 / (A*B) + 3*(sqrt(a)/sqrt(b)) 
        
    end function exampleFunction

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    !DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR DESTRUCTOR

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    subroutine KillHOF(ObjHOF)

        !Arguments---------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF
        
        !Return------------------------------------------------------------------         

        !External----------------------------------------------------------------

        !Local-------------------------------------------------------------------

        !------------------------------------------------------------------------

        !Deallocates Instance
        call DeallocateInstance (ObjHOF)

        !------------------------------------------------------------------------

    end subroutine KillHOF
        

    !------------------------------------------------------------------------
    
    
    subroutine DeallocateInstance (ObjHOF)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF
                                                    
        !Local-----------------------------------------------------------------

        !------------------------------------------------------------------------

        !Deallocates instance
        deallocate (ObjHOF%IMIN)
        deallocate (ObjHOF%IMAX)
        deallocate (ObjHOF%ILB )
        deallocate (ObjHOF%IUB )
        deallocate (ObjHOF%JMIN)
        deallocate (ObjHOF%JMAX)
        deallocate (ObjHOF%JLB )
        deallocate (ObjHOF%JUB )
        
        deallocate (ObjHOF%arrayRes)
        deallocate (ObjHOF%arrayA)
        deallocate (ObjHOF%arrayB)
        deallocate (ObjHOF)
            
    end subroutine DeallocateInstance

    !--------------------------------------------------------------------------

    subroutine HOFGarbageCollector (ObjHOF)

        !Arguments-------------------------------------------------------------
        type (T_Arrays), pointer :: ObjHOF
                                                    
        !Local-----------------------------------------------------------------

        !------------------------------------------------------------------------

        !Partially deallocates instance
        deallocate (ObjHOF%arrayRes)
        deallocate (ObjHOF)
            
        !------------------------------------------------------------------------

    end subroutine HOFGarbageCollector

    !--------------------------------------------------------------------------

end module ModuleHOF








