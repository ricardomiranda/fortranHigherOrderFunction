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

module ModuleFL

    implicit none

    private

    real(8) :: NULL_REAL =-99999.99

    !Subroutines---------------------------------------------------------------

    public  :: CalcArraysOp

    contains

    !---------------------------------------------------------------------------

    function CalcArraysOp(f2A,                                                 &
                               arrayA,                                              &
                               arrayB,                                              &
                               IMIN, IMAX, ILB, IUB,                                &
                               JMIN, JMAX, JLB, JUB)

        !Arguments-------------------------------------------------------------
        integer, intent(IN) :: IMIN, IMAX, ILB, IUB
        integer, intent(IN) :: JMIN, JMAX, JLB, JUB
        real(8), dimension(:, :), pointer :: CalcArraysOp

        interface
            real(8) pure function f2A(A,B)
                real(8), intent(IN) :: A, B
            end function f2A
        end interface

        !Return------------------------------------------------------------------
        real(8), dimension(:, :), pointer :: arrayA, arrayB

        !Local-------------------------------------------------------------------
        real(8), dimension(:, :), pointer :: arrayRes
        integer :: I, J

        !----------------------------------------------------------------------

        allocate (arrayRes(ILB:IUB,                                             &
                           JLB:JUB))
        arrayRes =  NULL_REAL

!$OMP PARALLEL
!$OMP DO
do1 :   DO I = IMIN, IMAX
do2 :   DO J = JMIN, JMAX
            arrayRes(I, J) = f2A(arrayA(I, J), arrayB(I, J))
        ENDDO do2
        ENDDO do1
!$OMP END DO NOWAIT
!$OMP END PARALLEL

        CalcArraysOp => arrayRes

    end function CalcArraysOp

end module ModuleFL
