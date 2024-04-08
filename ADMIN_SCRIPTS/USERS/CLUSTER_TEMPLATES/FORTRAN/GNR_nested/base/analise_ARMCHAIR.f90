!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!   PROGRAMA DE ANÁLISE DE MATERIAIS 2D
!!                                
!!      VERSAO DE DESENVOLVIMENTO 05-2017
!!
!!                   BY: PEDRO HENRIQUE 
!!                       WILIAM FERREIRA
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE GLOBAIS


!   PHYSICAL PARAMETERS
PARAMETER M_S=70     ! LINES
PARAMETER N_S=6    ! SITES IN EACH LINE
PARAMETER P_S  =1      ! TOTAL TIME STEPS
PARAMETER P_SI =1      ! THE INITIAL TIME STEP
PARAMETER P_SS =1      ! TIME STEP SIZE

! PARAMETER TO DIFINE MESH PRINT: M_IN 0% STAR IN THE BEGIN
!                                 M_FN 100% FINISH IN THE END
PARAMETER M_IN=0
PARAMETER M_FN=100

! MESH DENSITY
PARAMETER L_MH=100
! PARAMETER TO TRANSLATE MESH IN %
PARAMETER DMP=0

!!!!!!!! LOCAL PARAMETERS AND VARIABLES !!!!!!!!!!!!!!!!!!!!!!!!!!
PARAMETER L=N_S*M_S
INTEGER :: M,N,P
REAL( KIND = 8),ALLOCATABLE :: Yn(:),AUX(:,:),AUX2(:,:),AUX3(:,:)
!!!!!!!!!!!!   MALHA 1  !!!!!!!!!!!!!
REAL( KIND = 8 ),ALLOCATABLE :: XX(:)
REAL( KIND = 8 ),ALLOCATABLE :: YY(:)
!   PARAMETROS DA PRIMEIRA MALHA 
INTEGER :: NX
INTEGER :: NY
!!!!!!!!!!!!  MALHA 2   !!!!!!!!!!!!!!!
REAL( KIND = 8 ),ALLOCATABLE :: X(:)
REAL( KIND = 8 ),ALLOCATABLE :: Y(:)
INTEGER( KIND = 4 ),ALLOCATABLE :: IX(:)
INTEGER( KIND = 4 ),ALLOCATABLE :: IY(:)
INTEGER( KIND = 4 ),ALLOCATABLE :: IJK(:,:)
!!!!!!!!!!!!! MALHA 3   !!!!!!!!!!!!!
REAL( KIND = 8),ALLOCATABLE :: HEX_X(:,:)
REAL( KIND = 8),ALLOCATABLE :: HEX_Y(:,:)
INTEGER,ALLOCATABLE :: CONX_X(:,:)
INTEGER,ALLOCATABLE :: CONX_Y(:,:)
!   PARAMETROS DA MALHA 3
INTEGER :: NH,IHEX_R,IHEX_Q
!!!!!!!!!!!!! MALHA 4   !!!!!!!!!!!!!
INTEGER( KIND = 4 ),ALLOCATABLE :: IYB(:)
INTEGER( KIND = 4 ),ALLOCATABLE :: IZB(:)
INTEGER( KIND = 4 ),ALLOCATABLE :: IWB(:)
REAL( KIND = 8),ALLOCATABLE :: XBON_Y(:)
REAL( KIND = 8),ALLOCATABLE :: YBON_Y(:)
REAL( KIND = 8),ALLOCATABLE :: XBON_Z(:)
REAL( KIND = 8),ALLOCATABLE :: YBON_Z(:)
REAL( KIND = 8),ALLOCATABLE :: XBON_W(:)
REAL( KIND = 8),ALLOCATABLE :: YBON_W(:)
CHARACTER(len=100) :: GNPHEAD(100)
INTEGER :: IH_DC_2D(2),IH_DC_3D(2)
INTEGER :: IH_PO_2D(2),IH_PO_3D(2)

END MODULE GLOBAIS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM MAIN

CALL LOAD_DATA
CALL MAKE_MESHES
CALL MAKE_GNPHEAD
CALL GRAFICOS_CARGA(1,0,0)
CALL GRAFICOS_BOND(1,0,0)

WRITE(*,*)"Finish"

END PROGRAM MAIN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE MAKE_GNPHEAD
USE GLOBAIS
IMPLICIT NONE
INTEGER :: I

GNPHEAD=''
GNPHEAD(1)='#!/usr/bin/gnuplot'
GNPHEAD(2)='set terminal pngcairo fontscale 1.0 size 1000,1000'
GNPHEAD(3)='set out "DC-3D.png"'
GNPHEAD(4)='set view equal xy'
GNPHEAD(5)='#set view 34 , 26'
GNPHEAD(6)='#set zrange [0:2.0]'
GNPHEAD(7)='#set palette defined (1 "#0000C8", 2 "#00FFFF", 3 "#00C800", 4 "#FFFF00", 5 "#C80000")'
GNPHEAD(8)='#set cbrange [0.0:0.05]'
GNPHEAD(9)='#unset border'
GNPHEAD(10)='#unset ztics'
GNPHEAD(11)='#unset ytics'
GNPHEAD(12)='#unset xtics'
GNPHEAD(13)='#unset colorbox'
GNPHEAD(14)='splot "baseDC.dat" with pm3d at s notitle'

GNPHEAD(21)='#!/usr/bin/gnuplot'
GNPHEAD(22)='set terminal pngcairo fontscale 1.0 size 1000,1000'
GNPHEAD(23)='set out "DC-2D.png"'
GNPHEAD(24)='set pm3d map'
GNPHEAD(25)='set size ratio -1'
GNPHEAD(26)='#set zrange [-0.05:0.3]'
GNPHEAD(27)='#set palette defined (1 "#0000C8", 2 "#00FFFF", 3 "#00C800", 4 "#FFFF00", 5 "#C80000")'
GNPHEAD(28)='#set cbrange [0.0:0.06]'
GNPHEAD(29)='#set yrange [140:170]'
GNPHEAD(30)='#unset ztics'
GNPHEAD(31)='#unset ytics'
GNPHEAD(32)='#unset xtics'
GNPHEAD(33)='#unset colorbox'
GNPHEAD(34)='splot "baseDC.dat" using 1:2:3 with pm3d notitle'

IH_DC_3D(1)=1
IH_DC_3D(2)=14
IH_DC_2D(1)=21
IH_DC_2D(2)=34

do i=1,IH_DC_2D(2)
  GNPHEAD(i+50)=GNPHEAD(i)
enddo

GNPHEAD(53)='set out "PO-3D.png"'
GNPHEAD(57)='set palette defined (1 "#0000C8", 2 "#00FFFF", 3 "#00C800", 4 "#FFFF00", 5 "#C80000")'
GNPHEAD(64)='splot "basePO.dat" with pm3d at s notitle'
GNPHEAD(73)='set out "PO-2D.png"'
GNPHEAD(76)='#set zrange [-0.1:0.5]' 
GNPHEAD(77)='#set palette defined (1 "#0000C8", 2 "#00FFFF", 3 "#00C800", 4 "#FFFF00", 5 "#C80000")'
GNPHEAD(78)='#set cbrange [-0.05:0.3]' 
GNPHEAD(84)='splot "basePO.dat" using 1:2:3 with pm3d notitle'

IH_PO_3D(1)=51
IH_PO_3D(2)=64
IH_PO_2D(1)=71
IH_PO_2D(2)=84

! SEE TO FIND COLORS
! http://www.rapidtables.com/web/color/RGB_Color.htm
RETURN
END SUBROUTINE MAKE_GNPHEAD
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE LOAD_DATA
USE GLOBAIS
IMPLICIT NONE
 
 M=M_S
 N=N_S
 P=P_S

ALLOCATE(X(L),IX(L))
ALLOCATE(Y(L),IY(L))
ALLOCATE(IJK(L,L))

!    N EVEN
IF(N-2*(N/2) .EQ. 0)THEN
  ALLOCATE(IYB(N/2-(N-2):(N-1)*M),XBON_Y((N-1)*M),YBON_Y((N-1)*M))
  ALLOCATE(IZB((N-1)*M),XBON_Z((N-1)*M),YBON_Z((N-1)*M))
  ALLOCATE(IWB(N*M/2),XBON_W(1+N*M/2),YBON_W(1+N*M/2))
ENDIF
!    N ODD
IF(N-2*(N/2) .EQ. 1)THEN
  write(*,*)"verify N --- not done for odd indexes"
  STOP
ENDIF

X=0.0D0
Y=0.0D0
IX=0
IY=0
IYB=0
IZB=0
IWB=0

XBON_Y=0.0D0
YBON_Y=0.0D0
XBON_Z=0.0D0
YBON_Z=0.0D0
XBON_W=0.0D0
YBON_W=0.0D0

! NUMBER OF ROWS AND LINES
IHEX_R=M-1
IHEX_Q=N/2-1

! NUMBER OF HEXAGONS
NH=IHEX_R*IHEX_Q
ALLOCATE(HEX_X(NH,6),HEX_Y(NH,6))
ALLOCATE(CONX_X(NH,6),CONX_Y(NH,6))

ALLOCATE(Yn(L),AUX(P,L),AUX2(P,L),AUX3(P,L))

AUX =0.0D0
AUX2=0.0D0
AUX3=0.0D0

WRITE(*,*)'VECTORS LOADED'

RETURN
END SUBROUTINE LOAD_DATA
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE GRAFICOS_BOND(i2d,i3d,igif)
USE GLOBAIS
IMPLICIT NONE
INTEGER :: I,J,K,NYI,NYF,TMP(8),i2d,i3d,igif
REAL(KIND=8) :: F,DX,CS,PI
REAL(KIND=8),ALLOCATABLE :: Yn2(:)
CHARACTER(len=12) :: NOME

if(i2d.eq.0 .and. i3d.eq.0 .and. igif.eq.0 ) goto 2000

CALL LEITURA_F("fort.21",((N+1)/2)*M)
AUX2=AUX
CALL LEITURA_F("fort.31",(N*M/2))
AUX3=AUX
CALL LEITURA_F("fort.11",((N+1)/2)*M)

NYI=INT( DFLOAT(NY)*(DFLOAT(M_IN)/100.0D0) + 1.0D0 )
NYF=INT( DFLOAT(NY)*(DFLOAT(M_FN)/100.0D0) )

DX=0.0D0
OPEN(1,FILE='basePO.dat')
 DO K=P_SI,P_S,P_SS
   DO J=NYI,NYF
     DO I=1,NX
       CALL GAUSS_BOND(XX(I),YY(J),1.0D0,F,K)
       WRITE(1,*)XX(I)+DX,YY(J),F
     ENDDO
       WRITE(1,*)
   ENDDO
   WRITE(1,*)
   WRITE(1,*)
   write(*,*)k
   DX=DX+2.0D0+0.705D0*DFLOAT(N_S-1)*DSQRT(3.0D0)
 ENDDO
CLOSE(1)

IF(I3D.EQ.1)THEN 
 OPEN(1,FILE='PO-3D.gpl')
 do i=IH_PO_3D(1),IH_PO_3D(2)
    write(1,010)TRIM(GNPHEAD(i))
 enddo
 CALL SYSTEM('gnuplot PO-3D.gpl')
 write(*,*)'3d PO graphs done'
ENDIF

IF(I2D.EQ.1)THEN 
 OPEN(1,FILE='PO-2D.gpl')
 do i=IH_PO_2D(1),IH_PO_2D(2)
    write(1,010)TRIM(GNPHEAD(i))
 enddo
 CALL SYSTEM('gnuplot PO-2D.gpl')
 write(*,*)'2d PO graphs done'
ENDIF

IF(IGIF.EQ.1)THEN 
 !
 DO K=P_SI,P_S,P_SS
  WRITE(NOME,001)K
  WRITE(GNPHEAD(IH_PO_2D(1)+2),102)K
  OPEN(1,FILE=NOME)
   do i=IH_PO_2D(1),IH_PO_2D(2)-1
      write(1,010)TRIM(GNPHEAD(i))
   enddo
   write(1,010)'splot "-" using 1:2:3 with pm3d notitle'
   DO J=NYI,NYF
     DO I=1,NX
       CALL GAUSS_BOND(XX(I),YY(J),1.0D0,F,K)
       WRITE(1,*)XX(I),YY(J),F
     ENDDO
       WRITE(1,*)
   ENDDO
   WRITE(1,*)
   WRITE(1,*)
 ENDDO
 CALL SYSTEM('gnuplot PO2D*.fgif')
 CALL SYSTEM('convert -delay 20 -loop 0 time_*.png PO.gif')
 CALL SYSTEM('mkdir -p 2dfiles; mv PO2D*.fgif 2dfiles ; rm time_*.png')
 write(*,*)'gif PO 2d done'
 close(1)
ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
WRITE(*,*)"BOND PARAMETER - DONE"

100 format(100(e16.10,1x))
102 format('set out "time_',i0.4'.png"')
001 format('PO2D',i3.3,'.fgif')
010 format(A)

2000 RETURN
END SUBROUTINE GRAFICOS_BOND
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE GRAFICOS_CARGA(i2d,i3d,igif)
USE GLOBAIS
IMPLICIT NONE
INTEGER :: I2D,I3D,IGIF
INTEGER :: I,J,NYI,NYF,K
REAL (KIND=8) :: F,DX
CHARACTER(len=12) :: NOME

if(i2d.eq.0 .and. i3d.eq.0 .and. igif.eq.0 ) goto 1000

AUX= 0.0D0
AUX2=0.0D0
CALL LEITURA_F("fort.15",L)

NYI=INT( DFLOAT(NY)*(DFLOAT(M_IN)/100.0D0) + 1.0D0 )
NYF=INT( DFLOAT(NY)*(DFLOAT(M_FN)/100.0D0) )

DX=0.0D0
OPEN(1,FILE='baseDC.dat')
 DO K=P_SI,P_S,P_SS
   DO J=NYI,NYF
     DO I=1,NX
       CALL GAUSS(XX(I),YY(J),1.0D0,F,K)
       WRITE(1,*)XX(I)+DX,YY(J),F
     ENDDO
       WRITE(1,*)
   ENDDO
   WRITE(1,*)
   WRITE(1,*)
   write(*,*)k
   DX=DX+2.0D0+0.705D0*DFLOAT(N_S-1)*DSQRT(3.0D0)
 ENDDO
CLOSE(1)

IF(I3D.EQ.1)THEN 
 OPEN(1,FILE='DC-3D.gpl')
 do i=IH_DC_3D(1),IH_DC_3D(2)
    write(1,010)TRIM(GNPHEAD(i))
 enddo
 CALL SYSTEM('gnuplot DC-3D.gpl')
 write(*,*)'3d DC graphs done'
ENDIF

IF(I2D.EQ.1)THEN 
 OPEN(1,FILE='DC-2D.gpl')
 do i=IH_DC_2D(1),IH_DC_2D(2)
    write(1,010)TRIM(GNPHEAD(i))
 enddo
 CALL SYSTEM('gnuplot DC-2D.gpl')
 write(*,*)'2d DC graphs done'
ENDIF

IF(IGIF.EQ.1)THEN 
 !
 DO K=P_SI,P_S,P_SS
  WRITE(NOME,001)K
  WRITE(GNPHEAD(IH_DC_2D(1)+2),102)K
  OPEN(1,FILE=NOME)
   do i=IH_DC_2D(1),IH_DC_2D(2)-1
      write(1,010)TRIM(GNPHEAD(i))
   enddo
   write(1,010)'splot "-" using 1:2:3 with pm3d notitle'
   DO J=NYI,NYF
     DO I=1,NX
       CALL GAUSS(XX(I),YY(J),1.0D0,F,K)
       WRITE(1,*)XX(I),YY(J),F
     ENDDO
       WRITE(1,*)
   ENDDO
   WRITE(1,*)
   WRITE(1,*)
 ENDDO
 CALL SYSTEM('gnuplot DC2D*.fgif')
 CALL SYSTEM('convert -delay 20 -loop 0 time_*.png DC.gif')
 CALL SYSTEM('mkdir -p 2dfiles; mv DC2D*.fgif 2dfiles ; rm time_*.png')
 write(*,*)'gif DC 2d done'
 close(1)
ENDIF
WRITE(*,*)"CHARGE DENSITY - DONE"

100 format(100(e16.10,1x))
102 format('set out "time_',i0.4'.png"')
001 format('DC2D',i3.3,'.fgif')
010 format(A)

1000 RETURN
END SUBROUTINE GRAFICOS_CARGA
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE MAKE_MESHES
USE GLOBAIS
IMPLICIT NONE
INTEGER :: I,J,K,ICENT,LIM
REAL,DIMENSION(6) :: COORDX, COORDY
REAL,DIMENSION(2) :: CENTER
REAL(KIND=8) :: SIZE,HEIGHT,VERT,WIDTH,HORIZ
REAL( KIND =8 ) :: SN,L_X,L_Y
REAL( KIND =8 ) :: A,B,YI,YF
REAL( KIND =8 ) :: DX,DY

! INDICES DO X E Y
J=1
DO I=N,L+N-1
  IX(J)=I-N*(I/N)
  J=J+1
ENDDO

K=1
DO I=1,M
   DO J=1,N
     IY(K)=3*(I-1)+(1-((-1)**N)*(-1)**(I*(1-N+2*(N/2)))*(-1)**K)/2
      K=K+1
   ENDDO
ENDDO

! INDICE PARA X E Y.
K=1
DO J=1,M
  DO I=1,N
   IJK(I,J)=K
   K=K+1
  ENDDO
ENDDO

! BOND INDEXES FOR READING DATA
! Y Z AND W INDEXES EVEN
IF(N-2*(N/2) .EQ. 0)THEN
 K=0
 I=N/2-(N-2)
 LOOP_A : DO
    DO J=1,N-1
      IYB(I)=J+(N/2-(N-1))+K
      I=I+1
      IF(I .GT. (N-1)*M)EXIT LOOP_A
    ENDDO
     K=K+N
 ENDDO LOOP_A
! Z INDEX
 K=0
 I=1
 LOOP_C : DO
   DO J=2,N
      IZB(I)=J+K
      I=I+1
       IF(I .GT. (N-1)*M)EXIT LOOP_C
   ENDDO
   K=K+N
 ENDDO LOOP_C
 ! W INDEX
 DO I=1,N*M/2
    IWB(I)=I
 ENDDO
ENDIF

! Y AND Z INDEXES ODD
IF(N-2*(N/2) .EQ. 1)THEN
  WRITE(*,*)"verify N --- not done for odd indexes yet"
  STOP
ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! SIZE AND SPACING
SIZE=1.0D0
HEIGHT = 2.0D0*SIZE
VERT=HEIGHT*3.0D0/4.0D0
WIDTH=HEIGHT*DSQRT(3.0D0)/2.0D0
HORIZ=WIDTH
!!!!!!!!!!!!!!!!!!!!!!!!
SN=0.5D0*DSQRT(3.0D0)
L_X=0.50D0*WIDTH
L_Y=0.25D0*HEIGHT

! FAZ X E Y POSSÍVEIS DO SISTEMA FÍSICO -> MALHA 2
DO I=1,L
  X(I)=L_X*DFLOAT(IX(I))+1.0D0
ENDDO

DO I=1,L
  Y(I)=L_Y*DFLOAT(IY(I))
ENDDO
! FIM DOS INDICES

!!!!!!!!!!!! MALHA 1   !!!!!!!!!!!!!!!!

NX=L_MH
NY=NX*INT(Y(L)-Y(1))/INT(X(L)-X(1))

ALLOCATE(XX(NX),YY(NY))

YI=X(1)-1.5D0
YF=X(L)+1.5D0
A=(YI-YF)/(1.0D0-DFLOAT(NX))
B=( DFLOAT(NX)*YI-YF )/( DFLOAT(NX)-1.0D0 )
DO I=1,NX
   XX(I)=A*DFLOAT(I)+B
ENDDO

YI=Y(1)-1.5D0
YF=Y(L)+1.5D0
A=(YI-YF)/(1.0D0-DFLOAT(NY))
B=( DFLOAT(NY)*YI-YF )/( DFLOAT(NY)-1.0D0 )
DO I=1,NY
   YY(I)=A*DFLOAT(I)+B
ENDDO

!!!!!!!!!!!! MALHA 3   !!!!!!!!!!!!!!!!

K=1
CENTER(1)=1.0D0+1.0D0*WIDTH
CENTER(2)=0.5D0*HEIGHT
DO J=1,IHEX_R
LIM=IHEX_Q
  DO I=1,IHEX_Q
    CALL HEX_CORNER(CENTER,SIZE,COORDX,COORDY)
    HEX_X(K,:)=COORDX
    HEX_Y(K,:)=COORDY
    CALL MAKE_CONECTIONS(K)
    CENTER(1)= CENTER(1)+HORIZ
    CENTER(2)= CENTER(2)
    K=K+1
  ENDDO
  ICENT=2*(J/2)-J
  CENTER(1)=1.0D0+1.0D0*WIDTH+ICENT*WIDTH*0.5D0
  CENTER(2)=CENTER(2)+VERT
ENDDO

!!!!!!!!!!!! MALHA 4   !!!!!!!!!!!!!!!!
DX=WIDTH
DY=HEIGHT/4.0D0
K=1
L_Y=HEIGHT/8.0D0
DO J=1,M/2
  L_X=1.0D0+0.25D0*WIDTH
  DO I=1, N/2
     XBON_Y(K)=L_X
     YBON_Y(K)=L_Y
     K=K+1
     L_X=L_X+DX
  ENDDO
     L_Y=L_Y+3.0D0*DY
     L_X=1.0D0+0.75D0*WIDTH
  DO I=N/2+1,N-1
     XBON_Y(K)=L_X
     YBON_Y(K)=L_Y
     K=K+1
     L_X=L_X+DX
  ENDDO
     L_Y=L_Y+3.0D0*DY
ENDDO
     XBON_Y(K)=L_X
     YBON_Y(K)=L_Y
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
L_X=0.0D0
L_Y=0.0D0
K=1
L_Y=HEIGHT/8.0D0
DO J=1,M/2
  L_X=1.0D0+0.75D0*WIDTH
  DO I=1, (N+1)/2 - 1
     XBON_Z(K)=L_X
     YBON_Z(K)=L_Y
     K=K+1
     L_X=L_X+DX
  ENDDO
     L_Y=L_Y+3.0D0*DY
     L_X=1.0D0+0.25D0*WIDTH
  DO I=(N+1)/2,N-1
     XBON_Z(K)=L_X
     YBON_Z(K)=L_Y
     K=K+1
     L_X=L_X+DX
  ENDDO
     L_Y=L_Y+3.0D0*DY
ENDDO
     XBON_Z(K)=L_X
     YBON_Z(K)=L_Y
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

L_X=0.0D0
L_Y=0.0D0
K=1
L_Y=-0.25D0*HEIGHT
DO J=1,M/2
  L_X=1.0D0
  DO I=1, N/2 
     XBON_W(K)=L_X
     YBON_W(K)=L_Y
     K=K+1
     L_X=L_X+DX
  ENDDO
     L_Y=L_Y+3.0D0*DY
     L_X=1.0D0+0.5D0*WIDTH
  DO I=(N+1)/2,N-1
     XBON_W(K)=L_X
     YBON_W(K)=L_Y
     K=K+1
     L_X=L_X+DX
  ENDDO
     L_Y=L_Y+3.0D0*DY
ENDDO
     XBON_W(K)=L_X
     YBON_W(K)=L_Y
!OPEN(1,FILE="TESTE_MALHA-Y-Z-W.plt")
! DO I=1,(M/2)*(N-1)
!   WRITE(1,*)XBON_Y(I),YBON_Y(I)
! ENDDO
CLOSE(1)

RETURN
END SUBROUTINE MAKE_MESHES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE GAUSS(XG,YG,A,F,TP)
USE GLOBAIS
IMPLICIT NONE
REAL(KIND = 8) :: XG,YG,A,F
REAL(KIND = 8) :: SIGMX,SIGMY
INTEGER :: I,J,TP

SIGMX=3.0D-1
SIGMY=3.0D-1
F=0.0D0
DO I=1,L
   F=F+AUX(TP,I)*DEXP(-( (XG-X(I))**2/(2.0D0*SIGMX**2) + (YG-Y(I))**2/(2.0D0*SIGMY**2) ))
!                y^4       +2*       y^2        *   x^2        +   x^4        +y^3       -3    *   y        *x^2 - 0.1 
!   F=F+DEXP(-( (YG-Y(I))**4+2.0D0*((YG-Y(I))**2)*((XG-X(I))**2)+(XG-X(I))**4+(YG-Y(I))**3-3.0D0*(YG-Y(I))*(XG-X(I))**2-0.1D1 ))
ENDDO

RETURN
END SUBROUTINE GAUSS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE GAUSS_BOND(XG,YG,A,F,TP)
USE GLOBAIS
IMPLICIT NONE
REAL(KIND = 8) :: XG,YG,A,F
REAL(KIND = 8) :: SIGMX,SIGMY
REAL(KIND = 8) :: AA,BB,CC,PI,ANGL

INTEGER :: I,J,TP

PI=DACOS(-1.0D0) 

F=0.0D0

!!        Y BOND     
ANGL=30.0D0*PI/180.0D0

SIGMX=4.0D-1
SIGMY=1.0D-1
AA= ( DCOS(ANGL)*DCOS(ANGL) )/(2.0D0*SIGMX**2) + ( DSIN(ANGL)*DSIN(ANGL) )/(2.0D0*SIGMY**2)
BB=-( DSIN(   2.0D0*ANGL)    )/(4.0D0*SIGMX**2) + ( DSIN(   2.0D0*ANGL)    )/(4.0D0*SIGMY**2)
CC= ( DSIN(ANGL)*DSIN(ANGL) )/(2.0D0*SIGMX**2) + ( DCOS(ANGL)*DCOS(ANGL) )/(2.0D0*SIGMY**2)

DO I=1,(M/2)*(N-1)
   F=F+AUX(TP,IYB(I))*DEXP(-( AA*(XG-XBON_Y(I))**2 + CC*(YG-YBON_Y(I))**2 - 2.0D0*BB*(XG-XBON_Y(I))*(YG-YBON_Y(I)) ))
ENDDO

!!        Z BOND     
ANGL=150.0D0*PI/180.0D0
SIGMX=4.0D-1
SIGMY=1.0D-1
AA= ( DCOS(ANGL)*DCOS(ANGL) )/(2.0D0*SIGMX**2) + ( DSIN(ANGL)*DSIN(ANGL) )/(2.0D0*SIGMY**2)
BB=-( DSIN(   2.0D0*ANGL)    )/(4.0D0*SIGMX**2) + ( DSIN(   2.0D0*ANGL)    )/(4.0D0*SIGMY**2)
CC= ( DSIN(ANGL)*DSIN(ANGL) )/(2.0D0*SIGMX**2) + ( DCOS(ANGL)*DCOS(ANGL) )/(2.0D0*SIGMY**2)

DO I=1,(M/2)*(N-1)
   F=F+AUX2(TP,IZB(I))*DEXP(-( AA*(XG-XBON_Z(I))**2 + CC*(YG-YBON_Z(I))**2 - 2.0D0*BB*(XG-XBON_Z(I))*(YG-YBON_Z(I)) ))
ENDDO

!!        W BOND     
ANGL=0.0D0*PI/180.0D0
SIGMX=1.0D-1
SIGMY=4.0D-1
AA= ( DCOS(ANGL)*DCOS(ANGL) )/(2.0D0*SIGMX**2) + ( DSIN(ANGL)*DSIN(ANGL) )/(2.0D0*SIGMY**2)
BB=-( DSIN(   2.0D0*ANGL)    )/(4.0D0*SIGMX**2) + ( DSIN(   2.0D0*ANGL)    )/(4.0D0*SIGMY**2)
CC= ( DSIN(ANGL)*DSIN(ANGL) )/(2.0D0*SIGMX**2) + ( DCOS(ANGL)*DCOS(ANGL) )/(2.0D0*SIGMY**2)
DO I=1,N*M/2
   F=F+AUX3(TP,IWB(I))*DEXP(-( AA*(XG-XBON_W(I))**2 + CC*(YG-YBON_W(I))**2 - 2.0D0*BB*(XG-XBON_W(I))*(YG-YBON_W(I)) ))
ENDDO


RETURN
END SUBROUTINE GAUSS_BOND
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE HEX_CORNER(CENTER,SIZE,COORDX,COORDY)
! in:  center, size
! out: coordx, coordy
IMPLICIT NONE
REAL(KIND=8) :: SIZE,THETA,THETA_R,PI
REAL,DIMENSION(6) :: COORDX, COORDY
REAL,DIMENSION(2) :: CENTER
INTEGER :: I

PI=DACOS(-1.0D0)

DO I=1,6
  THETA=60.0D0*I+30.0D0
  THETA_R=THETA*PI/180.0D0
  COORDX(I)=CENTER(1)+SIZE*DCOS(THETA_R)
ENDDO

DO I=1,6
  THETA=60.0D0*I+30.0D0
  THETA_R=THETA*PI/180.0D0
  COORDY(I)=CENTER(2)+SIZE*DSIN(THETA_R)
ENDDO


RETURN
END SUBROUTINE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE MAKE_CONECTIONS(IH)
! make matriz of conections
USE GLOBAIS
IMPLICIT NONE
INTEGER :: I,IH,INIT

INIT=(IH-1)*6
DO I=1,5
   CONX_X(IH,I)=I + INIT
   CONX_Y(IH,I)=I+1 + INIT
ENDDO
   CONX_X(IH,6)=6 + INIT
   CONX_Y(IH,6)=1 + INIT

RETURN
END SUBROUTINE MAKE_CONECTIONS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE PRINT_MESH
USE GLOBAIS
IMPLICIT NONE
INTEGER :: I,J

OPEN(1,FILE='HEX_MESH.plt')
  WRITE(1,006)NH*6,NH*6
  DO J=1,NH
    DO I=1,6
      WRITE(1,*)HEX_X(J,I),HEX_Y(J,I)
    ENDDO
  ENDDO
! CONECTIONS
  DO J=1,NH
    DO I=1,6
      WRITE(1,*)CONX_X(J,I),CONX_Y(J,I)
    ENDDO
  ENDDO
CLOSE(1)

006 FORMAT('ZONE T="1", DATAPACKING=POINT, NODES=',I6,', ELEMENTS=',I6,',ZONETYPE=FELINESEG,PASSIVEVARLIST=[3]')

RETURN
END SUBROUTINE PRINT_MESH
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE LEITURA_F(FORT,NL)
USE GLOBAIS
IMPLICIT NONE
CHARACTER*7 :: FORT
INTEGER :: I,J,K,NL,DM
REAL :: TMP(N_S)

AUX= 0.0D0
Yn=0.0D0
!WRITE(*,*)FORT
!READ(*,*)
DM=M*DMP/200
IF(FORT .EQ. 'fort.15') DM=DM*2
open(unit=1,file=FORT,status='old')
do j=1,p
   read(unit=1,fmt=*)
   read(unit=1,fmt=*)
   read(unit=1,fmt=*)(Yn(i), i=1,NL)	
!!!!!!!!!! TRANSLATION   !!!!!!!!!!!!!!!!
   DO K=1,DM
      DO I=1,N
         TMP(I)=Yn(I)
      ENDDO
      DO I=1,NL-N
         Yn(I)=Yn(I+N)
      ENDDO
      DO I=1,N
         Yn(NL-N+I)=TMP(I)
      ENDDO
   ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   do i=1,L
      AUX(j,i)=Yn(i)
   end do
end do
close(unit=1,status='keep')

!stop
RETURN
END SUBROUTINE LEITURA_F
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
