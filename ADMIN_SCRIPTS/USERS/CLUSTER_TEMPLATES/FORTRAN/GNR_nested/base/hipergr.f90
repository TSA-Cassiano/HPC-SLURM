!     ****************************************************************          
!     *                                                              *          
!     *        Graphene Dynamics in the Electric Field               *          
!     *                                                              *          
!     *             Single-layer with Impurities                     *          
!     *                                                              *          
!     *                                                              *          
!     *        M   ---- number of lines (even number)                *          
!     *        N   ---- number of columns (even number)              *          
!     *        NM  ---- net size (L=N*M)                             *          
!     *        NS  ---- number of sites                              *          
!     *                                                              *          
!     *                   J index (1,...,N)                          *          
!     *                                                              *          
!     *               N-1 N  1  2  3 . . .                           *          
!     *                   o     o     o     o     o                  *          
!     *                 /   \ /   \ /   \ /   \ /   \                *          
!     *            .   o     o     o     o     o     o               *          
!     *            .   |     |     |     |     |     |               *          
!     *            .   o     o     o     o     o     o               *          
!     *            2    \   / \   / \   / \   / \   /                *          
!     *                   o     o     o     o     o                  *          
!     *                   |     |     |     |     |                  *          
!     *                   o     o     o     o     o                  *          
!     *            1    /   \ /   \ /   \ /   \ /   \                *          
!     *                o     o     o     o     o     o               *          
!     *                |     |     |     |     |     |               *          
!     *                o     o     o     o     o     o               *          
!     *            M    \   / \   / \   / \   / \   /                *          
!     *                   o     o     o     o     o                  *          
!     *                                                              *          
!     *                                                              *          
!     *                   I index (1,...,M)                          *          
!     *                                                              *          
!     *          4   \/\/\/\/\/\/\/\/\/\/\/\/\                       *          
!     *                                                              *          
!     *          3   /\/\/\/\/\/\/\/\/\/\/\/\/\                      *          
!     *                                                              *          
!     *          2   \/\/\/\/\/\/\/\/\/\/\/\/\                       *          
!     *                                                              *          
!     *          1   /\/\/\/\/\/\/\/\/\/\/\/\/\                      *          
!     *                                                              *          
!     *                                                              *          
!     *                   K index (1,...,NK=N+M/2)                   *          
!     *                                                              *          
!     *              |  |  |  |  |  |  |  |  |                       *          
!     *               \  \  \  \  \  \  \  \  \                      *          
!     *                |  |  |  |  |  |  |  |  |                     *          
!     *              \  \  \  \  \  \  \  \  \  \                    *          
!     *               |  |  |  |  |  |  |  |  |                      *          
!     *                \  \  \  \  \  \  \  \  \                     *          
!     *                 |  |  |  |  |  |  |  |  |                    *          
!     *                                                              *          
!     *                 1  2  3  4  . . .  NK                        *          
!     *                                                              *          
!     *                                                              *          
!     *                   L index (1,...,NL=NK)                      *          
!     *                                                              *          
!     *                |  |  |  |  |  |  |  |  |  |                  *          
!     *               /  /  /  /  /  /  /  /  /  /                   *          
!     *              |  |  |  |  |  |  |  |  |  |                    *          
!     *             /  /  /  /  /  /  /  /  /  /                     *          
!     *               |  |  |  |  |  |  |  |  |  |                   *          
!     *              /  /  /  /  /  /  /  /  /  /                    *          
!     *             |  |  |  |  |  |  |  |  |  |                     *          
!     *                                                              *          
!     *                   NL  . .  .  4  3  2  1                     *          
!     *                                                              *          
!     *                                                              *          
!     *                                                              *          
!     *        NU  ---- number of up-spin electrons                  *          
!     *        ND  ---- number of down-spin electrons                *          
!     *        ERR ---- precision of the iteration                   *          
!     *                                                              *          
!     *        Electric Field Direction: Unit Vector: (xc,yc)        *          
!     *               Obs: xc**2 + yc**2 = 1                         *          
!     *                                                              *          
!     *        DELTA0 - Brazovskii-Kirova term                       *          
!     *                                                              *          
!     *              CAUTION!!  NU >= ND, NE<=NS, N>2                *          
!     *                                                              *          
!     ****************************************************************          
!                                                                               
      INCLUDE 'PARM.INC'                                                        
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      REAL*8 MA,MB
      DIMENSION AUR(NM,0:NM),AUI(NM,0:NM)                                               
      DIMENSION ADR(NM,0:NM),ADI(NM,0:NM)                                               
!                                                                               
!     Initial Value                                                             
!                                                                               
      PI=4.D0*DATAN(1.D0)                                                       
      A=0.D0                                                                    
      DELT=0.01D0                                                              
      ITTOT=60000
      ITOFF=ITTOT
      XC= 0.0D0                                                                  
      YC= 1.0D0               
      TAU= 5.D0                                                                 
      ITAU1=ITOFF
      ITAU2=ITTOT-ITOFF
!                                                                               
!        A is the vector potential giving rise to the electric field.           
!        DA is the rate of change of A per unit of time                         
!        DelT is the time mesh in unit of 1/(OmegaQ)                            
!        ITtot is the total time step                                           
!        IToff is the switching off time                                        
!        DENBA is the electric field strengh                                    
!        TAU determines how the e. field is switched (smoothly or not)          
!        MA is the mass of the sites A in u.m.a (ex: C is 12, CH is 13.007)
!        MB is the mass of the sites B in u.m.a (MB >= MA)
!        T0, UA, UB, and V are in units of ENUN (ENUN = Energy Units in eV)                                        
!        TSOA(B) is the Spin-orbit interaction of sub-lattice A(B)
!        TNNA(B) is the Next-nearest-neighbor hopping of sub-lattice A(B)
!        Omega is in units of 10^15 s^(-1)
!        Time is in units of Omega^(-1)
!        TOmega is 1/hbar in units of Omega/ENUN                                                     
!        E0 is the unit of electric field (E0 = hbar*omega/e*a)
!        PL is the lattice parameter "a" in Angstroms
!                                                                               
      MA = 12.00D0
      MB = 12.00D0
      ENUN = 2.5D0
      T0  = 2.7D0/ENUN  
      TSOA = 0.000D0/ENUN  
      TSOB = 0.000D0/ENUN  
      TNNA = 0.00D0/ENUN  
      TNNB = 0.00D0/ENUN  
      UA =   0.0000D0/ENUN
      UB =   0.0000D0/ENUN   
      V =    0.0000D0/ENUN                                                       
      DELTA0Y = 0.00D0*T0
      DELTA0Z = 0.00D0*T0
      DELTA0W = 0.00D0*T0
      FCNST = 21.00D0                                                            
      ALP = XXX
      PL = 1.41 
      OMEGA = 0.0982D0*DSQRT(4.0D0*FCNST/MA)
      TIMEUN = 1.0D0/OMEGA
      TOMEGA= ENUN/0.658D0/OMEGA
      RM = MA/MB
      E0 = 658.D0*OMEGA/PL
!                                                                               
      ALPHA= ALP/ENUN                                                           
!                                                                               
      READ(20,606) ERR,IMP1,IMP2,TIMP1,TIMP2,DENBA,TEMP                            
!                                                                               
      WRITE(6,600) NM, NS, NE                                                        
      WRITE(6,590) N, M                                                         
      WRITE(6,601) NU, ND                                                       
      WRITE(6,604) TIMP1, IMP1                                                  
      WRITE(6,605) TIMP2, IMP2                                                  
      WRITE(10,600) NM, NS, NE                                                       
      WRITE(10,590) N, M                                                        
      WRITE(10,601) NU, ND                                                      
      WRITE(10,604) TIMP1, IMP1                                                 
      WRITE(10,605) TIMP2, IMP2                                                 
      WRITE(6,609) TEMP                                                            
      WRITE(6,611) TIMEUN, ENUN                                                 
      WRITE(6,613) PL, E0                                                       
      WRITE(6,602) A, DENBA, DELT                                               
      WRITE(6,612) MA, MB                                                       
      WRITE(6,607) T0, UA, UB, V                                                 
      WRITE(6,614) TSOA,TSOB,TNNA,TNNB                                                  
      WRITE(6,610) DELTA0Y, DELTA0Z, DELTA0W                                                 
      WRITE(10,609) TEMP                                                           
      WRITE(10,611) TIMEUN, ENUN                                                 
      WRITE(10,613) PL, E0                                                       
      WRITE(10,602) A, DENBA, DELT                                              
      WRITE(10,612) MA, MB                                                       
      WRITE(10,607) T0, UA, UB, V                                                
      WRITE(10,614) TSOA,TSOB,TNNA,TNNB                                                  
      WRITE(10,610) DELTA0Y, DELTA0Z, DELTA0W                                                
      WRITE(6,603) ITTOT, ITOFF                                                 
      WRITE(10,603) ITTOT,ITOFF
      CALL FLUSH( 6)                                                            
      CALL FLUSH(10)                                                            
!                                                                               
      CALL OCCNUM (T0)                                                          
      CALL PARAM (RM)                                                           
!                                                                               
      CALL STATIC (FCNST,ALPHA,ALP,ERR,T0,ENUN,UA,UB,V,TIMP1,TIMP2,IMP1,IMP2,DELTA0Y,DELTA0Z,DELTA0W,TSOA,TSOB,TNNA,TNNB)
!                                                                               
      GOTO 999                                                                  
!                                                                               
      DO 10 IT=1,ITTOT                                                          
       CALL HAMIL (IT,AUR,AUI,ADR,ADI,A,XC,YC,ALPHA,UA,UB,V,TIMP1,TIMP2,IMP1,IMP2,DELTA0Y,DELTA0Z,DELTA0W,T0,ENUN,TSOA,TSOB,TNNA,TNNB) 
       CALL DYNAM (ENUN,FCNST,ALP,A,XC,YC,DELT,T,IT)                              
       CALL DIAGON (AUR,AUI,ADR,ADI,IT,DELT,TOMEGA,ENUN,FCNST,UA,UB,V)                
       CALL NEW (IT)                                                           
       CALL VECTPOT (A,IT,DELT,DENBA,TAU,PI,ITAU1,ITAU2,TEMP,T)                              
10    CONTINUE                                                                  
!                                                                               
999   STOP                                                                      
!                                                                               
590   FORMAT (/3X,'No OF COLUMNS = ',I5,3X,'No OF LINES = ',I5/)                
600   FORMAT (/3X,'NET SIZE = ',I5,3X,'SYSTEM SIZE = ',I5,3X,'ELECTRON NUMBER = ',I5/)              
601   FORMAT (/3X,'UP ELECTRONS = ',I5,3X,'DOWN ELECTRONS = ',I5/)              
602   FORMAT (/3X,'A = ',F10.5,5X,'DENBA = ',F10.5,5X,'DelT = ',F10.5/)         
603   FORMAT (/3X,'Total time step = ',I7,5X,'Switch off time = ',I7/)          
604   FORMAT (/3X,'TIMP1 = ',D10.3,3X,'IMP1 = ',I3/)                            
605   FORMAT (/3X,'TIMP2 = ',D10.3,3X,'IMP2 = ',I3/)                            
606   FORMAT (1X,D10.5,1X,I4,1X,I4,1X,D10.3,1X,D10.3,1X,D10.3,1X,D10.3)         
607   FORMAT (/3X,'T0 = ',F8.3,5X,'UA = ',F8.3,5X,'UB = ',F8.3,5X,'V = ',F8.3,5X/)           
608   FORMAT(/3X,'Boundary cond. in x = ',I3,3X,'Boundary in y = ',I3/)         
609   FORMAT (/3X,'TEMP = ',F10.5/)                                                
610   FORMAT (/3X,'DELTA0Y = ',F10.5,5X,'DELTA0Z = ',F10.5,5X,'DELTA0W = ',F10.5,5X/)           
611   FORMAT (/3X,'Time Unit = ',F10.5,1X,'x10^(-15) s',5X,'Energy Unit = ',F10.5,1X,'eV',/)         
612   FORMAT (/3X,'MA = ',F10.5,1X,'u.m.a.',5X,'MB = ',F10.5,1X,'u.m.a.',/)         
613   FORMAT (/3X,'Lattice Parameter = ',F6.3,1X,'Angstroms',5X,'Electric Field Unit = ',F10.5,1X,'mV/Angstroms',/)         
614   FORMAT (/3X,'TSOA = ',F8.3,5X,'TSOB = ',F8.3,5X,'TNNA = ',F8.3,5X,'TNNB = ',F8.3,5X/)           
      END                                                                       
!                                                                               
! ***************************************  SUBROUTINE OCCNUM                    
!                                                                               
      SUBROUTINE OCCNUM (T0)                                                    
      INCLUDE 'PARM.INC'                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                                 
      COMMON /OCC/ OCU(NE),OCD(NE)                                              
!                                                                               
      DO 10 I=1, NU   
10     OCU(I)=1.D0   
      DO 20 I=NU+1, NE  
20     OCU(I)=0.D0   
                   
!     OCU(NU)=0.D0  
!     OCU(NU+1)=1.D0  
                    
      DO 30 I=1, ND 
30     OCD(I)=1.D0 
      DO 40 I=ND+1, NE 
40     OCD(I)=0.D0  
                  
!     OCD(ND)=0.D0
!     OCD(ND+1)=1.D0

      RETURN   
      END    
!                                                                               
! ***************************************  SUBROUTINE PARAM                     
!                                                                               
      SUBROUTINE PARAM (RM)                                                     
      INCLUDE 'PARM.INC'                                                        
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      INTEGER OS,OY,OZ,OW                                                          
      COMMON /MAP/ MP(NS),IMP(0:NM)                                               
      COMMON /ONT/ OS(NM),OY(0:NI,NL),OZ(0:NI,NK),OW(NK+1,0:NL),NB                     
      COMMON /PAR/ PY(NI,NL,5),PZ(NI,NK,5),PW(NK,NL,5)                          
!                                                                               
      READ(60,601) ( OS(I),I=1,NM)                                               
      READ(61,601) ((OY(I,N/2+I/2*2+1-K),K=I/2+1,I/2+N/2),I=1,M)                
      READ(62,601) ((OZ(I,            K),K=I/2+1,I/2+N/2),I=1,M)                
      READ(63,601) ((OW(K,      N/2+I-K),K=I/2+1,I/2+N/2),I=1,M)                
!
!        Boundary Conditions                                                     
!    
      DO I=1,M                                                                  
       OY(I,(I+1)/2+N/2*(1-(-1)**I)/2)=OY(I,(I+1)/2+N/2*(1+(-1)**I)/2)          
       OZ(I,(I+1)/2+N/2*(1-(-1)**I)/2)=OZ(I,(I+1)/2+N/2*(1+(-1)**I)/2)          
      ENDDO                                                                     
                                                                                
      DO J=1,N/2                                                                
       OY(0,J)=OY(M,M/2+J)                                                      
       OZ(0,J)=OZ(M,M/2+J)                                                      
      ENDDO                                                                     
       OW(N/2+1,0)=OW(1,N/2)                                                      
                                                                                
      DO J=1,N/2                                                                
       OW(J+M/2,NL-J+1)=OW(J,N/2-J+1)                                           
      ENDDO                                                                     
               
      DO I=1,M/2                                                                
       OW(I,N/2+I)=OW(N/2+I,I)                                                  
      ENDDO                                                                     

      DO I=1,M
       K=(I+N+2)/2
       L=K-(N+1)+N/2
       OW(K,L) = OW((I+2)/2,(I+2)/2-1+N/2)
      ENDDO
!                                                                               
!      Dynamical Parameters
!                                                                               
      DO I=1,M                                                                  
       DO K=I/2+1,I/2+N/2                                                       
        LY = N/2+I/2*2+1-K                                                      
        L = N/2+I-K                                                             
!                                                                               
      I1=(-1)**I                                                                
      RM12=(1+I1)/2+RM*(1-I1)/2
      RM34=(1-I1)/2+RM*(1+I1)/2
!                                                                               
      PY(I,LY,1) = OY(I,LY)*(OZ(I,K)    *2 + 1-OZ(I,K)   *OW(K,LY)   )*OZ(I,K)*RM12              
      PY(I,LY,2) = OY(I,LY)*(OW(K,LY)   *2 + 1-OZ(I,K)   *OW(K,LY)   )*OW(K,LY)*RM12             
      PY(I,LY,3) = OY(I,LY)*(OZ(I,K-I1) *2 + 1-OZ(I,K-I1)*OW(K-I1,LY))*OZ(I,K-I1)*RM34           
      PY(I,LY,4) = OY(I,LY)*(OW(K-I1,LY)*2 + 1-OZ(I,K-I1)*OW(K-I1,LY))*OW(K-I1,LY)*RM34          
!                                                                               
      PY(I,LY,5) = - PY(I,LY,1) - PY(I,LY,2) - PY(I,LY,3) - PY(I,LY,4)          
!                                                                               
      PZ(I,K,1) = OZ(I,K)*(OW(K,L)  *2 + 1 - OW(K,L)  *OY(I,L)  )*OW(K,L)*RM                  
      PZ(I,K,2) = OZ(I,K)*(OY(I,L)  *2 + 1 - OW(K,L)  *OY(I,L)  )*OY(I,L)*RM                  
      PZ(I,K,3) = OZ(I,K)*(OW(K,L+1)*2 + 1 - OW(K,L+1)*OY(I,L+1))*OW(K,L+1)                
      PZ(I,K,4) = OZ(I,K)*(OY(I,L+1)*2 + 1 - OW(K,L+1)*OY(I,L+1))*OY(I,L+1)                
!                                                                               
      PZ(I,K,5) = - PZ(I,K,1) - PZ(I,K,2) - PZ(I,K,3) - PZ(I,K,4)               
!                                                                               
      PW(K,L,1) = OW(K,L)*(OY(I,L)  *2 + 1 - OY(I,L)  *OZ(I,K)  )*OY(I,L)*RM           
      PW(K,L,2) = OW(K,L)*(OZ(I,K)  *2 + 1 - OY(I,L)  *OZ(I,K)  )*OZ(I,K)*RM          
      PW(K,L,3) = OW(K,L)*(OY(I-1,L)*2 + 1 - OY(I-1,L)*OZ(I-1,K))*OY(I-1,L)           
      PW(K,L,4) = OW(K,L)*(OZ(I-1,K)*2 + 1 - OY(I-1,L)*OZ(I-1,K))*OZ(I-1,K)           
!                                                                               
      PW(K,L,5) = - PW(K,L,1) - PW(K,L,2) - PW(K,L,3) - PW(K,L,4)               
!                                                                               
       ENDDO                                                                    
      ENDDO                                                                     
!                                                                               
      NB = 0
      DO I=1,M
       DO K=I/2+1,I/2+N/2
        LY = N/2+I/2*2+1-K
        L = N/2+I-K
        NB = NB + OY(I,LY) + OZ(I,K) + OW(K,L)
       ENDDO
      ENDDO
!                                                                               
      DO I=0,NM
       IMP(I)=1
      ENDDO
!
      JJ=0                                                                      
      DO I=1,NM                                                                 
       JJ=JJ+OS(I)                                                              
       IF(OS(I).EQ.1) THEN                                                      
        MP(JJ)=I                                                                
        IMP(I)=JJ                                                               
       ENDIF                                                                    
      ENDDO                                                                     
!
      IF(JJ.NE.NS) THEN
       WRITE(*,*) 'ERROR: INVALID "NS" PARAMETER' 
       STOP                                                         
      ENDIF
!                                                                               
      RETURN                                                                    
!                                                                               
601   FORMAT (I1)                                                           
602   FORMAT (5I3)                                                           
      END                                                                       
!                                                                               
! ***************************************  SUBROUTINE VECTPOT                   
!                                                                               
      SUBROUTINE VECTPOT (A,IT,DELT,DENBA,TAU,PI,ITAU1,ITAU2,TEMP,T)
      IMPLICIT REAL*8(A-H,O-Z)                                                  
!                                                                               
!       ITAU1  :  Steps with electric field                                  
!       ITAU2  :  Steps with temperature                                     
!                                                                               
      DA=DENBA*DELT                                                             
      ITAUD=ITAU1+ITAU2
      TAU1=DFLOAT(ITAU1)*DELT
      TAU2=DFLOAT(ITAU2)*DELT
      TAUD = TAU1 + TAU2
      TAUT = 2.0D0*TAU1 + TAU2
      TAUDD = 2.0D0*(TAU1 + TAU2)
!
      TM=DFLOAT(IT)*DELT
      J = IT/(2*ITAUD)+1
      TM=TM-TAUDD*(J-1)

      IF(TM.GT.TAUD.AND.TM.LE.2*TAUD) THEN

       IF(TM.GT.2*TAUD-TAU2.AND.TM.LE.2*TAUD) THEN

        IF(TM.GT.2*TAUD-TAU2+TAU.AND.TM.LE.2*TAUD) THEN
         A=0.0D0
         T=TEMP
        ELSE
         A=-DENBA*((TM-TAUT-TAU)-DSIN(PI/TAU*(TM-TAUT-TAU))*TAU/PI)/2.D0
         T=0.0D0
        ENDIF
       ELSE
        IF(TM.GT.TAUD+TAU.AND.TM.LE.2*TAUD-TAU2) THEN
         A=-DENBA*(TM-2.0D0*TAUD+TAU2-TAU/2.0D0)
         T=0.0D0
        ELSE
         A=-DENBA*((TM-TAUD)-DSIN(PI/TAU*(TM-TAUD))*TAU/PI)/2.D0+DENBA*TAU1
         T=0.0D0
        ENDIF
       ENDIF

      ELSE 

       IF(TM.GT.TAUD-TAU2.AND.TM.LE.TAUD) THEN

        IF(TM.GT.TAUD-TAU2+TAU.AND.TM.LE.TAUD) THEN
         A=DENBA*TAU1
         T=TEMP
        ELSE
         A=DENBA*(TM-DSIN(PI/TAU*(TM-TAU1)+PI)*TAU/PI+TAU1-TAU)/2.D0
         T=0.0D0
        ENDIF
       ELSE

        IF(TM.GT.TAU.AND.TM.LE.TAUD-TAU2) THEN
         A=DENBA*(TM-TAU/2.D0)
         T=0.0D0
        ELSE
         A=DENBA*(TM-DSIN(PI/TAU*TM)*TAU/PI)/2.D0
         T=0.0D0
        ENDIF
       ENDIF

      ENDIF

!     WRITE(26,*) A
!     WRITE(27,*) T

      RETURN
      END
!                                                                               
! ***************************************  SUBROUTINE STATIC                    
!                                                                               
      SUBROUTINE STATIC (FCNST,ALPHA,ALP,ERR,T0,ENUN,UA,UB,V,TIMP1,TIMP2,IMP1,IMP2,DELTA0Y,DELTA0Z,DELTA0W,TSOA,TSOB,TNNA,TNNB)
!                                                                               
      INCLUDE 'PARM.INC'                                                        
      IMPLICIT REAL*8 (A-H,O-Y)
      IMPLICIT COMPLEX*16 (Z)
      REAL*8 Z,Z1
      INTEGER OS,OY,OZ,OW
      DIMENSION Y1(NI,NL),Z1(NI,NK),W1(NK,NL)                                               
      DIMENSION AUR(NM,0:NM)                                                      
      DIMENSION ADR(NM,0:NM)                                                      
      DIMENSION AUI(NM,0:NM)                                                        
      DIMENSION ADI(NM,0:NM)                                                        
      DIMENSION BUR(NS,NS)                                                      
      DIMENSION BDR(NS,NS)                                                      
      DIMENSION BUI(NS,NS)                                                      
      DIMENSION BDI(NS,NS)                                                      
      DIMENSION ZEVU(NS,NS),ZEVD(NS,NS)
      DIMENSION ZTUY1(NI,NL),ZTUZ1(NI,NK),ZTUW1(NK,NL)
      DIMENSION ZTDY1(NI,NL),ZTDZ1(NI,NK),ZTDW1(NK,NL)
      DIMENSION RU(NS),RD(NS)                                         
      DIMENSION RU1(NS),RD1(NS)                                     
      DIMENSION TUYR(NI,NL),TUYI(NI,NL),TDYR(NI,NL),TDYI(NI,NL)
      DIMENSION TUZR(NI,NK),TUZI(NI,NK),TDZR(NI,NK),TDZI(NI,NK)
      DIMENSION TUWR(NK,NL),TUWI(NK,NL),TDWR(NK,NL),TDWI(NK,NL)
      DIMENSION TUYR1(NI,NL),TUYI1(NI,NL),TDYR1(NI,NL),TDYI1(NI,NL)
      DIMENSION TUZR1(NI,NK),TUZI1(NI,NK),TDZR1(NI,NK),TDZI1(NI,NK)
      DIMENSION TUWR1(NK,NL),TUWI1(NK,NL),TDWR1(NK,NL),TDWI1(NK,NL)
      DIMENSION CHDENNET(NM),SDENNET(NM)
      DIMENSION FV1(NS),FV2(NS),FM1(2,NS)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INTEGER,PARAMETER :: LWMAX=2*NS-1,LRWORK=3*NS-2
COMPLEX*16 :: A(NS,NS), WORK( LWMAX )
INTEGER :: INFO,LWORK
REAL*8 ::  RWORK( LRWORK )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                                          
      COMMON /MAP/ MP(NS),IMP(0:NM)                                               
      COMMON /CHD/ CHDEN(NS),SDEN(NS)                                             
      COMMON /LAT/ Y(0:NI,NL),Z(0:NI,NK),W(NK,NL) 
      COMMON /LATV/ YV(NI,NL),ZV(NI,NK),WV(NK,NL)
      COMMON /ELCUA/ EU(NS),EVUR(NS,NS),EVUI(NS,NS)                                  
      COMMON /ELCDA/ ED(NS),EVDR(NS,NS),EVDI(NS,NS)                                  
      COMMON /ELCUAA/ EVURAA(NS,NS),EVUIAA(NS,NS)                                  
      COMMON /ELCDAA/ EVDRAA(NS,NS),EVDIAA(NS,NS)                                  
      COMMON /TAUU/ ZTUY(NI,NL),ZTUZ(NI,NK),ZTUW(NK,NL)
      COMMON /TAUD/ ZTDY(NI,NL),ZTDZ(NI,NK),ZTDW(NK,NL)
      COMMON /ENG/ EE,EL,EDY,ET                                                 
      COMMON /OCC/ OCU(NE),OCD(NE)                                              
      COMMON /IND/ IC(0:M+1,-1:N+1)                                              
      COMMON /ONT/ OS(NM),OY(0:NI,NL),OZ(0:NI,NK),OW(NK+1,0:NL),NB                     
!
      ZI = DCMPLX(0.0D0,1.0D0)
      RAMDA = ALP/FCNST*2.0D0                                                   
      ITA   = 0                                                                 
!                                                                               
      DO II=0,M+1                                                               
       DO JJ=-1,N+1                                                              
         I=II                                                                   
         J=JJ                                                                   
         IF(J.EQ.-1) J=N-1
         IF(I.EQ.0)  I=M                                                         
         IF(J.EQ.0)  J=N                                                         
         IF(I.EQ.M+1) I=1                                                       
         IF(J.EQ.N+1) J=1                                                       
         IC(II,JJ) = N*(I-1) + J                                                
       ENDDO                                                                    
      ENDDO                                                                     
!                                                                               
!       Initial Lattice                                                         
!                                                                               
!      READ(32,620) (( Y(I,N/2+I/2*2+1-K),K=I/2+1,I/2+N/2),I=1,M)                
!      READ(33,620) (( Z(I,            K),K=I/2+1,I/2+N/2),I=1,M)                
!      READ(34,620) (( W(K,      N/2+I-K),K=I/2+1,I/2+N/2),I=1,M)                
y=0.d0
z=0.d0
w=0.d0
!                                                                               
!      Initial rho and tau                                                      
!                                                                               
      DO 10 I=1,NS                                                               
       RU(I) = 0.5D0                                                            
       RD(I) = 0.5D0                                                            
10    CONTINUE                                                                  
!                                                                               
      DO I=1,M
       DO K=I/2+1,I/2+N/2
        LY = N/2+I/2*2+1-K
        L = N/2+I-K
        TUYR(I,LY) = 0.0D0
        TUZR(I, K) = 0.0D0
        TUWR(K, L) = 0.0D0
        TDYR(I,LY) = 0.0D0
        TDZR(I, K) = 0.0D0
        TDWR(K, L) = 0.0D0
        TUYI(I,LY) = 0.00D0
        TUZI(I, K) = 0.00D0
        TUWI(K, L) = 0.00D0
        TDYI(I,LY) = 0.00D0
        TDZI(I, K) = 0.00D0
        TDWI(K, L) = 0.00D0
       ENDDO
      ENDDO
!                                                                               
! ************* ITERATION *****************                                     
!                                                                               
!                                                                               
1000  IF (ITA.GT.20000) STOP                                                    
!                                                                               
      DO 20 I=1,NM                                                               
       DO 30 J=1,NM                                                            
        AUR(I,J) = 0.D0                                                         
        ADR(I,J) = 0.D0                                                         
        AUI(I,J) = 0.D0                                                         
        ADI(I,J) = 0.D0                                                         
30     CONTINUE                                                                 
20    CONTINUE                                                                  
!                                                                               
!     Tight Binding
!
      DO I=1,M
       DO J=1,N
        IJ=IC(I,J)
        K=(I+J+1)/2
        L=K-J+N/2
        IF((I+J)/2*2.EQ.I+J) THEN
         AUR(IJ,IC(I,J-1)) = (-(T0 + DELTA0Z) + ALPHA*Z(I,K) - V*TUZR(I,K))*OZ(I,K)
         AUI(IJ,IC(I,J-1)) = (                               - V*TUZI(I,K))*OZ(I,K)
         ADR(IJ,IC(I,J-1)) = (-(T0 + DELTA0Z) + ALPHA*Z(I,K) - V*TDZR(I,K))*OZ(I,K)
         ADI(IJ,IC(I,J-1)) = (                               - V*TDZI(I,K))*OZ(I,K)
         AUR(IJ,IC(I-1,J)) = (-(T0 + DELTA0W) + ALPHA*W(K,L) - V*TUWR(K,L))*OW(K,L)
         AUI(IJ,IC(I-1,J)) = (                               - V*TUWI(K,L))*OW(K,L)
         ADR(IJ,IC(I-1,J)) = (-(T0 + DELTA0W) + ALPHA*W(K,L) - V*TDWR(K,L))*OW(K,L)
         ADI(IJ,IC(I-1,J)) = (                               - V*TDWI(K,L))*OW(K,L)
         AUR(IC(I,J-1),IJ) = AUR(IJ,IC(I,J-1))
         AUI(IC(I,J-1),IJ) =-AUI(IJ,IC(I,J-1))
         ADR(IC(I,J-1),IJ) = ADR(IJ,IC(I,J-1))
         ADI(IC(I,J-1),IJ) =-ADI(IJ,IC(I,J-1))
         AUR(IC(I-1,J),IJ) = AUR(IJ,IC(I-1,J))
         AUI(IC(I-1,J),IJ) =-AUI(IJ,IC(I-1,J))
         ADR(IC(I-1,J),IJ) = ADR(IJ,IC(I-1,J))
         ADI(IC(I-1,J),IJ) =-ADI(IJ,IC(I-1,J))
        ELSE
         AUR(IJ,IC(I,J-1)) = (-(T0 + DELTA0Y) + ALPHA*Y(I,L) - V*TUYR(I,L))*OY(I,L)
         AUI(IJ,IC(I,J-1)) = (                               - V*TUYI(I,L))*OY(I,L)
         ADR(IJ,IC(I,J-1)) = (-(T0 + DELTA0Y) + ALPHA*Y(I,L) - V*TDYR(I,L))*OY(I,L)
         ADI(IJ,IC(I,J-1)) = (                               - V*TDYI(I,L))*OY(I,L)
         AUR(IC(I,J-1),IJ) = AUR(IJ,IC(I,J-1))
         AUI(IC(I,J-1),IJ) =-AUI(IJ,IC(I,J-1))
         ADR(IC(I,J-1),IJ) = ADR(IJ,IC(I,J-1))
         ADI(IC(I,J-1),IJ) =-ADI(IJ,IC(I,J-1))
        ENDIF
       ENDDO
      ENDDO
!                                                                               
!     Spin-orbit interaction 
!
      DO I=1,M
       DO J=1,N
        IJ=IC(I,J)
        K=(I+J+1)/2
        L=K-J+N/2
        IF((I+J)/2*2.EQ.I+J) THEN
         AUI(IJ,IC(I  ,J-2)) =  TSOA*OZ(I,K)*OY(I,L+1)
         AUI(IJ,IC(I-1,J-1)) = -TSOA*OW(K,L)*OY(I-1,L)                 
         AUI(IJ,IC(I-1,J+1)) =  TSOA*OW(K,L)*OZ(I-1,K)                
         ADI(IJ,IC(I  ,J-2)) = -TSOA*OZ(I,K)*OY(I,L+1)                 
         ADI(IJ,IC(I-1,J-1)) =  TSOA*OW(K,L)*OY(I-1,L)                   
         ADI(IJ,IC(I-1,J+1)) = -TSOA*OW(K,L)*OZ(I-1,K)                 
!
         AUI(IC(I  ,J-2),IJ) = -AUI(IJ,IC(I  ,J-2)) 
         AUI(IC(I-1,J-1),IJ) = -AUI(IJ,IC(I-1,J-1))
         AUI(IC(I-1,J+1),IJ) = -AUI(IJ,IC(I-1,J+1))
         ADI(IC(I  ,J-2),IJ) = -ADI(IJ,IC(I  ,J-2))
         ADI(IC(I-1,J-1),IJ) = -ADI(IJ,IC(I-1,J-1))
         ADI(IC(I-1,J+1),IJ) = -ADI(IJ,IC(I-1,J+1))
        ELSE
         AUI(IJ,IC(I  ,J-2)) =  TSOB*OY(I,L)*OZ(I,K-1)
         AUI(IJ,IC(I-1,J-1)) = -TSOB*OY(I,L)*OW(K-1,L)
         AUI(IJ,IC(I-1,J+1)) =  TSOB*OZ(I,K)*OW(K,L-1)
         ADI(IJ,IC(I  ,J-2)) = -TSOB*OY(I,L)*OZ(I,K-1)
         ADI(IJ,IC(I-1,J-1)) =  TSOB*OY(I,L)*OW(K-1,L)
         ADI(IJ,IC(I-1,J+1)) = -TSOB*OZ(I,K)*OW(K,L-1)
!
         AUI(IC(I  ,J-2),IJ) = -AUI(IJ,IC(I  ,J-2))
         AUI(IC(I-1,J-1),IJ) = -AUI(IJ,IC(I-1,J-1))
         AUI(IC(I-1,J+1),IJ) = -AUI(IJ,IC(I-1,J+1))
         ADI(IC(I  ,J-2),IJ) = -ADI(IJ,IC(I  ,J-2))
         ADI(IC(I-1,J-1),IJ) = -ADI(IJ,IC(I-1,J-1))
         ADI(IC(I-1,J+1),IJ) = -ADI(IJ,IC(I-1,J+1))
        ENDIF
       ENDDO
      ENDDO
!                                                                               
!     Next-Nearest-Neighbour Hopping 
!
      DO I=1,M
       DO J=1,N
        IJ=IC(I,J)
        K=(I+J+1)/2
        L=K-J+N/2
        IF((I+J)/2*2.EQ.I+J) THEN
         AUR(IJ,IC(I  ,J-2)) =  TNNA*OZ(I,K)*OY(I,L+1)
         AUR(IJ,IC(I-1,J-1)) =  TNNA*OW(K,L)*OY(I-1,L)
         AUR(IJ,IC(I-1,J+1)) =  TNNA*OW(K,L)*OZ(I-1,K)
         ADR(IJ,IC(I  ,J-2)) =  TNNA*OZ(I,K)*OY(I,L+1)
         ADR(IJ,IC(I-1,J-1)) =  TNNA*OW(K,L)*OY(I-1,L)
         ADR(IJ,IC(I-1,J+1)) =  TNNA*OW(K,L)*OZ(I-1,K)
!
         AUR(IC(I  ,J-2),IJ) =  AUR(IJ,IC(I  ,J-2)) 
         AUR(IC(I-1,J-1),IJ) =  AUR(IJ,IC(I-1,J-1))
         AUR(IC(I-1,J+1),IJ) =  AUR(IJ,IC(I-1,J+1))
         ADR(IC(I  ,J-2),IJ) =  ADR(IJ,IC(I  ,J-2))
         ADR(IC(I-1,J-1),IJ) =  ADR(IJ,IC(I-1,J-1))
         ADR(IC(I-1,J+1),IJ) =  ADR(IJ,IC(I-1,J+1))
        ELSE
         AUR(IJ,IC(I  ,J-2)) =  TNNB*OY(I,L)*OZ(I,K-1)
         AUR(IJ,IC(I-1,J-1)) =  TNNB*OY(I,L)*OW(K-1,L)
         AUR(IJ,IC(I-1,J+1)) =  TNNB*OZ(I,K)*OW(K,L-1)
         ADR(IJ,IC(I  ,J-2)) =  TNNB*OY(I,L)*OZ(I,K-1)
         ADR(IJ,IC(I-1,J-1)) =  TNNB*OY(I,L)*OW(K-1,L)
         ADR(IJ,IC(I-1,J+1)) =  TNNB*OZ(I,K)*OW(K,L-1)
!
         AUR(IC(I  ,J-2),IJ) =  AUR(IJ,IC(I  ,J-2))
         AUR(IC(I-1,J-1),IJ) =  AUR(IJ,IC(I-1,J-1))
         AUR(IC(I-1,J+1),IJ) =  AUR(IJ,IC(I-1,J+1))
         ADR(IC(I  ,J-2),IJ) =  ADR(IJ,IC(I  ,J-2))
         ADR(IC(I-1,J-1),IJ) =  ADR(IJ,IC(I-1,J-1))
         ADR(IC(I-1,J+1),IJ) =  ADR(IJ,IC(I-1,J+1))
        ENDIF
       ENDDO
      ENDDO
!                                                                               
!     Matrix Compaction                                                         
!                                                                               
      DO I=1,NS                                                                 
       DO J=1,NS                                                                
        BUR(I,J)=AUR(MP(I),MP(J))                                               
        BUI(I,J)=AUI(MP(I),MP(J))                                               
        BDR(I,J)=ADR(MP(I),MP(J))                                               
        BDI(I,J)=ADI(MP(I),MP(J))                                               
       ENDDO                                                                    
      ENDDO                                                                     
!                                                                               
!     On-site Coulomb Interaction
!
      DO I=1,M                                                                  
       DO J=1,N                                                                 
        IJ=IC(I,J)                                                              
        IF(OS(IJ).EQ.1) THEN
         K=(I+J+1)/2                                                             
         L=K-J+N/2                                                               
         I1 = 2*(I+J-(I+J)/2*2)-1
         IR = IC(I, J+1)
         IS = IC(I, J-1)
         IV = IC(I+I1,J)
         IF((I+J)/2*2.EQ.I+J) THEN                                               
          VTERM = 0.D0
          VTERM = VTERM + (RU(IMP(IR)) + RD(IMP(IR)) - 1.D0)*OY(I,L)
          VTERM = VTERM + (RU(IMP(IS)) + RD(IMP(IS)) - 1.D0)*OZ(I,K) 
          VTERM = VTERM + (RU(IMP(IV)) + RD(IMP(IV)) - 1.D0)*OW(K,L) 
          BUR(IMP(IJ),IMP(IJ)) = UA*(RD(IMP(IJ))-0.5D0) + V*VTERM 
          BDR(IMP(IJ),IMP(IJ)) = UA*(RU(IMP(IJ))-0.5D0) + V*VTERM 
         ELSE
          VTERM = 0.D0
          VTERM = VTERM + (RU(IMP(IR)) + RD(IMP(IR)) - 1.D0)*OZ(I,K)     
          VTERM = VTERM + (RU(IMP(IS)) + RD(IMP(IS)) - 1.D0)*OY(I,L)  
          VTERM = VTERM + (RU(IMP(IV)) + RD(IMP(IV)) - 1.D0)*OW(K,L)     
          BUR(IMP(IJ),IMP(IJ)) = UB*(RD(IMP(IJ))-0.5D0) + V*VTERM 
          BDR(IMP(IJ),IMP(IJ)) = UB*(RU(IMP(IJ))-0.5D0) + V*VTERM
         ENDIF
        ENDIF
       ENDDO
      ENDDO
!
!     Impurities
!
      BUR(IMP(IMP1),IMP(IMP1))=BUR(IMP(IMP1),IMP(IMP1))+TIMP1  
      BDR(IMP(IMP1),IMP(IMP1))=BDR(IMP(IMP1),IMP(IMP1))+TIMP1  
      BUR(IMP(IMP2),IMP(IMP2))=BUR(IMP(IMP2),IMP(IMP2))+TIMP2  
      BDR(IMP(IMP2),IMP(IMP2))=BDR(IMP(IMP2),IMP(IMP2))+TIMP2  
!
!      MATZ = 1                                                                  
!      CALL CH(NS,NS,BUR,BUI,EU,MATZ,EVUR,EVUI,FV1,FV2,FM1,IERR)                 
!      MATZ = 1                                                                  
!      CALL CH(NS,NS,BDR,BDI,ED,MATZ,EVDR,EVDI,FV1,FV2,FM1,IERR)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
A=DCMPLX(BUR,BUI)
LWORK = -1
CALL ZHEEV( 'V', 'L', NS, A, NS, EU, WORK, LWORK, RWORK, INFO )
      LWORK = MIN( LWMAX, INT( WORK( 1 ) ) )
CALL ZHEEV( 'V', 'L', NS, A, NS, EU, WORK, LWORK, RWORK, INFO )
EVUR=DREAL(A)
EVUI=DIMAG(A)
A=DCMPLX(BDR,BDI)
CALL ZHEEV( 'V', 'L', NS, A, NS, ED, WORK, LWORK, RWORK, INFO )
EVDR=DREAL(A)
EVDI=DIMAG(A)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      DO I=1,M                                                                  
       DO K=I/2+1,I/2+N/2                                                       
        LY = N/2+I/2*2+1-K                                                      
        L = N/2+I-K                                                             
        Y1(I,LY) = 0.0D0                                                        
        Z1(I, K) = 0.0D0                                                        
        W1(K, L) = 0.0D0                                                        
       ENDDO                                                                    
      ENDDO                                                                     
!                                                                               
      DO I=1,NS                                                                 
        CHDEN(I)= 0.0D0                                                         
        SDEN(I) = 0.0D0                                                         
      ENDDO                                                                     
!                                                                               
      DO I=1,M                                                                  
       DO J=1,N                                                                 
        IJ=IC(I,J)                                                              
        K=(I+J+1)/2                                                             
        L=K-J+N/2                                                               
        IP=IMP(IC(I,J-1))                                                      
        IF((I+J)/2*2.NE.I+J) THEN                                               
         IF(OY(I,L).EQ.1) THEN   
          DO KK=1, NE                                                            
           Y1(I,L)=Y1(I,L) - (EVUR(IMP(IJ),KK)*EVUR(IP,KK)+EVUI(IMP(IJ),KK)*EVUI(IP,KK))*OCU(KK) &
                           - (EVDR(IMP(IJ),KK)*EVDR(IP,KK)+EVDI(IMP(IJ),KK)*EVDI(IP,KK))*OCD(KK)   
          ENDDO                                                                  
         ENDIF
        ELSE                                                                    
         IQ=IMP(IC(I-1,J))                                                      
         IF(OZ(I,K).EQ.1) THEN   
          DO KK=1, NE                                                            
           Z1(I,K)=Z1(I,K) - (EVUR(IMP(IJ),KK)*EVUR(IP,KK)+EVUI(IMP(IJ),KK)*EVUI(IP,KK))*OCU(KK) &
                           - (EVDR(IMP(IJ),KK)*EVDR(IP,KK)+EVDI(IMP(IJ),KK)*EVDI(IP,KK))*OCD(KK)   
          ENDDO                                                                  
         ENDIF
         IF(OW(K,L).EQ.1) THEN          
          DO KK=1, NE                                                            
           W1(K,L)=W1(K,L) - (EVUR(IMP(IJ),KK)*EVUR(IQ,KK)+EVUI(IMP(IJ),KK)*EVUI(IQ,KK))*OCU(KK) &
                           - (EVDR(IMP(IJ),KK)*EVDR(IQ,KK)+EVDI(IMP(IJ),KK)*EVDI(IQ,KK))*OCD(KK)   
          ENDDO                                                                  
         ENDIF
        ENDIF                                                                   
       ENDDO                                                                    
      ENDDO                                                                     
!
       YTOT = 0.0D0                                                             
!
      DO I=1,M
       DO J=1,N
        K=(I+J+1)/2
        L=K-J+N/2
        IF((I+J)/2*2.NE.I+J) THEN
         YTOT = YTOT + Y1(I,L)                             
        ELSE
         YTOT = YTOT + Z1(I,K)                              
         YTOT = YTOT + W1(K,L)                             
        ENDIF
       ENDDO
      ENDDO
!
       YTOT = YTOT/DFLOAT(NB)                                              
!
      DO I=1,M
       DO J=1,N
        K=(I+J+1)/2
        L=K-J+N/2
        IF((I+J)/2*2.NE.I+J) THEN
         Y1(I,L) = (Y1(I,L) - YTOT)*RAMDA*OY(I,L)                                     
        ELSE
         Z1(I,K) = (Z1(I,K) - YTOT)*RAMDA*OZ(I,K)                                    
         W1(K,L) = (W1(K,L) - YTOT)*RAMDA*OW(K,L)                                     
        ENDIF
       ENDDO
      ENDDO
!
!     Rho and Tau
!
      DO I=1,NS
       DO J=1,NS
        ZEVU(I,J) = DCMPLX(EVUR(I,J),EVUI(I,J))
        ZEVD(I,J) = DCMPLX(EVDR(I,J),EVDI(I,J))
       ENDDO
      ENDDO
!
      DO I=1,NS
        RU1(I)=0.D0
        RD1(I)=0.D0
        DO J=1,NE
          RU1(I) = RU1(I) + DCONJG(ZEVU(I,J))*ZEVU(I,J)*OCU(J)
          RD1(I) = RD1(I) + DCONJG(ZEVD(I,J))*ZEVD(I,J)*OCD(J)
        ENDDO   
      ENDDO   
!
      DO I=1,M
       DO J=1,N
        IJ=IC(I,J)
        I1=IC(I,J-1)
        K=(I+J+1)/2
        L=K-J+N/2
        IF((I+J)/2*2.NE.I+J) THEN
         ZTUY1(I,L) = (0.0D0,0.0D0)
         ZTDY1(I,L) = (0.0D0,0.0D0)
         DO JJ=1,NE
          ZTUY1(I,L) = ZTUY1(I,L) + DCONJG(ZEVU(IMP(IJ),JJ))*ZEVU(IMP(I1),JJ)*OCU(JJ)*OY(I,L)
          ZTDY1(I,L) = ZTDY1(I,L) + DCONJG(ZEVD(IMP(IJ),JJ))*ZEVD(IMP(I1),JJ)*OCD(JJ)*OY(I,L)
         ENDDO
        ELSE
         ZTUZ1(I,K) = (0.0D0,0.0D0)
         ZTDZ1(I,K) = (0.0D0,0.0D0)
         DO JJ=1,NE
          ZTUZ1(I,K) = ZTUZ1(I,K) + DCONJG(ZEVU(IMP(IJ),JJ))*ZEVU(IMP(I1),JJ)*OCU(JJ)*OZ(I,K)
          ZTDZ1(I,K) = ZTDZ1(I,K) + DCONJG(ZEVD(IMP(IJ),JJ))*ZEVD(IMP(I1),JJ)*OCD(JJ)*OZ(I,K)
         ENDDO
        ENDIF
       ENDDO
      ENDDO
!
       DO I=1,M
        IJ =IC(I,1)
        I1 =IC(I,N)
        K=(I+1+1)/2
        L=K-1+N/2
        IF(I/2*2.NE.I) THEN
         ZTUZ1(I,K) = (0.0D0,0.0D0)
         ZTDZ1(I,K) = (0.0D0,0.0D0)
         DO JJ=1,NE
          ZTUZ1(I,K) = ZTUZ1(I,K) + DCONJG(ZEVU(IMP(IJ),JJ))*ZEVU(IMP(I1),JJ)*OCU(JJ)*OZ(I,K)
          ZTDZ1(I,K) = ZTDZ1(I,K) + DCONJG(ZEVD(IMP(IJ),JJ))*ZEVD(IMP(I1),JJ)*OCD(JJ)*OZ(I,K)
         ENDDO
        ELSE
         ZTUY1(I,L) = (0.0D0,0.0D0)
         ZTDY1(I,L) = (0.0D0,0.0D0)
         DO JJ=1,NE
          ZTUY1(I,L) = ZTUY1(I,L) + DCONJG(ZEVU(IMP(IJ),JJ))*ZEVU(IMP(I1),JJ)*OCU(JJ)*OY(I,L)
          ZTDY1(I,L) = ZTDY1(I,L) + DCONJG(ZEVD(IMP(IJ),JJ))*ZEVD(IMP(I1),JJ)*OCD(JJ)*OY(I,L)
         ENDDO
        ENDIF
       ENDDO
!
      DO I=2,M
       DO J=1,N
        IF(2*((I+J)/2).EQ.I+J) THEN
         IJ =IC(I,J)
         I1 =IC(I-1,J)
         K=(I+J+1)/2
         L=K-J+N/2
         ZTUW1(K,L) = (0.0D0,0.0D0)
         ZTDW1(K,L) = (0.0D0,0.0D0)
         DO JJ=1,NE
          ZTUW1(K,L) = ZTUW1(K,L) + DCONJG(ZEVU(IMP(IJ),JJ))*ZEVU(IMP(I1),JJ)*OCU(JJ)*OW(K,L)
          ZTDW1(K,L) = ZTDW1(K,L) + DCONJG(ZEVD(IMP(IJ),JJ))*ZEVD(IMP(I1),JJ)*OCD(JJ)*OW(K,L)
         ENDDO
        ENDIF
       ENDDO
      ENDDO
!    
       DO J=1,N-1,2
        IJ =IC(1,J)
        I1 =IC(M,J)
        K=(1+J+1)/2
        L=K-J+N/2
        ZTUW1(K,L) = (0.0D0,0.0D0)
        ZTDW1(K,L) = (0.0D0,0.0D0)
        DO JJ=1,NE
         ZTUW1(K,L) = ZTUW1(K,L) + DCONJG(ZEVU(IMP(IJ),JJ))*ZEVU(IMP(I1),JJ)*OCU(JJ)*OW(K,L)
         ZTDW1(K,L) = ZTDW1(K,L) + DCONJG(ZEVD(IMP(IJ),JJ))*ZEVD(IMP(I1),JJ)*OCD(JJ)*OW(K,L)
        ENDDO
       ENDDO
!
      DO I=1,M
       DO K=I/2+1,I/2+N/2
        LY = N/2+I/2*2+1-K
        L = N/2+I-K
        TUYR1(I,LY) = DIMAG( ZTUY1(I,LY)*ZI)
        TUYI1(I,LY) = DIMAG( ZTUY1(I,LY)   )
        TUZR1(I, K) = DIMAG( ZTUZ1(I, K)*ZI)
        TUZI1(I, K) = DIMAG( ZTUZ1(I, K)   )
        TUWR1(K, L) = DIMAG( ZTUW1(K, L)*ZI)
        TUWI1(K, L) = DIMAG( ZTUW1(K, L)   )
        TDYR1(I,LY) = DIMAG( ZTDY1(I,LY)*ZI)
        TDYI1(I,LY) = DIMAG( ZTDY1(I,LY)   )
        TDZR1(I, K) = DIMAG( ZTDZ1(I, K)*ZI)
        TDZI1(I, K) = DIMAG( ZTDZ1(I, K)   )
        TDWR1(K, L) = DIMAG( ZTDW1(K, L)*ZI)
        TDWI1(K, L) = DIMAG( ZTDW1(K, L)   )
       ENDDO
      ENDDO
!
! *** Verification of Convergence/Consistency ***                               
!                                                                               
      CCY  = 0.0D0                                                              
      DDY  = 0.0D0                                                              
      EEY  = 0.0D0                                                              
!
      DO I=1,M
       DO J=1,N
        K=(I+J+1)/2
        L=K-J+N/2
        IF((I+J)/2*2.NE.I+J) THEN
        CCY = CCY + Y1(I,L)**2                         
        DDY = DDY + (Y(I,L)-Y1(I,L))*(Y(I,L)-Y1(I,L)) 
        ELSE
        CCY = CCY + Z1(I,K)**2 + W1(K,L)**2                        
        DDY = DDY + (Z(I,K)-Z1(I,K))*(Z(I,K)-Z1(I,K))  
        DDY = DDY + (W(K,L)-W1(K,L))*(W(K,L)-W1(K,L)) 
        ENDIF
       ENDDO
      ENDDO
!
      EEY  = DDY/CCY                                                            
!
       CCR = 0.D0                                                                
       DDR = 0.D0                                                                
       DO 105 I=1,NS                                                             
         CCR = CCR + RU1(I)**2 + RD1(I)**2                                       
         DDR = DDR + (RU(I)-RU1(I))**2 + (RD(I)-RD1(I))**2                       
105    CONTINUE                                                                  
       CCT = 0.D0                                                                
       DDT = 0.D0                                                                
      EEE = 0.D0                                                                
!      DO 107 I=1,L                                                             
!        CCT = CCT + TU1(I)**2 + TD1(I)**2                                      
!        DDT = DDT + (TU(I)-TU1(I))**2 + (TD(I)-TD1(I))**2                      
!107   CONTINUE                                                                 
!     EEE = (DDR+DDT)/(CCR+CCT)                                                 
!                                                                               
      ITA = ITA + 1                                                             
      WRITE(6,610) ITA, EEY+EEE, EEY, CCY, EEE                                  
      CALL FLUSH(6)                                                             
!
      DO I=1,M                                                                  
       DO K=I/2+1,I/2+N/2                                                       
        LY = N/2+I/2*2+1-K                                                      
        L = N/2+I-K                                                             
        Y(I,LY) = Y1(I,LY)                                                      
        Z(I, K) = Z1(I, K)                                                      
        W(K, L) = W1(K, L)                                                      
       ENDDO                                                                    
      ENDDO                                                                     
!
!        Boundary Conditions                                                     
!    
      DO I=1,M                                                                  
        Y (I,(I+1)/2+N/2*(1-(-1)**I)/2)= Y (I,(I+1)/2+N/2*(1+(-1)**I)/2)          
        Z (I,(I+1)/2+N/2*(1-(-1)**I)/2)= Z (I,(I+1)/2+N/2*(1+(-1)**I)/2)          
      ENDDO                                                                     
                                                                                
      DO J=1,N/2                                                                
        Y (0,J)= Y (M,M/2+J)                                                      
        Z (0,J)= Z (M,M/2+J)                                                      
      ENDDO                                                                     
                                                                                
      DO J=1,N/2                                                                
        W (J+M/2,NL-J+1)= W (J,N/2-J+1)                                           
      ENDDO                                                                     
               
      DO I=1,M/2                                                                
        W (I,N/2+I)= W (N/2+I,I)                                                  
      ENDDO                                                                     
!                                                                               
!       End of Boundary Conditions
!
      DO 110 I = 1, NS                                                          
      RU(I) = RU1(I)                                                            
      RD(I) = RD1(I)                                                            
110   CONTINUE                                                                  
!                                                                               
      DO I=1,M
       DO K=I/2+1,I/2+N/2
        LY = N/2+I/2*2+1-K
        L = N/2+I-K
        TUYR(I,LY) = TUYR1(I,LY) 
        TUYI(I,LY) = TUYI1(I,LY) 
        TUZR(I, K) = TUZR1(I, K)
        TUZI(I, K) = TUZI1(I, K)
        TUWR(K, L) = TUWR1(K, L)
        TUWI(K, L) = TUWI1(K, L)
        TDYR(I,LY) = TDYR1(I,LY)
        TDYI(I,LY) = TDYI1(I,LY)
        TDZR(I, K) = TDZR1(I, K) 
        TDZI(I, K) = TDZI1(I, K) 
        TDWR(K, L) = TDWR1(K, L) 
        TDWI(K, L) = TDWI1(K, L) 
       ENDDO
      ENDDO
!                                                                               
      IF (EEY+EEE .GT. ERR) GO TO 1000                                          
!                                                                               
! *** End of Iteration **************************                               
!                                                                               
!       Initial velocity is set to be 0.                                        
!                                                                               
      DO I=1,M                                                                  
       DO K=I/2+1,I/2+N/2                                                       
        LY = N/2+I/2*2+1-K                                                      
        L = N/2+I-K                                                             
        YV(I,LY)=0.D0                                                           
        ZV(I, K)=0.D0                                                           
        WV(K, L)=0.D0                                                           
       ENDDO                                                                    
      ENDDO                                                                     
!                                                                               
      DO I=1,NS
       DO J=1,NS
        EVURAA(I,J)=EVUR(I,J)
        EVUIAA(I,J)=EVUI(I,J)
        EVDRAA(I,J)=EVDR(I,J)
        EVDIAA(I,J)=EVDI(I,J)
       ENDDO
      ENDDO
!
      WRITE(11,600)                                                             
      WRITE(11,620) ((Y(I,N/2+I/2*2+1-K),K=I/2+1,I/2+N/2),I=1,M)                
      WRITE(21,602)                                                             
      WRITE(21,620) ((Z(I,            K),K=I/2+1,I/2+N/2),I=1,M)                
      WRITE(31,604)                                                             
      WRITE(31,620) ((W(K,      N/2+I-K),K=I/2+1,I/2+N/2),I=1,M)                
!                                                                               
      WRITE(12,590)                                                             
      WRITE(12,625) (EU(I),I=1,NS)                                              
      WRITE(13,595)                                                             
      WRITE(13,625) (ED(I),I=1,NS)                                              
      CALL FLUSH(11)                                                            
      CALL FLUSH(21)                                                            
      CALL FLUSH(31)                                                            
      CALL FLUSH(12)                                                            
!                                                                               
! *** Calculation of the Energies ***                                           
!                                                                               
      EE=0.D0                                                                   
      EL=0.D0                                                                   
      EDY=0.D0                                                                  
!                                                                               
      DO I=1,M                                                                  
       DO K=I/2+1,I/2+N/2                                                       
        LY = N/2+I/2*2+1-K                                                      
        L = N/2+I-K                                                             
        EL = EL + Y(I,LY)**2 + Z(I,K)**2 + W(K,L)**2                            
       ENDDO                                                                    
      ENDDO                                                                     
      EL = EL*FCNST/ENUN/2.0D0                                                    
!                                                                               
      DO 240 I=1,NE                                                             
       EE = EE + EU(I)*OCU(I) + ED(I)*OCD(I)                                    
240   CONTINUE                                                                  
!
!     Coulomb Energy
!
      UTERMA = 0.D0                                                               
      UTERMB = 0.D0                                                               
      VTERM1 = 0.D0                                                               
      VTERM2 = 0.D0                                                               
!
       DO I=1,M
        DO J=1,N
         IJ=IC(I,J)
         IF(OS(IJ).EQ.1) THEN
          IF((I+J)/2*2.EQ.I+J) THEN
           UTERMA = UTERMA  + RU(MP(IJ))*RD(MP(IJ)) - 0.25D0
          ELSE
           UTERMB = UTERMB  + RU(MP(IJ))*RD(MP(IJ)) - 0.25D0
          ENDIF
         ENDIF
        ENDDO
       ENDDO
!                                                                               
      DO I=1,M
       DO J=1,N
        IJ=IC(I,J)
        IF(OS(IJ).EQ.1) THEN
         I1 = 2*(I+J-(I+J)/2*2)-1
         IR = IC(I, J+1)
         IS = IC(I, J-1)
         IV = IC(I+I1,J)
         IF(OS(IR).EQ.1) VTERM1 = VTERM1 + (RU(MP(IJ))+RD(MP(IJ)))*(RU(MP(IR))+RD(MP(IR))) - 1.D0
         IF(OS(IS).EQ.1) VTERM1 = VTERM1 + (RU(MP(IJ))+RD(MP(IJ)))*(RU(MP(IS))+RD(MP(IS))) - 1.D0
         IF(OS(IV).EQ.1) VTERM1 = VTERM1 + (RU(MP(IJ))+RD(MP(IJ)))*(RU(MP(IV))+RD(MP(IV))) - 1.D0
        ENDIF
       ENDDO
      ENDDO
      VTERM1 = VTERM1/2.0D0
!
      DO I=1,M
       DO K=I/2+1,I/2+N/2
        LY = N/2+I/2*2+1-K
        L = N/2+I-K
        VTERM2 = VTERM2 + DCONJG(ZTUY(I,LY))*ZTUY(I,LY) + DCONJG(ZTDY(I,LY))*ZTDY(I,LY)
        VTERM2 = VTERM2 + DCONJG(ZTUZ(I, K))*ZTUZ(I, K) + DCONJG(ZTDZ(I, K))*ZTDZ(I, K)
        VTERM2 = VTERM2 + DCONJG(ZTUW(K, L))*ZTUW(K, L) + DCONJG(ZTDW(K, L))*ZTDW(K, L)
       ENDDO
      ENDDO
!                                                                               
      EE = EE - UA*UTERMA - UB*UTERMB - V*VTERM1 + V*VTERM2                                   
!                                                                               
      ET = EE + EL + EDY                                                        
      III=0                                                                     
      WRITE(14,580)                                                             
      WRITE(14,660) III,EE,EL,EDY,ET                                            
      CALL FLUSH(14)                                                            
!                                                                               
! *** Charge and Spin Density Calculation                                       
!                                                                               
      DO 400 I=1,NS                                                             
        CHDEN(I) = RU(I) + RD(I)                                                
        SDEN(I)  = 0.5D0*(RU(I)-RD(I))                                          
400   CONTINUE                                                                  
!
      DO I=1,NM
        CHDENNET(I) = 1.0D0
        SDENNET(I)  = 0.0D0
      ENDDO
      DO I=1,NS
        CHDENNET(MP(I)) = CHDEN(I)
        SDENNET (MP(I)) = SDEN (I)
      ENDDO
!
      WRITE(15,630)
      WRITE(15,625) (1.0D0-CHDENNET(I),I=1,NM)
      WRITE(16,635)
      WRITE(16,625) (SDENNET(I),I=1,NM)
      CALL FLUSH(15)
      CALL FLUSH(26)
      CALL FLUSH(16)
!
      RETURN                                                                    
580   FORMAT(/1X,'Time step, Electronic Energy, Lattice Energy, Kinetic Energy, Total Energy',/)
590   FORMAT(/3X,'Electronic Energy Spectrum Up at 0-th step',/)
595   FORMAT(/3X,'Electronic Energy Spectrum Down at 0-th step',/)
600   FORMAT (3X,/,'Lattice Displacement (Bond) Y at 0-th step',/)
602   FORMAT (3X,/,'Lattice Displacement (Bond) Z at 0-th step',/)
604   FORMAT (3X,/,'Lattice Displacement (Bond) W at 0-th step',/)
601   FORMAT (I1)
610   FORMAT (5X,I5,4(5X,D10.5))
!620   FORMAT (<N/2>D19.10)
620   FORMAT (<N>D19.10)
622   FORMAT (<2*N>D19.10)
625   FORMAT (<N  >D19.10)
630   FORMAT (3X,/,'Charge Density at 0-th step',/)
635   FORMAT (3X,'Spin Density at 0-th step',/)
641   FORMAT ( D15.7)
660   FORMAT(1X,I7,4D18.10)
      END                                                                       

!
! ************************************************ HAMIL
!
      SUBROUTINE HAMIL(IT,AUR,AUI,ADR,ADI,A,XC,YC,ALPHA,UA,UB,V,TIMP1,TIMP2,IMP1,IMP2,DELTA0Y,DELTA0Z,DELTA0W,T0,ENUN,TSOA,TSOB,TNNA,TNNB)
      INCLUDE 'PARM.INC'
      IMPLICIT REAL*8 (A-H,O-Y)
      IMPLICIT COMPLEX*16 (Z)
      REAL*8 Z,Z1           
      INTEGER OS,OY,OZ,OW
      DIMENSION AUR(NM,0:NM),AUI(NM,0:NM)
      DIMENSION ADR(NM,0:NM),ADI(NM,0:NM)
      DIMENSION ZEVU(NS,NS),ZEVD(NS,NS)
      DIMENSION TUYR(NI,NL),TUYI(NI,NL),TDYR(NI,NL),TDYI(NI,NL)
      DIMENSION TUZR(NI,NK),TUZI(NI,NK),TDZR(NI,NK),TDZI(NI,NK)
      DIMENSION TUWR(NK,NL),TUWI(NK,NL),TDWR(NK,NL),TDWI(NK,NL)
      COMMON /CHD/  CHDEN(NS),SDEN(NS)
      COMMON /BOR/  BYR(0:NI,NL),BZR(0:NI,NK),BWR(NK,NL)
      COMMON /BOI/  BYI(0:NI,NL),BZI(0:NI,NK),BWI(NK,NL)
      COMMON /LAT/ Y(0:NI,NL),Z(0:NI,NK),W(NK,NL)
      COMMON /ELCUA/ EU(NS),EVUR(NS,NS),EVUI(NS,NS)
      COMMON /ELCDA/ ED(NS),EVDR(NS,NS),EVDI(NS,NS)
      COMMON /RHO/  RU(NS),RD(NS)
      COMMON /TAUU/ ZTUY(NI,NL),ZTUZ(NI,NK),ZTUW(NK,NL)
      COMMON /TAUD/ ZTDY(NI,NL),ZTDZ(NI,NK),ZTDW(NK,NL)
      COMMON /OCC/ OCU(NE),OCD(NE)      
      COMMON /IND/ IC(0:M+1,-1:N+1)
      COMMON /MAP/ MP(NS),IMP(0:NM)
      COMMON /ONT/ OS(NM),OY(0:NI,NL),OZ(0:NI,NK),OW(NK+1,0:NL),NB
!
      ZI = DCMPLX(0.0D0,1.0D0)
!
      AY = A*(XC*1.0D0 + YC*1.0D0)
      AZ = A*(XC*1.0D0 + YC*1.0D0)
      AW = A*YC
!      
      CSY = DCOS(AY)
      SNY = DSIN(AY)
      CSZ = DCOS(AZ)
      SNZ = DSIN(AZ)
      CSW = DCOS(AW)
      SNW = DSIN(AW)
!      
      DO I=1,M                                                                  
       DO K=I/2+1,I/2+N/2                                                       
        LY = N/2+I/2*2+1-K                                                      
        L = N/2+I-K                                                             
        BYR(I,LY)=0.D0                                                           
        BYI(I,LY)=0.D0                                                           
        BZR(I, K)=0.D0                                                           
        BZI(I, K)=0.D0                                                           
        BWR(K, L)=0.D0                                                           
        BWI(K, L)=0.D0                                                           
       ENDDO                                                                    
      ENDDO                                                                     
!
      DO I = 1, NS
       CHDEN(I)= 0.0D0
       SDEN(I) = 0.0D0
      ENDDO
!
      DO 15 I=1, NS
        CHDEN(I) = RU(I) + RD(I)
        SDEN(I)  = 0.5D0*(RU(I)-RD(I))
15    CONTINUE     
!
!       ITOCCON=100	
!       ITOCCOFF=300
!       DEC=6.D0
!       OCU(NU)=0.5D0-0.5D0*DTANH(DEC*(DFLOAT(IT-(ITOCCON+ITOCCOFF)/2))/
!     &                               DFLOAT(ITOCCOFF-ITOCCON))     
!       OCU(NU+1)=1.0D0-OCU(NU)
!
      DO I=1,M  
       DO J=1,N 
        IJ=IC(I,J)
        K=(I+J+1)/2
        L=K-J+N/2
        IP=IMP(IC(I,J-1))
        IF((I+J)/2*2.NE.I+J) THEN
         IF(OY(I,L).EQ.1) THEN
          DO KK=1, NE
           BYR(I,L) = BYR(I,L) + (EVUR(IMP(IJ),KK)*EVUR(IP,KK)+EVUI(IMP(IJ),KK)*EVUI(IP,KK))*OCU(KK) 
           BYR(I,L) = BYR(I,L) + (EVDR(IMP(IJ),KK)*EVDR(IP,KK)+EVDI(IMP(IJ),KK)*EVDI(IP,KK))*OCD(KK)
           BYI(I,L) = BYI(I,L) + (EVUR(IMP(IJ),KK)*EVUI(IP,KK)-EVUI(IMP(IJ),KK)*EVUR(IP,KK))*OCU(KK) 
           BYI(I,L) = BYI(I,L) + (EVDR(IMP(IJ),KK)*EVDI(IP,KK)-EVDI(IMP(IJ),KK)*EVDR(IP,KK))*OCD(KK)
          ENDDO
         ENDIF
        ELSE
         IQ=IMP(IC(I-1,J))
         IF(OZ(I,K).EQ.1) THEN
          DO KK=1, NE
           BZR(I,K) = BZR(I,K) + (EVUR(IMP(IJ),KK)*EVUR(IP,KK)+EVUI(IMP(IJ),KK)*EVUI(IP,KK))*OCU(KK) 
           BZR(I,K) = BZR(I,K) + (EVDR(IMP(IJ),KK)*EVDR(IP,KK)+EVDI(IMP(IJ),KK)*EVDI(IP,KK))*OCD(KK)
           BZI(I,K) = BZI(I,K) + (EVUR(IMP(IJ),KK)*EVUI(IP,KK)-EVUI(IMP(IJ),KK)*EVUR(IP,KK))*OCU(KK) 
           BZI(I,K) = BZI(I,K) + (EVDR(IMP(IJ),KK)*EVDI(IP,KK)-EVDI(IMP(IJ),KK)*EVDR(IP,KK))*OCD(KK)
          ENDDO
         ENDIF
!         
         IF(OW(K,L).EQ.1) THEN
          DO KK=1, NE
           BWR(K,L) = BWR(K,L) + (EVUR(IMP(IJ),KK)*EVUR(IQ,KK)+EVUI(IMP(IJ),KK)*EVUI(IQ,KK))*OCU(KK) 
           BWR(K,L) = BWR(K,L) + (EVDR(IMP(IJ),KK)*EVDR(IQ,KK)+EVDI(IMP(IJ),KK)*EVDI(IQ,KK))*OCD(KK)
           BWI(K,L) = BWI(K,L) + (EVUR(IMP(IJ),KK)*EVUI(IQ,KK)-EVUI(IMP(IJ),KK)*EVUR(IQ,KK))*OCU(KK) 
           BWI(K,L) = BWI(K,L) + (EVDR(IMP(IJ),KK)*EVDI(IQ,KK)-EVDI(IMP(IJ),KK)*EVDR(IQ,KK))*OCD(KK)
          ENDDO
         ENDIF
        ENDIF
       ENDDO
      ENDDO
!
!        Boundary Conditions                                                     
!    
      DO I=1,M                                                                  
       BYR(I,(I+1)/2+N/2*(1-(-1)**I)/2)=BYR(I,(I+1)/2+N/2*(1+(-1)**I)/2)          
       BYI(I,(I+1)/2+N/2*(1-(-1)**I)/2)=BYI(I,(I+1)/2+N/2*(1+(-1)**I)/2)          
       BZR(I,(I+1)/2+N/2*(1-(-1)**I)/2)=BZR(I,(I+1)/2+N/2*(1+(-1)**I)/2)          
       BZI(I,(I+1)/2+N/2*(1-(-1)**I)/2)=BZI(I,(I+1)/2+N/2*(1+(-1)**I)/2)          
      ENDDO                                                                     
                                                                                
      DO J=1,N/2                                                                
       BYR(0,J)=BYR(M,M/2+J)                                                      
       BYI(0,J)=BYI(M,M/2+J)                                                      
       BZR(0,J)=BZR(M,M/2+J)                                                      
       BZI(0,J)=BZI(M,M/2+J)                                                      
      ENDDO                                                                     
                                                                                
      DO J=1,N/2                                                                
       BWR(J+M/2,NL-J+1)=BWR(J,N/2-J+1)                                           
       BWI(J+M/2,NL-J+1)=BWI(J,N/2-J+1)                                           
      ENDDO                                                                     
               
      DO I=1,M/2                                                                
       BWR(I,N/2+I)=BWR(N/2+I,I)                                                  
       BWI(I,N/2+I)=BWI(N/2+I,I)                                                  
      ENDDO                                                                     
!                                                                               
!       End of Boundary Conditions
!
      DO 50 I=1,NS
       DO 52 J=1,NS
        ZEVU(I,J) = DCMPLX(EVUR(I,J),EVUI(I,J))
        ZEVD(I,J) = DCMPLX(EVDR(I,J),EVDI(I,J))
52     CONTINUE
50    CONTINUE
!
      DO 70 I=1,NS
        RU(I)=0.D0
        RD(I)=0.D0
        DO 80 J=1,NE
          RU(I) = RU(I) + DCONJG(ZEVU(I,J))*ZEVU(I,J)*OCU(J)
          RD(I) = RD(I) + DCONJG(ZEVD(I,J))*ZEVD(I,J)*OCD(J)
80      CONTINUE                           
70    CONTINUE
!
      DO I=1,M
       DO J=1,N
        IJ=IC(I,J)
        I1=IC(I,J-1)
        K=(I+J+1)/2
        L=K-J+N/2
        IF((I+J)/2*2.NE.I+J) THEN
         ZTUY(I,L) = (0.0D0,0.0D0)
         ZTDY(I,L) = (0.0D0,0.0D0)
         DO JJ=1,NE
          ZTUY(I,L) = ZTUY(I,L) + DCONJG(ZEVU(IMP(IJ),JJ))*ZEVU(IMP(I1),JJ)*OCU(JJ)*OY(I,L)
          ZTDY(I,L) = ZTDY(I,L) + DCONJG(ZEVD(IMP(IJ),JJ))*ZEVD(IMP(I1),JJ)*OCD(JJ)*OY(I,L)
         ENDDO
        ELSE
         ZTUZ(I,K) = (0.0D0,0.0D0)
         ZTDZ(I,K) = (0.0D0,0.0D0)
         DO JJ=1,NE
          ZTUZ(I,K) = ZTUZ(I,K) + DCONJG(ZEVU(IMP(IJ),JJ))*ZEVU(IMP(I1),JJ)*OCU(JJ)*OZ(I,K)
          ZTDZ(I,K) = ZTDZ(I,K) + DCONJG(ZEVD(IMP(IJ),JJ))*ZEVD(IMP(I1),JJ)*OCD(JJ)*OZ(I,K)
         ENDDO
        ENDIF
       ENDDO
      ENDDO
!     
       DO I=1,M
        IJ =IC(I,1)
        I1 =IC(I,N)
        K=(I+1+1)/2
        L=K-1+N/2
        IF(I/2*2.NE.I) THEN
         ZTUZ(I,K) = (0.0D0,0.0D0)
         ZTDZ(I,K) = (0.0D0,0.0D0)
         DO JJ=1,NE
          ZTUZ(I,K) = ZTUZ(I,K) + DCONJG(ZEVU(IMP(IJ),JJ))*ZEVU(IMP(I1),JJ)*OCU(JJ)*OZ(I,K)
          ZTDZ(I,K) = ZTDZ(I,K) + DCONJG(ZEVD(IMP(IJ),JJ))*ZEVD(IMP(I1),JJ)*OCD(JJ)*OZ(I,K)
         ENDDO
        ELSE
         ZTUY(I,L) = (0.0D0,0.0D0)
         ZTDY(I,L) = (0.0D0,0.0D0)
         DO JJ=1,NE
          ZTUY(I,L) = ZTUY(I,L) + DCONJG(ZEVU(IMP(IJ),JJ))*ZEVU(IMP(I1),JJ)*OCU(JJ)*OY(I,L)
          ZTDY(I,L) = ZTDY(I,L) + DCONJG(ZEVD(IMP(IJ),JJ))*ZEVD(IMP(I1),JJ)*OCD(JJ)*OY(I,L)
         ENDDO
        ENDIF
       ENDDO
!
      DO I=2,M
       DO J=1,N
        IF(2*((I+J)/2).EQ.I+J) THEN
         IJ =IC(I,J)
         I1 =IC(I-1,J)
         K=(I+J+1)/2
         L=K-J+N/2
         ZTUW(K,L) = (0.0D0,0.0D0)
         ZTDW(K,L) = (0.0D0,0.0D0)
         DO JJ=1,NE
          ZTUW(K,L) = ZTUW(K,L) + DCONJG(ZEVU(IMP(IJ),JJ))*ZEVU(IMP(I1),JJ)*OCU(JJ)*OW(K,L)
          ZTDW(K,L) = ZTDW(K,L) + DCONJG(ZEVD(IMP(IJ),JJ))*ZEVD(IMP(I1),JJ)*OCD(JJ)*OW(K,L)
         ENDDO
        ENDIF
       ENDDO
      ENDDO
!     
       DO J=1,N-1,2
        IJ =IC(1,J)
        I1 =IC(M,J)
        K=(1+J+1)/2
        L=K-J+N/2
        ZTUW(K,L) = (0.0D0,0.0D0)
        ZTDW(K,L) = (0.0D0,0.0D0)
        DO JJ=1,NE
         ZTUW(K,L) = ZTUW(K,L) + DCONJG(ZEVU(IMP(IJ),JJ))*ZEVU(IMP(I1),JJ)*OCU(JJ)*OW(K,L)
         ZTDW(K,L) = ZTDW(K,L) + DCONJG(ZEVD(IMP(IJ),JJ))*ZEVD(IMP(I1),JJ)*OCD(JJ)*OW(K,L)
        ENDDO
       ENDDO
!
      DO I=1,M
       DO K=I/2+1,I/2+N/2
        LY = N/2+I/2*2+1-K
        L = N/2+I-K
        TUYR(I,LY) = DIMAG( ZTUY(I,LY)*ZI) 
        TUYI(I,LY) = DIMAG( ZTUY(I,LY)   ) 
        TUZR(I, K) = DIMAG( ZTUZ(I, K)*ZI) 
        TUZI(I, K) = DIMAG( ZTUZ(I, K)   )
        TUWR(K, L) = DIMAG( ZTUW(K, L)*ZI) 
        TUWI(K, L) = DIMAG( ZTUW(K, L)   )
        TDYR(I,LY) = DIMAG( ZTDY(I,LY)*ZI) 
        TDYI(I,LY) = DIMAG( ZTDY(I,LY)   )
        TDZR(I, K) = DIMAG( ZTDZ(I, K)*ZI) 
        TDZI(I, K) = DIMAG( ZTDZ(I, K)   )
        TDWR(K, L) = DIMAG( ZTDW(K, L)*ZI) 
        TDWI(K, L) = DIMAG( ZTDW(K, L)   )
       ENDDO
      ENDDO
!
      DO 55 I=1,NM
       DO 56 J=1,NM
        AUR(I,J) = 0.0D0
        AUI(I,J) = 0.0D0
        ADR(I,J) = 0.0D0
        ADI(I,J) = 0.0D0
56     CONTINUE
55    CONTINUE
!                                                                               
!     Tight Binding
!
      DO I=1,M
       DO J=1,N
        IJ=IC(I,J)
        K=(I+J+1)/2
        L=K-J+N/2
        IF((I+J)/2*2.EQ.I+J) THEN
         AUR(IJ,IC(I,J-1)) = ((-(T0 + DELTA0Z) + ALPHA*Z(I,K))*CSZ - V*TUZR(I,K))*OZ(I,K)
         AUI(IJ,IC(I,J-1)) = (( (T0 + DELTA0Z) - ALPHA*Z(I,K))*SNZ - V*TUZI(I,K))*OZ(I,K)
         ADR(IJ,IC(I,J-1)) = ((-(T0 + DELTA0Z) + ALPHA*Z(I,K))*CSZ - V*TDZR(I,K))*OZ(I,K)
         ADI(IJ,IC(I,J-1)) = (( (T0 + DELTA0Z) - ALPHA*Z(I,K))*SNZ - V*TDZI(I,K))*OZ(I,K)
         AUR(IJ,IC(I-1,J)) = ((-(T0 + DELTA0W) + ALPHA*W(K,L))*CSW - V*TUWR(K,L))*OW(K,L)
         AUI(IJ,IC(I-1,J)) = (( (T0 + DELTA0W) - ALPHA*W(K,L))*SNW - V*TUWI(K,L))*OW(K,L)
         ADR(IJ,IC(I-1,J)) = ((-(T0 + DELTA0W) + ALPHA*W(K,L))*CSW - V*TDWR(K,L))*OW(K,L)
         ADI(IJ,IC(I-1,J)) = (( (T0 + DELTA0W) - ALPHA*W(K,L))*SNW - V*TDWI(K,L))*OW(K,L)
         AUR(IC(I,J-1),IJ) = AUR(IJ,IC(I,J-1))
         AUI(IC(I,J-1),IJ) =-AUI(IJ,IC(I,J-1))
         ADR(IC(I,J-1),IJ) = ADR(IJ,IC(I,J-1))
         ADI(IC(I,J-1),IJ) =-ADI(IJ,IC(I,J-1))
         AUR(IC(I-1,J),IJ) = AUR(IJ,IC(I-1,J))
         AUI(IC(I-1,J),IJ) =-AUI(IJ,IC(I-1,J))
         ADR(IC(I-1,J),IJ) = ADR(IJ,IC(I-1,J))
         ADI(IC(I-1,J),IJ) =-ADI(IJ,IC(I-1,J))
        ELSE
         AUR(IJ,IC(I,J-1)) = ((-(T0 + DELTA0Y) + ALPHA*Y(I,L))*CSY - V*TUYR(I,L))*OY(I,L)
         AUI(IJ,IC(I,J-1)) = (( (T0 + DELTA0Y) - ALPHA*Y(I,L))*SNY - V*TUYI(I,L))*OY(I,L)
         ADR(IJ,IC(I,J-1)) = ((-(T0 + DELTA0Y) + ALPHA*Y(I,L))*CSY - V*TDYR(I,L))*OY(I,L)
         ADI(IJ,IC(I,J-1)) = (( (T0 + DELTA0Y) - ALPHA*Y(I,L))*SNY - V*TDYI(I,L))*OY(I,L)
         AUR(IC(I,J-1),IJ) = AUR(IJ,IC(I,J-1))
         AUI(IC(I,J-1),IJ) =-AUI(IJ,IC(I,J-1))
         ADR(IC(I,J-1),IJ) = ADR(IJ,IC(I,J-1))
         ADI(IC(I,J-1),IJ) =-ADI(IJ,IC(I,J-1))
        ENDIF
       ENDDO
      ENDDO
!                                                                               
!     Spin-orbit interaction 
!
      DO I=1,M
       DO J=1,N
        IJ=IC(I,J)
        K=(I+J+1)/2
        L=K-J+N/2
        IF((I+J)/2*2.EQ.I+J) THEN
         AUI(IJ,IC(I  ,J-2)) =  TSOA*OZ(I,K)*OY(I,L+1)
         AUI(IJ,IC(I-1,J-1)) = -TSOA*OW(K,L)*OY(I-1,L)
         AUI(IJ,IC(I-1,J+1)) =  TSOA*OW(K,L)*OZ(I-1,K)
         ADI(IJ,IC(I  ,J-2)) = -TSOA*OZ(I,K)*OY(I,L+1)
         ADI(IJ,IC(I-1,J-1)) =  TSOA*OW(K,L)*OY(I-1,L)
         ADI(IJ,IC(I-1,J+1)) = -TSOA*OW(K,L)*OZ(I-1,K)
!
         AUI(IC(I  ,J-2),IJ) = -AUI(IJ,IC(I  ,J-2)) 
         AUI(IC(I-1,J-1),IJ) = -AUI(IJ,IC(I-1,J-1))
         AUI(IC(I-1,J+1),IJ) = -AUI(IJ,IC(I-1,J+1))
         ADI(IC(I  ,J-2),IJ) = -ADI(IJ,IC(I  ,J-2))
         ADI(IC(I-1,J-1),IJ) = -ADI(IJ,IC(I-1,J-1))
         ADI(IC(I-1,J+1),IJ) = -ADI(IJ,IC(I-1,J+1))
        ELSE
         AUI(IJ,IC(I  ,J-2)) =  TSOB*OY(I,L)*OZ(I,K-1)
         AUI(IJ,IC(I-1,J-1)) = -TSOB*OY(I,L)*OW(K-1,L)
         AUI(IJ,IC(I-1,J+1)) =  TSOB*OZ(I,K)*OW(K,L-1)
         ADI(IJ,IC(I  ,J-2)) = -TSOB*OY(I,L)*OZ(I,K-1)
         ADI(IJ,IC(I-1,J-1)) =  TSOB*OY(I,L)*OW(K-1,L)
         ADI(IJ,IC(I-1,J+1)) = -TSOB*OZ(I,K)*OW(K,L-1)
!
         AUI(IC(I  ,J-2),IJ) = -AUI(IJ,IC(I  ,J-2))
         AUI(IC(I-1,J-1),IJ) = -AUI(IJ,IC(I-1,J-1))
         AUI(IC(I-1,J+1),IJ) = -AUI(IJ,IC(I-1,J+1))
         ADI(IC(I  ,J-2),IJ) = -ADI(IJ,IC(I  ,J-2))
         ADI(IC(I-1,J-1),IJ) = -ADI(IJ,IC(I-1,J-1))
         ADI(IC(I-1,J+1),IJ) = -ADI(IJ,IC(I-1,J+1))
        ENDIF
       ENDDO
      ENDDO
!                                                                               
!     Next-Nearest-Neighbour Hopping 
!
      DO I=1,M
       DO J=1,N
        IJ=IC(I,J)
        K=(I+J+1)/2
        L=K-J+N/2
        IF((I+J)/2*2.EQ.I+J) THEN
         AUR(IJ,IC(I  ,J-2)) =  TNNA*OZ(I,K)*OY(I,L+1)
         AUR(IJ,IC(I-1,J-1)) =  TNNA*OW(K,L)*OY(I-1,L)
         AUR(IJ,IC(I-1,J+1)) =  TNNA*OW(K,L)*OZ(I-1,K)
         ADR(IJ,IC(I  ,J-2)) =  TNNA*OZ(I,K)*OY(I,L+1)
         ADR(IJ,IC(I-1,J-1)) =  TNNA*OW(K,L)*OY(I-1,L)
         ADR(IJ,IC(I-1,J+1)) =  TNNA*OW(K,L)*OZ(I-1,K)
!
         AUR(IC(I  ,J-2),IJ) =  AUR(IJ,IC(I  ,J-2)) 
         AUR(IC(I-1,J-1),IJ) =  AUR(IJ,IC(I-1,J-1))
         AUR(IC(I-1,J+1),IJ) =  AUR(IJ,IC(I-1,J+1))
         ADR(IC(I  ,J-2),IJ) =  ADR(IJ,IC(I  ,J-2))
         ADR(IC(I-1,J-1),IJ) =  ADR(IJ,IC(I-1,J-1))
         ADR(IC(I-1,J+1),IJ) =  ADR(IJ,IC(I-1,J+1))
        ELSE
         AUR(IJ,IC(I  ,J-2)) =  TNNB*OY(I,L)*OZ(I,K-1)
         AUR(IJ,IC(I-1,J-1)) =  TNNB*OY(I,L)*OW(K-1,L)
         AUR(IJ,IC(I-1,J+1)) =  TNNB*OZ(I,K)*OW(K,L-1)
         ADR(IJ,IC(I  ,J-2)) =  TNNB*OY(I,L)*OZ(I,K-1)
         ADR(IJ,IC(I-1,J-1)) =  TNNB*OY(I,L)*OW(K-1,L)
         ADR(IJ,IC(I-1,J+1)) =  TNNB*OZ(I,K)*OW(K,L-1)
!
         AUR(IC(I  ,J-2),IJ) =  AUR(IJ,IC(I  ,J-2))
         AUR(IC(I-1,J-1),IJ) =  AUR(IJ,IC(I-1,J-1))
         AUR(IC(I-1,J+1),IJ) =  AUR(IJ,IC(I-1,J+1))
         ADR(IC(I  ,J-2),IJ) =  ADR(IJ,IC(I  ,J-2))
         ADR(IC(I-1,J-1),IJ) =  ADR(IJ,IC(I-1,J-1))
         ADR(IC(I-1,J+1),IJ) =  ADR(IJ,IC(I-1,J+1))
        ENDIF
       ENDDO
      ENDDO
!                                                                               
!     Coulomb Interaction
!
      DO I=1,M                                                                  
       DO J=1,N                                                                 
        IJ=IC(I,J)                                                              
        IF(OS(IJ).EQ.1) THEN
         K=(I+J+1)/2                                                             
         L=K-J+N/2                                                               
         I1 = 2*(I+J-(I+J)/2*2)-1
         IR = IC(I, J+1)
         IS = IC(I, J-1)
         IV = IC(I+I1,J)
         IF((I+J)/2*2.EQ.I+J) THEN                                               
          VTERM = 0.D0
          VTERM = VTERM + (RU(IMP(IR)) + RD(IMP(IR)) - 1.D0)*OY(I,L)
          VTERM = VTERM + (RU(IMP(IS)) + RD(IMP(IS)) - 1.D0)*OZ(I,K) 
          VTERM = VTERM + (RU(IMP(IV)) + RD(IMP(IV)) - 1.D0)*OW(K,L) 
          AUR(IJ,IJ) = UA*(RD(IMP(IJ))-0.5D0) + V*VTERM 
          ADR(IJ,IJ) = UA*(RU(IMP(IJ))-0.5D0) + V*VTERM 
         ELSE
          VTERM = 0.D0
          VTERM = VTERM + (RU(IMP(IR)) + RD(IMP(IR)) - 1.D0)*OZ(I,K)     
          VTERM = VTERM + (RU(IMP(IS)) + RD(IMP(IS)) - 1.D0)*OY(I,L)  
          VTERM = VTERM + (RU(IMP(IV)) + RD(IMP(IV)) - 1.D0)*OW(K,L)     
          AUR(IJ,IJ) = UB*(RD(IMP(IJ))-0.5D0) + V*VTERM 
          ADR(IJ,IJ) = UB*(RU(IMP(IJ))-0.5D0) + V*VTERM
         ENDIF
        ENDIF
       ENDDO
      ENDDO
!
       AUR(IMP1,IMP1)=AUR(IMP1,IMP1)+TIMP1
       ADR(IMP1,IMP1)=ADR(IMP1,IMP1)+TIMP1
!
       AUR(IMP2,IMP2)=AUR(IMP2,IMP2)+TIMP2
!      ADR(IMP2,IMP2)=ADR(IMP2,IMP2)+TIMP2
!
      RETURN
      END
!
! ******************************************** SUBROUTINE DIAGON
!
      SUBROUTINE DIAGON(AUR,AUI,ADR,ADI,IT,DELT,TOMEGA,ENUN,FCNST,UA,UB,V)
!
      INCLUDE 'PARM.INC'
      IMPLICIT REAL*8 (A-H,O-Y)
      IMPLICIT COMPLEX*16 (Z)
      REAL*8 Z,Z1          
      REAL*8 RWORKU(2*NU),RWORKD(2*ND)
      COMPLEX*16 VLU(1,NU),VRU(1,NU),WORKU(4*NU),VLD(1,ND),VRD(1,ND),WORKD(4*ND)
      COMPLEX*16 CU(NS,NS),CD(NS,NS),YCU(NU,NU),YCD(ND,ND),YEU(NU),YED(ND),YIELD
      INTEGER OS,OY,OZ,OW
      DIMENSION AUR(NM,0:NM),AUI(NM,0:NM)
      DIMENSION ADR(NM,0:NM),ADI(NM,0:NM)
      DIMENSION BUR(NS,NS),BUI(NS,NS)
      DIMENSION BDR(NS,NS),BDI(NS,NS)
      DIMENSION UV(NS)
      DIMENSION EU(NS),EVUR(NS,NS),EVUI(NS,NS)
      DIMENSION ED(NS),EVDR(NS,NS),EVDI(NS,NS)
      DIMENSION YCUR(1:NS,1:NS),YCUI(1:NS,1:NS)
      DIMENSION YCDR(1:NS,1:NS),YCDI(1:NS,1:NS)
      DIMENSION YEUR(NS),YEUI(NS),YEDR(NS),YEDI(NS)
      DIMENSION CHDENNET(NM),SDENNET(NM)
      DIMENSION FV1(NS),FV2(NS),FM1(2,NS)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INTEGER,PARAMETER :: LWMAX=2*NS-1,LRWORK=3*NS-2
COMPLEX*16 :: A(NS,NS), WORK( LWMAX )
INTEGER :: INFO,LWORK
REAL*8 ::  RWORK( LRWORK )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      COMMON /CHD/  CHDEN(NS),SDEN(NS)
      COMMON /LAT/ Y(0:NI,NL),Z(0:NI,NK),W(NK,NL)
!     COMMON /LATV/ YV(NI,NL),ZV(NI,NK),WV(NK,NL)
      COMMON /ELCUA/ EUA(NS),EVURA(NS,NS),EVUIA(NS,NS)
      COMMON /ELCDA/ EDA(NS),EVDRA(NS,NS),EVDIA(NS,NS)
      COMMON /ELCUAA/ EVURAA(NS,NS),EVUIAA(NS,NS)
      COMMON /ELCDAA/ EVDRAA(NS,NS),EVDIAA(NS,NS)
      COMMON /ENG/  EE0,EL0,EDY0,ET0
      COMMON /RHO/  RU(NS),RD(NS)
      COMMON /TAUU/ ZTUY(NI,NL),ZTUZ(NI,NK),ZTUW(NK,NL)
      COMMON /TAUD/ ZTDY(NI,NL),ZTDZ(NI,NK),ZTDW(NK,NL)
      COMMON /OCC/ OCU(NE),OCD(NE)
      COMMON /IND/ IC(0:M+1,-1:N+1)
      COMMON /MAP/ MP(NS),IMP(0:NM)
      COMMON /ONT/ OS(NM),OY(0:NI,NL),OZ(0:NI,NK),OW(NK+1,0:NL),NB
!
      ZI = DCMPLX(0.0D0,1.0D0)
!
!     Matrix Compaction
!
      DO I=1,NS
       DO J=1,NS
        BUR(I,J)=AUR(MP(I),MP(J))
        BUI(I,J)=AUI(MP(I),MP(J))
        BDR(I,J)=ADR(MP(I),MP(J))
        BDI(I,J)=ADI(MP(I),MP(J))
       ENDDO
      ENDDO
!
!      MATZ = 1
!      CALL CH(NS,NS,BUR,BUI,EU,MATZ,EVUR,EVUI,FV1,FV2,FM1,IERR)
!      MATZ = 1
!      CALL CH(NS,NS,BDR,BDI,ED,MATZ,EVDR,EVDI,FV1,FV2,FM1,IERR)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
A=DCMPLX(BUR,BUI)
LWORK = -1
CALL ZHEEV( 'V', 'L', NS, A, NS, EU, WORK, LWORK, RWORK, INFO )
      LWORK = MIN( LWMAX, INT( WORK( 1 ) ) )
CALL ZHEEV( 'V', 'L', NS, A, NS, EU, WORK, LWORK, RWORK, INFO )
EVUR=DREAL(A)
EVUI=DIMAG(A)
A=DCMPLX(BDR,BDI)
CALL ZHEEV( 'V', 'L', NS, A, NS, ED, WORK, LWORK, RWORK, INFO )
EVDR=DREAL(A)
EVDI=DIMAG(A)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! *** calculating energies ***
!
      IF(IT-IT/IDUM*IDUM.EQ.0) THEN
       DO 100 I=1,NS
        DO 101 J=1,NS
         CU(I,J)=0.D0
         CD(I,J)=0.D0
101     CONTINUE
100    CONTINUE
       DO 102 I=1,NS
        EUA(I)=0.D0
        EDA(I)=0.D0
102    CONTINUE
       DO 110 I=1,NS
        DO 111 J=1,NS
         DO 112 K=1,NS
          CU(I,J) = CU(I,J) + DCMPLX(EVUR (K,J),-EVUI (K,J))* DCMPLX(EVURA(K,I), EVUIA(K,I))
          CD(I,J) = CD(I,J) + DCMPLX(EVDR (K,J),-EVDI (K,J))* DCMPLX(EVDRA(K,I), EVDIA(K,I))
112      CONTINUE
111     CONTINUE
110    CONTINUE
       DO 120 I=1,NS
        DO 121 J=1,NS
         EUA(I) = EUA(I) + EU(J)*CDABS(CU(I,J))*CDABS(CU(I,J))
         EDA(I) = EDA(I) + ED(J)*CDABS(CD(I,J))*CDABS(CD(I,J))
121     CONTINUE
120    CONTINUE
      DO I=1,NS
       UV(I) = 0.0D0
      ENDDO 
!
!      AN  = DFLOAT(N)
!      UV1=0.D0
!      DO 200 I=1,L
!       UV1 = UV1 - (AN*.5D0-DFLOAT(I))/AN*YV(I)
!200   CONTINUE
!      UV(1) = 0.D0
!      DO 210 I=2,N
!       UV(I) = UV(I-1) + YV(I-1)
!210   CONTINUE
!      DO 220 I=1,L
!       UV(I) = UV(I) + UV1
!220   CONTINUE
!
      EE=0.D0
      EL=0.D0
      EDY=0.D0
!
      DO I=1,M
       DO K=I/2+1,I/2+N/2
        LY = N/2+I/2*2+1-K
        L = N/2+I-K
        EL = EL + Y(I,LY)**2 + Z(I,K)**2 + W(K,L)**2
       ENDDO
      ENDDO
!
      DO 230 I=1,NS
       EDY = EDY + UV(I)*UV(I)
230   CONTINUE
      DO 235 I=1,NE
       EE = EE + EUA(I)*OCU(I) + EDA(I)*OCD(I) 
235   CONTINUE
!
      UTERMA=0.D0                                                               
      UTERMB=0.D0                                                               
      VTERM1=0.D0                                                               
      VTERM2=0.D0                                                               
!
       DO I=1,M
        DO J=1,N
         IJ=IC(I,J)
         IF(OS(IJ).EQ.1) THEN
          IF((I+J)/2*2.EQ.I+J) THEN
           UTERMA = UTERMA  + RU(MP(IJ))*RD(MP(IJ)) - 0.25D0
          ELSE
           UTERMB = UTERMB  + RU(MP(IJ))*RD(MP(IJ)) - 0.25D0
          ENDIF
         ENDIF
        ENDDO
       ENDDO
!                                                                               
      DO I=1,M
       DO J=1,N
        IJ=IC(I,J)
        IF(OS(IJ).EQ.1) THEN
         I1 = 2*(I+J-(I+J)/2*2)-1
         IR = IC(I, J+1)
         IS = IC(I, J-1)
         IV = IC(I+I1,J)
         IF(OS(IR).EQ.1) VTERM1 = VTERM1 + (RU(MP(IJ))+RD(MP(IJ)))*(RU(MP(IR))+RD(MP(IR))) - 1.D0
         IF(OS(IS).EQ.1) VTERM1 = VTERM1 + (RU(MP(IJ))+RD(MP(IJ)))*(RU(MP(IS))+RD(MP(IS))) - 1.D0
         IF(OS(IV).EQ.1) VTERM1 = VTERM1 + (RU(MP(IJ))+RD(MP(IJ)))*(RU(MP(IV))+RD(MP(IV))) - 1.D0
        ENDIF
       ENDDO
      ENDDO
      VTERM1 = VTERM1/2.0D0
!
      DO I=1,M
       DO K=I/2+1,I/2+N/2
        LY = N/2+I/2*2+1-K
        L = N/2+I-K
        VTERM2 = VTERM2 + DCONJG(ZTUY(I,LY))*ZTUY(I,LY) + DCONJG(ZTDY(I,LY))*ZTDY(I,LY) 
        VTERM2 = VTERM2 + DCONJG(ZTUZ(I, K))*ZTUZ(I, K) + DCONJG(ZTDZ(I, K))*ZTDZ(I, K)  
        VTERM2 = VTERM2 + DCONJG(ZTUW(K, L))*ZTUW(K, L) + DCONJG(ZTDW(K, L))*ZTDW(K, L)
       ENDDO
      ENDDO
!                                                                               
      EE = EE - UA*UTERMA - UB*UTERMB - V*VTERM1 + V*VTERM2
      AMASS = 4.D0*FCNST
      EE = EE - EE0
      EL = EL*FCNST/ENUN/2.0D0 - EL0
      EDY = EDY*AMASS/ENUN/2.0D0 - EDY0
      ET = EE + EL + EDY
      WRITE(6,660) IT,EE,EL,EDY,ET
      WRITE(14,660) IT,EE,EL,EDY,ET
      WRITE(12,585) IT
      WRITE(12,625) (EU(I),I=1,NS)
      WRITE(13,586) IT
      WRITE(13,625) (ED(I),I=1,NS)
      CALL FLUSH( 6)
      CALL FLUSH(12)
      CALL FLUSH(13)
      CALL FLUSH(14)
!
! *** writing charge and spin densities ***
!
      DO I=1,NM
        CHDENNET(I) = 1.0D0
        SDENNET(I)  = 0.0D0
      ENDDO
      DO I=1,NS
        CHDENNET(MP(I)) = CHDEN(I)
        SDENNET (MP(I)) = SDEN (I)
      ENDDO
!
      WRITE(15,630) IT
      WRITE(15,625) (1.0D0-CHDENNET(I),I=1,NM)
      WRITE(16,635) IT
      WRITE(16,625) (SDENNET(I),I=1,NM)
      END IF
      DO I = 1, NS
       DO J = 1, NS
        EVURAA(I,J) = EVURA(I,J)
        EVUIAA(I,J) = EVUIA(I,J)
        EVDRAA(I,J) = EVDRA(I,J)
        EVDIAA(I,J) = EVDIA(I,J)
       ENDDO   
      ENDDO   
      CALL FLUSH(15)
      CALL FLUSH(16)
!
! *** calculating electronic wavefunctions ***
!
      DT = DELT*TOMEGA
       DO 400 I = 1, NS
        DO 401 J = 1, NS
         CU(I,J) = 0.0D0
         CD(I,J) = 0.0D0
401     CONTINUE
400    CONTINUE
      DO 410 I = 1, NS
       DO 411 J = 1, NS
        DO 412 K = 1, NS
         CU(I,J) = CU(I,J) + DCMPLX(EVUR (K,I),-EVUI (K,I))*DCMPLX(EVURA(K,J), EVUIA(K,J))
         CD(I,J) = CD(I,J) + DCMPLX(EVDR (K,I),-EVDI (K,I))*DCMPLX(EVDRA(K,J), EVDIA(K,J))
412     CONTINUE
411    CONTINUE
410   CONTINUE
      DO 420 I=1, NS
       DO 421 J=1, NS
        CU(I,J) = CU(I,J)*DCMPLX(DCOS(EU(I)*DT),-DSIN(EU(I)*DT))
        CD(I,J) = CD(I,J)*DCMPLX(DCOS(ED(I)*DT),-DSIN(ED(I)*DT))
421    CONTINUE
420   CONTINUE
      DO 430 I = 1, NS
       DO 431 J = 1, NS
        EVURA(I,J) = 0.0D0
        EVUIA(I,J) = 0.0D0
        EVDRA(I,J) = 0.0D0
        EVDIA(I,J) = 0.0D0
431    CONTINUE
430   CONTINUE
      DO 440 I = 1, NS
       DO 441 J = 1, NS
        DO 442 K = 1, NS
         EVURA(I,J)=EVURA(I,J)+DIMAG(ZI*DCMPLX(EVUR(I,K),EVUI(I,K))*CU(K,J))
         EVUIA(I,J)=EVUIA(I,J)+DIMAG(   DCMPLX(EVUR(I,K),EVUI(I,K))*CU(K,J))
         EVDRA(I,J)=EVDRA(I,J)+DIMAG(ZI*DCMPLX(EVDR(I,K),EVDI(I,K))*CD(K,J))
         EVDIA(I,J)=EVDIA(I,J)+DIMAG(   DCMPLX(EVDR(I,K),EVDI(I,K))*CD(K,J))
442     CONTINUE
441    CONTINUE
440   CONTINUE
!
! *** calculating yielding ***
!
!      IF(IT-IT/IDUM*IDUM.EQ.0) THEN
!       DO I = 1, NU
!        DO J = 1, NU
!         YCU(I,J) = 0.0D0
!        ENDDO   
!       ENDDO   
!       DO I = 1, ND
!        DO J = 1, ND
!         YCD(I,J) = 0.0D0
!        ENDDO   
!       ENDDO   
!       II = 1
!       DO I = 1, NE
!        IF(OCU(I).EQ.1.0D0) THEN
!         JJ = 1 
!         DO J = 1, NE
!          IF(OCU(J).EQ.1.0D0) THEN
!           DO K = 1, NS
!            YCU(II,JJ) = YCU(II,JJ) + DCMPLX(EVURA(K,I),EVUIA(K,I)) * DCMPLX(EVURAA(K,J),-EVUIAA(K,J))
!           ENDDO   
!           JJ = JJ+1 
!          ENDIF
!         ENDDO   
!         II = II+1
!        ENDIF
!       ENDDO   
!       II = 1
!       DO I = 1, NE
!        IF(OCD(I).EQ.1.0D0) THEN
!         JJ = 1 
!         DO J = 1, NE
!          IF(OCD(J).EQ.1.0D0) THEN
!           DO K = 1, NS
!            YCD(II,JJ) = YCD(II,JJ) + DCMPLX(EVDRA(K,I),EVDIA(K,I)) * DCMPLX(EVDRAA(K,J),-EVDIAA(K,J))
!           ENDDO   
!           JJ = JJ+1 
!          ENDIF
!         ENDDO   
!         II = II+1
!        ENDIF
!       ENDDO   
! 
!       LDA   = NU
!       LWORKU   = 4*NU
!      CALL ZGEEV('N','N',NU,YCU,LDA,YEU,VLU,1,VRU,1,WORKU,LWORKU,RWORKU,INFO )
!       LDA   = ND
!       LWORKD   = 4*ND
!      CALL ZGEEV('N','N',ND,YCD,LDA,YED,VLD,1,VRD,1,WORKD,LWORKD,RWORKD,INFO )
!
!       YIELD = DCMPLX(1.0D0,0.0D0)
!       DO I = 1, NU
!        YIELD = YIELD * YEU(I) 
!       ENDDO
!       DO I = 1, ND
!        YIELD = YIELD * YED(I) 
!       ENDDO
!       YIELDIT = YIELD*DCONJG(YIELD)
! 
!       WRITE(25,641) YIELDIT  
!
!      ENDIF
!
      RETURN
585   FORMAT(/3X,'Elect. Energy Spectrum Up  EU  at ',I7,'-th step',/)
586   FORMAT(/3X,'Elect. Energy Spectrum Down ED  at ',I7,'-th step',/)
590   FORMAT(/3X,'Electronic Energy Spectrum <E> at ',I7,'-th step',/)
600   FORMAT(/3X,'Lattice Displacement (Bond) at ',I7,'-th step',/)
630   FORMAT(/'  Charge Density at ',I7,'-th step',/)
635   FORMAT(/'  Spin Density at ',I7,'-th step',/)
620   FORMAT(<N/2>D19.10)                                                          
625   FORMAT(<N  >D19.10)                                                          
641   FORMAT ( D15.7)
660   FORMAT(1X,I7,4D18.10)
      END
!
! ******************************************** SUBROUTINE DYNAM
!
      SUBROUTINE DYNAM(ENUN,FCNST,ALP,A,XC,YC,DELT,T,IT)
!
      INCLUDE 'PARM.INC' 
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER OS,OY,OZ,OW
      DIMENSION YELEC(NI,NL),YACC(NI,NL)
      DIMENSION ZELEC(NI,NK),ZACC(NI,NK)
      DIMENSION WELEC(NK,NL),WACC(NK,NL)
      COMMON /LAT/ Y(0:NI,NL),Z(0:NI,NK),W(NK,NL)
      COMMON /LATV/ YV(NI,NL),ZV(NI,NK),WV(NK,NL)
      COMMON /LAT1/ Y1(0:NI,NL),Z1(0:NI,NK),W1(NK,NL),YV1(NI,NL),ZV1(NI,NK),WV1(NK,NL)
      COMMON /BOR/  BYR(0:NI,NL),BZR(0:NI,NK),BWR(NK,NL)
      COMMON /BOI/  BYI(0:NI,NL),BZI(0:NI,NK),BWI(NK,NL)
      COMMON /IND/ IC(0:M+1,-1:N+1)
      COMMON /MAP/ MP(NS),IMP(0:NM)
      COMMON /ONT/ OS(NM),OY(0:NI,NL),OZ(0:NI,NK),OW(NK+1,0:NL),NB
      COMMON /PAR/ PY(NI,NL,5),PZ(NI,NK,5),PW(NK,NL,5)
!
!                                                                               
!       Initial velocity is set to be 0.                                        
!                                                                               
      IF(IT.EQ.1) THEN
       DO I=1,M                                                                  
        DO K=I/2+1,I/2+N/2                                                       
         LY = N/2+I/2*2+1-K                                                      
         L = N/2+I-K                                                             
         YV(I,LY)=0.D0                                                           
         ZV(I, K)=0.D0                                                           
         WV(K, L)=0.D0                                                           
        ENDDO                                                                    
       ENDDO                                                                     
      ENDIF                                                                          
!
      GAMMAL = 0.01D0
      IF(T.EQ.0.D0) GAMMAL = 0.D0
      FCNSTL = FCNST/ENUN
      AKB = 3.4D-5
      FL = 0.25D0*DSQRT((6.0D0*GAMMAL*AKB*T)/(FCNSTL*DELT))
      IDUN = -10
!
      RAMDA=ALP/FCNST*2.D0
!      
      AY = A*(XC*1.0D0 + YC*1.0D0)
      AZ = A*(XC*1.0D0 + YC*1.0D0)
      AW = A*YC
!      
      CSY = DCOS(AY)
      SNY = DSIN(AY)
      CSZ = DCOS(AZ)
      SNZ = DSIN(AZ)
      CSW = DCOS(AW)
      SNW = DSIN(AW)
!
      DO I=1,M
       DO K=I/2+1,I/2+N/2
        LY = N/2+I/2*2+1-K
        L = N/2+I-K
        YELEC(I,LY) = 0.0D0
        ZELEC(I, K) = 0.0D0
        WELEC(K, L) = 0.0D0
        YACC (I,LY) = 0.0D0
        ZACC (I, K) = 0.0D0
        WACC (K, L) = 0.0D0
       ENDDO
      ENDDO
!
      DO I=1,M
       DO K=I/2+1,I/2+N/2
        LY = N/2+I/2*2+1-K
        L = N/2+I-K
        I1=(-1)**I
         IF(OY(I,LY).EQ.1) YELEC(I,LY) = (PY(I,LY,1)*BZR(I,K) + PY(I,LY,2)*BWR(K,LY) + PY(I,LY,3)*BZR(I,K-I1) + &
                                          PY(I,LY,4)*BWR(K-I1,LY) + PY(I,LY,5)*BYR(I,LY))*CSY                   &
                                       - (PY(I,LY,1)*BZI(I,K) + PY(I,LY,2)*BWI(K,LY) + PY(I,LY,3)*BZI(I,K-I1) + &
                                          PY(I,LY,4)*BWI(K-I1,LY) + PY(I,LY,5)*BYI(I,LY))*SNY

         IF(OZ(I, K).EQ.1) ZELEC(I, K) = (PZ(I, K,1)*BWR(K,L) + PZ(I, K,2)*BYR(I, L) + PZ(I, K,3)*BWR(K, L+1) + &
                                          PZ(I, K,4)*BYR(I,  L+1) + PZ(I, K,5)*BZR(I, K))*CSZ                   &
                                       - (PZ(I, K,1)*BWI(K,L) + PZ(I, K,2)*BYI(I, L) + PZ(I, K,3)*BWI(K, L+1) + &
                                          PZ(I, K,4)*BYI(I,  L+1) + PZ(I, K,5)*BZI(I, K))*SNZ

         IF(OW(K, L).EQ.1) WELEC(K, L) = (PW(K, L,1)*BYR(I,L) + PW(K, L,2)*BZR(I, K) + PW(K, L,3)*BYR(I-1, L) + &
                                          PW(K, L,4)*BZR(I-1,  K) + PW(K, L,5)*BWR(K, L))*CSW                   &
                                       - (PW(K, L,1)*BYI(I,L) + PW(K, L,2)*BZI(I, K) + PW(K, L,3)*BYI(I-1, L) + &
                                          PW(K, L,4)*BZI(I-1,  K) + PW(K, L,5)*BWI(K, L))*SNW
       ENDDO
      ENDDO
!
      DO I=1,M
       DO K=I/2+1,I/2+N/2
        LY = N/2+I/2*2+1-K
        L = N/2+I-K
        I1=(-1)**I
        IF(OY(I,LY).EQ.1)   YACC(I,LY) = 0.0625D0*(PY(I,LY,1)*Z(I,K) + PY(I,LY,2)*W(K,LY) + PY(I,LY,3)*Z(I,K-I1) + &
                                                 PY(I,LY,4)*W(K-I1,LY) + PY(I,LY,5)*Y(I,LY) + RAMDA*YELEC(I,LY)) &
                                       - GAMMAL*YV(I,LY) + FL*ANAN(IDUN) 

        IF(OZ(I, K).EQ.1)   ZACC(I, K) = 0.0625D0*(PZ(I, K,1)*W(K,L) + PZ(I, K,2)*Y(I, L) + PZ(I, K,3)*W(K, L+1) + &
                                                 PZ(I, K,4)*Y(I,  L+1) + PZ(I, K,5)*Z(I, K) + RAMDA*ZELEC(I, K)) &
                                       - GAMMAL*ZV(I, K) + FL*ANAN(IDUN) 

        IF(OW(K, L).EQ.1)   WACC(K, L) = 0.0625D0*(PW(K, L,1)*Y(I,L) + PW(K, L,2)*Z(I, K) + PW(K, L,3)*Y(I-1, L) + &
                                                 PW(K, L,4)*Z(I-1,  K) + PW(K, L,5)*W(K, L) + RAMDA*WELEC(K, L)) &
                                       - GAMMAL*WV(K, L) + FL*ANAN(IDUN) 
       ENDDO
      ENDDO
!
      DO I=1,M
       DO K=I/2+1,I/2+N/2
        LY = N/2+I/2*2+1-K
        L = N/2+I-K
!
        IF(OY(I,LY).EQ.1)       YV1(I,LY)=YV(I,LY)+YACC(I,LY)*DELT
        IF(OZ(I, K).EQ.1)       ZV1(I, K)=ZV(I, K)+ZACC(I, K)*DELT
        IF(OW(K, L).EQ.1)       WV1(K, L)=WV(K, L)+WACC(K, L)*DELT
!
        IF(OY(I,LY).EQ.1)       Y1(I,LY)=Y(I,LY)+YV(I,LY)*DELT
        IF(OZ(I, K).EQ.1)       Z1(I, K)=Z(I, K)+ZV(I, K)*DELT
        IF(OW(K, L).EQ.1)       W1(K, L)=W(K, L)+WV(K, L)*DELT
       ENDDO
      ENDDO
!
      RETURN
      END
!
! ******************************************** SUBROUTINE NEW
!
      SUBROUTINE NEW(IT)
!
      INCLUDE 'PARM.INC'
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /LAT/ Y(0:NI,NL),Z(0:NI,NK),W(NK,NL)
      COMMON /LATV/ YV(NI,NL),ZV(NI,NK),WV(NK,NL)
      COMMON /LAT1/ Y1(0:NI,NL),Z1(0:NI,NK),W1(NK,NL),YV1(NI,NL),ZV1(NI,NK),WV1(NK,NL)
!
      DO I=1,M
       DO K=I/2+1,I/2+N/2
        LY = N/2+I/2*2+1-K
        L = N/2+I-K

        Y(I,LY)=Y1(I,LY)
        Z(I,K)=Z1(I,K)
        W(K,L)=W1(K,L)

        YV(I,LY)=YV1(I,LY)
        ZV(I,K)=ZV1(I,K)
        WV(K,L)=WV1(K,L)

       ENDDO
      ENDDO
!
!        Boundary Conditions                                                     
!    
      DO I=1,M                                                                  
        Y (I,(I+1)/2+N/2*(1-(-1)**I)/2)= Y (I,(I+1)/2+N/2*(1+(-1)**I)/2)          
        Z (I,(I+1)/2+N/2*(1-(-1)**I)/2)= Z (I,(I+1)/2+N/2*(1+(-1)**I)/2)          
      ENDDO                                                                     
                                                                                
      DO J=1,N/2                                                                
        Y (0,J)= Y (M,M/2+J)                                                      
        Z (0,J)= Z (M,M/2+J)                                                      
      ENDDO                                                                     
                                                                                
      DO J=1,N/2                                                                
        W (J+M/2,NL-J+1)= W (J,N/2-J+1)                                           
      ENDDO                                                                     
               
      DO I=1,M/2                                                                
        W (I,N/2+I)= W (N/2+I,I)                                                  
      ENDDO                                                                     
!                                                                               
!       End of Boundary Conditions
!
      IF(IT-IT/IDUM*IDUM.EQ.0) THEN
       WRITE(6,6000) IT
       WRITE(11,610) IT
       WRITE(11,600) ((Y(I,N/2+I/2*2+1-K),K=I/2+1,I/2+N/2),I=1,M)
       WRITE(21,615) IT
       WRITE(21,600) ((Z(I,            K),K=I/2+1,I/2+N/2),I=1,M)
       WRITE(31,620) IT
       WRITE(31,600) ((W(K,      N/2+I-K),K=I/2+1,I/2+N/2),I=1,M)
       CALL FLUSH( 6)
       CALL FLUSH(11)
       CALL FLUSH(21)
       CALL FLUSH(31)
      ENDIF
!
      RETURN
600   FORMAT(<N/2>D19.10)
605   FORMAT(/' Bond order at ',I7,'-th step',/)
610   FORMAT(/' Bond order Y at ',I7,'-th step',/)
615   FORMAT(/' Bond order Z at ',I7,'-th step',/)
620   FORMAT(/' Bond order W at ',I7,'-th step',/)
6000  FORMAT(3X,' Time step = ',I7)
      END
!                                                                               
! ******************************************** FUNCTION ANAN                    
!                                                                               
!     *******************                                                       
      FUNCTION anan(idum)                                                       
!     *******************                                                       
      implicit real*8(a-h,o-z)                                                  
                                                                                
      a=0.0                                                                     
      b=1.0                                                                     
                                                                                
      pi=ACOS(-1.0)                                                             
                                                                                
                                                                                
      x1=ran1(idum)                                                             
      x2=ran1(idum)                                                             
                                                                                
      y1=sqrt(-2*log(x1))*cos(2*pi*x2)                                          
                                                                                
      anan = a + y1*b                                                           
                                                                                
      return                                                                    
                                                                                
      end                                                                       
!     *******************                                                       
      FUNCTION ran1(idum)                                                       
!     *******************                                                       
                                                                                
      integer idum,iaa,im,iq,ir,ntab,ndiv                                       
      real*8 ran1,am,eps,rnmx                                                   
      parameter (iaa=16807,im=2147483647,am=1./im,iq=127773,ir=2836,ntab=32,ndiv=1+(im-1)/ntab,eps=1.2e-7,rnmx=1.-eps)                   
      integer j,k,iv(ntab),iy                                                   
      save iv,iy                                                                
      data iv /ntab*0/, iy /0/                                                  
                                                                                
      if (idum.le.0.or.iy.eq.0) then                                            
         idum=max(-idum,1)                                                      
         do 11 j=ntab+8,1,-1                                                    
	    k=idum/iq                                                                  
	    idum=iaa*(idum-k*iq)-ir*k                                                  
	    if (idum.lt.0) idum=idum+im                                                
	    if (j.le.ntab) iv(j)=idum                                                  
11       continue                                                               
         iy=iv(1)                                                               
      endif                                                                     
      k=idum/iq                                                                 
      idum=iaa*(idum-k*iq)-ir*k                                                 
      if (idum.lt.0) idum=idum+im                                               
      j=1+iy/ndiv                                                               
      iy=iv(j)                                                                  
      iv(j)=idum                                                                
      ran1=min(am*iy,rnmx)                                                      
      return                                                                    
      END                                                                       
!                                                                               
! *****************************************************************             
!                                                                               
      subroutine ch(nm,n,ar,ai,w,matz,zr,zi,fv1,fv2,fm1,ierr)                   
!                                                                               
      integer i,j,n,nm,ierr,matz                                                
      double precision ar(nm,n),ai(nm,n),w(n),zr(nm,n),zi(nm,n),fv1(n),fv2(n),fm1(2,n)                                             
!                                                                               
!     this subroutine calls the recommended sequence of                         
!     subroutines from the eigensystem subroutine package (eispack)             
!     to find the eigenvalues and eigenvectors (if desired)                     
!     of a complex hermitian matrix.                                            
!                                                                               
!     on input                                                                  
!                                                                               
!        nm  must be set to the row dimension of the two-dimensional            
!        array parameters as declared in the calling program                    
!        dimension statement.                                                   
!                                                                               
!        n  is the order of the matrix  a=(ar,ai).                              
!                                                                               
!        ar  and  ai  contain the real and imaginary parts,                     
!        respectively, of the complex hermitian matrix.                         
!                                                                               
!        matz  is an integer variable set equal to zero if                      
!        only eigenvalues are desired.  otherwise it is set to                  
!        any non-zero integer for both eigenvalues and eigenvectors.            
!                                                                               
!     on output                                                                 
!                                                                               
!        w  contains the eigenvalues in ascending order.                        
!                                                                               
!        zr  and  zi  contain the real and imaginary parts,                     
!        respectively, of the eigenvectors if matz is not zero.                 
!                                                                               
!        ierr  is an integer output variable set equal to an error              
!           completion code described in the documentation for tqlrat           
!           and tql2.  the normal completion code is zero.                      
!                                                                               
!        fv1, fv2, and  fm1  are temporary storage arrays.                      
!                                                                               
!     questions and comments should be directed to burton s. garbow,            
!     mathematics and computer science div, argonne national laboratory         
!                                                                               
!     this version dated august 1983.                                           
!                                                                               
!     ------------------------------------------------------------------        
!                                                                               
      if (n .le. nm) go to 10                                                   
      ierr = 10 * n                                                             
      go to 50                                                                  
!                                                                               
   10 call  htridi(nm,n,ar,ai,w,fv1,fv2,fm1)                                    
      if (matz .ne. 0) go to 20                                                 
!     .......... find eigenvalues only ..........                               
      call  tqlrat(n,w,fv2,ierr)                                                
      go to 50                                                                  
!     .......... find both eigenvalues and eigenvectors ..........              
   20 do 40 i = 1, n                                                            
!                                                                               
         do 30 j = 1, n                                                         
            zr(j,i) = 0.0d0                                                     
   30    continue                                                               
!                                                                               
         zr(i,i) = 1.0d0                                                        
   40 continue                                                                  
!                                                                               
      call  tql2(nm,n,w,fv1,zr,ierr)                                            
      if (ierr .ne. 0) go to 50                                                 
      call  htribk(nm,n,ar,ai,fm1,n,zr,zi)                                      
   50 return                                                                    
      end                                                                       
      double precision function epslon (x)                                      
      double precision x                                                        
!                                                                               
!     estimate unit roundoff in quantities of size x.                           
!                                                                               
      double precision a,b,c,eps                                                
!                                                                               
!     this program should function properly on all systems                      
!     satisfying the following two assumptions,                                 
!        1.  the base used in representing floating point                       
!            numbers is not a power of three.                                   
!        2.  the quantity  a  in statement 10 is represented to                 
!            the accuracy used in floating point variables                      
!            that are stored in memory.                                         
!     the statement number 10 and the go to 10 are intended to                  
!     force optimizing compilers to generate code satisfying                    
!     assumption 2.                                                             
!     under these assumptions, it should be true that,                          
!            a  is not exactly equal to four-thirds,                            
!            b  has a zero for its last bit or digit,                           
!            c  is not exactly equal to one,                                    
!            eps  measures the separation of 1.0 from                           
!                 the next larger floating point number.                        
!     the developers of eispack would appreciate being informed                 
!     about any systems where these assumptions do not hold.                    
!                                                                               
!     this version dated 4/6/83.                                                
!                                                                               
      a = 4.0d0/3.0d0                                                           
   10 b = a - 1.0d0                                                             
      c = b + b + b                                                             
      eps = dabs(c-1.0d0)                                                       
      if (eps .eq. 0.0d0) go to 10                                              
      epslon = eps*dabs(x)                                                      
      return                                                                    
      end                                                                       
      subroutine htribk(nm,n,ar,ai,tau,m,zr,zi)                                 
!                                                                               
      integer i,j,k,l,m,n,nm                                                    
      double precision ar(nm,n),ai(nm,n),tau(2,n),zr(nm,m),zi(nm,m)             
      double precision h,s,si                                                   
!                                                                               
!     this subroutine is a translation of a complex analogue of                 
!     the algol procedure trbak1, num. math. 11, 181-195(1968)                  
!     by martin, reinsch, and wilkinson.                                        
!     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).           
!                                                                               
!     this subroutine forms the eigenvectors of a complex hermitian             
!     matrix by back transforming those of the corresponding                    
!     real symmetric tridiagonal matrix determined by  htridi.                  
!                                                                               
!     on input                                                                  
!                                                                               
!        nm must be set to the row dimension of two-dimensional                 
!          array parameters as declared in the calling program                  
!          dimension statement.                                                 
!                                                                               
!        n is the order of the matrix.                                          
!                                                                               
!        ar and ai contain information about the unitary trans-                 
!          formations used in the reduction by  htridi  in their                
!          full lower triangles except for the diagonal of ar.                  
!                                                                               
!        tau contains further information about the transformations.            
!                                                                               
!        m is the number of eigenvectors to be back transformed.                
!                                                                               
!        zr contains the eigenvectors to be back transformed                    
!          in its first m columns.                                              
!                                                                               
!     on output                                                                 
!                                                                               
!        zr and zi contain the real and imaginary parts,                        
!          respectively, of the transformed eigenvectors                        
!          in their first m columns.                                            
!                                                                               
!     note that the last component of each returned vector                      
!     is real and that vector euclidean norms are preserved.                    
!                                                                               
!     questions and comments should be directed to burton s. garbow,            
!     mathematics and computer science div, argonne national laboratory         
!                                                                               
!     this version dated august 1983.                                           
!                                                                               
!     ------------------------------------------------------------------        
!                                                                               
      if (m .eq. 0) go to 200                                                   
!     .......... transform the eigenvectors of the real symmetric               
!                tridiagonal matrix to those of the hermitian                   
!                tridiagonal matrix. ..........                                 
      do 50 k = 1, n                                                            
!                                                                               
         do 50 j = 1, m                                                         
            zi(k,j) = -zr(k,j) * tau(2,k)                                       
            zr(k,j) = zr(k,j) * tau(1,k)                                        
   50 continue                                                                  
!                                                                               
      if (n .eq. 1) go to 200                                                   
!     .......... recover and apply the householder matrices ..........          
      do 140 i = 2, n                                                           
         l = i - 1                                                              
         h = ai(i,i)                                                            
         if (h .eq. 0.0d0) go to 140                                            
!                                                                               
         do 130 j = 1, m                                                        
            s = 0.0d0                                                           
            si = 0.0d0                                                          
!                                                                               
            do 110 k = 1, l                                                     
               s = s + ar(i,k) * zr(k,j) - ai(i,k) * zi(k,j)                    
               si = si + ar(i,k) * zi(k,j) + ai(i,k) * zr(k,j)                  
  110       continue                                                            
!     .......... double divisions avoid possible underflow ..........           
            s = (s / h) / h                                                     
            si = (si / h) / h                                                   
!                                                                               
            do 120 k = 1, l                                                     
               zr(k,j) = zr(k,j) - s * ar(i,k) - si * ai(i,k)                   
               zi(k,j) = zi(k,j) - si * ar(i,k) + s * ai(i,k)                   
  120       continue                                                            
!                                                                               
  130    continue                                                               
!                                                                               
  140 continue                                                                  
!                                                                               
  200 return                                                                    
      end                                                                       
      subroutine htridi(nm,n,ar,ai,d,e,e2,tau)                                  
!                                                                               
      integer i,j,k,l,n,ii,nm,jp1                                               
      double precision ar(nm,n),ai(nm,n),d(n),e(n),e2(n),tau(2,n)               
      double precision f,g,h,fi,gi,hh,si,scale,pythag                           
!                                                                               
!     this subroutine is a translation of a complex analogue of                 
!     the algol procedure tred1, num. math. 11, 181-195(1968)                   
!     by martin, reinsch, and wilkinson.                                        
!     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).           
!                                                                               
!     this subroutine reduces a complex hermitian matrix                        
!     to a real symmetric tridiagonal matrix using                              
!     unitary similarity transformations.                                       
!                                                                               
!     on input                                                                  
!                                                                               
!        nm must be set to the row dimension of two-dimensional                 
!          array parameters as declared in the calling program                  
!          dimension statement.                                                 
!                                                                               
!        n is the order of the matrix.                                          
!                                                                               
!        ar and ai contain the real and imaginary parts,                        
!          respectively, of the complex hermitian input matrix.                 
!          only the lower triangle of the matrix need be supplied.              
!                                                                               
!     on output                                                                 
!                                                                               
!        ar and ai contain information about the unitary trans-                 
!          formations used in the reduction in their full lower                 
!          triangles.  their strict upper triangles and the                     
!          diagonal of ar are unaltered.                                        
!                                                                               
!        d contains the diagonal elements of the the tridiagonal matrix.        
!                                                                               
!        e contains the subdiagonal elements of the tridiagonal                 
!          matrix in its last n-1 positions.  e(1) is set to zero.              
!                                                                               
!        e2 contains the squares of the corresponding elements of e.            
!          e2 may coincide with e if the squares are not needed.                
!                                                                               
!        tau contains further information about the transformations.            
!                                                                               
!     calls pythag for  dsqrt(a*a + b*b) .                                      
!                                                                               
!     questions and comments should be directed to burton s. garbow,            
!     mathematics and computer science div, argonne national laboratory         
!                                                                               
!     this version dated august 1983.                                           
!                                                                               
!     ------------------------------------------------------------------        
!                                                                               
      tau(1,n) = 1.0d0                                                          
      tau(2,n) = 0.0d0                                                          
!                                                                               
      do 100 i = 1, n                                                           
  100 d(i) = ar(i,i)                                                            
!     .......... for i=n step -1 until 1 do -- ..........                       
      do 300 ii = 1, n                                                          
         i = n + 1 - ii                                                         
         l = i - 1                                                              
         h = 0.0d0                                                              
         scale = 0.0d0                                                          
         if (l .lt. 1) go to 130                                                
!     .......... scale row (algol tol then not needed) ..........               
         do 120 k = 1, l                                                        
  120    scale = scale + dabs(ar(i,k)) + dabs(ai(i,k))                          
!                                                                               
         if (scale .ne. 0.0d0) go to 140                                        
         tau(1,l) = 1.0d0                                                       
         tau(2,l) = 0.0d0                                                       
  130    e(i) = 0.0d0                                                           
         e2(i) = 0.0d0                                                          
         go to 290                                                              
!                                                                               
  140    do 150 k = 1, l                                                        
            ar(i,k) = ar(i,k) / scale                                           
            ai(i,k) = ai(i,k) / scale                                           
            h = h + ar(i,k) * ar(i,k) + ai(i,k) * ai(i,k)                       
  150    continue                                                               
!                                                                               
         e2(i) = scale * scale * h                                              
         g = dsqrt(h)                                                           
         e(i) = scale * g                                                       
         f = pythag(ar(i,l),ai(i,l))                                            
!     .......... form next diagonal element of matrix t ..........              
         if (f .eq. 0.0d0) go to 160                                            
         tau(1,l) = (ai(i,l) * tau(2,i) - ar(i,l) * tau(1,i)) / f               
         si = (ar(i,l) * tau(2,i) + ai(i,l) * tau(1,i)) / f                     
         h = h + f * g                                                          
         g = 1.0d0 + g / f                                                      
         ar(i,l) = g * ar(i,l)                                                  
         ai(i,l) = g * ai(i,l)                                                  
         if (l .eq. 1) go to 270                                                
         go to 170                                                              
  160    tau(1,l) = -tau(1,i)                                                   
         si = tau(2,i)                                                          
         ar(i,l) = g                                                            
  170    f = 0.0d0                                                              
!                                                                               
         do 240 j = 1, l                                                        
            g = 0.0d0                                                           
            gi = 0.0d0                                                          
!     .......... form element of a*u ..........                                 
            do 180 k = 1, j                                                     
               g = g + ar(j,k) * ar(i,k) + ai(j,k) * ai(i,k)                    
               gi = gi - ar(j,k) * ai(i,k) + ai(j,k) * ar(i,k)                  
  180       continue                                                            
!                                                                               
            jp1 = j + 1                                                         
            if (l .lt. jp1) go to 220                                           
!                                                                               
            do 200 k = jp1, l                                                   
               g = g + ar(k,j) * ar(i,k) - ai(k,j) * ai(i,k)                    
               gi = gi - ar(k,j) * ai(i,k) - ai(k,j) * ar(i,k)                  
  200       continue                                                            
!     .......... form element of p ..........                                   
  220       e(j) = g / h                                                        
            tau(2,j) = gi / h                                                   
            f = f + e(j) * ar(i,j) - tau(2,j) * ai(i,j)                         
  240    continue                                                               
!                                                                               
         hh = f / (h + h)                                                       
!     .......... form reduced a ..........                                      
         do 260 j = 1, l                                                        
            f = ar(i,j)                                                         
            g = e(j) - hh * f                                                   
            e(j) = g                                                            
            fi = -ai(i,j)                                                       
            gi = tau(2,j) - hh * fi                                             
            tau(2,j) = -gi                                                      
!                                                                               
            do 260 k = 1, j                                                     
               ar(j,k) = ar(j,k) - f * e(k) - g * ar(i,k) + fi * tau(2,k) + gi * ai(i,k)                 
               ai(j,k) = ai(j,k) - f * tau(2,k) - g * ai(i,k)  - fi * e(k) - gi * ar(i,k)                     
  260    continue                                                               
!                                                                               
  270    do 280 k = 1, l                                                        
            ar(i,k) = scale * ar(i,k)                                           
            ai(i,k) = scale * ai(i,k)                                           
  280    continue                                                               
!                                                                               
         tau(2,l) = -si                                                         
  290    hh = d(i)                                                              
         d(i) = ar(i,i)                                                         
         ar(i,i) = hh                                                           
         ai(i,i) = scale * dsqrt(h)                                             
  300 continue                                                                  
!                                                                               
      return                                                                    
      end                                                                       
      double precision function pythag(a,b)                                     
      double precision a,b                                                      
!                                                                               
!     finds dsqrt(a**2+b**2) without overflow or destructive underflow          
!                                                                               
      double precision p,r,s,t,u                                                
      p = dmax1(dabs(a),dabs(b))                                                
      if (p .eq. 0.0d0) go to 20                                                
      r = (dmin1(dabs(a),dabs(b))/p)**2                                         
   10 continue                                                                  
         t = 4.0d0 + r                                                          
         if (t .eq. 4.0d0) go to 20                                             
         s = r/t                                                                
         u = 1.0d0 + 2.0d0*s                                                    
         p = u*p                                                                
         r = (s/u)**2 * r                                                       
      go to 10                                                                  
   20 pythag = p                                                                
      return                                                                    
      end                                                                       
      subroutine tql2(nm,n,d,e,z,ierr)                                          
!                                                                               
      integer i,j,k,l,m,n,ii,l1,l2,nm,mml,ierr                                  
      double precision d(n),e(n),z(nm,n)                                        
      double precision c,c2,c3,dl1,el1,f,g,h,p,r,s,s2,tst1,tst2,pythag          
!                                                                               
!     this subroutine is a translation of the algol procedure tql2,             
!     num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and             
!     wilkinson.                                                                
!     handbook for auto. comp., vol.ii-linear algebra, 227-240(1971).           
!                                                                               
!     this subroutine finds the eigenvalues and eigenvectors                    
!     of a symmetric tridiagonal matrix by the ql method.                       
!     the eigenvectors of a full symmetric matrix can also                      
!     be found if  tred2  has been used to reduce this                          
!     full matrix to tridiagonal form.                                          
!                                                                               
!     on input                                                                  
!                                                                               
!        nm must be set to the row dimension of two-dimensional                 
!          array parameters as declared in the calling program                  
!          dimension statement.                                                 
!                                                                               
!        n is the order of the matrix.                                          
!                                                                               
!        d contains the diagonal elements of the input matrix.                  
!                                                                               
!        e contains the subdiagonal elements of the input matrix                
!          in its last n-1 positions.  e(1) is arbitrary.                       
!                                                                               
!        z contains the transformation matrix produced in the                   
!          reduction by  tred2, if performed.  if the eigenvectors              
!          of the tridiagonal matrix are desired, z must contain                
!          the identity matrix.                                                 
!                                                                               
!      on output                                                                
!                                                                               
!        d contains the eigenvalues in ascending order.  if an                  
!          error exit is made, the eigenvalues are correct but                  
!          unordered for indices 1,2,...,ierr-1.                                
!                                                                               
!        e has been destroyed.                                                  
!                                                                               
!        z contains orthonormal eigenvectors of the symmetric                   
!          tridiagonal (or full) matrix.  if an error exit is made,             
!          z contains the eigenvectors associated with the stored               
!          eigenvalues.                                                         
!                                                                               
!        ierr is set to                                                         
!          zero       for normal return,                                        
!          j          if the j-th eigenvalue has not been                       
!                     determined after 30 iterations.                           
!                                                                               
!     calls pythag for  dsqrt(a*a + b*b) .                                      
!                                                                               
!     questions and comments should be directed to burton s. garbow,            
!     mathematics and computer science div, argonne national laboratory         
!                                                                               
!     this version dated august 1983.                                           
!                                                                               
!     ------------------------------------------------------------------        
!                                                                               
      ierr = 0                                                                  
      if (n .eq. 1) go to 1001                                                  
!                                                                               
      do 100 i = 2, n                                                           
  100 e(i-1) = e(i)                                                             
!                                                                               
      f = 0.0d0                                                                 
      tst1 = 0.0d0                                                              
      e(n) = 0.0d0                                                              
!                                                                               
      do 240 l = 1, n                                                           
         j = 0                                                                  
         h = dabs(d(l)) + dabs(e(l))                                            
         if (tst1 .lt. h) tst1 = h                                              
!     .......... look for small sub-diagonal element ..........                 
         do 110 m = l, n                                                        
            tst2 = tst1 + dabs(e(m))                                            
            if (tst2 .eq. tst1) go to 120                                       
!     .......... e(n) is always zero, so there is no exit                       
!                through the bottom of the loop ..........                      
  110    continue                                                               
!                                                                               
  120    if (m .eq. l) go to 220                                                
  130    if (j .eq. 30) go to 1000                                              
         j = j + 1                                                              
!     .......... form shift ..........                                          
         l1 = l + 1                                                             
         l2 = l1 + 1                                                            
         g = d(l)                                                               
         p = (d(l1) - g) / (2.0d0 * e(l))                                       
         r = pythag(p,1.0d0)                                                    
         d(l) = e(l) / (p + dsign(r,p))                                         
         d(l1) = e(l) * (p + dsign(r,p))                                        
         dl1 = d(l1)                                                            
         h = g - d(l)                                                           
         if (l2 .gt. n) go to 145                                               
!                                                                               
         do 140 i = l2, n                                                       
  140    d(i) = d(i) - h                                                        
!                                                                               
  145    f = f + h                                                              
!     .......... ql transformation ..........                                   
         p = d(m)                                                               
         c = 1.0d0                                                              
         c2 = c                                                                 
         el1 = e(l1)                                                            
         s = 0.0d0                                                              
         mml = m - l                                                            
!     .......... for i=m-1 step -1 until l do -- ..........                     
         do 200 ii = 1, mml                                                     
            c3 = c2                                                             
            c2 = c                                                              
            s2 = s                                                              
            i = m - ii                                                          
            g = c * e(i)                                                        
            h = c * p                                                           
            r = pythag(p,e(i))                                                  
            e(i+1) = s * r                                                      
            s = e(i) / r                                                        
            c = p / r                                                           
            p = c * d(i) - s * g                                                
            d(i+1) = h + s * (c * g + s * d(i))                                 
!     .......... form vector ..........                                         
            do 180 k = 1, n                                                     
               h = z(k,i+1)                                                     
               z(k,i+1) = s * z(k,i) + c * h                                    
               z(k,i) = c * z(k,i) - s * h                                      
  180       continue                                                            
!                                                                               
  200    continue                                                               
!                                                                               
         p = -s * s2 * c3 * el1 * e(l) / dl1                                    
         e(l) = s * p                                                           
         d(l) = c * p                                                           
         tst2 = tst1 + dabs(e(l))                                               
         if (tst2 .gt. tst1) go to 130                                          
  220    d(l) = d(l) + f                                                        
  240 continue                                                                  
!     .......... order eigenvalues and eigenvectors ..........                  
      do 300 ii = 2, n                                                          
         i = ii - 1                                                             
         k = i                                                                  
         p = d(i)                                                               
!                                                                               
         do 260 j = ii, n                                                       
            if (d(j) .ge. p) go to 260                                          
            k = j                                                               
            p = d(j)                                                            
  260    continue                                                               
!                                                                               
         if (k .eq. i) go to 300                                                
         d(k) = d(i)                                                            
         d(i) = p                                                               
!                                                                               
         do 280 j = 1, n                                                        
            p = z(j,i)                                                          
            z(j,i) = z(j,k)                                                     
            z(j,k) = p                                                          
  280    continue                                                               
!                                                                               
  300 continue                                                                  
!                                                                               
      go to 1001                                                                
!     .......... set error -- no convergence to an                              
!                eigenvalue after 30 iterations ..........                      
 1000 ierr = l                                                                  
 1001 return                                                                    
      end                                                                       
!**** for old version, "send otqlrat from eispack"                              
!** From dana!moler Tue, 1 Sep 87 10:15:40 PDT                                  
!** New TQLRAT                                                                  
      SUBROUTINE TQLRAT(N,D,E2,IERR)                                            
!                                                                               
      INTEGER I,J,L,M,N,II,L1,MML,IERR                                          
      DOUBLE PRECISION D(N),E2(N)                                               
      DOUBLE PRECISION B,C,F,G,H,P,R,S,T,EPSLON,PYTHAG                          
!                                                                               
!     This subroutine is a translation of the Algol procedure tqlrat,           
!     Algorithm 464, Comm. ACM 16, 689(1973) by Reinsch.                        
!                                                                               
!     This subroutine finds the eigenvalues of a symmetric                      
!     tridiagonal matrix by the rational QL method.                             
!                                                                               
!     On input                                                                  
!                                                                               
!        N is the order of the matrix.                                          
!                                                                               
!        D contains the diagonal elements of the input matrix.                  
!                                                                               
!        E2 contains the squares of the subdiagonal elements of the             
!          input matrix in its last N-1 positions.  E2(1) is arbitrary.         
!                                                                               
!      On output                                                                
!                                                                               
!        D contains the eigenvalues in ascending order.  If an                  
!          error exit is made, the eigenvalues are correct and                  
!          ordered for indices 1,2,...IERR-1, but may not be                    
!          the smallest eigenvalues.                                            
!                                                                               
!        E2 has been destroyed.                                                 
!                                                                               
!        IERR is set to                                                         
!          zero       for normal return,                                        
!          J          if the J-th eigenvalue has not been                       
!                     determined after 30 iterations.                           
!                                                                               
!     Calls PYTHAG for  DSQRT(A*A + B*B) .                                      
!                                                                               
!     Questions and comments should be directed to Burton S. Garbow,            
!     Mathematics and Computer Science Div, Argonne National Laboratory         
!                                                                               
!     This version dated August 1987.                                           
!     Modified by C. Moler to fix underflow/overflow difficulties,              
!     especially on the VAX and other machines where epslon(1.0d0)**2           
!     nearly underflows.  See the loop involving statement 102 and              
!     the two statements just before statement 200.                             
!                                                                               
!     ------------------------------------------------------------------        
!                                                                               
      IERR = 0                                                                  
      IF (N .EQ. 1) GO TO 1001                                                  
!                                                                               
      DO 100 I = 2, N                                                           
  100 E2(I-1) = E2(I)                                                           
!                                                                               
      F = 0.0D0                                                                 
      T = 0.0D0                                                                 
      E2(N) = 0.0D0                                                             
!                                                                               
      DO 290 L = 1, N                                                           
         J = 0                                                                  
         H = DABS(D(L)) + DSQRT(E2(L))                                          
         IF (T .GT. H) GO TO 105                                                
         T = H                                                                  
         B = EPSLON(T)                                                          
         C = B * B                                                              
         if (c .ne. 0.0d0) go to 105                                            
!        Spliting tolerance underflowed.  Look for larger value.                
         do 102 i = l, n                                                        
            h = dabs(d(i)) + dsqrt(e2(i))                                       
            if (h .gt. t) t = h                                                 
  102    continue                                                               
         b = epslon(t)                                                          
         c = b * b                                                              
!     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT ..........         
  105    DO 110 M = L, N                                                        
            IF (E2(M) .LE. C) GO TO 120                                         
!     .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT                      
!                THROUGH THE BOTTOM OF THE LOOP ..........                      
  110    CONTINUE                                                               
!                                                                               
  120    IF (M .EQ. L) GO TO 210                                                
  130    IF (J .EQ. 30) GO TO 1000                                              
         J = J + 1                                                              
!     .......... FORM SHIFT ..........                                          
         L1 = L + 1                                                             
         S = DSQRT(E2(L))                                                       
         G = D(L)                                                               
         P = (D(L1) - G) / (2.0D0 * S)                                          
         R = PYTHAG(P,1.0D0)                                                    
         D(L) = S / (P + DSIGN(R,P))                                            
         H = G - D(L)                                                           
!                                                                               
         DO 140 I = L1, N                                                       
  140    D(I) = D(I) - H                                                        
!                                                                               
         F = F + H                                                              
!     .......... RATIONAL QL TRANSFORMATION ..........                          
         G = D(M)                                                               
         IF (G .EQ. 0.0D0) G = B                                                
         H = G                                                                  
         S = 0.0D0                                                              
         MML = M - L                                                            
!     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........                     
         DO 200 II = 1, MML                                                     
            I = M - II                                                          
            P = G * H                                                           
            R = P + E2(I)                                                       
            E2(I+1) = S * R                                                     
            S = E2(I) / R                                                       
            D(I+1) = H + S * (H + D(I))                                         
            G = D(I) - E2(I) / G                                                
!           Avoid division by zero on next pass                                 
            if (g .eq. 0.0d0) g = epslon(d(i))                                  
            h = g * (p / r)                                                     
  200    CONTINUE                                                               
!                                                                               
         E2(L) = S * G                                                          
         D(L) = H                                                               
!     .......... GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST ..........         
         IF (H .EQ. 0.0D0) GO TO 210                                            
         IF (DABS(E2(L)) .LE. DABS(C/H)) GO TO 210                              
         E2(L) = H * E2(L)                                                      
         IF (E2(L) .NE. 0.0D0) GO TO 130                                        
  210    P = D(L) + F                                                           
!     .......... ORDER EIGENVALUES ..........                                   
         IF (L .EQ. 1) GO TO 250                                                
!     .......... FOR I=L STEP -1 UNTIL 2 DO -- ..........                       
         DO 230 II = 2, L                                                       
            I = L + 2 - II                                                      
            IF (P .GE. D(I-1)) GO TO 270                                        
            D(I) = D(I-1)                                                       
  230    CONTINUE                                                               
!                                                                               
  250    I = 1                                                                  
  270    D(I) = P                                                               
  290 CONTINUE                                                                  
!                                                                               
      GO TO 1001                                                                
!     .......... SET ERROR -- NO CONVERGENCE TO AN                              
!                EIGENVALUE AFTER 30 ITERATIONS ..........                      
 1000 IERR = L                                                                  
 1001 RETURN                                                                    
      END                                                                       
      REAL*8 FUNCTION DABSCD(AR,AI)
!     ABSOLUTE VALUE OF A COMPLEX NUMBER C=AR+I*AI
!     DABSCD=DSQRT(AR**2+AI**2)
      REAL*8 AR,AI,XR,XI,W
      XR=DABS(AR)
      XI=DABS(AI)
      IF(XR.LE.XI) THEN
      W=XR
      XR=XI
      XI=W
      ENDIF
      IF(XI.EQ.0.D0) THEN
      DABSCD=XR
      ELSE
      DABSCD=XR*DSQRT(1.D0+(XI/XR)**2)
      ENDIF
      RETURN
      END

      SUBROUTINE DIVCD(AR,AI,BR,BI,ZR,ZI)
!     COMPLEX DIVISION Z=ZR+I*ZI=(AR+I*AI)/(BR+I*BI)
!     DO NOT USE IF BR=BI=0.
      REAL*8 AR,AI,BR,BI,YR,YI,ZR,ZI,W
      YR=BR
      YI=BI
      IF(DABS(YR).GT.DABS(YI)) THEN
      W=YI/YR
      YR=W*YI+YR
      ZR=(AR+W*AI)/YR
      ZI=(AI-W*AR)/YR
      ELSE
      W=YR/YI
      YI=W*YR+YI
      ZR=(W*AR+AI)/YI
      ZI=(W*AI-AR)/YI
      ENDIF
      RETURN
      END

      SUBROUTINE COMEIG (NDIM,N,NN,A,Z,T,U,IER,EN)
!--------------------------------------------------------------------------------
!     THIS SUBROUTINE CALCULATES THE EIGENVALUES/EIGENVECTORS OF A COMPLEX MATRIX 
!     C = A+I*Z BY THE JACOBI METHOD, THIS METHOD IS ALSO RECOMMANDED TO DEAL WITH 
!     REAL MATRICES THE EIGENVALUES OF WHICH ARE COMPLEX.

!     DATA:
!     NDIM    1ST DIMENSION OF TABLES A, Z, T, U IN MAIN PROGRAM (HERE NDIM=N)
!     N       REAL SIZE OF COMPLEX MATRIX C = A+I*Z
!     NN      MAXIMUM NUMBER OF ITERATIONS
!     A       TABLE STORING THE REAL PART OF GIVEN MATRIX
!     Z       TABLE STORING THE IMAGINARY PART OF GIVEN MATRIX

!     OUTPUTS:
!     A(J,J),Z(J,J),J=1,N   IN MAIN DIAGONALS OF TABLES A AND Z, YOU
!             HAVE NOW RESPECTIVELY THE REAL AND IMAGINARY PARTS OF EIGENVALUES.
!     T,U     THESE TABLES CONTAIN NOW RESPECTIVELY THE REAL AND IMAGINARY PARTS
!             OF THE EIGENVECTORS MATRIX X = T+I*U (STORED IN COLUMNS).
!     IER     ERROR CODE
!             = 0  CONVERGENCE OK
!             = 1  NO CONVERGENCE AFTER NN ITERATIONS

!     WORKING ZONE:
!     EN      TABLE OF SIZE 2*N

!     NOTES:
!     1/      IN CASE OF CONVERGENCE (IER = 0), THE MATRIX EQUATION  C*X = LAMDA*X
!             IS VERIFIED TO THE MACHINE PRECISION.
!
!     REFERENCE:
!     P.J.EBERLEIN, NUMER.MATH 14, PP 232-245 (1970)
!---------------------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(NDIM,*),Z(NDIM,*),T(NDIM,*),U(NDIM,*),EN(*)
      LOGICAL MARK

!     CHECK MACHINE EPSILON (AROUND 1.2E-16 FOR PC)

      EPS = 1.0
   10 EPS = 0.5*EPS
      EPS1 = EPS+1.0
      IF (EPS1.GT.1.0) GO TO 10
      MARK = .FALSE.

!     INITIALIZE EIGENVECTORS

      DO I = 1,N
      T(I,I) = 1.0
      U(I,I) = 0.0
        DO J = I+1,N
        T(I,J) = 0.0
        T(J,I) = 0.0
        U(I,J) = 0.0
        U(J,I) = 0.0
        ENDDO
      ENDDO
      IT = 0
   20 IT = IT+1

!     SAFETY TEST IN CASE OF NO CONVERGENCE

      IF (IT.GT.NN) GO TO 90
      IF (MARK)     GO TO 95

!     DEFINE CONVERGENCE CRITERIUM

      TAU = 0.0
      DO K = 1,N
      W1 = 0.0
        DO I = 1,N
        IF (I.NE.K) W1 = W1+DABS(A(I,K))+DABS(Z(I,K))
        ENDDO
      TAU = TAU+W1
      EN(K) = W1+DABS(A(K,K))+DABS(Z(K,K))
      ENDDO

!     PERMUTE  LINES AND COLUMNS

      DO K = 1,N-1
      EMAX = EN(K)
      I = K
        DO J = K+1,N
        IF (EN(J).GT.EMAX) THEN
        EMAX = EN(J)
        I = J
        ENDIF
        ENDDO
      IF (I.NE.K) THEN
      EN(I) = EN(K)
        DO J = 1,N
        W2 = A(K,J)
        A(K,J) = A(I,J)
        A(I,J) = W2
        W2 = Z(K,J)
        Z(K,J) = Z(I,J)
        Z(I,J) = W2
        ENDDO
        DO J = 1,N
        W2 = A(J,K)
        A(J,K) = A(J,I)
        A(J,I) = W2
        W2 = Z(J,K)
        Z(J,K) = Z(J,I)
        Z(J,I) = W2
        W2 = T(J,K)
        T(J,K) = T(J,I)
        T(J,I) = W2
        W2 = U(J,K)
        U(J,K) = U(J,I)
        U(J,I) = W2
        ENDDO
      END IF
      ENDDO

!     CONVERGENCE IF TAU < 100*EPS

      IF (TAU.LT.100.0*EPS) GO TO 95

!     BEGIN ITERATIONS

      MARK = .TRUE.
      DO K = 1,N-1
      DO M = K+1,N
      G = 0.0
      HR = 0.0
      HJ = 0.0
      HI = 0.0
        DO I = 1,N

        IF (I.NE.K.AND.I.NE.M) THEN

        HR = HR+A(K,I)*A(M,I)+Z(K,I)*Z(M,I)-A(I,K)*A(I,M)-Z(I,K)*Z(I,M)
        HI = HI+Z(K,I)*A(M,I)-A(K,I)*Z(M,I)-A(I,K)*Z(I,M)+Z(I,K)*A(I,M)
        T1 = A(I,K)*A(I,K)+Z(I,K)*Z(I,K)+A(M,I)*A(M,I)+Z(M,I)*Z(M,I)
        T2 = A(I,M)*A(I,M)+Z(I,M)*Z(I,M)+A(K,I)*A(K,I)+Z(K,I)*Z(K,I)
        G = G+T1+T2
        HJ = HJ-T1+T2
        ENDIF
        ENDDO
      BR = A(K,M)+A(M,K)
      BI = Z(K,M)+Z(M,K)
      ER = A(K,M)-A(M,K)
      EI = Z(K,M)-Z(M,K)
      DR = A(K,K)-A(M,M)
      DI = Z(K,K)-Z(M,M)
      T1 = BR*BR+EI*EI+DR*DR
      T2 = BI*BI+ER*ER+DI*DI

      IF (T1.GE.T2) THEN

      SW = 1.0
      C = BR
      S = EI
      D = DR
      DE = DI
      ROOT2 = SQRT(T1)
      ELSE
      SW =-1.0
      C = BI
      S =-ER
      D = DI
      DE = DR
      ROOT2 = SQRT(T2)
      ENDIF
      ROOT1 = SQRT(S*S+C*C)
      SIG = 1.0
      IF (D.LT.0.0) SIG = -1.0
      CA = 1.0
      IF (C.LT.0.0) CA = -1.0
      SA = 0.0

      IF (ROOT1.LT.EPS) THEN

      SX = 0.0
      SA = 0.0
      CX = 1.0
      CA = 1.0

      IF (SW.GT.0.0) THEN
      E = ER
      B = BI
      ELSE
      E = EI
      B =-BR
      ENDIF
      DN = D*D+DE*DE
      GO TO 65
      ENDIF

      IF (DABS(S).GT.EPS) THEN

      CA = C/ROOT1
      SA = S/ROOT1
      ENDIF
      COT2X = D/ROOT1
      COTX = COT2X+SIG*SQRT(1.0+COT2X*COT2X)
      SX = SIG/SQRT(1.0+COTX*COTX)
      CX = SX*COTX
      ETA = (ER*BR+BI*EI)/ROOT1
      TSE = (BR*BI-ER*EI)/ROOT1
      T1 = SIG*(TSE*D-ROOT1*DE)/ROOT2
      T2 = (D*DE+ROOT1*TSE)/ROOT2
      DN = ROOT2*ROOT2+T2*T2
      T2 = HJ*CX*SX
      COS2A = CA*CA-SA*SA
      SIN2A = 2.0*CA*SA
      W1 = HR*COS2A+HI*SIN2A
      W2 = HI*COS2A-HR*SIN2A
      HR = CX*CX*HR-SX*SX*W1-CA*T2
      HI = CX*CX*HI+SX*SX*W2-SA*T2
      B = SW*T1*CA+ETA*SA
      E = CA*ETA-SW*T1*SA

!     ROOT1 < EPS

   65 S = HR-SIG*ROOT2*E
      C = HI-SIG*ROOT2*B
      ROOT = DSQRT(C*C+S*S)

      IF (ROOT.LT.EPS) THEN

      CB = 1.0
      CH = 1.0
      SB = 0.0
      SH = 0.0
      GO TO 70
      END IF
      CB = -C/ROOT
      SB =  S/ROOT
      T2 = CB*B-E*SB
      CN = T2*T2
      TANH = ROOT/(G+2.0*(CN+DN))
      CH = 1.0/DSQRT(1.0-TANH*TANH)
      SH = CH*TANH

!     ROOT < EPS

   70 W1 = SX*SH*(SA*CB-SB*CA)
      C1R = CX*CH-W1
      C2R = CX*CH+W1
      C1I =-SX*SH*(CA*CB+SA*SB)
      C2I = C1I
      W2 = SX*CH*CA
      W1 = CX*SH*SB
      S1R = W2-W1
      S2R =-W2-W1
      W2 = SX*CH*SA
      W1 = CX*SH*CB
      S1I = W2+W1
      S2I = W2-W1
      W1 = SQRT(S1R*S1R+S1I*S1I)
      W2 = SQRT(S2R*S2R+S2I*S2I)

      IF (W1.GT.EPS.OR.W2.GT.EPS) THEN

!     BEGIN TRANSFORMATIONS

      MARK = .FALSE.
      DO I = 1,N
      AKI = A(K,I)
      AMI = A(M,I)
      ZKI = Z(K,I)
      ZMI = Z(M,I)
      A(K,I) = C1R*AKI-C1I*ZKI+S1R*AMI-S1I*ZMI
      Z(K,I) = C1R*ZKI+C1I*AKI+S1R*ZMI+S1I*AMI
      A(M,I) = S2R*AKI-S2I*ZKI+C2R*AMI-C2I*ZMI
      Z(M,I) = S2R*ZKI+S2I*AKI+C2R*ZMI+C2I*AMI
      ENDDO
      DO I = 1,N
      AIK = A(I,K)
      AIM = A(I,M)
      ZIK = Z(I,K)
      ZIM = Z(I,M)
      TIK = T(I,K)
      TIM = T(I,M)
      UIK = U(I,K)
      UIM = U(I,M)
      A(I,K) = C2R*AIK-C2I*ZIK-S2R*AIM+S2I*ZIM
      Z(I,K) = C2R*ZIK+C2I*AIK-S2R*ZIM-S2I*AIM
      A(I,M) = C1R*AIM-C1I*ZIM-S1R*AIK+S1I*ZIK
      Z(I,M) = C1R*ZIM+C1I*AIM-S1R*ZIK-S1I*AIK
      T(I,K) = C2R*TIK-C2I*UIK-S2R*TIM+S2I*UIM
      U(I,K) = C2R*UIK+C2I*TIK-S2R*UIM-S2I*TIM
      T(I,M) = C1R*TIM-C1I*UIM-S1R*TIK+S1I*UIK
      U(I,M) = C1R*UIM+C1I*TIM-S1R*UIK-S1I*TIK
      ENDDO
      ENDIF

!     END TRANSFORMATIONS

      ENDDO
      ENDDO

!     GO TO NEXT ITERATION

      GO TO 20

!     NO CONVERGENCE !

   90 IER = 1
      RETURN

!     CONVERGENCE OK

   95 IER = 0

!     SORT SOLUTIONS IN INCREASING ORDER
      DO J=2,N
      VR=A(J,J)
      VI=Z(J,J)
      DO K=1,N
      EN(K)=T(K,J)
      EN(K+N)=U(K,J)
      ENDDO
         DO I=J-1,1,-1
         IF(DABSCD(A(I,I),Z(I,I)).LE.DABSCD(VR,VI)) GO TO 97
         A(I+1,I+1)=A(I,I)
         Z(I+1,I+1)=Z(I,I)
            DO K=1,N
            T(K,I+1)=T(K,I)
            U(K,I+1)=U(K,I)
            ENDDO
         ENDDO
            I=0
   97       A(I+1,I+1)=VR
            Z(I+1,I+1)=VI
            DO K=1,N
            T(K,I+1)=EN(K)
            U(K,I+1)=EN(K+N)
            ENDDO
      ENDDO

!     NORMALIZE VECTORS (BIGGEST COMPONENT TO UNITY)
      DO J=1,N
      ZM=0.D0
        DO I=1,N
        ZI=DABS(T(I,J))+DABS(U(I,J))
        IF(ZI.GE.ZM) THEN
        IM=I
        ZM=ZI
        ENDIF
        ENDDO
        ZM=T(IM,J)
        ZI=U(IM,J)
        DO I=1,N
        CALL DIVCD(T(I,J),U(I,J),ZM,ZI,TR,TI)
        T(I,J)=TR
        U(I,J)=TI
        ENDDO
      ENDDO
      RETURN
      END

!end of file tcomeig.f90

