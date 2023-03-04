C     MAIN PROGRAM       S U T R A _ M A I N       SUTRA VERSION 2D3D.1  SUTRA_MAIN.....100
C_______________________________________________________________________ SUTRA_MAIN.....200
C|                                                                     | SUTRA_MAIN.....300
C|                                                                     | SUTRA_MAIN.....400
C|                   UNITED STATES GEOLOGICAL SURVEY                   | SUTRA_MAIN.....500
C|          MODEL FOR SATURATED-UNSATURATED, VARIABLE-DENSITY          | SUTRA_MAIN.....600
C|          GROUND-WATER FLOW WITH SOLUTE OR ENERGY TRANSPORT          | SUTRA_MAIN.....700
C|                                                                     | SUTRA_MAIN.....800
C|                                                                     | SUTRA_MAIN.....900
C|                                                                     | SUTRA_MAIN....1000
C|                                                                     | SUTRA_MAIN....1100
C|                       _______________________                       | SUTRA_MAIN....1200
C|                      |                       |                      | SUTRA_MAIN....1300
C|                      |   S   U   T   R   A   |                      | SUTRA_MAIN....1400
C|                      |_______________________|                      | SUTRA_MAIN....1500
C|                                                                     | SUTRA_MAIN....1600
C|                                                                     | SUTRA_MAIN....1700
C|                Saturated    Unsaturated    TRAnsport                | SUTRA_MAIN....1800
C|                =            =              ===                      | SUTRA_MAIN....1900
C|                                                                     | SUTRA_MAIN....2000
C|                                                                     | SUTRA_MAIN....2100
C|                                                                     | SUTRA_MAIN....2200
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....2300
C|    *                                                           *    | SUTRA_MAIN....2400
C|    *  PHYSICS OPTIONS:                                         *    | SUTRA_MAIN....2500
C|    *  -> Saturated and/or unsaturated ground-water flow        *    | SUTRA_MAIN....2600
C|    *  -> Either single species reactive solute transport       *    | SUTRA_MAIN....2700
C|    *     or thermal energy transport                           *    | SUTRA_MAIN....2800
C|    *  GEOMETRY OPTIONS:                                        *    | SUTRA_MAIN....2900
C|    *  -> Two-dimensional areal or cross-sectional simulation   *    | SUTRA_MAIN....3000
C|    *  -> Fully three-dimensional simulation                    *    | SUTRA_MAIN....3100
C|    *  -> Either two- or three-dimensional Cartesian or         *    | SUTRA_MAIN....3200
C|    *     two-dimensional radial coordinates                    *    | SUTRA_MAIN....3300
C|    *  NUMERICAL METHODS:                                       *    | SUTRA_MAIN....3400
C|    *  -> Hybrid Galerkin-finite-element method and             *    | SUTRA_MAIN....3500
C|    *     integrated-finite-difference method                   *    | SUTRA_MAIN....3600
C|    *     with two-dimensional quadrilateral or                 *    | SUTRA_MAIN....3700
C|    *     three-dimensional generalized hexahedral              *    | SUTRA_MAIN....3800
C|    *     finite elements                                       *    | SUTRA_MAIN....3900
C|    *  -> Finite-difference time discretization                 *    | SUTRA_MAIN....4000
C|    *  -> Nonlinear iterative, sequential or steady-state       *    | SUTRA_MAIN....4100
C|    *     solution modes                                        *    | SUTRA_MAIN....4200
C|    *  -> Direct and iterative solvers                          *    | SUTRA_MAIN....4300
C|    *  OUTPUT OPTIONS:                                          *    | SUTRA_MAIN....4400
C|    *  -> Optional fluid velocity calculation                   *    | SUTRA_MAIN....4500
C|    *  -> Optional observation well output                      *    | SUTRA_MAIN....4600
C|    *  -> Optional fluid mass and solute mass or energy budget  *    | SUTRA_MAIN....4700
C|    *  -> Flexible, columnwise output of solution               *    | SUTRA_MAIN....4800
C|    *                                                           *    | SUTRA_MAIN....4900
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....5000
C|                                                                     | SUTRA_MAIN....5100
C|                                                                     | SUTRA_MAIN....5200
C|                                                                     | SUTRA_MAIN....5300
C|       Complete explanation of the function and use of this code     | SUTRA_MAIN....5400
C|       is given in :                                                 | SUTRA_MAIN....5500
C|                                                                     | SUTRA_MAIN....5600
C|       Voss, Clifford I., and Provost, Alden M., 2002,               | SUTRA_MAIN....5700
C|            SUTRA - A model for saturated-unsaturated                | SUTRA_MAIN....5800
C|            variable-density ground-water flow with                  | SUTRA_MAIN....5900
C|            solute or energy transport: U.S. Geological              | SUTRA_MAIN....6000
C|            Survey Water-Resources Investigations Report             | SUTRA_MAIN....6100
C|            02-4231, 250p.                                           | SUTRA_MAIN....6200
C|                                                                     | SUTRA_MAIN....6300
C|                                                                     | SUTRA_MAIN....6400
C|                                                                     | SUTRA_MAIN....6500
C|       Users who wish to be notified of updates of the SUTRA         | SUTRA_MAIN....6600
C|       code and documentation may be added to the mailing list       | SUTRA_MAIN....6700
C|       by sending a request to :                                     | SUTRA_MAIN....6800
C|                                                                     | SUTRA_MAIN....6900
C|                           SUTRA Support                             | SUTRA_MAIN....7000
C|                       U.S. Geological Survey                        | SUTRA_MAIN....7100
C|                        431 National Center                          | SUTRA_MAIN....7200
C|                       Reston, Virginia 20192                        | SUTRA_MAIN....7300
C|                                USA                                  | SUTRA_MAIN....7400
C|                                                                     | SUTRA_MAIN....7500
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....7600
C|    *                                                           *    | SUTRA_MAIN....7700
C|    *  The SUTRA code and documentation were originally         *    | SUTRA_MAIN....7800
C|    *  prepared under a joint research project of the U.S.      *    | SUTRA_MAIN....7900
C|    *  Geological Survey, Department of the Interior, Reston,   *    | SUTRA_MAIN....8000
C|    *  Virginia, and the Engineering and Services Laboratory,   *    | SUTRA_MAIN....8100
C|    *  U.S. Air Force Engineering and Services Center, Tyndall  *    | SUTRA_MAIN....8200
C|    *  A.F.B., Florida.  The SUTRA code and documentation are   *    | SUTRA_MAIN....8300
C|    *  available for unlimited distribution.                    *    | SUTRA_MAIN....8400
C|    *                                                           *    | SUTRA_MAIN....8500
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....8600
C|                                                                     | SUTRA_MAIN....8700
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN....8800
C|    *                                                           *    | SUTRA_MAIN....8900
C|    *  Original Release: 1984                                   *    | SUTRA_MAIN....9000
C|    *  by: Clifford I. Voss, U.S. Geological Survey             *    | SUTRA_MAIN....9100
C|    *                                                           *    | SUTRA_MAIN....9200
C|    *  First Revision: June 1990, Version V06902D               *    | SUTRA_MAIN....9300
C|    *  by: Clifford I. Voss, U.S. Geological Survey             *    | SUTRA_MAIN....9400
C|    *                                                           *    | SUTRA_MAIN....9500
C|    *  Second Revision: September 1997, Version V09972D         *    | SUTRA_MAIN....9600
C|    *  by: C.I. Voss and David Boldt, U.S. Geological Survey    *    | SUTRA_MAIN....9700
C|    *                                                           *    | SUTRA_MAIN....9800
C|    *  Third Revision: September 2003, Version 2D3D.1           *    | SUTRA_MAIN....9900
C|    *  by: A.M. Provost & C.I. Voss, U.S. Geological Survey     *    | SUTRA_MAIN...10000
C|    *                                                           *    | SUTRA_MAIN...10100
C|    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    | SUTRA_MAIN...10200
C|                                                                     | SUTRA_MAIN...10300
C|                                                                     | SUTRA_MAIN...10400
C|_____________________________________________________________________| SUTRA_MAIN...10500
C                                                                        SUTRA_MAIN...10600
C                                                                        SUTRA_MAIN...10700
C                                                                        SUTRA_MAIN...10800
      PROGRAM SUTRA_MAIN                                                 SUTRA_MAIN...10900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SUTRA_MAIN...11000
C                                                                        SUTRA_MAIN...11100
C.....PROGRAMMERS SET SUTRA VERSION NUMBER HERE (8 CHARACTERS MAXIMUM)   SUTRA_MAIN...11200
      CHARACTER*8, PARAMETER :: VERN='2D3D.1'                            SUTRA_MAIN...11300
C                                                                        SUTRA_MAIN...11400
      CHARACTER*8 VERNUM                                                 SUTRA_MAIN...11500
      CHARACTER*1 TITLE1(80),TITLE2(80)                                  SUTRA_MAIN...11600
      CHARACTER*80 SIMULA(2),MSHTYP(2),SIMSTR, MSHSTR                    SUTRA_MAIN...11700
      CHARACTER*80 CUNSAT, CSSFLO ,CSSTRA, CREAD                         SUTRA_MAIN...11800
      CHARACTER*80 UNSSTR, SSFSTR ,SSTSTR, RDSTR                         SUTRA_MAIN...11900
      CHARACTER*80 UNAME,FNAME                                           SUTRA_MAIN...12000
      CHARACTER*80 ERRCOD,CHERR(10)                                      SUTRA_MAIN...12100
      CHARACTER*40 SOLNAM(0:10)                                          SUTRA_MAIN...12200
      CHARACTER*10 SOLWRD(0:10)                                          SUTRA_MAIN...12300
      CHARACTER*10 ADSMOD                                                SUTRA_MAIN...12400
      CHARACTER LETTER(8)*1                                              SUTRA_MAIN...12500
      CHARACTER INTFIL*1000, CDUM*1                                      SUTRA_MAIN...12600
      INTEGER RMDIM,RVDIM,RMVDIM,IMVDIM                                  SUTRA_MAIN...12700
      LOGICAL ONCEK5,ONCEK6,ONCEK7                                       SUTRA_MAIN...12800
      LOGICAL ISERR                                                      SUTRA_MAIN...12900
      LOGICAL ALLO1, ALLO2                                               SUTRA_MAIN...13000
      DIMENSION FNAME(0:7),IUNIT(0:7)                                    SUTRA_MAIN...13100
      DIMENSION INERR(10), RLERR(10)                                     SUTRA_MAIN...13200
      ALLOCATABLE PMAT(:,:),UMAT(:,:)                                    SUTRA_MAIN...13300
      ALLOCATABLE PITER(:),UITER(:),PM1(:),DPDTITR(:),UM1(:),UM2(:),     SUTRA_MAIN...13400
     1   PVEL(:),SL(:),SR(:),X(:),Y(:),Z(:),VOL(:),POR(:),               SUTRA_MAIN...13500
     2   CS1(:),CS2(:),CS3(:),SW(:),DSWDP(:),RHO(:),SOP(:),              SUTRA_MAIN...13600
     3   QIN(:),UIN(:),QUIN(:),QINITR(:),RCIT(:),RCITM1(:)               SUTRA_MAIN...13700
      ALLOCATABLE PVEC(:),UVEC(:)                                        SUTRA_MAIN...13800
      ALLOCATABLE ALMAX(:),ALMIN(:),ATMAX(:),ATMIN(:),VMAG(:),VANG1(:),  SUTRA_MAIN...13900
     1   PERMXX(:),PERMXY(:),PERMYX(:),PERMYY(:),PANGL1(:)               SUTRA_MAIN...14000
      ALLOCATABLE ALMID(:),ATMID(:),VANG2(:),PERMXZ(:),PERMYZ(:),        SUTRA_MAIN...14100
     1   PERMZX(:),PERMZY(:),PERMZZ(:),PANGL2(:),PANGL3(:)               SUTRA_MAIN...14200
      ALLOCATABLE PBC(:),UBC(:),QPLITR(:)                                SUTRA_MAIN...14300
      ALLOCATABLE GXSI(:,:),GETA(:,:),GZET(:,:)                          SUTRA_MAIN...14400
      ALLOCATABLE FWK(:),B(:)                                            SUTRA_MAIN...14500
      ALLOCATABLE IN(:),IQSOP(:),IQSOU(:),IPBC(:),IUBC(:),               SUTRA_MAIN...14600
     1   IOBS(:),NREG(:),LREG(:),NBI27(:),IWK(:),IA(:),JA(:)             SUTRA_MAIN...14700
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  SUTRA_MAIN...14800
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             SUTRA_MAIN...14900
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SUTRA_MAIN...15000
     1   NSOP,NSOU,NBCN                                                  SUTRA_MAIN...15100
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   SUTRA_MAIN...15200
      COMMON /DIMX2/ NELTA, NNVEC, NDIMJA                                SUTRA_MAIN...15300
      COMMON /ERRHAN/ ISERR                                              SUTRA_MAIN...15400
      COMMON /FNAMES/ FNAME                                              SUTRA_MAIN...15500
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        SUTRA_MAIN...15600
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      SUTRA_MAIN...15700
      COMMON /ITSOLI/ ITRMXP,ITOLP,NSAVEP,ITRMXU,ITOLU,NSAVEU            SUTRA_MAIN...15800
      COMMON /ITSOLR/ TOLP,TOLU                                          SUTRA_MAIN...15900
      COMMON /JCOLS/ NCOLPR, LCOLPR, NCOLS5, NCOLS6, J5COL, J6COL        SUTRA_MAIN...16000
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     SUTRA_MAIN...16100
     1   KSCRN,KPAUSE                                                    SUTRA_MAIN...16200
      COMMON /MODSOR/ ADSMOD                                             SUTRA_MAIN...16300
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC                                    SUTRA_MAIN...16400
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      SUTRA_MAIN...16500
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        SUTRA_MAIN...16600
      COMMON /PLT1/ ONCEK5, ONCEK6, ONCEK7                               SUTRA_MAIN...16700
      COMMON /SOLVC/ SOLWRD, SOLNAM                                      SUTRA_MAIN...16800
      COMMON /SOLVN/ NSLVRS                                              SUTRA_MAIN...16900
      COMMON /SOLVI/ KSOLVP, KSOLVU, NN1, NN2, NN3                       SUTRA_MAIN...17000
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       SUTRA_MAIN...17100
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  SUTRA_MAIN...17200
      COMMON /VER/ VERNUM                                                SUTRA_MAIN...17300
      DATA (LETTER(M), M=1,8) /'A','B','C','D','E','F','G','H'/          SUTRA_MAIN...17400
C....."NSLVRS" AND THE ARRAYS "SOLWRD" AND "SOLNAM" ARE INITIALIZED      SUTRA_MAIN...17500
C        IN THE BLOCK-DATA SUBPROGRAM "BDINIT"                           SUTRA_MAIN...17600
C                                                                        SUTRA_MAIN...17700
C                                                                        SUTRA_MAIN...17800
C.....SET THE GLOBAL ERROR FLAG TO FALSE                                 SUTRA_MAIN...17900
      ISERR = .FALSE.                                                    SUTRA_MAIN...18000
C.....SET THE ALLOCATION FLAGS TO FALSE                                  SUTRA_MAIN...18100
      ALLO1 = .FALSE.                                                    SUTRA_MAIN...18200
      ALLO2 = .FALSE.                                                    SUTRA_MAIN...18300
C                                                                        SUTRA_MAIN...18400
C_______________________________________________________________________ SUTRA_MAIN...18500
C|                                                                     | SUTRA_MAIN...18600
C|  *****************************************************************  | SUTRA_MAIN...18700
C|  *                                                               *  | SUTRA_MAIN...18800
C|  *   **********  M E M O R Y   A L L O C A T I O N  **********   *  | SUTRA_MAIN...18900
C|  *                                                               *  | SUTRA_MAIN...19000
C|  *   The main arrays used by SUTRA are dimensioned dynamically   *  | SUTRA_MAIN...19100
C|  *   in the main program, SUTRA_MAIN.  The amount of storage     *  | SUTRA_MAIN...19200
C|  *   required by these arrays depends on the dimensionality of   *  | SUTRA_MAIN...19300
C|  *   the problem (2D or 3D) and the particular solver(s) used.   *  | SUTRA_MAIN...19400
C|  *                                                               *  | SUTRA_MAIN...19500
C|  *               |---------------------|---------------------|   *  | SUTRA_MAIN...19600
C|  *               |     sum of real     |    sum of integer   |   *  | SUTRA_MAIN...19700
C|  *               |   array dimensions  |   array dimensions  |   *  | SUTRA_MAIN...19800
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...19900
C|  *   | 2D,       | (2*NBI+29)*NN+19*NE | 3*NN+9*NE+NSOP+NSOU |   *  | SUTRA_MAIN...20000
C|  *   | direct    |     +3*NBCN+19      |    +2*NBCN+NOBS+3   |   *  | SUTRA_MAIN...20100
C|  *   | solver    |                     |                     |   *  | SUTRA_MAIN...20200
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...20300
C|  *   | 2D,       | 2*NELT+30*NN+19*NE  | 2*NELT+NN+9*NE+NSOP |   *  | SUTRA_MAIN...20400
C|  *   | iterative |   +3*NBCN+NWF+17    |  +NSOU+2*NBCN+NOBS  |   *  | SUTRA_MAIN...20500
C|  *   | solver(s) |                     |     +NBI+NWI+1      |   *  | SUTRA_MAIN...20600
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...20700
C|  *   | 3D,       | (2*NBI+29)*NN+48*NE | 3*NN+9*NE+NSOP+NSOU |   *  | SUTRA_MAIN...20800
C|  *   | direct    |      +3*NBCN+2      |    +2*NBCN+NOBS+3   |   *  | SUTRA_MAIN...20900
C|  *   | solver    |                     |                     |   *  | SUTRA_MAIN...21000
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...21100
C|  *   | 3D,       | 2*NELT+30*NN+48*NE  | 2*NELT+NN+9*NE+NSOP |   *  | SUTRA_MAIN...21200
C|  *   | iterative |     +3*NBCN+NWF     |  +NSOU+2*NBCN+NOBS  |   *  | SUTRA_MAIN...21300
C|  *   | solver(s) |                     |     +NBI+NWI+1      |   *  | SUTRA_MAIN...21400
C|  *   |-----------|---------------------|---------------------|   *  | SUTRA_MAIN...21500
C|  *                                                               *  | SUTRA_MAIN...21600
C|  *   Quantities in the table above are defined in Section 7.3    *  | SUTRA_MAIN...21700
C|  *   of the published documentation (Voss & Provost, 2002,       *  | SUTRA_MAIN...21800
C|  *   USGS Water-Resources Investigations Report 02-4231).        *  | SUTRA_MAIN...21900
C|  *                                                               *  | SUTRA_MAIN...22000
C|  *   During each run, SUTRA writes memory usage information to   *  | SUTRA_MAIN...22100
C|  *   the LST output file.                                        *  | SUTRA_MAIN...22200
C|  *                                                               *  | SUTRA_MAIN...22300
C|  *****************************************************************  | SUTRA_MAIN...22400
C|_____________________________________________________________________| SUTRA_MAIN...22500
C                                                                        SUTRA_MAIN...22600
C ---> Programmers making code changes that affect dimensions must       SUTRA_MAIN...22700
C ---> check and change the following assignments for NNV and NEV:       SUTRA_MAIN...22800
C                                                                        SUTRA_MAIN...22900
C.....NNV IS NUMBER OF REAL VECTORS THAT ARE NN LONG.                    SUTRA_MAIN...23000
         NNV = 27                                                        SUTRA_MAIN...23100
C.....NEV IS NUMBER OF REAL VECTORS THAT ARE NE LONG.                    SUTRA_MAIN...23200
C        NEV = NEV2 for 2D; NEV3 for 3D                                  SUTRA_MAIN...23300
         NEV2 = 11                                                       SUTRA_MAIN...23400
         NEV3 = 21                                                       SUTRA_MAIN...23500
C                                                                        SUTRA_MAIN...23600
C_______________________________________________________________________ SUTRA_MAIN...23700
C|                                                                     | SUTRA_MAIN...23800
C|  *****************************************************************  | SUTRA_MAIN...23900
C|  *                                                               *  | SUTRA_MAIN...24000
C|  *   ***********  F I L E   A S S I G N M E N T S  ***********   *  | SUTRA_MAIN...24100
C|  *                                                               *  | SUTRA_MAIN...24200
C|  *   Unit K0 contains the FORTRAN unit number and filename       *  | SUTRA_MAIN...24300
C|  *   assignments for the various SUTRA input and output files.   *  | SUTRA_MAIN...24400
C|  *   Each line of Unit K0 begins with a file type, followed by   *  | SUTRA_MAIN...24500
C|  *   a unit number and a filename for that type, all in free     *  | SUTRA_MAIN...24600
C|  *   format. Permitted file types are INP, ICS, LST, RST, NOD,   *  | SUTRA_MAIN...24700
C|  *   ELE, OBS, and SMY. Assignments may be listed in any order.  *  | SUTRA_MAIN...24800
C|  *   Example ("#" indicates a comment):                          *  | SUTRA_MAIN...24900
C|  *   'INP'  50  'project.inp'   # required                       *  | SUTRA_MAIN...25000
C|  *   'ICS'  55  'project.ics'   # required                       *  | SUTRA_MAIN...25100
C|  *   'LST'  60  'project.lst'   # required                       *  | SUTRA_MAIN...25200
C|  *   'RST'  66  'project.rst'   # required if ISTORE>0           *  | SUTRA_MAIN...25300
C|  *   'NOD'  70  'project.nod'   # optional                       *  | SUTRA_MAIN...25400
C|  *   'ELE'  80  'project.ele'   # optional                       *  | SUTRA_MAIN...25500
C|  *   'OBS'  90  'project.obs'   # optional                       *  | SUTRA_MAIN...25600
C|  *   'SMY'  40  'project.smy'   # optional; defaults to unit=1,  *  | SUTRA_MAIN...25700
C|  *                              #           filename="SUTRA.SMY" *  | SUTRA_MAIN...25800
C|  *                                                               *  | SUTRA_MAIN...25900
C|  *****************************************************************  | SUTRA_MAIN...26000
C|_____________________________________________________________________| SUTRA_MAIN...26100
C                                                                        SUTRA_MAIN...26200
C.....SET FILENAME AND FORTRAN UNIT NUMBER FOR UNIT K0                   SUTRA_MAIN...26300
      UNAME = 'SUTRA.FIL'                                                SUTRA_MAIN...26400
      K0 = 99                                                            SUTRA_MAIN...26500
C.....ASSIGN UNIT NUMBERS AND OPEN FILE UNITS FOR THIS SIMULATION        SUTRA_MAIN...26600
      CALL FOPEN(UNAME,IUNIT,NFILE)                                      SUTRA_MAIN...26700
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...26800
C                                                                        SUTRA_MAIN...26900
C                                                                        SUTRA_MAIN...27000
C.....COPY PARAMETER VERN (SUTRA VERSION NUMBER) TO VARIABLE VERNUM,     SUTRA_MAIN...27100
C        WHICH IS PASSED THROUGH COMMON BLOCK VER.                       SUTRA_MAIN...27200
      VERNUM = VERN                                                      SUTRA_MAIN...27300
C                                                                        SUTRA_MAIN...27400
C.....KEEP TRACK IF OUTPUT ROUTINES HAVE BEEN EXECUTED, TO PRINT         SUTRA_MAIN...27500
C        HEADERS ONLY ONCE.                                              SUTRA_MAIN...27600
      ONCEK5 = .FALSE.                                                   SUTRA_MAIN...27700
      ONCEK6 = .FALSE.                                                   SUTRA_MAIN...27800
C                                                                        SUTRA_MAIN...27900
C.....OUTPUT BANNER                                                      SUTRA_MAIN...28000
      WRITE(K3,110) VERNUM(1:LEN_TRIM(VERNUM))                           SUTRA_MAIN...28100
  110 FORMAT(1H1,131(1H*)////3(132(1H*)////)////                         SUTRA_MAIN...28200
     1   47X,' SSSS   UU  UU  TTTTTT  RRRRR     AA  '/                   SUTRA_MAIN...28300
     2   47X,'SS   S  UU  UU  T TT T  RR  RR   AAAA '/                   SUTRA_MAIN...28400
     3   47X,'SSSS    UU  UU    TT    RRRRR   AA  AA'/                   SUTRA_MAIN...28500
     4   47X,'    SS  UU  UU    TT    RR R    AAAAAA'/                   SUTRA_MAIN...28600
     5   47X,'SS  SS  UU  UU    TT    RR RR   AA  AA'/                   SUTRA_MAIN...28700
     6   47X,' SSSS    UUUU     TT    RR  RR  AA  AA'/                   SUTRA_MAIN...28800
     7   7(/),37X,'U N I T E D    S T A T E S   ',                       SUTRA_MAIN...28900
     8   'G E O L O G I C A L   S U R V E Y'////                         SUTRA_MAIN...29000
     9   45X,'SUBSURFACE FLOW AND TRANSPORT SIMULATION MODEL'/           SUTRA_MAIN...29100
     *   //56X,'-SUTRA VERSION ',A,'-'///                                SUTRA_MAIN...29200
     A   36X,'*  SATURATED-UNSATURATED FLOW AND SOLUTE OR ENERGY',       SUTRA_MAIN...29300
     B   ' TRANSPORT  *'////4(////132(1H*)))                             SUTRA_MAIN...29400
C                                                                        SUTRA_MAIN...29500
C_______________________________________________________________________ SUTRA_MAIN...29600
C|                                                                     | SUTRA_MAIN...29700
C|  *****************************************************************  | SUTRA_MAIN...29800
C|  *                                                               *  | SUTRA_MAIN...29900
C|  *   *********  R E A D I N G   I N P U T   D A T A  *********   *  | SUTRA_MAIN...30000
C|  *   *********  A N D   E R R O R   H A N D L I N G  *********   *  | SUTRA_MAIN...30100
C|  *                                                               *  | SUTRA_MAIN...30200
C|  *   SUTRA typically reads input data line by line as follows.   *  | SUTRA_MAIN...30300
C|  *   Subroutine SKPCOM is called to skip over any comment        *  | SUTRA_MAIN...30400
C|  *   lines.  Subroutine READIF is then called to read a single   *  | SUTRA_MAIN...30500
C|  *   line of input data (up to 1000 characters) into internal    *  | SUTRA_MAIN...30600
C|  *   file INTFIL. If an error condition (ISERR set to .TRUE.)    *  | SUTRA_MAIN...30700
C|  *   exists after either of these two calls, control passes to   *  | SUTRA_MAIN...30800
C|  *   the termination sequence in the main program (line 9000).   *  | SUTRA_MAIN...30900
C|  *   Otherwise, the input data values are read from INTFIL. In   *  | SUTRA_MAIN...31000
C|  *   case of an error during this read, subroutine SUTERR is     *  | SUTRA_MAIN...31100
C|  *   called to report it, and control passes to the termination  *  | SUTRA_MAIN...31200
C|  *   sequence in the main program.  The variable ERRCOD is used  *  | SUTRA_MAIN...31300
C|  *   to identify the nature of the error and is set prior to     *  | SUTRA_MAIN...31400
C|  *   calling SKPCOM and READIF. The variables CHERR, INERR,      *  | SUTRA_MAIN...31500
C|  *   and RLERR can be used to send character, integer, or real   *  | SUTRA_MAIN...31600
C|  *   error information to subroutine SUTERR.                     *  | SUTRA_MAIN...31700
C|  *   Example from the main program:                              *  | SUTRA_MAIN...31800
C|  *    ERRCOD = 'REA-INP-S3'                                      *  | SUTRA_MAIN...31900
C|  *    CALL SKPCOM(K1, NLSKIP, ERRCOD)                            *  | SUTRA_MAIN...32000
C|  *    IF (ISERR) GOTO 9000                                       *  | SUTRA_MAIN...32100
C|  *    ERRCOD = 'REA-INP-3'                                       *  | SUTRA_MAIN...32200
C|  *    CALL READIF(K1, INTFIL, ERRCOD)                            *  | SUTRA_MAIN...32300
C|  *    READ(INTFIL,*,IOSTAT=INERR(1)) NN,NE,NPBC,NUBC,            *  | SUTRA_MAIN...32400
C|  *   1   NSOP,NSOU,NOBS                                          *  | SUTRA_MAIN...32500
C|  *    IF (INERR(1).NE.0) THEN                                    *  | SUTRA_MAIN...32600
C|  *       CALL SUTERR(ERRCOD,CHERR,INERR,RLERR)                   *  | SUTRA_MAIN...32700
C|  *       GOTO 9000                                               *  | SUTRA_MAIN...32800
C|  *    END IF                                                     *  | SUTRA_MAIN...32900
C|  *                                                               *  | SUTRA_MAIN...33000
C|  *****************************************************************  | SUTRA_MAIN...33100
C|_____________________________________________________________________| SUTRA_MAIN...33200
C                                                                        SUTRA_MAIN...33300
C.....INPUT DATASET 1:  OUTPUT HEADING                                   SUTRA_MAIN...33400
      ERRCOD = 'REA-INP-S1'                                              SUTRA_MAIN...33500
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    SUTRA_MAIN...33600
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...33700
      READ(K1,117,IOSTAT=INERR(1)) TITLE1,TITLE2                         SUTRA_MAIN...33800
      IF (INERR(1).NE.0) THEN                                            SUTRA_MAIN...33900
         ERRCOD = 'REA-INP-1'                                            SUTRA_MAIN...34000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...34100
         GOTO 9000                                                       SUTRA_MAIN...34200
      END IF                                                             SUTRA_MAIN...34300
  117 FORMAT(80A1/80A1)                                                  SUTRA_MAIN...34400
C                                                                        SUTRA_MAIN...34500
C.....INPUT DATASET 2A:  SIMULATION TYPE (TYPE OF TRANSPORT)             SUTRA_MAIN...34600
C        (SET ME=-1 FOR SOLUTE TRANSPORT, ME=+1 FOR ENERGY TRANSPORT)    SUTRA_MAIN...34700
      ERRCOD = 'REA-INP-S2A'                                             SUTRA_MAIN...34800
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    SUTRA_MAIN...34900
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...35000
      ERRCOD = 'REA-INP-2A'                                              SUTRA_MAIN...35100
      CALL READIF(K1, INTFIL, ERRCOD)                                    SUTRA_MAIN...35200
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...35300
      READ(INTFIL,*,IOSTAT=INERR(1)) SIMSTR                              SUTRA_MAIN...35400
      IF (INERR(1).NE.0) THEN                                            SUTRA_MAIN...35500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...35600
         GOTO 9000                                                       SUTRA_MAIN...35700
      END IF                                                             SUTRA_MAIN...35800
      CALL PRSWDS(SIMSTR, ' ', 2, SIMULA, NWORDS)                        SUTRA_MAIN...35900
      IF(SIMULA(1).NE.'SUTRA     ') THEN                                 SUTRA_MAIN...36000
         ERRCOD = 'INP-2A-1'                                             SUTRA_MAIN...36100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...36200
         GOTO 9000                                                       SUTRA_MAIN...36300
      END IF                                                             SUTRA_MAIN...36400
      IF(SIMULA(2).EQ.'SOLUTE    ') GOTO 120                             SUTRA_MAIN...36500
      IF(SIMULA(2).EQ.'ENERGY    ') GOTO 140                             SUTRA_MAIN...36600
      ERRCOD = 'INP-2A-2'                                                SUTRA_MAIN...36700
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           SUTRA_MAIN...36800
      GOTO 9000                                                          SUTRA_MAIN...36900
  120 ME=-1                                                              SUTRA_MAIN...37000
      WRITE(K3,130)                                                      SUTRA_MAIN...37100
  130 FORMAT(1H1//132(1H*)///20X,'* * * * *   S U T R A   S O L U ',     SUTRA_MAIN...37200
     1   'T E   T R A N S P O R T   S I M U L A T I O N   * * * * *'//   SUTRA_MAIN...37300
     2   /132(1H*)/)                                                     SUTRA_MAIN...37400
      GOTO 160                                                           SUTRA_MAIN...37500
  140 ME=+1                                                              SUTRA_MAIN...37600
      WRITE(K3,150)                                                      SUTRA_MAIN...37700
  150 FORMAT(1H1//132(1H*)///20X,'* * * * *   S U T R A   E N E R ',     SUTRA_MAIN...37800
     1   'G Y   T R A N S P O R T   S I M U L A T I O N   * * * * *'//   SUTRA_MAIN...37900
     2   /132(1H*)/)                                                     SUTRA_MAIN...38000
  160 CONTINUE                                                           SUTRA_MAIN...38100
C                                                                        SUTRA_MAIN...38200
C.....INPUT DATASET 2B:  MESH STRUCTURE                                  SUTRA_MAIN...38300
      ERRCOD = 'REA-INP-S2B'                                             SUTRA_MAIN...38400
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    SUTRA_MAIN...38500
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...38600
      ERRCOD = 'REA-INP-2B'                                              SUTRA_MAIN...38700
      CALL READIF(K1, INTFIL, ERRCOD)                                    SUTRA_MAIN...38800
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...38900
      READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR                              SUTRA_MAIN...39000
      IF (INERR(1).NE.0) THEN                                            SUTRA_MAIN...39100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...39200
         GOTO 9000                                                       SUTRA_MAIN...39300
      END IF                                                             SUTRA_MAIN...39400
      CALL PRSWDS(MSHSTR, ' ', 2, MSHTYP, NWORDS)                        SUTRA_MAIN...39500
C.....KTYPE SET ACCORDING TO THE TYPE OF FINITE-ELEMENT MESH:            SUTRA_MAIN...39600
C        3D, REGULAR MESH     ==>   KTYPE = -3                           SUTRA_MAIN...39700
C        2D, IRREGULAR MESH   ==>   KTYPE = +2                           SUTRA_MAIN...39800
C        2D, REGULAR MESH     ==>   KTYPE = -2                           SUTRA_MAIN...39900
C        (SUTRA DOES NOT CURRENTLY ALLOW 3D, IRREGULAR MESHES.)          SUTRA_MAIN...40000
      IF (MSHTYP(1).EQ.'2D        ') THEN                                SUTRA_MAIN...40100
         KTYPE = 2                                                       SUTRA_MAIN...40200
      ELSE IF (MSHTYP(1).EQ.'3D        ') THEN                           SUTRA_MAIN...40300
         KTYPE = 3                                                       SUTRA_MAIN...40400
      ELSE                                                               SUTRA_MAIN...40500
         ERRCOD = 'INP-2B-1'                                             SUTRA_MAIN...40600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...40700
         GOTO 9000                                                       SUTRA_MAIN...40800
      END IF                                                             SUTRA_MAIN...40900
      IF (MSHTYP(2).EQ.'IRREGULAR ') THEN                                SUTRA_MAIN...41000
         IF (KTYPE.EQ.3) THEN                                            SUTRA_MAIN...41100
            ERRCOD = 'INP-2B-2'                                          SUTRA_MAIN...41200
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     SUTRA_MAIN...41300
            GOTO 9000                                                    SUTRA_MAIN...41400
         END IF                                                          SUTRA_MAIN...41500
      ELSE IF ((MSHTYP(2).EQ.'REGULAR   ').OR.                           SUTRA_MAIN...41600
     1         (MSHTYP(2).EQ.'BLOCKWISE ')) THEN                         SUTRA_MAIN...41700
         KTYPE = - KTYPE                                                 SUTRA_MAIN...41800
         ERRCOD = 'REA-INP-2B'                                           SUTRA_MAIN...41900
         IF (KTYPE.EQ.-2) THEN                                           SUTRA_MAIN...42000
            READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR, NN1, NN2              SUTRA_MAIN...42100
            NN3 = 1                                                      SUTRA_MAIN...42200
         ELSE                                                            SUTRA_MAIN...42300
            READ(INTFIL,*,IOSTAT=INERR(1)) MSHSTR, NN1, NN2, NN3         SUTRA_MAIN...42400
         END IF                                                          SUTRA_MAIN...42500
         IF (INERR(1).NE.0) THEN                                         SUTRA_MAIN...42600
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     SUTRA_MAIN...42700
            GOTO 9000                                                    SUTRA_MAIN...42800
         END IF                                                          SUTRA_MAIN...42900
         IF ((NN1.LE.2).OR.((KTYPE.EQ.-2).AND.(NN2.LE.1))                SUTRA_MAIN...43000
     1      .OR.((KTYPE.EQ.-3).AND.((NN2.LE.2).OR.(NN3.LE.1)))) THEN     SUTRA_MAIN...43100
            ERRCOD = 'INP-2B-3'                                          SUTRA_MAIN...43200
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     SUTRA_MAIN...43300
            GOTO 9000                                                    SUTRA_MAIN...43400
         END IF                                                          SUTRA_MAIN...43500
         IF (MSHTYP(2).EQ.'BLOCKWISE ') THEN                             SUTRA_MAIN...43600
            ERRCOD = 'REA-INP-2B'                                        SUTRA_MAIN...43700
            DO 177 I1=1,-KTYPE                                           SUTRA_MAIN...43800
               CALL READIF(K1, INTFIL, ERRCOD)                           SUTRA_MAIN...43900
               IF (ISERR) GOTO 9000                                      SUTRA_MAIN...44000
               READ(INTFIL,*,IOSTAT=INERR(1)) IDUM1, (IDUM2, I2=1,IDUM1) SUTRA_MAIN...44100
               IF (INERR(1).NE.0) THEN                                   SUTRA_MAIN...44200
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               SUTRA_MAIN...44300
                  GOTO 9000                                              SUTRA_MAIN...44400
               END IF                                                    SUTRA_MAIN...44500
  177       CONTINUE                                                     SUTRA_MAIN...44600
         END IF                                                          SUTRA_MAIN...44700
      ELSE                                                               SUTRA_MAIN...44800
         ERRCOD = 'INP-2B-4'                                             SUTRA_MAIN...44900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...45000
         GOTO 9000                                                       SUTRA_MAIN...45100
      END IF                                                             SUTRA_MAIN...45200
C                                                                        SUTRA_MAIN...45300
C.....OUTPUT DATASET 1                                                   SUTRA_MAIN...45400
      WRITE(K3,180) TITLE1,TITLE2                                        SUTRA_MAIN...45500
  180 FORMAT(////1X,131(1H-)//26X,80A1//26X,80A1//1X,131(1H-))           SUTRA_MAIN...45600
C                                                                        SUTRA_MAIN...45700
C.....OUTPUT FILE UNIT ASSIGNMENTS                                       SUTRA_MAIN...45800
      WRITE(K3,202) (IUNIT(NF),FNAME(NF),NF=1,2),IUNIT(0),FNAME(0),      SUTRA_MAIN...45900
     1   IUNIT(3),FNAME(3)                                               SUTRA_MAIN...46000
  202 FORMAT(/////11X,'F I L E   U N I T   A S S I G N M E N T S'//      SUTRA_MAIN...46100
     1   13X,'INPUT UNITS:'/                                             SUTRA_MAIN...46200
     2   13X,' INP FILE (MAIN INPUT)         ',I3,4X,                    SUTRA_MAIN...46300
     3      'ASSIGNED TO ',A80/                                          SUTRA_MAIN...46400
     4   13X,' ICS FILE (INITIAL CONDITIONS) ',I3,4X,                    SUTRA_MAIN...46500
     5      'ASSIGNED TO ',A80//                                         SUTRA_MAIN...46600
     6   13X,'OUTPUT UNITS:'/                                            SUTRA_MAIN...46700
     7   13X,' SMY FILE (RUN SUMMARY)        ',I3,4X,                    SUTRA_MAIN...46800
     8      'ASSIGNED TO ',A80/                                          SUTRA_MAIN...46900
     9   13X,' LST FILE (GENERAL OUTPUT)     ',I3,4X,                    SUTRA_MAIN...47000
     T      'ASSIGNED TO ',A80)                                          SUTRA_MAIN...47100
      IF(IUNIT(4).NE.-1) WRITE(K3,203) IUNIT(4),FNAME(4)                 SUTRA_MAIN...47200
  203 FORMAT(13X,' RST FILE (RESTART DATA)       ',I3,4X,                SUTRA_MAIN...47300
     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...47400
      IF(IUNIT(5).NE.-1) WRITE(K3,204) IUNIT(5),FNAME(5)                 SUTRA_MAIN...47500
  204 FORMAT(13X,' NOD FILE (NODEWISE OUTPUT)    ',I3,4X,                SUTRA_MAIN...47600
     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...47700
      IF(IUNIT(6).NE.-1) WRITE(K3,206) IUNIT(6),FNAME(6)                 SUTRA_MAIN...47800
  206 FORMAT(13X,' ELE FILE (VELOCITY OUTPUT)    ',I3,4X,                SUTRA_MAIN...47900
     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...48000
      IF(IUNIT(7).NE.-1) WRITE(K3,207) IUNIT(7),FNAME(7)                 SUTRA_MAIN...48100
  207 FORMAT(13X,' OBS FILE (OBSERVATION OUTPUT) ',I3,4X,                SUTRA_MAIN...48200
     1   'ASSIGNED TO ',A80)                                             SUTRA_MAIN...48300
C                                                                        SUTRA_MAIN...48400
C.....INPUT DATASET 3:  SIMULATION CONTROL NUMBERS                       SUTRA_MAIN...48500
      ERRCOD = 'REA-INP-S3'                                              SUTRA_MAIN...48600
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    SUTRA_MAIN...48700
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...48800
      ERRCOD = 'REA-INP-3'                                               SUTRA_MAIN...48900
      CALL READIF(K1, INTFIL, ERRCOD)                                    SUTRA_MAIN...49000
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...49100
      READ(INTFIL,*,IOSTAT=INERR(1)) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS      SUTRA_MAIN...49200
      IF (INERR(1).NE.0) THEN                                            SUTRA_MAIN...49300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...49400
         GOTO 9000                                                       SUTRA_MAIN...49500
      END IF                                                             SUTRA_MAIN...49600
      IF (KTYPE.LT.0) THEN                                               SUTRA_MAIN...49700
         NN123 = NN1*NN2*NN3                                             SUTRA_MAIN...49800
         IF(NN123.NE.NN) THEN                                            SUTRA_MAIN...49900
           ERRCOD = 'INP-2B,3-1'                                         SUTRA_MAIN...50000
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                      SUTRA_MAIN...50100
           GOTO 9000                                                     SUTRA_MAIN...50200
         END IF                                                          SUTRA_MAIN...50300
         IF (IABS(KTYPE).EQ.3) THEN                                      SUTRA_MAIN...50400
            NE123 = (NN1 - 1)*(NN2 - 1)*(NN3 - 1)                        SUTRA_MAIN...50500
         ELSE                                                            SUTRA_MAIN...50600
            NE123 = (NN1 - 1)*(NN2 - 1)                                  SUTRA_MAIN...50700
         END IF                                                          SUTRA_MAIN...50800
         IF(NE123.NE.NE) THEN                                            SUTRA_MAIN...50900
           ERRCOD = 'INP-2B,3-2'                                         SUTRA_MAIN...51000
           CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                      SUTRA_MAIN...51100
           GOTO 9000                                                     SUTRA_MAIN...51200
         END IF                                                          SUTRA_MAIN...51300
      ENDIF                                                              SUTRA_MAIN...51400
C                                                                        SUTRA_MAIN...51500
C.....INPUT AND OUTPUT DATASET 4:  SIMULATION MODE OPTIONS               SUTRA_MAIN...51600
      ERRCOD = 'REA-INP-S4'                                              SUTRA_MAIN...51700
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    SUTRA_MAIN...51800
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...51900
      ERRCOD = 'REA-INP-4'                                               SUTRA_MAIN...52000
      CALL READIF(K1, INTFIL, ERRCOD)                                    SUTRA_MAIN...52100
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...52200
      READ(INTFIL,*,IOSTAT=INERR(1)) UNSSTR,SSFSTR,SSTSTR,RDSTR,ISTORE   SUTRA_MAIN...52300
      IF (INERR(1).NE.0) THEN                                            SUTRA_MAIN...52400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...52500
         GOTO 9000                                                       SUTRA_MAIN...52600
      END IF                                                             SUTRA_MAIN...52700
      CALL PRSWDS(UNSSTR, ' ', 1, CUNSAT, NWORDS)                        SUTRA_MAIN...52800
      CALL PRSWDS(SSFSTR, ' ', 1, CSSFLO, NWORDS)                        SUTRA_MAIN...52900
      CALL PRSWDS(SSTSTR, ' ', 1, CSSTRA, NWORDS)                        SUTRA_MAIN...53000
      CALL PRSWDS(RDSTR,  ' ', 1, CREAD, NWORDS)                         SUTRA_MAIN...53100
      ISMERR = 0                                                         SUTRA_MAIN...53200
      IF (CUNSAT.EQ.'UNSATURATED') THEN                                  SUTRA_MAIN...53300
         IUNSAT = +1                                                     SUTRA_MAIN...53400
      ELSE IF (CUNSAT.EQ.'SATURATED') THEN                               SUTRA_MAIN...53500
         IUNSAT = 0                                                      SUTRA_MAIN...53600
      ELSE                                                               SUTRA_MAIN...53700
         ERRCOD = 'INP-4-1'                                              SUTRA_MAIN...53800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...53900
         GOTO 9000                                                       SUTRA_MAIN...54000
      END IF                                                             SUTRA_MAIN...54100
      IF (CSSFLO.EQ.'TRANSIENT') THEN                                    SUTRA_MAIN...54200
         ISSFLO = 0                                                      SUTRA_MAIN...54300
      ELSE IF (CSSFLO.EQ.'STEADY') THEN                                  SUTRA_MAIN...54400
         ISSFLO = +1                                                     SUTRA_MAIN...54500
      ELSE                                                               SUTRA_MAIN...54600
         ERRCOD = 'INP-4-2'                                              SUTRA_MAIN...54700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...54800
         GOTO 9000                                                       SUTRA_MAIN...54900
      END IF                                                             SUTRA_MAIN...55000
      IF (CSSTRA.EQ.'TRANSIENT') THEN                                    SUTRA_MAIN...55100
         ISSTRA = 0                                                      SUTRA_MAIN...55200
      ELSE IF (CSSTRA.EQ.'STEADY') THEN                                  SUTRA_MAIN...55300
         ISSTRA = +1                                                     SUTRA_MAIN...55400
      ELSE                                                               SUTRA_MAIN...55500
         ERRCOD = 'INP-4-3'                                              SUTRA_MAIN...55600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...55700
         GOTO 9000                                                       SUTRA_MAIN...55800
      END IF                                                             SUTRA_MAIN...55900
      IF (CREAD.EQ.'COLD') THEN                                          SUTRA_MAIN...56000
         IREAD = +1                                                      SUTRA_MAIN...56100
      ELSE IF (CREAD.EQ.'WARM') THEN                                     SUTRA_MAIN...56200
         IREAD = -1                                                      SUTRA_MAIN...56300
      ELSE                                                               SUTRA_MAIN...56400
         ERRCOD = 'INP-4-4'                                              SUTRA_MAIN...56500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...56600
         GOTO 9000                                                       SUTRA_MAIN...56700
      END IF                                                             SUTRA_MAIN...56800
      WRITE(K3,210)                                                      SUTRA_MAIN...56900
  210 FORMAT(////11X,'S I M U L A T I O N   M O D E   ',                 SUTRA_MAIN...57000
     1   'O P T I O N S'/)                                               SUTRA_MAIN...57100
      IF(ISSTRA.EQ.1.AND.ISSFLO.NE.1) THEN                               SUTRA_MAIN...57200
         ERRCOD = 'INP-4-5'                                              SUTRA_MAIN...57300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...57400
         GOTO 9000                                                       SUTRA_MAIN...57500
      ENDIF                                                              SUTRA_MAIN...57600
      IF(IUNSAT.EQ.+1) WRITE(K3,215)                                     SUTRA_MAIN...57700
      IF(IUNSAT.EQ.0) WRITE(K3,216)                                      SUTRA_MAIN...57800
  215 FORMAT(11X,'- ALLOW UNSATURATED AND SATURATED FLOW:  UNSATURATED', SUTRA_MAIN...57900
     1   ' PROPERTIES ARE USER-PROGRAMMED IN SUBROUTINE   U N S A T')    SUTRA_MAIN...58000
  216 FORMAT(11X,'- ASSUME SATURATED FLOW ONLY')                         SUTRA_MAIN...58100
      IF(ISSFLO.EQ.+1.AND.ME.EQ.-1) WRITE(K3,219)                        SUTRA_MAIN...58200
      IF(ISSFLO.EQ.+1.AND.ME.EQ.+1) WRITE(K3,220)                        SUTRA_MAIN...58300
      IF(ISSFLO.EQ.0) WRITE(K3,221)                                      SUTRA_MAIN...58400
  219 FORMAT(11X,'- ASSUME STEADY-STATE FLOW FIELD CONSISTENT WITH ',    SUTRA_MAIN...58500
     1   'INITIAL CONCENTRATION CONDITIONS')                             SUTRA_MAIN...58600
  220 FORMAT(11X,'- ASSUME STEADY-STATE FLOW FIELD CONSISTENT WITH ',    SUTRA_MAIN...58700
     1   'INITIAL TEMPERATURE CONDITIONS')                               SUTRA_MAIN...58800
  221 FORMAT(11X,'- ALLOW TIME-DEPENDENT FLOW FIELD')                    SUTRA_MAIN...58900
      IF(ISSTRA.EQ.+1) WRITE(K3,225)                                     SUTRA_MAIN...59000
      IF(ISSTRA.EQ.0) WRITE(K3,226)                                      SUTRA_MAIN...59100
  225 FORMAT(11X,'- ASSUME STEADY-STATE TRANSPORT')                      SUTRA_MAIN...59200
  226 FORMAT(11X,'- ALLOW TIME-DEPENDENT TRANSPORT')                     SUTRA_MAIN...59300
      IF(IREAD.EQ.-1) WRITE(K3,230)                                      SUTRA_MAIN...59400
      IF(IREAD.EQ.+1) WRITE(K3,231)                                      SUTRA_MAIN...59500
  230 FORMAT(11X,'- WARM START - SIMULATION IS TO BE ',                  SUTRA_MAIN...59600
     1   'CONTINUED FROM PREVIOUSLY-STORED DATA')                        SUTRA_MAIN...59700
  231 FORMAT(11X,'- COLD START - BEGIN NEW SIMULATION')                  SUTRA_MAIN...59800
      IF(ISTORE.GT.0) WRITE(K3,240) ISTORE                               SUTRA_MAIN...59900
      IF(ISTORE.EQ.0) WRITE(K3,241)                                      SUTRA_MAIN...60000
  240 FORMAT(11X,'- STORE RESULTS AFTER EVERY',I9,' TIME STEPS IN',      SUTRA_MAIN...60100
     1   ' RESTART FILE AS BACKUP AND FOR USE IN A SIMULATION RESTART')  SUTRA_MAIN...60200
  241 FORMAT(11X,'- DO NOT STORE RESULTS FOR USE IN A',                  SUTRA_MAIN...60300
     1   ' RESTART OF SIMULATION')                                       SUTRA_MAIN...60400
C.....OUTPUT DATASET 3                                                   SUTRA_MAIN...60500
      IF(ME.EQ.-1)                                                       SUTRA_MAIN...60600
     1   WRITE(K3,245) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS                    SUTRA_MAIN...60700
  245 FORMAT(////11X,'S I M U L A T I O N   C O N T R O L   ',           SUTRA_MAIN...60800
     1   'N U M B E R S'// 8X,I9,5X,'NUMBER OF NODES IN FINITE-',        SUTRA_MAIN...60900
     2   'ELEMENT MESH'/ 8X,I9,5X,'NUMBER OF ELEMENTS IN MESH'//         SUTRA_MAIN...61000
     3    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...61100
     4   'PRESSURE IS A SPECIFIED CONSTANT OR FUNCTION OF TIME'/         SUTRA_MAIN...61200
     5    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...61300
     6   'SOLUTE CONCENTRATION IS A SPECIFIED CONSTANT OR ',             SUTRA_MAIN...61400
     7   'FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF NODES AT',       SUTRA_MAIN...61500
     8   ' WHICH FLUID INFLOW OR OUTFLOW IS A SPECIFIED CONSTANT',       SUTRA_MAIN...61600
     9   ' OR FUNCTION OF TIME'/ 8X,I9,5X,'EXACT NUMBER OF NODES AT',    SUTRA_MAIN...61700
     A   ' WHICH A SOURCE OR SINK OF SOLUTE MASS IS A SPECIFIED ',       SUTRA_MAIN...61800
     B   'CONSTANT OR FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF ',   SUTRA_MAIN...61900
     C   'NODES AT WHICH PRESSURE AND CONCENTRATION WILL BE OBSERVED')   SUTRA_MAIN...62000
C                                                                        SUTRA_MAIN...62100
      IF(ME.EQ.+1)                                                       SUTRA_MAIN...62200
     1    WRITE(K3,247) NN,NE,NPBC,NUBC,NSOP,NSOU,NOBS                   SUTRA_MAIN...62300
  247 FORMAT(////11X,'S I M U L A T I O N   C O N T R O L   ',           SUTRA_MAIN...62400
     1   'N U M B E R S'// 8X,I9,5X,'NUMBER OF NODES IN FINITE-',        SUTRA_MAIN...62500
     2   'ELEMENT MESH'/ 8X,I9,5X,'NUMBER OF ELEMENTS IN MESH'//         SUTRA_MAIN...62600
     3    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...62700
     4   'PRESSURE IS A SPECIFIED CONSTANT OR FUNCTION OF TIME'/         SUTRA_MAIN...62800
     5    8X,I9,5X,'EXACT NUMBER OF NODES IN MESH AT WHICH ',            SUTRA_MAIN...62900
     6   'TEMPERATURE IS A SPECIFIED CONSTANT OR ',                      SUTRA_MAIN...63000
     7   'FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF NODES AT',       SUTRA_MAIN...63100
     8   ' WHICH FLUID INFLOW OR OUTFLOW IS A SPECIFIED CONSTANT',       SUTRA_MAIN...63200
     9   ' OR FUNCTION OF TIME'/ 8X,I9,5X,'EXACT NUMBER OF NODES AT',    SUTRA_MAIN...63300
     A   ' WHICH A SOURCE OR SINK OF ENERGY IS A SPECIFIED CONSTANT',    SUTRA_MAIN...63400
     B   ' OR FUNCTION OF TIME'// 8X,I9,5X,'EXACT NUMBER OF NODES ',     SUTRA_MAIN...63500
     C   'AT WHICH PRESSURE AND TEMPERATURE WILL BE OBSERVED')           SUTRA_MAIN...63600
C                                                                        SUTRA_MAIN...63700
C.....INPUT DATASETS 5 - 7 (NUMERICAL, TEMPORAL, AND ITERATION CONTROLS) SUTRA_MAIN...63800
      CALL INDAT0()                                                      SUTRA_MAIN...63900
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...64000
C.....KSOLVP AND KSOLVU HAVE BEEN SET ACCORDING TO THE SOLVERS SELECTED: SUTRA_MAIN...64100
C        BANDED GAUSSIAN ELIMINATION (DIRECT)   ==>   0                  SUTRA_MAIN...64200
C        IC-PRECONDITIONED CG                   ==>   1                  SUTRA_MAIN...64300
C        ILU-PRECONDITIONED GMRES               ==>   2                  SUTRA_MAIN...64400
C        ILU-PRECONDITIONED ORTHOMIN            ==>   3                  SUTRA_MAIN...64500
C                                                                        SUTRA_MAIN...64600
C.....OUTPUT DATASETS 7B & 7C                                            SUTRA_MAIN...64700
      WRITE(K3,261)                                                      SUTRA_MAIN...64800
  261 FORMAT(////11X,'S O L V E R - R E L A T E D   ',                   SUTRA_MAIN...64900
     1   'P A R A M E T E R S')                                          SUTRA_MAIN...65000
      IF ((KTYPE.GT.0).AND.(KSOLVP.NE.0)) THEN                           SUTRA_MAIN...65100
         ERRCOD = 'INP-7B&C-4'                                           SUTRA_MAIN...65200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...65300
         GOTO 9000                                                       SUTRA_MAIN...65400
      ENDIF                                                              SUTRA_MAIN...65500
C.....OUTPUT DATASETS 3B & 3C                                            SUTRA_MAIN...65600
  266 IF (KSOLVP.NE.0) THEN                                              SUTRA_MAIN...65700
      WRITE(K3,268) NN1, NN2, NN3,                                       SUTRA_MAIN...65800
     1   SOLNAM(KSOLVP), ITRMXP, TOLP,                                   SUTRA_MAIN...65900
     2   SOLNAM(KSOLVU), ITRMXU, TOLU                                    SUTRA_MAIN...66000
  268 FORMAT(                                                            SUTRA_MAIN...66100
     1   /8X,I9,5X,'NUMBER OF NODES IN 1ST NUMBERING DIRECTION'          SUTRA_MAIN...66200
     2   /8X,I9,5X,'NUMBER OF NODES IN 2ND NUMBERING DIRECTION'          SUTRA_MAIN...66300
     3   /8X,I9,5X,'NUMBER OF NODES IN 3RD NUMBERING DIRECTION'          SUTRA_MAIN...66400
     3   //13X,'SOLVER FOR P: ',A40                                      SUTRA_MAIN...66500
     4   //20X,I6,5X,'MAXIMUM NUMBER OF MATRIX SOLVER ITERATIONS',       SUTRA_MAIN...66600
     5        ' DURING P SOLUTION'                                       SUTRA_MAIN...66700
     8   /11X,1PD15.4,5X,'CONVERGENCE TOLERANCE FOR MATRIX',             SUTRA_MAIN...66800
     9        ' SOLVER ITERATIONS DURING P SOLUTION'                     SUTRA_MAIN...66900
     1   //13X,'SOLVER FOR U: ',A40                                      SUTRA_MAIN...67000
     2   //20X,I6,5X,'MAXIMUM NUMBER OF MATRIX SOLVER ITERATIONS',       SUTRA_MAIN...67100
     3        ' DURING U SOLUTION'                                       SUTRA_MAIN...67200
     6   /11X,1PD15.4,5X,'CONVERGENCE TOLERANCE FOR MATRIX',             SUTRA_MAIN...67300
     7        ' SOLVER ITERATIONS DURING U SOLUTION' )                   SUTRA_MAIN...67400
      ELSE                                                               SUTRA_MAIN...67500
      WRITE(K3,269) SOLNAM(KSOLVP)                                       SUTRA_MAIN...67600
  269 FORMAT(/13X,'SOLVER FOR P AND U: ',A40)                            SUTRA_MAIN...67700
      END IF                                                             SUTRA_MAIN...67800
C                                                                        SUTRA_MAIN...67900
C                                                                        SUTRA_MAIN...68000
C.....CALCULATE THE NUMBER OF TIME STEPS ON WHICH OBSERVATIONS WILL      SUTRA_MAIN...68100
C        BE MADE, NTOBS.  THIS REQUIRES LOOKING AHEAD TO DATASET 8 OF    SUTRA_MAIN...68200
C        THE INP FILE AND DATASET 1 OF THE ICS FILE.  THE FILES ARE THEN SUTRA_MAIN...68300
C        "BACKSPACED" SO THAT THESE DATASETS CAN BE READ AGAIN LATER     SUTRA_MAIN...68400
C        BY SUBROUTINES INDAT1 AND INDAT2.                               SUTRA_MAIN...68500
C                                                                        SUTRA_MAIN...68600
      NTOBS = 0                                                          SUTRA_MAIN...68700
      IF (NOBS.EQ.0) GOTO 311                                            SUTRA_MAIN...68800
      NLSTOT = 0                                                         SUTRA_MAIN...68900
      DO 307 I=1,4                                                       SUTRA_MAIN...69000
         ERRCOD = 'REA-INP-S8' // LETTER(I)                              SUTRA_MAIN...69100
         CALL SKPCOM(K1, NLSKIP, ERRCOD)                                 SUTRA_MAIN...69200
         IF (ISERR) GOTO 9000                                            SUTRA_MAIN...69300
         NLSTOT = NLSTOT + NLSKIP                                        SUTRA_MAIN...69400
         ERRCOD = 'REA-INP-8' // LETTER(I)                               SUTRA_MAIN...69500
         CALL READIF(K1, INTFIL, ERRCOD)                                 SUTRA_MAIN...69600
         IF (ISERR) GOTO 9000                                            SUTRA_MAIN...69700
         READ(INTFIL,*,IOSTAT=INERR(1)) NOBCYC                           SUTRA_MAIN...69800
         IF (INERR(1).NE.0) THEN                                         SUTRA_MAIN...69900
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     SUTRA_MAIN...70000
            GOTO 9000                                                    SUTRA_MAIN...70100
         END IF                                                          SUTRA_MAIN...70200
  307 CONTINUE                                                           SUTRA_MAIN...70300
      DO 309 I=1,NLSTOT+4                                                SUTRA_MAIN...70400
  309    BACKSPACE(K1)                                                   SUTRA_MAIN...70500
      ERRCOD = 'REA-ICS-S1'                                              SUTRA_MAIN...70600
      CALL SKPCOM(K2, NLSKIP, ERRCOD)                                    SUTRA_MAIN...70700
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...70800
      ERRCOD = 'REA-ICS-1'                                               SUTRA_MAIN...70900
      CALL READIF(K2, INTFIL, ERRCOD)                                    SUTRA_MAIN...71000
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...71100
      READ(INTFIL,*,IOSTAT=INERR(1)) TSTART                              SUTRA_MAIN...71200
      IF (INERR(1).NE.0) THEN                                            SUTRA_MAIN...71300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA_MAIN...71400
         GOTO 9000                                                       SUTRA_MAIN...71500
      END IF                                                             SUTRA_MAIN...71600
      BACKSPACE(K2)                                                      SUTRA_MAIN...71700
      TS=TSTART                                                          SUTRA_MAIN...71800
      JT=0                                                               SUTRA_MAIN...71900
      IF (ISSTRA.NE.1) THEN                                              SUTRA_MAIN...72000
         KT = 1                                                          SUTRA_MAIN...72100
      ELSE                                                               SUTRA_MAIN...72200
         KT = 0                                                          SUTRA_MAIN...72300
      END IF                                                             SUTRA_MAIN...72400
      DELTK=DELT                                                         SUTRA_MAIN...72500
  310 CONTINUE                                                           SUTRA_MAIN...72600
         JT=JT+1                                                         SUTRA_MAIN...72700
         IF (MOD(JT,ITCYC).EQ.0 .AND. JT.GT.1) DELTK=DELTK*DTMULT        SUTRA_MAIN...72800
         IF (DELTK.GT.DTMAX) DELTK=DTMAX                                 SUTRA_MAIN...72900
         TS=TS+DELTK                                                     SUTRA_MAIN...73000
         IF (MOD(JT,NOBCYC).EQ.0 .OR.                                    SUTRA_MAIN...73100
     1      ((JT.EQ.1).AND.((ISSTRA.NE.0).OR.(NOBCYC.GT.0))))            SUTRA_MAIN...73200
     2      KT = KT + 1                                                  SUTRA_MAIN...73300
      IF(JT.LT.ITMAX .AND. TS.LT.TMAX) GOTO 310                          SUTRA_MAIN...73400
      JTMAX = JT                                                         SUTRA_MAIN...73500
      IF(JTMAX.GT.1 .AND. MOD(JT,NOBCYC).NE.0) KT = KT + 1               SUTRA_MAIN...73600
      NTOBS = KT                                                         SUTRA_MAIN...73700
  311 CONTINUE                                                           SUTRA_MAIN...73800
C                                                                        SUTRA_MAIN...73900
C.....CALCULATE ARRAY DIMENSIONS, EXCEPT THOSE THAT DEPEND ON BANDWIDTH  SUTRA_MAIN...74000
C                                                                        SUTRA_MAIN...74100
      IF (KSOLVP.EQ.0) THEN                                              SUTRA_MAIN...74200
C........SET DIMENSIONS FOR DIRECT SOLVER                                SUTRA_MAIN...74300
         NNNX = 1                                                        SUTRA_MAIN...74400
         NELTA = NN                                                      SUTRA_MAIN...74500
         NELT = NELTA                                                    SUTRA_MAIN...74600
         NDIMJA = NELT                                                   SUTRA_MAIN...74700
         NNVEC = NN                                                      SUTRA_MAIN...74800
         NWI = 1                                                         SUTRA_MAIN...74900
         NWF = 1                                                         SUTRA_MAIN...75000
      ELSE                                                               SUTRA_MAIN...75100
C........SET DIMENSIONS FOR ITERATIVE SOLVER(S)                          SUTRA_MAIN...75200
         NNNX = NN                                                       SUTRA_MAIN...75300
         IF (IABS(KTYPE).EQ.2) THEN                                      SUTRA_MAIN...75400
            NELTA = 9*NN - 6*NN1 - 2                                     SUTRA_MAIN...75500
         ELSE                                                            SUTRA_MAIN...75600
            NELTA = 27*NN - 6*NN1*(1 + 3*NN2) - 2                        SUTRA_MAIN...75700
         END IF                                                          SUTRA_MAIN...75800
         NELT = NELTA                                                    SUTRA_MAIN...75900
         NDIMJA = NELT                                                   SUTRA_MAIN...76000
         NNVEC = NN                                                      SUTRA_MAIN...76100
         KSOLVR = KSOLVP                                                 SUTRA_MAIN...76200
         NSAVE = NSAVEP                                                  SUTRA_MAIN...76300
         CALL DIMWRK(KSOLVR, NSAVE, NN, NELTA, NWIP, NWFP)               SUTRA_MAIN...76400
         KSOLVR = KSOLVU                                                 SUTRA_MAIN...76500
         NSAVE = NSAVEU                                                  SUTRA_MAIN...76600
         CALL DIMWRK(KSOLVR, NSAVE, NN, NELTA, NWIU, NWFU)               SUTRA_MAIN...76700
         NWI = MAX(NWIP, NWIU)                                           SUTRA_MAIN...76800
         NWF = MAX(NWFP, NWFU)                                           SUTRA_MAIN...76900
      END IF                                                             SUTRA_MAIN...77000
      NBCN=NPBC+NUBC+1                                                   SUTRA_MAIN...77100
      NSOP=NSOP+1                                                        SUTRA_MAIN...77200
      NSOU=NSOU+1                                                        SUTRA_MAIN...77300
      NIN=NE*8                                                           SUTRA_MAIN...77400
      NOBSN=NOBS+1                                                       SUTRA_MAIN...77500
      IF (IABS(KTYPE).EQ.3) THEN                                         SUTRA_MAIN...77600
         NEV = NEV3                                                      SUTRA_MAIN...77700
         N48 = 8                                                         SUTRA_MAIN...77800
         NEX = NE                                                        SUTRA_MAIN...77900
      ELSE                                                               SUTRA_MAIN...78000
         NEV = NEV2                                                      SUTRA_MAIN...78100
         N48 = 4                                                         SUTRA_MAIN...78200
         NEX = 1                                                         SUTRA_MAIN...78300
      END IF                                                             SUTRA_MAIN...78400
C.....NEXV IS THE NUMBER OF VECTORS THAT ARE OF LENGTH NE IN 3D AND      SUTRA_MAIN...78500
C        ARE TO BE DIMENSIONED TO LENGTH 1 IN 2D BECAUSE THEY ARE NOT    SUTRA_MAIN...78600
C        NEEDED.  THUS, IN 3D, NEXV=0; IN 2D, NEXV=NEV3-NEV2.            SUTRA_MAIN...78700
      NEVX = NEV3 - NEV                                                  SUTRA_MAIN...78800
      NEVG = 2*N48                                                       SUTRA_MAIN...78900
      NE8 = NE*N48                                                       SUTRA_MAIN...79000
      NE8X = NEX*N48                                                     SUTRA_MAIN...79100
C                                                                        SUTRA_MAIN...79200
C.....ALLOCATE REAL ARRAYS, EXCEPT THOSE THAT DEPEND ON BANDWIDTH        SUTRA_MAIN...79300
      ALLOCATE(PITER(NN),UITER(NN),PM1(NN),DPDTITR(NN),UM1(NN),UM2(NN),  SUTRA_MAIN...79400
     1   PVEL(NN),SL(NN),SR(NN),X(NN),Y(NN),Z(NN),VOL(NN),POR(NN),       SUTRA_MAIN...79500
     2   CS1(NN),CS2(NN),CS3(NN),SW(NN),DSWDP(NN),RHO(NN),SOP(NN),       SUTRA_MAIN...79600
     3   QIN(NN),UIN(NN),QUIN(NN),QINITR(NN),RCIT(NN),RCITM1(NN))        SUTRA_MAIN...79700
      ALLOCATE(PVEC(NNVEC),UVEC(NNVEC))                                  SUTRA_MAIN...79800
      ALLOCATE(ALMAX(NE),ALMIN(NE),ATMAX(NE),ATMIN(NE),VMAG(NE),         SUTRA_MAIN...79900
     1   VANG1(NE),PERMXX(NE),PERMXY(NE),PERMYX(NE),PERMYY(NE),          SUTRA_MAIN...80000
     2   PANGL1(NE))                                                     SUTRA_MAIN...80100
      ALLOCATE(ALMID(NEX),ATMID(NEX),                                    SUTRA_MAIN...80200
     1   VANG2(NEX),PERMXZ(NEX),PERMYZ(NEX),PERMZX(NEX),                 SUTRA_MAIN...80300
     2   PERMZY(NEX),PERMZZ(NEX),PANGL2(NEX),PANGL3(NEX))                SUTRA_MAIN...80400
      ALLOCATE(PBC(NBCN),UBC(NBCN),QPLITR(NBCN))                         SUTRA_MAIN...80500
      ALLOCATE(GXSI(NE,N48),GETA(NE,N48),GZET(NEX,N48))                  SUTRA_MAIN...80600
      ALLOCATE(FWK(NWF),B(NNNX))                                         SUTRA_MAIN...80700
C.....ALLOCATE INTEGER ARRAYS, EXCEPT THOSE THAT DEPEND ON BANDWIDTH     SUTRA_MAIN...80800
      ALLOCATE(IN(NIN),IQSOP(NSOP),IQSOU(NSOU),IPBC(NBCN),IUBC(NBCN),    SUTRA_MAIN...80900
     1   IOBS(NOBSN),NREG(NN),LREG(NE),IWK(NWI),IA(NELT),JA(NDIMJA))     SUTRA_MAIN...81000
      ALLO1 = .TRUE.                                                     SUTRA_MAIN...81100
C                                                                        SUTRA_MAIN...81200
C.....INPUT DATASETS 8 - 15 (OUTPUT CONTROLS; FLUID AND SOLID MATRIX     SUTRA_MAIN...81300
C        PROPERTIES; ADSORPTION PARAMETERS; PRODUCTION OF ENERGY OR      SUTRA_MAIN...81400
C        SOLUTE MASS; GRAVITY; AND NODEWISE AND ELEMENTWISE DATA)        SUTRA_MAIN...81500
      CALL INDAT1(X,Y,Z,POR,ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,         SUTRA_MAIN...81600
     1   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,                      SUTRA_MAIN...81700
     2   PERMZX,PERMZY,PERMZZ,PANGL1,PANGL2,PANGL3,SOP,NREG,LREG,IOBS)   SUTRA_MAIN...81800
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...81900
C                                                                        SUTRA_MAIN...82000
C.....INPUT DATASETS 17 & 18 (SOURCES OF FLUID MASS AND ENERGY OR        SUTRA_MAIN...82100
C        SOLUTE MASS)                                                    SUTRA_MAIN...82200
      CALL ZERO(QIN,NN,0.0D0)                                            SUTRA_MAIN...82300
      CALL ZERO(UIN,NN,0.0D0)                                            SUTRA_MAIN...82400
      CALL ZERO(QUIN,NN,0.0D0)                                           SUTRA_MAIN...82500
      IF(NSOP-1.GT.0.OR.NSOU-1.GT.0) THEN                                SUTRA_MAIN...82600
         CALL SOURCE(QIN,UIN,IQSOP,QUIN,IQSOU,IQSOPT,IQSOUT)             SUTRA_MAIN...82700
         IF (ISERR) GOTO 9000                                            SUTRA_MAIN...82800
      END IF                                                             SUTRA_MAIN...82900
C                                                                        SUTRA_MAIN...83000
C.....INPUT DATASETS 19 & 20 (SPECIFIED P AND U BOUNDARY CONDITIONS)     SUTRA_MAIN...83100
      IF(NBCN-1.GT.0) THEN                                               SUTRA_MAIN...83200
         CALL BOUND(IPBC,PBC,IUBC,UBC,IPBCT,IUBCT)                       SUTRA_MAIN...83300
         IF (ISERR) GOTO 9000                                            SUTRA_MAIN...83400
      END IF                                                             SUTRA_MAIN...83500
C                                                                        SUTRA_MAIN...83600
C.....INPUT DATASET 22 (ELEMENT INCIDENCE [MESH CONNECTION] DATA)        SUTRA_MAIN...83700
      CALL CONNEC(IN)                                                    SUTRA_MAIN...83800
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...83900
C                                                                        SUTRA_MAIN...84000
C.....CALCULATE BANDWIDTH                                                SUTRA_MAIN...84100
      CALL BANWID(IN)                                                    SUTRA_MAIN...84200
C                                                                        SUTRA_MAIN...84300
C.....CALCULATE ARRAY DIMENSIONS THAT DEPEND ON BANDWIDTH                SUTRA_MAIN...84400
      IF (KSOLVP.EQ.0) THEN                                              SUTRA_MAIN...84500
C........SET DIMENSIONS FOR DIRECT SOLVER                                SUTRA_MAIN...84600
         NCBI = NBI                                                      SUTRA_MAIN...84700
         NBIX = 1                                                        SUTRA_MAIN...84800
      ELSE                                                               SUTRA_MAIN...84900
C........SET DIMENSIONS FOR ITERATIVE SOLVER(S)                          SUTRA_MAIN...85000
         NCBI = 1                                                        SUTRA_MAIN...85100
         NBIX = NBI                                                      SUTRA_MAIN...85200
      END IF                                                             SUTRA_MAIN...85300
      MATDIM=NELT*NCBI                                                   SUTRA_MAIN...85400
C                                                                        SUTRA_MAIN...85500
C.....ALLOCATE REAL AND INTEGER ARRAYS THAT DEPEND ON BANDWIDTH          SUTRA_MAIN...85600
      ALLOCATE(PMAT(NELT,NCBI),UMAT(NELT,NCBI))                          SUTRA_MAIN...85700
      ALLOCATE(NBI27(NBIX))                                              SUTRA_MAIN...85800
      ALLO2 = .TRUE.                                                     SUTRA_MAIN...85900
C                                                                        SUTRA_MAIN...86000
C.....INPUT INITIAL OR RESTART CONDITIONS FROM THE ICS FILE AND          SUTRA_MAIN...86100
C        INITIALIZE PARAMETERS                                           SUTRA_MAIN...86200
      CALL INDAT2(PVEC,UVEC,PM1,UM1,UM2,CS1,CS2,CS3,SL,SR,RCIT,SW,DSWDP, SUTRA_MAIN...86300
     1   PBC,IPBC,IPBCT,NREG,QIN,DPDTITR)                                SUTRA_MAIN...86400
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...86500
C                                                                        SUTRA_MAIN...86600
C.....COMPUTE AND OUTPUT DIMENSIONS OF SIMULATION                        SUTRA_MAIN...86700
      MRMD=2*NELT*NCBI                                                   SUTRA_MAIN...86800
      MRVD=NNV*NN+(NEV+NEVG)*NE+NBCN*3+NWF+NNNX+NEVX+NE8X                SUTRA_MAIN...86900
      MIMVD=NE*9+NN+NSOP+NSOU+NBCN*2+NOBSN+NBIX+NWI+NELT+NDIMJA          SUTRA_MAIN...87000
      RMDIM = MRMD                                                       SUTRA_MAIN...87100
      RVDIM = MRVD                                                       SUTRA_MAIN...87200
      RMVDIM = RMDIM + RVDIM                                             SUTRA_MAIN...87300
      IMVDIM = MIMVD                                                     SUTRA_MAIN...87400
      TOTMB = (DBLE(RMVDIM)*8D0 + DBLE(IMVDIM)*4D0)/1000000.0            SUTRA_MAIN...87500
      WRITE(K3,3000) RMVDIM, IMVDIM, TOTMB                               SUTRA_MAIN...87600
 3000 FORMAT(////11X,'S I M U L A T I O N   D I M E N S I O N S'//       SUTRA_MAIN...87700
     1   13X,'REAL    ARRAYS WERE ALLOCATED ',I12/                       SUTRA_MAIN...87800
     2   13X,'INTEGER ARRAYS WERE ALLOCATED ',I12//                      SUTRA_MAIN...87900
     3   13X,F10.3,' Mbytes MEMORY USED FOR MAIN ARRAYS')                SUTRA_MAIN...88000
C                                                                        SUTRA_MAIN...88100
      WRITE(K3,4000)                                                     SUTRA_MAIN...88200
 4000 FORMAT(////////8(132("-")/))                                       SUTRA_MAIN...88300
C                                                                        SUTRA_MAIN...88400
C.....CALL MAIN CONTROL ROUTINE, SUTRA                                   SUTRA_MAIN...88500
      CALL SUTRA(TITLE1,TITLE2,PMAT,UMAT,PITER,UITER,PM1,DPDTITR,        SUTRA_MAIN...88600
     1   UM1,UM2,PVEL,SL,SR,X,Y,Z,VOL,POR,CS1,CS2,CS3,SW,DSWDP,RHO,SOP,  SUTRA_MAIN...88700
     2   QIN,UIN,QUIN,QINITR,RCIT,RCITM1,PVEC,UVEC,                      SUTRA_MAIN...88800
     3   ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,VMAG,VANG1,VANG2,           SUTRA_MAIN...88900
     4   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, SUTRA_MAIN...89000
     5   PANGL1,PANGL2,PANGL3,PBC,UBC,QPLITR,GXSI,GETA,GZET,FWK,B,       SUTRA_MAIN...89100
     6   IN,IQSOP,IQSOU,IPBC,IUBC,IOBS,NREG,LREG,NBI27,IWK,IA,JA,        SUTRA_MAIN...89200
     7   IQSOPT,IQSOUT,IPBCT,IUBCT)                                      SUTRA_MAIN...89300
      IF (ISERR) GOTO 9000                                               SUTRA_MAIN...89400
C                                                                        SUTRA_MAIN...89500
C.....TERMINATION SEQUENCE: DEALLOCATE ARRAYS, CLOSE FILES, AND END      SUTRA_MAIN...89600
9000  CONTINUE                                                           SUTRA_MAIN...89700
      IF (ALLO1) THEN                                                    SUTRA_MAIN...89800
         DEALLOCATE(PITER,UITER,PM1,DPDTITR,UM1,UM2,PVEL,SL,SR,X,Y,Z,    SUTRA_MAIN...89900
     1      VOL,POR,CS1,CS2,CS3,SW,DSWDP,RHO,SOP,QIN,UIN,QUIN,QINITR,    SUTRA_MAIN...90000
     2      RCIT,RCITM1)                                                 SUTRA_MAIN...90100
         DEALLOCATE(PVEC,UVEC)                                           SUTRA_MAIN...90200
         DEALLOCATE(ALMAX,ALMIN,ATMAX,ATMIN,VMAG,VANG1,PERMXX,PERMXY,    SUTRA_MAIN...90300
     1      PERMYX,PERMYY,PANGL1)                                        SUTRA_MAIN...90400
         DEALLOCATE(ALMID,ATMID,VANG2,PERMXZ,PERMYZ,PERMZX,PERMZY,       SUTRA_MAIN...90500
     1      PERMZZ,PANGL2,PANGL3)                                        SUTRA_MAIN...90600
         DEALLOCATE(PBC,UBC,QPLITR)                                      SUTRA_MAIN...90700
         DEALLOCATE(GXSI,GETA,GZET)                                      SUTRA_MAIN...90800
         DEALLOCATE(FWK,B)                                               SUTRA_MAIN...90900
         DEALLOCATE(IN,IQSOP,IQSOU,IPBC,IUBC,IOBS,NREG,LREG,IWK,IA,JA)   SUTRA_MAIN...91000
      END IF                                                             SUTRA_MAIN...91100
      IF (ALLO2) THEN                                                    SUTRA_MAIN...91200
         DEALLOCATE(PMAT,UMAT)                                           SUTRA_MAIN...91300
         DEALLOCATE(NBI27)                                               SUTRA_MAIN...91400
      END IF                                                             SUTRA_MAIN...91500
      CLOSE(K00)                                                         SUTRA_MAIN...91600
      CLOSE(K0)                                                          SUTRA_MAIN...91700
      CLOSE(K1)                                                          SUTRA_MAIN...91800
      CLOSE(K2)                                                          SUTRA_MAIN...91900
      CLOSE(K3)                                                          SUTRA_MAIN...92000
      CLOSE(K4)                                                          SUTRA_MAIN...92100
      CLOSE(K5)                                                          SUTRA_MAIN...92200
      CLOSE(K6)                                                          SUTRA_MAIN...92300
      CLOSE(K7)                                                          SUTRA_MAIN...92400
      IF ((KSCRN.EQ.1).AND.(KPAUSE.EQ.1)) THEN                           SUTRA_MAIN...92500
         WRITE(*,9990)                                                   SUTRA_MAIN...92600
9990     FORMAT(/' Press ENTER to exit ...')                             SUTRA_MAIN...92700
         READ(*,'(A1)') CDUM                                             SUTRA_MAIN...92800
      END IF                                                             SUTRA_MAIN...92900
      END                                                                SUTRA_MAIN...93000
C                                                                        SUTRA_MAIN...93100
C     SUBROUTINE        A  D  S  O  R  B           SUTRA VERSION 2D3D.1  ADSORB.........100
C                                                                        ADSORB.........200
C *** PURPOSE :                                                          ADSORB.........300
C ***  TO CALCULATE VALUES OF EQUILIBRIUM SORPTION PARAMETERS FOR        ADSORB.........400
C ***  LINEAR, FREUNDLICH, AND LANGMUIR MODELS.                          ADSORB.........500
C                                                                        ADSORB.........600
      SUBROUTINE ADSORB(CS1,CS2,CS3,SL,SR,U)                             ADSORB.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                ADSORB.........800
      CHARACTER*10 ADSMOD                                                ADSORB.........900
      DIMENSION CS1(NN),CS2(NN),CS3(NN),SL(NN),SR(NN),U(NN)              ADSORB........1000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              ADSORB........1100
     1   NSOP,NSOU,NBCN                                                  ADSORB........1200
      COMMON /MODSOR/ ADSMOD                                             ADSORB........1300
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      ADSORB........1400
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        ADSORB........1500
C                                                                        ADSORB........1600
C.....NOTE THAT THE CONCENTRATION OF ADSORBATE, CS(I), IS GIVEN BY       ADSORB........1700
C        CS(I) = SL(I)*U(I) + SR(I)                                      ADSORB........1800
C                                                                        ADSORB........1900
C.....NO SORPTION                                                        ADSORB........2000
      IF(ADSMOD.NE.'NONE      ') GOTO 450                                ADSORB........2100
      DO 250 I=1,NN                                                      ADSORB........2200
      CS1(I)=0.D0                                                        ADSORB........2300
      CS2(I)=0.D0                                                        ADSORB........2400
      CS3(I)=0.D0                                                        ADSORB........2500
      SL(I)=0.D0                                                         ADSORB........2600
      SR(I)=0.D0                                                         ADSORB........2700
  250 CONTINUE                                                           ADSORB........2800
      GOTO 2000                                                          ADSORB........2900
C                                                                        ADSORB........3000
C.....LINEAR SORPTION MODEL                                              ADSORB........3100
  450 IF(ADSMOD.NE.'LINEAR    ') GOTO 700                                ADSORB........3200
      DO 500 I=1,NN                                                      ADSORB........3300
      CS1(I)=CHI1*RHOW0                                                  ADSORB........3400
      CS2(I)=0.D0                                                        ADSORB........3500
      CS3(I)=0.D0                                                        ADSORB........3600
      SL(I)=CHI1*RHOW0                                                   ADSORB........3700
      SR(I)=0.D0                                                         ADSORB........3800
  500 CONTINUE                                                           ADSORB........3900
      GOTO 2000                                                          ADSORB........4000
C                                                                        ADSORB........4100
C.....FREUNDLICH SORPTION MODEL                                          ADSORB........4200
  700 IF(ADSMOD.NE.'FREUNDLICH') GOTO 950                                ADSORB........4300
      CHCH=CHI1/CHI2                                                     ADSORB........4400
      DCHI2=1.D0/CHI2                                                    ADSORB........4500
      RH2=RHOW0**DCHI2                                                   ADSORB........4600
      CHI2F=((1.D0-CHI2)/CHI2)                                           ADSORB........4700
      DO 750 I=1,NN                                                      ADSORB........4800
      IF(U(I)) 720,720,730                                               ADSORB........4900
  720 UCH=1.0D0                                                          ADSORB........5000
      GOTO 740                                                           ADSORB........5100
  730 UCH=U(I)**CHI2F                                                    ADSORB........5200
  740 RU=RH2*UCH                                                         ADSORB........5300
      CS1(I)=CHCH*RU                                                     ADSORB........5400
      CS2(I)=0.D0                                                        ADSORB........5500
      CS3(I)=0.D0                                                        ADSORB........5600
      SL(I)=CHI1*RU                                                      ADSORB........5700
      SR(I)=0.D0                                                         ADSORB........5800
  750 CONTINUE                                                           ADSORB........5900
      GOTO 2000                                                          ADSORB........6000
C                                                                        ADSORB........6100
C.....LANGMUIR SORPTION MODEL                                            ADSORB........6200
  950 IF(ADSMOD.NE.'LANGMUIR  ') GOTO 2000                               ADSORB........6300
      DO 1000 I=1,NN                                                     ADSORB........6400
      DD=1.D0+CHI2*RHOW0*U(I)                                            ADSORB........6500
      CS1(I)=(CHI1*RHOW0)/(DD*DD)                                        ADSORB........6600
      CS2(I)=0.D0                                                        ADSORB........6700
      CS3(I)=0.D0                                                        ADSORB........6800
      SL(I)=CS1(I)                                                       ADSORB........6900
      SR(I)=CS1(I)*CHI2*RHOW0*U(I)*U(I)                                  ADSORB........7000
 1000 CONTINUE                                                           ADSORB........7100
C                                                                        ADSORB........7200
 2000 RETURN                                                             ADSORB........7300
      END                                                                ADSORB........7400
C                                                                        ADSORB........7500
C     SUBROUTINE        B  A  N  W  I  D           SUTRA VERSION 2D3D.1  BANWID.........100
C                                                                        BANWID.........200
C *** PURPOSE :                                                          BANWID.........300
C ***  TO CALCULATE THE BANDWIDTH OF THE FINITE ELEMENT MESH.            BANWID.........400
C                                                                        BANWID.........500
      SUBROUTINE BANWID(IN)                                              BANWID.........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BANWID.........700
      CHARACTER*80 ERRCOD, CHERR(10), FNAME(0:7)                         BANWID.........800
      DIMENSION IN(NIN)                                                  BANWID.........900
      DIMENSION INERR(10), RLERR(10)                                     BANWID........1000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BANWID........1100
     1   NSOP,NSOU,NBCN                                                  BANWID........1200
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   BANWID........1300
      COMMON /FNAMES/FNAME                                               BANWID........1400
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        BANWID........1500
      COMMON /SOLVI/ KSOLVP, KSOLVU, NN1, NN2, NN3                       BANWID........1600
C                                                                        BANWID........1700
      NDIF=0                                                             BANWID........1800
      II=0                                                               BANWID........1900
      WRITE(K3,100)                                                      BANWID........2000
  100 FORMAT(////11X,'**** MESH ANALYSIS ****'//)                        BANWID........2100
C                                                                        BANWID........2200
C.....FIND ELEMENT WITH MAXIMUM DIFFERENCE IN NODE NUMBERS               BANWID........2300
      DO 2000 L=1,NE                                                     BANWID........2400
      II=II+1                                                            BANWID........2500
      IELO=IN(II)                                                        BANWID........2600
      IEHI=IN(II)                                                        BANWID........2700
      DO 1000 I=2,N48                                                    BANWID........2800
      II=II+1                                                            BANWID........2900
      IF(IN(II).LT.IELO) IELO=IN(II)                                     BANWID........3000
 1000 IF(IN(II).GT.IEHI) IEHI=IN(II)                                     BANWID........3100
      NDIFF=IEHI-IELO                                                    BANWID........3200
      IF(NDIFF.GT.NDIF) THEN                                             BANWID........3300
       NDIF=NDIFF                                                        BANWID........3400
       LEM=L                                                             BANWID........3500
      ENDIF                                                              BANWID........3600
 2000 CONTINUE                                                           BANWID........3700
C                                                                        BANWID........3800
C.....CALCULATE FULL BANDWIDTH, NB.                                      BANWID........3900
      NB=2*NDIF+1                                                        BANWID........4000
      NBHALF=NDIF+1                                                      BANWID........4100
C.....NBI IS USED TO DIMENSION ARRAYS WHOSE SIZE DEPENDS ON THE          BANWID........4200
C        BANDWIDTH.  IT IS THE SAME AS THE ACTUAL BANDWIDTH, NB.         BANWID........4300
      NBI = NB                                                           BANWID........4400
      WRITE(K3,2500) NB,LEM                                              BANWID........4500
 2500 FORMAT(//13X,'MAXIMUM FULL BANDWIDTH, ',I5,                        BANWID........4600
     1   ', WAS CALCULATED IN ELEMENT ',I9)                              BANWID........4700
C                                                                        BANWID........4800
      RETURN                                                             BANWID........4900
      END                                                                BANWID........5000
C                                                                        BANWID........5100
C     SUBROUTINE        B  A  S  I  S  2           SUTRA VERSION 2D3D.1  BASIS2.........100
C                                                                        BASIS2.........200
C *** PURPOSE :                                                          BASIS2.........300
C ***  TO CALCULATE VALUES OF BASIS AND WEIGHTING FUNCTIONS AND THEIR    BASIS2.........400
C ***  DERIVATIVES, TRANSFORMATION MATRICES BETWEEN LOCAL AND GLOBAL     BASIS2.........500
C ***  COORDINATES AND PARAMETER VALUES AT A SPECIFIED POINT IN A        BASIS2.........600
C ***  QUADRILATERAL FINITE ELEMENT.  THIS SUBROUTINE HANDLES 2D         BASIS2.........700
C ***  CALCULATIONS ONLY; 3D CALCULATIONS ARE PERFORMED IN SUBROUTINE    BASIS2.........800
C ***  BASIS3.                                                           BASIS2.........900
C                                                                        BASIS2........1000
      SUBROUTINE BASIS2(ICALL,L,XLOC,YLOC,IN,X,Y,F,W,DET,                BASIS2........1100
     1   DFDXG,DFDYG,DWDXG,DWDYG,PITER,UITER,PVEL,POR,THICK,THICKG,      BASIS2........1200
     2   VXG,VYG,SWG,RHOG,VISCG,PORG,VGMAG,RELKG,                        BASIS2........1300
     3   PERMXX,PERMXY,PERMYX,PERMYY,CJ11,CJ12,CJ21,CJ22,                BASIS2........1400
     4   GXSI,GETA,RCIT,RCITM1,RGXG,RGYG,LREG)                           BASIS2........1500
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BASIS2........1600
      DOUBLE PRECISION XLOC,YLOC                                         BASIS2........1700
      DIMENSION IN(NIN),X(NN),Y(NN),UITER(NN),PITER(NN),PVEL(NN),        BASIS2........1800
     1   POR(NN),PERMXX(NE),PERMXY(NE),PERMYX(NE),PERMYY(NE),THICK(NN)   BASIS2........1900
      DIMENSION GXSI(NE,4),GETA(NE,4),RCIT(NN),RCITM1(NN),LREG(NE)       BASIS2........2000
      DIMENSION F(4),W(4),DFDXG(4),DFDYG(4),DWDXG(4),DWDYG(4)            BASIS2........2100
      DIMENSION FX(4),FY(4),AFX(4),AFY(4),                               BASIS2........2200
     1   DFDXL(4),DFDYL(4),DWDXL(4),DWDYL(4),                            BASIS2........2300
     2   XDW(4),YDW(4),XIIX(4),YIIY(4)                                   BASIS2........2400
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  BASIS2........2500
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             BASIS2........2600
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BASIS2........2700
     1   NSOP,NSOU,NBCN                                                  BASIS2........2800
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      BASIS2........2900
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        BASIS2........3000
      DATA XIIX/-1.D0,+1.D0,+1.D0,-1.D0/,                                BASIS2........3100
     1     YIIY/-1.D0,-1.D0,+1.D0,+1.D0/                                 BASIS2........3200
      SAVE XIIX,YIIY                                                     BASIS2........3300
C                                                                        BASIS2........3400
C                                                                        BASIS2........3500
C.....AT THIS LOCATION IN LOCAL COORDINATES, (XLOC,YLOC),                BASIS2........3600
C        CALCULATE SYMMETRIC WEIGHTING FUNCTIONS, F(I),                  BASIS2........3700
C        SPACE DERIVATIVES, DFDXG(I) AND DFDYG(I), AND                   BASIS2........3800
C        DETERMINANT OF JACOBIAN, DET.                                   BASIS2........3900
C                                                                        BASIS2........4000
      XF1=1.D0-XLOC                                                      BASIS2........4100
      XF2=1.D0+XLOC                                                      BASIS2........4200
      YF1=1.D0-YLOC                                                      BASIS2........4300
      YF2=1.D0+YLOC                                                      BASIS2........4400
C                                                                        BASIS2........4500
C.....CALCULATE BASIS FUNCTION, F.                                       BASIS2........4600
      FX(1)=XF1                                                          BASIS2........4700
      FX(2)=XF2                                                          BASIS2........4800
      FX(3)=XF2                                                          BASIS2........4900
      FX(4)=XF1                                                          BASIS2........5000
      FY(1)=YF1                                                          BASIS2........5100
      FY(2)=YF1                                                          BASIS2........5200
      FY(3)=YF2                                                          BASIS2........5300
      FY(4)=YF2                                                          BASIS2........5400
      DO 10 I=1,4                                                        BASIS2........5500
   10 F(I)=0.250D0*FX(I)*FY(I)                                           BASIS2........5600
C                                                                        BASIS2........5700
C.....CALCULATE DERIVATIVES WITH RESPECT TO LOCAL COORDINATES.           BASIS2........5800
      DO 20 I=1,4                                                        BASIS2........5900
      DFDXL(I)=XIIX(I)*0.250D0*FY(I)                                     BASIS2........6000
   20 DFDYL(I)=YIIY(I)*0.250D0*FX(I)                                     BASIS2........6100
C                                                                        BASIS2........6200
C.....CALCULATE ELEMENTS OF JACOBIAN MATRIX, CJ.                         BASIS2........6300
      CJ11=0.D0                                                          BASIS2........6400
      CJ12=0.D0                                                          BASIS2........6500
      CJ21=0.D0                                                          BASIS2........6600
      CJ22=0.D0                                                          BASIS2........6700
      DO 100 IL=1,4                                                      BASIS2........6800
      II=(L-1)*4+IL                                                      BASIS2........6900
      I=IN(II)                                                           BASIS2........7000
      CJ11=CJ11+DFDXL(IL)*X(I)                                           BASIS2........7100
      CJ12=CJ12+DFDXL(IL)*Y(I)                                           BASIS2........7200
      CJ21=CJ21+DFDYL(IL)*X(I)                                           BASIS2........7300
  100 CJ22=CJ22+DFDYL(IL)*Y(I)                                           BASIS2........7400
C                                                                        BASIS2........7500
C.....CALCULATE DETERMINANT OF JACOBIAN MATRIX.                          BASIS2........7600
      DET=CJ11*CJ22-CJ21*CJ12                                            BASIS2........7700
C                                                                        BASIS2........7800
C.....RETURN TO ELEMEN2 WITH JACOBIAN MATRIX ON FIRST TIME STEP.         BASIS2........7900
      IF(ICALL.EQ.0) RETURN                                              BASIS2........8000
C                                                                        BASIS2........8100
C.....CALCULATE ELEMENTS OF INVERSE JACOBIAN MATRIX, CIJ.                BASIS2........8200
      ODET=1.D0/DET                                                      BASIS2........8300
      CIJ11=+ODET*CJ22                                                   BASIS2........8400
      CIJ12=-ODET*CJ12                                                   BASIS2........8500
      CIJ21=-ODET*CJ21                                                   BASIS2........8600
      CIJ22=+ODET*CJ11                                                   BASIS2........8700
C                                                                        BASIS2........8800
C.....CALCULATE DERIVATIVES WITH RESPECT TO GLOBAL COORDINATES           BASIS2........8900
      DO 200 I=1,4                                                       BASIS2........9000
      DFDXG(I)=CIJ11*DFDXL(I)+CIJ12*DFDYL(I)                             BASIS2........9100
  200 DFDYG(I)=CIJ21*DFDXL(I)+CIJ22*DFDYL(I)                             BASIS2........9200
C                                                                        BASIS2........9300
C.....CALCULATE CONSISTENT COMPONENTS OF (RHO*GRAV) TERM IN LOCAL        BASIS2........9400
C        COORDINATES AT THIS LOCATION, (XLOC,YLOC)                       BASIS2........9500
      RGXL=0.D0                                                          BASIS2........9600
      RGYL=0.D0                                                          BASIS2........9700
      RGXLM1=0.D0                                                        BASIS2........9800
      RGYLM1=0.D0                                                        BASIS2........9900
      DO 800 IL=1,4                                                      BASIS2.......10000
      II=(L-1)*4+IL                                                      BASIS2.......10100
      I=IN(II)                                                           BASIS2.......10200
      ADFDXL=DABS(DFDXL(IL))                                             BASIS2.......10300
      ADFDYL=DABS(DFDYL(IL))                                             BASIS2.......10400
      RGXL=RGXL+RCIT(I)*GXSI(L,IL)*ADFDXL                                BASIS2.......10500
      RGYL=RGYL+RCIT(I)*GETA(L,IL)*ADFDYL                                BASIS2.......10600
      RGXLM1=RGXLM1+RCITM1(I)*GXSI(L,IL)*ADFDXL                          BASIS2.......10700
      RGYLM1=RGYLM1+RCITM1(I)*GETA(L,IL)*ADFDYL                          BASIS2.......10800
  800 CONTINUE                                                           BASIS2.......10900
C                                                                        BASIS2.......11000
C.....TRANSFORM CONSISTENT COMPONENTS OF (RHO*GRAV) TERM TO              BASIS2.......11100
C        GLOBAL COORDINATES                                              BASIS2.......11200
      RGXG=CIJ11*RGXL+CIJ12*RGYL                                         BASIS2.......11300
      RGYG=CIJ21*RGXL+CIJ22*RGYL                                         BASIS2.......11400
      RGXGM1=CIJ11*RGXLM1+CIJ12*RGYLM1                                   BASIS2.......11500
      RGYGM1=CIJ21*RGXLM1+CIJ22*RGYLM1                                   BASIS2.......11600
C                                                                        BASIS2.......11700
C.....CALCULATE PARAMETER VALUES AT THIS LOCATION, (XLOC,YLOC)           BASIS2.......11800
      PITERG=0.D0                                                        BASIS2.......11900
      UITERG=0.D0                                                        BASIS2.......12000
      DPDXG=0.D0                                                         BASIS2.......12100
      DPDYG=0.D0                                                         BASIS2.......12200
      PORG=0.D0                                                          BASIS2.......12300
      THICKG=0.0D0                                                       BASIS2.......12400
      DO 1000 IL=1,4                                                     BASIS2.......12500
      II=(L-1)*4 +IL                                                     BASIS2.......12600
      I=IN(II)                                                           BASIS2.......12700
      DPDXG=DPDXG+PVEL(I)*DFDXG(IL)                                      BASIS2.......12800
      DPDYG=DPDYG+PVEL(I)*DFDYG(IL)                                      BASIS2.......12900
      PORG=PORG+POR(I)*F(IL)                                             BASIS2.......13000
      THICKG=THICKG+THICK(I)*F(IL)                                       BASIS2.......13100
      PITERG=PITERG+PITER(I)*F(IL)                                       BASIS2.......13200
      UITERG=UITERG+UITER(I)*F(IL)                                       BASIS2.......13300
 1000 CONTINUE                                                           BASIS2.......13400
C                                                                        BASIS2.......13500
C.....SET VALUES FOR DENSITY AND VISCOSITY.                              BASIS2.......13600
C.....RHOG = FUNCTION(UITER)                                             BASIS2.......13700
      RHOG=RHOW0+DRWDU*(UITERG-URHOW0)                                   BASIS2.......13800
C.....VISCG = FUNCTION(UITER); VISCOSITY IN UNITS OF VISC0*(KG/(M*SEC))  BASIS2.......13900
      IF(ME) 1300,1300,1200                                              BASIS2.......14000
 1200 VISCG=VISC0*239.4D-7*(10.D0**(248.37D0/(UITERG+133.15D0)))         BASIS2.......14100
      GOTO 1400                                                          BASIS2.......14200
C.....FOR SOLUTE TRANSPORT, VISCG IS TAKEN TO BE CONSTANT                BASIS2.......14300
 1300 VISCG=VISC0                                                        BASIS2.......14400
 1400 CONTINUE                                                           BASIS2.......14500
C                                                                        BASIS2.......14600
C.....SET UNSATURATED FLOW PARAMETERS SWG AND RELKG                      BASIS2.......14700
      IF(IUNSAT-2) 1600,1500,1600                                        BASIS2.......14800
 1500 IF(PITERG) 1550,1600,1600                                          BASIS2.......14900
 1550 CALL UNSAT(SWG,DSWDPG,RELKG,PITERG,LREG(L))                        BASIS2.......15000
      GOTO 1700                                                          BASIS2.......15100
 1600 SWG=1.0D0                                                          BASIS2.......15200
      RELKG=1.0D0                                                        BASIS2.......15300
 1700 CONTINUE                                                           BASIS2.......15400
C                                                                        BASIS2.......15500
C.....CALCULATE CONSISTENT FLUID VELOCITIES WITH RESPECT TO GLOBAL       BASIS2.......15600
C        COORDINATES, VXG, VYG, AND VGMAG, AT THIS LOCATION, (XLOC,YLOC) BASIS2.......15700
      DENOM=1.D0/(PORG*SWG*VISCG)                                        BASIS2.......15800
      PGX=DPDXG-RGXGM1                                                   BASIS2.......15900
      PGY=DPDYG-RGYGM1                                                   BASIS2.......16000
C.....ZERO OUT RANDOM BOUYANT DRIVING FORCES DUE TO DIFFERENCING         BASIS2.......16100
C        NUMBERS PAST PRECISION LIMIT.  MINIMUM DRIVING FORCE IS         BASIS2.......16200
C        1.D-10 OF PRESSURE GRADIENT.  (THIS VALUE MAY BE CHANGED        BASIS2.......16300
C        DEPENDING ON MACHINE PRECISION.)                                BASIS2.......16400
      IF(DPDXG) 1720,1730,1720                                           BASIS2.......16500
 1720 IF(DABS(PGX/DPDXG)-1.0D-10) 1725,1725,1730                         BASIS2.......16600
 1725 PGX=0.0D0                                                          BASIS2.......16700
 1730 IF(DPDYG) 1750,1760,1750                                           BASIS2.......16800
 1750 IF(DABS(PGY/DPDYG)-1.0D-10) 1755,1755,1760                         BASIS2.......16900
 1755 PGY=0.0D0                                                          BASIS2.......17000
 1760 VXG=-DENOM*(PERMXX(L)*PGX+PERMXY(L)*PGY)*RELKG                     BASIS2.......17100
      VYG=-DENOM*(PERMYX(L)*PGX+PERMYY(L)*PGY)*RELKG                     BASIS2.......17200
      VXG2=VXG*VXG                                                       BASIS2.......17300
      VYG2=VYG*VYG                                                       BASIS2.......17400
      VGMAG=DSQRT(VXG2+VYG2)                                             BASIS2.......17500
C                                                                        BASIS2.......17600
C.....AT THIS POINT IN LOCAL COORDINATES, (XLOC,YLOC),                   BASIS2.......17700
C        CALCULATE ASYMMETRIC WEIGHTING FUNCTIONS, W(I),                 BASIS2.......17800
C        AND SPACE DERIVATIVES, DWDXG(I) AND DWDYG(I).                   BASIS2.......17900
C                                                                        BASIS2.......18000
C.....ASYMMETRIC FUNCTIONS SIMPLIFY WHEN  UP=0.0                         BASIS2.......18100
      IF(UP.GT.1.0D-6.AND.NOUMAT.EQ.0) GOTO 1790                         BASIS2.......18200
      DO 1780 I=1,4                                                      BASIS2.......18300
      W(I)=F(I)                                                          BASIS2.......18400
      DWDXG(I)=DFDXG(I)                                                  BASIS2.......18500
      DWDYG(I)=DFDYG(I)                                                  BASIS2.......18600
 1780 CONTINUE                                                           BASIS2.......18700
C.....RETURN WHEN ONLY SYMMETRIC WEIGHTING FUNCTIONS ARE USED            BASIS2.......18800
      RETURN                                                             BASIS2.......18900
C                                                                        BASIS2.......19000
C.....CALCULATE FLUID VELOCITIES WITH RESPECT TO LOCAL COORDINATES,      BASIS2.......19100
C        VXL, VYL, AND VLMAG, AT THIS LOCATION, (XLOC,YLOC).             BASIS2.......19200
 1790 VXL=CIJ11*VXG+CIJ21*VYG                                            BASIS2.......19300
      VYL=CIJ12*VXG+CIJ22*VYG                                            BASIS2.......19400
      VLMAG=DSQRT(VXL*VXL+VYL*VYL)                                       BASIS2.......19500
C                                                                        BASIS2.......19600
      AA=0.0D0                                                           BASIS2.......19700
      BB=0.0D0                                                           BASIS2.......19800
      IF(VLMAG) 1900,1900,1800                                           BASIS2.......19900
 1800 AA=UP*VXL/VLMAG                                                    BASIS2.......20000
      BB=UP*VYL/VLMAG                                                    BASIS2.......20100
C                                                                        BASIS2.......20200
 1900 XIXI=.750D0*AA*XF1*XF2                                             BASIS2.......20300
      YIYI=.750D0*BB*YF1*YF2                                             BASIS2.......20400
      DO 2000 I=1,4                                                      BASIS2.......20500
      AFX(I)=.50D0*FX(I)+XIIX(I)*XIXI                                    BASIS2.......20600
 2000 AFY(I)=.50D0*FY(I)+YIIY(I)*YIYI                                    BASIS2.......20700
C                                                                        BASIS2.......20800
C.....CALCULATE ASYMMETRIC WEIGHTING FUNCTION, W.                        BASIS2.......20900
      DO 3000 I=1,4                                                      BASIS2.......21000
 3000 W(I)=AFX(I)*AFY(I)                                                 BASIS2.......21100
C                                                                        BASIS2.......21200
      THAAX=0.50D0-1.50D0*AA*XLOC                                        BASIS2.......21300
      THBBY=0.50D0-1.50D0*BB*YLOC                                        BASIS2.......21400
      DO 4000 I=1,4                                                      BASIS2.......21500
      XDW(I)=XIIX(I)*THAAX                                               BASIS2.......21600
 4000 YDW(I)=YIIY(I)*THBBY                                               BASIS2.......21700
C                                                                        BASIS2.......21800
C.....CALCULATE DERIVATIVES WITH RESPECT TO LOCAL COORDINATES.           BASIS2.......21900
      DO 5000 I=1,4                                                      BASIS2.......22000
      DWDXL(I)=XDW(I)*AFY(I)                                             BASIS2.......22100
 5000 DWDYL(I)=YDW(I)*AFX(I)                                             BASIS2.......22200
C                                                                        BASIS2.......22300
C.....CALCULATE DERIVATIVES WITH RESPECT TO GLOBAL COORDINATES.          BASIS2.......22400
      DO 6000 I=1,4                                                      BASIS2.......22500
      DWDXG(I)=CIJ11*DWDXL(I)+CIJ12*DWDYL(I)                             BASIS2.......22600
 6000 DWDYG(I)=CIJ21*DWDXL(I)+CIJ22*DWDYL(I)                             BASIS2.......22700
C                                                                        BASIS2.......22800
C                                                                        BASIS2.......22900
      RETURN                                                             BASIS2.......23000
      END                                                                BASIS2.......23100
C                                                                        BASIS2.......23200
C     SUBROUTINE        B  A  S  I  S  3           SUTRA VERSION 2D3D.1  BASIS3.........100
C                                                                        BASIS3.........200
C *** PURPOSE :                                                          BASIS3.........300
C ***  TO CALCULATE VALUES OF BASIS AND WEIGHTING FUNCTIONS AND THEIR    BASIS3.........400
C ***  DERIVATIVES, TRANSFORMATION MATRICES BETWEEN LOCAL AND GLOBAL     BASIS3.........500
C ***  COORDINATES AND PARAMETER VALUES AT A SPECIFIED POINT IN A        BASIS3.........600
C ***  QUADRILATERAL FINITE ELEMENT.  THIS SUBROUTINE HANDLES 3D         BASIS3.........700
C ***  CALCULATIONS ONLY; 2D CALCULATIONS ARE PERFORMED IN SUBROUTINE    BASIS3.........800
C ***  BASIS2.                                                           BASIS3.........900
C                                                                        BASIS3........1000
      SUBROUTINE BASIS3(ICALL,L,XLOC,YLOC,ZLOC,IN,X,Y,Z,F,W,DET,         BASIS3........1100
     1   DFDXG,DFDYG,DFDZG,DWDXG,DWDYG,DWDZG,PITER,UITER,PVEL,POR,       BASIS3........1200
     2   VXG,VYG,VZG,SWG,RHOG,VISCG,PORG,VGMAG,RELKG,                    BASIS3........1300
     3   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, BASIS3........1400
     4   CJ11,CJ12,CJ13,CJ21,CJ22,CJ23,CJ31,CJ32,CJ33,                   BASIS3........1500
     4   GXSI,GETA,GZET,RCIT,RCITM1,RGXG,RGYG,RGZG,LREG)                 BASIS3........1600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BASIS3........1700
      DOUBLE PRECISION XLOC,YLOC,ZLOC                                    BASIS3........1800
      DIMENSION IN(NIN),X(NN),Y(NN),Z(NN),UITER(NN),PITER(NN),PVEL(NN),  BASIS3........1900
     1   POR(NN),PERMXX(NE),PERMXY(NE),PERMXZ(NE),PERMYX(NE),            BASIS3........2000
     2   PERMYY(NE),PERMYZ(NE),PERMZX(NE),PERMZY(NE),PERMZZ(NE)          BASIS3........2100
      DIMENSION GXSI(NE,8),GETA(NE,8),GZET(NE,8)                         BASIS3........2200
      DIMENSION RCIT(NN),RCITM1(NN),LREG(NE)                             BASIS3........2300
      DIMENSION F(8),DFDXG(8),DFDYG(8),DFDZG(8)                          BASIS3........2400
      DIMENSION W(8),DWDXG(8),DWDYG(8),DWDZG(8)                          BASIS3........2500
      DIMENSION FX(8),FY(8),FZ(8),AFX(8),AFY(8),AFZ(8),                  BASIS3........2600
     1   DFDXL(8),DFDYL(8),DFDZL(8),DWDXL(8),DWDYL(8),DWDZL(8),          BASIS3........2700
     2   XDW(8),YDW(8),ZDW(8),XIIX(8),YIIY(8),ZIIZ(8)                    BASIS3........2800
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  BASIS3........2900
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             BASIS3........3000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BASIS3........3100
     1   NSOP,NSOU,NBCN                                                  BASIS3........3200
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      BASIS3........3300
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        BASIS3........3400
      DATA XIIX/-1.D0,+1.D0,+1.D0,-1.D0,-1.D0,+1.D0,+1.D0,-1.D0/         BASIS3........3500
      DATA YIIY/-1.D0,-1.D0,+1.D0,+1.D0,-1.D0,-1.D0,+1.D0,+1.D0/         BASIS3........3600
      DATA ZIIZ/-1.D0,-1.D0,-1.D0,-1.D0,+1.D0,+1.D0,+1.D0,+1.D0/         BASIS3........3700
      SAVE XIIX,YIIY,ZIIZ                                                BASIS3........3800
C                                                                        BASIS3........3900
C                                                                        BASIS3........4000
C.....AT THIS LOCATION IN LOCAL COORDINATES, (XLOC,YLOC,ZLOC),           BASIS3........4100
C        CALCULATE SYMMETRIC WEIGHTING FUNCTIONS, F(I),                  BASIS3........4200
C        SPACE DERIVATIVES, DFDXG(I), DFDYX(I), AND DFDZG(I),            BASIS3........4300
C        AND DETERMINANT OF JACOBIAN, DET.                               BASIS3........4400
C                                                                        BASIS3........4500
      XF1=1.D0-XLOC                                                      BASIS3........4600
      XF2=1.D0+XLOC                                                      BASIS3........4700
      YF1=1.D0-YLOC                                                      BASIS3........4800
      YF2=1.D0+YLOC                                                      BASIS3........4900
      ZF1=1.D0-ZLOC                                                      BASIS3........5000
      ZF2=1.D0+ZLOC                                                      BASIS3........5100
C                                                                        BASIS3........5200
C.....CALCULATE BASIS FUNCTION, F.                                       BASIS3........5300
      FX(1)=XF1                                                          BASIS3........5400
      FX(2)=XF2                                                          BASIS3........5500
      FX(3)=XF2                                                          BASIS3........5600
      FX(4)=XF1                                                          BASIS3........5700
      FX(5)=XF1                                                          BASIS3........5800
      FX(6)=XF2                                                          BASIS3........5900
      FX(7)=XF2                                                          BASIS3........6000
      FX(8)=XF1                                                          BASIS3........6100
      FY(1)=YF1                                                          BASIS3........6200
      FY(2)=YF1                                                          BASIS3........6300
      FY(3)=YF2                                                          BASIS3........6400
      FY(4)=YF2                                                          BASIS3........6500
      FY(5)=YF1                                                          BASIS3........6600
      FY(6)=YF1                                                          BASIS3........6700
      FY(7)=YF2                                                          BASIS3........6800
      FY(8)=YF2                                                          BASIS3........6900
      FZ(1)=ZF1                                                          BASIS3........7000
      FZ(2)=ZF1                                                          BASIS3........7100
      FZ(3)=ZF1                                                          BASIS3........7200
      FZ(4)=ZF1                                                          BASIS3........7300
      FZ(5)=ZF2                                                          BASIS3........7400
      FZ(6)=ZF2                                                          BASIS3........7500
      FZ(7)=ZF2                                                          BASIS3........7600
      FZ(8)=ZF2                                                          BASIS3........7700
      DO 10 I=1,8                                                        BASIS3........7800
   10 F(I)=0.125D0*FX(I)*FY(I)*FZ(I)                                     BASIS3........7900
C                                                                        BASIS3........8000
C.....CALCULATE DERIVATIVES WITH RESPECT TO LOCAL COORDINATES.           BASIS3........8100
      DO 20 I=1,8                                                        BASIS3........8200
      DFDXL(I)=XIIX(I)*0.125D0*FY(I)*FZ(I)                               BASIS3........8300
      DFDYL(I)=YIIY(I)*0.125D0*FX(I)*FZ(I)                               BASIS3........8400
   20 DFDZL(I)=ZIIZ(I)*0.125D0*FX(I)*FY(I)                               BASIS3........8500
C                                                                        BASIS3........8600
C.....CALCULATE ELEMENTS OF JACOBIAN MATRIX, CJ.                         BASIS3........8700
      CJ11=0.D0                                                          BASIS3........8800
      CJ12=0.D0                                                          BASIS3........8900
      CJ13=0.D0                                                          BASIS3........9000
      CJ21=0.D0                                                          BASIS3........9100
      CJ22=0.D0                                                          BASIS3........9200
      CJ23=0.D0                                                          BASIS3........9300
      CJ31=0.D0                                                          BASIS3........9400
      CJ32=0.D0                                                          BASIS3........9500
      CJ33=0.D0                                                          BASIS3........9600
      DO 100 IL=1,8                                                      BASIS3........9700
      II=(L-1)*8+IL                                                      BASIS3........9800
      I=IN(II)                                                           BASIS3........9900
      CJ11=CJ11+DFDXL(IL)*X(I)                                           BASIS3.......10000
      CJ12=CJ12+DFDXL(IL)*Y(I)                                           BASIS3.......10100
      CJ13=CJ13+DFDXL(IL)*Z(I)                                           BASIS3.......10200
      CJ21=CJ21+DFDYL(IL)*X(I)                                           BASIS3.......10300
      CJ22=CJ22+DFDYL(IL)*Y(I)                                           BASIS3.......10400
      CJ23=CJ23+DFDYL(IL)*Z(I)                                           BASIS3.......10500
      CJ31=CJ31+DFDZL(IL)*X(I)                                           BASIS3.......10600
      CJ32=CJ32+DFDZL(IL)*Y(I)                                           BASIS3.......10700
  100 CJ33=CJ33+DFDZL(IL)*Z(I)                                           BASIS3.......10800
C                                                                        BASIS3.......10900
C.....CALCULATE DETERMINANT OF JACOBIAN MATRIX.                          BASIS3.......11000
      DET=CJ11*(CJ22*CJ33-CJ32*CJ23)                                     BASIS3.......11100
     1   -CJ21*(CJ12*CJ33-CJ32*CJ13)                                     BASIS3.......11200
     2   +CJ31*(CJ12*CJ23-CJ22*CJ13)                                     BASIS3.......11300
C                                                                        BASIS3.......11400
C.....RETURN TO ELEMEN3 WITH JACOBIAN MATRIX ON FIRST TIME STEP.         BASIS3.......11500
      IF(ICALL.EQ.0) RETURN                                              BASIS3.......11600
C                                                                        BASIS3.......11700
C                                                                        BASIS3.......11800
C.....CALCULATE ELEMENTS OF INVERSE JACOBIAN MATRIX, CIJ.                BASIS3.......11900
      ODET=1.D0/DET                                                      BASIS3.......12000
      CIJ11=+ODET*(CJ22*CJ33-CJ32*CJ23)                                  BASIS3.......12100
      CIJ12=-ODET*(CJ12*CJ33-CJ32*CJ13)                                  BASIS3.......12200
      CIJ13=+ODET*(CJ12*CJ23-CJ22*CJ13)                                  BASIS3.......12300
      CIJ21=-ODET*(CJ21*CJ33-CJ31*CJ23)                                  BASIS3.......12400
      CIJ22=+ODET*(CJ11*CJ33-CJ31*CJ13)                                  BASIS3.......12500
      CIJ23=-ODET*(CJ11*CJ23-CJ21*CJ13)                                  BASIS3.......12600
      CIJ31=+ODET*(CJ21*CJ32-CJ31*CJ22)                                  BASIS3.......12700
      CIJ32=-ODET*(CJ11*CJ32-CJ31*CJ12)                                  BASIS3.......12800
      CIJ33=+ODET*(CJ11*CJ22-CJ21*CJ12)                                  BASIS3.......12900
C                                                                        BASIS3.......13000
C.....CALCULATE DERIVATIVES WITH RESPECT TO GLOBAL COORDINATES           BASIS3.......13100
      DO 200 I=1,8                                                       BASIS3.......13200
      DFDXG(I)=CIJ11*DFDXL(I)+CIJ12*DFDYL(I)+CIJ13*DFDZL(I)              BASIS3.......13300
      DFDYG(I)=CIJ21*DFDXL(I)+CIJ22*DFDYL(I)+CIJ23*DFDZL(I)              BASIS3.......13400
  200 DFDZG(I)=CIJ31*DFDXL(I)+CIJ32*DFDYL(I)+CIJ33*DFDZL(I)              BASIS3.......13500
C                                                                        BASIS3.......13600
C.....CALCULATE CONSISTENT COMPONENTS OF (RHO*GRAV) TERM IN LOCAL        BASIS3.......13700
C        COORDINATES AT THIS LOCATION, (XLOC,YLOC,ZLOC)                  BASIS3.......13800
      RGXL=0.D0                                                          BASIS3.......13900
      RGYL=0.D0                                                          BASIS3.......14000
      RGZL=0.D0                                                          BASIS3.......14100
      RGXLM1=0.D0                                                        BASIS3.......14200
      RGYLM1=0.D0                                                        BASIS3.......14300
      RGZLM1=0.D0                                                        BASIS3.......14400
      DO 800 IL=1,8                                                      BASIS3.......14500
      II=(L-1)*8+IL                                                      BASIS3.......14600
      I=IN(II)                                                           BASIS3.......14700
      ADFDXL=DABS(DFDXL(IL))                                             BASIS3.......14800
      ADFDYL=DABS(DFDYL(IL))                                             BASIS3.......14900
      ADFDZL=DABS(DFDZL(IL))                                             BASIS3.......15000
      RGXL=RGXL+RCIT(I)*GXSI(L,IL)*ADFDXL                                BASIS3.......15100
      RGYL=RGYL+RCIT(I)*GETA(L,IL)*ADFDYL                                BASIS3.......15200
      RGZL=RGZL+RCIT(I)*GZET(L,IL)*ADFDZL                                BASIS3.......15300
      RGXLM1=RGXLM1+RCITM1(I)*GXSI(L,IL)*ADFDXL                          BASIS3.......15400
      RGYLM1=RGYLM1+RCITM1(I)*GETA(L,IL)*ADFDYL                          BASIS3.......15500
      RGZLM1=RGZLM1+RCITM1(I)*GZET(L,IL)*ADFDZL                          BASIS3.......15600
  800 CONTINUE                                                           BASIS3.......15700
C                                                                        BASIS3.......15800
C.....TRANSFORM CONSISTENT COMPONENTS OF (RHO*GRAV) TERM TO              BASIS3.......15900
C        GLOBAL COORDINATES                                              BASIS3.......16000
      RGXG=CIJ11*RGXL+CIJ12*RGYL+CIJ13*RGZL                              BASIS3.......16100
      RGYG=CIJ21*RGXL+CIJ22*RGYL+CIJ23*RGZL                              BASIS3.......16200
      RGZG=CIJ31*RGXL+CIJ32*RGYL+CIJ33*RGZL                              BASIS3.......16300
      RGXGM1=CIJ11*RGXLM1+CIJ12*RGYLM1+CIJ13*RGZLM1                      BASIS3.......16400
      RGYGM1=CIJ21*RGXLM1+CIJ22*RGYLM1+CIJ23*RGZLM1                      BASIS3.......16500
      RGZGM1=CIJ31*RGXLM1+CIJ32*RGYLM1+CIJ33*RGZLM1                      BASIS3.......16600
C                                                                        BASIS3.......16700
C.....CALCULATE PARAMETER VALUES AT THIS LOCATION, (XLOC,YLOC,ZLOC)      BASIS3.......16800
      PITERG=0.D0                                                        BASIS3.......16900
      UITERG=0.D0                                                        BASIS3.......17000
      DPDXG=0.D0                                                         BASIS3.......17100
      DPDYG=0.D0                                                         BASIS3.......17200
      DPDZG=0.D0                                                         BASIS3.......17300
      PORG=0.D0                                                          BASIS3.......17400
      DO 1000 IL=1,8                                                     BASIS3.......17500
      II=(L-1)*8 +IL                                                     BASIS3.......17600
      I=IN(II)                                                           BASIS3.......17700
      DPDXG=DPDXG+PVEL(I)*DFDXG(IL)                                      BASIS3.......17800
      DPDYG=DPDYG+PVEL(I)*DFDYG(IL)                                      BASIS3.......17900
      DPDZG=DPDZG+PVEL(I)*DFDZG(IL)                                      BASIS3.......18000
      PORG=PORG+POR(I)*F(IL)                                             BASIS3.......18100
      PITERG=PITERG+PITER(I)*F(IL)                                       BASIS3.......18200
      UITERG=UITERG+UITER(I)*F(IL)                                       BASIS3.......18300
 1000 CONTINUE                                                           BASIS3.......18400
C                                                                        BASIS3.......18500
C.....SET VALUES FOR DENSITY AND VISCOSITY.                              BASIS3.......18600
C.....RHOG = FUNCTION(UITER)                                             BASIS3.......18700
      RHOG=RHOW0+DRWDU*(UITERG-URHOW0)                                   BASIS3.......18800
C.....VISCG = FUNCTION(UITER); VISCOSITY IN UNITS OF VISC0*(KG/(M*SEC))  BASIS3.......18900
      IF(ME) 1300,1300,1200                                              BASIS3.......19000
 1200 VISCG=VISC0*239.4D-7*(10.D0**(248.37D0/(UITERG+133.15D0)))         BASIS3.......19100
      GOTO 1400                                                          BASIS3.......19200
C.....FOR SOLUTE TRANSPORT, VISCG IS TAKEN TO BE CONSTANT                BASIS3.......19300
 1300 VISCG=VISC0                                                        BASIS3.......19400
 1400 CONTINUE                                                           BASIS3.......19500
C                                                                        BASIS3.......19600
C.....SET UNSATURATED FLOW PARAMETERS SWG AND RELKG                      BASIS3.......19700
      IF(IUNSAT-2) 1600,1500,1600                                        BASIS3.......19800
 1500 IF(PITERG) 1550,1600,1600                                          BASIS3.......19900
 1550 CALL UNSAT(SWG,DSWDPG,RELKG,PITERG,LREG(L))                        BASIS3.......20000
      GOTO 1700                                                          BASIS3.......20100
 1600 SWG=1.0D0                                                          BASIS3.......20200
      RELKG=1.0D0                                                        BASIS3.......20300
 1700 CONTINUE                                                           BASIS3.......20400
C                                                                        BASIS3.......20500
C.....CALCULATE CONSISTENT FLUID VELOCITIES WITH RESPECT TO GLOBAL       BASIS3.......20600
C        COORDINATES, VXG, VYG, VZG, AND VGMAG, AT THIS LOCATION,        BASIS3.......20700
C        (XLOC,YLOC,ZLOC)                                                BASIS3.......20800
      DENOM=1.D0/(PORG*SWG*VISCG)                                        BASIS3.......20900
      PGX=DPDXG-RGXGM1                                                   BASIS3.......21000
      PGY=DPDYG-RGYGM1                                                   BASIS3.......21100
      PGZ=DPDZG-RGZGM1                                                   BASIS3.......21200
C.....ZERO OUT RANDOM BOUYANT DRIVING FORCES DUE TO DIFFERENCING         BASIS3.......21300
C        NUMBERS PAST PRECISION LIMIT.  MINIMUM DRIVING FORCE IS         BASIS3.......21400
C        1.D-10 OF PRESSURE GRADIENT.  (THIS VALUE MAY BE CHANGED        BASIS3.......21500
C        DEPENDING ON MACHINE PRECISION.)                                BASIS3.......21600
      IF(DPDXG) 1720,1727,1720                                           BASIS3.......21700
 1720 IF(DABS(PGX/DPDXG)-1.0D-10) 1725,1725,1727                         BASIS3.......21800
 1725 PGX=0.0D0                                                          BASIS3.......21900
 1727 IF(DPDYG) 1730,1737,1730                                           BASIS3.......22000
 1730 IF(DABS(PGY/DPDYG)-1.0D-10) 1735,1735,1737                         BASIS3.......22100
 1735 PGY=0.0D0                                                          BASIS3.......22200
 1737 IF(DPDZG) 1740,1760,1740                                           BASIS3.......22300
 1740 IF(DABS(PGZ/DPDZG)-1.0D-10) 1745,1745,1760                         BASIS3.......22400
 1745 PGZ=0.0D0                                                          BASIS3.......22500
 1760 VXG=-DENOM*(PERMXX(L)*PGX+PERMXY(L)*PGY+PERMXZ(L)*PGZ)*RELKG       BASIS3.......22600
      VYG=-DENOM*(PERMYX(L)*PGX+PERMYY(L)*PGY+PERMYZ(L)*PGZ)*RELKG       BASIS3.......22700
      VZG=-DENOM*(PERMZX(L)*PGX+PERMZY(L)*PGY+PERMZZ(L)*PGZ)*RELKG       BASIS3.......22800
      VXG2=VXG*VXG                                                       BASIS3.......22900
      VYG2=VYG*VYG                                                       BASIS3.......23000
      VZG2=VZG*VZG                                                       BASIS3.......23100
      VGMAG=DSQRT(VXG2+VYG2+VZG2)                                        BASIS3.......23200
C                                                                        BASIS3.......23300
C.....AT THIS POINT IN LOCAL COORDINATES, (XLOC,YLOC,ZLOC),              BASIS3.......23400
C        CALCULATE ASYMMETRIC WEIGHTING FUNCTIONS, W(I),                 BASIS3.......23500
C        AND SPACE DERIVATIVES, DWDXG(I), DWDYG(I), AND DWDZG(I).        BASIS3.......23600
C                                                                        BASIS3.......23700
C.....ASYMMETRIC FUNCTIONS SIMPLIFY WHEN  UP=0.0                         BASIS3.......23800
      IF(UP.GT.1.0D-6.AND.NOUMAT.EQ.0) GOTO 1790                         BASIS3.......23900
      DO 1780 I=1,8                                                      BASIS3.......24000
      W(I)=F(I)                                                          BASIS3.......24100
      DWDXG(I)=DFDXG(I)                                                  BASIS3.......24200
      DWDYG(I)=DFDYG(I)                                                  BASIS3.......24300
      DWDZG(I)=DFDZG(I)                                                  BASIS3.......24400
 1780 CONTINUE                                                           BASIS3.......24500
C.....RETURN WHEN ONLY SYMMETRIC WEIGHTING FUNCTIONS ARE USED            BASIS3.......24600
      RETURN                                                             BASIS3.......24700
C                                                                        BASIS3.......24800
C.....CALCULATE FLUID VELOCITIES WITH RESPECT TO LOCAL COORDINATES,      BASIS3.......24900
C        VXL, VYL, VZL, AND VLMAG, AT THIS LOCATION, (XLOC,YLOC,ZLOC).   BASIS3.......25000
 1790 VXL=CIJ11*VXG+CIJ21*VYG+CIJ31*VZG                                  BASIS3.......25100
      VYL=CIJ12*VXG+CIJ22*VYG+CIJ32*VZG                                  BASIS3.......25200
      VZL=CIJ13*VXG+CIJ23*VYG+CIJ33*VZG                                  BASIS3.......25300
      VLMAG=DSQRT(VXL*VXL+VYL*VYL+VZL*VZL)                               BASIS3.......25400
C                                                                        BASIS3.......25500
      AA=0.0D0                                                           BASIS3.......25600
      BB=0.0D0                                                           BASIS3.......25700
      GG=0.0D0                                                           BASIS3.......25800
      IF(VLMAG) 1900,1900,1800                                           BASIS3.......25900
 1800 AA=UP*VXL/VLMAG                                                    BASIS3.......26000
      BB=UP*VYL/VLMAG                                                    BASIS3.......26100
      GG=UP*VZL/VLMAG                                                    BASIS3.......26200
C                                                                        BASIS3.......26300
 1900 XIXI=.750D0*AA*XF1*XF2                                             BASIS3.......26400
      YIYI=.750D0*BB*YF1*YF2                                             BASIS3.......26500
      ZIZI=.750D0*GG*ZF1*ZF2                                             BASIS3.......26600
      DO 2000 I=1,8                                                      BASIS3.......26700
      AFX(I)=.50D0*FX(I)+XIIX(I)*XIXI                                    BASIS3.......26800
      AFY(I)=.50D0*FY(I)+YIIY(I)*YIYI                                    BASIS3.......26900
 2000 AFZ(I)=.50D0*FZ(I)+ZIIZ(I)*ZIZI                                    BASIS3.......27000
C                                                                        BASIS3.......27100
C.....CALCULATE ASYMMETRIC WEIGHTING FUNCTION, W.                        BASIS3.......27200
      DO 3000 I=1,8                                                      BASIS3.......27300
 3000 W(I)=AFX(I)*AFY(I)*AFZ(I)                                          BASIS3.......27400
C                                                                        BASIS3.......27500
      THAAX=0.50D0-1.50D0*AA*XLOC                                        BASIS3.......27600
      THBBY=0.50D0-1.50D0*BB*YLOC                                        BASIS3.......27700
      THGGZ=0.50D0-1.50D0*GG*ZLOC                                        BASIS3.......27800
      DO 4000 I=1,8                                                      BASIS3.......27900
      XDW(I)=XIIX(I)*THAAX                                               BASIS3.......28000
      YDW(I)=YIIY(I)*THBBY                                               BASIS3.......28100
 4000 ZDW(I)=ZIIZ(I)*THGGZ                                               BASIS3.......28200
C                                                                        BASIS3.......28300
C.....CALCULATE DERIVATIVES WITH RESPECT TO LOCAL COORDINATES.           BASIS3.......28400
      DO 5000 I=1,8                                                      BASIS3.......28500
      DWDXL(I)=XDW(I)*AFY(I)*AFZ(I)                                      BASIS3.......28600
      DWDYL(I)=YDW(I)*AFX(I)*AFZ(I)                                      BASIS3.......28700
 5000 DWDZL(I)=ZDW(I)*AFX(I)*AFY(I)                                      BASIS3.......28800
C                                                                        BASIS3.......28900
C.....CALCULATE DERIVATIVES WITH RESPECT TO GLOBAL COORDINATES.          BASIS3.......29000
      DO 6000 I=1,8                                                      BASIS3.......29100
      DWDXG(I)=CIJ11*DWDXL(I)+CIJ12*DWDYL(I)+CIJ13*DWDZL(I)              BASIS3.......29200
      DWDYG(I)=CIJ21*DWDXL(I)+CIJ22*DWDYL(I)+CIJ23*DWDZL(I)              BASIS3.......29300
 6000 DWDZG(I)=CIJ31*DWDXL(I)+CIJ32*DWDYL(I)+CIJ33*DWDZL(I)              BASIS3.......29400
C                                                                        BASIS3.......29500
C                                                                        BASIS3.......29600
      RETURN                                                             BASIS3.......29700
      END                                                                BASIS3.......29800
C                                                                        BASIS3.......29900
C     SUBROUTINE        B  C                       SUTRA VERSION 2D3D.1  BC.............100
C                                                                        BC.............200
C *** PURPOSE :                                                          BC.............300
C ***  TO IMPLEMENT SPECIFIED PRESSURE AND SPECIFIED TEMPERATURE OR      BC.............400
C ***  CONCENTRATION CONDITIONS BY MODIFYING THE GLOBAL FLOW AND         BC.............500
C ***  TRANSPORT MATRIX EQUATIONS.                                       BC.............600
C                                                                        BC.............700
      SUBROUTINE BC(ML,PMAT,PVEC,UMAT,UVEC,IPBC,PBC,IUBC,UBC,QPLITR,     BC.............800
     1   MIOFF)                                                          BC.............900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BC............1000
      DIMENSION PMAT(NELT,NCBI),PVEC(NNVEC),UMAT(NELT,NCBI),UVEC(NNVEC), BC............1100
     1   IPBC(NBCN),PBC(NBCN),IUBC(NBCN),UBC(NBCN),QPLITR(NBCN)          BC............1200
      DIMENSION MIOFF(27)                                                BC............1300
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  BC............1400
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             BC............1500
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BC............1600
     1   NSOP,NSOU,NBCN                                                  BC............1700
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   BC............1800
      COMMON /DIMX2/ NELTA,NNVEC,NDIMJA                                  BC............1900
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      BC............2000
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        BC............2100
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           BC............2200
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       BC............2300
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  BC............2400
C                                                                        BC............2500
C                                                                        BC............2600
C.....SET UP MATRIX STRUCTURE INFORMATION                                BC............2700
      IF (KSOLVP.EQ.0) THEN                                              BC............2800
         IMID0 = 0                                                       BC............2900
         JMID = NBHALF                                                   BC............3000
      ELSE                                                               BC............3100
         IF (IABS(KTYPE).EQ.3) THEN                                      BC............3200
            IMID0 = MIOFF(14)                                            BC............3300
         ELSE                                                            BC............3400
            IMID0 = MIOFF(5)                                             BC............3500
         END IF                                                          BC............3600
         JMID = 1                                                        BC............3700
      END IF                                                             BC............3800
C                                                                        BC............3900
      IF(NPBC.EQ.0) GOTO 1050                                            BC............4000
C.....SPECIFIED P BOUNDARY CONDITIONS                                    BC............4100
      DO 1000 IP=1,NPBC                                                  BC............4200
      I=IABS(IPBC(IP))                                                   BC............4300
      IMID = IMID0 + I                                                   BC............4400
C                                                                        BC............4500
      IF(ML-1) 100,100,200                                               BC............4600
C.....MODIFY EQUATION FOR P BY ADDING FLUID SOURCE AT SPECIFIED          BC............4700
C        PRESSURE NODE                                                   BC............4800
  100 GPINL=-GNUP                                                        BC............4900
      GPINR=GNUP*PBC(IP)                                                 BC............5000
      PMAT(IMID,JMID)=PMAT(IMID,JMID)-GPINL                              BC............5100
      PVEC(I)=PVEC(I)+GPINR                                              BC............5200
C                                                                        BC............5300
      IF(ML-1) 200,1000,200                                              BC............5400
C.....MODIFY EQUATION FOR U BY ADDING U SOURCE WHEN FLUID FLOWS IN       BC............5500
C        AT SPECIFIED PRESSURE NODE                                      BC............5600
  200 GUR=0.0D0                                                          BC............5700
      GUL=0.0D0                                                          BC............5800
      IF(QPLITR(IP)) 360,360,340                                         BC............5900
  340 GUL=-CW*QPLITR(IP)                                                 BC............6000
      GUR=-GUL*UBC(IP)                                                   BC............6100
  360 IF(NOUMAT) 370,370,380                                             BC............6200
  370 UMAT(IMID,JMID)=UMAT(IMID,JMID)-GUL                                BC............6300
  380 UVEC(I)=UVEC(I)+GUR                                                BC............6400
 1000 CONTINUE                                                           BC............6500
C                                                                        BC............6600
C                                                                        BC............6700
 1050 IF(ML-1) 1100,3000,1100                                            BC............6800
 1100 IF(NUBC.EQ.0) GOTO 3000                                            BC............6900
C.....SPECIFIED U BOUNDARY CONDITIONS.                                   BC............7000
C        MODIFY EQUATION FOR U BY ADDING ENERGY/SOLUTE MASS SOURCE       BC............7100
C        AT SPECIFIED U NODE                                             BC............7200
      DO 2500 IU=1,NUBC                                                  BC............7300
      IUP=IU+NPBC                                                        BC............7400
      I=IABS(IUBC(IUP))                                                  BC............7500
      IMID = IMID0 + I                                                   BC............7600
      IF(NOUMAT) 1200,1200,2000                                          BC............7700
 1200 GUINL=-GNUU                                                        BC............7800
      UMAT(IMID,JMID)=UMAT(IMID,JMID)-GUINL                              BC............7900
 2000 GUINR=GNUU*UBC(IUP)                                                BC............8000
 2500 UVEC(I)=UVEC(I)+GUINR                                              BC............8100
C                                                                        BC............8200
 3000 CONTINUE                                                           BC............8300
C                                                                        BC............8400
C                                                                        BC............8500
      RETURN                                                             BC............8600
      END                                                                BC............8700
C                                                                        BC............8800
C     SUBPROGRAM        B  D  I  N  I  T           SUTRA VERSION 2D3D.1  BDINIT.........100
C                                                                        BDINIT.........200
C *** PURPOSE :                                                          BDINIT.........300
C ***  BLOCK-DATA SUBPROGRAM FOR INITIALIZING VARIABLES NAMED IN         BDINIT.........400
C ***  COMMON BLOCKS.                                                    BDINIT.........500
C                                                                        BDINIT.........600
      BLOCK DATA BDINIT                                                  BDINIT.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BDINIT.........800
      CHARACTER*40 SOLNAM(0:10)                                          BDINIT.........900
      CHARACTER*10 SOLWRD(0:10)                                          BDINIT........1000
      COMMON /SOLVC/ SOLWRD, SOLNAM                                      BDINIT........1100
      COMMON /SOLVN/ NSLVRS                                              BDINIT........1200
C.....SET THE NUMBER OF SOLVERS AVAILABLE                                BDINIT........1300
      DATA NSLVRS /4/                                                    BDINIT........1400
C.....DEFINE KEYWORDS AND NAMES FOR SOLVERS                              BDINIT........1500
      DATA (SOLWRD(M),SOLNAM(M),M=0,10) /                                BDINIT........1600
     1   'DIRECT', 'BANDED GAUSSIAN ELIMINATION (DIRECT)',               BDINIT........1700
     2   'CG', 'IC-PRECONDITIONED CONJUGATE GRADIENT',                   BDINIT........1800
     3   'GMRES', 'ILU-PRECONDITIONED GMRES',                            BDINIT........1900
     4   'ORTHOMIN', 'ILU-PRECONDITIONED ORTHOMIN',                      BDINIT........2000
     5   '', '',                                                         BDINIT........2100
     6   '', '',                                                         BDINIT........2200
     7   '', '',                                                         BDINIT........2300
     8   '', '',                                                         BDINIT........2400
     9   '', '',                                                         BDINIT........2500
     T   '', '',                                                         BDINIT........2600
     1   '', ''/                                                         BDINIT........2700
      END                                                                BDINIT........2800
C                                                                        BDINIT........2900
C     SUBROUTINE        B  O  U  N  D              SUTRA VERSION 2D3D.1  BOUND..........100
C                                                                        BOUND..........200
C *** PURPOSE :                                                          BOUND..........300
C ***  TO READ AND ORGANIZE SPECIFIED PRESSURE DATA AND                  BOUND..........400
C ***  SPECIFIED TEMPERATURE OR CONCENTRATION DATA.                      BOUND..........500
C                                                                        BOUND..........600
      SUBROUTINE BOUND(IPBC,PBC,IUBC,UBC,IPBCT,IUBCT)                    BOUND..........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BOUND..........800
      CHARACTER INTFIL*1000                                              BOUND..........900
      CHARACTER*80 ERRCOD,CHERR(10),FNAME(0:7)                           BOUND.........1000
      DIMENSION IPBC(NBCN),PBC(NBCN),IUBC(NBCN),UBC(NBCN)                BOUND.........1100
      DIMENSION INERR(10),RLERR(10)                                      BOUND.........1200
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  BOUND.........1300
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             BOUND.........1400
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BOUND.........1500
     1   NSOP,NSOU,NBCN                                                  BOUND.........1600
      COMMON /ERRHAN/ ISERR                                              BOUND.........1700
      COMMON /FNAMES/ FNAME                                              BOUND.........1800
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        BOUND.........1900
C                                                                        BOUND.........2000
C                                                                        BOUND.........2100
      IPBCT=1                                                            BOUND.........2200
      IUBCT=1                                                            BOUND.........2300
      IP=0                                                               BOUND.........2400
      IPU=0                                                              BOUND.........2500
      WRITE(K3,50)                                                       BOUND.........2600
   50 FORMAT(1H1////11X,'B O U N D A R Y   C O N D I T I O N S')         BOUND.........2700
      IF(NPBC.EQ.0) GOTO 400                                             BOUND.........2800
      WRITE(K3,100)                                                      BOUND.........2900
  100 FORMAT(//11X,'**** NODES AT WHICH PRESSURES ARE',                  BOUND.........3000
     1   ' SPECIFIED ****'/)                                             BOUND.........3100
      IF(ME) 107,107,114                                                 BOUND.........3200
  107 WRITE(K3,108)                                                      BOUND.........3300
  108 FORMAT(11X,'     (AS WELL AS SOLUTE CONCENTRATION OF ANY'          BOUND.........3400
     1   /16X,' FLUID INFLOW WHICH MAY OCCUR AT THE POINT'               BOUND.........3500
     2   /16X,' OF SPECIFIED PRESSURE)'//12X,'NODE',18X,'PRESSURE',      BOUND.........3600
     3   13X,'CONCENTRATION'//)                                          BOUND.........3700
      GOTO 120                                                           BOUND.........3800
  114 WRITE(K3,115)                                                      BOUND.........3900
  115 FORMAT(11X,'     (AS WELL AS TEMPERATURE {DEGREES CELSIUS} OF ANY' BOUND.........4000
     1   /16X,' FLUID INFLOW WHICH MAY OCCUR AT THE POINT'               BOUND.........4100
     2   /16X,' OF SPECIFIED PRESSURE)'//12X,'NODE',18X,                 BOUND.........4200
     2   'PRESSURE',13X,'  TEMPERATURE'//)                               BOUND.........4300
C                                                                        BOUND.........4400
C.....INPUT DATASET 19:  DATA FOR SPECIFIED PRESSURE NODES               BOUND.........4500
  120 ERRCOD = 'REA-INP-S19'                                             BOUND.........4600
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    BOUND.........4700
      IF (ISERR) RETURN                                                  BOUND.........4800
  125 IPU=IPU+1                                                          BOUND.........4900
      ERRCOD = 'REA-INP-19'                                              BOUND.........5000
      CALL READIF(K1, INTFIL, ERRCOD)                                    BOUND.........5100
      IF (ISERR) RETURN                                                  BOUND.........5200
      READ(INTFIL,*,IOSTAT=INERR(1)) IDUM                                BOUND.........5300
      IF (INERR(1).NE.0) THEN                                            BOUND.........5400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        BOUND.........5500
         RETURN                                                          BOUND.........5600
      END IF                                                             BOUND.........5700
      IDUMA = IABS(IDUM)                                                 BOUND.........5800
      IF (IDUM.EQ.0) THEN                                                BOUND.........5900
         GOTO 180                                                        BOUND.........6000
      ELSE IF (IDUMA.GT.NN) THEN                                         BOUND.........6100
         ERRCOD = 'INP-19-1'                                             BOUND.........6200
         INERR(1) = IDUMA                                                BOUND.........6300
         INERR(2) = NN                                                   BOUND.........6400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        BOUND.........6500
         RETURN                                                          BOUND.........6600
      ELSE IF (IPU.GT.NPBC) THEN                                         BOUND.........6700
         GOTO 125                                                        BOUND.........6800
      END IF                                                             BOUND.........6900
      IPBC(IPU) = IDUM                                                   BOUND.........7000
      IF (IPBC(IPU).GT.0) THEN                                           BOUND.........7100
         ERRCOD = 'REA-INP-19'                                           BOUND.........7200
         READ(INTFIL,*,IOSTAT=INERR(1)) IPBC(IPU),PBC(IPU),UBC(IPU)      BOUND.........7300
         IF (INERR(1).NE.0) THEN                                         BOUND.........7400
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     BOUND.........7500
            RETURN                                                       BOUND.........7600
         END IF                                                          BOUND.........7700
         WRITE(K3,160) IPBC(IPU),PBC(IPU),UBC(IPU)                       BOUND.........7800
      ELSE IF (IPBC(IPU).LT.0) THEN                                      BOUND.........7900
         IPBCT = -1                                                      BOUND.........8000
         WRITE(K3,160) IPBC(IPU)                                         BOUND.........8100
      ELSE                                                               BOUND.........8200
         GOTO 180                                                        BOUND.........8300
      END IF                                                             BOUND.........8400
  160 FORMAT(7X,I9,6X,1PD20.13,6X,1PD20.13)                              BOUND.........8500
      GOTO 125                                                           BOUND.........8600
  180 IPU=IPU-1                                                          BOUND.........8700
      IP=IPU                                                             BOUND.........8800
      IF(IP.EQ.NPBC) GOTO 200                                            BOUND.........8900
      ERRCOD = 'INP-3,19-1'                                              BOUND.........9000
      INERR(1) = IP                                                      BOUND.........9100
      INERR(2) = NPBC                                                    BOUND.........9200
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           BOUND.........9300
      RETURN                                                             BOUND.........9400
  200 IF(IPBCT.NE.-1) GOTO 400                                           BOUND.........9500
      IF(ME) 205,205,215                                                 BOUND.........9600
  205 WRITE(K3,206)                                                      BOUND.........9700
  206 FORMAT(//12X,'TIME-DEPENDENT SPECIFIED PRESSURE'/12X,'OR INFLOW ', BOUND.........9800
     1   'CONCENTRATION INDICATED'/12X,'BY NEGATIVE NODE NUMBER')        BOUND.........9900
      GOTO 400                                                           BOUND........10000
  215 WRITE(K3,216)                                                      BOUND........10100
  216 FORMAT(//11X,'TIME-DEPENDENT SPECIFIED PRESSURE'/12X,'OR INFLOW ', BOUND........10200
     1   'TEMPERATURE INDICATED'/12X,'BY NEGATIVE NODE NUMBER')          BOUND........10300
  400 IF(NUBC.EQ.0) GOTO 6000                                            BOUND........10400
C                                                                        BOUND........10500
      IF(ME) 500,500,550                                                 BOUND........10600
  500 WRITE(K3,1000)                                                     BOUND........10700
 1000 FORMAT(////11X,'**** NODES AT WHICH SOLUTE CONCENTRATIONS ARE ',   BOUND........10800
     1   'SPECIFIED TO BE INDEPENDENT OF LOCAL FLOWS AND FLUID SOURCES', BOUND........10900
     2   ' ****'//12X,'NODE',13X,'CONCENTRATION'//)                      BOUND........11000
      GOTO 1120                                                          BOUND........11100
  550 WRITE(K3,1001)                                                     BOUND........11200
 1001 FORMAT(////11X,'**** NODES AT WHICH TEMPERATURES ARE ',            BOUND........11300
     1   'SPECIFIED TO BE INDEPENDENT OF LOCAL FLOWS AND FLUID SOURCES', BOUND........11400
     2   ' ****'//12X,'NODE',15X,'TEMPERATURE'//)                        BOUND........11500
C                                                                        BOUND........11600
C.....INPUT DATASET 20:  DATA FOR SPECIFIED CONCENTRATION OR             BOUND........11700
C        TEMPERATURE NODES                                               BOUND........11800
 1120 ERRCOD = 'REA-INP-S20'                                             BOUND........11900
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    BOUND........12000
      IF (ISERR) RETURN                                                  BOUND........12100
 1125 IPU=IPU+1                                                          BOUND........12200
      ERRCOD = 'REA-INP-20'                                              BOUND........12300
      CALL READIF(K1, INTFIL, ERRCOD)                                    BOUND........12400
      IF (ISERR) RETURN                                                  BOUND........12500
      READ(INTFIL,*,IOSTAT=INERR(1)) IDUM                                BOUND........12600
      IF (INERR(1).NE.0) THEN                                            BOUND........12700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        BOUND........12800
         RETURN                                                          BOUND........12900
      END IF                                                             BOUND........13000
      IDUMA = IABS(IDUM)                                                 BOUND........13100
      IF (IDUM.EQ.0) THEN                                                BOUND........13200
         GOTO 1180                                                       BOUND........13300
      ELSE IF (IDUMA.GT.NN) THEN                                         BOUND........13400
         ERRCOD = 'INP-20-1'                                             BOUND........13500
         INERR(1) = IDUMA                                                BOUND........13600
         INERR(2) = NN                                                   BOUND........13700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        BOUND........13800
         RETURN                                                          BOUND........13900
      ELSE IF (IPU.GT.NPBC+NUBC) THEN                                    BOUND........14000
         GOTO 1125                                                       BOUND........14100
      END IF                                                             BOUND........14200
      IUBC(IPU) = IDUM                                                   BOUND........14300
      IF (IUBC(IPU).GT.0) THEN                                           BOUND........14400
         ERRCOD = 'REA-INP-20'                                           BOUND........14500
         READ(INTFIL,*,IOSTAT=INERR(1)) IUBC(IPU),UBC(IPU)               BOUND........14600
         IF (INERR(1).NE.0) THEN                                         BOUND........14700
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     BOUND........14800
            RETURN                                                       BOUND........14900
         END IF                                                          BOUND........15000
         WRITE(K3,1150) IUBC(IPU),UBC(IPU)                               BOUND........15100
      ELSE IF (IUBC(IPU).LT.0) THEN                                      BOUND........15200
         IUBCT = -1                                                      BOUND........15300
         WRITE(K3,1150) IUBC(IPU)                                        BOUND........15400
      ELSE                                                               BOUND........15500
         GOTO 1180                                                       BOUND........15600
      END IF                                                             BOUND........15700
 1150 FORMAT(11X,I9,6X,1PD20.13)                                         BOUND........15800
      GOTO 1125                                                          BOUND........15900
 1180 IPU=IPU-1                                                          BOUND........16000
      IU=IPU-IP                                                          BOUND........16100
      IF(IU.EQ.NUBC) GOTO 1200                                           BOUND........16200
      IF (ME.EQ.1) THEN                                                  BOUND........16300
         ERRCOD = 'INP-3,20-2'                                           BOUND........16400
      ELSE                                                               BOUND........16500
         ERRCOD = 'INP-3,20-1'                                           BOUND........16600
      END IF                                                             BOUND........16700
      INERR(1) = IU                                                      BOUND........16800
      INERR(2) = NUBC                                                    BOUND........16900
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           BOUND........17000
      RETURN                                                             BOUND........17100
 1200 IF(IUBCT.NE.-1) GOTO 6000                                          BOUND........17200
      IF(ME) 1205,1205,1215                                              BOUND........17300
 1205 WRITE(K3,1206)                                                     BOUND........17400
 1206 FORMAT(//12X,'TIME-DEPENDENT SPECIFIED CONCENTRATION'/12X,'IS ',   BOUND........17500
     1   'INDICATED BY NEGATIVE NODE NUMBER')                            BOUND........17600
      GOTO 6000                                                          BOUND........17700
 1215 WRITE(K3,1216)                                                     BOUND........17800
 1216 FORMAT(//11X,'TIME-DEPENDENT SPECIFIED TEMPERATURE'/12X,'IS ',     BOUND........17900
     1   'INDICATED BY NEGATIVE NODE NUMBER')                            BOUND........18000
C                                                                        BOUND........18100
 6000 IF(IPBCT.EQ.-1.OR.IUBCT.EQ.-1) WRITE(K3,7000)                      BOUND........18200
 7000 FORMAT(////11X,'THE SPECIFIED TIME VARIATIONS ARE ',               BOUND........18300
     1   'USER-PROGRAMMED IN SUBROUTINE  B C T I M E .')                 BOUND........18400
C                                                                        BOUND........18500
C                                                                        BOUND........18600
      RETURN                                                             BOUND........18700
      END                                                                BOUND........18800
C                                                                        BOUND........18900
C     SUBROUTINE        B  U  D  G  E  T           SUTRA VERSION 2D3D.1  BUDGET.........100
C                                                                        BUDGET.........200
C *** PURPOSE :                                                          BUDGET.........300
C ***  TO CALCULATE AND OUTPUT FLUID MASS AND SOLUTE MASS OR             BUDGET.........400
C ***  ENERGY BUDGETS.                                                   BUDGET.........500
C                                                                        BUDGET.........600
      SUBROUTINE BUDGET(ML,IBCT,VOL,SW,DSWDP,RHO,SOP,QIN,PVEC,PM1,       BUDGET.........700
     1   DPDTITR,PBC,QPLITR,IPBC,IQSOP,POR,UVEC,UM1,UM2,UIN,QUIN,QINITR, BUDGET.........800
     2   IQSOU,UBC,IUBC,CS1,CS2,CS3,SL,SR,NREG)                          BUDGET.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BUDGET........1000
      CHARACTER*10 ADSMOD                                                BUDGET........1100
      CHARACTER*13 UNAME(2)                                              BUDGET........1200
      DIMENSION QIN(NN),UIN(NN),IQSOP(NSOP),QUIN(NN),QINITR(NN),         BUDGET........1300
     1   IQSOU(NSOU)                                                     BUDGET........1400
      DIMENSION IPBC(NBCN),IUBC(NBCN),UBC(NBCN),QPLITR(NBCN),PBC(NBCN)   BUDGET........1500
      DIMENSION POR(NN),VOL(NN),PVEC(NNVEC),UVEC(NNVEC),SW(NN),          BUDGET........1600
     1   DSWDP(NN),RHO(NN),SOP(NN),PM1(NN),DPDTITR(NN),UM1(NN),UM2(NN),  BUDGET........1700
     2   CS1(NN),CS2(NN),CS3(NN),SL(NN),SR(NN),NREG(NN)                  BUDGET........1800
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  BUDGET........1900
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             BUDGET........2000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BUDGET........2100
     1   NSOP,NSOU,NBCN                                                  BUDGET........2200
      COMMON /DIMX2/ NELTA,NNVEC,NDIMJA                                  BUDGET........2300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        BUDGET........2400
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      BUDGET........2500
      COMMON /MODSOR/ ADSMOD                                             BUDGET........2600
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      BUDGET........2700
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        BUDGET........2800
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       BUDGET........2900
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  BUDGET........3000
      DATA UNAME(1)/'CONCENTRATION'/,UNAME(2)/' TEMPERATURE '/           BUDGET........3100
      SAVE UNAME                                                         BUDGET........3200
C                                                                        BUDGET........3300
C                                                                        BUDGET........3400
      MN=2                                                               BUDGET........3500
      IF(IUNSAT.NE.0) IUNSAT=1                                           BUDGET........3600
      IF(ME.EQ.-1) MN=1                                                  BUDGET........3700
      WRITE(K3,10)                                                       BUDGET........3800
   10 FORMAT(1H1)                                                        BUDGET........3900
C.....SET UNSATURATED FLOW PARAMETERS, SW(I) AND DSWDP(I)                BUDGET........4000
      IF(IUNSAT-1) 40,20,40                                              BUDGET........4100
   20 DO 30 I=1,NN                                                       BUDGET........4200
      IF(PVEC(I)) 25,27,27                                               BUDGET........4300
   25 CALL UNSAT(SW(I),DSWDP(I),RELK,PVEC(I),NREG(I))                    BUDGET........4400
      GOTO 30                                                            BUDGET........4500
   27 SW(I)=1.0D0                                                        BUDGET........4600
      DSWDP(I)=0.0D0                                                     BUDGET........4700
   30 CONTINUE                                                           BUDGET........4800
C                                                                        BUDGET........4900
C.....CALCULATE COMPONENTS OF FLUID MASS BUDGET                          BUDGET........5000
   40 IF(ML-1) 50,50,1000                                                BUDGET........5100
   50 CONTINUE                                                           BUDGET........5200
      STPPOS = 0D0                                                       BUDGET........5300
      STPNEG = 0D0                                                       BUDGET........5400
      STUPOS = 0D0                                                       BUDGET........5500
      STUNEG = 0D0                                                       BUDGET........5600
      QINPOS = 0D0                                                       BUDGET........5700
      QINNEG = 0D0                                                       BUDGET........5800
      DO 100 I=1,NN                                                      BUDGET........5900
      TERM = (1-ISSFLO/2)*RHO(I)*VOL(I)*                                 BUDGET........6000
     1   (SW(I)*SOP(I)+POR(I)*DSWDP(I))*(PVEC(I)-PM1(I))/DELTP           BUDGET........6100
      STPPOS = STPPOS + MAX(0D0, TERM)                                   BUDGET........6200
      STPNEG = STPNEG + MIN(0D0, TERM)                                   BUDGET........6300
      TERM = (1-ISSFLO/2)*POR(I)*SW(I)*DRWDU*VOL(I)*                     BUDGET........6400
     1   (UM1(I)-UM2(I))/DLTUM1                                          BUDGET........6500
      STUPOS = STUPOS + MAX(0D0, TERM)                                   BUDGET........6600
      STUNEG = STUNEG + MIN(0D0, TERM)                                   BUDGET........6700
      TERM = QIN(I)                                                      BUDGET........6800
      QINPOS = QINPOS + MAX(0D0, TERM)                                   BUDGET........6900
      QINNEG = QINNEG + MIN(0D0, TERM)                                   BUDGET........7000
  100 CONTINUE                                                           BUDGET........7100
      STPTOT = STPPOS + STPNEG                                           BUDGET........7200
      STUTOT = STUPOS + STUNEG                                           BUDGET........7300
      STFPOS = STPPOS + STUPOS                                           BUDGET........7400
      STFNEG = STPNEG + STUNEG                                           BUDGET........7500
      STFTOT = STPTOT + STUTOT                                           BUDGET........7600
      QINTOT = QINPOS + QINNEG                                           BUDGET........7700
C                                                                        BUDGET........7800
      QPLPOS = 0D0                                                       BUDGET........7900
      QPLNEG = 0D0                                                       BUDGET........8000
      DO 200 IP=1,NPBC                                                   BUDGET........8100
      I=IABS(IPBC(IP))                                                   BUDGET........8200
      TERM = GNUP*(PBC(IP)-PVEC(I))                                      BUDGET........8300
      QPLPOS = QPLPOS + MAX(0D0, TERM)                                   BUDGET........8400
      QPLNEG = QPLNEG + MIN(0D0, TERM)                                   BUDGET........8500
  200 CONTINUE                                                           BUDGET........8600
      QPLTOT = QPLPOS + QPLNEG                                           BUDGET........8700
      QFFPOS = QINPOS + QPLPOS                                           BUDGET........8800
      QFFNEG = QINNEG + QPLNEG                                           BUDGET........8900
      QFFTOT = QINTOT + QPLTOT                                           BUDGET........9000
C                                                                        BUDGET........9100
C.....OUTPUT FLUID MASS BUDGET                                           BUDGET........9200
      ACTFMB = 5D-1*(STFPOS - STFNEG + QFFPOS - QFFNEG)                  BUDGET........9300
      ERFMBA = STFTOT - QFFTOT                                           BUDGET........9400
      WRITE(K3,300) IT,STPPOS,STPNEG,STPTOT,                             BUDGET........9500
     1   UNAME(MN),STUPOS,STUNEG,STUTOT,STFPOS,STFNEG,STFTOT,            BUDGET........9600
     2   QINPOS,QINNEG,QINTOT,QPLPOS,QPLNEG,QPLTOT,                      BUDGET........9700
     3   QFFPOS,QFFNEG,QFFTOT,ACTFMB,ERFMBA                              BUDGET........9800
  300 FORMAT(//11X,'F L U I D   M A S S   B U D G E T      AFTER TIME',  BUDGET........9900
     1   ' STEP ',I5,',     IN (MASS/SECOND)'                            BUDGET.......10000
     2   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/87X,'INCREASES(+)',4X,    BUDGET.......10100
     3   'DECREASES(-)',7X,'CHANGE'/84X,3(2X,14('='))                    BUDGET.......10200
     4   /13X,'RATE OF CHANGE IN TOTAL STORED FLUID DUE TO PRESSURE',    BUDGET.......10300
     5   ' CHANGE',12X,3(1X,1PD15.7)                                     BUDGET.......10400
     6   /13X,'RATE OF CHANGE IN TOTAL STORED FLUID DUE TO ',A13,        BUDGET.......10500
     7   ' CHANGE',7X,3(1X,1PD15.7)/84X,3(2X,14('-'))                    BUDGET.......10600
     8   /13X,'TOTAL RATE OF CHANGE IN STORED FLUID [ S+, S-, S ]',      BUDGET.......10700
     9   21X,3(1X,1PD15.7)                                               BUDGET.......10800
     T   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/89X,'GAINS(+)',7X,        BUDGET.......10900
     1   'LOSSES(-)',7X,'GAIN/LOSS'/84X,3(2X,14('='))                    BUDGET.......11000
     2   /13X,'GAIN/LOSS OF FLUID THROUGH FLUID SOURCES AND SINKS',      BUDGET.......11100
     3   21X,3(1X,1PD15.7)                                               BUDGET.......11200
     4   /13X,'GAIN/LOSS OF FLUID THROUGH INFLOWS/OUTFLOWS AT'           BUDGET.......11300
     5   ' SPECIFIED P NODES',7X,3(1X,1PD15.7)/84X,3(2X,14('-'))         BUDGET.......11400
     6   /13X,'TOTAL RATE OF GAIN/LOSS OF FLUID THROUGH FLOWS',          BUDGET.......11500
     7   ' [ F+, F-, F ]',11X,3(1X,1PD15.7)                              BUDGET.......11600
     8   ///13X,'FLUID MASS BALANCE ACTIVITY',                           BUDGET.......11700
     9   ' [ A = ((S+) - (S-) + (F+) - (F-))/2 ]',14X,1PD15.7            BUDGET.......11800
     T   /13X,'ABSOLUTE FLUID MASS BALANCE ERROR [ S - F ]',36X,1PD15.7) BUDGET.......11900
      IF (ACTFMB.NE.0D0) THEN                                            BUDGET.......12000
         ERFMBR = 1D2*ERFMBA/ACTFMB                                      BUDGET.......12100
         WRITE(K3,301) ERFMBR                                            BUDGET.......12200
      ELSE                                                               BUDGET.......12300
         WRITE(K3,302)                                                   BUDGET.......12400
      END IF                                                             BUDGET.......12500
  301 FORMAT(13X,'RELATIVE FLUID MASS BALANCE ERROR',                    BUDGET.......12600
     1   ' [ 100*(S - F)/A ]',28X,1PD15.7,' (PERCENT)')                  BUDGET.......12700
  302 FORMAT(13X,'RELATIVE FLUID MASS BALANCE ERROR',                    BUDGET.......12800
     1   ' [ 100*(S - F)/A ]',28X,'  UNDEFINED')                         BUDGET.......12900
C                                                                        BUDGET.......13000
      IF(IBCT.EQ.4) GOTO 600                                             BUDGET.......13100
      NSOPI=NSOP-1                                                       BUDGET.......13200
      INEGCT=0                                                           BUDGET.......13300
      DO 500 IQP=1,NSOPI                                                 BUDGET.......13400
      I=IQSOP(IQP)                                                       BUDGET.......13500
      IF(I) 325,500,500                                                  BUDGET.......13600
  325 INEGCT=INEGCT+1                                                    BUDGET.......13700
      IF(INEGCT.EQ.1) WRITE(K3,350)                                      BUDGET.......13800
  350 FORMAT(///22X,'TIME-DEPENDENT FLUID SOURCES OR SINKS'//22X,        BUDGET.......13900
     1   ' NODE',5X,'INFLOW(+)/OUTFLOW(-)'/37X,'  (MASS/SECOND)'//)      BUDGET.......14000
      WRITE(K3,450) -I,QIN(-I)                                           BUDGET.......14100
  450 FORMAT(18X,I9,10X,1PD15.7)                                         BUDGET.......14200
  500 CONTINUE                                                           BUDGET.......14300
C                                                                        BUDGET.......14400
  600 IF(NPBC.EQ.0) GOTO 800                                             BUDGET.......14500
      WRITE(K3,650)                                                      BUDGET.......14600
  650 FORMAT(///22X,'FLUID SOURCES OR SINKS DUE TO SPECIFIED PRESSURES', BUDGET.......14700
     1   //22X,' NODE',5X,'INFLOW(+)/OUTFLOW(-)'/37X,'  (MASS/SECOND)'/) BUDGET.......14800
      DO 700 IP=1,NPBC                                                   BUDGET.......14900
      I=IABS(IPBC(IP))                                                   BUDGET.......15000
      WRITE(K3,450) I, GNUP*(PBC(IP)-PVEC(I))                            BUDGET.......15100
  700 CONTINUE                                                           BUDGET.......15200
C                                                                        BUDGET.......15300
C.....CALCULATE COMPONENTS OF ENERGY OR SOLUTE MASS BUDGET               BUDGET.......15400
  800 IF(ML-1) 1000,5500,1000                                            BUDGET.......15500
 1000 CONTINUE                                                           BUDGET.......15600
      FLDPOS = 0D0                                                       BUDGET.......15700
      FLDNEG = 0D0                                                       BUDGET.......15800
      SLDPOS = 0D0                                                       BUDGET.......15900
      SLDNEG = 0D0                                                       BUDGET.......16000
      DNSPOS = 0D0                                                       BUDGET.......16100
      DNSNEG = 0D0                                                       BUDGET.......16200
      P1FPOS = 0D0                                                       BUDGET.......16300
      P1FNEG = 0D0                                                       BUDGET.......16400
      P1SPOS = 0D0                                                       BUDGET.......16500
      P1SNEG = 0D0                                                       BUDGET.......16600
      P0FPOS = 0D0                                                       BUDGET.......16700
      P0FNEG = 0D0                                                       BUDGET.......16800
      P0SPOS = 0D0                                                       BUDGET.......16900
      P0SNEG = 0D0                                                       BUDGET.......17000
      QQUPOS = 0D0                                                       BUDGET.......17100
      QQUNEG = 0D0                                                       BUDGET.......17200
      QIUPOS = 0D0                                                       BUDGET.......17300
      QIUNEG = 0D0                                                       BUDGET.......17400
C.....SET ADSORPTION PARAMETERS                                          BUDGET.......17500
      IF(ME.EQ.-1.AND.ADSMOD.NE.'NONE      ')                            BUDGET.......17600
     1   CALL ADSORB(CS1,CS2,CS3,SL,SR,UVEC)                             BUDGET.......17700
      DO 1300 I=1,NN                                                     BUDGET.......17800
      ESRV=POR(I)*SW(I)*RHO(I)*VOL(I)                                    BUDGET.......17900
      EPRSV=(1.D0-POR(I))*RHOS*VOL(I)                                    BUDGET.......18000
      DUDT=(1-ISSTRA)*(UVEC(I)-UM1(I))/DELTU                             BUDGET.......18100
      TERM = ESRV*CW*DUDT                                                BUDGET.......18200
      FLDPOS = FLDPOS + MAX(0D0, TERM)                                   BUDGET.......18300
      FLDNEG = FLDNEG + MIN(0D0, TERM)                                   BUDGET.......18400
      TERM = EPRSV*CS1(I)*DUDT                                           BUDGET.......18500
      SLDPOS = SLDPOS + MAX(0D0, TERM)                                   BUDGET.......18600
      SLDNEG = SLDNEG + MIN(0D0, TERM)                                   BUDGET.......18700
      TERM = CW*UVEC(I)*(1-ISSFLO/2)*VOL(I)*                             BUDGET.......18800
     1   (RHO(I)*(SW(I)*SOP(I)+POR(I)*DSWDP(I))*DPDTITR(I)               BUDGET.......18900
     2   +POR(I)*SW(I)*DRWDU*(UM1(I)-UM2(I))/DLTUM1)                     BUDGET.......19000
      DNSPOS = DNSPOS + MAX(0D0, TERM)                                   BUDGET.......19100
      DNSNEG = DNSNEG + MIN(0D0, TERM)                                   BUDGET.......19200
      TERM = ESRV*PRODF1*UVEC(I)                                         BUDGET.......19300
      P1FPOS = P1FPOS + MAX(0D0, TERM)                                   BUDGET.......19400
      P1FNEG = P1FNEG + MIN(0D0, TERM)                                   BUDGET.......19500
      TERM = EPRSV*PRODS1*(SL(I)*UVEC(I)+SR(I))                          BUDGET.......19600
      P1SPOS = P1SPOS + MAX(0D0, TERM)                                   BUDGET.......19700
      P1SNEG = P1SNEG + MIN(0D0, TERM)                                   BUDGET.......19800
      TERM = ESRV*PRODF0                                                 BUDGET.......19900
      P0FPOS = P0FPOS + MAX(0D0, TERM)                                   BUDGET.......20000
      P0FNEG = P0FNEG + MIN(0D0, TERM)                                   BUDGET.......20100
      TERM = EPRSV*PRODS0                                                BUDGET.......20200
      P0SPOS = P0SPOS + MAX(0D0, TERM)                                   BUDGET.......20300
      P0SNEG = P0SNEG + MIN(0D0, TERM)                                   BUDGET.......20400
      TERM = QUIN(I)                                                     BUDGET.......20500
      QQUPOS = QQUPOS + MAX(0D0, TERM)                                   BUDGET.......20600
      QQUNEG = QQUNEG + MIN(0D0, TERM)                                   BUDGET.......20700
      IF (QINITR(I).LE.0D0) THEN                                         BUDGET.......20800
         TERM = QINITR(I)*CW*UVEC(I)                                     BUDGET.......20900
      ELSE                                                               BUDGET.......21000
         TERM = QINITR(I)*CW*UIN(I)                                      BUDGET.......21100
      END IF                                                             BUDGET.......21200
      QIUPOS = QIUPOS + MAX(0D0, TERM)                                   BUDGET.......21300
      QIUNEG = QIUNEG + MIN(0D0, TERM)                                   BUDGET.......21400
 1300 CONTINUE                                                           BUDGET.......21500
      FLDTOT = FLDPOS + FLDNEG                                           BUDGET.......21600
      SLDTOT = SLDPOS + SLDNEG                                           BUDGET.......21700
      DNSTOT = DNSPOS + DNSNEG                                           BUDGET.......21800
      STSPOS = FLDPOS + SLDPOS + DNSPOS                                  BUDGET.......21900
      STSNEG = FLDNEG + SLDNEG + DNSNEG                                  BUDGET.......22000
      STSTOT = FLDTOT + SLDTOT + DNSTOT                                  BUDGET.......22100
      P1FTOT = P1FPOS + P1FNEG                                           BUDGET.......22200
      P1STOT = P1SPOS + P1SNEG                                           BUDGET.......22300
      P0FTOT = P0FPOS + P0FNEG                                           BUDGET.......22400
      P0STOT = P0SPOS + P0SNEG                                           BUDGET.......22500
      PRSPOS = P1FPOS + P1SPOS + P0FPOS + P0SPOS                         BUDGET.......22600
      PRSNEG = P1FNEG + P1SNEG + P0FNEG + P0SNEG                         BUDGET.......22700
      PRSTOT = P1FTOT + P1STOT + P0FTOT + P0STOT                         BUDGET.......22800
      QQUTOT = QQUPOS + QQUNEG                                           BUDGET.......22900
      QIUTOT = QIUPOS + QIUNEG                                           BUDGET.......23000
C                                                                        BUDGET.......23100
      QPUPOS = 0D0                                                       BUDGET.......23200
      QPUNEG = 0D0                                                       BUDGET.......23300
      DO 1500 IP=1,NPBC                                                  BUDGET.......23400
      IF (QPLITR(IP).LE.0D0) THEN                                        BUDGET.......23500
         I=IABS(IPBC(IP))                                                BUDGET.......23600
         TERM = QPLITR(IP)*CW*UVEC(I)                                    BUDGET.......23700
      ELSE                                                               BUDGET.......23800
         TERM = QPLITR(IP)*CW*UBC(IP)                                    BUDGET.......23900
      END IF                                                             BUDGET.......24000
      QPUPOS = QPUPOS + MAX(0D0, TERM)                                   BUDGET.......24100
      QPUNEG = QPUNEG + MIN(0D0, TERM)                                   BUDGET.......24200
 1500 CONTINUE                                                           BUDGET.......24300
      QPUTOT = QPUPOS + QPUNEG                                           BUDGET.......24400
C                                                                        BUDGET.......24500
      QULPOS = 0D0                                                       BUDGET.......24600
      QULNEG = 0D0                                                       BUDGET.......24700
      QULTOT = 0D0                                                       BUDGET.......24800
      IF(NUBC.EQ.0) GOTO 1520                                            BUDGET.......24900
      DO 1510 IU=1,NUBC                                                  BUDGET.......25000
      IUP=IU+NPBC                                                        BUDGET.......25100
      I=IABS(IUBC(IUP))                                                  BUDGET.......25200
      QPLITR(IUP)=GNUU*(UBC(IUP)-UVEC(I))                                BUDGET.......25300
      TERM = QPLITR(IUP)                                                 BUDGET.......25400
      QULPOS = QULPOS + MAX(0D0, TERM)                                   BUDGET.......25500
      QULNEG = QULNEG + MIN(0D0, TERM)                                   BUDGET.......25600
 1510 CONTINUE                                                           BUDGET.......25700
 1520 QULTOT = QULPOS + QULNEG                                           BUDGET.......25800
      QFSPOS = QIUPOS + QPUPOS + QQUPOS + QULPOS                         BUDGET.......25900
      QFSNEG = QIUNEG + QPUNEG + QQUNEG + QULNEG                         BUDGET.......26000
      QFSTOT = QIUTOT + QPUTOT + QQUTOT + QULTOT                         BUDGET.......26100
C                                                                        BUDGET.......26200
 1540 IF(ME) 1550,1550,1615                                              BUDGET.......26300
C                                                                        BUDGET.......26400
C.....OUTPUT SOLUTE MASS BUDGET                                          BUDGET.......26500
 1550 ACTSMB = 5D-1*(STSPOS - STSNEG + PRSPOS - PRSNEG                   BUDGET.......26600
     1   + QFSPOS - QFSNEG)                                              BUDGET.......26700
      ERSMBA = STSTOT - PRSTOT - QFSTOT                                  BUDGET.......26800
      WRITE(K3,1600) IT,FLDPOS,FLDNEG,FLDTOT,SLDPOS,SLDNEG,SLDTOT,       BUDGET.......26900
     1   DNSPOS,DNSNEG,DNSTOT,STSPOS,STSNEG,STSTOT,                      BUDGET.......27000
     2   P1FPOS,P1FNEG,P1FTOT,P1SPOS,P1SNEG,P1STOT,                      BUDGET.......27100
     3   P0FPOS,P0FNEG,P0FTOT,P0SPOS,P0SNEG,P0STOT,PRSPOS,PRSNEG,PRSTOT, BUDGET.......27200
     4   QIUPOS,QIUNEG,QIUTOT,QPUPOS,QPUNEG,QPUTOT,                      BUDGET.......27300
     5   QQUPOS,QQUNEG,QQUTOT,QULPOS,QULNEG,QULTOT,QFSPOS,QFSNEG,QFSTOT, BUDGET.......27400
     6   ACTSMB,ERSMBA                                                   BUDGET.......27500
 1600 FORMAT(//11X,'S O L U T E   B U D G E T      AFTER TIME STEP ',I5, BUDGET.......27600
     1   ',   IN (SOLUTE MASS/SECOND)'                                   BUDGET.......27700
     2   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/87X,'INCREASES(+)',4X,    BUDGET.......27800
     3   'DECREASES(-)',7X,'CHANGE'/84X,3(2X,14('='))                    BUDGET.......27900
     4   /13X,'RATE OF CHANGE IN SOLUTE DUE TO CONCENTRATION CHANGE',    BUDGET.......28000
     5   19X,3(1X,1PD15.7)                                               BUDGET.......28100
     6   /13X,'RATE OF CHANGE OF ADSORBATE',44X,3(1X,1PD15.7)            BUDGET.......28200
     7   /13X,'RATE OF CHANGE IN SOLUTE DUE TO CHANGE IN MASS OF FLUID', BUDGET.......28300
     8   16X,3(1X,1PD15.7)/84X,3(2X,14('-'))                             BUDGET.......28400
     9   /13X,'TOTAL RATE OF CHANGE OF SOLUTE [ S+, S-, S ]',            BUDGET.......28500
     T   27X,3(1X,1PD15.7)                                               BUDGET.......28600
     1   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/87X,'PRODUCTION(+)',5X,   BUDGET.......28700
     2   'DECAY(-)',7X,'PROD./DECAY'/84X,3(2X,14('='))                   BUDGET.......28800
     3   /13X,'FIRST-ORDER PRODUCTION/DECAY OF SOLUTE',33X,3(1X,1PD15.7) BUDGET.......28900
     4   /13X,'FIRST-ORDER PRODUCTION/DECAY OF ADSORBATE',               BUDGET.......29000
     5   30X,3(1X,1PD15.7)                                               BUDGET.......29100
     6   /13X,'ZERO-ORDER PRODUCTION/DECAY OF SOLUTE',34X,3(1X,1PD15.7)  BUDGET.......29200
     7   /13X,'ZERO-ORDER PRODUCTION/DECAY OF ADSORBATE',                BUDGET.......29300
     8   31X,3(1X,1PD15.7)/84X,3(2X,14('-'))                             BUDGET.......29400
     9   /13X,'TOTAL RATE OF PRODUCTION/DECAY OF SOLUTE AND ADSORBATE',  BUDGET.......29500
     T   ' [ P+, P-, P ]',3X,3(1X,1PD15.7)                               BUDGET.......29600
     1   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/89X,'GAINS(+)',7X,        BUDGET.......29700
     2   'LOSSES(-)',7X,'GAIN/LOSS'/84X,3(2X,14('='))                    BUDGET.......29800
     3   /13X,'GAIN/LOSS OF SOLUTE THROUGH FLUID SOURCES AND SINKS',     BUDGET.......29900
     4   20X,3(1X,1PD15.7)                                               BUDGET.......30000
     5   /13X,'GAIN/LOSS OF SOLUTE THROUGH INFLOWS/OUTFLOWS AT'          BUDGET.......30100
     6   ' SPECIFIED P NODES',6X,3(1X,1PD15.7)                           BUDGET.......30200
     7   /13X,'GAIN/LOSS OF SOLUTE THROUGH SOLUTE SOURCES AND SINKS',    BUDGET.......30300
     8   19X,3(1X,1PD15.7)                                               BUDGET.......30400
     9   /13X,'GAIN/LOSS OF SOLUTE AT SPECIFIED CONCENTRATION NODES',    BUDGET.......30500
     T   19X,3(1X,1PD15.7)/84X,3(2X,14('-'))                             BUDGET.......30600
     1   /13X,'TOTAL RATE OF GAIN/LOSS OF SOLUTE',38X,3(1X,1PD15.7)      BUDGET.......30700
     2   /16X,' THROUGH FLOWS & SOURCES/SINKS [ F+, F-, F ]'             BUDGET.......30800
     3   ///13X,'SOLUTE MASS BAL. ACTIVITY [ A = ((S+) - (S-)',          BUDGET.......30900
     4   ' + (P+) - (P-) + (F+) - (F-))/2 ]',2X,1PD15.7                  BUDGET.......31000
     5   /13X,'ABSOLUTE SOLUTE MASS BALANCE ERROR [ S - P - F ]',        BUDGET.......31100
     6   31X,1PD15.7)                                                    BUDGET.......31200
      IF (ACTSMB.NE.0D0) THEN                                            BUDGET.......31300
         ERSMBR = 1D2*ERSMBA/ACTSMB                                      BUDGET.......31400
         WRITE(K3,1601) ERSMBR                                           BUDGET.......31500
      ELSE                                                               BUDGET.......31600
         WRITE(K3,1602)                                                  BUDGET.......31700
      END IF                                                             BUDGET.......31800
 1601 FORMAT(13X,'RELATIVE SOLUTE MASS BALANCE ERROR',                   BUDGET.......31900
     1   ' [ 100*(S - P - F)/A ]',23X,1PD15.7,' (PERCENT)')              BUDGET.......32000
 1602 FORMAT(13X,'RELATIVE SOLUTE MASS BALANCE ERROR',                   BUDGET.......32100
     1   ' [ 100*(S - P - F)/A ]',23X,'  UNDEFINED')                     BUDGET.......32200
      GOTO 1645                                                          BUDGET.......32300
C                                                                        BUDGET.......32400
C.....OUTPUT ENERGY BUDGET                                               BUDGET.......32500
 1615 ACTSMB = 5D-1*(STSPOS - STSNEG + PRSPOS - PRSNEG                   BUDGET.......32600
     1   + QFSPOS - QFSNEG)                                              BUDGET.......32700
      ERSMBA = STSTOT - PRSTOT - QFSTOT                                  BUDGET.......32800
      WRITE(K3,1635) IT,FLDPOS,FLDNEG,FLDTOT,SLDPOS,SLDNEG,SLDTOT,       BUDGET.......32900
     1   DNSPOS,DNSNEG,DNSTOT,STSPOS,STSNEG,STSTOT,                      BUDGET.......33000
     2   P0FPOS,P0FNEG,P0FTOT,P0SPOS,P0SNEG,P0STOT,PRSPOS,PRSNEG,PRSTOT, BUDGET.......33100
     3   QIUPOS,QIUNEG,QIUTOT,QPUPOS,QPUNEG,QPUTOT,                      BUDGET.......33200
     4   QQUPOS,QQUNEG,QQUTOT,QULPOS,QULNEG,QULTOT,QFSPOS,QFSNEG,QFSTOT, BUDGET.......33300
     5   ACTSMB,ERSMBA                                                   BUDGET.......33400
 1635 FORMAT(//11X,'E N E R G Y   B U D G E T      AFTER TIME STEP ',I5, BUDGET.......33500
     1   ',   IN (ENERGY/SECOND)'                                        BUDGET.......33600
     2   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/87X,'INCREASES(+)',4X,    BUDGET.......33700
     3   'DECREASES(-)',7X,'CHANGE'/84X,3(2X,14('='))                    BUDGET.......33800
     4   /13X,'RATE OF CHANGE OF ENERGY IN FLUID DUE TO TEMPERATURE',    BUDGET.......33900
     5   ' CHANGE',12X,3(1X,1PD15.7)                                     BUDGET.......34000
     6   /13X,'RATE OF CHANGE OF ENERGY IN SOLID GRAINS',                BUDGET.......34100
     7   31X,3(1X,1PD15.7)                                               BUDGET.......34200
     8   /13X,'RATE OF CHANGE OF ENERGY DUE TO CHANGE IN MASS OF FLUID', BUDGET.......34300
     9   16X,3(1X,1PD15.7)/84X,3(2X,14('-'))                             BUDGET.......34400
     T   /13X,'TOTAL RATE OF CHANGE OF ENERGY [ S+, S-, S ]',            BUDGET.......34500
     1   27X,3(1X,1PD15.7)                                               BUDGET.......34600
     2   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/87X,'PRODUCTION(+)',5X,   BUDGET.......34700
     3   'DECAY(-)',7X,'PROD./DECAY'/84X,3(2X,14('='))                   BUDGET.......34800
     4   /13X,'ZERO-ORDER PRODUCTION/DECAY OF ENERGY IN FLUID',          BUDGET.......34900
     5   25X,3(1X,1PD15.7)                                               BUDGET.......35000
     6   /13X,'ZERO-ORDER PRODUCTION/DECAY OF ENERGY IN SOLID GRAINS',   BUDGET.......35100
     7   18X,3(1X,1PD15.7)/84X,3(2X,14('-'))                             BUDGET.......35200
     8   /13X,'TOTAL RATE OF PRODUCTION/DECAY OF ENERGY',                BUDGET.......35300
     9   ' [ P+, P-, P ]',17X,3(1X,1PD15.7)                              BUDGET.......35400
     T   //89X,'SUM OF',10X,'SUM OF',12X,'NET'/89X,'GAINS(+)',7X,        BUDGET.......35500
     1   'LOSSES(-)',7X,'GAIN/LOSS'/84X,3(2X,14('='))                    BUDGET.......35600
     2   /13X,'GAIN/LOSS OF ENERGY THROUGH FLUID SOURCES AND SINKS',     BUDGET.......35700
     3   20X,3(1X,1PD15.7)                                               BUDGET.......35800
     4   /13X,'GAIN/LOSS OF ENERGY THROUGH INFLOWS/OUTFLOWS AT'          BUDGET.......35900
     5   ' SPECIFIED P NODES',6X,3(1X,1PD15.7)                           BUDGET.......36000
     6   /13X,'GAIN/LOSS OF ENERGY THROUGH ENERGY SOURCES AND SINKS',    BUDGET.......36100
     7   19X,3(1X,1PD15.7)                                               BUDGET.......36200
     8   /13X,'GAIN/LOSS OF ENERGY AT SPECIFIED TEMPERATURE NODES',      BUDGET.......36300
     9   21X,3(1X,1PD15.7)/84X,3(2X,14('-'))                             BUDGET.......36400
     T   /13X,'TOTAL RATE OF GAIN/LOSS OF ENERGY',38X,3(1X,1PD15.7)      BUDGET.......36500
     1   /16X,' THROUGH FLOWS & SOURCES/SINKS [ F+, F-, F ]'             BUDGET.......36600
     2   ///13X,'ENERGY BALANCE ACTIVITY [ A = ((S+) - (S-)',            BUDGET.......36700
     3   ' + (P+) - (P-) + (F+) - (F-))/2 ]',4X,1PD15.7                  BUDGET.......36800
     4   /13X,'ABSOLUTE ENERGY BALANCE ERROR [ S - P - F ]',             BUDGET.......36900
     5   36X,1PD15.7)                                                    BUDGET.......37000
      IF (ACTSMB.NE.0D0) THEN                                            BUDGET.......37100
         ERSMBR = 1D2*ERSMBA/ACTSMB                                      BUDGET.......37200
         WRITE(K3,1641) ERSMBR                                           BUDGET.......37300
      ELSE                                                               BUDGET.......37400
         WRITE(K3,1642)                                                  BUDGET.......37500
      END IF                                                             BUDGET.......37600
 1641 FORMAT(13X,'RELATIVE ENERGY BALANCE ERROR',                        BUDGET.......37700
     1   ' [ 100*(S - P - F)/A ]',28X,1PD15.7,' (PERCENT)')              BUDGET.......37800
 1642 FORMAT(13X,'RELATIVE ENERGY BALANCE ERROR',                        BUDGET.......37900
     1   ' [ 100*(S - P - F)/A ]',28X,'  UNDEFINED')                     BUDGET.......38000
C                                                                        BUDGET.......38100
 1645 IF ((IT.EQ.1).AND.(ITER.EQ.1).AND.(ISSTRA.NE.1)) WRITE(K3,1646)    BUDGET.......38200
 1646 FORMAT(/13X,'******** NOTE: ON THE FIRST ITERATION OF THE ',       BUDGET.......38300
     1   'FIRST TIME STEP, A LARGE RELATIVE ERROR IN THE  ********'      BUDGET.......38400
     2   /13X,'******** SOLUTE MASS OR ENERGY BUDGET DOES NOT ',         BUDGET.......38500
     3   'NECESSARILY INDICATE AN INACCURATE TRANSPORT  ********'        BUDGET.......38600
     4   /13X,'******** SOLUTION. THE BUDGET CALCULATION WILL ',         BUDGET.......38700
     5   'NOT YIELD A MEANINGFUL RESULT UNLESS THE      ********'        BUDGET.......38800
     6   /13X,'******** INITIAL CONDITIONS REPRESENT MUTUALLY ',         BUDGET.......38900
     7   'CONSISTENT SOLUTIONS FOR FLOW AND TRANSPORT   ********'        BUDGET.......39000
     8   /13X,'******** FROM A PREVIOUS SUTRA SIMULATION THAT ',         BUDGET.......39100
     9   'ARE ALSO CONSISTENT WITH THE PRESENT SOURCES  ********'        BUDGET.......39200
     T   /13X,'******** AND BOUNDARY CONDITIONS.',60X,'********')        BUDGET.......39300
C                                                                        BUDGET.......39400
      NSOPI=NSOP-1                                                       BUDGET.......39500
      IF(NSOPI.EQ.0) GOTO 2000                                           BUDGET.......39600
      IF(ME) 1649,1649,1659                                              BUDGET.......39700
 1649 WRITE(K3,1650)                                                     BUDGET.......39800
 1650 FORMAT(///22X,'SOLUTE SOURCES OR SINKS AT FLUID SOURCES AND ',     BUDGET.......39900
     1   'SINKS'//22X,' NODE',8X,'SOURCE(+)/SINK(-)'/32X,                BUDGET.......40000
     2   '(SOLUTE MASS/SECOND)'/)                                        BUDGET.......40100
      GOTO 1680                                                          BUDGET.......40200
 1659 WRITE(K3,1660)                                                     BUDGET.......40300
 1660 FORMAT(///22X,'ENERGY SOURCES OR SINKS AT FLUID SOURCES AND ',     BUDGET.......40400
     1   'SINKS'//22X,' NODE',8X,'SOURCE(+)/SINK(-)'/37X,                BUDGET.......40500
     2   '(ENERGY/SECOND)'/)                                             BUDGET.......40600
 1680 DO 1900 IQP=1,NSOPI                                                BUDGET.......40700
      I=IABS(IQSOP(IQP))                                                 BUDGET.......40800
      IF(QINITR(I)) 1700,1700,1750                                       BUDGET.......40900
 1700 QU=QINITR(I)*CW*UVEC(I)                                            BUDGET.......41000
      GOTO 1800                                                          BUDGET.......41100
 1750 QU=QINITR(I)*CW*UIN(I)                                             BUDGET.......41200
 1800 WRITE(K3,450) I,QU                                                 BUDGET.......41300
 1900 CONTINUE                                                           BUDGET.......41400
C                                                                        BUDGET.......41500
 2000 IF(NPBC.EQ.0) GOTO 4500                                            BUDGET.......41600
      IF(ME) 2090,2090,2150                                              BUDGET.......41700
 2090 WRITE(K3,2100)                                                     BUDGET.......41800
 2100 FORMAT(///22X,'SOLUTE SOURCES OR SINKS DUE TO FLUID INFLOWS OR ',  BUDGET.......41900
     1   'OUTFLOWS AT POINTS OF SPECIFIED PRESSURE'//22X,' NODE',8X,     BUDGET.......42000
     2   'SOURCE(+)/SINK(-)'/32X,'(SOLUTE MASS/SECOND)'/)                BUDGET.......42100
      GOTO 2190                                                          BUDGET.......42200
 2150 WRITE(K3,2160)                                                     BUDGET.......42300
 2160 FORMAT(///22X,'ENERGY SOURCES OR SINKS DUE TO FLUID INFLOWS OR ',  BUDGET.......42400
     1   'OUTFLOWS AT POINTS OF SPECIFIED PRESSURE'//22X,' NODE',8X,     BUDGET.......42500
     2   'SOURCE(+)/SINK(-)'/37X,'(ENERGY/SECOND)'/)                     BUDGET.......42600
 2190 DO 2400 IP=1,NPBC                                                  BUDGET.......42700
      I=IABS(IPBC(IP))                                                   BUDGET.......42800
      IF(QPLITR(IP)) 2200,2200,2250                                      BUDGET.......42900
 2200 QPU=QPLITR(IP)*CW*UVEC(I)                                          BUDGET.......43000
      GOTO 2300                                                          BUDGET.......43100
 2250 QPU=QPLITR(IP)*CW*UBC(IP)                                          BUDGET.......43200
 2300 WRITE(K3,450) I,QPU                                                BUDGET.......43300
 2400 CONTINUE                                                           BUDGET.......43400
C                                                                        BUDGET.......43500
      IF(IBCT.EQ.4) GOTO 4500                                            BUDGET.......43600
      NSOUI=NSOU-1                                                       BUDGET.......43700
      INEGCT=0                                                           BUDGET.......43800
      DO 3500 IQU=1,NSOUI                                                BUDGET.......43900
      I=IQSOU(IQU)                                                       BUDGET.......44000
      IF(I) 3400,3500,3500                                               BUDGET.......44100
 3400 INEGCT=INEGCT+1                                                    BUDGET.......44200
      IF(ME) 3450,3450,3460                                              BUDGET.......44300
 3450 IF(INEGCT.EQ.1) WRITE(K3,3455)                                     BUDGET.......44400
 3455 FORMAT(///22X,'TIME-DEPENDENT SOLUTE SOURCES AND SINKS'//22X,      BUDGET.......44500
     1   ' NODE',10X,'GAIN(+)/LOSS(-)'/30X,'  (SOLUTE MASS/SECOND)'//)   BUDGET.......44600
      GOTO 3475                                                          BUDGET.......44700
 3460 IF(INEGCT.EQ.1) WRITE(K3,3465)                                     BUDGET.......44800
 3465 FORMAT(///22X,'TIME-DEPENDENT ENERGY SOURCES AND SINKS'//22X,      BUDGET.......44900
     1   ' NODE',10X,'GAIN(+)/LOSS(-)'/35X,'  (ENERGY/SECOND)'//)        BUDGET.......45000
 3475 CONTINUE                                                           BUDGET.......45100
      WRITE(K3,3490) -I,QUIN(-I)                                         BUDGET.......45200
 3490 FORMAT(22X,I9,10X,1PD15.7)                                         BUDGET.......45300
 3500 CONTINUE                                                           BUDGET.......45400
C                                                                        BUDGET.......45500
 4500 IF(NUBC.EQ.0) GOTO 5500                                            BUDGET.......45600
      IF(ME) 4600,4600,4655                                              BUDGET.......45700
 4600 WRITE(K3,4650)                                                     BUDGET.......45800
 4650 FORMAT(///22X,'SOLUTE SOURCES OR SINKS DUE TO SPECIFIED ',         BUDGET.......45900
     1   'CONCENTRATIONS'//22X,' NODE',10X,'GAIN(+)/LOSS(-)'/30X,        BUDGET.......46000
     2   '  (SOLUTE MASS/SECOND)'/)                                      BUDGET.......46100
      GOTO 4690                                                          BUDGET.......46200
 4655 WRITE(K3,4660)                                                     BUDGET.......46300
 4660 FORMAT(///22X,'ENERGY SOURCES OR SINKS DUE TO SPECIFIED ',         BUDGET.......46400
     1   'TEMPERATURES'//22X,' NODE',10X,'GAIN(+)/LOSS(-)'/35X,          BUDGET.......46500
     2   '  (ENERGY/SECOND)'/)                                           BUDGET.......46600
 4690 CONTINUE                                                           BUDGET.......46700
      DO 4700 IU=1,NUBC                                                  BUDGET.......46800
      IUP=IU+NPBC                                                        BUDGET.......46900
      I=IABS(IUBC(IUP))                                                  BUDGET.......47000
      WRITE(K3,450) I,QPLITR(IUP)                                        BUDGET.......47100
 4700 CONTINUE                                                           BUDGET.......47200
C                                                                        BUDGET.......47300
C                                                                        BUDGET.......47400
 5500 CONTINUE                                                           BUDGET.......47500
C                                                                        BUDGET.......47600
      RETURN                                                             BUDGET.......47700
      END                                                                BUDGET.......47800
C                                                                        BUDGET.......47900
C     SUBROUTINE        C  O  N  N  E  C           SUTRA VERSION 2D3D.1  CONNEC.........100
C                                                                        CONNEC.........200
C *** PURPOSE :                                                          CONNEC.........300
C ***  TO READ, ORGANIZE, AND CHECK DATA ON NODE INCIDENCES.             CONNEC.........400
C                                                                        CONNEC.........500
      SUBROUTINE CONNEC(IN)                                              CONNEC.........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                CONNEC.........700
      CHARACTER INTFIL*1000                                              CONNEC.........800
      CHARACTER CDUM10*10                                                CONNEC.........900
      CHARACTER*80 ERRCOD,CHERR(10),FNAME(0:7)                           CONNEC........1000
      DIMENSION IN(NIN)                                                  CONNEC........1100
      DIMENSION IIN(8)                                                   CONNEC........1200
      DIMENSION INERR(10),RLERR(10)                                      CONNEC........1300
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              CONNEC........1400
     1   NSOP,NSOU,NBCN                                                  CONNEC........1500
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   CONNEC........1600
      COMMON /ERRHAN/ ISERR                                              CONNEC........1700
      COMMON /FNAMES/ FNAME                                              CONNEC........1800
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        CONNEC........1900
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     CONNEC........2000
     1   KSCRN,KPAUSE                                                    CONNEC........2100
C                                                                        CONNEC........2200
      IPIN=0                                                             CONNEC........2300
      IF(KINCID.EQ.0) WRITE(K3,1)                                        CONNEC........2400
    1 FORMAT(1H1////11X,'M E S H   C O N N E C T I O N   D A T A'//      CONNEC........2500
     1   16X,'PRINTOUT OF NODAL INCIDENCES CANCELLED.')                  CONNEC........2600
      IF(KINCID.EQ.+1) WRITE(K3,2)                                       CONNEC........2700
    2 FORMAT(1H1////11X,'M E S H   C O N N E C T I O N   D A T A',       CONNEC........2800
     1   ///11X,'**** NODAL INCIDENCES ****'///)                         CONNEC........2900
C                                                                        CONNEC........3000
C.....INPUT DATASET 22 AND CHECK FOR ERRORS                              CONNEC........3100
      ERRCOD = 'REA-INP-S22'                                             CONNEC........3200
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    CONNEC........3300
      IF (ISERR) RETURN                                                  CONNEC........3400
      ERRCOD = 'REA-INP-22'                                              CONNEC........3500
      CALL READIF(K1, INTFIL, ERRCOD)                                    CONNEC........3600
      IF (ISERR) RETURN                                                  CONNEC........3700
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10                              CONNEC........3800
      IF (INERR(1).NE.0) THEN                                            CONNEC........3900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        CONNEC........4000
         RETURN                                                          CONNEC........4100
      END IF                                                             CONNEC........4200
      IF (CDUM10.NE.'INCIDENCE ') THEN                                   CONNEC........4300
         ERRCOD = 'INP-22-1'                                             CONNEC........4400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        CONNEC........4500
         RETURN                                                          CONNEC........4600
      END IF                                                             CONNEC........4700
      DO 1000 L=1,NE                                                     CONNEC........4800
      ERRCOD = 'REA-INP-22'                                              CONNEC........4900
      CALL READIF(K1, INTFIL, ERRCOD)                                    CONNEC........5000
      IF (ISERR) RETURN                                                  CONNEC........5100
      READ(INTFIL,*,IOSTAT=INERR(1)) LL,(IIN(II),II=1,N48)               CONNEC........5200
      IF (INERR(1).NE.0) THEN                                            CONNEC........5300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        CONNEC........5400
         RETURN                                                          CONNEC........5500
      END IF                                                             CONNEC........5600
C.....PREPARE NODE INCIDENCE LIST FOR MESH, IN.                          CONNEC........5700
      DO 5 II=1,N48                                                      CONNEC........5800
      III=II+(L-1)*N48                                                   CONNEC........5900
    5 IN(III)=IIN(II)                                                    CONNEC........6000
      IF(IABS(LL).EQ.L) GOTO 500                                         CONNEC........6100
      ERRCOD = 'INP-22-2'                                                CONNEC........6200
      INERR(1) = LL                                                      CONNEC........6300
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           CONNEC........6400
      RETURN                                                             CONNEC........6500
C                                                                        CONNEC........6600
C                                                                        CONNEC........6700
  500 M1=(L-1)*N48+1                                                     CONNEC........6800
      M8=M1+N48-1                                                        CONNEC........6900
      IF(KINCID.EQ.0) GOTO 1000                                          CONNEC........7000
      WRITE(K3,650) L,(IN(M),M=M1,M8)                                    CONNEC........7100
  650 FORMAT(11X,'ELEMENT',I9,5X,' NODES AT : ',6X,'CORNERS ',           CONNEC........7200
     1   5(1H*),8I9,1X,5(1H*))                                           CONNEC........7300
C                                                                        CONNEC........7400
 1000 CONTINUE                                                           CONNEC........7500
C                                                                        CONNEC........7600
C                                                                        CONNEC........7700
 5000 RETURN                                                             CONNEC........7800
      END                                                                CONNEC........7900
C                                                                        CONNEC........8000
C     SUBROUTINE        D  I  M  W  R  K           SUTRA VERSION 2D3D.1  DIMWRK.........100
C                                                                        DIMWRK.........200
C *** PURPOSE :                                                          DIMWRK.........300
C ***  TO RETURN DIMENSIONS FOR THE SOLVER WORK ARRAYS, WHICH DEPEND ON  DIMWRK.........400
C ***  THE PARTICULAR SOLVER CHOSEN.                                     DIMWRK.........500
C                                                                        DIMWRK.........600
      SUBROUTINE DIMWRK(KSOLVR, NSAVE, NN, NELT, NWI, NWF)               DIMWRK.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                DIMWRK.........800
C                                                                        DIMWRK.........900
C.....COMPUTE SOLVER WORK ARRAY DIMENSIONS                               DIMWRK........1000
      IF (KSOLVR.EQ.1) THEN                                              DIMWRK........1100
         NL = (NELT + NN)/2                                              DIMWRK........1200
         NWI = 11 + 2*NL                                                 DIMWRK........1300
         NWF = NL + 5*NN + 1                                             DIMWRK........1400
      ELSE IF (KSOLVR.EQ.2) THEN                                         DIMWRK........1500
         NWI = 31 + 2*NELT                                               DIMWRK........1600
         NWF = 2 + NN*(NSAVE + 7) + NSAVE*(NSAVE + 3) + (NELT - NN)      DIMWRK........1700
      ELSE IF (KSOLVR.EQ.3) THEN                                         DIMWRK........1800
         NWI = 11 + 2*NELT                                               DIMWRK........1900
         NWF = 1 + 3*NN*(NSAVE + 1) + 7*NN + NSAVE + (NELT - NN)         DIMWRK........2000
      END IF                                                             DIMWRK........2100
C                                                                        DIMWRK........2200
      RETURN                                                             DIMWRK........2300
      END                                                                DIMWRK........2400
C                                                                        DIMWRK........2500
C     SUBROUTINE        D  I  S  P  R  3           SUTRA VERSION 2D3D.1  DISPR3.........100
C                                                                        DISPR3.........200
C *** PURPOSE :                                                          DISPR3.........300
C ***  TO COMPUTE THE COMPONENTS OF THE 3D DISPERSION TENSOR IN          DISPR3.........400
C ***  X,Y,Z-COORDINATES USING AN AD HOC, 3D ANISOTROPIC DISPERSION      DISPR3.........500
C ***  MODEL.                                                            DISPR3.........600
C                                                                        DISPR3.........700
      SUBROUTINE DISPR3(VX,VY,VZ,VMAG,ANG1,ANG2,ANG3,ALMAX,ALMID,ALMIN,  DISPR3.........800
     1   ATMAX,ATMID,ATMIN,DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ)          DISPR3.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                DISPR3........1000
      LOGICAL LISO,TISO                                                  DISPR3........1100
      DIMENSION AL(3),AT(3),VN(3),UN(3),WN(3)                            DISPR3........1200
      DIMENSION J(3)                                                     DISPR3........1300
C                                                                        DISPR3........1400
C.....HANDLE CASE OF ZERO VELOCITY.  (THIS CASE IS ALREADY HANDLED       DISPR3........1500
C        BY SUBROUTINE ELEMN3.  THE BLOCK IF STATEMENT BELOW CAN BE      DISPR3........1600
C        UNCOMMENTED IF NEEDED IN THE FUTURE.)                           DISPR3........1700
C     IF (VMAG.EQ.0D0) THEN                                              DISPR3........1800
C        DXX = 0D0                                                       DISPR3........1900
C        DXY = 0D0                                                       DISPR3........2000
C        DXZ = 0D0                                                       DISPR3........2100
C        DYX = 0D0                                                       DISPR3........2200
C        DYY = 0D0                                                       DISPR3........2300
C        DYZ = 0D0                                                       DISPR3........2400
C        DZX = 0D0                                                       DISPR3........2500
C        DZY = 0D0                                                       DISPR3........2600
C        DZZ = 0D0                                                       DISPR3........2700
C        RETURN                                                          DISPR3........2800
C     END IF                                                             DISPR3........2900
C                                                                        DISPR3........3000
C.....SET TOLERANCES USED TO DETERMINE WHETHER CERTAIN DEGENERATE        DISPR3........3100
C        CONDITIONS ARE TRUE:                                            DISPR3........3200
C        TOLISO -- IS DISPERSION ESSENTIALLY ISOTROPIC?                  DISPR3........3300
C        TOLVRT -- IS FLOW ESSENTIALLY VERTICAL?                         DISPR3........3400
C        TOLCIR -- IS SLICING ELLIPSE ESSENTIALLY A CIRCLE?              DISPR3........3500
      TOLISO = 1D-7                                                      DISPR3........3600
      TOLVRT = 1D-7                                                      DISPR3........3700
      TOLCIR = 9.999999D-1                                               DISPR3........3800
C                                                                        DISPR3........3900
C.....NORMALIZE THE VELOCITY VECTOR.                                     DISPR3........4000
      VNX = VX/VMAG                                                      DISPR3........4100
      VNY = VY/VMAG                                                      DISPR3........4200
      VNZ = VZ/VMAG                                                      DISPR3........4300
C                                                                        DISPR3........4400
C.....DETERMINE WHETHER LONGITUDINAL DISPERSION IS ESSENTIALLY           DISPR3........4500
C        ISOTROPIC.                                                      DISPR3........4600
      AL(1) = ALMAX                                                      DISPR3........4700
      AL(2) = ALMID                                                      DISPR3........4800
      AL(3) = ALMIN                                                      DISPR3........4900
      ALMXVL = MAXVAL(AL)                                                DISPR3........5000
      ALMNVL = MINVAL(AL)                                                DISPR3........5100
      IF (ALMXVL.EQ.0D0) THEN                                            DISPR3........5200
         LISO = .TRUE.                                                   DISPR3........5300
      ELSE                                                               DISPR3........5400
         LISO = ((ALMXVL - ALMNVL)/ALMXVL.LT.TOLISO)                     DISPR3........5500
      END IF                                                             DISPR3........5600
C                                                                        DISPR3........5700
C.....COMPUTE THE LONGITUDINAL DISPERSION COEFFICIENT.                   DISPR3........5800
      IF (LISO) THEN                                                     DISPR3........5900
C........ISOTROPIC CASE.                                                 DISPR3........6000
         DL = ALMAX*VMAG                                                 DISPR3........6100
      ELSE                                                               DISPR3........6200
C........ANISOTROPIC CASE.                                               DISPR3........6300
C........ROTATE V TO "MAX-MID-MIN" COORDINATES.                          DISPR3........6400
         CALL ROTMAT(ANG1,ANG2,ANG3,G11,G12,G13,G21,G22,G23,             DISPR3........6500
     1      G31,G32,G33)                                                 DISPR3........6600
         CALL ROTATE(G11,G21,G31,G12,G22,G32,G13,G23,G33,                DISPR3........6700
     1      VNX,VNY,VNZ,VNXX,VNYY,VNZZ)                                  DISPR3........6800
C........EVALUATE DL FROM THE LONGITUDINAL DISPERSIVITY ELLIPSOID.       DISPR3........6900
         DL = VMAG/(VNXX*VNXX/ALMAX+VNYY*VNYY/ALMID+VNZZ*VNZZ/ALMIN)     DISPR3........7000
      END IF                                                             DISPR3........7100
C                                                                        DISPR3........7200
C.....DETERMINE WHETHER TRANSVERSE DISPERSION IS ESSENTIALLY             DISPR3........7300
C        ISOTROPIC.                                                      DISPR3........7400
      AT(1) = ATMAX                                                      DISPR3........7500
      AT(2) = ATMID                                                      DISPR3........7600
      AT(3) = ATMIN                                                      DISPR3........7700
      ATMXVL = MAXVAL(AT)                                                DISPR3........7800
      ATMNVL = MINVAL(AT)                                                DISPR3........7900
      IF (ATMXVL.EQ.0D0) THEN                                            DISPR3........8000
         TISO = .TRUE.                                                   DISPR3........8100
      ELSE                                                               DISPR3........8200
         TISO = ((ATMXVL - ATMNVL)/ATMXVL.LT.TOLISO)                     DISPR3........8300
      END IF                                                             DISPR3........8400
C                                                                        DISPR3........8500
C.....COMPUTE THE TRANSVERSE DISPERSION DIRECTIONS AND COEFFICIENTS.     DISPR3........8600
      IF (TISO) THEN                                                     DISPR3........8700
C........ISOTROPIC CASE.                                                 DISPR3........8800
         TERM = 1D0 - VNZ*VNZ                                            DISPR3........8900
         IF (TERM.LT.TOLVRT) THEN                                        DISPR3........9000
C...........FLOW IS ESSENTIALLY IN Z-DIRECTION (VERTICAL)                DISPR3........9100
            UNX = 1D0                                                    DISPR3........9200
            UNY = 0D0                                                    DISPR3........9300
            UNZ = 0D0                                                    DISPR3........9400
            WNX = 0D0                                                    DISPR3........9500
            WNY = 1D0                                                    DISPR3........9600
            WNZ = 0D0                                                    DISPR3........9700
         ELSE                                                            DISPR3........9800
C...........FLOW IS NOT IN Z-DIRECTION (NOT VERTICAL)                    DISPR3........9900
            TERMH = DSQRT(TERM)                                          DISPR3.......10000
            UNX = -VNY/TERMH                                             DISPR3.......10100
            UNY = VNX/TERMH                                              DISPR3.......10200
            UNZ = 0D0                                                    DISPR3.......10300
            WNX = -VNZ*UNY                                               DISPR3.......10400
            WNY = VNZ*UNX                                                DISPR3.......10500
            WNZ = TERMH                                                  DISPR3.......10600
         END IF                                                          DISPR3.......10700
         AT1 = ATMAX                                                     DISPR3.......10800
         AT2 = AT1                                                       DISPR3.......10900
      ELSE                                                               DISPR3.......11000
C........ANISOTROPIC CASE.                                               DISPR3.......11100
C........ROTATE V TO "MAX-MID-MIN" COORDINATES, IF NOT DONE PREVIOUSLY.  DISPR3.......11200
         IF (LISO) THEN                                                  DISPR3.......11300
            CALL ROTMAT(ANG1,ANG2,ANG3,G11,G12,G13,G21,G22,G23,          DISPR3.......11400
     1         G31,G32,G33)                                              DISPR3.......11500
            CALL ROTATE(G11,G21,G31,G12,G22,G32,G13,G23,G33,             DISPR3.......11600
     1         VNX,VNY,VNZ,VNXX,VNYY,VNZZ)                               DISPR3.......11700
         END IF                                                          DISPR3.......11800
C........TRANSPOSE AXES SO THAT THE LONGEST AXIS OF THE TRANSVERSE       DISPR3.......11900
C           DISPERSIVITY ELLIPSOID IS "MAX", THE SECOND LONGEST IS       DISPR3.......12000
C           "MID", AND THE SHORTEST IS "MIN".                            DISPR3.......12100
         J(1:1) = MAXLOC(AT)                                             DISPR3.......12200
         J(3:3) = MINLOC(AT)                                             DISPR3.......12300
         J(2) = 6 - J(1) - J(3)                                          DISPR3.......12400
         VN(1) = VNXX                                                    DISPR3.......12500
         VN(2) = VNYY                                                    DISPR3.......12600
         VN(3) = VNZZ                                                    DISPR3.......12700
         VNTXX = VN(J(1))                                                DISPR3.......12800
         VNTYY = VN(J(2))                                                DISPR3.......12900
         VNTZZ = VN(J(3))                                                DISPR3.......13000
         A2 = AT(J(1))                                                   DISPR3.......13100
         B2 = AT(J(2))                                                   DISPR3.......13200
         C2 = AT(J(3))                                                   DISPR3.......13300
C........APPLY THE BIOT-FRESNEL CONSTRUCTION TO THE TRANSVERSE           DISPR3.......13400
C           DISPERSIVITY ELLIPSOID.                                      DISPR3.......13500
         A2B2 = A2*B2                                                    DISPR3.......13600
         A2C2 = A2*C2                                                    DISPR3.......13700
         B2C2 = B2*C2                                                    DISPR3.......13800
         COS2AV = (A2C2 - B2C2)/(A2B2 - B2C2)                            DISPR3.......13900
         SIN2AV = 1D0 - COS2AV                                           DISPR3.......14000
         COSAV = DSQRT(COS2AV)                                           DISPR3.......14100
         SINAV = DSQRT(SIN2AV)                                           DISPR3.......14200
         TERM1 = COSAV*VNTXX                                             DISPR3.......14300
         TERM2 = SINAV*VNTZZ                                             DISPR3.......14400
         OA1V = TERM1 + TERM2                                            DISPR3.......14500
         OA2V = TERM1 - TERM2                                            DISPR3.......14600
         IF (MAX(DABS(OA1V),DABS(OA2V)).GT.TOLCIR) THEN                  DISPR3.......14700
C...........SLICING ELLIPSE IS ESSENTIALLY A CIRCLE                      DISPR3.......14800
            UNTXX = -VNTZZ                                               DISPR3.......14900
            UNTYY = 0D0                                                  DISPR3.......15000
            UNTZZ = VNTXX                                                DISPR3.......15100
            WNTXX = 0D0                                                  DISPR3.......15200
            WNTYY = 1D0                                                  DISPR3.......15300
            WNTZZ = 0D0                                                  DISPR3.......15400
            AT1 = B2                                                     DISPR3.......15500
            AT2 = B2                                                     DISPR3.......15600
         ELSE                                                            DISPR3.......15700
C...........SLICING ELLIPSE IS NOT A CIRCLE                              DISPR3.......15800
            RVJ1MG = 1D0/DSQRT(1D0 - OA1V*OA1V)                          DISPR3.......15900
            RVJ2MG = 1D0/DSQRT(1D0 - OA2V*OA2V)                          DISPR3.......16000
            RSUM = RVJ1MG + RVJ2MG                                       DISPR3.......16100
            RDIF = RVJ1MG - RVJ2MG                                       DISPR3.......16200
            OAUXX = COSAV*RSUM                                           DISPR3.......16300
            OAUZZ = SINAV*RDIF                                           DISPR3.......16400
            OAWXX = COSAV*RDIF                                           DISPR3.......16500
            OAWZZ = SINAV*RSUM                                           DISPR3.......16600
            OAUV = OAUXX*VNTXX + OAUZZ*VNTZZ                             DISPR3.......16700
            OAWV = OAWXX*VNTXX + OAWZZ*VNTZZ                             DISPR3.......16800
            OAUOAU = OAUXX*OAUXX + OAUZZ*OAUZZ                           DISPR3.......16900
            OAWOAW = OAWXX*OAWXX + OAWZZ*OAWZZ                           DISPR3.......17000
            UMTERM = OAUOAU - OAUV*OAUV                                  DISPR3.......17100
            WMTERM = OAWOAW - OAWV*OAWV                                  DISPR3.......17200
C...........COMPUTE THE LARGER OF U AND W DIRECTLY, THEN COMPUTE THE     DISPR3.......17300
C              OTHER BY CROSS-PRODUCT WITH V.                            DISPR3.......17400
            IF (UMTERM.GT.WMTERM) THEN                                   DISPR3.......17500
               RUMAGH = 1D0/DSQRT(UMTERM)                                DISPR3.......17600
               UNTXX = (OAUXX - OAUV*VNTXX)*RUMAGH                       DISPR3.......17700
               UNTYY = -OAUV*VNTYY*RUMAGH                                DISPR3.......17800
               UNTZZ = (OAUZZ - OAUV*VNTZZ)*RUMAGH                       DISPR3.......17900
               WNTXX = UNTYY*VNTZZ - UNTZZ*VNTYY                         DISPR3.......18000
               WNTYY = UNTZZ*VNTXX - UNTXX*VNTZZ                         DISPR3.......18100
               WNTZZ = UNTXX*VNTYY - UNTYY*VNTXX                         DISPR3.......18200
            ELSE                                                         DISPR3.......18300
               RWMAGH = 1D0/DSQRT(WMTERM)                                DISPR3.......18400
               WNTXX = (OAWXX - OAWV*VNTXX)*RWMAGH                       DISPR3.......18500
               WNTYY = -OAWV*VNTYY*RWMAGH                                DISPR3.......18600
               WNTZZ = (OAWZZ - OAWV*VNTZZ)*RWMAGH                       DISPR3.......18700
               UNTXX = WNTYY*VNTZZ - WNTZZ*VNTYY                         DISPR3.......18800
               UNTYY = WNTZZ*VNTXX - WNTXX*VNTZZ                         DISPR3.......18900
               UNTZZ = WNTXX*VNTYY - WNTYY*VNTXX                         DISPR3.......19000
            END IF                                                       DISPR3.......19100
            A2B2C2 = A2B2*C2                                             DISPR3.......19200
            DEN1 = B2C2*UNTXX*UNTXX+A2C2*UNTYY*UNTYY+A2B2*UNTZZ*UNTZZ    DISPR3.......19300
            DEN2 = B2C2*WNTXX*WNTXX+A2C2*WNTYY*WNTYY+A2B2*WNTZZ*WNTZZ    DISPR3.......19400
            AT1 = A2B2C2/DEN1                                            DISPR3.......19500
            AT2 = A2B2C2/DEN2                                            DISPR3.......19600
         END IF                                                          DISPR3.......19700
C........TRANSPOSE AXES BACK TO ORIGINAL "MAX-MID-MIN" AXES.             DISPR3.......19800
         UN(J(1)) = UNTXX                                                DISPR3.......19900
         UN(J(2)) = UNTYY                                                DISPR3.......20000
         UN(J(3)) = UNTZZ                                                DISPR3.......20100
         UNXX = UN(1)                                                    DISPR3.......20200
         UNYY = UN(2)                                                    DISPR3.......20300
         UNZZ = UN(3)                                                    DISPR3.......20400
         WN(J(1)) = WNTXX                                                DISPR3.......20500
         WN(J(2)) = WNTYY                                                DISPR3.......20600
         WN(J(3)) = WNTZZ                                                DISPR3.......20700
         WNXX = WN(1)                                                    DISPR3.......20800
         WNYY = WN(2)                                                    DISPR3.......20900
         WNZZ = WN(3)                                                    DISPR3.......21000
C........ROTATE THE TRANSVERSE DISPERSION DIRECTIONS FROM "MAX-MID-MIN"  DISPR3.......21100
C           COORDINATES TO X,Y,Z-COORDINATES.                            DISPR3.......21200
         CALL ROTATE(G11,G12,G13,G21,G22,G23,G31,G32,G33,UNXX,UNYY,UNZZ, DISPR3.......21300
     1      UNX,UNY,UNZ)                                                 DISPR3.......21400
         CALL ROTATE(G11,G12,G13,G21,G22,G23,G31,G32,G33,WNXX,WNYY,WNZZ, DISPR3.......21500
     1      WNX,WNY,WNZ)                                                 DISPR3.......21600
      END IF                                                             DISPR3.......21700
C.....COMPUTE TRANSVERSE DISPERSION COEFFICIENTS FROM DISPERSIVITIES     DISPR3.......21800
      DT1 = AT1*VMAG                                                     DISPR3.......21900
      DT2 = AT2*VMAG                                                     DISPR3.......22000
C                                                                        DISPR3.......22100
C.....ROTATE THE DISPERSION TENSOR FROM EIGENVECTOR COORDINATES TO       DISPR3.......22200
C     X,Y,Z-COORDINATES.                                                 DISPR3.......22300
      CALL TENSYM(DL,DT1,DT2,VNX,UNX,WNX,VNY,UNY,WNY,VNZ,UNZ,WNZ,        DISPR3.......22400
     1   DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ)                            DISPR3.......22500
C                                                                        DISPR3.......22600
      RETURN                                                             DISPR3.......22700
      END                                                                DISPR3.......22800
C                                                                        DISPR3.......22900
C     SUBROUTINE        E  L  E  M  N  2           SUTRA VERSION 2D3D.1  ELEMN2.........100
C                                                                        ELEMN2.........200
C *** PURPOSE :                                                          ELEMN2.........300
C ***  TO CONTROL AND CARRY OUT ALL CALCULATIONS FOR EACH ELEMENT BY     ELEMN2.........400
C ***  OBTAINING ELEMENT INFORMATION FROM THE BASIS FUNCTION ROUTINE,    ELEMN2.........500
C ***  CARRYING OUT GAUSSIAN INTEGRATION OF FINITE ELEMENT INTEGRALS,    ELEMN2.........600
C ***  AND ASSEMBLING RESULTS OF ELEMENTWISE INTEGRATIONS INTO           ELEMN2.........700
C ***  A GLOBAL MATRIX AND GLOBAL VECTOR FOR BOTH FLOW AND TRANSPORT     ELEMN2.........800
C ***  EQUATIONS. ALSO CALCULATES VELOCITY AT EACH ELEMENT CENTROID FOR  ELEMN2.........900
C ***  PRINTED OUTPUT. THIS SUBROUTINE HANDLES 2D CALCULATIONS ONLY;     ELEMN2........1000
C ***  3D CALCULATIONS ARE PERFORMED IN SUBROUTINE ELEMN3.               ELEMN2........1100
C                                                                        ELEMN2........1200
C                                                                        ELEMN2........1300
      SUBROUTINE ELEMN2(ML,IN,X,Y,THICK,PITER,UITER,RCIT,RCITM1,POR,     ELEMN2........1400
     1   ALMAX,ALMIN,ATMAX,ATMIN,PERMXX,PERMXY,PERMYX,PERMYY,PANGLE,     ELEMN2........1500
     2   VMAG,VANG,VOL,PMAT,PVEC,UMAT,UVEC,GXSI,GETA,PVEL,LREG,          ELEMN2........1600
     3   NBI27,MIOFF)                                                    ELEMN2........1700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                ELEMN2........1800
      CHARACTER*80 ERRCOD,CHERR(10),FNAME(0:7)                           ELEMN2........1900
      DIMENSION IN(NIN),X(NN),Y(NN),THICK(NN),PITER(NN),                 ELEMN2........2000
     1   UITER(NN),RCIT(NN),RCITM1(NN),POR(NN),PVEL(NN)                  ELEMN2........2100
      DIMENSION PERMXX(NE),PERMXY(NE),PERMYX(NE),PERMYY(NE),PANGLE(NE),  ELEMN2........2200
     1   ALMAX(NE),ALMIN(NE),ATMAX(NE),ATMIN(NE),VMAG(NE),VANG(NE),      ELEMN2........2300
     2   GXSI(NE,4),GETA(NE,4),LREG(NE)                                  ELEMN2........2400
      DIMENSION VOL(NN),PMAT(NELT,NCBI),PVEC(NNVEC),UMAT(NELT,NCBI),     ELEMN2........2500
     1   UVEC(NNVEC)                                                     ELEMN2........2600
      DIMENSION BFLOWE(8,8),DFLOWE(8),BTRANE(8,8),DTRANE(8,8),VOLE(8)    ELEMN2........2700
      DIMENSION F(4,4),W(4,4),DET(4),DFDXG(4,4),DFDYG(4,4),              ELEMN2........2800
     1   DWDXG(4,4),DWDYG(4,4)                                           ELEMN2........2900
      DIMENSION SWG(4),RHOG(4),VISCG(4),PORG(4),VXG(4),VYG(4),           ELEMN2........3000
     1   RELKG(4),RGXG(4),RGYG(4),VGMAG(4),THICKG(4)                     ELEMN2........3100
      DIMENSION RXXG(4),RXYG(4),RYXG(4),RYYG(4)                          ELEMN2........3200
      DIMENSION BXXG(4),BXYG(4),BYXG(4),BYYG(4),EXG(4),EYG(4)            ELEMN2........3300
      DIMENSION GXLOC(4),GYLOC(4)                                        ELEMN2........3400
      DIMENSION NBI27(NBIX),MIOFF(27)                                    ELEMN2........3500
      DIMENSION INERR(10), RLERR(10)                                     ELEMN2........3600
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  ELEMN2........3700
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             ELEMN2........3800
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              ELEMN2........3900
     1   NSOP,NSOU,NBCN                                                  ELEMN2........4000
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   ELEMN2........4100
      COMMON /DIMX2/ NELTA,NNVEC,NDIMJA                                  ELEMN2........4200
      COMMON /FNAMES/ FNAME                                              ELEMN2........4300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        ELEMN2........4400
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  ELEMN2........4500
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      ELEMN2........4600
      COMMON /JCOLS/ NCOLPR,LCOLPR,NCOLS5,NCOLS6,J5COL,J6COL             ELEMN2........4700
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     ELEMN2........4800
     1   KSCRN,KPAUSE                                                    ELEMN2........4900
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      ELEMN2........5000
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        ELEMN2........5100
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           ELEMN2........5200
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       ELEMN2........5300
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  ELEMN2........5400
      DATA GLOC/0.577350269189626D0/                                     ELEMN2........5500
      DATA INTIM/0/,ISTOP/0/,GXLOC/-1.D0,1.D0,1.D0,-1.D0/,               ELEMN2........5600
     1   GYLOC/-1.D0,-1.D0,1.D0,1.D0/                                    ELEMN2........5700
      SAVE GLOC,INTIM,ISTOP,GXLOC,GYLOC                                  ELEMN2........5800
C                                                                        ELEMN2........5900
C.....DECIDE WHETHER TO CALCULATE CENTROID VELOCITIES ON THIS CALL       ELEMN2........6000
      IVCALC = 0                                                         ELEMN2........6100
      JVCALC = 0                                                         ELEMN2........6200
      IF ((ML.NE.2).AND.(ITER.EQ.1)) IVCALC = 1                          ELEMN2........6300
      IF (IT.EQ.1) IVCALC = 1                                            ELEMN2........6400
      IF ((KVEL.EQ.1).OR.(K6.NE.-1)) JVCALC = 1                          ELEMN2........6500
      KVCALC = IVCALC + JVCALC                                           ELEMN2........6600
C                                                                        ELEMN2........6700
C.....ON FIRST TIME STEP, PREPARE GRAVITY VECTOR COMPONENTS,             ELEMN2........6800
C        GXSI AND GETA, FOR CONSISTENT VELOCITIES,                       ELEMN2........6900
C        AND CHECK ELEMENT SHAPES                                        ELEMN2........7000
      IF(INTIM) 100,100,2000                                             ELEMN2........7100
  100 INTIM=1                                                            ELEMN2........7200
C.....LOOP THROUGH ALL ELEMENTS TO OBTAIN THE JACOBIAN                   ELEMN2........7300
C        AT EACH OF THE FOUR NODES IN EACH ELEMENT                       ELEMN2........7400
      DO 1000 L=1,NE                                                     ELEMN2........7500
       DO 500 IL=1,4                                                     ELEMN2........7600
        XLOC=GXLOC(IL)                                                   ELEMN2........7700
        YLOC=GYLOC(IL)                                                   ELEMN2........7800
        CALL BASIS2(0000,L,XLOC,YLOC,IN,X,Y,F(1,IL),W(1,IL),DET(IL),     ELEMN2........7900
     1     DFDXG(1,IL),DFDYG(1,IL),DWDXG(1,IL),DWDYG(1,IL),              ELEMN2........8000
     2     PITER,UITER,PVEL,POR,THICK,THICKG(IL),VXG(IL),VYG(IL),        ELEMN2........8100
     3     SWG(IL),RHOG(IL),VISCG(IL),PORG(IL),VGMAG(IL),RELKG(IL),      ELEMN2........8200
     4     PERMXX,PERMXY,PERMYX,PERMYY,CJ11,CJ12,CJ21,CJ22,              ELEMN2........8300
     5     GXSI,GETA,RCIT,RCITM1,RGXG(IL),RGYG(IL),LREG)                 ELEMN2........8400
        GXSI(L,IL)=CJ11*GRAVX+CJ12*GRAVY                                 ELEMN2........8500
        GETA(L,IL)=CJ21*GRAVX+CJ22*GRAVY                                 ELEMN2........8600
C.....CHECK FOR NEGATIVE- OR ZERO-AREA ERRORS IN ELEMENT SHAPES          ELEMN2........8700
        IF(DET(IL)) 200,200,500                                          ELEMN2........8800
  200   ISTOP=ISTOP+1                                                    ELEMN2........8900
        WRITE(K3,400) IN((L-1)*4+IL),L,DET(IL)                           ELEMN2........9000
        WRITE(K00,401) IN((L-1)*4+IL),L,DET(IL)                          ELEMN2........9100
  400   FORMAT(11X,'THE DETERMINANT OF THE JACOBIAN AT NODE ',I9,        ELEMN2........9200
     1     ' IN ELEMENT ',I9,' IS NEGATIVE OR ZERO, ',1PE15.7)           ELEMN2........9300
  401   FORMAT(1X,'THE DETERMINANT OF THE JACOBIAN AT NODE ',I9,         ELEMN2........9400
     1     ' IN ELEMENT ',I9,' IS NEGATIVE OR ZERO, ',1PE15.7)           ELEMN2........9500
  500  CONTINUE                                                          ELEMN2........9600
 1000 CONTINUE                                                           ELEMN2........9700
C                                                                        ELEMN2........9800
      IF(ISTOP.EQ.0) GOTO 2000                                           ELEMN2........9900
      ERRCOD = 'INP-14B,22-1'                                            ELEMN2.......10000
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           ELEMN2.......10100
      RETURN                                                             ELEMN2.......10200
C                                                                        ELEMN2.......10300
C.....LOOP THROUGH ALL ELEMENTS TO CARRY OUT SPATIAL INTEGRATION         ELEMN2.......10400
C        OF FLUX TERMS IN P AND/OR U EQUATIONS                           ELEMN2.......10500
 2000 IF(IUNSAT.NE.0) IUNSAT=2                                           ELEMN2.......10600
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2.......10700
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2.......10800
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2.......10900
      DO 9999 L=1,NE                                                     ELEMN2.......11000
       XIX=-1.D0                                                         ELEMN2.......11100
       YIY=-1.D0                                                         ELEMN2.......11200
       KG=0                                                              ELEMN2.......11300
C.....OBTAIN BASIS FUNCTION AND RELATED INFORMATION AT EACH OF           ELEMN2.......11400
C        FOUR GAUSS POINTS IN THE ELEMENT                                ELEMN2.......11500
       DO 2200 IYL=1,2                                                   ELEMN2.......11600
        DO 2100 IXL=1,2                                                  ELEMN2.......11700
         KG=KG+1                                                         ELEMN2.......11800
         XLOC=XIX*GLOC                                                   ELEMN2.......11900
         YLOC=YIY*GLOC                                                   ELEMN2.......12000
         CALL BASIS2(0001,L,XLOC,YLOC,IN,X,Y,F(1,KG),W(1,KG),DET(KG),    ELEMN2.......12100
     1      DFDXG(1,KG),DFDYG(1,KG),DWDXG(1,KG),DWDYG(1,KG),             ELEMN2.......12200
     2      PITER,UITER,PVEL,POR,THICK,THICKG(KG),VXG(KG),VYG(KG),       ELEMN2.......12300
     3      SWG(KG),RHOG(KG),VISCG(KG),PORG(KG),VGMAG(KG),RELKG(KG),     ELEMN2.......12400
     4      PERMXX,PERMXY,PERMYX,PERMYY,CJ11,CJ12,CJ21,CJ22,             ELEMN2.......12500
     5      GXSI,GETA,RCIT,RCITM1,RGXG(KG),RGYG(KG),LREG)                ELEMN2.......12600
 2100    XIX=-XIX                                                        ELEMN2.......12700
 2200   YIY=-YIY                                                         ELEMN2.......12800
C                                                                        ELEMN2.......12900
C.....CALCULATE VELOCITY AT ELEMENT CENTROID WHEN REQUIRED               ELEMN2.......13000
       IF(KVCALC-2) 3000,2300,3000                                       ELEMN2.......13100
 2300  AXSUM=0.0D0                                                       ELEMN2.......13200
       AYSUM=0.0D0                                                       ELEMN2.......13300
       DO 2400 KG=1,4                                                    ELEMN2.......13400
        AXSUM=AXSUM+VXG(KG)                                              ELEMN2.......13500
 2400   AYSUM=AYSUM+VYG(KG)                                              ELEMN2.......13600
       VMAG(L)=DSQRT(AXSUM*AXSUM+AYSUM*AYSUM)                            ELEMN2.......13700
       IF (VMAG(L).NE.0D0) THEN                                          ELEMN2.......13800
          VMAG(L)=VMAG(L)*2.5D-1                                         ELEMN2.......13900
          VANG(L)=DATAN2(AYSUM,AXSUM)*5.729577951308232D+1               ELEMN2.......14000
       ELSE                                                              ELEMN2.......14100
          VANG(L)=0D0                                                    ELEMN2.......14200
       END IF                                                            ELEMN2.......14300
C                                                                        ELEMN2.......14400
C.....INCLUDE MESH THICKNESS IN NUMERICAL INTEGRATION                    ELEMN2.......14500
 3000  DO 3300 KG=1,4                                                    ELEMN2.......14600
 3300   DET(KG)=THICKG(KG)*DET(KG)                                       ELEMN2.......14700
C                                                                        ELEMN2.......14800
C.....CALCULATE PARAMETERS FOR FLUID MASS BALANCE AT GAUSS POINTS        ELEMN2.......14900
       IF(ML-1) 3400,3400,6100                                           ELEMN2.......15000
 3400  SWTEST=0.D0                                                       ELEMN2.......15100
       DO 4000 KG=1,4                                                    ELEMN2.......15200
        SWTEST=SWTEST+SWG(KG)                                            ELEMN2.......15300
        ROMG=RHOG(KG)*RELKG(KG)/VISCG(KG)                                ELEMN2.......15400
        RXXG(KG)=PERMXX(L)*ROMG                                          ELEMN2.......15500
        RXYG(KG)=PERMXY(L)*ROMG                                          ELEMN2.......15600
        RYXG(KG)=PERMYX(L)*ROMG                                          ELEMN2.......15700
        RYYG(KG)=PERMYY(L)*ROMG                                          ELEMN2.......15800
 4000   CONTINUE                                                         ELEMN2.......15900
C                                                                        ELEMN2.......16000
C.....INTEGRATE FLUID MASS BALANCE IN AN UNSATURATED ELEMENT             ELEMN2.......16100
C        USING ASYMMETRIC WEIGHTING FUNCTIONS                            ELEMN2.......16200
       IF(UP.LE.1.0D-6) GOTO 5200                                        ELEMN2.......16300
       IF(SWTEST-3.999D0) 4200,5200,5200                                 ELEMN2.......16400
 4200  DO 4300 I=1,4                                                     ELEMN2.......16500
        VOLE(I) = 0.D0                                                   ELEMN2.......16600
        DFLOWE(I) = 0.D0                                                 ELEMN2.......16700
        DO 4300 J=1,4                                                    ELEMN2.......16800
         BFLOWE(I,J) = 0.D0                                              ELEMN2.......16900
 4300  CONTINUE                                                          ELEMN2.......17000
       DO 5000 KG=1,4                                                    ELEMN2.......17100
        RXXGD = RXXG(KG)*DET(KG)                                         ELEMN2.......17200
        RXYGD = RXYG(KG)*DET(KG)                                         ELEMN2.......17300
        RYXGD = RYXG(KG)*DET(KG)                                         ELEMN2.......17400
        RYYGD = RYYG(KG)*DET(KG)                                         ELEMN2.......17500
        RDRX = RXXGD*RGXG(KG) + RXYGD*RGYG(KG)                           ELEMN2.......17600
        RDRY = RYXGD*RGXG(KG) + RYYGD*RGYG(KG)                           ELEMN2.......17700
        DO 4400 I=1,4                                                    ELEMN2.......17800
         VOLE(I) = VOLE(I) + F(I,KG)*DET(KG)                             ELEMN2.......17900
         DFLOWE(I) = DFLOWE(I) + RDRX*DWDXG(I,KG) + RDRY*DWDYG(I,KG)     ELEMN2.......18000
 4400   CONTINUE                                                         ELEMN2.......18100
        DO 5000 J=1,4                                                    ELEMN2.......18200
         RDDFJX = RXXGD*DFDXG(J,KG) + RXYGD*DFDYG(J,KG)                  ELEMN2.......18300
         RDDFJY = RYXGD*DFDXG(J,KG) + RYYGD*DFDYG(J,KG)                  ELEMN2.......18400
         DO 5000 I=1,4                                                   ELEMN2.......18500
          BFLOWE(I,J) = BFLOWE(I,J) + DWDXG(I,KG)*RDDFJX                 ELEMN2.......18600
     1                  + DWDYG(I,KG)*RDDFJY                             ELEMN2.......18700
 5000  CONTINUE                                                          ELEMN2.......18800
       GOTO 6050                                                         ELEMN2.......18900
C                                                                        ELEMN2.......19000
C.....INTEGRATE FLUID MASS BALANCE IN A SATURATED OR UNSATURATED         ELEMN2.......19100
C        ELEMENT USING SYMMETRIC WEIGHTING FUNCTIONS                     ELEMN2.......19200
 5200  DO 5300 I=1,4                                                     ELEMN2.......19300
        VOLE(I) = 0.D0                                                   ELEMN2.......19400
        DFLOWE(I) = 0.D0                                                 ELEMN2.......19500
        DO 5300 J=1,4                                                    ELEMN2.......19600
         BFLOWE(I,J) = 0.D0                                              ELEMN2.......19700
 5300  CONTINUE                                                          ELEMN2.......19800
       DO 6000 KG=1,4                                                    ELEMN2.......19900
        RXXGD = RXXG(KG)*DET(KG)                                         ELEMN2.......20000
        RXYGD = RXYG(KG)*DET(KG)                                         ELEMN2.......20100
        RYXGD = RYXG(KG)*DET(KG)                                         ELEMN2.......20200
        RYYGD = RYYG(KG)*DET(KG)                                         ELEMN2.......20300
        RDRX = RXXGD*RGXG(KG) + RXYGD*RGYG(KG)                           ELEMN2.......20400
        RDRY = RYXGD*RGXG(KG) + RYYGD*RGYG(KG)                           ELEMN2.......20500
        DO 5400 I=1,4                                                    ELEMN2.......20600
         VOLE(I) = VOLE(I) + F(I,KG)*DET(KG)                             ELEMN2.......20700
         DFLOWE(I) = DFLOWE(I) + RDRX*DFDXG(I,KG) + RDRY*DFDYG(I,KG)     ELEMN2.......20800
 5400   CONTINUE                                                         ELEMN2.......20900
        DO 6000 J=1,4                                                    ELEMN2.......21000
         RDDFJX = RXXGD*DFDXG(J,KG) + RXYGD*DFDYG(J,KG)                  ELEMN2.......21100
         RDDFJY = RYXGD*DFDXG(J,KG) + RYYGD*DFDYG(J,KG)                  ELEMN2.......21200
         DO 6000 I=1,4                                                   ELEMN2.......21300
          BFLOWE(I,J) = BFLOWE(I,J) + DFDXG(I,KG)*RDDFJX                 ELEMN2.......21400
     1                  + DFDYG(I,KG)*RDDFJY                             ELEMN2.......21500
 6000  CONTINUE                                                          ELEMN2.......21600
 6050  CONTINUE                                                          ELEMN2.......21700
       IF(ML-1) 6100,9000,6100                                           ELEMN2.......21800
 6100  IF(NOUMAT.EQ.1) GOTO 9000                                         ELEMN2.......21900
C                                                                        ELEMN2.......22000
C                                                                        ELEMN2.......22100
C.....CALCULATE PARAMETERS FOR ENERGY BALANCE OR SOLUTE MASS BALANCE     ELEMN2.......22200
C        AT GAUSS POINTS                                                 ELEMN2.......22300
       DO 7000 KG=1,4                                                    ELEMN2.......22400
        ESWG=PORG(KG)*SWG(KG)                                            ELEMN2.......22500
        RHOCWG=RHOG(KG)*CW                                               ELEMN2.......22600
        ESRCG=ESWG*RHOCWG                                                ELEMN2.......22700
        IF(VGMAG(KG)) 6300,6300,6600                                     ELEMN2.......22800
 6300   EXG(KG)=0.0D0                                                    ELEMN2.......22900
        EYG(KG)=0.0D0                                                    ELEMN2.......23000
        DXXG=0.0D0                                                       ELEMN2.......23100
        DXYG=0.0D0                                                       ELEMN2.......23200
        DYXG=0.0D0                                                       ELEMN2.......23300
        DYYG=0.0D0                                                       ELEMN2.......23400
        GOTO 6900                                                        ELEMN2.......23500
 6600   EXG(KG)=ESRCG*VXG(KG)                                            ELEMN2.......23600
        EYG(KG)=ESRCG*VYG(KG)                                            ELEMN2.......23700
C                                                                        ELEMN2.......23800
C.....DISPERSIVITY MODEL FOR 2D ANISOTROPIC MEDIA                        ELEMN2.......23900
C        WITH PRINCIPAL DISPERSIVITIES ALMAX, ALMIN, ATMAX, AND ATMIN    ELEMN2.......24000
        VANGG=1.570796327D0                                              ELEMN2.......24100
        IF(VXG(KG)*VXG(KG).GT.0.D0) VANGG=DATAN(VYG(KG)/VXG(KG))         ELEMN2.......24200
        VKANGG=VANGG-PANGLE(L)                                           ELEMN2.......24300
        DCO=DCOS(VKANGG)                                                 ELEMN2.......24400
        DSI=DSIN(VKANGG)                                                 ELEMN2.......24500
C.....EFFECTIVE LONGITUDINAL DISPERSIVITY IN FLOW DIRECTION, ALEFF       ELEMN2.......24600
        ALEFF=0.0D0                                                      ELEMN2.......24700
        IF(ALMAX(L)+ALMIN(L)) 6800,6800,6700                             ELEMN2.......24800
 6700   ALEFF=ALMAX(L)*ALMIN(L)/(ALMIN(L)*DCO*DCO+ALMAX(L)*DSI*DSI)      ELEMN2.......24900
 6800   DLG=ALEFF*VGMAG(KG)                                              ELEMN2.......25000
C.....EFFECTIVE TRANSVERSE DISPERSIVITY IN FLOW DIRECTION, ATEFF.        ELEMN2.......25100
C        NOTE THAT, STARTING WITH SUTRA VERSION 2D3D.1, ATMAX AND ATMIN  ELEMN2.........100
C        HAVE EXCHANGED IDENTITIES TO MAKE THE 2D DISPERSION MODEL       ELEMN2.........200
C        CONCEPTUALLY CONSISTENT WITH THE 3D MODEL.                      ELEMN2.........300
        ATEFF=0.0D0                                                      ELEMN2.........400
        IF(ATMAX(L)+ATMIN(L)) 6860,6860,6840                             ELEMN2.........500
 6840   ATEFF=ATMAX(L)*ATMIN(L)/(ATMAX(L)*DCO*DCO+ATMIN(L)*DSI*DSI)      ELEMN2.........600
 6860   DTG=ATEFF*VGMAG(KG)                                              ELEMN2.........700
C                                                                        ELEMN2.........800
        VXVG=VXG(KG)/VGMAG(KG)                                           ELEMN2.........900
        VYVG=VYG(KG)/VGMAG(KG)                                           ELEMN2........1000
        VXVG2=VXVG*VXVG                                                  ELEMN2........1100
        VYVG2=VYVG*VYVG                                                  ELEMN2........1200
C.....DISPERSION TENSOR                                                  ELEMN2........1300
        DXXG=DLG*VXVG2+DTG*VYVG2                                         ELEMN2........1400
        DYYG=DTG*VXVG2+DLG*VYVG2                                         ELEMN2........1500
        DXYG=(DLG-DTG)*VXVG*VYVG                                         ELEMN2........1600
        DYXG=DXYG                                                        ELEMN2........1700
C                                                                        ELEMN2........1800
C.....IN-PARALLEL CONDUCTIVITIES (DIFFUSIVITIES) FORMULA                 ELEMN2........1900
 6900   IF (ME.EQ.1) THEN                                                ELEMN2........2000
C..........FOR ENERGY TRANSPORT:                                         ELEMN2........2100
           ESE = ESWG*SIGMAW + (1D0-PORG(KG))*SIGMAS                     ELEMN2........2200
        ELSE                                                             ELEMN2........2300
C..........FOR SOLUTE TRANSPORT:                                         ELEMN2........2400
           ESE = ESRCG*SIGMAW                                            ELEMN2........2500
        END IF                                                           ELEMN2........2600
C.....ADD DIFFUSION AND DISPERSION TERMS TO TOTAL DISPERSION TENSOR      ELEMN2........2700
        BXXG(KG)=ESRCG*DXXG+ESE                                          ELEMN2........2800
        BXYG(KG)=ESRCG*DXYG                                              ELEMN2........2900
        BYXG(KG)=ESRCG*DYXG                                              ELEMN2........3000
 7000   BYYG(KG)=ESRCG*DYYG+ESE                                          ELEMN2........3100
C                                                                        ELEMN2........3200
C.....INTEGRATE SOLUTE MASS BALANCE OR ENERGY BALANCE                    ELEMN2........3300
C        USING SYMMETRIC WEIGHTING FUNCTIONS FOR DISPERSION TERM AND     ELEMN2........3400
C        USING EITHER SYMMETRIC OR ASYMMETRIC WEIGHTING FUNCTIONS        ELEMN2........3500
C        FOR ADVECTION TERM                                              ELEMN2........3600
         DO 7400 I=1,4                                                   ELEMN2........3700
         DO 7400 J=1,4                                                   ELEMN2........3800
            BTRANE(I,J) = 0.D0                                           ELEMN2........3900
            DTRANE(I,J) = 0.D0                                           ELEMN2........4000
 7400    CONTINUE                                                        ELEMN2........4100
         DO 8000 KG=1,4                                                  ELEMN2........4200
            BXXGD = BXXG(KG)*DET(KG)                                     ELEMN2........4300
            BXYGD = BXYG(KG)*DET(KG)                                     ELEMN2........4400
            BYXGD = BYXG(KG)*DET(KG)                                     ELEMN2........4500
            BYYGD = BYYG(KG)*DET(KG)                                     ELEMN2........4600
            EXGD = EXG(KG)*DET(KG)                                       ELEMN2........4700
            EYGD = EYG(KG)*DET(KG)                                       ELEMN2........4800
            DO 8000 J=1,4                                                ELEMN2........4900
               BDDFJX = BXXGD*DFDXG(J,KG) + BXYGD*DFDYG(J,KG)            ELEMN2........5000
               BDDFJY = BYXGD*DFDXG(J,KG) + BYYGD*DFDYG(J,KG)            ELEMN2........5100
               EDDFJ = EXGD*DFDXG(J,KG) + EYGD*DFDYG(J,KG)               ELEMN2........5200
               DO 8000 I=1,4                                             ELEMN2........5300
                  BTRANE(I,J) = BTRANE(I,J) + DFDXG(I,KG)*BDDFJX         ELEMN2........5400
     1               + DFDYG(I,KG)*BDDFJY                                ELEMN2........5500
                  DTRANE(I,J) = DTRANE(I,J) + EDDFJ*W(I,KG)              ELEMN2........5600
 8000    CONTINUE                                                        ELEMN2........5700
 9000  CONTINUE                                                          ELEMN2........5800
C                                                                        ELEMN2........5900
C                                                                        ELEMN2........6000
C.....SEND RESULTS OF INTEGRATIONS FOR THIS ELEMENT                      ELEMN2........6100
C        TO GLOBAL ASSEMBLY ROUTINE:                                     ELEMN2........6200
C        GLOBAN -- SUTRA'S ORIGINAL BANDED FORMAT                        ELEMN2........6300
C        GLOTRI -- SLAP TRIAD FORMAT                                     ELEMN2........6400
      IF (KSOLVP.EQ.0) THEN                                              ELEMN2........6500
         CALL GLOBAN(L,ML,VOLE,BFLOWE,DFLOWE,BTRANE,DTRANE,              ELEMN2........6600
     1      IN,VOL,PMAT,PVEC,UMAT,UVEC)                                  ELEMN2........6700
      ELSE                                                               ELEMN2........6800
         CALL GLOTRI(L,ML,VOLE,BFLOWE,DFLOWE,BTRANE,DTRANE,              ELEMN2........6900
     1      IN,VOL,PMAT,PVEC,UMAT,UVEC,NBI27,MIOFF)                      ELEMN2........7000
      END IF                                                             ELEMN2........7100
 9999 CONTINUE                                                           ELEMN2........7200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2........7300
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2........7400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN2........7500
C                                                                        ELEMN2........7600
C                                                                        ELEMN2........7700
      RETURN                                                             ELEMN2........7800
      END                                                                ELEMN2........7900
C                                                                        ELEMN2........8000
C     SUBROUTINE        E  L  E  M  N  3           SUTRA VERSION 2D3D.1  ELEMN3.........100
C                                                                        ELEMN3.........200
C *** PURPOSE :                                                          ELEMN3.........300
C ***  TO CONTROL AND CARRY OUT ALL CALCULATIONS FOR EACH ELEMENT BY     ELEMN3.........400
C ***  OBTAINING ELEMENT INFORMATION FROM THE BASIS FUNCTION ROUTINE,    ELEMN3.........500
C ***  CARRYING OUT GAUSSIAN INTEGRATION OF FINITE ELEMENT INTEGRALS,    ELEMN3.........600
C ***  AND ASSEMBLING RESULTS OF ELEMENTWISE INTEGRATIONS INTO           ELEMN3.........700
C ***  A GLOBAL MATRIX AND GLOBAL VECTOR FOR BOTH FLOW AND TRANSPORT     ELEMN3.........800
C ***  EQUATIONS. ALSO CALCULATES VELOCITY AT EACH ELEMENT CENTROID FOR  ELEMN3.........900
C ***  PRINTED OUTPUT. THIS SUBROUTINE HANDLES 3D CALCULATIONS ONLY.     ELEMN3........1000
C ***  2D CALCULATIONS ARE PERFORMED IN SUBROUTINE ELEMN2.               ELEMN3........1100
C                                                                        ELEMN3........1200
C                                                                        ELEMN3........1300
      SUBROUTINE ELEMN3(ML,IN,X,Y,Z,PITER,UITER,RCIT,RCITM1,POR,         ELEMN3........1400
     1   ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,                            ELEMN3........1500
     2   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, ELEMN3........1600
     3   PANGL1,PANGL2,PANGL3,VMAG,VANG1,VANG2,VOL,PMAT,PVEC,            ELEMN3........1700
     4   UMAT,UVEC,GXSI,GETA,GZET,PVEL,LREG,NBI27,MIOFF)                 ELEMN3........1800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                ELEMN3........1900
      CHARACTER*80 ERRCOD,CHERR(10),FNAME(0:7)                           ELEMN3........2000
      DIMENSION IN(NIN),X(NN),Y(NN),Z(NN),PITER(NN),                     ELEMN3........2100
     1   UITER(NN),RCIT(NN),RCITM1(NN),POR(NN),PVEL(NN)                  ELEMN3........2200
      DIMENSION PERMXX(NE),PERMXY(NE),PERMXZ(NE),PERMYX(NE),             ELEMN3........2300
     1   PERMYY(NE),PERMYZ(NE),PERMZX(NE),PERMZY(NE),PERMZZ(NE),         ELEMN3........2400
     2   PANGL1(NE),PANGL2(NE),PANGL3(NE),                               ELEMN3........2500
     3   ALMAX(NE),ALMID(NE),ALMIN(NE),ATMAX(NE),ATMID(NE),ATMIN(NE),    ELEMN3........2600
     4   VMAG(NE),VANG1(NE),VANG2(NE),                                   ELEMN3........2700
     5   GXSI(NE,8),GETA(NE,8),GZET(NE,8),LREG(NE)                       ELEMN3........2800
      DIMENSION VOL(NN),PMAT(NELT,NCBI),PVEC(NNVEC),UMAT(NELT,NCBI),     ELEMN3........2900
     1   UVEC(NNVEC)                                                     ELEMN3........3000
      DIMENSION BFLOWE(8,8),DFLOWE(8),BTRANE(8,8),DTRANE(8,8),VOLE(8)    ELEMN3........3100
      DIMENSION F(8,8),W(8,8),DET(8),DFDXG(8,8),DFDYG(8,8),DFDZG(8,8),   ELEMN3........3200
     1   DWDXG(8,8),DWDYG(8,8),DWDZG(8,8)                                ELEMN3........3300
      DIMENSION SWG(8),RHOG(8),VISCG(8),PORG(8),VXG(8),VYG(8),VZG(8),    ELEMN3........3400
     1   RELKG(8),RGXG(8),RGYG(8),RGZG(8),VGMAG(8)                       ELEMN3........3500
      DIMENSION RXXG(8),RXYG(8),RXZG(8),RYXG(8),RYYG(8),RYZG(8),         ELEMN3........3600
     1   RZXG(8),RZYG(8),RZZG(8)                                         ELEMN3........3700
      DIMENSION BXXG(8),BXYG(8),BXZG(8),BYXG(8),BYYG(8),BYZG(8),         ELEMN3........3800
     1   BZXG(8),BZYG(8),BZZG(8),EXG(8),EYG(8),EZG(8)                    ELEMN3........3900
      DIMENSION GXLOC(8),GYLOC(8),GZLOC(8)                               ELEMN3........4000
      DIMENSION NBI27(NBIX),MIOFF(27)                                    ELEMN3........4100
      DIMENSION INERR(10),RLERR(10)                                      ELEMN3........4200
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  ELEMN3........4300
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             ELEMN3........4400
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              ELEMN3........4500
     1   NSOP,NSOU,NBCN                                                  ELEMN3........4600
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   ELEMN3........4700
      COMMON /DIMX2/ NELTA, NNVEC, NDIMJA                                ELEMN3........4800
      COMMON /FNAMES/ FNAME                                              ELEMN3........4900
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        ELEMN3........5000
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  ELEMN3........5100
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      ELEMN3........5200
      COMMON /JCOLS/ NCOLPR, LCOLPR, NCOLS5, NCOLS6, J5COL, J6COL        ELEMN3........5300
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     ELEMN3........5400
     1   KSCRN,KPAUSE                                                    ELEMN3........5500
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      ELEMN3........5600
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        ELEMN3........5700
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           ELEMN3........5800
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       ELEMN3........5900
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  ELEMN3........6000
      DATA GLOC/0.577350269189626D0/                                     ELEMN3........6100
      DATA INTIM/0/,ISTOP/0/                                             ELEMN3........6200
      DATA GXLOC/-1.D0,1.D0,1.D0,-1.D0,-1.D0,1.D0,1.D0,-1.D0/            ELEMN3........6300
      DATA GYLOC/-1.D0,-1.D0,1.D0,1.D0,-1.D0,-1.D0,1.D0,1.D0/            ELEMN3........6400
      DATA GZLOC/-1.D0,-1.D0,-1.D0,-1.D0,1.D0,1.D0,1.D0,1.D0/            ELEMN3........6500
      SAVE GLOC,INTIM,ISTOP,GXLOC,GYLOC,GZLOC                            ELEMN3........6600
C                                                                        ELEMN3........6700
C.....DECIDE WHETHER TO CALCULATE CENTROID VELOCITIES ON THIS CALL       ELEMN3........6800
      IVCALC = 0                                                         ELEMN3........6900
      JVCALC = 0                                                         ELEMN3........7000
      IF ((ML.NE.2).AND.(ITER.EQ.1)) IVCALC = 1                          ELEMN3........7100
      IF (IT.EQ.1) IVCALC = 1                                            ELEMN3........7200
      IF ((KVEL.EQ.1).OR.(K6.NE.-1)) JVCALC = 1                          ELEMN3........7300
      KVCALC = IVCALC + JVCALC                                           ELEMN3........7400
C                                                                        ELEMN3........7500
C.....ON FIRST TIME STEP, PREPARE GRAVITY VECTOR COMPONENTS,             ELEMN3........7600
C        GXSI, GETA, AND GZET FOR CONSISTENT VELOCITIES,                 ELEMN3........7700
C        AND CHECK ELEMENT SHAPES                                        ELEMN3........7800
      IF(INTIM) 100,100,2000                                             ELEMN3........7900
  100 INTIM=1                                                            ELEMN3........8000
C.....LOOP THROUGH ALL ELEMENTS TO OBTAIN THE JACOBIAN                   ELEMN3........8100
C        AT EACH OF THE EIGHT NODES IN EACH ELEMENT                      ELEMN3........8200
      DO 1000 L=1,NE                                                     ELEMN3........8300
       DO 500 IL=1,8                                                     ELEMN3........8400
        XLOC=GXLOC(IL)                                                   ELEMN3........8500
        YLOC=GYLOC(IL)                                                   ELEMN3........8600
        ZLOC=GZLOC(IL)                                                   ELEMN3........8700
        CALL BASIS3(0000,L,XLOC,YLOC,ZLOC,IN,X,Y,Z,F(1,IL),W(1,IL),      ELEMN3........8800
     1     DET(IL),DFDXG(1,IL),DFDYG(1,IL),DFDZG(1,IL),                  ELEMN3........8900
     2     DWDXG(1,IL),DWDYG(1,IL),DWDZG(1,IL),PITER,UITER,PVEL,POR,     ELEMN3........9000
     3     VXG(IL),VYG(IL),VZG(IL),SWG(IL),RHOG(IL),VISCG(IL),           ELEMN3........9100
     4     PORG(IL),VGMAG(IL),RELKG(IL),PERMXX,PERMXY,PERMXZ,            ELEMN3........9200
     5     PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ,                    ELEMN3........9300
     6     CJ11,CJ12,CJ13,CJ21,CJ22,CJ23,CJ31,CJ32,CJ33,                 ELEMN3........9400
     7     GXSI,GETA,GZET,RCIT,RCITM1,RGXG(IL),RGYG(IL),RGZG(IL),LREG)   ELEMN3........9500
        GXSI(L,IL)=CJ11*GRAVX+CJ12*GRAVY+CJ13*GRAVZ                      ELEMN3........9600
        GETA(L,IL)=CJ21*GRAVX+CJ22*GRAVY+CJ23*GRAVZ                      ELEMN3........9700
        GZET(L,IL)=CJ31*GRAVX+CJ32*GRAVY+CJ33*GRAVZ                      ELEMN3........9800
C.....CHECK FOR NEGATIVE- OR ZERO-AREA ERRORS IN ELEMENT SHAPES          ELEMN3........9900
        IF(DET(IL)) 200,200,500                                          ELEMN3.......10000
  200   ISTOP=ISTOP+1                                                    ELEMN3.......10100
        WRITE(K3,400) IN((L-1)*8+IL),L,DET(IL)                           ELEMN3.......10200
        WRITE(K00,401) IN((L-1)*8+IL),L,DET(IL)                          ELEMN3.......10300
  400   FORMAT(11X,'THE DETERMINANT OF THE JACOBIAN AT NODE ',I9,        ELEMN3.......10400
     1     ' IN ELEMENT ',I9,' IS NEGATIVE OR ZERO, ',1PE15.7)           ELEMN3.......10500
  401   FORMAT(1X,'THE DETERMINANT OF THE JACOBIAN AT NODE ',I9,         ELEMN3.......10600
     1     ' IN ELEMENT ',I9,' IS NEGATIVE OR ZERO, ',1PE15.7)           ELEMN3.......10700
  500  CONTINUE                                                          ELEMN3.......10800
 1000 CONTINUE                                                           ELEMN3.......10900
C                                                                        ELEMN3.......11000
      IF(ISTOP.EQ.0) GOTO 2000                                           ELEMN3.......11100
      ERRCOD = 'INP-14B,22-1'                                            ELEMN3.......11200
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           ELEMN3.......11300
      RETURN                                                             ELEMN3.......11400
C                                                                        ELEMN3.......11500
C.....LOOP THROUGH ALL ELEMENTS TO CARRY OUT SPATIAL INTEGRATION         ELEMN3.......11600
C        OF FLUX TERMS IN P AND/OR U EQUATIONS                           ELEMN3.......11700
 2000 IF(IUNSAT.NE.0) IUNSAT=2                                           ELEMN3.......11800
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN3.......11900
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN3.......12000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN3.......12100
      DO 9999 L=1,NE                                                     ELEMN3.......12200
       XIX=-1.D0                                                         ELEMN3.......12300
       YIY=-1.D0                                                         ELEMN3.......12400
       ZIZ=-1.D0                                                         ELEMN3.......12500
       KG=0                                                              ELEMN3.......12600
C.....OBTAIN BASIS FUNCTION AND RELATED INFORMATION AT EACH OF           ELEMN3.......12700
C        EIGHT GAUSS POINTS IN THE ELEMENT                               ELEMN3.......12800
       DO 2250 IZL=1,2                                                   ELEMN3.......12900
        DO 2200 IYL=1,2                                                  ELEMN3.......13000
         DO 2100 IXL=1,2                                                 ELEMN3.......13100
          KG=KG+1                                                        ELEMN3.......13200
          XLOC=XIX*GLOC                                                  ELEMN3.......13300
          YLOC=YIY*GLOC                                                  ELEMN3.......13400
          ZLOC=ZIZ*GLOC                                                  ELEMN3.......13500
          CALL BASIS3(0001,L,XLOC,YLOC,ZLOC,IN,X,Y,Z,F(1,KG),W(1,KG),    ELEMN3.......13600
     1       DET(KG),DFDXG(1,KG),DFDYG(1,KG),DFDZG(1,KG),                ELEMN3.......13700
     2       DWDXG(1,KG),DWDYG(1,KG),DWDZG(1,KG),PITER,UITER,PVEL,POR,   ELEMN3.......13800
     3       VXG(KG),VYG(KG),VZG(KG),SWG(KG),RHOG(KG),VISCG(KG),         ELEMN3.......13900
     4       PORG(KG),VGMAG(KG),RELKG(KG),PERMXX,PERMXY,PERMXZ,          ELEMN3.......14000
     5       PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ,                  ELEMN3.......14100
     6       CJ11,CJ12,CJ13,CJ21,CJ22,CJ23,CJ31,CJ32,CJ33,               ELEMN3.......14200
     7       GXSI,GETA,GZET,RCIT,RCITM1,RGXG(KG),RGYG(KG),RGZG(KG),LREG) ELEMN3.......14300
 2100     XIX=-XIX                                                       ELEMN3.......14400
 2200    YIY=-YIY                                                        ELEMN3.......14500
 2250   ZIZ=-ZIZ                                                         ELEMN3.......14600
C                                                                        ELEMN3.......14700
C.....CALCULATE VELOCITY AT ELEMENT CENTROID WHEN REQUIRED               ELEMN3.......14800
       IF(KVCALC-2) 3000,2300,3000                                       ELEMN3.......14900
 2300  AXSUM=0.0D0                                                       ELEMN3.......15000
       AYSUM=0.0D0                                                       ELEMN3.......15100
       AZSUM=0.0D0                                                       ELEMN3.......15200
       DO 2400 KG=1,8                                                    ELEMN3.......15300
        AXSUM=AXSUM+VXG(KG)                                              ELEMN3.......15400
        AYSUM=AYSUM+VYG(KG)                                              ELEMN3.......15500
 2400   AZSUM=AZSUM+VZG(KG)                                              ELEMN3.......15600
       VMAG(L)=DSQRT(AXSUM*AXSUM+AYSUM*AYSUM+AZSUM*AZSUM)                ELEMN3.......15700
       IF (VMAG(L).NE.0D0) THEN                                          ELEMN3.......15800
          VANG2(L)=DASIN(AZSUM/VMAG(L))*5.729577951308232D+1             ELEMN3.......15900
          VMAG(L)=VMAG(L)*1.25D-1                                        ELEMN3.......16000
          VANG1(L)=DATAN2(AYSUM,AXSUM)*5.729577951308232D+1              ELEMN3.......16100
       ELSE                                                              ELEMN3.......16200
          VANG1(L)=0D0                                                   ELEMN3.......16300
          VANG2(L)=0D0                                                   ELEMN3.......16400
       END IF                                                            ELEMN3.......16500
C                                                                        ELEMN3.......16600
 3000  CONTINUE                                                          ELEMN3.......16700
C                                                                        ELEMN3.......16800
C.....CALCULATE PARAMETERS FOR FLUID MASS BALANCE AT GAUSS POINTS        ELEMN3.......16900
       IF(ML-1) 3400,3400,6100                                           ELEMN3.......17000
 3400  SWTEST=0.D0                                                       ELEMN3.......17100
       DO 4000 KG=1,8                                                    ELEMN3.......17200
        SWTEST=SWTEST+SWG(KG)                                            ELEMN3.......17300
        ROMG=RHOG(KG)*RELKG(KG)/VISCG(KG)                                ELEMN3.......17400
        RXXG(KG)=PERMXX(L)*ROMG                                          ELEMN3.......17500
        RXYG(KG)=PERMXY(L)*ROMG                                          ELEMN3.......17600
        RXZG(KG)=PERMXZ(L)*ROMG                                          ELEMN3.......17700
        RYXG(KG)=PERMYX(L)*ROMG                                          ELEMN3.......17800
        RYYG(KG)=PERMYY(L)*ROMG                                          ELEMN3.......17900
        RYZG(KG)=PERMYZ(L)*ROMG                                          ELEMN3.......18000
        RZXG(KG)=PERMZX(L)*ROMG                                          ELEMN3.......18100
        RZYG(KG)=PERMZY(L)*ROMG                                          ELEMN3.......18200
        RZZG(KG)=PERMZZ(L)*ROMG                                          ELEMN3.......18300
 4000   CONTINUE                                                         ELEMN3.......18400
C                                                                        ELEMN3.......18500
C.....INTEGRATE FLUID MASS BALANCE IN AN UNSATURATED ELEMENT             ELEMN3.......18600
C        USING ASYMMETRIC WEIGHTING FUNCTIONS                            ELEMN3.......18700
       IF(UP.LE.1.0D-6) GOTO 5200                                        ELEMN3.......18800
       IF(SWTEST-3.999D0) 4200,5200,5200                                 ELEMN3.......18900
 4200  DO 4300 I=1,8                                                     ELEMN3.......19000
        VOLE(I) = 0.D0                                                   ELEMN3.......19100
        DFLOWE(I) = 0.D0                                                 ELEMN3.......19200
        DO 4300 J=1,8                                                    ELEMN3.......19300
         BFLOWE(I,J) = 0.D0                                              ELEMN3.......19400
 4300  CONTINUE                                                          ELEMN3.......19500
       DO 5000 KG=1,8                                                    ELEMN3.......19600
        RXXGD = RXXG(KG)*DET(KG)                                         ELEMN3.......19700
        RXYGD = RXYG(KG)*DET(KG)                                         ELEMN3.......19800
        RXZGD = RXZG(KG)*DET(KG)                                         ELEMN3.......19900
        RYXGD = RYXG(KG)*DET(KG)                                         ELEMN3.......20000
        RYYGD = RYYG(KG)*DET(KG)                                         ELEMN3.......20100
        RYZGD = RYZG(KG)*DET(KG)                                         ELEMN3.......20200
        RZXGD = RZXG(KG)*DET(KG)                                         ELEMN3.......20300
        RZYGD = RZYG(KG)*DET(KG)                                         ELEMN3.......20400
        RZZGD = RZZG(KG)*DET(KG)                                         ELEMN3.......20500
        RDRX = RXXGD*RGXG(KG) + RXYGD*RGYG(KG) + RXZGD*RGZG(KG)          ELEMN3.......20600
        RDRY = RYXGD*RGXG(KG) + RYYGD*RGYG(KG) + RYZGD*RGZG(KG)          ELEMN3.......20700
        RDRZ = RZXGD*RGXG(KG) + RZYGD*RGYG(KG) + RZZGD*RGZG(KG)          ELEMN3.......20800
        DO 4400 I=1,8                                                    ELEMN3.......20900
         VOLE(I) = VOLE(I) + F(I,KG)*DET(KG)                             ELEMN3.......21000
         DFLOWE(I) = DFLOWE(I) + RDRX*DWDXG(I,KG) + RDRY*DWDYG(I,KG)     ELEMN3.......21100
     1               + RDRZ*DWDZG(I,KG)                                  ELEMN3.......21200
 4400   CONTINUE                                                         ELEMN3.......21300
        DO 5000 J=1,8                                                    ELEMN3.......21400
         RDDFJX = RXXGD*DFDXG(J,KG) + RXYGD*DFDYG(J,KG)                  ELEMN3.......21500
     1            + RXZGD*DFDZG(J,KG)                                    ELEMN3.......21600
         RDDFJY = RYXGD*DFDXG(J,KG) + RYYGD*DFDYG(J,KG)                  ELEMN3.......21700
     1            + RYZGD*DFDZG(J,KG)                                    ELEMN3.......21800
         RDDFJZ = RZXGD*DFDXG(J,KG) + RZYGD*DFDYG(J,KG)                  ELEMN3.......21900
     1            + RZZGD*DFDZG(J,KG)                                    ELEMN3.......22000
         DO 5000 I=1,8                                                   ELEMN3.......22100
          BFLOWE(I,J) = BFLOWE(I,J) + DWDXG(I,KG)*RDDFJX                 ELEMN3.......22200
     1                  + DWDYG(I,KG)*RDDFJY + DWDZG(I,KG)*RDDFJZ        ELEMN3.......22300
 5000  CONTINUE                                                          ELEMN3.......22400
       GOTO 6050                                                         ELEMN3.......22500
C                                                                        ELEMN3.......22600
C.....INTEGRATE FLUID MASS BALANCE IN A SATURATED OR UNSATURATED         ELEMN3.......22700
C        ELEMENT USING SYMMETRIC WEIGHTING FUNCTIONS                     ELEMN3.......22800
 5200  DO 5300 I=1,8                                                     ELEMN3.......22900
        VOLE(I) = 0.D0                                                   ELEMN3.......23000
        DFLOWE(I) = 0.D0                                                 ELEMN3.......23100
        DO 5300 J=1,8                                                    ELEMN3.......23200
         BFLOWE(I,J) = 0.D0                                              ELEMN3.......23300
 5300  CONTINUE                                                          ELEMN3.......23400
       DO 6000 KG=1,8                                                    ELEMN3.......23500
        RXXGD = RXXG(KG)*DET(KG)                                         ELEMN3.......23600
        RXYGD = RXYG(KG)*DET(KG)                                         ELEMN3.......23700
        RXZGD = RXZG(KG)*DET(KG)                                         ELEMN3.......23800
        RYXGD = RYXG(KG)*DET(KG)                                         ELEMN3.......23900
        RYYGD = RYYG(KG)*DET(KG)                                         ELEMN3.......24000
        RYZGD = RYZG(KG)*DET(KG)                                         ELEMN3.......24100
        RZXGD = RZXG(KG)*DET(KG)                                         ELEMN3.......24200
        RZYGD = RZYG(KG)*DET(KG)                                         ELEMN3.......24300
        RZZGD = RZZG(KG)*DET(KG)                                         ELEMN3.......24400
        RDRX = RXXGD*RGXG(KG) + RXYGD*RGYG(KG) + RXZGD*RGZG(KG)          ELEMN3.......24500
        RDRY = RYXGD*RGXG(KG) + RYYGD*RGYG(KG) + RYZGD*RGZG(KG)          ELEMN3.......24600
        RDRZ = RZXGD*RGXG(KG) + RZYGD*RGYG(KG) + RZZGD*RGZG(KG)          ELEMN3.......24700
        DO 5400 I=1,8                                                    ELEMN3.......24800
         VOLE(I) = VOLE(I) + F(I,KG)*DET(KG)                             ELEMN3.......24900
         DFLOWE(I) = DFLOWE(I) + RDRX*DFDXG(I,KG) + RDRY*DFDYG(I,KG)     ELEMN3.......25000
     1               + RDRZ*DFDZG(I,KG)                                  ELEMN3.......25100
 5400   CONTINUE                                                         ELEMN3.......25200
        DO 6000 J=1,8                                                    ELEMN3.......25300
         RDDFJX = RXXGD*DFDXG(J,KG) + RXYGD*DFDYG(J,KG)                  ELEMN3.......25400
     1            + RXZGD*DFDZG(J,KG)                                    ELEMN3.......25500
         RDDFJY = RYXGD*DFDXG(J,KG) + RYYGD*DFDYG(J,KG)                  ELEMN3.......25600
     1            + RYZGD*DFDZG(J,KG)                                    ELEMN3.......25700
         RDDFJZ = RZXGD*DFDXG(J,KG) + RZYGD*DFDYG(J,KG)                  ELEMN3.......25800
     1            + RZZGD*DFDZG(J,KG)                                    ELEMN3.......25900
         DO 6000 I=1,8                                                   ELEMN3.......26000
          BFLOWE(I,J) = BFLOWE(I,J) + DFDXG(I,KG)*RDDFJX                 ELEMN3.......26100
     1                  + DFDYG(I,KG)*RDDFJY + DFDZG(I,KG)*RDDFJZ        ELEMN3.......26200
 6000  CONTINUE                                                          ELEMN3.......26300
 6050  CONTINUE                                                          ELEMN3.......26400
       IF(ML-1) 6100,9000,6100                                           ELEMN3.......26500
 6100  IF(NOUMAT.EQ.1) GOTO 9000                                         ELEMN3.......26600
C                                                                        ELEMN3.......26700
C                                                                        ELEMN3.......26800
C.....CALCULATE PARAMETERS FOR ENERGY BALANCE OR SOLUTE MASS BALANCE     ELEMN3.......26900
C        AT GAUSS POINTS                                                 ELEMN3.......27000
       DO 7000 KG=1,8                                                    ELEMN3.......27100
        ESWG=PORG(KG)*SWG(KG)                                            ELEMN3.......27200
        RHOCWG=RHOG(KG)*CW                                               ELEMN3.......27300
        ESRCG=ESWG*RHOCWG                                                ELEMN3.......27400
        IF(VGMAG(KG)) 6300,6300,6600                                     ELEMN3.......27500
 6300   EXG(KG)=0.0D0                                                    ELEMN3.......27600
        EYG(KG)=0.0D0                                                    ELEMN3.......27700
        EZG(KG)=0.0D0                                                    ELEMN3.......27800
        DXXG=0.0D0                                                       ELEMN3.......27900
        DXYG=0.0D0                                                       ELEMN3.......28000
        DXZG=0.0D0                                                       ELEMN3.......28100
        DYXG=0.0D0                                                       ELEMN3.......28200
        DYYG=0.0D0                                                       ELEMN3.......28300
        DYZG=0.0D0                                                       ELEMN3.......28400
        DZXG=0.0D0                                                       ELEMN3.......28500
        DZYG=0.0D0                                                       ELEMN3.......28600
        DZZG=0.0D0                                                       ELEMN3.......28700
        GOTO 6900                                                        ELEMN3.......28800
 6600   EXG(KG)=ESRCG*VXG(KG)                                            ELEMN3.......28900
        EYG(KG)=ESRCG*VYG(KG)                                            ELEMN3.......29000
        EZG(KG)=ESRCG*VZG(KG)                                            ELEMN3.......29100
C                                                                        ELEMN3.......29200
C                                                                        ELEMN3.......29300
C.....DISPERSIVITY MODEL FOR 3D ANISOTROPIC MEDIA                        ELEMN3.......29400
C        WITH PRINCIPAL DISPERSIVITIES ALMAX, ALMID, ALMIN,              ELEMN3.......29500
C        ATMAX, ATMID, ATMIN                                             ELEMN3.......29600
        CALL DISPR3(VXG(KG),VYG(KG),VZG(KG),VGMAG(KG),PANGL1(L),         ELEMN3.......29700
     1     PANGL2(L),PANGL3(L),ALMAX(L),ALMID(L),ALMIN(L),               ELEMN3.......29800
     2     ATMAX(L),ATMID(L),ATMIN(L),DXXG,DXYG,DXZG,DYXG,DYYG,DYZG,     ELEMN3.......29900
     3     DZXG,DZYG,DZZG)                                               ELEMN3.......30000
C                                                                        ELEMN3.......30100
C.....IN-PARALLEL CONDUCTIVITIES (DIFFUSIVITIES) FORMULA                 ELEMN3.......30200
 6900   IF (ME.EQ.1) THEN                                                ELEMN3.......30300
C..........FOR ENERGY TRANSPORT:                                         ELEMN3.......30400
           ESE = ESWG*SIGMAW + (1D0-PORG(KG))*SIGMAS                     ELEMN3.......30500
        ELSE                                                             ELEMN3.......30600
C..........FOR SOLUTE TRANSPORT:                                         ELEMN3.......30700
           ESE = ESRCG*SIGMAW + (1D0-PORG(KG))*RHOCWG*SIGMAS             ELEMN3.......30800
        END IF                                                           ELEMN3.......30900
C.....ADD DIFFUSION AND DISPERSION TERMS TO TOTAL DISPERSION TENSOR      ELEMN3.......31000
        BXXG(KG)=ESRCG*DXXG+ESE                                          ELEMN3.......31100
        BXYG(KG)=ESRCG*DXYG                                              ELEMN3.......31200
        BXZG(KG)=ESRCG*DXZG                                              ELEMN3.......31300
        BYXG(KG)=ESRCG*DYXG                                              ELEMN3.......31400
        BYYG(KG)=ESRCG*DYYG+ESE                                          ELEMN3.......31500
        BYZG(KG)=ESRCG*DYZG                                              ELEMN3.......31600
        BZXG(KG)=ESRCG*DZXG                                              ELEMN3.......31700
        BZYG(KG)=ESRCG*DZYG                                              ELEMN3.......31800
 7000   BZZG(KG)=ESRCG*DZZG+ESE                                          ELEMN3.......31900
C                                                                        ELEMN3.......32000
C.....INTEGRATE SOLUTE MASS BALANCE OR ENERGY BALANCE                    ELEMN3.......32100
C        USING SYMMETRIC WEIGHTING FUNCTIONS FOR DISPERSION TERM AND     ELEMN3.......32200
C        USING EITHER SYMMETRIC OR ASYMMETRIC WEIGHTING FUNCTIONS        ELEMN3.......32300
C        FOR ADVECTION TERM                                              ELEMN3.......32400
         DO 7400 I=1,8                                                   ELEMN3.......32500
         DO 7400 J=1,8                                                   ELEMN3.......32600
            BTRANE(I,J) = 0.D0                                           ELEMN3.......32700
            DTRANE(I,J) = 0.D0                                           ELEMN3.......32800
 7400    CONTINUE                                                        ELEMN3.......32900
         DO 8000 KG=1,8                                                  ELEMN3.......33000
            BXXGD = BXXG(KG)*DET(KG)                                     ELEMN3.......33100
            BXYGD = BXYG(KG)*DET(KG)                                     ELEMN3.......33200
            BXZGD = BXZG(KG)*DET(KG)                                     ELEMN3.......33300
            BYXGD = BYXG(KG)*DET(KG)                                     ELEMN3.......33400
            BYYGD = BYYG(KG)*DET(KG)                                     ELEMN3.......33500
            BYZGD = BYZG(KG)*DET(KG)                                     ELEMN3.......33600
            BZXGD = BZXG(KG)*DET(KG)                                     ELEMN3.......33700
            BZYGD = BZYG(KG)*DET(KG)                                     ELEMN3.......33800
            BZZGD = BZZG(KG)*DET(KG)                                     ELEMN3.......33900
            EXGD = EXG(KG)*DET(KG)                                       ELEMN3.......34000
            EYGD = EYG(KG)*DET(KG)                                       ELEMN3.......34100
            EZGD = EZG(KG)*DET(KG)                                       ELEMN3.......34200
            DO 8000 J=1,8                                                ELEMN3.......34300
               BDDFJX = BXXGD*DFDXG(J,KG) + BXYGD*DFDYG(J,KG)            ELEMN3.......34400
     1                 + BXZGD*DFDZG(J,KG)                               ELEMN3.......34500
               BDDFJY = BYXGD*DFDXG(J,KG) + BYYGD*DFDYG(J,KG)            ELEMN3.......34600
     1                 + BYZGD*DFDZG(J,KG)                               ELEMN3.......34700
               BDDFJZ = BZXGD*DFDXG(J,KG) + BZYGD*DFDYG(J,KG)            ELEMN3.......34800
     1                 + BZZGD*DFDZG(J,KG)                               ELEMN3.......34900
               EDDFJ = EXGD*DFDXG(J,KG) + EYGD*DFDYG(J,KG)               ELEMN3.......35000
     1                 + EZGD*DFDZG(J,KG)                                ELEMN3.......35100
               DO 8000 I=1,8                                             ELEMN3.......35200
                  BTRANE(I,J) = BTRANE(I,J) + DFDXG(I,KG)*BDDFJX         ELEMN3.......35300
     1               + DFDYG(I,KG)*BDDFJY + DFDZG(I,KG)*BDDFJZ           ELEMN3.......35400
                  DTRANE(I,J) = DTRANE(I,J) + EDDFJ*W(I,KG)              ELEMN3.......35500
 8000    CONTINUE                                                        ELEMN3.......35600
 9000  CONTINUE                                                          ELEMN3.......35700
C                                                                        ELEMN3.......35800
C                                                                        ELEMN3.......35900
C.....SEND RESULTS OF INTEGRATIONS FOR THIS ELEMENT                      ELEMN3.......36000
C        TO GLOBAL ASSEMBLY ROUTINE:                                     ELEMN3.......36100
C        GLOBAN -- SUTRA'S ORIGINAL BANDED FORMAT                        ELEMN3.......36200
C        GLOTRI -- SLAP TRIAD FORMAT                                     ELEMN3.......36300
      IF (KSOLVP.EQ.0) THEN                                              ELEMN3.......36400
         CALL GLOBAN(L,ML,VOLE,BFLOWE,DFLOWE,BTRANE,DTRANE,              ELEMN3.......36500
     1      IN,VOL,PMAT,PVEC,UMAT,UVEC)                                  ELEMN3.......36600
      ELSE                                                               ELEMN3.......36700
         CALL GLOTRI(L,ML,VOLE,BFLOWE,DFLOWE,BTRANE,DTRANE,              ELEMN3.......36800
     1      IN,VOL,PMAT,PVEC,UMAT,UVEC,NBI27,MIOFF)                      ELEMN3.......36900
      END IF                                                             ELEMN3.......37000
 9999 CONTINUE                                                           ELEMN3.......37100
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN3.......37200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN3.......37300
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ELEMN3.......37400
C                                                                        ELEMN3.......37500
C                                                                        ELEMN3.......37600
      RETURN                                                             ELEMN3.......37700
      END                                                                ELEMN3.......37800
C                                                                        ELEMN3.......37900
C     SUBROUTINE        F  O  P  E  N              SUTRA VERSION 2D3D.1  FOPEN..........100
C                                                                        FOPEN..........200
C *** PURPOSE :                                                          FOPEN..........300
C ***  OPENS FILES FOR SUTRA SIMULATION.                                 FOPEN..........400
C ***  OPENS ERROR OUTPUT FILE, READS FILE NUMBERS AND NAMES,            FOPEN..........500
C ***  AND CHECKS FOR EXISTENCE OF INPUT FILES.                          FOPEN..........600
C                                                                        FOPEN..........700
      SUBROUTINE FOPEN(UNAME,IUNIT,NFILE)                                FOPEN..........800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                FOPEN..........900
      CHARACTER*80 FT,FN,UNAME,FNAME,ENAME,FTYPE,FTSTR,FNTMP             FOPEN.........1000
      CHARACTER*80 ERRCOD,CHERR(10)                                      FOPEN.........1100
      LOGICAL IS                                                         FOPEN.........1200
      DIMENSION FTYPE(0:7),FNAME(0:7),IUNIT(0:7)                         FOPEN.........1300
      DIMENSION FTSTR(0:7),FNTMP(0:7),IUTMP(0:7)                         FOPEN.........1400
      DIMENSION INERR(10),RLERR(10)                                      FOPEN.........1500
      COMMON /FNAMES/ FNAME                                              FOPEN.........1600
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        FOPEN.........1700
      DATA (FTSTR(NFT),NFT=0,7)/'SMY','INP','ICS','LST','RST',           FOPEN.........1800
     1   'NOD','ELE','OBS'/                                              FOPEN.........1900
C                                                                        FOPEN.........2000
C.....INITIALIZE UNIT NUMBERS                                            FOPEN.........2100
      K1 = -1                                                            FOPEN.........2200
      K2 = -1                                                            FOPEN.........2300
      K3 = -1                                                            FOPEN.........2400
      K4 = -1                                                            FOPEN.........2500
      K5 = -1                                                            FOPEN.........2600
      K6 = -1                                                            FOPEN.........2700
      K7 = -1                                                            FOPEN.........2800
C                                                                        FOPEN.........2900
C.....SET DEFAULT VALUES FOR THE ERROR OUTPUT FILE                       FOPEN.........3000
      K00 = 1                                                            FOPEN.........3100
      ENAME = 'SUTRA.SMY'                                                FOPEN.........3200
C                                                                        FOPEN.........3300
C.....OPEN FILE UNIT CONTAINING UNIT NUMBERS AND FILE ASSIGNMENTS        FOPEN.........3400
      IU=K0                                                              FOPEN.........3500
      FN=UNAME                                                           FOPEN.........3600
      INQUIRE(FILE=UNAME,EXIST=IS)                                       FOPEN.........3700
      IF(IS) THEN                                                        FOPEN.........3800
       OPEN(UNIT=IU,FILE=UNAME,STATUS='OLD',FORM='FORMATTED',            FOPEN.........3900
     1   IOSTAT=KERR)                                                    FOPEN.........4000
      ELSE                                                               FOPEN.........4100
C......OPEN DEFAULT ERROR OUTPUT FILE                                    FOPEN.........4200
       OPEN(UNIT=K00,FILE=ENAME,STATUS='REPLACE')                        FOPEN.........4300
       GOTO 8000                                                         FOPEN.........4400
      ENDIF                                                              FOPEN.........4500
      IF(KERR.GT.0) THEN                                                 FOPEN.........4600
C......OPEN DEFAULT ERROR OUTPUT FILE                                    FOPEN.........4700
       OPEN(UNIT=K00,FILE=ENAME,STATUS='REPLACE')                        FOPEN.........4800
       GOTO 9000                                                         FOPEN.........4900
      END IF                                                             FOPEN.........5000
C                                                                        FOPEN.........5100
C.....IDENTIFY AND OPEN ERROR OUTPUT FILE (IF ASSIGNED), OTHERWISE       FOPEN.........5200
C        OPEN DEFAULT ERROR OUTPUT FILE                                  FOPEN.........5300
      DO 90 NF=0,7                                                       FOPEN.........5400
         READ(K0,*,IOSTAT=INERR(1),END=99) FT, IU, FN                    FOPEN.........5500
         IF (INERR(1).NE.0) THEN                                         FOPEN.........5600
C...........OPEN DEFAULT ERROR OUTPUT FILE                               FOPEN.........5700
            OPEN(UNIT=K00,FILE=ENAME,STATUS='REPLACE')                   FOPEN.........5800
            ERRCOD = 'REA-FIL'                                           FOPEN.........5900
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     FOPEN.........6000
            RETURN                                                       FOPEN.........6100
         END IF                                                          FOPEN.........6200
         IF (FT.EQ.FTSTR(0)) THEN                                        FOPEN.........6300
            K00 = IU                                                     FOPEN.........6400
            ENAME = FN                                                   FOPEN.........6500
            GOTO 99                                                      FOPEN.........6600
         END IF                                                          FOPEN.........6700
   90 CONTINUE                                                           FOPEN.........6800
   99 OPEN(UNIT=K00,FILE=ENAME,STATUS='REPLACE',IOSTAT=KERR)             FOPEN.........6900
      IF (KERR.GT.0) THEN                                                FOPEN.........7000
         IU = K00                                                        FOPEN.........7100
         FN = ENAME                                                      FOPEN.........7200
         K00 = 1                                                         FOPEN.........7300
         ENAME = 'SUTRA.SMY'                                             FOPEN.........7400
C........OPEN DEFAULT ERROR OUTPUT FILE                                  FOPEN.........7500
         OPEN(UNIT=K00,FILE=ENAME,STATUS='REPLACE')                      FOPEN.........7600
         GOTO 9000                                                       FOPEN.........7700
      END IF                                                             FOPEN.........7800
      REWIND(K0)                                                         FOPEN.........7900
      FTYPE(0) = FTSTR(0)                                                FOPEN.........8000
      IUTMP(0) = K00                                                     FOPEN.........8100
      FNTMP(0) = ENAME                                                   FOPEN.........8200
      NFILE = 0                                                          FOPEN.........8300
C                                                                        FOPEN.........8400
C.....REREAD FILE CONTAINING UNIT NUMBERS AND FILE ASSIGNMENTS           FOPEN.........8500
C        AND CHECK FOR VALIDITY OF FILE TYPE SPECIFICATIONS              FOPEN.........8600
      ILOG = 0                                                           FOPEN.........8700
      DO 190 NF=0,7                                                      FOPEN.........8800
  100    READ(K0,*,IOSTAT=INERR(1),END=200) FT, IU, FN                   FOPEN.........8900
         IF (INERR(1).NE.0) THEN                                         FOPEN.........9000
            ERRCOD = 'REA-FIL'                                           FOPEN.........9100
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     FOPEN.........9200
            RETURN                                                       FOPEN.........9300
         END IF                                                          FOPEN.........9400
         IF (FT.EQ.FTSTR(0)) THEN                                        FOPEN.........9500
            ILOG = ILOG + 1                                              FOPEN.........9600
            IF (ILOG.EQ.1) THEN                                          FOPEN.........9700
               GOTO 190                                                  FOPEN.........9800
            ELSE                                                         FOPEN.........9900
               GOTO 170                                                  FOPEN........10000
            END IF                                                       FOPEN........10100
         END IF                                                          FOPEN........10200
         DO 160 NFT=1,7                                                  FOPEN........10300
            IF (FT.EQ.FTSTR(NFT)) GOTO 170                               FOPEN........10400
  160    CONTINUE                                                        FOPEN........10500
         ERRCOD = 'FIL-5'                                                FOPEN........10600
         CHERR(1) = UNAME                                                FOPEN........10700
         CHERR(2) = FT                                                   FOPEN........10800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        FOPEN........10900
         RETURN                                                          FOPEN........11000
  170    NFILE=NFILE+1                                                   FOPEN........11100
         FTYPE(NFILE)=FT                                                 FOPEN........11200
         IUTMP(NFILE)=IU                                                 FOPEN........11300
         FNTMP(NFILE)=FN                                                 FOPEN........11400
  190 CONTINUE                                                           FOPEN........11500
  200 CONTINUE                                                           FOPEN........11600
C                                                                        FOPEN........11700
C.....CHECK FOR REPEATED UNIT NUMBERS AND FILENAMES                      FOPEN........11800
      DO 250 NF=0,NFILE-1                                                FOPEN........11900
      DO 250 NF2=NF+1,NFILE                                              FOPEN........12000
         IF (IUTMP(NF2).EQ.IUTMP(NF)) THEN                               FOPEN........12100
            ERRCOD = 'FIL-3'                                             FOPEN........12200
            INERR(1) = IUTMP(NF)                                         FOPEN........12300
            CHERR(1) = UNAME                                             FOPEN........12400
            CHERR(2) = FNTMP(NF)                                         FOPEN........12500
            CHERR(3) = FNTMP(NF2)                                        FOPEN........12600
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     FOPEN........12700
            RETURN                                                       FOPEN........12800
         END IF                                                          FOPEN........12900
         IF (FNTMP(NF2).EQ.FNTMP(NF)) THEN                               FOPEN........13000
            ERRCOD = 'FIL-4'                                             FOPEN........13100
            CHERR(1) = UNAME                                             FOPEN........13200
            CHERR(2) = FNTMP(NF)                                         FOPEN........13300
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     FOPEN........13400
            RETURN                                                       FOPEN........13500
         END IF                                                          FOPEN........13600
  250 CONTINUE                                                           FOPEN........13700
C                                                                        FOPEN........13800
C.....PUT FILES IN STANDARD ORDER                                        FOPEN........13900
      DO 280 NFT=0,7                                                     FOPEN........14000
         IUNIT(NFT) = -1                                                 FOPEN........14100
         DO 270 NF=0,NFILE                                               FOPEN........14200
            IF (FTYPE(NF).EQ.FTSTR(NFT)) THEN                            FOPEN........14300
               IF (IUNIT(NFT).EQ.-1) THEN                                FOPEN........14400
                  IUNIT(NFT) = IUTMP(NF)                                 FOPEN........14500
                  FNAME(NFT) = FNTMP(NF)                                 FOPEN........14600
               ELSE                                                      FOPEN........14700
                  ERRCOD = 'FIL-6'                                       FOPEN........14800
                  CHERR(1) = UNAME                                       FOPEN........14900
                  CHERR(2) = FTYPE(NF)                                   FOPEN........15000
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               FOPEN........15100
                  RETURN                                                 FOPEN........15200
               END IF                                                    FOPEN........15300
            END IF                                                       FOPEN........15400
  270    CONTINUE                                                        FOPEN........15500
         IF ((NFT.GE.1).AND.(NFT.LE.4).AND.(IUNIT(NFT).EQ.-1)) THEN      FOPEN........15600
            ERRCOD = 'FIL-7'                                             FOPEN........15700
            CHERR(1) = UNAME                                             FOPEN........15800
            CHERR(2) = FTSTR(NFT)                                        FOPEN........15900
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     FOPEN........16000
            RETURN                                                       FOPEN........16100
         END IF                                                          FOPEN........16200
  280 CONTINUE                                                           FOPEN........16300
C.....(K00 HAS BEEN SET PREVIOUSLY)                                      FOPEN........16400
      K1=IUNIT(1)                                                        FOPEN........16500
      K2=IUNIT(2)                                                        FOPEN........16600
      K3=IUNIT(3)                                                        FOPEN........16700
      K4=IUNIT(4)                                                        FOPEN........16800
      K5=IUNIT(5)                                                        FOPEN........16900
      K6=IUNIT(6)                                                        FOPEN........17000
      K7=IUNIT(7)                                                        FOPEN........17100
C                                                                        FOPEN........17200
C.....CHECK FOR EXISTENCE OF INPUT FILES                                 FOPEN........17300
C        AND OPEN INPUT AND OUTPUT FILES (EXCEPT SMY FILE)               FOPEN........17400
      DO 300 NF=1,7                                                      FOPEN........17500
      IU=IUNIT(NF)                                                       FOPEN........17600
      FN=FNAME(NF)                                                       FOPEN........17700
      IF (IU.EQ.-1) GOTO 300                                             FOPEN........17800
      IF(NF.LE.2) THEN                                                   FOPEN........17900
       INQUIRE(FILE=FN,EXIST=IS)                                         FOPEN........18000
       IF(IS) THEN                                                       FOPEN........18100
        OPEN(UNIT=IU,FILE=FN,STATUS='OLD',FORM='FORMATTED',IOSTAT=KERR)  FOPEN........18200
       ELSE                                                              FOPEN........18300
        GOTO 8000                                                        FOPEN........18400
       ENDIF                                                             FOPEN........18500
      ELSE                                                               FOPEN........18600
       OPEN(UNIT=IU,FILE=FN,STATUS='REPLACE',FORM='FORMATTED',           FOPEN........18700
     1    IOSTAT=KERR)                                                   FOPEN........18800
      ENDIF                                                              FOPEN........18900
      IF(KERR.GT.0) GOTO 9000                                            FOPEN........19000
  300 CONTINUE                                                           FOPEN........19100
      RETURN                                                             FOPEN........19200
C                                                                        FOPEN........19300
 8000 CONTINUE                                                           FOPEN........19400
C.....WRITE ERROR MESSAGE AND RETURN                                     FOPEN........19500
      ERRCOD = 'FIL-1'                                                   FOPEN........19600
      CHERR(1) = UNAME                                                   FOPEN........19700
      CHERR(2) = FN                                                      FOPEN........19800
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           FOPEN........19900
      RETURN                                                             FOPEN........20000
C                                                                        FOPEN........20100
 9000 CONTINUE                                                           FOPEN........20200
C.....WRITE ERROR MESSAGE AND RETURN                                     FOPEN........20300
      ERRCOD = 'FIL-2'                                                   FOPEN........20400
      CHERR(1) = UNAME                                                   FOPEN........20500
      CHERR(2) = FN                                                      FOPEN........20600
      INERR(1) = IU                                                      FOPEN........20700
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           FOPEN........20800
      RETURN                                                             FOPEN........20900
C                                                                        FOPEN........21000
      END                                                                FOPEN........21100
C                                                                        FOPEN........21200
C     SUBROUTINE        G  L  O  B  A  N           SUTRA VERSION 2D3D.1  GLOBAN.........100
C                                                                        GLOBAN.........200
C *** PURPOSE :                                                          GLOBAN.........300
C ***  TO ASSEMBLE RESULTS OF ELEMENTWISE INTEGRATIONS INTO              GLOBAN.........400
C ***  A GLOBAL BANDED MATRIX AND GLOBAL VECTOR FOR BOTH                 GLOBAN.........500
C ***  FLOW AND TRANSPORT EQUATIONS.                                     GLOBAN.........600
C                                                                        GLOBAN.........700
      SUBROUTINE GLOBAN(L,ML,VOLE,BFLOWE,DFLOWE,BTRANE,DTRANE,           GLOBAN.........800
     1      IN,VOL,PMAT,PVEC,UMAT,UVEC)                                  GLOBAN.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                GLOBAN........1000
      DIMENSION BFLOWE(8,8),DFLOWE(8),BTRANE(8,8),DTRANE(8,8),VOLE(8)    GLOBAN........1100
      DIMENSION VOL(NN),PMAT(NELT,NCBI),PVEC(NNVEC)                      GLOBAN........1200
      DIMENSION UMAT(NELT,NCBI),UVEC(NNVEC)                              GLOBAN........1300
      DIMENSION IN(NIN)                                                  GLOBAN........1400
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  GLOBAN........1500
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             GLOBAN........1600
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              GLOBAN........1700
     1   NSOP,NSOU,NBCN                                                  GLOBAN........1800
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   GLOBAN........1900
      COMMON /DIMX2/ NELTA,NNVEC,NDIMJA                                  GLOBAN........2000
C                                                                        GLOBAN........2100
      N1=(L-1)*N48+1                                                     GLOBAN........2200
      N8=N1+N48-1                                                        GLOBAN........2300
C                                                                        GLOBAN........2400
C.....ADD RESULTS OF INTEGRATIONS OVER ELEMENT L TO GLOBAL               GLOBAN........2500
C        P-MATRIX AND P-VECTOR                                           GLOBAN........2600
      IF(ML-1) 9050,9050,9150                                            GLOBAN........2700
 9050 IE=0                                                               GLOBAN........2800
      DO 9100 II=N1,N8                                                   GLOBAN........2900
      IE=IE+1                                                            GLOBAN........3000
      IB=IN(II)                                                          GLOBAN........3100
      VOL(IB)=VOL(IB)+VOLE(IE)                                           GLOBAN........3200
      PVEC(IB)=PVEC(IB)+DFLOWE(IE)                                       GLOBAN........3300
      JE=0                                                               GLOBAN........3400
      DO 9100 JJ=N1,N8                                                   GLOBAN........3500
      JE=JE+1                                                            GLOBAN........3600
      JB=IN(JJ)-IB+NBHALF                                                GLOBAN........3700
 9100 PMAT(IB,JB)=PMAT(IB,JB)+BFLOWE(IE,JE)                              GLOBAN........3800
      IF(ML-1) 9150,9300,9150                                            GLOBAN........3900
C                                                                        GLOBAN........4000
C.....ADD RESULTS OF INTEGRATIONS OVER ELEMENT L TO GLOBAL               GLOBAN........4100
C        U-MATRIX                                                        GLOBAN........4200
 9150 IF(NOUMAT.EQ.1) GOTO 9300                                          GLOBAN........4300
      IE=0                                                               GLOBAN........4400
      DO 9200 II=N1,N8                                                   GLOBAN........4500
      IE=IE+1                                                            GLOBAN........4600
      IB=IN(II)                                                          GLOBAN........4700
C.....POSITION FOR ADDITION TO U-VECTOR                                  GLOBAN........4800
C        UVEC(IB)=UVEC(IB)+ ((   ))                                      GLOBAN........4900
      JE=0                                                               GLOBAN........5000
      DO 9200 JJ=N1,N8                                                   GLOBAN........5100
      JE=JE+1                                                            GLOBAN........5200
      JB=IN(JJ)-IB+NBHALF                                                GLOBAN........5300
 9200 UMAT(IB,JB)=UMAT(IB,JB)+DTRANE(IE,JE)+BTRANE(IE,JE)                GLOBAN........5400
C                                                                        GLOBAN........5500
 9300 CONTINUE                                                           GLOBAN........5600
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  GLOBAN........5700
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  GLOBAN........5800
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  GLOBAN........5900
C                                                                        GLOBAN........6000
C                                                                        GLOBAN........6100
      RETURN                                                             GLOBAN........6200
      END                                                                GLOBAN........6300
C                                                                        GLOBAN........6400
C     SUBROUTINE        G  L  O  T  R  I           SUTRA VERSION 2D3D.1  GLOTRI.........100
C                                                                        GLOTRI.........200
C *** PURPOSE :                                                          GLOTRI.........300
C ***  TO ASSEMBLE RESULTS OF ELEMENTWISE INTEGRATIONS INTO              GLOTRI.........400
C ***  A GLOBAL "SLAP TRIAD"-FORMAT MATRIX AND GLOBAL VECTOR             GLOTRI.........500
C ***  FOR BOTH FLOW AND TRANSPORT EQUATIONS.                            GLOTRI.........600
C                                                                        GLOTRI.........700
      SUBROUTINE GLOTRI(L,ML,VOLE,BFLOWE,DFLOWE,BTRANE,DTRANE,           GLOTRI.........800
     1      IN,VOL,PMAT,PVEC,UMAT,UVEC,NBI27,MIOFF)                      GLOTRI.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                GLOTRI........1000
      DIMENSION BFLOWE(8,8),DFLOWE(8),BTRANE(8,8),DTRANE(8,8),VOLE(8)    GLOTRI........1100
      DIMENSION VOL(NN),PMAT(NELT,NCBI),PVEC(NNVEC)                      GLOTRI........1200
      DIMENSION UMAT(NELT,NCBI),UVEC(NNVEC)                              GLOTRI........1300
      DIMENSION IN(NIN),NBI27(NBIX),MIOFF(27)                            GLOTRI........1400
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  GLOTRI........1500
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             GLOTRI........1600
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              GLOTRI........1700
     1   NSOP,NSOU,NBCN                                                  GLOTRI........1800
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   GLOTRI........1900
      COMMON /DIMX2/ NELTA, NNVEC, NDIMJA                                GLOTRI........2000
C                                                                        GLOTRI........2100
      N1=(L-1)*N48+1                                                     GLOTRI........2200
      N8=N1+N48-1                                                        GLOTRI........2300
C                                                                        GLOTRI........2400
C.....ADD RESULTS OF INTEGRATIONS OVER ELEMENT L TO GLOBAL               GLOTRI........2500
C        P-MATRIX AND P-VECTOR                                           GLOTRI........2600
      IF(ML-1) 9050,9050,9150                                            GLOTRI........2700
 9050 IE=0                                                               GLOTRI........2800
      DO 9100 II=N1,N8                                                   GLOTRI........2900
      IE=IE+1                                                            GLOTRI........3000
      IB=IN(II)                                                          GLOTRI........3100
      VOL(IB)=VOL(IB)+VOLE(IE)                                           GLOTRI........3200
      PVEC(IB)=PVEC(IB)+DFLOWE(IE)                                       GLOTRI........3300
      JE=0                                                               GLOTRI........3400
      DO 9100 JJ=N1,N8                                                   GLOTRI........3500
      JE=JE+1                                                            GLOTRI........3600
      JB=IN(JJ)-IB+NBHALF                                                GLOTRI........3700
      J27 = NBI27(JB)                                                    GLOTRI........3800
      M = MIOFF(J27) + IB                                                GLOTRI........3900
 9100 PMAT(M,1)=PMAT(M,1)+BFLOWE(IE,JE)                                  GLOTRI........4000
      IF(ML-1) 9150,9300,9150                                            GLOTRI........4100
C                                                                        GLOTRI........4200
C.....ADD RESULTS OF INTEGRATIONS OVER ELEMENT L TO GLOBAL               GLOTRI........4300
C        U-MATRIX                                                        GLOTRI........4400
 9150 IF(NOUMAT.EQ.1) GOTO 9300                                          GLOTRI........4500
      IE=0                                                               GLOTRI........4600
      DO 9200 II=N1,N8                                                   GLOTRI........4700
      IE=IE+1                                                            GLOTRI........4800
      IB=IN(II)                                                          GLOTRI........4900
C.....POSITION FOR ADDITION TO U-VECTOR                                  GLOTRI........5000
C        UVEC(IB)=UVEC(IB)+ ((   ))                                      GLOTRI........5100
      JE=0                                                               GLOTRI........5200
      DO 9200 JJ=N1,N8                                                   GLOTRI........5300
      JE=JE+1                                                            GLOTRI........5400
      JB=IN(JJ)-IB+NBHALF                                                GLOTRI........5500
      J27 = NBI27(JB)                                                    GLOTRI........5600
      M = MIOFF(J27) + IB                                                GLOTRI........5700
 9200 UMAT(M,1)=UMAT(M,1)+DTRANE(IE,JE)+BTRANE(IE,JE)                    GLOTRI........5800
C                                                                        GLOTRI........5900
 9300 CONTINUE                                                           GLOTRI........6000
 9999 CONTINUE                                                           GLOTRI........6100
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  GLOTRI........6200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  GLOTRI........6300
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  GLOTRI........6400
C                                                                        GLOTRI........6500
C                                                                        GLOTRI........6600
      RETURN                                                             GLOTRI........6700
      END                                                                GLOTRI........6800
C                                                                        GLOTRI........6900
C     SUBROUTINE        I  N  D  A  T  0           SUTRA VERSION 2D3D.1  INDAT0.........100
C                                                                        INDAT0.........200
C *** PURPOSE :                                                          INDAT0.........300
C ***  TO INPUT, OUTPUT, AND ORGANIZE A PORTION OF THE INP FILE          INDAT0.........400
C ***  INPUT DATA (DATASETS 5 THROUGH 7)                                 INDAT0.........500
C                                                                        INDAT0.........600
      SUBROUTINE INDAT0()                                                INDAT0.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                INDAT0.........800
      CHARACTER INTFIL*1000                                              INDAT0.........900
      CHARACTER*10 ADSMOD,CDUM10                                         INDAT0........1000
      CHARACTER SOLWRD(0:10)*10,SOLNAM(0:10)*40                          INDAT0........1100
      CHARACTER*14 UTYPE(2)                                              INDAT0........1200
      CHARACTER*6 STYPE(2)                                               INDAT0........1300
      CHARACTER*10 CSOLVP,CSOLVU                                         INDAT0........1400
      CHARACTER*80 ERRCOD,CHERR(10),FNAME(0:7)                           INDAT0........1500
      DIMENSION INERR(10),RLERR(10)                                      INDAT0........1600
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  INDAT0........1700
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             INDAT0........1800
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              INDAT0........1900
     1   NSOP,NSOU,NBCN                                                  INDAT0........2000
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   INDAT0........2100
      COMMON /DIMX2/ NELTA,NNVEC,NDIMJA                                  INDAT0........2200
      COMMON /ERRHAN/ ISERR                                              INDAT0........2300
      COMMON /FNAMES/ FNAME                                              INDAT0........2400
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        INDAT0........2500
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  INDAT0........2600
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      INDAT0........2700
      COMMON /ITSOLI/ ITRMXP,ITOLP,NSAVEP,ITRMXU,ITOLU,NSAVEU            INDAT0........2800
      COMMON /ITSOLR/ TOLP,TOLU                                          INDAT0........2900
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     INDAT0........3000
     1   KSCRN,KPAUSE                                                    INDAT0........3100
      COMMON /MODSOR/ ADSMOD                                             INDAT0........3200
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      INDAT0........3300
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        INDAT0........3400
      COMMON /SOLVC/ SOLWRD,SOLNAM                                       INDAT0........3500
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           INDAT0........3600
      COMMON /SOLVN/ NSLVRS                                              INDAT0........3700
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       INDAT0........3800
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  INDAT0........3900
      DATA UTYPE(1)/' TEMPERATURES '/,UTYPE(2)/'CONCENTRATIONS'/         INDAT0........4000
      DATA STYPE(1)/'ENERGY'/,STYPE(2)/'SOLUTE'/                         INDAT0........4100
      SAVE UTYPE,STYPE                                                   INDAT0........4200
C                                                                        INDAT0........4300
      INSTOP=0                                                           INDAT0........4400
C                                                                        INDAT0........4500
C.....INPUT DATASET 5: NUMERICAL CONTROL PARAMETERS                      INDAT0........4600
      ERRCOD = 'REA-INP-S5'                                              INDAT0........4700
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT0........4800
      IF (ISERR) RETURN                                                  INDAT0........4900
      ERRCOD = 'REA-INP-5'                                               INDAT0........5000
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT0........5100
      IF (ISERR) RETURN                                                  INDAT0........5200
      READ(INTFIL,*,IOSTAT=INERR(1)) UP,GNUP,GNUU                        INDAT0........5300
      IF (INERR(1).NE.0) THEN                                            INDAT0........5400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0........5500
         RETURN                                                          INDAT0........5600
      END IF                                                             INDAT0........5700
      IF(ME.EQ.-1) WRITE(K3,70) UP,GNUP,GNUU                             INDAT0........5800
   70 FORMAT(////11X,'N U M E R I C A L   C O N T R O L   D A T A'//     INDAT0........5900
     1   11X,F15.5,5X,'"UPSTREAM WEIGHTING" FACTOR'/                     INDAT0........6000
     2   11X,1PD15.4,5X,'SPECIFIED PRESSURE BOUNDARY CONDITION FACTOR'/  INDAT0........6100
     3   11X,1PD15.4,5X,'SPECIFIED CONCENTRATION BOUNDARY CONDITION ',   INDAT0........6200
     4   'FACTOR')                                                       INDAT0........6300
      IF(ME.EQ.+1) WRITE(K3,80) UP,GNUP,GNUU                             INDAT0........6400
   80 FORMAT(////11X,'N U M E R I C A L   C O N T R O L   D A T A'//     INDAT0........6500
     1   11X,F15.5,5X,'"UPSTREAM WEIGHTING" FACTOR'/                     INDAT0........6600
     2   11X,1PD15.4,5X,'SPECIFIED PRESSURE BOUNDARY CONDITION FACTOR'/  INDAT0........6700
     3   11X,1PD15.4,5X,'SPECIFIED TEMPERATURE BOUNDARY CONDITION ',     INDAT0........6800
     4   'FACTOR')                                                       INDAT0........6900
C                                                                        INDAT0........7000
C.....INPUT DATASET 6: TEMPORAL CONTROL AND SOLUTION CYCLING DATA        INDAT0........7100
      ERRCOD = 'REA-INP-S6'                                              INDAT0........7200
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT0........7300
      IF (ISERR) RETURN                                                  INDAT0........7400
      ERRCOD = 'REA-INP-6'                                               INDAT0........7500
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT0........7600
      IF (ISERR) RETURN                                                  INDAT0........7700
      READ(INTFIL,*,IOSTAT=INERR(1)) ITMAX,DELT,TMAX,ITCYC,DTMULT,DTMAX, INDAT0........7800
     1   NPCYC,NUCYC                                                     INDAT0........7900
      IF (INERR(1).NE.0) THEN                                            INDAT0........8000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0........8100
         RETURN                                                          INDAT0........8200
      END IF                                                             INDAT0........8300
      WRITE(K3,120) ITMAX,DELT,TMAX,ITCYC,DTMULT,DTMAX,NPCYC,NUCYC       INDAT0........8400
  120 FORMAT(1H1////11X,'T E M P O R A L   C O N T R O L   A N D   ',    INDAT0........8500
     1   'S O L U T I O N   C Y C L I N G   D A T A',                    INDAT0........8600
     2   //11X,I15,5X,'MAXIMUM ALLOWED NUMBER OF TIME STEPS'             INDAT0........8700
     3   /11X,1PD15.4,5X,'INITIAL TIME STEP (IN SECONDS)'                INDAT0........8800
     4   /11X,1PD15.4,5X,'MAXIMUM ALLOWED SIMULATION TIME (IN SECONDS)'  INDAT0........8900
     5   //11X,I15,5X,'TIME STEP MULTIPLIER CYCLE (IN TIME STEPS)'       INDAT0........9000
     6   /11X,0PF15.5,5X,'MULTIPLICATION FACTOR FOR TIME STEP CHANGE'    INDAT0........9100
     7   /11X,1PD15.4,5X,'MAXIMUM ALLOWED TIME STEP (IN SECONDS)'        INDAT0........9200
     8   //11X,I15,5X,'FLOW SOLUTION CYCLE (IN TIME STEPS)'              INDAT0........9300
     9   /11X,I15,5X,'TRANSPORT SOLUTION CYCLE (IN TIME STEPS)')         INDAT0........9400
      IF(NPCYC.GE.1.AND.NUCYC.GE.1) GOTO 140                             INDAT0........9500
      ERRCOD = 'INP-6-1'                                                 INDAT0........9600
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           INDAT0........9700
      RETURN                                                             INDAT0........9800
  140 IF(NPCYC.EQ.1.OR.NUCYC.EQ.1) GOTO 160                              INDAT0........9900
      ERRCOD = 'INP-6-2'                                                 INDAT0.......10000
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           INDAT0.......10100
      RETURN                                                             INDAT0.......10200
  160 IF (DELT.LE.DTMAX) GOTO 180                                        INDAT0.......10300
      ERRCOD = 'INP-6-3'                                                 INDAT0.......10400
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           INDAT0.......10500
      RETURN                                                             INDAT0.......10600
  180 CONTINUE                                                           INDAT0.......10700
C.....SET MAXIMUM ALLOWED TIME STEPS IN SIMULATION FOR                   INDAT0.......10800
C        STEADY-STATE FLOW AND STEADY-STATE TRANSPORT SOLUTION MODES     INDAT0.......10900
      IF(ISSFLO.EQ.1) THEN                                               INDAT0.......11000
         NPCYC=ITMAX+1                                                   INDAT0.......11100
         NUCYC=1                                                         INDAT0.......11200
      END IF                                                             INDAT0.......11300
      IF(ISSTRA.EQ.1) ITMAX=1                                            INDAT0.......11400
C                                                                        INDAT0.......11500
C.....INPUT DATASET 7A:  ITERATION CONTROLS FOR RESOLVING NONLINEARITIES INDAT0.......11600
      ERRCOD = 'REA-INP-S7A'                                             INDAT0.......11700
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT0.......11800
      IF (ISERR) RETURN                                                  INDAT0.......11900
      ERRCOD = 'REA-INP-7A'                                              INDAT0.......12000
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT0.......12100
      IF (ISERR) RETURN                                                  INDAT0.......12200
      READ(INTFIL,*,IOSTAT=INERR(1)) ITRMAX                              INDAT0.......12300
      IF (INERR(1).NE.0) THEN                                            INDAT0.......12400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0.......12500
         RETURN                                                          INDAT0.......12600
      END IF                                                             INDAT0.......12700
      IF (ITRMAX.GT.1) THEN                                              INDAT0.......12800
         ERRCOD = 'REA-INP-7A'                                           INDAT0.......12900
         READ(INTFIL,*,IOSTAT=INERR(1)) ITRMAX,RPMAX,RUMAX               INDAT0.......13000
         IF (INERR(1).NE.0) THEN                                         INDAT0.......13100
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT0.......13200
            RETURN                                                       INDAT0.......13300
         END IF                                                          INDAT0.......13400
      END IF                                                             INDAT0.......13500
      IF(ITRMAX-1) 192,192,194                                           INDAT0.......13600
  192 WRITE(K3,193)                                                      INDAT0.......13700
  193 FORMAT(////11X,'I T E R A T I O N   C O N T R O L   D A T A',      INDAT0.......13800
     1   //11X,'  NON-ITERATIVE SOLUTION')                               INDAT0.......13900
      GOTO 196                                                           INDAT0.......14000
  194 WRITE(K3,195) ITRMAX,RPMAX,RUMAX                                   INDAT0.......14100
  195 FORMAT(////11X,'I T E R A T I O N   C O N T R O L   D A T A',      INDAT0.......14200
     1   //11X,I15,5X,'MAXIMUM NUMBER OF ITERATIONS PER TIME STEP',      INDAT0.......14300
     2   /11X,1PD15.4,5X,'ABSOLUTE CONVERGENCE CRITERION FOR FLOW',      INDAT0.......14400
     3   ' SOLUTION'/11X,1PD15.4,5X,'ABSOLUTE CONVERGENCE CRITERION',    INDAT0.......14500
     4   ' FOR TRANSPORT SOLUTION')                                      INDAT0.......14600
  196 CONTINUE                                                           INDAT0.......14700
C                                                                        INDAT0.......14800
C.....INPUT DATASETS 7B & 7C:  MATRIX EQUATION SOLVER CONTROLS FOR       INDAT0.......14900
C        PRESSURE AND TRANSPORT SOLUTIONS                                INDAT0.......15000
      ERRCOD = 'REA-INP-S7B'                                             INDAT0.......15100
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT0.......15200
      IF (ISERR) RETURN                                                  INDAT0.......15300
      ERRCOD = 'REA-INP-7B'                                              INDAT0.......15400
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT0.......15500
      IF (ISERR) RETURN                                                  INDAT0.......15600
      READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVP                              INDAT0.......15700
      IF (INERR(1).NE.0) THEN                                            INDAT0.......15800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0.......15900
         RETURN                                                          INDAT0.......16000
      END IF                                                             INDAT0.......16100
      IF ((CSOLVP.NE.SOLWRD(0))) THEN                                    INDAT0.......16200
         ERRCOD = 'REA-INP-7B'                                           INDAT0.......16300
         READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVP,ITRMXP,TOLP               INDAT0.......16400
         IF (INERR(1).NE.0) THEN                                         INDAT0.......16500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT0.......16600
            RETURN                                                       INDAT0.......16700
         END IF                                                          INDAT0.......16800
      END IF                                                             INDAT0.......16900
      ERRCOD = 'REA-INP-S7C'                                             INDAT0.......17000
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT0.......17100
      IF (ISERR) RETURN                                                  INDAT0.......17200
      ERRCOD = 'REA-INP-7C'                                              INDAT0.......17300
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT0.......17400
      IF (ISERR) RETURN                                                  INDAT0.......17500
      READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVU                              INDAT0.......17600
      IF (INERR(1).NE.0) THEN                                            INDAT0.......17700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0.......17800
         RETURN                                                          INDAT0.......17900
      END IF                                                             INDAT0.......18000
      IF ((CSOLVU.NE.SOLWRD(0))) THEN                                    INDAT0.......18100
         ERRCOD = 'REA-INP-7C'                                           INDAT0.......18200
         READ(INTFIL,*,IOSTAT=INERR(1)) CSOLVU,ITRMXU,TOLU               INDAT0.......18300
         IF (INERR(1).NE.0) THEN                                         INDAT0.......18400
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT0.......18500
            RETURN                                                       INDAT0.......18600
         END IF                                                          INDAT0.......18700
      END IF                                                             INDAT0.......18800
      KSOLVP = -1                                                        INDAT0.......18900
      KSOLVU = -1                                                        INDAT0.......19000
      DO 250 M=0,NSLVRS-1                                                INDAT0.......19100
         IF (CSOLVP.EQ.SOLWRD(M)) KSOLVP = M                             INDAT0.......19200
         IF (CSOLVU.EQ.SOLWRD(M)) KSOLVU = M                             INDAT0.......19300
  250 CONTINUE                                                           INDAT0.......19400
      IF ((KSOLVP.LT.0).OR.(KSOLVU.LT.0)) THEN                           INDAT0.......19500
         ERRCOD = 'INP-7B&C-1'                                           INDAT0.......19600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0.......19700
         RETURN                                                          INDAT0.......19800
      ELSE IF ((KSOLVP*KSOLVU.EQ.0).AND.(KSOLVP+KSOLVU.NE.0)) THEN       INDAT0.......19900
         ERRCOD = 'INP-7B&C-2'                                           INDAT0.......20000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0.......20100
         RETURN                                                          INDAT0.......20200
      ELSE IF ((KSOLVU.EQ.1).OR.((KSOLVP.EQ.1).AND.(UP.NE.0D0))) THEN    INDAT0.......20300
         ERRCOD = 'INP-7B&C-3'                                           INDAT0.......20400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT0.......20500
         RETURN                                                          INDAT0.......20600
      END IF                                                             INDAT0.......20700
      IF (KSOLVP.EQ.2) THEN                                              INDAT0.......20800
         ITOLP = 0                                                       INDAT0.......20900
      ELSE                                                               INDAT0.......21000
         ITOLP = 1                                                       INDAT0.......21100
      END IF                                                             INDAT0.......21200
      IF (KSOLVU.EQ.2) THEN                                              INDAT0.......21300
         ITOLU = 0                                                       INDAT0.......21400
      ELSE                                                               INDAT0.......21500
         ITOLU = 1                                                       INDAT0.......21600
      END IF                                                             INDAT0.......21700
      NSAVEP = 10                                                        INDAT0.......21800
      NSAVEU = 10                                                        INDAT0.......21900
C                                                                        INDAT0.......22000
C                                                                        INDAT0.......22100
 1000 RETURN                                                             INDAT0.......22200
      END                                                                INDAT0.......22300
C     SUBROUTINE        I  N  D  A  T  1           SUTRA VERSION 2D3D.1  INDAT1.........100
C                                                                        INDAT1.........200
C *** PURPOSE :                                                          INDAT1.........300
C ***  TO INPUT, OUTPUT, AND ORGANIZE A MAJOR PORTION OF INP FILE        INDAT1.........400
C ***  INPUT DATA (DATASETS 8 THROUGH 15)                                INDAT1.........500
C                                                                        INDAT1.........600
      SUBROUTINE INDAT1(X,Y,Z,POR,ALMAX,ALMID,ALMIN,ATMAX,ATMID,         INDAT1.........700
     1   ATMIN,PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,                       INDAT1.........800
     2   PERMYZ,PERMZX,PERMZY,PERMZZ,PANGL1,PANGL2,PANGL3,SOP,NREG,LREG, INDAT1.........900
     3   IOBS)                                                           INDAT1........1000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                INDAT1........1100
      PARAMETER (NCOLMX=9)                                               INDAT1........1200
      CHARACTER*10 ADSMOD,CDUM10                                         INDAT1........1300
      CHARACTER*14 UTYPE(2)                                              INDAT1........1400
      CHARACTER*6 STYPE(2)                                               INDAT1........1500
      CHARACTER K5SYM(7)*1, NCOL(NCOLMX)*1, VARNK5(7)*25                 INDAT1........1600
      CHARACTER K6SYM(7)*2, LCOL(NCOLMX)*2, VARNK6(7)*25                 INDAT1........1700
      CHARACTER*1 CNODAL,CELMNT,CINCID,CVEL,CBUDG,CSCRN,CPAUSE           INDAT1........1800
      CHARACTER*80 ERRCOD,CHERR(10),FNAME(0:7)                           INDAT1........1900
      CHARACTER INTFIL*1000                                              INDAT1........2000
      DIMENSION IOBS(NOBSN)                                              INDAT1........2100
      DIMENSION J5COL(NCOLMX), J6COL(NCOLMX)                             INDAT1........2200
      DIMENSION X(NN),Y(NN),Z(NN),POR(NN),SOP(NN),NREG(NN)               INDAT1........2300
      DIMENSION PERMXX(NE),PERMXY(NE),PERMXZ(NEX),PERMYX(NE),PERMYY(NE), INDAT1........2400
     1   PERMYZ(NEX),PERMZX(NEX),PERMZY(NEX),PERMZZ(NEX),PANGL1(NE),     INDAT1........2500
     2   PANGL2(NEX),PANGL3(NEX),ALMAX(NE),ALMID(NEX),ALMIN(NE),         INDAT1........2600
     3   ATMAX(NE),ATMID(NEX),ATMIN(NE),LREG(NE)                         INDAT1........2700
      DIMENSION INERR(10),RLERR(10)                                      INDAT1........2800
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  INDAT1........2900
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             INDAT1........3000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              INDAT1........3100
     1   NSOP,NSOU,NBCN                                                  INDAT1........3200
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   INDAT1........3300
      COMMON /ERRHAN/ ISERR                                              INDAT1........3400
      COMMON /FNAMES/ FNAME                                              INDAT1........3500
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        INDAT1........3600
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  INDAT1........3700
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      INDAT1........3800
      COMMON /JCOLS/ NCOLPR, LCOLPR, NCOLS5, NCOLS6, J5COL, J6COL        INDAT1........3900
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     INDAT1........4000
     1   KSCRN,KPAUSE                                                    INDAT1........4100
      COMMON /MODSOR/ ADSMOD                                             INDAT1........4200
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC                                    INDAT1........4300
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      INDAT1........4400
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        INDAT1........4500
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       INDAT1........4600
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  INDAT1........4700
      DATA UTYPE(1)/' TEMPERATURES '/,UTYPE(2)/'CONCENTRATIONS'/         INDAT1........4800
      DATA STYPE(1)/'ENERGY'/,STYPE(2)/'SOLUTE'/                         INDAT1........4900
      DATA (K5SYM(MM), MM=1,7) /'N', 'X', 'Y', 'Z', 'P', 'U', 'S'/       INDAT1........5000
      DATA (VARNK5(MM), MM=1,7) /'NODE NUMBER',                          INDAT1........5100
     1   'X-COORDINATE', 'Y-COORDINATE', 'Z-COORDINATE',                 INDAT1........5200
     2   'PRESSURE', 'CONCENTRATION/TEMPERATURE', 'SATURATION'/          INDAT1........5300
      DATA (K6SYM(MM), MM=1,7) /'E', 'X', 'Y', 'Z', 'VX', 'VY', 'VZ'/    INDAT1........5400
      DATA (VARNK6(MM), MM=1,7) /'ELEMENT NUMBER',                       INDAT1........5500
     1   'X-COORDINATE OF CENTROID', 'Y-COORDINATE OF CENTROID',         INDAT1........5600
     2   'Z-COORDINATE OF CENTROID', 'X-VELOCITY', 'Y-VELOCITY',         INDAT1........5700
     3   'Z-VELOCITY'/                                                   INDAT1........5800
      SAVE UTYPE,STYPE,K5SYM,VARNK5,K6SYM,VARNK6                         INDAT1........5900
C                                                                        INDAT1........6000
      INSTOP=0                                                           INDAT1........6100
C                                                                        INDAT1........6200
C.....INPUT DATASET 8A:  OUTPUT CONTROLS AND OPTIONS FOR LST FILE        INDAT1........6300
C        AND SCREEN                                                      INDAT1........6400
      ERRCOD = 'REA-INP-S8A'                                             INDAT1........6500
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1........6600
      IF (ISERR) RETURN                                                  INDAT1........6700
      ERRCOD = 'REA-INP-8A'                                              INDAT1........6800
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1........6900
      IF (ISERR) RETURN                                                  INDAT1........7000
      READ(INTFIL,*,IOSTAT=INERR(1)) NPRINT,CNODAL,CELMNT,CINCID,        INDAT1........7100
     1   CVEL,CBUDG,CSCRN,CPAUSE                                         INDAT1........7200
      IF (INERR(1).NE.0) THEN                                            INDAT1........7300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1........7400
         RETURN                                                          INDAT1........7500
      END IF                                                             INDAT1........7600
      IF (CNODAL.EQ.'Y') THEN                                            INDAT1........7700
         KNODAL = +1                                                     INDAT1........7800
      ELSE IF (CNODAL.EQ.'N') THEN                                       INDAT1........7900
         KNODAL = 0                                                      INDAT1........8000
      ELSE                                                               INDAT1........8100
         ERRCOD = 'INP-8A-1'                                             INDAT1........8200
         CHERR(1) = 'CNODAL '                                            INDAT1........8300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1........8400
         RETURN                                                          INDAT1........8500
      END IF                                                             INDAT1........8600
      IF (CELMNT.EQ.'Y') THEN                                            INDAT1........8700
         KELMNT = +1                                                     INDAT1........8800
      ELSE IF (CELMNT.EQ.'N') THEN                                       INDAT1........8900
         KELMNT = 0                                                      INDAT1........9000
      ELSE                                                               INDAT1........9100
         ERRCOD = 'INP-8A-2'                                             INDAT1........9200
         CHERR(1) = 'CELMNT'                                             INDAT1........9300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1........9400
         RETURN                                                          INDAT1........9500
      END IF                                                             INDAT1........9600
      IF (CINCID.EQ.'Y') THEN                                            INDAT1........9700
         KINCID = +1                                                     INDAT1........9800
      ELSE IF (CINCID.EQ.'N') THEN                                       INDAT1........9900
         KINCID = 0                                                      INDAT1.......10000
      ELSE                                                               INDAT1.......10100
         ERRCOD = 'INP-8A-3'                                             INDAT1.......10200
         CHERR(1) = 'CINCID'                                             INDAT1.......10300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......10400
         RETURN                                                          INDAT1.......10500
      END IF                                                             INDAT1.......10600
      IF (CVEL.EQ.'Y') THEN                                              INDAT1.......10700
         KVEL = +1                                                       INDAT1.......10800
      ELSE IF (CVEL.EQ.'N') THEN                                         INDAT1.......10900
         KVEL = 0                                                        INDAT1.......11000
      ELSE                                                               INDAT1.......11100
         ERRCOD = 'INP-8A-4'                                             INDAT1.......11200
         CHERR(1) = 'CVEL  '                                             INDAT1.......11300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......11400
         RETURN                                                          INDAT1.......11500
      END IF                                                             INDAT1.......11600
      IF (CBUDG.EQ.'Y') THEN                                             INDAT1.......11700
         KBUDG = +1                                                      INDAT1.......11800
      ELSE IF (CBUDG.EQ.'N') THEN                                        INDAT1.......11900
         KBUDG = 0                                                       INDAT1.......12000
      ELSE                                                               INDAT1.......12100
         ERRCOD = 'INP-8A-5'                                             INDAT1.......12200
         CHERR(1) = 'CBUDG '                                             INDAT1.......12300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......12400
         RETURN                                                          INDAT1.......12500
      END IF                                                             INDAT1.......12600
      IF (CSCRN.EQ.'Y') THEN                                             INDAT1.......12700
         KSCRN = +1                                                      INDAT1.......12800
      ELSE IF (CSCRN.EQ.'N') THEN                                        INDAT1.......12900
         KSCRN = 0                                                       INDAT1.......13000
      ELSE                                                               INDAT1.......13100
         ERRCOD = 'INP-8A-6'                                             INDAT1.......13200
         CHERR(1) = 'CSCRN '                                             INDAT1.......13300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......13400
         RETURN                                                          INDAT1.......13500
      END IF                                                             INDAT1.......13600
      IF (CPAUSE.EQ.'Y') THEN                                            INDAT1.......13700
         KPAUSE = +1                                                     INDAT1.......13800
      ELSE IF (CPAUSE.EQ.'N') THEN                                       INDAT1.......13900
         KPAUSE = 0                                                      INDAT1.......14000
      ELSE                                                               INDAT1.......14100
         ERRCOD = 'INP-8A-7'                                             INDAT1.......14200
         CHERR(1) = 'CPAUSE'                                             INDAT1.......14300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......14400
         RETURN                                                          INDAT1.......14500
      END IF                                                             INDAT1.......14600
C                                                                        INDAT1.......14700
      WRITE(K3,72) NPRINT                                                INDAT1.......14800
   72 FORMAT(////11X,'O U T P U T   C O N T R O L S   A N D   ',         INDAT1.......14900
     1   'O P T I O N S'//13X,'.LST FILE'/13X,'---------'                INDAT1.......15000
     2   //13X,I6,5X,'PRINTED OUTPUT CYCLE (IN TIME STEPS)')             INDAT1.......15100
      IF(KNODAL.EQ.+1) WRITE(K3,74)                                      INDAT1.......15200
      IF(KNODAL.EQ.0) WRITE(K3,75)                                       INDAT1.......15300
   74 FORMAT(/13X,'- PRINT NODE COORDINATES, THICKNESSES AND',           INDAT1.......15400
     1   ' POROSITIES')                                                  INDAT1.......15500
   75 FORMAT(/13X,'- CANCEL PRINT OF NODE COORDINATES, THICKNESSES AND', INDAT1.......15600
     1   ' POROSITIES')                                                  INDAT1.......15700
      IF(KELMNT.EQ.+1) WRITE(K3,76)                                      INDAT1.......15800
      IF(KELMNT.EQ.0) WRITE(K3,77)                                       INDAT1.......15900
   76 FORMAT(13X,'- PRINT ELEMENT PERMEABILITIES AND DISPERSIVITIES')    INDAT1.......16000
   77 FORMAT(13X,'- CANCEL PRINT OF ELEMENT PERMEABILITIES AND ',        INDAT1.......16100
     1   'DISPERSIVITIES')                                               INDAT1.......16200
      IF(KINCID.EQ.+1) WRITE(K3,78)                                      INDAT1.......16300
      IF(KINCID.EQ.0) WRITE(K3,79)                                       INDAT1.......16400
   78 FORMAT(13X,'- PRINT NODE INCIDENCES IN EACH ELEMENT')              INDAT1.......16500
   79 FORMAT(13X,'- CANCEL PRINT OF NODE INCIDENCES IN EACH ELEMENT')    INDAT1.......16600
      IME=2                                                              INDAT1.......16700
      IF(ME.EQ.+1) IME=1                                                 INDAT1.......16800
      IF(KVEL.EQ.+1) WRITE(K3,84)                                        INDAT1.......16900
      IF(KVEL.EQ.0) WRITE(K3,85)                                         INDAT1.......17000
   84 FORMAT(/13X,'- CALCULATE AND PRINT VELOCITIES AT ELEMENT ',        INDAT1.......17100
     1   'CENTROIDS ON EACH TIME STEP WITH OUTPUT')                      INDAT1.......17200
   85 FORMAT(/13X,'- CANCEL PRINT OF VELOCITIES')                        INDAT1.......17300
      IF(KBUDG.EQ.+1) WRITE(K3,86) STYPE(IME)                            INDAT1.......17400
      IF(KBUDG.EQ.0) WRITE(K3,87)                                        INDAT1.......17500
   86 FORMAT(/13X,'- CALCULATE AND PRINT FLUID AND ',A6,' BUDGETS ',     INDAT1.......17600
     1   'ON EACH TIME STEP WITH OUTPUT')                                INDAT1.......17700
   87 FORMAT(/13X,'- CANCEL PRINT OF BUDGETS')                           INDAT1.......17800
C                                                                        INDAT1.......17900
C.....INPUT DATASET 8B:  OUTPUT CONTROLS AND OPTIONS FOR NOD FILE        INDAT1.......18000
      ERRCOD = 'REA-INP-S8B'                                             INDAT1.......18100
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......18200
      IF (ISERR) RETURN                                                  INDAT1.......18300
      ERRCOD = 'REA-INP-8B'                                              INDAT1.......18400
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......18500
      IF (ISERR) RETURN                                                  INDAT1.......18600
      READ(INTFIL,*,IOSTAT=INERR(1)) NCOLPR                              INDAT1.......18700
      IF (INERR(1).NE.0) THEN                                            INDAT1.......18800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......18900
         RETURN                                                          INDAT1.......19000
      END IF                                                             INDAT1.......19100
      DO 140 M=1,NCOLMX                                                  INDAT1.......19200
         READ(INTFIL,*,IOSTAT=INERR(1)) NCOLPR, (NCOL(MM), MM=1,M)       INDAT1.......19300
         IF (INERR(1).NE.0) THEN                                         INDAT1.......19400
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT1.......19500
            RETURN                                                       INDAT1.......19600
         END IF                                                          INDAT1.......19700
         IF (NCOL(M).EQ.'-') THEN                                        INDAT1.......19800
            NCOLS5 = M - 1                                               INDAT1.......19900
            GOTO 142                                                     INDAT1.......20000
         END IF                                                          INDAT1.......20100
  140 CONTINUE                                                           INDAT1.......20200
      NCOLS5 = NCOLMX                                                    INDAT1.......20300
  142 CONTINUE                                                           INDAT1.......20400
      WRITE(K3,144) NCOLPR                                               INDAT1.......20500
  144 FORMAT (//13X,'.NOD FILE'/13X,'---------'                          INDAT1.......20600
     1   //13X,I6,5X,'PRINTED OUTPUT CYCLE (IN TIME STEPS)'/)            INDAT1.......20700
      DO 148 M=1,NCOLS5                                                  INDAT1.......20800
         DO 146 MM=1,7                                                   INDAT1.......20900
            IF (NCOL(M).EQ.K5SYM(MM)) THEN                               INDAT1.......21000
               IF ((MM.EQ.1).AND.(M.NE.1)) THEN                          INDAT1.......21100
                  ERRCOD = 'INP-8B-1'                                    INDAT1.......21200
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT1.......21300
                  RETURN                                                 INDAT1.......21400
               END IF                                                    INDAT1.......21500
               IF ((MM.EQ.4).AND.(IABS(KTYPE).EQ.2)) THEN                INDAT1.......21600
                  ERRCOD = 'INP-8B-2'                                    INDAT1.......21700
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT1.......21800
                  RETURN                                                 INDAT1.......21900
               END IF                                                    INDAT1.......22000
               J5COL(M) = MM                                             INDAT1.......22100
               GOTO 148                                                  INDAT1.......22200
            END IF                                                       INDAT1.......22300
  146    CONTINUE                                                        INDAT1.......22400
         ERRCOD = 'INP-8B-3'                                             INDAT1.......22500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......22600
         RETURN                                                          INDAT1.......22700
  148 CONTINUE                                                           INDAT1.......22800
      WRITE(K3,150) (M,VARNK5(J5COL(M)),M=1,NCOLS5)                      INDAT1.......22900
  150 FORMAT (13X,'COLUMN ',I1,':',2X,A)                                 INDAT1.......23000
C                                                                        INDAT1.......23100
C.....INPUT DATASET 8C:  OUTPUT CONTROLS AND OPTIONS FOR ELE FILE        INDAT1.......23200
      ERRCOD = 'REA-INP-S8C'                                             INDAT1.......23300
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......23400
      IF (ISERR) RETURN                                                  INDAT1.......23500
      ERRCOD = 'REA-INP-8C'                                              INDAT1.......23600
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......23700
      IF (ISERR) RETURN                                                  INDAT1.......23800
      READ(INTFIL,*,IOSTAT=INERR(1)) LCOLPR                              INDAT1.......23900
      IF (INERR(1).NE.0) THEN                                            INDAT1.......24000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......24100
         RETURN                                                          INDAT1.......24200
      END IF                                                             INDAT1.......24300
      DO 160 M=1,NCOLMX                                                  INDAT1.......24400
         READ(INTFIL,*,IOSTAT=INERR(1)) LCOLPR, (LCOL(MM), MM=1,M)       INDAT1.......24500
         IF (INERR(1).NE.0) THEN                                         INDAT1.......24600
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT1.......24700
            RETURN                                                       INDAT1.......24800
         END IF                                                          INDAT1.......24900
         IF (LCOL(M).EQ.'-') THEN                                        INDAT1.......25000
            NCOLS6 = M - 1                                               INDAT1.......25100
            GOTO 162                                                     INDAT1.......25200
         END IF                                                          INDAT1.......25300
  160 CONTINUE                                                           INDAT1.......25400
      NCOLS6 = NCOLMX                                                    INDAT1.......25500
  162 CONTINUE                                                           INDAT1.......25600
      WRITE(K3,164) LCOLPR                                               INDAT1.......25700
  164 FORMAT (//13X,'.ELE FILE'/13X,'---------'                          INDAT1.......25800
     1   //13X,I6,5X,'PRINTED OUTPUT CYCLE (IN TIME STEPS)'/)            INDAT1.......25900
      DO 168 M=1,NCOLS6                                                  INDAT1.......26000
         DO 166 MM=1,7                                                   INDAT1.......26100
            IF (LCOL(M).EQ.K6SYM(MM)) THEN                               INDAT1.......26200
               IF ((MM.EQ.1).AND.(M.NE.1)) THEN                          INDAT1.......26300
                  ERRCOD = 'INP-8C-1'                                    INDAT1.......26400
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT1.......26500
                  RETURN                                                 INDAT1.......26600
               END IF                                                    INDAT1.......26700
               IF ((MM.EQ.4).AND.(IABS(KTYPE).EQ.2)) THEN                INDAT1.......26800
                  ERRCOD = 'INP-8C-2'                                    INDAT1.......26900
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT1.......27000
                  RETURN                                                 INDAT1.......27100
               END IF                                                    INDAT1.......27200
               IF ((MM.EQ.7).AND.(IABS(KTYPE).EQ.2)) THEN                INDAT1.......27300
                  ERRCOD = 'INP-8C-4'                                    INDAT1.......27400
                  CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)               INDAT1.......27500
                  RETURN                                                 INDAT1.......27600
               END IF                                                    INDAT1.......27700
               J6COL(M) = MM                                             INDAT1.......27800
               GOTO 168                                                  INDAT1.......27900
            END IF                                                       INDAT1.......28000
  166    CONTINUE                                                        INDAT1.......28100
         ERRCOD = 'INP-8C-3'                                             INDAT1.......28200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......28300
         RETURN                                                          INDAT1.......28400
  168 CONTINUE                                                           INDAT1.......28500
      WRITE(K3,170) (M,VARNK6(J6COL(M)),M=1,NCOLS6)                      INDAT1.......28600
  170 FORMAT (13X,'COLUMN ',I1,':',2X,A)                                 INDAT1.......28700
C                                                                        INDAT1.......28800
C.....INPUT DATASET 8D:  OUTPUT CONTROLS AND OPTIONS FOR OBS FILE        INDAT1.......28900
      NOBCYC = ITMAX + 1                                                 INDAT1.......29000
      IF (NOBSN-1.EQ.0) GOTO 199                                         INDAT1.......29100
      ERRCOD = 'REA-INP-S8D'                                             INDAT1.......29200
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......29300
      IF (ISERR) RETURN                                                  INDAT1.......29400
C.....NOBS IS ACTUAL NUMBER OF OBSERVATION NODES                         INDAT1.......29500
C.....NTOBS IS MAXIMUM NUMBER OF TIME STEPS WITH OBSERVATIONS            INDAT1.......29600
      NOBS=NOBSN-1                                                       INDAT1.......29700
      ERRCOD = 'REA-INP-8D'                                              INDAT1.......29800
      READ(K1,*,IOSTAT=INERR(1)) NOBCYC, (IOBS(JJ), JJ=1,NOBSN)          INDAT1.......29900
      IF (INERR(1).NE.0) THEN                                            INDAT1.......30000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......30100
         RETURN                                                          INDAT1.......30200
      END IF                                                             INDAT1.......30300
      WRITE(K3,180) NOBCYC                                               INDAT1.......30400
  180 FORMAT (//13X,'.OBS FILE'/13X,'---------'                          INDAT1.......30500
     1   //13X,I6,5X,'PRINTED OUTPUT CYCLE (IN TIME STEPS)'/)            INDAT1.......30600
      JSTOP=0                                                            INDAT1.......30700
      WRITE(K3,182)                                                      INDAT1.......30800
  182 FORMAT(////11X,'O B S E R V A T I O N   N O D E S')                INDAT1.......30900
      WRITE(K3,184) NOBCYC, NTOBS                                        INDAT1.......31000
  184 FORMAT(//13X,'OBSERVATIONS WILL BE MADE EVERY ',I5,' TIME STEPS,'  INDAT1.......31100
     1   /13X,'AS WELL AS ON THE FIRST AND LAST TIME STEP,'              INDAT1.......31200
     2   /13X,'FOR A TOTAL OF ',I5,' TIME STEPS.')                       INDAT1.......31300
      WRITE(K3,186)                                                      INDAT1.......31400
  186 FORMAT(//13X,'**** NODES AT WHICH OBSERVATIONS WILL BE MADE',      INDAT1.......31500
     1   ' ****'//)                                                      INDAT1.......31600
      IF (IOBS(NOBSN).NE.0) THEN                                         INDAT1.......31700
         ERRCOD = 'INP-8D-1'                                             INDAT1.......31800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......31900
         RETURN                                                          INDAT1.......32000
      END IF                                                             INDAT1.......32100
      DO 188 JJ=1,NOBS                                                   INDAT1.......32200
         IF (IOBS(JJ).LE.0) THEN                                         INDAT1.......32300
            ERRCOD = 'INP-8D-2'                                          INDAT1.......32400
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT1.......32500
            RETURN                                                       INDAT1.......32600
         END IF                                                          INDAT1.......32700
  188 CONTINUE                                                           INDAT1.......32800
      WRITE(K3,190) (IOBS(JJ),JJ=1,NOBS)                                 INDAT1.......32900
  190 FORMAT((13X,10(1X,I9)))                                            INDAT1.......33000
  199 CONTINUE                                                           INDAT1.......33100
C                                                                        INDAT1.......33200
C.....INPUT DATASET 9:  FLUID PROPERTIES                                 INDAT1.......33300
      ERRCOD = 'REA-INP-S9'                                              INDAT1.......33400
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......33500
      IF (ISERR) RETURN                                                  INDAT1.......33600
      ERRCOD = 'REA-INP-9'                                               INDAT1.......33700
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......33800
      IF (ISERR) RETURN                                                  INDAT1.......33900
      READ(INTFIL,*,IOSTAT=INERR(1)) COMPFL,CW,SIGMAW,RHOW0,URHOW0,      INDAT1.......34000
     1   DRWDU,VISC0                                                     INDAT1.......34100
      IF (INERR(1).NE.0) THEN                                            INDAT1.......34200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......34300
         RETURN                                                          INDAT1.......34400
      END IF                                                             INDAT1.......34500
C.....INPUT DATASET 10:  SOLID MATRIX PROPERTIES                         INDAT1.......34600
      ERRCOD = 'REA-INP-S10'                                             INDAT1.......34700
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......34800
      IF (ISERR) RETURN                                                  INDAT1.......34900
      ERRCOD = 'REA-INP-10'                                              INDAT1.......35000
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......35100
      IF (ISERR) RETURN                                                  INDAT1.......35200
      READ(INTFIL,*,IOSTAT=INERR(1)) COMPMA,CS,SIGMAS,RHOS               INDAT1.......35300
      IF (INERR(1).NE.0) THEN                                            INDAT1.......35400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......35500
         RETURN                                                          INDAT1.......35600
      END IF                                                             INDAT1.......35700
      IF(ME.EQ.+1)                                                       INDAT1.......35800
     1  WRITE(K3,210) COMPFL,COMPMA,CW,CS,VISC0,RHOS,RHOW0,DRWDU,URHOW0, INDAT1.......35900
     2                SIGMAW,SIGMAS                                      INDAT1.......36000
  210 FORMAT(1H1////11X,'C O N S T A N T   P R O P E R T I E S   O F',   INDAT1.......36100
     1   '   F L U I D   A N D   S O L I D   M A T R I X'                INDAT1.......36200
     2   //11X,1PD15.4,5X,'COMPRESSIBILITY OF FLUID'/11X,1PD15.4,5X,     INDAT1.......36300
     3   'COMPRESSIBILITY OF POROUS MATRIX'//11X,1PD15.4,5X,             INDAT1.......36400
     4   'SPECIFIC HEAT CAPACITY OF FLUID',/11X,1PD15.4,5X,              INDAT1.......36500
     5   'SPECIFIC HEAT CAPACITY OF SOLID GRAIN'//13X,'FLUID VISCOSITY', INDAT1.......36600
     6   ' IS CALCULATED BY SUTRA AS A FUNCTION OF TEMPERATURE IN ',     INDAT1.......36700
     7   'UNITS OF {kg/(m*s)}'//11X,1PD15.4,5X,'VISC0, CONVERSION ',     INDAT1.......36800
     8   'FACTOR FOR VISCOSITY UNITS,  {desired units} = VISC0*',        INDAT1.......36900
     9   '{kg/(m*s)}'//11X,1PD15.4,5X,'DENSITY OF A SOLID GRAIN'         INDAT1.......37000
     *   //13X,'FLUID DENSITY, RHOW'/13X,'CALCULATED BY ',               INDAT1.......37100
     1   'SUTRA IN TERMS OF TEMPERATURE, U, AS:'/13X,'RHOW = RHOW0 + ',  INDAT1.......37200
     2   'DRWDU*(U-URHOW0)'//11X,1PD15.4,5X,'FLUID BASE DENSITY, RHOW0'  INDAT1.......37300
     3   /11X,1PD15.4,5X,'COEFFICIENT OF DENSITY CHANGE WITH ',          INDAT1.......37400
     4   'TEMPERATURE, DRWDU'/11X,1PD15.4,5X,'TEMPERATURE, URHOW0, ',    INDAT1.......37500
     5   'AT WHICH FLUID DENSITY IS AT BASE VALUE, RHOW0'                INDAT1.......37600
     6   //11X,1PD15.4,5X,'THERMAL CONDUCTIVITY OF FLUID'                INDAT1.......37700
     7   /11X,1PD15.4,5X,'THERMAL CONDUCTIVITY OF SOLID GRAIN')          INDAT1.......37800
      IF(ME.EQ.-1)                                                       INDAT1.......37900
     1  WRITE(K3,220) COMPFL,COMPMA,VISC0,RHOS,RHOW0,DRWDU,URHOW0,SIGMAW INDAT1.......38000
  220 FORMAT(1H1////11X,'C O N S T A N T   P R O P E R T I E S   O F',   INDAT1.......38100
     1   '   F L U I D   A N D   S O L I D   M A T R I X'                INDAT1.......38200
     2   //11X,1PD15.4,5X,'COMPRESSIBILITY OF FLUID'/11X,1PD15.4,5X,     INDAT1.......38300
     3   'COMPRESSIBILITY OF POROUS MATRIX'                              INDAT1.......38400
     4   //11X,1PD15.4,5X,'FLUID VISCOSITY'                              INDAT1.......38500
     4   //11X,1PD15.4,5X,'DENSITY OF A SOLID GRAIN'                     INDAT1.......38600
     5   //13X,'FLUID DENSITY, RHOW'/13X,'CALCULATED BY ',               INDAT1.......38700
     6   'SUTRA IN TERMS OF SOLUTE CONCENTRATION, U, AS:',               INDAT1.......38800
     7   /13X,'RHOW = RHOW0 + DRWDU*(U-URHOW0)'                          INDAT1.......38900
     8   //11X,1PD15.4,5X,'FLUID BASE DENSITY, RHOW0'                    INDAT1.......39000
     9   /11X,1PD15.4,5X,'COEFFICIENT OF DENSITY CHANGE WITH ',          INDAT1.......39100
     *   'SOLUTE CONCENTRATION, DRWDU'                                   INDAT1.......39200
     1   /11X,1PD15.4,5X,'SOLUTE CONCENTRATION, URHOW0, ',               INDAT1.......39300
     4   'AT WHICH FLUID DENSITY IS AT BASE VALUE, RHOW0'                INDAT1.......39400
     5   //11X,1PD15.4,5X,'MOLECULAR DIFFUSIVITY OF SOLUTE IN FLUID')    INDAT1.......39500
C                                                                        INDAT1.......39600
C.....INPUT DATASET 11:  ADSORPTION PARAMETERS                           INDAT1.......39700
      ERRCOD = 'REA-INP-S11'                                             INDAT1.......39800
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......39900
      IF (ISERR) RETURN                                                  INDAT1.......40000
      ERRCOD = 'REA-INP-11'                                              INDAT1.......40100
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......40200
      IF (ISERR) RETURN                                                  INDAT1.......40300
      READ(INTFIL,*,IOSTAT=INERR(1)) ADSMOD                              INDAT1.......40400
      IF (INERR(1).NE.0) THEN                                            INDAT1.......40500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......40600
         RETURN                                                          INDAT1.......40700
      END IF                                                             INDAT1.......40800
      IF (ADSMOD.NE.'NONE      ') THEN                                   INDAT1.......40900
         READ(INTFIL,*,IOSTAT=INERR(1)) ADSMOD,CHI1,CHI2                 INDAT1.......41000
         IF (INERR(1).NE.0) THEN                                         INDAT1.......41100
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT1.......41200
            RETURN                                                       INDAT1.......41300
         END IF                                                          INDAT1.......41400
      END IF                                                             INDAT1.......41500
      IF(ME.EQ.+1) GOTO 248                                              INDAT1.......41600
      IF(ADSMOD.EQ.'NONE      ') GOTO 234                                INDAT1.......41700
      WRITE(K3,232) ADSMOD                                               INDAT1.......41800
  232 FORMAT(////11X,'A D S O R P T I O N   P A R A M E T E R S'         INDAT1.......41900
     1   //16X,A10,5X,'EQUILIBRIUM SORPTION ISOTHERM')                   INDAT1.......42000
      GOTO 236                                                           INDAT1.......42100
  234 WRITE(K3,235)                                                      INDAT1.......42200
  235 FORMAT(////11X,'A D S O R P T I O N   P A R A M E T E R S'         INDAT1.......42300
     1   //16X,'NON-SORBING SOLUTE')                                     INDAT1.......42400
  236 IF((ADSMOD.EQ.'NONE ').OR.(ADSMOD.EQ.'LINEAR    ').OR.             INDAT1.......42500
     1   (ADSMOD.EQ.'FREUNDLICH').OR.(ADSMOD.EQ.'LANGMUIR  ')) GOTO 238  INDAT1.......42600
      ERRCOD = 'INP-11-1'                                                INDAT1.......42700
      CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                           INDAT1.......42800
      RETURN                                                             INDAT1.......42900
  238 IF(ADSMOD.EQ.'LINEAR    ') WRITE(K3,242) CHI1                      INDAT1.......43000
  242 FORMAT(11X,1PD15.4,5X,'LINEAR DISTRIBUTION COEFFICIENT')           INDAT1.......43100
      IF(ADSMOD.EQ.'FREUNDLICH') WRITE(K3,244) CHI1,CHI2                 INDAT1.......43200
  244 FORMAT(11X,1PD15.4,5X,'FREUNDLICH DISTRIBUTION COEFFICIENT'        INDAT1.......43300
     1   /11X,1PD15.4,5X,'SECOND FREUNDLICH COEFFICIENT')                INDAT1.......43400
      IF(ADSMOD.EQ.'FREUNDLICH'.AND.CHI2.LE.0.D0) THEN                   INDAT1.......43500
         ERRCOD = 'INP-11-2'                                             INDAT1.......43600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......43700
         RETURN                                                          INDAT1.......43800
      ENDIF                                                              INDAT1.......43900
      IF(ADSMOD.EQ.'LANGMUIR  ') WRITE(K3,246) CHI1,CHI2                 INDAT1.......44000
  246 FORMAT(11X,1PD15.4,5X,'LANGMUIR DISTRIBUTION COEFFICIENT'          INDAT1.......44100
     1   /11X,1PD15.4,5X,'SECOND LANGMUIR COEFFICIENT')                  INDAT1.......44200
C                                                                        INDAT1.......44300
C.....INPUT DATASET 12:  PRODUCTION OF ENERGY OR SOLUTE MASS             INDAT1.......44400
  248 ERRCOD = 'REA-INP-S12'                                             INDAT1.......44500
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......44600
      IF (ISERR) RETURN                                                  INDAT1.......44700
      ERRCOD = 'REA-INP-12'                                              INDAT1.......44800
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......44900
      IF (ISERR) RETURN                                                  INDAT1.......45000
      READ(INTFIL,*,IOSTAT=INERR(1)) PRODF0,PRODS0,PRODF1,PRODS1         INDAT1.......45100
      IF (INERR(1).NE.0) THEN                                            INDAT1.......45200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......45300
         RETURN                                                          INDAT1.......45400
      END IF                                                             INDAT1.......45500
      IF(ME.EQ.-1) WRITE(K3,250) PRODF0,PRODS0,PRODF1,PRODS1             INDAT1.......45600
  250 FORMAT(////11X,'P R O D U C T I O N   A N D   D E C A Y   O F   ', INDAT1.......45700
     1   'S P E C I E S   M A S S'//13X,'PRODUCTION RATE (+)'/13X,       INDAT1.......45800
     2   'DECAY RATE (-)'//11X,1PD15.4,5X,'ZERO-ORDER RATE OF SOLUTE ',  INDAT1.......45900
     3   'MASS PRODUCTION/DECAY IN FLUID'/11X,1PD15.4,5X,                INDAT1.......46000
     4   'ZERO-ORDER RATE OF ADSORBATE MASS PRODUCTION/DECAY IN ',       INDAT1.......46100
     5   'IMMOBILE PHASE'/11X,1PD15.4,5X,'FIRST-ORDER RATE OF SOLUTE ',  INDAT1.......46200
     3   'MASS PRODUCTION/DECAY IN FLUID'/11X,1PD15.4,5X,                INDAT1.......46300
     4   'FIRST-ORDER RATE OF ADSORBATE MASS PRODUCTION/DECAY IN ',      INDAT1.......46400
     5   'IMMOBILE PHASE')                                               INDAT1.......46500
      IF(ME.EQ.+1) WRITE(K3,260) PRODF0,PRODS0                           INDAT1.......46600
  260 FORMAT(////11X,'P R O D U C T I O N   A N D   L O S S   O F   ',   INDAT1.......46700
     1   'E N E R G Y'//13X,'PRODUCTION RATE (+)'/13X,                   INDAT1.......46800
     2   'LOSS RATE (-)'//11X,1PD15.4,5X,'ZERO-ORDER RATE OF ENERGY ',   INDAT1.......46900
     3   'PRODUCTION/LOSS IN FLUID'/11X,1PD15.4,5X,                      INDAT1.......47000
     4   'ZERO-ORDER RATE OF ENERGY PRODUCTION/LOSS IN ',                INDAT1.......47100
     5   'SOLID GRAINS')                                                 INDAT1.......47200
C.....SET PARAMETER SWITCHES FOR EITHER ENERGY OR SOLUTE TRANSPORT       INDAT1.......47300
      IF(ME) 272,272,274                                                 INDAT1.......47400
C     FOR SOLUTE TRANSPORT:                                              INDAT1.......47500
  272 CS=0.0D0                                                           INDAT1.......47600
      CW=1.D00                                                           INDAT1.......47700
      SIGMAS=0.0D0                                                       INDAT1.......47800
      GOTO 278                                                           INDAT1.......47900
C     FOR ENERGY TRANSPORT:                                              INDAT1.......48000
  274 ADSMOD='NONE      '                                                INDAT1.......48100
      CHI1=0.0D0                                                         INDAT1.......48200
      CHI2=0.0D0                                                         INDAT1.......48300
      PRODF1=0.0D0                                                       INDAT1.......48400
      PRODS1=0.0D0                                                       INDAT1.......48500
  278 CONTINUE                                                           INDAT1.......48600
C                                                                        INDAT1.......48700
      IF (IABS(KTYPE).EQ.3) THEN                                         INDAT1.......48800
C.....READ 3D INPUT FROM DATASETS 13 - 15.                               INDAT1.......48900
C                                                                        INDAT1.......49000
C.....INPUT DATASET 13:  ORIENTATION OF COORDINATES TO GRAVITY           INDAT1.......49100
      ERRCOD = 'REA-INP-S13'                                             INDAT1.......49200
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......49300
      IF (ISERR) RETURN                                                  INDAT1.......49400
      ERRCOD = 'REA-INP-13'                                              INDAT1.......49500
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......49600
      IF (ISERR) RETURN                                                  INDAT1.......49700
      READ(INTFIL,*,IOSTAT=INERR(1)) GRAVX,GRAVY,GRAVZ                   INDAT1.......49800
      IF (INERR(1).NE.0) THEN                                            INDAT1.......49900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......50000
         RETURN                                                          INDAT1.......50100
      END IF                                                             INDAT1.......50200
      WRITE(K3,320) GRAVX,GRAVY,GRAVZ                                    INDAT1.......50300
  320 FORMAT(////11X,'C O O R D I N A T E   O R I E N T A T I O N   ',   INDAT1.......50400
     1   'T O   G R A V I T Y'//13X,'COMPONENT OF GRAVITY VECTOR',       INDAT1.......50500
     2   /13X,'IN +X DIRECTION, GRAVX'/11X,1PD15.4,5X,                   INDAT1.......50600
     3   'GRAVX = -GRAV * D(ELEVATION)/DX'//13X,'COMPONENT OF GRAVITY',  INDAT1.......50700
     4   ' VECTOR'/13X,'IN +Y DIRECTION, GRAVY'/11X,1PD15.4,5X,          INDAT1.......50800
     5   'GRAVY = -GRAV * D(ELEVATION)/DY'//13X,'COMPONENT OF GRAVITY',  INDAT1.......50900
     6   ' VECTOR'/13X,'IN +Z DIRECTION, GRAVZ'/11X,1PD15.4,5X,          INDAT1.......51000
     7   'GRAVZ = -GRAV * D(ELEVATION)/DZ')                              INDAT1.......51100
C                                                                        INDAT1.......51200
C.....INPUT DATASETS 14A & 14B:  NODEWISE DATA                           INDAT1.......51300
      ERRCOD = 'REA-INP-S14A'                                            INDAT1.......51400
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......51500
      IF (ISERR) RETURN                                                  INDAT1.......51600
      ERRCOD = 'REA-INP-14A'                                             INDAT1.......51700
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......51800
      IF (ISERR) RETURN                                                  INDAT1.......51900
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,SCALX,SCALY,SCALZ,PORFAC     INDAT1.......52000
      IF (INERR(1).NE.0) THEN                                            INDAT1.......52100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......52200
         RETURN                                                          INDAT1.......52300
      END IF                                                             INDAT1.......52400
      IF (CDUM10.NE.'NODE      ') THEN                                   INDAT1.......52500
         ERRCOD = 'INP-14A-1'                                            INDAT1.......52600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......52700
         RETURN                                                          INDAT1.......52800
      END IF                                                             INDAT1.......52900
      NRTEST=1                                                           INDAT1.......53000
      ERRCOD = 'REA-INP-S14B'                                            INDAT1.......53100
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......53200
      IF (ISERR) RETURN                                                  INDAT1.......53300
      DO 450 I=1,NN                                                      INDAT1.......53400
      ERRCOD = 'REA-INP-14B'                                             INDAT1.......53500
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......53600
      IF (ISERR) RETURN                                                  INDAT1.......53700
      READ(INTFIL,*,IOSTAT=INERR(1)) II,NREG(II),X(II),Y(II),Z(II),      INDAT1.......53800
     1   POR(II)                                                         INDAT1.......53900
      IF (INERR(1).NE.0) THEN                                            INDAT1.......54000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......54100
         RETURN                                                          INDAT1.......54200
      END IF                                                             INDAT1.......54300
      X(II)=X(II)*SCALX                                                  INDAT1.......54400
      Y(II)=Y(II)*SCALY                                                  INDAT1.......54500
      Z(II)=Z(II)*SCALZ                                                  INDAT1.......54600
      POR(II)=POR(II)*PORFAC                                             INDAT1.......54700
      IF(I.GT.1.AND.NREG(II).NE.NROLD) NRTEST=NRTEST+1                   INDAT1.......54800
      NROLD=NREG(II)                                                     INDAT1.......54900
C.....SET SPECIFIC PRESSURE STORATIVITY, SOP.                            INDAT1.......55000
  450 SOP(II)=(1.D0-POR(II))*COMPMA+POR(II)*COMPFL                       INDAT1.......55100
  460 IF(KNODAL.EQ.0) WRITE(K3,461) SCALX,SCALY,SCALZ,PORFAC             INDAT1.......55200
  461 FORMAT(1H1////11X,'N O D E   I N F O R M A T I O N'//16X,          INDAT1.......55300
     1   'PRINTOUT OF NODE COORDINATES AND POROSITIES ',                 INDAT1.......55400
     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PD15.4,5X,'X-SCALE'/   INDAT1.......55500
     3   33X,1PD15.4,5X,'Y-SCALE'/33X,1PD15.4,5X,'Z-SCALE'/              INDAT1.......55600
     4   33X,1PD15.4,5X,'POROSITY FACTOR')                               INDAT1.......55700
      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.NE.1) WRITE(K3,463)      INDAT1.......55800
      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.EQ.1) WRITE(K3,465)      INDAT1.......55900
  463 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1.......56000
     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1.......56100
  465 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1.......56200
     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1.......56300
      IF(KNODAL.EQ.+1.AND.IUNSAT.NE.1)                                   INDAT1.......56400
     1   WRITE(K3,470)(I,X(I),Y(I),Z(I),POR(I),I=1,NN)                   INDAT1.......56500
  470 FORMAT(1H1//11X,'N O D E   I N F O R M A T I O N'//14X,            INDAT1.......56600
     1   'NODE',7X,'X',16X,'Y',16X,'Z',15X,'POROSITY'//                  INDAT1.......56700
     2   (9X,I9,3(3X,1PD14.5),6X,0PF8.5))                                INDAT1.......56800
      IF(KNODAL.EQ.+1.AND.IUNSAT.EQ.1)                                   INDAT1.......56900
     1   WRITE(K3,480)(I,NREG(I),X(I),Y(I),Z(I),POR(I),I=1,NN)           INDAT1.......57000
  480 FORMAT(1H1//11X,'N O D E   I N F O R M A T I O N'//14X,'NODE',3X,  INDAT1.......57100
     1   'REGION',7X,'X',16X,'Y',16X,'Z',15X,'POROSITY'//                INDAT1.......57200
     2   (9X,I9,3X,I6,3(3X,1PD14.5),6X,0PF8.5))                          INDAT1.......57300
C                                                                        INDAT1.......57400
C.....INPUT DATASETS 15A & 15B:  ELEMENTWISE DATA                        INDAT1.......57500
      ERRCOD = 'REA-INP-S15A'                                            INDAT1.......57600
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......57700
      IF (ISERR) RETURN                                                  INDAT1.......57800
      ERRCOD = 'REA-INP-15A'                                             INDAT1.......57900
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......58000
      IF (ISERR) RETURN                                                  INDAT1.......58100
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,PMAXFA,PMIDFA,PMINFA,        INDAT1.......58200
     1   ANG1FA,ANG2FA,ANG3FA,ALMAXF,ALMIDF,ALMINF,                      INDAT1.......58300
     1   ATMXF,ATMDF,ATMNF                                               INDAT1.......58400
      IF (INERR(1).NE.0) THEN                                            INDAT1.......58500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......58600
         RETURN                                                          INDAT1.......58700
      END IF                                                             INDAT1.......58800
      IF (CDUM10.NE.'ELEMENT   ') THEN                                   INDAT1.......58900
         ERRCOD = 'INP-15A-1'                                            INDAT1.......59000
         CHERR(1) = '3D'                                                 INDAT1.......59100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......59200
         RETURN                                                          INDAT1.......59300
      END IF                                                             INDAT1.......59400
      IF(KELMNT.EQ.+1) THEN                                              INDAT1.......59500
         IF (IUNSAT.EQ.1) THEN                                           INDAT1.......59600
            WRITE(K3,500)                                                INDAT1.......59700
  500       FORMAT(1H1//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1.......59800
     1         11X,'ELEMENT',3X,'REGION',4X,                             INDAT1.......59900
     2         'MAXIMUM',9X,'MIDDLE',10X,'MINIMUM',18X,                  INDAT1.......60000
     2         'ANGLE1',9X,'ANGLE2',9X,'ANGLE3',4X,                      INDAT1.......60100
     2         'LONGITUDINAL',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,    INDAT1.......60200
     2         'TRANSVERSE',5X,'TRANSVERSE',5X,'TRANSVERSE'/             INDAT1.......60300
     3         31X,'PERMEABILITY',4X,'PERMEABILITY',4X,'PERMEABILITY',   INDAT1.......60400
     4         8X,'(IN DEGREES)',3X,'(IN DEGREES)',3X,'(IN DEGREES)',3X, INDAT1.......60500
     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY',3X,    INDAT1.......60600
     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY'/       INDAT1.......60700
     4         128X,' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM',  INDAT1.......60800
     4         3X,' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM'/    INDAT1.......60900
     1         128X,'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION',  INDAT1.......61000
     2         3X,'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)   INDAT1.......61100
         ELSE                                                            INDAT1.......61200
            WRITE(K3,501)                                                INDAT1.......61300
  501       FORMAT(1H1//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1.......61400
     1         11X,'ELEMENT',4X,                                         INDAT1.......61500
     2         'MAXIMUM',9X,'MIDDLE',10X,'MINIMUM',18X,                  INDAT1.......61600
     2         'ANGLE1',9X,'ANGLE2',9X,'ANGLE3',4X,                      INDAT1.......61700
     2         'LONGITUDINAL',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,    INDAT1.......61800
     2         'TRANSVERSE',5X,'TRANSVERSE',5X,'TRANSVERSE'/             INDAT1.......61900
     3         22X,'PERMEABILITY',4X,'PERMEABILITY',4X,'PERMEABILITY',   INDAT1.......62000
     4         8X,'(IN DEGREES)',3X,'(IN DEGREES)',3X,'(IN DEGREES)',3X, INDAT1.......62100
     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY',3X,    INDAT1.......62200
     4         'DISPERSIVITY',3X,'DISPERSIVITY',3X,'DISPERSIVITY'/       INDAT1.......62300
     4         119X,' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM',  INDAT1.......62400
     4         3X,' IN MAX-PERM',3X,' IN MID-PERM',3X,' IN MIN-PERM'/    INDAT1.......62500
     1         119X,'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION',  INDAT1.......62600
     2         3X,'   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)   INDAT1.......62700
         END IF                                                          INDAT1.......62800
      END IF                                                             INDAT1.......62900
      LRTEST=1                                                           INDAT1.......63000
      ERRCOD = 'REA-INP-S15B'                                            INDAT1.......63100
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......63200
      IF (ISERR) RETURN                                                  INDAT1.......63300
      DO 550 LL=1,NE                                                     INDAT1.......63400
      ERRCOD = 'REA-INP-15B'                                             INDAT1.......63500
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......63600
      IF (ISERR) RETURN                                                  INDAT1.......63700
      READ(INTFIL,*,IOSTAT=INERR(1)) L,LREG(L),PMAX,PMID,PMIN,           INDAT1.......63800
     1   ANGLE1,ANGLE2,ANGLE3,ALMAX(L),ALMID(L),ALMIN(L),                INDAT1.......63900
     1   ATMAX(L),ATMID(L),ATMIN(L)                                      INDAT1.......64000
      IF (INERR(1).NE.0) THEN                                            INDAT1.......64100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......64200
         RETURN                                                          INDAT1.......64300
      END IF                                                             INDAT1.......64400
      IF(LL.GT.1.AND.LREG(L).NE.LROLD) LRTEST=LRTEST+1                   INDAT1.......64500
      LROLD=LREG(L)                                                      INDAT1.......64600
      PMAX=PMAX*PMAXFA                                                   INDAT1.......64700
      PMID=PMID*PMIDFA                                                   INDAT1.......64800
      PMIN=PMIN*PMINFA                                                   INDAT1.......64900
      ANGLE1=ANGLE1*ANG1FA                                               INDAT1.......65000
      ANGLE2=ANGLE2*ANG2FA                                               INDAT1.......65100
      ANGLE3=ANGLE3*ANG3FA                                               INDAT1.......65200
      ALMAX(L)=ALMAX(L)*ALMAXF                                           INDAT1.......65300
      ALMID(L)=ALMID(L)*ALMIDF                                           INDAT1.......65400
      ALMIN(L)=ALMIN(L)*ALMINF                                           INDAT1.......65500
      ATMAX(L)=ATMAX(L)*ATMXF                                            INDAT1.......65600
      ATMID(L)=ATMID(L)*ATMDF                                            INDAT1.......65700
      ATMIN(L)=ATMIN(L)*ATMNF                                            INDAT1.......65800
      IF(KELMNT.EQ.+1.AND.IUNSAT.NE.1) WRITE(K3,520) L,                  INDAT1.......65900
     1   PMAX,PMID,PMIN,ANGLE1,ANGLE2,ANGLE3,                            INDAT1.......66000
     2   ALMAX(L),ALMID(L),ALMIN(L),ATMAX(L),ATMID(L),ATMIN(L)           INDAT1.......66100
  520 FORMAT(9X,I9,2X,3(1PD14.5,2X),7X,9(G11.4,4X))                      INDAT1.......66200
      IF(KELMNT.EQ.+1.AND.IUNSAT.EQ.1) WRITE(K3,530) L,LREG(L),          INDAT1.......66300
     1   PMAX,PMID,PMIN,ANGLE1,ANGLE2,ANGLE3,                            INDAT1.......66400
     2   ALMAX(L),ALMID(L),ALMIN(L),ATMAX(L),ATMID(L),ATMIN(L)           INDAT1.......66500
  530 FORMAT(9X,I9,4X,I5,2X,3(1PD14.5,2X),7X,9(G11.4,4X))                INDAT1.......66600
C                                                                        INDAT1.......66700
C.....ROTATE PERMEABILITY FROM MAX/MID/MIN TO X/Y/Z DIRECTIONS.          INDAT1.......66800
C        BASED ON CODE WRITTEN BY DAVID POLLOCK (USGS).                  INDAT1.......66900
      D2R=1.745329252D-2                                                 INDAT1.......67000
      PANGL1(L)=D2R*ANGLE1                                               INDAT1.......67100
      PANGL2(L)=D2R*ANGLE2                                               INDAT1.......67200
      PANGL3(L)=D2R*ANGLE3                                               INDAT1.......67300
      ZERO = 0D0                                                         INDAT1.......67400
      CALL ROTMAT(PANGL1(L),PANGL2(L),PANGL3(L),Q11,Q12,Q13,             INDAT1.......67500
     1   Q21,Q22,Q23,Q31,Q32,Q33)                                        INDAT1.......67600
      CALL TENSYM(PMAX,PMID,PMIN,Q11,Q12,Q13,Q21,Q22,Q23,Q31,Q32,Q33,    INDAT1.......67700
     1   PERMXX(L),PERMXY(L),PERMXZ(L),PERMYX(L),PERMYY(L),PERMYZ(L),    INDAT1.......67800
     2   PERMZX(L),PERMZY(L),PERMZZ(L))                                  INDAT1.......67900
  550 CONTINUE                                                           INDAT1.......68000
      IF(KELMNT.EQ.0)                                                    INDAT1.......68100
     1   WRITE(K3,569) PMAXFA,PMIDFA,PMINFA,ANG1FA,ANG2FA,ANG3FA,        INDAT1.......68200
     2      ALMAXF,ALMIDF,ALMINF,ATMXF,ATMDF,ATMNF                       INDAT1.......68300
  569 FORMAT(////11X,'E L E M E N T   I N F O R M A T I O N'//           INDAT1.......68400
     1   16X,'PRINTOUT OF ELEMENT PERMEABILITIES AND DISPERSIVITIES ',   INDAT1.......68500
     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PD15.4,5X,'MAXIMUM ',  INDAT1.......68600
     3   'PERMEABILITY FACTOR'/33X,1PD15.4,5X,'MIDDLE PERMEABILITY ',    INDAT1.......68700
     4   'FACTOR '/33X,1PD15.4,5X,'MINIMUM PERMEABILITY FACTOR'/         INDAT1.......68800
     5   33X,1PD15.4,5X,'ANGLE1 FACTOR'/33X,1PD15.4,5X,'ANGLE2 FACTOR'/  INDAT1.......68900
     6   33X,1PD15.4,5X,'ANGLE3 FACTOR'/                                 INDAT1.......69000
     7   33X,1PD15.4,5X,'FACTOR FOR LONGITUDINAL DISPERSIVITY IN ',      INDAT1.......69100
     8   'MAX-PERM DIRECTION'/33X,1PD15.4,5X,'FACTOR FOR LONGITUDINAL ', INDAT1.......69200
     9   'DISPERSIVITY IN MID-PERM DIRECTION'/33X,1PD15.4,5X,'FACTOR ',  INDAT1.......69300
     T   'FOR LONGITUDINAL DISPERSIVITY IN MIN-PERM DIRECTION'/          INDAT1.......69400
     1   33X,1PD15.4,5X,'FACTOR FOR TRANSVERSE DISPERSIVITY IN ',        INDAT1.......69500
     2   'MAX-PERM DIRECTION'/33X,1PD15.4,5X,'FACTOR FOR TRANSVERSE ',   INDAT1.......69600
     3   'DISPERSIVITY IN MID-PERM DIRECTION'/33X,1PD15.4,5X,'FACTOR',   INDAT1.......69700
     4   ' FOR TRANSVERSE DISPERSIVITY IN MIN-PERM DIRECTION')           INDAT1.......69800
      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.NE.1) WRITE(K3,573)      INDAT1.......69900
      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.EQ.1) WRITE(K3,575)      INDAT1.......70000
  573 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1.......70100
     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1.......70200
  575 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1.......70300
     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1.......70400
C                                                                        INDAT1.......70500
      ELSE                                                               INDAT1.......70600
C.....READ 2D INPUT FROM DATASETS 13 - 15.                               INDAT1.......70700
C.....NOTE THAT Z = THICKNESS AND PANGL1 = PANGLE.                       INDAT1.......70800
C                                                                        INDAT1.......70900
C.....INPUT DATASET 13:  ORIENTATION OF COORDINATES TO GRAVITY           INDAT1.......71000
      ERRCOD = 'REA-INP-S13'                                             INDAT1.......71100
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......71200
      IF (ISERR) RETURN                                                  INDAT1.......71300
      ERRCOD = 'REA-INP-13'                                              INDAT1.......71400
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......71500
      IF (ISERR) RETURN                                                  INDAT1.......71600
      READ(INTFIL,*,IOSTAT=INERR(1)) GRAVX,GRAVY                         INDAT1.......71700
      IF (INERR(1).NE.0) THEN                                            INDAT1.......71800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......71900
         RETURN                                                          INDAT1.......72000
      END IF                                                             INDAT1.......72100
      GRAVZ = 0D0                                                        INDAT1.......72200
      WRITE(K3,1320) GRAVX,GRAVY                                         INDAT1.......72300
 1320 FORMAT(////11X,'C O O R D I N A T E   O R I E N T A T I O N   ',   INDAT1.......72400
     1   'T O   G R A V I T Y'//13X,'COMPONENT OF GRAVITY VECTOR',       INDAT1.......72500
     2   /13X,'IN +X DIRECTION, GRAVX'/11X,1PD15.4,5X,                   INDAT1.......72600
     3   'GRAVX = -GRAV * D(ELEVATION)/DX'//13X,'COMPONENT OF GRAVITY',  INDAT1.......72700
     4   ' VECTOR'/13X,'IN +Y DIRECTION, GRAVY'/11X,1PD15.4,5X,          INDAT1.......72800
     5   'GRAVY = -GRAV * D(ELEVATION)/DY')                              INDAT1.......72900
C                                                                        INDAT1.......73000
C.....INPUT DATASETS 14A & 14B:  NODEWISE DATA                           INDAT1.......73100
      ERRCOD = 'REA-INP-S14A'                                            INDAT1.......73200
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......73300
      IF (ISERR) RETURN                                                  INDAT1.......73400
      ERRCOD = 'REA-INP-14A'                                             INDAT1.......73500
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......73600
      IF (ISERR) RETURN                                                  INDAT1.......73700
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,SCALX,SCALY,SCALTH,PORFAC    INDAT1.......73800
      IF (INERR(1).NE.0) THEN                                            INDAT1.......73900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......74000
         RETURN                                                          INDAT1.......74100
      END IF                                                             INDAT1.......74200
      IF (CDUM10.NE.'NODE      ') THEN                                   INDAT1.......74300
         ERRCOD = 'INP-14A-1'                                            INDAT1.......74400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......74500
         RETURN                                                          INDAT1.......74600
      END IF                                                             INDAT1.......74700
      NRTEST=1                                                           INDAT1.......74800
      ERRCOD = 'REA-INP-S14B'                                            INDAT1.......74900
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......75000
      IF (ISERR) RETURN                                                  INDAT1.......75100
      DO 1450 I=1,NN                                                     INDAT1.......75200
      ERRCOD = 'REA-INP-14B'                                             INDAT1.......75300
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......75400
      IF (ISERR) RETURN                                                  INDAT1.......75500
      READ(INTFIL,*,IOSTAT=INERR(1)) II,NREG(II),X(II),Y(II),Z(II),      INDAT1.......75600
     1   POR(II)                                                         INDAT1.......75700
      IF (INERR(1).NE.0) THEN                                            INDAT1.......75800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......75900
         RETURN                                                          INDAT1.......76000
      END IF                                                             INDAT1.......76100
      X(II)=X(II)*SCALX                                                  INDAT1.......76200
      Y(II)=Y(II)*SCALY                                                  INDAT1.......76300
      Z(II)=Z(II)*SCALTH                                                 INDAT1.......76400
      POR(II)=POR(II)*PORFAC                                             INDAT1.......76500
      IF(I.GT.1.AND.NREG(II).NE.NROLD) NRTEST=NRTEST+1                   INDAT1.......76600
      NROLD=NREG(II)                                                     INDAT1.......76700
C.....SET SPECIFIC PRESSURE STORATIVITY, SOP.                            INDAT1.......76800
 1450 SOP(II)=(1.D0-POR(II))*COMPMA+POR(II)*COMPFL                       INDAT1.......76900
 1460 IF(KNODAL.EQ.0) WRITE(K3,1461) SCALX,SCALY,SCALTH,PORFAC           INDAT1.......77000
 1461 FORMAT(1H1////11X,'N O D E   I N F O R M A T I O N'//16X,          INDAT1.......77100
     1   'PRINTOUT OF NODE COORDINATES, THICKNESSES AND POROSITIES ',    INDAT1.......77200
     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PD15.4,5X,'X-SCALE'/   INDAT1.......77300
     1   33X,1PD15.4,5X,'Y-SCALE'/33X,1PD15.4,5X,'THICKNESS FACTOR'/     INDAT1.......77400
     2   33X,1PD15.4,5X,'POROSITY FACTOR')                               INDAT1.......77500
      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.NE.1) WRITE(K3,1463)     INDAT1.......77600
      IF(IUNSAT.EQ.1.AND.KNODAL.EQ.0.AND.NRTEST.EQ.1) WRITE(K3,1465)     INDAT1.......77700
 1463 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1.......77800
     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1.......77900
 1465 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1.......78000
     1   'BEEN SPECIFIED AMONG THE NODES.')                              INDAT1.......78100
      IF(KNODAL.EQ.+1.AND.IUNSAT.NE.1)                                   INDAT1.......78200
     1   WRITE(K3,1470)(I,X(I),Y(I),Z(I),POR(I),I=1,NN)                  INDAT1.......78300
 1470 FORMAT(1H1//11X,'N O D E   I N F O R M A T I O N'//14X,            INDAT1.......78400
     1   'NODE',7X,'X',16X,'Y',17X,'THICKNESS',6X,'POROSITY'//           INDAT1.......78500
     2   (9X,I9,3(3X,1PD14.5),6X,0PF8.5))                                INDAT1.......78600
      IF(KNODAL.EQ.+1.AND.IUNSAT.EQ.1)                                   INDAT1.......78700
     1   WRITE(K3,1480)(I,NREG(I),X(I),Y(I),Z(I),POR(I),I=1,NN)          INDAT1.......78800
 1480 FORMAT(1H1//11X,'N O D E   I N F O R M A T I O N'//14X,'NODE',3X,  INDAT1.......78900
     1   'REGION',7X,'X',16X,'Y',17X,'THICKNESS',6X,'POROSITY'//         INDAT1.......79000
     2   (9X,I9,3X,I6,3(3X,1PD14.5),6X,0PF8.5))                          INDAT1.......79100
C                                                                        INDAT1.......79200
C.....INPUT DATASETS 15A & 15B:  ELEMENTWISE DATA                        INDAT1.......79300
      ERRCOD = 'REA-INP-S15A'                                            INDAT1.......79400
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......79500
      IF (ISERR) RETURN                                                  INDAT1.......79600
      ERRCOD = 'REA-INP-15A'                                             INDAT1.......79700
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......79800
      IF (ISERR) RETURN                                                  INDAT1.......79900
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10,PMAXFA,PMINFA,ANGFAC,        INDAT1.......80000
     1   ALMAXF,ALMINF,ATMAXF,ATMINF                                     INDAT1.......80100
      IF (INERR(1).NE.0) THEN                                            INDAT1.......80200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......80300
         RETURN                                                          INDAT1.......80400
      END IF                                                             INDAT1.......80500
      IF (CDUM10.NE.'ELEMENT   ') THEN                                   INDAT1.......80600
         ERRCOD = 'INP-15A-1'                                            INDAT1.......80700
         CHERR(1) = '2D'                                                 INDAT1.......80800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......80900
         RETURN                                                          INDAT1.......81000
      END IF                                                             INDAT1.......81100
      IF (KELMNT.EQ.+1) THEN                                             INDAT1.......81200
         IF (IUNSAT.EQ.1) THEN                                           INDAT1.......81300
            WRITE(K3,1500)                                               INDAT1.......81400
 1500       FORMAT(1H1//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1.......81500
     1         11X,'ELEMENT',3X,'REGION',4X,'MAXIMUM',9X,'MINIMUM',12X,  INDAT1.......81600
     2         'ANGLE BETWEEN',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,   INDAT1.......81700
     3         'TRANSVERSE',5X,'TRANSVERSE'/                             INDAT1.......81800
     4         31X,'PERMEABILITY',4X,'PERMEABILITY',4X,                  INDAT1.......81900
     5         '+X-DIRECTION AND',3X,'DISPERSIVITY',3X,'DISPERSIVITY',   INDAT1.......82000
     6         3X,'DISPERSIVITY',3X,'DISPERSIVITY'/                      INDAT1.......82100
     7         59X,'MAXIMUM PERMEABILITY',3X,' IN MAX-PERM',             INDAT1.......82200
     8         3X,' IN MIN-PERM',3X,' IN MAX-PERM',3X,' IN MIN-PERM'/    INDAT1.......82300
     9         67X,'(IN DEGREES)',3X,'   DIRECTION',3X,                  INDAT1.......82400
     1         '   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)      INDAT1.......82500
         ELSE                                                            INDAT1.......82600
            WRITE(K3,1501)                                               INDAT1.......82700
 1501       FORMAT(1H1//11X,'E L E M E N T   I N F O R M A T I O N'//    INDAT1.......82800
     1         11X,'ELEMENT',4X,'MAXIMUM',9X,'MINIMUM',12X,              INDAT1.......82900
     2         'ANGLE BETWEEN',3X,'LONGITUDINAL',3X,'LONGITUDINAL',5X,   INDAT1.......83000
     3         'TRANSVERSE',5X,'TRANSVERSE'/                             INDAT1.......83100
     4         22X,'PERMEABILITY',4X,'PERMEABILITY',4X,                  INDAT1.......83200
     5         '+X-DIRECTION AND',3X,'DISPERSIVITY',3X,'DISPERSIVITY',   INDAT1.......83300
     6         3X,'DISPERSIVITY',3X,'DISPERSIVITY'/                      INDAT1.......83400
     7         50X,'MAXIMUM PERMEABILITY',3X,' IN MAX-PERM',             INDAT1.......83500
     8         3X,' IN MIN-PERM',3X,' IN MAX-PERM',3X,' IN MIN-PERM'/    INDAT1.......83600
     9         58X,'(IN DEGREES)',3X,'   DIRECTION',3X,                  INDAT1.......83700
     1         '   DIRECTION',3X,'   DIRECTION',3X,'   DIRECTION'/)      INDAT1.......83800
         END IF                                                          INDAT1.......83900
      END IF                                                             INDAT1.......84000
      LRTEST=1                                                           INDAT1.......84100
      ERRCOD = 'REA-INP-S15B'                                            INDAT1.......84200
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    INDAT1.......84300
      IF (ISERR) RETURN                                                  INDAT1.......84400
      DO 1550 LL=1,NE                                                    INDAT1.......84500
      ERRCOD = 'REA-INP-15B'                                             INDAT1.......84600
      CALL READIF(K1, INTFIL, ERRCOD)                                    INDAT1.......84700
      IF (ISERR) RETURN                                                  INDAT1.......84800
      READ(INTFIL,*,IOSTAT=INERR(1)) L,LREG(L),PMAX,PMIN,ANGLEX,         INDAT1.......84900
     1   ALMAX(L),ALMIN(L),ATMAX(L),ATMIN(L)                             INDAT1.......85000
      IF (INERR(1).NE.0) THEN                                            INDAT1.......85100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT1.......85200
         RETURN                                                          INDAT1.......85300
      END IF                                                             INDAT1.......85400
      IF(LL.GT.1.AND.LREG(L).NE.LROLD) LRTEST=LRTEST+1                   INDAT1.......85500
      LROLD=LREG(L)                                                      INDAT1.......85600
      PMAX=PMAX*PMAXFA                                                   INDAT1.......85700
      PMIN=PMIN*PMINFA                                                   INDAT1.......85800
      ANGLEX=ANGLEX*ANGFAC                                               INDAT1.......85900
      ALMAX(L)=ALMAX(L)*ALMAXF                                           INDAT1.......86000
      ALMIN(L)=ALMIN(L)*ALMINF                                           INDAT1.......86100
      ATMAX(L)=ATMAX(L)*ATMAXF                                           INDAT1.......86200
      ATMIN(L)=ATMIN(L)*ATMINF                                           INDAT1.......86300
      IF(KELMNT.EQ.+1.AND.IUNSAT.NE.1) WRITE(K3,1520) L,                 INDAT1.......86400
     1   PMAX,PMIN,ANGLEX,ALMAX(L),ALMIN(L),ATMAX(L),ATMIN(L)            INDAT1.......86500
 1520 FORMAT(9X,I9,2X,2(1PD14.5,2X),7X,5(G11.4,4X))                      INDAT1.......86600
      IF(KELMNT.EQ.+1.AND.IUNSAT.EQ.1) WRITE(K3,1530) L,LREG(L),         INDAT1.......86700
     1   PMAX,PMIN,ANGLEX,ALMAX(L),ALMIN(L),ATMAX(L),ATMIN(L)            INDAT1.......86800
 1530 FORMAT(9X,I9,4X,I5,2X,2(1PD14.5,2X),7X,5(G11.4,4X))                INDAT1.......86900
C                                                                        INDAT1.......87000
C.....ROTATE PERMEABILITY FROM MAXIMUM/MINIMUM TO X/Y DIRECTIONS         INDAT1.......87100
      RADIAX=1.745329D-2*ANGLEX                                          INDAT1.......87200
      SINA=DSIN(RADIAX)                                                  INDAT1.......87300
      COSA=DCOS(RADIAX)                                                  INDAT1.......87400
      SINA2=SINA*SINA                                                    INDAT1.......87500
      COSA2=COSA*COSA                                                    INDAT1.......87600
      PERMXX(L)=PMAX*COSA2+PMIN*SINA2                                    INDAT1.......87700
      PERMYY(L)=PMAX*SINA2+PMIN*COSA2                                    INDAT1.......87800
      PERMXY(L)=(PMAX-PMIN)*SINA*COSA                                    INDAT1.......87900
      PERMYX(L)=PERMXY(L)                                                INDAT1.......88000
      PANGL1(L)=RADIAX                                                   INDAT1.......88100
 1550 CONTINUE                                                           INDAT1.......88200
      IF(KELMNT.EQ.0)                                                    INDAT1.......88300
     1   WRITE(K3,1569) PMAXFA,PMINFA,ANGFAC,ALMAXF,ALMINF,ATMAXF,ATMINF INDAT1.......88400
 1569 FORMAT(////11X,'E L E M E N T   I N F O R M A T I O N'//           INDAT1.......88500
     1   16X,'PRINTOUT OF ELEMENT PERMEABILITIES AND DISPERSIVITIES ',   INDAT1.......88600
     2   'CANCELLED.'//16X,'SCALE FACTORS :'/33X,1PD15.4,5X,'MAXIMUM ',  INDAT1.......88700
     3   'PERMEABILITY FACTOR'/33X,1PD15.4,5X,'MINIMUM PERMEABILITY ',   INDAT1.......88800
     4   'FACTOR'/33X,1PD15.4,5X,'ANGLE FROM +X TO MAXIMUM DIRECTION',   INDAT1.......88900
     5   ' FACTOR'/33X,1PD15.4,5X,'FACTOR FOR LONGITUDINAL DISPERSIVITY' INDAT1.......89000
     6  ,' IN MAX-PERM DIRECTION'/33X,1PD15.4,5X,                        INDAT1.......89100
     7   'FACTOR FOR LONGITUDINAL DISPERSIVITY IN MIN-PERM DIRECTION',   INDAT1.......89200
     8   /33X,1PD15.4,5X,'FACTOR FOR TRANSVERSE DISPERSIVITY',           INDAT1.......89300
     9   ' IN MAX-PERM DIRECTION'/33X,1PD15.4,5X,                        INDAT1.......89400
     *   'FACTOR FOR TRANSVERSE DISPERSIVITY IN MIN-PERM DIRECTION')     INDAT1.......89500
      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.NE.1) WRITE(K3,1573)     INDAT1.......89600
      IF(IUNSAT.EQ.1.AND.KELMNT.EQ.0.AND.LRTEST.EQ.1) WRITE(K3,1575)     INDAT1.......89700
 1573 FORMAT(33X,'MORE THAN ONE REGION OF UNSATURATED PROPERTIES HAS ',  INDAT1.......89800
     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1.......89900
 1575 FORMAT(33X,'ONLY ONE REGION OF UNSATURATED PROPERTIES HAS ',       INDAT1.......90000
     1   'BEEN SPECIFIED AMONG THE ELEMENTS.')                           INDAT1.......90100
C                                                                        INDAT1.......90200
      END IF                                                             INDAT1.......90300
C                                                                        INDAT1.......90400
      RETURN                                                             INDAT1.......90500
      END                                                                INDAT1.......90600
C                                                                        INDAT1.......90700
C     SUBROUTINE        I  N  D  A  T  2           SUTRA VERSION 2D3D.1  INDAT2.........100
C                                                                        INDAT2.........200
C *** PURPOSE :                                                          INDAT2.........300
C ***  TO READ INITIAL CONDITIONS FROM ICS FILE, AND TO                  INDAT2.........400
C ***  INITIALIZE DATA FOR EITHER WARM OR COLD START OF                  INDAT2.........500
C ***  THE SIMULATION.                                                   INDAT2.........600
C                                                                        INDAT2.........700
      SUBROUTINE INDAT2(PVEC,UVEC,PM1,UM1,UM2,CS1,CS2,CS3,SL,SR,RCIT,    INDAT2.........800
     1   SW,DSWDP,PBC,IPBC,IPBCT,NREG,QIN,DPDTITR)                       INDAT2.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                INDAT2........1000
      CHARACTER*10 CPUNI,CUUNI                                           INDAT2........1100
      CHARACTER INTFIL*1000                                              INDAT2........1200
      CHARACTER*80 ERRCOD,CHERR(10),FNAME(0:7)                           INDAT2........1300
      DIMENSION INERR(10),RLERR(10)                                      INDAT2........1400
      DIMENSION PVEC(NNVEC),UVEC(NNVEC),PM1(NN),UM1(NN),UM2(NN),SL(NN),  INDAT2........1500
     1   SR(NN),CS1(NN),CS2(NN),CS3(NN),RCIT(NN),SW(NN),DSWDP(NN),       INDAT2........1600
     2   PBC(NBCN),IPBC(NBCN),NREG(NN),QIN(NN),DPDTITR(NN)               INDAT2........1700
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  INDAT2........1800
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             INDAT2........1900
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              INDAT2........2000
     1   NSOP,NSOU,NBCN                                                  INDAT2........2100
      COMMON /DIMX2/ NELTA,NNVEC,NDIMJA                                  INDAT2........2200
      COMMON /ERRHAN/ ISERR                                              INDAT2........2300
      COMMON /FNAMES/ FNAME                                              INDAT2........2400
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        INDAT2........2500
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      INDAT2........2600
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        INDAT2........2700
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       INDAT2........2800
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  INDAT2........2900
C                                                                        INDAT2........3000
C                                                                        INDAT2........3100
      ERRCOD = 'REA-ICS-S1'                                              INDAT2........3200
      CALL SKPCOM(K2, NLSKIP, ERRCOD)                                    INDAT2........3300
      IF (ISERR) RETURN                                                  INDAT2........3400
      IF(IREAD) 500,500,620                                              INDAT2........3500
C.....INPUT INITIAL CONDITIONS FOR WARM START                            INDAT2........3600
  500 ERRCOD = 'REA-ICS-1'                                               INDAT2........3700
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2........3800
      IF (ISERR) RETURN                                                  INDAT2........3900
      READ(INTFIL,*,IOSTAT=INERR(1)) TSTART,DELTP,DELTU                  INDAT2........4000
      IF (INERR(1).NE.0) THEN                                            INDAT2........4100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2........4200
         RETURN                                                          INDAT2........4300
      END IF                                                             INDAT2........4400
      ERRCOD = 'REA-ICS-S2'                                              INDAT2........4500
      CALL SKPCOM(K2, NLSKIP, ERRCOD)                                    INDAT2........4600
      IF (ISERR) RETURN                                                  INDAT2........4700
      ERRCOD = 'REA-ICS-2'                                               INDAT2........4800
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2........4900
      IF (ISERR) RETURN                                                  INDAT2........5000
      READ(INTFIL,*,IOSTAT=INERR(1)) CPUNI                               INDAT2........5100
      IF (INERR(1).NE.0) THEN                                            INDAT2........5200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2........5300
         RETURN                                                          INDAT2........5400
      END IF                                                             INDAT2........5500
      IF (CPUNI.NE.'NONUNIFORM') THEN                                    INDAT2........5600
         ERRCOD = 'ICS-2-2'                                              INDAT2........5700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2........5800
         RETURN                                                          INDAT2........5900
      END IF                                                             INDAT2........6000
      READ(K2,*,IOSTAT=INERR(1)) (PVEC(I),I=1,NN)                        INDAT2........6100
      IF (INERR(1).NE.0) THEN                                            INDAT2........6200
         ERRCOD = 'REA-ICS-2'                                            INDAT2........6300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2........6400
         RETURN                                                          INDAT2........6500
      END IF                                                             INDAT2........6600
      ERRCOD = 'REA-ICS-S3'                                              INDAT2........6700
      CALL SKPCOM(K2, NLSKIP, ERRCOD)                                    INDAT2........6800
      IF (ISERR) RETURN                                                  INDAT2........6900
      ERRCOD = 'REA-ICS-3'                                               INDAT2........7000
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2........7100
      IF (ISERR) RETURN                                                  INDAT2........7200
      READ(INTFIL,*,IOSTAT=INERR(1)) CUUNI                               INDAT2........7300
      IF (INERR(1).NE.0) THEN                                            INDAT2........7400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2........7500
         RETURN                                                          INDAT2........7600
      END IF                                                             INDAT2........7700
      IF (CUUNI.NE.'NONUNIFORM') THEN                                    INDAT2........7800
         ERRCOD = 'ICS-3-2'                                              INDAT2........7900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2........8000
         RETURN                                                          INDAT2........8100
      END IF                                                             INDAT2........8200
      READ(K2,*,IOSTAT=INERR(1)) (UVEC(I),I=1,NN)                        INDAT2........8300
      IF (INERR(1).NE.0) THEN                                            INDAT2........8400
         ERRCOD = 'REA-ICS-3'                                            INDAT2........8500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2........8600
         RETURN                                                          INDAT2........8700
      END IF                                                             INDAT2........8800
      ERRCOD = 'REA-ICS-S4'                                              INDAT2........8900
      CALL SKPCOM(K2, NLSKIP, ERRCOD)                                    INDAT2........9000
      IF (ISERR) RETURN                                                  INDAT2........9100
      ERRCOD = 'REA-ICS-4'                                               INDAT2........9200
      READ(K2,*,IOSTAT=INERR(1)) (PM1(I),I=1,NN)                         INDAT2........9300
      IF (INERR(1).NE.0) THEN                                            INDAT2........9400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2........9500
         RETURN                                                          INDAT2........9600
      END IF                                                             INDAT2........9700
      READ(K2,*,IOSTAT=INERR(1)) (UM1(I),I=1,NN)                         INDAT2........9800
      IF (INERR(1).NE.0) THEN                                            INDAT2........9900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2.......10000
         RETURN                                                          INDAT2.......10100
      END IF                                                             INDAT2.......10200
      READ(K2,*,IOSTAT=INERR(1)) (CS1(I),I=1,NN)                         INDAT2.......10300
      IF (INERR(1).NE.0) THEN                                            INDAT2.......10400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2.......10500
         RETURN                                                          INDAT2.......10600
      END IF                                                             INDAT2.......10700
      READ(K2,*,IOSTAT=INERR(1)) (RCIT(I),I=1,NN)                        INDAT2.......10800
      IF (INERR(1).NE.0) THEN                                            INDAT2.......10900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2.......11000
         RETURN                                                          INDAT2.......11100
      END IF                                                             INDAT2.......11200
      READ(K2,*,IOSTAT=INERR(1)) (SW(I),I=1,NN)                          INDAT2.......11300
      IF (INERR(1).NE.0) THEN                                            INDAT2.......11400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2.......11500
         RETURN                                                          INDAT2.......11600
      END IF                                                             INDAT2.......11700
      READ(K2,*,IOSTAT=INERR(1)) (QIN(I),I=1,NN)                         INDAT2.......11800
      IF (INERR(1).NE.0) THEN                                            INDAT2.......11900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2.......12000
         RETURN                                                          INDAT2.......12100
      END IF                                                             INDAT2.......12200
      READ(K2,*,IOSTAT=INERR(1)) (PBC(IPU),IPU=1,NBCN)                   INDAT2.......12300
      IF (INERR(1).NE.0) THEN                                            INDAT2.......12400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2.......12500
         RETURN                                                          INDAT2.......12600
      END IF                                                             INDAT2.......12700
C     CALL ZERO(CS2,NN,0.0D0)                                            INDAT2.......12800
C     CALL ZERO(CS3,NN,0.0D0)                                            INDAT2.......12900
      CALL ZERO(SL,NN,0.0D0)                                             INDAT2.......13000
      CALL ZERO(SR,NN,0.0D0)                                             INDAT2.......13100
      CALL ZERO(DSWDP,NN,0.0D0)                                          INDAT2.......13200
      DO 550 I=1,NN                                                      INDAT2.......13300
  550 UM2(I)=UM1(I)                                                      INDAT2.......13400
      GOTO 1000                                                          INDAT2.......13500
C                                                                        INDAT2.......13600
C.....INPUT INITIAL CONDITIONS FOR COLD START                            INDAT2.......13700
  620 ERRCOD = 'REA-ICS-1'                                               INDAT2.......13800
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2.......13900
      IF (ISERR) RETURN                                                  INDAT2.......14000
      READ(INTFIL,*,IOSTAT=INERR(1)) TSTART                              INDAT2.......14100
      IF (INERR(1).NE.0) THEN                                            INDAT2.......14200
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2.......14300
         RETURN                                                          INDAT2.......14400
      END IF                                                             INDAT2.......14500
      ERRCOD = 'REA-ICS-S2'                                              INDAT2.......14600
      CALL SKPCOM(K2, NLSKIP, ERRCOD)                                    INDAT2.......14700
      IF (ISERR) RETURN                                                  INDAT2.......14800
      ERRCOD = 'REA-ICS-2'                                               INDAT2.......14900
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2.......15000
      IF (ISERR) RETURN                                                  INDAT2.......15100
      READ(INTFIL,*,IOSTAT=INERR(1)) CPUNI                               INDAT2.......15200
      IF (INERR(1).NE.0) THEN                                            INDAT2.......15300
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2.......15400
         RETURN                                                          INDAT2.......15500
      END IF                                                             INDAT2.......15600
      IF (CPUNI.EQ.'UNIFORM') THEN                                       INDAT2.......15700
         READ(K2,*,IOSTAT=INERR(1)) PUNI                                 INDAT2.......15800
         IF (INERR(1).NE.0) THEN                                         INDAT2.......15900
            ERRCOD = 'REA-ICS-2'                                         INDAT2.......16000
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT2.......16100
            RETURN                                                       INDAT2.......16200
         END IF                                                          INDAT2.......16300
         DO 625 I=1,NN                                                   INDAT2.......16400
            PVEC(I) = PUNI                                               INDAT2.......16500
  625    CONTINUE                                                        INDAT2.......16600
      ELSE IF (CPUNI.EQ.'NONUNIFORM') THEN                               INDAT2.......16700
         READ(K2,*,IOSTAT=INERR(1)) (PVEC(I),I=1,NN)                     INDAT2.......16800
         IF (INERR(1).NE.0) THEN                                         INDAT2.......16900
            ERRCOD = 'REA-ICS-2'                                         INDAT2.......17000
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT2.......17100
            RETURN                                                       INDAT2.......17200
         END IF                                                          INDAT2.......17300
      ELSE                                                               INDAT2.......17400
         ERRCOD = 'ICS-2-1'                                              INDAT2.......17500
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2.......17600
         RETURN                                                          INDAT2.......17700
      END IF                                                             INDAT2.......17800
      ERRCOD = 'REA-ICS-S3'                                              INDAT2.......17900
      CALL SKPCOM(K2, NLSKIP, ERRCOD)                                    INDAT2.......18000
      IF (ISERR) RETURN                                                  INDAT2.......18100
      ERRCOD = 'REA-ICS-3'                                               INDAT2.......18200
      CALL READIF(K2, INTFIL, ERRCOD)                                    INDAT2.......18300
      IF (ISERR) RETURN                                                  INDAT2.......18400
      READ(INTFIL,*,IOSTAT=INERR(1)) CUUNI                               INDAT2.......18500
      IF (INERR(1).NE.0) THEN                                            INDAT2.......18600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2.......18700
         RETURN                                                          INDAT2.......18800
      END IF                                                             INDAT2.......18900
      IF (CUUNI.EQ.'UNIFORM') THEN                                       INDAT2.......19000
         READ(K2,*,IOSTAT=INERR(1)) UUNI                                 INDAT2.......19100
         IF (INERR(1).NE.0) THEN                                         INDAT2.......19200
            ERRCOD = 'REA-ICS-3'                                         INDAT2.......19300
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT2.......19400
            RETURN                                                       INDAT2.......19500
         END IF                                                          INDAT2.......19600
         DO 630 I=1,NN                                                   INDAT2.......19700
            UVEC(I) = UUNI                                               INDAT2.......19800
  630    CONTINUE                                                        INDAT2.......19900
      ELSE IF (CUUNI.EQ.'NONUNIFORM') THEN                               INDAT2.......20000
         READ(K2,*,IOSTAT=INERR(1)) (UVEC(I),I=1,NN)                     INDAT2.......20100
         IF (INERR(1).NE.0) THEN                                         INDAT2.......20200
            ERRCOD = 'REA-ICS-3'                                         INDAT2.......20300
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INDAT2.......20400
            RETURN                                                       INDAT2.......20500
         END IF                                                          INDAT2.......20600
      ELSE                                                               INDAT2.......20700
         ERRCOD = 'ICS-3-1'                                              INDAT2.......20800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INDAT2.......20900
         RETURN                                                          INDAT2.......21000
      END IF                                                             INDAT2.......21100
C.....START-UP WITH NO PROJECTIONS BY SETTING DELTP AND DELTU            INDAT2.......21200
C        SUCH THAT BDELP=BDELU=0.5D-16 IN PROJECTION FORMULAE FOUND      INDAT2.......21300
C        IN SUBROUTINE SUTRA.                                            INDAT2.......21400
      DELTP=DELT*1.D16                                                   INDAT2.......21500
      DELTU=DELT*1.D16                                                   INDAT2.......21600
C.....INITIALIZE SPECIFIED TIME-VARYING PRESSURES TO INITIAL PRESSURE    INDAT2.......21700
C        VALUES FOR START-UP CALCULATION OF INFLOWS OR OUTFLOWS          INDAT2.......21800
C        (SET QPLITR=0)                                                  INDAT2.......21900
      IF(IPBCT) 680,740,740                                              INDAT2.......22000
  680 DO 730 IP=1,NPBC                                                   INDAT2.......22100
      I=IPBC(IP)                                                         INDAT2.......22200
      IF(I) 700,700,730                                                  INDAT2.......22300
  700 PBC(IP)=PVEC(-I)                                                   INDAT2.......22400
  730 CONTINUE                                                           INDAT2.......22500
C.....INITIALIZE P, U, AND CONSISTENT DENSITY                            INDAT2.......22600
  740 DO 800 I=1,NN                                                      INDAT2.......22700
      PM1(I)=PVEC(I)                                                     INDAT2.......22800
      UM1(I)=UVEC(I)                                                     INDAT2.......22900
      UM2(I)=UVEC(I)                                                     INDAT2.......23000
      RCIT(I)=RHOW0+DRWDU*(UVEC(I)-URHOW0)                               INDAT2.......23100
  800 CONTINUE                                                           INDAT2.......23200
C.....INITIALIZE SATURATION, SW(I)                                       INDAT2.......23300
      CALL ZERO(SW,NN,1.0D0)                                             INDAT2.......23400
      CALL ZERO(DSWDP,NN,0.0D0)                                          INDAT2.......23500
      IF(IUNSAT.NE.1) GOTO 990                                           INDAT2.......23600
      IUNSAT=3                                                           INDAT2.......23700
      DO 900 I=1,NN                                                      INDAT2.......23800
  900 IF(PVEC(I).LT.0) CALL UNSAT(SW(I),DSWDP(I),RELK,PVEC(I),NREG(I))   INDAT2.......23900
  990 CONTINUE                                                           INDAT2.......24000
      CALL ZERO(CS1,NN,CS)                                               INDAT2.......24100
C     CALL ZERO(CS2,NN,0.0D0)                                            INDAT2.......24200
C     CALL ZERO(CS3,NN,0.0D0)                                            INDAT2.......24300
      CALL ZERO(SL,NN,0.0D0)                                             INDAT2.......24400
      CALL ZERO(SR,NN,0.0D0)                                             INDAT2.......24500
      CALL ZERO(DPDTITR,NN,0.0D0)                                        INDAT2.......24600
 1000 CONTINUE                                                           INDAT2.......24700
C                                                                        INDAT2.......24800
C.....SET STARTING TIME OF SIMULATION CLOCK, TSEC                        INDAT2.......24900
      TSEC=TSTART                                                        INDAT2.......25000
C                                                                        INDAT2.......25100
C                                                                        INDAT2.......25200
      RETURN                                                             INDAT2.......25300
      END                                                                INDAT2.......25400
C                                                                        INDAT2.......25500
C     SUBROUTINE        N  O  D  A  L              SUTRA VERSION 2D3D.1  NODAL..........100
C                                                                        NODAL..........200
C *** PURPOSE :                                                          NODAL..........300
C ***  (1) TO CARRY OUT ALL CELLWISE CALCULATIONS AND TO ADD CELLWISE    NODAL..........400
C ***      TERMS TO THE GLOBAL MATRIX AND GLOBAL VECTOR FOR BOTH FLOW    NODAL..........500
C ***      AND TRANSPORT EQUATIONS.                                      NODAL..........600
C ***  (2) TO ADD FLUID SOURCE AND SOLUTE MASS OR ENERGY SOURCE TERMS    NODAL..........700
C ***      TO THE MATRIX EQUATIONS.                                      NODAL..........800
C                                                                        NODAL..........900
      SUBROUTINE NODAL(ML,VOL,PMAT,PVEC,UMAT,UVEC,PITER,UITER,PM1,UM1,   NODAL.........1000
     1   UM2,POR,QIN,UIN,QUIN,QINITR,CS1,CS2,CS3,SL,SR,SW,DSWDP,RHO,SOP, NODAL.........1100
     1   NREG,MIOFF)                                                     NODAL.........1200
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                NODAL.........1300
      DIMENSION VOL(NN),PMAT(NELT,NCBI),PVEC(NNVEC),UMAT(NELT,NCBI),     NODAL.........1400
     1   UVEC(NNVEC)                                                     NODAL.........1500
      DIMENSION PITER(NN),UITER(NN),PM1(NN),UM1(NN),UM2(NN),             NODAL.........1600
     1   POR(NN),QIN(NN),UIN(NN),QUIN(NN),QINITR(NN),                    NODAL.........1700
     2   CS1(NN),CS2(NN),CS3(NN),SL(NN),SR(NN),SW(NN),RHO(NN),DSWDP(NN), NODAL.........1800
     3   SOP(NN),NREG(NN)                                                NODAL.........1900
      DIMENSION MIOFF(27)                                                NODAL.........2000
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  NODAL.........2100
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             NODAL.........2200
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              NODAL.........2300
     1   NSOP,NSOU,NBCN                                                  NODAL.........2400
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   NODAL.........2500
      COMMON /DIMX2/ NELTA,NNVEC,NDIMJA                                  NODAL.........2600
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      NODAL.........2700
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        NODAL.........2800
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           NODAL.........2900
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       NODAL.........3000
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  NODAL.........3100
C                                                                        NODAL.........3200
C                                                                        NODAL.........3300
      IF(IUNSAT.NE.0) IUNSAT=1                                           NODAL.........3400
C                                                                        NODAL.........3500
C.....SET UP MATRIX STRUCTURE INFORMATION                                NODAL.........3600
      IF (KSOLVP.EQ.0) THEN                                              NODAL.........3700
         IMID0 = 0                                                       NODAL.........3800
         JMID = NBHALF                                                   NODAL.........3900
      ELSE                                                               NODAL.........4000
         IF (IABS(KTYPE).EQ.3) THEN                                      NODAL.........4100
            IMID0 = MIOFF(14)                                            NODAL.........4200
         ELSE                                                            NODAL.........4300
            IMID0 = MIOFF(5)                                             NODAL.........4400
         END IF                                                          NODAL.........4500
         JMID = 1                                                        NODAL.........4600
      END IF                                                             NODAL.........4700
C                                                                        NODAL.........4800
C.....DO NOT UPDATE NODAL PARAMETERS ON A TIME STEP WHEN ONLY U IS       NODAL.........4900
C        SOLVED FOR BY BACK SUBSTITUTION (I.E., WHEN NOUMAT=1)           NODAL.........5000
      IF(NOUMAT) 50,50,200                                               NODAL.........5100
C.....SET UNSATURATED FLOW PARAMETERS AT NODES, SW(I) AND DSWDP(I)       NODAL.........5200
   50 DO 120 I=1,NN                                                      NODAL.........5300
      IF(IUNSAT-1) 120,100,120                                           NODAL.........5400
  100 IF(PITER(I)) 110,115,115                                           NODAL.........5500
  110 CALL UNSAT(SW(I),DSWDP(I),RELK,PITER(I),NREG(I))                   NODAL.........5600
      GOTO 120                                                           NODAL.........5700
  115 SW(I)=1.0D0                                                        NODAL.........5800
      DSWDP(I)=0.0D0                                                     NODAL.........5900
  120 CONTINUE                                                           NODAL.........6000
C.....SET FLUID DENSITY AT NODES, RHO(I)                                 NODAL.........6100
C        RHO = F (UITER(I))                                              NODAL.........6200
      DO 150 I=1,NN                                                      NODAL.........6300
  150 RHO(I)=RHOW0+DRWDU*(UITER(I)-URHOW0)                               NODAL.........6400
  200 CONTINUE                                                           NODAL.........6500
C                                                                        NODAL.........6600
      DO 1000 I=1,NN                                                     NODAL.........6700
      IMID = IMID0 + I                                                   NODAL.........6800
C                                                                        NODAL.........6900
      SWRHON=SW(I)*RHO(I)                                                NODAL.........7000
C                                                                        NODAL.........7100
      IF(ML-1) 220,220,230                                               NODAL.........7200
C                                                                        NODAL.........7300
C.....CALCULATE CELLWISE TERMS FOR P EQUATION.                           NODAL.........7400
C.....FOR STEADY-STATE FLOW, ISSFLO=2; FOR TRANSIENT FLOW, ISSFLO=0.     NODAL.........7500
  220 AFLN=(1-ISSFLO/2)*                                                 NODAL.........7600
     1   (SWRHON*SOP(I)+POR(I)*RHO(I)*DSWDP(I))*VOL(I)/DELTP             NODAL.........7700
      CFLN=POR(I)*SW(I)*DRWDU*VOL(I)                                     NODAL.........7800
      DUDT=(1-ISSFLO/2)*(UM1(I)-UM2(I))/DLTUM1                           NODAL.........7900
      CFLN=CFLN*DUDT                                                     NODAL.........8000
C.....ADD CELLWISE TERMS AND FLUID SOURCES OR FLUXES TO P EQUATION       NODAL.........8100
      PMAT(IMID,JMID) = PMAT(IMID,JMID) + AFLN                           NODAL.........8200
      PVEC(I) = PVEC(I) - CFLN + AFLN*PM1(I) + QIN(I)                    NODAL.........8300
C                                                                        NODAL.........8400
      IF(ML-1) 230,1000,230                                              NODAL.........8500
C                                                                        NODAL.........8600
C.....CALCULATE CELLWISE TERMS FOR U-EQUATION                            NODAL.........8700
  230 EPRS=(1.D0-POR(I))*RHOS                                            NODAL.........8800
      ATRN=(1-ISSTRA)*(POR(I)*SWRHON*CW+EPRS*CS1(I))*VOL(I)/DELTU        NODAL.........8900
      GTRN=POR(I)*SWRHON*PRODF1*VOL(I)                                   NODAL.........9000
      GSV=EPRS*PRODS1*VOL(I)                                             NODAL.........9100
      GSLTRN=GSV*SL(I)                                                   NODAL.........9200
      GSRTRN=GSV*SR(I)                                                   NODAL.........9300
      ETRN=(POR(I)*SWRHON*PRODF0+EPRS*PRODS0)*VOL(I)                     NODAL.........9400
C.....CALCULATE SOURCES OF SOLUTE OR ENERGY CONTAINED IN                 NODAL.........9500
C        SOURCES OF FLUID (ZERO CONTRIBUTION FOR OUTFLOWING FLUID)       NODAL.........9600
      QUR=0.0D0                                                          NODAL.........9700
      QUL=0.0D0                                                          NODAL.........9800
      IF(QINITR(I)) 360,360,340                                          NODAL.........9900
  340 QUL=-CW*QINITR(I)                                                  NODAL........10000
      QUR=-QUL*UIN(I)                                                    NODAL........10100
C.....ADD CELLWISE TERMS, SOURCES OF SOLUTE OR ENERGY IN FLUID INFLOWS,  NODAL........10200
C        AND PURE SOURCES OR FLUXES OF SOLUTE OR ENERGY TO U-EQUATION    NODAL........10300
  360 IF(NOUMAT) 370,370,380                                             NODAL........10400
  370 UMAT(IMID,JMID) = UMAT(IMID,JMID) + ATRN - GTRN - GSLTRN - QUL     NODAL........10500
  380 UVEC(I) = UVEC(I) + ATRN*UM1(I) + ETRN + GSRTRN + QUR + QUIN(I)    NODAL........10600
C                                                                        NODAL........10700
 1000 CONTINUE                                                           NODAL........10800
C                                                                        NODAL........10900
      RETURN                                                             NODAL........11000
      END                                                                NODAL........11100
C                                                                        NODAL........11200
C     SUBROUTINE        O  U  T  E  L  E           SUTRA VERSION 2D3D.1  OUTELE.........100
C                                                                        OUTELE.........200
C *** PURPOSE :                                                          OUTELE.........300
C ***  TO PRINT ELEMENT CENTROID COORDINATES AND VELOCITY COMPONENTS     OUTELE.........400
C ***  IN A FLEXIBLE, COLUMNWISE FORMAT.  OUTPUT IS TO THE ELE FILE.     OUTELE.........500
C                                                                        OUTELE.........600
      SUBROUTINE OUTELE(VMAG,VANG1,VANG2,IN,X,Y,Z,TITLE1,TITLE2)         OUTELE.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTELE.........800
      PARAMETER (NCOLMX=9)                                               OUTELE.........900
      CHARACTER*1  TITLE1(80),TITLE2(80)                                 OUTELE........1000
      CHARACTER*15 COLTK6(7)                                             OUTELE........1100
      CHARACTER*1 CPVX,CPVY,CPVZ                                         OUTELE........1200
      LOGICAL ONCEK5,ONCEK6,ONCEK7                                       OUTELE........1300
      LOGICAL PRINTE                                                     OUTELE........1400
      DIMENSION IN(NIN),IIN(8)                                           OUTELE........1500
      DIMENSION VMAG(NE),VANG1(NE),VANG2(NEX)                            OUTELE........1600
      DIMENSION X(NN),Y(NN),Z(NN)                                        OUTELE........1700
      DIMENSION VCOL(NCOLMX),VVAR(7)                                     OUTELE........1800
      DIMENSION J5COL(NCOLMX),J6COL(NCOLMX)                              OUTELE........1900
      ALLOCATABLE TT(:),ITT(:),ISVEL(:)                                  OUTELE........2000
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTELE........2100
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             OUTELE........2200
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTELE........2300
     1   NSOP,NSOU,NBCN                                                  OUTELE........2400
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   OUTELE........2500
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        OUTELE........2600
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  OUTELE........2700
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      OUTELE........2800
      COMMON /JCOLS/ NCOLPR,LCOLPR,NCOLS5,NCOLS6,J5COL,J6COL             OUTELE........2900
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     OUTELE........3000
     1   KSCRN,KPAUSE                                                    OUTELE........3100
      COMMON /PLT1/ ONCEK5,ONCEK6,ONCEK7                                 OUTELE........3200
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTELE........3300
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTELE........3400
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  OUTELE........3500
      DATA (COLTK6(MM), MM=1,7) /'Element',                              OUTELE........3600
     1   '       X origin', '       Y origin', '       Z origin',        OUTELE........3700
     2   '     X velocity', '     Y velocity', '     Z velocity'/        OUTELE........3800
      SAVE COLTK6                                                        OUTELE........3900
C                                                                        OUTELE........4000
C.....CALCULATE HEADERS ON FIRST CALL AND CREATE OUTPUT ON EACH CALL.    OUTELE........4100
C                                                                        OUTELE........4200
      DKTM2 = DBLE(IABS(KTYPE) - 2)                                      OUTELE........4300
C                                                                        OUTELE........4400
      IF (.NOT. ONCEK6)  THEN                                            OUTELE........4500
C.....FIRST CALL -- CREATE FILE HEADER.                                  OUTELE........4600
C                                                                        OUTELE........4700
C........CALCULATE THE MAXIMUM NUMBER OF TIME STEPS, KTMAX.              OUTELE........4800
         TS=TSTART                                                       OUTELE........4900
         JT=0                                                            OUTELE........5000
         KT=0                                                            OUTELE........5100
         DELTK=DELT                                                      OUTELE........5200
    4    CONTINUE                                                        OUTELE........5300
            JT=JT+1                                                      OUTELE........5400
            IF (MOD(JT,ITCYC).EQ.0 .AND. JT.GT.1) DELTK=DELTK*DTMULT     OUTELE........5500
            IF (DELTK.GT.DTMAX) DELTK=DTMAX                              OUTELE........5600
            TS=TS+DELTK                                                  OUTELE........5700
            IF (MOD(JT,LCOLPR).EQ.0 .OR. JT.EQ.1) KT = KT + 1            OUTELE........5800
         IF(JT.LT.ITMAX .AND. TS.LT.TMAX) GOTO 4                         OUTELE........5900
         JTMAX = JT                                                      OUTELE........6000
         IF(JTMAX.GT.1 .AND. MOD(JT,LCOLPR).NE.0) KT = KT + 1            OUTELE........6100
         KTMAX = KT                                                      OUTELE........6200
C                                                                        OUTELE........6300
C........ALLOCATE LOCAL ARRAYS                                           OUTELE........6400
         ALLOCATE(TT(KTMAX),ITT(KTMAX))                                  OUTELE........6500
         ALLOCATE(ISVEL(KTMAX))                                          OUTELE........6600
C                                                                        OUTELE........6700
C........CALCULATE AND PRINT TIME STEP INFORMATION                       OUTELE........6800
         TS=TSTART                                                       OUTELE........6900
C........TIME STEP VALUE                                                 OUTELE........7000
         JT=0                                                            OUTELE........7100
C........NUMBER OF PRINTED TIME STEPS                                    OUTELE........7200
         KT=0                                                            OUTELE........7300
C........TIME STEP INCREMENT                                             OUTELE........7400
         DELTK=DELT                                                      OUTELE........7500
C........INDICATORS OF WHEN VARIABLES ARE CALCULATED AND PRINTED         OUTELE........7600
         LCP = 0                                                         OUTELE........7700
         CPVX = 'N'                                                      OUTELE........7800
         CPVY = 'N'                                                      OUTELE........7900
         CPVZ = 'N'                                                      OUTELE........8000
         DO 8 M=1,NCOLS6                                                 OUTELE........8100
            IF (J6COL(M).EQ.5) CPVX = 'Y'                                OUTELE........8200
            IF (J6COL(M).EQ.6) CPVY = 'Y'                                OUTELE........8300
            IF (J6COL(M).EQ.7) CPVZ = 'Y'                                OUTELE........8400
    8    CONTINUE                                                        OUTELE........8500
   10    CONTINUE                                                        OUTELE........8600
            JT=JT+1                                                      OUTELE........8700
            IF (MOD(JT,ITCYC).EQ.0 .AND. JT.GT.1) DELTK=DELTK*DTMULT     OUTELE........8800
            IF (DELTK.GT.DTMAX) DELTK=DTMAX                              OUTELE........8900
            TS=TS+DELTK                                                  OUTELE........9000
            LCV = LCP                                                    OUTELE........9100
            IF (MOD(JT,NPCYC).EQ.0 .OR. JT.EQ.1) LCP = JT                OUTELE........9200
            IF (MOD(JT,LCOLPR).EQ.0 .OR. JT.EQ.1) THEN                   OUTELE........9300
               KT = KT + 1                                               OUTELE........9400
               TT(KT) = TS                                               OUTELE........9500
               ITT(KT) = JT                                              OUTELE........9600
               ISVEL(KT) = LCV                                           OUTELE........9700
               IF (JT.NE.1 .OR. ISSFLO.EQ.2) THEN                        OUTELE........9800
                  IF (MOD(JT,NUCYC).NE.0) ISVEL(KT) = 0                  OUTELE........9900
               ENDIF                                                     OUTELE.......10000
            ENDIF                                                        OUTELE.......10100
         IF(JT.LT.ITMAX .AND. TS.LT.TMAX) GOTO 10                        OUTELE.......10200
         JTMAX = JT                                                      OUTELE.......10300
         IF (ISSTRA.EQ.1) TT(KT) = TSTART                                OUTELE.......10400
C                                                                        OUTELE.......10500
C                                                                        OUTELE.......10600
C........PRINT LAST TIME STEP ALWAYS, UNLESS ALREADY PRINTED             OUTELE.......10700
         IF(JTMAX.GT.1 .AND. MOD(JT,LCOLPR).NE.0) THEN                   OUTELE.......10800
            KT = KT + 1                                                  OUTELE.......10900
            TT(KT) = TS                                                  OUTELE.......11000
            ITT(KT) = JT                                                 OUTELE.......11100
            ISVEL(KT) = LCV                                              OUTELE.......11200
         ENDIF                                                           OUTELE.......11300
C                                                                        OUTELE.......11400
C........IF STEADY-STATE FLOW, V BASED ON TIME STEP 0 ONLY, AND          OUTELE.......11500
C           OUTPUT OCCURS ONLY ON TIME STEP 1.                           OUTELE.......11600
         IF (ISSFLO.NE.0) THEN                                           OUTELE.......11700
            KTMAX = 1                                                    OUTELE.......11800
            ISVEL(1) = 0                                                 OUTELE.......11900
         END IF                                                          OUTELE.......12000
C........WRITE HEADER INFORMATION                                        OUTELE.......12100
         WRITE(K6,960) TITLE1, TITLE2                                    OUTELE.......12200
         IF (KTYPE.LT.0) THEN                                            OUTELE.......12300
            IF (IABS(KTYPE).EQ.3) THEN                                   OUTELE.......12400
               WRITE(K6,961) IABS(KTYPE), NN1-1, NN2-1, NN3-1,           OUTELE.......12500
     1            NE, " Elements"                                        OUTELE.......12600
            ELSE                                                         OUTELE.......12700
               WRITE(K6,962) IABS(KTYPE), NN1-1, NN2-1,                  OUTELE.......12800
     1            NE, " Elements"                                        OUTELE.......12900
            END IF                                                       OUTELE.......13000
         ELSE                                                            OUTELE.......13100
            WRITE(K6,963) IABS(KTYPE), NE, " Elements"                   OUTELE.......13200
         END IF                                                          OUTELE.......13300
         WRITE(K6,964) "VELOCITY RESULTS",                               OUTELE.......13400
     1      KTMAX, "Vx", "Vy", "Vz"                                      OUTELE.......13500
         DO 20  KT=1, KTMAX                                              OUTELE.......13600
            WRITE(K6,965) ITT(KT), TT(KT), CPVX, ISVEL(KT),              OUTELE.......13700
     1         CPVY, ISVEL(KT), CPVZ, ISVEL(KT)                          OUTELE.......13800
   20    CONTINUE                                                        OUTELE.......13900
  960    FORMAT("## ", 80A1,                                             OUTELE.......14000
     1         /"## ", 80A1,                                             OUTELE.......14100
     2         /"## ")                                                   OUTELE.......14200
  961    FORMAT("## ", I1, "-D, REGULAR MESH  ", 2X,                     OUTELE.......14300
     1                 "(", 2(I9, ")*("), I9, ") = ", I9, A,             OUTELE.......14400
     2         /"## ")                                                   OUTELE.......14500
  962    FORMAT("## ", I1, "-D, REGULAR MESH  ", 14X,                    OUTELE.......14600
     1                 "(", I9, ")*(", I9, ") = ", I9, A,                OUTELE.......14700
     2         /"## ")                                                   OUTELE.......14800
  963    FORMAT("## ", I1, "-D, IRREGULAR MESH", 40X, I9, A,             OUTELE.......14900
     1         /"## ")                                                   OUTELE.......15000
  964    FORMAT("## ", 77("="),                                          OUTELE.......15100
     4         /"## ", A, 33X, I9, " Time steps printed",                OUTELE.......15200
     5         /"## ", 77("="),                                          OUTELE.......15300
     6         /"## ",                                                   OUTELE.......15400
     7         /"##  Time steps", 20X,                                   OUTELE.......15500
     8                 "[Printed? / Time step on which V is based]"      OUTELE.......15600
     9         /"## in this file    Time (sec)",10X,A2, 13X,A2, 13X,A2,  OUTELE.......15700
     T         /"## ", 12("-"), 3X, 12("-"), 1X, 3(3X, 12("-")) )        OUTELE.......15800
  965    FORMAT ("## ", 3X, I8, 3X, 1PE13.6, 3(5X, A1, 1X, I8))          OUTELE.......15900
                                                                         OUTELE.......16000
C                                                                        OUTELE.......16100
C........DEALLOCATE LOCAL ARRAYS.                                        OUTELE.......16200
         DEALLOCATE(TT,ITT,ISVEL)                                        OUTELE.......16300
C                                                                        OUTELE.......16400
         ONCEK6 = .TRUE.                                                 OUTELE.......16500
      ENDIF                                                              OUTELE.......16600
C                                                                        OUTELE.......16700
C.....OUTPUT VELOCITIES FOR STEADY FLOW ONLY ON THE FIRST TIME STEP      OUTELE.......16800
      IF ((ISSFLO.EQ.2).AND.(IT.GT.1)) GOTO 9999                         OUTELE.......16900
C                                                                        OUTELE.......17000
C.....VELOCITY HEADER INFORMATION REPEATED BEFORE EACH TIME STEP         OUTELE.......17100
      IF ((IT.EQ.1).AND.(ISSTRA.EQ.1)) THEN                              OUTELE.......17200
         DURN = 0D0                                                      OUTELE.......17300
         TOUT = TSTART                                                   OUTELE.......17400
      ELSE                                                               OUTELE.......17500
         DURN = DELT                                                     OUTELE.......17600
         TOUT = TSEC                                                     OUTELE.......17700
      END IF                                                             OUTELE.......17800
      WRITE(K6,966) IT, DURN, TOUT                                       OUTELE.......17900
  966 FORMAT('## ',                                                      OUTELE.......18000
     1      /'## ', 77('='),                                             OUTELE.......18100
     2      /'## TIME STEP ', I6, 9X, 'Duration: ', 1PE11.4, ' sec',     OUTELE.......18200
     3                            6X, 'Time: ', 1PE11.4, ' sec',         OUTELE.......18300
     4      /'## ', 77('='))                                             OUTELE.......18400
      PRINTE = (J6COL(1).EQ.1)                                           OUTELE.......18500
      IF (PRINTE) THEN                                                   OUTELE.......18600
         WRITE(K6,982) (COLTK6(J6COL(M)), M=1,NCOLS6)                    OUTELE.......18700
  982    FORMAT ("## ", A8, 19(A15))                                     OUTELE.......18800
      ELSE                                                               OUTELE.......18900
         WRITE(K6,983) COLTK6(J6COL(1))(3:15),                           OUTELE.......19000
     1      (COLTK6(J6COL(M)), M=2,NCOLS6)                               OUTELE.......19100
  983    FORMAT ("## ", A13, 19(A15))                                    OUTELE.......19200
      END IF                                                             OUTELE.......19300
C                                                                        OUTELE.......19400
C.....VELOCITY DATA FOR THIS TIME STEP                                   OUTELE.......19500
      RN48 = 1D0/DBLE(N48)                                               OUTELE.......19600
      DO 5000 L=1, NE                                                    OUTELE.......19700
         CENTRX = 0D0                                                    OUTELE.......19800
         CENTRY = 0D0                                                    OUTELE.......19900
         CENTRZ = 0D0                                                    OUTELE.......20000
         DO 1400 II=1, N48                                               OUTELE.......20100
            III=II+(L-1)*N48                                             OUTELE.......20200
            IIN(II)=IN(III)                                              OUTELE.......20300
            CENTRX = CENTRX + X(IIN(II))                                 OUTELE.......20400
            CENTRY = CENTRY + Y(IIN(II))                                 OUTELE.......20500
            CENTRZ = CENTRZ + Z(IIN(II))                                 OUTELE.......20600
 1400    CONTINUE                                                        OUTELE.......20700
         CENTRX = CENTRX*RN48                                            OUTELE.......20800
         CENTRY = CENTRY*RN48                                            OUTELE.......20900
         CENTRZ = CENTRZ*RN48                                            OUTELE.......21000
         VA1 = 0.017453292D0*VANG1(L)                                    OUTELE.......21100
         LL = MIN(L, NEX)                                                OUTELE.......21200
         VA2 = 0.017453292D0*VANG2(LL)*DKTM2                             OUTELE.......21300
         CVA2 = DCOS(VA2)                                                OUTELE.......21400
         VECTRX=VMAG(L) * DCOS(VA1)*CVA2                                 OUTELE.......21500
         VECTRY=VMAG(L) * DSIN(VA1)*CVA2                                 OUTELE.......21600
         VECTRZ=VMAG(L) * DSIN(VA2)                                      OUTELE.......21700
         VVAR(1) = DBLE(L)                                               OUTELE.......21800
         VVAR(2) = CENTRX                                                OUTELE.......21900
         VVAR(3) = CENTRY                                                OUTELE.......22000
         VVAR(4) = CENTRZ                                                OUTELE.......22100
         VVAR(5) = VECTRX                                                OUTELE.......22200
         VVAR(6) = VECTRY                                                OUTELE.......22300
         VVAR(7) = VECTRZ                                                OUTELE.......22400
         DO 1984 M=1,NCOLS6                                              OUTELE.......22500
            VCOL(M) = VVAR(J6COL(M))                                     OUTELE.......22600
 1984    CONTINUE                                                        OUTELE.......22700
         IF (PRINTE) THEN                                                OUTELE.......22800
            WRITE(K6,1985) L,(VCOL(M), M=2,NCOLS6)                       OUTELE.......22900
 1985       FORMAT (I9, 2X, 19(1PE15.7))                                 OUTELE.......23000
         ELSE                                                            OUTELE.......23100
            WRITE(K6,1986) (VCOL(M), M=1,NCOLS6)                         OUTELE.......23200
 1986       FORMAT (1X, 20(1PE15.7))                                     OUTELE.......23300
         END IF                                                          OUTELE.......23400
 5000 CONTINUE                                                           OUTELE.......23500
C                                                                        OUTELE.......23600
 9999 CONTINUE                                                           OUTELE.......23700
      RETURN                                                             OUTELE.......23800
C                                                                        OUTELE.......23900
      END                                                                OUTELE.......24000
C                                                                        OUTELE.......24100
C     SUBROUTINE        O  U  T  L  S  T  2        SUTRA VERSION 2D3D.1  OUTLST2........100
C                                                                        OUTLST2........200
C *** PURPOSE :                                                          OUTLST2........300
C ***  TO PRINT PRESSURE AND TEMPERATURE OR CONCENTRATION                OUTLST2........400
C ***  SOLUTIONS AND TO OUTPUT INFORMATION ON TIME STEP, ITERATIONS,     OUTLST2........500
C ***  SATURATIONS, AND FLUID VELOCITIES FOR 2D PROBLEMS.                OUTLST2........600
C ***  OUTPUT IS TO THE LST FILE.                                        OUTLST2........700
C                                                                        OUTLST2........800
      SUBROUTINE OUTLST2(ML,ISTOP,IGOI,IERRP,ITRSP,ERRP,                 OUTLST2........900
     1   IERRU,ITRSU,ERRU,PVEC,UVEC,VMAG,VANG,SW)                        OUTLST2.......1000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTLST2.......1100
      DIMENSION PVEC(NNVEC),UVEC(NNVEC),VMAG(NE),VANG(NE),SW(NN)         OUTLST2.......1200
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTLST2.......1300
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             OUTLST2.......1400
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTLST2.......1500
     1   NSOP,NSOU,NBCN                                                  OUTLST2.......1600
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   OUTLST2.......1700
      COMMON /DIMX2/ NELTA,NNVEC,NDIMJA                                  OUTLST2.......1800
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        OUTLST2.......1900
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      OUTLST2.......2000
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     OUTLST2.......2100
     1   KSCRN,KPAUSE                                                    OUTLST2.......2200
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTLST2.......2300
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTLST2.......2400
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  OUTLST2.......2500
C                                                                        OUTLST2.......2600
C.....OUTPUT MAJOR HEADINGS FOR CURRENT TIME STEP                        OUTLST2.......2700
      IF(IT.GT.0.OR.ISSFLO.EQ.2.OR.ISSTRA.EQ.1) GOTO 100                 OUTLST2.......2800
      WRITE(K3,60)                                                       OUTLST2.......2900
   60 FORMAT(1H1////11X,'I N I T I A L   C O N D I T I O N S',           OUTLST2.......3000
     1             /11X,'___________________________________')           OUTLST2.......3100
      IF(IREAD.EQ.-1) WRITE(K3,65)                                       OUTLST2.......3200
   65 FORMAT(//11X,'INITIAL CONDITIONS RETRIEVED FROM A RESTART',        OUTLST2.......3300
     1   ' FILE (WARM START).')                                          OUTLST2.......3400
      GOTO 500                                                           OUTLST2.......3500
C                                                                        OUTLST2.......3600
  100 WRITE(K3,350) IT                                                   OUTLST2.......3700
  350 FORMAT(1H1//11X,'RESULTS FOR TIME STEP ',I4/                       OUTLST2.......3800
     1   11X,'_______ ___ ____ ____ ____')                               OUTLST2.......3900
C                                                                        OUTLST2.......4000
      IF(ITRMAX.GT.1) THEN                                               OUTLST2.......4100
         IF(IGOI.EQ.0) THEN                                              OUTLST2.......4200
            WRITE(K3,355) ITER                                           OUTLST2.......4300
         ELSE                                                            OUTLST2.......4400
            WRITE(K3,356) ITER                                           OUTLST2.......4500
         END IF                                                          OUTLST2.......4600
  355    FORMAT(/11X,'NON-LINEARITY ITERATIONS CONVERGED AFTER ',I5,     OUTLST2.......4700
     1      ' ITERATIONS')                                               OUTLST2.......4800
  356    FORMAT(/11X,'NON-LINEARITY ITERATIONS  N O T  CONVERGED',       OUTLST2.......4900
     1      ' AFTER ',I5,' ITERATIONS')                                  OUTLST2.......5000
         WRITE(K3,450) RPM,IPWORS,RUM,IUWORS                             OUTLST2.......5100
  450    FORMAT(11X,'MAXIMUM P CHANGE FROM PREVIOUS ITERATION ',         OUTLST2.......5200
     1      1PD14.5,' AT NODE ',I9/11X,'MAXIMUM U CHANGE FROM PREVIOUS', OUTLST2.......5300
     2      ' ITERATION ',1PD14.5,' AT NODE ',I9)                        OUTLST2.......5400
      END IF                                                             OUTLST2.......5500
C                                                                        OUTLST2.......5600
      IF ((ML.EQ.0).OR.(ML.EQ.1)) THEN                                   OUTLST2.......5700
         IF (KSOLVP.EQ.0) THEN                                           OUTLST2.......5800
            WRITE(K3,452)                                                OUTLST2.......5900
         ELSE IF (IERRP.EQ.0) THEN                                       OUTLST2.......6000
            WRITE(K3,455) ITRSP, ERRP                                    OUTLST2.......6100
         ELSE                                                            OUTLST2.......6200
            WRITE(K3,456) ITRSP, ERRP                                    OUTLST2.......6300
         END IF                                                          OUTLST2.......6400
      END IF                                                             OUTLST2.......6500
      IF ((ML.EQ.0).OR.(ML.EQ.2)) THEN                                   OUTLST2.......6600
         IF (ML.EQ.2) WRITE(K3,*) ' '                                    OUTLST2.......6700
         IF (KSOLVU.EQ.0) THEN                                           OUTLST2.......6800
            WRITE(K3,453)                                                OUTLST2.......6900
         ELSE IF (IERRU.EQ.0) THEN                                       OUTLST2.......7000
            WRITE(K3,457) ITRSU, ERRU                                    OUTLST2.......7100
         ELSE                                                            OUTLST2.......7200
            WRITE(K3,458) ITRSU, ERRU                                    OUTLST2.......7300
         END IF                                                          OUTLST2.......7400
      END IF                                                             OUTLST2.......7500
  452 FORMAT(/11X,'P-SOLUTION COMPUTED USING DIRECT SOLVER')             OUTLST2.......7600
  453 FORMAT(11X,'U-SOLUTION COMPUTED USING DIRECT SOLVER')              OUTLST2.......7700
  455 FORMAT(/11X,'P-SOLUTION CONVERGED AFTER ',I5,' MATRIX'             OUTLST2.......7800
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PD14.5)                 OUTLST2.......7900
  456 FORMAT(/11X,'P-SOLUTION  F A I L E D  AFTER ',I5,' MATRIX'         OUTLST2.......8000
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PD14.5)                 OUTLST2.......8100
  457 FORMAT(11X,'U-SOLUTION CONVERGED AFTER ',I5,' MATRIX'              OUTLST2.......8200
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PD14.5)                 OUTLST2.......8300
  458 FORMAT(11X,'U-SOLUTION  F A I L E D  AFTER ',I5,' MATRIX'          OUTLST2.......8400
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PD14.5)                 OUTLST2.......8500
C                                                                        OUTLST2.......8600
  500 IF(IT.EQ.0.AND.ISSFLO.EQ.2) GOTO 680                               OUTLST2.......8700
      IF(ISSTRA.EQ.1) GOTO 800                                           OUTLST2.......8800
      WRITE(K3,550) DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,                     OUTLST2.......8900
     1   TMONTH,TYEAR                                                    OUTLST2.......9000
  550 FORMAT(///11X,'TIME INCREMENT :',T27,1PD15.4,' SECONDS'//11X,      OUTLST2.......9100
     1   'TIME AT END',3X,T27,1PD15.4,' SECONDS',/11X,'OF STEP:',6X,T27, OUTLST2.......9200
     2   1PD15.4,' MINUTES'/T27,1PD15.4,' HOURS'/T27,1PD15.4,' DAYS'     OUTLST2.......9300
     3   /T27,1PD15.4,' WEEKS'/T27,1PD15.4,' MONTHS'/T27,1PD15.4,        OUTLST2.......9400
     4   ' YEARS')                                                       OUTLST2.......9500
C                                                                        OUTLST2.......9600
C.....OUTPUT PRESSURES FOR TRANSIENT FLOW SOLUTION (AND, POSSIBLY,       OUTLST2.......9700
C        SATURATION AND VELOCITY)                                        OUTLST2.......9800
      IF(ML.EQ.2.AND.ISTOP.GE.0) GOTO 700                                OUTLST2.......9900
      IF(ISSFLO.GT.0) GOTO 700                                           OUTLST2......10000
      WRITE(K3,650) (I,PVEC(I),I=1,NN)                                   OUTLST2......10100
  650 FORMAT(///11X,'P  R  E  S  S  U  R  E'                             OUTLST2......10200
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PD15.8)))                 OUTLST2......10300
      IF(IUNSAT.NE.0) WRITE(K3,651) (I,SW(I),I=1,NN)                     OUTLST2......10400
  651 FORMAT(///11X,'S  A  T  U  R  A  T  I  O  N'                       OUTLST2......10500
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PD15.8)))                 OUTLST2......10600
      IF(KVEL.EQ.1.AND.IT.GT.0) WRITE(K3,655) (L,VMAG(L),L=1,NE)         OUTLST2......10700
      IF(KVEL.EQ.1.AND.IT.GT.0) WRITE(K3,656) (L,VANG(L),L=1,NE)         OUTLST2......10800
  655 FORMAT(///11X,'F  L  U  I  D     V  E  L  O  C  I  T  Y'//         OUTLST2......10900
     1   11X,'M A G N I T U D E   AT CENTROID OF ELEMENT'//              OUTLST2......11000
     2   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PD15.8)))                OUTLST2......11100
  656 FORMAT(///11X,'F  L  U  I  D     V  E  L  O  C  I  T  Y'//         OUTLST2......11200
     1   11X,'A N G L E   IN DEGREES FROM +X-AXIS TO FLOW DIRECTION ',   OUTLST2......11300
     2   'AT CENTROID OF ELEMENT'//                                      OUTLST2......11400
     3   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PD15.8)))                OUTLST2......11500
      GOTO 700                                                           OUTLST2......11600
C                                                                        OUTLST2......11700
C.....OUTPUT PRESSURES FOR STEADY-STATE FLOW SOLUTION                    OUTLST2......11800
  680 WRITE(K3,690) (I,PVEC(I),I=1,NN)                                   OUTLST2......11900
  690 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     P  R  E  S', OUTLST2......12000
     1   '  S  U  R  E'//2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PD15.8)))   OUTLST2......12100
      IF(IUNSAT.NE.0) WRITE(K3,651) (I,SW(I),I=1,NN)                     OUTLST2......12200
      GOTO 1000                                                          OUTLST2......12300
C                                                                        OUTLST2......12400
C.....OUTPUT CONCENTRATIONS OR TEMPERATURES FOR                          OUTLST2......12500
C        TRANSIENT TRANSPORT SOLUTION                                    OUTLST2......12600
  700 IF(ML.EQ.1.AND.ISTOP.GE.0) GOTO 1000                               OUTLST2......12700
      IF(ME) 720,720,730                                                 OUTLST2......12800
  720 WRITE(K3,725) (I,UVEC(I),I=1,NN)                                   OUTLST2......12900
  725 FORMAT(///11X,'C  O  N  C  E  N  T  R  A  T  I  O  N'              OUTLST2......13000
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PD15.8)))                 OUTLST2......13100
      GOTO 900                                                           OUTLST2......13200
  730 WRITE(K3,735) (I,UVEC(I),I=1,NN)                                   OUTLST2......13300
  735 FORMAT(///11X,'T  E  M  P  E  R  A  T  U  R  E'                    OUTLST2......13400
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PD15.8)))                 OUTLST2......13500
      GOTO 900                                                           OUTLST2......13600
C                                                                        OUTLST2......13700
C.....OUTPUT CONCENTRATIONS OR TEMPERATURES FOR                          OUTLST2......13800
C        STEADY-STATE TRANSPORT SOLUTION                                 OUTLST2......13900
  800 IF(ME) 820,820,830                                                 OUTLST2......14000
  820 WRITE(K3,825) (I,UVEC(I),I=1,NN)                                   OUTLST2......14100
  825 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     C  O  N  C', OUTLST2......14200
     1   '  E  N  T  R  A  T  I  O  N'                                   OUTLST2......14300
     2   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PD15.8)))                 OUTLST2......14400
      GOTO 900                                                           OUTLST2......14500
  830 WRITE(K3,835) (I,UVEC(I),I=1,NN)                                   OUTLST2......14600
  835 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     T  E  M  P', OUTLST2......14700
     1   '  E  R  A  T  U  R  E'                                         OUTLST2......14800
     2   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PD15.8)))                 OUTLST2......14900
C                                                                        OUTLST2......15000
C.....OUTPUT VELOCITIES FOR STEADY-STATE FLOW SOLUTION                   OUTLST2......15100
  900 IF(ISSFLO.NE.2.OR.IT.NE.1.OR.KVEL.NE.1) GOTO 1000                  OUTLST2......15200
      WRITE(K3,925) (L,VMAG(L),L=1,NE)                                   OUTLST2......15300
      WRITE(K3,950) (L,VANG(L),L=1,NE)                                   OUTLST2......15400
  925 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     ',           OUTLST2......15500
     1   'F  L  U  I  D     V  E  L  O  C  I  T  Y'//                    OUTLST2......15600
     2   11X,'M A G N I T U D E   AT CENTROID OF ELEMENT'//              OUTLST2......15700
     3   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PD15.8)))                OUTLST2......15800
  950 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     ',           OUTLST2......15900
     1   'F  L  U  I  D     V  E  L  O  C  I  T  Y'//                    OUTLST2......16000
     2   11X,'A N G L E   IN DEGREES FROM +X-AXIS TO FLOW DIRECTION ',   OUTLST2......16100
     3   'AT CENTROID OF ELEMENT'//                                      OUTLST2......16200
     4   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PD15.8)))                OUTLST2......16300
C                                                                        OUTLST2......16400
 1000 RETURN                                                             OUTLST2......16500
C                                                                        OUTLST2......16600
      END                                                                OUTLST2......16700
C                                                                        OUTLST2......16800
C     SUBROUTINE        O  U  T  L  S  T  3        SUTRA VERSION 2D3D.1  OUTLST3........100
C                                                                        OUTLST3........200
C *** PURPOSE :                                                          OUTLST3........300
C ***  TO PRINT PRESSURE AND TEMPERATURE OR CONCENTRATION                OUTLST3........400
C ***  SOLUTIONS AND TO OUTPUT INFORMATION ON TIME STEP, ITERATIONS,     OUTLST3........500
C ***  SATURATIONS, AND FLUID VELOCITIES FOR 3D PROBLEMS.                OUTLST3........600
C ***  OUTPUT IS TO THE LST FILE.                                        OUTLST3........700
C                                                                        OUTLST3........800
      SUBROUTINE OUTLST3(ML,ISTOP,IGOI,IERRP,ITRSP,ERRP,                 OUTLST3........900
     1   IERRU,ITRSU,ERRU,PVEC,UVEC,VMAG,VANG1,VANG2,SW)                 OUTLST3.......1000
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTLST3.......1100
      DIMENSION PVEC(NNVEC),UVEC(NNVEC),VMAG(NE),VANG1(NE),VANG2(NEX),   OUTLST3.......1200
     1   SW(NN)                                                          OUTLST3.......1300
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTLST3.......1400
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             OUTLST3.......1500
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTLST3.......1600
     1   NSOP,NSOU,NBCN                                                  OUTLST3.......1700
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   OUTLST3.......1800
      COMMON /DIMX2/ NELTA,NNVEC,NDIMJA                                  OUTLST3.......1900
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        OUTLST3.......2000
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      OUTLST3.......2100
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     OUTLST3.......2200
     1   KSCRN,KPAUSE                                                    OUTLST3.......2300
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTLST3.......2400
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTLST3.......2500
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  OUTLST3.......2600
C                                                                        OUTLST3.......2700
C.....OUTPUT MAJOR HEADINGS FOR CURRENT TIME STEP                        OUTLST3.......2800
      IF(IT.GT.0.OR.ISSFLO.EQ.2.OR.ISSTRA.EQ.1) GOTO 100                 OUTLST3.......2900
      WRITE(K3,60)                                                       OUTLST3.......3000
   60 FORMAT(1H1////11X,'I N I T I A L   C O N D I T I O N S',           OUTLST3.......3100
     1             /11X,'___________________________________')           OUTLST3.......3200
      IF(IREAD.EQ.-1) WRITE(K3,65)                                       OUTLST3.......3300
   65 FORMAT(//11X,'INITIAL CONDITIONS RETRIEVED FROM A RESTART',        OUTLST3.......3400
     1   ' FILE (WARM START).')                                          OUTLST3.......3500
      GOTO 500                                                           OUTLST3.......3600
C                                                                        OUTLST3.......3700
  100 WRITE(K3,350) IT                                                   OUTLST3.......3800
  350 FORMAT(1H1//11X,'RESULTS FOR TIME STEP ',I4/                       OUTLST3.......3900
     1   11X,'_______ ___ ____ ____ ____')                               OUTLST3.......4000
C                                                                        OUTLST3.......4100
      IF(ITRMAX.GT.1) THEN                                               OUTLST3.......4200
         IF(IGOI.EQ.0) THEN                                              OUTLST3.......4300
            WRITE(K3,355) ITER                                           OUTLST3.......4400
         ELSE                                                            OUTLST3.......4500
            WRITE(K3,356) ITER                                           OUTLST3.......4600
         END IF                                                          OUTLST3.......4700
  355    FORMAT(/11X,'NON-LINEARITY ITERATIONS CONVERGED AFTER ',I5,     OUTLST3.......4800
     1      ' ITERATIONS')                                               OUTLST3.......4900
  356    FORMAT(/11X,'NON-LINEARITY ITERATIONS  N O T  CONVERGED',       OUTLST3.......5000
     1      ' AFTER ',I5,' ITERATIONS')                                  OUTLST3.......5100
         WRITE(K3,450) RPM,IPWORS,RUM,IUWORS                             OUTLST3.......5200
  450    FORMAT(11X,'MAXIMUM P CHANGE FROM PREVIOUS ITERATION ',         OUTLST3.......5300
     1      1PD14.5,' AT NODE ',I9/11X,'MAXIMUM U CHANGE FROM PREVIOUS', OUTLST3.......5400
     2      ' ITERATION ',1PD14.5,' AT NODE ',I9)                        OUTLST3.......5500
      END IF                                                             OUTLST3.......5600
C                                                                        OUTLST3.......5700
      IF ((ML.EQ.0).OR.(ML.EQ.1)) THEN                                   OUTLST3.......5800
         IF (KSOLVP.EQ.0) THEN                                           OUTLST3.......5900
            WRITE(K3,452)                                                OUTLST3.......6000
         ELSE IF (IERRP.EQ.0) THEN                                       OUTLST3.......6100
            WRITE(K3,455) ITRSP, ERRP                                    OUTLST3.......6200
         ELSE                                                            OUTLST3.......6300
            WRITE(K3,456) ITRSP, ERRP                                    OUTLST3.......6400
         END IF                                                          OUTLST3.......6500
      END IF                                                             OUTLST3.......6600
      IF ((ML.EQ.0).OR.(ML.EQ.2)) THEN                                   OUTLST3.......6700
         IF (ML.EQ.2) WRITE(K3,*) ' '                                    OUTLST3.......6800
         IF (KSOLVU.EQ.0) THEN                                           OUTLST3.......6900
            WRITE(K3,453)                                                OUTLST3.......7000
         ELSE IF (IERRU.EQ.0) THEN                                       OUTLST3.......7100
            WRITE(K3,457) ITRSU, ERRU                                    OUTLST3.......7200
         ELSE                                                            OUTLST3.......7300
            WRITE(K3,458) ITRSU, ERRU                                    OUTLST3.......7400
         END IF                                                          OUTLST3.......7500
      END IF                                                             OUTLST3.......7600
  452 FORMAT(/11X,'P-SOLUTION COMPUTED USING DIRECT SOLVER')             OUTLST3.......7700
  453 FORMAT(11X,'U-SOLUTION COMPUTED USING DIRECT SOLVER')              OUTLST3.......7800
  455 FORMAT(/11X,'P-SOLUTION CONVERGED AFTER ',I5,' MATRIX'             OUTLST3.......7900
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PD14.5)                 OUTLST3.......8000
  456 FORMAT(/11X,'P-SOLUTION  F A I L E D  AFTER ',I5,' MATRIX'         OUTLST3.......8100
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PD14.5)                 OUTLST3.......8200
  457 FORMAT(11X,'U-SOLUTION CONVERGED AFTER ',I5,' MATRIX'              OUTLST3.......8300
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PD14.5)                 OUTLST3.......8400
  458 FORMAT(11X,'U-SOLUTION  F A I L E D  AFTER ',I5,' MATRIX'          OUTLST3.......8500
     1   ' SOLVER ITERATIONS; ESTIMATED ERROR ',1PD14.5)                 OUTLST3.......8600
C                                                                        OUTLST3.......8700
  500 IF(IT.EQ.0.AND.ISSFLO.EQ.2) GOTO 680                               OUTLST3.......8800
      IF(ISSTRA.EQ.1) GOTO 800                                           OUTLST3.......8900
      WRITE(K3,550) DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,                     OUTLST3.......9000
     1   TMONTH,TYEAR                                                    OUTLST3.......9100
  550 FORMAT(///11X,'TIME INCREMENT :',T27,1PD15.4,' SECONDS'//11X,      OUTLST3.......9200
     1   'TIME AT END',3X,T27,1PD15.4,' SECONDS',/11X,'OF STEP:',6X,T27, OUTLST3.......9300
     2   1PD15.4,' MINUTES'/T27,1PD15.4,' HOURS'/T27,1PD15.4,' DAYS'     OUTLST3.......9400
     3   /T27,1PD15.4,' WEEKS'/T27,1PD15.4,' MONTHS'/T27,1PD15.4,        OUTLST3.......9500
     4   ' YEARS')                                                       OUTLST3.......9600
C                                                                        OUTLST3.......9700
C.....OUTPUT PRESSURES FOR TRANSIENT FLOW SOLUTION (AND, POSSIBLY,       OUTLST3.......9800
C        SATURATION AND VELOCITY)                                        OUTLST3.......9900
      IF(ML.EQ.2.AND.ISTOP.GE.0) GOTO 700                                OUTLST3......10000
      IF(ISSFLO.GT.0) GOTO 700                                           OUTLST3......10100
      WRITE(K3,650) (I,PVEC(I),I=1,NN)                                   OUTLST3......10200
  650 FORMAT(///11X,'P  R  E  S  S  U  R  E'                             OUTLST3......10300
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PD15.8)))                 OUTLST3......10400
      IF(IUNSAT.NE.0) WRITE(K3,651) (I,SW(I),I=1,NN)                     OUTLST3......10500
  651 FORMAT(///11X,'S  A  T  U  R  A  T  I  O  N'                       OUTLST3......10600
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PD15.8)))                 OUTLST3......10700
      IF(KVEL.EQ.1.AND.IT.GT.0) THEN                                     OUTLST3......10800
         WRITE(K3,655) (L,VMAG(L),L=1,NE)                                OUTLST3......10900
         WRITE(K3,656) (L,VANG1(L),L=1,NE)                               OUTLST3......11000
         WRITE(K3,657) (L,VANG2(L),L=1,NE)                               OUTLST3......11100
      END IF                                                             OUTLST3......11200
  655 FORMAT(///11X,'F  L  U  I  D     V  E  L  O  C  I  T  Y'//         OUTLST3......11300
     1   11X,'M A G N I T U D E   AT CENTROID OF ELEMENT'//              OUTLST3......11400
     2   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PD15.8)))                OUTLST3......11500
  656 FORMAT(///11X,'F  L  U  I  D     V  E  L  O  C  I  T  Y'//         OUTLST3......11600
     1   11X,'A N G L E 1   AT CENTROID OF ELEMENT, IN DEGREES FROM ',   OUTLST3......11700
     2   '+X-AXIS TO PROJECTION OF FLOW DIRECTION IN XY-PLANE'//         OUTLST3......11800
     3   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PD15.8)))                OUTLST3......11900
  657 FORMAT(///11X,'F  L  U  I  D     V  E  L  O  C  I  T  Y'//         OUTLST3......12000
     1   11X,'A N G L E 2   AT CENTROID OF ELEMENT, IN DEGREES FROM ',   OUTLST3......12100
     2   'XY-PLANE TO FLOW DIRECTION'//                                  OUTLST3......12200
     3   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PD15.8)))                OUTLST3......12300
C     END IF                                                             OUTLST3......12400
      GOTO 700                                                           OUTLST3......12500
C                                                                        OUTLST3......12600
C.....OUTPUT PRESSURES FOR STEADY-STATE FLOW SOLUTION                    OUTLST3......12700
  680 WRITE(K3,690) (I,PVEC(I),I=1,NN)                                   OUTLST3......12800
  690 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     P  R  E  S', OUTLST3......12900
     1   '  S  U  R  E'//2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PD15.8)))   OUTLST3......13000
      IF(IUNSAT.NE.0) WRITE(K3,651) (I,SW(I),I=1,NN)                     OUTLST3......13100
      GOTO 1000                                                          OUTLST3......13200
C                                                                        OUTLST3......13300
C.....OUTPUT CONCENTRATIONS OR TEMPERATURES FOR                          OUTLST3......13400
C        TRANSIENT TRANSPORT SOLUTION                                    OUTLST3......13500
  700 IF(ML.EQ.1.AND.ISTOP.GE.0) GOTO 1000                               OUTLST3......13600
      IF(ME) 720,720,730                                                 OUTLST3......13700
  720 WRITE(K3,725) (I,UVEC(I),I=1,NN)                                   OUTLST3......13800
  725 FORMAT(///11X,'C  O  N  C  E  N  T  R  A  T  I  O  N'              OUTLST3......13900
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PD15.8)))                 OUTLST3......14000
      GOTO 900                                                           OUTLST3......14100
  730 WRITE(K3,735) (I,UVEC(I),I=1,NN)                                   OUTLST3......14200
  735 FORMAT(///11X,'T  E  M  P  E  R  A  T  U  R  E'                    OUTLST3......14300
     1   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PD15.8)))                 OUTLST3......14400
      GOTO 900                                                           OUTLST3......14500
C                                                                        OUTLST3......14600
C.....OUTPUT CONCENTRATIONS OR TEMPERATURES FOR                          OUTLST3......14700
C        STEADY-STATE TRANSPORT SOLUTION                                 OUTLST3......14800
  800 IF(ME) 820,820,830                                                 OUTLST3......14900
  820 WRITE(K3,825) (I,UVEC(I),I=1,NN)                                   OUTLST3......15000
  825 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     C  O  N  C', OUTLST3......15100
     1   '  E  N  T  R  A  T  I  O  N'                                   OUTLST3......15200
     2   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PD15.8)))                 OUTLST3......15300
      GOTO 900                                                           OUTLST3......15400
  830 WRITE(K3,835) (I,UVEC(I),I=1,NN)                                   OUTLST3......15500
  835 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     T  E  M  P', OUTLST3......15600
     1   '  E  R  A  T  U  R  E'                                         OUTLST3......15700
     2   //2X,5(6X,'NODE',16X)/(2X,5(1X,I9,1X,1PD15.8)))                 OUTLST3......15800
C                                                                        OUTLST3......15900
C.....OUTPUT VELOCITIES FOR STEADY-STATE FLOW SOLUTION                   OUTLST3......16000
  900 IF(ISSFLO.NE.2.OR.IT.NE.1.OR.KVEL.NE.1) GOTO 1000                  OUTLST3......16100
      WRITE(K3,925) (L,VMAG(L),L=1,NE)                                   OUTLST3......16200
      WRITE(K3,950) (L,VANG1(L),L=1,NE)                                  OUTLST3......16300
      WRITE(K3,951) (L,VANG2(L),L=1,NE)                                  OUTLST3......16400
  925 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     ',           OUTLST3......16500
     1   'F  L  U  I  D     V  E  L  O  C  I  T  Y'//                    OUTLST3......16600
     2   11X,'M A G N I T U D E   AT CENTROID OF ELEMENT'//              OUTLST3......16700
     3   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PD15.8)))                OUTLST3......16800
  950 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     ',           OUTLST3......16900
     1   'F  L  U  I  D     V  E  L  O  C  I  T  Y'//                    OUTLST3......17000
     2   11X,'A N G L E 1   AT CENTROID OF ELEMENT, IN DEGREES FROM ',   OUTLST3......17100
     3   '+X-AXIS TO PROJECTION OF FLOW DIRECTION IN XY-PLANE'//         OUTLST3......17200
     4   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PD15.8)))                OUTLST3......17300
  951 FORMAT(///11X,'S  T  E  A  D  Y  -  S  T  A  T  E     ',           OUTLST3......17400
     1   'F  L  U  I  D     V  E  L  O  C  I  T  Y'//                    OUTLST3......17500
     2   11X,'A N G L E 2   AT CENTROID OF ELEMENT, IN DEGREES FROM ',   OUTLST3......17600
     3   'XY-PLANE TO FLOW DIRECTION'//                                  OUTLST3......17700
     4   2X,5(3X,'ELEMENT',16X)/(2X,5(1X,I9,1X,1PD15.8)))                OUTLST3......17800
C                                                                        OUTLST3......17900
 1000 RETURN                                                             OUTLST3......18000
C                                                                        OUTLST3......18100
      END                                                                OUTLST3......18200
C                                                                        OUTLST3......18300
C     SUBROUTINE        O  U  T  N  O  D           SUTRA VERSION 2D3D.1  OUTNOD.........100
C                                                                        OUTNOD.........200
C *** PURPOSE :                                                          OUTNOD.........300
C ***  TO PRINT NODE COORDINATES, PRESSURES, CONCENTRATIONS OR           OUTNOD.........400
C ***  TEMPERATURES, AND SATURATIONS IN A FLEXIBLE, COLUMNWISE FORMAT.   OUTNOD.........500
C ***  OUTPUT IS TO THE NOD FILE.                                        OUTNOD.........600
C                                                                        OUTNOD.........700
      SUBROUTINE OUTNOD(PVEC,UVEC,SW,IN,X,Y,Z,TITLE1,TITLE2)             OUTNOD.........800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTNOD.........900
      PARAMETER (NCOLMX=9)                                               OUTNOD........1000
      CHARACTER*1  TITLE1(80),TITLE2(80)                                 OUTNOD........1100
      CHARACTER*8  HORP                                                  OUTNOD........1200
      CHARACTER*13 TORC                                                  OUTNOD........1300
      CHARACTER*15 COLTK5(7)                                             OUTNOD........1400
      CHARACTER*1 CPHORP,CPTORC,CPSATU                                   OUTNOD........1500
      LOGICAL ONCEK5,ONCEK6,ONCEK7                                       OUTNOD........1600
      LOGICAL PRINTN                                                     OUTNOD........1700
      DIMENSION IN(NIN),IIN(8)                                           OUTNOD........1800
      DIMENSION PVEC(NNVEC),UVEC(NNVEC),SW(NN)                           OUTNOD........1900
      DIMENSION X(NN),Y(NN),Z(NN)                                        OUTNOD........2000
      DIMENSION VCOL(NCOLMX),VVAR(7)                                     OUTNOD........2100
      DIMENSION J5COL(NCOLMX),J6COL(NCOLMX)                              OUTNOD........2200
      ALLOCATABLE TT(:),ITT(:),ISTORC(:),ISHORP(:),ISSATU(:)             OUTNOD........2300
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTNOD........2400
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             OUTNOD........2500
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTNOD........2600
     1   NSOP,NSOU,NBCN                                                  OUTNOD........2700
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   OUTNOD........2800
      COMMON /DIMX2/ NELTA,NNVEC,NDIMJA                                  OUTNOD........2900
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        OUTNOD........3000
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  OUTNOD........3100
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      OUTNOD........3200
      COMMON /JCOLS/ NCOLPR,LCOLPR,NCOLS5,NCOLS6,J5COL,J6COL             OUTNOD........3300
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     OUTNOD........3400
     1   KSCRN,KPAUSE                                                    OUTNOD........3500
      COMMON /PLT1/ ONCEK5,ONCEK6,ONCEK7                                 OUTNOD........3600
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTNOD........3700
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTNOD........3800
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  OUTNOD........3900
      DATA (COLTK5(MM), MM=1,7) /'Node',                                 OUTNOD........4000
     1   '              X', '              Y', '              Z',        OUTNOD........4100
     2   '       Pressure', '  Concentration', '     Saturation'/        OUTNOD........4200
      SAVE COLTK5                                                        OUTNOD........4300
C                                                                        OUTNOD........4400
C.....CALCULATE HEADERS ON FIRST CALL AND CREATE OUTPUT ON EACH CALL.    OUTNOD........4500
C                                                                        OUTNOD........4600
      DKTM2 = DBLE(IABS(KTYPE) - 2)                                      OUTNOD........4700
C                                                                        OUTNOD........4800
      IF (.NOT. ONCEK5)  THEN                                            OUTNOD........4900
C.....FIRST CALL -- CREATE FILE HEADER.                                  OUTNOD........5000
C                                                                        OUTNOD........5100
C........CALCULATE THE MAXIMUM NUMBER OF TIME STEPS, KTMAX.              OUTNOD........5200
         TS=TSTART                                                       OUTNOD........5300
         JT=0                                                            OUTNOD........5400
         IF (ISSTRA.NE.1) THEN                                           OUTNOD........5500
            KT = 1                                                       OUTNOD........5600
         ELSE                                                            OUTNOD........5700
            KT = 0                                                       OUTNOD........5800
         END IF                                                          OUTNOD........5900
         DELTK=DELT                                                      OUTNOD........6000
    4    CONTINUE                                                        OUTNOD........6100
            JT=JT+1                                                      OUTNOD........6200
            IF (MOD(JT,ITCYC).EQ.0 .AND. JT.GT.1) DELTK=DELTK*DTMULT     OUTNOD........6300
            IF (DELTK.GT.DTMAX) DELTK=DTMAX                              OUTNOD........6400
            TS=TS+DELTK                                                  OUTNOD........6500
            IF (MOD(JT,NCOLPR).EQ.0 .OR.                                 OUTNOD........6600
     1         ((JT.EQ.1).AND.((ISSTRA.NE.0).OR.(NCOLPR.GT.0))))         OUTNOD........6700
     2         KT = KT + 1                                               OUTNOD........6800
         IF(JT.LT.ITMAX .AND. TS.LT.TMAX) GOTO 4                         OUTNOD........6900
         JTMAX = JT                                                      OUTNOD........7000
         IF(JTMAX.GT.1 .AND. MOD(JT,NCOLPR).NE.0) KT = KT + 1            OUTNOD........7100
         KTMAX = KT                                                      OUTNOD........7200
C                                                                        OUTNOD........7300
C........ALLOCATE LOCAL ARRAYS                                           OUTNOD........7400
         ALLOCATE(TT(KTMAX),ITT(KTMAX))                                  OUTNOD........7500
         ALLOCATE(ISTORC(KTMAX),ISHORP(KTMAX),ISSATU(KTMAX))             OUTNOD........7600
C                                                                        OUTNOD........7700
C........CALCULATE AND PRINT TIME STEP INFORMATION                       OUTNOD........7800
         TS=TSTART                                                       OUTNOD........7900
C........TIME STEP VALUE                                                 OUTNOD........8000
         JT=0                                                            OUTNOD........8100
C........NUMBER OF PRINTED TIME STEPS                                    OUTNOD........8200
         KT=0                                                            OUTNOD........8300
C........TIME STEP INCREMENT                                             OUTNOD........8400
         DELTK=DELT                                                      OUTNOD........8500
C........INDICATORS OF WHEN VARIABLES ARE CALCULATED AND PRINTED         OUTNOD........8600
         LCHORP = 0                                                      OUTNOD........8700
         LCTORC = 0                                                      OUTNOD........8800
         CPHORP = 'N'                                                    OUTNOD........8900
         CPTORC = 'N'                                                    OUTNOD........9000
         CPSATU = 'N'                                                    OUTNOD........9100
         DO 8 M=1,NCOLS5                                                 OUTNOD........9200
            IF (J5COL(M).EQ.5) CPHORP = 'Y'                              OUTNOD........9300
            IF (J5COL(M).EQ.6) CPTORC = 'Y'                              OUTNOD........9400
            IF (J5COL(M).EQ.7) CPSATU = 'Y'                              OUTNOD........9500
    8    CONTINUE                                                        OUTNOD........9600
         IF (ISSTRA.NE.1) THEN                                           OUTNOD........9700
            KT = KT + 1                                                  OUTNOD........9800
            TT(KT) = TS                                                  OUTNOD........9900
            ITT(KT) = JT                                                 OUTNOD.......10000
            ISHORP(KT) = 0                                               OUTNOD.......10100
            ISTORC(KT) = 0                                               OUTNOD.......10200
            ISSATU(KT) = 0                                               OUTNOD.......10300
         END IF                                                          OUTNOD.......10400
   10    CONTINUE                                                        OUTNOD.......10500
            JT=JT+1                                                      OUTNOD.......10600
            IF (MOD(JT,ITCYC).EQ.0 .AND. JT.GT.1) DELTK=DELTK*DTMULT     OUTNOD.......10700
            IF (DELTK.GT.DTMAX) DELTK=DTMAX                              OUTNOD.......10800
            TS=TS+DELTK                                                  OUTNOD.......10900
            IF (MOD(JT,NPCYC).EQ.0 .OR. JT.EQ.1) LCHORP = JT             OUTNOD.......11000
            IF (MOD(JT,NUCYC).EQ.0 .OR. JT.EQ.1) LCTORC = JT             OUTNOD.......11100
            IF (MOD(JT,NCOLPR).EQ.0 .OR.                                 OUTNOD.......11200
     1         ((JT.EQ.1).AND.((ISSTRA.NE.0).OR.(NCOLPR.GT.0)))) THEN    OUTNOD.......11300
               KT = KT + 1                                               OUTNOD.......11400
               TT(KT) = TS                                               OUTNOD.......11500
               ITT(KT) = JT                                              OUTNOD.......11600
               ISHORP(KT) = LCHORP                                       OUTNOD.......11700
               ISTORC(KT) = LCTORC                                       OUTNOD.......11800
               ISSATU(KT) = LCHORP                                       OUTNOD.......11900
            ENDIF                                                        OUTNOD.......12000
         IF(JT.LT.ITMAX .AND. TS.LT.TMAX) GOTO 10                        OUTNOD.......12100
         JTMAX = JT                                                      OUTNOD.......12200
         IF (ISSTRA.EQ.1) TT(KT) = TSTART                                OUTNOD.......12300
C                                                                        OUTNOD.......12400
C                                                                        OUTNOD.......12500
C........PRINT LAST TIME STEP ALWAYS, UNLESS ALREADY PRINTED             OUTNOD.......12600
         IF(JTMAX.GT.1 .AND. MOD(JT,NCOLPR).NE.0) THEN                   OUTNOD.......12700
            KT = KT + 1                                                  OUTNOD.......12800
            TT(KT) = TS                                                  OUTNOD.......12900
            ITT(KT) = JT                                                 OUTNOD.......13000
            IF (MOD(JT,NPCYC).EQ.0) LCHORP = JT                          OUTNOD.......13100
            IF (MOD(JT,NUCYC).EQ.0) LCTORC = JT                          OUTNOD.......13200
            ISHORP(KT) = LCHORP                                          OUTNOD.......13300
            ISTORC(KT) = LCTORC                                          OUTNOD.......13400
            ISSATU(KT) = LCHORP                                          OUTNOD.......13500
         ENDIF                                                           OUTNOD.......13600
C                                                                        OUTNOD.......13700
C........IF STEADY-STATE FLOW, P AND S CALCULATED ON TIME STEP 0 ONLY.   OUTNOD.......13800
         IF (ISSFLO.NE.0) THEN                                           OUTNOD.......13900
            DO 14 KT=1,KTMAX                                             OUTNOD.......14000
               ISHORP(KT) = 0                                            OUTNOD.......14100
               ISSATU(KT) = 0                                            OUTNOD.......14200
   14       CONTINUE                                                     OUTNOD.......14300
         END IF                                                          OUTNOD.......14400
C                                                                        OUTNOD.......14500
C........SET TEMPERATURE OR CONCENTRATION TEXT STRING FOR HEADER         OUTNOD.......14600
         IF (ME .GT. 0) THEN                                             OUTNOD.......14700
            TORC = "Temperature  "                                       OUTNOD.......14800
            COLTK5(6) = "    Temperature"                                OUTNOD.......14900
         ELSE                                                            OUTNOD.......15000
            TORC = "Concentration"                                       OUTNOD.......15100
         ENDIF                                                           OUTNOD.......15200
C                                                                        OUTNOD.......15300
C........SET PRESSURE TEXT STRING FOR HEADER                             OUTNOD.......15400
         HORP = "Pressure"                                               OUTNOD.......15500
C                                                                        OUTNOD.......15600
C........WRITE HEADER INFORMATION                                        OUTNOD.......15700
         WRITE(K5,960) TITLE1, TITLE2                                    OUTNOD.......15800
         IF (KTYPE.LT.0) THEN                                            OUTNOD.......15900
            IF (IABS(KTYPE).EQ.3) THEN                                   OUTNOD.......16000
               WRITE(K5,961) IABS(KTYPE), NN1, NN2, NN3, NN, " Nodes"    OUTNOD.......16100
            ELSE                                                         OUTNOD.......16200
               WRITE(K5,962) IABS(KTYPE), NN1, NN2, NN, " Nodes"         OUTNOD.......16300
            END IF                                                       OUTNOD.......16400
         ELSE                                                            OUTNOD.......16500
            WRITE(K5,963) IABS(KTYPE), NN, " Nodes"                      OUTNOD.......16600
         END IF                                                          OUTNOD.......16700
         WRITE(K5,964) "NODEWISE RESULTS",                               OUTNOD.......16800
     1      KTMAX, HORP, TORC, "Sat"                                     OUTNOD.......16900
         DO 20  KT=1, KTMAX                                              OUTNOD.......17000
            WRITE(K5,965) ITT(KT), TT(KT), CPHORP, ISHORP(KT),           OUTNOD.......17100
     1         CPTORC, ISTORC(KT), CPSATU, ISSATU(KT)                    OUTNOD.......17200
   20    CONTINUE                                                        OUTNOD.......17300
  960    FORMAT("## ", 80A1,                                             OUTNOD.......17400
     1         /"## ", 80A1,                                             OUTNOD.......17500
     2         /"## ")                                                   OUTNOD.......17600
  961    FORMAT("## ", I1, "-D, REGULAR MESH  ", 5X,                     OUTNOD.......17700
     1                 "(", 2(I9, ")*("), I9, ") = ", I9, A,             OUTNOD.......17800
     2         /"## ")                                                   OUTNOD.......17900
  962    FORMAT("## ", I1, "-D, REGULAR MESH  ", 17X,                    OUTNOD.......18000
     1                 "(", I9, ")*(", I9, ") = ", I9, A,                OUTNOD.......18100
     2         /"## ")                                                   OUTNOD.......18200
  963    FORMAT("## ", I1, "-D, IRREGULAR MESH", 43X, I9, A,             OUTNOD.......18300
     1         /"## ")                                                   OUTNOD.......18400
  964    FORMAT("## ", 77("="),                                          OUTNOD.......18500
     4         /"## ", A, 33X, I9, " Time steps printed",                OUTNOD.......18600
     5         /"## ", 77("="),                                          OUTNOD.......18700
     6         /"## ",                                                   OUTNOD.......18800
     7         /"##  Time steps", 21X,                                   OUTNOD.......18900
     8                 "[Printed? / Latest time step computed]",         OUTNOD.......19000
     9         /"## in this file    Time (sec)", 9X,A5, 10X,A4, 11X,A3,  OUTNOD.......19100
     T         /"## ", 12("-"), 3X, 12("-"), 1X, 3(3X, 12("-")) )        OUTNOD.......19200
  965    FORMAT ("## ", 3X, I8, 3X, 1PE13.6, 3(5X, A1, 1X, I8))          OUTNOD.......19300
                                                                         OUTNOD.......19400
C                                                                        OUTNOD.......19500
C........DEALLOCATE LOCAL ARRAYS.                                        OUTNOD.......19600
         DEALLOCATE(TT,ITT,ISTORC,ISHORP,ISSATU)                         OUTNOD.......19700
C                                                                        OUTNOD.......19800
         ONCEK5 = .TRUE.                                                 OUTNOD.......19900
      ENDIF                                                              OUTNOD.......20000
C                                                                        OUTNOD.......20100
C.....NODEWISE HEADER INFORMATION REPEATED BEFORE EACH TIME STEP         OUTNOD.......20200
      IF ((IT.EQ.0).OR.((IT.EQ.1).AND.(ISSTRA.EQ.1))) THEN               OUTNOD.......20300
         DURN = 0D0                                                      OUTNOD.......20400
         TOUT = TSTART                                                   OUTNOD.......20500
      ELSE                                                               OUTNOD.......20600
         DURN = DELT                                                     OUTNOD.......20700
         TOUT = TSEC                                                     OUTNOD.......20800
      END IF                                                             OUTNOD.......20900
      WRITE(K5,966) IT, DURN, TOUT                                       OUTNOD.......21000
  966 FORMAT('## ',                                                      OUTNOD.......21100
     1      /'## ', 77('='),                                             OUTNOD.......21200
     2      /'## TIME STEP ', I6, 9X, 'Duration: ', 1PE11.4, ' sec',     OUTNOD.......21300
     3                            6X, 'Time: ', 1PE11.4, ' sec',         OUTNOD.......21400
     4      /'## ', 77('='))                                             OUTNOD.......21500
      PRINTN = (J5COL(1).EQ.1)                                           OUTNOD.......21600
      IF (PRINTN) THEN                                                   OUTNOD.......21700
         WRITE(K5,968) (COLTK5(J5COL(M)), M=1,NCOLS5)                    OUTNOD.......21800
  968    FORMAT ("## ", 2X, A4, 19(A15))                                 OUTNOD.......21900
      ELSE                                                               OUTNOD.......22000
         WRITE(K5,969) COLTK5(J5COL(1))(3:15),                           OUTNOD.......22100
     1      (COLTK5(J5COL(M)), M=2,NCOLS5)                               OUTNOD.......22200
  969    FORMAT ("## ", A13, 19(A15))                                    OUTNOD.......22300
      END IF                                                             OUTNOD.......22400
C                                                                        OUTNOD.......22500
C.....NODEWISE DATA FOR THIS TIME STEP                                   OUTNOD.......22600
      DO 978 I=1,NN                                                      OUTNOD.......22700
         VVAR(1) = DBLE(I)                                               OUTNOD.......22800
         VVAR(2) = X(I)                                                  OUTNOD.......22900
         VVAR(3) = Y(I)                                                  OUTNOD.......23000
         VVAR(4) = Z(I)                                                  OUTNOD.......23100
         VVAR(5) = PVEC(I)                                               OUTNOD.......23200
         VVAR(6) = UVEC(I)                                               OUTNOD.......23300
         VVAR(7) = SW(I)                                                 OUTNOD.......23400
         DO 972 M=1,NCOLS5                                               OUTNOD.......23500
            VCOL(M) = VVAR(J5COL(M))                                     OUTNOD.......23600
  972    CONTINUE                                                        OUTNOD.......23700
         IF (PRINTN) THEN                                                OUTNOD.......23800
            WRITE(K5,975) I,(VCOL(M), M=2,NCOLS5)                        OUTNOD.......23900
  975       FORMAT (I9, 19(1PE15.7))                                     OUTNOD.......24000
         ELSE                                                            OUTNOD.......24100
            WRITE(K5,976) (VCOL(M), M=1,NCOLS5)                          OUTNOD.......24200
  976       FORMAT (1X, 20(1PE15.7))                                     OUTNOD.......24300
         END IF                                                          OUTNOD.......24400
  978 CONTINUE                                                           OUTNOD.......24500
C                                                                        OUTNOD.......24600
      RETURN                                                             OUTNOD.......24700
C                                                                        OUTNOD.......24800
      END                                                                OUTNOD.......24900
C                                                                        OUTNOD.......25000
C     SUBROUTINE        O  U  T  O  B  S           SUTRA VERSION 2D3D.1  OUTOBS.........100
C                                                                        OUTOBS.........200
C *** PURPOSE :                                                          OUTOBS.........300
C ***  TO PRINT THE SOLUTION AT OBSERVATION NODES.  SPECIFICALLY,        OUTOBS.........400
C ***  TO PRINT PRESSURES, CONCENTRATIONS OR TEMPERATURES, AND           OUTOBS.........500
C ***  SATURATIONS IN A COLUMNWISE FORMAT.  OUTPUT IS TO THE OBS FILE.   OUTOBS.........600
C                                                                        OUTOBS.........700
      SUBROUTINE OUTOBS(IOBS,X,Y,Z,PVEC,UVEC,SW,TITLE1,TITLE2)           OUTOBS.........800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTOBS.........900
      CHARACTER*1  TITLE1(80),TITLE2(80)                                 OUTOBS........1000
      CHARACTER*8  HORP                                                  OUTOBS........1100
      CHARACTER*13 TORC1,TORC2                                           OUTOBS........1200
      LOGICAL ONCEK5,ONCEK6,ONCEK7                                       OUTOBS........1300
      DIMENSION X(NN),Y(NN),Z(NN)                                        OUTOBS........1400
      DIMENSION PVEC(NNVEC),UVEC(NNVEC),SW(NN)                           OUTOBS........1500
      DIMENSION IOBS(NOBSN)                                              OUTOBS........1600
      ALLOCATABLE TT(:),ITT(:),ISTORC(:),ISHORP(:),ISSATU(:)             OUTOBS........1700
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTOBS........1800
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             OUTOBS........1900
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTOBS........2000
     1   NSOP,NSOU,NBCN                                                  OUTOBS........2100
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   OUTOBS........2200
      COMMON /DIMX2/ NELTA,NNVEC,NDIMJA                                  OUTOBS........2300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        OUTOBS........2400
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  OUTOBS........2500
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      OUTOBS........2600
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     OUTOBS........2700
     1   KSCRN,KPAUSE                                                    OUTOBS........2800
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC                                    OUTOBS........2900
      COMMON /PLT1/ ONCEK5,ONCEK6,ONCEK7                                 OUTOBS........3000
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTOBS........3100
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTOBS........3200
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  OUTOBS........3300
C                                                                        OUTOBS........3400
C.....CALCULATE HEADERS ON FIRST CALL AND CREATE OUTPUT ON EACH CALL.    OUTOBS........3500
C                                                                        OUTOBS........3600
      NOBS=NOBSN-1                                                       OUTOBS........3700
      DKTM2 = DBLE(IABS(KTYPE) - 2)                                      OUTOBS........3800
C                                                                        OUTOBS........3900
      IF (.NOT. ONCEK7)  THEN                                            OUTOBS........4000
C.....FIRST CALL -- CREATE FILE HEADER.                                  OUTOBS........4100
C                                                                        OUTOBS........4200
C........IF NO OBSERVATION NODES, WRITE MESSAGE AND RETURN               OUTOBS........4300
         IF (NOBSN-1.EQ.0) THEN                                          OUTOBS........4400
            WRITE(K7,960) TITLE1, TITLE2                                 OUTOBS........4500
            WRITE(K7,2)                                                  OUTOBS........4600
    2       FORMAT(/'  *** NO OBSERVATION NODES SPECIFIED (NOBS=0) ***') OUTOBS........4700
            ONCEK7 = .TRUE.                                              OUTOBS........4800
            RETURN                                                       OUTOBS........4900
         END IF                                                          OUTOBS........5000
C                                                                        OUTOBS........5100
C........CALCULATE THE MAXIMUM NUMBER OF TIME STEPS, KTMAX.              OUTOBS........5200
         TS=TSTART                                                       OUTOBS........5300
         JT=0                                                            OUTOBS........5400
         IF (ISSTRA.NE.1) THEN                                           OUTOBS........5500
            KT = 1                                                       OUTOBS........5600
         ELSE                                                            OUTOBS........5700
            KT = 0                                                       OUTOBS........5800
         END IF                                                          OUTOBS........5900
         DELTK=DELT                                                      OUTOBS........6000
    4    CONTINUE                                                        OUTOBS........6100
            JT=JT+1                                                      OUTOBS........6200
            IF (MOD(JT,ITCYC).EQ.0 .AND. JT.GT.1) DELTK=DELTK*DTMULT     OUTOBS........6300
            IF (DELTK.GT.DTMAX) DELTK=DTMAX                              OUTOBS........6400
            TS=TS+DELTK                                                  OUTOBS........6500
            IF (MOD(JT,NOBCYC).EQ.0 .OR.                                 OUTOBS........6600
     1         ((JT.EQ.1).AND.((ISSTRA.NE.0).OR.(NOBCYC.GT.0))))         OUTOBS........6700
     2         KT = KT + 1                                               OUTOBS........6800
         IF(JT.LT.ITMAX .AND. TS.LT.TMAX) GOTO 4                         OUTOBS........6900
         JTMAX = JT                                                      OUTOBS........7000
         IF(JTMAX.GT.1 .AND. MOD(JT,NOBCYC).NE.0) KT = KT + 1            OUTOBS........7100
         KTMAX = KT                                                      OUTOBS........7200
C                                                                        OUTOBS........7300
C........ALLOCATE LOCAL ARRAYS                                           OUTOBS........7400
         ALLOCATE(TT(KTMAX),ITT(KTMAX))                                  OUTOBS........7500
         ALLOCATE(ISTORC(KTMAX),ISHORP(KTMAX),ISSATU(KTMAX))             OUTOBS........7600
C                                                                        OUTOBS........7700
C........CALCULATE AND PRINT TIME STEP INFORMATION                       OUTOBS........7800
         TS=TSTART                                                       OUTOBS........7900
C........TIME STEP VALUE                                                 OUTOBS........8000
         JT=0                                                            OUTOBS........8100
C........NUMBER OF PRINTED TIME STEPS                                    OUTOBS........8200
         KT=0                                                            OUTOBS........8300
C........TIME STEP INCREMENT                                             OUTOBS........8400
         DELTK=DELT                                                      OUTOBS........8500
C........INDICATORS OF WHEN VARIABLES ARE CALCULATED AND PRINTED         OUTOBS........8600
         LCHORP = 0                                                      OUTOBS........8700
         LCTORC = 0                                                      OUTOBS........8800
         IF (ISSTRA.NE.1) THEN                                           OUTOBS........8900
            KT = KT + 1                                                  OUTOBS........9000
            TT(KT) = TS                                                  OUTOBS........9100
            ITT(KT) = JT                                                 OUTOBS........9200
            ISHORP(KT) = 0                                               OUTOBS........9300
            ISTORC(KT) = 0                                               OUTOBS........9400
            ISSATU(KT) = 0                                               OUTOBS........9500
         END IF                                                          OUTOBS........9600
   10    CONTINUE                                                        OUTOBS........9700
            JT=JT+1                                                      OUTOBS........9800
            IF (MOD(JT,ITCYC).EQ.0 .AND. JT.GT.1) DELTK=DELTK*DTMULT     OUTOBS........9900
            IF (DELTK.GT.DTMAX) DELTK=DTMAX                              OUTOBS.......10000
            TS=TS+DELTK                                                  OUTOBS.......10100
            IF (MOD(JT,NPCYC).EQ.0 .OR. JT.EQ.1) LCHORP = JT             OUTOBS.......10200
            IF (MOD(JT,NUCYC).EQ.0 .OR. JT.EQ.1) LCTORC = JT             OUTOBS.......10300
            IF (MOD(JT,NOBCYC).EQ.0 .OR.                                 OUTOBS.......10400
     1         ((JT.EQ.1).AND.((ISSTRA.NE.0).OR.(NOBCYC.GT.0)))) THEN    OUTOBS.......10500
               KT = KT + 1                                               OUTOBS.......10600
               TT(KT) = TS                                               OUTOBS.......10700
               ITT(KT) = JT                                              OUTOBS.......10800
               ISHORP(KT) = LCHORP                                       OUTOBS.......10900
               ISTORC(KT) = LCTORC                                       OUTOBS.......11000
               ISSATU(KT) = LCHORP                                       OUTOBS.......11100
            ENDIF                                                        OUTOBS.......11200
         IF(JT.LT.ITMAX .AND. TS.LT.TMAX) GOTO 10                        OUTOBS.......11300
         JTMAX = JT                                                      OUTOBS.......11400
         IF (ISSTRA.EQ.1) TT(KT) = TSTART                                OUTOBS.......11500
C                                                                        OUTOBS.......11600
C                                                                        OUTOBS.......11700
C........PRINT LAST TIME STEP ALWAYS, UNLESS ALREADY PRINTED             OUTOBS.......11800
         IF(JTMAX.GT.1 .AND. MOD(JT,NOBCYC).NE.0) THEN                   OUTOBS.......11900
            KT = KT + 1                                                  OUTOBS.......12000
            TT(KT) = TS                                                  OUTOBS.......12100
            ITT(KT) = JT                                                 OUTOBS.......12200
            IF (MOD(JT,NPCYC).EQ.0) LCHORP = JT                          OUTOBS.......12300
            IF (MOD(JT,NUCYC).EQ.0) LCTORC = JT                          OUTOBS.......12400
            ISHORP(KT) = LCHORP                                          OUTOBS.......12500
            ISTORC(KT) = LCTORC                                          OUTOBS.......12600
            ISSATU(KT) = LCHORP                                          OUTOBS.......12700
         ENDIF                                                           OUTOBS.......12800
C........NUMBER OF PRINTED TIME STEPS                                    OUTOBS.......12900
         KTMAX = KT                                                      OUTOBS.......13000
C                                                                        OUTOBS.......13100
C........IF STEADY-STATE FLOW, P AND S CALCULATED ON TIME STEP 0 ONLY.   OUTOBS.......13200
         IF (ISSFLO.NE.0) THEN                                           OUTOBS.......13300
            DO 14 KT=1,KTMAX                                             OUTOBS.......13400
               ISHORP(KT) = 0                                            OUTOBS.......13500
               ISSATU(KT) = 0                                            OUTOBS.......13600
   14       CONTINUE                                                     OUTOBS.......13700
         END IF                                                          OUTOBS.......13800
C                                                                        OUTOBS.......13900
C........SET TEMPERATURE OR CONCENTRATION TEXT STRING FOR HEADER         OUTOBS.......14000
         IF (ME .GT. 0) THEN                                             OUTOBS.......14100
            TORC1 = "Temperature  "                                      OUTOBS.......14200
            TORC2 = "  Temperature"                                      OUTOBS.......14300
         ELSE                                                            OUTOBS.......14400
            TORC1 = "Concentration"                                      OUTOBS.......14500
            TORC2 = "Concentration"                                      OUTOBS.......14600
         ENDIF                                                           OUTOBS.......14700
C                                                                        OUTOBS.......14800
C........SET PRESSURE TEXT STRING FOR HEADER                             OUTOBS.......14900
         HORP = "Pressure"                                               OUTOBS.......15000
C                                                                        OUTOBS.......15100
C........WRITE HEADER INFORMATION                                        OUTOBS.......15200
         WRITE(K7,960) TITLE1, TITLE2                                    OUTOBS.......15300
         IF (KTYPE.LT.0) THEN                                            OUTOBS.......15400
            IF (IABS(KTYPE).EQ.3) THEN                                   OUTOBS.......15500
               WRITE(K7,961) IABS(KTYPE), NN1, NN2, NN3, NN, " Nodes"    OUTOBS.......15600
            ELSE                                                         OUTOBS.......15700
               WRITE(K7,962) IABS(KTYPE), NN1, NN2, NN, " Nodes"         OUTOBS.......15800
            END IF                                                       OUTOBS.......15900
         ELSE                                                            OUTOBS.......16000
            WRITE(K7,963) IABS(KTYPE), NN, " Nodes"                      OUTOBS.......16100
         END IF                                                          OUTOBS.......16200
         WRITE(K7,964) "OBSERVATION NODE RESULTS",                       OUTOBS.......16300
     1      KTMAX, HORP, TORC1, "Sat"                                    OUTOBS.......16400
         DO 20  KT=1, KTMAX                                              OUTOBS.......16500
            WRITE(K7,965) ITT(KT), TT(KT), ISHORP(KT),                   OUTOBS.......16600
     1         ISTORC(KT), ISSATU(KT)                                    OUTOBS.......16700
   20    CONTINUE                                                        OUTOBS.......16800
         NOBS = NOBSN - 1                                                OUTOBS.......16900
         WRITE(K7,966) (IOBS(JJ),JJ=1,NOBS)                              OUTOBS.......17000
         WRITE(K7,968) ('---------------',JJ=1,3*NOBS)                   OUTOBS.......17100
         WRITE(K7,967) (X(IOBS(JJ)), Y(IOBS(JJ)), Z(IOBS(JJ)),           OUTOBS.......17200
     1      JJ=1,NOBS)                                                   OUTOBS.......17300
         WRITE(K7,968) ('---------------',JJ=1,3*NOBS)                   OUTOBS.......17400
         WRITE(K7,969) (HORP, TORC2, JJ=1,NOBS)                          OUTOBS.......17500
  960    FORMAT("## ", 80A1,                                             OUTOBS.......17600
     1         /"## ", 80A1,                                             OUTOBS.......17700
     2         /"## ")                                                   OUTOBS.......17800
  961    FORMAT("## ", I1, "-D, REGULAR MESH  ", 5X,                     OUTOBS.......17900
     1                 "(", 2(I9, ")*("), I9, ") = ", I9, A,             OUTOBS.......18000
     2         /"## ")                                                   OUTOBS.......18100
  962    FORMAT("## ", I1, "-D, REGULAR MESH  ", 17X,                    OUTOBS.......18200
     1                 "(", I9, ")*(", I9, ") = ", I9, A,                OUTOBS.......18300
     2         /"## ")                                                   OUTOBS.......18400
  963    FORMAT("## ", I1, "-D, IRREGULAR MESH", 43X, I9, A,             OUTOBS.......18500
     1         /"## ")                                                   OUTOBS.......18600
  964    FORMAT("## ", 77("="),                                          OUTOBS.......18700
     4         /"## ", A, 25X, I9, " Time steps printed",                OUTOBS.......18800
     5         /"## ", 77("="),                                          OUTOBS.......18900
     6         /"## ",                                                   OUTOBS.......19000
     7         /"##  Time steps", 27X,                                   OUTOBS.......19100
     8                 "[Latest time step computed]",                    OUTOBS.......19200
     9         /"## in this file    Time (sec)", 9X,A5, 10X,A4, 11X,A3,  OUTOBS.......19300
     T         /"## ", 12("-"), 3X, 12("-"), 1X, 3(3X, 12("-")) )        OUTOBS.......19400
  965    FORMAT("## ", 3X, I8, 3X, 1PE13.6, 3(7X, I8))                   OUTOBS.......19500
  966    FORMAT("## ",                                                   OUTOBS.......19600
     1         /"## ", 77("="),                                          OUTOBS.......19700
     2         /"## ",                                                   OUTOBS.......19800
     3         /"## ", 24X, 999(:3X, 16X, 'NODE ', I9, 15X))             OUTOBS.......19900
  967    FORMAT("## ", 25X, 999(:2X, '(', 2(1PE14.7,','), 1PE14.7, ')')) OUTOBS.......20000
  968    FORMAT("## ", 24X, 999(:3X, 1X, A14, 2A15))                     OUTOBS.......20100
  969    FORMAT("## ", 'Time Step', 5X, 'Time (sec)', 999(:10X, A, 2X,   OUTOBS.......20200
     1         A, 5X, 'Saturation'))                                     OUTOBS.......20300
                                                                         OUTOBS.......20400
C                                                                        OUTOBS.......20500
C........DEALLOCATE LOCAL ARRAYS.                                        OUTOBS.......20600
         DEALLOCATE(TT,ITT,ISTORC,ISHORP,ISSATU)                         OUTOBS.......20700
C                                                                        OUTOBS.......20800
         ONCEK7 = .TRUE.                                                 OUTOBS.......20900
      ENDIF                                                              OUTOBS.......21000
C                                                                        OUTOBS.......21100
      IF (NOBSN-1.EQ.0) RETURN                                           OUTOBS.......21200
C                                                                        OUTOBS.......21300
      IF ((IT.EQ.0).OR.((IT.EQ.1).AND.(ISSTRA.EQ.1))) THEN               OUTOBS.......21400
         TOUT = TSTART                                                   OUTOBS.......21500
      ELSE                                                               OUTOBS.......21600
         TOUT = TSEC                                                     OUTOBS.......21700
      END IF                                                             OUTOBS.......21800
      WRITE(K7,980) IT, TOUT,                                            OUTOBS.......21900
     1   (PVEC(IOBS(JJ)), UVEC(IOBS(JJ)), SW(IOBS(JJ)), JJ=1,NOBS)       OUTOBS.......22000
  980 FORMAT(3X, I9, 1PE15.7, 999(:3X, 3(1PE15.7)))                      OUTOBS.......22100
C                                                                        OUTOBS.......22200
C                                                                        OUTOBS.......22300
      RETURN                                                             OUTOBS.......22400
C                                                                        OUTOBS.......22500
      END                                                                OUTOBS.......22600
C                                                                        OUTOBS.......22700
C     SUBROUTINE        O  U  T  R  S  T           SUTRA VERSION 2D3D.1  OUTRST.........100
C                                                                        OUTRST.........200
C *** PURPOSE :                                                          OUTRST.........300
C ***  TO STORE RESULTS THAT MAY LATER BE USED TO RESTART                OUTRST.........400
C ***  THE SIMULATION.                                                   OUTRST.........500
C                                                                        OUTRST.........600
      SUBROUTINE OUTRST(PVEC,UVEC,PM1,UM1,CS1,RCIT,SW,QINITR,PBC)        OUTRST.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTRST.........800
      DIMENSION PVEC(NNVEC),UVEC(NNVEC),PM1(NN),UM1(NN),CS1(NN),         OUTRST.........900
     1   RCIT(NN),SW(NN),PBC(NBCN),QINITR(NN)                            OUTRST........1000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTRST........1100
     1   NSOP,NSOU,NBCN                                                  OUTRST........1200
      COMMON /DIMX2/ NELTA, NNVEC, NDIMJA                                OUTRST........1300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        OUTRST........1400
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTRST........1500
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  OUTRST........1600
C                                                                        OUTRST........1700
C.....REWIND RST FILE FOR WRITING RESULTS OF CURRENT TIME STEP           OUTRST........1800
      REWIND(K4)                                                         OUTRST........1900
C                                                                        OUTRST........2000
C.....STORE TIME INFORMATION                                             OUTRST........2100
      WRITE(K4,100) TSEC,DELTP,DELTU                                     OUTRST........2200
  100 FORMAT(4D20.10)                                                    OUTRST........2300
C                                                                        OUTRST........2400
C.....STORE SOLUTION                                                     OUTRST........2500
      WRITE(K4,105)                                                      OUTRST........2600
      WRITE(K4,110) (PVEC(I),I=1,NN)                                     OUTRST........2700
      WRITE(K4,105)                                                      OUTRST........2800
      WRITE(K4,110) (UVEC(I),I=1,NN)                                     OUTRST........2900
      WRITE(K4,110) (PM1(I),I=1,NN)                                      OUTRST........3000
      WRITE(K4,110) (UM1(I),I=1,NN)                                      OUTRST........3100
      WRITE(K4,110) (CS1(I),I=1,NN)                                      OUTRST........3200
      WRITE(K4,110) (RCIT(I),I=1,NN)                                     OUTRST........3300
      WRITE(K4,110) (SW(I),I=1,NN)                                       OUTRST........3400
      write(k4,110) (QINITR(I),I=1,NN)                                   OUTRST........3500
      WRITE(K4,110) (PBC(IP),IP=1,NBCN)                                  OUTRST........3600
  105 FORMAT("'NONUNIFORM'")                                             OUTRST........3700
  110 FORMAT(1PD20.13,1X,1PD20.13,1X,1PD20.13,1X,1PD20.13)               OUTRST........3800
C                                                                        OUTRST........3900
      ENDFILE(K4)                                                        OUTRST........4000
C                                                                        OUTRST........4100
      RETURN                                                             OUTRST........4200
      END                                                                OUTRST........4300
C                                                                        OUTRST........4400
C     SUBROUTINE        P  R  S  W  D  S           SUTRA VERSION 2D3D.1  PRSWDS.........100
C                                                                        PRSWDS.........200
C *** PURPOSE :                                                          PRSWDS.........300
C ***  PARSE A CHARACTER STRING INTO WORDS.  WORDS ARE CONSIDERED TO BE  PRSWDS.........400
C ***  SEPARATED BY ONE OR MORE OF THE SINGLE-CHARACTER DELIMITER DELIM  PRSWDS.........500
C ***  AND/OR BLANKS.  PARSING CONTINUES UNTIL THE ENTIRE STRING HAS     PRSWDS.........600
C ***  BEEN PROCESSED OR THE NUMBER OF WORDS PARSED EQUALS NWMAX.        PRSWDS.........700
C                                                                        PRSWDS.........800
      SUBROUTINE PRSWDS(STRING, DELIM, NWMAX, WORD, NWORDS)              PRSWDS.........900
      CHARACTER*80 STRING,WORD(NWMAX)                                    PRSWDS........1000
      CHARACTER*1 DELIM                                                  PRSWDS........1100
C                                                                        PRSWDS........1200
C.....INITIALIZE WORD LIST AND COUNTERS                                  PRSWDS........1300
      DO 50 I=1,NWMAX                                                    PRSWDS........1400
         WORD(I) = ""                                                    PRSWDS........1500
   50 CONTINUE                                                           PRSWDS........1600
      NWORDS = 0                                                         PRSWDS........1700
      M2 = 1                                                             PRSWDS........1800
C                                                                        PRSWDS........1900
  300 CONTINUE                                                           PRSWDS........2000
C.....FIND THE NEXT CHARACTER THAT IS NOT A DELIMITER                    PRSWDS........2100
      DO 350 M=M2,80                                                     PRSWDS........2200
         IF ((STRING(M:M).NE.DELIM).AND.(STRING(M:M).NE.' ')) THEN       PRSWDS........2300
            M1 = M                                                       PRSWDS........2400
            GOTO 400                                                     PRSWDS........2500
         END IF                                                          PRSWDS........2600
  350 CONTINUE                                                           PRSWDS........2700
      RETURN                                                             PRSWDS........2800
C                                                                        PRSWDS........2900
  400 CONTINUE                                                           PRSWDS........3000
C.....FIND THE NEXT CHARACTER THAT IS A DELIMITER                        PRSWDS........3100
      DO 450 M=M1+1,80                                                   PRSWDS........3200
         IF ((STRING(M:M).EQ.DELIM).OR.(STRING(M:M).EQ.' ')) THEN        PRSWDS........3300
            M2 = M                                                       PRSWDS........3400
            GOTO 500                                                     PRSWDS........3500
         END IF                                                          PRSWDS........3600
  450 CONTINUE                                                           PRSWDS........3700
      M2 = 80                                                            PRSWDS........3800
C                                                                        PRSWDS........3900
  500 CONTINUE                                                           PRSWDS........4000
C.....STORE THE LATEST WORD FOUND                                        PRSWDS........4100
      NWORDS = NWORDS + 1                                                PRSWDS........4200
      WORD(NWORDS) = STRING(M1:M2-1)                                     PRSWDS........4300
C                                                                        PRSWDS........4400
C.....IF END OF STRING NOT REACHED AND NUMBER OF WORDS IS LESS THAN      PRSWDS........4500
C        NWMAX, CONTINUE PARSING                                         PRSWDS........4600
      IF ((M2.LT.80).AND.(NWORDS.LT.NWMAX)) GOTO 300                     PRSWDS........4700
C                                                                        PRSWDS........4800
      RETURN                                                             PRSWDS........4900
      END                                                                PRSWDS........5000
C                                                                        PRSWDS........5100
C     SUBROUTINE        P  T  R  S  E  T           SUTRA VERSION 2D3D.1  PTRSET.........100
C                                                                        PTRSET.........200
C *** PURPOSE :                                                          PTRSET.........300
C ***  TO SET UP POINTER ARRAYS NEEDED TO SPECIFY THE MATRIX STRUCTURE.  PTRSET.........400
C                                                                        PTRSET.........500
      SUBROUTINE PTRSET(NBI27, IA, JA, MIOFF)                            PTRSET.........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                PTRSET.........700
      DIMENSION NBI27(NBIX),MIOFF(27),IA(NELT),JA(NDIMJA)                PTRSET.........800
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  PTRSET.........900
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             PTRSET........1000
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              PTRSET........1100
     1   NSOP,NSOU,NBCN                                                  PTRSET........1200
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   PTRSET........1300
      COMMON /DIMX2/ NELTA, NNVEC, NDIMJA                                PTRSET........1400
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           PTRSET........1500
C                                                                        PTRSET........1600
C.....CREATE THE POINTER ARRAY NBI27, WHICH GIVES THE CORRESPONDENCE     PTRSET........1700
C        BETWEEN COLUMN INDICES IN SUTRA BANDED FORMAT AND LOCAL NODE    PTRSET........1800
C        NUMBERS IN THE 9-NODE (2D) OR 27-NODE (3D) MOLECULE.            PTRSET........1900
C                                                                        PTRSET........2000
      IF (IABS(KTYPE).EQ.3) THEN                                         PTRSET........2100
C.....3D PROBLEM.                                                        PTRSET........2200
C                                                                        PTRSET........2300
      DO 200 ILOC=-1,1                                                   PTRSET........2400
      DO 200 JLOC=-1,1                                                   PTRSET........2500
      DO 200 KLOC=-1,1                                                   PTRSET........2600
C                                                                        PTRSET........2700
C........COMPUTE COLUMN INDEX OFFSET IN SUTRA BANDED FORMAT.             PTRSET........2800
C           THIS IS EQUIVALENT TO THE FOLLOWING:                         PTRSET........2900
C           I1 = I + ILOC ;  J1 = J + JLOC ;  K1 = K + KLOC ;            PTRSET........3000
C           N = NN1*(NN2*(K - 1) + J - 1) + I ;                          PTRSET........3100
C           N1 = NN1*(NN2*(K1 - 1) + J1 - 1) + I1 ;                      PTRSET........3200
C           INDOFF = N - N1 .                                            PTRSET........3300
         INDOFF = NN1*(NN2*KLOC + JLOC) + ILOC                           PTRSET........3400
C                                                                        PTRSET........3500
C........COMPUTE LOCAL NODE NUMBER IN 27-NODE MOLECULE.                  PTRSET........3600
C           THIS IS EQUIVALENT TO THE FOLLOWING:                         PTRSET........3700
C           I = ILOC + 2 ;  J = JLOC + 2 ;  K = KLOC + 2 ;               PTRSET........3800
C           INDLOC = NI*(NJ*(K - 1) + J - 1) + I  (NI=NJ=3).             PTRSET........3900
         INDLOC = 9*KLOC + 3*JLOC + ILOC + 14                            PTRSET........4000
C                                                                        PTRSET........4100
C........COMPUTE AND STORE CORRESPONDENCE BETWEEN COLUMN INDEX IN SUTRA  PTRSET........4200
C           BANDED FORMAT AND LOCAL NODE NUMBER IN 27-NODE MOLECULE.     PTRSET........4300
         JB = INDOFF + NBHALF                                            PTRSET........4400
         NBI27(JB) = INDLOC                                              PTRSET........4500
C                                                                        PTRSET........4600
  200 CONTINUE                                                           PTRSET........4700
C                                                                        PTRSET........4800
C.....DEFINE CERTAIN QUANTITIES FOR CONVENIENCE AND EFFICIENCY.          PTRSET........4900
      NN12 = NN1*NN2                                                     PTRSET........5000
      NNNBH = NN - NBHALF                                                PTRSET........5100
      NBH1 = NBHALF + 1                                                  PTRSET........5200
      NB1 = NB + 1                                                       PTRSET........5300
C                                                                        PTRSET........5400
C.....CREATE THE POINTER ARRAY MIOFF, WHICH IS USED AS FOLLOWS TO        PTRSET........5500
C        COMPUTE THE POSITION, M, OF A MATRIX COEFFICIENT IN THE ARRAY:  PTRSET........5600
C        M = MIOFF(J27) + I, WHERE I IS THE ROW INDEX AND J27 IS         PTRSET........5700
C        THE LOCAL NODE NUMBER IN THE 27-NODE MOLECULE.  THIS IS USED    PTRSET........5800
C        IN THE GLOBAL MATRIX ASSEMBLY ROUTINE GLOTRI.                   PTRSET........5900
      MBEG = 1                                                           PTRSET........6000
      DO 400 KS=0,2                                                      PTRSET........6100
         NBMK = KS*NN12                                                  PTRSET........6200
         NSTK = KS*9                                                     PTRSET........6300
      DO 400 JS=0,2                                                      PTRSET........6400
         NBMJ = NBMK + JS*NN1                                            PTRSET........6500
         NSTJ = NSTK + JS*3                                              PTRSET........6600
      DO 400 IS=1,3                                                      PTRSET........6700
         NBM = NBMJ + IS                                                 PTRSET........6800
         NBMC = NB1 - NBM                                                PTRSET........6900
         NST = NSTJ + IS                                                 PTRSET........7000
         IF (NST.LT.14) THEN                                             PTRSET........7100
            IBEG = NBH1 - NBM                                            PTRSET........7200
            IEND = NN                                                    PTRSET........7300
         ELSE                                                            PTRSET........7400
            IBEG = 1                                                     PTRSET........7500
            IEND = NNNBH + NBMC                                          PTRSET........7600
         END IF                                                          PTRSET........7700
         MIOFF(NST) = MBEG - IBEG                                        PTRSET........7800
         ILEN = IEND - IBEG + 1                                          PTRSET........7900
         MBEG = MBEG + ILEN                                              PTRSET........8000
  400 CONTINUE                                                           PTRSET........8100
C                                                                        PTRSET........8200
      ELSE                                                               PTRSET........8300
C.....2D PROBLEM.                                                        PTRSET........8400
C                                                                        PTRSET........8500
      DO 1200 ILOC=-1,1                                                  PTRSET........8600
      DO 1200 JLOC=-1,1                                                  PTRSET........8700
C                                                                        PTRSET........8800
C........COMPUTE COLUMN INDEX OFFSET IN SUTRA BANDED FORMAT.             PTRSET........8900
C           THIS IS EQUIVALENT TO THE FOLLOWING:                         PTRSET........9000
C           I1 = I + ILOC ;  J1 = J + JLOC ;                             PTRSET........9100
C           N = NN1*(J - 1) + I ;                                        PTRSET........9200
C           N1 = NN1*(J1 - 1) + I1 ;                                     PTRSET........9300
C           INDOFF = N - N1 .                                            PTRSET........9400
         INDOFF = NN1*JLOC + ILOC                                        PTRSET........9500
C                                                                        PTRSET........9600
C........COMPUTE LOCAL NODE NUMBER IN 9-NODE MOLECULE.                   PTRSET........9700
C           THIS IS EQUIVALENT TO THE FOLLOWING:                         PTRSET........9800
C           I = ILOC + 2 ;  J = JLOC + 2 ;                               PTRSET........9900
C           INDLOC = NI*(J - 1) + I  (NI=3).                             PTRSET.......10000
         INDLOC = 3*JLOC + ILOC + 5                                      PTRSET.......10100
C                                                                        PTRSET.......10200
C........COMPUTE AND STORE CORRESPONDENCE BETWEEN COLUMN INDEX IN SUTRA  PTRSET.......10300
C           BANDED FORMAT AND LOCAL NODE NUMBER IN 9-NODE MOLECULE.      PTRSET.......10400
         JB = INDOFF + NBHALF                                            PTRSET.......10500
         NBI27(JB) = INDLOC                                              PTRSET.......10600
C                                                                        PTRSET.......10700
 1200 CONTINUE                                                           PTRSET.......10800
C                                                                        PTRSET.......10900
C.....DEFINE CERTAIN QUANTITIES FOR CONVENIENCE AND EFFICIENCY.          PTRSET.......11000
      NNNBH = NN - NBHALF                                                PTRSET.......11100
      NBH1 = NBHALF + 1                                                  PTRSET.......11200
      NB1 = NB + 1                                                       PTRSET.......11300
C                                                                        PTRSET.......11400
C.....CREATE THE POINTER ARRAY MIOFF, WHICH IS USED AS FOLLOWS TO        PTRSET.......11500
C        COMPUTE THE POSITION, M, OF A MATRIX COEFFICIENT IN THE ARRAY:  PTRSET.......11600
C        M = MIOFF(J9) + I, WHERE I IS THE ROW INDEX AND J9 IS           PTRSET.......11700
C        THE LOCAL NODE NUMBER IN THE 9-NODE MOLECULE.  THIS IS USED     PTRSET.......11800
C        IN THE GLOBAL MATRIX ASSEMBLY ROUTINE GLOTRI.                   PTRSET.......11900
      MBEG = 1                                                           PTRSET.......12000
      DO 1400 JS=0,2                                                     PTRSET.......12100
         NBMJ = JS*NN1                                                   PTRSET.......12200
         NSTJ = JS*3                                                     PTRSET.......12300
      DO 1400 IS=1,3                                                     PTRSET.......12400
         NBM = NBMJ + IS                                                 PTRSET.......12500
         NBMC = NB1 - NBM                                                PTRSET.......12600
         NST = NSTJ + IS                                                 PTRSET.......12700
         IF (NST.LT.5) THEN                                              PTRSET.......12800
            IBEG = NBH1 - NBM                                            PTRSET.......12900
            IEND = NN                                                    PTRSET.......13000
         ELSE                                                            PTRSET.......13100
            IBEG = 1                                                     PTRSET.......13200
            IEND = NNNBH + NBMC                                          PTRSET.......13300
         END IF                                                          PTRSET.......13400
         MIOFF(NST) = MBEG - IBEG                                        PTRSET.......13500
         ILEN = IEND - IBEG + 1                                          PTRSET.......13600
         MBEG = MBEG + ILEN                                              PTRSET.......13700
 1400 CONTINUE                                                           PTRSET.......13800
C                                                                        PTRSET.......13900
      END IF                                                             PTRSET.......14000
C                                                                        PTRSET.......14100
C.....CREATE THE POINTER ARRAYS IA AND JA.                               PTRSET.......14200
      CALL TRISET(IA, JA)                                                PTRSET.......14300
C                                                                        PTRSET.......14400
      RETURN                                                             PTRSET.......14500
      END                                                                PTRSET.......14600
C                                                                        PTRSET.......14700
C     SUBROUTINE        R  E  A  D  I  F           SUTRA VERSION 2D3D.1  READIF.........100
C                                                                        READIF.........200
C *** PURPOSE :                                                          READIF.........300
C ***  TO READ A LINE FROM AN INPUT FILE INTO THE CHARACTER VARIABLE     READIF.........400
C ***  INTFIL.                                                           READIF.........500
C                                                                        READIF.........600
      SUBROUTINE READIF(KU, INTFIL, ERRCOD)                              READIF.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                READIF.........800
      CHARACTER INTFIL*1000                                              READIF.........900
      CHARACTER*80 ERRCOD,CHERR(10)                                      READIF........1000
      DIMENSION INERR(10),RLERR(10)                                      READIF........1100
C                                                                        READIF........1200
C.....READ A LINE OF INPUT (UP TO 80 CHARACTERS) FROM UNIT KU            READIF........1300
C        INTO INTFIL                                                     READIF........1400
      READ(KU,'(A)',IOSTAT=INERR(1)) INTFIL                              READIF........1500
      IF (INERR(1).NE.0) THEN                                            READIF........1600
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        READIF........1700
         RETURN                                                          READIF........1800
      END IF                                                             READIF........1900
C                                                                        READIF........2000
      RETURN                                                             READIF........2100
      END                                                                READIF........2200
C                                                                        READIF........2300
C     SUBROUTINE        R  O  T  A  T  E           SUTRA VERSION 2D3D.1  ROTATE.........100
C                                                                        ROTATE.........200
C *** PURPOSE :                                                          ROTATE.........300
C ***  TO TRANSFORM THE COORDINATES OF A VECTOR, {x}, BY APPLYING THE    ROTATE.........400
C ***  ROTATION MATRIX, [G]:  {xp}=[G]{x}.                               ROTATE.........500
C                                                                        ROTATE.........600
      SUBROUTINE ROTATE(G11,G12,G13,G21,G22,G23,G31,G32,G33,X,Y,Z,       ROTATE.........700
     1   XP,YP,ZP)                                                       ROTATE.........800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                ROTATE.........900
C                                                                        ROTATE........1000
C.....COMPUTE VECTOR {xp} AS THE PRODUCT OF MATRIX [G] AND VECTOR {x}    ROTATE........1100
      XP= G11*X + G12*Y + G13*Z                                          ROTATE........1200
      YP= G21*X + G22*Y + G23*Z                                          ROTATE........1300
      ZP= G31*X + G32*Y + G33*Z                                          ROTATE........1400
C                                                                        ROTATE........1500
      RETURN                                                             ROTATE........1600
      END                                                                ROTATE........1700
C                                                                        ROTATE........1800
C     SUBROUTINE        R  O  T  M  A  T           SUTRA VERSION 2D3D.1  ROTMAT.........100
C                                                                        ROTMAT.........200
C *** PURPOSE :                                                          ROTMAT.........300
C ***  TO COMPUTE A TRANSFORMATION MATRIX, [G], THAT CONVERTS            ROTMAT.........400
C ***  COORDINATES OF A VECTOR, {v}, FROM A COORDINATE SYSTEM (X, Y, Z)  ROTMAT.........500
C ***  TO A NEW COORDINATE SYSTEM (X', Y', Z'):  {v'} = [G]{v}.          ROTMAT.........600
C ***  THE OVERALL TRANSFORMATION IS THE RESULT OF THREE ROTATIONS       ROTMAT.........700
C ***  APPLIED CONSECUTIVELY:                                            ROTMAT.........800
C ***  A1 = ROTATION IN THE XY-PLANE, COUNTER-CLOCKWISE FROM THE         ROTMAT.........900
C ***     +X-AXIS (LOOKING DOWN THE +Z-AXIS TOWARD THE ORIGIN),          ROTMAT........1000
C ***  A2 = ROTATION IN THE NEW XZ-PLANE, COUNTER-CLOCKWISE FROM THE     ROTMAT........1100
C ***     NEW +X-AXIS (LOOKING DOWN THE NEW +Y-AXIS TOWARD THE ORIGIN),  ROTMAT........1200
C ***  A3 = ROTATION IN THE NEW YZ-PLANE, COUNTER-CLOCKWISE FROM THE     ROTMAT........1300
C ***     NEW +Y-AXIS (LOOKING DOWN THE NEW +X-AXIS TOWARD THE ORIGIN).  ROTMAT........1400
C                                                                        ROTMAT........1500
      SUBROUTINE ROTMAT(A1,A2,A3,G11,G12,G13,G21,G22,G23,G31,G32,G33)    ROTMAT........1600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                ROTMAT........1700
C                                                                        ROTMAT........1800
C.....COMPUTE SINES AND COSINES OF ANGLES.                               ROTMAT........1900
      S1= DSIN(A1)                                                       ROTMAT........2000
      C1= DCOS(A1)                                                       ROTMAT........2100
      S2= DSIN(A2)                                                       ROTMAT........2200
      C2= DCOS(A2)                                                       ROTMAT........2300
      S3= DSIN(A3)                                                       ROTMAT........2400
      C3= DCOS(A3)                                                       ROTMAT........2500
C                                                                        ROTMAT........2600
C.....COMPUTE ROTATION MATRIX.                                           ROTMAT........2700
      G11 =  C1*C2                                                       ROTMAT........2800
      G12 =  -C1*S2*S3 - S1*C3                                           ROTMAT........2900
      G13 =  -C1*S2*C3 + S1*S3                                           ROTMAT........3000
      G21 =  S1*C2                                                       ROTMAT........3100
      G22 =  -S1*S2*S3 + C1*C3                                           ROTMAT........3200
      G23 =  -S1*S2*C3 - C1*S3                                           ROTMAT........3300
      G31 =  S2                                                          ROTMAT........3400
      G32 =  C2*S3                                                       ROTMAT........3500
      G33 =  C2*C3                                                       ROTMAT........3600
      RETURN                                                             ROTMAT........3700
      END                                                                ROTMAT........3800
C                                                                        ROTMAT........3900
C     SUBROUTINE        S  K  P  C  O  M           SUTRA VERSION 2D3D.1  SKPCOM.........100
C                                                                        SKPCOM.........200
C *** PURPOSE :                                                          SKPCOM.........300
C ***  TO IDENTIFY AND SKIP OVER COMMENT LINES IN AN INPUT FILE          SKPCOM.........400
C ***  AND RETURN THE NUMBER OF LINES SKIPPED.                           SKPCOM.........500
C                                                                        SKPCOM.........600
      SUBROUTINE SKPCOM(KU, NLSKIP, ERRCOD)                              SKPCOM.........700
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               SKPCOM.........800
      CHARACTER*1 CDUM                                                   SKPCOM.........900
      CHARACTER*80 ERRCOD,CHERR(10),FNAME(0:7)                           SKPCOM........1000
      DIMENSION INERR(10),RLERR(10)                                      SKPCOM........1100
      COMMON /FNAMES/ FNAME                                              SKPCOM........1200
C                                                                        SKPCOM........1300
C.....SKIP LINES UNTIL A NON-COMMENT LINE IS ENCOUNTERED                 SKPCOM........1400
      NLSKIP = 0                                                         SKPCOM........1500
  100 READ(KU,111,IOSTAT=INERR(1)) CDUM                                  SKPCOM........1600
  111 FORMAT (A1)                                                        SKPCOM........1700
      IF (INERR(1).NE.0) THEN                                            SKPCOM........1800
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SKPCOM........1900
         RETURN                                                          SKPCOM........2000
      END IF                                                             SKPCOM........2100
      IF (CDUM.EQ.'#') THEN                                              SKPCOM........2200
         NLSKIP = NLSKIP + 1                                             SKPCOM........2300
         GOTO 100                                                        SKPCOM........2400
      END IF                                                             SKPCOM........2500
C                                                                        SKPCOM........2600
C.....BACKSPACE THE INPUT FILE TO THE LAST LINE READ (WHICH IS A         SKPCOM........2700
C        NON-COMMENT LINE)                                               SKPCOM........2800
      BACKSPACE(KU)                                                      SKPCOM........2900
C                                                                        SKPCOM........3000
  900 RETURN                                                             SKPCOM........3100
      END                                                                SKPCOM........3200
C                                                                        SKPCOM........3300
C     SUBROUTINE        S  O  L  V  E  B           SUTRA VERSION 2D3D.1  SOLVEB.........100
C                                                                        SOLVEB.........200
C *** PURPOSE :                                                          SOLVEB.........300
C ***  TO SOLVE THE MATRIX EQUATION BY:                                  SOLVEB.........400
C ***   (1) DECOMPOSING THE MATRIX                                       SOLVEB.........500
C ***   (2) MODIFYING THE RIGHT-HAND SIDE                                SOLVEB.........600
C ***   (3) BACK-SUBSTITUTING FOR THE SOLUTION                           SOLVEB.........700
C                                                                        SOLVEB.........800
      SUBROUTINE SOLVEB(KMT,C,R,NNP,IHALFB,MAXNP,MAXBW)                  SOLVEB.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SOLVEB........1000
      DIMENSION C(MAXNP,MAXBW),R(MAXNP)                                  SOLVEB........1100
C                                                                        SOLVEB........1200
      IHBP=IHALFB+1                                                      SOLVEB........1300
C                                                                        SOLVEB........1400
C.....DECOMPOSE MATRIX C BY BANDED GAUSSIAN ELIMINATION FOR              SOLVEB........1500
C        NON-SYMMETRIC MATRIX                                            SOLVEB........1600
      IF(KMT-1) 5,5,50                                                   SOLVEB........1700
    5 NU=NNP-IHALFB                                                      SOLVEB........1800
      DO 20 NI=1,NU                                                      SOLVEB........1900
      PIVOTI=1.D0/C(NI,IHBP)                                             SOLVEB........2000
      NJ=NI+1                                                            SOLVEB........2100
      IB=IHBP                                                            SOLVEB........2200
      NK=NI+IHALFB                                                       SOLVEB........2300
      DO 10 NL=NJ,NK                                                     SOLVEB........2400
      IB=IB-1                                                            SOLVEB........2500
      A=-C(NL,IB)*PIVOTI                                                 SOLVEB........2600
      C(NL,IB)=A                                                         SOLVEB........2700
      JB=IB+1                                                            SOLVEB........2800
      KB=IB+IHALFB                                                       SOLVEB........2900
      LB=IHBP-IB                                                         SOLVEB........3000
      DO10 MB=JB,KB                                                      SOLVEB........3100
      NB=LB+MB                                                           SOLVEB........3200
   10 C(NL,MB)=C(NL,MB)+A*C(NI,NB)                                       SOLVEB........3300
   20 CONTINUE                                                           SOLVEB........3400
      NR=NU+1                                                            SOLVEB........3500
      NU=NNP-1                                                           SOLVEB........3600
      NK=NNP                                                             SOLVEB........3700
      DO 40 NI=NR,NU                                                     SOLVEB........3800
      PIVOTI=1.D0/(C(NI,IHBP))                                           SOLVEB........3900
      NJ=NI+1                                                            SOLVEB........4000
      IB=IHBP                                                            SOLVEB........4100
      DO 30 NL=NJ,NK                                                     SOLVEB........4200
      IB=IB-1                                                            SOLVEB........4300
      A=-C(NL,IB)*PIVOTI                                                 SOLVEB........4400
      C(NL,IB)=A                                                         SOLVEB........4500
      JB=IB+1                                                            SOLVEB........4600
      KB=IB+IHALFB                                                       SOLVEB........4700
      LB=IHBP-IB                                                         SOLVEB........4800
      DO 30 MB=JB,KB                                                     SOLVEB........4900
      NB=LB+MB                                                           SOLVEB........5000
   30 C(NL,MB)=C(NL,MB)+A*C(NI,NB)                                       SOLVEB........5100
   40 CONTINUE                                                           SOLVEB........5200
      IF(KMT-1) 50,44,50                                                 SOLVEB........5300
   44 RETURN                                                             SOLVEB........5400
C                                                                        SOLVEB........5500
C.....UPDATE RIGHT-HAND SIDE VECTOR, R                                   SOLVEB........5600
   50 NU=NNP+1                                                           SOLVEB........5700
      IBAND=2*IHALFB+1                                                   SOLVEB........5800
      DO 70 NI=2,IHBP                                                    SOLVEB........5900
      IB=IHBP-NI+1                                                       SOLVEB........6000
      NJ=1                                                               SOLVEB........6100
      SUM=0.0D0                                                          SOLVEB........6200
      DO 60 JB=IB,IHALFB                                                 SOLVEB........6300
      SUM=SUM+C(NI,JB)*R(NJ)                                             SOLVEB........6400
   60 NJ=NJ+1                                                            SOLVEB........6500
   70 R(NI)=R(NI)+SUM                                                    SOLVEB........6600
      IB=1                                                               SOLVEB........6700
      NL=IHBP+1                                                          SOLVEB........6800
      DO 90 NI=NL,NNP                                                    SOLVEB........6900
      NJ=NI-IHBP+1                                                       SOLVEB........7000
      SUM=0.D0                                                           SOLVEB........7100
      DO 80 JB=IB,IHALFB                                                 SOLVEB........7200
      SUM=SUM+C(NI,JB)*R(NJ)                                             SOLVEB........7300
   80 NJ=NJ+1                                                            SOLVEB........7400
   90 R(NI)=R(NI)+SUM                                                    SOLVEB........7500
C                                                                        SOLVEB........7600
C.....BACK SOLVE                                                         SOLVEB........7700
      R(NNP)=R(NNP)/C(NNP,IHBP)                                          SOLVEB........7800
      DO 110 IB=2,IHBP                                                   SOLVEB........7900
      NI=NU-IB                                                           SOLVEB........8000
      NJ=NI                                                              SOLVEB........8100
      MB=IHALFB+IB                                                       SOLVEB........8200
      SUM=0.D0                                                           SOLVEB........8300
      DO 100 JB=NL,MB                                                    SOLVEB........8400
      NJ=NJ+1                                                            SOLVEB........8500
  100 SUM=SUM+C(NI,JB)*R(NJ)                                             SOLVEB........8600
  110 R(NI)=(R(NI)-SUM)/C(NI,IHBP)                                       SOLVEB........8700
      MB=IBAND                                                           SOLVEB........8800
      DO 130 IB=NL,NNP                                                   SOLVEB........8900
      NI=NU-IB                                                           SOLVEB........9000
      NJ=NI                                                              SOLVEB........9100
      SUM=0.D0                                                           SOLVEB........9200
      DO 120 JB=NL,MB                                                    SOLVEB........9300
      NJ=NJ+1                                                            SOLVEB........9400
  120 SUM=SUM+C(NI,JB)*R(NJ)                                             SOLVEB........9500
  130 R(NI)=(R(NI)-SUM)/C(NI,IHBP)                                       SOLVEB........9600
C                                                                        SOLVEB........9700
C                                                                        SOLVEB........9800
      RETURN                                                             SOLVEB........9900
      END                                                                SOLVEB.......10000
C                                                                        SOLVEB.......10100
C     SUBROUTINE        S  O  L  V  E  R           SUTRA VERSION 2D3D.1  SOLVER.........100
C                                                                        SOLVER.........200
C *** PURPOSE :                                                          SOLVER.........300
C ***  TO CALL THE APPROPRIATE MATRIX EQUATION SOLVER.                   SOLVER.........400
C                                                                        SOLVER.........500
      SUBROUTINE SOLVER(KMT,KPU,KSOLVR,C,R,XITER,B,NNP,IHALFB,MAXNP,     SOLVER.........600
     1                  MAXBW,IWK,FWK,IA,JA,IERR,ITRS,ERR)               SOLVER.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SOLVER.........800
      CHARACTER SOLNAM(0:10)*40,SOLWRD(0:10)*10,KPUTXT(2)*1              SOLVER.........900
      DIMENSION C(MAXNP,MAXBW),R(NNVEC),XITER(NNP),B(NNNX)               SOLVER........1000
      DIMENSION IWK(NWI),FWK(NWF)                                        SOLVER........1100
      DIMENSION IA(NELT),JA(NDIMJA)                                      SOLVER........1200
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   SOLVER........1300
      COMMON /DIMX2/ NELTA, NNVEC, NDIMJA                                SOLVER........1400
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        SOLVER........1500
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     SOLVER........1600
     1   KSCRN,KPAUSE                                                    SOLVER........1700
      COMMON /SOLVC/ SOLWRD, SOLNAM                                      SOLVER........1800
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           SOLVER........1900
      DATA (KPUTXT(K), K=1,2) /'P', 'U'/                                 SOLVER........2000
      SAVE KPUTXT                                                        SOLVER........2100
C                                                                        SOLVER........2200
C.....IF THE RIGHT-HAND-SIDE VECTOR OF THE MATRIX EQUATION IS ZERO,      SOLVER........2300
C        RETURN A ZERO SOLUTION VECTOR.                                  SOLVER........2400
      LENR = NNVEC                                                       SOLVER........2500
      RHSNRM = DNRM2(LENR, R, 1)                                         SOLVER........2600
      IF (RHSNRM.EQ.0D0) THEN                                            SOLVER........2700
         IERR = 0                                                        SOLVER........2800
         DO 44 I=1,NNVEC                                                 SOLVER........2900
   44       R(I) = 0D0                                                   SOLVER........3000
         IF (KSCRN.EQ.1) WRITE(*,55) KPUTXT(KPU),KPUTXT(KPU),KPUTXT(KPU) SOLVER........3100
         WRITE (K00,55) KPUTXT(KPU), KPUTXT(KPU), KPUTXT(KPU)            SOLVER........3200
   55    FORMAT (1X, 6X, A1, '-solution (', A1, '=0)'                    SOLVER........3300
     1      ' inferred from matrix equation A*', A1, '=0;'               SOLVER........3400
     1      ' solver not called.')                                       SOLVER........3500
         RETURN                                                          SOLVER........3600
      END IF                                                             SOLVER........3700
C                                                                        SOLVER........3800
C.....SIGNAL THE START OF A SOLUTION                                     SOLVER........3900
      LENSLW = LEN_TRIM(SOLWRD(KSOLVR))                                  SOLVER........4000
  101 IF (KSCRN.EQ.1) WRITE(*,133) KPUTXT(KPU),SOLWRD(KSOLVR)(1:LENSLW)  SOLVER........4100
      WRITE (K00,133) KPUTXT(KPU), SOLWRD(KSOLVR)(1:LENSLW)              SOLVER........4200
  133 FORMAT (1X, 6X, "Starting ", A1, "-solution using ", A,            SOLVER........4300
     1   " solver ...")                                                  SOLVER........4400
C                                                                        SOLVER........4500
C.....IF KSOLVR=0, CALL BANDED GAUSSIAN (DIRECT) SOLVER.                 SOLVER........4600
C        OTHERWISE, CALL ITERATIVE SOLVER.                               SOLVER........4700
      IF (KSOLVR.EQ.0) THEN                                              SOLVER........4800
         CALL SOLVEB(KMT, C, R, NNP, IHALFB, MAXNP, MAXBW)               SOLVER........4900
         IERR = 0                                                        SOLVER........5000
         ITRS = 0                                                        SOLVER........5100
         ERR = 0D0                                                       SOLVER........5200
      ELSE                                                               SOLVER........5300
         CALL SOLWRP(KMT, KPU, KSOLVR, C, R, XITER, B, NNP, IHALFB,      SOLVER........5400
     1         MAXNP, MAXBW, IWK, FWK, IA, JA, IERR, ITRS, ERR)          SOLVER........5500
      END IF                                                             SOLVER........5600
C                                                                        SOLVER........5700
      RETURN                                                             SOLVER........5800
      END                                                                SOLVER........5900
C                                                                        SOLVER........6000
C     SUBROUTINE        S  O  L  W  R  P           SUTRA VERSION 2D3D.1  SOLWRP.........100
C                                                                        SOLWRP.........200
C *** PURPOSE :                                                          SOLWRP.........300
C ***  TO SERVE AS A WRAPPER FOR THE ITERATIVE SOLVERS, PERFORMING       SOLWRP.........400
C ***  SOME PRELIMINARIES ON VECTORS AND MATRIX POINTERS BEFORE          SOLWRP.........500
C ***  CALLING A SOLVER.                                                 SOLWRP.........600
C                                                                        SOLWRP.........700
      SUBROUTINE SOLWRP(KMT, KPU, KSOLVR, A, R, XITER, B, NNP, IHALFB,   SOLWRP.........800
     1                  MAXNP, MAXBW, IWK, FWK, IA, JA, IERR, ITRS, ERR) SOLWRP.........900
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               SOLWRP........1000
      CHARACTER*80 ERRCOD,CHERR(10),FNAME(0:7)                           SOLWRP........1100
      CHARACTER*1 KPUTXT(2)                                              SOLWRP........1200
      CHARACTER*40 SOLNAM(0:10)                                          SOLWRP........1300
      CHARACTER*10 SOLWRD(0:10)                                          SOLWRP........1400
      DIMENSION A(NELT)                                                  SOLWRP........1500
      DIMENSION IA(NELT),JA(NDIMJA)                                      SOLWRP........1600
      DIMENSION IWK(NWI),FWK(NWF)                                        SOLWRP........1700
      DIMENSION XITER(NNP),R(NNP),B(NNNX)                                SOLWRP........1800
      DIMENSION INERR(10),RLERR(10)                                      SOLWRP........1900
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   SOLWRP........2000
      COMMON /DIMX2/ NELTA,NNVEC,NDIMJA                                  SOLWRP........2100
      COMMON /FNAMES/ FNAME                                              SOLWRP........2200
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        SOLWRP........2300
      COMMON /ITSOLI/ ITRMXP,ITOLP,NSAVEP,ITRMXU,ITOLU,NSAVEU            SOLWRP........2400
      COMMON /ITSOLR/ TOLP,TOLU                                          SOLWRP........2500
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     SOLWRP........2600
     1   KSCRN,KPAUSE                                                    SOLWRP........2700
      COMMON /SOLVC/ SOLWRD, SOLNAM                                      SOLWRP........2800
      COMMON /SOLVI/ KSOLVP, KSOLVU, NN1, NN2, NN3                       SOLWRP........2900
      COMMON /SOLVN/ NSLVRS                                              SOLWRP........3000
      DATA (KPUTXT(K), K=1,2) /'P', 'U'/                                 SOLWRP........3100
      SAVE KPUTXT                                                        SOLWRP........3200
      EXTERNAL DSLUGM,DSLUOM                                             SOLWRP........3300
C                                                                        SOLWRP........3400
C.....RESET MATRIX ARRAY POINTERS IF NECESSARY.                          SOLWRP........3500
      IF (KMT.EQ.0) CALL TRISET(IA, JA)                                  SOLWRP........3600
C                                                                        SOLWRP........3700
C.....COPY THE RHS VECTOR R INTO VECTOR B, THEN USE R AS THE             SOLWRP........3800
C        SOLUTION VECTOR.  INITIALIZE IT FROM THE LATEST SUTRA           SOLWRP........3900
C        SOLUTION.  XITER IS NOT USED AS THE SOLUTION VECTOR BECAUSE     SOLWRP........4000
C        DOING SO MIGHT INTERFERE WITH SUBSEQUENT CALCULATIONS.          SOLWRP........4100
      DO 150 N=1,NNP                                                     SOLWRP........4200
         B(N) = R(N)                                                     SOLWRP........4300
         R(N) = XITER(N)                                                 SOLWRP........4400
  150 CONTINUE                                                           SOLWRP........4500
C                                                                        SOLWRP........4600
C.....SET ITERATIVE SOLVER PARAMETERS.                                   SOLWRP........4700
C        IUNIT --> UNIT ON WHICH TO WRITE SOLVER ERROR (0 = NONE)        SOLWRP........4800
C        ISYM  --> 0 = FULL STORAGE; 1 = SYMMETRIC STORAGE               SOLWRP........4900
C        ITRMX --> MAXIMUM NUMBER OF SOLVER ITERATIONS                   SOLWRP........5000
C        ITOL  --> TYPE OF CONVERGENCE CRITERION                         SOLWRP........5100
C        TOL   --> CONVERGENCE TOLERANCE                                 SOLWRP........5200
C        NSAVE --> NUMBER OF DIRECTION VECTORS                           SOLWRP........5300
      IF (KPU.EQ.1) THEN                                                 SOLWRP........5400
C........SET PARAMETERS FOR ITERATIVE P SOLUTION.                        SOLWRP........5500
         ISYM = 0                                                        SOLWRP........5600
         ITRMX = MAX(ITRMXP, 1)                                          SOLWRP........5700
         ITOL = ITOLP                                                    SOLWRP........5800
         TOL = TOLP                                                      SOLWRP........5900
         NSAVE = NSAVEP                                                  SOLWRP........6000
      ELSE                                                               SOLWRP........6100
C........SET PARAMETERS FOR ITERATIVE U SOLUTION.                        SOLWRP........6200
         ISYM = 0                                                        SOLWRP........6300
         ITRMX = MAX(ITRMXU, 1)                                          SOLWRP........6400
         ITOL = ITOLU                                                    SOLWRP........6500
         TOL = TOLU                                                      SOLWRP........6600
         NSAVE = NSAVEU                                                  SOLWRP........6700
      END IF                                                             SOLWRP........6800
      IUNIT = K00                                                        SOLWRP........6900
C                                                                        SOLWRP........7000
C.....CALL AN ITERATIVE SOLVER:                                          SOLWRP........7100
C        DSICCG = CG WITH IC PRECONDITIONING,                            SOLWRP........7200
C        DSLUGM = GMRES WITH ILU PRECONDITIONING,                        SOLWRP........7300
C        DSLUOM = ORTHOMIN WITH ILU PRECONDITIONING.                     SOLWRP........7400
      IF (KSOLVR.EQ.1) THEN                                              SOLWRP........7500
         CALL DSICCG(NNP, B, R, NELT, IA, JA, A, ISYM, ITOL, TOL,        SOLWRP........7600
     1            ITRMX, ITRS, ERR, IERR, IUNIT, FWK, NWF, IWK, NWI)     SOLWRP........7700
      ELSE IF (KSOLVR.EQ.2) THEN                                         SOLWRP........7800
         CALL DSLUGM(NNP, B, R, NELT, IA, JA, A, ISYM, NSAVE, ITOL, TOL, SOLWRP........7900
     1            ITRMX, ITRS, ERR, IERR, IUNIT, FWK, NWF, IWK, NWI)     SOLWRP........8000
      ELSE                                                               SOLWRP........8100
         CALL DSLUOM(NNP, B, R, NELT, IA, JA, A, ISYM, NSAVE, ITOL, TOL, SOLWRP........8200
     1            ITRMX, ITRS, ERR, IERR, IUNIT, FWK, NWF, IWK, NWI)     SOLWRP........8300
      END IF                                                             SOLWRP........8400
C                                                                        SOLWRP........8500
C.....WRITE CONVERGENCE INFORMATION.                                     SOLWRP........8600
      IF (IERR.EQ.0) THEN                                                SOLWRP........8700
         IF (KSCRN.EQ.1) WRITE (*,555) KPUTXT(KPU), ITRS, ERR            SOLWRP........8800
         WRITE (K00,555) KPUTXT(KPU), ITRS, ERR                          SOLWRP........8900
  555    FORMAT (1X, 6X, A1, '-solution converged in ', I5,              SOLWRP........9000
     1      ' solver iterations  (Error ~ ', 1PE8.1, ')')                SOLWRP........9100
      ELSE                                                               SOLWRP........9200
         IF (KSCRN.EQ.1) WRITE (*,557) KPUTXT(KPU), ITRS, ERR            SOLWRP........9300
         WRITE (K00,557) KPUTXT(KPU), ITRS, ERR                          SOLWRP........9400
  557    FORMAT (1X, 6X, A1, '-solution FAILED after ', I5,              SOLWRP........9500
     1      ' solver iterations  (Error ~ ', 1PE8.1, ')')                SOLWRP........9600
      END IF                                                             SOLWRP........9700
C                                                                        SOLWRP........9800
      RETURN                                                             SOLWRP........9900
      END                                                                SOLWRP.......10000
C                                                                        SOLWRP.......10100
C     SUBROUTINE        S  O  U  R  C  E           SUTRA VERSION 2D3D.1  SOURCE.........100
C                                                                        SOURCE.........200
C *** PURPOSE :                                                          SOURCE.........300
C ***  TO READ AND ORGANIZE FLUID MASS SOURCE DATA AND ENERGY OR         SOURCE.........400
C ***  SOLUTE MASS SOURCE DATA.                                          SOURCE.........500
C                                                                        SOURCE.........600
      SUBROUTINE SOURCE(QIN,UIN,IQSOP,QUIN,IQSOU,IQSOPT,IQSOUT)          SOURCE.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SOURCE.........800
      CHARACTER INTFIL*1000                                              SOURCE.........900
      CHARACTER*80 ERRCOD,CHERR(10),FNAME(0:7)                           SOURCE........1000
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  SOURCE........1100
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             SOURCE........1200
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SOURCE........1300
     1   NSOP,NSOU,NBCN                                                  SOURCE........1400
      COMMON /ERRHAN/ ISERR                                              SOURCE........1500
      COMMON /FNAMES/ FNAME                                              SOURCE........1600
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        SOURCE........1700
      DIMENSION QIN(NN),UIN(NN),IQSOP(NSOP),QUIN(NN),IQSOU(NSOU)         SOURCE........1800
      DIMENSION INERR(10),RLERR(10)                                      SOURCE........1900
C                                                                        SOURCE........2000
C.....NSOPI IS ACTUAL NUMBER OF FLUID SOURCE NODES.                      SOURCE........2100
C.....NSOUI IS ACTUAL NUMBER OF SOLUTE MASS OR ENERGY SOURCE NODES.      SOURCE........2200
      NSOPI=NSOP-1                                                       SOURCE........2300
      NSOUI=NSOU-1                                                       SOURCE........2400
      IQSOPT=1                                                           SOURCE........2500
      IQSOUT=1                                                           SOURCE........2600
      NIQP=0                                                             SOURCE........2700
      NIQU=0                                                             SOURCE........2800
      IF(NSOPI.EQ.0) GOTO 1000                                           SOURCE........2900
      IF(ME) 50,50,150                                                   SOURCE........3000
   50 WRITE(K3,100)                                                      SOURCE........3100
  100 FORMAT(1H1////11X,'F L U I D   S O U R C E   D A T A'              SOURCE........3200
     1   ////11X,'**** NODES AT WHICH FLUID INFLOWS OR OUTFLOWS ARE ',   SOURCE........3300
     2   'SPECIFIED ****'//11X,'NODE NUMBER',10X,                        SOURCE........3400
     3   'FLUID INFLOW(+)/OUTFLOW(-)',5X,'SOLUTE CONCENTRATION OF'       SOURCE........3500
     4   /11X,'(MINUS INDICATES',5X,'(FLUID MASS/SECOND)',               SOURCE........3600
     5   12X,'INFLOWING FLUID'/12X,'TIME-VARYING',39X,                   SOURCE........3700
     6   '(MASS SOLUTE/MASS WATER)'/12X,'FLOW RATE OR'/12X,              SOURCE........3800
     7   'CONCENTRATION)'//)                                             SOURCE........3900
      GOTO 300                                                           SOURCE........4000
  150 WRITE(K3,200)                                                      SOURCE........4100
  200 FORMAT(1H1////11X,'F L U I D   S O U R C E   D A T A'              SOURCE........4200
     1   ////11X,'**** NODES AT WHICH FLUID INFLOWS OR OUTFLOWS ARE ',   SOURCE........4300
     2   'SPECIFIED ****'//11X,'NODE NUMBER',10X,                        SOURCE........4400
     3   'FLUID INFLOW(+)/OUTFLOW(-)',5X,'TEMPERATURE {DEGREES CELSIUS}' SOURCE........4500
     4   /11X,'(MINUS INDICATES',5X,'(FLUID MASS/SECOND)',12X,           SOURCE........4600
     5   'OF INFLOWING FLUID'/12X,'TIME-VARYING'/12X,'FLOW OR'/12X,      SOURCE........4700
     6   'TEMPERATURE)'//)                                               SOURCE........4800
C                                                                        SOURCE........4900
C.....INPUT DATASET 17:  DATA FOR FLUID SOURCES AND SINKS                SOURCE........5000
  300 CONTINUE                                                           SOURCE........5100
      ERRCOD = 'REA-INP-S17'                                             SOURCE........5200
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    SOURCE........5300
      IF (ISERR) RETURN                                                  SOURCE........5400
  305 NIQP=NIQP+1                                                        SOURCE........5500
      ERRCOD = 'REA-INP-17'                                              SOURCE........5600
      CALL READIF(K1, INTFIL, ERRCOD)                                    SOURCE........5700
      IF (ISERR) RETURN                                                  SOURCE........5800
      READ(INTFIL,*,IOSTAT=INERR(1)) IQCP                                SOURCE........5900
      IF (INERR(1).NE.0) THEN                                            SOURCE........6000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SOURCE........6100
         RETURN                                                          SOURCE........6200
      END IF                                                             SOURCE........6300
      IQCPA = IABS(IQCP)                                                 SOURCE........6400
      IF (IQCP.EQ.0) THEN                                                SOURCE........6500
         GOTO 700                                                        SOURCE........6600
      ELSE IF (IQCPA.GT.NN) THEN                                         SOURCE........6700
         ERRCOD = 'INP-17-1'                                             SOURCE........6800
         INERR(1) = IQCPA                                                SOURCE........6900
         INERR(2) = NN                                                   SOURCE........7000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SOURCE........7100
         RETURN                                                          SOURCE........7200
      ELSE IF (NIQP.GT.NSOPI) THEN                                       SOURCE........7300
         GOTO 305                                                        SOURCE........7400
      END IF                                                             SOURCE........7500
      ERRCOD = 'REA-INP-17'                                              SOURCE........7600
      IF (IQCP.GT.0) THEN                                                SOURCE........7700
         READ(INTFIL,*,IOSTAT=INERR(1)) IQCP,QINC                        SOURCE........7800
         IF (INERR(1).NE.0) THEN                                         SOURCE........7900
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     SOURCE........8000
            RETURN                                                       SOURCE........8100
         END IF                                                          SOURCE........8200
         IF (QINC.GT.0D0) THEN                                           SOURCE........8300
            READ(INTFIL,*,IOSTAT=INERR(1)) IQCP,QINC,UINC                SOURCE........8400
            IF (INERR(1).NE.0) THEN                                      SOURCE........8500
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  SOURCE........8600
               RETURN                                                    SOURCE........8700
            END IF                                                       SOURCE........8800
         END IF                                                          SOURCE........8900
      END IF                                                             SOURCE........9000
      IQSOP(NIQP)=IQCP                                                   SOURCE........9100
      IF(IQCP.LT.0) IQSOPT=-1                                            SOURCE........9200
      IQP=IABS(IQCP)                                                     SOURCE........9300
      QIN(IQP)=QINC                                                      SOURCE........9400
      UIN(IQP)=UINC                                                      SOURCE........9500
      IF(IQCP.GT.0) GOTO 450                                             SOURCE........9600
      WRITE(K3,500) IQCP                                                 SOURCE........9700
      GOTO 600                                                           SOURCE........9800
  450 IF(QINC.GT.0) GOTO 460                                             SOURCE........9900
      WRITE(K3,500) IQCP,QINC                                            SOURCE.......10000
      GOTO 600                                                           SOURCE.......10100
  460 WRITE(K3,500) IQCP,QINC,UINC                                       SOURCE.......10200
  500 FORMAT(11X,I10,13X,1PE14.7,16X,1PE14.7)                            SOURCE.......10300
  600 GOTO 305                                                           SOURCE.......10400
  700 NIQP = NIQP - 1                                                    SOURCE.......10500
      IF(NIQP.EQ.NSOPI) GOTO 890                                         SOURCE.......10600
         ERRCOD = 'INP-3,17-1'                                           SOURCE.......10700
         INERR(1) = NIQP                                                 SOURCE.......10800
         INERR(2) = NSOPI                                                SOURCE.......10900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SOURCE.......11000
         RETURN                                                          SOURCE.......11100
  890 IF(IQSOPT.EQ.-1) WRITE(K3,900)                                     SOURCE.......11200
  900 FORMAT(////11X,'THE SPECIFIED TIME VARIATIONS ARE ',               SOURCE.......11300
     1   'USER-PROGRAMMED IN SUBROUTINE  B C T I M E .')                 SOURCE.......11400
C                                                                        SOURCE.......11500
C                                                                        SOURCE.......11600
 1000 IF(NSOUI.EQ.0) GOTO 9000                                           SOURCE.......11700
      IF(ME) 1050,1050,1150                                              SOURCE.......11800
 1050 WRITE(K3,1100)                                                     SOURCE.......11900
 1100 FORMAT(////////11X,'S O L U T E   S O U R C E   D A T A'           SOURCE.......12000
     1   ////11X,'**** NODES AT WHICH SOURCES OR SINKS OF SOLUTE ',      SOURCE.......12100
     2   'MASS ARE SPECIFIED ****'//11X,'NODE NUMBER',10X,               SOURCE.......12200
     3   'SOLUTE SOURCE(+)/SINK(-)'/11X,'(MINUS INDICATES',5X,           SOURCE.......12300
     4   '(SOLUTE MASS/SECOND)'/12X,'TIME-VARYING'/12X,                  SOURCE.......12400
     5   'SOURCE OR SINK)'//)                                            SOURCE.......12500
      GOTO 1300                                                          SOURCE.......12600
 1150 WRITE(K3,1200)                                                     SOURCE.......12700
 1200 FORMAT(////////11X,'E N E R G Y   S O U R C E   D A T A'           SOURCE.......12800
     1   ////11X,'**** NODES AT WHICH SOURCES OR SINKS OF ',             SOURCE.......12900
     2   'ENERGY ARE SPECIFIED ****'//11X,'NODE NUMBER',10X,             SOURCE.......13000
     3   'ENERGY SOURCE(+)/SINK(-)'/11X,'(MINUS INDICATES',5X,           SOURCE.......13100
     4   '(ENERGY/SECOND)'/12X,'TIME-VARYING'/12X,                       SOURCE.......13200
     5   'SOURCE OR SINK)'//)                                            SOURCE.......13300
C                                                                        SOURCE.......13400
C.....INPUT DATASET 18:  DATA FOR ENERGY OR SOLUTE MASS SOURCES OR SINKS SOURCE.......13500
 1300 ERRCOD = 'REA-INP-S18'                                             SOURCE.......13600
      CALL SKPCOM(K1, NLSKIP, ERRCOD)                                    SOURCE.......13700
      IF (ISERR) RETURN                                                  SOURCE.......13800
 1305 NIQU=NIQU+1                                                        SOURCE.......13900
      ERRCOD = 'REA-INP-18'                                              SOURCE.......14000
      CALL READIF(K1, INTFIL, ERRCOD)                                    SOURCE.......14100
      IF (ISERR) RETURN                                                  SOURCE.......14200
      READ(INTFIL,*,IOSTAT=INERR(1)) IQCU                                SOURCE.......14300
      IF (INERR(1).NE.0) THEN                                            SOURCE.......14400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SOURCE.......14500
         RETURN                                                          SOURCE.......14600
      END IF                                                             SOURCE.......14700
      IQCUA = IABS(IQCU)                                                 SOURCE.......14800
      IF (IQCU.EQ.0) THEN                                                SOURCE.......14900
         GOTO 1700                                                       SOURCE.......15000
      ELSE IF (IQCUA.GT.NN) THEN                                         SOURCE.......15100
         ERRCOD = 'INP-18-1'                                             SOURCE.......15200
         INERR(1) = IQCUA                                                SOURCE.......15300
         INERR(2) = NN                                                   SOURCE.......15400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SOURCE.......15500
         RETURN                                                          SOURCE.......15600
      ELSE IF (NIQU.GT.NSOUI) THEN                                       SOURCE.......15700
         GOTO 1305                                                       SOURCE.......15800
      END IF                                                             SOURCE.......15900
      IF (IQCU.GT.0) THEN                                                SOURCE.......16000
         ERRCOD = 'REA-INP-18'                                           SOURCE.......16100
         READ(INTFIL,*,IOSTAT=INERR(1)) IQCU,QUINC                       SOURCE.......16200
         IF (INERR(1).NE.0) THEN                                         SOURCE.......16300
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     SOURCE.......16400
            RETURN                                                       SOURCE.......16500
         END IF                                                          SOURCE.......16600
      END IF                                                             SOURCE.......16700
      IQSOU(NIQU)=IQCU                                                   SOURCE.......16800
      IF(IQCU.LT.0) IQSOUT=-1                                            SOURCE.......16900
      IQU=IABS(IQCU)                                                     SOURCE.......17000
      QUIN(IQU)=QUINC                                                    SOURCE.......17100
      IF(IQCU.GT.0) GOTO 1450                                            SOURCE.......17200
      WRITE(K3,1500) IQCU                                                SOURCE.......17300
      GOTO 1600                                                          SOURCE.......17400
 1450 WRITE(K3,1500) IQCU,QUINC                                          SOURCE.......17500
 1500 FORMAT(11X,I10,13X,1PE14.7)                                        SOURCE.......17600
 1600 GOTO 1305                                                          SOURCE.......17700
 1700 NIQU = NIQU - 1                                                    SOURCE.......17800
      IF(NIQU.EQ.NSOUI) GOTO 1890                                        SOURCE.......17900
         ERRCOD = 'INP-3,18-1'                                           SOURCE.......18000
         IF (ME.EQ.1) THEN                                               SOURCE.......18100
            CHERR(1) = 'energy'                                          SOURCE.......18200
         ELSE                                                            SOURCE.......18300
            CHERR(1) = 'solute'                                          SOURCE.......18400
         END IF                                                          SOURCE.......18500
         INERR(1) = NIQU                                                 SOURCE.......18600
         INERR(2) = NSOUI                                                SOURCE.......18700
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SOURCE.......18800
         RETURN                                                          SOURCE.......18900
 1890 IF(IQSOUT.EQ.-1) WRITE(K3,900)                                     SOURCE.......19000
C                                                                        SOURCE.......19100
 9000 RETURN                                                             SOURCE.......19200
C                                                                        SOURCE.......19300
      END                                                                SOURCE.......19400
C                                                                        SOURCE.......19500
C     SUBROUTINE        S  U  T  E  R  R           SUTRA VERSION 2D3D.1  SUTERR.........100
C                                                                        SUTERR.........200
C *** PURPOSE :                                                          SUTERR.........300
C ***  TO HANDLE SUTRA AND FORTRAN ERRORS.                               SUTERR.........400
C                                                                        SUTERR.........500
      SUBROUTINE SUTERR(ERRCOD, CHERR, INERR, RLERR)                     SUTERR.........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SUTERR.........700
      CHARACTER*80 ERRCOD,CHERR(10),CODE(3),FNAME(0:7)                   SUTERR.........800
      CHARACTER*70 DS(50),EX(50),GN(50)                                  SUTERR.........900
      CHARACTER CDUM*1,CDUM80*80                                         SUTERR........1000
      CHARACTER CINERR(10)*9,CRLERR(10)*15                               SUTERR........1100
      CHARACTER SOLNAM(0:10)*40,SOLWRD(0:10)*10                          SUTERR........1200
      LOGICAL ISERR                                                      SUTERR........1300
      DIMENSION INERR(10), RLERR(10)                                     SUTERR........1400
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SUTERR........1500
     1   NSOP,NSOU,NBCN                                                  SUTERR........1600
      COMMON /ERRHAN/ ISERR                                              SUTERR........1700
      COMMON /FNAMES/ FNAME                                              SUTERR........1800
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        SUTERR........1900
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     SUTERR........2000
     1   KSCRN,KPAUSE                                                    SUTERR........2100
      COMMON /SOLVC/ SOLWRD,SOLNAM                                       SUTERR........2200
      COMMON /SOLVN/ NSLVRS                                              SUTERR........2300
C                                                                        SUTERR........2400
C.....SET THE GLOBAL ERROR FLAG TO TRUE                                  SUTERR........2500
      ISERR = .TRUE.                                                     SUTERR........2600
C                                                                        SUTERR........2700
C.....PARSE THE ERROR CODE                                               SUTERR........2800
      CALL PRSWDS(ERRCOD, '-', 3, CODE, NWORDS)                          SUTERR........2900
C                                                                        SUTERR........3000
C.....IF AN ERROR OTHER THAN A MATRIX SOLVER OR NONLINEAR CONVERGENCE    SUTERR........3100
C        ERROR HAS OCCURRED, OVERRIDE THE SCREEN OUTPUT CONTROLS SO      SUTERR........3200
C        THAT THE ERROR IS PRINTED TO THE SCREEN AND SUTRA PAUSES FOR    SUTERR........3300
C        A USER RESPONSE.                                                SUTERR........3400
      IF ((CODE(1).NE.'SOL').AND.(CODE(1).NE.'CON')) THEN                SUTERR........3500
         KSCRN = +1                                                      SUTERR........3600
         KPAUSE = +1                                                     SUTERR........3700
      END IF                                                             SUTERR........3800
C                                                                        SUTERR........3900
C.....COPY INTEGER AND REAL ERROR PARAMETERS INTO CHARACTER STRINGS      SUTERR........4000
      DO 150 I=1,10                                                      SUTERR........4100
         WRITE(UNIT=CINERR(I), FMT='(I9)') INERR(I)                      SUTERR........4200
         WRITE(UNIT=CRLERR(I), FMT='(1PE15.7)') RLERR(I)                 SUTERR........4300
  150 CONTINUE                                                           SUTERR........4400
C                                                                        SUTERR........4500
C.....INITIALIZE THE ERROR OUTPUT STRINGS                                SUTERR........4600
      DO 200 I=1,50                                                      SUTERR........4700
         DS(I) = "null_line"                                             SUTERR........4800
         EX(I) = "null_line"                                             SUTERR........4900
  200 CONTINUE                                                           SUTERR........5000
C                                                                        SUTERR........5100
C.....SET THE ERROR OUTPUT STRINGS ACCORDING TO THE TYPE OF ERROR        SUTERR........5200
      IF (ERRCOD.EQ.'INP-2A-1') THEN                                     SUTERR........5300
        DS(1)="The first word of SIMULA is not 'SUTRA'."                 SUTERR........5400
        EX(1)="In dataset 2A of the main input file, the first word"     SUTERR........5500
        EX(2)="of the variable SIMULA must be 'SUTRA'."                  SUTERR........5600
        EX(3)=" "                                                        SUTERR........5700
        EX(4)="Example of a valid dataset 2A:"                           SUTERR........5800
        EX(5)="'SUTRA SOLUTE TRANSPORT'"                                 SUTERR........5900
      ELSE IF (ERRCOD.EQ.'INP-2A-2') THEN                                SUTERR........6000
        DS(1)="The second word of SIMULA is not 'SOLUTE' or 'ENERGY'."   SUTERR........6100
        EX(1)="In dataset 2A of the main input file, the second word"    SUTERR........6200
        EX(2)="of the variable SIMULA must be 'SOLUTE' or 'ENERGY'."     SUTERR........6300
        EX(3)=" "                                                        SUTERR........6400
        EX(4)="Example of a valid dataset 2A:"                           SUTERR........6500
        EX(5)="'SUTRA SOLUTE TRANSPORT'"                                 SUTERR........6600
      ELSE IF (ERRCOD.EQ.'INP-2B-1') THEN                                SUTERR........6700
        DS(1)="The first word of MSHSTR is not '2D' or '3D'."            SUTERR........6800
        EX(1)="In dataset 2B of the main input file, the first word"     SUTERR........6900
        EX(2)="of the variable MSHSTR must be '2D' or '3D'."             SUTERR........7000
        EX(3)=" "                                                        SUTERR........7100
        EX(4)="Example of a valid dataset 2B:"                           SUTERR........7200
        EX(5)="'3D BLOCKWISE MESH'  10  20  30"                          SUTERR........7300
      ELSE IF (ERRCOD.EQ.'INP-2B-2') THEN                                SUTERR........7400
        DS(1)="A 3D IRREGULAR mesh has been specified."                  SUTERR........7500
        EX(1)="3D IRREGULAR meshes are not currently allowed; 3D meshes" SUTERR........7600
        EX(2)="must be REGULAR or BLOCKWISE.  However, 2D meshes may be" SUTERR........7700
        EX(3)="IRREGULAR, REGULAR, or BLOCKWISE."                        SUTERR........7800
        EX(4)=" "                                                        SUTERR........7900
        EX(5)="Example of a valid dataset 2B:"                           SUTERR........8000
        EX(6)="'3D BLOCKWISE MESH'  10  20  30"                          SUTERR........8100
      ELSE IF (ERRCOD.EQ.'INP-2B-3') THEN                                SUTERR........8200
        DS(1)="At least one of the rectangular dimensions NN1, NN2,"     SUTERR........8300
        DS(2)="and NN3 is set improperly."                               SUTERR........8400
        EX(1)="In dataset 2B of the main input file, the rectangular"    SUTERR........8500
        EX(2)="dimensions NN1, NN2, and NN3 must obey the following"     SUTERR........8600
        EX(3)="constraints:"                                             SUTERR........8700
        EX(4)="   For a 2D regular mesh, set NN1>2 and NN2>1;"           SUTERR........8800
        EX(5)="   for a 3D regular mesh, set NN1>2, NN2>2, and NN3>1."   SUTERR........8900
        EX(6)=" "                                                        SUTERR........9000
        EX(7)="Example of a valid dataset 2B:"                           SUTERR........9100
        EX(8)="'3D BLOCKWISE MESH'  10  20  30"                          SUTERR........9200
      ELSE IF (ERRCOD.EQ.'INP-2B-4') THEN                                SUTERR........9300
        DS(1)="The second word of MSHSTR is not 'IRREGULAR', 'REGULAR'," SUTERR........9400
        DS(2)="or 'BLOCKWISE'."                                          SUTERR........9500
        EX(1)="In dataset 2B of the main input file, the second word"    SUTERR........9600
        EX(2)="of the variable MSHSTR must be 'IRREGULAR', 'REGULAR',"   SUTERR........9700
        EX(3)="or 'BLOCKWISE'.  Note that 3D IRREGULAR meshes are not"   SUTERR........9800
        EX(4)="currently allowed."                                       SUTERR........9900
        EX(5)=" "                                                        SUTERR.......10000
        EX(6)="Example of a valid dataset 2B:"                           SUTERR.......10100
        EX(7)="'3D BLOCKWISE MESH'  10  20  30"                          SUTERR.......10200
      ELSE IF (ERRCOD.EQ.'INP-2B,3-1') THEN                              SUTERR.......10300
        DS(1)="The number of nodes, NN, does not match the rectangular"  SUTERR.......10400
        DS(2)="dimensions, NN1*NN2*NN3."                                 SUTERR.......10500
        EX(1)="In datasets 2B and 3 of the main input file, the total"   SUTERR.......10600
        EX(2)="number of nodes, NN, must equal the product of the"       SUTERR.......10700
        EX(3)="rectangular dimensions, NN1*NN2*NN3."                     SUTERR.......10800
        EX(4)=" "                                                        SUTERR.......10900
        EX(5)="Example:"                                                 SUTERR.......11000
        EX(6)="If NN1=10, NN2=20, and NN3=30 (dataset 2B), then"         SUTERR.......11100
        EX(7)="NN=10*20*30=6000 (dataset 3)."                            SUTERR.......11200
      ELSE IF (ERRCOD.EQ.'INP-2B,3-2') THEN                              SUTERR.......11300
        DS(1)="The number of elements, NE, does not match the"           SUTERR.......11400
        DS(2)="rectangular dimensions, (NN1-1)*(NN2-1)*(NN3-1)."         SUTERR.......11500
        EX(1)="In datasets 2B and 3 of the main input file, the total"   SUTERR.......11600
        EX(2)="number of elements, NE, must equal the product of the"    SUTERR.......11700
        EX(3)="rectangular dimensions, (NN1-1)*(NN2-1)*(NN3-1)."         SUTERR.......11800
        EX(4)=" "                                                        SUTERR.......11900
        EX(5)="Example:"                                                 SUTERR.......12000
        EX(6)="If NN1=10, NN2=20, and NN3=30 (dataset 2B), then"         SUTERR.......12100
        EX(7)="NE=9*19*29=4959 (dataset 3)."                             SUTERR.......12200
      ELSE IF (ERRCOD.EQ.'INP-4-1') THEN                                 SUTERR.......12300
        DS(1)="The first word of CUNSAT is not 'SATURATED' or"           SUTERR.......12400
        DS(2)="'UNSATURATED'."                                           SUTERR.......12500
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......12600
        EX(2)="of the variable CUNSAT must be 'SATURATED' or"            SUTERR.......12700
        EX(3)="'UNSATURATED'."                                           SUTERR.......12800
        EX(4)=" "                                                        SUTERR.......12900
        EX(5)="Example of a valid dataset 4:"                            SUTERR.......13000
        EX(6)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......13100
     1        " 'COLD' 10"                                               SUTERR.......13200
      ELSE IF (ERRCOD.EQ.'INP-4-2') THEN                                 SUTERR.......13300
        DS(1)="The first word of CSSFLO is not 'STEADY' or 'TRANSIENT'." SUTERR.......13400
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......13500
        EX(2)="of the variable CSSFLO must be 'STEADY' or 'TRANSIENT'."  SUTERR.......13600
        EX(3)=" "                                                        SUTERR.......13700
        EX(4)="Example of a valid dataset 4:"                            SUTERR.......13800
        EX(5)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......13900
     1        " 'COLD' 10"                                               SUTERR.......14000
      ELSE IF (ERRCOD.EQ.'INP-4-3') THEN                                 SUTERR.......14100
        DS(1)="The first word of CSSTRA is not 'STEADY' or 'TRANSIENT'." SUTERR.......14200
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......14300
        EX(2)="of the variable CSSTRA must be 'STEADY' or 'TRANSIENT'."  SUTERR.......14400
        EX(3)=" "                                                        SUTERR.......14500
        EX(4)="Example of a valid dataset 4:"                            SUTERR.......14600
        EX(5)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......14700
     1        " 'COLD' 10"                                               SUTERR.......14800
      ELSE IF (ERRCOD.EQ.'INP-4-4') THEN                                 SUTERR.......14900
        DS(1)="The first word of CREAD is not 'COLD' or 'WARM'."         SUTERR.......15000
        EX(1)="In dataset 4 of the main input file, the first word"      SUTERR.......15100
        EX(2)="of the variable CREAD must be 'COLD' or 'WARM'."          SUTERR.......15200
        EX(3)=" "                                                        SUTERR.......15300
        EX(4)="Example of a valid dataset 4:"                            SUTERR.......15400
        EX(5)="'SATURATED FLOW' 'STEADY FLOW' 'TRANSIENT TRANSPORT'" //  SUTERR.......15500
     1        " 'COLD' 10"                                               SUTERR.......15600
      ELSE IF (ERRCOD.EQ.'INP-4-5') THEN                                 SUTERR.......15700
        DS(1)="Specified TRANSIENT flow with STEADY transport."          SUTERR.......15800
        EX(1)="In dataset 4 of the main input file, TRANSIENT flow"      SUTERR.......15900
        EX(2)="requires TRANSIENT transport.  Likewise, STEADY"          SUTERR.......16000
        EX(3)="transport requires STEADY flow.  The following are"       SUTERR.......16100
        EX(4)="valid combinations:"                                      SUTERR.......16200
        EX(5)=" "                                                        SUTERR.......16300
        EX(6)="     CSSFLO      CSSTRA"                                  SUTERR.......16400
        EX(7)="   ----------- -----------"                               SUTERR.......16500
        EX(8)="    'STEADY'    'STEADY'"                                 SUTERR.......16600
        EX(9)="    'STEADY'   'TRANSIENT'"                               SUTERR.......16700
        EX(10)="   'TRANSIENT' 'TRANSIENT'"                              SUTERR.......16800
        EX(11)=" "                                                       SUTERR.......16900
        EX(12)="Example of a valid dataset 4:"                           SUTERR.......17000
        EX(13)="'SATURATED FLOW' 'STEADY FLOW' 'STEADY TRANSPORT'" //    SUTERR.......17100
     1        " 'COLD' 10"                                               SUTERR.......17200
      ELSE IF (ERRCOD.EQ.'INP-7B&C-1') THEN                              SUTERR.......17300
        DS(1)="Unrecognized solver name."                                SUTERR.......17400
        EX(1)="In datasets 7B&C, valid solver selections are:"           SUTERR.......17500
        EX(2)=" "                                                        SUTERR.......17600
        DO 400 M=0,NSLVRS-1                                              SUTERR.......17700
           EX(M+3)=SOLWRD(M) // " --> " // SOLNAM(M)                     SUTERR.......17800
  400   CONTINUE                                                         SUTERR.......17900
        EX(NSLVRS+3)=" "                                                 SUTERR.......18000
        EX(NSLVRS+4)="Note that solver selections for P and U must be"   SUTERR.......18100
        EX(NSLVRS+5)="both DIRECT or both iterative."                    SUTERR.......18200
      ELSE IF (ERRCOD.EQ.'INP-7B&C-2') THEN                              SUTERR.......18300
        DS(1)="Solver selections for P and U are not both DIRECT or"     SUTERR.......18400
        DS(2)="both iterative."                                          SUTERR.......18500
        EX(1)="The solver selections for P and U must be both"           SUTERR.......18600
        EX(2)="DIRECT or both iterative."                                SUTERR.......18700
      ELSE IF (ERRCOD.EQ.'INP-7B&C-3') THEN                              SUTERR.......18800
        DS(1)="Invalid selection of the CG solver."                      SUTERR.......18900
        EX(1)="The CG solver may be used only for the flow (P) equation" SUTERR.......19000
        EX(2)="with no upstream weighting (UP=0.0).  It may not be used" SUTERR.......19100
        EX(3)="for the transport (U) equation."                          SUTERR.......19200
      ELSE IF (ERRCOD.EQ.'INP-7B&C-4') THEN                              SUTERR.......19300
        DS(1)="Specified an iterative solver for an IRREGULAR mesh."     SUTERR.......19400
        EX(1)="In datasets 7B&C of the main input file, the DIRECT"      SUTERR.......19500
        EX(2)="solver must currently be used if the mesh is IRREGULAR."  SUTERR.......19600
      ELSE IF (ERRCOD.EQ.'INP-3,19-1') THEN                              SUTERR.......19700
        DS(1)="The actual number of specified pressure nodes, "          SUTERR.......19800
     1        // CINERR(1) // ","                                        SUTERR.......19900
        DS(2)="does not equal the input value,                "          SUTERR.......20000
     1        // CINERR(2) // "."                                        SUTERR.......20100
        EX(1)="In dataset 3 of the main input file, the variable NPBC"   SUTERR.......20200
        EX(2)="must specify the exact number of specified pressure"      SUTERR.......20300
        EX(3)="nodes listed in dataset 19."                              SUTERR.......20400
      ELSE IF (ERRCOD.EQ.'INP-3,20-1') THEN                              SUTERR.......20500
        DS(1)="The actual number of specified conc. nodes, "             SUTERR.......20600
     1        // CINERR(1) // ","                                        SUTERR.......20700
        DS(2)="does not equal the input value,             "             SUTERR.......20800
     1        // CINERR(2) // "."                                        SUTERR.......20900
        EX(1)="In dataset 3 of the main input file, the variable NUBC"   SUTERR.......21000
        EX(2)="must specify the exact number of specified concentration" SUTERR.......21100
        EX(3)="nodes listed in dataset 20."                              SUTERR.......21200
      ELSE IF (ERRCOD.EQ.'INP-3,20-2') THEN                              SUTERR.......21300
        DS(1)="The actual number of specified temp. nodes, "             SUTERR.......21400
     1        // CINERR(1) // ","                                        SUTERR.......21500
        DS(2)="does not equal the input value,             "             SUTERR.......21600
     1        // CINERR(2) // "."                                        SUTERR.......21700
        EX(1)="In dataset 3 of the main input file, the variable NUBC"   SUTERR.......21800
        EX(2)="must specify the exact number of specified temperature"   SUTERR.......21900
        EX(3)="nodes listed in dataset 20."                              SUTERR.......22000
      ELSE IF (ERRCOD.EQ.'INP-22-1') THEN                                SUTERR.......22100
        DS(1)="Line 1 of the element incidence data does not begin with" SUTERR.......22200
        DS(2)="the word 'INCIDENCE'."                                    SUTERR.......22300
        EX(1)="In dataset 22 of the main input file, the first line"     SUTERR.......22400
        EX(2)="must begin with the word 'INCIDENCE'."                    SUTERR.......22500
      ELSE IF (ERRCOD.EQ.'INP-22-2') THEN                                SUTERR.......22600
        DS(1)="The incidence data for element " // CINERR(1)             SUTERR.......22700
        DS(2)="are not in numerical order in the dataset."               SUTERR.......22800
        EX(1)="In dataset 22 of the main input file, incidence data"     SUTERR.......22900
        EX(2)="must be listed in order of increasing element number."    SUTERR.......23000
        EX(3)="Note that the numbering of elements must begin at 1"      SUTERR.......23100
        EX(4)="and be continuous; element numbers may not be skipped."   SUTERR.......23200
      ELSE IF (ERRCOD.EQ.'INP-14B,22-1') THEN                            SUTERR.......23300
        DS(1)="At least one element has incorrect geometry."             SUTERR.......23400
        EX(1)="Incorrect element geometry can result from improper"      SUTERR.......23500
        EX(2)="specification of node coordinates in dataset 14B of the"  SUTERR.......23600
        EX(3)="main input file, or from improper ordering of nodes in"   SUTERR.......23700
        EX(4)="a node incidence list in dataset 22 of the same file."    SUTERR.......23800
      ELSE IF (ERRCOD.EQ.'FIL-1') THEN                                   SUTERR.......23900
        DS(1)="The file " // CHERR(2)                                    SUTERR.......24000
        DS(2)="does not exist."                                          SUTERR.......24100
        EX(1)="One of the files required by SUTRA does not exist."       SUTERR.......24200
        EX(2)="Check the filename and the directory path."               SUTERR.......24300
      ELSE IF (ERRCOD.EQ.'FIL-2') THEN                                   SUTERR.......24400
        DS(1)="The file " // CHERR(2)                                    SUTERR.......24500
        DS(2)="could not be opened on FORTRAN unit " // CINERR(1) // "." SUTERR.......24600
        EX(1)="One of the files required by SUTRA could not be opened."  SUTERR.......24700
        EX(2)="Check to make sure the file is not protected or in use"   SUTERR.......24800
        EX(3)="by another application, and that the FORTRAN unit number" SUTERR.......24900
        EX(4)="is valid."                                                SUTERR.......25000
      ELSE IF (ERRCOD.EQ.'FIL-3') THEN                                   SUTERR.......25100
        DS(1)="The files " // CHERR(2)                                   SUTERR.......25200
        DS(2)="and       " // CHERR(3)                                   SUTERR.......25300
        DS(3)="have been assigned the same FORTRAN unit, " // CINERR(1)  SUTERR.......25400
     1         // "."                                                    SUTERR.......25500
        EX(1)='Each file listed in "SUTRA.FIL" must be assigned a'       SUTERR.......25600
        EX(2)="unique unit number."                                      SUTERR.......25700
      ELSE IF (ERRCOD.EQ.'FIL-4') THEN                                   SUTERR.......25800
        DS(1)="The filename " // CHERR(2)                                SUTERR.......25900
        DS(2)="has been used more than once."                            SUTERR.......26000
        EX(1)='Each file listed in "SUTRA.FIL" must be assigned a'       SUTERR.......26100
        EX(2)="unique filename."                                         SUTERR.......26200
      ELSE IF (ERRCOD.EQ.'FIL-5') THEN                                   SUTERR.......26300
        DS(1)="Invalid file type: " // CHERR(2)                          SUTERR.......26400
        EX(1)="Valid file types are:"                                    SUTERR.......26500
        EX(2)='   INP (".inp" input file)'                               SUTERR.......26600
        EX(3)='   ICS (".ics" input file)'                               SUTERR.......26700
        EX(4)='   SMY (".smy" output file)'                              SUTERR.......26800
        EX(5)='   LST (".lst" output file)'                              SUTERR.......26900
        EX(6)='   RST (".rst" output file)'                              SUTERR.......27000
        EX(7)='   NOD (".nod" output file)'                              SUTERR.......27100
        EX(8)='   ELE (".ele" output file)'                              SUTERR.......27200
        EX(9)='   OBS (".obs" output file)'                              SUTERR.......27300
      ELSE IF (ERRCOD.EQ.'FIL-6') THEN                                   SUTERR.......27400
        DS(1)="File type " // CHERR(2)                                   SUTERR.......27500
        DS(2)="has been assigned more than once."                        SUTERR.......27600
        EX(1)="The following file types must be assigned:"               SUTERR.......27700
        EX(2)='   INP (".inp" input file)'                               SUTERR.......27800
        EX(3)='   ICS (".ics" input file)'                               SUTERR.......27900
        EX(4)='   LST (".lst" output file)'                              SUTERR.......28000
        EX(5)='   RST (".rst" output file)'                              SUTERR.......28100
        EX(6)="The following file types are optional:"                   SUTERR.......28200
        EX(7)='   SMY (".smy" output file; defaults to "SUTRA.SMY")'     SUTERR.......28300
        EX(8)='   NOD (".nod" output file)'                              SUTERR.......28400
        EX(9)='   ELE (".ele" output file)'                              SUTERR.......28500
        EX(10)='   OBS (".obs" output file)'                             SUTERR.......28600
        EX(11)="No file type may be assigned more than once."            SUTERR.......28700
      ELSE IF (ERRCOD.EQ.'FIL-7') THEN                                   SUTERR.......28800
        DS(1)="Required file type " // CHERR(2)                          SUTERR.......28900
        DS(2)="has not been assigned."                                   SUTERR.......29000
        EX(1)="The following file types must be assigned:"               SUTERR.......29100
        EX(2)='   INP (".inp" input file)'                               SUTERR.......29200
        EX(3)='   ICS (".ics" input file)'                               SUTERR.......29300
        EX(4)='   LST (".lst" output file)'                              SUTERR.......29400
        EX(5)='   RST (".rst" output file)'                              SUTERR.......29500
        EX(6)="The following file types are optional:"                   SUTERR.......29600
        EX(7)='   SMY (".smy" output file; defaults to "SUTRA.SMY")'     SUTERR.......29700
        EX(8)='   NOD (".nod" output file)'                              SUTERR.......29800
        EX(9)='   ELE (".ele" output file)'                              SUTERR.......29900
        EX(10)='   OBS (".obs" output file)'                             SUTERR.......30000
        EX(11)="No file type may be assigned more than once."            SUTERR.......30100
      ELSE IF (ERRCOD.EQ.'INP-6-1') THEN                                 SUTERR.......30200
        DS(1)="NPCYC<1 and/or NUCYC<1."                                  SUTERR.......30300
        EX(1)="In dataset 6 of the main input file, both NPCYC and"      SUTERR.......30400
        EX(2)="NUCYC must be set greater than or equal to 1."            SUTERR.......30500
      ELSE IF (ERRCOD.EQ.'INP-6-2') THEN                                 SUTERR.......30600
        DS(1)="Neither NPCYC nor NUCYC is set to 1."                     SUTERR.......30700
        EX(1)="In dataset 6 of the main input file, either NPCYC or"     SUTERR.......30800
        EX(2)="NUCYC (or both) must be set to 1."                        SUTERR.......30900
      ELSE IF (ERRCOD.EQ.'INP-6-3') THEN                                 SUTERR.......31000
        DS(1)="DELT is greater than DTMAX."                              SUTERR.......31100
        EX(1)="In dataset 6 of the main input file, DELT must be set"    SUTERR.......31200
        EX(2)="less than or equal to DTMAX."                             SUTERR.......31300
      ELSE IF ((ERRCOD.EQ.'INP-8A-1').OR.(ERRCOD.EQ.'INP-8A-2')          SUTERR.......31400
     1     .OR.(ERRCOD.EQ.'INP-8A-3').OR.(ERRCOD.EQ.'INP-8A-4')          SUTERR.......31500
     1     .OR.(ERRCOD.EQ.'INP-8A-5').OR.(ERRCOD.EQ.'INP-8A-6')          SUTERR.......31600
     1     .OR.(ERRCOD.EQ.'INP-8A-7')) THEN                              SUTERR.......31700
        DS(1)=CHERR(1)(1:6) // " is not 'Y' or 'N'."                     SUTERR.......31800
        EX(1)="In dataset 8A of the main input file, " // CHERR(1)(1:6)  SUTERR.......31900
        EX(2)="must be set to either 'Y' or 'N'."                        SUTERR.......32000
        EX(3)=" "                                                        SUTERR.......32100
        EX(4)="Example of a valid dataset 8A:"                           SUTERR.......32200
        EX(5)="10   'N'   'N'   'N'   'Y'   'Y'   'Y'   'Y'"             SUTERR.......32300
      ELSE IF (ERRCOD.EQ.'INP-8B-1') THEN                                SUTERR.......32400
        DS(1)="Node number listed in column other than column 1."        SUTERR.......32500
        EX(1)="In dataset 8B of the main input file, if the node number" SUTERR.......32600
        EX(2)="is to appear, it must appear only in column 1, i.e.,"     SUTERR.......32700
        EX(3)="only NCOL(1) can be set to 'N'."                          SUTERR.......32800
      ELSE IF (ERRCOD.EQ.'INP-8B-2') THEN                                SUTERR.......32900
        DS(1)="Specified that 'Z' be output for a 2D problem."           SUTERR.......33000
        EX(1)="In dataset 8B of the main input file, 'Z' can be listed"  SUTERR.......33100
        EX(2)="only if the problem is 3D."                               SUTERR.......33200
      ELSE IF (ERRCOD.EQ.'INP-8B-3') THEN                                SUTERR.......33300
        DS(1)="Unrecognized value for NCOL."                             SUTERR.......33400
        EX(1)="In dataset 8B of the main input file, the following"      SUTERR.......33500
        EX(2)="variables may be listed:"                                 SUTERR.......33600
        EX(3)=" "                                                        SUTERR.......33700
        EX(4)="'N'  =  node number (if used, it must appear first)"      SUTERR.......33800
        EX(5)="'X'  =  X-coordinate of node"                             SUTERR.......33900
        EX(6)="'Y'  =  Y-coordinate of node"                             SUTERR.......34000
        EX(7)="'Z'  =  Z-coordinate of node (3D only)"                   SUTERR.......34100
        EX(8)="'P'  =  pressure"                                         SUTERR.......34200
        EX(9)="'U'  =  concentration or temperature"                     SUTERR.......34300
        EX(10)="'S'  =  saturation"                                      SUTERR.......34400
        EX(11)=" "                                                       SUTERR.......34500
        EX(12)="The symbol '-' (a single dash) is used to end the list." SUTERR.......34600
        EX(13)="Any symbols following '-' are ignored."                  SUTERR.......34700
        EX(14)=" "                                                       SUTERR.......34800
        EX(15)="Example of a valid dataset 8B for a 3D problem:"         SUTERR.......34900
        EX(16)="10  'N'  'X'  'Y'  'Z'  'S'  'U'  '-'"                   SUTERR.......35000
      ELSE IF (ERRCOD.EQ.'INP-8C-1') THEN                                SUTERR.......35100
        DS(1)="Element number listed in column other than column 1."     SUTERR.......35200
        EX(1)="In dataset 8C of the main input file, if the element"     SUTERR.......35300
        EX(2)="number is to appear, it must appear only in column 1,"    SUTERR.......35400
        EX(3)="i.e., only LCOL(1) can be set to 'E'."                    SUTERR.......35500
      ELSE IF (ERRCOD.EQ.'INP-8C-2') THEN                                SUTERR.......35600
        DS(1)="Specified that 'Z' be output for a 2D problem."           SUTERR.......35700
        EX(1)="In dataset 8C of the main input file, 'Z' can be listed"  SUTERR.......35800
        EX(2)="only if the problem is 3D."                               SUTERR.......35900
      ELSE IF (ERRCOD.EQ.'INP-8C-3') THEN                                SUTERR.......36000
        DS(1)="Unrecognized value for LCOL."                             SUTERR.......36100
        EX(1)="In dataset 8C of the main input file, the following"      SUTERR.......36200
        EX(2)="variables may be listed:"                                 SUTERR.......36300
        EX(3)=" "                                                        SUTERR.......36400
        EX(4)="'E'  =  element number (if used, it must appear first)"   SUTERR.......36500
        EX(5)="'X'  =  X-coordinate of element centroid"                 SUTERR.......36600
        EX(6)="'Y'  =  Y-coordinate of element centroid"                 SUTERR.......36700
        EX(7)="'Z'  =  Z-coordinate of element centroid (3D only)"       SUTERR.......36800
        EX(8)="'VX'  =  X-component of fluid velocity"                   SUTERR.......36900
        EX(9)="'VY'  =  Y-component of fluid velocity"                   SUTERR.......37000
        EX(10)="'VZ'  =  Z-component of fluid velocity (3D only)"        SUTERR.......37100
        EX(11)=" "                                                       SUTERR.......37200
        EX(12)="The symbol '-' (a single dash) is used to end the list." SUTERR.......37300
        EX(13)="Any symbols following '-' are ignored."                  SUTERR.......37400
        EX(14)=" "                                                       SUTERR.......37500
        EX(15)="Example of a valid dataset 8B for a 3D problem:"         SUTERR.......37600
        EX(16)="10  'E'  'X'  'Y'  'Z'  'VX'  'VY'  'VZ'  '-'"           SUTERR.......37700
      ELSE IF (ERRCOD.EQ.'INP-8C-4') THEN                                SUTERR.......37800
        DS(1)="Specified that 'VZ' be output for a 2D problem."          SUTERR.......37900
        EX(1)="In dataset 8C of the main input file, 'VZ' can be listed" SUTERR.......38000
        EX(2)="only if the problem is 3D."                               SUTERR.......38100
      ELSE IF (ERRCOD.EQ.'INP-8D-1') THEN                                SUTERR.......38200
        DS(1)="The actual number of observation nodes listed does not"   SUTERR.......38300
        DS(2)="equal the input value, or the observation node list"      SUTERR.......38400
        DS(3)="does not end with a zero."                                SUTERR.......38500
        EX(1)="In dataset 8D of the main input file, the number of"      SUTERR.......38600
        EX(2)="nodes listed must equal the number, NOBS, specified in"   SUTERR.......38700
        EX(3)="dataset 3 of the same file, and a zero must appear after" SUTERR.......38800
        EX(4)="the last node in the list.  Any information appearing"    SUTERR.......38900
        EX(5)="after the zero is ignored."                               SUTERR.......39000
        EX(6)=" "                                                        SUTERR.......39100
        EX(7)="Example of a valid dataset 8D with three observation"     SUTERR.......39200
        EX(8)="nodes (45, 46, and 7347):"                                SUTERR.......39300
        EX(9)="10   45   46   7347   0"                                  SUTERR.......39400
      ELSE IF (ERRCOD.EQ.'INP-8D-2') THEN                                SUTERR.......39500
        DS(1)="The observation node list contains a node number that"    SUTERR.......39600
        DS(2)="is negative or zero."                                     SUTERR.......39700
        EX(1)="In dataset 8D of the main input file, all node numbers"   SUTERR.......39800
        EX(2)="must be positive.  The last entry must be a zero, which"  SUTERR.......39900
        EX(3)="signals the end of the list."                             SUTERR.......40000
        EX(4)=" "                                                        SUTERR.......40100
        EX(5)="Example of a valid dataset 8D with three observation"     SUTERR.......40200
        EX(6)="nodes (45, 46, and 7347):"                                SUTERR.......40300
        EX(7)="10   45   46   7347   0"                                  SUTERR.......40400
      ELSE IF (ERRCOD.EQ.'INP-11-1') THEN                                SUTERR.......40500
        DS(1)="Unrecognized sorption model."                             SUTERR.......40600
        EX(1)="In dataset 11 of the main input file, the sorption model" SUTERR.......40700
        EX(2)="may be chosen from the following:"                        SUTERR.......40800
        EX(3)=" "                                                        SUTERR.......40900
        EX(4)="'NONE'       =  No sorption"                              SUTERR.......41000
        EX(5)="'LINEAR'     =  Linear sorption model"                    SUTERR.......41100
        EX(6)="'FREUNDLICH' =  Freundlich sorption model"                SUTERR.......41200
        EX(7)="'LANGMUIR'   =  Langmuir sorption model"                  SUTERR.......41300
      ELSE IF (ERRCOD.EQ.'INP-11-2') THEN                                SUTERR.......41400
        DS(1)="The second Freundlich sorption coefficient is less than"  SUTERR.......41500
        DS(2)="or equal to zero."                                        SUTERR.......41600
        EX(1)="In dataset 11 of the main input file, the second"         SUTERR.......41700
        EX(2)="coefficient, CHI2, must be positive if Freundlich"        SUTERR.......41800
        EX(3)="sorption is chosen."                                      SUTERR.......41900
      ELSE IF (ERRCOD.EQ.'INP-14A-1') THEN                               SUTERR.......42000
        DS(1)="Dataset 14A does not begin with the word 'NODE'."         SUTERR.......42100
        EX(1)="Dataset 14A of the main input file must begin with the"   SUTERR.......42200
        EX(2)="word 'NODE'."                                             SUTERR.......42300
        EX(3)=" "                                                        SUTERR.......42400
        EX(4)="Example of a valid dataset 14A:"                          SUTERR.......42500
        EX(5)="'NODE'  1000.  1000.  1.  0.1"                            SUTERR.......42600
      ELSE IF (ERRCOD.EQ.'INP-15A-1') THEN                               SUTERR.......42700
        DS(1)="Dataset 15A does not begin with the word 'ELEMENT'."      SUTERR.......42800
        EX(1)="Dataset 15A of the main input file must begin with the"   SUTERR.......42900
        EX(2)="word 'ELEMENT'."                                          SUTERR.......43000
        EX(3)=" "                                                        SUTERR.......43100
        EX(4)="Example of a valid dataset 15A for a " // CHERR(1)(1:2)   SUTERR.......43200
     1         // " problem:"                                            SUTERR.......43300
        IF (CHERR(1).EQ."3D") THEN                                       SUTERR.......43400
          EX(5)="'ELEMENT' 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1." SUTERR.......43500
        ELSE                                                             SUTERR.......43600
          EX(5)="'ELEMENT' 1. 1. 1. 1. 1. 1. 1."                         SUTERR.......43700
        END IF                                                           SUTERR.......43800
      ELSE IF (ERRCOD.EQ.'ICS-2-1') THEN                                 SUTERR.......43900
        DS(1)="Unrecognized initialization type."                        SUTERR.......44000
        EX(1)="In dataset 2 of the initial conditions input file,"       SUTERR.......44100
        EX(2)="the valid types of initializations for P are UNIFORM"     SUTERR.......44200
        EX(3)="and NONUNIFORM."                                          SUTERR.......44300
      ELSE IF (ERRCOD.EQ.'ICS-2-2') THEN                                 SUTERR.......44400
        DS(1)="Did not specify NONUNIFORM initial values during a WARM"  SUTERR.......44500
        DS(2)="start."                                                   SUTERR.......44600
        EX(1)="In dataset 2 of the initial conditions input file,"       SUTERR.......44700
        EX(2)="initial values for P must be specified as NONUNIFORM"     SUTERR.......44800
        EX(3)="during a WARM start (i.e., if CREAD='WARM' in dataset 4"  SUTERR.......44900
        EX(4)="of the main input file)."                                 SUTERR.......45000
      ELSE IF (ERRCOD.EQ.'ICS-3-1') THEN                                 SUTERR.......45100
        DS(1)="Unrecognized initialization type."                        SUTERR.......45200
        EX(1)="In dataset 3 of the initial conditions input file,"       SUTERR.......45300
        EX(2)="the valid types of initializations for U are UNIFORM"     SUTERR.......45400
        EX(3)="and NONUNIFORM."                                          SUTERR.......45500
      ELSE IF (ERRCOD.EQ.'ICS-3-2') THEN                                 SUTERR.......45600
        DS(1)="Did not specify NONUNIFORM initial values during a WARM"  SUTERR.......45700
        DS(2)="start."                                                   SUTERR.......45800
        EX(1)="In dataset 3 of the initial conditions input file,"       SUTERR.......45900
        EX(2)="initial values for U must be specified as NONUNIFORM"     SUTERR.......46000
        EX(3)="during a WARM start (i.e., if CREAD='WARM' in dataset 4"  SUTERR.......46100
        EX(4)="of the main input file)."                                 SUTERR.......46200
      ELSE IF (ERRCOD.EQ.'SOL-1') THEN                                   SUTERR.......46300
        DS(1)="Error returned by the " // CHERR(2)(1:10)                 SUTERR.......46400
        DS(2)="solver while solving for " // CHERR(1)(1:1) // "."        SUTERR.......46500
        EX(1)="The iterative solver has stopped because of an error."    SUTERR.......46600
        EX(2)="Error flag values are interpreted as follows:"            SUTERR.......46700
        EX(3)="  "                                                       SUTERR.......46800
        EX(4)="IERR = 2  =>  Method stalled or failed to converge in"    SUTERR.......46900
        EX(5)="              the maximum number of iterations allowed."  SUTERR.......47000
        EX(6)="IERR = 4  =>  Convergence tolerance set too tight for"    SUTERR.......47100
        EX(7)="              machine precision."                         SUTERR.......47200
        EX(8)="IERR = 5  =>  Method broke down because preconditioning"  SUTERR.......47300
        EX(9)="              matrix is non-positive-definite."           SUTERR.......47400
        EX(10)="IERR = 6  =>  Method broke down because matrix is non-"  SUTERR.......47500
        EX(11)="              positive-definite or nearly so."           SUTERR.......47600
        EX(12)=" "                                                       SUTERR.......47700
        EX(13)="If the P-solution resulted in a solver error, an"        SUTERR.......47800
        EX(14)="attempt was still made to obtain a U-solution."          SUTERR.......47900
        EX(15)="The last P and U solutions were written to the"          SUTERR.......48000
        EX(16)="appropriate output files (except the restart file)"      SUTERR.......48100
        EX(17)="whether or not they resulted in solver errors."          SUTERR.......48200
      ELSE IF (ERRCOD.EQ.'INP-3,17-1') THEN                              SUTERR.......48300
        DS(1)="The actual number of"                                     SUTERR.......48400
        DS(2)="specified fluid source nodes,   " // CINERR(1) // ","     SUTERR.......48500
        DS(3)="does not equal the input value, " // CINERR(2) // "."     SUTERR.......48600
        EX(1)="In dataset 3 of the main input file, the variable NSOP"   SUTERR.......48700
        EX(2)="must specify the exact number of specified fluid source"  SUTERR.......48800
        EX(3)="nodes listed in dataset 17."                              SUTERR.......48900
      ELSE IF (ERRCOD.EQ.'INP-3,18-1') THEN                              SUTERR.......49000
        DS(1)="The actual number of"                                     SUTERR.......49100
        DS(2)="specified " // CHERR(1)(1:6) // " source nodes,  "        SUTERR.......49200
     1         // CINERR(1) // ","                                       SUTERR.......49300
        DS(3)="does not equal the input value, " // CINERR(2) // "."     SUTERR.......49400
        EX(1)="In dataset 3 of the main input file, the variable NSOU"   SUTERR.......49500
        EX(2)="must specify the exact number of specified "              SUTERR.......49600
     1         // CHERR(1)(1:6) // " source"                             SUTERR.......49700
        EX(3)="nodes listed in dataset 18."                              SUTERR.......49800
      ELSE IF (ERRCOD.EQ.'INP-17-1') THEN                                SUTERR.......49900
        DS(1)="Invalid node number referenced in dataset 17: "           SUTERR.......50000
     1         // CINERR(1)                                              SUTERR.......50100
        EX(1)="Dataset 17 of the main input file contains a reference"   SUTERR.......50200
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......50300
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......50400
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......50500
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......50600
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......50700
      ELSE IF (ERRCOD.EQ.'INP-18-1') THEN                                SUTERR.......50800
        DS(1)="Invalid node number referenced in dataset 18: "           SUTERR.......50900
     1         // CINERR(1)                                              SUTERR.......51000
        EX(1)="Dataset 18 of the main input file contains a reference"   SUTERR.......51100
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......51200
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......51300
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......51400
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......51500
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......51600
      ELSE IF (ERRCOD.EQ.'INP-19-1') THEN                                SUTERR.......51700
        DS(1)="Invalid node number referenced in dataset 19: "           SUTERR.......51800
     1         // CINERR(1)                                              SUTERR.......51900
        EX(1)="Dataset 19 of the main input file contains a reference"   SUTERR.......52000
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......52100
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......52200
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......52300
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......52400
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......52500
      ELSE IF (ERRCOD.EQ.'INP-20-1') THEN                                SUTERR.......52600
        DS(1)="Invalid node number referenced in dataset 20: "           SUTERR.......52700
     1         // CINERR(1)                                              SUTERR.......52800
        EX(1)="Dataset 20 of the main input file contains a reference"   SUTERR.......52900
        EX(2)="to a non-existent node number.  All node numbers must"    SUTERR.......53000
        EX(3)="be less than or equal to the total number of nodes,"      SUTERR.......53100
        EX(4)="NN = " // CINERR(2)                                       SUTERR.......53200
        EX(5)="(excluding the negative sign that precedes nodes with"    SUTERR.......53300
        EX(6)="time-dependent boundary conditions)."                     SUTERR.......53400
      ELSE IF (ERRCOD.EQ.'CON-1') THEN                                   SUTERR.......53500
        CDUM80 = 's'                                                     SUTERR.......53600
        IF (INERR(4).GT.13) THEN                                         SUTERR.......53700
           LDUM = 1                                                      SUTERR.......53800
        ELSE                                                             SUTERR.......53900
           LDUM = 0                                                      SUTERR.......54000
        END IF                                                           SUTERR.......54100
        DS(1)="Simulation terminated due to unconverged non-linearity"   SUTERR.......54200
        DS(2)="iterations.  Tolerance" // CDUM80(1:LDUM)                 SUTERR.......54300
     1         // " for " // CHERR(1)(1:INERR(4))                        SUTERR.......54400
        DS(3)="not reached."                                             SUTERR.......54500
        EX(1)="The " // CHERR(1)(1:INERR(4)) // " solution"              SUTERR.......54600
     1         // CDUM80(1:LDUM) // " failed"                            SUTERR.......54700
        EX(2)="to converge to the specified tolerance"                   SUTERR.......54800
     1         // CDUM80(1:LDUM) // " within"                            SUTERR.......54900
        EX(3)="the maximum number of iterations allowed to resolve"      SUTERR.......55000
        EX(4)="non-linearities.  The parameters that control these"      SUTERR.......55100
        EX(5)="iterations are set in dataset 7A of the main input file." SUTERR.......55200
      ELSE IF ((CODE(1).EQ.'REA').AND.                                   SUTERR.......55300
     1         ((CODE(2).EQ.'INP').OR.(CODE(2).EQ.'ICS'))) THEN          SUTERR.......55400
        IF (CODE(2).EQ.'INP') THEN                                       SUTERR.......55500
           CDUM80 = 'main input file'                                    SUTERR.......55600
           LDUM = 15                                                     SUTERR.......55700
        ELSE                                                             SUTERR.......55800
           CDUM80 = 'init. cond. input file'                             SUTERR.......55900
           LDUM = 22                                                     SUTERR.......56000
        END IF                                                           SUTERR.......56100
        IF (CODE(3)(1:1).EQ.'S') THEN                                    SUTERR.......56200
          CODE(3) = CODE(3)(2:4)                                         SUTERR.......56300
          IF ((CODE(2).EQ.'ICS').AND.(CODE(3).EQ.'4')) THEN              SUTERR.......56400
           DS(1)="FORTRAN returned an error while reading comment lines" SUTERR.......56500
           DS(2)="that precede the restart information following"        SUTERR.......56600
           DS(3)="dataset 3 of the initial conditions input file,"       SUTERR.......56700
           DS(4)="or while reading the first line of that information."  SUTERR.......56800
          ELSE                                                           SUTERR.......56900
           DS(1)="FORTRAN returned an error while reading comment lines" SUTERR.......57000
           DS(2)="that precede dataset " // CODE(3)(1:3)                 SUTERR.......57100
     1            // " of the " // CDUM80(1:LDUM) // ","                 SUTERR.......57200
           DS(3)="or while reading the first line of that dataset."      SUTERR.......57300
          END IF                                                         SUTERR.......57400
        ELSE                                                             SUTERR.......57500
          IF ((CODE(2).EQ.'ICS').AND.(CODE(3).EQ.'4')) THEN              SUTERR.......57600
           DS(1)="FORTRAN returned an error while reading the restart"   SUTERR.......57700
           DS(2)="information following dataset 3 of the initial"        SUTERR.......57800
           DS(3)="conditions input file."                                SUTERR.......57900
          ELSE                                                           SUTERR.......58000
           DS(1)="FORTRAN returned an error while reading"               SUTERR.......58100
           DS(2)="dataset " // CODE(3)(1:3)                              SUTERR.......58200
     1            // " of the " // CDUM80(1:LDUM) // "."                 SUTERR.......58300
          END IF                                                         SUTERR.......58400
        END IF                                                           SUTERR.......58500
        EX(1)="A FORTRAN error has occurred while reading input data."   SUTERR.......58600
        EX(2)="Error status flag values are interpreted as follows:"     SUTERR.......58700
        EX(3)=" "                                                        SUTERR.......58800
        EX(4)="IOSTAT < 0  =>  The end of a line was reached before"     SUTERR.......58900
        EX(5)="                all the required data were read from"     SUTERR.......59000
        EX(6)="                that line.  Check the specified dataset"  SUTERR.......59100
        EX(7)="                for missing data."                        SUTERR.......59200
        EX(8)="IOSTAT > 0  =>  An error occurred while the specified"    SUTERR.......59300
        EX(9)="                dataset was being read.  Usually, this"   SUTERR.......59400
        EX(10)="                indicates that the READ statement"       SUTERR.......59500
        EX(11)="                encountered data of a type that is"      SUTERR.......59600
        EX(12)="                incompatible with the type it expected." SUTERR.......59700
        EX(13)="                Check the dataset for typographical"     SUTERR.......59800
        EX(14)="                errors and missing or extraneous data."  SUTERR.......59900
      ELSE IF ((CODE(1).EQ.'REA').AND.(CODE(2).EQ.'FIL')) THEN           SUTERR.......60000
        DS(1)='FORTRAN returned an error while reading "SUTRA.FIL".'     SUTERR.......60100
        EX(1)='A FORTRAN error has occurred while reading "SUTRA.FIL".'  SUTERR.......60200
        EX(2)="Error status flag values are interpreted as follows:"     SUTERR.......60300
        EX(3)=" "                                                        SUTERR.......60400
        EX(4)="IOSTAT < 0  =>  The end of a line was reached before"     SUTERR.......60500
        EX(5)="                all the required data were read from"     SUTERR.......60600
        EX(6)='                that line.  Check "SUTRA.FIL" for'        SUTERR.......60700
        EX(7)="                missing data."                            SUTERR.......60800
        EX(8)="IOSTAT > 0  =>  An error occurred while the input"        SUTERR.......60900
        EX(9)="                file was being read.  Usually, this"      SUTERR.......61000
        EX(10)="                indicates that the READ statement"       SUTERR.......61100
        EX(11)="                encountered data of a type that is"      SUTERR.......61200
        EX(12)="                incompatible with the type it expected." SUTERR.......61300
        EX(13)='                Check "SUTRA.FIL" for typographical'     SUTERR.......61400
        EX(14)="                errors and missing or extraneous data."  SUTERR.......61500
      END IF                                                             SUTERR.......61600
C                                                                        SUTERR.......61700
C.....WRITE ERROR MESSAGE.  FORMAT DEPENDS ON THE TYPE OF ERROR.         SUTERR.......61800
      IF ((CODE(1).EQ.'INP').OR.(CODE(1).EQ.'ICS')) THEN                 SUTERR.......61900
C........ERROR TYPES 'INP' AND 'ICS' (INPUT DATA ERROR)                  SUTERR.......62000
         IF (KSCRN.EQ.1)                                                 SUTERR.......62100
     1      WRITE (*,1888) '           INPUT DATA ERROR           '      SUTERR.......62200
         WRITE (K00,1888) '           INPUT DATA ERROR           '       SUTERR.......62300
         IF (KSCRN.EQ.1) WRITE (*,1011)                                  SUTERR.......62400
         WRITE (K00,1011)                                                SUTERR.......62500
 1011    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......62600
         IF (CODE(1).EQ.'INP') THEN                                      SUTERR.......62700
            CDUM80 = FNAME(1)                                            SUTERR.......62800
         ELSE                                                            SUTERR.......62900
            CDUM80 = FNAME(2)                                            SUTERR.......63000
         END IF                                                          SUTERR.......63100
         IF (KSCRN.EQ.1) WRITE (*,1013) ERRCOD, CDUM80, CODE(2)          SUTERR.......63200
         WRITE (K00,1013) ERRCOD, CDUM80, CODE(2)                        SUTERR.......63300
 1013    FORMAT (/4X,'Error code:',2X,A40                                SUTERR.......63400
     1           /4X,'File:      ',2X,A40                                SUTERR.......63500
     1           /4X,'Dataset(s):',2X,A40/)                              SUTERR.......63600
         DO 1015 I=1,50                                                  SUTERR.......63700
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......63800
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......63900
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......64000
 1015    CONTINUE                                                        SUTERR.......64100
         IF (KSCRN.EQ.1) WRITE (*,1021)                                  SUTERR.......64200
         WRITE (K00,1021)                                                SUTERR.......64300
 1021    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......64400
         DO 1025 I=1,50                                                  SUTERR.......64500
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......64600
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......64700
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......64800
 1025    CONTINUE                                                        SUTERR.......64900
         IF (KSCRN.EQ.1) WRITE (*,1081)                                  SUTERR.......65000
         WRITE (K00,1081)                                                SUTERR.......65100
 1081    FORMAT (/1X,'GENERAL NOTE'/                                     SUTERR.......65200
     1     /4X,'If the dataset for which SUTRA has reported an error'    SUTERR.......65300
     1     /4X,'appears to be correct, check the preceding datasets'     SUTERR.......65400
     1     /4X,'for missing data or extra lines of data.')               SUTERR.......65500
      ELSE IF (CODE(1).EQ.'FIL') THEN                                    SUTERR.......65600
C........ERROR TYPE 'FIL' (FILE ERROR)                                   SUTERR.......65700
         IF (KSCRN.EQ.1)                                                 SUTERR.......65800
     1      WRITE (*,1888)'              FILE ERROR              '       SUTERR.......65900
         WRITE (K00,1888) '              FILE ERROR              '       SUTERR.......66000
         IF (KSCRN.EQ.1) WRITE (*,1211)                                  SUTERR.......66100
         WRITE (K00,1211)                                                SUTERR.......66200
 1211    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......66300
         IF (KSCRN.EQ.1) WRITE (*,1213) ERRCOD, CHERR(1)                 SUTERR.......66400
         WRITE (K00,1213) ERRCOD, CHERR(1)                               SUTERR.......66500
 1213    FORMAT (/4X,'Error code:',2X,A40                                SUTERR.......66600
     1           /4X,'File:      ',2X,A40/)                              SUTERR.......66700
         DO 1215 I=1,50                                                  SUTERR.......66800
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......66900
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......67000
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......67100
 1215    CONTINUE                                                        SUTERR.......67200
         IF (KSCRN.EQ.1) WRITE (*,1221)                                  SUTERR.......67300
         WRITE (K00,1221)                                                SUTERR.......67400
 1221    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......67500
         DO 1225 I=1,50                                                  SUTERR.......67600
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......67700
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......67800
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......67900
 1225    CONTINUE                                                        SUTERR.......68000
      ELSE IF (CODE(1).EQ.'SOL') THEN                                    SUTERR.......68100
C........ERROR TYPE 'SOL' (MATRIX SOLVER ERROR)                          SUTERR.......68200
         IF (KSCRN.EQ.1)                                                 SUTERR.......68300
     1      WRITE (*,1888) '         MATRIX SOLVER ERROR          '      SUTERR.......68400
         WRITE (K00,1888) '         MATRIX SOLVER ERROR          '       SUTERR.......68500
         IF (KSCRN.EQ.1) WRITE (*,1311)                                  SUTERR.......68600
         WRITE (K00,1311)                                                SUTERR.......68700
 1311    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......68800
         IF (KSCRN.EQ.1) WRITE (*,1313) ERRCOD, CHERR(2),                SUTERR.......68900
     1      INERR(1), INERR(2), RLERR(1), RLERR(2)                       SUTERR.......69000
         WRITE (K00,1313) ERRCOD, CHERR(2), INERR(1), INERR(2),          SUTERR.......69100
     1      RLERR(1), RLERR(2)                                           SUTERR.......69200
 1313    FORMAT (/4X,'Error code:',2X,A40                                SUTERR.......69300
     1           /4X,'Solver:    ',2X,A40                                SUTERR.......69400
     1          //4X,'Error flag..........IERR = ',I3                    SUTERR.......69500
     1           /4X,'# of solver iters...ITRS = ',I5                    SUTERR.......69600
     1           /4X,'Error estimate.......ERR = ',1PE8.1                SUTERR.......69700
     1           /4X,'Error tolerance......TOL = ',1PE8.1/)              SUTERR.......69800
         DO 1315 I=1,50                                                  SUTERR.......69900
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......70000
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......70100
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......70200
 1315    CONTINUE                                                        SUTERR.......70300
         IF (KSCRN.EQ.1) WRITE (*,1321)                                  SUTERR.......70400
         WRITE (K00,1321)                                                SUTERR.......70500
 1321    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......70600
         DO 1325 I=1,50                                                  SUTERR.......70700
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......70800
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......70900
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......71000
 1325    CONTINUE                                                        SUTERR.......71100
      ELSE IF (CODE(1).EQ.'CON') THEN                                    SUTERR.......71200
C........ERROR TYPE 'CON' (CONVERGENCE ERROR)                            SUTERR.......71300
         IF (KSCRN.EQ.1)                                                 SUTERR.......71400
     1      WRITE (*,1888) '          CONVERGENCE ERROR           '      SUTERR.......71500
         WRITE (K00,1888) '         CONVERGENCE ERROR          '         SUTERR.......71600
         IF (KSCRN.EQ.1) WRITE (*,1411)                                  SUTERR.......71700
         WRITE (K00,1411)                                                SUTERR.......71800
 1411    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......71900
         IF (KSCRN.EQ.1) WRITE (*,1413) ERRCOD, CHERR(1), INERR(3),      SUTERR.......72000
     1       RLERR(1), INERR(1), RLERR(2), RLERR(3), INERR(2), RLERR(4)  SUTERR.......72100
         WRITE (K00,1413) ERRCOD, CHERR(1), INERR(3),                    SUTERR.......72200
     1       RLERR(1), INERR(1), RLERR(2), RLERR(3), INERR(2), RLERR(4)  SUTERR.......72300
 1413    FORMAT (/4X,'Error code: ',2X,A40                               SUTERR.......72400
     1           /4X,'Unconverged:',2X,A40                               SUTERR.......72500
     1      //4X,'# of iterations.....ITER = ',I5                        SUTERR.......72600
     1       /4X,'Maximum P change.....RPM = ',1PD14.5,' (node ',I9,')'  SUTERR.......72700
     1       /4X,'Tolerance for P....RPMAX = ',1PD14.5                   SUTERR.......72800
     1       /4X,'Maximum U change.....RUM = ',1PD14.5,' (node ',I9,')'  SUTERR.......72900
     1       /4X,'Tolerance for U....RUMAX = ',1PD14.5/)                 SUTERR.......73000
         DO 1415 I=1,50                                                  SUTERR.......73100
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......73200
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......73300
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......73400
 1415    CONTINUE                                                        SUTERR.......73500
         IF (KSCRN.EQ.1) WRITE (*,1421)                                  SUTERR.......73600
         WRITE (K00,1421)                                                SUTERR.......73700
 1421    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......73800
         DO 1425 I=1,50                                                  SUTERR.......73900
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......74000
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......74100
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......74200
 1425    CONTINUE                                                        SUTERR.......74300
      ELSE IF ((CODE(1).EQ.'REA').AND.                                   SUTERR.......74400
     1         ((CODE(2).EQ.'INP').OR.(CODE(2).EQ.'ICS'))) THEN          SUTERR.......74500
C........ERROR TYPE 'REA-INP' OR 'REA-ICS' (FORTRAN READ ERROR)          SUTERR.......74600
         IF (KSCRN.EQ.1)                                                 SUTERR.......74700
     1      WRITE (*,1888) '          FORTRAN READ ERROR          '      SUTERR.......74800
         WRITE (K00,1888) '          FORTRAN READ ERROR          '       SUTERR.......74900
         IF (KSCRN.EQ.1) WRITE (*,1511)                                  SUTERR.......75000
         WRITE (K00,1511)                                                SUTERR.......75100
 1511    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......75200
         IF (CODE(2).EQ.'INP') THEN                                      SUTERR.......75300
            CDUM80 = FNAME(1)                                            SUTERR.......75400
         ELSE                                                            SUTERR.......75500
            CDUM80 = FNAME(2)                                            SUTERR.......75600
         END IF                                                          SUTERR.......75700
         IF ((CODE(2).EQ.'ICS').AND.(CODE(3).EQ.'4')) THEN               SUTERR.......75800
           IF (KSCRN.EQ.1) WRITE (*,1513) ERRCOD, CDUM80, INERR(1)       SUTERR.......75900
           WRITE (K00,1513) ERRCOD, CDUM80, INERR(1)                     SUTERR.......76000
 1513      FORMAT (/4X,'Error code:',2X,A40                              SUTERR.......76100
     1             /4X,'File:      ',2X,A40                              SUTERR.......76200
     1            //4X,'Error status flag.....IOSTAT = ',I5/)            SUTERR.......76300
         ELSE                                                            SUTERR.......76400
           IF (KSCRN.EQ.1) WRITE (*,1514) ERRCOD, CDUM80, CODE(3)(1:3),  SUTERR.......76500
     1        INERR(1)                                                   SUTERR.......76600
           WRITE (K00,1514) ERRCOD, CDUM80, CODE(3)(1:3), INERR(1)       SUTERR.......76700
 1514      FORMAT (/4X,'Error code:',2X,A40                              SUTERR.......76800
     1             /4X,'File:      ',2X,A40                              SUTERR.......76900
     1             /4X,'Dataset:   ',2X,A3                               SUTERR.......77000
     1            //4X,'Error status flag.....IOSTAT = ',I5/)            SUTERR.......77100
         END IF                                                          SUTERR.......77200
         DO 1515 I=1,50                                                  SUTERR.......77300
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......77400
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......77500
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......77600
 1515    CONTINUE                                                        SUTERR.......77700
         IF (KSCRN.EQ.1) WRITE (*,1521)                                  SUTERR.......77800
         WRITE (K00,1521)                                                SUTERR.......77900
 1521    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......78000
         DO 1525 I=1,50                                                  SUTERR.......78100
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......78200
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......78300
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......78400
 1525    CONTINUE                                                        SUTERR.......78500
         IF (KSCRN.EQ.1) WRITE (*,1581)                                  SUTERR.......78600
         WRITE (K00,1581)                                                SUTERR.......78700
 1581    FORMAT (/1X,'GENERAL NOTE'/                                     SUTERR.......78800
     1     /4X,'If the dataset for which SUTRA has reported an error'    SUTERR.......78900
     1     /4X,'appears to be correct, check the preceding datasets'     SUTERR.......79000
     1     /4X,'for missing data or extra lines of data.')               SUTERR.......79100
      ELSE IF ((CODE(1).EQ.'REA').AND.(CODE(2).EQ.'FIL')) THEN           SUTERR.......79200
C........ERROR TYPE 'REA-FIL' (FORTRAN READ ERROR)                       SUTERR.......79300
         IF (KSCRN.EQ.1)                                                 SUTERR.......79400
     1      WRITE (*,1888) '          FORTRAN READ ERROR          '      SUTERR.......79500
         WRITE (K00,1888) '          FORTRAN READ ERROR          '       SUTERR.......79600
         IF (KSCRN.EQ.1) WRITE (*,1611)                                  SUTERR.......79700
         WRITE (K00,1611)                                                SUTERR.......79800
 1611    FORMAT (/1X,'DESCRIPTION')                                      SUTERR.......79900
         IF (KSCRN.EQ.1) WRITE (*,1613) ERRCOD, INERR(1)                 SUTERR.......80000
         WRITE (K00,1613) ERRCOD, INERR(1)                               SUTERR.......80100
 1613    FORMAT (/4X,'Error code:',2X,A40                                SUTERR.......80200
     1           /4X,'File:      ',2X,'SUTRA.FIL'                        SUTERR.......80300
     1          //4X,'Error status flag.....IOSTAT = ',I5/)              SUTERR.......80400
         DO 1615 I=1,50                                                  SUTERR.......80500
            IF (DS(I).EQ.'null_line') EXIT                               SUTERR.......80600
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') DS(I)                    SUTERR.......80700
            WRITE(K00,'(4X,A70)') DS(I)                                  SUTERR.......80800
 1615    CONTINUE                                                        SUTERR.......80900
         IF (KSCRN.EQ.1) WRITE (*,1621)                                  SUTERR.......81000
         WRITE (K00,1621)                                                SUTERR.......81100
 1621    FORMAT (/1X,'EXPLANATION'/)                                     SUTERR.......81200
         DO 1625 I=1,50                                                  SUTERR.......81300
            IF (EX(I).EQ.'null_line') EXIT                               SUTERR.......81400
            IF (KSCRN.EQ.1) WRITE(*,'(4X,A70)') EX(I)                    SUTERR.......81500
            WRITE(K00,'(4X,A70)') EX(I)                                  SUTERR.......81600
 1625    CONTINUE                                                        SUTERR.......81700
      END IF                                                             SUTERR.......81800
 1888 FORMAT (                                                           SUTERR.......81900
     1   /1X,'+--------+',38('-'),'+--------+'                           SUTERR.......82000
     1   /1X,'| \\  // |',38('-'),'| \\  // |'                           SUTERR.......82100
     1   /1X,'|  \\//  |',38(' '),'|  \\//  |'                           SUTERR.......82200
     1   /1X,'|   //   |',A38     '|   //   |'                           SUTERR.......82300
     1   /1X,'|  //\\  |',38(' '),'|  //\\  |'                           SUTERR.......82400
     1   /1X,'| //  \\ |',38('-'),'| //  \\ |'                           SUTERR.......82500
     1   /1X,'+--------+',38('-'),'+--------+')                          SUTERR.......82600
C                                                                        SUTERR.......82700
C.....WRITE RUN TERMINATION MESSAGES AND RETURN                          SUTERR.......82800
      IF (KSCRN.EQ.1) WRITE (*,8888)                                     SUTERR.......82900
      WRITE (K00,8888)                                                   SUTERR.......83000
      IF (K3.NE.-1) WRITE (K3,8889)                                      SUTERR.......83100
      IF (K5.NE.-1) WRITE (K5,8889)                                      SUTERR.......83200
      IF (K6.NE.-1) WRITE (K6,8889)                                      SUTERR.......83300
      IF (K7.NE.-1) WRITE (K7,8889)                                      SUTERR.......83400
 8888 FORMAT (/1X,'+',56('-'),'+'/1X,'| ',54X,' |'/1X,'|',3X,            SUTERR.......83500
     1   8('*'),3X,'RUN TERMINATED DUE TO ERROR',3X,9('*'),              SUTERR.......83600
     1   3X,'|'/1X,'| ',54X,' |'/1X,'+',56('-'),'+')                     SUTERR.......83700
 8889 FORMAT (//13X,'+',56('-'),'+'/13X,'| ',54X,' |'/13X,'|',3X,        SUTERR.......83800
     1   8('*'),3X,'RUN TERMINATED DUE TO ERROR',3X,9('*'),              SUTERR.......83900
     1   3X,'|'/13X,'| ',54X,' |'/13X,'+',56('-'),'+')                   SUTERR.......84000
      IF (KSCRN.EQ.1) WRITE (*,8890)                                     SUTERR.......84100
 8890 FORMAT (/' The above error message also appears in the SMY file,'  SUTERR.......84200
     1        /' which may contain additional error information.')       SUTERR.......84300
C                                                                        SUTERR.......84400
      RETURN                                                             SUTERR.......84500
      END                                                                SUTERR.......84600
C                                                                        SUTERR.......84700
C     SUBROUTINE        S  U  T  R  A              SUTRA VERSION 2D3D.1  SUTRA..........100
C                                                                        SUTRA..........200
C *** PURPOSE :                                                          SUTRA..........300
C ***  MAIN CONTROL ROUTINE FOR SUTRA SIMULATION.  ORGANIZES             SUTRA..........400
C ***  INITIALIZATION, CALCULATIONS FOR EACH TIME STEP AND ITERATION,    SUTRA..........500
C ***  AND VARIOUS OUTPUTS.  CALLS MOST OTHER SUBROUTINES.               SUTRA..........600
C                                                                        SUTRA..........700
      SUBROUTINE SUTRA(TITLE1,TITLE2,PMAT,UMAT,PITER,UITER,PM1,DPDTITR,  SUTRA..........800
     1   UM1,UM2,PVEL,SL,SR,X,Y,Z,VOL,POR,CS1,CS2,CS3,SW,DSWDP,RHO,SOP,  SUTRA..........900
     2   QIN,UIN,QUIN,QINITR,RCIT,RCITM1,PVEC,UVEC,                      SUTRA.........1000
     3   ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,VMAG,VANG1,VANG2,           SUTRA.........1100
     4   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, SUTRA.........1200
     5   PANGL1,PANGL2,PANGL3,PBC,UBC,QPLITR,GXSI,GETA,GZET,FWK,B,       SUTRA.........1300
     6   IN,IQSOP,IQSOU,IPBC,IUBC,IOBS,NREG,LREG,NBI27,IWK,IA,JA,        SUTRA.........1400
     7   IQSOPT,IQSOUT,IPBCT,IUBCT)                                      SUTRA.........1500
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                SUTRA.........1600
      CHARACTER*8 VERNUM                                                 SUTRA.........1700
      CHARACTER*1 TITLE1(80),TITLE2(80)                                  SUTRA.........1800
      CHARACTER*10 ADSMOD                                                SUTRA.........1900
      CHARACTER*80 ERRCOD,CHERR(10),FNAME(0:7),CDUM80                    SUTRA.........2000
      CHARACTER*40 SOLNAM(0:10)                                          SUTRA.........2100
      CHARACTER*10 SOLWRD(0:10)                                          SUTRA.........2200
      LOGICAL PRNALL,PRN0,PRNK3,PRNK5,PRNK6,PRNK7                        SUTRA.........2300
      LOGICAL ISERR                                                      SUTRA.........2400
      DIMENSION MIOFF(27)                                                SUTRA.........2500
      DIMENSION INERR(10),RLERR(10)                                      SUTRA.........2600
      DIMENSION PMAT(NELT,NCBI),UMAT(NELT,NCBI)                          SUTRA.........2700
      DIMENSION PITER(NN),UITER(NN),PM1(NN),DPDTITR(NN),UM1(NN),UM2(NN), SUTRA.........2800
     1   PVEL(NN),SL(NN),SR(NN),X(NN),Y(NN),Z(NN),VOL(NN),POR(NN),       SUTRA.........2900
     2   CS1(NN),CS2(NN),CS3(NN),SW(NN),DSWDP(NN),RHO(NN),SOP(NN),       SUTRA.........3000
     3   QIN(NN),QINITR(NN),UIN(NN),QUIN(NN),RCIT(NN),RCITM1(NN)         SUTRA.........3100
      DIMENSION PVEC(NNVEC),UVEC(NNVEC)                                  SUTRA.........3200
      DIMENSION ALMAX(NE),ALMIN(NE),ATMAX(NE),ATMIN(NE),VMAG(NE),        SUTRA.........3300
     1   VANG1(NE),PERMXX(NE),PERMXY(NE),PERMYX(NE),PERMYY(NE),          SUTRA.........3400
     2   PANGL1(NE)                                                      SUTRA.........3500
      DIMENSION ALMID(NEX),ATMID(NEX),                                   SUTRA.........3600
     1   VANG2(NEX),PERMXZ(NEX),PERMYZ(NEX),PERMZX(NEX),                 SUTRA.........3700
     2   PERMZY(NEX),PERMZZ(NEX),PANGL2(NEX),PANGL3(NEX)                 SUTRA.........3800
      DIMENSION PBC(NBCN),UBC(NBCN),QPLITR(NBCN)                         SUTRA.........3900
      DIMENSION GXSI(NE,N48),GETA(NE,N48),GZET(NEX,N48)                  SUTRA.........4000
      DIMENSION FWK(NWF),B(NNNX)                                         SUTRA.........4100
      DIMENSION IN(NIN),IQSOP(NSOP),IQSOU(NSOU),IPBC(NBCN),IUBC(NBCN),   SUTRA.........4200
     1   IOBS(NOBSN),NREG(NN),LREG(NE),NBI27(NBIX),IWK(NWI),             SUTRA.........4300
     2   IA(NELT),JA(NDIMJA)                                             SUTRA.........4400
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  SUTRA.........4500
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             SUTRA.........4600
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SUTRA.........4700
     1   NSOP,NSOU,NBCN                                                  SUTRA.........4800
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   SUTRA.........4900
      COMMON /DIMX2/ NELTA, NNVEC, NDIMJA                                SUTRA.........5000
      COMMON /ERRHAN/ ISERR                                              SUTRA.........5100
      COMMON /FNAMES/ FNAME                                              SUTRA.........5200
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7                        SUTRA.........5300
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      SUTRA.........5400
      COMMON /ITSOLR/ TOLP,TOLU                                          SUTRA.........5500
      COMMON /JCOLS/ NCOLPR,LCOLPR,NCOLS5,NCOLS6,J5COL,J6COL             SUTRA.........5600
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,KVEL,KBUDG,     SUTRA.........5700
     1   KSCRN,KPAUSE                                                    SUTRA.........5800
      COMMON /MODSOR/ ADSMOD                                             SUTRA.........5900
      COMMON /OBS/ NOBSN,NTOBS,NOBCYC                                    SUTRA.........6000
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      SUTRA.........6100
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        SUTRA.........6200
      COMMON /SOLVC/ SOLWRD,SOLNAM                                       SUTRA.........6300
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           SUTRA.........6400
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       SUTRA.........6500
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITMAX,TSTART                  SUTRA.........6600
      COMMON /VER/ VERNUM                                                SUTRA.........6700
C                                                                        SUTRA.........6800
C.....WRITE TITLE TO CONSOLE                                             SUTRA.........6900
      DO 100 I=80,1,-1                                                   SUTRA.........7000
         IF (TITLE1(I).NE.' ') THEN                                      SUTRA.........7100
            LENT1 = I                                                    SUTRA.........7200
            GOTO 101                                                     SUTRA.........7300
         END IF                                                          SUTRA.........7400
  100 CONTINUE                                                           SUTRA.........7500
      LENT1 = 1                                                          SUTRA.........7600
  101 DO 105 I=80,1,-1                                                   SUTRA.........7700
         IF (TITLE2(I).NE.' ') THEN                                      SUTRA.........7800
            LENT2 = I                                                    SUTRA.........7900
            GOTO 106                                                     SUTRA.........8000
         END IF                                                          SUTRA.........8100
  105 CONTINUE                                                           SUTRA.........8200
      LENT2 = 1                                                          SUTRA.........8300
  106 CONTINUE                                                           SUTRA.........8400
      IF (KSCRN.EQ.1) WRITE (*,121) VERNUM                               SUTRA.........8500
      WRITE (K00,121) VERNUM                                             SUTRA.........8600
  121 FORMAT (/1X,9X,53("=")//1X,25X,"S    U    T    R    A",//          SUTRA.........8700
     1   29X,"Version ",A8//1X,9X,53("=")/)                              SUTRA.........8800
      IF (KSCRN.EQ.1) WRITE (*,122) (TITLE1(I),I=1,LENT1)                SUTRA.........8900
      WRITE (K00,122) (TITLE1(I),I=1,LENT1)                              SUTRA.........9000
      IF (KSCRN.EQ.1) WRITE (*,122) (TITLE2(I),I=1,LENT2)                SUTRA.........9100
      WRITE (K00,122) (TITLE2(I),I=1,LENT2)                              SUTRA.........9200
  122 FORMAT (1X,80A1)                                                   SUTRA.........9300
      IF (KSCRN.EQ.1) WRITE (*,*)                                        SUTRA.........9400
      WRITE (K00,*)                                                      SUTRA.........9500
C                                                                        SUTRA.........9600
C.....COMPUTE ACTUAL NUMBER OF TIME STEPS AND DURATION BASED ON          SUTRA.........9700
C        TIME STEP CYCLING                                               SUTRA.........9800
      IF (ISSTRA.EQ.0) THEN                                              SUTRA.........9900
         TS=TSTART                                                       SUTRA........10000
         JT=0                                                            SUTRA........10100
         DELTK=DELT                                                      SUTRA........10200
  310    CONTINUE                                                        SUTRA........10300
            JT=JT+1                                                      SUTRA........10400
            IF (MOD(JT,ITCYC).EQ.0 .AND. JT.GT.1) DELTK=DELTK*DTMULT     SUTRA........10500
            IF (DELTK.GT.DTMAX) DELTK=DTMAX                              SUTRA........10600
            TS=TS+DELTK                                                  SUTRA........10700
         IF(JT.LT.ITMAX .AND. TS.LT.TMAX) GOTO 310                       SUTRA........10800
         ITMAXA = JT                                                     SUTRA........10900
         TMAXA = TS                                                      SUTRA........11000
      ELSE                                                               SUTRA........11100
         ITMAXA = ITMAX                                                  SUTRA........11200
         TMAXA = TSTART                                                  SUTRA........11300
      END IF                                                             SUTRA........11400
      TEMAXA = TMAXA - TSTART                                            SUTRA........11500
C                                                                        SUTRA........11600
C.....INITIALIZE TIME STEP NUMBER                                        SUTRA........11700
      IT=0                                                               SUTRA........11800
C                                                                        SUTRA........11900
C.....SET FLAG FOR TIME-DEPENDENT SOURCES OR BOUNDARY CONDITIONS.        SUTRA........12000
C        WHEN IBCT=+4, THERE ARE NO TIME-DEPENDENT SPECIFICATIONS.       SUTRA........12100
      IBCT=IQSOPT+IQSOUT+IPBCT+IUBCT                                     SUTRA........12200
C                                                                        SUTRA........12300
C.....SET UP POINTER ARRAYS NEEDED TO SPECIFY THE MATRIX STRUCTURE USED  SUTRA........12400
C        BY THE ITERATIVE SOLVERS.                                       SUTRA........12500
      IF (KSOLVP.NE.0) CALL PTRSET(NBI27,IA,JA,MIOFF)                    SUTRA........12600
C                                                                        SUTRA........12700
C.....SET STARTING TIME OF SIMULATION CLOCK                              SUTRA........12800
C     TSEC=TSTART                                                        SUTRA........12900
      TSECP0=TSEC                                                        SUTRA........13000
      TSECU0=TSEC                                                        SUTRA........13100
      TMIN=TSEC/60.D0                                                    SUTRA........13200
      THOUR=TMIN/60.D0                                                   SUTRA........13300
      TDAY=THOUR/24.D0                                                   SUTRA........13400
      TWEEK=TDAY/7.D0                                                    SUTRA........13500
      TMONTH=TDAY/30.4375D0                                              SUTRA........13600
      TYEAR=TDAY/365.25D0                                                SUTRA........13700
C                                                                        SUTRA........13800
C.....OUTPUT INITIAL CONDITIONS OR STARTING CONDITIONS                   SUTRA........13900
      IF(ISSTRA.NE.1) THEN                                               SUTRA........14000
         IF (IABS(KTYPE).EQ.3) THEN                                      SUTRA........14100
            CALL OUTLST3(0,0,0,0,0,0d0,0,0,0d0,PVEC,UVEC,VMAG,VANG1,     SUTRA........14200
     1         VANG2,SW)                                                 SUTRA........14300
         ELSE                                                            SUTRA........14400
            CALL OUTLST2(0,0,0,0,0,0d0,0,0,0d0,PVEC,UVEC,VMAG,VANG1,SW)  SUTRA........14500
         END IF                                                          SUTRA........14600
         IF (ISSFLO.EQ.0) THEN                                           SUTRA........14700
            IF (K5.NE.-1)                                                SUTRA........14800
     1         CALL OUTNOD(PVEC,UVEC,SW,IN,X,Y,Z,TITLE1,TITLE2)          SUTRA........14900
            IF (K7.NE.-1)                                                SUTRA........15000
     1         CALL OUTOBS(IOBS,X,Y,Z,PVEC,UVEC,SW,TITLE1,TITLE2)        SUTRA........15100
         END IF                                                          SUTRA........15200
      END IF                                                             SUTRA........15300
C                                                                        SUTRA........15400
C.....SET SWITCHES AND PARAMETERS FOR SOLUTION WITH STEADY-STATE FLOW    SUTRA........15500
      IF(ISSFLO.NE.1) GOTO 1000                                          SUTRA........15600
      ML=1                                                               SUTRA........15700
      NOUMAT=0                                                           SUTRA........15800
      ISSFLO=2                                                           SUTRA........15900
      ITER=0                                                             SUTRA........16000
      DLTPM1=DELTP                                                       SUTRA........16100
      DLTUM1=DELTU                                                       SUTRA........16200
      BDELP1 = 1D0                                                       SUTRA........16300
      BDELP=0.0D0                                                        SUTRA........16400
      BDELU=0.0D0                                                        SUTRA........16500
      IF (ISSTRA.NE.0) THEN                                              SUTRA........16600
         IF (KSCRN.EQ.1) WRITE (*,902) IT, ITMAXA                        SUTRA........16700
         WRITE (K00,902) IT, ITMAXA                                      SUTRA........16800
      ELSE                                                               SUTRA........16900
         TELAPS = TSEC - TSTART                                          SUTRA........17000
         IF (KSCRN.EQ.1) WRITE (*,903) IT, ITMAXA, TELAPS, TEMAXA        SUTRA........17100
         WRITE (K00,903) IT, ITMAXA, TSEC, TEMAXA                        SUTRA........17200
      END IF                                                             SUTRA........17300
  902 FORMAT (1X, 'TIME STEP ', I5, ' OF ', I5)                          SUTRA........17400
  903 FORMAT (1X, 'TIME STEP ', I5, ' OF ', I5, ';'                      SUTRA........17500
     1        4X, 'ELAPSED TIME: ', 1PD11.4, ' OF ', 1PD11.4, ' [s]')    SUTRA........17600
      GOTO 1100                                                          SUTRA........17700
C                                                                        SUTRA........17800
C                                                                        SUTRA........17900
C ********************************************************************** SUTRA........18000
C.....BEGIN TIME STEP ************************************************** SUTRA........18100
C ********************************************************************** SUTRA........18200
C.....INCREMENT TIME STEP NUMBER                                         SUTRA........18300
 1000 IT=IT+1                                                            SUTRA........18400
      ITER=0                                                             SUTRA........18500
      ML=0                                                               SUTRA........18600
      NOUMAT=0                                                           SUTRA........18700
C.....SET NOUMAT TO OBTAIN U SOLUTION BY SIMPLE BACK SUBSTITUTION        SUTRA........18800
C        BEGINNING ON SECOND TIME STEP AFTER A PRESSURE SOLUTION         SUTRA........18900
C        IF THE SOLUTION IS NON-ITERATIVE (ITRMAX=1)                     SUTRA........19000
      IF(MOD(IT-1,NPCYC).NE.0.AND.MOD(IT,NPCYC).NE.0.AND.IT.GT.2         SUTRA........19100
     1   .AND.ITRMAX.EQ.1) NOUMAT=1                                      SUTRA........19200
C.....CHOOSE SOLUTION VARIABLE ON THIS TIME STEP:                        SUTRA........19300
C        ML=0 FOR P AND U, ML=1 FOR P ONLY, AND ML=2 FOR U ONLY.         SUTRA........19400
      IF(IT.EQ.1.AND.ISSFLO.NE.2) GOTO 1005                              SUTRA........19500
      IF(MOD(IT,NPCYC).NE.0) ML=2                                        SUTRA........19600
      IF(MOD(IT,NUCYC).NE.0) ML=1                                        SUTRA........19700
C.....MULTIPLY TIME STEP SIZE BY DTMULT EACH ITCYC TIME STEPS            SUTRA........19800
      DELTM1=DELT                                                        SUTRA........19900
      IF(MOD(IT,ITCYC).EQ.0.AND.IT.GT.1) THEN                            SUTRA........20000
         DELT=DELT*DTMULT                                                SUTRA........20100
C........SET TIME STEP SIZE TO MAXIMUM ALLOWED SIZE, DTMAX               SUTRA........20200
         IF(DELT.GT.DTMAX) DELT=DTMAX                                    SUTRA........20300
      END IF                                                             SUTRA........20400
C.....NO SIMPLE BACK SUBSTITUTION FOR U IF TIME STEP HAS CHANGED         SUTRA........20500
      IF(DELT.NE.DELTM1) NOUMAT=0                                        SUTRA........20600
C.....INCREMENT SIMULATION CLOCK, TSEC, TO END OF NEW TIME STEP          SUTRA........20700
 1005 TSEC=TSEC+DELT                                                     SUTRA........20800
      TMIN=TSEC/60.D0                                                    SUTRA........20900
      THOUR=TMIN/60.D0                                                   SUTRA........21000
      TDAY=THOUR/24.D0                                                   SUTRA........21100
      TWEEK=TDAY/7.D0                                                    SUTRA........21200
      TMONTH=TDAY/30.4375D0                                              SUTRA........21300
      TYEAR=TDAY/365.25D0                                                SUTRA........21400
C                                                                        SUTRA........21500
C.....WRITE TIME STEP NUMBER AND ELAPSED TIME                            SUTRA........21600
      IF (ISSTRA.NE.0) THEN                                              SUTRA........21700
         IF (KSCRN.EQ.1) WRITE (*,902) IT, ITMAXA                        SUTRA........21800
         WRITE (K00,902) IT, ITMAXA                                      SUTRA........21900
      ELSE                                                               SUTRA........22000
         TELAPS = TSEC - TSTART                                          SUTRA........22100
         IF (KSCRN.EQ.1) WRITE (*,903) IT, ITMAXA, TELAPS, TEMAXA        SUTRA........22200
         WRITE (K00,903) IT, ITMAXA, TSEC, TEMAXA                        SUTRA........22300
      END IF                                                             SUTRA........22400
C                                                                        SUTRA........22500
C.....SET TIME STEP (DELTP AND/OR DELTU) AND INCREMENT CLOCK             SUTRA........22600
C        FOR WHICHEVER OF P AND/OR U ARE SOLVED FOR ON THIS TIME STEP    SUTRA........22700
      IF(ML-1) 1010,1020,1030                                            SUTRA........22800
 1010 DLTPM1=DELTP                                                       SUTRA........22900
      DLTUM1=DELTU                                                       SUTRA........23000
      DELTP=TSEC-TSECP0                                                  SUTRA........23100
      DELTU=TSEC-TSECU0                                                  SUTRA........23200
      TSECP0=TSEC                                                        SUTRA........23300
      TSECU0=TSEC                                                        SUTRA........23400
      GOTO 1040                                                          SUTRA........23500
 1020 DLTPM1=DELTP                                                       SUTRA........23600
      DELTP=TSEC-TSECP0                                                  SUTRA........23700
      TSECP0=TSEC                                                        SUTRA........23800
      GOTO 1040                                                          SUTRA........23900
 1030 DLTUM1=DELTU                                                       SUTRA........24000
      DELTU=TSEC-TSECU0                                                  SUTRA........24100
      TSECU0=TSEC                                                        SUTRA........24200
 1040 CONTINUE                                                           SUTRA........24300
C.....SET PROJECTION FACTORS USED ON FIRST ITERATION TO EXTRAPOLATE      SUTRA........24400
C        AHEAD ONE-HALF TIME STEP                                        SUTRA........24500
      BDELP=(DELTP/DLTPM1)*0.50D0                                        SUTRA........24600
      BDELU=(DELTU/DLTUM1)*0.50D0                                        SUTRA........24700
      BDELP1=BDELP+1.0D0                                                 SUTRA........24800
      BDELU1=BDELU+1.0D0                                                 SUTRA........24900
C                                                                        SUTRA........25000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........25100
C.....BEGIN ITERATION - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........25200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........25300
C.....INCREMENT ITERATION NUMBER                                         SUTRA........25400
 1100 ITER=ITER+1                                                        SUTRA........25500
C.....IF ITERATIVE SOLUTION, WRITE ITERATION NUMBER                      SUTRA........25600
      IF (ITRMAX.NE.1) THEN                                              SUTRA........25700
         IF (KSCRN.EQ.1) WRITE (*,1104) ITER                             SUTRA........25800
         WRITE (K00,1104) ITER                                           SUTRA........25900
      END IF                                                             SUTRA........26000
 1104 FORMAT (1X, 3X, 'NON-LINEARITY ITERATION ', I5)                    SUTRA........26100
C                                                                        SUTRA........26200
      IF(ML-1) 2000,2200,2400                                            SUTRA........26300
C.....SHIFT AND SET VECTORS FOR TIME STEP WITH BOTH P AND U SOLUTIONS    SUTRA........26400
 2000 DO 2025 I=1,NN                                                     SUTRA........26500
C.....SET DPDT-ITERATE TO VALUE FROM PREVIOUS ITERATION FOR PRESSURE     SUTRA........26600
C      COMING FROM THIS TIME STEP                                        SUTRA........26700
C       (THIS IS OVERWRITTEN ON THE FIRST ITERATION JUST BELOW)          SUTRA........26800
C     NOTE: DPDTITR IS USED ONLY IN THE BUDGET                           SUTRA........26900
      DPDTITR(I)=(PVEC(I)-PM1(I))/DELTP                                  SUTRA........27000
      PITER(I)=PVEC(I)                                                   SUTRA........27100
      PVEL(I)=PVEC(I)                                                    SUTRA........27200
      UITER(I)=UVEC(I)                                                   SUTRA........27300
      RCITM1(I)=RCIT(I)                                                  SUTRA........27400
 2025 RCIT(I)=RHOW0+DRWDU*(UITER(I)-URHOW0)                              SUTRA........27500
      DO 2050 IP=1,NPBC                                                  SUTRA........27600
      I=IABS(IPBC(IP))                                                   SUTRA........27700
      QPLITR(IP)=GNUP*(PBC(IP)-PITER(I))                                 SUTRA........27800
 2050 CONTINUE                                                           SUTRA........27900
C.....QINITR VALUE DIFFERS FROM QIN ONLY IF BCTIME CHANGED QIN           SUTRA........28000
      IF (ITER.LE.2) THEN                                                SUTRA........28100
         DO 2060 I=1,NN                                                  SUTRA........28200
 2060    QINITR(I)=QIN(I)                                                SUTRA........28300
      END IF                                                             SUTRA........28400
      IF(ITER.GT.1) GOTO 2600                                            SUTRA........28500
      DO 2075 I=1,NN                                                     SUTRA........28600
      PITER(I)=BDELP1*PVEC(I)-BDELP*PM1(I)                               SUTRA........28700
      UITER(I)=BDELU1*UVEC(I)-BDELU*UM1(I)                               SUTRA........28800
C.....RESETS DPDT-ITERATE TO VALUE FROM MOST RECENT PRESSURE TIME STEP   SUTRA........28900
C      ON THE FIRST ITERATION FOR THIS TIME STEP                         SUTRA........29000
      DPDTITR(I)=(PVEC(I)-PM1(I))/DLTPM1                                 SUTRA........29100
      PM1(I)=PVEC(I)                                                     SUTRA........29200
      UM2(I)=UM1(I)                                                      SUTRA........29300
 2075 UM1(I)=UVEC(I)                                                     SUTRA........29400
      GOTO 2600                                                          SUTRA........29500
C.....SHIFT AND SET VECTORS FOR TIME STEP WITH P SOLUTION ONLY           SUTRA........29600
 2200 DO 2225 I=1,NN                                                     SUTRA........29700
      PVEL(I)=PVEC(I)                                                    SUTRA........29800
 2225 PITER(I)=PVEC(I)                                                   SUTRA........29900
      IF(ITER.GT.1) GOTO 2600                                            SUTRA........30000
      DO 2250 I=1,NN                                                     SUTRA........30100
      PITER(I)=BDELP1*PVEC(I)-BDELP*PM1(I)                               SUTRA........30200
      UITER(I)=UVEC(I)                                                   SUTRA........30300
      RCITM1(I)=RCIT(I)                                                  SUTRA........30400
      RCIT(I)=RHOW0+DRWDU*(UITER(I)-URHOW0)                              SUTRA........30500
 2250 PM1(I)=PVEC(I)                                                     SUTRA........30600
      GOTO 2600                                                          SUTRA........30700
C.....SHIFT AND SET VECTORS FOR TIME STEP WITH U SOLUTION ONLY           SUTRA........30800
 2400 IF (ITER.EQ.1) THEN                                                SUTRA........30900
         DO 2405 I=1,NN                                                  SUTRA........31000
 2405       UITER(I)=BDELU1*UVEC(I)-BDELU*UM1(I)                         SUTRA........31100
      ELSE                                                               SUTRA........31200
         DO 2410 I=1,NN                                                  SUTRA........31300
 2410       UITER(I)=UVEC(I)                                             SUTRA........31400
      END IF                                                             SUTRA........31500
      IF(NOUMAT.EQ.1) GOTO 2480                                          SUTRA........31600
C.....SET PARAMETERS FROM MOST RECENT PRESSURE TIME STEP                 SUTRA........31700
      IF(ITER.GT.1) GOTO 2600                                            SUTRA........31800
      DO 2450 I=1,NN                                                     SUTRA........31900
      DPDTITR(I)=(PVEC(I)-PM1(I))/DELTP                                  SUTRA........32000
      QINITR(I)=QIN(I)                                                   SUTRA........32100
      PITER(I)=PVEC(I)                                                   SUTRA........32200
      PVEL(I)=PVEC(I)                                                    SUTRA........32300
 2450 RCITM1(I)=RCIT(I)                                                  SUTRA........32400
      DO 2475 IP=1,NPBC                                                  SUTRA........32500
      I=IABS(IPBC(IP))                                                   SUTRA........32600
      QPLITR(IP)=GNUP*(PBC(IP)-PITER(I))                                 SUTRA........32700
 2475 CONTINUE                                                           SUTRA........32800
 2480 DO 2500 I=1,NN                                                     SUTRA........32900
      UM2(I)=UM1(I)                                                      SUTRA........33000
 2500 UM1(I)=UVEC(I)                                                     SUTRA........33100
 2600 CONTINUE                                                           SUTRA........33200
C                                                                        SUTRA........33300
C.....INITIALIZE ARRAYS WITH VALUE OF ZERO                               SUTRA........33400
      MATDIM=NELT*NCBI                                                   SUTRA........33500
      IF(ML-1) 3000,3000,3300                                            SUTRA........33600
 3000 CALL ZERO(PMAT,MATDIM,0.0D0)                                       SUTRA........33700
      CALL ZERO(PVEC,NNVEC,0.0D0)                                        SUTRA........33800
      CALL ZERO(VOL,NN,0.0D0)                                            SUTRA........33900
      IF(ML-1) 3300,3400,3300                                            SUTRA........34000
 3300 IF(NOUMAT) 3350,3350,3375                                          SUTRA........34100
 3350 CALL ZERO(UMAT,MATDIM,0.0D0)                                       SUTRA........34200
 3375 CALL ZERO(UVEC,NNVEC,0.0D0)                                        SUTRA........34300
 3400 CONTINUE                                                           SUTRA........34400
C                                                                        SUTRA........34500
C.....SET TIME-DEPENDENT BOUNDARY CONDITIONS, SOURCES AND SINKS          SUTRA........34600
C        FOR THIS TIME STEP                                              SUTRA........34700
      IF(ITER.EQ.1.AND.IBCT.NE.4)                                        SUTRA........34800
     1   CALL BCTIME(IPBC,PBC,IUBC,UBC,QIN,UIN,QUIN,IQSOP,IQSOU,         SUTRA........34900
     2   IPBCT,IUBCT,IQSOPT,IQSOUT,X,Y,Z)                                SUTRA........35000
C                                                                        SUTRA........35100
C.....SET SORPTION PARAMETERS FOR THIS TIME STEP                         SUTRA........35200
      IF(ML.NE.1.AND.ME.EQ.-1.AND.NOUMAT.EQ.0.AND.                       SUTRA........35300
     1   ADSMOD.NE.'NONE      ') CALL ADSORB(CS1,CS2,CS3,SL,SR,UITER)    SUTRA........35400
C                                                                        SUTRA........35500
C.....DO ELEMENTWISE CALCULATIONS IN MATRIX EQUATION FOR P AND/OR U      SUTRA........35600
      IF (NOUMAT.EQ.0) THEN                                              SUTRA........35700
       IF (IABS(KTYPE).EQ.3) THEN                                        SUTRA........35800
C..... 3D PROBLEM                                                        SUTRA........35900
       CALL ELEMN3(ML,IN,X,Y,Z,PITER,UITER,RCIT,RCITM1,POR,              SUTRA........36000
     2   ALMAX,ALMID,ALMIN,ATMAX,ATMID,ATMIN,                            SUTRA........36100
     3   PERMXX,PERMXY,PERMXZ,PERMYX,PERMYY,PERMYZ,PERMZX,PERMZY,PERMZZ, SUTRA........36200
     4   PANGL1,PANGL2,PANGL3,VMAG,VANG1,VANG2,VOL,PMAT,PVEC,            SUTRA........36300
     5   UMAT,UVEC,GXSI,GETA,GZET,PVEL,LREG,NBI27,MIOFF)                 SUTRA........36400
       IF (ISERR) RETURN                                                 SUTRA........36500
       ELSE                                                              SUTRA........36600
C..... 2D PROBLEM                                                        SUTRA........36700
       CALL ELEMN2(ML,IN,X,Y,Z,PITER,UITER,RCIT,RCITM1,POR,              SUTRA........36800
     2   ALMAX,ALMIN,ATMAX,ATMIN,PERMXX,PERMXY,PERMYX,PERMYY,PANGL1,     SUTRA........36900
     3   VMAG,VANG1,VOL,PMAT,PVEC,UMAT,UVEC,GXSI,GETA,PVEL,LREG,         SUTRA........37000
     4   NBI27,MIOFF)                                                    SUTRA........37100
       IF (ISERR) RETURN                                                 SUTRA........37200
       END IF                                                            SUTRA........37300
      END IF                                                             SUTRA........37400
C                                                                        SUTRA........37500
C.....DO NODEWISE CALCULATIONS IN MATRIX EQUATION FOR P AND/OR U         SUTRA........37600
      CALL NODAL(ML,VOL,PMAT,PVEC,UMAT,UVEC,PITER,UITER,PM1,UM1,UM2,     SUTRA........37700
     1   POR,QIN,UIN,QUIN,QINITR,CS1,CS2,CS3,SL,SR,SW,DSWDP,RHO,SOP,     SUTRA........37800
     2   NREG,MIOFF)                                                     SUTRA........37900
C                                                                        SUTRA........38000
C.....SET SPECIFIED P AND U CONDITIONS IN MATRIX EQUATION FOR P AND/OR U SUTRA........38100
      CALL BC(ML,PMAT,PVEC,UMAT,UVEC,IPBC,PBC,IUBC,UBC,QPLITR,MIOFF)     SUTRA........38200
C                                                                        SUTRA........38300
C.....MATRIX EQUATION FOR P AND/OR U COMPLETE.  SOLVE EQUATIONS:         SUTRA........38400
C        WITH DIRECT SOLVER,                                             SUTRA........38500
C           WHEN KMT=0, DECOMPOSE AND BACK-SUBSTITUTE,                   SUTRA........38600
C           WHEN KMT=2, BACK-SUBSTITUTE ONLY.                            SUTRA........38700
C        WITH ITERATIVE SOLVER,                                          SUTRA........38800
C           WHEN KMT=0, RESET MATRIX POINTERS TO "TRIAD" FORMAT,         SUTRA........38900
C           WHEN KMT=2, LEAVE MATRIX POINTERS IN "COLUMN" FORMAT.        SUTRA........39000
C        KPU=1 WHEN SOLVING FOR P,                                       SUTRA........39100
C        KPU=2 WHEN SOLVING FOR U.                                       SUTRA........39200
      IHALFB=NBHALF-1                                                    SUTRA........39300
      IERRP = 0                                                          SUTRA........39400
      IERRU = 0                                                          SUTRA........39500
      IF(ML-1) 5000,5000,5500                                            SUTRA........39600
C                                                                        SUTRA........39700
C.....SOLVE FOR P                                                        SUTRA........39800
 5000 KMT=000000                                                         SUTRA........39900
      KPU=1                                                              SUTRA........40000
      KSOLVR = KSOLVP                                                    SUTRA........40100
      CALL SOLVER(KMT,KPU,KSOLVR,PMAT,PVEC,PITER,B,NN,IHALFB,NELT,NCBI,  SUTRA........40200
     1            IWK,FWK,IA,JA,IERRP,ITRSP,ERRP)                        SUTRA........40300
C.....P SOLUTION NOW IN PVEC                                             SUTRA........40400
      IF(ML-1) 5500,6000,5500                                            SUTRA........40500
C                                                                        SUTRA........40600
C.....SOLVE FOR U                                                        SUTRA........40700
 5500 KMT=000000                                                         SUTRA........40800
      KPU=2                                                              SUTRA........40900
      IF(NOUMAT) 5700,5700,5600                                          SUTRA........41000
 5600 KMT=2                                                              SUTRA........41100
 5700 KSOLVR = KSOLVU                                                    SUTRA........41200
      CALL SOLVER(KMT,KPU,KSOLVR,UMAT,UVEC,UITER,B,NN,IHALFB,NELT,NCBI,  SUTRA........41300
     1            IWK,FWK,IA,JA,IERRU,ITRSU,ERRU)                        SUTRA........41400
 6000 CONTINUE                                                           SUTRA........41500
C.....U SOLUTION NOW IN UVEC                                             SUTRA........41600
C                                                                        SUTRA........41700
      IERR = IABS(IERRP) + IABS(IERRU)                                   SUTRA........41800
C                                                                        SUTRA........41900
C.....CHECK PROGRESS AND CONVERGENCE OF NON-LINEARITY ITERATIONS         SUTRA........42000
C        AND SET STOP AND GO FLAGS:                                      SUTRA........42100
C           ISTOP = -1   NOT CONVERGED - STOP SIMULATION                 SUTRA........42200
C           ISTOP =  0   ITERATIONS LEFT OR CONVERGED - KEEP SIMULATING  SUTRA........42300
C           ISTOP =  1   LAST TIME STEP REACHED - STOP SIMULATION        SUTRA........42400
C           ISTOP =  2   MAXIMUM TIME REACHED - STOP SIMULATION          SUTRA........42500
C           IGOI = 0   P AND U CONVERGED, OR NO ITERATIONS DONE          SUTRA........42600
C           IGOI = 1   ONLY P HAS NOT YET CONVERGED TO CRITERION         SUTRA........42700
C           IGOI = 2   ONLY U HAS NOT YET CONVERGED TO CRITERION         SUTRA........42800
C           IGOI = 3   BOTH P AND U HAVE NOT YET CONVERGED TO CRITERIA   SUTRA........42900
      ISTOP=0                                                            SUTRA........43000
      IGOI=0                                                             SUTRA........43100
      IF(ITRMAX-1) 7500,7500,7000                                        SUTRA........43200
 7000 RPM=0.D0                                                           SUTRA........43300
      RUM=0.D0                                                           SUTRA........43400
      IPWORS=0                                                           SUTRA........43500
      IUWORS=0                                                           SUTRA........43600
      IF(ML-1) 7050,7050,7150                                            SUTRA........43700
 7050 DO 7100 I=1,NN                                                     SUTRA........43800
      RP=DABS(PVEC(I)-PITER(I))                                          SUTRA........43900
      IF(RP-RPM) 7100,7060,7060                                          SUTRA........44000
 7060 RPM=RP                                                             SUTRA........44100
      IPWORS=I                                                           SUTRA........44200
 7100 CONTINUE                                                           SUTRA........44300
      IF(RPM.GT.RPMAX) IGOI=IGOI+1                                       SUTRA........44400
 7150 IF(ML-1) 7200,7350,7200                                            SUTRA........44500
 7200 DO 7300 I=1,NN                                                     SUTRA........44600
      RU=DABS(UVEC(I)-UITER(I))                                          SUTRA........44700
      IF(RU-RUM) 7300,7260,7260                                          SUTRA........44800
 7260 RUM=RU                                                             SUTRA........44900
      IUWORS=I                                                           SUTRA........45000
 7300 CONTINUE                                                           SUTRA........45100
      IF(RUM.GT.RUMAX) IGOI=IGOI+2                                       SUTRA........45200
 7350 CONTINUE                                                           SUTRA........45300
      IF (KSCRN.EQ.1) WRITE (*,7377) RPM, RUM                            SUTRA........45400
      WRITE (K00,7377) RPM, RUM                                          SUTRA........45500
 7377 FORMAT (1X, 6X, 'Maximum changes in P, U: ',1PE8.1,", ",1PE8.1)    SUTRA........45600
      IF(IGOI.GT.0.AND.ITER.EQ.ITRMAX) ISTOP=-1                          SUTRA........45700
      IF(IGOI.GT.0.AND.ISTOP.EQ.0.AND.IERR.EQ.0) GOTO 1100               SUTRA........45800
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........45900
C.....END ITERATION - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........46000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  SUTRA........46100
C                                                                        SUTRA........46200
 7500 CONTINUE                                                           SUTRA........46300
      IF(ISTOP.NE.-1.AND.IT.EQ.ITMAX) ISTOP=1                            SUTRA........46400
      IF(ISTOP.NE.-1.AND.TSEC.GE.TMAX.AND.ISSTRA.NE.1) ISTOP=2           SUTRA........46500
C                                                                        SUTRA........46600
C.....OUTPUT RESULTS FOR TIME STEP IN ACCORDANCE WITH PRINT CYCLES       SUTRA........46700
      PRNALL = ((ISTOP.NE.0).OR.(IERR.NE.0))                             SUTRA........46800
      PRN0 = ((IT.EQ.0).AND.(ISSFLO.NE.0).AND.(ISSTRA.NE.1))             SUTRA........46900
      PRNK3 = (PRNALL.OR.PRN0.OR.(MOD(IT,NPRINT).EQ.0)                   SUTRA........47000
     1         .OR.((IT.EQ.1).AND.(NPRINT.GT.0)))                        SUTRA........47100
      PRNK5 = ((PRNALL.OR.PRN0.OR.((IT.NE.0).AND.(MOD(IT,NCOLPR).EQ.0))  SUTRA........47200
     1         .OR.((IT.EQ.1).AND.(NCOLPR.GT.0))).AND.(K5.NE.-1))        SUTRA........47300
      PRNK6 = ((PRNALL.OR.((IT.NE.0).AND.(MOD(IT,LCOLPR).EQ.0))          SUTRA........47400
     1         .OR.(IT.EQ.1)).AND.(K6.NE.-1))                            SUTRA........47500
      PRNK7 = ((PRNALL.OR.PRN0.OR.((IT.NE.0).AND.(MOD(IT,NOBCYC).EQ.0))  SUTRA........47600
     1         .OR.((IT.EQ.1).AND.(NOBCYC.GT.0))).AND.(K7.NE.-1))        SUTRA........47700
      IF (PRNK3) THEN                                                    SUTRA........47800
      IF (IABS(KTYPE).EQ.3) THEN                                         SUTRA........47900
         CALL OUTLST3(ML,ISTOP,IGOI,IERRP,ITRSP,ERRP,IERRU,ITRSU,ERRU,   SUTRA........48000
     1      PVEC,UVEC,VMAG,VANG1,VANG2,SW)                               SUTRA........48100
      ELSE                                                               SUTRA........48200
         CALL OUTLST2(ML,ISTOP,IGOI,IERRP,ITRSP,ERRP,IERRU,ITRSU,ERRU,   SUTRA........48300
     1      PVEC,UVEC,VMAG,VANG1,SW)                                     SUTRA........48400
      END IF                                                             SUTRA........48500
C.....CALCULATE AND PRINT FLUID MASS AND/OR ENERGY OR SOLUTE MASS BUDGET SUTRA........48600
      IF(KBUDG.EQ.1)                                                     SUTRA........48700
     1   CALL BUDGET(ML,IBCT,VOL,SW,DSWDP,RHO,SOP,QIN,PVEC,PM1,DPDTITR,  SUTRA........48800
     2      PBC,QPLITR,IPBC,IQSOP,POR,UVEC,UM1,UM2,UIN,QUIN,QINITR,      SUTRA........48900
     3      IQSOU,UBC,IUBC,CS1,CS2,CS3,SL,SR,NREG)                       SUTRA........49000
      END IF                                                             SUTRA........49100
C.....PRINT TO COLUMNWISE OUTPUT FILES AND OBSERVATION FILE              SUTRA........49200
      IF (PRNK5) CALL OUTNOD(PVEC,UVEC,SW,IN,X,Y,Z,TITLE1,TITLE2)        SUTRA........49300
      IF (PRNK6) CALL OUTELE(VMAG,VANG1,VANG2,IN,X,Y,Z,TITLE1,TITLE2)    SUTRA........49400
      IF (PRNK7) CALL OUTOBS(IOBS,X,Y,Z,PVEC,UVEC,SW,TITLE1,TITLE2)      SUTRA........49500
C                                                                        SUTRA........49600
C.....STORE RESULTS FOR POSSIBLE RESTART OF SIMULATION EACH              SUTRA........49700
C        ISTORE TIME STEPS AND AFTER LAST TIME STEP, THEN GO             SUTRA........49800
C        TO NEXT TIME STEP                                               SUTRA........49900
      IF (IERR.EQ.0) THEN                                                SUTRA........50000
         IF ((ISTORE.NE.0).AND.((ISTOP.NE.0).OR.(MOD(IT,ISTORE).EQ.0)))  SUTRA........50100
     1      CALL OUTRST(PVEC,UVEC,PM1,UM1,CS1,RCIT,SW,QINITR,PBC)        SUTRA........50200
         IF (ISTOP.EQ.0) GOTO 1000                                       SUTRA........50300
      END IF                                                             SUTRA........50400
C                                                                        SUTRA........50500
C ********************************************************************** SUTRA........50600
C.....END TIME STEP **************************************************** SUTRA........50700
C ********************************************************************** SUTRA........50800
C                                                                        SUTRA........50900
C                                                                        SUTRA........51000
C.....COMPLETE OUTPUT AND TERMINATE SIMULATION                           SUTRA........51100
      IF (IERRP.NE.0) THEN                                               SUTRA........51200
         ERRCOD = 'SOL-1'                                                SUTRA........51300
         CHERR(1) = 'P'                                                  SUTRA........51400
         CHERR(2) = SOLWRD(KSOLVP)                                       SUTRA........51500
         INERR(1) = IERRP                                                SUTRA........51600
         INERR(2) = ITRSP                                                SUTRA........51700
         RLERR(1) = ERRP                                                 SUTRA........51800
         RLERR(2) = TOLP                                                 SUTRA........51900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA........52000
         RETURN                                                          SUTRA........52100
      ELSE IF (IERRU.NE.0) THEN                                          SUTRA........52200
         ERRCOD = 'SOL-1'                                                SUTRA........52300
         CHERR(1) = 'U'                                                  SUTRA........52400
         CHERR(2) = SOLWRD(KSOLVU)                                       SUTRA........52500
         INERR(1) = IERRU                                                SUTRA........52600
         INERR(2) = ITRSU                                                SUTRA........52700
         RLERR(1) = ERRU                                                 SUTRA........52800
         RLERR(2) = TOLU                                                 SUTRA........52900
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA........53000
         RETURN                                                          SUTRA........53100
      END IF                                                             SUTRA........53200
C                                                                        SUTRA........53300
      IF(ISTORE.GT.0) WRITE(K3,8100)                                     SUTRA........53400
 8100 FORMAT(//////11X,'*** LAST SOLUTION HAS BEEN STORED ',             SUTRA........53500
     1   'IN THE RESTART DATA FILE ***')                                 SUTRA........53600
C                                                                        SUTRA........53700
C.....OUTPUT END OF SIMULATION MESSAGE AND RETURN TO MAIN FOR STOP       SUTRA........53800
      IF(ISTOP.EQ.-1) THEN                                               SUTRA........53900
         ERRCOD = 'CON-1'                                                SUTRA........54000
         IF (ME.EQ.1) THEN                                               SUTRA........54100
            CDUM80 = 'temperature'                                       SUTRA........54200
            LENC = 11                                                    SUTRA........54300
         ELSE                                                            SUTRA........54400
            CDUM80 = 'concentration'                                     SUTRA........54500
            LENC = 13                                                    SUTRA........54600
         END IF                                                          SUTRA........54700
         IF (IGOI.EQ.1) THEN                                             SUTRA........54800
            CHERR(1) = 'pressure'                                        SUTRA........54900
            LENC = 8                                                     SUTRA........55000
         ELSE IF (IGOI.EQ.2) THEN                                        SUTRA........55100
            CHERR(1) = CDUM80                                            SUTRA........55200
         ELSE IF (IGOI.EQ.3) THEN                                        SUTRA........55300
            CHERR(1) = 'pressure and ' // CDUM80(1:LENC)                 SUTRA........55400
            LENC = 13 + LENC                                             SUTRA........55500
         END IF                                                          SUTRA........55600
         INERR(1) = IPWORS                                               SUTRA........55700
         INERR(2) = IUWORS                                               SUTRA........55800
         INERR(3) = ITER                                                 SUTRA........55900
         INERR(4) = LENC                                                 SUTRA........56000
         RLERR(1) = RPM                                                  SUTRA........56100
         RLERR(2) = RPMAX                                                SUTRA........56200
         RLERR(3) = RUM                                                  SUTRA........56300
         RLERR(4) = RUMAX                                                SUTRA........56400
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        SUTRA........56500
         RETURN                                                          SUTRA........56600
      ELSE IF (ISTOP.EQ.2) THEN                                          SUTRA........56700
         WRITE(K3,8450)                                                  SUTRA........56800
 8450    FORMAT(////////11X,'SUTRA SIMULATION TERMINATED AT',            SUTRA........56900
     1      ' COMPLETION OF TIME PERIOD'/                                SUTRA........57000
     2                  11X,'***** ********** ********** **',            SUTRA........57100
     3      ' ********** ** **** ******')                                SUTRA........57200
      ELSE                                                               SUTRA........57300
         WRITE(K3,8550)                                                  SUTRA........57400
 8550    FORMAT(////////11X,'SUTRA SIMULATION TERMINATED AT',            SUTRA........57500
     1      ' COMPLETION OF TIME STEPS'/                                 SUTRA........57600
     2                  11X,'***** ********** ********** **',            SUTRA........57700
     3      ' ********** ** **** *****')                                 SUTRA........57800
      END IF                                                             SUTRA........57900
C                                                                        SUTRA........58000
      IF (KSCRN.EQ.1) WRITE(*,8590)                                      SUTRA........58100
      WRITE(K00,8590)                                                    SUTRA........58200
 8590 FORMAT(/1X,'S I M U L A T I O N   E N D E D'/)                     SUTRA........58300
      RETURN                                                             SUTRA........58400
C                                                                        SUTRA........58500
      END                                                                SUTRA........58600
C                                                                        SUTRA........58700
C     SUBROUTINE        T  E  N  S  Y  M           SUTRA VERSION 2D3D.1  TENSYM.........100
C                                                                        TENSYM.........200
C *** PURPOSE :                                                          TENSYM.........300
C ***  TO TRANSFORM A DIAGONAL MATRIX TO A NEW COORDINATE SYSTEM.        TENSYM.........400
C ***  [T] IS THE DIAGONAL MATRIX EXPRESSED IN THE FIRST (INPUT)         TENSYM.........500
C ***  COORDINATE SYSTEM; [P] IS THE (SYMMETRIC) MATRIX EXPRESSED        TENSYM.........600
C ***  IN THE SECOND (OUTPUT) COORDINATE SYSTEM; AND [Q] IS THE          TENSYM.........700
C ***  THE TRANSFORMATION MATRIX.                                        TENSYM.........800
C                                                                        TENSYM.........900
      SUBROUTINE TENSYM(T11,T22,T33,Q11,Q12,Q13,Q21,Q22,Q23,             TENSYM........1000
     1   Q31,Q32,Q33,P11,P12,P13,P21,P22,P23,P31,P32,P33)                TENSYM........1100
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                TENSYM........1200
C                                                                        TENSYM........1300
C.....COMPUTE TRANSFORMED MATRIX.                                        TENSYM........1400
      P11= T11*Q11*Q11 + T22*Q12*Q12 + T33*Q13*Q13                       TENSYM........1500
      P12= T11*Q11*Q21 + T22*Q12*Q22 + T33*Q13*Q23                       TENSYM........1600
      P13= T11*Q11*Q31 + T22*Q12*Q32 + T33*Q13*Q33                       TENSYM........1700
      P22= T11*Q21*Q21 + T22*Q22*Q22 + T33*Q23*Q23                       TENSYM........1800
      P23= T11*Q21*Q31 + T22*Q22*Q32 + T33*Q23*Q33                       TENSYM........1900
      P33= T11*Q31*Q31 + T22*Q32*Q32 + T33*Q33*Q33                       TENSYM........2000
      P21= P12                                                           TENSYM........2100
      P31= P13                                                           TENSYM........2200
      P32= P23                                                           TENSYM........2300
C                                                                        TENSYM........2400
      RETURN                                                             TENSYM........2500
      END                                                                TENSYM........2600
C                                                                        TENSYM........2700
C     SUBROUTINE        T  R  I  S  E  T           SUTRA VERSION 2D3D.1  TRISET.........100
C                                                                        TRISET.........200
C *** PURPOSE :                                                          TRISET.........300
C ***  TO SET UP THE POINTER ARRAYS THAT GIVE THE MATRIX STRUCTURE       TRISET.........400
C ***  IN "SLAP TRIAD" FORMAT.                                           TRISET.........500
C                                                                        TRISET.........600
      SUBROUTINE TRISET(IA, JA)                                          TRISET.........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                TRISET.........800
      DIMENSION IA(NELT),JA(NDIMJA)                                      TRISET.........900
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  TRISET........1000
     1   NPCYC,NUCYC,NPRINT,IREAD,ISTORE,NOUMAT,IUNSAT,KTYPE             TRISET........1100
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              TRISET........1200
     1   NSOP,NSOU,NBCN                                                  TRISET........1300
      COMMON /DIMX/ NBIX,NWI,NWF,NWL,NELT,NNNX,NEX,N48                   TRISET........1400
      COMMON /DIMX2/ NELTA,NNVEC,NDIMJA                                  TRISET........1500
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           TRISET........1600
C                                                                        TRISET........1700
C.....DEFINE CERTAIN QUANTITIES FOR CONVENIENCE AND EFFICIENCY.          TRISET........1800
      NN12 = NN1*NN2                                                     TRISET........1900
      NNNBH = NN - NBHALF                                                TRISET........2000
      NBH1 = NBHALF + 1                                                  TRISET........2100
      NB1 = NB + 1                                                       TRISET........2200
C                                                                        TRISET........2300
C.....CREATE THE POINTER ARRAYS IA AND JA, WHICH SPECIFY THE             TRISET........2400
C        MATRIX ARRAY STRUCTURE IN "SLAP TRIAD" FORMAT.                  TRISET........2500
C                                                                        TRISET........2600
      IF (IABS(KTYPE).EQ.3) THEN                                         TRISET........2700
C.....3D PROBLEM.                                                        TRISET........2800
C                                                                        TRISET........2900
      M = 0                                                              TRISET........3000
      DO 400 KS=0,2                                                      TRISET........3100
         NBMK = KS*NN12                                                  TRISET........3200
         NSTK = KS*9                                                     TRISET........3300
      DO 400 JS=0,2                                                      TRISET........3400
         NBMJ = NBMK + JS*NN1                                            TRISET........3500
         NSTJ = NSTK + JS*3                                              TRISET........3600
      DO 400 IS=1,3                                                      TRISET........3700
         NBM = NBMJ + IS                                                 TRISET........3800
         NBMC = NB1 - NBM                                                TRISET........3900
         NST = NSTJ + IS                                                 TRISET........4000
         IF (NST.LT.14) THEN                                             TRISET........4100
            IBEG = NBH1 - NBM                                            TRISET........4200
            IEND = NN                                                    TRISET........4300
         ELSE                                                            TRISET........4400
            IBEG = 1                                                     TRISET........4500
            IEND = NNNBH + NBMC                                          TRISET........4600
         END IF                                                          TRISET........4700
         NBMNBH = NBM - NBHALF                                           TRISET........4800
         DO 300 N=IBEG,IEND                                              TRISET........4900
            M = M + 1                                                    TRISET........5000
            IA(M) = N                                                    TRISET........5100
            JA(M) = N + NBMNBH                                           TRISET........5200
  300    CONTINUE                                                        TRISET........5300
  400 CONTINUE                                                           TRISET........5400
C                                                                        TRISET........5500
      ELSE                                                               TRISET........5600
C.....2D PROBLEM.                                                        TRISET........5700
C                                                                        TRISET........5800
      M = 0                                                              TRISET........5900
      DO 1400 JS=0,2                                                     TRISET........6000
         NBMJ = JS*NN1                                                   TRISET........6100
         NSTJ = JS*3                                                     TRISET........6200
      DO 1400 IS=1,3                                                     TRISET........6300
         NBM = NBMJ + IS                                                 TRISET........6400
         NBMC = NB1 - NBM                                                TRISET........6500
         NST = NSTJ + IS                                                 TRISET........6600
         IF (NST.LT.5) THEN                                              TRISET........6700
            IBEG = NBH1 - NBM                                            TRISET........6800
            IEND = NN                                                    TRISET........6900
         ELSE                                                            TRISET........7000
            IBEG = 1                                                     TRISET........7100
            IEND = NNNBH + NBMC                                          TRISET........7200
         END IF                                                          TRISET........7300
         NBMNBH = NBM - NBHALF                                           TRISET........7400
         DO 1300 N=IBEG,IEND                                             TRISET........7500
            M = M + 1                                                    TRISET........7600
            IA(M) = N                                                    TRISET........7700
            JA(M) = N + NBMNBH                                           TRISET........7800
 1300     CONTINUE                                                       TRISET........7900
 1400  CONTINUE                                                          TRISET........8000
C                                                                        TRISET........8100
      END IF                                                             TRISET........8200
C                                                                        TRISET........8300
      RETURN                                                             TRISET........8400
      END                                                                TRISET........8500
C                                                                        TRISET........8600
C     SUBROUTINE        Z  E  R  O                 SUTRA VERSION 2D3D.1  ZERO...........100
C                                                                        ZERO...........200
C *** PURPOSE :                                                          ZERO...........300
C ***  TO FILL AN ARRAY WITH A CONSTANT VALUE (USUALLY ZERO).            ZERO...........400
C                                                                        ZERO...........500
      SUBROUTINE ZERO(A,IADIM,FILL)                                      ZERO...........600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                ZERO...........700
      DIMENSION A(IADIM)                                                 ZERO...........800
C                                                                        ZERO...........900
C.....FILL ARRAY A WITH VALUE IN VARIABLE 'FILL'                         ZERO..........1000
      DO 10 I=1,IADIM                                                    ZERO..........1100
   10 A(I)=FILL                                                          ZERO..........1200
C                                                                        ZERO..........1300
C                                                                        ZERO..........1400
      RETURN                                                             ZERO..........1500
      END                                                                ZERO..........1600
