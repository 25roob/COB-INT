       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTTARJE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO DISK.
           SELECT REP-TAC ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES.
       01 REG-CLI.
           03 ID-CLIENTE PIC 9(03).
           03 NOM-CLI    PIC X(30).
           03 CLA-TAR    PIC 9.
           03 NOM-TAR    PIC X(20).
           03 ANUA       PIC 9(05)V99.
           03 FEC-ING    PIC 9(08).
       
       FD REP-TAC.
       01 LINEA          PIC X(132).
       
       WORKING-STORAGE SECTION.
      *-----------TABLAS----------------*
       01 WKS-MESES.
           03 FILLER    PIC X(12) VALUE "ENERO     01".
           03 FILLER    PIC X(12) VALUE "FEBRERO   02".
           03 FILLER    PIC X(12) VALUE "MARZO     03".
           03 FILLER    PIC X(12) VALUE "ABRIL     04".
           03 FILLER    PIC X(12) VALUE "MAYO      05".
           03 FILLER    PIC X(12) VALUE "JUNIO     06".
           03 FILLER    PIC X(12) VALUE "JULIO     07".
           03 FILLER    PIC X(12) VALUE "AGOSTO    08".
           03 FILLER    PIC X(12) VALUE "SEPTIEMBRE09".
           03 FILLER    PIC X(12) VALUE "OCTUBRE   10".
           03 FILLER    PIC X(12) VALUE "NOVIEMBRE 11".
           03 FILLER    PIC X(12) VALUE "DICIEMBRE 12".
       
       01 T-MESES REDEFINES WKS-MESES.
           03 T-MESE OCCURS 12 TIMES.
               05 T-MES PIC X(10).
               05 T-NME PIC 99.
               
       01 WKS-ANIOS.
           03 FILLER    PIC 9999 VALUE 2020.
           03 FILLER    PIC 9999 VALUE 2021.
           03 FILLER    PIC 9999 VALUE 2022.
           03 FILLER    PIC 9999 VALUE 2023.
           
       01 T-ANIOS REDEFINES WKS-ANIOS.
           03 T-ANIO OCCURS 4 TIMES.
               05 T-AN PIC 9999.
           
       01 T-TARJETAS.
           03 T-TAR OCCURS 3 TIMES PIC X(20).
           
       01 T-CLIENTES.
           03 T-ANO OCCURS 4 TIMES.
               04 T-TIPO OCCURS 3 TIMES.
                   05 T-MON OCCURS 12 TIMES.
                       07 T-CLTS PIC 99.
      
      
      *-----------VARIABLES-------------*
       
       77 WKS-FIN            PIC 9.
       77 WKS-DESC           PIC X.
       77 WKS-I              PIC 99.
       77 WKS-J              PIC 99.
       77 WKS-K              PIC 99.
       77 WKS-L              PIC 99.
       77 WKS-M              PIC 99.
       77 WKS-N              PIC 99.
       77 WKS-ESPERA         PIC X.
       
       01 WKS-CTAR1          PIC 99.
       01 WKS-CTAR2          PIC 99.
       01 WKS-CTAR3          PIC 99.
       
       01 WKS-SPLITFE.
           03 WKS-SANO       PIC 9999.
           03 WKS-SMES       PIC 99.
           03 FILLER         PIC 99.
           
       01 WKS-HOY.
           03 WKS-ANO     PIC 99.
           03 WKS-MES     PIC 99.
           03 WKS-DIA     PIC 99.
           
      *-----------REPORTES---------------*
      
       01 WKS-ENCB.
           03 FILLER         PIC X(43) VALUE SPACES.
           03 FILLER         PIC X(20)
                   VALUE "FECHA DE PROCESO: 20".
           03 WKS-ANO        PIC 99.
           03 FILLER         PIC X VALUE "/".
           03 WKS-MES        PIC 99.
           03 FILLER         PIC X VALUE "/".
           03 WKS-DIA        PIC 99.
      *     03 FILLER         PIC X(38) VALUE SPACES.
      *     03 FILLER         PIC X(07) VALUE "PAG. : ".
      *     03 WKS-NUM-PAGA   PIC 9(02).
      
       01 WKS-ENCA.
           03 FILLER         PIC X(20) VALUE SPACES.
           03 FILLER         PIC X(33)
               VALUE "RESUMEN DE MESES DE ALTA DEL ANO ".
           03 WKS-CUANO      PIC 9999.
           
       01 WKS-ENCC.
           03 FILLER         PIC X(14) VALUE SPACES.
           03 WKS-TTAR1      PIC X(07).       
           03 FILLER         PIC X(11) VALUE SPACES.
           03 WKS-TTAR2      PIC X(07).
           03 FILLER         PIC X(11) VALUE SPACES.
           03 WKS-TTAR3      PIC X(07).
           03 FILLER         PIC X(11) VALUE SPACES.
           03 FILLER         PIC X(05) VALUE "TOTAL".
           
       01 WKS-DETALLE.
           03 WKS-MESDE      PIC X(10).
           03 FILLER         PIC X(06) VALUE SPACES.
           03 WKS-CTARF1     PIC Z9.
           03 FILLER         PIC X(16) VALUE SPACES.
           03 WKS-CTARF2     PIC Z9.
           03 FILLER         PIC X(16) VALUE SPACES.
           03 WKS-CTARF3     PIC Z9.
           03 FILLER         PIC X(15) VALUE SPACES.
           03 WKS-CTOTAL     PIC 99.
           
       01 WKS-PIE.
           03 FILLER         PIC X(10) VALUE "TOTAL:".
           03 FILLER         PIC X(06) VALUE SPACES.
           03 WKS-CTO1       PIC 99.
           03 FILLER         PIC X(16) VALUE SPACES.
           03 WKS-CTO2       PIC 99.
           03 FILLER         PIC X(16) VALUE SPACES.
           03 WKS-CTO3       PIC 99.
           03 FILLER         PIC X(15) VALUE SPACES.
           03 WKS-CTOTALF    PIC 99.
       
       01 WKS-LINEA-SOL      PIC X(73).
           
       PROCEDURE DIVISION.
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO.
           PERFORM 3000-FIN.
           STOP RUN.
       
       1000-INICIO.
           DISPLAY SPACES ERASE.
           OPEN OUTPUT REP-TAC.
           OPEN INPUT CLIENTES.
           
       
           READ CLIENTES
           AT END MOVE 1 TO WKS-FIN.
       
       2000-PROCESO.
       
           PERFORM 2100-LEER-CARGAR UNTIL WKS-FIN = 1.
       
           PERFORM 2200-GENERA-REPORTE.
       
       2100-LEER-CARGAR.
           MOVE FEC-ING TO WKS-SPLITFE.
       
           PERFORM 2120-LLENA-TABLA 
               VARYING WKS-I FROM 1 BY 1 UNTIL WKS-I > 4
               AFTER WKS-J FROM 1 BY 1 UNTIL WKS-J > 3
               AFTER WKS-K FROM 1 BY 1 UNTIL WKS-K > 12.
               
           READ CLIENTES
           AT END MOVE 1 TO WKS-FIN.
           
       2120-LLENA-TABLA.
           IF T-TAR (WKS-J) = SPACES AND T-AN (WKS-I) = WKS-SANO
      *           DISPLAY WKS-I WKS-J WKS-K NOM-TAR
      *           ACCEPT WKS-ESPERA
               MOVE NOM-TAR TO T-TAR (WKS-J)
               ADD 1 TO T-CLTS (WKS-I WKS-J WKS-K)
               ADD 15 TO WKS-I WKS-J WKS-K.
           
           IF T-TAR (WKS-J) = NOM-TAR AND T-NME (WKS-K) = WKS-SMES
               AND T-AN (WKS-I) = WKS-SANO
               ADD 1 TO T-CLTS (WKS-I WKS-J WKS-K).
       
       2200-GENERA-REPORTE.
           PERFORM 2220-INICIO.
           
           PERFORM 2250-PROCESO
               VARYING WKS-L FROM 1 BY 1 UNTIL WKS-L > 4.
               
           PERFORM 2270-FIN.
           
       2220-INICIO.
           MOVE 1 TO WKS-M WKS-N.
       
       2250-PROCESO.
           INITIALIZE WKS-CTO1
                      WKS-CTO2
                      WKS-CTO3.
           
           PERFORM 2252-ENCABEZADO.
           
           PERFORM 2253-ESCRIBE-DETALLE
               VARYING WKS-N FROM 1 BY 1 UNTIL WKS-N > 12.
               
           PERFORM 2257-GENERA-PIE.
       
       2252-ENCABEZADO.
           ACCEPT WKS-HOY FROM DATE.
           MOVE CORR WKS-HOY       TO WKS-ENCB.
           MOVE WKS-ENCB TO LINEA.
           WRITE LINEA AFTER PAGE.
           
           MOVE T-AN (WKS-L) TO WKS-CUANO.
           MOVE WKS-ENCA TO LINEA.
           WRITE LINEA AFTER 1.
           
           MOVE T-TAR (1) TO WKS-TTAR1.
           MOVE T-TAR (2) TO WKS-TTAR2.
           MOVE T-TAR (3) TO WKS-TTAR3.
           MOVE WKS-ENCC TO LINEA.
           WRITE LINEA AFTER 2.
           
               
       2253-ESCRIBE-DETALLE.
           MOVE T-MES (WKS-N) TO WKS-MESDE.
           
           MOVE T-CLTS (WKS-L 1 WKS-N) TO WKS-CTAR1.
           MOVE T-CLTS (WKS-L 2 WKS-N) TO WKS-CTAR2.
           MOVE T-CLTS (WKS-L 3 WKS-N) TO WKS-CTAR3.
           
           MOVE WKS-CTAR1 TO WKS-CTARF1.
           MOVE WKS-CTAR2 TO WKS-CTARF2.
           MOVE WKS-CTAR3 TO WKS-CTARF3.
           
           ADD WKS-CTAR1 TO WKS-CTO1.
           ADD WKS-CTAR2 TO WKS-CTO2.
           ADD WKS-CTAR3 TO WKS-CTO3.
           
           ADD WKS-CTAR1 WKS-CTAR2 TO WKS-CTAR3 GIVING WKS-CTOTAL.
           
           MOVE WKS-DETALLE TO LINEA.
           WRITE LINEA AFTER 1.
           
       2257-GENERA-PIE.
       
           MOVE ALL "-" TO WKS-LINEA-SOL.
           MOVE WKS-LINEA-SOL TO LINEA.
           WRITE LINEA AFTER 1.
       
           ADD WKS-CTO1 WKS-CTO2 TO WKS-CTO3 GIVING WKS-CTOTALF.
           MOVE WKS-PIE TO LINEA.
           WRITE LINEA AFTER 1.
       
       2270-FIN.
       
       
       3000-FIN.
       
       
           CLOSE CLIENTES.
           CLOSE REP-TAC.