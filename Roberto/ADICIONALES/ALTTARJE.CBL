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
       77 WKS-I              PIC 9.
       77 WKS-J              PIC 99.
       77 WKS-K              PIC 99.
       77 WKS-L              PIC 99.
       77 WKS-M              PIC 99.
       77 WKS-N              PIC 99.
       
       
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
           03 FILLER         PIC X(28) VALUE SPACES.
           03 FILLER         PIC X(33)
               VALUE "RESUMEN DE MESES DE ALTA DEL AÑO ".
		   03 WKS-CUANO      PIC 9999.
       
       PROCEDURE DIVISION.
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO.
           PERFORM 3000-FIN.
           STOP RUN.
       
       1000-INICIO.
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
           IF T-TAR (WKS-I) = SPACES
               MOVE NOM-TAR TO T-TAR (WKS-I)
               ADD 1 TO T-CLTS (WKS-I WKS-J WKS-K).
           
           IF T-TAR (WKS-I) = NOM-TAR AND T-NME (WKS-J) = WKS-SMES
               AND T-AN (WKS-K) = WKS-SANO
               ADD 1 TO T-CLTS (WKS-I WKS-J WKS-K).
       
       2200-GENERA-REPORTE.
           PERFORM 2220-INICIO.
           PERFORM 2250-PROCESO
               VARYING WKS-L FROM 1 BY 1 UNTIL WKS-L > 4
               AFTER WKS-M FROM 1 BY 1 UNTIL WKS-M > 3
               AFTER WKS-N FROM 1 BY 1 UNTIL WKS-N > 12.
           PERFORM 2270-FIN.
           
       2220-INICIO.
       
       2250-PROCESO.
       
           PERFORM 2252-ENCABEZADO.
       
       
       2252-ENCABEZADO.
	       ACCEPT WKS-HOY FROM DATE.
		   MOVE CORR WKS-HOY       TO WKS-ENCB.
		   MOVE WKS-ENCB TO LINEA.
		   WRITE LINEA AFTER PAGE.
		   
		   MOVE T-AN (WKS-I) TO WKS-CUANO.
		   MOVE 
       
       2270-FIN.
       
       
       3000-FIN.
       
       
           CLOSE CLIENTES.
           CLOSE REP-TAC.