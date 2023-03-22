       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT-PRO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO DISK.
           SELECT REP-SOR ASSIGN TO PRINTER.
           SELECT SOR-TEM ASSIGN TO SORT.
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
           
       FD REP-SOR.
       01 LINEA          PIC X(132).
       
       SD SOR-TEM.
       01 REG-CLI-S.
           03 ID-CLIENTE-S PIC 9(03).
           03 NOM-CLI-S    PIC X(30).
           03 CLA-TAR-S    PIC 9.
           03 NOM-TAR-S    PIC X(20).
           03 ANUA-S       PIC 9(05)V99.
           03 FEC-ING-S    PIC 9(08).
       
       
       WORKING-STORAGE SECTION.
       01 WKS-CONT-LIN     PIC 99.
       01 WKS-TOTAL-CLI    PIC 99.
       
       77 WKS-FIN            PIC 9.
       77 WKS-NOM-TAR           PIC X(20).
       
       
       01 WKS-HOY.
           03 WKS-ANO     PIC 99.
           03 WKS-MES     PIC 99.
           03 WKS-DIA     PIC 99.
       
       
      *------VARIABLES-DE-REPORTE-----------*
       01 WKS-ENCA.
           03 FILLER         PIC X(28) VALUE SPACES.
           03 FILLER         PIC X(40)
                      VALUE "REPORTE DE TARJETAS".
      
       01 WKS-ENCB.
      *       03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER         PIC X(18)
                   VALUE "FECHA DE PROCESO: ".
           03 WKS-DIA        PIC 99.
           03 FILLER         PIC X VALUE "/".
           03 WKS-MES        PIC 99.
           03 FILLER         PIC X VALUE "/".
           03 WKS-ANO        PIC 99.
           03 FILLER         PIC X(38) VALUE SPACES.
           03 FILLER         PIC X(07) VALUE "PAG. : ".
           03 WKS-NUM-PAGA   PIC 9(02).
      
       01 WKS-ENCBB.
           03 FILLER         PIC X(20) VALUE
                "TIPO DE TARJETA:".
           03 WKS-NOM-TARB   PIC X(20).
      
       01 WKS-ENCC.
      *       03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER         PIC X(10) VALUE "ID-CLIENTE".
           03 FILLER         PIC X(02) VALUE SPACES.
           03 FILLER         PIC X(06) VALUE "NOMBRE".
           03 FILLER         PIC X(10) VALUE SPACES.
           03 FILLER         PIC X(16) VALUE "CLAVE DE TARJETA".
           03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER         PIC X(14) VALUE "FEC DE INGRESO".
           03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER         PIC X(09) VALUE "ANUALIDAD".      
           
           
       01 WKS-DET1.
           03 FILLER         PIC X(02) VALUE SPACES.
           03 WKS-ID         PIC 999.
           03 FILLER         PIC X(07) VALUE SPACES.
           03 WKS-NOMB       PIC X(21).
           03 FILLER         PIC X(02) VALUE SPACES.
           03 WKS-TIPT       PIC X(10).
           03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER         PIC X(02) VALUE "20".
           03 WKS-FECH       PIC 9(06).
           03 FILLER         PIC X(07) VALUE SPACES.
           03 WKS-ANUF       PIC $$$,$$9.99.
           
       01 WKS-TOT1.
           03 FILLER         PIC X(38) VALUE SPACES.
           03 FILLER         PIC X(18) VALUE "TOTAL DE CLIENTES:".
           03 FILLER         PIC X(09) VALUE SPACES.
           03 WKS-TOCLI      PIC 99. 
          
       
       PROCEDURE DIVISION.
	   PRINCIPAL SECTION.
       INICIO.
           SORT SOR-TEM
           ASCENDING NOM-CLI-S
           INPUT PROCEDURE 1000-ORDENA-ARCHIVO 
           OUTPUT PROCEDURE 2000-GENERA-REPORTE    
           STOP RUN.
           
       1000-ORDENA-ARCHIVO SECTION.
	   INICIO-ORDENA.
           PERFORM 1200-INICIO.
           PERFORM 1500-PROCESO UNTIL WKS-FIN = 1.
           PERFORM 1700-FIN.
		   GO TO 1800-FIN-SECTION.
       
       1200-INICIO.
           OPEN INPUT CLIENTES.
           
           DISPLAY SPACES LINE 01 POSITION 01 ERASE.
           
           READ CLIENTES
               AT END MOVE 1 TO WKS-FIN.
               
           DISPLAY "QUE TIPO DE TARJETA QUIERES CONSULTAR?:"
               LINE 10 POSITION 05.
           ACCEPT WKS-NOM-TAR LINE 10 POSITION 50.
       
       1500-PROCESO.
           
           IF WKS-NOM-TAR = NOM-TAR
               MOVE REG-CLI TO REG-CLI-S
               RELEASE REG-CLI-S.
               
           READ CLIENTES
           AT END MOVE 1 TO WKS-FIN.
       
       1700-FIN.
           CLOSE CLIENTES.
           
           INITIALIZE WKS-FIN.
		   
	   1800-FIN-SECTION.
	       EXIT.
	   
       
       2000-GENERA-REPORTE SECTION.
	   INICIO-REPORTE.
           PERFORM 2200-INICIO.
           PERFORM 2500-PROCESO UNTIL WKS-FIN = 1.
           PERFORM 2700-FIN.
		   GO TO 2800-FIN-SECTION.
       
       2200-INICIO.
           
           OPEN OUTPUT REP-SOR
           RETURN SOR-TEM
               AT END MOVE 1 TO WKS-FIN.
               
           PERFORM 2220-GENERA-ENCABEZADO.
               
       2220-GENERA-ENCABEZADO.
      
           ADD 1                   TO WKS-NUM-PAGA.
           MOVE SPACES             TO LINEA.
           MOVE WKS-ENCA           TO LINEA.
           WRITE LINEA AFTER PAGE.
      
           ACCEPT WKS-HOY FROM DATE.
           MOVE CORR WKS-HOY       TO WKS-ENCB.
      
           MOVE WKS-ENCB           TO LINEA.
           WRITE LINEA AFTER 1.
      
           MOVE WKS-NOM-TAR            TO WKS-NOM-TARB.
           MOVE WKS-ENCBB          TO LINEA.
           WRITE LINEA AFTER 2.
      
           MOVE WKS-ENCC           TO LINEA.
           WRITE LINEA AFTER 2.
           MOVE 3                  TO WKS-CONT-LIN.
       
       2500-PROCESO.
           
           MOVE ID-CLIENTE-S         TO WKS-ID.
           MOVE NOM-CLI-S            TO WKS-NOMB.
           MOVE NOM-TAR-S            TO WKS-TIPT.
           MOVE FEC-ING-S            TO WKS-FECH.
           MOVE ANUA-S               TO WKS-ANUF.
           
           IF WKS-CONT-LIN > 60
               PERFORM 2220-GENERA-ENCABEZADO.
               
           MOVE WKS-DET1 TO LINEA
           WRITE LINEA AFTER 1
           ADD 1 TO WKS-TOTAL-CLI.
           
           RETURN SOR-TEM
               AT END MOVE 1 TO WKS-FIN.
       
       2700-FIN.
           
           MOVE WKS-TOTAL-CLI      TO WKS-TOCLI.
           
           MOVE WKS-TOT1           TO LINEA.
           WRITE LINEA AFTER 2.
           
           CLOSE REP-SOR.
       
	   2800-FIN-SECTION.
	       EXIT.
       
       
       
       