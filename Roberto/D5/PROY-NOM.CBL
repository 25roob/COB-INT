       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRO-FIN2.
       AUTHOR.     ROBERTO Q.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARC-PRO ASSIGN TO DISK.
           SELECT SOR-TEM ASSIGN TO SORT.
           SELECT ARC-CAT ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
               RECORD KEY IS ID-PROYECTO
               ALTERNATE RECORD KEY IS NOMBRE-PROY WITH DUPLICATES.
           SELECT R-NOMPRO ASSIGN TO PRINTER.
           SELECT R-CATPRO ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
       FD ARC-PRO VALUE OF TITLE WKS-ETIQ-SEC.
       01 REG-PRO.
           03 ID-PROYECT        PIC 9(02).
           03 STATUS-PROY       PIC 9(02).
           03 NOMBRE-CONSULTOR  PIC X(30).
           03 INGRESO-MENSUAL   PIC 9(08)V99.
           03 FECHA-INICIO      PIC 9(08).
           03 FECHA-TERMINO     PIC 9(08).
           
       SD SOR-TEM.
       01 REG-PRO.
           03 SID-PROYECTO      PIC 9(02).
           03 SSTATUS-PROY      PIC 9(02).
           03 SNOMBRE-CONSULTOR PIC X(30).
           03 SINGRESO-MENSUAL  PIC 9(08)V99.
           03 SFECHA-INICIO     PIC 9(08).
           03 SFECHA-TERMINO    PIC 9(08).
      
      FD ARC-CAT VALUE OF TITLE WKS-ETIQ-IND.
      01 REG-CAT.
           03 ID-PROYECTO       PIC 9(02).
           03 NOMBRE-PROY       PIC X(17).
           03 IMP-TOTAL         PIC 9(10)V99.
      
       FD R-NOMPRO VALUE OF TITLE WKS-ETIQ-R1
           VALUE PROTECTION IS      PROTECTED
           VALUE SECURITYTYPE IS    PUBLIC
           RECORD CONTAINS          132 CHARACTERS
           BLOCK CONTAINS           1 RECORDS.
       01 LINEAN           PIC X(132).
      
       FD R-CATPRO VALUE OF TITLE WKS-ETIQ-R2
       VALUE PROTECTION IS          PROTECTED
           VALUE SECURITYTYPE IS    PUBLIC
           RECORD CONTAINS          132 CHARACTERS
           BLOCK CONTAINS           1 RECORDS.
       01 LINEAC           PIC X(132).
      
      
      
       WORKING-STORAGE SECTION.
       01 WKS-ETIQ-SEC    PIC X(24) VALUE "RFQV/S015/FILE/ARC-PRO.".
       01 WKS-ETIQ-IND    PIC X(24) VALUE "RFQV/S015/FILE/ARC-CAT.".
       01 WKS-ETIQ-R1     PIC X(24) VALUE "RFQV/S015/LIST/R-NOMPRO.".
       01 WKS-ETIQ-R2     PIC X(24) VALUE "RFQV/S015/LIST/R-CATPRO.".
	   
	   77 WKS-CONT-LIN    PIC 99.
	   77 WKS-FINR1       PIC 9.
	   77 WKS-I           PIC 99.
	   
       
       01 WKS-MESES.
           03 FILLER       PIC X(05) VALUE "ENE01".
           03 FILLER       PIC X(05) VALUE "FEB02".
           03 FILLER       PIC X(05) VALUE "MAR03".
           03 FILLER       PIC X(05) VALUE "ABR04".
           03 FILLER       PIC X(05) VALUE "MAY05".
           03 FILLER       PIC X(05) VALUE "JUN06".
           03 FILLER       PIC X(05) VALUE "JUL07".
           03 FILLER       PIC X(05) VALUE "AGO08".
           03 FILLER       PIC X(05) VALUE "SEP09".
           03 FILLER       PIC X(05) VALUE "OCT10".
           03 FILLER       PIC X(05) VALUE "NOV11".
           03 FILLER       PIC X(05) VALUE "DIC12".
           
       01 T-MESES REDEFINES WKS-MESES.
           03 T-MESE OCCURS 12 TIMES.
               05 T-MES    PIC XXX.
               05 T-IDM    PIC 99.  

       01 WKS-PROYECTOS.
           03 FILLER      PIC X(18) VALUE "01INTELAR         ".    
           03 FILLER      PIC X(18) VALUE "23IMPUESTOS LIDE  ".
           03 FILLER      PIC X(18) VALUE "11TARJETAS B24    ".
           03 FILLER      PIC X(18) VALUE "15DOMICILIACION   ".
           03 FILLER      PIC X(18) VALUE "02PAGO ELECTRONICO".
           03 FILLER      PIC X(18) VALUE "74CHEQUES         ".    
           03 FILLER      PIC X(18) VALUE "18SPEI            ".
           03 FILLER      PIC X(18) VALUE "16GBS             ".
           03 FILLER      PIC X(18) VALUE "24BOLSA DE DINERO ".
           03 FILLER      PIC X(18) VALUE "13REMATE          ".
           
       01 T-PROYECTOS REDEFINES WKS-PROYECTOS.
           03 T-PROYECTO OCCURS 10 TIMES.
               05 T-IDP   PIC 99.
               05 T-NOP   PIC X(16).  
       
       01 WKS-HOY.
           03 WKS-ANO     PIC 99.
           03 WKS-MES     PIC 99.
           03 WKS-DIA     PIC 99.
       
      *===========VARIABLES DE REPORTE 1===========
       01 WKS-ENC1.
           03 FILLER      PIC X(20) VALUE SPACES.
           03 FILLER      PIC X(40)
               VALUE "NOMINA DE PROYECTOS-INFOWARE,SC".
               
       01 WKS-ENC2.
      *       03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER      PIC X(18)
                     VALUE "FECHA DE PROCESO: ".
           03 WKS-DIA     PIC 99.
           03 FILLER      PIC X VALUE "/".
           03 WKS-MES     PIC 99.
           03 FILLER      PIC XXX VALUE "/20".
           03 WKS-ANO     PIC 99.
           03 FILLER      PIC X(35) VALUE SPACES.
           03 FILLER      PIC X(08) VALUE "PAGINA: ".
           03 WKS-NUM-PAG PIC 9(02).
       
       01 WKS-ENC3.
           03 FILLER       PIC X(21) VALUE "NOMBRE DEL PROYECTO: ".
           03 WKS-NOM-PROE PIC X(20).
           
       01 WKS-ENC4.
      *       03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER       PIC X(16) VALUE "NOMBRE CONSULTOR".
           03 FILLER       PIC X(01) VALUE SPACES.
           03 FILLER       PIC X(12) VALUE "FECHA INICIO".
           03 FILLER       PIC X(01) VALUE SPACES.
           03 FILLER       PIC X(13) VALUE "FECHA TERMINO".
           03 FILLER       PIC X(01) VALUE SPACES.
           03 FILLER       PIC X(15) VALUE "INGRESO MENSUAL".
           03 FILLER       PIC X(01) VALUE SPACES.
           03 FILLER       PIC X(13) VALUE "INGRESO TOTAL".
           
       01 WKS-DET1.
           03 FILLER       PIC X(01) VALUE SPACES.
           03 WKS-NOM-CONS PIC X(20).
           03 FILLER       PIC X(03) VALUE SPACES.
           03 WKS-FEC-INID PIC X(09).
           03 FILLER       PIC X(02) VALUE SPACES.
           03 WKS-FEC-TERD PIC X(09).
           03 FILLER       PIC X(01) VALUE SPACES.
           03 WKS-ING-MEND PIC ZZ,ZZZ,ZZ9.99.
           03 FILLER       PIC X(01) VALUE SPACES.
           03 WKS-ING-TOTD PIC ZZ,ZZZ,ZZ9.99.
           
       01 WKS-PIE1.
           03 FILLER       PIC X(20) VALUE SPACES.
           03 FILLER       PIC X(16) VALUE
               "TOTAL NOMINA DE ".
           03 WKS-NOM-PROP PIC X(15).
           03 FILLER       PIC X(05) VALUE ":    ".
           03 WKS-TOANU    PIC $$,$$$,$$9.99.
              

      *===========VARIABLES DE REPORTE 2===========
       01 WKS-ENCA.
           03 FILLER       PIC X(20) VALUE SPACES.
           03 FILLER       PIC X(40)
               VALUE "CATALOGO DE PROYECTOS-INFOWARE,SC".
               
       01 WKS-ENCB.
      *       03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER       PIC X(18)
                     VALUE "FECHA DE PROCESO: ".
           03 WKS-DIA      PIC 99.
           03 FILLER       PIC X VALUE "/".
           03 WKS-MESEB    PIC XXX.
           03 FILLER       PIC XXX VALUE "/20".
           03 WKS-ANO      PIC 99.
           03 FILLER       PIC X(35) VALUE SPACES.
           03 FILLER       PIC X(08) VALUE "PAGINA: ".
           03 WKS-NUM-PAGA PIC 9(02).
           
       01 WKS-ENCC.
      *       03 FILLER         PIC X(03) VALUE SPACES.
           03 FILLER       PIC X(16) VALUE "ID PROYECTO".
           03 FILLER       PIC X(01) VALUE SPACES.
           03 FILLER       PIC X(12) VALUE "NOMBRE".
           03 FILLER       PIC X(01) VALUE SPACES.
           03 FILLER       PIC X(13) VALUE SPACES.
           03 FILLER       PIC X(01) VALUE SPACES.
           03 FILLER       PIC X(15) VALUE SPACES.
           03 FILLER       PIC X(01) VALUE SPACES.
           03 FILLER       PIC X(13) VALUE "IMPORTE".
           
       01 WKS-DETA.
           03 FILLER       PIC X(01) VALUE SPACES.
           03 WKS-ID-PROD  PIC X(20).
           03 FILLER       PIC X(03) VALUE SPACES.
           03 WKS-NOM-PROD PIC X(17).
           03 FILLER       PIC X(02) VALUE SPACES.
           03 FILLER       PIC X(01) VALUE SPACES.
           03 FILLER       PIC X(01) VALUE SPACES.
           03 FILLER       PIC X(13) VALUE SPACES.
           03 FILLER       PIC X(01) VALUE SPACES.
           03 WKS-ING-TOTD PIC ZZ,ZZZ,ZZ9.99.
           
       01 WKS-PIEA.
           03 FILLER       PIC X(12) VALUE "TOTAL PROY: ".
           03 WKS-PROY-CO  PIC 99.
           03 FILLER       PIC X(30) VALUE SPACES.
           03 FILLER       PIC X(16) VALUE "TOTAL IMPORTES: ".
           03 WKS-TOT-IMPO PIC $$,$$$,$$9.99.
           
       PROCEDURE DIVISION.
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO.
           PERFORM 3000-FIN.
           
       1000-INICIO.
           OPEN INPUT ARC-PRO.
           OPEN INPUT ARC-CAT.
           OPEN OUTPUT R-NOMPRO.
           OPEN OUTPUT R-CATPRO.
       
       2000-PROCESO.
           PERFORM 2200-REPORTE-NOMINA.
           
           PERFORM 2600-REPORTE-CATALOGO.
       
       2200-REPORTE-NOMINA.
           SORT SOR-TEM
		   ASCENDING ID-PROYECT FECHA-TERMINO
		   USING ARC-PRO
		   OUTPUT PROCEDURE 2210-GENERA-REPORTE.
		   
	   2210-GENERA-REPORTE.
	   
	       PERFORM 2220-INICIO.
		   PERFORM 2250-PROCESO UNTIL WKS-FINR1 = 1.
		   PERFORM 2270-FIN.
       
	   2220-INICIO.
	   
	       RETURN SOR-TEM
		   AT END MOVE 1 TO WKS-FINR1.
		   
	   2222-GENERA-ENCABEZADO.
	       ADD 1 TO WKS-NUM-PAG.
		   MOVE SPACES TO LINEAN.
		   MOVE WKS-ENC1 TO LINEAN.
		   WRITE LINEAN AFTER PAGE.
		   
		   ACCEPT WKS-HOY FROM DATE.
		   MOVE CORR WKS-HOY TO WKS-ENC2.
		   MOVE WKS-ENC2 TO LINEAN.
		   WRITE LINEAN AFTER 2.
		   
		   SET WKS-I TO 1
		   SEARCH T-PROYECTO VARYING WKS-I
		   AT END DISPLAY "NO EXISTE"
		   WHEN T-IDP (WKS-I) = SID-PROYECTO
		   MOVE T-NOP (WKS-I) TO WKS-NOM-PROE.
		   MOVE WKS-ENC3 TO LINEAN.
		   WRITE LINEAN AFTER 2.
		   
		   MOVE WKS-ENC4 TO LINEAN.
		   WRITE LINEAN AFTER 2.
		   
		   MOVE 7 TO WKS-CONT-LIN.
	   
	   2250-PROCESO.
	       MOVE SNOMBRE-CONSULTOR TO WKS-NOM-CONS.
		   
	   
       2270-FIN.
	   
	   
	   2600-REPORTE-CATALOGO.
       
       
       3000-FIN.
       
           CLOSE ARC-PRO.
           CLOSE ARC-CAT.
           CLOSE R-NOMPRO.
           CLOSE R-CATPRO.