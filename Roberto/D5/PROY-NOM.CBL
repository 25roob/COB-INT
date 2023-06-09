       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROY-NOM.
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
      * FD ARC-PRO VALUE OF TITLE WKS-ETIQ-SEC.
       FD ARC-PRO.
       01 REG-PRO.
           03 ID-PROYECT        PIC 9(02).
           03 STATUS-PROY       PIC 9(02).
           03 NOMBRE-CONSULTOR  PIC X(30).
           03 INGRESO-MENSUAL   PIC 9(08)V99.
           03 FECHA-INICIO      PIC 9(08).
           03 FECHA-TERMINO     PIC 9(08).
           
       SD SOR-TEM.
       01 SREG-PRO.
           03 SID-PROYECTO      PIC 9(02).
           03 SSTATUS-PROY      PIC 9(02).
           03 SNOMBRE-CONSULTOR PIC X(30).
           03 SINGRESO-MENSUAL  PIC 9(08)V99.
           03 SFECHA-INICIO     PIC 9(08).
           03 SFECHA-TERMINO    PIC 9(08).
      
      * FD ARC-CAT VALUE OF TITLE WKS-ETIQ-IND.
       FD ARC-CAT.
       01 REG-CAT.
           03 ID-PROYECTO       PIC 9(02).
           03 NOMBRE-PROY       PIC X(17).
           03 IMP-TOTAL         PIC 9(10)V99.
      
      * FD R-NOMPRO VALUE OF TITLE WKS-ETIQ-R1
      *     VALUE PROTECTION IS      PROTECTED
      *     VALUE SECURITYTYPE IS    PUBLIC
      *     RECORD CONTAINS          132 CHARACTERS
      *     BLOCK CONTAINS           1 RECORDS.
       FD R-NOMPRO.
       01 LINEAN           PIC X(132).
      
      * FD R-CATPRO VALUE OF TITLE WKS-ETIQ-R2
      * VALUE PROTECTION IS          PROTECTED
      *     VALUE SECURITYTYPE IS    PUBLIC
      *     RECORD CONTAINS          132 CHARACTERS
      *     BLOCK CONTAINS           1 RECORDS.
       FD R-CATPRO.
       01 LINEAC           PIC X(132).
      
      
      
       WORKING-STORAGE SECTION.
       01 WKS-ETIQ-SEC    PIC X(24) VALUE "RFQV/S015/FILE/ARC-PRO.".
       01 WKS-ETIQ-IND    PIC X(24) VALUE "RFQV/S015/FILE/ARC-CAT.".
       01 WKS-ETIQ-R1     PIC X(24) VALUE "RFQV/S015/LIST/R-NOMPRO.".
       01 WKS-ETIQ-R2     PIC X(24) VALUE "RFQV/S015/LIST/R-CATPRO.".
       
       01 WKS-SUM-TNP     PIC 9(12)V99.
       01 WKS-SUM-CUFE    PIC 9(12)V99.
       01 WKS-IMP-FINAL   PIC 9(12)V99.
       01 WKS-CHECK-FEC   PIC 9999.

       01 WKS-INPUTFEC.
           03 FILLER      PIC 99.
           03 WKS-INFEC   PIC 9(06).
       
       77 WKS-CONT-LIN    PIC 99.
       77 WKS-FINR1       PIC 9.
      * 77 WKS-I           PIC 99.
       77 WKS-IDR1CUT     PIC 99.
       77 WKS-FEC-INCUT   PIC 9999.
       77 WKS-TEM-NOMPRO  PIC X(16).
       77 WKS-ESPERA      PIC X.
       77 WKS-FINR2       PIC 9.
       77 WKS-FINO1       PIC 9.
       77 WKS-F-RWRT      PIC 9.
       
       77 WKS-TEM-INGRETO PIC 9(13)V99.
       
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
           03 T-MESE OCCURS 12 TIMES INDEXED BY WKS-I.
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
           03 T-PROYECTO OCCURS 10 TIMES INDEXED BY WKS-J.
               05 T-IDP   PIC 99.
               05 T-NOP   PIC X(16).  
       
       01 WKS-HOY.
           03 WKS-ANO     PIC 99.
           03 WKS-MES     PIC 99.
           03 WKS-DIA     PIC 99.
           
       01 WKS-HOY-TBM.
           03 WKS-ANOTBM  PIC 99.
           03 WKS-MESTBM  PIC 99.
           03 WKS-DIATBM  PIC 99.

       01 WKS-FORM-FEC.
           03 WKS-DAY     PIC 99.
           03 FILLER      PIC X VALUE "/".
           03 WKS-MONTH   PIC XXX.
           03 FILLER      PIC X VALUE "/".
           03 WKS-YEAR    PIC 99. 
           
       01 WKS-FEC-SPLIT.
           03 WKS-SPANO   PIC 9999.
           03 WKS-SPMES   PIC 99.
           03 WKS-SPDIA   PIC 99.
           
        01 WKS-LINK-DATA.
           03 WKS-FEC-INICIO   PIC 9(08).
           03 WKS-FEC-TERMIN   PIC 9(08).
           03 WKS-ING-MENSUA   PIC 9(10)V99.
           03 WKS-ING-TOTALD   PIC 9(13)V99.
       
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
      *     03 FILLER       PIC X(01) VALUE SPACES.
           03 WKS-NOM-CONS PIC X(20).
           03 FILLER       PIC X(03) VALUE SPACES.
           03 WKS-FEC-INID PIC X(09).
           03 FILLER       PIC X(02) VALUE SPACES.
           03 WKS-FEC-TERD PIC X(09).
           03 FILLER       PIC X(02) VALUE SPACES.
           03 WKS-ING-MEND PIC ZZZ,ZZ9.99.
           03 FILLER       PIC X(03) VALUE SPACES.
           03 WKS-ING-TOT1 PIC ZZZZ,ZZZ,ZZ9.99.
           
       01 WKS-PIE1.
           03 FILLER       PIC X(20) VALUE SPACES.
           03 FILLER       PIC X(16) VALUE
               "TOTAL NOMINA DE ".
           03 WKS-NOM-PROP PIC X(17).
           03 FILLER       PIC X(02) VALUE ": ".
           03 WKS-TOANU    PIC $$$,$$$,$$$,$$9.99.
              

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
           03 FILLER       PIC X(08) VALUE SPACES.
           03 FILLER       PIC X(12) VALUE "NOMBRE".
           03 FILLER       PIC X(01) VALUE SPACES.
           03 FILLER       PIC X(06) VALUE SPACES.
           03 FILLER       PIC X(01) VALUE SPACES.
           03 FILLER       PIC X(15) VALUE SPACES.
           03 FILLER       PIC X(03) VALUE SPACES.
           03 FILLER       PIC X(13) VALUE "IMPORTE".
           
       01 WKS-DETA.
           03 FILLER       PIC X(01) VALUE SPACES.
           03 WKS-ID-PROD  PIC X(20).
           03 FILLER       PIC X(03) VALUE SPACES.
           03 WKS-NOM-PROD PIC X(17).
           03 FILLER       PIC X(02) VALUE SPACES.
           03 FILLER       PIC X(03) VALUE SPACES.
           03 FILLER       PIC X(01) VALUE SPACES.
           03 FILLER       PIC X(13) VALUE SPACES.
           03 FILLER       PIC X(01) VALUE SPACES.
           03 WKS-ING-TOTD PIC ZZ,ZZZ,ZZ9.99.
           
       01 WKS-TOTPAR.
           03 FILLER       PIC X(29) VALUE SPACES.
           03 FILLER       PIC X(22) VALUE "TOTAL ANUALIDADES ANO ".
           03 WKS-ANOT     PIC 9999.
           03 FILLER       PIC X(01) VALUE ":".
           03 WKS-PARTOT   PIC $$,$$$,$$$,$$9.99.
           
       01 WKS-PIEA.
           03 FILLER       PIC X(12) VALUE "TOTAL PROY: ".
           03 WKS-PROY-CO  PIC 99.
           03 FILLER       PIC X(26) VALUE SPACES.
           03 FILLER       PIC X(16) VALUE "TOTAL IMPORTES: ".
           03 WKS-TOT-IMPO PIC $$$,$$$,$$$,$$9.99.
           
       01 WKS-LINEAA       PIC X(74).
           
       PROCEDURE DIVISION.
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO.
           PERFORM 3000-FIN.
           STOP RUN.
           
       1000-INICIO.
           OPEN INPUT   ARC-PRO.
           OPEN I-O     ARC-CAT.
           OPEN OUTPUT  R-NOMPRO.
           OPEN OUTPUT  R-CATPRO.
           DISPLAY SPACES ERASE.
       
       2000-PROCESO.
           PERFORM 2200-REPORTE-NOMINA.
           
           PERFORM 2600-REPORTE-CATALOGO.
       
       2200-REPORTE-NOMINA.
           SORT SOR-TEM
           ASCENDING SID-PROYECTO SFECHA-INICIO
           INPUT PROCEDURE 2201-ORDENA-ARCHIVO
           OUTPUT PROCEDURE 2210-GENERA-REPORTE.

       2201-ORDENA-ARCHIVO.
           PERFORM 2202-INICIO.
           PERFORM 2205-PROCESO UNTIL WKS-FINO1 = 1.
           PERFORM 2207-FIN.
           
       2202-INICIO.
           READ ARC-PRO
           AT END MOVE 1            TO WKS-FINO1.
           
           ACCEPT WKS-HOY FROM DATE.

       2205-PROCESO.
           MOVE FECHA-TERMINO       TO WKS-INPUTFEC.
           IF STATUS-PROY = 01
      *     IF STATUS-PROY = 01 AND WKS-INFEC >= WKS-HOY
               MOVE REG-PRO         TO SREG-PRO
               RELEASE SREG-PRO.

           READ ARC-PRO
           AT END MOVE 1            TO WKS-FINO1.
       
       2207-FIN.
           DISPLAY "FIN DEL INGRESO AL SORT".

       2210-GENERA-REPORTE.
       
           PERFORM 2220-INICIO.
           PERFORM 2250-PROCESO UNTIL WKS-FINR1 = 1.
           PERFORM 2270-FIN.
       
       2220-INICIO.
       
           RETURN SOR-TEM
           AT END MOVE 1            TO WKS-FINR1.
           
       2222-GENERA-ENCABEZADO.
           ADD 1                    TO WKS-NUM-PAG.
           MOVE SPACES              TO LINEAN.
           MOVE WKS-ENC1            TO LINEAN.
           WRITE LINEAN AFTER PAGE.
           
           ACCEPT WKS-HOY FROM DATE.
           MOVE CORR WKS-HOY        TO WKS-ENC2.
           MOVE WKS-ENC2            TO LINEAN.
           WRITE LINEAN AFTER 2.
           
           PERFORM 2225-ID-ACT-A-NOM-PRO.
           MOVE WKS-TEM-NOMPRO      TO WKS-NOM-PROE.
           MOVE WKS-ENC3            TO LINEAN.
           WRITE LINEAN AFTER 2.
           
           MOVE WKS-ENC4 TO LINEAN.
           WRITE LINEAN AFTER 2.
           
           MOVE 7                   TO WKS-CONT-LIN.
           
       2225-ID-ACT-A-NOM-PRO.
           SET WKS-J TO 1
           SEARCH T-PROYECTO VARYING WKS-J
           AT END DISPLAY "NO EXISTE PROYECTO"
           WHEN T-IDP (WKS-J) = SID-PROYECTO
           MOVE T-NOP (WKS-J)      TO WKS-TEM-NOMPRO.
           
       
       2250-PROCESO.
           MOVE SNOMBRE-CONSULTOR  TO WKS-NOM-CONS.

           MOVE SFECHA-INICIO      TO WKS-FEC-SPLIT.
           PERFORM 2252-FORMATEAR-FECHA.
           MOVE WKS-FORM-FEC       TO WKS-FEC-INID.
           MOVE WKS-SPANO          TO WKS-CHECK-FEC.

           MOVE SFECHA-TERMINO     TO WKS-FEC-SPLIT.
           PERFORM 2252-FORMATEAR-FECHA.
           MOVE WKS-FORM-FEC       TO WKS-FEC-TERD.

           MOVE SINGRESO-MENSUAL   TO WKS-ING-MEND.
           
      *----------CAL-ING------------
      
           MOVE SFECHA-INICIO      TO WKS-FEC-INICIO.
           MOVE SFECHA-TERMINO     TO WKS-FEC-TERMIN.
           MOVE SINGRESO-MENSUAL   TO WKS-ING-MENSUA.

           CALL "CAL-ING" USING WKS-LINK-DATA.
      *     DISPLAY SNOMBRE-CONSULTOR.
           
           
           MOVE WKS-ING-TOTALD     TO WKS-ING-TOT1.
           
           
      *     DISPLAY WKS-ING-TOTALD.
      *     DISPLAY WKS-SUM-CUFE.
      *     DISPLAY WKS-SUM-TNP.
      *       ACCEPT WKS-ESPERA.
      *--------------------------------------------------
      
      *-----------EXPERIMENTOS CON EL CORTE POR FECHA-------
      *
           IF WKS-CHECK-FEC IS NOT EQUAL WKS-FEC-INCUT OR 
               SID-PROYECTO IS NOT EQUAL WKS-IDR1CUT
               
               MOVE WKS-FEC-INCUT  TO WKS-ANOT
               MOVE WKS-CHECK-FEC  TO WKS-FEC-INCUT
               PERFORM 2257-ESCRIBE-CORTE-FECHA.
           
           IF SID-PROYECTO = WKS-IDR1CUT
               NEXT SENTENCE
           ELSE
               PERFORM 2255-GENERA-PIE
               
               PERFORM 2222-GENERA-ENCABEZADO
               PERFORM 2259-ACTUALIZA-ARC-CAT
               MOVE SID-PROYECTO   TO WKS-IDR1CUT
               
               INITIALIZE WKS-SUM-TNP WKS-SUM-CUFE. 
               
           ADD WKS-ING-TOTALD     TO WKS-SUM-TNP.
           
           ADD WKS-ING-TOTALD     TO WKS-SUM-CUFE.
           
           
           MOVE WKS-DET1 TO LINEAN.
           WRITE LINEAN AFTER 1.
           
           RETURN SOR-TEM
           AT END MOVE 1           TO WKS-FINR1.

       2252-FORMATEAR-FECHA.
           MOVE WKS-SPDIA          TO WKS-DAY.
           SET WKS-I               TO 1 
           SEARCH T-MESE VARYING WKS-I
           AT END DISPLAY "NO EXISTE MES"
           WHEN T-IDM (WKS-I) = WKS-SPMES
           MOVE T-MES (WKS-I)      TO WKS-MONTH.
           MOVE WKS-SPANO          TO WKS-YEAR.
           
       2255-GENERA-PIE.
           PERFORM 2225-ID-ACT-A-NOM-PRO.
           MOVE WKS-NOM-PROE TO WKS-NOM-PROP.
           MOVE WKS-SUM-TNP TO WKS-TOANU.
           
           IF WKS-NUM-PAG = 0
               NEXT SENTENCE
           ELSE
               
               MOVE WKS-PIE1 TO LINEAN
               WRITE LINEAN AFTER 2.
       
       2257-ESCRIBE-CORTE-FECHA.
           IF WKS-NUM-PAG = 0
               NEXT SENTENCE
           ELSE
      *         MOVE WKS-SUM-TNP    TO WKS-PARTOT
               MOVE WKS-SUM-CUFE   TO WKS-PARTOT
               MOVE WKS-TOTPAR     TO LINEAN
               WRITE LINEAN AFTER 1
               INITIALIZE WKS-SUM-CUFE.
       
       2259-ACTUALIZA-ARC-CAT.
           MOVE 0 TO WKS-F-RWRT.
           MOVE WKS-IDR1CUT       TO ID-PROYECTO.
           READ ARC-CAT
               INVALID KEY
               MOVE WKS-NOM-PROP   TO NOMBRE-PROY
               MOVE WKS-TOANU    TO IMP-TOTAL
               DISPLAY "WRITE" ID-PROYECTO NOMBRE-PROY IMP-TOTAL
               WRITE REG-CAT
               MOVE 1 TO WKS-F-RWRT.

           IF WKS-F-RWRT = 0
		       MOVE WKS-NOM-PROP       TO NOMBRE-PROY
               MOVE WKS-TOANU        TO IMP-TOTAL
               DISPLAY "REWRITE" ID-PROYECTO NOMBRE-PROY IMP-TOTAL
               REWRITE REG-CAT.
			   DISPLAY SPACES.
               
       2270-FIN.
           MOVE WKS-FEC-INCUT  TO WKS-ANOT.
           MOVE WKS-CHECK-FEC  TO WKS-FEC-INCUT.
           PERFORM 2257-ESCRIBE-CORTE-FECHA.
           PERFORM 2255-GENERA-PIE.
           PERFORM 2259-ACTUALIZA-ARC-CAT.
           DISPLAY "TERMINO EL PROCESO DE REPORTE 1".

      *----------------SEGUNDO-REPOTE---------------*

       2600-REPORTE-CATALOGO.
           PERFORM 2620-INICIO.
           PERFORM 2650-PROCESO UNTIL WKS-FINR2 = 1.
           PERFORM 2680-FIN.
           
       2620-INICIO.
           MOVE SPACES TO NOMBRE-PROY.
           START ARC-CAT KEY IS GREATER THAN NOMBRE-PROY
           INVALID KEY MOVE 1 TO WKS-FINR2.
           
           READ ARC-CAT
           NEXT
           AT END MOVE 1 TO WKS-FINR2.
           
           PERFORM 2622-GENERA-ENCABEZADO.
           
       2622-GENERA-ENCABEZADO.
           ADD 1              TO WKS-NUM-PAGA.
           MOVE SPACES        TO LINEAC.
           MOVE WKS-ENCA      TO LINEAC.
           WRITE LINEAC AFTER PAGE.
           
           ACCEPT WKS-HOY-TBM FROM DATE.
           SET WKS-I TO 1 
           SEARCH T-MESE VARYING WKS-I
           AT END DISPLAY "NO EXISTE MES"
           WHEN T-IDM (WKS-I) = WKS-MESTBM
           
           MOVE T-MES (WKS-I) TO WKS-MESEB.
           
           MOVE CORR WKS-HOY  TO WKS-ENCB.
           MOVE WKS-ENCB      TO LINEAC.
           WRITE LINEAC AFTER 2.
           
           MOVE WKS-ENCC      TO LINEAC.
           WRITE LINEAC AFTER 2.
           
           MOVE 5             TO WKS-CONT-LIN.
       
       2650-PROCESO.
           
           MOVE ID-PROYECTO   TO WKS-ID-PROD.
           MOVE NOMBRE-PROY   TO WKS-NOM-PROD.
           MOVE IMP-TOTAL     TO WKS-ING-TOTD.
           
           IF WKS-CONT-LIN > 60
               PERFORM 2622-GENERA-ENCABEZADO.
           
           MOVE WKS-DETA      TO LINEAC
           WRITE LINEAC AFTER 1
           ADD 1              TO WKS-PROY-CO.
           
           ADD IMP-TOTAL      TO WKS-IMP-FINAL.
           
           READ ARC-CAT
           NEXT
           AT END MOVE 1      TO WKS-FINR2.
       
       2680-FIN.
           MOVE ALL "-" TO WKS-LINEAA.
           MOVE WKS-LINEAA TO LINEAC.
           WRITE LINEAC AFTER 1.
           
           MOVE WKS-IMP-FINAL TO WKS-TOT-IMPO.
           MOVE WKS-PIEA      TO LINEAC.
           WRITE LINEAC AFTER 1.
       
       3000-FIN.
              
      *    CLOSE ARC-PRO  WITH SAVE.
      *    CLOSE ARC-CAT  WITH SAVE.
      *    CLOSE R-NOMPRO WITH SAVE.
      *    CLOSE R-CATPRO WITH SAVE.

           CLOSE ARC-PRO.  
           CLOSE ARC-CAT. 
           CLOSE R-NOMPRO. 
           CLOSE R-CATPRO. 
           
           DISPLAY "TERMINO EL PROCESO DE REPORTE 2".
           DISPLAY "EL PROGRAMA TERMINO CON EXITO.".
           ACCEPT WKS-ESPERA.