       IDENTIFICATION DIVISION.
       PROGRAM-ID. LINK-CAT.
       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 WKS-DATOS.
           03 FILLER     PIC X(12) VALUE "1DEBITO 1000".
           03 FILLER     PIC X(12) VALUE "2CREDITO3500".
           03 FILLER     PIC X(12) VALUE "3NOMINA 0000".
           
       01 T-DATOS REDEFINES WKS-DATOS.
           03 T-DAT OCCURS 3 TIMES
               ASCENDING KEY IS T-CLA INDEXED BY WKS-I.
               05 T-CLA PIC 9.
               05 T-NOM PIC X(07).
               05 T-ANU PIC 9999.
       
       LINKAGE SECTION.
       01 REG-CLI.
           03 ID-CLIENTE PIC 9(03).
           03 NOM-CLI    PIC X(30).
           03 CLA-TAR    PIC 9.
           03 NOM-TAR    PIC X(20).
           03 ANUA       PIC 9(05)V99.
           03 FEC-ING    PIC 9(08).
       
       PROCEDURE DIVISION 
           USING REG-CLI.
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO.
           PERFORM 3000-FIN.
           EXIT PROGRAM.
         
       1000-INICIO.
           ACCEPT CLA-TAR LINE 12 POSITION 33.
       
       2000-PROCESO.
           SEARCH ALL T-DAT
           AT END DISPLAY "NO EXISTE" LINE 12 POSITION 05
           WHEN T-CLA (WKS-I) = CLA-TAR
           MOVE T-NOM (WKS-I) TO NOM-TAR
           MOVE T-ANU (WKS-I) TO ANUA.
       
       3000-FIN.