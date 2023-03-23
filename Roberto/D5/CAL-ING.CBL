       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAL-ING.
       AUTHOR.     ROBERTO Q.
      *--------------------------------------------------------------
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       DATA  DIVISION.
       FILE  SECTION.
       WORKING-STORAGE            SECTION.
       
       01 WKS-MESES.
           03 FILLER           PIC 9999 VALUE 0131. 
           03 FILLER           PIC 9999 VALUE 0228.
           03 FILLER           PIC 9999 VALUE 0331.
           03 FILLER           PIC 9999 VALUE 0430.
           03 FILLER           PIC 9999 VALUE 0531.
           03 FILLER           PIC 9999 VALUE 0630.
           03 FILLER           PIC 9999 VALUE 0731.
           03 FILLER           PIC 9999 VALUE 0831.
           03 FILLER           PIC 9999 VALUE 0930.
           03 FILLER           PIC 9999 VALUE 1031.
           03 FILLER           PIC 9999 VALUE 1130.
           03 FILLER           PIC 9999 VALUE 1231.

       01 T-MESES REDEFINES WKS-MESES.
           03 T-MESE OCCURS 12 TIMES.
               05 T-MES PIC 99.
               05 T-DIA PIC 99.

       01 WKS-SPLIT-FI.
           03 WKS-ANOI         PIC 9999.
           03 WKS-MESI         PIC 99.
           03 WKS-DIAI         PIC 99.

       01 WKS-SPLIT-FT.
           03 WKS-ANOT         PIC 9999.
           03 WKS-MEST         PIC 99.
           03 WKS-DIAI         PIC 99.

       01 WKS-PAGO-FACT        PIC 9999V99.
       
       LINKAGE SECTION.
       01 WKS-LINK-DATA.
           03 WKS-FEC-INICIO   PIC 9(08).
           03 WKS-FEC-TERMIN   PIC 9(08).
           03 WKS-ING-MENSUA   PIC 9(08)V99.
           03 WKS-ING-TOTALD   PIC 9(10)V99.

       PROCEDURE  DIVISION USING WKS-LINK-DATA.
       000-INI.
           PERFORM 100-PROCESO.
           EXIT PROGRAM.

       100-PROCESO.
      *=============
      *
           MOVE WKS-FEC-INICIO TO WKS-SPLIT-FI.
           MOVE WKS-FEC-TERMIN TO WKS-SPLIT-FT.