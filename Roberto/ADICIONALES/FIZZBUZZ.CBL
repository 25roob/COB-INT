       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIZ-BUZ.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WKS-COUNTER  PIC 999.
	   01 WKS-RES3     PIC 99V99.                                       
       01 WKS-RES5     PIC 99V99. 
       01 WKS-CHECK3.
           03 WKS-INT3 PIC 99.
           03 WKS-FLO3 PIC V99.
       01 WKS-CHECK5.
           03 WKS-INT5 PIC 99.
           03 WKS-FLO5 PIC V99.   
       77 WKS-FIN      PIC 9.   
       PROCEDURE DIVISION.
       INICIO.
           PERFORM 2000-PROCESO UNTIL WKS-FIN = 1.
           STOP RUN.
       2000-PROCESO.
           ADD 1 TO WKS-COUNTER.
           COMPUTE WKS-RES5 = WKS-COUNTER / 5.
           COMPUTE WKS-RES3 = WKS-COUNTER / 3.
		   MOVE WKS-RES5 TO WKS-CHECK5.
		   MOVE WKS-RES3 TO WKS-CHECK3.
           IF WKS-FLO5 = 0 AND WKS-FLO3 = 0
               DISPLAY "FIZZ-BUZZ"
           ELSE IF WKS-FLO5 = 0
               DISPLAY "BUZZ"
           ELSE IF WKS-FLO3 = 0
               DISPLAY "FIZZ"
           ELSE 
               DISPLAY WKS-COUNTER.
           IF WKS-COUNTER = 100
               MOVE 1 TO WKS-FIN.