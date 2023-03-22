       IDENTIFICATION DIVISION.
       PROGRAM-ID. T-VTAS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WKS-TOVEN  PIC 9(07)V99.
       01 WKS-PRO    PIC 9(06)V99.
       
       01 WKS-PRO-F  PIC $$$$,$$9.99.
       
       77 WKS-I      PIC 99.
	   77 WKS-J      PIC 99.
        
       01 WKS-MESES.
           03 FILLER PIC X(10) VALUE "ENERO".
           03 FILLER PIC X(10) VALUE "FEBRERO".
           03 FILLER PIC X(10) VALUE "MARZO".
           03 FILLER PIC X(10) VALUE "ABRIL".
           03 FILLER PIC X(10) VALUE "MAYO".
           03 FILLER PIC X(10) VALUE "JUNIO".
           03 FILLER PIC X(10) VALUE "JULIO".
           03 FILLER PIC X(10) VALUE "AGOSTO".
           03 FILLER PIC X(10) VALUE "SEPTIEMBRE".
           03 FILLER PIC X(10) VALUE "OCTUBRE".
           03 FILLER PIC X(10) VALUE "NOVIEMBRE".
           03 FILLER PIC X(10) VALUE "DICIEMBRE".
       
       01 T-MESES REDEFINES WKS-MESES.
           03 T-MES OCCURS 12 TIMES PIC X(10).
           
       01 T-VENTAS.
           03 T-VENTA OCCURS 12 TIMES PIC 9(06)V99.
           
       PROCEDURE DIVISION.
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO.
           PERFORM 3000-FIN.
		   STOP RUN.
           
       1000-INICIO.
           DISPLAY SPACES LINE 01 POSITION 01 ERASE.
       
           PERFORM 1100-LLENA-TABLA VARYING WKS-I 
               FROM 1 BY 1 UNTIL WKS-I > 12.
               
      *    INITIALIZE WKS-I.
               
       1100-LLENA-TABLA.
           
           DISPLAY "INTRODUCE VENTAS DE " LINE WKS-I POSITION 10.
           DISPLAY T-MES (WKS-I) LINE WKS-I POSITION 30.
           ACCEPT T-VENTA (WKS-I) LINE WKS-I POSITION 45.
           
       
       2000-PROCESO.
       
           PERFORM 2100-EXTRAER VARYING WKS-J 
               FROM 1 BY 1 UNTIL WKS-J > 12.
               
           DIVIDE WKS-TOVEN BY WKS-J GIVING WKS-PRO.
           MOVE WKS-PRO TO WKS-PRO-F.
           
               
       2100-EXTRAER.
           ADD T-VENTA (WKS-J) TO WKS-TOVEN.
       
       3000-FIN.
	   
	       ADD 2 TO WKS-J.
      *     DISPLAY SPACES LINE 01 POSITION 01 ERASE.
           DISPLAY "EL PROMEDIO DE VENTAS ANUAL ES: "
               LINE WKS-J POSITION 10.
           DISPLAY WKS-PRO-F LINE WKS-J POSITION 45.
       
       
       