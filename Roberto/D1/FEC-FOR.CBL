       IDENTIFICATION DIVISION.
       PROGRAM-ID. FEC-FOR.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        
       01 WKS-FECHA.
           05 WKS-LINE01   PIC X(10).
        
       01 WKS-HOY.
           05 WKS-ANO     PIC 99.
           05 WKS-MES     PIC 99.
           05 WKS-DIA       PIC 99.
		   
	   01 TAB-MESES.
	       05 T-MES OCCURS 12 TIMES.
		       10 T-MM        PIC X(10).
		   
	   PROCEDURE DIVISION.
       INICIO.
	       PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO.
           PERFORM 3000-FIN.
           STOP RUN.
		   
	   1000-INICIO.
	       
		   MOVE "ENERO" TO T-MM (1)
		   MOVE "FEBRERO" TO T-MM (2)
		   MOVE "MARZO" TO T-MM (3)
		   MOVE "ABRIL" TO T-MM (4)
		   MOVE "MAYO" TO T-MM (5)
		   MOVE "JUNIO" TO T-MM (6)
		   MOVE "JULIO" TO T-MM (7)
		   MOVE "AGOSTO" TO T-MM (8)
		   MOVE "SEPTIEMBRE" TO T-MM (9)
		   MOVE "OCTUBRE" TO T-MM (10)
		   MOVE "NOVIEMBRE" TO T-MM (11)
		   MOVE "DICIEMBRE" TO T-MM (12)
	   
	   2000-PROCESO.
	   
	   3000-FIN.