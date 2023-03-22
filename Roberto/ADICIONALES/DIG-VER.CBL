       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIG-VER.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *---------------TABLAS----------------* 
       01 WKS-LINEA.
           03 WKS-NO-REC           PIC X(12) VALUE "01AA93050001".
           03 WKS-PI               PIC 999.
           03 WKS-PF               PIC 999.
           
       01 T-LINEA REDEFINES WKS-LINEA.
           03 T-LIN OCCURS 18 TIMES PIC X.
       
       01 WKS-ALFA.
           03 FILLER               PIC X VALUE "A".
           03 FILLER               PIC X VALUE "B".
           03 FILLER               PIC X VALUE "C".
           03 FILLER               PIC X VALUE "D".
           03 FILLER               PIC X VALUE "E".
           03 FILLER               PIC X VALUE "F".
           03 FILLER               PIC X VALUE "G".
           03 FILLER               PIC X VALUE "H".
           03 FILLER               PIC X VALUE "I".
           03 FILLER               PIC X VALUE "J".
           03 FILLER               PIC X VALUE "K".
           03 FILLER               PIC X VALUE "L".
           03 FILLER               PIC X VALUE "M".
           03 FILLER               PIC X VALUE "N".
           03 FILLER               PIC X VALUE "O".
           03 FILLER               PIC X VALUE "P".
           03 FILLER               PIC X VALUE "Q".
           03 FILLER               PIC X VALUE "R".
           03 FILLER               PIC X VALUE "S".
           03 FILLER               PIC X VALUE "T".
           03 FILLER               PIC X VALUE "U".
           03 FILLER               PIC X VALUE "V".
           03 FILLER               PIC X VALUE "W".
           03 FILLER               PIC X VALUE "X".
           03 FILLER               PIC X VALUE "Y".
           03 FILLER               PIC X VALUE "Z".
           
       01 T-ALFA REDEFINES WKS-ALFA.
           03 T-ALF OCCURS 26 TIMES PIC X.
           
       01 WKS-NUMERIC.
           03 FILLER               PIC 99 VALUE 10.
           03 FILLER               PIC 99 VALUE 11.
           03 FILLER               PIC 99 VALUE 12.
           03 FILLER               PIC 99 VALUE 13.
           03 FILLER               PIC 99 VALUE 14.
           03 FILLER               PIC 99 VALUE 15.
           03 FILLER               PIC 99 VALUE 16.
           03 FILLER               PIC 99 VALUE 17.
           03 FILLER               PIC 99 VALUE 18.
           03 FILLER               PIC 99 VALUE 19.
           03 FILLER               PIC 99 VALUE 20.
           03 FILLER               PIC 99 VALUE 21.
           03 FILLER               PIC 99 VALUE 22.
           03 FILLER               PIC 99 VALUE 23.
           03 FILLER               PIC 99 VALUE 24.
           03 FILLER               PIC 99 VALUE 25.
           03 FILLER               PIC 99 VALUE 26.
           03 FILLER               PIC 99 VALUE 27.
           03 FILLER               PIC 99 VALUE 28.
           03 FILLER               PIC 99 VALUE 29.
           03 FILLER               PIC 99 VALUE 30.
           03 FILLER               PIC 99 VALUE 31.
           03 FILLER               PIC 99 VALUE 32.
           03 FILLER               PIC 99 VALUE 33.
           03 FILLER               PIC 99 VALUE 34.
           03 FILLER               PIC 99 VALUE 35.
           
       01 T-NUMERIC REDEFINES WKS-NUMERIC.
           03 T-NUME OCCURS 26 TIMES PIC 99.
           
       01 WKS-PONDERA.
           03 FILLER               PIC 99 VALUE 11.
           03 FILLER               PIC 99 VALUE 13.
           03 FILLER               PIC 99 VALUE 17.
           03 FILLER               PIC 99 VALUE 19.
           03 FILLER               PIC 99 VALUE 23.
           
       01 T-PONDERA REDEFINES WKS-PONDERA.
           03 T-POND OCCURS 5 TIMES PIC 99.
           
      *---------------VARIABLES-----------------*
       77 WKS-I                    PIC 99.
       77 WKS-J                    PIC 99.
       77 WKS-K                    PIC 99.
       77 WKS-L                    PIC 99.
      
       01 WKS-PE-INI.
           03 FILLER               PIC 99.
           03 WKS-PI-C             PIC 999.
       
       01 WKS-PE-FIN.
           03 FILLER               PIC 99.
           03 WKS-PF-C             PIC 999.
           
       01 WKS-COERCION             PIC 9.
       01 WKS-ESLETRA              PIC 9.
       01 WKS-MRES                 PIC 9999.
       01 WKS-SUMRES               PIC 999999.
	   
	   
	   01 WKS-CO                   PIC 999V99.
	   01 WKS-COCIENTE.
	       03 WKS-COS              PIC 999.
		   03 FILLER               PIC V99.
	
	   01 WKS-REMAN                PIC 99.
	   
	   01 WKS-FINAL-L.
	       03 WKS-LINEA-C          PIC X(18).
		   03 WKS-REMAN-C          PIC 99.
       
       PROCEDURE DIVISION.
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO.
           PERFORM 3000-FIN.
           STOP RUN.
           
       1000-INICIO.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           DISPLAY "NUMERO DE RECIBO:" LINE 5 POSITION 5.
      *       ACCEPT WKS-NO-REC LINE 5 POSITION 35.
           DISPLAY "PERIODO INICIAL (99999):" LINE 6 POSITION 5.
           ACCEPT WKS-PE-INI LINE 6 POSITION 35.
           DISPLAY "PERIODO FINAL (99999):" LINE 7 POSITION 5.
           ACCEPT WKS-PE-FIN LINE 7 POSITION 35.
           
       2000-PROCESO.
           MOVE WKS-PI-C TO WKS-PI.
           MOVE WKS-PF-C TO WKS-PF.
           
           PERFORM 2100-MULTIPLICAR-FACTOR VARYING WKS-I
               FROM 18 BY -1 UNTIL WKS-I < 1.
			   
		   DIVIDE WKS-SUMRES BY 97 GIVING WKS-CO.
		   MOVE WKS-CO TO WKS-COCIENTE.
		   COMPUTE WKS-REMAN = WKS-SUMRES - WKS-COS * 97.
		   
		   ADD 1 TO WKS-REMAN.
		   
		   MOVE WKS-REMAN TO WKS-REMAN-C.
		   
		   MOVE WKS-LINEA TO WKS-LINEA-C.
               
           DISPLAY WKS-FINAL-L LINE 10 POSITION 10.
       
       2100-MULTIPLICAR-FACTOR.
           INITIALIZE WKS-ESLETRA.
           
           IF WKS-L > 4
               MOVE 0 TO WKS-L.
           ADD 1 TO WKS-L.
           
           
           PERFORM 2120-ES-LETRA VARYING WKS-J 
               FROM 1 BY 1 UNTIL WKS-J > 26.
           IF WKS-ESLETRA = 1
               PERFORM 2140-MULTIPLICAR-LETRA VARYING WKS-K
                   FROM 1 BY 1 UNTIL WKS-K > 26
           ELSE
               MOVE T-LIN (WKS-I) TO WKS-COERCION
               MULTIPLY WKS-COERCION BY T-POND(WKS-L) 
                   GIVING WKS-MRES
               ADD WKS-MRES TO WKS-SUMRES.
       
       
       2120-ES-LETRA.
           IF T-LIN (WKS-I) = T-ALF (WKS-J)
               MOVE 1 TO WKS-ESLETRA.
       
       2140-MULTIPLICAR-LETRA.
           IF T-LIN (WKS-I) = T-ALF (WKS-K)
               MULTIPLY T-NUME (WKS-K) BY T-POND (WKS-L)
                  GIVING WKS-MRES
               ADD WKS-MRES TO WKS-SUMRES.
       
       3000-FIN.   
       
       
       
       
       
       