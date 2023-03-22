       IDENTIFICATION DIVISION.
       PROGRAM-ID. TBID-HOR.
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *----------------TABLAS----------------------*
       
       01 WKS-DIAS.
           03 FILLER    PIC X(09) VALUE "LUNES".
           03 FILLER    PIC X(09) VALUE "MARTES".
           03 FILLER    PIC X(09) VALUE "MIERCOLES".
           03 FILLER    PIC X(09) VALUE "JUEVES".
           03 FILLER    PIC X(09) VALUE "VIERNES".
       
       01 T-DIAS REDEFINES WKS-DIAS.
           03 T-DI OCCURS 5 TIMES PIC X(09).
       
       01 WKS-MATERIAS.
           03 FILLER    PIC XXXX VALUE "SO".
           03 FILLER    PIC XXXX VALUE "BD".
           03 FILLER    PIC XXXX VALUE "RDS".
           03 FILLER    PIC XXXX VALUE "IA".
       
       01 T-MATERIAS REDEFINES WKS-MATERIAS.
           03 T-MA OCCURS 4 TIMES PIC XXXX.
       
       
       01 T-HORARIOS.
           03 T-MATERIAS OCCURS 4 TIMES.
               05 T-DIAS OCCURS 5 TIMES.
                   07 T-HORA PIC 9999.
                   
      *------------------VARIABLES----------------*
       
       77 WKS-I         PIC 9.
       77 WKS-J         PIC 9.
       77 WKS-K         PIC 9.
       77 WKS-L         PIC 9.
       
      *-----------------LATERAL-------------------* 
       01 WKS-LIN-S     PIC 99 VALUE 5.
       01 WKS-POS-S     PIC 99 VALUE 5.
       01 WKS-DISMAS    PIC 99.
       
      *-----------------CABECERA------------------*

       01 WKS-LIN-H     PIC 99  VALUE 5.
       01 WKS-POS-H     PIC 999.
       01 WKS-DISMAS2   PIC 99.

      
       PROCEDURE DIVISION.
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO.
           PERFORM 3000-FIN.
           STOP RUN.
           
       1000-INICIO.
           DISPLAY SPACES ERASE.
       
       2000-PROCESO.
           PERFORM 2100-LLENA-TABLA 
               VARYING WKS-I FROM 1 BY 1 UNTIL WKS-I > 4
               AFTER WKS-J FROM 1 BY 1 UNTIL WKS-J > 5.
               
           DISPLAY SPACES ERASE.
           
           PERFORM 2200-DESPLIEGA-TABLA
               VARYING WKS-K FROM 1 BY 1 UNTIL WKS-K > 4
               AFTER WKS-L FROM 1 BY 1 UNTIL WKS-L > 5.
       
       2100-LLENA-TABLA.
         
           DISPLAY "INTRODUCE HORARIO DE " T-MA (WKS-I) T-DI (WKS-J)
                ":".
           ACCEPT T-HORA (WKS-I WKS-J).
               
       
       2200-DESPLIEGA-TABLA.
       
	   
           DISPLAY T-MA (WKS-K) ", " T-DI (WKS-L) " ES " 
		       T-HORA (WKS-K WKS-L).
       
	   
	   
       
       COMENTADO.    
      *-------------LATERAL1-----------------*     
           
           IF WKS-LIN-S > 8
               MOVE 5 TO WKS-LIN-S.
               
           ADD 1 TO WKS-LIN-S.
           
           IF WKS-DISMAS > 3
               MOVE 0 TO WKS-DISMAS.
           
           ADD 1 TO WKS-DISMAS.
           
           DISPLAY T-MA (WKS-DISMAS) LINE WKS-LIN-S POSITION WKS-POS-S.
           
      *-------------CABECERA1-----------------*
           IF WKS-POS-H > 50
               MOVE 0 TO WKS-POS-H.
           
           ADD 12 TO WKS-POS-H.
           
           IF WKS-DISMAS2 > 4
               MOVE 0 TO WKS-DISMAS2.
           
           ADD 1 TO WKS-DISMAS2.
           
           DISPLAY T-DI (WKS-DISMAS2) 
               LINE WKS-LIN-H POSITION WKS-POS-H.
           
       3000-FIN.
       
       
       
       
       
       
       
       