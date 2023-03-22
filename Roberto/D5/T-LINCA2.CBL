       IDENTIFICATION DIVISION.
       PROGRAM-ID. T-LINCA2.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *---------------TABLAS----------------*  
       01 WKS-ALF-NUM.
           03 FILLER               PIC Xxx VALUE " 00".
           03 FILLER               PIC Xxx VALUE "A01".
           03 FILLER               PIC Xxx VALUE "B02".
           03 FILLER               PIC Xxx VALUE "C03".
           03 FILLER               PIC Xxx VALUE "D04".
           03 FILLER               PIC Xxx VALUE "E05".
           03 FILLER               PIC Xxx VALUE "F06".
           03 FILLER               PIC Xxx VALUE "G07".
           03 FILLER               PIC Xxx VALUE "H08".
           03 FILLER               PIC Xxx VALUE "I09".
           03 FILLER               PIC Xxx VALUE "J10".
           03 FILLER               PIC Xxx VALUE "K11".
           03 FILLER               PIC Xxx VALUE "L12".
           03 FILLER               PIC Xxx VALUE "M13".
           03 FILLER               PIC Xxx VALUE "N14".
           03 FILLER               PIC Xxx VALUE "O15".
           03 FILLER               PIC Xxx VALUE "P16".
           03 FILLER               PIC Xxx VALUE "Q17".
           03 FILLER               PIC Xxx VALUE "R18".
           03 FILLER               PIC Xxx VALUE "S19".
           03 FILLER               PIC Xxx VALUE "T20".
           03 FILLER               PIC Xxx VALUE "U21".
           03 FILLER               PIC Xxx VALUE "V22".
           03 FILLER               PIC Xxx VALUE "W23".
           03 FILLER               PIC Xxx VALUE "X24".
           03 FILLER               PIC Xxx VALUE "Y25".
           03 FILLER               PIC Xxx VALUE "Z26".
           
       01 T-ALF-NUM REDEFINES WKS-ALF-NUM.
           03 T-AL-NU OCCURS 27 TIMES.
               05 T-ALF PIC X.
               05 T-NUM PIC 99.
               
       01 WKS-CALEN.
           03 FILLER    PIC X(12) VALUE "ENERO     31".
           03 FILLER    PIC X(12) VALUE "FEBRERO   28".
           03 FILLER    PIC X(12) VALUE "MARZO     31".
           03 FILLER    PIC X(12) VALUE "ABRIL     30".
           03 FILLER    PIC X(12) VALUE "MAYO      31".
           03 FILLER    PIC X(12) VALUE "JUNIO     30".
           03 FILLER    PIC X(12) VALUE "JULIO     31".
           03 FILLER    PIC X(12) VALUE "AGOSTO    31".
           03 FILLER    PIC X(12) VALUE "SEPTIEMBRE30".
           03 FILLER    PIC X(12) VALUE "OCTUBRE   31".
           03 FILLER    PIC X(12) VALUE "NOVIEMBRE 30".
           03 FILLER    PIC X(12) VALUE "DICIEMBRE 31".
       
       01 T-CALEN REDEFINES WKS-CALEN.
           03 T-CAL OCCURS 12 TIMES.
               05 T-MES PIC X(10).
               05 T-DIA PIC 99.
           
      *---------------VARIABLES----------------* 
             
       77 WKS-I        PIC 99.
       77 WKS-J        PIC 99.
       77 WKS-ESC      PIC 9.
       
       77 WKS-INIZ     PIC 99.
       77 WKS-INDE     PIC 99.
       
       01 WKS-LINEA.
           03 FILLER   PIC 9(6).
           03 WKS-LI   PIC X.
           03 WKS-LD   PIC X.
           03 FILLER   PIC 9999.
       
       01 WKS-RESI     PIC 99.
       01 WKS-SUMA     PIC 999.
       01 WKS-MULT     PIC 999.
      
       01 WKS-ACO      PIC 999.
       01 WKS-DACT     PIC 99.
       
       PROCEDURE DIVISION.
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO.
           PERFORM 3000-FIN.
           STOP RUN.
           
       1000-INICIO.
           DISPLAY SPACES ERASE.
           DISPLAY "INTRODUCE LINEA DE CAPTURA:" LINE 05 POSITION 05.
           ACCEPT WKS-LINEA LINE 05 POSITION 45.
       
       2000-PROCESO.
           PERFORM 2100-LETRAS-DIGITO VARYING WKS-I
               FROM 1 BY 1 UNTIL WKS-I > 26.
               
           COMPUTE WKS-SUMA = (WKS-INIZ - 1) * 26 + WKS-INDE.
		   
           PERFORM 2200-DIGITO-FECHA VARYING WKS-J
               FROM 1 BY 1 UNTIL WKS-J > 12.
           
       2100-LETRAS-DIGITO.
           IF WKS-LI = T-ALF (WKS-I)
               MOVE T-NUM (WKS-I) TO WKS-INIZ
               ADD 1 TO WKS-ESC.
               
           IF WKS-LD = T-ALF (WKS-I)
               MOVE T-NUM (WKS-I) TO WKS-INDE
               ADD 1 TO WKS-ESC.
               
           IF WKS-ESC = 2
               ADD 30 TO WKS-I.
               
       2200-DIGITO-FECHA.
           ADD T-DIA (WKS-J) TO WKS-ACO.

           IF WKS-ACO GREATER THAN OR EQUAL TO WKS-SUMA
               SUBTRACT 1 FROM WKS-J GIVING WKS-RESI
               SUBTRACT T-DIA (WKS-J) FROM WKS-ACO

               SUBTRACT WKS-ACO FROM WKS-SUMA GIVING WKS-DACT
               DISPLAY "DIA DE PAGO: " LINE 9 POSITION 05
               DISPLAY WKS-DACT LINE 9 POSITION 35
               DISPLAY "MES: " LINE 10 POSITION 05
               DISPLAY T-MES (WKS-J) LINE 10 POSITION 35
               MOVE 15 TO WKS-J.
           
       3000-FIN.