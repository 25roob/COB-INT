       IDENTIFICATION DIVISION.
       PROGRAM-ID. T-ANUAL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES     ASSIGN TO DISK.
           SELECT REP-ANU2     ASSIGN TO PRINTER.

       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES.
       01 REG-CLI.
           03 ID-CLIENTE        PIC 9(03).
           03 NOM-CLI           PIC X(30).
           03 CLA-TAR           PIC 9.
           03 NOM-TAR           PIC X(20).
           03 ANUA              PIC 9(05)V99.
           03 FEC-ING           PIC 9(08).
       
       FD REP-ANU2.
       01 LINEA                 PIC X(132).
       
       
       WORKING-STORAGE SECTION.
      
      *-------------- TABLAS -------------------

      
      * 01 WKS-TARJETAS.
      *     03 FILLER PIC X(07) VALUE "CREDITO".
      *     03 FILLER PIC X(07) VALUE "DEBITO".
      *     03 FILLER PIC X(07) VALUE "NOMINA".
  
  
       01 T-TARJETAS.
           03 T-TAR   OCCURS 3 TIMES PIC X(07).

       01 T-ANUALIDADES.
           03 T-ANUA  OCCURS 3 TIMES PIC 9(07).       
       
        
      *--------------   VARIABLES DE TRABAJO  -------------------

       01 TODAY.
           03 THIS-MONTH        PIC 99.
           03 THIS-DAY          PIC 99.
           03 THIS-YEAR         PIC 99.

       01 CURRENT-DATE.
           03 THIS-MONTH        PIC 99.
           03 FILLER            PIC X VALUE "/".
           03 THIS-DAY          PIC 99.
           03 FILLER            PIC X VALUE "/".
           03 THIS-YEAR         PIC 99.
        
       01 WKS-CONT-LIN          PIC 99.
       01 WKS-FIN               PIC XX.
       01 WKS-TOTAL-ANU         PIC 9(05).
       
       01 WKS-I                 PIC 99.
       01 WKS-J                 PIC 99.
       01 WKS-BUSCADOR          PIC 99.
       

      *--------------VARIABLES DE REPORTE TARJETAS--------------------
      
       01  WKS-ENC-1-TAR.

           03 FILLER                PIC X(13) VALUE SPACES.
           03 FILLER                PIC X(35)
                     VALUE "REPORTE DE ANUALIDADES".


       01 WKS-ENC-2-TAR.
           03 FILLER                PIC X(07) VALUE "FECHA: ".
           03 WKS-DATE-TAR          PIC X(10).
           03 FILLER                PIC X(24) VALUE SPACES.
           03 FILLER                PIC X(07) VALUE "PAG. : ".
           03 WKS-NUM-PAG-TAR       PIC 9(02).


       01  WKS-ENC-3-TAR.
           03 FILLER                PIC X(15)
                     VALUE "TIPO TARJETA ".
           03 FILLER                PIC X(17) VALUE SPACES. 
           03 FILLER                PIC X(10)
                     VALUE "ANUALIDAD".

           
       01  WKS-DET-1-TAR.
           03 FILLER                PIC X(03) VALUE SPACES.
           03 WKS-TIPO-TAR          PIC X(10).
           03 FILLER                PIC X(20) VALUE SPACES.
           03 WKS-ANUALIDAD         PIC 9(05)V99.
           
           
       01  WKS-RAYA-PIE             PIC X(50).
                       
       
       01 WKS-TOT-ANU               PIC 9(07). 
       01  WKS-PIE-TOT-ANU.
           03 FILLER                PIC X(26) VALUE SPACES.
           03 FILLER                PIC X(07) 
                      VALUE "TOTAL: ".
           03 WKS-TOT-ANU-E         PIC $99,999.99. 
           
           
           
       PROCEDURE DIVISION.
       INICIO.
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO UNTIL WKS-FIN = 1.
           PERFORM 3000-FIN.
           STOP RUN.

       1000-INICIO.
           OPEN INPUT CLIENTES.
           OPEN OUTPUT REP-ANU2.
           
            READ CLIENTES
                AT END 
                    MOVE 1 TO WKS-FIN.   

       
           PERFORM 1100-GENERA-ENCABEZADO.    
           
           
       1100-GENERA-ENCABEZADO.   
       
           ADD  1                         TO WKS-NUM-PAG-TAR.
           MOVE SPACES                    TO  LINEA.
           MOVE WKS-ENC-1-TAR             TO  LINEA.
           WRITE  LINEA AFTER PAGE.   

           ACCEPT TODAY FROM DATE.
           MOVE CORR TODAY TO CURRENT-DATE.
           MOVE CURRENT-DATE TO WKS-DATE-TAR.
           MOVE WKS-ENC-2-TAR             TO  LINEA.
           WRITE  LINEA AFTER 2.
           
           MOVE WKS-ENC-3-TAR             TO  LINEA.
           WRITE  LINEA AFTER 2.
           MOVE 4                         TO WKS-CONT-LIN.   
           

       2000-PROCESO.   
                    
       
           PERFORM 2100-LLENA-TABLA VARYING WKS-I FROM 1 BY 1
                                             UNTIL WKS-I > 3.
                                             
           READ CLIENTES
           AT END 
           MOVE 1 TO WKS-FIN.
                                                                                                                       
                                                                                                                         
       2100-LLENA-TABLA.
                
           IF T-TAR (WKS-I) = SPACES
		      MOVE NOM-TAR TO T-TAR (WKS-I)
			  ADD ANUA TO T-ANUA (WKS-I)
			  ADD 5 TO WKS-I
           
           ELSE IF T-TAR (WKS-I) = WKS-BUSCADOR
              ADD ANUA TO T-ANUA (WKS-I)
			  ADD 5 TO WKS-I.


                     
                    
               
               
       2200-EXTRAER-TABLA.
       
           MOVE T-ANUA (WKS-J) TO WKS-ANUALIDAD.
           MOVE T-TAR (WKS-J) TO WKS-TIPO-TAR.
           MOVE WKS-DET-1-TAR  TO LINEA.
           WRITE LINEA AFTER 1.
           ADD T-ANUA (WKS-J)  TO WKS-TOT-ANU.       
               

               
               
                   
               
       3000-FIN.
       
           PERFORM 2200-EXTRAER-TABLA VARYING WKS-J FROM 1 BY 1
                                             UNTIL WKS-J > 3.
                                             
                                                 
                      
           MOVE ALL "_"         TO WKS-RAYA-PIE.
           MOVE WKS-RAYA-PIE    TO LINEA.
           WRITE LINEA AFTER 1.    

          
           MOVE WKS-TOT-ANU     TO WKS-TOT-ANU-E.
           MOVE WKS-PIE-TOT-ANU TO LINEA.
           WRITE LINEA AFTER 1.
           
           CLOSE CLIENTES.
           CLOSE REP-ANU2.    
                  
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               

               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               

               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               

               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
                 
               
    
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
            
       
           