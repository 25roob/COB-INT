       IDENTIFICATION DIVISION.
       PROGRAM-ID. SENTENC.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        
       01 WKS-FECHA.
           05 WKS-LINE01   PIC X(10).
        
       01 WKS-HOY.
           05 WKS-ANO      PIC 99.
           05 WKS-MES      PIC 99.
           05 WKS-DIA      PIC 99.
           
       01 WKS-IDENT.
           05 WKS-LINE02   PIC X(80).
           
       01 WKS-NOMBREC.
           05 WKS-NOM      PIC X(15).
           05 WKS-APE-PA   PIC X(15).
           05 WKS-APE-MA   PIC X(15).
           
       01 WKS-COUNT        PIC 99.
        
       01 WKS-NAPEPA       PIC X(15).
       01 WKS-NAPEMA       PIC    X(15).   
       
      * 01 PARA-CONV       PIC X(60).
      * 01 PARA-REPL       PIC X(60).
        
       PROCEDURE DIVISION.
       INICIO.
           
           PERFORM 1000-INICIO.
           PERFORM 2000-PROCESO.
           PERFORM 3000-FIN.
           STOP RUN.
           
       1000-INICIO.
           ACCEPT WKS-HOY FROM DATE.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           
       2000-PROCESO.
      *------------------------1--------------------*
	  
           STRING WKS-DIA DELIMITED BY SIZE
               "/" DELIMITED BY SIZE
               WKS-MES DELIMITED BY SIZE
               "/20" DELIMITED BY SIZE
               WKS-ANO DELIMITED BY SIZE
               INTO WKS-LINE01.
               
           DISPLAY WKS-FECHA LINE 05 POSITION 10.
      *-----------------------2----------------------*     
           DISPLAY "NOMBRE: " LINE 07 POSITION 10.
           ACCEPT WKS-NOM LINE 07 POSITION  40.
           DISPLAY "APELLIDO PATERNO: " LINE 08 POSITION 10.
           ACCEPT WKS-APE-PA LINE 08 POSITION 40.
           DISPLAY "APELLIDO MATERNO:" LINE 09 POSITION 10.
           ACCEPT WKS-APE-MA LINE 09 POSITION 40.
           
           STRING WKS-NOM DELIMITED BY " "
               " " DELIMITED BY SIZE
               WKS-APE-PA DELIMITED BY " "
               " " DELIMITED BY SIZE
               WKS-APE-MA DELIMITED BY " "
               INTO WKS-LINE02.
               
           DISPLAY WKS-IDENT LINE 11 POSITION 10.
        
      *-----------------------3--------------------------*		
           INSPECT WKS-IDENT TALLYING WKS-COUNT
               FOR CHARACTERS BEFORE " ".
               
           ADD 2 TO WKS-COUNT.
               
           UNSTRING WKS-IDENT DELIMITED BY " "
               INTO WKS-NAPEPA WKS-NAPEMA
               WITH POINTER WKS-COUNT.
               
           DISPLAY "APELLIDO PATERNO: " LINE 13 POSITION 10.
           DISPLAY WKS-NAPEPA LINE 13 POSITION 30.
           DISPLAY "APELLIDO MATERNO: " LINE 14 POSITION 10.
           DISPLAY WKS-NAPEMA LINE 14 POSITION 30.
      
      *----------------------4-----------------------------*	  
           INSPECT WKS-IDENT REPLACING ALL "MAR" BY "PIT".
           DISPLAY WKS-IDENT LINE 16 POSITION 10.
      
      *-------------------------5---------------------------*	  
           INSPECT WKS-IDENT CONVERTING "MAR" TO "PIT".
           DISPLAY WKS-IDENT LINE 17 POSITION 10.
           
           
       3000-FIN.