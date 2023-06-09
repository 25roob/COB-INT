       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. CAL-ING.                                             
       AUTHOR.     ROBERTO Q.                                           
      *--------------------------------------------------------------   
       ENVIRONMENT                DIVISION.                             
       CONFIGURATION              SECTION.                              
                                                                        
       DATA  DIVISION.                                                  
       FILE  SECTION.                                                   
       WORKING-STORAGE            SECTION.                              
                                                                        
       77 WKS-I              PIC 99.                                    
       77 WKS-FIN1           PIC 9.
       77 WKS-ESPERA         PIC X.
                                                                        
       01 WKS-MESES.                                                    
           03 FILLER         PIC 9999 VALUE 0131.                       
           03 FILLER         PIC 9999 VALUE 0228.                       
           03 FILLER         PIC 9999 VALUE 0331.                       
           03 FILLER         PIC 9999 VALUE 0430.                       
           03 FILLER         PIC 9999 VALUE 0531.                       
           03 FILLER         PIC 9999 VALUE 0630.                       
           03 FILLER         PIC 9999 VALUE 0731.                       
           03 FILLER         PIC 9999 VALUE 0831.                       
           03 FILLER         PIC 9999 VALUE 0930.                       
           03 FILLER         PIC 9999 VALUE 1031.                       
           03 FILLER         PIC 9999 VALUE 1130.                       
           03 FILLER         PIC 9999 VALUE 1231.                        
                                                                        
       01 T-MESES REDEFINES WKS-MESES.                                  
           03 T-MESE OCCURS 12 TIMES.                                   
               05 T-MES      PIC 99.                                    
               05 T-DIA      PIC 99.                                    
                                                                        
       01 WKS-HOY.                                                      
           03 WKS-ANO        PIC 99.                                    
           03 WKS-MES        PIC 99.                                    
           03 WKS-DIA        PIC 99.                                    
                                                                        
       01 WKS-SPLIT-FI.                                                 
           03 FILLER         PIC 99.                                    
           03 WKS-ANOI       PIC 99.                                    
           03 WKS-MESI       PIC 99.                                    
           03 WKS-DIAI       PIC 99.                                    
                                                                        
       01 WKS-SPLIT-FT.                                                 
           03 FILLER         PIC 99.                                    
           03 WKS-ANOT       PIC 99.                                    
           03 WKS-MEST       PIC 99.                                    
           03 WKS-DIAT       PIC 99.                                    
                                                                        
       01 WKS-PAGO-FACT      PIC 9999999V99.                              
       01 WKS-DPAS           PIC 9999.                                  
       01 WKS-D-JU           PIC 9999.                                  
       01 WKS-TEM-D          PIC 9999.                                  
       01 WKS-D-INI          PIC 9999.                                   
                                                                        
       LINKAGE SECTION.                                                
       01 WKS-LINK-DATA.
           03 WKS-FEC-INICIO   PIC 9(08).
           03 WKS-FEC-TERMIN   PIC 9(08).
           03 WKS-ING-MENSUA   PIC 9(10)V99.
           03 WKS-ING-TOTALD   PIC 9(13)V99.                            
                                                                        
       PROCEDURE  DIVISION USING WKS-LINK-DATA.                                                                      
       000-INI.                                                         
           PERFORM 100-PROCESO.                                         
           EXIT PROGRAM.                                               
                                                              
                                                                        
       100-PROCESO.                                                     
      *=============                                                    
      *     
           MOVE ZEROES TO WKS-TEM-D.      
           MOVE ZEROES TO WKS-I.
           MOVE ZEROES TO WKS-D-JU.
           MOVE ZEROES TO WKS-ING-TOTALD.
           MOVE ZEROES TO WKS-D-INI.
           MOVE ZEROES TO WKS-FIN1.
           ACCEPT WKS-HOY FROM DATE.                                    
           DIVIDE WKS-ING-MENSUA BY 30 GIVING WKS-PAGO-FACT.                                                                                                 
           MOVE WKS-FEC-INICIO TO WKS-SPLIT-FI.                         
           MOVE WKS-FEC-TERMIN TO WKS-SPLIT-FT.                         
                                                                        
           IF WKS-ANOI = WKS-ANO                                       
               PERFORM 120-DIAS-TRANSC-MA                               
               MULTIPLY WKS-D-JU BY WKS-PAGO-FACT                       
                   GIVING WKS-ING-TOTALD                                
           ELSE                                                         
               PERFORM 170-DIAS-TRANSC-DA                               
               MULTIPLY WKS-D-JU BY WKS-PAGO-FACT                       
                   GIVING WKS-ING-TOTALD.  
      *     DISPLAY WKS-FEC-INICIO.               
      *     DISPLAY WKS-D-JU.
      *     DISPLAY WKS-PAGO-FACT.
      *     DISPLAY WKS-ING-TOTALD.
                                                                        
       120-DIAS-TRANSC-MA.  
      *     DISPLAY "MISMO ANO QUE ACTUAL".       
           PERFORM 150-CALCULO-DIAS VARYING WKS-I                       
               FROM 1 BY 1 UNTIL WKS-I GREATER THAN 12.                 
                                                                        
           SUBTRACT WKS-D-INI FROM WKS-D-JU.                            
                                                                        
       150-CALCULO-DIAS.                                                
      *    DISPLAY WKS-I.                                               
                                                                        
           ADD T-DIA (WKS-I) TO WKS-TEM-D.                              
      *    DISPLAY WKS-TEM-D.                                           
                                                                        
           IF T-MES (WKS-I) = WKS-MES                                   
      *        DISPLAY "T-MES (WKS-I) = WKS-MES"                        
               MOVE WKS-TEM-D TO WKS-D-JU                               
               SUBTRACT T-DIA (WKS-I) FROM WKS-D-JU                     
               ADD WKS-DIA TO WKS-D-JU                                  
               ADD 1 TO WKS-FIN1.                                       
                                                                        
           IF T-MES (WKS-I) = WKS-MESI                                  
      *        DISPLAY "T-MES (WKS-I) = WKS-MESI"                       
               MOVE WKS-TEM-D TO WKS-D-INI                              
               SUBTRACT T-DIA (WKS-I) FROM WKS-D-INI                    
               ADD WKS-DIAI TO WKS-D-INI                                
               ADD 1 TO WKS-FIN1.                                       
                                                                        
           IF WKS-FIN1 = 2                                              
               ADD 15 TO WKS-I.                                         
                                                                        
       170-DIAS-TRANSC-DA.   
      *     DISPLAY "DIFERENTE ANO QUE ACTUAL".       
           SUBTRACT WKS-ANOI FROM WKS-ANO. 
      *     DISPLAY "VALORES DE CALCULO:".
      *     DISPLAY WKS-ANO.           
           PERFORM 190-DELTA-DIAS VARYING WKS-I                         
               FROM 1 BY 1 UNTIL WKS-I > 12.                            
           ADD WKS-DPAS TO WKS-D-JU.  
      *     DISPLAY "FIN DE VALORES DE CALCULO".           
                                                                        
       190-DELTA-DIAS.                                                  
           ADD T-DIA (WKS-I) TO WKS-TEM-D.                              
      *     DISPLAY WKS-TEM-D.
      *     DISPLAY T-MES (WKS-I).          
           
           IF T-MES (WKS-I) = WKS-MESI
      *         DISPLAY T-MES (WKS-I) "=" WKS-MESI           
               MOVE WKS-TEM-D TO WKS-D-INI                              
               SUBTRACT T-DIA (WKS-I) FROM WKS-D-INI                    
               ADD WKS-DIAI TO WKS-D-INI
      *         MULTIPLY WKS-ANO BY 365 GIVING WKS-DPAS                 
      *         SUBTRACT WKS-D-INI FROM WKS-DPAS                                   
               COMPUTE WKS-DPAS = WKS-ANO * 365 - WKS-D-INI            
               ADD 1 TO WKS-FIN1.                                       
                                                                        
           IF T-MES (WKS-I) = WKS-MES
      *         DISPLAY T-MES (WKS-I) "=" WKS-MES           
               MOVE WKS-TEM-D TO WKS-D-JU                               
               SUBTRACT T-DIA (WKS-I) FROM WKS-D-JU                     
               ADD WKS-DIA TO WKS-D-JU                                  
               ADD 1 TO WKS-FIN1.                                       
                                                                        
           IF WKS-FIN1 = 2                                              
               ADD 15 TO WKS-I.                                         
                                                                        
      