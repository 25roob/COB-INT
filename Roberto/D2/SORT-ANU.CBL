       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT-ANU.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO DISK.
           SELECT ARC-SOR ASSIGN TO DISK.
           SELECT SOR-TEM ASSIGN TO SORT.
       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES.
       01 REG-CLI.
           03 ID-CLIENTE PIC 9(03).
           03 NOM-CLI    PIC X(30).
           03 CLA-TAR    PIC 9.
           03 NOM-TAR    PIC X(20).
           03 ANUA       PIC 9(05)V99.
           03 FEC-ING    PIC 9(08).
           
       FD ARC-SOR.
       01 REG-CLI-A.
           03 ID-CLIENTE-A PIC 9(03).
           03 NOM-CLI-A    PIC X(30).
           03 CLA-TAR-A    PIC 9.
           03 NOM-TAR-A    PIC X(20).
           03 ANUA-A       PIC 9(05)V99.
           03 FEC-ING-A    PIC 9(08).
       
       SD SOR-TEM.
       01 REG-CLI-S.
           03 ID-CLIENTE-S PIC 9(03).
           03 NOM-CLI-S    PIC X(30).
           03 CLA-TAR-S    PIC 9.
           03 NOM-TAR-S    PIC X(20).
           03 ANUA-S       PIC 9(05)V99.
           03 FEC-ING-S    PIC 9(08).
           
           
       WORKING-STORAGE SECTION.
       01 WKS-AN-INI       PIC 9(08).
       01 WKS-AN-FIN      PIC 9(08).
       
       77 WKS-FIN            PIC 9.
       77 WKS-NOM-TAR           PIC X(20).
       
       PROCEDURE DIVISION.
       PRINCIPAL SECTION.
       INICIO.
           SORT SOR-TEM
           ASCENDING ANUA-S FEC-ING-S 
           INPUT PROCEDURE 1000-ORDENA-ARCHIVO 
           GIVING ARC-SOR.
      *     OUTPUT PROCEDURE 2000-GENERA-ARCHIVO    
           STOP RUN.
       
       1000-ORDENA-ARCHIVO SECTION.
       INICIO-ORDENA.
           PERFORM 1200-INICIO.
           PERFORM 1500-PROCESO UNTIL WKS-FIN = 1.
           PERFORM 1700-FIN.
           GO TO 1800-FIN-SECTION.
       
       1200-INICIO.
           OPEN INPUT CLIENTES.
           DISPLAY SPACES LINE 01 POSITION 01 ERASE.
           
           READ CLIENTES
               AT END MOVE 1 TO WKS-FIN.
           
           DISPLAY "ELIGE FECHA INICIAL (YYYYMMDD): "
               LINE 10 POSITION 05.
           ACCEPT WKS-AN-INI LINE 10 POSITION 50.
           DISPLAY "ELIGE FECHA FINAL (YYYYMMDD): "
               LINE 11 POSITION 05.
           ACCEPT WKS-AN-FIN LINE 11 POSITION 50.
       
       1500-PROCESO.
           
           IF FEC-ING >= WKS-AN-INI AND FEC-ING <= WKS-AN-FIN
               MOVE REG-CLI TO REG-CLI-S
               RELEASE REG-CLI-S.
           
           READ CLIENTES
               AT END MOVE 1 TO WKS-FIN.
       
       1700-FIN.
       
           CLOSE CLIENTES.
           
           INITIALIZE WKS-FIN.
       
       1800-FIN-SECTION.
           EXIT.
       
       2000-GENERA-ARCHIVO  SECTION.
       INICIO-GEN-ARC.
           PERFORM 2200-INICIO.
           PERFORM 2500-PROCESO UNTIL WKS-FIN = 1.
           PERFORM 2700-FIN.
           GO TO 2800-FIN-SECTION.
       
       2200-INICIO.
       
           OPEN OUTPUT ARC-SOR
           
           RETURN SOR-TEM
               AT END MOVE 1 TO WKS-FIN.
       
       2500-PROCESO.
       
           MOVE REG-CLI-S TO REG-CLI-A.
           WRITE REG-CLI-A.
           
           RETURN SOR-TEM
               AT END MOVE 1 TO WKS-FIN.
           
       
       2700-FIN.
           
           CLOSE ARC-SOR.
       
       
       2800-FIN-SECTION.
           EXIT.