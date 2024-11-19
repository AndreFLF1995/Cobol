       IDENTIFICATION DIVISION.
       PROGRAM-ID. NOTAS.
      *--------------------------------------------------
      * este programa adiciona dados a um ficheiro e lista os dados.
      *--------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY "slnotas.cbl".

       DATA DIVISION.
       FILE SECTION.
           COPY "fdnotas.cbl".

       WORKING-STORAGE SECTION.
           77 OPCAO                 PIC X(1).
           77 SAI                   PIC X(1).
           77 WS-NOTAS-NUMBER       PIC 9(5).
           77 WS-NOMEALUNO          PIC A(20).
           77 WS-NOTA1              PIC 99.
           77 WS-NOTA2              PIC 99.
           77 WS-NOTA3              PIC 99.
           77 WS-MEDIAALUNO         PIC 99.

       SCREEN SECTION.
       01 MENU-SCREEN.
           05 LINE 1 COL 1 BLANK SCREEN.
           05 LINE 1 COL 1 VALUE "BEM-VINDO".
           05 LINE 3 COL 1 VALUE "1 - INSERIR".
           05 LINE 4 COL 1 VALUE "2 - LISTAR TODOS".
           05 LINE 5 COL 1 VALUE "3 - ALTERAR".
           05 LINE 6 COL 1 VALUE "4 - APAGAR".
           05 LINE 7 COL 1 VALUE "0 - SAIR".
           05 LINE 9 COL 1 VALUE "OPCAO: ".
           05 LINE 9 COL 8 PIC X(1) TO OPCAO.

       01 INSERIR-SCREEN.
           05 LINE 1 COL 1 BLANK SCREEN.
           05 LINE 1 COL 1 VALUE "INSERIR ALUNO".
           05 LINE 2 COL 1 VALUE "NUMERO DO ALUNO: ".
           05 LINE 2 COL 20 PIC 9(5) TO WS-NOTAS-NUMBER.
           05 LINE 3 COL 1 VALUE "NOME DO ALUNO: ".
           05 LINE 3 COL 20 PIC A(20) TO WS-NOMEALUNO.
           05 LINE 4 COL 1 VALUE "PRIMEIRA NOTA: ".
           05 LINE 4 COL 20 PIC 99V99 TO WS-NOTA1.
           05 LINE 5 COL 1 VALUE "SEGUNDA NOTA: ".
           05 LINE 5 COL 20 PIC 99V99 TO WS-NOTA2.
           05 LINE 6 COL 1 VALUE "TERCEIRA NOTA: ".
           05 LINE 6 COL 20 PIC 99V99 TO WS-NOTA3.

       01 ALTERAR-SCREEN.
           05 LINE 1 COL 1 BLANK SCREEN.
           05 LINE 1 COL 1 VALUE "ALTERAR ALUNO".
           05 LINE 2 COL 1 VALUE "NUMERO DO ALUNO: ".
           05 LINE 2 COL 20 PIC 9(5) TO WS-NOTAS-NUMBER.
           05 LINE 3 COL 1 VALUE "NOME DO ALUNO (ENTER PARA MANTER): ".
           05 LINE 3 COL 20 PIC A(20) TO WS-NOMEALUNO.
           05 LINE 4 COL 1 VALUE "PRIMEIRA NOTA (ENTER PARA MANTER): ".
           05 LINE 4 COL 20 PIC 99V99 TO WS-NOTA1.
           05 LINE 5 COL 1 VALUE "SEGUNDA NOTA (ENTER PARA MANTER): ".
           05 LINE 5 COL 20 PIC 99V99 TO WS-NOTA2.
           05 LINE 6 COL 1 VALUE "TERCEIRA NOTA (ENTER PARA MANTER): ".
           05 LINE 6 COL 20 PIC 99V99 TO WS-NOTA3.

       01 APAGAR-SCREEN.
           05 LINE 1 COL 1 BLANK SCREEN.
           05 LINE 1 COL 1 VALUE "APAGAR ALUNO".
           05 LINE 2 COL 1 VALUE "NUMERO DO ALUNO: ".
           05 LINE 2 COL 20 PIC 9(5) TO WS-NOTAS-NUMBER.

       01 LISTAR-SCREEN.
           05 LINE 1 COL 1 BLANK SCREEN.
           05 LINE 1 COL 1 VALUE "LISTAR TODOS".
           05 LINE 3 COL 1 VALUE "ID: ".
           05 LINE 3 COL 10 PIC 9(5) FROM NOTAS-NUMBER.
           05 LINE 4 COL 1 VALUE "NOME: ".
           05 LINE 4 COL 10 PIC A(20) FROM NOMEALUNO.
           05 LINE 5 COL 1 VALUE "1a NOTA: ".
           05 LINE 5 COL 10 PIC 99V99 FROM NOTA1.
           05 LINE 6 COL 1 VALUE "2a NOTA: ".
           05 LINE 6 COL 10 PIC 99V99 FROM NOTA2.
           05 LINE 7 COL 1 VALUE "3a NOTA: ".
           05 LINE 7 COL 10 PIC 99V99 FROM NOTA3.
           05 LINE 8 COL 1 VALUE "MEDIA: ".
           05 LINE 8 COL 10 PIC 99V99 FROM MEDIAALUNO.

       PROCEDURE DIVISION.
       MAIN SECTION.
       PROGRAM-BEGIN.

           INICIO.
           PERFORM UNTIL OPCAO = "0"
               DISPLAY MENU-SCREEN
               ACCEPT OPCAO

               EVALUATE OPCAO
                   WHEN "1"
                       PERFORM INSERIR
                   WHEN "2"
                       PERFORM LISTAR
                   WHEN "3"
                       PERFORM ALTERAR
                   WHEN "4"
                       PERFORM APAGAR
                   WHEN OTHER
                       DISPLAY "OPCAO INVALIDA. TENTE NOVAMENTE."
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       INSERIR.
           DISPLAY INSERIR-SCREEN
           ACCEPT INSERIR-SCREEN


           OPEN I-O NOTAS-FILE
           MOVE WS-NOTAS-NUMBER TO NOTAS-NUMBER
           MOVE WS-NOMEALUNO TO NOMEALUNO
           MOVE WS-NOTA1 TO NOTA1
           MOVE WS-NOTA2 TO NOTA2
           MOVE WS-NOTA3 TO NOTA3
           COMPUTE WS-MEDIAALUNO = (WS-NOTA1 + WS-NOTA2 + WS-NOTA3) / 3
           MOVE WS-MEDIAALUNO TO MEDIAALUNO
           WRITE REG-ALUNO
           CLOSE NOTAS-FILE.

       LISTAR.
           OPEN INPUT NOTAS-FILE
           PERFORM UNTIL SAI = "S"
               READ NOTAS-FILE
                   AT END
                       MOVE "S" TO SAI
                   NOT AT END
                       DISPLAY LISTAR-SCREEN
               END-READ
           END-PERFORM
           CLOSE NOTAS-FILE.

       ALTERAR.
           DISPLAY ALTERAR-SCREEN
           ACCEPT WS-NOTAS-NUMBER
           OPEN I-O NOTAS-FILE
           READ NOTAS-FILE KEY IS WS-NOTAS-NUMBER
               INVALID KEY
                   DISPLAY "ALUNO NAO ENCONTRADO!"
               NOT INVALID KEY
                   DISPLAY "REGISTO ATUAL:"
                   DISPLAY LISTAR-SCREEN
                   DISPLAY "INSIRA NOVOS DADOS (ENTER PARA MANTER):"
                   ACCEPT WS-NOMEALUNO
            IF WS-NOMEALUNO NOT = SPACES
                MOVE WS-NOMEALUNO TO NOMEALUNO
            END-IF
            ACCEPT WS-NOTA1
            IF WS-NOTA1 NOT = 0
                MOVE WS-NOTA1 TO NOTA1
            END-IF
            ACCEPT WS-NOTA2
            IF WS-NOTA2 NOT = 0
                MOVE WS-NOTA2 TO NOTA2
            END-IF
            ACCEPT WS-NOTA3
            IF WS-NOTA3 NOT = 0
                MOVE WS-NOTA3 TO NOTA3
            END-IF
            COMPUTE MEDIAALUNO = (NOTA1 + NOTA2 + NOTA3) / 3
            REWRITE REG-ALUNO
           CLOSE NOTAS-FILE.

       APAGAR.
           DISPLAY APAGAR-SCREEN
           ACCEPT WS-NOTAS-NUMBER
           OPEN I-O NOTAS-FILE
           READ NOTAS-FILE KEY IS WS-NOTAS-NUMBER
               INVALID KEY
                   DISPLAY "ALUNO NAO ENCONTRADO!"
               NOT INVALID KEY
                   DELETE NOTAS-FILE
                   DISPLAY "ALUNO APAGADO COM SUCESSO."
           END-READ
           CLOSE NOTAS-FILE
           PERFORM INICIO.

       SAIR.
           STOP RUN.
