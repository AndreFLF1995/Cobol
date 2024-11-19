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
           77 OPCAO PIC X(1).
           77 SAI PIC X(1).

       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.
       PROGRAM-BEGIN.

           INICIO.
           MOVE "N" TO SAI
               DISPLAY "              BEM-VINDO".
               DISPLAY " ".
               DISPLAY "          ***   MENU   ***".
               DISPLAY "        ***   1-INSERIR   ***".
               DISPLAY "        ***   2-LISTAR   ***".
               DISPLAY "     ***   3-PROCURAR NOME   ***".
               DISPLAY "        ***   4-APAGAR   ***".
               DISPLAY "         ***   0-SAIR   ***".

           ACCEPT OPCAO.
           EVALUATE OPCAO
               WHEN "1"
                   PERFORM INSERIR
               WHEN "2"
                   PERFORM LISTAR
      *         WHEN "3"
      *             PERFORM ALTERAR
      *         WHEN "4"
      *             PERFORM APAGAR
               WHEN "0"
                   PERFORM SAIR
           END-EVALUATE.

           INSERIR.
           OPEN I-O NOTAS-FILE
           MOVE SPACE TO REG-ALUNO
               DISPLAY "POR FAVOR INSIRA O NUMERO DO ALUNO: "
                   ACCEPT NOTAS-NUMBER

               DISPLAY "POR FAVOR INSIRA O NOME DO ALUNO: "
                   ACCEPT NOMEALUNO

               DISPLAY "INSIRA A PRIMEIRA NOTA: "
                   ACCEPT NOTA1

               DISPLAY "INSIRA A SEGUNDA NOTA: "
                   ACCEPT NOTA2

               DISPLAY "INSIRA A TERCEIRA NOTA: "
                   ACCEPT NOTA3

               WRITE REG-ALUNO
           PERFORM VALIDAR
           CLOSE NOTAS-FILE
           PERFORM INICIO
           .

           VALIDAR.
           IF NOMEALUNO = SPACE
               DISPLAY "TEM QUE INSERIR UM NOME!"
           END-IF
           IF NOTA1 = SPACE
               DISPLAY "TEM QUE INSERIR UM NUMERO!"
           END-IF
           IF NOTA2 = SPACE
               DISPLAY "TEM QUE INSERIR UM NUMERO!"
           END-IF
           IF NOTA3 = SPACE
               DISPLAY "TEM QUE INSERIR UM NUMERO!"
           END-IF.

           LISTAR.
           OPEN I-O NOTAS-FILE
           PERFORM UNTIL SAI = "S"
           READ NOTAS-FILE
               AT END
                   MOVE "S" TO SAI
               NOT AT END
           DISPLAY "NOME: " NOMEALUNO
           DISPLAY "1a NOTA: " NOTA1
           DISPLAY "2a NOTA: " NOTA2
           DISPLAY "3a NOTA: " NOTA3
           END-READ
           END-PERFORM
           CLOSE NOTAS-FILE
           PERFORM INICIO
           .

           SAIR.
               STOP RUN.
