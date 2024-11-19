      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 TABULEIRO.
           05 ESPACO1 OCCURS 6 TIMES.
               10 ESPACO2 PIC X(1) OCCURS 7 TIMES.
       77 CONT1 PIC 9(1).
       77 CONT2 PIC 9(1).
       77 CONT3 PIC 9(1).
       77 CONT4 PIC 9(1).
       77 CONT5 PIC 9(1).
       77 CONT6 PIC 9(1).
       77 PECA PIC X(1).
       77 COLUNA PIC 9(1).
       77 JOGADAS PIC 99.
       77 EMPATE PIC 9(1).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM RULES
           PERFORM BOARD
           PERFORM PLAY.

           PLAY.
           PERFORM VARYING CONT4 FROM 1 BY 1 UNTIL CONT4 > 42
               PERFORM PLAYER1TURN
               ADD 1 TO EMPATE
               PERFORM PLAYER2TURN
               ADD 1 TO EMPATE
               IF EMPATE = 42
                   PERFORM DRAW
               END-IF
           END-PERFORM.

           RULES.
           DISPLAY "BEM-VINDO AO 4 EM LINHA."
           DISPLAY "EXISTEM DUAS PECAS EM JOGO. X / O"
           DISPLAY "O PLAYER 1 = X E O PLAYER 2 = O"
           DISPLAY "O PLAYER 1 E SEMPRE O PRIMEIRO A JOGAR."
           DISPLAY " "
           DISPLAY "CADA JOGADOR ESCOLHE UMA COLUNA, DE 1 A 7,"
           WITH NO ADVANCING
           DISPLAY " E AS PECAS VAO AMONTOANDO."
           DISPLAY "GANHA QUEM FIZER 4 EM LINHA!"

           PERFORM VARYING CONT1 FROM 1 BY 1 UNTIL CONT1 > 6
              PERFORM VARYING CONT2 FROM 1 BY 1 UNTIL CONT2 > 7
                  MOVE "_" TO ESPACO2 (CONT1, CONT2)
              END-PERFORM
           END-PERFORM.

           BOARD.
           DISPLAY "|" ESPACO2 (1,1) "|" ESPACO2(1,2) "|" ESPACO2 (1,3)
           WITH NO ADVANCING
           DISPLAY "|" ESPACO2 (1,4) "|" ESPACO2(1,5) "|" ESPACO2 (1,6)
           WITH NO ADVANCING
           DISPLAY "|" ESPACO2 (1,7) "|"

           DISPLAY "|" ESPACO2 (2,1) "|" ESPACO2(2,2) "|" ESPACO2 (2,3)
           WITH NO ADVANCING
           DISPLAY "|" ESPACO2 (2,4) "|" ESPACO2(2,5) "|" ESPACO2 (2,6)
           WITH NO ADVANCING
           DISPLAY "|" ESPACO2 (2,7) "|"

           DISPLAY "|" ESPACO2 (3,1) "|" ESPACO2(3,2) "|" ESPACO2 (3,3)
           WITH NO ADVANCING
           DISPLAY "|" ESPACO2 (3,4) "|" ESPACO2(3,5) "|" ESPACO2 (3,6)
           WITH NO ADVANCING
           DISPLAY "|" ESPACO2 (3,7) "|"

           DISPLAY "|" ESPACO2 (4,1) "|" ESPACO2(4,2) "|" ESPACO2 (4,3)
           WITH NO ADVANCING
           DISPLAY "|" ESPACO2 (4,4) "|" ESPACO2(4,5) "|" ESPACO2 (4,6)
           WITH NO ADVANCING
           DISPLAY "|" ESPACO2 (4,7) "|"

           DISPLAY "|" ESPACO2 (5,1) "|" ESPACO2(5,2) "|" ESPACO2 (5,3)
           WITH NO ADVANCING
           DISPLAY "|" ESPACO2 (5,4) "|" ESPACO2(5,5) "|" ESPACO2 (5,6)
           WITH NO ADVANCING
           DISPLAY "|" ESPACO2 (5,7) "|"

           DISPLAY "|" ESPACO2 (6,1) "|" ESPACO2(6,2) "|" ESPACO2 (6,3)
           WITH NO ADVANCING
           DISPLAY "|" ESPACO2 (6,4) "|" ESPACO2(6,5) "|" ESPACO2 (6,6)
           WITH NO ADVANCING
           DISPLAY "|" ESPACO2 (6,7) "|"

           DISPLAY "|" 1 "|" 2 "|" 3 "|" 4 "|" 5 "|" 6 "|" 7 "|"
           DISPLAY " ".

           PLAYER1TURN.
               DISPLAY "TURNO DO PLAYER 1. INDICA A COLUNA:"
               MOVE "X" TO PECA
               ACCEPT COLUNA
               PERFORM CHECK1.

           PLAYER2TURN.
               DISPLAY "TURNO DO PLAYER 2. INDICA A COLUNA:"
               MOVE "O" TO PECA
               ACCEPT COLUNA
               PERFORM CHECK2.

           CHECK1.
           IF COLUNA > 0 AND COLUNA < 8
               IF COLUNA = 1
                   IF ESPACO2 (6,1) = "_"
                       MOVE PECA TO ESPACO2 (6,1)
                   ELSE
                       IF ESPACO2 (5,1) = "_"
                           MOVE PECA TO ESPACO2 (5,1)
                       ELSE
                           IF ESPACO2 (4,1) = "_"
                               MOVE PECA TO ESPACO2 (4,1)
                           ELSE
                               IF ESPACO2 (3,1) = "_"
                                   MOVE PECA TO ESPACO2 (3,1)
                               ELSE
                                   IF ESPACO2 (2,1) = "_"
                                       MOVE PECA TO ESPACO2 (2,1)
                                   ELSE
                                       IF ESPACO2 (1,1) = "_"
                                           MOVE PECA TO ESPACO2 (1,1)
                                       ELSE
                                           DISPLAY
                                           "JOGADA FORA DO TABULEIRO"
                                           PERFORM PLAYER1TURN
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

               IF COLUNA = 2
                   IF ESPACO2 (6,2) = "_"
                       MOVE PECA TO ESPACO2 (6,2)
                   ELSE
                       IF ESPACO2 (5,2) = "_"
                           MOVE PECA TO ESPACO2 (5,2)
                       ELSE
                           IF ESPACO2 (4,2) = "_"
                               MOVE PECA TO ESPACO2 (4,2)
                           ELSE
                               IF ESPACO2 (3,2) = "_"
                                   MOVE PECA TO ESPACO2 (3,2)
                               ELSE
                                   IF ESPACO2 (2,2) = "_"
                                       MOVE PECA TO ESPACO2 (2,2)
                                   ELSE
                                       IF ESPACO2 (1,2) = "_"
                                           MOVE PECA TO ESPACO2 (1,2)
                                       ELSE
                                           DISPLAY
                                           "JOGADA FORA DO TABULEIRO"
                                           PERFORM PLAYER1TURN
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

               IF COLUNA = 3
                   IF ESPACO2 (6,3) = "_"
                       MOVE PECA TO ESPACO2 (6,3)
                   ELSE
                       IF ESPACO2 (5,3) = "_"
                           MOVE PECA TO ESPACO2 (5,3)
                       ELSE
                           IF ESPACO2 (4,3) = "_"
                               MOVE PECA TO ESPACO2 (4,3)
                           ELSE
                               IF ESPACO2 (3,3) = "_"
                                   MOVE PECA TO ESPACO2 (3,3)
                               ELSE
                                   IF ESPACO2 (2,3) = "_"
                                       MOVE PECA TO ESPACO2 (2,3)
                                   ELSE
                                       IF ESPACO2 (1,3) = "_"
                                           MOVE PECA TO ESPACO2 (1,3)
                                       ELSE
                                           DISPLAY
                                           "JOGADA FORA DO TABULEIRO"
                                           PERFORM PLAYER1TURN
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

               IF COLUNA = 4
                   IF ESPACO2 (6,4) = "_"
                       MOVE PECA TO ESPACO2 (6,4)
                   ELSE
                       IF ESPACO2 (5,4) = "_"
                           MOVE PECA TO ESPACO2 (5,4)
                       ELSE
                           IF ESPACO2 (4,4) = "_"
                               MOVE PECA TO ESPACO2 (4,4)
                           ELSE
                               IF ESPACO2 (3,4) = "_"
                                   MOVE PECA TO ESPACO2 (3,4)
                               ELSE
                                   IF ESPACO2 (2,4) = "_"
                                       MOVE PECA TO ESPACO2 (2,4)
                                   ELSE
                                       IF ESPACO2 (1,4) = "_"
                                           MOVE PECA TO ESPACO2 (1,4)
                                       ELSE
                                           DISPLAY
                                           "JOGADA FORA DO TABULEIRO"
                                           PERFORM PLAYER1TURN
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

               IF COLUNA = 5
                   IF ESPACO2 (6,5) = "_"
                       MOVE PECA TO ESPACO2 (6,5)
                   ELSE
                       IF ESPACO2 (5,5) = "_"
                           MOVE PECA TO ESPACO2 (5,5)
                       ELSE
                           IF ESPACO2 (4,5) = "_"
                               MOVE PECA TO ESPACO2 (4,5)
                           ELSE
                               IF ESPACO2 (3,5) = "_"
                                   MOVE PECA TO ESPACO2 (3,5)
                               ELSE
                                   IF ESPACO2 (2,5) = "_"
                                       MOVE PECA TO ESPACO2 (2,5)
                                   ELSE
                                       IF ESPACO2 (1,5) = "_"
                                           MOVE PECA TO ESPACO2 (1,5)
                                       ELSE
                                           DISPLAY
                                           "JOGADA FORA DO TABULEIRO"
                                           PERFORM PLAYER1TURN
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

               IF COLUNA = 6
                   IF ESPACO2 (6,6) = "_"
                       MOVE PECA TO ESPACO2 (6,6)
                   ELSE
                       IF ESPACO2 (5,6) = "_"
                           MOVE PECA TO ESPACO2 (5,6)
                       ELSE
                           IF ESPACO2 (4,6) = "_"
                               MOVE PECA TO ESPACO2 (4,6)
                           ELSE
                               IF ESPACO2 (3,6) = "_"
                                   MOVE PECA TO ESPACO2 (3,6)
                               ELSE
                                   IF ESPACO2 (2,6) = "_"
                                       MOVE PECA TO ESPACO2 (2,6)
                                   ELSE
                                       IF ESPACO2 (1,6) = "_"
                                           MOVE PECA TO ESPACO2 (1,6)
                                       ELSE
                                           DISPLAY
                                           "JOGADA FORA DO TABULEIRO"
                                           PERFORM PLAYER1TURN
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

               IF COLUNA = 7
                   IF ESPACO2 (6,7) = "_"
                       MOVE PECA TO ESPACO2 (6,7)
                   ELSE
                       IF ESPACO2 (5,7) = "_"
                           MOVE PECA TO ESPACO2 (5,7)
                       ELSE
                           IF ESPACO2 (4,7) = "_"
                               MOVE PECA TO ESPACO2 (4,7)
                           ELSE
                               IF ESPACO2 (3,7) = "_"
                                   MOVE PECA TO ESPACO2 (3,7)
                               ELSE
                                   IF ESPACO2 (2,7) = "_"
                                       MOVE PECA TO ESPACO2 (2,7)
                                   ELSE
                                       IF ESPACO2 (1,7) = "_"
                                           MOVE PECA TO ESPACO2 (1,7)
                                       ELSE
                                           DISPLAY
                                           "JOGADA FORA DO TABULEIRO"
                                           PERFORM PLAYER1TURN
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
               ELSE
                   DISPLAY "JOGADA FORA DO TABULEIRO"
                   PERFORM PLAYER1TURN
           END-IF

           PERFORM BOARD

      *VITORIA HORIZONTAL
           PERFORM VARYING CONT5 FROM 1 BY 1 UNTIL CONT5 > 6
               PERFORM VARYING CONT6 FROM 1 BY 1 UNTIL CONT6 > 4
                   IF ESPACO2 (CONT5, CONT6) = PECA AND
                      ESPACO2 (CONT5, CONT6 + 1) = PECA AND
                      ESPACO2 (CONT5, CONT6 + 2) = PECA AND
                      ESPACO2 (CONT5, CONT6 + 3) = PECA
                       DISPLAY "O VENCEDOR E O PLAYER 1!"
                       PERFORM WIN
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM.

      *VITORIA VERTICAL
           PERFORM VARYING CONT5 FROM 1 BY 1 UNTIL CONT5 > 7
               PERFORM VARYING CONT6 FROM 1 BY 1 UNTIL CONT6 > 3
                   IF ESPACO2 (CONT6, CONT5) = PECA AND
                      ESPACO2 (CONT6 + 1, CONT5) = PECA AND
                      ESPACO2 (CONT6 + 2, CONT5) = PECA AND
                      ESPACO2 (CONT6 + 3, CONT5) = PECA
                       DISPLAY "O VENCEDOR E O PLAYER 1!"
                       PERFORM WIN
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM.

      *VITORIA DIAGONAL PARA A DIREITA
           PERFORM VARYING CONT5 FROM 1 BY 1 UNTIL CONT5 > 3
               PERFORM VARYING CONT6 FROM 1 BY 1 UNTIL CONT6 > 4
                   IF ESPACO2 (CONT5, CONT6) = PECA AND
                      ESPACO2 (CONT5 + 1, CONT6 + 1) = PECA AND
                      ESPACO2 (CONT5 + 2, CONT6 + 2) = PECA AND
                      ESPACO2 (CONT5 + 3, CONT6 + 3) = PECA
                       DISPLAY "O VENCEDOR E O PLAYER 1!"
                       PERFORM WIN
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM.

      *VITORIA DIAGONAL PARA A ESQUERDA
           PERFORM VARYING CONT5 FROM 1 BY 1 UNTIL CONT5 > 3
               PERFORM VARYING CONT6 FROM 4 BY 1 UNTIL CONT6 > 7
                   IF ESPACO2 (CONT5, CONT6) = PECA AND
                      ESPACO2 (CONT5 + 1, CONT6 - 1) = PECA AND
                      ESPACO2 (CONT5 + 2, CONT6 - 2) = PECA AND
                      ESPACO2 (CONT5 + 3, CONT6 - 3) = PECA
                       DISPLAY "O VENCEDOR E O PLAYER 1!"
                       PERFORM WIN
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM.

           ERRO.
               DISPLAY "JOGADA INVÁLIDA"
               IF PECA = "O"
                   MOVE "N" TO PECA
                   PERFORM CHECK2
               ELSE
                   MOVE "N" TO PECA
                   PERFORM CHECK1.

           CHECK2.
           IF COLUNA > 0 AND COLUNA < 8
               IF COLUNA = 1
                   IF ESPACO2 (6,1) = "_"
                       MOVE PECA TO ESPACO2 (6,1)
                   ELSE
                       IF ESPACO2 (5,1) = "_"
                           MOVE PECA TO ESPACO2 (5,1)
                       ELSE
                           IF ESPACO2 (4,1) = "_"
                               MOVE PECA TO ESPACO2 (4,1)
                           ELSE
                               IF ESPACO2 (3,1) = "_"
                                   MOVE PECA TO ESPACO2 (3,1)
                               ELSE
                                   IF ESPACO2 (2,1) = "_"
                                       MOVE PECA TO ESPACO2 (2,1)
                                   ELSE
                                       IF ESPACO2 (1,1) = "_"
                                           MOVE PECA TO ESPACO2 (1,1)
                                       ELSE
                                           DISPLAY
                                           "JOGADA FORA DO TABULEIRO"
                                           PERFORM PLAYER2TURN
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

                   IF COLUNA = 2
                   IF ESPACO2 (6,2) = "_"
                       MOVE PECA TO ESPACO2 (6,2)
                   ELSE
                       IF ESPACO2 (5,2) = "_"
                           MOVE PECA TO ESPACO2 (5,2)
                       ELSE
                           IF ESPACO2 (4,2) = "_"
                               MOVE PECA TO ESPACO2 (4,2)
                           ELSE
                               IF ESPACO2 (3,2) = "_"
                                   MOVE PECA TO ESPACO2 (3,2)
                               ELSE
                                   IF ESPACO2 (2,2) = "_"
                                       MOVE PECA TO ESPACO2 (2,2)
                                   ELSE
                                       IF ESPACO2 (1,2) = "_"
                                           MOVE PECA TO ESPACO2 (1,2)
                                       ELSE
                                           DISPLAY
                                           "JOGADA FORA DO TABULEIRO"
                                           PERFORM PLAYER2TURN
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

                   IF COLUNA = 3
                   IF ESPACO2 (6,3) = "_"
                       MOVE PECA TO ESPACO2 (6,3)
                   ELSE
                       IF ESPACO2 (5,3) = "_"
                           MOVE PECA TO ESPACO2 (5,3)
                       ELSE
                           IF ESPACO2 (4,3) = "_"
                               MOVE PECA TO ESPACO2 (4,3)
                           ELSE
                               IF ESPACO2 (3,3) = "_"
                                   MOVE PECA TO ESPACO2 (3,3)
                               ELSE
                                   IF ESPACO2 (2,3) = "_"
                                       MOVE PECA TO ESPACO2 (2,3)
                                   ELSE
                                       IF ESPACO2 (1,3) = "_"
                                           MOVE PECA TO ESPACO2 (1,3)
                                       ELSE
                                           DISPLAY
                                           "JOGADA FORA DO TABULEIRO"
                                           PERFORM PLAYER2TURN
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

                   IF COLUNA = 4
                   IF ESPACO2 (6,4) = "_"
                       MOVE PECA TO ESPACO2 (6,4)
                   ELSE
                       IF ESPACO2 (5,4) = "_"
                           MOVE PECA TO ESPACO2 (5,4)
                       ELSE
                           IF ESPACO2 (4,4) = "_"
                               MOVE PECA TO ESPACO2 (4,4)
                           ELSE
                               IF ESPACO2 (3,4) = "_"
                                   MOVE PECA TO ESPACO2 (3,4)
                               ELSE
                                   IF ESPACO2 (2,4) = "_"
                                       MOVE PECA TO ESPACO2 (2,4)
                                   ELSE
                                       IF ESPACO2 (1,4) = "_"
                                           MOVE PECA TO ESPACO2 (1,4)
                                       ELSE
                                           DISPLAY
                                           "JOGADA FORA DO TABULEIRO"
                                           PERFORM PLAYER2TURN
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

                   IF COLUNA = 5
                   IF ESPACO2 (6,5) = "_"
                       MOVE PECA TO ESPACO2 (6,5)
                   ELSE
                       IF ESPACO2 (5,5) = "_"
                           MOVE PECA TO ESPACO2 (5,5)
                       ELSE
                           IF ESPACO2 (4,5) = "_"
                               MOVE PECA TO ESPACO2 (4,5)
                           ELSE
                               IF ESPACO2 (3,5) = "_"
                                   MOVE PECA TO ESPACO2 (3,5)
                               ELSE
                                   IF ESPACO2 (2,5) = "_"
                                       MOVE PECA TO ESPACO2 (2,5)
                                   ELSE
                                       IF ESPACO2 (1,5) = "_"
                                           MOVE PECA TO ESPACO2 (1,5)
                                       ELSE
                                           DISPLAY
                                           "JOGADA FORA DO TABULEIRO"
                                           PERFORM PLAYER2TURN
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

                   IF COLUNA = 6
                   IF ESPACO2 (6,6) = "_"
                       MOVE PECA TO ESPACO2 (6,6)
                   ELSE
                       IF ESPACO2 (5,6) = "_"
                           MOVE PECA TO ESPACO2 (5,6)
                       ELSE
                           IF ESPACO2 (4,6) = "_"
                               MOVE PECA TO ESPACO2 (4,6)
                           ELSE
                               IF ESPACO2 (3,6) = "_"
                                   MOVE PECA TO ESPACO2 (3,6)
                               ELSE
                                   IF ESPACO2 (2,6) = "_"
                                       MOVE PECA TO ESPACO2 (2,6)
                                   ELSE
                                       IF ESPACO2 (1,6) = "_"
                                           MOVE PECA TO ESPACO2 (1,6)
                                       ELSE
                                           DISPLAY
                                           "JOGADA FORA DO TABULEIRO"
                                           PERFORM PLAYER2TURN
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

                   IF COLUNA = 7
                   IF ESPACO2 (6,7) = "_"
                       MOVE PECA TO ESPACO2 (6,7)
                   ELSE
                       IF ESPACO2 (5,7) = "_"
                           MOVE PECA TO ESPACO2 (5,7)
                       ELSE
                           IF ESPACO2 (4,7) = "_"
                               MOVE PECA TO ESPACO2 (4,7)
                           ELSE
                               IF ESPACO2 (3,7) = "_"
                                   MOVE PECA TO ESPACO2 (3,7)
                               ELSE
                                   IF ESPACO2 (2,7) = "_"
                                       MOVE PECA TO ESPACO2 (2,7)
                                   ELSE
                                       IF ESPACO2 (1,7) = "_"
                                           MOVE PECA TO ESPACO2 (1,7)
                                       ELSE
                                           DISPLAY
                                           "JOGADA FORA DO TABULEIRO"
                                           PERFORM PLAYER2TURN
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
               ELSE
                   DISPLAY "JOGADA FORA DO TABULEIRO"
                   PERFORM PLAYER2TURN
           END-IF

           PERFORM BOARD

      *VITORIA HORIZONTAL
           PERFORM VARYING CONT5 FROM 1 BY 1 UNTIL CONT5 > 6
               PERFORM VARYING CONT6 FROM 1 BY 1 UNTIL CONT6 > 4
                   IF ESPACO2 (CONT5, CONT6) = PECA AND
                      ESPACO2 (CONT5, CONT6 + 1) = PECA AND
                      ESPACO2 (CONT5, CONT6 + 2) = PECA AND
                      ESPACO2 (CONT5, CONT6 + 3) = PECA
                       DISPLAY "O VENCEDOR E O PLAYER 2!"
                       PERFORM WIN
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM.

      *VITORIA VERTICAL
           PERFORM VARYING CONT5 FROM 1 BY 1 UNTIL CONT5 > 7
               PERFORM VARYING CONT6 FROM 1 BY 1 UNTIL CONT6 > 3
                   IF ESPACO2 (CONT6, CONT5) = PECA AND
                      ESPACO2 (CONT6 + 1, CONT5) = PECA AND
                      ESPACO2 (CONT6 + 2, CONT5) = PECA AND
                      ESPACO2 (CONT6 + 3, CONT5) = PECA
                       DISPLAY "O VENCEDOR E O PLAYER 2!"
                       PERFORM WIN
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM.

      *VITORIA DIAGONAL PARA A DIREITA
           PERFORM VARYING CONT5 FROM 1 BY 1 UNTIL CONT5 > 3
               PERFORM VARYING CONT6 FROM 1 BY 1 UNTIL CONT6 > 4
                   IF ESPACO2 (CONT5, CONT6) = PECA AND
                      ESPACO2 (CONT5 + 1, CONT6 + 1) = PECA AND
                      ESPACO2 (CONT5 + 2, CONT6 + 2) = PECA AND
                      ESPACO2 (CONT5 + 3, CONT6 + 3) = PECA
                       DISPLAY "O VENCEDOR E O PLAYER 2!"
                       PERFORM WIN
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM.

      *VITORIA DIAGONAL PARA A ESQUERDA
           PERFORM VARYING CONT5 FROM 1 BY 1 UNTIL CONT5 > 3
               PERFORM VARYING CONT6 FROM 4 BY 1 UNTIL CONT6 > 7
                   IF ESPACO2 (CONT5, CONT6) = PECA AND
                      ESPACO2 (CONT5 + 1, CONT6 - 1) = PECA AND
                      ESPACO2 (CONT5 + 2, CONT6 - 2) = PECA AND
                      ESPACO2 (CONT5 + 3, CONT6 - 3) = PECA
                       DISPLAY "O VENCEDOR E O PLAYER 2!"
                       PERFORM WIN
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM.

           WIN.
               STOP RUN.

           DRAW.
               DISPLAY "EMPATE."
               STOP RUN.


       END PROGRAM YOUR-PROGRAM-NAME.
