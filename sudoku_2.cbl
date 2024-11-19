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
       01 SUDOKU.
           05 LINHA OCCURS 9 TIMES.
               10 COLUNA PIC X(1) OCCURS 9 TIMES.
       77 CONT1 PIC 99.
       77 CONT2 PIC 99.
       77 J PIC 99.
       77 I PIC 99.
       77 K PIC 99.
       77 X PIC 99.
       77 Y PIC 99.
       77 P PIC 99.
       77 Q PIC 99.
       77 ESPACO PIC X(2).
       77 NUMERO PIC 9.
       77 WIN PIC X(1) VALUE "L".
       77 RANDOM-NUMBER PIC 9.
       77 RANDOM-LINE PIC 9.
       77 RANDOM-COLUMN PIC 9.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           INIT-VALUES.
           PERFORM VARYING CONT1 FROM 1 BY 1 UNTIL CONT1 > 9
               PERFORM VARYING CONT2 FROM 1 BY 1 UNTIL CONT2 > 9
                   MOVE "_" TO COLUNA (CONT1, CONT2)
               END-PERFORM
           END-PERFORM

           PERFORM 10 TIMES
               COMPUTE RANDOM-NUMBER = FUNCTION RANDOM * 9 + 1
               COMPUTE RANDOM-LINE = FUNCTION RANDOM * 9 + 1
               COMPUTE RANDOM-COLUMN = FUNCTION RANDOM * 9 + 1
               MOVE RANDOM-NUMBER TO COLUNA (RANDOM-LINE, RANDOM-COLUMN)
           END-PERFORM

           PERFORM BOARD
           PERFORM PLAY.

           PLAY.
           PERFORM UNTIL WIN = "W"
               DISPLAY "INSIRA UMA LINHA E COLUNA (EX. B7)."
               ACCEPT ESPACO
               DISPLAY "INSIRA UM NUMERO DE 1 A 9."
               ACCEPT NUMERO
               PERFORM CHECK1
               PERFORM CHECK2
           END-PERFORM

           DISPLAY "VENCEU!"
           STOP RUN.

           BOARD.
               DISPLAY "  123 456 789"
           DISPLAY "A|" COLUNA (1,1) COLUNA (1,2) COLUNA (1,3) "|"
           COLUNA (1,4) COLUNA (1,5) COLUNA (1,6) "|" COLUNA (1,7)
           COLUNA (1,8) COLUNA (1,9) "|"
           DISPLAY "B|" COLUNA (2,1) COLUNA (2,2) COLUNA (2,3) "|"
           COLUNA (2,4) COLUNA (2,5) COLUNA (2,6) "|" COLUNA (2,7)
           COLUNA (2,8) COLUNA (2,9) "|"
           DISPLAY "C|" COLUNA (3,1) COLUNA (3,2) COLUNA (3,3) "|"
           COLUNA (3,4) COLUNA (3,5) COLUNA (3,6) "|" COLUNA (3,7)
           COLUNA (3,8) COLUNA (3,9) "|"
           DISPLAY "D|" COLUNA (4,1) COLUNA (4,2) COLUNA (4,3) "|"
           COLUNA (4,4) COLUNA (4,5) COLUNA (4,6) "|" COLUNA (4,7)
           COLUNA (4,8) COLUNA (4,9) "|"
           DISPLAY "E|" COLUNA (5,1) COLUNA (5,2) COLUNA (5,3) "|"
           COLUNA (5,4) COLUNA (5,5) COLUNA (5,6) "|" COLUNA (5,7)
           COLUNA (5,8) COLUNA (5,9) "|"
           DISPLAY "F|" COLUNA (6,1) COLUNA (6,2) COLUNA (6,3) "|"
           COLUNA (6,4) COLUNA (6,5) COLUNA (6,6) "|" COLUNA (6,7)
           COLUNA (6,8) COLUNA (6,9) "|"
           DISPLAY "G|" COLUNA (7,1) COLUNA (7,2) COLUNA (7,3) "|"
           COLUNA (7,4) COLUNA (7,5) COLUNA (7,6) "|" COLUNA (7,7)
           COLUNA (7,8) COLUNA (7,9) "|"
           DISPLAY "H|" COLUNA (8,1) COLUNA (8,2) COLUNA (8,3) "|"
           COLUNA (8,4) COLUNA (8,5) COLUNA (8,6) "|" COLUNA (8,7)
           COLUNA (8,8) COLUNA (8,9) "|"
           DISPLAY "I|" COLUNA (9,1) COLUNA (9,2) COLUNA (9,3) "|"
           COLUNA (9,4) COLUNA (9,5) COLUNA (9,6) "|" COLUNA (9,7)
           COLUNA (9,8) COLUNA (9,9) "|"
           .

           PLACE-IN-BOARD.
      *LINHA A
               IF ESPACO = "A1"
                   MOVE NUMERO TO COLUNA (1,1)
                   PERFORM BOARD
               ELSE IF ESPACO = "A2"
                   MOVE NUMERO TO COLUNA (1,2)
                   PERFORM BOARD
               ELSE IF ESPACO = "A3"
                   MOVE NUMERO TO COLUNA (1,3)
                   PERFORM BOARD
               ELSE IF ESPACO = "A4"
                   MOVE NUMERO TO COLUNA (1,4)
                   PERFORM BOARD
               ELSE IF ESPACO = "A5"
                   MOVE NUMERO TO COLUNA (1,5)
                   PERFORM BOARD
               ELSE IF ESPACO = "A6"
                   MOVE NUMERO TO COLUNA (1,6)
                   PERFORM BOARD
               ELSE IF ESPACO = "A7"
                   MOVE NUMERO TO COLUNA (1,7)
                   PERFORM BOARD
               ELSE IF ESPACO = "A8"
                   MOVE NUMERO TO COLUNA (1,8)
                   PERFORM BOARD
               ELSE IF ESPACO = "A9"
                   MOVE NUMERO TO COLUNA (1,9)
                   PERFORM BOARD

      *LINHA B
               ELSE IF ESPACO = "B1"
                   MOVE NUMERO TO COLUNA (2,1)
                   PERFORM BOARD
               ELSE IF ESPACO = "B2"
                   MOVE NUMERO TO COLUNA (2,2)
                   PERFORM BOARD
               ELSE IF ESPACO = "B3"
                   MOVE NUMERO TO COLUNA (2,3)
                   PERFORM BOARD
               ELSE IF ESPACO = "B4"
                   MOVE NUMERO TO COLUNA (2,4)
                   PERFORM BOARD
               ELSE IF ESPACO = "B5"
                   MOVE NUMERO TO COLUNA (2,5)
                   PERFORM BOARD
               ELSE IF ESPACO = "B6"
                   MOVE NUMERO TO COLUNA (2,6)
                   PERFORM BOARD
               ELSE IF ESPACO = "B7"
                   MOVE NUMERO TO COLUNA (2,7)
                   PERFORM BOARD
               ELSE IF ESPACO = "B8"
                   MOVE NUMERO TO COLUNA (2,8)
                   PERFORM BOARD
               ELSE IF ESPACO = "B9"
                   MOVE NUMERO TO COLUNA (2,9)
                   PERFORM BOARD

      *LINHA C
               ELSE IF ESPACO = "C1"
                   MOVE NUMERO TO COLUNA (3,1)
                   PERFORM BOARD
               ELSE IF ESPACO = "C2"
                   MOVE NUMERO TO COLUNA (3,2)
                   PERFORM BOARD
               ELSE IF ESPACO = "C3"
                   MOVE NUMERO TO COLUNA (3,3)
                   PERFORM BOARD
               ELSE IF ESPACO = "C4"
                   MOVE NUMERO TO COLUNA (3,4)
                   PERFORM BOARD
               ELSE IF ESPACO = "C5"
                   MOVE NUMERO TO COLUNA (3,5)
                   PERFORM BOARD
               ELSE IF ESPACO = "C6"
                   MOVE NUMERO TO COLUNA (3,6)
                   PERFORM BOARD
               ELSE IF ESPACO = "C7"
                   MOVE NUMERO TO COLUNA (3,7)
                   PERFORM BOARD
               ELSE IF ESPACO = "C8"
                   MOVE NUMERO TO COLUNA (3,8)
                   PERFORM BOARD
               ELSE IF ESPACO = "C9"
                   MOVE NUMERO TO COLUNA (3,9)
                   PERFORM BOARD

      *LINHA D
               ELSE IF ESPACO = "D1"
                   MOVE NUMERO TO COLUNA (4,1)
                   PERFORM BOARD
               ELSE IF ESPACO = "D2"
                   MOVE NUMERO TO COLUNA (4,2)
                   PERFORM BOARD
               ELSE IF ESPACO = "D3"
                   MOVE NUMERO TO COLUNA (4,3)
                   PERFORM BOARD
               ELSE IF ESPACO = "D4"
                   MOVE NUMERO TO COLUNA (4,4)
                   PERFORM BOARD
               ELSE IF ESPACO = "D5"
                   MOVE NUMERO TO COLUNA (4,5)
                   PERFORM BOARD
               ELSE IF ESPACO = "D6"
                   MOVE NUMERO TO COLUNA (4,6)
                   PERFORM BOARD
               ELSE IF ESPACO = "D7"
                   MOVE NUMERO TO COLUNA (4,7)
                   PERFORM BOARD
               ELSE IF ESPACO = "D8"
                   MOVE NUMERO TO COLUNA (4,8)
                   PERFORM BOARD
               ELSE IF ESPACO = "D9"
                   MOVE NUMERO TO COLUNA (4,9)
                   PERFORM BOARD

      *LINHA E
               ELSE IF ESPACO = "E1"
                   MOVE NUMERO TO COLUNA (5,1)
                   PERFORM BOARD
               ELSE IF ESPACO = "E2"
                   MOVE NUMERO TO COLUNA (5,2)
                   PERFORM BOARD
               ELSE IF ESPACO = "E3"
                   MOVE NUMERO TO COLUNA (5,3)
                   PERFORM BOARD
               ELSE IF ESPACO = "E4"
                   MOVE NUMERO TO COLUNA (5,4)
                   PERFORM BOARD
               ELSE IF ESPACO = "E5"
                   MOVE NUMERO TO COLUNA (5,5)
                   PERFORM BOARD
               ELSE IF ESPACO = "E6"
                   MOVE NUMERO TO COLUNA (5,6)
                   PERFORM BOARD
               ELSE IF ESPACO = "E7"
                   MOVE NUMERO TO COLUNA (5,7)
                   PERFORM BOARD
               ELSE IF ESPACO = "E8"
                   MOVE NUMERO TO COLUNA (5,8)
                   PERFORM BOARD
               ELSE IF ESPACO = "E9"
                   MOVE NUMERO TO COLUNA (5,9)
                   PERFORM BOARD

      *LINHA F
               ELSE IF ESPACO = "F1"
                   MOVE NUMERO TO COLUNA (6,1)
                   PERFORM BOARD
               ELSE IF ESPACO = "F2"
                   MOVE NUMERO TO COLUNA (6,2)
                   PERFORM BOARD
               ELSE IF ESPACO = "F3"
                   MOVE NUMERO TO COLUNA (6,3)
                   PERFORM BOARD
               ELSE IF ESPACO = "F4"
                   MOVE NUMERO TO COLUNA (6,4)
                   PERFORM BOARD
               ELSE IF ESPACO = "F5"
                   MOVE NUMERO TO COLUNA (6,5)
                   PERFORM BOARD
               ELSE IF ESPACO = "F6"
                   MOVE NUMERO TO COLUNA (6,6)
                   PERFORM BOARD
               ELSE IF ESPACO = "F7"
                   MOVE NUMERO TO COLUNA (6,7)
                   PERFORM BOARD
               ELSE IF ESPACO = "F8"
                   MOVE NUMERO TO COLUNA (6,8)
                   PERFORM BOARD
               ELSE IF ESPACO = "F9"
                   MOVE NUMERO TO COLUNA (6,9)
                   PERFORM BOARD

      *LINHA G
               ELSE IF ESPACO = "G1"
                   MOVE NUMERO TO COLUNA (7,1)
                   PERFORM BOARD
               ELSE IF ESPACO = "G2"
                   MOVE NUMERO TO COLUNA (7,2)
                   PERFORM BOARD
               ELSE IF ESPACO = "G3"
                   MOVE NUMERO TO COLUNA (7,3)
                   PERFORM BOARD
               ELSE IF ESPACO = "G4"
                   MOVE NUMERO TO COLUNA (7,4)
                   PERFORM BOARD
               ELSE IF ESPACO = "G5"
                   MOVE NUMERO TO COLUNA (7,5)
                   PERFORM BOARD
               ELSE IF ESPACO = "G6"
                   MOVE NUMERO TO COLUNA (7,6)
                   PERFORM BOARD
               ELSE IF ESPACO = "G7"
                   MOVE NUMERO TO COLUNA (7,7)
                   PERFORM BOARD
               ELSE IF ESPACO = "G8"
                   MOVE NUMERO TO COLUNA (7,8)
                   PERFORM BOARD
               ELSE IF ESPACO = "G9"
                   MOVE NUMERO TO COLUNA (7,9)
                   PERFORM BOARD

      *LINHA H
               ELSE IF ESPACO = "H1"
                   MOVE NUMERO TO COLUNA (8,1)
                   PERFORM BOARD
               ELSE IF ESPACO = "H2"
                   MOVE NUMERO TO COLUNA (8,2)
                   PERFORM BOARD
               ELSE IF ESPACO = "H3"
                   MOVE NUMERO TO COLUNA (8,3)
                   PERFORM BOARD
               ELSE IF ESPACO = "H4"
                   MOVE NUMERO TO COLUNA (8,4)
                   PERFORM BOARD
               ELSE IF ESPACO = "H5"
                   MOVE NUMERO TO COLUNA (8,5)
                   PERFORM BOARD
               ELSE IF ESPACO = "H6"
                   MOVE NUMERO TO COLUNA (8,6)
                   PERFORM BOARD
               ELSE IF ESPACO = "H7"
                   MOVE NUMERO TO COLUNA (8,7)
                   PERFORM BOARD
               ELSE IF ESPACO = "H8"
                   MOVE NUMERO TO COLUNA (8,8)
                   PERFORM BOARD
               ELSE IF ESPACO = "H9"
                   MOVE NUMERO TO COLUNA (8,9)
                   PERFORM BOARD

      *LINHA I
               ELSE IF ESPACO = "I1"
                   MOVE NUMERO TO COLUNA (9,1)
                   PERFORM BOARD
               ELSE IF ESPACO = "I2"
                   MOVE NUMERO TO COLUNA (9,2)
                   PERFORM BOARD
               ELSE IF ESPACO = "I3"
                   MOVE NUMERO TO COLUNA (9,3)
                   PERFORM BOARD
               ELSE IF ESPACO = "I4"
                   MOVE NUMERO TO COLUNA (9,4)
                   PERFORM BOARD
               ELSE IF ESPACO = "I5"
                   MOVE NUMERO TO COLUNA (9,5)
                   PERFORM BOARD
               ELSE IF ESPACO = "I6"
                   MOVE NUMERO TO COLUNA (9,6)
                   PERFORM BOARD
               ELSE IF ESPACO = "I7"
                   MOVE NUMERO TO COLUNA (9,7)
                   PERFORM BOARD
               ELSE IF ESPACO = "I8"
                   MOVE NUMERO TO COLUNA (9,8)
                   PERFORM BOARD
               ELSE IF ESPACO = "I9"
                   MOVE NUMERO TO COLUNA (9,9)
                   PERFORM BOARD
           .
           CHECK1.
               IF ESPACO = "A1" OR ESPACO = "A2" OR ESPACO = "A3" OR
               ESPACO = "A4" OR ESPACO = "A5" OR ESPACO = "A6" OR
               ESPACO = "A7" OR ESPACO = "A8" OR ESPACO = "A9" OR
               ESPACO = "B1" OR ESPACO = "B2" OR ESPACO = "B3" OR
               ESPACO = "B4" OR ESPACO = "B5" OR ESPACO = "B6" OR
               ESPACO = "B7" OR ESPACO = "B8" OR ESPACO = "B9" OR
               ESPACO = "C1" OR ESPACO = "C2" OR ESPACO = "C3" OR
               ESPACO = "C4" OR ESPACO = "C5" OR ESPACO = "C6" OR
               ESPACO = "C7" OR ESPACO = "C8" OR ESPACO = "C9" OR
               ESPACO = "D1" OR ESPACO = "D2" OR ESPACO = "D3" OR
               ESPACO = "D4" OR ESPACO = "D5" OR ESPACO = "D6" OR
               ESPACO = "D7" OR ESPACO = "D8" OR ESPACO = "D9" OR
               ESPACO = "E1" OR ESPACO = "E2" OR ESPACO = "E3" OR
               ESPACO = "E4" OR ESPACO = "E5" OR ESPACO = "E6" OR
               ESPACO = "E7" OR ESPACO = "E8" OR ESPACO = "E9" OR
               ESPACO = "F1" OR ESPACO = "F2" OR ESPACO = "F3" OR
               ESPACO = "F4" OR ESPACO = "F5" OR ESPACO = "F6" OR
               ESPACO = "F7" OR ESPACO = "F8" OR ESPACO = "F9" OR
               ESPACO = "G1" OR ESPACO = "G2" OR ESPACO = "G3" OR
               ESPACO = "G4" OR ESPACO = "G5" OR ESPACO = "G6" OR
               ESPACO = "G7" OR ESPACO = "G8" OR ESPACO = "G9" OR
               ESPACO = "H1" OR ESPACO = "H2" OR ESPACO = "H3" OR
               ESPACO = "H4" OR ESPACO = "H5" OR ESPACO = "H6" OR
               ESPACO = "H7" OR ESPACO = "H8" OR ESPACO = "H9" OR
               ESPACO = "I1" OR ESPACO = "I2" OR ESPACO = "I3" OR
               ESPACO = "I4" OR ESPACO = "I5" OR ESPACO = "I6" OR
               ESPACO = "I7" OR ESPACO = "I8" OR ESPACO = "I9" AND
               NUMERO = "1" OR NUMERO = "2" OR NUMERO = "3" OR
               NUMERO = "4" OR NUMERO = "5" OR NUMERO = "6" OR
               NUMERO = "7" OR NUMERO = "8" OR NUMERO = "9"
                   PERFORM PLACE-IN-BOARD
               ELSE
                   DISPLAY "ESPACO ENTRE A1 E I9 E"
                   DISPLAY "NUMERO TEM DE SER DE 1 A 9."
                   WITH NO ADVANCING
                   PERFORM PLAY
               END-IF.

           VENCEDOR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 8
                   COMPUTE K = J + 1
                   PERFORM VARYING K FROM K BY 1 UNTIL K > 9
                       IF COLUNA(I, J) = COLUNA(I, K)
                           DISPLAY "HÁ UM DUPLICADO NA LINHA "I""
                           MOVE "L" TO WIN
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM


           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 9
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
                   COMPUTE K = I + 1
                   PERFORM VARYING K FROM K BY 1 UNTIL K > 9
                       IF COLUNA(I, J) = COLUNA(K, J)
                           DISPLAY "HÁ UM DUPLICADO NA COLUNA "J""
                           MOVE "L" TO WIN
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM


           PERFORM VARYING I FROM 1 BY 3 UNTIL I > 7
           PERFORM VARYING J FROM 1 BY 3 UNTIL J > 7
               PERFORM VARYING X FROM I BY 1 UNTIL X > I + 2
                   PERFORM VARYING Y FROM J BY 1 UNTIL Y > J + 2
                       PERFORM VARYING P FROM I BY 1 UNTIL P > I + 2
                           PERFORM VARYING Q FROM J BY 1 UNTIL Q > J + 2
                               IF COLUNA(X, Y) NOT = COLUNA(P, Q) AND
                                   COLUNA(X, Y) = COLUNA(P, Q)
                                   DISPLAY "EXISTE DUPLICADO"
                                   DISPLAY "(" I "," J ")"
                                   WITH NO ADVANCING
                                   MOVE "L" TO WIN
                                   PERFORM PLAY
                               ELSE
                                   MOVE "W" TO WIN
                               END-IF
                           END-PERFORM
                       END-PERFORM
                   END-PERFORM
               END-PERFORM
           END-PERFORM
           END-PERFORM.

           CHECK2.
               IF COLUNA (1,1) = "_" OR COLUNA (1,2) = "_" OR
                COLUNA (1,3) = "_" OR COLUNA (1,4) = "_" OR
                COLUNA (1,5) = "_" OR COLUNA (1,6) = "_" OR
                COLUNA (1,7) = "_" OR COLUNA (1,8) = "_" OR
                COLUNA (1,9) = "_" OR

                COLUNA (2,1) = "_" OR COLUNA (2,2) = "_" OR
                COLUNA (2,3) = "_" OR COLUNA (2,4) = "_" OR
                COLUNA (2,5) = "_" OR COLUNA (2,6) = "_" OR
                COLUNA (2,7) = "_" OR COLUNA (2,8) = "_" OR
                COLUNA (2,9) = "_" OR

                COLUNA (3,1) = "_" OR COLUNA (3,2) = "_" OR
                COLUNA (3,3) = "_" OR COLUNA (3,4) = "_" OR
                COLUNA (3,5) = "_" OR COLUNA (3,6) = "_" OR
                COLUNA (3,7) = "_" OR COLUNA (3,8) = "_" OR
                COLUNA (3,9) = "_" OR

                COLUNA (4,1) = "_" OR COLUNA (4,2) = "_" OR
                COLUNA (4,3) = "_" OR COLUNA (4,4) = "_" OR
                COLUNA (4,5) = "_" OR COLUNA (4,6) = "_" OR
                COLUNA (4,7) = "_" OR COLUNA (4,8) = "_" OR
                COLUNA (4,9) = "_" OR

                COLUNA (5,1) = "_" OR COLUNA (5,2) = "_" OR
                COLUNA (5,3) = "_" OR COLUNA (5,4) = "_" OR
                COLUNA (5,5) = "_" OR COLUNA (5,6) = "_" OR
                COLUNA (5,7) = "_" OR COLUNA (5,8) = "_" OR
                COLUNA (5,9) = "_" OR

                COLUNA (6,1) = "_" OR COLUNA (6,2) = "_" OR
                COLUNA (6,3) = "_" OR COLUNA (6,4) = "_" OR
                COLUNA (6,5) = "_" OR COLUNA (6,6) = "_" OR
                COLUNA (6,7) = "_" OR COLUNA (6,8) = "_" OR
                COLUNA (6,9) = "_" OR

                COLUNA (7,1) = "_" OR COLUNA (7,2) = "_" OR
                COLUNA (7,3) = "_" OR COLUNA (7,4) = "_" OR
                COLUNA (7,5) = "_" OR COLUNA (7,6) = "_" OR
                COLUNA (7,7) = "_" OR COLUNA (7,8) = "_" OR
                COLUNA (7,9) = "_" OR

                COLUNA (8,1) = "_" OR COLUNA (8,2) = "_" OR
                COLUNA (8,3) = "_" OR COLUNA (8,4) = "_" OR
                COLUNA (8,5) = "_" OR COLUNA (8,6) = "_" OR
                COLUNA (8,7) = "_" OR COLUNA (8,8) = "_" OR
                COLUNA (8,9) = "_" OR

                COLUNA (9,1) = "_" OR COLUNA (9,2) = "_" OR
                COLUNA (9,3) = "_" OR COLUNA (9,4) = "_" OR
                COLUNA (9,5) = "_" OR COLUNA (9,6) = "_" OR
                COLUNA (9,7) = "_" OR COLUNA (9,8) = "_" OR
                COLUNA (9,9) = "_"
                   PERFORM PLAY
               ELSE
                   PERFORM VENCEDOR.



       END PROGRAM YOUR-PROGRAM-NAME.
