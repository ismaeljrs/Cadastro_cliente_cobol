      *================================================================*
      *   PROGRAMA   : PROGVAR2                                        *
      *   ANALISTA   : RICARDO COSTA COUTINHO                          *
      *   PROGRAMADOR: ISAMEL JORGE BRANDAO                            *
      *   OBJETIVO   : DESENVOLVER UM PROGRAMA DE CADASTRO DE CLIENTE  *
      *================================================================*
      *                       HISTORICO DE VERSOES                     *
      *----------------------------------------------------------------*
      *  VRS |        AUTOR        |     DATA     |     DESCRICAO      *
      *----------------------------------------------------------------*
      *  003 |  ISMAEL J. BRANDAO  |  03/02/2025  |   IMPLEMENTACAO    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGVAR2.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WRK-CONT              PIC 9(10).
       77 WRK-ENTER             PIC X(01).
       77 WRK-OPC1               PIC 9(01).
       77 WRK-OPC2              PIC 9(01).
       77 WRK-OPC3              PIC 9(01).
       77 WRK-OPC4              PIC 9(01).
       77 WRK-PASSOU            PIC 9(20) VALUE 0.
       77 I                     PIC 9(10) VALUE 1.
       77 CLI-N                 PIC X(20).
       77 WRK-TAM               PIC 9(03) VALUE 0.

      ** LOOPS / LAÇOS DE REPETIÇÃO
       77 PARA                 PIC X VALUE 'N'.
           88 FIM-LOOP VALUE 'S'.
       77 PARA1                 PIC X VALUE 'N'.
           88 FIM-LOOP1 VALUE 'S'.

      ** LOOPS CADASTO DO CLIENTE 
       77 WRK-CAD-CLI-LOOP1    PIC X VALUE 'N'.
           88 CAD-CLI-FIM1 VALUE 'S'.
       77 WRK-CAD-CLI-LOOP2    PIC X VALUE 'N'.
           88 CAD-CLI-FIM1 VALUE 'S'.
       77 WRK-CAD-CLI-LOOP3    PIC X VALUE 'N'.
           88 CAD-CLI-FIM3 VALUE 'S'.





      ** ---------------------------------------------------------------
      ** LOGUIN
       77 LOG-EMAIL             PIC X(40) VALUE SPACES.
       77 LOG-SENHA             PIC X(40) VALUE SPACES.

      ** CADASTRO
       01 CADASTRO.
           05 CAD-NOME          PIC X(30) VALUE SPACES.
           05 CAD-DATA.
               10 DT-DIA          PIC 9(02) VALUE ZEROS.
               10 DT-MES          PIC 9(02) VALUE ZEROS.
               10 DT-ANO          PIC 9(04) VALUE ZEROS.
           05 CAD-GENE          PIC X(01) VALUE SPACES.
           05 CAD-CPF           PIC 9(11) VALUE ZEROS.
           05 CAD-TELEFONE.
               10 TEL-DD          PIC 9(02) VALUE ZEROS.
               10 TEL-NUMERO      PIC 9(09) VALUE ZEROS.
           05 CAD-CEP           PIC 9(08) VALUE ZEROS.
           05 CAD-NUMERO        PIC X(5) VALUE SPACES.
           05 CAD-UF            PIC X(02) VALUE SPACES.
           05 CAD-ENDERECO      PIC X(30) VALUE SPACES.
           05 CAD-COMPLEMENTO   PIC X(30) VALUE SPACES.
           05 CAD-EMAIL         PIC X(30) VALUE SPACES.
           05 CAD-SENHA         PIC X(30) VALUE SPACES.

      *-----------------------------------------------------------------*
      *** -  TELA
       SCREEN SECTION.
       01 LIMPA-TELA.
           05 BLANK SCREEN.
       01 LINHA-TELA.

      **linha esquerda
      *----linha 1
           10 LINE 01 COLUMN 39 VALUE '╔'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      * linhas em cima ---
           10 LINE 01 COLUMN 40 VALUE
           '══════════════════════════════════════════════════════════'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 2
           10 LINE 02 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 3
           10 LINE 03 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 4
           10 LINE 04 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 5
           10 LINE 05 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 6
           10 LINE 06 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 7
           10 LINE 07 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 8
           10 LINE 08 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 9
           10 LINE 09 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 10
           10 LINE 10 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 11
           10 LINE 11 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 12
           10 LINE 12 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 13
           10 LINE 13 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 14
           10 LINE 14 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 15
           10 LINE 15 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 16
           10 LINE 16 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 17
           10 LINE 17 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 18
           10 LINE 18 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 19
           10 LINE 19 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 20
           10 LINE 20 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 21
           10 LINE 21 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 22
           10 LINE 22 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 23
           10 LINE 23 COLUMN 39 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
      *----linha 24
           10 LINE 24 COLUMN 39 VALUE '╚'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.

      ** linha da direita
           10 LINE 1 COLUMN 98 VALUE '╗'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 2 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 3 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 4 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 5 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 6 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 7 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 8 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 9 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 10 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 11 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 12 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 13 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 14 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 15 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 16 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 17 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 18 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 19 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 20 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 21 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 22 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 23 COLUMN 98 VALUE '║'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.

           10 LINE 24 COLUMN 40 VALUE
           '══════════════════════════════════════════════════════════╝'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
       01 LOGUIN.

           10 LINE 02 COLUMN 40 VALUE
           '                        ──────────                        '
                   FOREGROUND-COLOR 7
                   BACKGROUND-COLOR 4.
           10 LINE 03 COLUMN 40 VALUE
           '                        AMERICANAS                        '
                   FOREGROUND-COLOR 7
                   BACKGROUND-COLOR 4.
           10 LINE 04 COLUMN 40 VALUE
           '                        ──────────                        '
                   FOREGROUND-COLOR 7
                   BACKGROUND-COLOR 4.

           10 LINE 05 COLUMN 40 VALUE
           '──────────────────────────────────────────────────────────'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.


           10 LINE 06 COLUMN 40 VALUE
           '                    LOGUIN  DO  CLIENTE                   '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.


           10 LINE 07 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.


           10 LINE 08 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.


           10 LINE 09 COLUMN 40 VALUE
           '      E-MAIL                                              '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 11 COLUMN 47 USING LOG-EMAIL
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.

           10 LINE 10 COLUMN 40 VALUE
           '       '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 11 COLUMN 40 VALUE
           '       '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 12 COLUMN 40 VALUE
           '       '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 10 COLUMN 46 VALUE
           '┌──────────────────────────────────────────────┐'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.


           10 LINE 11 COLUMN 46 VALUE
           '│                                              │'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.


           10 LINE 12 COLUMN 46 VALUE
           '└──────────────────────────────────────────────┘'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 10 COLUMN 94 VALUE
           '    '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 11 COLUMN 94 VALUE
           '    '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 12 COLUMN 94 VALUE
           '    '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.

           10 LINE 13 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 15 COLUMN 40 VALUE
           '       '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 16 COLUMN 40 VALUE
           '       '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 17 COLUMN 40 VALUE
           '       '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.

           10 LINE 14 COLUMN 40 VALUE
           '      SENHA                                               '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 16 COLUMN 47 USING LOG-SENHA
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.

           10 LINE 15 COLUMN 46 VALUE
           '┌──────────────────────────────────────────────┐'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.


           10 LINE 16 COLUMN 46 VALUE
           '│                                              │'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.


           10 LINE 17 COLUMN 46 VALUE
           '└──────────────────────────────────────────────┘'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.

           10 LINE 15 COLUMN 94 VALUE
           '    '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 16 COLUMN 94 VALUE
           '    '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 17 COLUMN 94 VALUE
           '    '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 18 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.


           10 LINE 19 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 20 COLUMN 40 VALUE
           '  '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 20 COLUMN 42 VALUE
            '1 - sair'
                    FOREGROUND-COLOR 7
                    BACKGROUND-COLOR 4.
           10 LINE 20 COLUMN 50 VALUE
           '      '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 20 COLUMN 56 VALUE
            '2 - cadastrar usuario'
                    FOREGROUND-COLOR 7
                    BACKGROUND-COLOR 6.
           10 LINE 20 COLUMN 77 VALUE
           '       '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 20 COLUMN 84 VALUE
            '3 - entrar'
                    FOREGROUND-COLOR 7
                    BACKGROUND-COLOR 2.
           10 LINE 20 COLUMN 94 VALUE
           '    '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.


          10 LINE 21 COLUMN 40 VALUE
          '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.


           10 LINE 22 COLUMN 40 VALUE
           '                         OPCAO___                         '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 22 COLUMN 71 USING WRK-OPC1
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.


           10 LINE 23 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.



       01 INF-ERRO-LOGUIN.
           05 LOG-ERRO-EMAIL.
               10 LINE 13 COLUMN 46 VALUE
              'Preencha o campo Email antes de continuar'
              FOREGROUND-COLOR 4
              BACKGROUND-COLOR 7.
           05 LOG-ERRO-EMAIL2.
               10 LINE 13 COLUMN 46 VALUE
              'O e-mail informado deve conter @gmail.com.'
              FOREGROUND-COLOR 4
              BACKGROUND-COLOR 7.
           05 LOG-ERRO-EMAIL3.
               10 LINE 13 COLUMN 46 VALUE
              'O endereço de E-mail e muito Pequeno'
              FOREGROUND-COLOR 4
              BACKGROUND-COLOR 7.
           05 LOG-ERRO-EMAIL4.
               10 LINE 13 COLUMN 46 VALUE
              'O endereço de E-mail e muito Grande'
              FOREGROUND-COLOR 4
              BACKGROUND-COLOR 7.
           05 LOG-ERRO-SENHA.
               10 LINE 18 COLUMN 46 VALUE
              'Preencha o campo senha antes de continuar'
              FOREGROUND-COLOR 4
              BACKGROUND-COLOR 7.
           05 LOG-ERRO-OPCAO.
               10 LINE 23 COLUMN 53 VALUE
              'DIGITE APENAS NUMEROS DE 1 A 3 '
              FOREGROUND-COLOR 4
              BACKGROUND-COLOR 7.

           05 CAD-ERRO-NOME.
               10 LINE 9 COLUMN 40 VALUE
              'Nome muito pequeno para digite um nome Valido'
              FOREGROUND-COLOR 4
              BACKGROUND-COLOR 7.
           05 CAD-ERRO-NOME1.
               10 LINE 9 COLUMN 40 VALUE
              'Nome Muito Grande digite um Nome valido'
              FOREGROUND-COLOR 4
              BACKGROUND-COLOR 7.
           05 CAD-ERRO-NOME2.
               10 LINE 9 COLUMN 40 VALUE
              'Preencha o campo Nome antes de continuar'
              FOREGROUND-COLOR 4
              BACKGROUND-COLOR 7.

       01 PRENCHIMENTO-LINHA.
           05 LINE 2 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 3 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 4 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 5 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 6 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 7 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 8 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 9 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 10 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 11 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 12 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 13 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 14 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 15 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 16 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 17 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 18 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 19 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 20 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 21 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 22 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           05 LINE 23 COLUMN 40 VALUE
           '                                                          '
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
       01 CADASTRO-CLIENTE1.
           10 LINE 2 COLUMN 59 VALUE
           'CADASTRO DO CLIENTE'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 3 COLUMN 59 VALUE
           '───────────────────'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 4 COLUMN 40 VALUE
           '1. DADOS PESSOAIS                                         '
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 5 COLUMN 40 VALUE
           'NOME COMPLETO                                             '
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 6 COLUMN 40 VALUE
           '┌──────────────────────────────────────────────────────┐'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 7 COLUMN 40 VALUE
           '│                                                      │'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 8 COLUMN 40 VALUE
           '└──────────────────────────────────────────────────────┘'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 7 COLUMN 41 USING CAD-NOME
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 10 COLUMN 40 VALUE
           'DATA DE NASCIMENTO'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 11 COLUMN 40 VALUE
           '┌─────────────────┐'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 12 COLUMN 40 VALUE
           '│     /     /     │'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 13 COLUMN 40 VALUE
           '└─────────────────┘'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 12 COLUMN 42 USING DT-DIA
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 12 COLUMN 48 USING DT-MES
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 12 COLUMN 54 USING DT-ANO
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 10 COLUMN 60 VALUE
           'GENERO F/M'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 11 COLUMN 60 VALUE
           '┌─────────┐'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 12 COLUMN 60 VALUE
           '│         │'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 13 COLUMN 60 VALUE
           '└─────────┘'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 12 COLUMN 65 USING CAD-GENE
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 10 COLUMN 71 VALUE
           'CPF'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 11 COLUMN 71 VALUE
           '┌───────────────────────┐'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 12 COLUMN 71 VALUE
           '│                       │'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 13 COLUMN 71 VALUE
           '└───────────────────────┘'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 12 COLUMN 72 USING CAD-CPF
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 15 COLUMN 40 VALUE
           'DD'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 16 COLUMN 40 VALUE
           '┌───┐'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 17 COLUMN 40 VALUE
           '│   │'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 18 COLUMN 40 VALUE
           '└───┘'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 17 COLUMN 41 USING TEL-DD
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 15 COLUMN 45 VALUE
           'TELEFONE'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 16 COLUMN 45 VALUE
           '┌───────────────────────┐'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 17 COLUMN 45 VALUE
           '│                       │'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 18 COLUMN 45 VALUE
           '└───────────────────────┘'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 17 COLUMN 46 USING TEL-NUMERO
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 20 COLUMN 63 VALUE
           'OPCAO ___'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 20 COLUMN 70 USING WRK-OPC2
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 21 COLUMN 41 VALUE
           '1 - VOLTAR'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 21 COLUMN 85 VALUE
           '2 - PROXIMO'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
      *01 INF-ERRO-CADAST1.
      *    05 EMAIL1
      *        10 LINE 13 COLUMN 40 VALUE
      *       'Preencha o campo Email antes de continuar'
      *       FOREGROUND-COLOR 4
      *       BACKGROUND-COLOR 7.
       01 CADASTRO-CLIENTE2.
           10 LINE 2 COLUMN 59 VALUE
           'CADASTRO DO CLIENTE'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 3 COLUMN 59 VALUE
           '───────────────────'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 4 COLUMN 40 VALUE
           '2. ENDERECO'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 5 COLUMN 40 VALUE
           'CEP'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 6 COLUMN 40 VALUE
           '┌───────────────────────┐'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 7 COLUMN 40 VALUE
           '│                       │'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 8 COLUMN 40 VALUE
           '└───────────────────────┘'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 7 COLUMN 41 USING CAD-CEP
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 5 COLUMN 66 VALUE
           'NUMERO'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 6 COLUMN 66 VALUE
           '┌───────┐'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 7 COLUMN 66 VALUE
           '│       │'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 8 COLUMN 66 VALUE
           '└───────┘'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 7 COLUMN 67 USING CAD-NUMERO
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 5 COLUMN 75 VALUE
           'UF'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 6 COLUMN 75 VALUE
           '┌─────┐'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 7 COLUMN 75 VALUE
           '│     │'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 8 COLUMN 75 VALUE
           '└─────┘'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 7 COLUMN 76 USING CAD-UF
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.


           10 LINE 10 COLUMN 40 VALUE
           'ENDERECO'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 11 COLUMN 40 VALUE
           '┌──────────────────────────────────────────────────────┐'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 12 COLUMN 40 VALUE
           '│                                                      │'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 13 COLUMN 40 VALUE
           '└──────────────────────────────────────────────────────┘'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 12 COLUMN 41 USING CAD-ENDERECO
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 15 COLUMN 40 VALUE
           'COMPLEMENTO'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 16 COLUMN 40 VALUE
           '┌──────────────────────────────────────────────────────┐'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 17 COLUMN 40 VALUE
           '│                                                      │'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 18 COLUMN 40 VALUE
           '└──────────────────────────────────────────────────────┘'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 17 COLUMN 41 USING CAD-COMPLEMENTO
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 20 COLUMN 63 VALUE
           'OPCAO ___'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 20 COLUMN 70 USING WRK-OPC3
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 21 COLUMN 41 VALUE
           '1 - VOLTAR'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 21 COLUMN 85 VALUE
           '2 - PROXIMO'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
      *01 INF-ERRO-CADAST2.
      *    05 EMAIL1
      *        10 LINE 13 COLUMN 40 VALUE
      *       'Preencha o campo Email antes de continuar'
      *       FOREGROUND-COLOR 4
      *       BACKGROUND-COLOR 7.

       01 CADASTRO-CLIENTE3.

           10 LINE 2 COLUMN 59 VALUE
           'CADASTRO DO CLIENTE'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 3 COLUMN 59 VALUE
           '───────────────────'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 4 COLUMN 40 VALUE
           '3. ACESSO AO SISTEMA'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 7 COLUMN 40 VALUE
           'E-MAIL'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 8 COLUMN 40 VALUE
           '┌──────────────────────────────────────────────────────┐'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 9 COLUMN 40 VALUE
           '│                                                      │'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 10 COLUMN 40 VALUE
           '└──────────────────────────────────────────────────────┘'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 9 COLUMN 41 USING  CAD-EMAIL
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 12 COLUMN 40 VALUE
           'SENHA'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 13 COLUMN 40 VALUE
           '┌──────────────────────────────────────────────────────┐'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 14 COLUMN 40 VALUE
           '│                                                      │'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 15 COLUMN 40 VALUE
           '└──────────────────────────────────────────────────────┘'
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 14 COLUMN 41 USING  CAD-SENHA
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.

           10 LINE 20 COLUMN 63 VALUE
           'OPCAO ___'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 20 COLUMN 70 USING WRK-OPC4
                   FOREGROUND-COLOR 0
                   BACKGROUND-COLOR 7.
           10 LINE 21 COLUMN 41 VALUE
           '1 - VOLTAR'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.
           10 LINE 21 COLUMN 85 VALUE
           '2 - SALVAR'
                 FOREGROUND-COLOR 0
                 BACKGROUND-COLOR 7.

      *01 INF-ERRO-CADAST3.
      *    05 EMAIL1
      *        10 LINE 13 COLUMN 40 VALUE
      *       'Preencha o campo Email antes de continuar'
      *       FOREGROUND-COLOR 4
      *       BACKGROUND-COLOR 7.


       PROCEDURE DIVISION.
           0001-PRINCIPAL                                      SECTION.
               PERFORM 1000-LOGUIN.
               PERFORM 10000-FINALIZAR.
           1000-LOGUIN.
             MOVE 0 TO WRK-CONT
             PERFORM UNTIL FIM-LOOP
               MOVE 0 TO WRK-CONT
               ACCEPT LIMPA-TELA
      *** TELA LOGUIN]
               DISPLAY LINHA-TELA
               ACCEPT LOGUIN
               IF WRK-OPC1 = 3
                  IF LOG-EMAIL = ' '
                   DISPLAY LOG-ERRO-EMAIL
                   ADD 1 TO WRK-CONT
                  END-IF
                   IF LOG-SENHA  = ' '
                   DISPLAY LOG-ERRO-SENHA
                   ADD 1 TO WRK-CONT
                   END-IF
               END-IF
               IF WRK-CONT = 0
                   EVALUATE WRK-OPC1
                   WHEN 1
                     PERFORM 10000-FINALIZAR
                         SET FIM-LOOP TO TRUE
                   WHEN 2
                     PERFORM 2000-CADASTRO-CLIENTE
                   WHEN 3
                     PERFORM 3000-ENTROU-PAG
                   WHEN OTHER
                       DISPLAY LOG-ERRO-OPCAO
                       ACCEPT WRK-ENTER
                   END-EVALUATE
                ELSE
                   ACCEPT WRK-ENTER
                   CONTINUE
              END-IF
             END-PERFORM.

           2000-CADASTRO-CLIENTE.
      *** TELA CADASTRO CLIENTE DADOS PESSOAS
             PERFORM UNTIL FIM-LOOP1
               ACCEPT LIMPA-TELA
               DISPLAY PRENCHIMENTO-LINHA
               DISPLAY LINHA-TELA
               ACCEPT CADASTRO-CLIENTE1
               IF CAD-NOME = ' '
                   DISPLAY CAD-ERRO-NOME2
               END-IF
               IF WRK-OPC2 = 1
                   PERFORM 10000-FINALIZAR
                         SET FIM-LOOP1 TO TRUE
               END-IF
               
      *        IF WRK-OPC2 <> 1 OR WRK-OPC2 <> 2
      *            WRK-ENTER
      *        END-IF
      *** TELA CADASTRO CLIENTE ENDERECO
                ACCEPT LIMPA-TELA
                DISPLAY PRENCHIMENTO-LINHA
                DISPLAY LINHA-TELA
                ACCEPT CADASTRO-CLIENTE2
                ACCEPT WRK-ENTER

      *** TELA CADASTRO CLIENTE ACESSO AO SISTEMA
                ACCEPT LIMPA-TELA
                DISPLAY PRENCHIMENTO-LINHA
                DISPLAY LINHA-TELA
                ACCEPT CADASTRO-CLIENTE3
                ACCEPT WRK-ENTER
             END-PERFORM.
      *** ENTRANDO NO ENTRAR
           3000-ENTROU-PAG.
             ACCEPT LIMPA-TELA.
             DISPLAY LINHA-TELA.
             ACCEPT WRK-ENTER.

      *** ENTRANDO NO SISTEMA
           10000-FINALIZAR.
               STOP RUN.
