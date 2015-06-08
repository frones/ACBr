CLS
ECHO OFF
ECHO ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
ECHO ³  Esse arquivo de Lote permite demonstarar o funcionamento do  ³
ECHO ³  ACBrMonitor monitorando arquivos TXT.                        ³
ECHO ³                                                               ³
ECHO ³  - O ACBrMonitor deve estar configurado para TXT              ³
ECHO ³  - O arquivo de Entrada deve ser ENT.TXT e o de saida SAI.TXT ³
ECHO ³  - Configure e Teste o MODELO e PORTA do ECF no ACBrMonitor   ³
ECHO ³  - Deixe o ACBrmonitor Aberto para visualizar as Respostas    ³
ECHO ³                                                               ³
ECHO ³ Pressione CTRL-C para interromper ou ENTER para continuar     ³
ECHO ³       ( Ligue e configure o ECF antes de continuar )          ³
ECHO ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
pause

:ATIVAR
ECHO *** Ativando o ECF ***
ECHO ECF.Ativar > ..\ENT.TXT

:INICIO
ECHO ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
ECHO ³  0 - Ativar ACBrECF                    ³
ECHO ³  1 - Imprime uma Leitura X             ³
ECHO ³  2 - Imprime uma Cupom Fiscal          ³
ECHO ³  3 - Cancela o Ultimo Cupom            ³
ECHO ³  4 - Retorna Informacoes da Impressora ³
ECHO ³  5 - Reducao Z                         ³
ECHO ³                                        ³
ECHO ³  9 - Voltar para o DOS                 ³
ECHO ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
choice /c:0123459

if errorlevel 7 GOTO FIM
if errorlevel 6 GOTO REDUCAOZ
if errorlevel 5 GOTO INFO
if errorlevel 4 GOTO CANCELA
if errorlevel 3 GOTO CUPOM
if errorlevel 2 GOTO LEITURAX
if errorlevel 1 GOTO ATIVAR

GOTO INICIO

:REDUCAOZ
ECHO ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
ECHO ³  Deseja realmente imprimir uma REDUCAO Z ? ³
ECHO ³                                            ³
ECHO ³        1 - S I M      2 - N A O            ³
ECHO ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
choice /c:12

if errorlevel 2 GOTO INICIO

ECHO ECF.ReducaoZ > ..\ENT.TXT
GOTO INICIO

:LEITURAX
ECHO ECF.LeituraX > ..\ENT.TXT
GOTO INICIO

:CUPOM
REM *** Criando Arquivo ENT.TMP (temporario) com todos os comandos ***
ECHO ECF.AbreCupom("12345678901")  > ENT.TMP
ECHO ECF.VendeItem("789012456789","PRODUTO TESTE, ISENTO","II", 1.5, 2.5 ) >> ENT.TMP
ECHO ECF.VendeItem("789012223233","PRODUTO TESTE, TRIBUTADO","18", 1, 100, 10, "LT" ) >> ENT.TMP
ECHO ECF.VendeItem("789012443434","PRODUTO COM A DESCRICAO GRANDE. M U I T O   G R A N D E   SERA QUE IMPRIME TUDO ?","18", 1, 5, 0, "UN" ) >> ENT.TMP
ECHO ECF.SubTotalizaCupom(-0.75) >> ENT.TMP
ECHO ECF.EfetuaPagamento("01",100,"DEVE VOLTAR TROCO DE 1,25") >> ENT.TMP
ECHO ECF.FechaCupom("Componentes ACBr|http://acbr.sourceforge.net") >> ENT.TMP

REM *** copiando para o arquivo de Entrada **
copy ENT.TMP ..\ENT.TXT

GOTO INICIO

:CANCELA
ECHO ECF.CancelaCupom > ..\ENT.TXT
GOTO INICIO

:INFO
del ..\SAI.TXT
cls
ECHO ECF.DataHora >  ENT.TMP
ECHO ECF.NumCupom >> ENT.TMP
ECHO ECF.NumSerie >> ENT.TMP
copy ENT.TMP ..\ENT.TXT

ECHO ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
ECHO ³ Pressione ENTER para ver         ³
ECHO ³ DataHora,  NumCupom  e  NumSerie ³
ECHO ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
pause

type ..\SAI.TXT
GOTO INICIO

:FIM
ECHO FIM >> ..\ENT.TXT
