unit ACBrLibSATTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

const
  CLibSATNome = 'ACBrLibSAT';

type

  { TTestACBrSATLib }

  TTestACBrSATLib = class(TTestCase)
  published
    procedure Test_SAT_Inicializar_Com_DiretorioInvalido;
    procedure Test_SAT_Inicializar;
    procedure Test_SAT_Inicializar_Ja_Inicializado;
    procedure Test_SAT_Finalizar;
    procedure Test_SAT_Finalizar_Ja_Finalizado;
    procedure Test_SAT_Nome_Obtendo_LenBuffer;
    procedure Test_SAT_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_SAT_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_SAT_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_SAT_Versao;
    procedure Test_SAT_ConfigLerValor;
    procedure Test_SAT_ConfigGravarValor;
    procedure Test_SAT_CriarCFe;
    procedure Test_SAT_EnviarCFe;
    procedure Test_SAT_CriarEnviarCFe;
    procedure Test_SAT_ImpressaoExtratoFortes;
    procedure Test_SAT_ImpressaoExtratoEscPOS;
    procedure Test_SAT_ImpressaoExtratoPDF_Sem_Path;
    procedure Test_SAT_ImpressaoExtratoPDF_Com_Path;
  end;

implementation

uses
  Printers, OSPrinters,
  ACBrLibSATStaticImport, ACBrLibSATConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrSATLib.Test_SAT_Inicializar_Com_DiretorioInvalido;
begin
  SAT_Finalizar();
  AssertEquals(ErrDiretorioNaoExiste, SAT_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrSATLib.Test_SAT_Inicializar;
begin
  AssertEquals(ErrOk, SAT_Inicializar('',''));
end;

procedure TTestACBrSATLib.Test_SAT_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, SAT_Inicializar('',''));
end;

procedure TTestACBrSATLib.Test_SAT_Finalizar;
begin
  AssertEquals(ErrOk, SAT_Finalizar());
end;

procedure TTestACBrSATLib.Test_SAT_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, SAT_Finalizar());
end;

procedure TTestACBrSATLib.Test_SAT_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, SAT_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibSATNome), Bufflen);
end;

procedure TTestACBrSATLib.Test_SAT_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibSATNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, SAT_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibSATNome), Bufflen);
  AssertEquals(CLibSATNome, AStr);
end;

procedure TTestACBrSATLib.Test_SAT_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibSATNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, SAT_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibSATNome), Bufflen);
  AssertEquals(CLibSATNome, AStr);
end;

procedure TTestACBrSATLib.Test_SAT_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, SAT_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibSATNome), Bufflen);
  AssertEquals(copy(CLibSATNome,1,4), AStr);
end;

procedure TTestACBrSATLib.Test_SAT_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, SAT_Versao(Nil, Bufflen));
  Assert(Bufflen > 0);

  // Lendo a resSATta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, SAT_Versao(PChar(AStr), Bufflen));
  Assert(Bufflen > 0);
  Assert(AStr <> '');
end;

procedure TTestACBrSATLib.Test_SAT_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, SAT_ConfigLerValor(CSessaoVersao, CACBrLib, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CACBrLibVersaoConfig, AStr);
end;

procedure TTestACBrSATLib.Test_SAT_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, SAT_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrSATLib.Test_SAT_CriarCFe;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, SAT_Inicializar('',''));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveModelo, '1'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveCodigoDeAtivacao, 'sefaz1234'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveSignAC, '111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111'));
  AssertEquals(ErrOK, SAT_ConfigGravar(''));
  AssertEquals(ErrOK, SAT_InicializarSAT);

   // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao tentar criar o CFe', ErrOK, SAT_CriarCFe('..\CFe.ini', PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, SAT_UltimoRetorno(PChar(AStr), Bufflen));
  end;

  AssertEquals(ErrOK, SAT_Finalizar());
end;

procedure TTestACBrSATLib.Test_SAT_EnviarCFe;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, SAT_Inicializar('',''));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveCodigoDeAtivacao, 'sefaz1234'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveSignAC, '111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111'));
  AssertEquals(ErrOK, SAT_ConfigGravar(''));
  AssertEquals(ErrOK, SAT_InicializarSAT);

   // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao tentar enviar o CFe', ErrOK, SAT_EnviarCFe('..\001-000000-satcfe.xml', PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, SAT_UltimoRetorno(PChar(AStr), Bufflen));
  end;

  AssertEquals(ErrOK, SAT_Finalizar());
end;

procedure TTestACBrSATLib.Test_SAT_CriarEnviarCFe;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, SAT_Inicializar('',''));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveModelo, '1'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveCodigoDeAtivacao, 'sefaz1234'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveSignAC, '111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111111111111122222222222222111111111111112222222222222211111111111111222222222222221111'));
  AssertEquals(ErrOK, SAT_ConfigGravar(''));
  AssertEquals(ErrOK, SAT_InicializarSAT);

   // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao tentar criar e enviar o CFe', ErrOK, SAT_CriarEnviarCFe('..\CFe.ini', PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, SAT_UltimoRetorno(PChar(AStr), Bufflen));
  end;

  AssertEquals(ErrOK, SAT_Finalizar());
end;

procedure TTestACBrSATLib.Test_SAT_ImpressaoExtratoFortes;
var
  NomeImpressoraPDF: String;
  I: Integer;
begin
  NomeImpressoraPDF := '';
  I := 0;
  while (I < Printer.Printers.Count) and (NomeImpressoraPDF = '') do
  begin
    if (pos(' PDF', UpperCase(Printer.Printers[I])) > 0) then
      NomeImpressoraPDF := Printer.Printers[I];

    Inc( I );
  end;

  AssertEquals(ErrOk, SAT_Inicializar('',''));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveModelo, '1'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoExtrato, CChaveTipo, '0'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoExtrato, CChavePrinterName, PChar(NomeImpressoraPDF)));
  AssertEquals(ErrOK, SAT_ConfigGravar(''));
  AssertEquals(ErrOK, SAT_InicializarSAT);
  AssertEquals(ErrOK, SAT_ImprimirExtratoVenda('..\AD35180911111111111111591234567890001684429520.xml', ''));

  AssertEquals(ErrOK, SAT_Finalizar());
end;

procedure TTestACBrSATLib.Test_SAT_ImpressaoExtratoEscPOS;
var
  SaidaImpressao: String;
begin
  SaidaImpressao := ApplicationPath+'posprinter.txt';
  AssertEquals(ErrOk, SAT_Inicializar('',''));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveModelo, '1'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoExtrato, CChaveTipo, '1'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPosPrinter, CChaveModelo, '1'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPosPrinter, CChavePorta, PChar(SaidaImpressao)));
  AssertEquals(ErrOK, SAT_ConfigGravar(''));
  AssertEquals(ErrOK, SAT_InicializarSAT);
  AssertEquals(ErrOK, SAT_ImprimirExtratoVenda('..\AD35180911111111111111591234567890001684429520.xml', ''));

  AssertEquals(ErrOK, SAT_Finalizar());
end;

procedure TTestACBrSATLib.Test_SAT_ImpressaoExtratoPDF_Sem_Path;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, SAT_Inicializar('',''));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveModelo, '1'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoExtrato, CChaveTipo, '0'));
  AssertEquals(ErrOK, SAT_ConfigGravar(''));
  AssertEquals(ErrOK, SAT_InicializarSAT);

   // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao tentar geara PDF do CFe', ErrOK,
                SAT_GerarPDFExtratoVenda('..\AD35180911111111111111591234567890001684429520.xml', '', PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, SAT_UltimoRetorno(PChar(AStr), Bufflen));
  end;

  AssertEquals(ErrOK, SAT_Finalizar());
end;

procedure TTestACBrSATLib.Test_SAT_ImpressaoExtratoPDF_Com_Path;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, SAT_Inicializar('',''));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoPrincipal, CChaveLogPath, PChar(ApplicationPath)));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveModelo, '1'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveNomeDLL, 'C:\SAT\SAT.dll'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoExtrato, CChaveTipo, '0'));
  AssertEquals(ErrOK, SAT_ConfigGravar(''));
  AssertEquals(ErrOK, SAT_InicializarSAT);

  if FileExists('AD35180911111111111111591234567890001684429520.pdf') then
    DeleteFile('AD35180911111111111111591234567890001684429520.pdf');

   // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);

  AssertEquals('Erro ao tentar geara PDF do CFe', ErrOK,
                SAT_GerarPDFExtratoVenda('..\AD35180911111111111111591234567890001684429520.xml', 'AD35180911111111111111591234567890001684429520.pdf', PChar(AStr), Bufflen));

  if Bufflen > 255 then
  begin
    AStr := Space(Bufflen);
    AssertEquals(ErrOK, SAT_UltimoRetorno(PChar(AStr), Bufflen));
  end;

  AssertEquals(ErrOK, SAT_Finalizar());
end;

initialization
  RegisterTest(TTestACBrSATLib);

end.

