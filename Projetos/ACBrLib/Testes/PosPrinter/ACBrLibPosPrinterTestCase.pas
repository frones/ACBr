unit ACBrLibPosPrinterTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrPosPrinterLib }

  TTestACBrPosPrinterLib = class(TTestCase)
  published
    procedure Test_POS_Inicializar_Com_DiretorioValido;
    procedure Test_POS_Inicializar_Com_DiretorioInvalido;
    procedure Test_POS_Inicializar;
    procedure Test_POS_Inicializar_Ja_Inicializado;
    procedure Test_POS_Finalizar;
    procedure Test_POS_Finalizar_Ja_Finalizado;
    procedure Test_POS_Nome_Obtendo_LenBuffer;
    procedure Test_POS_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_POS_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_POS_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_POS_Versao;
    procedure Test_POS_ConfigLerValor;
    procedure Test_POS_ConfigGravarValor;
    procedure Test_POS_InicializarConfigGravarValoresEFinalizar;
    procedure Test_POS_InicializarAtivarEFinalizar;
    procedure Test_POS_ImpressaoDeTags;
    procedure Test_POS_RetornarEInterpretarTags;
  end;

implementation

uses
  ACBrLibPosPrinterStaticImport, ACBrLibPosPrinterConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrPosPrinterLib.Test_POS_Inicializar_Com_DiretorioValido;
var
  fileName: String;
begin
  fileName := PathWithDelim(ApplicationPath)+'ACBrLib.ini';
  if FileExists(fileName) then
    DeleteFile(fileName);

  try
    AssertEquals(ErrOK, POS_Inicializar( PChar(fileName),''));
    AssertTrue(FileExists(fileName));
    AssertEquals(ErrOK, POS_Finalizar);
  finally
    DeleteFile(fileName);
  end;
end;

procedure TTestACBrPosPrinterLib.Test_POS_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, POS_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrPosPrinterLib.Test_POS_Inicializar;
begin
  AssertEquals(ErrOk, POS_Inicializar('',''));
end;

procedure TTestACBrPosPrinterLib.Test_POS_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, POS_Inicializar('',''));
end;

procedure TTestACBrPosPrinterLib.Test_POS_Finalizar;
begin
  AssertEquals(ErrOk, POS_Finalizar());
end;

procedure TTestACBrPosPrinterLib.Test_POS_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, POS_Finalizar());
end;

procedure TTestACBrPosPrinterLib.Test_POS_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, POS_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibPosPrinterNome), Bufflen);
end;

procedure TTestACBrPosPrinterLib.Test_POS_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibPosPrinterNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, POS_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibPosPrinterNome), Bufflen);
  AssertEquals(CLibPosPrinterNome, AStr);
end;

procedure TTestACBrPosPrinterLib.Test_POS_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibPosPrinterNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, POS_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibPosPrinterNome), Bufflen);
  AssertEquals(CLibPosPrinterNome, AStr);
end;

procedure TTestACBrPosPrinterLib.Test_POS_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, POS_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibPosPrinterNome), Bufflen);
  AssertEquals(copy(CLibPosPrinterNome,1,4), AStr);
end;

procedure TTestACBrPosPrinterLib.Test_POS_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, POS_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibPosPrinterVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, POS_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibPosPrinterVersao), Bufflen);
  AssertEquals(CLibPosPrinterVersao, AStr);
end;

procedure TTestACBrPosPrinterLib.Test_POS_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, POS_ConfigLerValor(CSessaoVersao, CLibPosPrinterNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibPosPrinterVersao, AStr);
end;

procedure TTestACBrPosPrinterLib.Test_POS_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, POS_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, POS_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrPosPrinterLib.Test_POS_InicializarConfigGravarValoresEFinalizar;
begin
  AssertEquals(ErrOk, POS_Inicializar('',''));

  AssertEquals(ErrOK, POS_ConfigGravarValor(CSessaoPosPrinter, CChaveModelo, '1'));
  AssertEquals(ErrOK, POS_ConfigGravarValor(CSessaoPosPrinter, CChavePorta, PChar(ApplicationPath+'posprinter.txt')));
  AssertEquals(ErrOK, POS_ConfigGravar(''));
  AssertEquals(ErrOK, POS_ConfigLer(''));
  AssertEquals(ErrOK, POS_Ativar);

  AssertEquals(ErrOK, POS_Finalizar());
end;

procedure TTestACBrPosPrinterLib.Test_POS_InicializarAtivarEFinalizar;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, POS_Inicializar('',''));

  AssertEquals(ErrOK, POS_ConfigGravarValor(CSessaoPosPrinter, CChaveModelo, '1'));
  AssertEquals(ErrOK, POS_ConfigGravarValor(CSessaoPosPrinter, CChavePorta, ''));
  AssertEquals(ErrOK, POS_ConfigGravar(''));
  AssertEquals(ErrOK, POS_ConfigLer(''));
  AssertEquals(ErrExecutandoMetodo, POS_Ativar);

  // Checando se é possivel pegar a descrição do erro //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrExecutandoMetodo, POS_UltimoRetorno(PChar(AStr), bufflen));
  AssertEquals('Porta não definida', Trim(AStr));

  AssertEquals(ErrOK, POS_Finalizar());
end;

procedure TTestACBrPosPrinterLib.Test_POS_ImpressaoDeTags;
var
  SaidaImpressao: String;
begin
  SaidaImpressao := ApplicationPath+'posprinter.txt';
  AssertEquals(ErrOk, POS_Inicializar('',''));
  AssertEquals(ErrOK, POS_ConfigGravarValor(CSessaoPosPrinter, CChaveModelo, '1'));
  AssertEquals(ErrOK, POS_ConfigGravarValor(CSessaoPosPrinter, CChavePorta, PChar(SaidaImpressao) ));
  AssertEquals(ErrOK, POS_ConfigGravar(''));
  AssertEquals(ErrOK, POS_Ativar);
  AssertEquals(ErrOK, POS_ImprimirTags);
  AssertEquals(ErrOK, POS_Desativar);
  AssertEquals(ErrOK, POS_Finalizar());
end;

procedure TTestACBrPosPrinterLib.Test_POS_RetornarEInterpretarTags;
var
  Bufflen: Integer;
  AStr: String;
  SaidaImpressao: String;
begin
  Bufflen := 0;
  AStr := '';
  SaidaImpressao := ApplicationPath+'posprinter.txt';
  AssertEquals(ErrOk, POS_Inicializar('',''));
  AssertEquals(ErrOK, POS_ConfigGravarValor(CSessaoPosPrinter, CChaveModelo, '1'));
  AssertEquals(ErrOK, POS_ConfigGravarValor(CSessaoPosPrinter, CChavePorta, PChar(SaidaImpressao) ));
  AssertEquals(ErrOK, POS_ConfigGravar(''));
  AssertEquals(ErrOK, POS_Ativar);
  AssertEquals(ErrOK, POS_RetornarTags(PChar(AStr), Bufflen, False));
  AStr := Space(Bufflen);
  AssertEquals(ErrOK, POS_RetornarTags(PChar(AStr), Bufflen, False));
  AssertEquals(copy(AStr,1,133), '<e>|</e>|<a>|</a>|<n>|</n>|<s>|</s>|<c>|</c>|<i>|</i>|</fn>|</fa>|</fb>|<in>|</in>|</ae>|</ce>|</ad>|</linha_simples>|</linha_dupla>|');
  AssertEquals(ErrOK, POS_Imprimir(PChar(AStr), True, True, True, 1));
  AssertEquals(ErrOK, POS_Desativar);
  AssertEquals(ErrOK, POS_Finalizar());
end;

initialization
  RegisterTest(TTestACBrPosPrinterLib);

end.

