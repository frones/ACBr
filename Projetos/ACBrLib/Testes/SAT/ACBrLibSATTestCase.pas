unit ACBrLibSATTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

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
    procedure Test_SAT_InicializarConfigGravarValoresEFinalizar;
    procedure Test_SAT_InicializarAtivarEFinalizar;
  end;

implementation

uses
  ACBrLibSATStaticImport, ACBrLibSATConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrSATLib.Test_SAT_Inicializar_Com_DiretorioInvalido;
begin
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
  AssertEquals(4, Bufflen);
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
  AssertEquals(Length(CLibSATVersao), Bufflen);

  // Lendo a resSATta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, SAT_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibSATVersao), Bufflen);
  AssertEquals(CLibSATVersao, AStr);
end;

procedure TTestACBrSATLib.Test_SAT_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, SAT_ConfigLerValor(CSessaoVersao, CLibSATNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibSATVersao, AStr);
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

procedure TTestACBrSATLib.Test_SAT_InicializarConfigGravarValoresEFinalizar;
begin
  AssertEquals(ErrOk, SAT_Inicializar('',''));

  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveModelo, '1'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChavePorta, PChar(ApplicationPath+'teste.txt')));
  AssertEquals(ErrOK, SAT_ConfigGravar(''));
  AssertEquals(ErrOK, SAT_ConfigLer(''));
  AssertEquals(ErrOK, SAT_Ativar);

  AssertEquals(ErrOK, SAT_Finalizar());
end;

procedure TTestACBrSATLib.Test_SAT_InicializarAtivarEFinalizar;
var
  Bufflen: Integer;
  AStr: String;
begin
  AssertEquals(ErrOk, SAT_Inicializar('',''));

  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChaveModelo, '1'));
  AssertEquals(ErrOK, SAT_ConfigGravarValor(CSessaoSAT, CChavePorta, ''));
  AssertEquals(ErrOK, SAT_ConfigGravar(''));
  AssertEquals(ErrOK, SAT_ConfigLer(''));
  AssertEquals(ErrExecutandoMetodo, SAT_Ativar);

  // Checando se é SATsivel pegar a descrição do erro //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrExecutandoMetodo, SAT_UltimoRetorno(PChar(AStr), bufflen));
  AssertEquals('Porta não definida', Trim(AStr));

  AssertEquals(ErrOK, SAT_Finalizar());
end;

initialization
  RegisterTest(TTestACBrSATLib);

end.

