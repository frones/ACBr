unit ACBrLibNFeTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrNFeLib }

  TTestACBrNFeLib = class(TTestCase)
  published
    procedure Test_NFE_Inicializar_Com_DiretorioInvalido;
    procedure Test_NFE_Inicializar;
    procedure Test_NFE_Inicializar_Ja_Inicializado;
    procedure Test_NFE_Finalizar;
    procedure Test_NFE_Finalizar_Ja_Finalizado;
    procedure Test_NFE_NomeEVersao_Inicializacao_Automatica;
  end;

implementation

uses
  ACBrLibNFeStaticImport, ACBrLibNFeConsts;

procedure TTestACBrNFeLib.Test_NFE_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, NFE_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrNFeLib.Test_NFE_Inicializar;
begin
  AssertEquals(ErrOk, NFE_Inicializar('',''));
end;

procedure TTestACBrNFeLib.Test_NFE_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, NFE_Inicializar('',''));
end;

procedure TTestACBrNFeLib.Test_NFE_Finalizar;
begin
  AssertEquals(ErrOk, NFE_Finalizar());
end;

procedure TTestACBrNFeLib.Test_NFE_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, NFE_Finalizar());
end;

procedure TTestACBrNFeLib.Test_NFE_NomeEVersao_Inicializacao_Automatica;
var
  ANome, AVersao: String;
  L1, L2: Integer;
begin
  ANome := Space(255);
  AVersao := Space(255);

  AssertEquals(ErrOk, NFE_NomeEVersao(PChar(ANome), PChar(AVersao)));

  ANome := TrimRight(ANome);
  AssertEquals(CLibNFeNome, ANome);
  AVersao := TrimRight(AVersao);
  AssertEquals(CLibNFeVersao, AVersao);
end;

initialization
  RegisterTest(TTestACBrNFeLib);
end.

