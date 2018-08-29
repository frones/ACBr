unit ACBrLibGAVTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrGAVLib }

  TTestACBrGAVLib = class(TTestCase)
  published
    procedure Test_GAV_Inicializar_Com_DiretorioInvalido;
    procedure Test_GAV_Inicializar;
    procedure Test_GAV_Inicializar_Ja_Inicializado;
    procedure Test_GAV_Finalizar;
    procedure Test_GAV_Finalizar_Ja_Finalizado;
    procedure Test_GAV_Nome_Obtendo_LenBuffer;
    procedure Test_GAV_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_GAV_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_GAV_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_GAV_Versao;
    procedure Test_GAV_ConfigLerValor;
    procedure Test_GAV_ConfigGravarValor;

    procedure Test_GAV_Ativar;
    procedure Test_GAV_AbreGaveta;
    procedure Test_GAV_Desativar;
  end;

implementation

uses
  ACBrLibGAVStaticImport, ACBrLibGAVConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrGAVLib.Test_GAV_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, GAV_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrGAVLib.Test_GAV_Inicializar;
begin
  AssertEquals(ErrOk, GAV_Inicializar('',''));
end;

procedure TTestACBrGAVLib.Test_GAV_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, GAV_Inicializar('',''));
end;

procedure TTestACBrGAVLib.Test_GAV_Finalizar;
begin
  AssertEquals(ErrOk, GAV_Finalizar());
end;

procedure TTestACBrGAVLib.Test_GAV_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, GAV_Finalizar());
end;

procedure TTestACBrGAVLib.Test_GAV_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, GAV_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibGAVNome), Bufflen);
end;

procedure TTestACBrGAVLib.Test_GAV_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibGAVNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GAV_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibGAVNome), Bufflen);
  AssertEquals(CLibGAVNome, AStr);
end;

procedure TTestACBrGAVLib.Test_GAV_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibGAVNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GAV_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibGAVNome), Bufflen);
  AssertEquals(CLibGAVNome, AStr);
end;

procedure TTestACBrGAVLib.Test_GAV_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GAV_Nome(PChar(AStr), Bufflen));
  AssertEquals(4, Bufflen);
  AssertEquals(copy(CLibGAVNome,1,4), AStr);
end;

procedure TTestACBrGAVLib.Test_GAV_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, GAV_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibGAVVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GAV_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibGAVVersao), Bufflen);
  AssertEquals(CLibGAVVersao, AStr);
end;

procedure TTestACBrGAVLib.Test_GAV_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GAV_ConfigLerValor(CSessaoVersao, CLibGAVNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibGAVVersao, AStr);
end;

procedure TTestACBrGAVLib.Test_GAV_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, GAV_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GAV_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrGAVLib.Test_GAV_Ativar;
begin
  // Ativando a Gaveta
  AssertEquals('Erro ao Ativar a Gaveta', ErrOk, GAV_Ativar);
end;

procedure TTestACBrGAVLib.Test_GAV_Desativar;
begin
  // Desativando a Gaveta
  AssertEquals('Erro ao Desativar a Gaveta', ErrOk, GAV_Desativar);
end;

procedure TTestACBrGAVLib.Test_GAV_AbreGaveta;
begin
  // Abrindo a Gaveta
  AssertEquals('Erro ao abrir a Gaveta', ErrOk, GAV_AbreGaveta);
end;

initialization
  RegisterTest(TTestACBrGAVLib);

end.

