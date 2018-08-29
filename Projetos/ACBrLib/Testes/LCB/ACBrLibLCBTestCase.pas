unit ACBrLibLCBTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrLCBLib }

  TTestACBrLCBLib = class(TTestCase)
  published
    procedure Test_LCB_Inicializar_Com_DiretorioInvalido;
    procedure Test_LCB_Inicializar;
    procedure Test_LCB_Inicializar_Ja_Inicializado;
    procedure Test_LCB_Finalizar;
    procedure Test_LCB_Finalizar_Ja_Finalizado;
    procedure Test_LCB_Nome_Obtendo_LenBuffer;
    procedure Test_LCB_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_LCB_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_LCB_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_LCB_Versao;
    procedure Test_LCB_ConfigLerValor;
    procedure Test_LCB_ConfigGravarValor;

    procedure Test_LCB_Ativar;
    procedure Test_LCB_LerFila;
    procedure Test_LCB_ApagarFila;
    procedure Test_LCB_EnviarString;
    procedure Test_LCB_LerString;
    procedure Test_LCB_Desativar;
  end;

implementation

uses
  ACBrLibLCBStaticImport, ACBrLibLCBConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrLCBLib.Test_LCB_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, LCB_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrLCBLib.Test_LCB_Inicializar;
begin
  AssertEquals(ErrOk, LCB_Inicializar('',''));
end;

procedure TTestACBrLCBLib.Test_LCB_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, LCB_Inicializar('',''));
end;

procedure TTestACBrLCBLib.Test_LCB_Finalizar;
begin
  AssertEquals(ErrOk, LCB_Finalizar());
end;

procedure TTestACBrLCBLib.Test_LCB_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, LCB_Finalizar());
end;

procedure TTestACBrLCBLib.Test_LCB_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, LCB_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibLCBNome), Bufflen);
end;

procedure TTestACBrLCBLib.Test_LCB_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibLCBNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, LCB_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibLCBNome), Bufflen);
  AssertEquals(CLibLCBNome, AStr);
end;

procedure TTestACBrLCBLib.Test_LCB_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibLCBNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, LCB_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibLCBNome), Bufflen);
  AssertEquals(CLibLCBNome, AStr);
end;

procedure TTestACBrLCBLib.Test_LCB_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, LCB_Nome(PChar(AStr), Bufflen));
  AssertEquals(4, Bufflen);
  AssertEquals(copy(CLibLCBNome,1,4), AStr);
end;

procedure TTestACBrLCBLib.Test_LCB_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, LCB_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibLCBVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, LCB_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibLCBVersao), Bufflen);
  AssertEquals(CLibLCBVersao, AStr);
end;

procedure TTestACBrLCBLib.Test_LCB_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, LCB_ConfigLerValor(CSessaoVersao, CLibLCBNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibLCBVersao, AStr);
end;

procedure TTestACBrLCBLib.Test_LCB_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, LCB_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, LCB_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrLCBLib.Test_LCB_Ativar;
begin
  // Ativando o Leitor de Código de Barras
  AssertEquals('Erro ao Ativar o Leitor de Código de Barras', ErrOk, LCB_Ativar);
end;

procedure TTestACBrLCBLib.Test_LCB_Desativar;
begin
  // Desativando o Leitor de Código de Barras
  AssertEquals('Erro ao Desativar o Leitor de Código de Barras', ErrOk, LCB_Desativar);
end;

procedure TTestACBrLCBLib.Test_LCB_ApagarFila;
begin
  // Apagando a Fila
  AssertEquals('Erro ao apagar a fila', ErrOk, LCB_ApagarFila);
end;

procedure TTestACBrLCBLib.Test_LCB_EnviarString;
begin
  // Enviando uma string
  AssertEquals('Erro ao enviar uma string', ErrOk, LCB_EnviarString('123'));
end;

procedure TTestACBrLCBLib.Test_LCB_LerFila;
var
  Fila: PChar;
begin
  // Lendo uma fila
  Fila := '';
  AssertEquals('Erro ao ler uma fila', ErrOk, LCB_LerFila(Fila));
  AssertEquals('Fila=', AnsiString(Fila));
end;

procedure TTestACBrLCBLib.Test_LCB_LerString;
var
  Str: PChar;
begin
  // Lendo uma string
  Str := '';
  AssertEquals('Erro ao ler uma string', ErrOk, LCB_LerString(Str));
  AssertEquals('String=', AnsiString(Str));
end;

initialization
  RegisterTest(TTestACBrLCBLib);

end.

