unit ACBrLibBALTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrBALLib }

  TTestACBrBALLib = class(TTestCase)
  published
    procedure Test_BAL_Inicializar_Com_DiretorioInvalido;
    procedure Test_BAL_Inicializar;
    procedure Test_BAL_Inicializar_Ja_Inicializado;
    procedure Test_BAL_Finalizar;
    procedure Test_BAL_Finalizar_Ja_Finalizado;
    procedure Test_BAL_Nome_Obtendo_LenBuffer;
    procedure Test_BAL_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_BAL_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_BAL_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_BAL_Versao;
    procedure Test_BAL_ConfigLerValor;
    procedure Test_BAL_ConfigGravarValor;

    procedure Test_BAL_Ativar;
    procedure Test_BAL_SolicitarPeso;
    procedure Test_BAL_LePeso;
    procedure Test_BAL_InterpretarRespostaPeso;
    procedure Test_BAL_Desativar;
  end;

implementation

uses
  ACBrLibBALStaticImport, ACBrLibBALConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrBALLib.Test_BAL_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, BAL_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrBALLib.Test_BAL_Inicializar;
begin
  AssertEquals(ErrOk, BAL_Inicializar('',''));
end;

procedure TTestACBrBALLib.Test_BAL_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, BAL_Inicializar('',''));
end;

procedure TTestACBrBALLib.Test_BAL_Finalizar;
begin
  AssertEquals(ErrOk, BAL_Finalizar());
end;

procedure TTestACBrBALLib.Test_BAL_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, BAL_Finalizar());
end;

procedure TTestACBrBALLib.Test_BAL_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, BAL_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibBALNome), Bufflen);
end;

procedure TTestACBrBALLib.Test_BAL_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibBALNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BAL_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibBALNome), Bufflen);
  AssertEquals(CLibBALNome, AStr);
end;

procedure TTestACBrBALLib.Test_BAL_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibBALNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BAL_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibBALNome), Bufflen);
  AssertEquals(CLibBALNome, AStr);
end;

procedure TTestACBrBALLib.Test_BAL_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BAL_Nome(PChar(AStr), Bufflen));
  AssertEquals(4, Bufflen);
  AssertEquals(copy(CLibBALNome,1,4), AStr);
end;

procedure TTestACBrBALLib.Test_BAL_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, BAL_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibBALVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BAL_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibBALVersao), Bufflen);
  AssertEquals(CLibBALVersao, AStr);
end;

procedure TTestACBrBALLib.Test_BAL_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BAL_ConfigLerValor(CSessaoVersao, CLibBALNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibBALVersao, AStr);
end;

procedure TTestACBrBALLib.Test_BAL_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, BAL_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, BAL_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrBALLib.Test_BAL_Ativar;
begin
  // Ativando a Balança
  AssertEquals('Erro ao Ativar a Balança', ErrOk, BAL_Ativar);
end;

procedure TTestACBrBALLib.Test_BAL_Desativar;
begin
  // Desativando a Balança
  AssertEquals('Erro ao Desativar a Balança', ErrOk, BAL_Desativar);
end;

procedure TTestACBrBALLib.Test_BAL_LePeso;
var
  Peso: Double;
  sPeso: String;
begin
  // Iniciando a leitura do peso
  AssertEquals('Erro ao Iniciar a leitura do peso', ErrOk, BAL_LePeso(3000, Peso));
  sPeso := FloatToStr(Peso);
  AssertEquals('Peso= ', sPeso);
end;

procedure TTestACBrBALLib.Test_BAL_SolicitarPeso;
begin
  // Solicitar o peso
  AssertEquals('Erro ao solicitar o peso', ErrOk, BAL_SolicitarPeso);
end;

procedure TTestACBrBALLib.Test_BAL_InterpretarRespostaPeso;
var
  Peso: Double;
  sPeso: String;
begin
  // Interpretando a resposta
  AssertEquals('Erro ao interpretar a resposta', ErrOk,
               BAL_InterpretarRespostaPeso('125', Peso));
  sPeso := FloatToStr(Peso);
  AssertEquals('Peso= ', sPeso);
end;

initialization
  RegisterTest(TTestACBrBALLib);

end.

