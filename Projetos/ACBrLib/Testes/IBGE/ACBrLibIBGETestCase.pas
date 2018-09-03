unit ACBrLibIBGETestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrIBGELib }

  TTestACBrIBGELib = class(TTestCase)
  published
    procedure Test_IBGE_Inicializar_Com_DiretorioInvalido;
    procedure Test_IBGE_Inicializar;
    procedure Test_IBGE_Inicializar_Ja_Inicializado;
    procedure Test_IBGE_Finalizar;
    procedure Test_IBGE_Finalizar_Ja_Finalizado;
    procedure Test_IBGE_Nome_Obtendo_LenBuffer;
    procedure Test_IBGE_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_IBGE_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_IBGE_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_IBGE_Versao;
    procedure Test_IBGE_ConfigLerValor;
    procedure Test_IBGE_ConfigGravarValor;

    procedure Test_IBGE_BuscarPorCodigo;
    procedure Test_IBGE_BuscarPorNome;
  end;

implementation

uses
  ACBrLibIBGEStaticImport, ACBrLibIBGEConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrIBGELib.Test_IBGE_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, IBGE_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrIBGELib.Test_IBGE_Inicializar;
begin
  AssertEquals(ErrOk, IBGE_Inicializar('',''));
end;

procedure TTestACBrIBGELib.Test_IBGE_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, IBGE_Inicializar('',''));
end;

procedure TTestACBrIBGELib.Test_IBGE_Finalizar;
begin
  AssertEquals(ErrOk, IBGE_Finalizar());
end;

procedure TTestACBrIBGELib.Test_IBGE_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, IBGE_Finalizar());
end;

procedure TTestACBrIBGELib.Test_IBGE_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, IBGE_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibIBGENome), Bufflen);
end;

procedure TTestACBrIBGELib.Test_IBGE_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibIBGENome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibIBGENome), Bufflen);
  AssertEquals(CLibIBGENome, AStr);
end;

procedure TTestACBrIBGELib.Test_IBGE_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibIBGENome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibIBGENome), Bufflen);
  AssertEquals(CLibIBGENome, AStr);
end;

procedure TTestACBrIBGELib.Test_IBGE_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_Nome(PChar(AStr), Bufflen));
  AssertEquals(4, Bufflen);
  AssertEquals(copy(CLibIBGENome,1,4), AStr);
end;

procedure TTestACBrIBGELib.Test_IBGE_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, IBGE_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibIBGEVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibIBGEVersao), Bufflen);
  AssertEquals(CLibIBGEVersao, AStr);
end;

procedure TTestACBrIBGELib.Test_IBGE_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_ConfigLerValor(CSessaoVersao, CLibIBGENome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibIBGEVersao, AStr);
end;

procedure TTestACBrIBGELib.Test_IBGE_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, IBGE_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, IBGE_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrIBGELib.Test_IBGE_BuscarPorCodigo;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a busca por código
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Buscar Por Código', ErrOk,
                              IBGE_BuscarPorCodigo(3554003, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

procedure TTestACBrIBGELib.Test_IBGE_BuscarPorNome;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a busca por nome
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Buscar Por Nome', ErrOk,
                     IBGE_BuscarPorNome('Araraquara', 'SP', False, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

initialization
  RegisterTest(TTestACBrIBGELib);

end.

