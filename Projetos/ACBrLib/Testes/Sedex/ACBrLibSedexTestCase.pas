unit ACBrLibSedexTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrSedexLib }

  TTestACBrSedexLib = class(TTestCase)
  published
    procedure Test_Sedex_Inicializar_Com_DiretorioInvalido;
    procedure Test_Sedex_Inicializar;
    procedure Test_Sedex_Inicializar_Ja_Inicializado;
    procedure Test_Sedex_Finalizar;
    procedure Test_Sedex_Finalizar_Ja_Finalizado;
    procedure Test_Sedex_Nome_Obtendo_LenBuffer;
    procedure Test_Sedex_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_Sedex_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_Sedex_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_Sedex_Versao;
    procedure Test_Sedex_ConfigLerValor;
    procedure Test_Sedex_ConfigGravarValor;

    procedure Test_Sedex_LerArqIni;
    procedure Test_Sedex_Consultar;
    procedure Test_Sedex_Rastrear;
  end;

implementation

uses
  ACBrLibSedexStaticImport, ACBrLibSedexConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrSedexLib.Test_Sedex_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, Sedex_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrSedexLib.Test_Sedex_Inicializar;
begin
  AssertEquals(ErrOk, Sedex_Inicializar('',''));
end;

procedure TTestACBrSedexLib.Test_Sedex_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, Sedex_Inicializar('',''));
end;

procedure TTestACBrSedexLib.Test_Sedex_Finalizar;
begin
  AssertEquals(ErrOk, Sedex_Finalizar());
end;

procedure TTestACBrSedexLib.Test_Sedex_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, Sedex_Finalizar());
end;

procedure TTestACBrSedexLib.Test_Sedex_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, Sedex_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibSedexNome), Bufflen);
end;

procedure TTestACBrSedexLib.Test_Sedex_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibSedexNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Sedex_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibSedexNome), Bufflen);
  AssertEquals(CLibSedexNome, AStr);
end;

procedure TTestACBrSedexLib.Test_Sedex_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibSedexNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Sedex_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibSedexNome), Bufflen);
  AssertEquals(CLibSedexNome, AStr);
end;

procedure TTestACBrSedexLib.Test_Sedex_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Sedex_Nome(PChar(AStr), Bufflen));
  AssertEquals(4, Bufflen);
  AssertEquals(copy(CLibSedexNome,1,4), AStr);
end;

procedure TTestACBrSedexLib.Test_Sedex_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, Sedex_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibSedexVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Sedex_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibSedexVersao), Bufflen);
  AssertEquals(CLibSedexVersao, AStr);
end;

procedure TTestACBrSedexLib.Test_Sedex_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Sedex_ConfigLerValor(CSessaoVersao, CLibSedexNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibSedexVersao, AStr);
end;

procedure TTestACBrSedexLib.Test_Sedex_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, Sedex_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, Sedex_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrSedexLib.Test_Sedex_LerArqIni;
begin
  // Lendo o arquivo INI
  AssertEquals('Erro ao ler o arquivo INI', ErrOk, Sedex_LerArqIni('C:\NAOEXISTE\Arquivo.ini'));
end;

procedure TTestACBrSedexLib.Test_Sedex_Consultar;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a consulta
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao consultar', ErrOk, Sedex_Consultar(Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

procedure TTestACBrSedexLib.Test_Sedex_Rastrear;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando o Rastreio
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Rastrear', ErrOk, Sedex_Rastrear('PP800891131BR', Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

initialization
  RegisterTest(TTestACBrSedexLib);

end.

