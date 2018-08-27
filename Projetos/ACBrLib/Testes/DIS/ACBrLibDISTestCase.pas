unit ACBrLibDISTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrDISLib }

  TTestACBrDISLib = class(TTestCase)
  published
    procedure Test_DIS_Inicializar_Com_DiretorioInvalido;
    procedure Test_DIS_Inicializar;
    procedure Test_DIS_Inicializar_Ja_Inicializado;
    procedure Test_DIS_Finalizar;
    procedure Test_DIS_Finalizar_Ja_Finalizado;
    procedure Test_DIS_Nome_Obtendo_LenBuffer;
    procedure Test_DIS_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_DIS_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_DIS_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_DIS_Versao;
    procedure Test_DIS_ConfigLerValor;
    procedure Test_DIS_ConfigGravarValor;

    procedure Test_DIS_Ativar;
    procedure Test_DIS_LimparDisplay;

    procedure Test_DIS_LimparLinha;
    procedure Test_DIS_PosicionarCursor;
    procedure Test_DIS_Escrever;
    procedure Test_DIS_ExibirLinha;
    procedure Test_DIS_RolarLinha;
    procedure Test_DIS_Parar;
    procedure Test_DIS_Continuar;

    procedure Test_DIS_PararLinha;
    procedure Test_DIS_ContinuarLinha;

    procedure Test_DIS_Desativar;
  end;

implementation

uses
  ACBrLibDISStaticImport, ACBrLibDISConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrDISLib.Test_DIS_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, DIS_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrDISLib.Test_DIS_Inicializar;
begin
  AssertEquals(ErrOk, DIS_Inicializar('',''));
end;

procedure TTestACBrDISLib.Test_DIS_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, DIS_Inicializar('',''));
end;

procedure TTestACBrDISLib.Test_DIS_Finalizar;
begin
  AssertEquals(ErrOk, DIS_Finalizar());
end;

procedure TTestACBrDISLib.Test_DIS_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, DIS_Finalizar());
end;

procedure TTestACBrDISLib.Test_DIS_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, DIS_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibDISNome), Bufflen);
end;

procedure TTestACBrDISLib.Test_DIS_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibDISNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, DIS_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibDISNome), Bufflen);
  AssertEquals(CLibDISNome, AStr);
end;

procedure TTestACBrDISLib.Test_DIS_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibDISNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, DIS_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibDISNome), Bufflen);
  AssertEquals(CLibDISNome, AStr);
end;

procedure TTestACBrDISLib.Test_DIS_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, DIS_Nome(PChar(AStr), Bufflen));
  AssertEquals(4, Bufflen);
  AssertEquals(copy(CLibDISNome,1,4), AStr);
end;

procedure TTestACBrDISLib.Test_DIS_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, DIS_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibDISVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, DIS_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibDISVersao), Bufflen);
  AssertEquals(CLibDISVersao, AStr);
end;

procedure TTestACBrDISLib.Test_DIS_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, DIS_ConfigLerValor(CSessaoVersao, CLibDISNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibDISVersao, AStr);
end;

procedure TTestACBrDISLib.Test_DIS_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, DIS_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, DIS_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrDISLib.Test_DIS_Ativar;
begin
  // Ativando o Display
  AssertEquals('Erro ao Ativar o Display', ErrOk, DIS_Ativar);
end;

procedure TTestACBrDISLib.Test_DIS_Desativar;
begin
  // Desativando o Display
  AssertEquals('Erro ao Desativar o Display', ErrOk, DIS_Desativar);
end;

procedure TTestACBrDISLib.Test_DIS_LimparDisplay;
begin
  // Iniciando a limpeza do Display
  AssertEquals('Erro ao limpar o Display', ErrOk, DIS_LImparDisplay);
end;

procedure TTestACBrDISLib.Test_DIS_LimparLinha;
begin
  // Iniciando a Limpeza da Linha
  AssertEquals('Erro ao limpar a linha', ErrOk, DIS_LimparLinha(1));
end;

procedure TTestACBrDISLib.Test_DIS_PosicionarCursor;
begin
  // Definindo a posição do cursor
  AssertEquals('Erro ao posicionar o cursor', ErrOk, DIS_PosicionarCursor(1,1));
end;

procedure TTestACBrDISLib.Test_DIS_Escrever;
begin
  // Escrevendo no Display
  AssertEquals('Erro ao escrever no display', ErrOk, DIS_Escrever('Teste'));
end;

procedure TTestACBrDISLib.Test_DIS_ExibirLinha;
begin
  // Exibindo a linha
  AssertEquals('Erro ao exibir a linha', ErrOk, DIS_ExibirLinha(1, 'Teste', -1, -1));
end;

procedure TTestACBrDISLib.Test_DIS_RolarLinha;
begin
  // Rolando a Linha
  AssertEquals('Erro ao rolar a linha', ErrOk, DIS_RolarLinha(1, 2));
end;

procedure TTestACBrDISLib.Test_DIS_Parar;
begin
  // Parando o Display
  AssertEquals('Erro ao parar o Display', ErrOk, DIS_Parar);
end;

procedure TTestACBrDISLib.Test_DIS_Continuar;
begin
  // Continuar o Display
  AssertEquals('Erro ao Continuar o Display', ErrOk, DIS_Continuar);
end;

procedure TTestACBrDISLib.Test_DIS_PararLinha;
begin
  // Parando uma linha
  AssertEquals('Erro ao parar uma linha', ErrOk, DIS_PararLinha(1));
end;

procedure TTestACBrDISLib.Test_DIS_ContinuarLinha;
begin
  // Continuando uma linha
  AssertEquals('Erro ao continuar uma linha', ErrOk, DIS_ContinuarLinha(1));
end;

initialization
  RegisterTest(TTestACBrDISLib);

end.

