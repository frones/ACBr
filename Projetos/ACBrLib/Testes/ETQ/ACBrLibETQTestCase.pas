unit ACBrLibETQTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

const
  CLibETQNome = 'ACBrLibETQ';

type

  { TTestACBrETQLib }

  TTestACBrETQLib = class(TTestCase)
  published
    procedure Test_ETQ_Inicializar_Com_DiretorioInvalido;
    procedure Test_ETQ_Inicializar;
    procedure Test_ETQ_Inicializar_Ja_Inicializado;
    procedure Test_ETQ_Finalizar;
    procedure Test_ETQ_Finalizar_Ja_Finalizado;
    procedure Test_ETQ_Nome_Obtendo_LenBuffer;
    procedure Test_ETQ_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_ETQ_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_ETQ_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_ETQ_Versao;
    procedure Test_ETQ_ConfigLerValor;
    procedure Test_ETQ_ConfigGravarValor;

    procedure Test_ETQ_Ativar;
    procedure Test_ETQ_IniciarEtiqueta;
    procedure Test_ETQ_CarregarImagem;

    procedure Test_ETQ_ImprimirTexto;
    procedure Test_ETQ_ImprimirBarras;
    procedure Test_ETQ_ImprimirLinha;
    procedure Test_ETQ_ImprimirCaixa;
    procedure Test_ETQ_ImprimirImagem;

    procedure Test_ETQ_FinalizarEtiqueta;

    procedure Test_ETQ_Imprimir;

    procedure Test_ETQ_Desativar;
  end;

implementation

uses
  ACBrLibETQStaticImport, ACBrLibETQConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBrETQLib.Test_ETQ_Inicializar_Com_DiretorioInvalido;
begin
  ETQ_Finalizar();
  AssertEquals(ErrDiretorioNaoExiste, ETQ_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrETQLib.Test_ETQ_Inicializar;
begin
  AssertEquals(ErrOk, ETQ_Inicializar('',''));
end;

procedure TTestACBrETQLib.Test_ETQ_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, ETQ_Inicializar('',''));
end;

procedure TTestACBrETQLib.Test_ETQ_Finalizar;
begin
  AssertEquals(ErrOk, ETQ_Finalizar());
end;

procedure TTestACBrETQLib.Test_ETQ_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, ETQ_Finalizar());
end;

procedure TTestACBrETQLib.Test_ETQ_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, ETQ_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibETQNome), Bufflen);
end;

procedure TTestACBrETQLib.Test_ETQ_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibETQNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, ETQ_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibETQNome), Bufflen);
  AssertEquals(CLibETQNome, AStr);
end;

procedure TTestACBrETQLib.Test_ETQ_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibETQNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, ETQ_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibETQNome), Bufflen);
  AssertEquals(CLibETQNome, AStr);
end;

procedure TTestACBrETQLib.Test_ETQ_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, ETQ_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibETQNome), Bufflen);
  AssertEquals(copy(CLibETQNome,1,4), AStr);
end;

procedure TTestACBrETQLib.Test_ETQ_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, ETQ_Versao(Nil, Bufflen));
  Assert(Bufflen > 0);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, ETQ_Versao(PChar(AStr), Bufflen));
  Assert(Bufflen > 0);
  Assert(AStr <> '');
end;

procedure TTestACBrETQLib.Test_ETQ_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, ETQ_ConfigLerValor(CSessaoVersao, CACBrLib, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CACBrLibVersaoConfig, AStr);
end;

procedure TTestACBrETQLib.Test_ETQ_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, ETQ_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, ETQ_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrETQLib.Test_ETQ_Ativar;
begin
  // Ativando a Etiqueta
  AssertEquals('Erro ao Ativar a etiqueta', ErrOk, ETQ_Ativar);
end;

procedure TTestACBrETQLib.Test_ETQ_Desativar;
begin
  // Desativando a Etiqueta
  AssertEquals('Erro ao Desativar a etiqueta', ErrOk, ETQ_Desativar);
end;

procedure TTestACBrETQLib.Test_ETQ_IniciarEtiqueta;
begin
  // Iniciando uma nova etiqueta
  AssertEquals('Erro ao Iniciar uma nova etiqueta', ErrOk, ETQ_IniciarEtiqueta);
end;

procedure TTestACBrETQLib.Test_ETQ_FinalizarEtiqueta;
begin
  // Finalizando uma etiqueta
  AssertEquals('Erro ao Finalizar uma etiqueta', ErrOk, ETQ_FinalizarEtiqueta(1, 1));
end;

procedure TTestACBrETQLib.Test_ETQ_CarregarImagem;
begin
  // Carregar uma imagem
  AssertEquals('Erro ao carregar uma imagem', ErrOk,
               ETQ_CarregarImagem('..'+PathDelim+'LOGOACBR.bmp', 'logo da empresa', False));
end;

procedure TTestACBrETQLib.Test_ETQ_Imprimir;
begin
  // Imprimindo uma etiqueta
  AssertEquals('Erro ao imprimir uma etiqueta', ErrOk, ETQ_Imprimir(1, 1));
end;

procedure TTestACBrETQLib.Test_ETQ_ImprimirTexto;
begin
  // Imprimindo um texto
  AssertEquals('Erro ao imprimir um texto', ErrOk,
            ETQ_ImprimirTexto(1, 1, 1, 1, 1, 1, 'Texto na etiqueta', 1, False));
end;

procedure TTestACBrETQLib.Test_ETQ_ImprimirBarras;
begin
  // Imprimindo Barras na etiqueta
  AssertEquals('Erro ao imprimir barras na etiqueta', ErrOk,
   ETQ_ImprimirBarras(1, 1, 2, 1, 1, 1, '12345678', 10, 1));
end;

procedure TTestACBrETQLib.Test_ETQ_ImprimirLinha;
begin
  // Imprimindo uma Linha na etiqueta
  AssertEquals('Erro ao imprimir uma linha', ErrOk, ETQ_ImprimirLinha(1, 1, 20, 10));
end;

procedure TTestACBrETQLib.Test_ETQ_ImprimirCaixa;
begin
  // Imprimindo uma caixa
  AssertEquals('Erro ao imprimir uma caixa', ErrOk, ETQ_ImprimirCaixa(1, 1, 20, 10, 2, 2));
end;

procedure TTestACBrETQLib.Test_ETQ_ImprimirImagem;
begin
  // Imprimindo uma imagem
  AssertEquals('Erro ao imprimir uma imagem', ErrOk, ETQ_ImprimirImagem(1, 1, 1, 'C:\Erp\logo.jpg'));
end;

initialization
  RegisterTest(TTestACBrETQLib);

end.

