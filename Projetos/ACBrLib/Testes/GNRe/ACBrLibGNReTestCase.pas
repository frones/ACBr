unit ACBrLibGNReTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrGNReLib }

  TTestACBrGNReLib = class(TTestCase)
  published
    procedure Test_GNRe_Inicializar_Com_DiretorioInvalido;
    procedure Test_GNRe_Inicializar;
    procedure Test_GNRe_Inicializar_Ja_Inicializado;
    procedure Test_GNRe_Finalizar;
    procedure Test_GNRe_Finalizar_Ja_Finalizado;
    procedure Test_GNRe_Nome_Obtendo_LenBuffer;
    procedure Test_GNRe_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_GNRe_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_GNRe_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_GNRe_Versao;
    procedure Test_GNRe_ConfigLerValor;
    procedure Test_GNRe_ConfigGravarValor;

    procedure Test_GNRe_LimparLista;
    procedure Test_GNRe_CarregarINI;

    procedure Test_GNRe_Enviar;

//    procedure Test_GNRe_LimparListaGuiaRetorno;
//    procedure Test_GNRe_CarregarGuiaRetorno;

//    procedure Test_GNRe_Imprimir;
//    procedure Test_GNRe_ImprimirPDF;

//    procedure Test_GNRe_Consultar;
//    procedure Test_GNRe_EnviarEmail;
  end;

implementation

uses
  ACBrLibGNReStaticImport, ACBrLibGNReConsts, ACBrLibConsts, ACBrLibComum;

procedure TTestACBrGNReLib.Test_GNRe_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, GNRe_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrGNReLib.Test_GNRe_Inicializar;
begin
  AssertEquals(ErrOk, GNRe_Inicializar('',''));
end;

procedure TTestACBrGNReLib.Test_GNRe_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, GNRe_Inicializar('',''));
end;

procedure TTestACBrGNReLib.Test_GNRe_Finalizar;
begin
  AssertEquals(ErrOk, GNRe_Finalizar());
end;

procedure TTestACBrGNReLib.Test_GNRe_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, GNRe_Finalizar());
end;

procedure TTestACBrGNReLib.Test_GNRe_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, GNRe_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibGNReNome), Bufflen);
end;

procedure TTestACBrGNReLib.Test_GNRe_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibGNReNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GNRe_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibGNReNome), Bufflen);
  AssertEquals(CLibGNReNome, AStr);
end;

procedure TTestACBrGNReLib.Test_GNRe_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibGNReNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GNRe_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibGNReNome), Bufflen);
  AssertEquals(CLibGNReNome, AStr);
end;

procedure TTestACBrGNReLib.Test_GNRe_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GNRe_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibGNReNome), Bufflen);
  AssertEquals(copy(CLibGNReNome,1,4), AStr);
end;

procedure TTestACBrGNReLib.Test_GNRe_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, GNRe_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibGNReVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GNRe_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibGNReVersao), Bufflen);
  AssertEquals(CLibGNReVersao, AStr);
end;

procedure TTestACBrGNReLib.Test_GNRe_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GNRe_ConfigLerValor(CSessaoVersao, CLibGNReNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibGNReVersao, AStr);
end;

procedure TTestACBrGNReLib.Test_GNRe_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, GNRe_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, GNRe_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
end;

procedure TTestACBrGNReLib.Test_GNRe_LimparLista;
begin
  // Iniciando a Limpeza da Lista de GNRe
  AssertEquals('Erro ao limpar a lista de GNRe', ErrOk, GNRe_LimparLista);
end;

procedure TTestACBrGNReLib.Test_GNRe_CarregarINI;
var
  Path, NomeINI: String;
begin
  Test_GNRe_LimparLista;

  Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\GNRe\bin\';
  NomeINI := 'GNRe.ini';
  Path := Path + NomeINI;

  // Iniciando o Carregamento do INI do GNRe
  AssertEquals('Erro ao carregar o INI do GNRe', ErrOk, GNRe_CarregarINI(Pchar(Path)));
end;

procedure TTestACBrGNReLib.Test_GNRe_Enviar;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando o Envio do Lote de Guias
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Enviar Lote de Guias', ErrOk, GNRe_Enviar(Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;
(*
procedure TTestACBrGNReLib.Test_GNRe_LimparListaGuiaRetorno;
begin
  // Iniciando a Limpeza da Lista de Guia Retorno
  AssertEquals('Erro ao limpar a lista de Guia Retorno', ErrOk, GNRe_LimparListaGuiaRetorno);
end;

procedure TTestACBrGNReLib.Test_GNRe_CarregarGuiaRetorno;
var
  Path, NomeTXT: String;
begin
  Test_GNRe_LimparListaGuiaRetorno;

  Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\GNRe\bin\';
  NomeTXT := '858000000011773803021828990100000021739916043002-gnre.txt';
  Path := Path + NomeTXT;

  // Iniciando o Carregamento do TXT da Guia Retorno
  AssertEquals('Erro ao carregar o TXT da Guia Retorno', ErrOk,
    GNRe_CarregarGuiaRetorno(PChar(Path)));
end;

procedure TTestACBrGNReLib.Test_GNRe_Imprimir;
begin
  // Iniciando a Impressão do Guia
  AssertEquals('Erro ao Imprimir o Guia', ErrOk, GNRe_Imprimir);
end;

procedure TTestACBrGNReLib.Test_GNRe_ImprimirPDF;
begin
  // Iniciando a geração do PDF do Guia
  AssertEquals('Erro ao gerar o PDF do Guia', ErrOk, GNRe_ImprimirPDF);
end;

procedure TTestACBrGNReLib.Test_GNRe_Consultar;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a Consulta
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Consultar a UF', ErrOk, GNRe_Consultar('PE', 1500, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

procedure TTestACBrGNReLib.Test_GNRe_EnviarEmail;
var
  Path, ArqGNRe: String;
begin
  // Iniciando o envio do e-mail
  Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\GNRe\bin\';
  ArqGNRe := Path + '28140417957142000144580170000000031895518397-GNRe.xml';

  AssertEquals('Erro ao enviar o e-mail', ErrOk,
    GNRe_EnviarEmail('italo.jurisato@gmail.com', PChar(ArqGNRe), True,
      'Teste de envio', '', '', 'Em anexo o GNRe') );
end;
*)
initialization
  RegisterTest(TTestACBrGNReLib);

end.

