{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit ACBrLibeSocialTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Dialogs;

type

  { TTestACBreSocialLib }

  TTestACBreSocialLib = class(TTestCase)
  published
    procedure Test_eSocial_Inicializar_Com_DiretorioInvalido;
    procedure Test_eSocial_Inicializar;
    procedure Test_eSocial_Inicializar_Ja_Inicializado;
    procedure Test_eSocial_Finalizar;
    procedure Test_eSocial_Finalizar_Ja_Finalizado;
    procedure Test_eSocial_Nome_Obtendo_LenBuffer;
    procedure Test_eSocial_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_eSocial_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_eSocial_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_eSocial_Versao;
    procedure Test_eSocial_ConfigLerValor;
    procedure Test_eSocial_ConfigGravarValor;

    procedure Test_eSocial_CriarEventoeSocial;
    procedure Test_eSocial_EnviareSocial;
    procedure Test_eSocial_ConsultareSocial;
    procedure Test_eSocial_CriarEnviareSocial;
    procedure Test_eSocial_LimpareSocial;
    procedure Test_eSocial_CarregarXMLEventoeSocial;
    procedure Test_eSocial_SetIDEmpregador;
    procedure Test_eSocial_SetIDTransmissor;
    procedure Test_eSocial_SetTipoEmpregador;
    procedure Test_eSocial_SetVersaoDF;
    procedure Test_eSocial_ConsultaIdentificadoresEventosEmpregador;
    procedure Test_eSocial_ConsultaIdentificadoresEventosTabela;
    procedure Test_eSocial_ConsultaIdentificadoresEventosTrabalhador;
    procedure Test_eSocial_DownloadEventos;

  end;

implementation

uses
  ACBrLibeSocialStaticImportST, ACBrLibeSocialConsts, ACBrLibConsts, ACBrUtil;

procedure TTestACBreSocialLib.Test_eSocial_Inicializar_Com_DiretorioInvalido;
var
  Handle: THandle;
begin
  try
    AssertEquals(ErrLibNaoInicializada, eSocial_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
  except
    on E: exception do
    ShowMessage('Error: '+ E.ClassName + #13#10 + E.Message);
  end;
end;

procedure TTestACBreSocialLib.Test_eSocial_Inicializar;
var
  Handle: THandle;
begin
  AssertEquals(ErrLibNaoInicializada, eSocial_Inicializar( '',''));
  AssertEquals(ErrLibNaoFinalizada, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_Inicializar_Ja_Inicializado;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, eSocial_Inicializar( '', ''));
  AssertEquals(ErrOk, eSocial_Inicializar( '',''));
  AssertEquals(ErrOK, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_Finalizar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, eSocial_Inicializar( '', ''));
  AssertEquals(ErrOk, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_Finalizar_Ja_Finalizado;
var
  Handle: THandle;
begin
  try
   AssertEquals(ErrOk, eSocial_Inicializar('', ''));
   AssertEquals(ErrOk, eSocial_Finalizar());
   //AssertEquals(ErrOk, eSocial_Finalizar());
  except
    on E: Exception do
    ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBreSocialLib.Test_eSocial_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, eSocial_Inicializar( '', ''));
  Bufflen := 0;
  AssertEquals(ErrOk, eSocial_Nome( Nil, Bufflen));
  AssertEquals(Length(CLibeSocialNome), Bufflen);
  AssertEquals(ErrOK, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, eSocial_Inicializar( '',''));
  Bufflen := Length(CLibeSocialNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, eSocial_Nome( PChar(AStr), Bufflen));
  AssertEquals(Length(CLibeSocialNome), Bufflen);
  AssertEquals(CLibeSocialNome, AStr);
  AssertEquals(ErrOK, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, eSocial_Inicializar( '', ''));
  Bufflen := Length(CLibeSocialNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, eSocial_Nome( PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibeSocialNome), Bufflen);
  AssertEquals(CLibeSocialNome, AStr);
  AssertEquals(ErrOK, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, eSocial_Inicializar( '', ''));
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, eSocial_Nome( PChar(AStr), Bufflen));
  AssertEquals(4, Bufflen);
  AssertEquals(copy(CLibeSocialNome,1,4), AStr);
  AssertEquals(ErrOK, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_Versao;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, eSocial_Inicializar( '', ''));
  Bufflen := 0;
  AssertEquals(ErrOk, eSocial_Versao( Nil, Bufflen));
  AssertEquals(Length(CLibeSocialVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, eSocial_Versao( PChar(AStr), Bufflen));
  AssertEquals(Length(CLibeSocialVersao), Bufflen);
  AssertEquals(CLibeSocialVersao, AStr);
  AssertEquals(ErrOK, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, eSocial_Inicializar( '', ''));
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, eSocial_ConfigLerValor( CSessaoVersao, CLibeSocialNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibeSocialVersao, AStr);
  AssertEquals(ErrOK, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr: String;
  Handle: THandle;
begin
  // Gravando o valor
  AssertEquals(ErrOK, eSocial_Inicializar( '', ''));
  AssertEquals('Erro ao Mudar configuração', ErrOk, eSocial_ConfigGravarValor( CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, eSocial_ConfigLerValor( CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);
  AssertEquals(ErrOK, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_CriarEventoeSocial;
var
  Handle: THandle;
begin
  // Lendo o arquivo INI
  AssertEquals(ErrOK, eSocial_Inicializar( '', ''));
  AssertEquals('Erro ao ler o arquivo INI', ErrOk,
  eSocial_CriarEventoeSocial( 'C:\ProjetoACBr\ACBr\Projetos\ACBrLib\Testes\eSocial\S1000.ini'));
  AssertEquals(ErrOK, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_EnviareSocial;
var
  Resposta: PChar;
  Tamanho: Longint;
  Handle: THandle;
begin
  // Iniciando o Envio
  Resposta := '';
  Tamanho := 0;

  AssertEquals(ErrOK, eSocial_Inicializar( '', ''));
  AssertEquals('Erro ao Enviar', ErrOk, eSocial_EnviareSocial( 1, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOK, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_ConsultareSocial;
var
  Resposta: PChar;
  Tamanho: Longint;
  Handle: THandle;
begin
  // Iniciando a consulta
  Resposta := '';
  Tamanho := 0;

  AssertEquals(ErrOK, eSocial_Inicializar( '', ''));
  AssertEquals('Erro ao consultar', ErrOk, eSocial_ConsultareSocial( '123456789', Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(ErrOK, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_CriarEnviareSocial;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, eSocial_Inicializar('', ''));
  AssertEquals(ErrOk, eSocial_CriarEnviareSocial('C:\ProjetoACBr\ACBr\Projetos\ACBrLib\Testes\eSocial\S1000.ini', 1));
  AssertEquals(ErrOK, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_LimpareSocial;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, eSocial_Inicializar('', ''));
  AssertEquals(ErrOk, eSocial_LimpareSocial());
  AssertEquals(ErrOk, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_CarregarXMLEventoeSocial;
var
  Handle: THandle;
begin
  AssertEquals(ErrLibNaoInicializada, eSocial_Inicializar('', ''));
  AssertEquals('Erro ao carregar o XML e-Social', ErrExecutandoMetodo, eSocial_CarregarXMLEventoeSocial('C:\ProjetoACBr\ACBr\Projetos\ACBrLib\Testes\eSocial\1038734840000002022060611235282164-S-2200-0.xml'));
  AssertEquals(ErrLibNaoFinalizada, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_SetIDEmpregador;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, eSocial_Inicializar('', ''));
  AssertEquals(ErrOk, eSocial_SetIDEmpregador('03873484'));
  AssertEquals(ErrOk, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_SetIDTransmissor;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, eSocial_Inicializar('', ''));
  AssertEquals(ErrOk, eSocial_SetIDTransmissor('1234'));
  AssertEquals(ErrOk, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_SetTipoEmpregador;
var
  Handle: THandle;
begin
  AssertEquals(errOk, eSocial_Inicializar('',''));
  AssertEquals(errOk, eSocial_SetTipoEmpregador(1));
  AssertEquals(errOk, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_SetVersaoDF;
var
  Handle: THandle;
begin
  AssertEquals(errOk, eSocial_Inicializar('', ''));
  AssertEquals(ErrOk, eSocial_SetVersaoDF('ve02_04_02'));
  AssertEquals(errOk, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_ConsultaIdentificadoresEventosEmpregador;
var
  Resposta: PChar;
  Tamanho: Longint;
  Handle: THandle;
begin
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals(errOk, eSocial_Inicializar('', ''));
  AssertEquals('Erro ao Consultar Evento Empregador', errOk, eSocial_ConsultaIdentificadoresEventosEmpregador('03873484',1,20220609, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(errOk, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_ConsultaIdentificadoresEventosTabela;
var
  Resposta: Pchar;
  Tamanho: Longint;
  Handle: THandle;
begin
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals(errOk, eSocial_Inicializar('', ''));
  AssertEquals('Erro ao Consultar Evento Tabela', errOk, eSocial_ConsultaIdentificadoresEventosTabela('03873484',1,'1234',20220609,20220609, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(errOk, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_ConsultaIdentificadoresEventosTrabalhador;
var
  Resposta: Pchar;
  Tamanho: Longint;
  Handle: THandle;
begin
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals(errOk, eSocial_Inicializar('', ''));
  AssertEquals('Erro ao Consultar Evento Trabalhador', errOk, eSocial_ConsultaIdentificadoresEventosTrabalhador('03873484','73714542191',20220609,20220609, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(errOk, eSocial_Finalizar());
end;

procedure TTestACBreSocialLib.Test_eSocial_DownloadEventos;
var
  Resposta: Pchar;
  Tamanho: Longint;
  Handle: THandle;
begin
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals(errOk, eSocial_Inicializar('', ''));
  AssertEquals('Erro ao realizar Download Eventos', errOk, eSocial_DownloadEventos('03873484','73714542191',20220609,20220609, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  AssertEquals(errOk, eSocial_Finalizar());
end;

initialization
  RegisterTest(TTestACBreSocialLib);

end.

