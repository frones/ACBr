{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrLibNFSeTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrNFSeLib }

  TTestACBrNFSeLib= class(TTestCase)
  published
    procedure Test_NFSE_Inicializar_Com_DiretorioInvalido;
    procedure Test_NFSE_Inicializar;
    procedure Test_NFSE_Inicializar_Ja_Inicializado;
    procedure Test_NFSE_Finalizar;
    procedure Test_NFSE_Finalizar_Ja_Finalizado;
    procedure Test_NFSE_Nome_Obtendo_LenBuffer;
    procedure Test_NFSE_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_NFSE_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_NFSE_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_NFSE_Versao;
    procedure Test_NFSE_ConfigLerValor;
    procedure Test_NFSE_ConfigGravarValor;
    procedure Test_NFSE_LimparLista;
    procedure Test_NFSE_CarregarXML;
    procedure Test_NFSE_CarregarINI;
    procedure Test_NFSE_ObterXml;
    procedure Test_NFSE_GravarXml;
    procedure Test_NFSE_ObterIni;
    procedure Test_NFSE_GravarIni;
    procedure Test_NFSE_ObterCertificados;
    procedure Test_NFSE_Emitir;

  end;

implementation

uses
  ACBrLibNFSeStaticImportMT, ACBrLibNFSeConsts, ACBrLibConsts, Dialogs;

procedure TTestACBrNFSeLib.Test_NFSE_Inicializar_Com_DiretorioInvalido;
var
  Handle: THandle;
begin
  try
    //NFSE_Finalizar(Handle);
    AssertEquals(ErrDiretorioNaoExiste, NFSE_Inicializar(Handle,'C:\NAOEXISTE\ACBrLib.ini',''));
  except
  on E: Exception do
     ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end
end;

procedure TTestACBrNFSeLib.Test_NFSE_Inicializar;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, NFSE_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Inicializar_Ja_Inicializado;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, NFSE_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, NFSE_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Finalizar;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, NFSE_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Finalizar_Ja_Finalizado;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, NFSE_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, NFSE_Finalizar(Handle));
    //AssertEquals(ErrOk, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Nome_Obtendo_LenBuffer;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, NFSE_Inicializar(Handle,'',''));

  Bufflen := 0;
  AssertEquals(ErrOk, NFSE_Nome(Handle,Nil, Bufflen));
  AssertEquals(Length(CLibNFSeNome), Bufflen);

  AssertEquals(ErrOk, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '',''));

  Bufflen := Length(CLibNFSeNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFSE_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibNFSeNome), Bufflen);
  AssertEquals(CLibNFSeNome, AStr);

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  Bufflen := Length(CLibNFSeNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFSE_Nome(Handle, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibNFSeNome), Bufflen);
  AssertEquals(CLibNFSeNome, AStr);

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle,  '', ''));

  Bufflen := 11;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFSE_Nome(Handle,  PChar(AStr), Bufflen));
  AssertEquals(11, Bufflen);
  AssertEquals(copy(CLibNFSeNome,1,11), AStr);

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Versao;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, NFSE_Inicializar(Handle,  '', ''));

  Bufflen := 0;
  AssertEquals(ErrOk, NFSE_Versao(Handle,  Nil, Bufflen));
  AssertEquals(Length(CLibNFSeVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFSE_Versao(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibNFSeVersao), Bufflen);
  AssertEquals(CLibNFSeVersao, AStr);

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConfigLerValor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, CSessaoVersao, CLibNFSeNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibNFSeVersao, AStr);

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_ConfigGravarValor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
    // Gravando o valor
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFSE_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFSE_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSe_LimparLista;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao Limpar Lista NFSe', ErrOK, NFSE_LimparLista(Handle));

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSe_CarregarXML;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao carregar XML NFSe', ErrOK,
   NFSE_CarregarXML(Handle, 'C:\ProjetoACBr\ACBr\Projetos\ACBrLib\Testes\NFSe\bin\NFSe.xml'));

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSe_CarregarINI;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao carregar o INI NFSe', ErrExecutandoMetodo,
  NFSE_CarregarINI(Handle, 'C:\ProjetoACBr\ACBr\Projetos\ACBrLib\Testes\NFSe\bin\NFSe.ini'));

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_ObterXml;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals('Erro ao ObterXML', ErrIndex,
  NFSE_ObterXml(Handle, 0, Resposta, Tamanho));
  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_GravarXml;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao Gravar XML', ErrIndex,
  NFSE_GravarXml(Handle, 0, 'NFSeTeste', 'C:\ProjetoACBr\ACBr\Projetos\ACBrLib\Testes\NFSe\bin'));

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_ObterIni;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals('Erro ao Obter Ini', ErrIndex,
  NFSE_ObterIni(Handle, 0, Resposta, Tamanho));
  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_GravarIni;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao Gravar Ini', ErrIndex,
  NFSE_GravarIni(Handle, 0, 'NFSeTeste', 'C:\ProjetoACBr\ACBr\Projetos\ACBrLib\Testes\NFSe\bin'));

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_ObterCertificados;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
  Resposta:= '';
  Tamanho:= 0;

  AssertEquals('Erro ao Obter Certificados', ErrExecutandoMetodo,
  NFSE_ObterCertificados(Handle, Resposta, Tamanho));
  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

procedure TTestACBrNFSeLib.Test_NFSE_Emitir;
var
  Handle: THandle;
  Resposta: PChar;
  Tamanho: Longint;
begin
  AssertEquals(ErrOK, NFSE_Inicializar(Handle, '', ''));
    Resposta:= '';
  Tamanho:= 0;

  AssertEquals('Erro ao Emitir NFSe', ErrExecutandoMetodo,
  NFSE_Emitir(Handle, '1', 0, False, Resposta, Tamanho));
  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');

  AssertEquals(ErrOK, NFSE_Finalizar(Handle));
end;

initialization

  RegisterTest(TTestACBrNFSeLib);
end.

