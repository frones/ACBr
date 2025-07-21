{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
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

unit ACBrLibNFComTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrNFComLib }

  TTestACBrNFComLib = class(TTestCase)
  private
    fCaminhoExec: string;
  public
    procedure SetUp; override;
  published
    procedure Test_NFCom_Inicializar_Com_DiretorioInvalido;
    procedure Test_NFCom_Inicializar;
    procedure Test_NFCom_Inicializar_Ja_Inicializado;
    procedure Test_NFCom_Finalizar;
    procedure Test_NFCom_Finalizar_Ja_Finalizado;
    procedure Test_NFCom_Nome_Obtendo_LenBuffer;
    procedure Test_NFCom_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_NFCom_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_NFCom_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_NFCom_Versao;
    procedure Test_NFCom_ConfigLerValor;
    procedure Test_NFCom_ConfigGravarValor;
end;

implementation

uses
  ACBrLibNFComStaticImportMT, ACBrLibNFComConsts, ACBrLibConsts, Dialogs;

{ TTestACBrNFComLib }

procedure TTestACBrNFComLib.SetUp;
begin
  inherited SetUp;
  fCaminhoExec := ExtractFileDir(ParamStr(0));
end;

procedure TTestACBrNFComLib.Test_NFCom_Inicializar_Com_DiretorioInvalido;
var
  Handle: THandle;
begin
  try
    //CupomVerde_Inicializar(Handle);
    AssertEquals(ErrDiretorioNaoExiste, NFCom_Inicializar(Handle, 'C:\NAOEXISTE\ACBrLib.ini',''));
  except
    on E: Exception do
     ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrNFComLib.Test_NFCom_Inicializar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFCom_Inicializar(Handle, '',''));
  AssertEquals(ErrOK, NFCom_Finalizar(Handle));
end;

procedure TTestACBrNFComLib.Test_NFCom_Inicializar_Ja_Inicializado;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, NFCom_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, NFCom_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, NFCom_Finalizar(Handle));
end;

procedure TTestACBrNFComLib.Test_NFCom_Finalizar;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, NFCom_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, NFCom_Finalizar(Handle));
end;

procedure TTestACBrNFComLib.Test_NFCom_Finalizar_Ja_Finalizado;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, NFCom_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, NFCom_Finalizar(Handle));
    //AssertEquals(ErrOk, NFCom_Finalizar(Handle));
end;

procedure TTestACBrNFComLib.Test_NFCom_Nome_Obtendo_LenBuffer;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, NFCom_Inicializar(Handle,'',''));

  Bufflen := 0;
  AssertEquals(ErrOk, NFCom_Nome(Handle,Nil, Bufflen));
  AssertEquals(Length(CLibNFComNome), Bufflen);

  AssertEquals(ErrOk, NFCom_Finalizar(Handle));
end;

procedure TTestACBrNFComLib.Test_NFCom_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFCom_Inicializar(Handle, '',''));

  Bufflen := Length(CLibNFComNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFCom_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibNFComNome), Bufflen);
  AssertEquals(CLibNFComNome, AStr);

  AssertEquals(ErrOK, NFCom_Finalizar(Handle));
end;

procedure TTestACBrNFComLib.Test_NFCom_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFCom_Inicializar(Handle, '', ''));

  Bufflen := Length(CLibNFComNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFCom_Nome(Handle, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibNFComNome), Bufflen);
  AssertEquals(CLibNFComNome, AStr);

  AssertEquals(ErrOK, NFCom_Finalizar(Handle));
end;

procedure TTestACBrNFComLib.Test_NFCom_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, NFCom_Inicializar(Handle,  '', ''));

  Bufflen := 12;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFCom_Nome(Handle,  PChar(AStr), Bufflen));
  AssertEquals(12, Bufflen);
  AssertEquals(copy(CLibNFComNome,1,12), AStr);

  AssertEquals(ErrOK, NFCom_Finalizar(Handle));
end;

procedure TTestACBrNFComLib.Test_NFCom_Versao;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, NFCom_Inicializar(Handle,  '', ''));

  Bufflen := 0;
  AssertEquals(ErrOk, NFCom_Versao(Handle,  Nil, Bufflen));
  AssertEquals(Length(CLibNFComVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFCom_Versao(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibNFComVersao), Bufflen);
  AssertEquals(CLibNFComVersao, AStr);

  AssertEquals(ErrOK, NFCom_Finalizar(Handle));
end;

procedure TTestACBrNFComLib.Test_NFCom_ConfigLerValor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, NFCom_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFCom_ConfigLerValor(Handle, CSessaoVersao, CLibNFComNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibNFComVersao, AStr);

  AssertEquals(ErrOK, NFCom_Finalizar(Handle));
end;

procedure TTestACBrNFComLib.Test_NFCom_ConfigGravarValor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
    // Gravando o valor
  AssertEquals(ErrOK, NFCom_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao Mudar configuração', ErrOk, NFCom_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, NFCom_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);

  AssertEquals(ErrOK, NFCom_Finalizar(Handle));
end;



initialization

  RegisterTest(TTestACBrNFComLib);

end.

