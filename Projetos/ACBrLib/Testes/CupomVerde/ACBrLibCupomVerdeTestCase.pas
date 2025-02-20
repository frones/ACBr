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

unit ACBrLibCupomVerdeTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrCupomVerdeLib }

  TTestACBrCupomVerdeLib = class(TTestCase)
    private
      fCaminhoExec: string;
    public
      procedure SetUp; override;
    published
      procedure Test_CupomVerde_Inicializar_Com_DiretorioInvalido;
      procedure Test_CupomVerde_Inicializar;
      procedure Test_CupomVerde_Inicializar_Ja_Inicializado;
      procedure Test_CupomVerde_Finalizar;
      procedure Test_CupomVerde_Finalizar_Ja_Finalizado;
      procedure Test_CupomVerde_Nome_Obtendo_LenBuffer;
      procedure Test_CupomVerde_Nome_Lendo_Buffer_Tamanho_Identico;
      procedure Test_CupomVerde_Nome_Lendo_Buffer_Tamanho_Maior;
      procedure Test_CupomVerde_Nome_Lendo_Buffer_Tamanho_Menor;
      procedure Test_CupomVerde_Versao;
      procedure Test_CupomVerde_ConfigLerValor;
      procedure Test_CupomVerde_ConfigGravarValor;
  end;

implementation

uses
  ACBrLibCupomVerdeStaticImportMT, ACBrLibCupomVerdeConsts, ACBrLibConsts, Dialogs;

{ TTestACBrCupomVerdeLib }

procedure TTestACBrCupomVerdeLib.SetUp;
begin
  inherited SetUp;
  fCaminhoExec := ExtractFileDir(ParamStr(0));
end;

procedure TTestACBrCupomVerdeLib.Test_CupomVerde_Inicializar_Com_DiretorioInvalido;
var
  Handle: THandle;
begin
  try
    //CupomVerde_Inicializar(Handle);
    AssertEquals(ErrDiretorioNaoExiste, CupomVerde_Inicializar(Handle, 'C:\NAOEXISTE\ACBrLib.ini',''));
  except
    on E: Exception do
     ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message );
  end;
end;

procedure TTestACBrCupomVerdeLib.Test_CupomVerde_Inicializar;
var
  Handle: THandle;
begin
  AssertEquals(ErrOK, CupomVerde_Inicializar(Handle, '',''));
  AssertEquals(ErrOK, CupomVerde_Finalizar(Handle));
end;

procedure TTestACBrCupomVerdeLib.Test_CupomVerde_Inicializar_Ja_Inicializado;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, CupomVerde_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, CupomVerde_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, CupomVerde_Finalizar(Handle));
end;

procedure TTestACBrCupomVerdeLib.Test_CupomVerde_Finalizar;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, CupomVerde_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, CupomVerde_Finalizar(Handle));
end;

procedure TTestACBrCupomVerdeLib.Test_CupomVerde_Finalizar_Ja_Finalizado;
var
  Handle: THandle;
begin
    AssertEquals(ErrOk, CupomVerde_Inicializar(Handle,'',''));
    AssertEquals(ErrOk, CupomVerde_Finalizar(Handle));
    //AssertEquals(ErrOk, CupomVerde_Finalizar(Handle));
end;

procedure TTestACBrCupomVerdeLib.Test_CupomVerde_Nome_Obtendo_LenBuffer;
var
  Handle: THandle;
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOk, CupomVerde_Inicializar(Handle,'',''));

  Bufflen := 0;
  AssertEquals(ErrOk, CupomVerde_Nome(Handle,Nil, Bufflen));
  AssertEquals(Length(CLibCupomVerdeNome), Bufflen);

  AssertEquals(ErrOk, CupomVerde_Finalizar(Handle));
end;

procedure TTestACBrCupomVerdeLib.Test_CupomVerde_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, CupomVerde_Inicializar(Handle, '',''));

  Bufflen := Length(CLibCupomVerdeNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CupomVerde_Nome(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibCupomVerdeNome), Bufflen);
  AssertEquals(CLibCupomVerdeNome, AStr);

  AssertEquals(ErrOK, CupomVerde_Finalizar(Handle));
end;

procedure TTestACBrCupomVerdeLib.Test_CupomVerde_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, CupomVerde_Inicializar(Handle, '', ''));

  Bufflen := Length(CLibCupomVerdeNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CupomVerde_Nome(Handle, PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibCupomVerdeNome), Bufflen);
  AssertEquals(CLibCupomVerdeNome, AStr);

  AssertEquals(ErrOK, CupomVerde_Finalizar(Handle));
end;

procedure TTestACBrCupomVerdeLib.Test_CupomVerde_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  AssertEquals(ErrOK, CupomVerde_Inicializar(Handle,  '', ''));

  Bufflen := 17;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CupomVerde_Nome(Handle,  PChar(AStr), Bufflen));
  AssertEquals(17, Bufflen);
  AssertEquals(copy(CLibCupomVerdeNome,1,17), AStr);

  AssertEquals(ErrOK, CupomVerde_Finalizar(Handle));
end;

procedure TTestACBrCupomVerdeLib.Test_CupomVerde_Versao;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, CupomVerde_Inicializar(Handle,  '', ''));

  Bufflen := 0;
  AssertEquals(ErrOk, CupomVerde_Versao(Handle,  Nil, Bufflen));
  AssertEquals(Length(CLibCupomVerdeVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CupomVerde_Versao(Handle, PChar(AStr), Bufflen));
  AssertEquals(Length(CLibCupomVerdeVersao), Bufflen);
  AssertEquals(CLibCupomVerdeVersao, AStr);

  AssertEquals(ErrOK, CupomVerde_Finalizar(Handle));
end;

procedure TTestACBrCupomVerdeLib.Test_CupomVerde_ConfigLerValor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
  // Obtendo o Tamanho //
  AssertEquals(ErrOK, CupomVerde_Inicializar(Handle, '', ''));

  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CupomVerde_ConfigLerValor(Handle, CSessaoVersao, CLibCupomVerdeNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibCupomVerdeVersao, AStr);

  AssertEquals(ErrOK, CupomVerde_Finalizar(Handle));
end;

procedure TTestACBrCupomVerdeLib.Test_CupomVerde_ConfigGravarValor;
var
  AStr: String;
  Bufflen: Integer;
  Handle: THandle;
begin
    // Gravando o valor
  AssertEquals(ErrOK, CupomVerde_Inicializar(Handle, '', ''));

  AssertEquals('Erro ao Mudar configuração', ErrOk, CupomVerde_ConfigGravarValor(Handle, CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, CupomVerde_ConfigLerValor(Handle, CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);

  AssertEquals(ErrOK, CupomVerde_Finalizar(Handle));
end;

initialization

  RegisterTest(TTestACBrCupomVerdeLib);

end.

