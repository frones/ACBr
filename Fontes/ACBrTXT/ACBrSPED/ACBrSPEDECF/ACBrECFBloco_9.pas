{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti e Isaque Pinheiro            }
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

{$I ACBr.inc}


unit ACBrECFBloco_9;

interface

uses
  ACBrECFBlocos, Classes, Contnrs, DateUtils, SysUtils;

type
  /// Registro 9001 - ABERTURA DO BLOCO 9

  TRegistro9001 = class(TOpenBlocos)
  private
  public
  end;

  /// Registro 9100 - AVISOS DA ESCITURAÇÃO

  TRegistro9100 = class(TBlocos)
  private
    fCAMPO:     string;
    fCONTEUDO:  variant;
    fMSG_REGRA: string;
    fREGISTRO:  string;
    fVALOR_ESPERADO: variant;
  public
    property MSG_REGRA: string read fMSG_REGRA write fMSG_REGRA;
    property REGISTRO: string read fREGISTRO write fREGISTRO;
    property CAMPO: string read fCAMPO write fCAMPO;
    property CONTEUDO: variant read fCONTEUDO write fCONTEUDO;
    property VALOR_ESPERADO: variant read fVALOR_ESPERADO write fVALOR_ESPERADO;
  end;

  /// Registro 9100 - Lista

  TRegistro9100List = class(TObjectList)
  private
    function GetItem(Index: integer): TRegistro9100; /// GetItem
    procedure SetItem(Index: integer; const Value: TRegistro9100); /// SetItem
  public
    function New: TRegistro9100;
    property Items[Index: integer]: TRegistro9100 read GetItem write SetItem;
  end;

  /// Registro 9900 - REGISTROS DO ARQUIVO

  TRegistro9900 = class(TBlocos)
  private
    fREG_BLC:     string;    /// Registro que será totalizado no próximo campo.
    fQTD_REG_BLC: integer;   /// Total de registros do tipo informado no campo anterior.
    fVERSAO:      string;
    // versão da tabela dinamica utilizada, será somente para registro dinamicos
    fID_TAB_DIN:  string;    //identificação da tabela dinamica utilizada
  public
    property REG_BLC: string read fREG_BLC write fREG_BLC;
    property QTD_REG_BLC: integer read fQTD_REG_BLC write fQTD_REG_BLC;
    property VERSAO: string read fVERSAO write fVERSAO;
    property ID_TAB_DIN: string read fID_TAB_DIN write fID_TAB_DIN;
  end;

  /// Registro 9900 - Lista

  TRegistro9900List = class(TObjectList)
  private
    function GetItem(Index: integer): TRegistro9900; /// GetItem
    procedure SetItem(Index: integer; const Value: TRegistro9900); /// SetItem
  public
    function New: TRegistro9900;
    property Items[Index: integer]: TRegistro9900 read GetItem write SetItem;
  end;

  /// Registro 9990 - ENCERRAMENTO DO BLOCO 9

  TRegistro9990 = class(TCloseBlocos)
  end;

  /// Registro 9999 - ENCERRAMENTO DO ARQUIVO DIGITAL
  TRegistro9999 = class(TCloseBlocos)
  end;

implementation

{ TRegistro9100List }

function TRegistro9100List.GetItem(Index: integer): TRegistro9100;
begin
  Result := TRegistro9100(inherited Items[Index]);
end;

procedure TRegistro9100List.SetItem(Index: integer; const Value: TRegistro9100);
begin
  Put(Index, Value);
end;

function TRegistro9100List.New: TRegistro9100;
begin
  Result := TRegistro9100.Create;
  Add(Result);
end;

{ TRegistro9900List }

function TRegistro9900List.GetItem(Index: integer): TRegistro9900;
begin
  Result := TRegistro9900(inherited Items[Index]);
end;

function TRegistro9900List.New: TRegistro9900;
begin
  Result := TRegistro9900.Create;
  Add(Result);
end;

procedure TRegistro9900List.SetItem(Index: integer; const Value: TRegistro9900);
begin
  Put(Index, Value);
end;

end.
