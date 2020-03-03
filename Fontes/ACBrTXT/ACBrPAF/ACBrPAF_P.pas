{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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

unit ACBrPAF_P;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrPAFRegistros;

type
  /// REGISTRO TIPO P1 - IDENTIFICAÇÃO DO ESTABELECIMENTO USUÁRIO DO PAF-ECF

  TRegistroP1 = class(TRegistroX1)
  end;

  /// REGISTRO TIPO P2 - RELAÇÃO DE MERCADORIAS E SERVIÇOS

  TRegistroP2 = class
  private
    fRegistroValido: boolean;
    fCOD_MERC_SERV: string;     /// Código da mercadoria ou serviço
    fCEST : string;             /// Código Especificador da Substituição Tributária
    fNCM : string;              /// Nomeclatura Comum do Mercosul
    fDESC_MERC_SERV: string;    /// Descrição da mercadoria ou serviço
    fUN_MED: string;            /// Unidade de medida
    fIAT: string;               /// Indicador de Arredondamento ou Truncamento, conforme item 7.2.1.3
    fIPPT: string;              /// Indicador de Produção Própria ou de Terceiro, conforme item 7.2.1.4
    fST: string;                /// Código da Situação Tributaria conforme tabela constante no item 7.2.1.5
    fALIQ: currency;            /// Alíquota, conforme item 7.2.1.6
    fVL_UNIT: currency;         /// Valor unitário com duas casas decimais
  public
    constructor Create; virtual; /// Create

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property COD_MERC_SERV: string read FCOD_MERC_SERV write FCOD_MERC_SERV;
    property CEST: string read fCEST write fCEST;
    property NCM: string read fNCM write fNCM;
    property DESC_MERC_SERV: string read FDESC_MERC_SERV write FDESC_MERC_SERV;
    property UN_MED: string read FUN_MED write FUN_MED;
    property IAT: string read FIAT write FIAT;
    property IPPT: string read FIPPT write FIPPT;
    property ST: string read FST write FST;
    property ALIQ: currency read FALIQ write FALIQ;
    property VL_UNIT: currency read FVL_UNIT write FVL_UNIT;
  end;

  /// REGISTRO P2 - Lista

  TRegistroP2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroP2;
    procedure SetItem(Index: Integer; const Value: TRegistroP2);
  public
    function New: TRegistroP2;
    property Items[Index: Integer]: TRegistroP2 read GetItem write SetItem;
  end;

  /// REGISTRO TIPO E9 - TOTALIZAÇÃO DO ARQUIVO

  TRegistroP9 = class(TRegistroX9)
  end;

implementation

(* TRegistroP2List *)

function TRegistroP2List.GetItem(Index: Integer): TRegistroP2;
begin
  Result := TRegistroP2(inherited Items[Index]);
end;

function TRegistroP2List.New: TRegistroP2;
begin
  Result := TRegistroP2.Create;
  Add(Result);
end;

procedure TRegistroP2List.SetItem(Index: Integer; const Value: TRegistroP2);
begin
  Put(Index, Value);
end;

{ TRegistroP2 }

constructor TRegistroP2.Create;
begin
   fRegistroValido := True;
end;

end.
