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

unit ACBrPAF_T;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrPAFRegistros;

type
  /// REGISTRO TIPO T1 - IDENTIFICAÇÃO DO ESTABELECIMENTO USUÁRIO DO PAF-ECF

  TRegistroT1 = class(TRegistroX1)
  end;

  /// REGISTRO TIPO T2 - MOVIMENTO DIÁRIO - TRANSPORTE DE PASSAGEIROS

  TRegistroT2 = class
  private
    fRegistroValido: boolean;
    fDT_MOV: TDateTime;       /// Data a que se refere o movimento informado
    fTP_DOCTO: string;        /// Tipo do documento a que se refere o movimento informado, conforme item 7.2.1.3
    fSERIE: string;           /// Série do bilhete de passagem, no caso deste tipo de documento
    fNUM_BILH_I: integer;     /// Nº do primeiro bilhete de passagem emitido no dia informado no campo 03, no caso deste tipo de documento
    fNUM_BILH_F: integer;     /// Nº do último bilhete de passagem emitido no dia informado no campo 03, no caso deste tipo de documento
    fNUM_ECF: string;         /// Número de fabricação do ECF, no caso de documento emitido por este equipamento
    fCRZ: integer;            /// Nº do Contador de Redução Z relativo ao documento Redução Z emitido pelo ECF informado no campo 08 no dia informado no campo 03
    fCFOP: string;            /// CFOP relativo ao movimento informado
    fVL_CONT: currency;       /// Valor contábil do movimento informado, com duas casas decimais
    fVL_BASECALC: currency;   /// Base de Cálculo relativa ao movimento informado, com duas casas decimais
    fALIQ: currency;          /// Alíquota do ICMS incidente sobre o movimento informado
    fVL_IMPOSTO: currency;    /// Valor do ICMS incidente sobre o movimento informado, com duas casas decimais
    fVL_ISENTAS: currency;    /// Valor das prestações isentas do ICMS relativas ao movimento informado, com duas casas decimais
    fVL_OUTRAS: currency;     /// Valor de outras situações tributárias relativas ao movimento informado, com duas casas decimais
  public
    constructor Create; virtual; /// Create

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property DT_MOV: TDateTime read FDT_MOV write FDT_MOV;
    property TP_DOCTO: string read FTP_DOCTO write FTP_DOCTO;
    property SERIE: string read FSERIE write FSERIE;
    property NUM_BILH_I: integer read FNUM_BILH_I write FNUM_BILH_I;
    property NUM_BILH_F: integer read FNUM_BILH_F write FNUM_BILH_F;
    property NUM_ECF: string read FNUM_ECF write FNUM_ECF;
    property CRZ: integer read FCRZ write FCRZ;
    property CFOP: string read FCFOP write FCFOP;
    property VL_CONT: currency read FVL_CONT write FVL_CONT;
    property VL_BASECALC: currency read FVL_BASECALC write FVL_BASECALC;
    property ALIQ: currency read FALIQ write FALIQ;
    property VL_IMPOSTO: currency read FVL_IMPOSTO write FVL_IMPOSTO;
    property VL_ISENTAS: currency read FVL_ISENTAS write FVL_ISENTAS;
    property VL_OUTRAS: currency read FVL_OUTRAS write FVL_OUTRAS;
  end;

  /// REGISTRO T2 - Lista

  TRegistroT2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroT2;
    procedure SetItem(Index: Integer; const Value: TRegistroT2);
  public
    function New: TRegistroT2;
    property Items[Index: Integer]: TRegistroT2 read GetItem write SetItem;
  end;

  /// REGISTRO TIPO T9 - TOTALIZAÇÃO DO ARQUIVO

  TRegistroT9 = class(TRegistroX9)
  end;

implementation

(* TRegistroT2List *)

function TRegistroT2List.GetItem(Index: Integer): TRegistroT2;
begin
  Result := TRegistroT2(inherited Items[Index]);
end;

function TRegistroT2List.New: TRegistroT2;
begin
  Result := TRegistroT2.Create;
  Add(Result);
end;

procedure TRegistroT2List.SetItem(Index: Integer; const Value: TRegistroT2);
begin
  Put(Index, Value);
end;

{ TRegistroT2 }

constructor TRegistroT2.Create;
begin
   fRegistroValido := True;
end;

end.
