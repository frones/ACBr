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

unit ACBrPAF_C;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrPAFRegistros;

type
  /// REGISTRO TIPO C1 - IDENTIFICAÇÃO DO ESTABELECIMENTO USUÁRIO DO PAF-ECF

  TRegistroC1 = class(TRegistroX1)
  end;

  /// REGISTRO TIPO C2 - RELAÇÃO DE MERCADORIAS E SERVIÇOS

  TRegistroC2 = class
  private
    fRegistroValido: boolean;
    fID_ABASTECIMENTO: string;
    fTANQUE: string;
    fBOMBA: string;
    fBICO: string;
    fCOMBUSTIVEL: string;
    fDATA_ABASTECIMENTO: TDateTime;
    fHORA_ABASTECIMENTO: TDateTime;
    fENCERRANTE_INICIAL: currency;
    fENCERRANTE_FINAL: currency;
    fSTATUS_ABASTECIMENTO: string;
    fNRO_SERIE_ECF: string;
    fDATA: TDateTime;
    fHORA: TDateTime;
    fCOO: Integer;
    fNRO_NOTA_FISCAL: Integer;
    fVOLUME: Currency;
  public
    constructor Create; virtual; /// Create

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property ID_ABASTECIMENTO: string read fID_ABASTECIMENTO write fID_ABASTECIMENTO;
    property TANQUE: string read fTANQUE write fTANQUE;
    property BOMBA: string read fBOMBA write fBOMBA;
    property BICO: string read fBICO write fBICO;
    property COMBUSTIVEL: string read fCOMBUSTIVEL write fCOMBUSTIVEL;
    property DATA_ABASTECIMENTO: TDateTime read fDATA_ABASTECIMENTO write fDATA_ABASTECIMENTO;
    property HORA_ABASTECIMENTO: TDateTime read fHORA_ABASTECIMENTO write fHORA_ABASTECIMENTO;
    property ENCERRANTE_INICIAL: currency read fENCERRANTE_INICIAL write fENCERRANTE_INICIAL;
    property ENCERRANTE_FINAL: currency read fENCERRANTE_FINAL write fENCERRANTE_FINAL;
    property STATUS_ABASTECIMENTO: string read fSTATUS_ABASTECIMENTO write fSTATUS_ABASTECIMENTO;
    property NRO_SERIE_ECF: string read fNRO_SERIE_ECF write fNRO_SERIE_ECF;
    property DATA: TDateTime read fDATA write fDATA;
    property HORA: TDateTime read fHORA write fHORA;
    property COO: Integer read fCOO write fCOO;
    property NRO_NOTA_FISCAL: Integer read fNRO_NOTA_FISCAL write fNRO_NOTA_FISCAL;
    property VOLUME: Currency read fVOLUME write fVOLUME;

  end;

  /// REGISTRO C2 - Lista

  TRegistroC2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC2;
    procedure SetItem(Index: Integer; const Value: TRegistroC2);
  public
    function New: TRegistroC2;
    property Items[Index: Integer]: TRegistroC2 read GetItem write SetItem;
  end;

  /// REGISTRO TIPO C9 - TOTALIZAÇÃO DO ARQUIVO

  TRegistroC9 = class(TRegistroX9)
  end;

implementation

(* TRegistroC2List *)

function TRegistroC2List.GetItem(Index: Integer): TRegistroC2;
begin
  Result := TRegistroC2(inherited Items[Index]);
end;

function TRegistroC2List.New: TRegistroC2;
begin
  Result := TRegistroC2.Create;
  Add(Result);
end;

procedure TRegistroC2List.SetItem(Index: Integer; const Value: TRegistroC2);
begin
  Put(Index, Value);
end;

{ TRegistroC2 }

constructor TRegistroC2.Create;
begin
   fRegistroValido := True;
end;

end.
