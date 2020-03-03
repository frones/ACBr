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

unit ACBrPAF_B;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrPAFRegistros;

type
  /// REGISTRO TIPO B1 - IDENTIFICAÇÃO DO ESTABELECIMENTO USUÁRIO DO PAF-ECF

  TRegistroB1 = class(TRegistroX1)
  end;

  /// REGISTRO TIPO B2 - REGISTRO DE SUBSTITUIÇÃO DA PLACA ELETRÓNICA DE GERENCIAMENTO DE BOMBA DE COMBUSTIVEL

  TRegistroB2 = class
  private
    fRegistroValido: boolean;
//    fTANQUE: string;
    fBOMBA: string;
    fBICO: string;
    fDATA: TDateTime;
    fHORA: TDateTime;
    fMOTIVO: string;
    fCNPJ_EMPRESA: string;
    fCPF_TECNICO: string;
    fNRO_LACRE_ANTES: string;
    fNRO_LACRE_APOS: string;
    fENCERRANTE_ANTES: currency;
    fENCERRANTE_APOS: currency;
  public
    constructor Create; virtual; /// Create

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property BOMBA: string read fBOMBA write fBOMBA;
    property BICO: string read fBICO write fBICO;
    property DATA: TDateTime read fDATA write fDATA;
    property HORA: TDateTime read fHORA write fHORA;
    property MOTIVO: string read fMOTIVO write fMOTIVO;
    property CNPJ_EMPRESA: string read fCNPJ_EMPRESA write fCNPJ_EMPRESA;
    property CPF_TECNICO: string read fCPF_TECNICO write fCPF_TECNICO;
    property NRO_LACRE_ANTES: string read fNRO_LACRE_ANTES write fNRO_LACRE_ANTES;
    property NRO_LACRE_APOS: string read fNRO_LACRE_APOS write fNRO_LACRE_APOS;
    property ENCERRANTE_ANTES: currency read fENCERRANTE_ANTES write fENCERRANTE_ANTES;
    property ENCERRANTE_APOS: currency read fENCERRANTE_APOS write fENCERRANTE_APOS;
  end;

  /// REGISTRO B2 - Lista

  TRegistroB2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroB2;
    procedure SetItem(Index: Integer; const Value: TRegistroB2);
  public
    function New: TRegistroB2;
    property Items[Index: Integer]: TRegistroB2 read GetItem write SetItem;
  end;

  /// REGISTRO TIPO B9 - TOTALIZAÇÃO DO ARQUIVO

  TRegistroB9 = class(TRegistroX9)
  end;

implementation

(* TRegistroB2List *)

function TRegistroB2List.GetItem(Index: Integer): TRegistroB2;
begin
  Result := TRegistroB2(inherited Items[Index]);
end;

function TRegistroB2List.New: TRegistroB2;
begin
  Result := TRegistroB2.Create;
  Add(Result);
end;

procedure TRegistroB2List.SetItem(Index: Integer; const Value: TRegistroB2);
begin
  Put(Index, Value);
end;

{ TRegistroB2 }

constructor TRegistroB2.Create;
begin
  fRegistroValido := True;
end;

end.

