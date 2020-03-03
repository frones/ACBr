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

unit ACBrPAF_E;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrPAFRegistros;

type
  /// REGISTRO TIPO E1 - IDENTIFICAÇÃO DO ESTABELECIMENTO USUÁRIO DO PAF-ECF

  TRegistroE1 = class(TRegistroX1)
  private
    fDT_EST: TDateTime;
    fMODELO_ECF: string;
    fMARCA_ECF: string;
    fTIPO_ECF: string;
    fNUM_FAB: string;
    fMF_ADICIONAL: string;
    fRegistroValido: Boolean;
  public
    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property NUM_FAB: string read fNUM_FAB write fNUM_FAB;
    property MF_ADICIONAL: string read fMF_ADICIONAL write fMF_ADICIONAL;
    property TIPO_ECF: string read fTIPO_ECF write fTIPO_ECF;
    property MARCA_ECF: string read fMARCA_ECF write fMARCA_ECF;
    property MODELO_ECF: string read fMODELO_ECF write fMODELO_ECF;
    property DT_EST: TDateTime read fDT_EST write fDT_EST;
  end;

  /// REGISTRO TIPO E2 - RELAÇÃO DAS MERCADORIAS EM ESTOQUE

  TRegistroE2 = class
  private
    fRegistroValido: boolean;
    fCOD_MERC: string;     /// Código da mercadoria ou produto cadastrado na tabela a que se refere o requisito XI
    fCEST: string;         /// Código Especificador da Substituição Tributária
    fNCM: string;          /// Nomeclatura Comum do Mercosul
    fDESC_MERC: string;    /// Descrição da mercadoria ou produto cadastrada na tabela a que se refere o requisito XI
    fUN_MED: string;       /// Unidade de medida cadastrada na tabela a que se refere o requisito XI
    fQTDE_EST: currency;   /// Quantidade da mercadoria ou produto constante no estoque, com duas casas decimais.
    //fDT_EST: TDateTime;    /// Data de emissão do DAV
  public
    constructor Create; virtual; /// Create

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property COD_MERC: string read FCOD_MERC write FCOD_MERC;
    property CEST: string read fCEST write fCEST;
    property NCM: string read fNCM write fNCM;
    property DESC_MERC: string read FDESC_MERC write FDESC_MERC;
    property UN_MED: string read FUN_MED write FUN_MED;
    property QTDE_EST: currency read FQTDE_EST write FQTDE_EST;
  end;

  /// REGISTRO E2 - Lista

  TRegistroE2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE2;
    procedure SetItem(Index: Integer; const Value: TRegistroE2);
  public
    function New: TRegistroE2;
    property Items[Index: Integer]: TRegistroE2 read GetItem write SetItem;
  end;

  TRegistroE3 = class
  private
    fRegistroValido: boolean;
    fNUM_FAB: string;      //Nº de fabricação do ECF
    fMF_ADICIONAL: string; //Letra indicativa de MF adicional
    fTIPO_ECF: string;     //Tipo do ECF
    fMARCA_ECF: string;    //Marca do ECF
    fMODELO_ECF: string;   //Modelo do ECF
    fDT_EST: TDateTime;    //DataHora de atualização do estoque
  public
    constructor Create; virtual;

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property NUM_FAB: string read FNUM_FAB write FNUM_FAB;
    property MF_ADICIONAL: string read FMF_ADICIONAL write FMF_ADICIONAL;
    property TIPO_ECF: string read FTIPO_ECF write FTIPO_ECF;
    property MARCA_ECF: string read FMARCA_ECF write FMARCA_ECF;
    property MODELO_ECF: string read FMODELO_ECF write FMODELO_ECF;
    property DT_EST: TDateTime read fDT_EST write fDT_EST;
  end;

  /// REGISTRO TIPO E9 - TOTALIZAÇÃO DO ARQUIVO

  TRegistroE9 = class(TRegistroX9)
  end;

implementation

(* TRegistroE2List *)

function TRegistroE2List.GetItem(Index: Integer): TRegistroE2;
begin
  Result := TRegistroE2(inherited Items[Index]);
end;

function TRegistroE2List.New: TRegistroE2;
begin
  Result := TRegistroE2.Create;
  Add(Result);
end;

procedure TRegistroE2List.SetItem(Index: Integer; const Value: TRegistroE2);
begin
  Put(Index, Value);
end;

{ TRegistroE2 }

constructor TRegistroE2.Create;
begin
   fRegistroValido := True;
end;

{ TRegistroE3 }

constructor TRegistroE3.Create;
begin
  fRegistroValido := True;
end;

end.
