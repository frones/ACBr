{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
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

unit ACBrPAF_J;

interface

uses
  SysUtils, Classes, Contnrs, DateUtils, ACBrPAFRegistros;

type
  TRegistroJ2List = class;


  TRegistroJ1 = class
  private
    fCNPJ:                String;    //CNPJ do emissor do doc. fiscal
    fDATA_EMISSAO:        TDateTime; //Data de emissão do documento fiscal
    fSUBTOTAL:            Currency;  //Valor total do documento
    fDESC_SUBTOTAL:       Currency;  //Valor desconto (Valor/Percentual) sobre o subtotal
    fINDICADOR_DESC:      String;      //Indicador de tipo de desconto no subtotal (V - Valor, P - Percentual)
    fACRES_SUBTOTAL:      Currency;
    fINDICADOR_ACRES:     String;
    fVALOR_LIQUIDO:       Currency;
    fINDICADOR_CANC:      String;
    fVAL_CANC_ACRES:      Currency;
    fORDEM_APLIC_DES_ACR: String;
    fNOME_CLIENTE:        String;
    fCPFCNPJ_CLIENTE:     String;
    fNUMERO_NOTA:         String;
    fSERIE_NOTA:          String;
    fCHAVE_NF:            String;
    fTIPO_DOC:            String;

    fRegistroValido:      Boolean;

    fRegistroJ2:          TRegistroJ2List;
  public
    constructor Create; virtual; /// Create
    destructor Destroy; override; /// Destroy

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido default True;
    property CNPJ: String read fCNPJ write fCNPJ;
    property DATA_EMISSAO: TDateTime read fDATA_EMISSAO write fDATA_EMISSAO;
    property SUBTOTAL: Currency read fSUBTOTAL write fSUBTOTAL;
    property DESC_SUBTOTAL: Currency read fDESC_SUBTOTAL write fDESC_SUBTOTAL;
    property INDICADOR_DESC: String read fINDICADOR_DESC write fINDICADOR_DESC;
    property ACRES_SUBTOTAL: Currency read fACRES_SUBTOTAL write fACRES_SUBTOTAL;
    property INDICADOR_ACRES: String read fINDICADOR_ACRES write fINDICADOR_ACRES;
    property VALOR_LIQUIDO: Currency read fVALOR_LIQUIDO write fVALOR_LIQUIDO;
    property INDICADOR_CANC: String read fINDICADOR_CANC write fINDICADOR_CANC;
    property VAL_CANC_ACRES: Currency read fVAL_CANC_ACRES write fVAL_CANC_ACRES;
    property ORDEM_APLIC_DES_ACRES: String read fORDEM_APLIC_DES_ACR write fORDEM_APLIC_DES_ACR;
    property NOME_CLIENTE: String read fNOME_CLIENTE write fNOME_CLIENTE;
    property CPFCNPJ_CLIENTE: String read fCPFCNPJ_CLIENTE write fCPFCNPJ_CLIENTE;
    property NUMERO_NOTA: String read fNUMERO_NOTA write fNUMERO_NOTA;
    property SERIE_NOTA: String read fSERIE_NOTA write fSERIE_NOTA;
    property CHAVE_NF: String read fCHAVE_NF write fCHAVE_NF;
    property TIPO_DOC: String read fTIPO_DOC write fTIPO_DOC;

    property RegistroJ2: TRegistroJ2List read FRegistroJ2 write FRegistroJ2;
  end;

  //RegistroJ1 Lista

  TRegistroJ1List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ1;
    procedure SetItem(Index: Integer; const Value: TRegistroJ1);
  public
    function New: TRegistroJ1;
    property Items[Index: Integer]: TRegistroJ1 read GetItem write SetItem;
  end;

  TRegistroJ2 = class
  private
    fCNPJ:                    String;
    fDATA_EMISSAO:            TDateTime;
    fNUMERO_ITEM:             String;
    fCODIGO_PRODUTO:          String;
    fDESCRICAO:               String;
    fQUANTIDADE:              Currency;
    fUNIDADE:                 String;
    fVALOR_UNITARIO:          Currency;
    fDESCONTO_ITEM:           Currency;
    fACRESCIMO_ITEM:          Currency;
    fVALOR_LIQUIDO:           Currency;
    fTOTALIZADOR_PARCIAL:     String;
    fCASAS_DECIMAIS_QTDE:     String;
    fCASAS_DECIMAIS_VAL_UNIT: String;
    fNUMERO_NOTA:             String;
    fSERIE_NOTA:              String;
    fCHAVE_NF:                String;
    fTIPO_DOC:                String;

    fRegistroValido:          Boolean;
  public
    constructor Create; virtual;

    property RegistroValido: Boolean read fRegistroValido write fRegistroValido;

    property CNPJ: String read FCNPJ write FCNPJ;
    property DATA_EMISSAO: TDateTime read fDATA_EMISSAO write fDATA_EMISSAO;
    property NUMERO_ITEM: String read fNUMERO_ITEM write fNUMERO_ITEM;
    property CODIGO_PRODUTO: String read fCODIGO_PRODUTO write fCODIGO_PRODUTO;
    property DESCRICAO: String read fDESCRICAO write fDESCRICAO;
    property QUANTIDADE: Currency read fQUANTIDADE write fQUANTIDADE;
    property UNIDADE: String read fUNIDADE write fUNIDADE;
    property VALOR_UNITARIO: Currency read fVALOR_UNITARIO write fVALOR_UNITARIO;
    property DESCONTO_ITEM: Currency read fDESCONTO_ITEM write fDESCONTO_ITEM;
    property ACRESCIMO_ITEM: Currency read fACRESCIMO_ITEM write fACRESCIMO_ITEM;
    property VALOR_LIQUIDO: Currency read fVALOR_LIQUIDO write fVALOR_LIQUIDO;
    property TOTALIZADOR_PARCIAL: String read fTOTALIZADOR_PARCIAL write fTOTALIZADOR_PARCIAL;
    property CASAS_DECIMAIS_QTDE: String read fCASAS_DECIMAIS_QTDE write fCASAS_DECIMAIS_QTDE;
    property CASAS_DECIMAIS_VAL_UNIT: String read fCASAS_DECIMAIS_VAL_UNIT write fCASAS_DECIMAIS_VAL_UNIT;
    property NUMERO_NOTA: String read fNUMERO_NOTA write fNUMERO_NOTA;
    property SERIE_NOTA: String read fSERIE_NOTA write fSERIE_NOTA;
    property CHAVE_NF: String read fCHAVE_NF write fCHAVE_NF;
    property TIPO_DOC: String read fTIPO_DOC write fTIPO_DOC;
  end;

  //RegistroJ2 Lista

  TRegistroJ2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ2;
    procedure SetItem(Index: Integer; const Value: TRegistroJ2);
  public
    function New: TRegistroJ2;
    property Items[Index: Integer]: TRegistroJ2 read GetItem write SetItem;
  end;
implementation

(* TRegistroJ1List *)

function TRegistroJ1List.GetItem(Index: Integer): TRegistroJ1;
begin
  Result := TRegistroJ1(inherited Items[Index]);
end;

function TRegistroJ1List.New: TRegistroJ1;
begin
  Result := TRegistroJ1.Create;
  Add(Result);
end;

procedure TRegistroJ1List.SetItem(Index: Integer;
  const Value: TRegistroJ1);
begin
  Put(Index, Value);
end;

{ TRegistroJ1 }

constructor TRegistroJ1.Create;
begin
  fRegistroJ2 := TRegistroJ2List.Create;
  fRegistroValido := True;
end;

destructor TRegistroJ1.Destroy;
begin
  fRegistroJ2.Free;
  inherited;
end;

{ TRegistroJ2 }

constructor TRegistroJ2.Create;
begin
  fRegistroValido := True;
end;

{ TRegistroJ2List }

function TRegistroJ2List.GetItem(Index: Integer): TRegistroJ2;
begin
  Result := TRegistroJ2(inherited Items[Index]);
end;

function TRegistroJ2List.New: TRegistroJ2;
begin
  Result := TRegistroJ2.Create;
  Add(Result);
end;

procedure TRegistroJ2List.SetItem(Index: Integer;
  const Value: TRegistroJ2);
begin
  Put(Index, Value);
end;

end.
 
