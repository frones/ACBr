{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrDCe.Classes;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase, ACBrXmlBase,
//  ACBrDFeComum.SignatureClass,
  ACBrDFeComum.Proc,
  pcnSignature,
  ACBrDCe.Conversao;

type

  TinfDCe = class(TObject)
  private
    FId: string;
    FVersao: Double;

  public
    property Id: string read FId write FId;
    property versao: Double read FVersao write FVersao;
  end;

  TIde = class(TObject)
  private
    FcUF: Integer;
    FcDC: Integer;
    Fmodelo: Integer;
    Fserie: Integer;
    FnDC: Integer;
    FdhEmi: TDateTime;
    FtpEmis: TACBrTipoEmissao;
    FtpEmit: TEmitenteDCe;
    FnSiteAutoriz: Integer;
    FcDV: Integer;
    FtpAmb: TACBrTipoAmbiente;
    FverProc: string;
  public
    constructor Create;
    destructor Destroy; override;

    property cUF: Integer read FcUF write FcUF;
    property cDC: Integer read FcDC write FcDC;
    property modelo: Integer read Fmodelo write Fmodelo;
    property serie: Integer read Fserie write Fserie;
    property nDC: Integer read FnDC write FnDC;
    property dhEmi: TDateTime read FdhEmi write FdhEmi;
    property tpEmis: TACBrTipoEmissao read FtpEmis write FtpEmis;
    property tpEmit: TEmitenteDCe read FtpEmit write FtpEmit;
    property nSiteAutoriz: Integer read FnSiteAutoriz write FnSiteAutoriz;
    property cDV: Integer read FcDV write FcDV;
    property tpAmb: TACBrTipoAmbiente read FtpAmb write FtpAmb;
    property verProc: string read FverProc write FverProc;
  end;

  Tendereco = class(TObject)
  private
    FxLgr: string;
    Fnro: string;
    FxCpl: string;
    FxBairro: string;
    FcMun: Integer;
    FxMun: string;
    FUF: string;
    FCEP: Integer;
    FcPais: Integer;
    FxPais: string;
    Ffone: string;
    Femail: string;
  public
    property xLgr: string read FxLgr write FxLgr;
    property nro: string read Fnro write Fnro;
    property xCpl: string read FxCpl write FxCpl;
    property xBairro: string read FxBairro write FxBairro;
    property cMun: Integer read FcMun write FcMun;
    property xMun: string read FxMun write FxMun;
    property UF: string read FUF write FUF;
    property CEP: Integer read FCEP write FCEP;
    property cPais: Integer read FcPais write FcPais;
    property xPais: string read FxPais write FxPais;
    property fone: string read Ffone write Ffone;
    property email: string read Femail write Femail;
  end;

  Temit = class(TObject)
  private
    FCNPJCPF: string;
    FidOutros: string;
    FxNome: string;
    FenderEmit: Tendereco;
  public
    constructor Create;
    destructor Destroy; override;

    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property idOutros: string read FidOutros write FidOutros;
    property xNome: string read FxNome write FxNome;
    property enderEmit: Tendereco read FenderEmit write FenderEmit;
  end;

  TFisco = class(TObject)
  private
    FCNPJ: string;
    FxOrgao: string;
    FUF: string;
  public
    property CNPJ: string read FCNPJ write FCNPJ;
    property xOrgao: string read FxOrgao write FxOrgao;
    property UF: string read FUF write FUF;
  end;

  TMarketplace = class(TObject)
  private
    FCNPJ: string;
    FxNome: string;
    FSite: string;
  public
    property CNPJ: string read FCNPJ write FCNPJ;
    property xNome: string read FxNome write FxNome;
    property Site: string read FSite write FSite;
  end;

  TTransportadora = class(TObject)
  private
    FCNPJ: string;
    FxNome: string;
  public
    property CNPJ: string read FCNPJ write FCNPJ;
    property xNome: string read FxNome write FxNome;
  end;

  TEmpEmisProp = class(TObject)
  private
    FCNPJ: string;
    FxNome: string;
  public
    property CNPJ: string read FCNPJ write FCNPJ;
    property xNome: string read FxNome write FxNome;
  end;

  Tdest = class(TObject)
  private
    FCNPJCPF: string;
    FidOutros: string;
    FxNome: string;
    FenderDest: Tendereco;
  public
    constructor Create;
    destructor Destroy; override;

    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property idOutros: string read FidOutros write FidOutros;
    property xNome: string read FxNome write FxNome;
    property enderDest: Tendereco read FenderDest write FenderDest;
  end;

  TautXMLCollectionItem = class(TObject)
  private
    FCNPJCPF: string;
  public
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
  end;

  TautXMLCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TautXMLCollectionItem;
    procedure SetItem(Index: Integer; Value: TautXMLCollectionItem);
  public
    function Add: TautXMLCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TautXMLCollectionItem;
    property Items[Index: Integer]: TautXMLCollectionItem read GetItem write SetItem; default;
  end;

  TProd = class(TObject)
  private
    FnItem: Integer;
    FxProd: string;
    FNCM: string;
    FqCom: Currency;
    FvUnCom: Double;
    FvProd: Currency;
  public
    property nItem: Integer read FnItem write FnItem;
    property xProd: string read FxProd write FxProd;
    property NCM: string read FNCM write FNCM;
    property qCom: Currency read FqCom write FqCom;
    property vUnCom: Double read FvUnCom write FvUnCom;
    property vProd: Currency read FvProd write FvProd;
  end;

  TDetCollectionItem = class(TObject)
  private
    FProd: TProd;
    FinfAdProd: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Prod: TProd read FProd write FProd;
    property infAdProd: string read FinfAdProd write FinfAdProd;
  end;

  TDetCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDetCollectionItem;
    procedure SetItem(Index: Integer; Value: TDetCollectionItem);
  public
    function Add: TDetCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDetCollectionItem;
    property Items[Index: Integer]: TDetCollectionItem read GetItem write SetItem; default;
  end;

  Ttotal = class(TObject)
  private
    FvDC: Double;
  public
    property vDC: Double read FvDC write FvDC;
  end;

  Ttransp = class(TObject)
  private
    FmodTrans: TModTrans;
    FCNPJTrans: string;
  public
    property modTrans: TModTrans read FmodTrans write FmodTrans;
    property CNPJTrans: string read FCNPJTrans write FCNPJTrans;
  end;

  TobsContCollectionItem = class(TObject)
  private
    FxCampo: string;
    FxTexto: string;
  public
    property xCampo: string read FxCampo write FxCampo;
    property xTexto: string read FxTexto write FxTexto;
  end;

  TobsContCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TobsContCollectionItem;
    procedure SetItem(Index: Integer; Value: TobsContCollectionItem);
  public
    function Add: TobsContCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TobsContCollectionItem;
    property Items[Index: Integer]: TobsContCollectionItem read GetItem write SetItem; default;
  end;

  TobsMarketplaceCollectionItem = class(TObject)
  private
    FxCampo: string;
    FxTexto: string;
  public
    property xCampo: string read FxCampo write FxCampo;
    property xTexto: string read FxTexto write FxTexto;
  end;

  TobsMarketplaceCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TobsMarketplaceCollectionItem;
    procedure SetItem(Index: Integer; Value: TobsMarketplaceCollectionItem);
  public
    function Add: TobsMarketplaceCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TobsMarketplaceCollectionItem;
    property Items[Index: Integer]: TobsMarketplaceCollectionItem read GetItem write SetItem; default;
  end;

  TinfAdic = class(TObject)
  private
    FinfAdFisco: string;
    FinfCpl: string;
    FinfAdMarketplace: string;
    FinfAdTransp: string;
  public
    property infAdFisco: string read FinfAdFisco write FinfAdFisco;
    property infCpl: string read FinfCpl write FinfCpl;
    property infAdMarketplace: string read FinfAdMarketplace write FinfAdMarketplace;
    property infAdTransp: string read FinfAdTransp write FinfAdTransp;
  end;

  TinfDec = class(TObject)
  private
    FxObs1: string;
    FxObs2: string;
  public
    property xObs1: string read FxObs1 write FxObs1;
    property xObs2: string read FxObs2 write FxObs2;
  end;

  TinfSolicDCe = class(TObject)
  private
    FxSolic: string;
  public
    property xSolic: string read FxSolic write FxSolic;
  end;

  TinfDCeSupl = class(TObject)
  private
    FqrCode: string;
    FurlChave: string;
  public
    property qrCode: string read FqrCode write FqrCode;
    property urlChave: string read FurlChave write FurlChave;
  end;

  TDCe = class(TObject)
  private
    FinfDCe: TinfDCe;
    FIde: TIde;
    Femit: Temit;
    FFisco: TFisco;
    FMarketplace: TMarketplace;
    FTransportadora: TTransportadora;
    FEmpEmisProp: TEmpEmisProp;
    Fdest: Tdest;
    FautXML: TautXMLCollection;
    Fdet: TDetCollection;
    Ftotal: Ttotal;
    Ftransp: Ttransp;
    FinfAdic: TinfAdic;
    FobsCont: TobsContCollection;
    FobsMarketplace: TobsMarketplaceCollection;
    FinfDec: TinfDec;
    FinfSolicDCe: TinfSolicDCe;
    FinfDCeSupl: TinfDCeSupl;

    FProcDCe: TProcDFe;
    FSignature: TSignature;

    procedure SetautXML(const Value: TautXMLCollection);
    procedure Setdet(Value: TDetCollection);
    procedure SetobsCont(Value: TobsContCollection);
    procedure SetobsMarketplace(Value: TobsMarketplaceCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property infDCe: TinfDCe  read FinfDCe write FinfDCe;
    property Ide: TIde read FIde write FIde;
    property emit: Temit read Femit write Femit;
    property Fisco: TFisco read FFisco write FFisco;
    property Marketplace: TMarketplace read FMarketplace write FMarketplace;
    property Transportadora: TTransportadora read FTransportadora write FTransportadora;
    property EmpEmisProp: TEmpEmisProp read FEmpEmisProp write FEmpEmisProp;
    property dest: TDest read Fdest write Fdest;
    property autXML: TautXMLCollection read FautXML write SetautXML;
    property det: TDetCollection read Fdet write Setdet;
    property total: Ttotal read Ftotal write Ftotal;
    property transp: Ttransp read Ftransp write Ftransp;
    property infAdic: TinfAdic read FinfAdic write FinfAdic;
    property obsCont: TobsContCollection read FobsCont write SetobsCont;
    property obsMarketplace: TobsMarketplaceCollection read FobsMarketplace write SetobsMarketplace;
    property infDec: TinfDec read FinfDec write FinfDec;
    property infSolicDCe: TinfSolicDCe read FinfSolicDCe write FinfSolicDCe;
    property infDCeSupl: TinfDCeSupl read FinfDCeSupl write FinfDCeSupl;
    property procDCe: TProcDFe read FProcDCe write FProcDCe;
    property signature: Tsignature read Fsignature write Fsignature;
  end;

implementation

uses
  ACBrUtil.Base;

{ TDCe }

constructor TDCe.Create;
begin
  inherited Create;

  FinfDCe := TInfDCe.Create;
  Fide := Tide.Create;
  Femit := Temit.Create;

  FFisco := TFisco.Create;
  FMarketplace := TMarketplace.Create;
  FEmpEmisProp := TEmpEmisProp.Create;
  FTransportadora := TTransportadora.Create;
  Fdest := Tdest.Create;
  FautXML := TautXMLCollection.Create;
  FDet := TDetCollection.Create;
  Ftotal := Ttotal.Create;
  Ftransp := Ttransp.Create;
  FinfAdic := TinfAdic.Create;
  FinfDec := TinfDec.Create;
  FinfSolicDCe := TinfSolicDCe.Create;
  FinfDCeSupl := TinfDCeSupl.Create;
  FobsCont := TobsContCollection.Create;
  FobsMarketplace := TobsMarketplaceCollection.Create;
  FProcDCe := TProcDFe.Create('', '', '');
  Fsignature := Tsignature.create;
end;

destructor TDCe.Destroy;
begin
  FinfDCe.Free;
  Fide.Free;
  Femit.Free;
  FFisco.Free;
  FMarketplace.Free;
  FTransportadora.Free;
  FEmpEmisProp.Free;
  Fdest.Free;
  FautXML.Free;
  FDet.Free;
  Ftotal.Free;
  Ftransp.Free;
  FinfAdic.Free;
  FinfDec.Free;
  FinfSolicDCe.Free;
  FinfDCeSupl.Free;
  FobsCont.Free;
  FobsMarketplace.Free;
  FProcDCe.Free;
  Fsignature.Free;

  inherited;
end;

procedure TDCe.SetautXML(const Value: TautXMLCollection);
begin
  FautXML := Value;
end;

procedure TDCe.Setdet(Value: TDetCollection);
begin
  Fdet.Assign(Value);
end;

procedure TDCe.SetobsCont(Value: TobsContCollection);
begin
  FobsCont.Assign(Value);
end;

procedure TDCe.SetobsMarketplace(Value: TobsMarketplaceCollection);
begin
  FobsMarketplace.Assign(Value);
end;

{ TIde }

constructor TIde.Create;
begin
  inherited Create;

end;

destructor TIde.Destroy;
begin

  inherited;
end;

{ Temit }

constructor Temit.Create;
begin
  inherited Create;

  FenderEmit := Tendereco.Create;
end;

destructor Temit.Destroy;
begin
  FenderEmit.Free;

  inherited;
end;

{ TautXMLCollection }

function TautXMLCollection.Add: TautXMLCollectionItem;
begin
  Result := Self.New;
end;

function TautXMLCollection.GetItem(Index: Integer): TautXMLCollectionItem;
begin
  Result := TautXMLCollectionItem(inherited Items[Index]);
end;

procedure TautXMLCollection.SetItem(Index: Integer;
  Value: TautXMLCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TautXMLCollection.New: TautXMLCollectionItem;
begin
  Result := TautXMLCollectionItem.Create;
  Self.Add(Result);
end;

{ Tdest }

constructor Tdest.Create;
begin
  inherited Create;

  FenderDest := Tendereco.Create;
end;

destructor Tdest.Destroy;
begin
  FenderDest.Free;

  inherited;
end;

{ TDetCollection }

function TDetCollection.Add: TDetCollectionItem;
begin
  Result := Self.New;
end;

function TDetCollection.GetItem(Index: Integer): TDetCollectionItem;
begin
  Result := TDetCollectionItem(inherited Items[Index]);
end;

procedure TDetCollection.SetItem(Index: Integer; Value: TDetCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDetCollection.New: TDetCollectionItem;
begin
  Result := TDetCollectionItem.Create;
  Self.Add(Result);
end;

{ TDetCollectionItem }

constructor TDetCollectionItem.Create;
begin
  inherited Create;

  FProd := TProd.Create();
end;

destructor TDetCollectionItem.Destroy;
begin
  FProd.Free;

  inherited;
end;

{ TobsContCollection }

function TobsContCollection.Add: TobsContCollectionItem;
begin
  Result := Self.New;
end;

function TobsContCollection.GetItem(Index: Integer): TobsContCollectionItem;
begin
  Result := TobsContCollectionItem(inherited Items[Index]);
end;

procedure TobsContCollection.SetItem(Index: Integer; Value: TobsContCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TobsContCollection.New: TobsContCollectionItem;
begin
  Result := TobsContCollectionItem.Create;
  Self.Add(Result);
end;

{ TobsContCollection }

function TobsMarketplaceCollection.Add: TobsMarketplaceCollectionItem;
begin
  Result := Self.New;
end;

function TobsMarketplaceCollection.GetItem(Index: Integer): TobsMarketplaceCollectionItem;
begin
  Result := TobsMarketplaceCollectionItem(inherited Items[Index]);
end;

procedure TobsMarketplaceCollection.SetItem(Index: Integer; Value: TobsMarketplaceCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TobsMarketplaceCollection.New: TobsMarketplaceCollectionItem;
begin
  Result := TobsMarketplaceCollectionItem.Create;
  Self.Add(Result);
end;

end.

