{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Leivio Ramos de Fontenele                       }
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

unit pcnReinfR2020;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pcnGerador,
  ACBrUtil.Base, ACBrUtil.DateTime, ACBrUtil.FilesIO,
  ACBrDFeConsts,
  pcnCommonReinf, pcnConversaoReinf, pcnGeradorReinf;

type
  TR2020Collection = class;
  TR2020CollectionItem = class;
  TevtServPrest = class;

  {Classes específicas deste evento}
  TinfoServPrest = class;
  TideEstabPrest = class;
  TideTomador = class;
  TnfsCollection = class;
  TnfsCollectionItem = class;
  TinfoTpServCollection = class;
  TinfoTpServCollectionItem = class;
  TinfoProcRetPrCollection = class;
  TinfoProcRetPrCollectionItem = class;
  TinfoProcRetAdCollection = class;
  TinfoProcRetAdCollectionItem = class;

  TR2020Collection = class(TReinfCollection)
  private
    function GetItem(Index: Integer): TR2020CollectionItem;
    procedure SetItem(Index: Integer; Value: TR2020CollectionItem);
  public
    function Add: TR2020CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TR2020CollectionItem;

    property Items[Index: Integer]: TR2020CollectionItem read GetItem write SetItem; default;
  end;

  TR2020CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtServPrest: TevtServPrest;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtServPrest: TevtServPrest read FevtServPrest write FevtServPrest;
  end;

  TevtServPrest = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;
    FinfoServPrest: TinfoServPrest;

    {Geradores específicos desta classe}
    procedure GerarideEstabPrest;
    procedure GerarideTomador;
    procedure GerarNFs(Lista: TnfsCollection);
    procedure GerarinfoTpServ(Lista: TinfoTpServCollection);
    procedure GerarinfoProcRetPr(Lista: TinfoProcRetPrCollection);
    procedure GerarinfoProcRetAd(Lista: TinfoProcRetAdCollection);
  public
    constructor Create(AACBrReinf: TObject); override;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
    property infoServPrest: TinfoServPrest read FinfoServPrest write FinfoServPrest;
  end;

  { TinfoServPrest }
  TinfoServPrest = class(TObject)
  private
    FideEstabPrest: TideEstabPrest;
  public
    constructor Create;
    destructor Destroy; override;

    property ideEstabPrest: TideEstabPrest read FideEstabPrest write FideEstabPrest;
  end;

  { TideEstabPrest }
  TideEstabPrest = class(TObject)
  private
    FtpInscEstabPrest: TtpInsc;
    FnrInscEstabPrest: string;
    FideTomador: TideTomador;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInscEstabPrest: TtpInsc read FtpInscEstabPrest write FtpInscEstabPrest default tiCNPJ;
    property nrInscEstabPrest: string read FnrInscEstabPrest write FnrInscEstabPrest;
    property ideTomador: TideTomador read FideTomador write FideTomador;
  end;

  { TideTomador }
  TideTomador = class(TObject)
  private
    FtpInscTomador: TtpInsc;
    FnrInscTomador: string;
    FindObra: TpindObra;
    FvlrTotalBruto: Double;
    FvlrTotalRetPrinc: Double;
    FvlrTotalNRetPrinc: Double;
    FvlrTotalRetAdic: Double;
    FvlrTotalNRetAdic: Double;
    FvlrTotalBaseRet: Double;
    Fnfs: TnfsCollection;
    FinfoProcRetPr: TinfoProcRetPrCollection;
    FinfoProcRetAd: TinfoProcRetAdCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInscTomador: TtpInsc read FtpInscTomador write FtpInscTomador default tiCNPJ;
    property nrInscTomador: string read FnrInscTomador write FnrInscTomador;
    property indObra: TpindObra read FindObra write FindObra;
    property vlrTotalBruto: Double read FvlrTotalBruto write FvlrTotalBruto;
    property vlrTotalBaseRet: Double read FvlrTotalBaseRet write FvlrTotalBaseRet;
    property vlrTotalRetPrinc: Double read FvlrTotalRetPrinc write FvlrTotalRetPrinc;
    property vlrTotalRetAdic: Double read FvlrTotalRetAdic write FvlrTotalRetAdic;
    property vlrTotalNRetPrinc: Double read FvlrTotalNRetPrinc write FvlrTotalNRetPrinc;
    property vlrTotalNRetAdic: Double read FvlrTotalNRetAdic write FvlrTotalNRetAdic;
    property nfs: TnfsCollection read Fnfs write Fnfs;
    property infoProcRetPr: TinfoProcRetPrCollection read FinfoProcRetPr write FinfoProcRetPr;
    property infoProcRetAd: TinfoProcRetAdCollection read FinfoProcRetAd write FinfoProcRetAd;
  end;

  TnfsCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TnfsCollectionItem;
    procedure SetItem(Index: Integer; Value: TnfsCollectionItem);
  public
    function Add: TnfsCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TnfsCollectionItem;

    property Items[Index: Integer]: TnfsCollectionItem read GetItem write SetItem; default;
  end;

  TnfsCollectionItem = class(TObject)
  private
    Fserie: string;
    FnumDocto: string;
    FdtEmissaoNF: TDateTime;
    FvlrBruto: Double;
    Fobs: string;
    FinfoTpServ: TinfoTpServCollection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property serie: string read Fserie write Fserie;
    property numDocto: string read FnumDocto write FnumDocto;
    property dtEmissaoNF: TDateTime read FdtEmissaoNF write FdtEmissaoNF;
    property vlrBruto: Double read FvlrBruto write FvlrBruto;
    property obs: string read Fobs write Fobs;
    property infoTpServ: TinfoTpServCollection read FinfoTpServ write FinfoTpServ;
  end;

  TinfoTpServCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoTpServCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoTpServCollectionItem);
  public
    function Add: TinfoTpServCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoTpServCollectionItem;

    property Items[Index: Integer]: TinfoTpServCollectionItem read GetItem write SetItem; default;
  end;

  TinfoTpServCollectionItem = class(TObject)
  private
    FtpServico: string;
    FvlrBaseRet: Double;
    FvlrRetencao: Double;
    FvlrRetSub: Double;
    FvlrNRetPrinc: Double;
    FvlrServicos15: Double;
    FvlrServicos20: Double;
    FvlrServicos25: Double;
    FvlrAdicional: Double;
    FvlrNRetAdic: Double;
  public
    property tpServico: string read FtpServico write FtpServico;
    property vlrBaseRet: Double read FvlrBaseRet write FvlrBaseRet;
    property vlrRetencao: Double read FvlrRetencao write FvlrRetencao;
    property vlrRetSub: Double read FvlrRetSub write FvlrRetSub;
    property vlrNRetPrinc: Double read FvlrNRetPrinc write FvlrNRetPrinc;
    property vlrServicos15: Double read FvlrServicos15 write FvlrServicos15;
    property vlrServicos20: Double read FvlrServicos20 write FvlrServicos20;
    property vlrServicos25: Double read FvlrServicos25 write FvlrServicos25;
    property vlrAdicional: Double read FvlrAdicional write FvlrAdicional;
    property vlrNRetAdic: Double read FvlrNRetAdic write FvlrNRetAdic;
  end;

  TinfoProcRetPrCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoProcRetPrCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoProcRetPrCollectionItem);
  public
    function Add: TinfoProcRetPrCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoProcRetPrCollectionItem;

    property Items[Index: Integer]: TinfoProcRetPrCollectionItem read GetItem write SetItem; default;
  end;

  TinfoProcRetPrCollectionItem = class(TObject)
  private
    FtpProcRetPrinc: TtpProc;
    FnrProcRetPrinc: string;
    FcodSuspPrinc: integer;
    FvalorPrinc: Double;
  public
    property tpProcRetPrinc: TtpProc read  FtpProcRetPrinc write FtpProcRetPrinc;
    property nrProcRetPrinc: string read FnrProcRetPrinc write FnrProcRetPrinc;
    property codSuspPrinc: integer read FcodSuspPrinc write FcodSuspPrinc;
    property valorPrinc: Double read FvalorPrinc write FvalorPrinc;
  end;

  TinfoProcRetAdCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfoProcRetAdCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoProcRetAdCollectionItem);
  public
    function Add: TinfoProcRetAdCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TinfoProcRetAdCollectionItem;

    property Items[Index: Integer]: TinfoProcRetAdCollectionItem read GetItem write SetItem; default;
  end;

  TinfoProcRetAdCollectionItem = class(TObject)
  private
    FtpProcRetAdic: TtpProc;
    FnrProcRetAdic: string;
    FcodSuspAdic: integer;
    FvalorAdic: Double;
  public
    property tpProcRetAdic: TtpProc read  FtpProcRetAdic write FtpProcRetAdic;
    property nrProcRetAdic: string read FnrProcRetAdic write FnrProcRetAdic;
    property codSuspAdic: integer read FcodSuspAdic write FcodSuspAdic;
    property valorAdic: Double read FvalorAdic write FvalorAdic;
  end;

implementation

uses
  IniFiles,
  ACBrReinf, ACBrDFeUtil;

{ TR2020Collection }

function TR2020Collection.Add: TR2020CollectionItem;
begin
  Result := Self.New;
end;

function TR2020Collection.GetItem(Index: Integer): TR2020CollectionItem;
begin
  Result := TR2020CollectionItem(inherited Items[Index]);
end;

function TR2020Collection.New: TR2020CollectionItem;
begin
  Result := TR2020CollectionItem.Create(FACBrReinf);
  Self.Add(Result);
end;

procedure TR2020Collection.SetItem(Index: Integer; Value: TR2020CollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TR2020CollectionItem }

constructor TR2020CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;

  FTipoEvento   := teR2020;
  FevtServPrest := TevtServPrest.Create(AOwner);
end;

destructor TR2020CollectionItem.Destroy;
begin
  inherited;

  FevtServPrest.Free;
end;

{ TevtServPrest }

constructor TevtServPrest.Create(AACBrReinf: TObject);
begin
  inherited Create(AACBrReinf);

  FideContri     := TideContri.Create;
  FIdeEvento     := TIdeEvento2.Create;
  FinfoServPrest := TinfoServPrest.Create;
end;

destructor TevtServPrest.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FinfoServPrest.Free;

  inherited;
end;

{ TinfoServPrest }

constructor TinfoServPrest.Create;
begin
  FideEstabPrest := TideEstabPrest.Create;
end;

destructor TinfoServPrest.Destroy;
begin
  FideEstabPrest.Free;

  inherited;
end;

{ TideEstabPrest }

constructor TideEstabPrest.Create;
begin
  FideTomador := TideTomador.Create;
end;

destructor TideEstabPrest.Destroy;
begin
  FideTomador.Free;

  inherited;
end;

{ TideTomador }

constructor TideTomador.Create;
begin
  Fnfs           := TnfsCollection.Create;
  FinfoProcRetPr := TinfoProcRetPrCollection.Create;
  FinfoProcRetAd := TinfoProcRetAdCollection.Create;
end;

destructor TideTomador.Destroy;
begin
  Fnfs.Free;
  FinfoProcRetPr.Free;
  FinfoProcRetAd.Free;

  inherited;
end;

{ TnfsCollection }

function TnfsCollection.Add: TnfsCollectionItem;
begin
  Result := Self.New;
end;

function TnfsCollection.GetItem(
  Index: Integer): TnfsCollectionItem;
begin
  Result := TnfsCollectionItem(inherited Items[Index]);
end;

function TnfsCollection.New: TnfsCollectionItem;
begin
  Result := TnfsCollectionItem.Create;
  Self.Add(Result);
end;

procedure TnfsCollection.SetItem(Index: Integer;
  Value: TnfsCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TnfsCollectionItem }

constructor TnfsCollectionItem.Create;
begin
  FinfoTpServ := TinfoTpServCollection.Create;
end;

destructor TnfsCollectionItem.Destroy;
begin
  FinfoTpServ.Free;

  inherited;
end;

{ TinfoTpServCollection }

function TinfoTpServCollection.Add: TinfoTpServCollectionItem;
begin
  Result := Self.New;
end;

function TinfoTpServCollection.GetItem(
  Index: Integer): TinfoTpServCollectionItem;
begin
  Result := TinfoTpServCollectionItem(inherited Items[Index]);
end;

function TinfoTpServCollection.New: TinfoTpServCollectionItem;
begin
  Result := TinfoTpServCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoTpServCollection.SetItem(Index: Integer;
  Value: TinfoTpServCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoProcRetPrCollection }

function TinfoProcRetPrCollection.Add: TinfoProcRetPrCollectionItem;
begin
  Result := Self.New;
end;

function TinfoProcRetPrCollection.GetItem(
  Index: Integer): TinfoProcRetPrCollectionItem;
begin
  Result := TinfoProcRetPrCollectionItem(inherited Items[Index]);
end;

function TinfoProcRetPrCollection.New: TinfoProcRetPrCollectionItem;
begin
  Result := TinfoProcRetPrCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoProcRetPrCollection.SetItem(Index: Integer;
  Value: TinfoProcRetPrCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfoProcRetAdCollection }

function TinfoProcRetAdCollection.Add: TinfoProcRetAdCollectionItem;
begin
  Result := Self.New;
end;

function TinfoProcRetAdCollection.GetItem(
  Index: Integer): TinfoProcRetAdCollectionItem;
begin
  Result := TinfoProcRetAdCollectionItem(inherited Items[Index]);
end;

function TinfoProcRetAdCollection.New: TinfoProcRetAdCollectionItem;
begin
  Result := TinfoProcRetAdCollectionItem.Create;
  Self.Add(Result);
end;

procedure TinfoProcRetAdCollection.SetItem(Index: Integer;
  Value: TinfoProcRetAdCollectionItem);
begin
  inherited Items[Index] := Value;
end;

procedure TevtServPrest.GerarideEstabPrest;
begin
  Gerador.wGrupo('ideEstabPrest');

  Gerador.wCampo(tcStr, '', 'tpInscEstabPrest', 1,  1, 1, TpInscricaoToStr(Self.FinfoServPrest.ideEstabPrest.tpInscEstabPrest));
  Gerador.wCampo(tcStr, '', 'nrInscEstabPrest', 1, 14, 1, Self.FinfoServPrest.ideEstabPrest.nrInscEstabPrest);

  GerarideTomador;

  Gerador.wGrupo('/ideEstabPrest');
end;

procedure TevtServPrest.GerarideTomador;
begin
  Gerador.wGrupo('ideTomador');

  with Self.FinfoServPrest.ideEstabPrest do
  begin
    Gerador.wCampo(tcStr, '', 'tpInscTomador',     1,  1, 1, TpInscricaoToStr(ideTomador.tpInscTomador));
    Gerador.wCampo(tcStr, '', 'nrInscTomador',     1, 14, 1, ideTomador.nrInscTomador);
    Gerador.wCampo(tcStr, '', 'indObra',           1,  1, 1, indObraToStr(ideTomador.indObra));
    Gerador.wCampo(tcDe2, '', 'vlrTotalBruto',     1, 14, 1, ideTomador.vlrTotalBruto);
    Gerador.wCampo(tcDe2, '', 'vlrTotalBaseRet',   1, 14, 1, ideTomador.vlrTotalBaseRet);
    Gerador.wCampo(tcDe2, '', 'vlrTotalRetPrinc',  1, 14, 1, ideTomador.vlrTotalRetPrinc);
    Gerador.wCampo(tcDe2, '', 'vlrTotalRetAdic',   1, 14, 0, ideTomador.vlrTotalRetAdic);
    Gerador.wCampo(tcDe2, '', 'vlrTotalNRetPrinc', 1, 14, 0, ideTomador.vlrTotalNRetPrinc);
    Gerador.wCampo(tcDe2, '', 'vlrTotalNRetAdic',  1, 14, 0, ideTomador.vlrTotalNRetAdic);

    GerarNFs(ideTomador.nfs);
    GerarinfoProcRetPr(ideTomador.infoProcRetPr);
    GerarinfoProcRetAd(ideTomador.infoProcRetAd);
  end;

  Gerador.wGrupo('/ideTomador');
end;

procedure TevtServPrest.GerarNFs(Lista: TnfsCollection);
var
  item: TnfsCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('nfs');

    Gerador.wCampo(tcStr, '', 'serie',        1,   5, 1, item.serie);
    Gerador.wCampo(tcStr, '', 'numDocto',     1,  15, 1, item.numDocto);
    Gerador.wCampo(tcDat, '', 'dtEmissaoNF', 10,  10, 1, item.dtEmissaoNF);
    Gerador.wCampo(tcDe2, '', 'vlrBruto',     1,  14, 1, item.vlrBruto);
    Gerador.wCampo(tcStr, '', 'obs',          1, 255, 0, item.obs);

    GerarinfoTpServ(item.infoTpServ);

    Gerador.wGrupo('/nfs');
  end;

  if Lista.Count > 999 then
    Gerador.wAlerta('', 'nfs', 'Lista de Notas Fiscais', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TevtServPrest.GerarinfoTpServ(Lista: TinfoTpServCollection);
var
  item: TinfoTpServCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoTpServ');

    Gerador.wCampo(tcStr, '', 'tpServico',     1,  9, 1, item.tpServico);
    Gerador.wCampo(tcDe2, '', 'vlrBaseRet',    1, 14, 1, item.vlrBaseRet);
    Gerador.wCampo(tcDe2, '', 'vlrRetencao',   1, 14, 1, item.vlrRetencao);
    Gerador.wCampo(tcDe2, '', 'vlrRetSub',     1, 14, 0, item.vlrRetSub);
    Gerador.wCampo(tcDe2, '', 'vlrNRetPrinc',  1, 14, 0, item.vlrNRetPrinc);
    Gerador.wCampo(tcDe2, '', 'vlrServicos15', 1, 14, 0, item.vlrServicos15);
    Gerador.wCampo(tcDe2, '', 'vlrServicos20', 1, 14, 0, item.vlrServicos20);
    Gerador.wCampo(tcDe2, '', 'vlrServicos25', 1, 14, 0, item.vlrServicos25);
    Gerador.wCampo(tcDe2, '', 'vlrAdicional',  1, 14, 0, item.vlrAdicional);
    Gerador.wCampo(tcDe2, '', 'vlrNRetAdic',   1, 14, 0, item.vlrNRetAdic);

    Gerador.wGrupo('/infoTpServ');
  end;

  if Lista.Count > 9 then
    Gerador.wAlerta('', 'infoTpServ', 'Lista de Informações sobre os tipos de serviços', ERR_MSG_MAIOR_MAXIMO + '9');
end;

procedure TevtServPrest.GerarinfoProcRetPr(Lista: TinfoProcRetPrCollection);
var
  item: TinfoProcRetPrCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoProcRetPr');

    Gerador.wCampo(tcStr, '', 'tpProcRetPrinc', 1,  1, 1, TpProcToStr(item.tpProcRetPrinc));
    Gerador.wCampo(tcStr, '', 'nrProcRetPrinc', 1, 21, 1, item.nrProcRetPrinc);
    Gerador.wCampo(tcInt, '', 'codSuspPrinc',   1, 14, 0, item.codSuspPrinc);
    Gerador.wCampo(tcDe2, '', 'valorPrinc',     1, 14, 1, item.valorPrinc);

    Gerador.wGrupo('/infoProcRetPr');
  end;

  if Lista.Count > 50 then
    Gerador.wAlerta('', 'infoProcRetPr', 'Lista de Processos', ERR_MSG_MAIOR_MAXIMO + '50');
end;

procedure TevtServPrest.GerarinfoProcRetAd(Lista: TinfoProcRetAdCollection);
var
  item: TinfoProcRetAdCollectionItem;
  i: Integer;
begin
  for i := 0 to Lista.Count - 1 do
  begin
    Item := Lista.Items[i];

    Gerador.wGrupo('infoProcRetAd');

    Gerador.wCampo(tcStr, '', 'tpProcRetAdic', 1,  1, 1, TpProcToStr(item.tpProcRetAdic));
    Gerador.wCampo(tcStr, '', 'nrProcRetAdic', 1, 21, 1, item.nrProcRetAdic);
    Gerador.wCampo(tcInt, '', 'codSuspAdic',   1, 14, 0, item.codSuspAdic);
    Gerador.wCampo(tcDe2, '', 'valorAdic',     1, 14, 1, item.valorAdic);

    Gerador.wGrupo('/infoProcRetAd');
  end;

  if Lista.Count > 50 then
    Gerador.wAlerta('', 'infoProcRetAd', 'Lista de Processos', ERR_MSG_MAIOR_MAXIMO + '50');
end;

function TevtServPrest.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial, self.ideContri.TpInsc);

    GerarCabecalho('evtPrestadorServicos');
    Gerador.wGrupo('evtServPrest id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);
    GerarideContri(Self.ideContri);

    Gerador.wGrupo('infoServPrest');

    GerarideEstabPrest;

    Gerador.wGrupo('/infoServPrest');
    Gerador.wGrupo('/evtServPrest');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtServPrest');

//    Validar(schevtPrestadorServicos);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtServPrest.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtServPrest';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif := StrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.perApur  := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.ProcEmi  := StrToProcEmiReinf(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc  := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideContri';
      ideContri.OrgaoPublico := (TACBrReinf(FACBrReinf).Configuracoes.Geral.TipoContribuinte = tcOrgaoPublico);
      ideContri.TpInsc       := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideContri.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideEstabPrest';
      infoServPrest.ideEstabPrest.tpInscEstabPrest := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscEstabPrest', '1'));
      infoServPrest.ideEstabPrest.nrInscEstabPrest := INIRec.ReadString(sSecao, 'nrInscEstabPrest', EmptyStr);

      with infoServPrest.ideEstabPrest do
      begin
        sSecao := 'ideTomador';
        ideTomador.tpInscTomador     := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscTomador', '1'));
        ideTomador.nrInscTomador     := INIRec.ReadString(sSecao, 'nrInscTomador', EmptyStr);
        ideTomador.indObra           := StrToindObra(Ok, INIRec.ReadString(sSecao, 'indObra', '1'));
        ideTomador.vlrTotalBruto     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalBruto', ''), 0);
        ideTomador.vlrTotalBaseRet   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalBaseRet', ''), 0);
        ideTomador.vlrTotalRetPrinc  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalRetPrinc', ''), 0);
        ideTomador.vlrTotalRetAdic   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalRetAdic', ''), 0);
        ideTomador.vlrTotalNRetPrinc := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalNRetPrinc', ''), 0);
        ideTomador.vlrTotalNRetAdic  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalNRetAdic', ''), 0);

        I := 1;
        while true do
        begin
          // de 01 até 999
          sSecao := 'nfs' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'serie', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with ideTomador.nfs.New do
          begin
            serie       := sFim;
            numDocto    := INIRec.ReadString(sSecao, 'numDocto', '');
            dtEmissaoNF := StringToDateTime(INIRec.ReadString(sSecao, 'dtEmissaoNF', '0'));
            vlrBruto    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBruto', ''), 0);
            obs         := INIRec.ReadString(sSecao, 'obs', '');

            J := 1;
            while true do
            begin
              // de 1 até 9
              sSecao := 'infoTpServ' + IntToStrZero(I, 3) + IntToStrZero(J, 1);
              sFim   := INIRec.ReadString(sSecao, 'tpServico', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with infoTpServ.New do
              begin
                tpServico     := sFim;
                vlrBaseRet    := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrBaseRet', ''), 0);
                vlrRetencao   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRetencao', ''), 0);
                vlrRetSub     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrRetSub', ''), 0);
                vlrNRetPrinc  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrNRetPrinc', ''), 0);
                vlrServicos15 := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrServicos15', ''), 0);
                vlrServicos20 := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrServicos20', ''), 0);
                vlrServicos25 := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrServicos25', ''), 0);
                vlrAdicional  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrAdicional', ''), 0);
                vlrNRetAdic   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrNRetAdic', ''), 0);
              end;

              Inc(J);
            end;
          end;

          Inc(I);
        end;

        I := 1;
        while true do
        begin
          // de 00 até 50
          sSecao := 'infoProcRetPr' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'tpProcRetPrinc', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with ideTomador.infoProcRetPr.New do
          begin
            tpProcRetPrinc := StrToTpProc(Ok, sFim);
            nrProcRetPrinc := INIRec.ReadString(sSecao, 'nrProcRetPrinc', '');
            codSuspPrinc   := INIRec.ReadInteger(sSecao, 'codSuspPrinc', 0);
            valorPrinc     := StringToFloatDef(INIRec.ReadString(sSecao, 'valorPrinc', ''), 0);
          end;

          Inc(I);
        end;

        I := 1;
        while true do
        begin
          // de 00 até 50
          sSecao := 'infoProcRetAd' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'tpProcRetAdic', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with ideTomador.infoProcRetAd.New do
          begin
            tpProcRetAdic := StrToTpProc(Ok, sFim);
            nrProcRetAdic := INIRec.ReadString(sSecao, 'nrProcRetAdic', '');
            codSuspAdic   := INIRec.ReadInteger(sSecao, 'codSuspAdic', 0);
            valorAdic     := StringToFloatDef(INIRec.ReadString(sSecao, 'valorAdic', ''), 0);
          end;

          Inc(I);
        end;
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
