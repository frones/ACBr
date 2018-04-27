{******************************************************************************}
{ Projeto: Componente ACBrReinf                                                }
{  Biblioteca multiplataforma de componentes Delphi para envio de eventos do   }
{ Reinf                                                                        }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 04/12/2017: Renato Rubinho
|*  - Implementados registros que faltavam e isoladas as respectivas classes
*******************************************************************************}

{$I ACBr.inc}

unit pcnReinfR2010;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, ACBrUtil,
  pcnCommonReinf, pcnConversaoReinf, pcnGeradorReinf;

type
  TR2010Collection = class;
  TR2010CollectionItem = class;
  TevtServTom = class;

  {Classes específicas deste evento}
  TinfoServTom = class;
  TideEstabObra = class;
  TidePrestServ = class;
  TnfsCollection = class;
  TnfsCollectionItem = class;
  TinfoTpServCollection = class;
  TinfoTpServCollectionItem = class;
  TinfoProcRetPrCollection = class;
  TinfoProcRetPrCollectionItem = class;
  TinfoProcRetAdCollection = class;
  TinfoProcRetAdCollectionItem = class;

  TR2010Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TR2010CollectionItem;
    procedure SetItem(Index: Integer; Value: TR2010CollectionItem);
  public
    function Add: TR2010CollectionItem;
    property Items[Index: Integer]: TR2010CollectionItem read GetItem write SetItem; default;
  end;

  TR2010CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FevtServTom: TevtServTom;
    procedure setevtServTom(const Value: TevtServTom);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtServTom: TevtServTom read FevtServTom write setevtServTom;
  end;

  TevtServTom = class(TReinfEvento) //Classe do elemento principal do XML do evento!
  private
    FIdeEvento: TIdeEvento2;
    FideContri: TideContri;
    FACBrReinf: TObject;
    FinfoServTom: TinfoServTom;

    {Geradores específicos desta classe}
    procedure GerarideEstabObra;
    procedure GeraridePrestServ;
    procedure GerarNFs(Lista: TnfsCollection);
    procedure GerarinfoTpServ(Lista: TinfoTpServCollection);
    procedure GerarinfoProcRetPr(Lista: TinfoProcRetPrCollection);
    procedure GerarinfoProcRetAd(Lista: TinfoProcRetAdCollection);
  public
    constructor Create(AACBrReinf: TObject); overload;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideContri: TideContri read FideContri write FideContri;
    property infoServTom: TinfoServTom read FinfoServTom write FinfoServTom;
  end;

  { TinfoServTom }
  TinfoServTom = class(TPersistent)
  private
    FideEstabObra: TideEstabObra;
  public
    constructor Create;
    destructor Destroy; override;

    property ideEstabObra: TideEstabObra read FideEstabObra write FideEstabObra;
  end;

  { TideEstabObra }
  TideEstabObra = class(TPersistent)
  private
    FtpInscEstab: TtpInsc;
    FnrInscEstab: string;
    FindObra: TpindObra;
    FidePrestServ: TidePrestServ;
  public
    constructor Create;
    destructor Destroy; override;

    property tpInscEstab: TtpInsc read FtpInscEstab write FtpInscEstab default tiCNPJ;
    property nrInscEstab: string read FnrInscEstab write FnrInscEstab;
    property indObra: TpindObra read FindObra write FindObra;
    property idePrestServ: TidePrestServ read FidePrestServ write FidePrestServ;
  end;

  { TidePrestServ }
  TidePrestServ = class(TPersistent)
  private
    FcnpjPrestador: string;
    FvlrTotalBruto: Double;
    FvlrTotalRetPrinc: Double;
    FvlrTotalNRetPrinc: Double;
    FvlrTotalRetAdic: Double;
    FvlrTotalNRetAdic: Double;
    FvlrTotalBaseRet: Double;
    FindCPRB: TpindCPRB;
    Fnfs: TnfsCollection;
    FinfoProcRetPr: TinfoProcRetPrCollection;
    FinfoProcRetAd: TinfoProcRetAdCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property cnpjPrestador: string read FcnpjPrestador write FcnpjPrestador;
    property vlrTotalBruto: Double read FvlrTotalBruto write FvlrTotalBruto;
    property vlrTotalBaseRet: Double read FvlrTotalBaseRet write FvlrTotalBaseRet;
    property vlrTotalRetPrinc: Double read FvlrTotalRetPrinc write FvlrTotalRetPrinc;
    property vlrTotalRetAdic: Double read FvlrTotalRetAdic write FvlrTotalRetAdic;
    property vlrTotalNRetPrinc: Double read FvlrTotalNRetPrinc write FvlrTotalNRetPrinc;
    property vlrTotalNRetAdic: Double read FvlrTotalNRetAdic write FvlrTotalNRetAdic;
    property indCPRB: TpindCPRB read FindCPRB write FindCPRB;
    property nfs: TnfsCollection read Fnfs write Fnfs;
    property infoProcRetPr: TinfoProcRetPrCollection read FinfoProcRetPr write FinfoProcRetPr;
    property infoProcRetAd: TinfoProcRetAdCollection read FinfoProcRetAd write FinfoProcRetAd;
  end;

  TnfsCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TnfsCollectionItem;
    procedure SetItem(Index: Integer; Value: TnfsCollectionItem);
  public
    constructor create(AOwner: TidePrestServ);
    function Add: TnfsCollectionItem;
    property Items[Index: Integer]: TnfsCollectionItem read GetItem write SetItem; default;
  end;

  TnfsCollectionItem = class(TCollectionItem)
  private
    Fserie: string;
    FnumDocto: string;
    FdtEmissaoNF: TDateTime;
    FvlrBruto: Double;
    Fobs: string;
    FinfoTpServ: TinfoTpServCollection;
  public
    constructor create; reintroduce;
    destructor Destroy; override;

    property serie: string read Fserie write Fserie;
    property numDocto: string read FnumDocto write FnumDocto;
    property dtEmissaoNF: TDateTime read FdtEmissaoNF write FdtEmissaoNF;
    property vlrBruto: Double read FvlrBruto write FvlrBruto;
    property obs: string read Fobs write Fobs;
    property infoTpServ: TinfoTpServCollection read FinfoTpServ write FinfoTpServ;
  end;

  TinfoTpServCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfoTpServCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoTpServCollectionItem);
  public
    constructor create(); reintroduce;
    function Add: TinfoTpServCollectionItem;
    property Items[Index: Integer]: TinfoTpServCollectionItem read GetItem write SetItem; default;
  end;

  TinfoTpServCollectionItem = class(TCollectionItem)
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

  TinfoProcRetPrCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfoProcRetPrCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoProcRetPrCollectionItem);
  public
    constructor create(); reintroduce;
    function Add: TinfoProcRetPrCollectionItem;
    property Items[Index: Integer]: TinfoProcRetPrCollectionItem read GetItem write SetItem; default;
  end;

  TinfoProcRetPrCollectionItem = class(TCollectionItem)
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

  TinfoProcRetAdCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TinfoProcRetAdCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfoProcRetAdCollectionItem);
  public
    constructor create(); reintroduce;
    function Add: TinfoProcRetAdCollectionItem;
    property Items[Index: Integer]: TinfoProcRetAdCollectionItem read GetItem write SetItem; default;
  end;

  TinfoProcRetAdCollectionItem = class(TCollectionItem)
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

{ TR2010Collection }

function TR2010Collection.Add: TR2010CollectionItem;
begin
  Result := TR2010CollectionItem(inherited Add);
end;

function TR2010Collection.GetItem(Index: Integer): TR2010CollectionItem;
begin
  Result := TR2010CollectionItem(inherited GetItem(Index));
end;

procedure TR2010Collection.SetItem(Index: Integer; Value: TR2010CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TR2010CollectionItem }

procedure TR2010CollectionItem.AfterConstruction;
begin
  inherited;
  FTipoEvento := teR2010;
  FevtServTom := TevtServTom.Create(Collection.Owner);
end;

procedure TR2010CollectionItem.BeforeDestruction;
begin
  inherited;
  FevtServTom.Free;
end;

procedure TR2010CollectionItem.setevtServTom(const Value: TevtServTom);
begin
  FevtServTom.Assign(Value);
end;

{ TevtServTom }

constructor TevtServTom.Create(AACBrReinf: TObject);
begin
  inherited;

  FACBrReinf := AACBrReinf;

  FideContri   := TideContri.create;
  FIdeEvento   := TIdeEvento2.create;
  FinfoServTom := TinfoServTom.Create;
end;

destructor TevtServTom.Destroy;
begin
  FideContri.Free;
  FIdeEvento.Free;
  FinfoServTom.Free;

  inherited;
end;

{ TinfoServTom }

constructor TinfoServTom.Create;
begin
  FideEstabObra := TideEstabObra.Create;
end;

destructor TinfoServTom.Destroy;
begin
  FideEstabObra.Free;

  inherited;
end;

{ TideEstabObra }

constructor TideEstabObra.Create;
begin
  FidePrestServ := TidePrestServ.Create;
end;

destructor TideEstabObra.Destroy;
begin
  FidePrestServ.Free;

  inherited;
end;

{ TidePrestServ }

constructor TidePrestServ.Create;
begin
  Fnfs           := TnfsCollection.Create(Self);
  FinfoProcRetPr := TinfoProcRetPrCollection.Create;
  FinfoProcRetAd := TinfoProcRetAdCollection.create;
end;

destructor TidePrestServ.Destroy;
begin
  Fnfs.Free;
  FinfoProcRetPr.Free;
  FinfoProcRetAd.Free;

  inherited;
end;

{ TnfsCollection }

function TnfsCollection.Add: TnfsCollectionItem;
begin
  Result := TnfsCollectionItem(inherited add());
  Result.Create;
end;

constructor TnfsCollection.create(AOwner: TidePrestServ);
begin
  Inherited create(TnfsCollectionItem);
end;

function TnfsCollection.GetItem(
  Index: Integer): TnfsCollectionItem;
begin
  Result := TnfsCollectionItem(inherited GetItem(Index));
end;

procedure TnfsCollection.SetItem(Index: Integer;
  Value: TnfsCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TnfsCollectionItem }

constructor TnfsCollectionItem.create;
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
  Result := TinfoTpServCollectionItem(inherited add());
//  Result.Create;
end;

constructor TinfoTpServCollection.create();
begin
  Inherited create(TinfoTpServCollectionItem);
end;

function TinfoTpServCollection.GetItem(
  Index: Integer): TinfoTpServCollectionItem;
begin
  Result := TinfoTpServCollectionItem(inherited GetItem(Index));
end;

procedure TinfoTpServCollection.SetItem(Index: Integer;
  Value: TinfoTpServCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfoProcRetPrCollection }

function TinfoProcRetPrCollection.Add: TinfoProcRetPrCollectionItem;
begin
  Result := TinfoProcRetPrCollectionItem(inherited add());
//  Result.Create;
end;

constructor TinfoProcRetPrCollection.create;
begin
  Inherited create(TinfoProcRetPrCollectionItem);
end;

function TinfoProcRetPrCollection.GetItem(
  Index: Integer): TinfoProcRetPrCollectionItem;
begin
  Result := TinfoProcRetPrCollectionItem(inherited GetItem(Index));
end;

procedure TinfoProcRetPrCollection.SetItem(Index: Integer;
  Value: TinfoProcRetPrCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TinfoProcRetAdCollection }

function TinfoProcRetAdCollection.Add: TinfoProcRetAdCollectionItem;
begin
  Result := TinfoProcRetAdCollectionItem(inherited add());
//  Result.Create;
end;

constructor TinfoProcRetAdCollection.create;
begin
  Inherited create(TinfoProcRetAdCollectionItem);
end;

function TinfoProcRetAdCollection.GetItem(
  Index: Integer): TinfoProcRetAdCollectionItem;
begin
  Result := TinfoProcRetAdCollectionItem(inherited GetItem(Index));
end;

procedure TinfoProcRetAdCollection.SetItem(Index: Integer;
  Value: TinfoProcRetAdCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TevtServTom.GerarideEstabObra;
begin
  Gerador.wGrupo('ideEstabObra');

  Gerador.wCampo(tcStr, '', 'tpInscEstab', 1,  1, 1, TpInscricaoToStr(Self.FinfoServTom.ideEstabObra.tpInscEstab));
  Gerador.wCampo(tcStr, '', 'nrInscEstab', 1, 14, 1, Self.FinfoServTom.ideEstabObra.nrInscEstab);
  Gerador.wCampo(tcStr, '', 'indObra',     1,  1, 1, indObraToStr(Self.FinfoServTom.ideEstabObra.indObra));

  GeraridePrestServ;

  Gerador.wGrupo('/ideEstabObra');
end;

procedure TevtServTom.GeraridePrestServ;
begin
  Gerador.wGrupo('idePrestServ');

  with Self.FinfoServTom.ideEstabObra do
  begin
    Gerador.wCampo(tcStr, '', 'cnpjPrestador',     14, 14, 1, idePrestServ.cnpjPrestador);
    Gerador.wCampo(tcDe2, '', 'vlrTotalBruto',      1, 14, 1, idePrestServ.vlrTotalBruto);
    Gerador.wCampo(tcDe2, '', 'vlrTotalBaseRet',    1, 14, 1, idePrestServ.vlrTotalBaseRet);
    Gerador.wCampo(tcDe2, '', 'vlrTotalRetPrinc',   1, 14, 1, idePrestServ.vlrTotalRetPrinc);
    Gerador.wCampo(tcDe2, '', 'vlrTotalRetAdic',    1, 14, 0, idePrestServ.vlrTotalRetAdic);
    Gerador.wCampo(tcDe2, '', 'vlrTotalNRetPrinc',  1, 14, 0, idePrestServ.vlrTotalNRetPrinc);
    Gerador.wCampo(tcDe2, '', 'vlrTotalNRetAdic',   1, 14, 0, idePrestServ.vlrTotalNRetAdic);
    Gerador.wCampo(tcStr, '', 'indCPRB',            1,  1, 1, indCPRBToStr(idePrestServ.indCPRB));

    GerarNFs(idePrestServ.nfs);
    GerarinfoProcRetPr(idePrestServ.infoProcRetPr);
    GerarinfoProcRetAd(idePrestServ.infoProcRetAd);
  end;

  Gerador.wGrupo('/idePrestServ');
end;

procedure TevtServTom.GerarNFs(Lista: TnfsCollection);
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

procedure TevtServTom.GerarinfoTpServ(Lista: TinfoTpServCollection);
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

procedure TevtServTom.GerarinfoProcRetPr(Lista: TinfoProcRetPrCollection);
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

procedure TevtServTom.GerarinfoProcRetAd(Lista: TinfoProcRetAdCollection);
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

function TevtServTom.GerarXML: Boolean;
begin
  try
    Self.VersaoDF := TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveReinf(now, self.ideContri.NrInsc, self.Sequencial);

    GerarCabecalho('evtTomadorServicos');
    Gerador.wGrupo('evtServTom id="' + Self.Id + '"');

    GerarIdeEvento2(Self.IdeEvento);
    GerarideContri(Self.ideContri);

    Gerador.wGrupo('infoServTom');

    GerarideEstabObra;

    Gerador.wGrupo('/infoServTom');
    Gerador.wGrupo('/evtServTom');

    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtServTom');

    Validar(schevtTomadorServicos);
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtServTom.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J: Integer;
begin
  Result := False;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtServTom';
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif := StrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.perApur  := INIRec.ReadString(sSecao, 'perApur', EmptyStr);
      ideEvento.TpAmb    := StrTotpAmbReinf(Ok, INIRec.ReadString(sSecao, 'tpAmb', '1'));
      ideEvento.ProcEmi  := StrToProcEmiReinf(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc  := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideContri';
      ideContri.OrgaoPublico := (TACBrReinf(FACBrReinf).Configuracoes.Geral.TipoContribuinte = tcOrgaoPublico);
      ideContri.TpInsc       := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideContri.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideEstabObra';
      infoServTom.ideEstabObra.tpInscEstab := StrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInscEstab', '1'));
      infoServTom.ideEstabObra.nrInscEstab := INIRec.ReadString(sSecao, 'nrInscEstab', EmptyStr);
      infoServTom.ideEstabObra.indObra     := StrToindObra(Ok, INIRec.ReadString(sSecao, 'indObra', '1'));

      with infoServTom.ideEstabObra do
      begin
        sSecao := 'idePrestServ';
        idePrestServ.cnpjPrestador     := INIRec.ReadString(sSecao, 'cnpjPrestador', EmptyStr);
        idePrestServ.vlrTotalBruto     := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalBruto', ''), 0);
        idePrestServ.vlrTotalBaseRet   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalBaseRet', ''), 0);
        idePrestServ.vlrTotalRetPrinc  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalRetPrinc', ''), 0);
        idePrestServ.vlrTotalRetAdic   := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalRetAdic', ''), 0);
        idePrestServ.vlrTotalNRetPrinc := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalNRetPrinc', ''), 0);
        idePrestServ.vlrTotalNRetAdic  := StringToFloatDef(INIRec.ReadString(sSecao, 'vlrTotalNRetAdic', ''), 0);
        idePrestServ.indCPRB           := StrToindCPRB(Ok, INIRec.ReadString(sSecao, 'indCPRB', '1'));

        I := 1;
        while true do
        begin
          // de 01 até 999
          sSecao := 'nfs' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'serie', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with idePrestServ.nfs.Add do
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

              with infoTpServ.Add do
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

          with idePrestServ.infoProcRetPr.Add do
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

          with idePrestServ.infoProcRetAd.Add do
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

    Result := True;
  finally
     INIRec.Free;
  end;
end;

end.
