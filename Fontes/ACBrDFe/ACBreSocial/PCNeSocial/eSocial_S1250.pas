{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 01/03/2016: Guilherme Costa
|*  - Alterações para validação com o XSD
******************************************************************************}
{$I ACBr.inc}

unit eSocial_S1250;

interface

uses
  SysUtils, Classes,
  pcnConversao,
  eSocial_Common, eSocial_Conversao, eSocial_Gerador;

type
  TS1250Collection = class;
  TS1250CollectionItem = class;
  TEvtAqProd = class;
  TInfoAquisProd=class;
  TIdeEstabAdquir=class;
  TTpAquisItem = class;
  TTpAquisColecao = class;
  TIdeProdutorItem = class;
  TIdeProdutorColecao = class;

  TS1250Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS1250CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1250CollectionItem);
  public
    function Add: TS1250CollectionItem;
    property Items[Index: Integer]: TS1250CollectionItem read GetItem write SetItem; default;
  end;

  TS1250CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtAqProd: TEvtAqProd;
    procedure setEvtAqProd(const Value: TEvtAqProd);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtAqProd: TEvtAqProd read FEvtAqProd write setEvtAqProd;
  end;

  TEvtAqProd = class(TESocialEvento)
  private
    FIdeEvento: TIdeEvento3;
    FIdeEmpregador: TIdeEmpregador;
    FInfoAquisProd: TInfoAquisProd;

    {Geradores específicos da classe}
    procedure GerarInfoAquisProd();
    procedure GerarIdeEstabAdquir();
    procedure GerarTpAquis(pTpAquis: TTpAquisColecao);
    procedure GerarIdeProdutor(pIdeProdutor: TIdeProdutorColecao);
    procedure GerarInfoProcJud(pInfoProcJud: TInfoProcJudCollection);
  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor  Destroy; override;

    function GerarXML: boolean; override;

    property IdeEvento: TIdeEvento3 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property InfoAquisProd: TInfoAquisProd read FInfoAquisProd write FInfoAquisProd;
  end;

  TInfoAquisProd=class(TPersistent)
  private
    FIdeEstabAdquir: TIdeEstabAdquir;
  public
    constructor create;
    destructor destroy; override;

    property IdeEstabAdquir: TIdeEstabAdquir read FIdeEstabAdquir write FIdeEstabAdquir;
  end;

  TIdeEstabAdquir=class(TPersistent)
  private
    FtpInscAdq: tpTpInsc;
    FnrInscAdq: string;
    FTpAquis: TTpAquisColecao;
  public
    constructor create;
    destructor destroy; override;

    property tpInscAdq: tpTpInsc read FtpInscAdq write FtpInscAdq;
    property nrInscAdq: string read FnrInscAdq write FnrInscAdq;
    property TpAquis: TTpAquisColecao read FTpAquis write FTpAquis;
  end;

  TTpAquisColecao = class(TCollection)
  private
    function GetItem(Index: Integer): TTpAquisItem;
    procedure SetItem(Index: Integer; const Value: TTpAquisItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TTpAquisItem;
    property Items[Index: Integer]: TTpAquisItem read GetItem write SetItem;
  end;

  TTpAquisItem = class(TCollectionItem)
  private
    FindAquis: tpIdAquis;
    FvlrTotAquis: Double;
    FIdeProdutor: TIdeProdutorColecao;
  public
    constructor create; reintroduce;
    destructor destroy; override;

    property indAquis: tpIdAquis read FindAquis write FindAquis;
    property vlrTotAquis: double read FvlrTotAquis write FvlrTotAquis;
    property IdeProdutor: TIdeProdutorColecao read FIdeProdutor write FIdeProdutor;
  end;


  TIdeProdutorColecao = class(TCollection)
  private
    function GetItem(Index: Integer): TIdeProdutorItem;
    procedure SetItem(Index: Integer; const Value: TIdeProdutorItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TIdeProdutorItem;
    property Items[Index: Integer]: TIdeProdutorItem read GetItem write SetItem;
  end;

  TIdeProdutorItem = class(TCollectionItem)
  private
    FtpInscProd: tpTpInsc;
    FnrInscProd: string;
    FvlrBruto: Double;
    FvrCPDescPR: Double;
    FvrRatDescPR: Double;
    FvrSenarDesc: Double;

    FNfs: TNfsColecao;
    FInfoProcJud: TInfoProcJudCollection;
  public
    constructor create; reintroduce;
    destructor destroy; override;

    property tpInscProd: tpTpInsc read FtpInscProd write FtpInscProd;
    property nrInscProd: string read FnrInscProd write FnrInscProd;
    property vlrBruto: Double read FvlrBruto write FvlrBruto;
    property vrCPDescPR: Double read FvrCPDescPR write FvrCPDescPR;
    property vrRatDescPR: Double read FvrRatDescPR write FvrRatDescPR;
    property vrSenarDesc: Double read FvrSenarDesc write FvrSenarDesc;

    property Nfs: TNfsColecao read FNfs write FNfs;
    property InfoProcJud: TInfoProcJudCollection read FInfoProcJud write FInfoProcJud;
  end;

implementation

uses
  eSocial_Periodicos;

{ TS1250Collection }
function TS1250Collection.Add: TS1250CollectionItem;
begin
  Result := TS1250CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS1250Collection.GetItem(Index: Integer): TS1250CollectionItem;
begin
  Result := TS1250CollectionItem(inherited GetItem(Index));
end;

procedure TS1250Collection.SetItem(Index: Integer;
  Value: TS1250CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{TS1250CollectionItem}
constructor TS1250CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento     := teS1250;
  FEvtAqProd := TEvtAqProd.Create(AOwner);
end;

destructor TS1250CollectionItem.Destroy;
begin
  FEvtAqProd.Free;
  inherited;
end;

procedure TS1250CollectionItem.setEvtAqProd(const Value: TEvtAqProd);
begin
  FEvtAqProd.Assign(Value);
end;

{ TEvtContratAvNP }
constructor TEvtAqProd.Create(AACBreSocial: TObject);
begin
  inherited;
  FIdeEvento     := TIdeEvento3.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FInfoAquisProd := TInfoAquisProd.create;
end;

destructor TEvtAqProd.destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FInfoAquisProd.Free;
  inherited;
end;

procedure TEvtAqProd.GerarInfoAquisProd;
begin
  Gerador.wGrupo('infoAquisProd');
    GerarIdeEstabAdquir();
  Gerador.wGrupo('/infoAquisProd');
end;

procedure TEvtAqProd.GerarIdeEstabAdquir;
begin
  Gerador.wGrupo('ideEstabAdquir');
    Gerador.wCampo(tcStr, '', 'tpInscAdq', 0, 0, 0, eSTpInscricaoToStr(InfoAquisProd.IdeEstabAdquir.tpInscAdq));
    Gerador.wCampo(tcStr, '', 'nrInscAdq', 0, 0, 0, InfoAquisProd.IdeEstabAdquir.nrInscAdq);

    GerarTpAquis(InfoAquisProd.IdeEstabAdquir.TpAquis);
  Gerador.wGrupo('/ideEstabAdquir');
end;

procedure TEvtAqProd.GerarIdeProdutor(pIdeProdutor: TIdeProdutorColecao);
var
  i: integer;
begin
  for i := 0 to pIdeProdutor.Count - 1 do
  begin
    Gerador.wGrupo('ideProdutor');
      Gerador.wCampo(tcStr, '', 'tpInscProd',  0, 0, 0, eSTpInscricaoToStr(pIdeProdutor.Items[i].tpInscProd));
      Gerador.wCampo(tcStr, '', 'nrInscProd',  0, 0, 0, pIdeProdutor.Items[i].nrInscProd);
      Gerador.wCampo(tcDe2, '', 'vlrBruto',    0, 0, 0, pIdeProdutor.Items[i].vlrBruto);
      Gerador.wCampo(tcDe2, '', 'vrCPDescPR',  0, 0, 0, pIdeProdutor.Items[i].vrCPDescPR);
      Gerador.wCampo(tcDe2, '', 'vrRatDescPR', 0, 0, 0, pIdeProdutor.Items[i].vrRatDescPR);
      Gerador.wCampo(tcDe2, '', 'vrSenarDesc', 0, 0, 0, pIdeProdutor.Items[i].vrSenarDesc);

      GerarNfs(pIdeProdutor.Items[i].Nfs);
      GerarInfoProcJud(pIdeProdutor.Items[i].InfoProcJud);
    Gerador.wGrupo('/ideProdutor');
  end;
end;

procedure TEvtAqProd.GerarInfoProcJud(pInfoProcJud: TInfoProcJudCollection);
var
  i : integer;
begin
  for i := 0 to pInfoProcJud.Count - 1 do
    begin
      Gerador.wGrupo('infoProcJud');
        Gerador.wCampo(tcStr, '', 'nrProcJud',   0, 0, 0, pInfoProcJud.Items[i].nrProcJud);
        Gerador.wCampo(tcInt, '', 'codSusp',   0, 0, 0, pInfoProcJud.Items[i].codSusp);
        Gerador.wCampo(tcDe2, '', 'vrCPNRet',    0, 0, 0, pInfoProcJud.Items[i].vrCPNRet);
        Gerador.wCampo(tcDe2, '', 'vrRatNRet',   0, 0, 0, pInfoProcJud.Items[i].vrRatNRet);
        Gerador.wCampo(tcDe2, '', 'vrSenarNRet', 0, 0, 0,pInfoProcJud.Items[i].vrSenarNRet);
      Gerador.wGrupo('/infoProcJud');
    end;
end;

procedure TEvtAqProd.GerarTpAquis(pTpAquis: TTpAquisColecao);
var
  iTpAquis: Integer;
begin
  for iTpAquis := 0 to InfoAquisProd.IdeEstabAdquir.TpAquis.Count - 1 do
  begin
    Gerador.wGrupo('tpAquis');
      Gerador.wCampo(tcStr, '', 'indAquis',    0, 0, 0, InfoAquisProd.IdeEstabAdquir.TpAquis.Items[iTpAquis].indAquis);
      Gerador.wCampo(tcDe2, '', 'vlrTotAquis', 0, 0, 0, InfoAquisProd.IdeEstabAdquir.TpAquis.Items[iTpAquis].vlrTotAquis);

      GerarIdeProdutor(InfoAquisProd.IdeEstabAdquir.TpAquis.Items[iTpAquis].IdeProdutor);
    Gerador.wGrupo('/tpAquis');
  end;
end;

function TEvtAqProd.GerarXML: boolean;
begin
  try
    GerarCabecalho('evtAqProd');
      Gerador.wGrupo('evtAqProd Id="'+GerarChaveEsocial(now, self.ideEmpregador.NrInsc, 0)+'"');
        gerarIdeEvento3(self.IdeEvento);
        gerarIdeEmpregador(self.IdeEmpregador);
        GerarInfoAquisProd;
      Gerador.wGrupo('/evtAqProd');
    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtAqProd');
    Validar('evtAqProd');
  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

{ TTpAquisColecao }
function TTpAquisColecao.Add: TTpAquisItem;
begin
  Result := TTpAquisItem(inherited add);
  Result.Create;
end;

constructor TTpAquisColecao.create(AOwner: TPersistent);
begin
  inherited create(TTpAquisItem)
end;

function TTpAquisColecao.GetItem(Index: Integer): TTpAquisItem;
begin
  Result := TTpAquisItem(inherited GetItem(Index));
end;

procedure TTpAquisColecao.SetItem(Index: Integer; const Value: TTpAquisItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoAquisProd }
constructor TInfoAquisProd.create;
begin
  inherited;
  FIdeEstabAdquir := TIdeEstabAdquir.Create;
end;

destructor TInfoAquisProd.destroy;
begin
  FIdeEstabAdquir.Free;
  inherited;
end;

{ TTpAquisItem }
constructor TTpAquisItem.create;
begin
  FIdeProdutor := TIdeProdutorColecao.Create(self);
end;

destructor TTpAquisItem.destroy;
begin
  FIdeProdutor.Free;
  inherited;
end;

{ TIdeProdutorColecao }
function TIdeProdutorColecao.Add: TIdeProdutorItem;
begin
  Result := TIdeProdutorItem(inherited Add);
  Result.Create;
end;

constructor TIdeProdutorColecao.Create(AOwner: TPersistent);
begin
  inherited Create(TIdeProdutorItem);
end;

function TIdeProdutorColecao.GetItem(Index: Integer): TIdeProdutorItem;
begin
  Result := TIdeProdutorItem(inherited GetItem(Index));
end;

procedure TIdeProdutorColecao.SetItem(Index: Integer;
  const Value: TIdeProdutorItem);
begin
  inherited SetItem(Index, Value);
end;

{ TIdeEstabAdquir }
constructor TIdeEstabAdquir.create;
begin
  inherited;
  FTpAquis := TTpAquisColecao.Create(self);
end;

destructor TIdeEstabAdquir.destroy;
begin
  FTpAquis.Free;
  inherited;
end;

{ TIdeProdutorItem }
constructor TIdeProdutorItem.create;
begin
  FNfs := TNfsColecao.Create(self);
  FInfoProcJud := TInfoProcJudCollection.Create(Self);
end;

destructor TIdeProdutorItem.destroy;
begin
  FNfs.Free;
  FInfoProcJud.Free;
  inherited;
end;

end.
