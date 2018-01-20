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
|* 29/02/2016: Guilherme Costa
|*  - Alterado os atributos que não estavam de acordo com o leiaute/xsd
******************************************************************************}
{$I ACBr.inc}

unit eSocial_S1000;

interface

uses
  SysUtils, Classes,
  pcnConversao, ACBrUtil,
  eSocial_Common, eSocial_Conversao, eSocial_Consts, eSocial_Gerador;

type
  TS1000Collection = class;
  TS1000CollectionItem = class;
  TevtInfoEmpregador = class;

  {Classes específicas deste evento}
  TInfoEmpregador = class;
  TInfoCadastro = class;
  TDadosIsencao = class;
  TInfoOrgInternacional = class;
  TSoftwareHouseCollection = class;
  TSoftwareHouseCollectionItem = class;
  TInfoComplementares = class;
  TSituacaoPJ = class;
  TSituacaoPF = class;
  TInfoOP = class;
  TInfoEFR = class;
  TInfoEnte = class;

  TS1000Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS1000CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1000CollectionItem);
  public
    function Add: TS1000CollectionItem;
    property Items[Index: Integer]: TS1000CollectionItem read GetItem write SetItem; default;
  end;

  TS1000CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FevtInfoEmpregador: TevtInfoEmpregador;
    procedure setevtInfoEmpregador(const Value: TevtInfoEmpregador);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtInfoEmpregador: TevtInfoEmpregador read FevtInfoEmpregador write setevtInfoEmpregador;
  end;

  TevtInfoEmpregador = class(TeSocialEvento) //Classe do elemento principal do XML do evento!
  private
    FXMLAssinado: String;
    FModoLancamento: TModoLancamento;
    FIdeEvento: TIdeEvento;
    FIdeEmpregador: TIdeEmpregador;
    FInfoEmpregador: TInfoEmpregador;
    FACBreSocial: TObject;

    {Geradores específicos desta classe}
    procedure GerarInfoCadastro();
    procedure GerarInfoFap();
    procedure GerarDadosIsencao();
    procedure GerarContato();
    procedure GerarInfoOp();
    procedure GerarInfoEFR();
    procedure GerarInfoEnte();
    procedure GerarInfoOrgInternacional();
    procedure GerarSoftwareHouse();
    procedure GerarSituacaoPJ();
    procedure GerarSituacaoPF();
    procedure GerarInfoComplementares();
    procedure GerarLimitesRem();
    procedure GerarPerc(indexAliqEnteFed: Integer);
  public
    constructor Create(AACBreSocial: TObject); overload;
    destructor  Destroy; override;

    function  GerarXML: boolean; override;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property ideEvento: TIdeEvento read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property infoEmpregador: TInfoEmpregador read FInfoEmpregador write FInfoEmpregador;
  end;

  TInfoEmpregador = class(TPersistent)
   private
    FidePeriodo: TIdePeriodo;
    FinfoCadastro: TInfoCadastro;
    FNovaValidade: TidePeriodo;

    function getInfoCadastro(): TInfoCadastro;
    function getNovaValidade(): TidePeriodo;
  public
    constructor Create;
    destructor Destroy; override;
    function infoCadastroInst(): Boolean;
    function novaValidadeInst(): Boolean;

    property idePeriodo: TIdePeriodo read FidePeriodo write FidePeriodo;
    property infoCadastro: TInfoCadastro read getInfoCadastro write FinfoCadastro;
    property novaValidade: TIdePeriodo read getNovaValidade write FnovaValidade;
  end;

  TInfoCadastro = class(TPersistent)
   private
    FNmRazao: string;
    FClassTrib: String;
    FNatJurid: string;
    FIndCoop: TpIndCoop;
    FIndConstr: TpIndConstr;
    FIndDesFolha: TpIndDesFolha;
    FIndOptRegEletron: TpIndOptRegEletron;
    FIndEntEd: tpSimNao;
    FIndEtt: tpSimNao;
    FNrRegEtt: String;
    FDadosIsencao: TDadosIsencao;
    FContato: TContato;
    FInfoOp: TInfoOp;
    FInfoOrgInternacional: TInfoOrgInternacional;
    FSoftwareHouse: TSoftwareHouseCollection;
    FInfoComplementares: TInfoComplementares;

    function getInfoOp(): TInfoOp;
    function getDadosIsencao(): TDadosIsencao;
    function getInfoOrgInternacional(): TInfoOrgInternacional;
  public
    constructor Create;
    destructor Destroy; override;
    function infoOrgInternacionalInst(): Boolean;
    function dadosIsencaoInst(): Boolean;
    function infoOpInst(): Boolean;

    property NmRazao: string read FNmRazao write FNmRazao;
    property ClassTrib: String read FClassTrib write FClassTrib;
    property NatJurid: string read FNatJurid write FNatJurid;
    property IndCoop: TpIndCoop read FIndCoop write FIndCoop;
    property IndConstr: TpIndConstr read FIndConstr write FIndConstr;
    property IndDesFolha: TpIndDesFolha read FIndDesFolha write FIndDesFolha;
    property IndOptRegEletron: TpIndOptRegEletron read FIndOptRegEletron write FIndOptRegEletron;
    property IndEntEd: tpSimNao read FIndEntEd write FIndEntEd;
    property IndEtt: tpSimNao read FIndEtt write FIndEtt;
    property nrRegEtt: String read FNrRegEtt write FNrRegEtt;
    property DadosIsencao: TDadosIsencao read getDadosIsencao write FDadosIsencao;
    property Contato: TContato read FContato write FContato;
    property InfoOp: TInfoOp read getInfoOp write FInfoOp;
    property InfoOrgInternacional: TInfoOrgInternacional read getInfoOrgInternacional write FInfoOrgInternacional;
    property SoftwareHouse: TSoftwareHouseCollection read FSoftwareHouse write FSoftwareHouse;
    property InfoComplementares: TInfoComplementares read FInfoComplementares write FInfoComplementares;
  end;

  TInfoComplementares = class(TPersistent)
   private
    FSituacaoPJ: TSituacaoPJ;
    FSituacaoPF: TSituacaoPF;

    function getSituacaoPJ(): TSituacaoPJ;
    function getSituacaoPF(): TSituacaoPF;
  public
    destructor destroy; override;
    function situacaoPFInst(): Boolean;
    function situacaoPJInst(): Boolean;

    property SituacaoPJ: TSituacaoPJ read getSituacaoPJ write FSituacaoPJ;
    property SituacaoPF: TSituacaoPF read getSituacaoPF write FSituacaoPF;
  end;

  TSituacaoPJ = class(TPersistent)
   private
    FIndSitPJ: tpIndSitPJ;
  public
    property IndSitPJ: tpIndSitPJ read FIndSitPJ write FIndSitPJ;
  end;

  TSituacaoPF = class(TPersistent)
   private
    FIndSitPF: tpIndSitPF;
  public
    property IndSitPF: tpIndSitPF read FIndSitPF write FIndSitPF;
  end;

  TDadosIsencao = class(TPersistent)
   private
    FIdeMinLei: String;
    FNrCertif: string;
    FDtEmisCertif : TDateTime;
    FDtVencCertif: TDateTime;
    FNrProtRenov: string;
    FDtProtRenov: TDateTime;
    FDtDou: TDateTime;
    FPagDou: string;
  public
    property IdeMinLei: String read FIdeMinLei write FIdeMinLei;
    property NrCertif: string read FNrCertif write FNrCertif;
    property DtEmisCertif: TDateTime read FDtEmisCertif write FDtEmisCertif;
    property DtVencCertif: TDateTime read FDtVencCertif write FDtVencCertif;
    property NrProtRenov: string read FNrProtRenov write FNrProtRenov;
    property DtProtRenov: TDateTime read FDtProtRenov write FDtProtRenov;
    property DtDou: TDateTime read FDtDou write FDtDou;
    property PagDou: string read FPagDou write FPagDou;
  end;

  TInfoOrgInternacional = class(TPersistent)
  private
    FIndAcordoIsenMulta: tpIndAcordoIsencaoMulta;
  public
    property IndAcordoIsenMulta: tpIndAcordoIsencaoMulta read FIndAcordoIsenMulta write FIndAcordoIsenMulta;
  end;

  TSoftwareHouseCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TSoftwareHouseCollectionItem;
    procedure SetItem(Index: Integer; Value: TSoftwareHouseCollectionItem);
  public
    constructor create(); reintroduce;
    function Add: TSoftwareHouseCollectionItem;
    property Items[Index: Integer]: TSoftwareHouseCollectionItem read GetItem write SetItem; default;
  end;

  TSoftwareHouseCollectionItem = class(TCollectionItem)
   private
    FCnpjSoftHouse: string;
    FNmRazao: string;
    FNmCont: string;
    FTelefone: string;
    Femail: string;
  public
    constructor create; reintroduce;

    property CnpjSoftHouse: string read FCnpjSoftHouse write FCnpjSoftHouse;
    property NmRazao: string read FNmRazao write FNmRazao;
    property NmCont: string read FNmCont write FNmCont;
    property Telefone: string read FTelefone write FTelefone;
    property email: string read Femail write Femail;
  end;

  TInfoEFR = class(TPersistent)
   private
     FideEFR: tpSimNao;
     FcnpjEFR: String;
  public
    property ideEFR: tpSimNao read FideEFR write FideEFR;
    property cnpjEFR: String read FcnpjEFR write FcnpjEFR;
  end;

  TInfoEnte = class(TPersistent)
    private
      FNmEnte: String;
      FUf: tpuf;
      FCodMunic: Integer;
      FIndRPPS: tpSimNao;
      FSubteto: tpIdeSubteto;
      FVrSubTeto: double;
  public
    property nmEnte: String read FNmEnte write FNmEnte;
    property uf: tpuf read FUf write FUf;
    property codMunic: Integer read FCodMunic write FCodMunic;
    property indRPPS: tpSimNao read FIndRPPS write FIndRPPS;
    property subteto: tpIdeSubteto read FSubteto write FSubteto;
    property vrSubteto: Double read FVrSubTeto write FVrSubTeto;
  end;

  TInfoOp = class(TPersistent)
   private
     FNrSiafi: String;
     FInfoEFR: TInfoEFR;
     FInfoEnte: TInfoEnte;
     function getInfoEFR(): TInfoEFR;
     function getInfoEnte(): TInfoEnte;
  public
    constructor Create;
    destructor Destroy; override;
    function InfoEFRInst(): Boolean;
    function InfoEnteInst(): Boolean;

    property nrSiafi: String read FNrSiafi write FNrSiafi;
    property infoEFR: TInfoEFR read getInfoEFR write FInfoEFR;
    property infoEnte: TInfoEnte read getInfoEnte write FInfoEnte;
  end;

implementation

uses
  eSocial_Iniciais, ACBreSocial;

{ TS1000Collection }

function TS1000Collection.Add: TS1000CollectionItem;
begin
  Result := TS1000CollectionItem(inherited Add);
end;

function TS1000Collection.GetItem(Index: Integer): TS1000CollectionItem;
begin
  Result := TS1000CollectionItem(inherited GetItem(Index));
end;

procedure TS1000Collection.SetItem(Index: Integer; Value: TS1000CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS1000CollectionItem }

procedure TS1000CollectionItem.AfterConstruction;
begin
  inherited;
  FTipoEvento := teS1000;
  FevtInfoEmpregador := TevtInfoEmpregador.Create(Collection.Owner);
end;

procedure TS1000CollectionItem.BeforeDestruction;
begin
  inherited;
  FevtInfoEmpregador.Free;
end;

procedure TS1000CollectionItem.setevtInfoEmpregador(const Value: TevtInfoEmpregador);
begin
  FevtInfoEmpregador.Assign(Value);
end;

{ TevtInfoEmpregador }

constructor TevtInfoEmpregador.Create(AACBreSocial: TObject);
begin
  inherited;
  FACBreSocial := AACBreSocial;
  FIdeEmpregador:= TIdeEmpregador.create;
  FIdeEvento:= TIdeEvento.create;
  FInfoEmpregador := TInfoEmpregador.Create;
end;

destructor TevtInfoEmpregador.Destroy;
begin
  FIdeEmpregador.Free;
  FIdeEvento.Free;
  FInfoEmpregador.Free;
  inherited;
end;

procedure TevtInfoEmpregador.GerarContato;
begin
  Gerador.wGrupo('contato');
    Gerador.wCampo(tcStr, '', 'nmCtt', 0, 0, 0, Self.infoEmpregador.infoCadastro.Contato.NmCtt);
    Gerador.wCampo(tcStr, '', 'cpfCtt', 0, 0, 0, Self.infoEmpregador.infoCadastro.Contato.CpfCtt);

    if (Self.infoEmpregador.infoCadastro.Contato.FoneFixo <> '') then
      Gerador.wCampo(tcStr, '', 'foneFixo', 0, 0, 0, Self.infoEmpregador.infoCadastro.Contato.FoneFixo);

    if (Self.infoEmpregador.infoCadastro.Contato.FoneCel <> '') then
      Gerador.wCampo(tcStr, '', 'foneCel', 0, 0, 0, Self.infoEmpregador.infoCadastro.Contato.FoneCel);

    if (Self.infoEmpregador.infoCadastro.Contato.email <> '') then
      Gerador.wCampo(tcStr, '', 'email', 0, 0, 0, Self.infoEmpregador.infoCadastro.Contato.email);
  Gerador.wGrupo('/contato');
end;

procedure TevtInfoEmpregador.GerarInfoEFR;
begin
  if infoEmpregador.infoCadastro.InfoOp.InfoEFRInst() and (infoEmpregador.infoCadastro.InfoOp.infoEFR.cnpjEFR <> EmptyStr) then
  begin
    Gerador.wGrupo('infoEFR');
      Gerador.wCampo(tcStr, '', 'ideEFR', 1, 1, 0, eSSimNaoToStr(infoEmpregador.infoCadastro.InfoOp.infoEFR.ideEFR));
      Gerador.wCampo(tcStr, '', 'cnpjEFR', 0, 1, 0, infoEmpregador.infoCadastro.InfoOp.infoEFR.cnpjEFR);
    Gerador.wGrupo('/infoEFR')
  end;
end;

procedure TevtInfoEmpregador.GerarInfoEnte;
begin
  if infoEmpregador.infoCadastro.InfoOp.InfoEnteInst() and (infoEmpregador.infoCadastro.InfoOp.infoEnte.nmEnte <> EmptyStr) then
  begin
    Gerador.wGrupo('infoEnte');
      Gerador.wCampo(tcStr, '', 'nmEnte', 1, 1, 0, infoEmpregador.infoCadastro.InfoOp.infoEnte.nmEnte);
      Gerador.wCampo(tcStr, '', 'uf', 1, 1, 0, eSufToStr(infoEmpregador.infoCadastro.InfoOp.infoEnte.uf));
      Gerador.wCampo(tcInt, '', 'codMunic', 0, 1, 0, infoEmpregador.infoCadastro.InfoOp.infoEnte.codMunic);
      Gerador.wCampo(tcStr, '', 'indRPPS', 1, 1, 0, eSSimNaoToStr(infoEmpregador.infoCadastro.InfoOp.infoEnte.indRPPS));
      Gerador.wCampo(tcInt, '', 'subteto', 1, 1, 0, eSIdeSubtetoToStr(infoEmpregador.infoCadastro.InfoOp.infoEnte.subteto));
      Gerador.wCampo(tcInt, '', 'vrSubteto', 1, 1, 0, infoEmpregador.infoCadastro.InfoOp.infoEnte.vrSubteto);
    Gerador.wGrupo('/infoEnte');
  end;
end;

procedure TevtInfoEmpregador.GerarInfoOp;
begin
  if infoEmpregador.infoCadastro.infoOpInst() and (infoEmpregador.infoCadastro.InfoOp.nrSiafi <> EmptyStr) then
  begin
    Gerador.wGrupo('infoOP');
      Gerador.wCampo(tcStr, '', 'nrSiafi', 1, 1, 0, infoEmpregador.infoCadastro.InfoOp.nrSiafi);
      GerarInfoEFR();
      GerarInfoEnte();
    Gerador.wGrupo('/infoOP');
  end;
end;

procedure TevtInfoEmpregador.GerarDadosIsencao;
begin
  if infoEmpregador.infoCadastro.dadosIsencaoInst() then
  begin
    Gerador.wGrupo('dadosIsencao');
      Gerador.wCampo(tcStr, '', 'ideMinLei', 0, 0, 0, infoEmpregador.infoCadastro.DadosIsencao.IdeMinLei);
      Gerador.wCampo(tcStr, '', 'nrCertif', 0, 0, 0, infoEmpregador.infoCadastro.DadosIsencao.NrCertif);
      Gerador.wCampo(tcDat, '', 'dtEmisCertif', 0, 0, 0, infoEmpregador.infoCadastro.DadosIsencao.DtEmisCertif);
      Gerador.wCampo(tcDat, '', 'dtVencCertif', 0, 0, 0, infoEmpregador.infoCadastro.DadosIsencao.DtVencCertif);

      if (infoEmpregador.infoCadastro.DadosIsencao.NrProtRenov <> '') then
        Gerador.wCampo(tcStr, '', 'nrProtRenov', 0, 0, 0, infoEmpregador.infoCadastro.DadosIsencao.NrProtRenov);

      if (DateToStr(infoEmpregador.infoCadastro.DadosIsencao.DtProtRenov) <> dDataBrancoNula) then
        Gerador.wCampo(tcDat, '', 'dtProtRenov', 0, 0, 0, infoEmpregador.infoCadastro.DadosIsencao.DtProtRenov);

      if (DateToStr(infoEmpregador.infoCadastro.DadosIsencao.DtDou) <> dDataBrancoNula) then
        Gerador.wCampo(tcDat, '', 'dtDou', 0, 0, 0, infoEmpregador.infoCadastro.DadosIsencao.DtDou);

      if (infoEmpregador.infoCadastro.DadosIsencao.PagDou <> '') then
        Gerador.wCampo(tcStr, '', 'pagDou', 0, 0, 0, infoEmpregador.infoCadastro.DadosIsencao.PagDou);
    Gerador.wGrupo('/dadosIsencao');
  end;
end;

procedure TevtInfoEmpregador.GerarInfoCadastro;
begin
  Gerador.wGrupo('infoCadastro');

  Gerador.wCampo(tcStr, '', 'nmRazao',          0, 100, 1, Self.infoEmpregador.infoCadastro.NmRazao);
  Gerador.wCampo(tcStr, '', 'classTrib',        0, 002, 1, Self.infoEmpregador.infoCadastro.ClassTrib);
  Gerador.wCampo(tcStr, '', 'natJurid',         0, 004, 0, Self.infoEmpregador.infoCadastro.NatJurid);
  Gerador.wCampo(tcStr, '', 'indCoop',          0, 001, 0, eSIndCooperativaToStr(Self.infoEmpregador.infoCadastro.IndCoop));
  Gerador.wCampo(tcStr, '', 'indConstr',        0, 001, 0, eSIndConstrutoraToStr(Self.infoEmpregador.infoCadastro.IndConstr));
  Gerador.wCampo(tcStr, '', 'indDesFolha',      0, 001, 1, eSIndDesFolhaToStr(Self.infoEmpregador.infoCadastro.IndDesFolha));
  Gerador.wCampo(tcStr, '', 'indOptRegEletron', 0, 001, 1, eSIndOptRegEletronicoToStr(Self.infoEmpregador.infoCadastro.IndOptRegEletron));
  Gerador.wCampo(tcStr, '', 'indEntEd',         0, 001, 0, eSSimNaoToStr(Self.infoEmpregador.infoCadastro.IndEntEd));
  Gerador.wCampo(tcStr, '', 'indEtt',           1, 001, 0, eSSimNaoToStr(Self.infoEmpregador.infoCadastro.IndEtt));
  Gerador.wCampo(tcStr, '', 'nrRegEtt',         0, 030, 0, Self.infoEmpregador.infoCadastro.nrRegEtt);

  GerarDadosIsencao();
  GerarContato();
  GerarInfoOp();
  GerarInfoOrgInternacional();
  GerarSoftwareHouse();
  GerarInfoComplementares();

  Gerador.wGrupo('/infoCadastro');
end;

procedure TevtInfoEmpregador.GerarInfoComplementares;
begin
  Gerador.wGrupo('infoComplementares');    
  GerarSituacaoPJ();
  GerarSituacaoPF();
  Gerador.wGrupo('/infoComplementares');
end;

procedure TevtInfoEmpregador.GerarInfoFap;
begin
  //if infoEmpregador.infoCadastro.infoFapInst() then
  //begin
  //  Gerador.wGrupo('infoFap');
  //    Gerador.wCampo(tcDe4, '', 'fap', 0, 0, 0, infoEmpregador.infoCadastro.InfoFap.fap);
  //    if (infoEmpregador.infoCadastro.InfoFap.procAdmJudFapInst()) then
  //      GerarProcessoAdmJudFap(infoEmpregador.infoCadastro.InfoFap.procAdmJudFap);
  //  Gerador.wGrupo('/infoFap');
  //end;
end;

procedure TevtInfoEmpregador.GerarInfoOrgInternacional;
begin
  if infoEmpregador.infoCadastro.infoOrgInternacionalInst() then
  begin
    Gerador.wGrupo('infoOrgInternacional');
      Gerador.wCampo(tcStr, '', 'indAcordoIsenMulta', 0, 0, 0, eSIndAcordoIsencaoMultaToStr(infoEmpregador.infoCadastro.InfoOrgInternacional.IndAcordoIsenMulta));
    Gerador.wGrupo('/infoOrgInternacional');
  end;
end;

procedure TevtInfoEmpregador.GerarLimitesRem;
var
  iLimitesRem: Integer;
begin
  //for iLimitesRem := 0 to infoEmpregador.infoCadastro.InfoRPPS.infEnteFed.limitesRem.Count - 1 do
  //begin
  //  Gerador.wGrupo('limitesRem');
  //    Gerador.wCampo(tcStr, '', 'ideSubteto', 0, 0, 0, eSIdeSubtetoToStr(Self.infoEmpregador.infoCadastro.InfoRPPS.infEnteFed.limitesRem.Items[iLimitesRem].IdeSubteto));
  //    Gerador.wCampo(tcDe2, '', 'valSubteto', 0, 0, 0, Self.infoEmpregador.infoCadastro.InfoRPPS.infEnteFed.limitesRem.Items[iLimitesRem].ValSubteto);
  //    Gerador.wCampo(tcStr, '', 'idMaior', 0, 0, 0, Self.infoEmpregador.infoCadastro.InfoRPPS.infEnteFed.limitesRem.Items[iLimitesRem].IdMaior);
  //  Gerador.wGrupo('/limitesRem');
  //end;
end;

procedure TevtInfoEmpregador.GerarPerc(indexAliqEnteFed: Integer);
begin
  //Gerador.wGrupo('perc');
  //  Gerador.wCampo(tcDe2, '', 'percSeg', 0, 0, 0, Self.infoEmpregador.infoCadastro.InfoRPPS.infEnteFed.aliqEnteFed.Items[indexAliqEnteFed].Perc.PercSeg);
  //  Gerador.wCampo(tcDe2, '', 'percEnte', 0, 0, 0, Self.infoEmpregador.infoCadastro.InfoRPPS.infEnteFed.aliqEnteFed.Items[indexAliqEnteFed].Perc.PercEnte);
  //  Gerador.wCampo(tcDe2, '', 'percSupl', 0, 0, 0, Self.infoEmpregador.infoCadastro.InfoRPPS.infEnteFed.aliqEnteFed.Items[indexAliqEnteFed].Perc.percSupl);
  //Gerador.wGrupo('/perc');
end;

procedure TevtInfoEmpregador.GerarSituacaoPF;
begin
  if infoEmpregador.infoCadastro.InfoComplementares.situacaoPFInst() then
  begin
    Gerador.wGrupo('situacaoPF');
      Gerador.wCampo(tcStr, '', 'indSitPF', 0, 0, 0, eSIndSitPFToStr(Self.infoEmpregador.infoCadastro.InfoComplementares.SituacaoPF.IndSitPF));
    Gerador.wGrupo('/situacaoPF');
  end;
end;

procedure TevtInfoEmpregador.GerarSituacaoPJ;
begin
  if infoEmpregador.infoCadastro.InfoComplementares.situacaoPJInst() then
  begin
    Gerador.wGrupo('situacaoPJ');
      Gerador.wCampo(tcStr, '', 'indSitPJ', 0, 0, 0, eSIndSitPJToStr(infoEmpregador.infoCadastro.InfoComplementares.SituacaoPJ.IndSitPJ));
    Gerador.wGrupo('/situacaoPJ');
  end;
end;

procedure TevtInfoEmpregador.GerarSoftwareHouse;
var
  iSoftwareHouse: Integer;
begin
  for iSoftwareHouse := 0 to infoEmpregador.infoCadastro.SoftwareHouse.Count - 1 do
  begin
    Gerador.wGrupo('softwareHouse');
      Gerador.wCampo(tcStr, '', 'cnpjSoftHouse', 0, 0, 0, infoEmpregador.infoCadastro.SoftwareHouse[iSoftwareHouse].CnpjSoftHouse);
      Gerador.wCampo(tcStr, '', 'nmRazao', 0, 0, 0, infoEmpregador.infoCadastro.SoftwareHouse[iSoftwareHouse].NmRazao);
      Gerador.wCampo(tcStr, '', 'nmCont', 0, 0, 0, infoEmpregador.infoCadastro.SoftwareHouse[iSoftwareHouse].NmCont);
      Gerador.wCampo(tcStr, '', 'telefone', 0, 0, 0, infoEmpregador.infoCadastro.SoftwareHouse[iSoftwareHouse].Telefone);

      if (infoEmpregador.infoCadastro.SoftwareHouse[iSoftwareHouse].email <> '') then
        Gerador.wCampo(tcStr, '', 'email', 0, 0, 0,  infoEmpregador.infoCadastro.SoftwareHouse[iSoftwareHouse].email);
    Gerador.wGrupo('/softwareHouse');
  end;
end;

function TevtInfoEmpregador.GerarXML: boolean;
begin
  try
    GerarCabecalho('evtInfoEmpregador');
      Gerador.wGrupo('evtInfoEmpregador Id="'+ GerarChaveEsocial(now, self.ideEmpregador.NrInsc, 0) +'"');
        GerarIdeEvento(Self.IdeEvento);
        GerarIdeEmpregador(Self.IdeEmpregador);
        Gerador.wGrupo('infoEmpregador');
          GerarModoAbertura(Self.ModoLancamento);
          GerarIdePeriodo(Self.infoEmpregador.idePeriodo);
          if (Self.ModoLancamento <> mlExclusao) then
          begin
            GerarInfoCadastro;
            if ModoLancamento = mlAlteracao then
              if (InfoEmpregador.novaValidadeInst()) then
                GerarIdePeriodo(InfoEmpregador.novaValidade,'novaValidade');
          end;
          GerarModoFechamento(Self.ModoLancamento);
        Gerador.wGrupo('/infoEmpregador');
      Gerador.wGrupo('/evtInfoEmpregador');
    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtInfoEmpregador');//Gerador.ArquivoFormatoXML;
    {$IFDEF DEBUG}
    with TStringList.Create do
    try
      Text := XML;
      SaveToFile(IncludeTrailingPathDelimiter(TACBreSocial(FACBreSocial).Configuracoes.Arquivos.PathSalvar) + 'S1000.xml');
    finally
      Free;
    end;
    {$ENDIF}
    Validar('evtInfoEmpregador');

  except on e:exception do
    raise Exception.Create(e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

{ TInfoEmpregador }

constructor TInfoEmpregador.Create;
begin
  inherited;
  FidePeriodo:= TIdePeriodo.Create;
  FinfoCadastro:= nil;
  FNovaValidade:= nil;
end;

destructor TInfoEmpregador.Destroy;
begin
  FidePeriodo.Free;
  FreeAndNil(FinfoCadastro);
  FreeAndNil(FNovaValidade);
  inherited;
end;

function TInfoEmpregador.getInfoCadastro: TInfoCadastro;
begin
  if Not(Assigned(FinfoCadastro)) then
    FinfoCadastro := TInfoCadastro.Create;
  Result := FinfoCadastro;
end;

function TInfoEmpregador.getNovaValidade: TidePeriodo;
begin
  if Not(Assigned(FNovaValidade)) then
    FNovaValidade := TIdePeriodo.Create;
  Result := FNovaValidade;
end;

function TInfoEmpregador.infoCadastroInst: Boolean;
begin
  Result := Assigned(FinfoCadastro);
end;

function TInfoEmpregador.novaValidadeInst: Boolean;
begin
  Result := Assigned(FNovaValidade);
end;

{ TInfoCadastro }

constructor TInfoCadastro.Create;
begin
  FDadosIsencao:= nil;
  FContato := TContato.Create;
  FInfoOrgInternacional := nil;
  FSoftwareHouse := TSoftwareHouseCollection.Create;
  FInfoComplementares := TInfoComplementares.Create;
  FInfoOp := TInfoOp.Create;
end;

function TInfoCadastro.dadosIsencaoInst: Boolean;
begin
  Result := Assigned(FDadosIsencao);
end;

destructor TInfoCadastro.Destroy;
begin
  FreeAndNil(FDadosIsencao);
  FContato.Free;
  FreeAndNil(FInfoOrgInternacional);
  FSoftwareHouse.Free;
  FInfoComplementares.Free;
  FreeAndNil(FInfoOp);
  inherited;
end;

function TInfoCadastro.getInfoOp: TInfoOp;
begin
  if Not(Assigned(FInfoOp)) then
    FInfoOp := TInfoOp.Create;
  Result := FInfoOp;
end;

function TInfoCadastro.infoOpInst: Boolean;
begin
  Result := Assigned(FInfoOp);
end;

function TInfoCadastro.getDadosIsencao: TDadosIsencao;
begin
  if Not(Assigned(FDadosIsencao)) then
    FDadosIsencao := TDadosIsencao.Create;
  Result := FDadosIsencao;
end;

function TInfoCadastro.getInfoOrgInternacional: TInfoOrgInternacional;
begin
  if Not(Assigned(FInfoOrgInternacional)) then
    FInfoOrgInternacional := TInfoOrgInternacional.Create;
  Result := FInfoOrgInternacional;
end;

function TInfoCadastro.infoOrgInternacionalInst: Boolean;
begin
  Result := Assigned(FInfoOrgInternacional);
end;

{ TInfoOp }

constructor TInfoOp.Create;
begin
  FInfoEFR := TInfoEFR.Create;
  FInfoEnte := TInfoEnte.Create;
end;

destructor TInfoOp.Destroy;
begin
  FreeAndNil(FInfoEFR);
  FreeAndNil(FInfoEnte);
end;

function TInfoOp.getInfoEFR: TInfoEFR;
begin
  if Not(Assigned(FInfoEFR)) then
    FInfoEFR := TInfoEFR.Create;
  Result := FInfoEFR;
end;

function TInfoOp.getInfoEnte: TInfoEnte;
begin
  if Not(Assigned(FInfoEnte)) then
     FInfoEnte := TInfoEnte.Create;
  result := FInfoEnte;
end;

function TInfoOp.InfoEFRInst: Boolean;
begin
  result := Assigned(FInfoEFR);
end;

function TInfoOp.InfoEnteInst: Boolean;
begin
  Result := Assigned(FInfoEnte);
end;

{ TInfoComplementares }

destructor TInfoComplementares.destroy;
begin
  FreeAndNil(FSituacaoPJ);
  FreeAndNil(FSituacaoPF);
  inherited;
end;

function TInfoComplementares.getSituacaoPF: TSituacaoPF;
begin
  if Not(Assigned(FSituacaoPF)) then
    FSituacaoPF := TSituacaoPF.Create;
  Result := FSituacaoPF;
end;

function TInfoComplementares.getSituacaoPJ(): TSituacaoPJ;
begin
  if Not(Assigned(FSituacaoPJ)) then
    FSituacaoPJ := TSituacaoPJ.Create;
  Result := FSituacaoPJ;
end;

function TInfoComplementares.situacaoPFInst: Boolean;
begin
  Result := Assigned(FSituacaoPF);
end;

function TInfoComplementares.situacaoPJInst: Boolean;
begin
  Result := Assigned(FSituacaoPJ);
end;

{ TSoftwareHouseCollection }

function TSoftwareHouseCollection.Add: TSoftwareHouseCollectionItem;
begin
  Result := TSoftwareHouseCollectionItem(inherited add());
  Result.Create;
end;

constructor TSoftwareHouseCollection.create;
begin
  Inherited create(TSoftwareHouseCollectionItem);
end;

function TSoftwareHouseCollection.GetItem(
  Index: Integer): TSoftwareHouseCollectionItem;
begin
  Result := TSoftwareHouseCollectionItem(inherited GetItem(Index));
end;

procedure TSoftwareHouseCollection.SetItem(Index: Integer;
  Value: TSoftwareHouseCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TSoftwareHouseCollectionItem }

constructor TSoftwareHouseCollectionItem.create;
begin

end;

end.
