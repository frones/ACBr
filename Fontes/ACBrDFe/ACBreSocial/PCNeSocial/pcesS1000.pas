{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Jean Carlo Cantu                                }
{                              Tiago Ravache                                   }
{                              Guilherme Costa                                 }
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

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}

{$I ACBr.inc}

unit pcesS1000;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  ACBrDFeConsts,
  pcnConversao, pcnGerador,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS1000CollectionItem = class;
  TevtInfoEmpregador = class;

  {Classes específicas deste evento}
  TInfoEmpregador = class;
  TInfoCadastro = class;
  TDadosIsencao = class;
  TInfoOrgInternacional = class;
  TSoftwareHouseCollection = class;
  TSoftwareHouseCollectionItem = class;
  TInfoEFR = class;
  TInfoOP = class;
  TInfoEnte = class;

  TS1000Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1000CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1000CollectionItem);
  public
    function Add: TS1000CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1000CollectionItem;
    property Items[Index: Integer]: TS1000CollectionItem read GetItem write SetItem; default;
  end;

  TS1000CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FevtInfoEmpregador: TevtInfoEmpregador;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtInfoEmpregador: TevtInfoEmpregador read FevtInfoEmpregador write FevtInfoEmpregador;
  end;

  TevtInfoEmpregador = class(TeSocialEvento) //Classe do elemento principal do XML do evento!
  private
    FModoLancamento: TModoLancamento;
    FIdeEvento: TIdeEvento;
    FIdeEmpregador: TIdeEmpregador;
    FInfoEmpregador: TInfoEmpregador;

    {Geradores específicos desta classe}
    procedure GerarInfoCadastro;
    procedure GerarDadosIsencao;
    procedure GerarContato;
    procedure GerarInfoOp;
    procedure GerarInfoEFR;
    procedure GerarInfoEnte;
    procedure GerarInfoOrgInternacional;
    procedure GerarSoftwareHouse;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: Boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property ideEvento: TIdeEvento read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property infoEmpregador: TInfoEmpregador read FInfoEmpregador write FInfoEmpregador;
  end;

  TInfoEmpregador = class(TObject)
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

  TInfoCadastro = class(TObject)
  private
    FNmRazao: String;
    FNatJurid: String;
    FClassTrib: TpClassTrib;
    FIndCoop: TpIndCoop;
    FIndConstr: TpIndConstr;
    FIndDesFolha: TpIndDesFolha;
    FIndOptRegEletron: TpIndOptRegEletron;
    FIndOpcCP: TpIndOpcCP;
    FIndPorte: tpSimNao;
    FIndEntEd: tpSimNaoFacultativo;
    FIndEtt: tpSimNaoFacultativo;
    FNrRegEtt: String;
    FDadosIsencao: TDadosIsencao;
    FContato: TContato;
    FInfoOp: TInfoOp;
    FInfoOrgInternacional: TInfoOrgInternacional;
    FSoftwareHouse: TSoftwareHouseCollection;
    FcnpjEFR: String;
    FdtTrans11096: TDatetime;
    FIndTribFolhaPisCofins: TpSimNaoFacultativo;

    function getDadosIsencao(): TDadosIsencao;
    function getInfoOrgInternacional(): TInfoOrgInternacional;
  public
    constructor Create;
    destructor Destroy; override;

    function infoOrgInternacionalInst(): Boolean;
    function dadosIsencaoInst(): Boolean;
    function infoOpInst(): Boolean;

    property NmRazao: String read FNmRazao write FNmRazao;
    property NatJurid: String read FNatJurid write FNatJurid;
    property ClassTrib: TpClassTrib read FClassTrib write FClassTrib;
    property IndCoop: TpIndCoop read FIndCoop write FIndCoop;
    property IndConstr: TpIndConstr read FIndConstr write FIndConstr;
    property IndDesFolha: TpIndDesFolha read FIndDesFolha write FIndDesFolha;
    property IndOptRegEletron: TpIndOptRegEletron read FIndOptRegEletron write FIndOptRegEletron;
    property cnpjEFR: String read FcnpjEFR write FcnpjEFR;
    property IndOpcCP: TpIndOpcCP read FIndOpcCP write FIndOpcCP;
    property IndPorte: tpSimNao read FIndPorte write FIndPorte;
    property IndEntEd: tpSimNaoFacultativo read FIndEntEd write FIndEntEd;
    property IndEtt: tpSimNaoFacultativo read FIndEtt write FIndEtt;
    property nrRegEtt: String read FNrRegEtt write FNrRegEtt;
    property DadosIsencao: TDadosIsencao read getDadosIsencao write FDadosIsencao;
    property Contato: TContato read FContato write FContato;
    property InfoOp: TInfoOp read FInfoOp write FInfoOp;
    property InfoOrgInternacional: TInfoOrgInternacional read getInfoOrgInternacional write FInfoOrgInternacional;
    property SoftwareHouse: TSoftwareHouseCollection read FSoftwareHouse write FSoftwareHouse;
    property dtTrans11096 : TDatetime read FdtTrans11096 write FdtTrans11096;
    property indTribFolhaPisCofins: tpSimNaoFacultativo read FIndTribFolhaPisCofins write FIndTribFolhaPisCofins default snfNao;
  end;

  TDadosIsencao = class(TObject)
  private
    FIdeMinLei: String;
    FNrCertif: String;
    FDtEmisCertif : TDateTime;
    FDtVencCertif: TDateTime;
    FNrProtRenov: String;
    FDtProtRenov: TDateTime;
    FDtDou: TDateTime;
    FPagDou: String;
  public
    property IdeMinLei: String read FIdeMinLei write FIdeMinLei;
    property NrCertif: String read FNrCertif write FNrCertif;
    property DtEmisCertif: TDateTime read FDtEmisCertif write FDtEmisCertif;
    property DtVencCertif: TDateTime read FDtVencCertif write FDtVencCertif;
    property NrProtRenov: String read FNrProtRenov write FNrProtRenov;
    property DtProtRenov: TDateTime read FDtProtRenov write FDtProtRenov;
    property DtDou: TDateTime read FDtDou write FDtDou;
    property PagDou: String read FPagDou write FPagDou;
  end;

  TInfoOrgInternacional = class(TObject)
  private
    FIndAcordoIsenMulta: tpIndAcordoIsencaoMulta;
  public
    property IndAcordoIsenMulta: tpIndAcordoIsencaoMulta read FIndAcordoIsenMulta write FIndAcordoIsenMulta;
  end;

  TSoftwareHouseCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TSoftwareHouseCollectionItem;
    procedure SetItem(Index: Integer; Value: TSoftwareHouseCollectionItem);
  public
    function Add: TSoftwareHouseCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TSoftwareHouseCollectionItem;
    property Items[Index: Integer]: TSoftwareHouseCollectionItem read GetItem write SetItem; default;
  end;

  TSoftwareHouseCollectionItem = class(TObject)
  private
    FCnpjSoftHouse: String;
    FNmRazao: String;
    FNmCont: String;
    FTelefone: String;
    Femail: String;
  public
    property CnpjSoftHouse: String read FCnpjSoftHouse write FCnpjSoftHouse;
    property NmRazao: String read FNmRazao write FNmRazao;
    property NmCont: String read FNmCont write FNmCont;
    property Telefone: String read FTelefone write FTelefone;
    property email: String read Femail write Femail;
  end;

  TInfoEFR = class(TObject)
  private
     FideEFR: tpSimNao;
     FcnpjEFR: String;
  public
    property ideEFR: tpSimNao read FideEFR write FideEFR;
    property cnpjEFR: String read FcnpjEFR write FcnpjEFR;
  end;

  TInfoEnte = class(TObject)
  private
    FNmEnte: String;
    FUf: string;
    FCodMunic: Integer;
    FIndRPPS: tpSimNao;
    FVrSubTeto: Double;
  public
    property nmEnte: String read FNmEnte write FNmEnte;
    property uf: string read FUf write FUf;
    property codMunic: Integer read FCodMunic write FCodMunic;
    property indRPPS: tpSimNao read FIndRPPS write FIndRPPS;
    property vrSubteto: Double read FVrSubTeto write FVrSubTeto;
  end;

  TInfoOp = class(TObject)
  private
     FNrSiafi: String;
     FInfoEFR: TInfoEFR;
     FInfoEnte: TInfoEnte;
  public
    constructor Create;
    destructor Destroy; override;

    function InfoEFRInst(): Boolean;
    function InfoEnteInst(): Boolean;

    property nrSiafi: String read FNrSiafi write FNrSiafi;
    property infoEFR: TInfoEFR read FInfoEFR write FInfoEFR;
    property infoEnte: TInfoEnte read FInfoEnte write FInfoEnte;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS1000Collection }

function TS1000Collection.Add: TS1000CollectionItem;
begin
  Result := Self.New;
end;

function TS1000Collection.GetItem(Index: Integer): TS1000CollectionItem;
begin
  Result := TS1000CollectionItem(inherited Items[Index]);
end;

procedure TS1000Collection.SetItem(Index: Integer; Value: TS1000CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1000Collection.New: TS1000CollectionItem;
begin
  Result := TS1000CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS1000CollectionItem }

constructor TS1000CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento        := teS1000;
  FevtInfoEmpregador := TevtInfoEmpregador.Create(AOwner);
end;

destructor TS1000CollectionItem.Destroy;
begin
  FevtInfoEmpregador.Free;
  inherited;
end;

{ TevtInfoEmpregador }

constructor TevtInfoEmpregador.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEmpregador  := TIdeEmpregador.create;
  FIdeEvento      := TIdeEvento.create;
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
  if (Self.infoEmpregador.infoCadastro.Contato.NmCtt <> '') or (VersaoDF < ve02_05_00) then
  begin
    Gerador.wGrupo('contato');

    Gerador.wCampo(tcStr, '', 'nmCtt',     1, 70, 1, Self.infoEmpregador.infoCadastro.Contato.NmCtt);
    Gerador.wCampo(tcStr, '', 'cpfCtt',   11, 11, 1, Self.infoEmpregador.infoCadastro.Contato.CpfCtt);
    Gerador.wCampo(tcStr, '', 'foneFixo',  1, 13, 0, Self.infoEmpregador.infoCadastro.Contato.FoneFixo);
    Gerador.wCampo(tcStr, '', 'foneCel',   1, 13, 0, Self.infoEmpregador.infoCadastro.Contato.FoneCel);
    Gerador.wCampo(tcStr, '', 'email',     1, 60, 0, Self.infoEmpregador.infoCadastro.Contato.email);

    Gerador.wGrupo('/contato');
  end;
end;

procedure TevtInfoEmpregador.GerarInfoEFR;
begin
  if infoEmpregador.infoCadastro.InfoOp.InfoEFRInst() and (infoEmpregador.infoCadastro.InfoOp.infoEFR.cnpjEFR <> EmptyStr) then
  begin
    Gerador.wGrupo('infoEFR');

    Gerador.wCampo(tcStr, '', 'ideEFR',   1,  1, 1, eSSimNaoToStr(infoEmpregador.infoCadastro.InfoOp.infoEFR.ideEFR));
    Gerador.wCampo(tcStr, '', 'cnpjEFR', 14, 14, 0, infoEmpregador.infoCadastro.InfoOp.infoEFR.cnpjEFR);

    Gerador.wGrupo('/infoEFR')
  end;
end;

procedure TevtInfoEmpregador.GerarInfoEnte;
begin
  if infoEmpregador.infoCadastro.InfoOp.InfoEnteInst() and (infoEmpregador.infoCadastro.InfoOp.infoEnte.nmEnte <> EmptyStr) then
  begin
    Gerador.wGrupo('infoEnte');

    Gerador.wCampo(tcStr, '', 'nmEnte',    1, 100, 1, infoEmpregador.infoCadastro.InfoOp.infoEnte.nmEnte);
    Gerador.wCampo(tcStr, '', 'uf',        2,   2, 1, infoEmpregador.infoCadastro.InfoOp.infoEnte.uf);
    Gerador.wCampo(tcInt, '', 'codMunic',  7,   7, 0, infoEmpregador.infoCadastro.InfoOp.infoEnte.codMunic);
    Gerador.wCampo(tcStr, '', 'indRPPS',   1,   1, 1, eSSimNaoToStr(infoEmpregador.infoCadastro.InfoOp.infoEnte.indRPPS));
    Gerador.wCampo(tcDe2, '', 'vrSubteto', 1,  14, 1, infoEmpregador.infoCadastro.InfoOp.infoEnte.vrSubteto);

    Gerador.wGrupo('/infoEnte');
  end;
end;

procedure TevtInfoEmpregador.GerarInfoOp;
begin
  //if infoEmpregador.infoCadastro.infoOpInst() and (infoEmpregador.infoCadastro.InfoOp.nrSiafi <> EmptyStr) then
  if infoEmpregador.infoCadastro.infoOpInst() and (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico) then
  begin
    Gerador.wGrupo('infoOP');

    Gerador.wCampo(tcStr, '', 'nrSiafi', 1, 6, 1, infoEmpregador.infoCadastro.InfoOp.nrSiafi);

    GerarInfoEFR;
    GerarInfoEnte;

    Gerador.wGrupo('/infoOP');
  end;
end;

procedure TevtInfoEmpregador.GerarDadosIsencao;
begin
  if infoEmpregador.infoCadastro.dadosIsencaoInst() then
  begin
    Gerador.wGrupo('dadosIsencao');

    Gerador.wCampo(tcStr, '', 'ideMinLei',     1, 70, 1, infoEmpregador.infoCadastro.DadosIsencao.IdeMinLei);
    Gerador.wCampo(tcStr, '', 'nrCertif',      1, 40, 1, infoEmpregador.infoCadastro.DadosIsencao.NrCertif);
    Gerador.wCampo(tcDat, '', 'dtEmisCertif', 10, 10, 1, infoEmpregador.infoCadastro.DadosIsencao.DtEmisCertif);
    Gerador.wCampo(tcDat, '', 'dtVencCertif', 10, 10, 1, infoEmpregador.infoCadastro.DadosIsencao.DtVencCertif);
    Gerador.wCampo(tcStr, '', 'nrProtRenov',   0, 40, 0, infoEmpregador.infoCadastro.DadosIsencao.NrProtRenov);

    if (DateToStr(infoEmpregador.infoCadastro.DadosIsencao.DtProtRenov) <> dDataBrancoNula) then
      Gerador.wCampo(tcDat, '', 'dtProtRenov', 10, 10, 0, infoEmpregador.infoCadastro.DadosIsencao.DtProtRenov);

    if (DateToStr(infoEmpregador.infoCadastro.DadosIsencao.DtDou) <> dDataBrancoNula) then
      Gerador.wCampo(tcDat, '', 'dtDou', 10, 10, 0, infoEmpregador.infoCadastro.DadosIsencao.DtDou);

    Gerador.wCampo(tcStr, '', 'pagDou', 1, 5, 0, infoEmpregador.infoCadastro.DadosIsencao.PagDou); // Não deveria ser do tipo Integer

    Gerador.wGrupo('/dadosIsencao');
  end;
end;

procedure TevtInfoEmpregador.GerarInfoCadastro;
begin
  Gerador.wGrupo('infoCadastro');

  if VersaoDF <= ve02_05_00 then
    Gerador.wCampo(tcStr, '', 'nmRazao',          1, 100, 1, Self.infoEmpregador.infoCadastro.NmRazao);
  Gerador.wCampo(tcStr, '', 'classTrib',        2, 2, 1, tpClassTribToStr(Self.infoEmpregador.infoCadastro.ClassTrib));

  if (Self.ideEmpregador.TpInsc = tiCNPJ) then
  begin
    if VersaoDF <= ve02_05_00 then
      Gerador.wCampo(tcStr, '', 'natJurid',       4, 4, 0, Self.infoEmpregador.infoCadastro.NatJurid); // criar enumerador

    Gerador.wCampo(tcStr, '', 'indCoop',        1, 1, 0, eSIndCooperativaToStr(Self.infoEmpregador.infoCadastro.IndCoop));
    Gerador.wCampo(tcStr, '', 'indConstr',      1, 1, 0, eSIndConstrutoraToStr(Self.infoEmpregador.infoCadastro.IndConstr));
  end;

  Gerador.wCampo(tcStr, '', 'indDesFolha',      1, 1, 1, eSIndDesFolhaToStr(Self.infoEmpregador.infoCadastro.IndDesFolha));

  if (VersaoDF >= ve02_05_00) and (Self.infoEmpregador.infoCadastro.ClassTrib in [ct07, ct08, ct21]) and
      (Self.infoEmpregador.infoCadastro.IndOpcCP <> icpNenhum) then
    Gerador.wCampo(tcStr, '', 'indOpcCP', 1, 1, 0, eSIndOpcCPToStr(Self.infoEmpregador.infoCadastro.IndOpcCP));

  if (VersaoDF >= ve02_05_00) and (Not (Self.infoEmpregador.infoCadastro.ClassTrib in [ct21, ct22])) then
    if (Self.infoEmpregador.infoCadastro.IndPorte = tpSim) then //Somente empresas que não são (ME e EPP)
      Gerador.wCampo(tcStr, '', 'indPorte',       1, 1, 0, eSSimNaoToStr(Self.infoEmpregador.infoCadastro.IndPorte));

  Gerador.wCampo(tcStr, '', 'indOptRegEletron', 1, 1, 1, eSIndOptRegEletronicoToStr(Self.infoEmpregador.infoCadastro.IndOptRegEletron));

  if VersaoDF <= ve02_05_00 then
  begin
    if (Self.ideEmpregador.TpInsc = tiCNPJ) then
      Gerador.wCampo(tcStr, '', 'indEntEd',       0, 1, 0, eSSimNaoFacultativoToStr(Self.infoEmpregador.infoCadastro.IndEntEd));

    Gerador.wCampo(tcStr, '', 'indEtt',           0, 1, 0, eSSimNaoFacultativoToStr(Self.infoEmpregador.infoCadastro.IndEtt));
    Gerador.wCampo(tcStr, '', 'nrRegEtt',         0, 30, 0, Self.infoEmpregador.infoCadastro.nrRegEtt);
    GerarContato;
    GerarInfoOp;
  end
  else
  begin
    if Self.infoEmpregador.infoCadastro.cnpjEFR <> '' then
      Gerador.wCampo(tcStr, '', 'cnpjEFR', 14, 14, 0, Self.infoEmpregador.infoCadastro.cnpjEFR);

    if DateToStr(infoEmpregador.infoCadastro.dtTrans11096) <> dDataBrancoNula then
      Gerador.wCampo(tcDat, '', 'dtTrans11096', 10, 10, 0, infoEmpregador.infoCadastro.dtTrans11096);

    if (VersaoDF >= veS01_01_00) and (infoEmpregador.infoCadastro.indTribFolhaPisCofins = snfSim) then
      Gerador.wCampo(tcStr, '', 'indTribFolhaPisCofins',  0, 1, 0, eSSimNaoFacultativoToStr(Self.infoEmpregador.infoCadastro.indTribFolhaPisCofins));
  end;

  GerarDadosIsencao;
  GerarInfoOrgInternacional;

  if VersaoDF <= ve02_05_00 then
  begin
    GerarSoftwareHouse;
  end;

  Gerador.wGrupo('/infoCadastro');
end;

procedure TevtInfoEmpregador.GerarInfoOrgInternacional;
begin
  if infoEmpregador.infoCadastro.infoOrgInternacionalInst() then
  begin
    Gerador.wGrupo('infoOrgInternacional');

    Gerador.wCampo(tcStr, '', 'indAcordoIsenMulta', 1, 1, 1, eSIndAcordoIsencaoMultaToStr(infoEmpregador.infoCadastro.InfoOrgInternacional.IndAcordoIsenMulta));

    Gerador.wGrupo('/infoOrgInternacional');
  end;
end;

procedure TevtInfoEmpregador.GerarSoftwareHouse;
var
  i: Integer;
begin
  for i := 0 to infoEmpregador.infoCadastro.SoftwareHouse.Count - 1 do
  begin
    Gerador.wGrupo('softwareHouse');

    Gerador.wCampo(tcStr, '', 'cnpjSoftHouse', 14,  14, 1, infoEmpregador.infoCadastro.SoftwareHouse[i].CnpjSoftHouse);
    Gerador.wCampo(tcStr, '', 'nmRazao',        1, 100, 1, infoEmpregador.infoCadastro.SoftwareHouse[i].NmRazao);
    Gerador.wCampo(tcStr, '', 'nmCont',         1,  70, 1, infoEmpregador.infoCadastro.SoftwareHouse[i].NmCont);
    Gerador.wCampo(tcStr, '', 'telefone',       1,  13, 1, infoEmpregador.infoCadastro.SoftwareHouse[i].Telefone);
    Gerador.wCampo(tcStr, '', 'email',          1,  60, 0,  infoEmpregador.infoCadastro.SoftwareHouse[i].email);

    Gerador.wGrupo('/softwareHouse');
  end;

  if infoEmpregador.infoCadastro.SoftwareHouse.Count > 99 then
    Gerador.wAlerta('', 'softwareHouse', 'Lista de Software House', ERR_MSG_MAIOR_MAXIMO + '99');
end;

function TevtInfoEmpregador.GerarXML: Boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtInfoEmpregador');
    Gerador.wGrupo('evtInfoEmpregador Id="' + Self.Id + '"');

    GerarIdeEvento(Self.IdeEvento);
    GerarIdeEmpregador(Self.IdeEmpregador);

    Gerador.wGrupo('infoEmpregador');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdePeriodo(Self.infoEmpregador.idePeriodo);

    if (Self.ModoLancamento <> mlExclusao) then
    begin
      if ((VersaoDF <= veS01_01_00) or
          (Self.infoEmpregador.infoCadastro.ClassTrib <> ct00) or
          (Self.ideEvento.ProcEmi <> peAplicGovEvtJud)) then
        GerarInfoCadastro;
      if ModoLancamento = mlAlteracao then
        if (InfoEmpregador.novaValidadeInst()) then
          GerarIdePeriodo(InfoEmpregador.novaValidade, 'novaValidade');
    end;

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoEmpregador');
    Gerador.wGrupo('/evtInfoEmpregador');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtInfoEmpregador');

//    Validar(schevtInfoEmpregador);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '');
end;

function TevtInfoEmpregador.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtInfoEmpregador';
      Id             := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial     := INIRec.ReadInteger(sSecao, 'Sequencial', 0);
      ModoLancamento := eSStrToModoLancamento(Ok, INIRec.ReadString(sSecao, 'ModoLancamento', 'inclusao'));

      sSecao := 'ideEvento';
      ideEvento.ProcEmi := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'idePeriodo';
      infoEmpregador.idePeriodo.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
      infoEmpregador.idePeriodo.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);

      if (ModoLancamento <> mlExclusao) then
      begin
        sSecao := 'infoCadastro';
        infoEmpregador.infoCadastro.NmRazao               := INIRec.ReadString(sSecao, 'nmRazao', EmptyStr);
        infoEmpregador.infoCadastro.ClassTrib             := StrTotpClassTrib(Ok, INIRec.ReadString(sSecao, 'classTrib', '00'));
        infoEmpregador.infoCadastro.NatJurid              := INIRec.ReadString(sSecao, 'natJurid', EmptyStr);
        infoEmpregador.infoCadastro.IndCoop               := eSStrToIndCooperativa(Ok, INIRec.ReadString(sSecao, 'indCoop', '0'));
        infoEmpregador.infoCadastro.IndConstr             := eSStrToIndConstrutora(Ok, INIRec.ReadString(sSecao, 'indConstr', '0'));
        infoEmpregador.infoCadastro.IndDesFolha           := eSStrToIndDesFolha(Ok, INIRec.ReadString(sSecao, 'indDesFolha', '0'));
        infoEmpregador.infoCadastro.IndOpcCP              := eSStrToIndOpcCP(Ok, INIRec.ReadString(sSecao, 'indOpcCP', '0'));
        infoEmpregador.infoCadastro.IndPorte              := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indPorte', 'S'));
        infoEmpregador.infoCadastro.IndOptRegEletron      := eSStrToIndOptRegEletronico(Ok, INIRec.ReadString(sSecao, 'indOptRegEletron', '0'));
        infoEmpregador.infoCadastro.IndEntEd              := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indEntEd', 'S'));
        infoEmpregador.infoCadastro.IndEtt                := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indEtt', 'S'));
        infoEmpregador.infoCadastro.nrRegEtt              := INIRec.ReadString(sSecao, 'nrRegEtt', EmptyStr);
        infoEmpregador.infoCadastro.cnpjEFR               := INIRec.ReadString(sSecao, 'cnpjEFR', EmptyStr);
        infoEmpregador.infoCadastro.indTribFolhaPisCofins := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'indTribFolhaPisCofins', ''));

        sSecao := 'dadosIsencao';
        if INIRec.ReadString(sSecao, 'ideMinLei', '') <> '' then
        begin
          infoEmpregador.infoCadastro.DadosIsencao.IdeMinLei    := INIRec.ReadString(sSecao, 'ideMinLei', EmptyStr);
          infoEmpregador.infoCadastro.DadosIsencao.nrCertif     := INIRec.ReadString(sSecao, 'nrCertif', EmptyStr);
          infoEmpregador.infoCadastro.DadosIsencao.dtEmisCertif := StringToDateTime(INIRec.ReadString(sSecao, 'dtEmisCertif', '0'));
          infoEmpregador.infoCadastro.DadosIsencao.dtVencCertif := StringToDateTime(INIRec.ReadString(sSecao, 'dtVencCertif', '0'));
          infoEmpregador.infoCadastro.DadosIsencao.nrProtRenov  := INIRec.ReadString(sSecao, 'nrProtRenov', EmptyStr);
          infoEmpregador.infoCadastro.DadosIsencao.dtProtRenov  := StringToDateTime(INIRec.ReadString(sSecao, 'dtProtRenov', '0'));
          infoEmpregador.infoCadastro.DadosIsencao.dtDou        := StringToDateTime(INIRec.ReadString(sSecao, 'dtDou', '0'));
          infoEmpregador.infoCadastro.DadosIsencao.pagDou       := INIRec.ReadString(sSecao, 'pagDou', EmptyStr);
        end;

        sSecao := 'contato';
        infoEmpregador.infoCadastro.Contato.NmCtt    := INIRec.ReadString(sSecao, 'nmCtt', EmptyStr);
        infoEmpregador.infoCadastro.Contato.cpfCtt   := INIRec.ReadString(sSecao, 'cpfCtt', EmptyStr);
        infoEmpregador.infoCadastro.Contato.foneFixo := INIRec.ReadString(sSecao, 'foneFixo', EmptyStr);
        infoEmpregador.infoCadastro.Contato.foneCel  := INIRec.ReadString(sSecao, 'foneCel', EmptyStr);
        infoEmpregador.infoCadastro.Contato.email    := INIRec.ReadString(sSecao, 'email', EmptyStr);

        sSecao := 'infoOP';
        if INIRec.ReadString(sSecao, 'nrSiafi', '') <> '' then
        begin
          infoEmpregador.infoCadastro.InfoOp.nrSiafi := INIRec.ReadString(sSecao, 'nrSiafi', EmptyStr);

          sSecao := 'infoEFR';
          if INIRec.ReadString(sSecao, 'ideEFR', '') <> '' then
          begin
            infoEmpregador.infoCadastro.InfoOp.infoEFR.ideEFR  := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'ideEFR', 'S'));
            infoEmpregador.infoCadastro.InfoOp.infoEFR.cnpjEFR := INIRec.ReadString(sSecao, 'cnpjEFR', EmptyStr);
          end;

          sSecao := 'infoEnte';
          if INIRec.ReadString(sSecao, 'nmEnte', '') <> '' then
          begin
            infoEmpregador.infoCadastro.InfoOp.infoEnte.nmEnte    := INIRec.ReadString(sSecao, 'nmEnte', EmptyStr);
            infoEmpregador.infoCadastro.InfoOp.infoEnte.uf        := INIRec.ReadString(sSecao, 'uf', 'SP');
            infoEmpregador.infoCadastro.InfoOp.infoEnte.codMunic  := INIRec.ReadInteger(sSecao, 'codMunic', 0);
            infoEmpregador.infoCadastro.InfoOp.infoEnte.indRPPS   := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indRPPS', 'S'));
            infoEmpregador.infoCadastro.InfoOp.infoEnte.vrSubteto := StringToFloatDef(INIRec.ReadString(sSecao, 'vrSubteto', ''), 0);
          end;
        end;

        sSecao := 'infoOrgInternacional';
        if INIRec.ReadString(sSecao, 'indAcordoIsenMulta', '') <> '' then
          infoEmpregador.infoCadastro.InfoOrgInternacional.IndAcordoIsenMulta := eSStrToIndAcordoIsencaoMulta(Ok, INIRec.ReadString(sSecao, 'indAcordoIsenMulta', '0'));

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'softwareHouse' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'cnpjSoftHouse', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoEmpregador.infoCadastro.SoftwareHouse.New do
          begin
            CnpjSoftHouse := sFim;
            nmRazao       := INIRec.ReadString(sSecao, 'nmRazao', '');
            nmCont        := INIRec.ReadString(sSecao, 'nmCont', '');
            telefone      := INIRec.ReadString(sSecao, 'telefone', '');
            email         := INIRec.ReadString(sSecao, 'email', '');
          end;

          Inc(I);
        end;

        if ModoLancamento = mlAlteracao then
        begin
          sSecao := 'novaValidade';
          infoEmpregador.novaValidade.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
          infoEmpregador.novaValidade.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);
        end;
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
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
  inherited Create;
  FDadosIsencao         := nil;
  FContato              := TContato.Create;
  FInfoOrgInternacional := nil;
  FSoftwareHouse        := TSoftwareHouseCollection.Create;
  FInfoOp               := TInfoOp.Create;
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
  FInfoOp.Free;

  inherited;
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
  inherited Create;
  FInfoEFR  := TInfoEFR.Create;
  FInfoEnte := TInfoEnte.Create;
end;

destructor TInfoOp.Destroy;
begin
  FInfoEFR.Free;
  FInfoEnte.Free;
  inherited;
end;

function TInfoOp.InfoEFRInst: Boolean;
begin
  result := Assigned(FInfoEFR);
end;

function TInfoOp.InfoEnteInst: Boolean;
begin
  Result := Assigned(FInfoEnte);
end;

{ TSoftwareHouseCollection }

function TSoftwareHouseCollection.Add: TSoftwareHouseCollectionItem;
begin
  Result := Self.New;
end;

function TSoftwareHouseCollection.GetItem(
  Index: Integer): TSoftwareHouseCollectionItem;
begin
  Result := TSoftwareHouseCollectionItem(inherited Items[Index]);
end;

procedure TSoftwareHouseCollection.SetItem(Index: Integer;
  Value: TSoftwareHouseCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TSoftwareHouseCollection.New: TSoftwareHouseCollectionItem;
begin
  Result := TSoftwareHouseCollectionItem.Create;
  Self.Add(Result);
end;

end.
