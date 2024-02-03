{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pcesS2410;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$ELSE}
   Contnrs,
  {$IFEND}
  ACBrBase, pcnConversao, ACBrUtil.Strings,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2410Collection = class;
  TS2410CollectionItem = class;
  TEvtCdBenIn = class;
  TBeneficiario = class;
  TInfoBenInicio = class;
  TDadosBeneficio = class;
  TInfoPenMorte = class;
  TInstPenMorte = class;
  TSucessaoBenef = class;
  TMudancaCPF = class;
  TInfoBenTermino = class;

  TS2410Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2410CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2410CollectionItem);
  public
    function Add: TS2410CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2410CollectionItem;
    property Items[Index: Integer]: TS2410CollectionItem read GetItem write SetItem; default;
  end;

  TS2410CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtCdBenIn : TEvtCdBenIn;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtCdBenIn: TEvtCdBenIn read FEvtCdBenIn write FEvtCdBenIn;
  end;

  TEvtCdBenIn = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FBeneficiario: TBeneficiario;
    FInfoBenInicio: TInfoBenInicio;

    procedure GerarBeneficiario(pBeneficiario: TBeneficiario);
    procedure GerarDadosBeneficio(pDadosBeneficio: TDadosBeneficio);
    procedure GerarInfoBenInicio(pInfoBenInicio: TInfoBenInicio);
    procedure GerarInfoPenMorte(pInfoPenMorte: TInfoPenMorte);
    procedure GerarInfoBenTermino(pinfoBenTermino: TInfoBenTermino);
    procedure GerarInstPenMorte(pInstPenMorte: TInstPenMorte);
    procedure GerarSucessaoBenef(pSucessaoBenef: TSucessaoBenef);
    procedure GerarMudancaCPF(pMudancaCPF: TMudancaCPF);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property Beneficiario: TBeneficiario read FBeneficiario write FBeneficiario;
    property InfoBenInicio: TInfoBenInicio read FInfoBenInicio write FInfoBenInicio;
  end;

  TInfoBenTermino = class(TObject)
  private
    FDtTermBeneficio: TDateTime;
    FMtvTermino: tpMotCessBenef;
  public
    property dtTermBeneficio: TDateTime read FDtTermBeneficio write FDtTermBeneficio;
    property mtvTermino: tpMotCessBenef read FMtvTermino write FMtvTermino;
  end;
  
  TInstPenMorte = class(TObject)
  private
    FCpfInst: string;
    FDtInst: TDateTime;
  public
    property cpfInst: string read FCpfInst write FCpfInst;
    property dtInst: TDateTime read FDtInst write FDtInst;
  end;

  TInfoPenMorte = class(TObject)
  private
    FTpTpPenMorte: tpTpPenMorte;
    FInstPenMorte: TInstPenMorte;
    
    function getInstPenMorte(): TInstPenMorte;
  public
    constructor Create;
    destructor Destroy; override;
    
    function instPenMorteInst(): Boolean;
    
    property tpPenMorte: tpTpPenMorte read FTpTpPenMorte write FTpTpPenMorte;
    property instPenMorte: TInstPenMorte read getInstPenMorte write FInstPenMorte;
  end;

  TSucessaoBenef = class(TObject)
  private
    FCnpjOrgaoAnt: string;
    FNrBeneficioAnt: string;
    FDtTransf: TDateTime;
    FObservacao: string;
  public
    property cnpjOrgaoAnt: string read FCnpjOrgaoAnt write FCnpjOrgaoAnt;
    property nrBeneficioAnt: string read FNrBeneficioAnt write FNrBeneficioAnt;
    property dtTransf: TDateTime read FDtTransf write FDtTransf;
    property observacao: string read FObservacao write FObservacao;
  end;
  
  TMudancaCPF = class(TObject)
  private
    FCpfAnt: string;
    FNrBeneficioAnt: string;
    FDtAltCPF: TDateTime;
    FObservacao: string;
  public
    property cpfAnt: string read FCpfAnt write FCpfAnt;
    property nrBeneficioAnt: string read FNrBeneficioAnt write FNrBeneficioAnt;
    property dtAltCPF: TDateTime read FDtAltCPF write FDtAltCPF;
    property observacao: string read FObservacao write FObservacao;
  end;
  
  TInfoBenInicio = class(TObject)
  private
    FCadIni: tpSimNao;
    FIndSitBenef: tpIndSitBenef;
    FNrBeneficio: string;
    FDtIniBeneficio: TDateTime;
    FDtPublic: TDateTime;
    
    FDadosBeneficio: TDadosBeneficio;
    FSucessaoBenef: TSucessaoBenef;
    FMudancaCPF: TMudancaCPF;
    FInfoBenTermino: TInfoBenTermino;
    
    function getSucessaoBenef: TSucessaoBenef;
    function getMudancaCPF: TMudancaCPF;
    function getInfoBenTermino: TInfoBenTermino;
  public
    constructor Create;
    destructor Destroy; override;

    function infoSucessaoBenefInst(): Boolean;
    function infoMudancaCPFInst(): Boolean;
    function infoBenTerminoInst(): Boolean;
    
    property cadIni: tpSimNao read FCadIni write FCadIni;
    property indSitBenef: tpIndSitBenef read FIndSitBenef write FIndSitBenef;
    property nrBeneficio: string read FNrBeneficio write FNrBeneficio;
    property dtIniBeneficio: TDateTime read FDtIniBeneficio write FDtIniBeneficio;
    property dtPublic: TDateTime read FDtPublic write FDtPublic;
    property dadosBeneficio: TDadosBeneficio read FDadosBeneficio write FDadosBeneficio;
    property sucessaoBenef: TSucessaoBenef read getSucessaoBenef write FSucessaoBenef;
    property mudancaCPF: TMudancaCPF read getMudancaCPF write FMudancaCPF;
    property infoBenTermino: TInfoBenTermino read getInfoBenTermino write FInfoBenTermino;
  end;

  TDadosBeneficio = class(TObject)
  private
    FTpBeneficio: integer;
    FTpPlanRP: tpPlanRP;
    FDsc: String;
    FIndDecJud: TpSimNaoFacultativo;
    
    FInfoPenMorte: TInfoPenMorte;
    
    function getInfoPenMorte(): TInfoPenMorte;
  public
    constructor Create;
    destructor Destroy; override;
    
    function infoPenMorteInst(): Boolean;
    
    property tpBeneficio: integer read FTpBeneficio write FTpBeneficio;
    property tpPlanRP: tpPlanRP read FTpPlanRP write FTpPlanRP;
    property dsc: String read FDsc write FDsc;
    property indDecJud: TpSimNaoFacultativo read FIndDecJud write FIndDecJud;
    property infoPenMorte: TInfoPenMorte read getInfoPenMorte write FInfoPenMorte;
  end;

  TBeneficiario = class(TObject)
  private
    FCpfBenef: string;
    FMatricula: string;
    FCnpjOrigem: string;
  public
    property cpfBenef: String read FCpfBenef write FCpfBenef;
    property matricula: string read FMatricula write FMatricula;
    property cnpjOrigem: string read FCnpjOrigem write FCnpjOrigem;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TS2410Collection }

function TS2410Collection.Add: TS2410CollectionItem;
begin
  Result := Self.New;
end;

function TS2410Collection.GetItem(Index: Integer): TS2410CollectionItem;
begin
  Result := TS2410CollectionItem(inherited Items[Index]);
end;

procedure TS2410Collection.SetItem(Index: Integer; Value: TS2410CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2410Collection.New: TS2410CollectionItem;
begin
  Result := TS2410CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2410CollectionItem }

constructor TS2410CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento   := teS2410;
  FEvtCdBenIn   := TEvtCdBenIn.Create(AOwner);
end;

destructor TS2410CollectionItem.Destroy;
begin
  FreeAndNil(FEvtCdBenIn);

  inherited;
end;

{ TInfoBenInicio }

constructor TInfoBenInicio.Create;
begin
  inherited Create;
  
  FDadosBeneficio := TDadosBeneficio.Create;
  FSucessaoBenef := nil;
  FMudancaCPF := nil;
  FInfoBenTermino := nil;
end;

destructor TInfoBenInicio.Destroy;
begin
  FreeAndNil(FDadosBeneficio);
  
  if infoSucessaoBenefInst() then
    FreeAndNil(FSucessaoBenef);
  if infoMudancaCPFInst() then  
    FreeAndNil(FMudancaCPF);
  if infoBenTerminoInst() then
    FreeAndNil(FInfoBenTermino);
  
  inherited;
end;

function TInfoBenInicio.getSucessaoBenef: TSucessaoBenef;
begin
  if not(Assigned(FSucessaoBenef)) then
    FSucessaoBenef := TSucessaoBenef.Create;
  Result := FSucessaoBenef;
end;

function TInfoBenInicio.getMudancaCPF: TMudancaCPF;
begin
  if not(Assigned(FMudancaCPF)) then
    FMudancaCPF := TMudancaCPF.Create;
  Result := FMudancaCPF;
end;

function TInfoBenInicio.getInfoBenTermino: TInfoBenTermino;
begin
  if not(Assigned(FInfoBenTermino)) then
    FInfoBenTermino := TInfoBenTermino.Create;
  Result := FInfoBenTermino;
end;

function TInfoBenInicio.infoSucessaoBenefInst: Boolean;
begin
  Result := Assigned(FSucessaoBenef);
end;

function TInfoBenInicio.infoMudancaCPFInst: Boolean;
begin
  Result := Assigned(FMudancaCPF);
end;

function TInfoBenInicio.infoBenTerminoInst: Boolean;
begin
  Result := Assigned(FInfoBenTermino);
end;

{ TDadosBeneficio }

constructor TDadosBeneficio.Create;
begin
  inherited Create;

  FInfoPenMorte := nil;
end;

destructor TDadosBeneficio.Destroy;
begin
  if infoPenMorteInst() then
    FreeAndNil(FInfoPenMorte);
  
  inherited;
end;

function TDadosBeneficio.getInfoPenMorte(): TInfoPenMorte;
begin
  if not(Assigned(FInfoPenMorte)) then
    FInfoPenMorte := TInfoPenMorte.Create;
  Result := FInfoPenMorte;
end;

function TDadosBeneficio.infoPenMorteInst: Boolean;
begin
  Result := Assigned(FInfoPenMorte);
end;

{ TInfoPenMorte }

constructor TInfoPenMorte.Create;
begin
  inherited;
  
  FInstPenMorte := nil;
end;

destructor TInfoPenMorte.Destroy;
begin
  if instPenMorteInst() then
    FreeAndNil(FInstPenMorte);
  
  inherited;
end;

function TInfoPenMorte.getInstPenMorte(): TInstPenMorte;
begin
  if not(Assigned(FInstPenMorte)) then
    FInstPenMorte := TInstPenMorte.Create;
  Result := FInstPenMorte;
end;

function TInfoPenMorte.instPenMorteInst(): Boolean;
begin
  Result := Assigned(FInstPenMorte);
end;

{ TEvtCdBenIn }

constructor TEvtCdBenIn.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FBeneficiario  := TBeneficiario.Create;
  FInfoBenInicio := TInfoBenInicio.Create;
end;

destructor TEvtCdBenIn.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FBeneficiario.Free;
  FInfoBenInicio.Free;

  inherited;
end;

procedure TEvtCdBenIn.GerarDadosBeneficio(pDadosBeneficio: TDadosBeneficio);
begin
  Gerador.wGrupo('dadosBeneficio');

  Gerador.wCampo(tcInt, '', 'tpBeneficio', 4,   4, 1, pDadosBeneficio.tpBeneficio);
  Gerador.wCampo(tcStr, '', 'tpPlanRP',    1,   1, 1, eSTpPlanRPToStr(pDadosBeneficio.tpPlanRP));
  Gerador.wCampo(tcStr, '', 'dsc',         0, 255, 0, pDadosBeneficio.dsc);
  
  if pDadosBeneficio.indDecJud <> snfNada then
    Gerador.wCampo(tcStr, '', 'indDecJud', 0,   1, 0, eSSimNaoFacultativoToStr(pDadosBeneficio.indDecJud));

  if pDadosBeneficio.infoPenMorteInst() then
    GerarInfoPenMorte(pDadosBeneficio.infoPenMorte);
  
  Gerador.wGrupo('/dadosBeneficio');
end;

procedure TEvtCdBenIn.GerarBeneficiario(pBeneficiario: TBeneficiario);
begin
  Gerador.wGrupo('beneficiario');

  Gerador.wCampo(tcStr, '', 'cpfBenef',   11, 11, 1, pBeneficiario.cpfBenef);
  Gerador.wCampo(tcStr, '', 'matricula',   0, 30, 0, pBeneficiario.matricula);
  Gerador.wCampo(tcStr, '', 'cnpjOrigem', 14, 14, 0, pBeneficiario.cnpjOrigem);

  Gerador.wGrupo('/beneficiario');
end;

procedure TEvtCdBenIn.GerarSucessaoBenef(pSucessaoBenef: TSucessaoBenef);
begin
  if pSucessaoBenef.cnpjOrgaoAnt = '' then
    Exit;
    
  Gerador.wGrupo('sucessaoBenef');

  Gerador.wCampo(tcStr, '', 'cnpjOrgaoAnt',   14,  14, 1, pSucessaoBenef.cnpjOrgaoAnt);
  Gerador.wCampo(tcStr, '', 'nrBeneficioAnt', 20,  20, 1, pSucessaoBenef.nrBeneficioAnt);
  Gerador.wCampo(tcDat, '', 'dtTransf'      , 10,  10, 1, pSucessaoBenef.dtTransf);
  Gerador.wCampo(tcStr, '', 'observacao'    ,  0, 255, 0, pSucessaoBenef.observacao);
  
  Gerador.wGrupo('/sucessaoBenef');
end;

procedure TEvtCdBenIn.GerarMudancaCPF(pMudancaCPF: TMudancaCPF);
begin
  if pMudancaCPF.cpfAnt = '' then
    Exit;

  Gerador.wGrupo('mudancaCPF');
  
  Gerador.wCampo(tcStr, '', 'cpfAnt',         11,  11, 1, pMudancaCPF.cpfAnt);
  Gerador.wCampo(tcStr, '', 'nrBeneficioAnt', 20,  20, 1, pMudancaCPF.nrBeneficioAnt);
  Gerador.wCampo(tcDat, '', 'dtAltCPF'      , 10,  10, 1, pMudancaCPF.dtAltCPF);
  Gerador.wCampo(tcStr, '', 'observacao'    ,  0, 255, 0, pMudancaCPF.observacao);
  
  Gerador.wGrupo('/mudancaCPF');
end;

procedure TEvtCdBenIn.GerarInstPenMorte(pInstPenMorte: TInstPenMorte);
begin
  if pInstPenMorte.cpfInst = '' then
    Exit;
    
  Gerador.wGrupo('instPenMorte');
  
  Gerador.wCampo(tcStr, '', 'cpfInst',   11, 11, 1, pInstPenMorte.cpfInst);
  Gerador.wCampo(tcDat, '', 'dtInst'   , 10, 10, 1, pInstPenMorte.dtInst);
  
  Gerador.wGrupo('/instPenMorte');
end;

procedure TEvtCdBenIn.GerarInfoPenMorte(pInfoPenMorte: TInfoPenMorte);
begin
  if pInfoPenMorte.tpPenMorte = pmNada then
    Exit;
    
  Gerador.wGrupo('infoPenMorte');

  Gerador.wCampo(tcStr, '', 'tpPenMorte',  1,  1, 1, eStpTpPenMorteToStrEX(pInfoPenMorte.tpPenMorte));

  GerarInstPenMorte(pInfoPenMorte.instPenMorte);
    
  Gerador.wGrupo('/infoPenMorte');
end;

procedure TEvtCdBenIn.GerarInfoBenTermino(pInfoBenTermino: TInfoBenTermino);
begin
  if (pInfoBenTermino.mtvTermino = tmcbNenhum) or 
     (pInfoBenTermino.mtvTermino = tmcbTransferenciaDeOrgaoAdministrador) or 
     (pInfoBenTermino.mtvTermino = tmcbMudancaDeCPFDoBeneficiario) then
    Exit;

  Gerador.wGrupo('infoBenTermino');

  Gerador.wCampo(tcDat, '', 'dtTermBeneficio', 10, 10, 1, pInfoBenTermino.dtTermBeneficio);
  Gerador.wCampo(tcInt, '', 'mtvTermino',       2,  2, 1, eStpTpMotCessBenefToStrEX(pInfoBenTermino.mtvTermino));

  Gerador.wGrupo('/infoBenTermino');
end;

procedure TEvtCdBenIn.GerarInfoBenInicio(pInfoBenInicio: TInfoBenInicio);
begin
  Gerador.wGrupo('infoBenInicio');

  Gerador.wCampo(tcStr, '', 'cadIni',          1, 1, 1, eSSimNaoToStr(pInfoBenInicio.cadIni));
  
  if pInfoBenInicio.IndSitBenef <> tpisbNenhum then
    Gerador.wCampo(tcStr, '', 'indSitBenef',   1, 1, 1, pInfoBenInicio.indSitBenef);
    
  Gerador.wCampo(tcStr, '', 'nrBeneficio',     1, 20, 1, pInfoBenInicio.nrBeneficio);
  Gerador.wCampo(tcDat, '', 'dtIniBeneficio', 10, 10, 1, pInfoBenInicio.dtIniBeneficio);
  Gerador.wCampo(tcDat, '', 'dtPublic',        0, 10, 0, pInfoBenInicio.dtPublic);

  GerarDadosBeneficio(pInfoBenInicio.dadosBeneficio);
  
  if pInfoBenInicio.infoSucessaoBenefInst() then
    GerarSucessaoBenef(pInfoBenInicio.SucessaoBenef);
    
  if pInfoBenInicio.infoMudancaCPFInst() then
    GerarMudancaCPF(pInfoBenInicio.mudancaCPF);
    
  if pInfoBenInicio.infoBenTerminoInst() then
    GerarInfoBenTermino(pInfoBenInicio.infoBenTermino);  
  
  Gerador.wGrupo('/infoBenInicio');
end;

function TEvtCdBenIn.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtCdBenIn');
    Gerador.wGrupo('evtCdBenIn Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarBeneficiario(self.Beneficiario);
    GerarInfoBenInicio(self.InfoBenInicio);

    Gerador.wGrupo('/evtCdBenIn');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'EvtCdBenIn');

//    Validar(schEvtCdBenIn);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtCdBenIn.LerArqIni(const AIniString: String): Boolean;
//var
//  INIRec: TMemIniFile;
//  Ok: Boolean;
//  sSecao: String;
begin
  Result := True;
end;

end.
