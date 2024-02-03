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

unit pcesS2416;

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
  TS2416Collection = class;
  TS2416CollectionItem = class;
  TEvtCdBenAlt = class;
  TIdeBeneficio = class;
  TInfoBenAlteracao = class;
  TDadosBeneficio = class;
  TInfoPenMorte = class;
  TSuspensao = class;

  TS2416Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2416CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2416CollectionItem);
  public
    function Add: TS2416CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2416CollectionItem;
    property Items[Index: Integer]: TS2416CollectionItem read GetItem write SetItem; default;
  end;

  TS2416CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtCdBenAlt : TEvtCdBenAlt;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtCdBenAlt: TEvtCdBenAlt read FEvtCdBenAlt write FEvtCdBenAlt;
  end;

  TEvtCdBenAlt = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeBeneficio: TIdeBeneficio;
    FInfoBenAlteracao: TInfoBenAlteracao;

    procedure GerarIdeBeneficio(pIdeBeneficio: TIdeBeneficio);
    procedure GerarInfoBenAlteracao(pInfoBenAlteracao: TInfoBenAlteracao);
    procedure GerarDadosBeneficio(pDadosBeneficio: TDadosBeneficio);
    procedure GerarInfoPenMorte(pInfoPenMorte: TInfoPenMorte);
    procedure GerarSuspensao(pSuspensao: TSuspensao);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeBeneficio: TIdeBeneficio read FIdeBeneficio write FIdeBeneficio;
    property InfoBenAlteracao: TInfoBenAlteracao read FInfoBenAlteracao write FInfoBenAlteracao;
  end;

  TIdeBeneficio = class(TObject)
  private
    FCpfBenef: string;
    FNrBeneficio: string;
  public
    property cpfBenef: String read FCpfBenef write FCpfBenef;
    property nrBeneficio: string read FNrBeneficio write FNrBeneficio;
  end;

  TInfoBenAlteracao = class(TObject)
  private
    FDtAltBeneficio: TDateTime;
    FDadosBeneficio: TDadosBeneficio;
  public
    constructor Create;
    destructor Destroy; override;

    property dtAltBeneficio: TDateTime read FDtAltBeneficio write FDtAltBeneficio;
    property dadosBeneficio: TDadosBeneficio read FDadosBeneficio write FDadosBeneficio;
  end;
  
  TDadosBeneficio = class(TObject)
  private
    FTpBeneficio: integer;
    FTpPlanRP: tpPlanRP;
    FDsc: String;
    FIndSuspensao: TpSimNao;
    FInfoPenMorte: TInfoPenMorte;
    FSuspensao: TSuspensao;
    
    function getInfoPenMorte(): TInfoPenMorte;
    function getSuspensao(): TSuspensao;
  public
    constructor Create;
    destructor Destroy; override;
    
    function infoPenMorteInst(): Boolean;
    function SuspensaoInst(): Boolean;
    
    property tpBeneficio: integer read FTpBeneficio write FTpBeneficio;
    property tpPlanRP: tpPlanRP read FTpPlanRP write FTpPlanRP;
    property dsc: String read FDsc write FDsc;
    property indSuspensao: TpSimNao read FIndSuspensao write FIndSuspensao;
    property infoPenMorte: TInfoPenMorte read getInfoPenMorte write FInfoPenMorte;
    property suspensao: TSuspensao read getSuspensao write FSuspensao;
  end;
  
  TInfoPenMorte = class(TObject)
  private
    FTpTpPenMorte: tpTpPenMorte;
  public
    property tpPenMorte: tpTpPenMorte read FTpTpPenMorte write FTpTpPenMorte;
  end;
  
  TSuspensao = class(TObject)
  private
    FMtvSuspensao: tpMtvSuspensao;
    FDscSuspensao: string;
  public
    property mtvSuspensao: tpMtvSuspensao read FMtvSuspensao write FMtvSuspensao;
    property dscSuspensao: string read FDscSuspensao write FDscSuspensao;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2416Collection }

function TS2416Collection.Add: TS2416CollectionItem;
begin
  Result := Self.New;
end;

function TS2416Collection.GetItem(Index: Integer): TS2416CollectionItem;
begin
  Result := TS2416CollectionItem(inherited Items[Index]);
end;

procedure TS2416Collection.SetItem(Index: Integer; Value: TS2416CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2416Collection.New: TS2416CollectionItem;
begin
  Result := TS2416CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2416CollectionItem }

constructor TS2416CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento   := teS2416;
  FEvtCdBenAlt  := TEvtCdBenAlt.Create(AOwner);
end;

destructor TS2416CollectionItem.Destroy;
begin
  FreeAndNil(FEvtCdBenAlt);

  inherited;
end;

{ TInfoBenAlteracao }

constructor TInfoBenAlteracao.Create;
begin
  inherited Create;
  
  FDadosBeneficio := TDadosBeneficio.Create;
end;

destructor TInfoBenAlteracao.Destroy;
begin
  FreeAndNil(FDadosBeneficio);
  
  inherited;
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

function TDadosBeneficio.getSuspensao(): TSuspensao;
begin
  if not(Assigned(FSuspensao)) then
    FSuspensao := TSuspensao.Create;
  Result := FSuspensao;
end;

function TDadosBeneficio.SuspensaoInst: Boolean;
begin
  Result := Assigned(FSuspensao);
end;

{ TEvtCdBenAlt }

constructor TEvtCdBenAlt.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento        := TIdeEvento2.Create;
  FIdeEmpregador    := TIdeEmpregador.Create;
  FIdeBeneficio     := TIdeBeneficio.Create;
  FInfoBenAlteracao := TInfoBenAlteracao.Create;
end;

destructor TEvtCdBenAlt.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeBeneficio.Free;
  FInfoBenAlteracao.Free;

  inherited;
end;

procedure TEvtCdBenAlt.GerarIdeBeneficio(pIdeBeneficio: TIdeBeneficio);
begin
  Gerador.wGrupo('ideBeneficio');

  Gerador.wCampo(tcStr, '', 'cpfBenef',    11, 11, 1, pIdeBeneficio.cpfBenef);
  Gerador.wCampo(tcStr, '', 'nrBeneficio',  0, 30, 1, pIdeBeneficio.nrBeneficio);

  Gerador.wGrupo('/ideBeneficio');
end;

procedure TEvtCdBenAlt.GerarInfoBenAlteracao(pInfoBenAlteracao: TInfoBenAlteracao);
begin
  Gerador.wGrupo('infoBenAlteracao');

  Gerador.wCampo(tcDat, '', 'dtAltBeneficio', 10, 10, 1, pInfoBenAlteracao.dtAltBeneficio);
  
  GerarDadosBeneficio(pInfoBenAlteracao.dadosBeneficio);

  Gerador.wGrupo('/infoBenAlteracao');
end;

procedure TEvtCdBenAlt.GerarDadosBeneficio(pDadosBeneficio: TDadosBeneficio);
begin
  Gerador.wGrupo('dadosBeneficio');

  Gerador.wCampo(tcInt, '', 'tpBeneficio',    4,   4, 1, pDadosBeneficio.tpBeneficio);
  Gerador.wCampo(tcStr, '', 'tpPlanRP',       1,   1, 1, eSTpPlanRPToStr(pDadosBeneficio.tpPlanRP));
  Gerador.wCampo(tcStr, '', 'dsc',            0, 255, 0, pDadosBeneficio.dsc);
  Gerador.wCampo(tcStr, '', 'indSuspensao',   1,   1, 1, eSSimNaoToStr(pDadosBeneficio.indSuspensao));

  if pDadosBeneficio.infoPenMorteInst() then
    GerarInfoPenMorte(pDadosBeneficio.infoPenMorte);

  if pDadosBeneficio.SuspensaoInst() then
    GerarSuspensao(pDadosBeneficio.Suspensao);
   
  Gerador.wGrupo('/dadosBeneficio');
end;

procedure TEvtCdBenAlt.GerarInfoPenMorte(pInfoPenMorte: TInfoPenMorte);
begin
  if pInfoPenMorte.tpPenMorte = pmNada then
    Exit;
    
  Gerador.wGrupo('infoPenMorte');

  Gerador.wCampo(tcStr, '', 'tpPenMorte',  1,  1, 1, eStpTpPenMorteToStrEX(pInfoPenMorte.tpPenMorte));

  Gerador.wGrupo('/infoPenMorte');
end;

procedure TEvtCdBenAlt.GerarSuspensao(pSuspensao: TSuspensao);
begin
  if pSuspensao.mtvSuspensao = mtvNada then
    Exit;
    
  Gerador.wGrupo('suspensao');

  Gerador.wCampo(tcStr, '', 'mtvSuspensao', 2,   2, 1, eStpTpMtvSuspensaoToStr(pSuspensao.mtvSuspensao));
  Gerador.wCampo(tcStr, '', 'dscSuspensao', 0, 255, 0, pSuspensao.dscSuspensao);
  
  Gerador.wGrupo('/suspensao');
end;

function TEvtCdBenAlt.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtCdBenAlt');
    Gerador.wGrupo('evtCdBenAlt Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeBeneficio(self.IdeBeneficio);
    GerarInfoBenAlteracao(self.InfoBenAlteracao);

    Gerador.wGrupo('/evtCdBenAlt');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtCdBenAlt');

//    Validar(schevtCdBenAlt);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtCdBenAlt.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
begin
  Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;

  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtCdBenAlt';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideBeneficio';
      IdeBeneficio.cpfBenef    := INIRec.ReadString(sSecao, 'cpfBenef', EmptyStr);
      IdeBeneficio.nrBeneficio := INIRec.ReadString(sSecao, 'nrBeneficio', EmptyStr);

      sSecao := 'infoBenAlteracao';
      InfoBenAlteracao.dtAltBeneficio := StringToDateTime(INIRec.ReadString(sSecao, 'dtAltBeneficio', '0'));

      sSecao := 'dadosBeneficio';
      InfoBenAlteracao.dadosBeneficio.tpBeneficio := INIRec.ReadInteger(sSecao, 'tpBeneficio', 0);
      InfoBenAlteracao.dadosBeneficio.tpPlanRP := eSStrToTpPlanRP(Ok, INIRec.ReadString(sSecao, 'tpPlanRP', EmptyStr));
      InfoBenAlteracao.dadosBeneficio.dsc := INIRec.ReadString(sSecao, 'dsc', EmptyStr);
      InfoBenAlteracao.dadosBeneficio.indSuspensao := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'indSuspensao', EmptyStr));

      sSecao := 'infoPenMorte';
      InfoBenAlteracao.dadosBeneficio.infoPenMorte.tpPenMorte := eSStrTotpTpPenMorteEX(INIRec.ReadString(sSecao, 'tpPenMorte', EmptyStr));

      sSecao := 'suspensao';
      InfoBenAlteracao.dadosBeneficio.suspensao.mtvSuspensao := eSStrToTpMtvSuspensao(Ok, INIRec.ReadString(sSecao, 'mtvSuspensao', EmptyStr));
      InfoBenAlteracao.dadosBeneficio.suspensao.dscSuspensao := INIRec.ReadString(sSecao, 'dscSuspensao', EmptyStr);

    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;


end.
