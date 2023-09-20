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

unit pcesS2240;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao, pcnGerador, pcnConsts,
  pcesCommon, pcesConversaoeSocial, pcesGerador, pcnLeitor;

type
  TS2240Collection = class;
  TS2240CollectionItem = class;
  TEvtExpRisco = class;
  TinfoExpRisco = class;
  TInfoAmbCollection = class;
  TInfoAmbCollectionItem = class;
  TInfoAtiv = class;
{  
  TAtivPericInsalCollection = class;
  TAtivPericInsalCollectionItem = class;
}  
  TAgNocCollection = class;
  TAgNocCollectionItem = class;
  TEpcEpi = class;
  TEpiCollection = class;
  TEpiCollectionItem = class;
  TEpiCompl = class;
  TRespRegCollection = class;
  TRespRegCollectionItem = class;
  TObs = class;

  TS2240Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2240CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2240CollectionItem);
  public
    function Add: TS2240CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2240CollectionItem;
    property Items[Index: Integer]: TS2240CollectionItem read GetItem write SetItem; default;
  end;

  TS2240CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtExpRisco: TEvtExpRisco;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtExpRisco: TEvtExpRisco read FEvtExpRisco write FEvtExpRisco;
  end;

  TEvtExpRisco = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FinfoExpRisco: TinfoExpRisco;

    { Geradores da classe }
    procedure GerarInfoExpRisco(objInfoExpRisco: TInfoExpRisco);
    procedure GerarInfoAmb(objInfoAmb: TinfoAmbCollection);
    procedure GerarInfoAtiv(objInfoAtiv: TInfoAtiv);
    procedure GerarAgNoc(objFatRisco: TAgNocCollection);
    procedure GerarEpcEpi(pEpcEpi: TEpcEpi);
    procedure GerarEPI(objEPI: TEpiCollection);
    procedure GerarEpiCompl(objEPICompl: TEpiCompl);
    procedure GerarRespReg(pRespReg: TRespRegCollection);
    procedure GerarObs(pObs: TObs);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerXML : Boolean;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property infoExpRisco: TinfoExpRisco read FinfoExpRisco write FinfoExpRisco;
  end;

  TRespRegCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRespRegCollectionItem;
    procedure SetItem(Index: Integer; Value: TRespRegCollectionItem);
  public
    function Add: TRespRegCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRespRegCollectionItem;
    property Items[Index: Integer]: TRespRegCollectionItem read GetItem write SetItem; default;
  end;

  TRespRegCollectionItem = class(TObject)
  private
    FcpfResp: String;
    FNisResp: String;
    FNmResp: String;
    FIdeOC: tpIdeOC;
    FdscOC: String;
    FNrOc: String;
    FUfOC: string;
  public
    property cpfResp: string read FcpfResp write FcpfResp;
    property nisResp: string read FNisResp write FNisResp;
    property nmResp: string read FNmResp write FNmResp;
    property ideOC: tpIdeOC read FIdeOC write FIdeOC;
    property dscOC: string read FdscOC write FdscOC;
    property nrOC: string read FNrOc write FNrOc;
    property ufOC: string read FUfOC write FUfOC;
  end;

  TinfoExpRisco = class(TObject)
  private
    FdtIniCondicao: TDateTime;
    FdtFimCondicao: TDateTime;
    FInfoAmb: TInfoAmbCollection;
    FInfoAtiv: TInfoAtiv;
    FFatRisco: TAgNocCollection;
    FRespReg: TRespRegCollection;
    FObs: TObs;

    function getRespReg: TRespRegCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property dtIniCondicao: TDateTime read FdtIniCondicao write FdtIniCondicao;
    property dtFimCondicao: TDateTime read FdtFimCondicao write FdtFimCondicao;
    property InfoAmb: TInfoAmbCollection read FInfoAmb write FInfoAmb;
    property infoAtiv: TInfoAtiv read FInfoAtiv write FInfoAtiv;
    property agNoc: TagNocCollection read FFatRisco write FFatRisco;
    property respReg: TRespRegCollection read getRespReg write FRespReg;
    property obs: TObs read FObs write FObs;
  end;

  TInfoAmbCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoAmbCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoAmbCollectionItem);
  public
    function Add: TInfoAmbCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoAmbCollectionItem;
    property Items[Index: Integer]: TInfoAmbCollectionItem read GetItem write SetItem;
  end;

  TInfoAmbCollectionItem = class(TObject)
  private
    FcodAmb: String;
    FlocalAmb: tpLocalAmb;
    FdscSetor: string;
    FtpInsc: TptpInsc;
    FnrInsc: String;
  public
    property codAmb: String read FcodAmb write FcodAmb;
    property localAmb: tpLocalAmb read FlocalAmb write FlocalAmb;
    property dscSetor: String read FdscSetor write FdscSetor;
    property tpInsc: TptpInsc read FtpInsc write FtpInsc;
    property nrInsc: String read FnrInsc write FnrInsc;
  end;

  TInfoAtiv = class(TObject)
  private
    FdscAtivDes: String;
  public
    constructor Create;
    destructor  Destroy; override;

    property dscAtivDes: String read FdscAtivDes write FdscAtivDes;
  end;

  TAgNocCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TagNocCollectionItem;
    procedure SetItem(Index: Integer; Value: TAgNocCollectionItem);
  public
    function Add: TAgNocCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TAgNocCollectionItem;
    property Items[Index: Integer]: TagNocCollectionItem read GetItem write SetItem;
  end;

  TAgNocCollectionItem = class(TObject)
  private
    FcodAgNoc: String;
    FdscAgNoc: string;
    FtpAval: tptpAval;
    FintConc: Double;
    FlimTol: Double;
    FunMed: Integer;
    FtecMedicao: String;
    FnrProcJud: String;
    FEpcEpi: TEpcEpi;
    function getEpcEpi: TEpcEpi;
  public
    constructor Create;
    destructor Destroy; override;
    function epcEpiInst: Boolean;

    property codAgNoc: String read FcodAgNoc write FcodAgNoc;
    property dscAgNoc: String read FdscAgNoc write FdscAgNoc;
    property tpAval: tptpAval read FtpAval write FtpAval;
    property intConc: Double read FintConc write FintConc;
    property limTol: Double read FlimTol write FlimTol;
    property unMed: Integer read FunMed write FunMed;
    property tecMedicao: String read FtecMedicao write FtecMedicao;
    property nrProcJud: String read FnrProcJud write FnrProcJud;
    property epcEpi: TEpcEpi read getEpcEpi write FEpcEpi;
  end;

  TEpcEpi = class(TObject)
  private
    FUtilizEPC: tpUtilizEPC;
    FEficEpc: tpSimNao;
    FUtilizEPI: tpUtilizEPI;
    FEpi: TEpiCollection;
    FEpiCompl: TEpiCompl;
    FeficEpi: tpSimNaoFacultativo;

    function getEpi: TEpiCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function epiInst(): boolean;

    property utilizEPC: tpUtilizEPC read FUtilizEPC write FUtilizEPC;
    property eficEpc: tpSimNao read FEficEpc write FEficEpc;
    property utilizEPI: tpUtilizEPI read FUtilizEPI write FUtilizEPI;
    property epi: TEpiCollection read getEpi write FEpi;
    property epiCompl: TEpiCompl read FEpiCompl write FEpiCompl;
    property eficEpi : tpSimNaoFacultativo read FeficEpi write FeficEpi;
  end;

  TEpiCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TEpiCollectionItem;
    procedure SetItem(Index: Integer; Value: TEpiCollectionItem);
  public
    function Add: TEpiCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TEpiCollectionItem;
    property Items[Index: Integer]: TEpiCollectionItem read GetItem write SetItem; default;
  end;

  TEpiCollectionItem = class(TObject)
  private
    FcaEPI: String;
    FdscEPI: String;
    FmedProtecao: tpSimNao;
    FcondFuncto: tpSimNao;
    FusoInint: tpSimNao;
    FprzValid: tpSimNao;
    FperiodicTroca: tpSimNao;
    Fhigienizacao: tpSimNao;
    FdocAval: String;
  public
    property caEPI: string read FcaEPI write FcaEPI;
    property dscEPI: string read FdscEPI write FdscEPI;
    property medProtecao: tpSimNao read FmedProtecao write FmedProtecao;
    property condFuncto: tpSimNao read FcondFuncto write FcondFuncto;
    property usoInint: tpSimNao read FusoInint write FusoInint;
    property przValid: tpSimNao read FprzValid write FprzValid;
    property periodicTroca: tpSimNao read FperiodicTroca write FperiodicTroca;
    property higienizacao: tpSimNao read Fhigienizacao write Fhigienizacao;
    property docAval: string read FdocAval write FdocAval;
  end;
  
  TEpiCompl = class(TObject)
  private
    FmedProtecao: tpSimNaoFacultativo;
    FcondFuncto: tpSimNaoFacultativo;
    FusoInint: tpSimNaoFacultativo;
    FprzValid: tpSimNaoFacultativo;
    FperiodicTroca: tpSimNaoFacultativo;
    Fhigienizacao: tpSimNaoFacultativo;
  public
    property medProtecao: tpSimNaoFacultativo read FmedProtecao write FmedProtecao;
    property condFuncto: tpSimNaoFacultativo read FcondFuncto write FcondFuncto;
    property usoInint: tpSimNaoFacultativo read FusoInint write FusoInint;
    property przValid: tpSimNaoFacultativo read FprzValid write FprzValid;
    property periodicTroca: tpSimNaoFacultativo read FperiodicTroca write FperiodicTroca;
    property higienizacao: tpSimNaoFacultativo read Fhigienizacao write Fhigienizacao;
  end;
  
  TObs = class(TObject)
  private
    FmetErg: String;
    FobsCompl: String;
  public
    property metErg: String read FmetErg write FmetErg;
    property obsCompl: String read FobsCompl write FobsCompl;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2240Collection }

function TS2240Collection.Add: TS2240CollectionItem;
begin
  Result := Self.New;
end;

function TS2240Collection.GetItem(Index: Integer): TS2240CollectionItem;
begin
  Result := TS2240CollectionItem(inherited Items[Index]);
end;

procedure TS2240Collection.SetItem(Index: Integer; Value: TS2240CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2240Collection.New: TS2240CollectionItem;
begin
  Result := TS2240CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2240CollectionItem }

constructor TS2240CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento  := teS2240;
  FEvtExpRisco := TEvtExpRisco.Create(AOwner);
end;

destructor TS2240CollectionItem.Destroy;
begin
  FEvtExpRisco.Free;

  inherited;
end;

{ TEvtAltContratual }

constructor TEvtExpRisco.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo    := TIdeVinculo.Create;
  FInfoExpRisco  := TInfoExpRisco.Create;
end;

destructor TEvtExpRisco.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeVinculo.Free;
  FInfoExpRisco.Free;

  inherited;
end;

procedure TEvtExpRisco.GerarEPI(objEPI: TEpiCollection);
var
  i: integer;
begin
  for i := 0 to objEPI.Count -1 do
  begin
    Gerador.wGrupo('epi');

    if VersaoDF <= ve02_05_00 then
      Gerador.wCampo(tcStr, '', 'caEPI',         1,  20, 0, objEPI[i].caEPI)
    else
      Gerador.wCampo(tcStr, '', 'docAval',       1, 255, 0, objEPI[i].docAval);
      
    Gerador.wCampo(tcStr, '', 'dscEPI',          1, 999, 0, objEPI[i].dscEPI);
{    
    Gerador.wCampo(tcStr, '', 'eficEpi',         1,   1, 1, eSSimNaoToStr(objEPI[i].eficEpi));
}    
    if VersaoDF <= ve02_05_00 then
    begin
      Gerador.wCampo(tcStr, '', 'medProtecao',   1,   1, 1, eSSimNaoToStr(objEPI[i].medProtecao));
      Gerador.wCampo(tcStr, '', 'condFuncto',    1,   1, 1, eSSimNaoToStr(objEPI[i].condFuncto));
      Gerador.wCampo(tcStr, '', 'usoInint',      1,   1, 1, eSSimNaoToStr(objEPI[i].usoInint));
      Gerador.wCampo(tcStr, '', 'przValid',      1,   1, 1, eSSimNaoToStr(objEPI[i].przValid));
      Gerador.wCampo(tcStr, '', 'periodicTroca', 1,   1, 1, eSSimNaoToStr(objEPI[i].periodicTroca));
      Gerador.wCampo(tcStr, '', 'higienizacao',  1,   1, 1, eSSimNaoToStr(objEPI[i].higienizacao));
    end;

    Gerador.wGrupo('/epi');
  end;

  if objEPI.Count > 50 then
    Gerador.wAlerta('', 'epi', 'Lista de EPI', ERR_MSG_MAIOR_MAXIMO + '50');
end;

procedure TEvtExpRisco.GerarEpiCompl(objEPICompl: TEpiCompl);
begin
  if VersaoDF <= ve02_05_00 then
    Exit;
    
  Gerador.wGrupo('epiCompl');
  
  Gerador.wCampo(tcStr, '', 'medProtecao',   1,   1, 0, eSSimNaoFacultativoToStr(objEPICompl.medProtecao));
  Gerador.wCampo(tcStr, '', 'condFuncto',    1,   1, 0, eSSimNaoFacultativoToStr(objEPICompl.condFuncto));
  Gerador.wCampo(tcStr, '', 'usoInint',      1,   1, 0, eSSimNaoFacultativoToStr(objEPICompl.usoInint));
  Gerador.wCampo(tcStr, '', 'przValid',      1,   1, 0, eSSimNaoFacultativoToStr(objEPICompl.przValid));
  Gerador.wCampo(tcStr, '', 'periodicTroca', 1,   1, 0, eSSimNaoFacultativoToStr(objEPICompl.periodicTroca));
  Gerador.wCampo(tcStr, '', 'higienizacao',  1,   1, 0, eSSimNaoFacultativoToStr(objEPICompl.higienizacao));
  
  Gerador.wGrupo('/epiCompl');
end;

procedure TEvtExpRisco.GerarEpcEpi(pEpcEpi: TEpcEpi);
begin
  Gerador.wGrupo('epcEpi');

  Gerador.wCampo(tcInt, '', 'utilizEPC', 1, 1, 1, eStpUtilizEPCToStr(pEpcEpi.utilizEPC));
  if pEpcEpi.utilizEPC = uEPCImplementa then
    Gerador.wCampo(tcStr, '', 'eficEpc',   1, 1, 1, eSSimNaoToStr(pEpcEpi.eficEpc));

  Gerador.wCampo(tcInt, '', 'utilizEPI', 1, 1, 1, eStpUtilizEPIToStr(pEpcEpi.utilizEPI));

  if pEpcEpi.utilizEPI = uEPIUtilizado then
    Gerador.wCampo(tcStr, '', 'eficEpi',         1,   1, 0, eSSimNaoFacultativoToStr(pEpcEpi.eficEpi));

  if pEpcEpi.epiInst() then
  begin
    GerarEPI(pEpcEpi.epi);
    if(pEpcEpi.utilizEPI = uEPIUtilizado)then
      GerarEPICompl(pEpcEpi.epiCompl);
  end;  

  Gerador.wGrupo('/epcEpi');
end;

procedure TEvtExpRisco.GerarAgNoc(objFatRisco: TagNocCollection);
var
  i: Integer;
begin
  for I := 0 to objFatRisco.Count - 1 do
  begin
    Gerador.wGrupo('agNoc');

    Gerador.wCampo(tcStr, '', 'codAgNoc',     1,  10, 1, objFatRisco.Items[i].codAgNoc);
    Gerador.wCampo(tcStr, '', 'dscAgNoc',     1, 100, 0, objFatRisco.Items[i].dscAgNoc);

    if objFatRisco.Items[i].codAgNoc <> '09.01.001' then
      Gerador.wCampo(tcStr, '', 'tpAval',     1,   1, 1, tpAvalToStr(objFatRisco.Items[i].tpAval));

    Gerador.wCampo(tcDe4, '', 'intConc',      1,  10, 0, objFatRisco.Items[i].intConc);
    Gerador.wCampo(tcDe4, '', 'limTol',       1,  10, 0, objFatRisco.Items[i].limTol);
    Gerador.wCampo(tcInt, '', 'unMed',        1,   2, 0, objFatRisco.Items[i].unMed);
    Gerador.wCampo(tcStr, '', 'tecMedicao',   1,  40, 0, objFatRisco.Items[i].tecMedicao);

    if (VersaoDF >= veS01_02_00) and (objFatRisco.Items[i].codAgNoc = '05.01.001') and (objFatRisco.Items[i].nrProcJud <> '') then
      Gerador.wCampo(tcStr, '', 'nrProcJud', 20,  20, 1, objFatRisco.Items[i].nrProcJud);
    
    if (objFatRisco.Items[i].epcEpiInst()) and (objFatRisco.Items[i].codAgNoc <> '09.01.001') then
      GerarEpcEpi(objFatRisco.Items[i].epcEpi);

    Gerador.wGrupo('/agNoc');
  end;

  if objFatRisco.Count > 999 then
    Gerador.wAlerta('', 'agNoc', 'Lista de Fatores de Riscos', ERR_MSG_MAIOR_MAXIMO + '999');
end;

procedure TEvtExpRisco.GerarInfoAmb(objInfoAmb: TinfoAmbCollection);
var
  i: integer;
begin
  for i := 0 to objInfoAmb.Count - 1 do
  begin
    Gerador.wGrupo('infoAmb');

    if VersaoDF <= ve02_05_00 then
      Gerador.wCampo(tcStr, '', 'codAmb',   1,  30, 1, objInfoAmb.items[i].codAmb)
    else
    begin
      Gerador.wCampo(tcStr, '', 'localAmb', 1,   1, 1, eSLocalAmbToStr(objInfoAmb.items[i].localAmb));
      Gerador.wCampo(tcStr, '', 'dscSetor', 1, 100, 1, objInfoAmb.items[i].dscSetor);
      Gerador.wCampo(tcStr, '', 'tpInsc',   1,   1, 1, eSTpInscricaoToStr(objInfoAmb.items[i].tpInsc));
      Gerador.wCampo(tcStr, '', 'nrInsc',  12,  14, 1, objInfoAmb.items[i].nrInsc); 
    end;
    
    Gerador.wGrupo('/infoAmb');
  end;

  if objInfoAmb.Count > 99 then
    Gerador.wAlerta('', 'infoAmb', 'Lista de Informações Ambientais', ERR_MSG_MAIOR_MAXIMO + '99');
end;

procedure TEvtExpRisco.GerarInfoAtiv(objInfoAtiv: TInfoAtiv);
begin
  Gerador.wGrupo('infoAtiv');

  Gerador.wCampo(tcStr, '', 'dscAtivDes', 1, 999, 1, objInfoAtiv.dscAtivDes);

  Gerador.wGrupo('/infoAtiv');
end;

procedure TEvtExpRisco.GerarRespReg(pRespReg: TRespRegCollection);
var
  i: Integer;
begin
  for i := 0 to pRespReg.Count - 1 do
  begin
    Gerador.wGrupo('respReg');

    Gerador.wCampo(tcStr, '', 'cpfResp', 1, 11, 1, pRespReg[i].cpfResp);
    
    if VersaoDF <= ve02_05_00 then
    begin
      Gerador.wCampo(tcStr, '', 'nisResp', 1, 11, 1, pRespReg[i].nisResp);
      Gerador.wCampo(tcStr, '', 'nmResp',  1, 70, 1, pRespReg[i].nmResp);
    end;

    if pRespReg[i].ideOC <> idNenhum then
      Gerador.wCampo(tcStr, '', 'ideOC',   1,  1, 1, eSIdeOCToStrEX(pRespReg[i].ideOC));

    if pRespReg[i].ideOC = idOutros then
      Gerador.wCampo(tcStr, '', 'dscOC',   1, 20, 1, pRespReg[i].dscOC);

    if pRespReg[i].ideOC <> idNenhum then
    begin
      Gerador.wCampo(tcStr, '', 'nrOC',    1, 14, 1, pRespReg[i].nrOc);
      Gerador.wCampo(tcStr, '', 'ufOC',    2,  2, 0, pRespReg[i].ufOC);
    end;

    Gerador.wGrupo('/respReg');
  end;

  if pRespReg.Count > 9 then
    Gerador.wAlerta('', 'respReg', 'Lista de Responsáveis pelo registro', ERR_MSG_MAIOR_MAXIMO + '9');
end;

procedure TEvtExpRisco.GerarInfoExpRisco(objInfoExpRisco: TInfoExpRisco);
begin
  Gerador.wGrupo('infoExpRisco');

  Gerador.wCampo(tcDat, '', 'dtIniCondicao',   10, 10, 1, objInfoExpRisco.dtIniCondicao);

  if ((objInfoExpRisco.dtIniCondicao >= StringToDateTime('16/01/2022')) and (DateToStr(objInfoExpRisco.dtFimCondicao) <> dDataBrancoNula)) then
    Gerador.wCampo(tcDat, '', 'dtFimCondicao', 10, 10, 1, objInfoExpRisco.dtFimCondicao);

  GerarInfoAmb(objInfoExpRisco.InfoAmb);
  GerarInfoAtiv(objInfoExpRisco.infoAtiv);
  GerarAgNoc(objInfoExpRisco.agNoc);
  GerarRespReg(objInfoExpRisco.respReg);
  GerarObs(objInfoExpRisco.obs);

  Gerador.wGrupo('/infoExpRisco');
end;

procedure TEvtExpRisco.GerarObs(pObs: TObs);
begin
  if (((pObs.metErg <> '') or (pObs.obsCompl <> '')) and (VersaoDF <= ve02_05_00)) or
     ((pObs.obsCompl <> '') and (VersaoDF > ve02_05_00)) then
  begin
    Gerador.wGrupo('obs');
    
    if VersaoDF <= ve02_05_00 then
      if (pObs.metErg <> '') then
        Gerador.wCampo(tcStr, '', 'metErg',   1, 999, 0, pObs.metErg);
    
    if (pObs.obsCompl <> '') then
      Gerador.wCampo(tcStr, '', 'obsCompl', 1, 999, 0, pObs.obsCompl);
    
    Gerador.wGrupo('/obs');
  end;
end;

function TEvtExpRisco.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtExpRisco');
    Gerador.wGrupo('evtExpRisco Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeVinculo(self.IdeVinculo);
    GerarInfoExpRisco(self.InfoExpRisco);

    Gerador.wGrupo('/evtExpRisco');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtExpRisco');

//    Validar(schevtExpRisco);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

{ TinfoExpRisco }

constructor TinfoExpRisco.Create;
begin
  inherited;

  FInfoAmb  := TInfoAmbCollection.Create;
  FInfoAtiv := TInfoAtiv.Create;
  FFatRisco := TagNocCollection.Create;
  FRespReg  := TRespRegCollection.Create;;
  FObs      := TObs.Create;
end;

destructor TinfoExpRisco.destroy;
begin
  FinfoAmb.Free;
  FInfoAtiv.Free;
  FFatRisco.Free;
  FRespReg.Free;
  FObs.Free;

  inherited;
end;

function TinfoExpRisco.getRespReg: TRespRegCollection;
begin
  if not Assigned(FRespReg) then
    FRespReg := TRespRegCollection.Create;
  Result := FRespReg;
end;

{ TRespRegCollection }

function TRespRegCollection.Add: TRespRegCollectionItem;
begin
  Result := Self.New;
end;

function TRespRegCollection.GetItem(Index: Integer): TRespRegCollectionItem;
begin
  Result := TRespRegCollectionItem(inherited Items[Index]);
end;

procedure TRespRegCollection.SetItem(Index: Integer; Value: TRespRegCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRespRegCollection.New: TRespRegCollectionItem;
begin
  Result := TRespRegCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoAmbCollection }

function TInfoAmbCollection.Add: TInfoAmbCollectionItem;
begin
  Result := Self.New;
end;

function TInfoAmbCollection.GetItem(Index: Integer): TInfoAmbCollectionItem;
begin
  Result := TInfoAmbCollectionItem(inherited Items[Index]);
end;

procedure TInfoAmbCollection.SetItem(Index: Integer; Value: TInfoAmbCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoAmbCollection.New: TInfoAmbCollectionItem;
begin
  Result := TInfoAmbCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoAtiv }

constructor TInfoAtiv.Create;
begin
  inherited;

//  FativPericInsal := TAtivPericInsalCollection.Create;
end;

destructor TInfoAtiv.Destroy;
begin
//  FativPericInsal.Free;

  inherited;
end;

{ TFatRiscoCollection }

function TAgNocCollection.Add: TAgNocCollectionItem;
begin
  Result := Self.New;
end;

function TAgNocCollection.GetItem(Index: Integer): TAgNocCollectionItem;
begin
  Result := TAgNocCollectionItem(inherited Items[Index])
end;

procedure TAgNocCollection.SetItem(Index: Integer; Value: TAgNocCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TAgNocCollection.New: TAgNocCollectionItem;
begin
  Result := TAgNocCollectionItem.Create;
  Self.Add(Result);
end;

{ TFatRiscoCollectionItem }

constructor TAgNocCollectionItem.Create;
begin
  inherited Create;
  
  FEpcEpi := TEpcEpi.Create;
end;

destructor TAgNocCollectionItem.Destroy;
begin
  if Assigned(FEpcEpi) then
    FEpcEpi.Free;
  
  inherited;
end;

function TAgNocCollectionItem.epcEpiInst: Boolean;
begin
   Result := Assigned(FEpcEpi);
end;

function TAgNocCollectionItem.getEpcEpi: TEpcEpi;
begin
  if not Assigned(FEpcEpi) then
    FEpcEpi := TEpcEpi.Create;
  result := FEpcEpi;
end;

{ TEpcEpi }

constructor TEpcEpi.Create;
begin
  inherited;

  FEpi := nil;
  FEpiCompl := TEpiCompl.Create;
end;

destructor TEpcEpi.Destroy;
begin
  FreeAndNil(FEpiCompl);
  FreeAndNil(FEpi);
  
  inherited;
end;

function TEpcEpi.getEpi: TEpiCollection;
begin
  if not Assigned(FEpi) then
    FEpi := TEpiCollection.Create;
  result := FEpi;
end;

function TEpcEpi.epiInst: boolean;
begin
  result := Assigned(FEpi);
end;

{ TEpiCollection }
function TEpiCollection.Add: TEpiCollectionItem;
begin
  Result := Self.New;
end;

function TEpiCollection.GetItem(Index: Integer): TEpiCollectionItem;
begin
  Result := TEpiCollectionItem(inherited Items[Index]);
end;

procedure TEpiCollection.SetItem(Index: Integer; Value: TEpiCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TEpiCollection.New: TEpiCollectionItem;
begin
  Result := TEpiCollectionItem.Create;
  Self.Add(Result);
end;

function TEvtExpRisco.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao, sFim: String;
  I, J, K: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtExpRisco';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.ProcEmi  := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc  := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideVinculo';
      ideVinculo.CpfTrab   := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      ideVinculo.Matricula := INIRec.ReadString(sSecao, 'matricula', EmptyStr);
      ideVinculo.codCateg  := INIRec.ReadInteger(sSecao, 'codCateg', 0);

      sSecao := 'infoExpRisco';
      if INIRec.ReadString(sSecao, 'dtIniCondicao', '') <> '' then
      begin
        infoExpRisco.dtIniCondicao := StringToDateTime(INIRec.ReadString(sSecao, 'dtIniCondicao', '0'));
        infoExpRisco.dtFimCondicao := StringToDateTime(INIRec.ReadString(sSecao, 'dtFimCondicao', '0'));

        if ((infoExpRisco.dtIniCondicao >= StringToDateTime('16/01/2022')) and (infoExpRisco.dtFimCondicao > 0)) then
          infoExpRisco.dtFimCondicao := StringToDateTime(INIRec.ReadString(sSecao, 'dtFimCondicao', '0'));

        I := 1;
        while true do
        begin
          // de 01 até 99
          sSecao := 'infoAmb' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'localAmb', 'FIM');

          if (sFim = 'FIM') or (Length(sFim) <= 0) then
            break;

          with infoExpRisco.infoAmb.New do
          begin
            localAmb:= eSStrToLocalAmb(Ok, INIRec.ReadString(sSecao, 'localAmb', '0'));
            dscSetor:= INIRec.ReadString(sSecao, 'dscSetor', EmptyStr);
            tpInsc:= eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
            nrInsc:= INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);
//            codAmb := sFim;
          end;

          with infoExpRisco do
          begin
            sSecao := 'infoAtiv' + IntToStrZero(I, 2);
            infoAtiv.dscAtivDes := INIRec.ReadString(sSecao, 'dscAtivDes', EmptyStr);

            J := 1;
            while true do
            begin
              // de 01 até 99
              sSecao := 'ativPericInsal' + IntToStrZero(I, 2) + IntToStrZero(J, 2);
              sFim   := INIRec.ReadString(sSecao, 'codAtiv', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              Inc(J);
            end;

            J := 1;
            while true do
            begin
              // de 001 até 999
              sSecao := 'agNoc' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
              sFim   := INIRec.ReadString(sSecao, 'codAgNoc', 'FIM');

              if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break;

              with agNoc.New do
              begin
                codAgNoc   := sFim;
                dscAgNoc   := INIRec.ReadString(sSecao, 'dscAgNoc', EmptyStr);
                tpAval     := StrTotpAval(Ok, INIRec.ReadString(sSecao, 'tpAval', '0'));
                intConc    := StringToFloatDef(INIRec.ReadString(sSecao, 'intConc', EmptyStr), 0);
                limTol     := StringToFloatDef(INIRec.ReadString(sSecao, 'limTol', EmptyStr), 0);
                unMed      := INIRec.ReadInteger(sSecao, 'unMed', 0);
                tecMedicao := INIRec.ReadString(sSecao, 'tecMedicao', EmptyStr);
                nrProcJud  := INIRec.ReadString(sSecao, 'nrProcJud', EmptyStr);

                epcEpi.utilizEPC := eSStrTotpUtilizEPC(Ok, INIRec.ReadString(sSecao, 'utilizEPC', '0'));
                epcEpi.eficEpc   := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'eficEpc', '0'));
                epcEpi.utilizEPI := eSStrTotpUtilizEPI(Ok, INIRec.ReadString(sSecao, 'utilizEPI', '0'));
                epcEpi.eficEpi   := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'eficEpi', 'S'));

                K := 1;
                while true do
                begin
                  // de 00 até 50
                  sSecao := 'epi' + IntToStrZero(I, 2) + IntToStrZero(J, 3) +
                               IntToStrZero(K, 2);
                  sFim   := INIRec.ReadString(sSecao, 'caEPI', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    sFim   := INIRec.ReadString(sSecao, 'docAval', 'FIM');

                  if (sFim = 'FIM') or (Length(sFim) <= 0) then
                    break;

                  with epcEpi.epi.New do
                  begin
                    caEPI         := INIRec.ReadString(sSecao, 'caEPI', EmptyStr);
                    docAval       := INIRec.ReadString(sSecao, 'docAval', EmptyStr);
                    dscEPI        := INIRec.ReadString(sSecao, 'dscEPI', EmptyStr);
                    medProtecao   := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'medProtecao', 'S'));
                    condFuncto    := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'condFuncto', 'S'));
                    usoInint      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'usoInint', 'S'));
                    przValid      := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'przValid', 'S'));
                    periodicTroca := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'periodicTroca', 'S'));
                    higienizacao  := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'higienizacao', 'S'));
                  end;

                  Inc(K);
                end;

                sSecao := 'epiCompl' + IntToStrZero(I, 2) + IntToStrZero(J, 3);
                sFim   := INIRec.ReadString(sSecao, 'medProtecao', 'FIM');

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;
                with epcEpi.epiCompl do
                begin
                  medProtecao        := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'medProtecao', 'S'));
                  condFuncto         := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'condFuncto', 'S'));
                  usoInint           := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'usoInint', 'S'));
                  przValid           := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'przValid', 'S'));
                  periodicTroca      := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'periodicTroca', 'S'));
                  higienizacao       := eSStrToSimNaoFacultativo(Ok, INIRec.ReadString(sSecao, 'higienizacao', 'S'));
                end;

              end;

              Inc(J);
            end;
          end;

          Inc(I);
        end;
      end;

      I := 1;
      while true do
      begin
        //S1.1.0
        sSecao := 'respReg' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'cpfResp', 'FIM');
        if(sFim = 'FIM') or (Length(sFim) <= 0)then
        begin
          // layout 2.5 de 1 até 9
          sSecao := 'respReg' + IntToStrZero(I, 1);
          sFim   := INIRec.ReadString(sSecao, 'cpfResp', 'FIM');
        end;

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infoExpRisco.respReg.New do
        begin
          cpfResp := INIRec.ReadString(sSecao, 'cpfResp', EmptyStr);
          nmResp  := INIRec.ReadString(sSecao, 'nmResp', EmptyStr);
          ideOC   := eSStrToIdeOCEX(INIRec.ReadString(sSecao, 'ideOC', EmptyStr));
          dscOC   := INIRec.ReadString(sSecao, 'dscOC', EmptyStr);
          nrOC    := INIRec.ReadString(sSecao, 'nrOc', EmptyStr);
          ufOC    := INIRec.ReadString(sSecao, 'ufOC', 'SP');
        end;

        Inc(I);
      end;

      sSecao := 'obs';
      infoExpRisco.obs.obsCompl := INIRec.ReadString(sSecao, 'obsCompl', EmptyStr);
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

function TEvtExpRisco.LerXML: Boolean;
var
  Leitor: TLeitor;
  ok: Boolean;
  i, j: integer;
begin
  Result := False;
  Leitor := TLeitor.Create;
  try
    Leitor.Arquivo := XML;

    if Leitor.rExtrai(1, 'evtExpRisco') <> '' then
    begin
      if Leitor.rExtrai(2, 'ideEvento') <> '' then
        with Self.ideEvento do
        begin
          indRetif := eSStrToIndRetificacao(ok, leitor.rCampo(tcStr, 'indRetif'));
          nrRecibo := Leitor.rCampo(tcStr,'nrRecibo');
          procEmi  := eSStrToprocEmi(ok, Leitor.rCampo(tcStr, 'procEmi'));
          verProc  := Leitor.rCampo(tcStr, 'verProc');
        end;

      if Leitor.rExtrai(2, 'ideEmpregador') <> '' then
        with Self.ideEmpregador do
        begin
          tpInsc := eSStrToTpInscricao(ok, Leitor.rCampo(tcStr, 'tpInsc'));
          nrInsc := Leitor.rCampo(tcStr, 'nrInsc');
        end;

      if Leitor.rExtrai(2, 'ideVinculo') <> '' then
        with Self.ideVinculo do
        begin
          cpfTrab   := Leitor.rCampo(tcStr, 'cpfTrab');
          matricula := Leitor.rCampo(tcStr, 'matricula');
          codCateg  := Leitor.rCampo(tcInt, 'codCateg');
        end;

      if Leitor.rExtrai(2, 'infoExpRisco') <> '' then
      begin
        with Self.infoExpRisco do
        begin
          dtIniCondicao := Leitor.rCampo(tcDat, 'dtIniCondicao');
          dtFimCondicao := Leitor.rCampo(tcDat, 'dtFimCondicao');

          if ((dtIniCondicao >= StringToDateTime('16/01/2022')) and (dtFimCondicao > 0)) then
            dtFimCondicao := Leitor.rCampo(tcDat, 'dtFimCondicao');

          if Leitor.rExtrai(3, 'infoAmb') <> '' then
            with infoAmb.New do
            begin
              localAmb := eSStrToLocalAmb(ok, Leitor.rCampo(tcStr, 'localAmb'));
              dscSetor := Leitor.rCampo(tcStr, 'dscSetor');
              tpInsc   := eSStrToTpInscricao(ok, Leitor.rCampo(tcStr, 'tpInsc'));
              nrInsc   := Leitor.rCampo(tcStr, 'nrInsc');
            end;
          
          if Leitor.rExtrai(3, 'infoAtiv') <> '' then
            with infoAtiv do
              dscAtivDes := Leitor.rCampo(tcStr, 'dscAtivDes');
              
          i := 0;
          while Leitor.rExtrai(3, 'agNoc', '', i + 1) <> '' do
          begin
            with agNoc.New do 
            begin
              codAgNoc   := Leitor.rCampo(tcStr, 'codAgNoc');
              dscAgNoc   := Leitor.rCampo(tcStr, 'dscAgNoc');
              tpAval     := StrTotpAval(ok, Leitor.rCampo(tcStr, 'tpAval'));
              intConc    := Leitor.rCampo(tcDe4, 'intConc');
              limTol     := Leitor.rCampo(tcDe4, 'limTol');
              unMed      := Leitor.rCampo(tcInt, 'unMed');
              tecMedicao := Leitor.rCampo(tcStr, 'tecMedicao');
              nrProcJud  := Leitor.rCampo(tcStr, 'nrProcJud');

              if Leitor.rExtrai(4, 'epcEpi') <> '' then
                with epcEpi do
                begin
                  utilizEPC := eSStrTotpUtilizEPC(ok, Leitor.rCampo(tcStr, 'utilizEPC'));
                  eficEpc   := eSStrToSimNao(ok, Leitor.rCampo(tcStr, 'eficEpc'));
                  utilizEPI := eSStrTotpUtilizEPI(ok , Leitor.rCampo(tcStr, 'utilizEPI'));
                  eficEpi   := eSStrToSimNaoFacultativo(ok, Leitor.rCampo(tcStr, 'eficEpi'));
                  
                  j := 0;
                  while Leitor.rExtrai(5, 'epi', '', j + 1) <> '' do
                  begin
                    with epi.New do
                    begin
                      docAval := Leitor.rCampo(tcStr, 'docAval');
                      dscEPI  := Leitor.rCampo(tcStr, 'dscEPI');
                    end;

                    Inc(j);
                  end;

                  if Leitor.rExtrai(5, 'epiCompl') <> '' then
                    with epiCompl do
                    begin
                      medProtecao   := eSStrToSimNaoFacultativo(ok, Leitor.rCampo(tcStr, 'medProtecao'));
                      condFuncto    := eSStrToSimNaoFacultativo(ok, Leitor.rCampo(tcStr, 'condFuncto'));
                      usoInint      := eSStrToSimNaoFacultativo(ok, Leitor.rCampo(tcStr, 'usoInint'));
                      przValid      := eSStrToSimNaoFacultativo(ok, Leitor.rCampo(tcStr, 'przValid'));
                      periodicTroca := eSStrToSimNaoFacultativo(ok, Leitor.rCampo(tcStr, 'periodicTroca'));
                      higienizacao  := eSStrToSimNaoFacultativo(ok, Leitor.rCampo(tcStr, 'higienizacao'));
                    end;
                end;
            end;
            
            Inc(i);
          end;
          
          i := 0;
          while Leitor.rExtrai(3, 'respReg', '', i + 1) <> '' do
          begin
            with respReg.New do
            begin
              cpfResp := Leitor.rCampo(tcStr, 'cpfResp');
              ideOC   := eSStrToIdeOCEX(Leitor.rCampo(tcStr, 'ideOC'));
              dscOC   := Leitor.rCampo(tcStr, 'dscOC');
              nrOC    := Leitor.rCampo(tcStr, 'nrOC');
              ufOC    := Leitor.rCampo(tcStr, 'ufOC');
            end;
            
            Inc(i);
          end;
          
          if Leitor.rExtrai(3, 'obs') <> '' then
            with obs do
              obsCompl := Leitor.rCampo(tcStr, 'obsCompl');
        end;
      end;
    end;
  finally
    Leitor.Free;
  end;
end;

end.
