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

{$I ACBr.inc}

unit pcesS2400;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$ELSE}
   Contnrs,
  {$IFEND}
  ACBrBase, pcnConversao, ACBrUtil,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2400Collection = class;
  TS2400CollectionItem = class;
  TEvtCdBenPrRP = class;
  TIdeBenef = class;
  TDadosBenef = class;
  TInfoBeneficio = class;
  TBeneficio = class;
  TInfoPenMorte = class;
  TFimBeneficio = class;

  TS2400Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2400CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2400CollectionItem);
  public
    function Add: TS2400CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2400CollectionItem;
    property Items[Index: Integer]: TS2400CollectionItem read GetItem write SetItem; default;
  end;

  TS2400CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtCdBenPrRP : TEvtCdBenPrRP;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtCdBenPrRP: TEvtCdBenPrRP read FEvtCdBenPrRP write FEvtCdBenPrRP;
  end;

  TEvtCdBenPrRP = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeBenef: TIdeBenef;
    FInfoBeneficio: TInfoBeneficio;

    procedure GerarIdeBenef(pIdeBenef: TIdeBenef);
    procedure GerarDadosBenef(pDadosBenef: TDadosBenef);
    procedure GerarInfoBeneficio(pInfoBeneficio: TInfoBeneficio);
    procedure GerarBeneficio(pBeneficio: TBeneficio; const pGroupName: String);
    procedure GerarInfoPenMorte(pInfoPenMorte: TInfoPenMorte);
    procedure GerarFimBeneficio(pFimBeneficio: TFimBeneficio);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property ideBenef: TIdeBenef read FIdeBenef write FIdeBenef;
    property infoBeneficio: TInfoBeneficio read FInfoBeneficio write FInfoBeneficio;
  end;

  TFimBeneficio = class(TObject)
  private
    FTpBenef: integer;
    FNrBenefic: string;
    FDtFimBenef: TDateTime;
    FMtvFim: integer;
  public
    property tpBenef: integer read FTpBenef write FTpBenef;
    property nrBenefic: string read FNrBenefic write FNrBenefic;
    property dtFimBenef: TDateTime read FDtFimBenef write FDtFimBenef;
    property mtvFim: Integer read FMtvFim write FMtvFim;
  end;

  TInfoPenMorte = class(TObject)
  private
    FIdQuota: String;
    FCpfInst: String;
  public
    property idQuota: string read FIdQuota write FIdQuota;
    property cpfInst: string read FCpfInst write FCpfInst;
  end;

  TBeneficio = class(TObject)
  private
    FTpBenef: integer;
    FNrBenefic: string;
    FDtIniBenef: TDateTime;
    FVrBenef: Double;
    FInfoPenMorte: TInfoPenMorte;

    function getInfoPenMorte: TInfoPenMorte;
  public
    constructor Create;
    destructor Destroy; override;

    function infoPenMorteInst: boolean;

    property tpBenef: integer read FTpBenef write FTpBenef;
    property nrBenefic: string read FNrBenefic write FNrBenefic;
    property dtIniBenef: TDateTime read FDtIniBenef write FDtIniBenef;
    property vrBenef: Double read FVrBenef write FVrBenef;
    property infoPenMorte: TInfoPenMorte read getInfoPenMorte write FInfoPenMorte;
  end;

  TInfoBeneficio = class(TObject)
  private
    FTpPlanRP: tpPlanRP;
    FIniBeneficio: TBeneficio;
    FAltBeneficio: TBeneficio;
    FFimBeneficio: TFimBeneficio;

    function getIniBeneficio: TBeneficio;
    function getAltBeneficio: TBeneficio;
    function getFimBeneficio: TFimBeneficio;
  public
    constructor Create;
    destructor Destroy; override;

    function iniBeneficioInst: boolean;
    function altBeneficioInst: boolean;
    function fimBeneficioInst: boolean;

    property tpPlanRP: tpPlanRP read FTpPlanRP write FTpPlanRP;
    property iniBeneficio: TBeneficio read getIniBeneficio write FIniBeneficio;
    property altBeneficio: TBeneficio read getAltBeneficio write FAltBeneficio;
    property fimBeneficio: TFimBeneficio read getFimBeneficio write FFimBeneficio;
  end;

  TDadosBenef = class(TObject)
  private
    FDadosNasc: TNascimento;
    FEndereco: TEndereco;
  public
    constructor Create;
    destructor Destroy; override;
    property dadosNasc: TNascimento read FDadosNasc write FDadosNasc;
    property endereco: TEndereco read FEndereco write FEndereco;
  end;

  TIdeBenef = class(TObject)
  private
    FCpfBEnef: string;
    FNmBenefic: string;
    FDadosBenef: TDadosBenef;
  public
    constructor Create;
    destructor Destroy; override;
    property cpfBenef: String read FCpfBEnef write FCpfBEnef;
    property nmBenefic: string read FNmBenefic write FNmBenefic;
    property dadosBenef: TDadosBenef read FDadosBenef write FDadosBenef;
  end;

implementation

uses
  IniFiles,
  ACBreSocial;

{ TS2400Collection }

function TS2400Collection.Add: TS2400CollectionItem;
begin
  Result := Self.New;
end;

function TS2400Collection.GetItem(Index: Integer): TS2400CollectionItem;
begin
  Result := TS2400CollectionItem(inherited Items[Index]);
end;

procedure TS2400Collection.SetItem(Index: Integer; Value: TS2400CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2400Collection.New: TS2400CollectionItem;
begin
  Result := TS2400CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2400CollectionItem }

constructor TS2400CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento   := teS2400;
  FEvtCdBenPrRP := TEvtCdBenPrRP.Create(AOwner);
end;

destructor TS2400CollectionItem.Destroy;
begin
  FEvtCdBenPrRP.Free;

  inherited;
end;

{ TBeneficio }

constructor TBeneficio.Create;
begin
  inherited Create;
  FInfoPenMorte := nil;
end;

destructor TBeneficio.Destroy;
begin
  FreeAndNil(FInfoPenMorte);
  inherited;
end;

function TBeneficio.getInfoPenMorte: TInfoPenMorte;
begin
  if not Assigned(FInfoPenMorte) then
    FInfoPenMorte := TInfoPenMorte.Create;
  Result := FInfoPenMorte;
end;

function TBeneficio.infoPenMorteInst: boolean;
begin
  result := Assigned(FInfoPenMorte);
end;

{ TInfoBeneficio }

constructor TInfoBeneficio.Create;
begin
  inherited Create;
  FIniBeneficio := nil;
  FAltBeneficio := nil;
  FFimBeneficio := nil;
end;

destructor TInfoBeneficio.Destroy;
begin
  FreeAndNil(FIniBeneficio);
  FreeAndNil(FAltBeneficio);
  FreeAndNil(FFimBeneficio);
  inherited;
end;

function TInfoBeneficio.getIniBeneficio: TBeneficio;
begin
  if not Assigned(FIniBeneficio) then
    FIniBeneficio := TBeneficio.Create;
  result := FIniBeneficio;
end;

function TInfoBeneficio.getAltBeneficio: TBeneficio;
begin
  if not Assigned(FAltBeneficio) then
    FAltBeneficio := TBeneficio.Create;
  result := FAltBeneficio;
end;

function TInfoBeneficio.getFimBeneficio: TFimBeneficio;
begin
  if not Assigned(FFimBeneficio) then
    FFimBeneficio := TFimBeneficio.Create;
  Result := FFimBeneficio;
end;

function TInfoBeneficio.iniBeneficioInst: boolean;
begin
  result := Assigned(FIniBeneficio);
end;

function TInfoBeneficio.altBeneficioInst: boolean;
begin
  result := Assigned(FAltBeneficio);
end;

function TInfoBeneficio.fimBeneficioInst: boolean;
begin
  Result := Assigned(FFimBeneficio);
end;

{ TIdeBenef }

constructor TIdeBenef.Create;
begin
  inherited Create;
  FDadosBenef := TDadosBenef.Create;
end;

destructor TIdeBenef.Destroy;
begin
  FDadosBenef.Free;
  inherited;
end;

{ TDadosBenef }

constructor TDadosBenef.Create;
begin
  inherited Create;
  FDadosNasc := TNascimento.Create;
  FEndereco  := TEndereco.Create;
end;

destructor TDadosBenef.Destroy;
begin
  FDadosNasc.Free;
  FEndereco.Free;
  inherited;
end;

{ TEvtCdBenPrRP }

constructor TEvtCdBenPrRP.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeBenef      := TIdeBenef.Create;
  FInfoBeneficio := TInfoBeneficio.Create;
end;

destructor TEvtCdBenPrRP.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeBenef.Free;
  FInfoBeneficio.Free;

  inherited;
end;

procedure TEvtCdBenPrRP.GerarDadosBenef(pDadosBenef: TDadosBenef);
begin
  Gerador.wGrupo('dadosBenef');

  GerarNascimento(pDadosBenef.dadosNasc, 'dadosNasc');

  GerarEndereco(pDadosBenef.endereco, (pDadosBenef.endereco.Exterior.PaisResid <> ''));

  Gerador.wGrupo('/dadosBenef');
end;

procedure TEvtCdBenPrRP.GerarIdeBenef(pIdeBenef: TIdeBenef);
begin
  Gerador.wGrupo('ideBenef');

  Gerador.wCampo(tcStr, '', 'cpfBenef',  11, 11, 1, pIdeBenef.cpfBenef);
  Gerador.wCampo(tcStr, '', 'nmBenefic',  0, 70, 1, pIdeBenef.nmBenefic);

  GerarDadosBenef(pIdeBenef.dadosBenef);

  Gerador.wGrupo('/ideBenef');
end;

procedure TEvtCdBenPrRP.GerarInfoPenMorte(pInfoPenMorte: TInfoPenMorte);
begin
  Gerador.wGrupo('infoPenMorte');

  Gerador.wCampo(tcStr, '', 'idQuota',  1, 30, 1, pInfoPenMorte.idQuota);
  Gerador.wCampo(tcStr, '', 'cpfInst', 11, 11, 1, pInfoPenMorte.cpfInst);

  Gerador.wGrupo('/infoPenMorte');
end;

procedure TEvtCdBenPrRP.GerarBeneficio(pBeneficio: TBeneficio; const pGroupName: String);
begin
  Gerador.wGrupo(pGroupName);

  Gerador.wCampo(tcInt, '', 'tpBenef',     1,  2, 1, pBeneficio.tpBenef);
  Gerador.wCampo(tcStr, '', 'nrBenefic',   1, 20, 1, pBeneficio.nrBenefic);
  Gerador.wCampo(tcDat, '', 'dtIniBenef', 10, 10, 1, pBeneficio.dtIniBenef);
  Gerador.wCampo(tcDe2, '', 'vrBenef',     1, 14, 1, pBeneficio.vrBenef);

  if pBeneficio.infoPenMorteInst then
    GerarInfoPenMorte(pBeneficio.infoPenMorte);

  Gerador.wGrupo('/' + pGroupName);
end;

procedure TEvtCdBenPrRP.GerarFimBeneficio(pFimBeneficio: TFimBeneficio);
begin
  Gerador.wGrupo('fimBeneficio');

  Gerador.wCampo(tcInt, '', 'tpBenef',     1,  2, 1, pFimBeneficio.tpBenef);
  Gerador.wCampo(tcStr, '', 'nrBenefic',   1, 20, 1, pFimBeneficio.nrBenefic);
  Gerador.wCampo(tcDat, '', 'dtFimBenef', 10, 10, 1, pFimBeneficio.dtFimBenef);
  Gerador.wCampo(tcInt, '', 'mtvFim',      1,  2, 1, pFimBeneficio.mtvFim);

  Gerador.wGrupo('/fimBeneficio');
end;

procedure TEvtCdBenPrRP.GerarInfoBeneficio(pInfoBeneficio: TInfoBeneficio);
begin
  Gerador.wGrupo('infoBeneficio');

  Gerador.wCampo(tcStr, '', 'tpPlanRP', 1, 1, 1, eSTpPlanRPToStr(pInfoBeneficio.tpPlanRP));

  if pInfoBeneficio.iniBeneficioInst then
    GerarBeneficio(pInfoBeneficio.iniBeneficio, 'iniBeneficio');

  if pInfoBeneficio.altBeneficioInst then
    GerarBeneficio(pInfoBeneficio.altBeneficio, 'altBeneficio');

  if pInfoBeneficio.fimBeneficioInst then
    GerarFimBeneficio(pInfoBeneficio.fimBeneficio);

  Gerador.wGrupo('/infoBeneficio');
end;

function TEvtCdBenPrRP.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtCdBenPrRP');
    Gerador.wGrupo('evtCdBenPrRP Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeBenef(ideBenef);
    GerarInfoBeneficio(infoBeneficio);

    Gerador.wGrupo('/evtCdBenPrRP');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtCdBenPrRP');

//    Validar(schevtCdBenPrRP);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtCdBenPrRP.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao: String;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtCdBenPrRP';
      Id         := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial := INIRec.ReadInteger(sSecao, 'Sequencial', 0);

      sSecao := 'ideEvento';
      ideEvento.indRetif    := eSStrToIndRetificacao(Ok, INIRec.ReadString(sSecao, 'indRetif', '1'));
      ideEvento.NrRecibo    := INIRec.ReadString(sSecao, 'nrRecibo', EmptyStr);
      ideEvento.ProcEmi     := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc     := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideBenef';
      ideBenef.cpfBenef  := INIRec.ReadString(sSecao, 'cpfBenef', EmptyStr);
      ideBenef.nmBenefic := INIRec.ReadString(sSecao, 'nmBenefic', EmptyStr);

      sSecao := 'dadosNasc';
      ideBenef.dadosBenef.dadosNasc.dtNascto   := StringToDateTime(INIRec.ReadString(sSecao, 'dtNascto', '0'));
      ideBenef.dadosBenef.dadosNasc.codMunic   := INIRec.ReadInteger(sSecao, 'codMunic', 0);
      ideBenef.dadosBenef.dadosNasc.UF         := INIRec.ReadString(sSecao, 'uf', '');
      ideBenef.dadosBenef.dadosNasc.PaisNascto := INIRec.ReadString(sSecao, 'paisNascto', '');
      ideBenef.dadosBenef.dadosNasc.PaisNac    := INIRec.ReadString(sSecao, 'paisNac', '');
      ideBenef.dadosBenef.dadosNasc.NmMae      := INIRec.ReadString(sSecao, 'nmMae', '');
      ideBenef.dadosBenef.dadosNasc.NmPai      := INIRec.ReadString(sSecao, 'nmPai', '');

      sSecao := 'enderecoBrasil';
      if INIRec.ReadString(sSecao, 'tpLograd', '') <> '' then
      begin
        ideBenef.dadosBenef.Endereco.Brasil.TpLograd    := INIRec.ReadString(sSecao, 'tpLograd', '');
        ideBenef.dadosBenef.Endereco.Brasil.DscLograd   := INIRec.ReadString(sSecao, 'dscLograd', '');
        ideBenef.dadosBenef.Endereco.Brasil.NrLograd    := INIRec.ReadString(sSecao, 'nrLograd', '');
        ideBenef.dadosBenef.Endereco.Brasil.Complemento := INIRec.ReadString(sSecao, 'complemento', '');
        ideBenef.dadosBenef.Endereco.Brasil.Bairro      := INIRec.ReadString(sSecao, 'bairro', '');
        ideBenef.dadosBenef.Endereco.Brasil.Cep         := INIRec.ReadString(sSecao, 'cep', '');
        ideBenef.dadosBenef.Endereco.Brasil.CodMunic    := INIRec.ReadInteger(sSecao, 'codMunic', 0);
        ideBenef.dadosBenef.Endereco.Brasil.UF          := eSStrTouf(Ok, INIRec.ReadString(sSecao, 'uf', 'SP'));
      end;

      sSecao := 'enderecoExterior';
      if INIRec.ReadString(sSecao, 'paisResid', '') <> '' then
      begin
        ideBenef.dadosBenef.Endereco.Exterior.PaisResid   := INIRec.ReadString(sSecao, 'paisResid', '');
        ideBenef.dadosBenef.Endereco.Exterior.DscLograd   := INIRec.ReadString(sSecao, 'dscLograd', '');
        ideBenef.dadosBenef.Endereco.Exterior.NrLograd    := INIRec.ReadString(sSecao, 'nrLograd', '');
        ideBenef.dadosBenef.Endereco.Exterior.Complemento := INIRec.ReadString(sSecao, 'complemento', '');
        ideBenef.dadosBenef.Endereco.Exterior.Bairro      := INIRec.ReadString(sSecao, 'bairro', '');
        ideBenef.dadosBenef.Endereco.Exterior.NmCid       := INIRec.ReadString(sSecao, 'nmCid', '');
        ideBenef.dadosBenef.Endereco.Exterior.CodPostal   := INIRec.ReadString(sSecao, 'codPostal', '');
      end;

      sSecao := 'infoBeneficio';
      infoBeneficio.tpPlanRP := eSStrToTpPlanRP(Ok, INIRec.ReadString(sSecao, 'dtTerm', '1'));

      sSecao := 'iniBeneficio';
      if INIRec.ReadString(sSecao, 'tpBenef', '') <> '' then
      begin
        infoBeneficio.iniBeneficio.tpBenef    := INIRec.ReadInteger(sSecao, 'tpBenef', 0);
        infoBeneficio.iniBeneficio.nrBenefic  := INIRec.ReadString(sSecao, 'nrBenefic', '');
        infoBeneficio.iniBeneficio.dtIniBenef := StringToDateTime(INIRec.ReadString(sSecao, 'dtIniBenef', '0'));
        infoBeneficio.iniBeneficio.vrBenef    := StringToFloatDef(INIRec.ReadString(sSecao, 'vrBenef', ''), 0);

        sSecao := 'infoPenMorte';
        if INIRec.ReadString(sSecao, 'idQuota', '') <> '' then
        begin
          infoBeneficio.iniBeneficio.infoPenMorte.idQuota := INIRec.ReadString(sSecao, 'idQuota', '');
          infoBeneficio.iniBeneficio.infoPenMorte.cpfInst := INIRec.ReadString(sSecao, 'cpfInst', '');
        end;
      end;

      sSecao := 'altBeneficio';
      if INIRec.ReadString(sSecao, 'tpBenef', '') <> '' then
      begin
        infoBeneficio.altBeneficio.tpBenef    := INIRec.ReadInteger(sSecao, 'tpBenef', 0);
        infoBeneficio.altBeneficio.nrBenefic  := INIRec.ReadString(sSecao, 'nrBenefic', '');
        infoBeneficio.altBeneficio.dtIniBenef := StringToDateTime(INIRec.ReadString(sSecao, 'dtIniBenef', '0'));
        infoBeneficio.altBeneficio.vrBenef    := StringToFloatDef(INIRec.ReadString(sSecao, 'vrBenef', ''), 0);

        sSecao := 'infoPenMorte';
        if INIRec.ReadString(sSecao, 'idQuota', '') <> '' then
        begin
          infoBeneficio.altBeneficio.infoPenMorte.idQuota := INIRec.ReadString(sSecao, 'idQuota', '');
          infoBeneficio.altBeneficio.infoPenMorte.cpfInst := INIRec.ReadString(sSecao, 'cpfInst', '');
        end;
      end;

      sSecao := 'fimBeneficio';
      if INIRec.ReadString(sSecao, 'tpBenef', '') <> '' then
      begin
        infoBeneficio.fimBeneficio.tpBenef    := INIRec.ReadInteger(sSecao, 'tpBenef', 0);
        infoBeneficio.fimBeneficio.nrBenefic  := INIRec.ReadString(sSecao, 'nrBenefic', '');
        infoBeneficio.fimBeneficio.dtFimBenef := StringToDateTime(INIRec.ReadString(sSecao, 'dtFimBenef', '0'));
        infoBeneficio.fimBeneficio.mtvFim     := INIRec.ReadInteger(sSecao, 'mtvFim', 0);
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
