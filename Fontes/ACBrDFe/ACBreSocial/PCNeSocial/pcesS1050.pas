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

unit pcesS1050;

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
  ACBrBase, pcnConversao, pcnGerador,
  pcesCommon, pcesConversaoeSocial, pcesGerador;


type
  TS1050CollectionItem = class;
  TEvtTabHorTur = class;
  TideHorContratual = class;
  TDadosHorContratual = class;
  TInfoHorContratual = class;

  TS1050Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1050CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1050CollectionItem);
  public
    function Add: TS1050CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1050CollectionItem;
    property Items[Index: Integer]: TS1050CollectionItem read GetItem write SetItem; default;
  end;

  TS1050CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtTabHorContratual: TEvtTabHorTur;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTabHorContratual: TEvtTabHorTur read FEvtTabHorContratual write FEvtTabHorContratual;
  end;

  TEvtTabHorTur = class(TESocialEvento)
  private
    FModoLancamento: TModoLancamento;
    fIdeEvento: TIdeEvento;
    fIdeEmpregador: TIdeEmpregador;
    fInfoHorContratual: TInfoHorContratual;

    {Geradores específicos da classe}
    procedure GerarDadosHorContratual;
    procedure GerarIdeHorContratual;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property IdeEvento: TIdeEvento read fIdeEvento write fIdeEvento;
    property IdeEmpregador: TIdeEmpregador read fIdeEmpregador write fIdeEmpregador;
    property InfoHorContratual: TInfoHorContratual read fInfoHorContratual write fInfoHorContratual;
  end;

  TideHorContratual = class(TObject)
  private
    FCodHorContrat: string;
    FIniValid: string;
    FFimValid: string;
  public
    property codHorContrat: string read FCodHorContrat write FCodHorContrat;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TDadosHorContratual = class(TObject)
  private
    FHrEntr: string;
    FHrSaida: string;
    FDurJornada: integer;
    FPerHorFlexivel: tpSimNao;
    FHorarioIntervalo: THorarioIntervaloCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property hrEntr: string read FHrEntr write FHrEntr;
    property hrSaida: string read FHrSaida write FHrSaida;
    property durJornada: integer read FDurJornada write FDurJornada;
    property perHorFlexivel: tpSimNao read FPerHorFlexivel write FPerHorFlexivel;
    property horarioIntervalo: THorarioIntervaloCollection read FHorarioIntervalo write FHorarioIntervalo;
  end;

  TInfoHorContratual = class(TObject)
  private
    fideHorContratual: TideHorContratual;
    fdadosHorContratual: TdadosHorContratual;
    fnovaValidade : TIdePeriodo;

    function getDadosHorContratual: TdadosHorContratual;
    function getNovaValidade: TIdePeriodo;
  public
    constructor Create;
    destructor Destroy; override;

    function dadosHorContratualInst(): Boolean;
    function novaValidadeInst(): Boolean;

    property ideHorContratual: TideHorContratual read fideHorContratual write fideHorContratual;
    property dadosHorContratual: TdadosHorContratual read getDadosHorContratual write fdadosHorContratual;
    property novaValidade: TIdePeriodo read getNovaValidade write fnovaValidade;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBreSocial;

{ TS1050Collection }

function TS1050Collection.Add: TS1050CollectionItem;
begin
  Result := Self.New;
end;

function TS1050Collection.GetItem(Index: Integer): TS1050CollectionItem;
begin
  Result := TS1050CollectionItem(inherited Items[Index]);
end;

procedure TS1050Collection.SetItem(Index: Integer;
  Value: TS1050CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1050Collection.New: TS1050CollectionItem;
begin
  Result := TS1050CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS1050CollectionItem }

constructor TS1050CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento := teS1050;
  FEvtTabHorContratual := TEvtTabHorTur.Create(AOwner);
end;

destructor TS1050CollectionItem.Destroy;
begin
  FEvtTabHorContratual.Free;

  inherited;
end;

{ TdadosHorContratual }

constructor TdadosHorContratual.Create;
begin
  inherited Create;
  FHorarioIntervalo := THorarioIntervaloCollection.Create;
end;

destructor TdadosHorContratual.Destroy;
begin
  FreeAndNil(FHorarioIntervalo);

  inherited;
end;

{ TInfoHorContratual }

constructor TInfoHorContratual.Create;
begin
  inherited Create;
  fideHorContratual   := TideHorContratual.Create;
  fdadosHorContratual := nil;
  fnovaValidade       := nil;
end;

function TInfoHorContratual.dadosHorContratualInst: Boolean;
begin
  Result := Assigned(fdadosHorContratual);
end;

destructor TInfoHorContratual.Destroy;
begin
  fideHorContratual.Free;
  FreeAndNil(fdadosHorContratual);
  FreeAndNil(fnovaValidade);

  inherited;
end;

function TInfoHorContratual.getDadosHorContratual: TdadosHorContratual;
begin
  if Not(Assigned(fdadosHorContratual)) then
    fdadosHorContratual := TDadosHorContratual.create;
  Result := fdadosHorContratual;
end;

function TInfoHorContratual.getNovaValidade: TIdePeriodo;
begin
  if Not(Assigned(fnovaValidade)) then
    fnovaValidade := TIdePeriodo.Create;
  Result := fnovaValidade;
end;

function TInfoHorContratual.novaValidadeInst: Boolean;
begin
  Result := Assigned(fnovaValidade);
end;

{ TEvtTabHorContratual }

constructor TEvtTabHorTur.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  fIdeEvento         := TIdeEvento.Create;
  fIdeEmpregador     := TIdeEmpregador.Create;
  fInfoHorContratual := TInfoHorContratual.Create;
end;

destructor TEvtTabHorTur.Destroy;
begin
  fIdeEvento.Free;
  fIdeEmpregador.Free;
  fInfoHorContratual.Free;

  inherited;
end;

procedure TEvtTabHorTur.GerarDadosHorContratual;
begin
  Gerador.wGrupo('dadosHorContratual');

  Gerador.wCampo(tcStr, '', 'hrEntr',         4, 4, 1, self.InfoHorContratual.dadosHorContratual.hrEntr);
  Gerador.wCampo(tcStr, '', 'hrSaida',        4, 4, 1, self.InfoHorContratual.dadosHorContratual.hrSaida);
  Gerador.wCampo(tcStr, '', 'durJornada',     1, 4, 1, self.InfoHorContratual.dadosHorContratual.durJornada);
  Gerador.wCampo(tcStr, '', 'perHorFlexivel', 1, 1, 1, eSSimNaoToStr(self.InfoHorContratual.dadosHorContratual.perHorFlexivel));


  Gerador.wGrupo('/dadosHorContratual');
end;

procedure TEvtTabHorTur.GerarIdeHorContratual;
begin
  Gerador.wGrupo('ideHorContratual');

  Gerador.wCampo(tcStr, '', 'codHorContrat', 1, 30, 1, InfoHorContratual.ideHorContratual.codHorContrat);
  Gerador.wCampo(tcStr, '', 'iniValid',      7,  7, 1, InfoHorContratual.ideHorContratual.iniValid);
  Gerador.wCampo(tcStr, '', 'fimValid',      7,  7, 0, InfoHorContratual.ideHorContratual.fimValid);

  Gerador.wGrupo('/ideHorContratual');
end;

function TEvtTabHorTur.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTabHorTur');
    Gerador.wGrupo('evtTabHorTur Id="' + Self.Id + '"');

    GerarIdeEvento(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    Gerador.wGrupo('infoHorContratual');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdeHorContratual;

    if Self.ModoLancamento <> mlExclusao then
    begin
      GerarDadosHorContratual;

      if Self.ModoLancamento = mlAlteracao then
        if (InfoHorContratual.novaValidadeInst()) then
          GerarIdePeriodo(self.InfoHorContratual.novaValidade, 'novaValidade');
    end;

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoHorContratual');
    Gerador.wGrupo('/evtTabHorTur');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabHorTur');

//    Validar(schevtTabHorTur);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '') 
end;

function TEvtTabHorTur.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtTabHorTur';
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

      sSecao := 'ideHorContratual';
      infoHorContratual.ideHorContratual.codHorContrat := INIRec.ReadString(sSecao, 'codHorContrat', EmptyStr);
      infoHorContratual.ideHorContratual.IniValid      := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
      infoHorContratual.ideHorContratual.FimValid      := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);

      if (ModoLancamento <> mlExclusao) then
      begin
        sSecao := 'dadosHorContratual';
        infoHorContratual.dadosHorContratual.hrEntr         := INIRec.ReadString(sSecao, 'hrEntr', EmptyStr);
        infoHorContratual.dadosHorContratual.hrSaida        := INIRec.ReadString(sSecao, 'hrSaida', EmptyStr);
        infoHorContratual.dadosHorContratual.durJornada     := INIRec.ReadInteger(sSecao, 'durJornada', 0);
        infoHorContratual.dadosHorContratual.perHorFlexivel := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'perHorFlexivel', 'S'));

        if ModoLancamento = mlAlteracao then
        begin
          sSecao := 'novaValidade';
          infoHorContratual.novaValidade.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
          infoHorContratual.novaValidade.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);
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

