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
{$I ACBr.inc}

unit pcesS1035;

interface

uses
  SysUtils, Classes, DateUtils, Controls, ACBrUtil,
  pcnConversao,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS1035Collection = class;
  TS1035CollectionItem = class;
  TEvtTabCarreira = class;
  TIdeCarreira = class;
  TInfoCarreira = class;
  TDadosCarreira = class;

  TS1035Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS1035CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1035CollectionItem);
  public
    function Add: TS1035CollectionItem;
    property Items[Index: Integer]: TS1035CollectionItem read GetItem write SetItem; default;
  end;

  TS1035CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtTabCarreira: TEvtTabCarreira;
    procedure setEvtTabCarreira(const Value: TEvtTabCarreira);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property evtTabCarreira: TEvtTabCarreira read FEvtTabCarreira write setEvtTabCarreira;
  end;

  TEvtTabCarreira = class(TeSocialEvento)
  private
    FModoLancamento: TModoLancamento;
    FIdeEvento: TIdeEvento;
    FIdeEmpregador: TIdeEmpregador;
    FInfoCarreira: TInfoCarreira;
    FACBreSocial: TObject;

  public
    constructor Create(AACBreSocial: TObject);overload;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    procedure GerarIdeCarreira;
    procedure GerarDadosCarreira;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property IdeEvento: TIdeEvento read fIdeEvento write fIdeEvento;
    property IdeEmpregador: TIdeEmpregador read fIdeEmpregador write fIdeEmpregador;
    property InfoCarreira: TInfoCarreira read FInfoCarreira write FInfoCarreira;
  end;

  TDadosCarreira = class(TPersistent)
  private
    FDSCCarreira: string;
    FLeiCarr: string;
    FDTLeiCarr: TDate;
    FSitCarr: tpSitCarr;
  public
    property dscCarreira: string read FDSCCarreira write FDSCCarreira;
    property leiCarr: string read FLeiCarr write FLeiCarr;
    property dtLeiCarr: TDate read FDTLeiCarr write FDTLeiCarr;
    property sitCarr: tpSitCarr read FSitCarr write FSitCarr;
  end;

  TIdeCarreira = class(TPersistent)
  private
    FCodCarreira: string;
    FIniValid : string;
    FFimValid : string;
  public
    property codCarreira: string read FCodCarreira write FCodCarreira;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TInfoCarreira = class(TPersistent)
  private
    FIdeCarreira: TIdeCarreira;
    FDadosCarreira: TDadosCarreira;
    FNovaValidade: TidePeriodo;

    function getNovaValidade: TidePeriodo;
  public
    constructor create;
    destructor destroy; override;

    function novaValidadeInst(): Boolean;

    property dadosCarreira: TDadosCarreira read FDadosCarreira write FDadosCarreira;
    property ideCarreira: TIdeCarreira read FIdeCarreira write FIdeCarreira;
    property NovaValidade: TidePeriodo read getNovaValidade write FNovaValidade;
  end;

implementation

uses
  IniFiles,
  ACBreSocial, ACBrDFeUtil;

{ TS1035Collection }

function TS1035Collection.Add: TS1035CollectionItem;
begin
  Result := TS1035CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS1035Collection.GetItem(Index: Integer): TS1035CollectionItem;
begin
  Result := TS1035CollectionItem(inherited GetItem(Index));
end;

procedure TS1035Collection.SetItem(Index: Integer;
  Value: TS1035CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS1035CollectionItem }

constructor TS1035CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS1035;
  FEvtTabCarreira := TEvtTabCarreira.Create(AOwner);
end;

destructor TS1035CollectionItem.Destroy;
begin
  FEvtTabCarreira.Free;

  inherited;
end;

procedure TS1035CollectionItem.setEvtTabCarreira(const Value: TEvtTabCarreira);
begin
  FEvtTabCarreira.Assign(Value);
end;

{ TInfoCarreira }

constructor TInfoCarreira.create;
begin
  FIdeCarreira := TIdeCarreira.Create;
  FDadosCarreira := TDadosCarreira.Create;
  FNovaValidade := nil;
end;


destructor TInfoCarreira.destroy;
begin
  FIdeCarreira.Free;
  FDadosCarreira.Free;
  FreeAndNil(FNovaValidade);

  inherited;
end;

function TInfoCarreira.getNovaValidade: TidePeriodo;
begin
  if Not(Assigned(FNovaValidade)) then
    FNovaValidade := TIdePeriodo.Create;
  Result := FNovaValidade;
end;

function TInfoCarreira.novaValidadeInst: Boolean;
begin
  Result := Assigned(FNovaValidade);
end;

{ TEvtTabCarreira }

constructor TEvtTabCarreira.Create(AACBreSocial: TObject);
begin
  inherited;

  FACBreSocial := AACBreSocial;
  fIdeEvento := TIdeEvento.Create;
  fIdeEmpregador := TIdeEmpregador.Create;
  FInfoCarreira := TInfoCarreira.Create;
end;

destructor TEvtTabCarreira.Destroy;
begin
  fIdeEvento.Free;
  fIdeEmpregador.Free;
  FInfoCarreira.Free;

  inherited;
end;

procedure TEvtTabCarreira.GerarDadosCarreira;
begin
  Gerador.wGrupo('dadosCarreira');

  Gerador.wCampo(tcStr, '', 'dscCarreira',  1, 100, 1, InfoCarreira.dadosCarreira.dscCarreira);
  Gerador.wCampo(tcStr, '', 'leiCarr',      1,  12, 0, InfoCarreira.dadosCarreira.leiCarr);
  Gerador.wCampo(tcDat, '', 'dtLeiCarr',   10,  10, 1, InfoCarreira.dadosCarreira.dtLeiCarr);
  Gerador.wCampo(tcInt, '', 'sitCarr',      1,   1, 1, eStpSitCarrToStr(InfoCarreira.dadosCarreira.sitCarr));

  Gerador.wGrupo('/dadosCarreira');
end;

procedure TEvtTabCarreira.GerarIdeCarreira;
begin
  Gerador.wGrupo('ideCarreira');

  Gerador.wCampo(tcStr, '', 'codCarreira', 1, 30, 1, InfoCarreira.ideCarreira.codCarreira);
  Gerador.wCampo(tcStr, '', 'iniValid',    7,  7, 1, InfoCarreira.ideCarreira.iniValid);
  Gerador.wCampo(tcStr, '', 'fimValid',    7,  7, 0, InfoCarreira.ideCarreira.fimValid);

  Gerador.wGrupo('/ideCarreira');
end;

function TEvtTabCarreira.GerarXML: boolean;
begin
  try
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTabCarreira');
    Gerador.wGrupo('evtTabCarreira Id="' + Self.Id + '"');

    GerarIdeEvento(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    Gerador.wGrupo('infoCarreira');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdeCarreira;

    if Self.ModoLancamento <> mlExclusao then
    begin
      GerarDadosCarreira;

      if Self.ModoLancamento = mlAlteracao then
        if (InfoCarreira.novaValidadeInst()) then
          GerarIdePeriodo(InfoCarreira.NovaValidade, 'novaValidade');
    end;

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoCarreira');
    Gerador.wGrupo('/evtTabCarreira');
    GerarRodape;

    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabCarreira');

    Validar(schevtTabCarreira);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;


function TEvtTabCarreira.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  Ok: Boolean;
  sSecao: String;
begin
  Result := False;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with Self do
    begin
      sSecao := 'evtTabCarreira';
      Id             := INIRec.ReadString(sSecao, 'Id', '');
      Sequencial     := INIRec.ReadInteger(sSecao, 'Sequencial', 0);
      ModoLancamento := eSStrToModoLancamento(Ok, INIRec.ReadString(sSecao, 'ModoLancamento', 'inclusao'));

      sSecao := 'ideEvento';
      ideEvento.TpAmb   := eSStrTotpAmb(Ok, INIRec.ReadString(sSecao, 'tpAmb', '1'));
      ideEvento.ProcEmi := eSStrToProcEmi(Ok, INIRec.ReadString(sSecao, 'procEmi', '1'));
      ideEvento.VerProc := INIRec.ReadString(sSecao, 'verProc', EmptyStr);

      sSecao := 'ideEmpregador';
      ideEmpregador.OrgaoPublico := (TACBreSocial(FACBreSocial).Configuracoes.Geral.TipoEmpregador = teOrgaoPublico);
      ideEmpregador.TpInsc       := eSStrToTpInscricao(Ok, INIRec.ReadString(sSecao, 'tpInsc', '1'));
      ideEmpregador.NrInsc       := INIRec.ReadString(sSecao, 'nrInsc', EmptyStr);

      sSecao := 'ideCarreira';
      infoCarreira.ideCarreira.codCarreira := INIRec.ReadString(sSecao, 'codCarreira', EmptyStr);
      infoCarreira.ideCarreira.IniValid    := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
      infoCarreira.ideCarreira.FimValid    := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);

      if (ModoLancamento <> mlExclusao) then
      begin
        sSecao := 'dadosCarreira';
        infoCarreira.dadosCarreira.dscCarreira := INIRec.ReadString(sSecao, 'dscCarreira', EmptyStr);
        infoCarreira.dadosCarreira.leiCarr     := INIRec.ReadString(sSecao, 'leiCarr', EmptyStr);
        infoCarreira.dadosCarreira.dtleiCarr   := StringToDateTime(INIRec.ReadString(sSecao, 'dtleiCarr', '0'));
        infoCarreira.dadosCarreira.sitCarr     := eSStrToSitCarr(Ok, INIRec.ReadString(sSecao, 'sitCarr', '1'));

        if ModoLancamento = mlAlteracao then
        begin
          sSecao := 'novaValidade';
          infoCarreira.novaValidade.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
          infoCarreira.novaValidade.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);
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
