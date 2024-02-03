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
{                              Leivio Fontenele                                }
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

unit pcesS1030;

interface

uses
  SysUtils, Classes, DateUtils, Controls,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$ELSE}
   Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS1030Collection = class;
  TS1030CollectionItem = class;
  TEvtTabCargo = class;
  TIdeCargo = class;
  TDadosCargo = class;
  TInfoCargo = class;
  TLeiCargo = class;
  TCargoPublico = class;

  TS1030Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1030CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1030CollectionItem);
  public
    function Add: TS1030CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1030CollectionItem;
    property Items[Index: Integer]: TS1030CollectionItem read GetItem write SetItem; default;
  end;

  TS1030CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtTabCargo: TEvtTabCargo;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTabCargo: TEvtTabCargo read FEvtTabCargo write FEvtTabCargo;
  end;

  TEvtTabCargo = class(TeSocialEvento)
  private
    FModoLancamento: TModoLancamento;
    fIdeEvento: TIdeEvento;
    fIdeEmpregador: TIdeEmpregador;
    fInfoCargo: TInfoCargo;

    {Geradores específicos da classe}
    procedure GerarIdeCargo;
    procedure GerarLeiCargo;
    procedure GerarCargoPublico;
    procedure GerarDadosCargo;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property IdeEvento: TIdeEvento read fIdeEvento write fIdeEvento;
    property IdeEmpregador: TIdeEmpregador read fIdeEmpregador write fIdeEmpregador;
    property InfoCargo: TInfoCargo read fInfoCargo write fInfoCargo;
  end;

  TIdeCargo = class(TObject)
  private
    FCodCargo : string;
    FIniValid : string;
    FFimValid : string;
  public
    property CodCargo: string read FCodCargo write FCodCargo;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TDadosCargo = class(TObject)
  private
    FNmCargo: string;
    FCodCBO: string;
    FCargoPublico: TCargoPublico;

    function getCargoPublico: TCargoPublico;
  public
    constructor Create;
    destructor Destroy; override;

    function cargoPublicInst(): Boolean;

    property nmCargo: string read FNmCargo write FNmCargo;
    property codCBO: string read FCodCBO write FCodCBO;
    property cargoPublico: TCargoPublico read getCargoPublico write FCargoPublico;
  end;

  TInfoCargo = class(TObject)
  private
    FIdeCargo: TIdeCargo;
    FDadosCargo: TDadosCargo;
    FNovaValidade: TidePeriodo;

    function getDadosCargo: TDadosCargo;
    function getNovaValidade: TidePeriodo;
  public
    constructor Create;
    destructor Destroy; override;

    function dadosCargoInst(): Boolean;
    function novaValidadeInst(): Boolean;

    property IdeCargo: TIdeCargo read FIdeCargo write FIdeCargo;
    property DadosCargo: TDadosCargo read getDadosCargo write FDadosCargo;
    property NovaValidade: TidePeriodo read getNovaValidade write FNovaValidade;
  end;

  TLeiCargo = class(TObject)
  private
    FNrLei: string;
    FDtLei: TDate;
  public
    property nrLei: string read FNrLei write FNrLei;
    property dtLei: TDate read FDtLei write FDtLei;
  end;

  TCargoPublico = class(TObject)
  private
    FAcumCargo: tpAcumCargo;
    FContagemEsp: tpContagemEsp;
    FDedicExcl: tpSimNao;
    FLeiCargo: TLeiCargo;
  public
    constructor Create;
    destructor Destroy; override;

    property acumCargo: tpAcumCargo read FAcumCargo write FAcumCargo;
    property contagemEsp: tpContagemEsp read FContagemEsp write FContagemEsp;
    property dedicExcl: tpSimNao read FDedicExcl write FDedicExcl;
    property leiCargo: TLeiCargo read FLeiCargo write FLeiCargo;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.DateTime,
  ACBrUtil.FilesIO,
  ACBreSocial;

{ TS1030Collection }

function TS1030Collection.Add: TS1030CollectionItem;
begin
  Result := Self.New;
end;

function TS1030Collection.GetItem(Index: Integer): TS1030CollectionItem;
begin
  Result := TS1030CollectionItem(inherited Items[Index]);
end;

procedure TS1030Collection.SetItem(Index: Integer;
  Value: TS1030CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1030Collection.New: TS1030CollectionItem;
begin
  Result := TS1030CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS1030CollectionItem }

constructor TS1030CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento  := teS1030;
  FEvtTabCargo := TEvtTabCargo.Create(AOwner);
end;

destructor TS1030CollectionItem.Destroy;
begin
  FEvtTabCargo.Free;
  inherited;
end;

{ TInfoCargo }

constructor TInfoCargo.Create;
begin
  inherited Create;
  FIdeCargo     := TIdeCargo.Create;
  FDadosCargo   := nil;
  FNovaValidade := nil;
end;

function TInfoCargo.dadosCargoInst: Boolean;
begin
  Result := Assigned(FDadosCargo);
end;

destructor TInfoCargo.destroy;
begin
  FIdeCargo.Free;
  FreeAndNil(FDadosCargo);
  FreeAndNil(FNovaValidade);

  inherited;
end;

function TInfoCargo.getDadosCargo: TDadosCargo;
begin
  if Not(Assigned(FDadosCargo)) then
    FDadosCargo := TDadosCargo.create;
  Result := FDadosCargo;
end;

function TInfoCargo.getNovaValidade: TidePeriodo;
begin
  if Not(Assigned(FNovaValidade)) then
    FNovaValidade := TIdePeriodo.Create;
  Result := FNovaValidade;
end;

function TInfoCargo.novaValidadeInst: Boolean;
begin
  Result := Assigned(FNovaValidade);
end;

{ TEvtTabCargo }

constructor TEvtTabCargo.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  fIdeEvento     := TIdeEvento.Create;
  fIdeEmpregador := TIdeEmpregador.Create;
  fInfoCargo     := TInfoCargo.Create;
end;

destructor TEvtTabCargo.Destroy;
begin
  fIdeEvento.Free;
  fIdeEmpregador.Free;
  fInfoCargo.Free;

  inherited;
end;

procedure TEvtTabCargo.GerarCargoPublico;
begin
  if (infoCargo.DadosCargo.cargoPublicInst()) then
  begin
    Gerador.wGrupo('cargoPublico');

    Gerador.wCampo(tcStr, '', 'acumCargo',   1, 1, 1, eSAcumCargoToStr(infoCargo.DadosCargo.cargoPublico.acumCargo));
    Gerador.wCampo(tcStr, '', 'contagemEsp', 1, 1, 1, eSContagemEspToStr(infoCargo.DadosCargo.cargoPublico.contagemEsp));
    Gerador.wCampo(tcStr, '', 'dedicExcl',   1, 1, 1, eSSimNaoToStr(infoCargo.DadosCargo.cargoPublico.dedicExcl));

    GerarLeiCargo;

    Gerador.wGrupo('/cargoPublico');
  end;
end;

procedure TEvtTabCargo.GerarDadosCargo;
begin
  Gerador.wGrupo('dadosCargo');

  Gerador.wCampo(tcStr, '', 'nmCargo', 1, 100, 1, self.InfoCargo.dadosCargo.nmCargo);
  Gerador.wCampo(tcStr, '', 'codCBO',  1,   6, 1, self.InfoCargo.dadosCargo.codCBO);

  GerarCargoPublico;

  Gerador.wGrupo('/dadosCargo');
end;

procedure TEvtTabCargo.GerarIdeCargo;
begin
  Gerador.wGrupo('ideCargo');

  Gerador.wCampo(tcStr, '', 'codCargo', 1, 30, 1, infoCargo.ideCargo.CodCargo);
  Gerador.wCampo(tcStr, '', 'iniValid', 7,  7, 1, infoCargo.ideCargo.iniValid);
  Gerador.wCampo(tcStr, '', 'fimValid', 7,  7, 0, infoCargo.ideCargo.fimValid);

  Gerador.wGrupo('/ideCargo');
end;

procedure TEvtTabCargo.GerarLeiCargo;
begin
  Gerador.wGrupo('leiCargo');

  Gerador.wCampo(tcStr, '', 'nrLei',     1, 12, 1, infoCargo.DadosCargo.cargoPublico.leiCargo.nrLei);
  Gerador.wCampo(tcDat, '', 'dtLei',    10, 10, 1, infoCargo.DadosCargo.cargoPublico.leiCargo.dtLei);

  Gerador.wGrupo('/leiCargo');
end;

function TEvtTabCargo.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTabCargo');
    Gerador.wGrupo('evtTabCargo Id="' + Self.Id + '"');

    GerarIdeEvento(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    Gerador.wGrupo('infoCargo');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdeCargo;

    if Self.ModoLancamento <> mlExclusao then
    begin
      GerarDadosCargo;

      if Self.ModoLancamento = mlAlteracao then
        if (InfoCargo.novaValidadeInst()) then
          GerarIdePeriodo(self.InfoCargo.NovaValidade, 'novaValidade');
    end;

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoCargo');
    Gerador.wGrupo('/evtTabCargo');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabCargo');

//    Validar(schevtTabCargo);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtTabCargo.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtTabCargo';
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

      sSecao := 'ideCargo';
      infoCargo.ideCargo.codCargo := INIRec.ReadString(sSecao, 'codCargo', EmptyStr);
      infoCargo.ideCargo.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
      infoCargo.ideCargo.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);

      if (ModoLancamento <> mlExclusao) then
      begin
        sSecao := 'dadosCargo';
        infoCargo.dadosCargo.nmCargo := INIRec.ReadString(sSecao, 'nmCargo', EmptyStr);
        infoCargo.dadosCargo.codCBO  := INIRec.ReadString(sSecao, 'codCBO', '1');

        sSecao := 'cargoPublico';
        if INIRec.ReadString(sSecao, 'acumCargo', '') <> '' then
        begin
          infoCargo.dadosCargo.cargoPublico.acumCargo   := eSStrToAcumCargo(Ok, INIRec.ReadString(sSecao, 'acumCargo', '1'));
          infoCargo.dadosCargo.cargoPublico.contagemEsp := eSStrToContagemEsp(Ok, INIRec.ReadString(sSecao, 'contagemEsp', '1'));
          infoCargo.dadosCargo.cargoPublico.dedicExcl   := eSStrToSimNao(Ok, INIRec.ReadString(sSecao, 'dedicExcl', 'S'));

          sSecao := 'leiCargo';
          infoCargo.dadosCargo.cargoPublico.leiCargo.nrLei    := INIRec.ReadString(sSecao, 'nrLei', '');
          infoCargo.dadosCargo.cargoPublico.leiCargo.dtLei    := StringToDateTime(INIRec.ReadString(sSecao, 'dtLei', '0'));
        end;

        if ModoLancamento = mlAlteracao then
        begin
          sSecao := 'novaValidade';
          infoCargo.novaValidade.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
          infoCargo.novaValidade.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);
        end;
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

{ TCargoPublico }

constructor TCargoPublico.Create;
begin
  inherited Create;
  FLeiCargo := TLeiCargo.Create;
end;

destructor TCargoPublico.Destroy;
begin
  FLeiCargo.Free;

  inherited;
end;

{ TDadosCargo }

function TDadosCargo.cargoPublicInst: Boolean;
begin
  Result := Assigned(FCargoPublico);
end;

constructor TDadosCargo.Create;
begin
  inherited Create;
  FCargoPublico := nil;
end;

destructor TDadosCargo.Destroy;
begin
  FreeAndNil(FCargoPublico);

  inherited;
end;

function TDadosCargo.getCargoPublico: TCargoPublico;
begin
  if Not(Assigned(FCargoPublico)) then
    FCargoPublico := TCargoPublico.create;
  Result := FCargoPublico;
end;

end.

