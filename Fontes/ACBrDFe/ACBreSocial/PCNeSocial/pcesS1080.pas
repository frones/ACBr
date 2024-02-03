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

unit pcesS1080;

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
  ACBrBase,
  pcnConversao,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS1080CollectionItem = class;
  TEvtTabOperPort = class;
  TInfoOperPortuario = class;

  TS1080Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS1080CollectionItem;
    procedure SetItem(Index: Integer; Value: TS1080CollectionItem);
  public
    function Add: TS1080CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS1080CollectionItem;
    property Items[Index: Integer]: TS1080CollectionItem read GetItem write SetItem; default;
  end;

  TS1080CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtTabOperPortuario: TEvtTabOperPort;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtTabOperPortuario: TEvtTabOperPort read FEvtTabOperPortuario write FEvtTabOperPortuario;
  end;

  TEvtTabOperPort = class(TESocialEvento)
  private
    FModoLancamento: TModoLancamento;
    fIdeEvento: TIdeEvento;
    fIdeEmpregador: TIdeEmpregador;
    fInfoOperPortuario: TInfoOperPortuario;

    {Geradores específicos da classe}
    procedure GerarIdeOperPortuario;
    procedure GerarDadosOperPortuario;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ModoLancamento: TModoLancamento read FModoLancamento write FModoLancamento;
    property IdeEvento: TIdeEvento read fIdeEvento write fIdeEvento;
    property IdeEmpregador: TIdeEmpregador read fIdeEmpregador write fIdeEmpregador;
    property InfoOperPortuario: TInfoOperPortuario read fInfoOperPortuario write fInfoOperPortuario;
  end;

  TDadosOperPortuario = class(TObject)
  private
    FAliqRat: tpAliqRat;
    FFap: Double;
    FAliqRatAjust: Double;
  public
    property aliqRat: tpAliqRat read FAliqRat write FAliqRat;
    property fap: Double read FFap write FFap;
    property aliqRatAjust: Double read FAliqRatAjust write FAliqRatAjust;
  end;

  TIdeOperPortuario = class(TObject)
  private
    FCnpjOpPortuario: string;
    FIniValid: string;
    FFimValid : string;
  public
    property cnpjOpPortuario: string read FCnpjOpPortuario write FCnpjOpPortuario;
    property iniValid: string read FIniValid write FIniValid;
    property fimValid: string read FFimValid write FFimValid;
  end;

  TInfoOperPortuario = class(TObject)
  private
    FIdeOperPortuario: TIdeOperPortuario;
    FDadosOperPortuario: TDadosOperPortuario;
    FNovaValidade: TIdePeriodo;

    function getDadosOperPortuario: TDadosOperPortuario;
    function getNovaValidade: TIdePeriodo;
  public
    constructor Create;
    destructor Destroy; override;
    
    function dadosOperPortuarioInst(): Boolean;
    function novaValidadeInst(): Boolean;

    property ideOperPortuario: TIdeOperPortuario read FIdeOperPortuario write FIdeOperPortuario;
    property dadosOperPortuario: TDadosOperPortuario read getDadosOperPortuario write FDadosOperPortuario;
    property novaValidade: TIdePeriodo read getNovaValidade write FNovaValidade;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBreSocial;

{ TS1080Collection }

function TS1080Collection.Add: TS1080CollectionItem;
begin
  Result := Self.New;
end;

function TS1080Collection.GetItem(Index: Integer): TS1080CollectionItem;
begin
  Result := TS1080CollectionItem(inherited Items[Index]);
end;

procedure TS1080Collection.SetItem(Index: Integer;
  Value: TS1080CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS1080Collection.New: TS1080CollectionItem;
begin
  Result := TS1080CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS1080CollectionItem }

constructor TS1080CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento          := teS1080;
  FEvtTabOperPortuario := TEvtTabOperPort.Create(AOwner);
end;

destructor TS1080CollectionItem.Destroy;
begin
  FEvtTabOperPortuario.Free;

  inherited;
end;

{ TInfoOperPortuario }

constructor TInfoOperPortuario.Create;
begin
  inherited Create;
  FIdeOperPortuario   := TIdeOperPortuario.Create;
  FDadosOperPortuario := nil;
  FNovaValidade       := nil;
end;

function TInfoOperPortuario.dadosOperPortuarioInst: Boolean;
begin
  Result := Assigned(FDadosOperPortuario);
end;

destructor TInfoOperPortuario.Destroy;
begin
  FIdeOperPortuario.Free;
  FreeAndNil(FDadosOperPortuario);
  FreeAndNil(FNovaValidade);

  inherited;
end;

function TInfoOperPortuario.getDadosOperPortuario: TDadosOperPortuario;
begin
  if Not(Assigned(FDadosOperPortuario)) then
    FDadosOperPortuario := TDadosOperPortuario.Create;  
  Result := FDadosOperPortuario;
end;

function TInfoOperPortuario.getNovaValidade: TIdePeriodo;
begin
  if Not(Assigned(FNovaValidade)) then
    FNovaValidade :=TIdePeriodo.Create;
  Result := FNovaValidade;
end;

function TInfoOperPortuario.novaValidadeInst: Boolean;
begin
  Result := Assigned(FNovaValidade);
end;

{ TEvtTabOperPortuario }

constructor TEvtTabOperPort.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  fIdeEvento         := TIdeEvento.Create;
  fIdeEmpregador     := TIdeEmpregador.Create;
  fInfoOperPortuario := TInfoOperPortuario.Create;
end;

destructor TEvtTabOperPort.Destroy;
begin
  fIdeEvento.Free;
  fIdeEmpregador.Free;
  fInfoOperPortuario.Free;

  inherited;
end;

procedure TEvtTabOperPort.GerarDadosOperPortuario;
begin
  Gerador.wGrupo('dadosOperPortuario');

  Gerador.wCampo(tcStr, '', 'aliqRat',      1, 1, 1, eSAliqRatToStr(self.InfoOperPortuario.DadosOperPortuario.aliqRat));
  Gerador.wCampo(tcDe4, '', 'fap',          1, 5, 1, self.InfoOperPortuario.DadosOperPortuario.fap);
  Gerador.wCampo(tcDe4, '', 'aliqRatAjust', 1, 5, 1, self.InfoOperPortuario.DadosOperPortuario.aliqRatAjust);

  Gerador.wGrupo('/dadosOperPortuario');
end;

procedure TEvtTabOperPort.GerarIdeOperPortuario;
begin
  Gerador.wGrupo('ideOperPortuario');

  Gerador.wCampo(tcStr, '', 'cnpjOpPortuario', 14, 14, 1, self.InfoOperPortuario.IdeOperPortuario.cnpjOpPortuario);
  Gerador.wCampo(tcStr, '', 'iniValid',         7,  7, 1, self.InfoOperPortuario.IdeOperPortuario.iniValid);
  Gerador.wCampo(tcStr, '', 'fimValid',         7,  7, 0, self.InfoOperPortuario.IdeOperPortuario.fimValid);

  Gerador.wGrupo('/ideOperPortuario');
end;

function TEvtTabOperPort.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtTabOperPort');
    Gerador.wGrupo('evtTabOperPort Id="' + Self.Id + '"');

    GerarIdeEvento(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);

    Gerador.wGrupo('infoOperPortuario');

    GerarModoAbertura(Self.ModoLancamento);
    GerarIdeOperPortuario;

    if Self.ModoLancamento <> mlExclusao then
    begin
      GerarDadosOperPortuario;

      if Self.ModoLancamento = mlAlteracao then
        if (InfoOperPortuario.novaValidadeInst()) then
          GerarIdePeriodo(self.InfoOperPortuario.NovaValidade, 'novaValidade');
    end;

    GerarModoFechamento(Self.ModoLancamento);

    Gerador.wGrupo('/infoOperPortuario');
    Gerador.wGrupo('/evtTabOperPort');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtTabOperPort');

//    Validar(schevtTabOperPort);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtTabOperPort.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtTabOperPort';
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

      sSecao := 'ideOperPortuario';
      infoOperPortuario.ideOperPortuario.cnpjOpPortuario := INIRec.ReadString(sSecao, 'cnpjOpPortuario', EmptyStr);
      infoOperPortuario.ideOperPortuario.IniValid        := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
      infoOperPortuario.ideOperPortuario.FimValid        := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);

      if (ModoLancamento <> mlExclusao) then
      begin
        sSecao := 'dadosOperPortuario';
        infoOperPortuario.dadosOperPortuario.aliqRat      := eSStrToAliqRat(Ok, INIRec.ReadString(sSecao, 'aliqRat', '1'));
        infoOperPortuario.dadosOperPortuario.fap          := StringToFloatDef(INIRec.ReadString(sSecao, 'fap', ''), 0);
        infoOperPortuario.dadosOperPortuario.aliqRatAjust := StringToFloatDef(INIRec.ReadString(sSecao, 'aliqRatAjust', ''), 0);

        if ModoLancamento = mlAlteracao then
        begin
          sSecao := 'novaValidade';
          infoOperPortuario.novaValidade.IniValid := INIRec.ReadString(sSecao, 'iniValid', EmptyStr);
          infoOperPortuario.novaValidade.FimValid := INIRec.ReadString(sSecao, 'fimValid', EmptyStr);
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

