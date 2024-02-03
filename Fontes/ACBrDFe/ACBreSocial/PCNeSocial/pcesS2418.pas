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

unit pcesS2418;

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
  TS2418Collection = class;
  TS2418CollectionItem = class;
  TEvtReativBen = class;
  TIdeBeneficio = class;
  TInfoReativ = class;

  TS2418Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2418CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2418CollectionItem);
  public
    function Add: TS2418CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2418CollectionItem;
    property Items[Index: Integer]: TS2418CollectionItem read GetItem write SetItem; default;
  end;

  TS2418CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtReativBen : TEvtReativBen;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtReativBen: TEvtReativBen read FEvtReativBen write FEvtReativBen;
  end;

  TEvtReativBen = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeBeneficio: TIdeBeneficio;
    FInfoReativ: TInfoReativ;

    procedure GerarIdeBeneficio(pIdeBeneficio: TIdeBeneficio);
    procedure GerarInfoReativ(pInfoReativ: TInfoReativ);
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property ideEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property ideBeneficio: TIdeBeneficio read FIdeBeneficio write FIdeBeneficio;
    property infoReativ: TInfoReativ read FInfoReativ write FInfoReativ;
  end;
  
  TIdeBeneficio = class(TObject)
  private
    FCpfBenef: string;
    FNrBeneficio: string;
  public
    property cpfBenef: String read FCpfBenef write FCpfBenef;
    property nrBeneficio: string read FNrBeneficio write FNrBeneficio;
  end;

  TInfoReativ = class(TObject)
  private
    FDtEfetReativ: TDateTime;
    FDtEfeito: TDateTime;
  public
    property dtEfetReativ: TDateTime read FDtEfetReativ write FDtEfetReativ;
    property dtEfeito: TDateTime read FDtEfeito write FDtEfeito;
  end;
  
implementation

uses
  IniFiles,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2418Collection }

function TS2418Collection.Add: TS2418CollectionItem;
begin
  Result := Self.New;
end;

function TS2418Collection.GetItem(Index: Integer): TS2418CollectionItem;
begin
  Result := TS2418CollectionItem(inherited Items[Index]);
end;

procedure TS2418Collection.SetItem(Index: Integer; Value: TS2418CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2418Collection.New: TS2418CollectionItem;
begin
  Result := TS2418CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2418CollectionItem }

constructor TS2418CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento   := teS2418;
  FEvtReativBen  := TEvtReativBen.Create(AOwner);
end;

destructor TS2418CollectionItem.Destroy;
begin
  FreeAndNil(FEvtReativBen);

  inherited;
end;

{ TEvtReativBen }

constructor TEvtReativBen.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeBeneficio  := TIdeBeneficio.Create;
  FInfoReativ    := TInfoReativ.Create;
end;

destructor TEvtReativBen.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeBeneficio.Free;
  FInfoReativ.Free;

  inherited;
end;

procedure TEvtReativBen.GerarIdeBeneficio(pIdeBeneficio: TIdeBeneficio);
begin
  Gerador.wGrupo('ideBeneficio');

  Gerador.wCampo(tcStr, '', 'cpfBenef',     11, 11, 1, pIdeBeneficio.cpfBenef);
  Gerador.wCampo(tcStr, '', 'nrBeneficio',  20, 20, 1, pIdeBeneficio.nrBeneficio);

  Gerador.wGrupo('/ideBeneficio');
end;

procedure TEvtReativBen.GerarInfoReativ(pInfoReativ: TInfoReativ);
begin
  Gerador.wGrupo('infoReativ');

  Gerador.wCampo(tcDat, '', 'dtEfetReativ',  10, 10, 1, pInfoReativ.dtEfetReativ);
  Gerador.wCampo(tcDat, '', 'dtEfeito',      10, 10, 1, pInfoReativ.dtEfeito);

  Gerador.wGrupo('/infoReativ');
end;

function TEvtReativBen.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtReativBen');
    Gerador.wGrupo('evtReativBen Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeBeneficio(self.IdeBeneficio);
    GerarInfoReativ(self.InfoReativ);

    Gerador.wGrupo('/evtReativBen');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'EvtReativBen');

//    Validar(schEvtReativBen);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtReativBen.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtReativBen';
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

      sSecao := 'infoReativ';
      infoReativ.dtEfetReativ := StringToDateTime(INIRec.ReadString(sSecao, 'dtEfetReativ', '0'));
      infoReativ.dtEfeito := StringToDateTime(INIRec.ReadString(sSecao, 'dtEfeito', '0'));
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
