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

unit pcesS2250;

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
  ACBrBase, pcnConversao,
  pcesCommon, pcesConversaoeSocial, pcesGerador;

type
  TS2250CollectionItem = class;
  TEvtAvPrevio = class;
  TInfoAvPrevio = class;
  TDetAvPrevio = class;
  TCancAvPrevio = class;

  TS2250Collection = class(TeSocialCollection)
  private
    function GetItem(Index: Integer): TS2250CollectionItem;
    procedure SetItem(Index: Integer; Value: TS2250CollectionItem);
  public
    function Add: TS2250CollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TS2250CollectionItem;
    property Items[Index: Integer]: TS2250CollectionItem read GetItem write SetItem; default;
  end;

  TS2250CollectionItem = class(TObject)
  private
    FTipoEvento: TTipoEvento;
    FEvtAvPrevio: TEvtAvPrevio;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtAvPrevio: TEvtAvPrevio read FEvtAvPrevio write FEvtAvPrevio;
  end;

  TEvtAvPrevio = class(TeSocialEvento)
  private
    FIdeEvento: TIdeEvento2;
    FIdeEmpregador: TIdeEmpregador;
    FIdeVinculo: TIdeVinculo;
    FInfoAvPrevio: TInfoAvPrevio;

    procedure GerarInfoAvPrevio;
    procedure GerarDetAvPrevio;
    procedure GerarCancAvPrevio;
  public
    constructor Create(AACBreSocial: TObject); override;
    destructor  Destroy; override;

    function GerarXML: boolean; override;
    function LerArqIni(const AIniString: String): Boolean;

    property IdeEvento: TIdeEvento2 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeVinculo: TIdeVinculo read FIdeVinculo write FIdeVinculo;
    property InfoAvPrevio: TInfoAvPrevio read FInfoAvPrevio write FInfoAvPrevio;
  end;

  TInfoAvPrevio = class
  private
    FDetAvPrevio : TDetAvPrevio;
    FCancAvPrevio: TCancAvPrevio;
  public
    constructor Create;
    destructor Destroy; override;
    property DetAvPrevio: TDetAvPrevio read FDetAvPrevio write FDetAvPrevio;
    property CancAvPrevio: TCancAvPrevio read FCancAvPrevio write FCancAvPrevio;
  end;

  TDetAvPrevio = class
  private
    FdtAvPrv: TDateTime;
    FdtPrevDeslig: TDateTime;
    Fobservacao: string;
  public
    property dtAvPrv: TDateTime read FdtAvPrv write FdtAvPrv;
    property dtPrevDeslig: TDateTime read FdtPrevDeslig write FdtPrevDeslig;
    property observacao: string read Fobservacao write Fobservacao;
  end;

  TCancAvPrevio = class
  private
    FdtCancAvPrv: TDateTime;
    Fobservacao: string;
  public
    property dtCancAvPrv: TDateTime read FdtCancAvPrv write FdtCancAvPrv;
    property observacao: string read Fobservacao write Fobservacao;
  end;

implementation

uses
  IniFiles,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBreSocial;

{ TS2250Collection }

function TS2250Collection.Add: TS2250CollectionItem;
begin
  Result := Self.New;
end;

function TS2250Collection.GetItem(Index: Integer): TS2250CollectionItem;
begin
  Result := TS2250CollectionItem(inherited Items[Index]);
end;

procedure TS2250Collection.SetItem(Index: Integer;
  Value: TS2250CollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TS2250Collection.New: TS2250CollectionItem;
begin
  Result := TS2250CollectionItem.Create(FACBreSocial);
  Self.Add(Result);
end;

{ TS2250CollectionItem }

constructor TS2250CollectionItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FTipoEvento  := teS2250;
  FEvtAvPrevio := TEvtAvPrevio.Create(AOwner);
end;

destructor TS2250CollectionItem.Destroy;
begin
  FEvtAvPrevio.Free;

  inherited;
end;

{ TInfoAvPrevio }

constructor TInfoAvPrevio.Create;
begin
  inherited Create;
  FDetAvPrevio  := TDetAvPrevio.Create;
  FCancAvPrevio := TCancAvPrevio.Create;
end;

destructor TInfoAvPrevio.Destroy;
begin
  FDetAvPrevio.Free;
  FCancAvPrevio.Free;

  inherited;
end;

{ TEvtAvPrevio }

constructor TEvtAvPrevio.Create(AACBreSocial: TObject);
begin
  inherited Create(AACBreSocial);

  FIdeEvento     := TIdeEvento2.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeVinculo    := TIdeVinculo.Create;
  FInfoAvPrevio  := TInfoAvPrevio.Create;
end;


destructor TEvtAvPrevio.Destroy;
begin
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeVinculo.Free;
  FInfoAvPrevio.Free;

  inherited;
end;

procedure TEvtAvPrevio.GerarCancAvPrevio;
begin
  Gerador.wGrupo('cancAvPrevio');

  Gerador.wCampo(tcDat, '', 'dtCancAvPrv',     10,  10, 1, self.InfoAvPrevio.CancAvPrevio.dtCancAvPrv);
  Gerador.wCampo(tcStr, '', 'observacao',       1, 255, 0, self.InfoAvPrevio.CancAvPrevio.observacao);

  Gerador.wGrupo('/cancAvPrevio');
end;

procedure TEvtAvPrevio.GerarDetAvPrevio;
begin
  Gerador.wGrupo('detAvPrevio');

  Gerador.wCampo(tcDat, '', 'dtAvPrv',      10,  10, 1, self.InfoAvPrevio.DetAvPrevio.dtAvPrv);
  Gerador.wCampo(tcDat, '', 'dtPrevDeslig', 10,  10, 1, self.InfoAvPrevio.DetAvPrevio.dtPrevDeslig);
  Gerador.wCampo(tcStr, '', 'observacao',    1, 255, 0, self.InfoAvPrevio.DetAvPrevio.observacao);

  Gerador.wGrupo('/detAvPrevio');
end;

procedure TEvtAvPrevio.GerarInfoAvPrevio;
begin
  Gerador.wGrupo('infoAvPrevio');

  if (DateToStr(self.InfoAvPrevio.DetAvPrevio.dtAvPrv) <> dDataBrancoNula) then
    GerarDetAvPrevio;

  if (DateToStr(self.InfoAvPrevio.CancAvPrevio.dtCancAvPrv) <> dDataBrancoNula) then
    GerarCancAvPrevio;

  Gerador.wGrupo('/infoAvPrevio');
end;

function TEvtAvPrevio.GerarXML: boolean;
begin
  try
    inherited GerarXML;
    Self.VersaoDF := TACBreSocial(FACBreSocial).Configuracoes.Geral.VersaoDF;
     
    Self.Id := GerarChaveEsocial(now, self.ideEmpregador.NrInsc, self.Sequencial);

    GerarCabecalho('evtAvPrevio');
    Gerador.wGrupo('evtAvPrevio Id="' + Self.Id + '"');

    GerarIdeEvento2(self.IdeEvento);
    GerarIdeEmpregador(self.IdeEmpregador);
    GerarIdeVinculo(self.IdeVinculo);
    GerarInfoAvPrevio;

    Gerador.wGrupo('/evtAvPrevio');

    GerarRodape;

    FXML := Gerador.ArquivoFormatoXML;
//    XML := Assinar(Gerador.ArquivoFormatoXML, 'evtAvPrevio');

//    Validar(schevtAvPrevio);
  except on e:exception do
    raise Exception.Create('ID: ' + Self.Id + sLineBreak + ' ' + e.Message);
  end;

  Result := (Gerador.ArquivoFormatoXML <> '')
end;

function TEvtAvPrevio.LerArqIni(const AIniString: String): Boolean;
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
      sSecao := 'evtAvPrevio';
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

      sSecao := 'ideVinculo';
      ideVinculo.CpfTrab   := INIRec.ReadString(sSecao, 'cpfTrab', EmptyStr);
      ideVinculo.NisTrab   := INIRec.ReadString(sSecao, 'nisTrab', EmptyStr);
      ideVinculo.Matricula := INIRec.ReadString(sSecao, 'matricula', EmptyStr);

      sSecao := 'detAvPrevio';
      if INIRec.ReadString(sSecao, 'dtAvPrv', '') <> ''then
      begin
        InfoAvPrevio.detAvPrevio.dtAvPrv      := StringToDateTime(INIRec.ReadString(sSecao, 'dtAvPrv', '0'));
        InfoAvPrevio.detAvPrevio.dtPrevDeslig := StringToDateTime(INIRec.ReadString(sSecao, 'dtPrevDeslig', '0'));
        InfoAvPrevio.DetAvPrevio.observacao   := INIRec.ReadString(sSecao, 'observacao', EmptyStr);
      end;

      sSecao := 'cancAvPrevio';
      if INIRec.ReadString(sSecao, 'dtCancAvPrv', '') <> ''then
      begin
        InfoAvPrevio.cancAvPrevio.dtCancAvPrv     := StringToDateTime(INIRec.ReadString(sSecao, 'dtCancAvPrv', '0'));
        InfoAvPrevio.cancAvPrevio.observacao      := INIRec.ReadString(sSecao, 'observacao', EmptyStr);
      end;
    end;

    GerarXML;
    XML := FXML;
  finally
    INIRec.Free;
  end;
end;

end.
