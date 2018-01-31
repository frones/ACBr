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

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 29/02/2016: Guilherme Costa
|*  - Alterado os atributos que não estavam de acordo com o leiaute/xsd
******************************************************************************}
{$I ACBr.inc}

unit pcesS5012;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnLeitor,
  pcesCommon, pcesConversaoeSocial;

type
  TS5012Collection = class;
  TS5012CollectionItem = class;
  TInfoIRRF = class;
  TInfoCRContribCollection = class;
  TInfoCRContribCollectionItem = class;

  TEvtIrrf = class;

  TS5012Collection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TS5012CollectionItem;
    procedure SetItem(Index: Integer; Value: TS5012CollectionItem);
  public
    function Add: TS5012CollectionItem;
    property Items[Index: Integer]: TS5012CollectionItem read GetItem write SetItem; default;
  end;

  TS5012CollectionItem = class(TCollectionItem)
  private
    FTipoEvento: TTipoEvento;
    FEvtIrrf: TEvtIrrf;

    procedure setEvtIrrf(const Value: TEvtIrrf);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  published
    property TipoEvento: TTipoEvento read FTipoEvento;
    property EvtIrrf: TEvtIrrf read FEvtIrrf write setEvtIrrf;
  end;

  TInfoCRContribCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfoCRContribCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoCRContribCollectionItem);
  public
    constructor Create; reintroduce;
    function Add: TInfoCRContribCollectionItem;
    property Items[Index: Integer]: TInfoCRContribCollectionItem read GetItem write SetItem;
  end;

  TInfoCRContribCollectionItem = class(TCollectionItem)
  private
    FtpCR: String;
    FvrCR: Double;
  public
    property tpCR: String read FtpCR write FtpCR;
    property vrCR: Double read FvrCR write FvrCR;
  end;

  TInfoIRRF = class(TPersistent)
  private
    FnrRecArqBase: String;
    FindExistInfo: Integer;
    FInfoCRContrib: TInfoCRContribCollection;
  public
    property nrRecArqBase: String read FnrRecArqBase write FnrRecArqBase;
    property indExistInfo: Integer read FindExistInfo write FindExistInfo;
    property InfoCRContrib: TInfoCRContribCollection read FInfoCRContrib write FInfoCRContrib;
  end;

  TEvtIrrf = class(TPersistent)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;

    FIdeEvento: TIdeEvento5;
    FIdeEmpregador: TIdeEmpregador;
    FInfoIRRF: TInfoIRRF;
  public
    constructor Create(AACBreSocial: TObject); overload;
    destructor  Destroy; override;

    function LerXML: Boolean;

    property IdeEvento: TIdeEvento5 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property InfoIRRF: TInfoIRRF read FInfoIRRF write FInfoIRRF;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String      read FId     write FId;
    property XML: String     read FXML    write FXML;
  end;

implementation

{ TS5012Collection }

function TS5012Collection.Add: TS5012CollectionItem;
begin
  Result := TS5012CollectionItem(inherited Add);
  Result.Create(TComponent(Self.Owner));
end;

function TS5012Collection.GetItem(Index: Integer): TS5012CollectionItem;
begin
  Result := TS5012CollectionItem(inherited GetItem(Index));
end;

procedure TS5012Collection.SetItem(Index: Integer;
  Value: TS5012CollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TS5012CollectionItem }

constructor TS5012CollectionItem.Create(AOwner: TComponent);
begin
  FTipoEvento := teS5012;
  FEvtIrrf := TEvtIrrf.Create(AOwner);
end;

destructor TS5012CollectionItem.Destroy;
begin
  FEvtIrrf.Free;

  inherited;
end;

procedure TS5012CollectionItem.setEvtIrrf(
  const Value: TEvtIrrf);
begin
  FEvtIrrf.Assign(Value);
end;

{ TEvtIrrf }

constructor TEvtIrrf.Create(AACBreSocial: TObject);
begin
  FLeitor := TLeitor.Create;

  FIdeEvento := TIdeEvento5.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FInfoIRRF := TInfoIRRF.Create;
end;

destructor TEvtIrrf.Destroy;
begin
  FLeitor.Free;

  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FInfoIRRF.Free;

  inherited;
end;

{ TInfoCRContribCollection }

function TInfoCRContribCollection.Add: TInfoCRContribCollectionItem;
begin
  Result := TInfoCRContribCollectionItem(inherited Add);
end;

constructor TInfoCRContribCollection.Create;
begin
  inherited create(TInfoCRContribCollectionItem);
end;

function TInfoCRContribCollection.GetItem(
  Index: Integer): TInfoCRContribCollectionItem;
begin
  Result := TInfoCRContribCollectionItem(inherited GetItem(Index));
end;

procedure TInfoCRContribCollection.SetItem(Index: Integer;
  Value: TInfoCRContribCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TEvtIrrf.LerXML: Boolean;
var
  ok: Boolean;
  i: Integer;
begin
  Result := False;
  try
    XML := Leitor.Arquivo;

    if leitor.rExtrai(1, 'evtIrrf') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

      if leitor.rExtrai(2, 'ideEvento') <> '' then
        IdeEvento.perApur := leitor.rCampo(tcStr, 'perApur');

      if leitor.rExtrai(2, 'ideEmpregador') <> '' then
      begin
        IdeEmpregador.TpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
        IdeEmpregador.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
      end;

      if leitor.rExtrai(2, 'infoIRRF') <> '' then
      begin
        infoIRRF.nrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
        infoIRRF.indExistInfo := leitor.rCampo(tcInt, 'indExistInfo');

        i := 0;
        while Leitor.rExtrai(3, 'infoCRContrib', '', i + 1) <> '' do
        begin
          infoIRRF.infoCRContrib.Add;
          infoIRRF.infoCRContrib.Items[i].tpCR := leitor.rCampo(tcStr, 'tpCR');
          infoIRRF.infoCRContrib.Items[i].vrCR := leitor.rCampo(tcDe2, 'vrCR');
          inc(i);
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.
