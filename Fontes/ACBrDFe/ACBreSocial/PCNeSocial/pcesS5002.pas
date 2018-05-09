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

unit pcesS5002;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnLeitor, ACBrUtil,
  pcesCommon, pcesConversaoeSocial;

type
  TS5002 = class;
  TInfoDep = class;

  TInfoIrrfCollection = class;
  TInfoIrrfCollectionItem = class;
  TbasesIrrfCollection = class;
  TbasesIrrfCollectionItem = class;
  TirrfCollection = class;
  TirrfCollectionItem = class;
  TidePgtoExt = class;
  TEvtIrrfBenef = class;

  TS5002 = class(TInterfacedObject, IEventoeSocial)
  private
    FTipoEvento: TTipoEvento;
    FEvtirrfBenef: TEvtirrfBenef;

    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
    procedure SetEvtirrfBenef(const Value: TEvtirrfBenef);

  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento : TObject;

  published
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property EvtirrfBenef: TEvtirrfBenef read FEvtirrfBenef write setEvtirrfBenef;
  end;

  TInfoDep = class(TPersistent)
  private
    FvrDedDep: Double;
  public
    property vrDedDep: Double read FvrDedDep write FvrDedDep;
  end;

  TInfoIrrfCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TInfoIrrfCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoIrrfCollectionItem);
  public
    constructor Create(AOwner: TEvtIrrfBenef);
    function Add: TInfoIrrfCollectionItem;
    property Items[Index: Integer]: TInfoIrrfCollectionItem read GetItem write SetItem;
  end;

  TInfoIrrfCollectionItem = class(TCollectionItem)
  private
    FCodCateg: integer;
    FindResBr: String;
    FbasesIrrf: TbasesIrrfCollection;
    Firrf: TirrfCollection;
    FidePgtoExt: TidePgtoExt;

    procedure SetbasesIrrf(const Value: TbasesIrrfCollection);
    procedure Setirrf(const Value: TirrfCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property CodCateg: integer read FCodCateg write FCodCateg;
    property indResBr: String read FindResBr write FindResBr;
    property basesIrrf: TbasesIrrfCollection read FbasesIrrf write SetbasesIrrf;
    property irrf: TirrfCollection read Firrf write Setirrf;
    property idePgtoExt: TidePgtoExt read FidePgtoExt write FidePgtoExt;
  end;

  TbasesIrrfCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TbasesIrrfCollectionItem;
    procedure SetItem(Index: Integer; Value: TbasesIrrfCollectionItem);
  public
    constructor Create(AOwner: TInfoIrrfCollectionItem);
    function Add: TbasesIrrfCollectionItem;
    property Items[Index: Integer]: TbasesIrrfCollectionItem read GetItem write SetItem;
  end;

  TbasesIrrfCollectionItem = class(TCollectionItem)
  private
    Fvalor: Double;
    FtpValor: Integer;
  public
    property tpValor: Integer read FtpValor write FtpValor;
    property valor: Double read Fvalor write Fvalor;
  end;

  TirrfCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TirrfCollectionItem;
    procedure SetItem(Index: Integer; Value: TirrfCollectionItem);
  public
    constructor Create(AOwner: TInfoIrrfCollectionItem);
    function Add: TirrfCollectionItem;
    property Items[Index: Integer]: TirrfCollectionItem read GetItem write SetItem;
  end;

  TirrfCollectionItem = class(TCollectionItem)
  private
    FtpCR: string;
    FvrIrrfDesc: Double;
  public
    property tpCR: string read FtpCR write FtpCR;
    property vrIrrfDesc: Double read FvrIrrfDesc write FvrIrrfDesc;
  end;

  TidePgtoExt = class(TPersistent)
  private
    FidePais: TidePais;
    FendExt: TendExt;
  public
    constructor Create(AOwner: TInfoIrrfCollectionItem);
    destructor Destroy; override;

    property idePais: TidePais read FidePais write FidePais;
    property endExt: TendExt read FendExt write FendExt;
  end;

  TEvtIrrfBenef = class(TPersistent)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;

    FIdeEvento: TIdeEvento5;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTrabalhador: TIdeTrabalhador3;
    FInfoDep: TInfoDep;
    FInfoIrrf: TInfoIrrfCollection;

    procedure SetInfoIrrf(const Value: TInfoIrrfCollection);
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: boolean;
    function SalvarINI: boolean;

    property IdeEvento: TIdeEvento5 read FIdeEvento write FIdeEvento;
    property IdeEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTrabalhador: TIdeTrabalhador3 read FIdeTrabalhador write FIdeTrabalhador;
    property InfoDep: TInfoDep read FInfoDep write FInfoDep;
    property InfoIrrf: TInfoIrrfCollection read FInfoIrrf write SetInfoIrrf;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String      read FId     write FId;
    property XML: String     read FXML    write FXML;
  end;

implementation

uses
  IniFiles;

{ TS5002 }

constructor TS5002.Create;
begin
  FTipoEvento := teS5002;
  FEvtIrrfBenef := TEvtIrrfBenef.Create;
end;

destructor TS5002.Destroy;
begin
  FEvtIrrfBenef.Free;

  inherited;
end;

function TS5002.GetEvento : TObject;
begin
  Result := self;
end;

function TS5002.GetXml : string;
begin
  Result := FEvtIrrfBenef.XML;
end;

procedure TS5002.SetXml(const Value: string);
begin
  if Value = FEvtIrrfBenef.XML then Exit;

  FEvtIrrfBenef.XML := Value;
  FEvtIrrfBenef.Leitor.Arquivo := Value;
  FEvtIrrfBenef.LerXML;

end;

function TS5002.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

procedure TS5002.SetEvtIrrfBenef(const Value: TEvtIrrfBenef);
begin
  FEvtIrrfBenef.Assign(Value);
end;

{ TInfoIrrfCollection }

function TInfoIrrfCollection.Add: TInfoIrrfCollectionItem;
begin
  Result := TInfoIrrfCollectionItem(inherited Add);
  Result.create;
end;

constructor TInfoIrrfCollection.Create(AOwner: TEvtIrrfBenef);
begin
  inherited create(TInfoIrrfCollectionItem);
end;

function TInfoIrrfCollection.GetItem(
  Index: Integer): TInfoIrrfCollectionItem;
begin
  Result := TInfoIrrfCollectionItem(inherited GetItem(Index));
end;

procedure TInfoIrrfCollection.SetItem(Index: Integer;
  Value: TInfoIrrfCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInfoIrrfCollectionItem }

constructor TInfoIrrfCollectionItem.Create;
begin
  FbasesIrrf  := TbasesIrrfCollection.Create(Self);
  Firrf       := TirrfCollection.Create(Self);
  FidePgtoExt := TidePgtoExt.Create(Self);
end;

destructor TInfoIrrfCollectionItem.Destroy;
begin
  FbasesIrrf.Free;
  Firrf.Free;
  FidePgtoExt.Free;

  inherited;
end;

procedure TInfoIrrfCollectionItem.SetbasesIrrf(const Value: TbasesIrrfCollection);
begin
  FbasesIrrf := Value;
end;

procedure TInfoIrrfCollectionItem.Setirrf(const Value: TirrfCollection);
begin
  Firrf := Value;
end;

{ TbaseIrrfCollection }

function TbasesIrrfCollection.Add: TbasesIrrfCollectionItem;
begin
  Result := TbasesIrrfCollectionItem(inherited Add);
end;

constructor TbasesIrrfCollection.Create(AOwner: TInfoIrrfCollectionItem);
begin
  inherited create(TbasesIrrfCollectionItem);
end;

function TbasesIrrfCollection.GetItem(
  Index: Integer): TbasesIrrfCollectionItem;
begin
  Result := TbasesIrrfCollectionItem(inherited GetItem(Index));
end;

procedure TbasesIrrfCollection.SetItem(Index: Integer;
  Value: TbasesIrrfCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TirrfCollection }

function TirrfCollection.Add: TirrfCollectionItem;
begin
  Result := TirrfCollectionItem(inherited Add);
end;

constructor TirrfCollection.Create(AOwner: TInfoIrrfCollectionItem);
begin
  inherited create(TirrfCollectionItem);
end;

function TirrfCollection.GetItem(Index: Integer): TirrfCollectionItem;
begin
  Result := TirrfCollectionItem(inherited GetItem(Index));
end;

procedure TirrfCollection.SetItem(Index: Integer;
  Value: TirrfCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TidePgtoExt }

constructor TidePgtoExt.Create(AOwner: TInfoIrrfCollectionItem);
begin
  FidePais := TidePais.Create;
  FendExt := TendExt.Create;
end;

destructor TidePgtoExt.Destroy;
begin
  FidePais.Free;
  FendExt.Free;

  inherited;
end;

{ TEvtIrrfBenef }

constructor TEvtIrrfBenef.Create;
begin
  FLeitor := TLeitor.Create;

  FIdeEvento := TIdeEvento5.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FIdeTrabalhador := TIdeTrabalhador3.Create;
  FInfoDep := TInfoDep.Create;
  FInfoIrrf := TInfoIrrfCollection.Create(Self);
end;

destructor TEvtIrrfBenef.Destroy;
begin
  FLeitor.Free;

  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FIdeTrabalhador.Free;
  FInfoDep.Free;
  FInfoIrrf.Free;

  inherited;
end;

procedure TEvtIrrfBenef.SetInfoIrrf(const Value: TInfoIrrfCollection);
begin
  FInfoIrrf := Value;
end;

function TEvtIrrfBenef.LerXML: boolean;
var
  ok: Boolean;
  i, j: Integer;
begin
  Result := False;
  try
    XML := Leitor.Arquivo;

    if leitor.rExtrai(1, 'evtIrrfBenef') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

      if leitor.rExtrai(2, 'ideEvento') <> '' then
      begin
        IdeEvento.nrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
        IdeEvento.IndApuracao  := eSStrToIndApuracao(ok, leitor.rCampo(tcStr, 'IndApuracao'));
        IdeEvento.perApur      := leitor.rCampo(tcStr, 'perApur');
      end;

      if leitor.rExtrai(2, 'ideEmpregador') <> '' then
      begin
        IdeEmpregador.TpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
        IdeEmpregador.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
      end;

      if leitor.rExtrai(2, 'ideTrabalhador') <> '' then
        IdeTrabalhador.cpfTrab := leitor.rCampo(tcStr, 'cpfTrab');

      if leitor.rExtrai(2, 'infoDep') <> '' then
        infoDep.vrDedDep := leitor.rCampo(tcDe2, 'vrDedDep');

      i := 0;
      while Leitor.rExtrai(2, 'infoIrrf', '', i + 1) <> '' do
      begin
        InfoIrrf.Add;
        InfoIrrf.Items[i].CodCateg := leitor.rCampo(tcInt, 'codCateg');
        InfoIrrf.Items[i].indResBr := leitor.rCampo(tcStr, 'indResBr');

        j := 0;
        while Leitor.rExtrai(3, 'basesIrrf', '', j + 1) <> '' do
        begin
          InfoIrrf.Items[i].basesIrrf.Add;
          InfoIrrf.Items[i].basesIrrf.Items[j].tpValor := leitor.rCampo(tcInt, 'tpValor');
          InfoIrrf.Items[i].basesIrrf.Items[j].valor   := leitor.rCampo(tcDe2, 'valor');
          inc(j);
        end;

        j := 0;
        while Leitor.rExtrai(3, 'irrf', '', j + 1) <> '' do
        begin
          InfoIrrf.Items[i].irrf.Add;
          InfoIrrf.Items[i].irrf.Items[j].tpCR       := leitor.rCampo(tcStr, 'tpCR');
          InfoIrrf.Items[i].irrf.Items[j].vrIrrfDesc := leitor.rCampo(tcDe2, 'vrIrrfDesc');
          inc(j);
        end;
        
        if leitor.rExtrai(3, 'idePgtoExt') <> '' then
        begin
          if leitor.rExtrai(4, 'idePais') <> '' then
          begin
            InfoIrrf.Items[i].idePgtoExt.idePais.codPais  := leitor.rCampo(tcStr, 'codPais');
            InfoIrrf.Items[i].idePgtoExt.idePais.indNIF   := eSStrToIndNIF(ok, leitor.rCampo(tcStr, 'indNIF'));
            InfoIrrf.Items[i].idePgtoExt.idePais.nifBenef := leitor.rCampo(tcStr, 'nifBenef');
          end;

          if leitor.rExtrai(4, 'endExt') <> '' then
          begin
            InfoIrrf.Items[i].idePgtoExt.endExt.dscLograd := leitor.rCampo(tcStr, 'dscLograd');
            InfoIrrf.Items[i].idePgtoExt.endExt.nrLograd  := leitor.rCampo(tcStr, 'nrLograd');
            InfoIrrf.Items[i].idePgtoExt.endExt.complem   := leitor.rCampo(tcStr, 'complem');
            InfoIrrf.Items[i].idePgtoExt.endExt.bairro    := leitor.rCampo(tcStr, 'bairro');
            InfoIrrf.Items[i].idePgtoExt.endExt.nmCid     := leitor.rCampo(tcStr, 'nmCid');
            InfoIrrf.Items[i].idePgtoExt.endExt.codPostal := leitor.rCampo(tcStr, 'codPostal');
          end;
        end;

        inc(i);
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

function TEvtIrrfBenef.SalvarINI: boolean;
var
  AIni: TMemIniFile;
  sSecao: String;
  i, j: Integer;
begin
  Result := False;

  AIni := TMemIniFile.Create('');
  try
    Result := True;

    with Self do
    begin
      sSecao := 'evtIrrfBenef';
      AIni.WriteString(sSecao, 'Id', Id);

      sSecao := 'ideEvento';
      AIni.WriteString(sSecao, 'nrRecArqBase', IdeEvento.nrRecArqBase);
      AIni.WriteString(sSecao, 'perApur',      IdeEvento.perApur);

      sSecao := 'ideEmpregador';
      AIni.WriteString(sSecao, 'tpInsc', eSTpInscricaoToStr(IdeEmpregador.TpInsc));
      AIni.WriteString(sSecao, 'nrInsc', IdeEmpregador.nrInsc);

      sSecao := 'ideTrabalhador';
      AIni.WriteString(sSecao, 'cpfTrab', ideTrabalhador.cpfTrab);

      sSecao := 'infoDep';
      AIni.WriteFloat(sSecao, 'vrDedDep', infoDep.vrDedDep);

      for i := 0 to infoIrrf.Count -1 do
      begin
        sSecao := 'infoIrrf' + IntToStrZero(I, 1);

        AIni.WriteInteger(sSecao, 'codCateg', infoIrrf.Items[i].CodCateg);
        AIni.WriteString(sSecao, 'indResBr',  infoIrrf.Items[i].indResBr);

        for j := 0 to InfoIrrf.Items[i].basesIrrf.Count -1 do
        begin
          sSecao := 'basesIrrf' + IntToStrZero(I, 1) + IntToStrZero(j, 2);

          AIni.WriteInteger(sSecao, 'tpValor', InfoIrrf.Items[i].basesIrrf.Items[j].tpValor);
          AIni.WriteFloat(sSecao, 'valor',     InfoIrrf.Items[i].basesIrrf.Items[j].valor);
        end;

        for j := 0 to InfoIrrf.Items[i].irrf.Count -1 do
        begin
          sSecao := 'irrf' + IntToStrZero(I, 1) + IntToStrZero(j, 2);

          AIni.WriteString(sSecao, 'tpCR',      InfoIrrf.Items[i].irrf.Items[j].tpCR);
          AIni.WriteFloat(sSecao, 'vrIrrfDesc', InfoIrrf.Items[i].irrf.Items[j].vrIrrfDesc);
        end;

        sSecao := 'idePais' + IntToStrZero(I, 1);

        AIni.WriteString(sSecao, 'codPais',  infoIrrf.Items[i].idePgtoExt.idePais.codPais);
        AIni.WriteString(sSecao, 'indNIF',   eSIndNIFToStr(infoIrrf.Items[i].idePgtoExt.idePais.indNIF));
        AIni.WriteString(sSecao, 'nifBenef', infoIrrf.Items[i].idePgtoExt.idePais.nifBenef);

        sSecao := 'endExt' + IntToStrZero(I, 1);

        AIni.WriteString(sSecao, 'dscLograd', infoIrrf.Items[i].idePgtoExt.endExt.dscLograd);
        AIni.WriteString(sSecao, 'nrLograd',  infoIrrf.Items[i].idePgtoExt.endExt.nrLograd);
        AIni.WriteString(sSecao, 'complem',   infoIrrf.Items[i].idePgtoExt.endExt.complem);
        AIni.WriteString(sSecao, 'bairro',    infoIrrf.Items[i].idePgtoExt.endExt.bairro);
        AIni.WriteString(sSecao, 'nmCid',     infoIrrf.Items[i].idePgtoExt.endExt.nmCid);
        AIni.WriteString(sSecao, 'codPostal', infoIrrf.Items[i].idePgtoExt.endExt.codPostal);
      end;
    end;
  finally
    AIni.Free;
  end;
end;

end.
