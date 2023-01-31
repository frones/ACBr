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

unit pcesS5013;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase, pcnConversao, pcnLeitor,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  pcesCommon, pcesConversaoeSocial;

type
  TEvtFGTS = class;
  TInfoFGTS = class;
  TIdeEstabCollection = class;
  TIdeEstabCollectionItem = class;
  TIdeLotacaoCollection = class;
  TIdeLotacaoCollectionItem = class;
  TInfoBaseFGTS = class;
  TBasePerApurCollection = class;
  TBasePerApurCollectionItem = class;
  TInfoBasePerAntECollection = class;
  TInfoBasePerAntECollectionItem = class;
  TBasePerAntECollection = class;
  TBasePerAntECollectionItem = class;
  TInfoDpsFGTS = class;
  TDpsPerApurCollection = class;
  TDpsPerApurCollectionItem = class;
  
  TS5013 = class(TInterfacedObject, IEventoeSocial)
  private
    FTipoEvento: TTipoEvento;
    FEvtFGTS: TEvtFGTS;

    function GetXml : string;
    procedure SetXml(const Value: string);
    function GetTipoEvento : TTipoEvento;
  public
    constructor Create;
    destructor Destroy; override;

    function GetEvento : TObject;
    property Xml: String read GetXml write SetXml;
    property TipoEvento: TTipoEvento read GetTipoEvento;
    property EvtFGTS: TEvtFGTS read FEvtFGTS write FEvtFGTS;
  end;

  TBasePerApurCollectionItem = class(TObject)
  private
    FtpValor: Integer;
    FindIncid: Integer;
    FbaseFGTS: Currency;
    FvrFGTS: Currency;
  public
    property tpValor: Integer read FtpValor write FtpValor;
    property baseFGTS: Currency read FbaseFGTS write FbaseFGTS;
    property indIncid: Integer read FindIncid write FindIncid;
    property vrFGTS: Currency read FvrFGTS write FvrFGTS;
  end;

  TInfoBasePerAntECollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoBasePerAntECollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoBasePerAntECollectionItem);
  public
    function Add: TInfoBasePerAntECollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoBasePerAntECollectionItem;
    property Items[Index: Integer]: TInfoBasePerAntECollectionItem read GetItem write SetItem;
  end;

  TInfoBasePerAntECollectionItem = class(TObject)
  private
    FperRef: string;
    FtpAcConv: string;
    FBasePerAntE: TBasePerAntECollection;
   
    function getBasePerAntE(): TBasePerAntECollection;
  public
    constructor Create;
    destructor Destroy; override;
    
    function BasePerAntEInst(): Boolean;
    
    property perRef: string read FperRef write FperRef;
    property tpAcConv: string read FtpAcConv write FtpAcConv;
    property basePerAntE: TBasePerAntECollection read getBasePerAntE write FBasePerAntE;
  end;

  TBasePerAntECollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TBasePerAntECollectionItem;
    procedure SetItem(Index: Integer; Value: TBasePerAntECollectionItem);
  public
    function Add: TBasePerAntECollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TBasePerAntECollectionItem;
    property Items[Index: Integer]: TBasePerAntECollectionItem read GetItem write SetItem;
  end;

  TBasePerAntECollectionItem = class(TObject)
  private
    FtpValorE: Integer;
    FindIncidE: Integer;
    FbaseFGTSE: Currency;
    FvrFGTSE: Currency;
  public
    property tpValorE: Integer read FtpValorE write FtpValorE;
    property baseFGTSE: Currency read FbaseFGTSE write FbaseFGTSE;
    property indIncidE: Integer read FindIncidE write FindIncidE;
    property vrFGTSE: Currency read FvrFGTSE write FvrFGTSE;
  end;

  TInfoBaseFGTS = class(TObject)
  private
    FBasePerApur: TBasePerApurCollection;
    FInfoBasePerAntE: TInfoBasePerAntECollection;
    
    function getBasePerApur(): TBasePerApurCollection;
    function getInfoBasePerAntE(): TInfoBasePerAntECollection;
  public
    constructor Create;
    destructor Destroy; override;

    function BasePerApurInst(): Boolean;
    function InfoBasePerAntEInst(): Boolean;
    
    property basePerApur: TBasePerApurCollection read getBasePerApur write FBasePerApur;
    property infoBasePerAntE: TInfoBasePerAntECollection read getInfoBasePerAntE write FInfoBasePerAntE;
  end;

  TInfoFGTS = class(TObject)
  private
    FNrRecArqBase: String;
    FIndExistInfo: Integer;
    FInfoBaseFGTS: TInfoBaseFGTS;
    FInfoDpsFGTS: TInfoDpsFGTS;
    FIdeEstab: TIdeEstabCollection;

    function getInfoBaseFGTS(): TInfoBaseFGTS;
    function getInfoDpsFGTS(): TInfoDpsFGTS;
    function getIdeEstab(): TIdeEstabCollection;
  public
    constructor Create;
    destructor Destroy; override;
             
    function IdeEstabInst(): Boolean;
    function InfoBaseFGTSInst(): Boolean;
    function InfoDpsFGTSInst(): Boolean;
    
    property ideEstab: TIdeEstabCollection read getIdeEstab write FIdeEstab;
    property indExistInfo: Integer read FindExistInfo write FindExistInfo;
    property infoBaseFGTS: TInfoBaseFGTS read getInfoBaseFGTS write FInfoBaseFGTS;
    property infoDpsFGTS: TInfoDpsFGTS read getInfoDpsFGTS write FInfoDpsFGTS;
    property nrRecArqBase: string read FNrRecArqBase write FNrRecArqBase;
  end;

  TEvtFGTS = class(TObject)
  private
    FLeitor: TLeitor;
    FId: String;
    FXML: String;
    FVersaoDF: TVersaoeSocial;

    FIdeEvento: TIdeEvento5;
    FIdeEmpregador: TIdeEmpregador;
    FInfoFGTS: TInfoFGTS;
  public
    constructor Create;
    destructor  Destroy; override;

    function LerXML: Boolean;
    function SalvarINI: boolean;

    property ideEvento: TIdeEvento5 read FIdeEvento write FIdeEvento;
    property ideEmpregador: TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property infoFGTS: TInfoFGTS read FInfoFGTS write FInfoFGTS;
    property Leitor: TLeitor read FLeitor write FLeitor;
    property Id: String read FId;
    property XML: String read FXML;
    property VersaoDF: TVersaoeSocial read FVersaoDF write FVersaoDF;
  end;

  TIdeEstabCollectionItem = class(TObject)
  private
    FNrInsc: string;
    FTpInsc: tpTpInsc;
    FIdeLotacao: TIdeLotacaoCollection;

    function getIdeLotacao(): TIdeLotacaoCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function IdeLotacaoInst(): Boolean;
    
    property tpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property ideLotacao: TIdeLotacaoCollection read getIdeLotacao write FIdeLotacao;
  end;

  TIdeEstabCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdeEstabCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeEstabCollectionItem);
  public
    function Add: TIdeEstabCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeEstabCollectionItem;
    property Items[Index: Integer]: TIdeEstabCollectionItem read GetItem write SetItem;
  end;

  TIdeLotacaoCollectionItem = class(TObject)
  private
    FCodLotacao: String;
    FTpLotacao: String;
    FTpInsc: tpTpInsc;
    FNrInsc: string;
    FInfoBaseFGTS: TInfoBaseFGTS;

    function getInfoBaseFGTS(): TInfoBaseFGTS;
  public
    constructor Create;
    destructor Destroy; override;

    function InfoBaseFGTSInst(): Boolean;
    
    property codLotacao: string read FCodLotacao write FCodLotacao;
    property tpLotacao: string read FTpLotacao write FTpLotacao;
    property tpInsc: tpTpInsc read FTpInsc write FTpInsc;
    property nrInsc: string read FNrInsc write FNrInsc;
    property infoBaseFGTS: TInfoBaseFGTS read getInfoBaseFGTS write FInfoBaseFGTS;
  end;

  TIdeLotacaoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TIdeLotacaoCollectionItem;
    procedure SetItem(Index: Integer; Value: TIdeLotacaoCollectionItem);
  public
    function Add: TIdeLotacaoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TIdeLotacaoCollectionItem;
    property Items[Index: Integer]: TIdeLotacaoCollectionItem read GetItem write SetItem;
  end;

  TInfoDpsFGTS = class(TObject)
  private
    FDpsPerApur: TDpsPerApurCollection;

    function getDpsPerApur(): TDpsPerApurCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function DpsPerApurInst(): Boolean;
    
    property dpsPerApur: TDpsPerApurCollection read getDpsPerApur write FDpsPerApur;
  end;

  TBasePerApurCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TBasePerApurCollectionItem;
    procedure SetItem(Index: Integer; Value: TBasePerApurCollectionItem);
  public
    function Add: TBasePerApurCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TBasePerApurCollectionItem;
    property Items[Index: Integer]: TBasePerApurCollectionItem read GetItem write SetItem;
  end;

  TDpsPerApurCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDpsPerApurCollectionItem;
    procedure SetItem(Index: Integer; Value: TDpsPerApurCollectionItem);
  public
    function Add: TDpsPerApurCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDpsPerApurCollectionItem;
    property Items[Index: Integer]: TDpsPerApurCollectionItem read GetItem write SetItem;
  end;

  TDpsPerApurCollectionItem = class(TObject)
  private
    FtpDps: Integer;
    FvrFGTS: Currency;
  public
    property tpDps: Integer read FtpDps write FtpDps;
    property vrFGTS: Currency read FvrFGTS write FvrFGTS;
  end;

implementation

uses
  IniFiles;

{ TS5013 }

constructor TS5013.Create;
begin
  FTipoEvento := teS5013;
  FEvtFGTS := TEvtFGTS.Create;
end;

destructor TS5013.Destroy;
begin
  FEvtFGTS.Free;

  inherited;
end;

function TS5013.GetEvento : TObject;
begin
  Result := self;
end;

function TS5013.GetXml : string;
begin
  Result := FEvtFGTS.XML;
end;

procedure TS5013.SetXml(const Value: string);
begin
  if Value = FEvtFGTS.XML then Exit;

  FEvtFGTS.FXML := Value;
  FEvtFGTS.Leitor.Arquivo := Value;
  FEvtFGTS.LerXML;
end;

function TS5013.GetTipoEvento : TTipoEvento;
begin
  Result := FTipoEvento;
end;

{ TEvtFGTS }

constructor TEvtFGTS.Create;
begin
  inherited Create;
  FLeitor        := TLeitor.Create;
  FIdeEvento     := TIdeEvento5.Create;
  FIdeEmpregador := TIdeEmpregador.Create;
  FInfoFGTS      := TInfoFGTS.Create;
end;

destructor TEvtFGTS.Destroy;
begin
  FLeitor.Free;
  FIdeEvento.Free;
  FIdeEmpregador.Free;
  FInfoFGTS.Free;

  inherited;
end;

function TEvtFGTS.LerXML: Boolean;
var
  ok: Boolean;
  i, j, k, l, m: Integer;
  s: String;
begin
  Result := False;
  try
    FXML := Leitor.Arquivo;

    // Capturar a versão do evento
    s := Copy(FXML, Pos('/evt/evtFGTS/', FXML)+13, 16);
    s := Copy(s, 1, Pos('"', s)-1);
    Self.VersaoDF := StrToVersaoeSocialSchemas(s);

    if leitor.rExtrai(1, 'evtFGTS') <> '' then
    begin
      FId := Leitor.rAtributo('Id=');

      if leitor.rExtrai(2, 'ideEvento') <> '' then
      begin
        if VersaoDF >= veS01_00_00 then
          IdeEvento.indApuracao := eSStrToIndApuracao(ok, leitor.rCampo(tcStr, 'indApuracao'));
        IdeEvento.perApur := leitor.rCampo(tcStr, 'perApur');
      end;

      if leitor.rExtrai(2, 'ideEmpregador') <> '' then
      begin
        IdeEmpregador.TpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
        IdeEmpregador.NrInsc := leitor.rCampo(tcStr, 'nrInsc');
      end;

      if leitor.rExtrai(2, 'infoFGTS') <> '' then
      begin
        InfoFGTS.FnrRecArqBase := leitor.rCampo(tcStr, 'nrRecArqBase');
        InfoFGTS.FindExistInfo := leitor.rCampo(tcInt, 'indExistInfo');

        if VersaoDF <= ve02_05_00 then
        begin
          if leitor.rExtrai(3, 'infoBaseFGTS') <> '' then
          begin
             i := 0;
             while Leitor.rExtrai(4, 'basePerApur', '', i + 1) <> '' do
             begin
               InfoFGTS.infoBaseFGTS.basePerApur.New;
               InfoFGTS.infoBaseFGTS.basePerApur.Items[i].tpValor  := leitor.rCampo(tcInt, 'tpValor');
               InfoFGTS.infoBaseFGTS.basePerApur.Items[i].baseFGTS := leitor.rCampo(tcDe2, 'baseFGTS');
               inc(i);
             end;
          end;
         
          if leitor.rExtrai(3, 'infoDpsFGTS') <> '' then
          begin
             i := 0;
             while Leitor.rExtrai(4, 'dpsPerApur', '', i + 1) <> '' do
             begin
               InfoFGTS.infoDpsFGTS.dpsPerApur.New;
               InfoFGTS.infoDpsFGTS.dpsPerApur.Items[i].tpDps  := leitor.rCampo(tcInt, 'tpDps');
               InfoFGTS.infoDpsFGTS.dpsPerApur.Items[i].vrFGTS := leitor.rCampo(tcDe2, 'vrFGTS');
               inc(i);
             end;
          end;
        end
        else
        begin
          i := 0;
          while Leitor.rExtrai(3, 'ideEstab', '', i + 1) <> '' do
          begin
            InfoFGTS.IdeEstab.New;
            InfoFGTS.IdeEstab.Items[i].TpInsc := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
            InfoFGTS.IdeEstab.Items[i].NrInsc := leitor.rCampo(tcStr, 'nrInsc');

            j := 0;
            while Leitor.rExtrai(4, 'ideLotacao', '', j + 1) <> '' do
            begin
              InfoFGTS.IdeEstab.Items[i].IdeLotacao.New;
              InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].codLotacao := leitor.rCampo(tcStr, 'codLotacao');
              InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].tpLotacao  := leitor.rCampo(tcStr, 'tpLotacao');

              if (StrToInt(InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].tpLotacao) in [2, 3, 4, 5, 6, 7, 8, 9]) then
              begin
                InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].TpInsc     := eSStrToTpInscricao(ok, leitor.rCampo(tcStr, 'tpInsc'));
                InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].NrInsc     := leitor.rCampo(tcStr, 'nrInsc');
              end;

              if Leitor.rExtrai(5, 'infoBaseFGTS') <> '' then
              begin
                k := 0;
                while Leitor.rExtrai(6, 'basePerApur', '', k + 1) <> '' do
                begin
                  InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoBaseFGTS.BasePerApur.New;
                  InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoBaseFGTS.BasePerApur.Items[k].tpValor  := leitor.rCampo(tcInt, 'tpValor');
                  InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoBaseFGTS.BasePerApur.Items[k].indIncid := leitor.rCampo(tcInt, 'indIncid');
                  InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].infoBaseFGTS.basePerApur.Items[k].baseFGTS := leitor.rCampo(tcDe2, 'baseFGTS');
                  InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].infoBaseFGTS.basePerApur.Items[k].vrFGTS   := leitor.rCampo(tcDe2, 'vrFGTS');
                
                  inc(k);
                end;

                if leitor.rExtrai(6, 'infoBasePerAntE') <> '' then
                begin
                  l := 0;
                  InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoBaseFGTS.InfoBasePerAntE.New;
                  InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoBaseFGTS.InfoBasePerAntE.Items[l].perRef   := leitor.rCampo(tcStr, 'perRef');
                  InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoBaseFGTS.InfoBasePerAntE.Items[l].tpAcConv := leitor.rCampo(tcStr, 'tpAcConv');

                  m := 0;
                  while Leitor.rExtrai(7, 'basePerAntE', '', m + 1) <> '' do
                  begin
                    InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoBaseFGTS.InfoBasePerAntE.Items[l].BasePerAntE.New;
                    InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoBaseFGTS.InfoBasePerAntE.Items[l].BasePerAntE.Items[m].tpValorE  := leitor.rCampo(tcInt, 'tpValorE');
                    InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].InfoBaseFGTS.InfoBasePerAntE.Items[l].BasePerAntE.Items[m].indIncidE := leitor.rCampo(tcInt, 'indIncidE');
                    InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].infoBaseFGTS.InfoBasePerAntE.Items[l].basePerAntE.Items[m].baseFGTSE := leitor.rCampo(tcDe2, 'baseFGTSE');
                    InfoFGTS.IdeEstab.Items[i].IdeLotacao.Items[j].infoBaseFGTS.InfoBasePerAntE.Items[l].basePerAntE.Items[m].vrFGTSE   := leitor.rCampo(tcDe2, 'vrFGTSE');
                                                                                               
                    inc(m);
                  end;
                end;  
              end;

              inc(j);
            end;

            inc(i);
          end;
        end;
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

function TEvtFGTS.SalvarINI: boolean;
//var
//  AIni: TMemIniFile;
//  sSecao: String;
//  i: Integer;
begin
  Result := True;

//  AIni := TMemIniFile.Create('');
//  try
//    Result := True;
//
//  finally
//    AIni.Free;
//  end;
end;

{ TInfoFGTS }

constructor TInfoFGTS.Create;
begin
  inherited Create;
  
  FInfoBaseFGTS := nil;
  FInfoDpsFGTS := nil;
  FIdeEstab := nil;
end;

destructor TInfoFGTS.Destroy;
begin
  if InfoBaseFGTSInst() then
    FInfoBaseFGTS.Free;
    
  if InfoDpsFGTSInst() then
    FInfoDpsFGTS.Free;

  if IdeEstabInst() then
    FIdeEstab.Free;

  inherited;
end;

function TInfoFGTS.InfoBaseFGTSInst(): Boolean;
begin
  Result := Assigned(FInfoBaseFGTS);
end;

function TInfoFGTS.getInfoBaseFGTS(): TInfoBaseFGTS;
begin
  if not(InfoBaseFGTSInst()) then
    FInfoBaseFGTS := TInfoBaseFGTS.Create;
  Result := FInfoBaseFGTS;
end;

function TInfoFGTS.InfoDpsFGTSInst(): Boolean;
begin
  Result := Assigned(FInfoDpsFGTS);
end;

function TInfoFGTS.getInfoDpsFGTS(): TInfoDpsFGTS;
begin
  if not(InfoDpsFGTSInst()) then
    FInfoDpsFGTS := TInfoDpsFGTS.Create;
  Result := FInfoDpsFGTS;
end;

function TInfoFGTS.IdeEstabInst(): Boolean;
begin
  Result := Assigned(FIdeEstab);
end;

function TInfoFGTS.getIdeEstab(): TIdeEstabCollection;
begin
  if not(IdeEstabInst()) then
    FIdeEstab := TIdeEstabCollection.Create;
  Result := FIdeEstab;
end;

{ TInfoBaseFGTS }

constructor TInfoBaseFGTS.Create;
begin
  inherited Create;
  
  FBasePerApur := nil;
  FInfoBasePerAntE := nil;
end;

destructor TInfoBaseFGTS.Destroy;
begin
  if BasePerApurInst() then
    FBasePerApur.Free;
  
  if InfoBasePerAntEInst() then
    FInfoBasePerAntE.Free;
    
  inherited;
end;

function TInfoBaseFGTS.getBasePerApur(): TBasePerApurCollection;
begin
  if not(BasePerApurInst()) then
    FBasePerApur := TBasePerApurCollection.Create;
  Result := FBasePerApur;
end;

function TInfoBaseFGTS.BasePerApurInst(): Boolean;
begin
  Result := Assigned(FBasePerApur);
end;

function TInfoBaseFGTS.getInfoBasePerAntE(): TInfoBasePerAntECollection;
begin
  if not(InfoBasePerAntEInst()) then
    FInfoBasePerAntE := TInfoBasePerAntECollection.Create;
  Result := FInfoBasePerAntE;
end;

function TInfoBaseFGTS.InfoBasePerAntEInst(): Boolean;
begin
  Result := Assigned(FInfoBasePerAntE);
end;

{ TBasePerApurCollection }

function TBasePerApurCollection.Add: TBasePerApurCollectionItem;
begin
  Result := Self.New;
end;

function TBasePerApurCollection.GetItem(Index: Integer): TBasePerApurCollectionItem;
begin
  Result := TBasePerApurCollectionItem(inherited Items[Index]);
end;

procedure TBasePerApurCollection.SetItem(Index: Integer; Value: TBasePerApurCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TBasePerApurCollection.New: TBasePerApurCollectionItem;
begin
  Result := TBasePerApurCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoBasePerAntECollection }

function TInfoBasePerAntECollection.Add: TInfoBasePerAntECollectionItem;
begin
  Result := Self.New;
end;

function TInfoBasePerAntECollection.GetItem(Index: Integer): TInfoBasePerAntECollectionItem;
begin
  Result := TInfoBasePerAntECollectionItem(inherited Items[Index]);
end;

procedure TInfoBasePerAntECollection.SetItem(Index: Integer; Value: TInfoBasePerAntECollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoBasePerAntECollection.New: TInfoBasePerAntECollectionItem;
begin
  Result := TInfoBasePerAntECollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoBasePerAntECollectionItem }

constructor TInfoBasePerAntECollectionItem.Create;
begin
  inherited Create;

  FBasePerAntE := nil;
end;

destructor TInfoBasePerAntECollectionItem.Destroy;
begin
  if BasePerAntEInst() then
    FBasePerAntE.Free;
  
  inherited;
end;

function TInfoBasePerAntECollectionItem.getBasePerAntE(): TBasePerAntECollection;
begin
  if not(BasePerAntEInst()) then
    FBasePerAntE := TBasePerAntECollection.Create;
  Result := FBasePerAntE;
end;

function TInfoBasePerAntECollectionItem.BasePerAntEInst(): Boolean;
begin
  Result := Assigned(FBasePerAntE);
end;

{ TDpsPerApurCollection }

function TDpsPerApurCollection.Add: TDpsPerApurCollectionItem;
begin
  Result := Self.New;
end;

function TDpsPerApurCollection.GetItem(Index: Integer): TDpsPerApurCollectionItem;
begin
  Result := TDpsPerApurCollectionItem(inherited Items[Index]);
end;

procedure TDpsPerApurCollection.SetItem(Index: Integer; Value: TDpsPerApurCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDpsPerApurCollection.New: TDpsPerApurCollectionItem;
begin
  Result := TDpsPerApurCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfoDpsFGTS }

constructor TInfoDpsFGTS.Create;
begin
  inherited Create;
  
  FDpsPerApur := nil;
end;

destructor TInfoDpsFGTS.Destroy;
begin
  if DpsPerApurInst() then
    FDpsPerApur.Free;

  inherited;
end;

function TInfoDpsFGTS.getDpsPerApur(): TDpsPerApurCollection;
begin
  if not(DpsPerApurInst()) then
    FDpsPerApur := TDpsPerApurCollection.Create;
  Result := FDpsPerApur;
end;

function TInfoDpsFGTS.DpsPerApurInst(): Boolean;
begin
  Result := Assigned(FDpsPerApur);
end;

{ TIdeEstabCollection }

function TIdeEstabCollection.Add: TIdeEstabCollectionItem;
begin
  Result := Self.New;
end;

function TIdeEstabCollection.GetItem(
  Index: Integer): TIdeEstabCollectionItem;
begin
  Result := TIdeEstabCollectionItem(inherited Items[Index]);
end;

procedure TIdeEstabCollection.SetItem(Index: Integer;
  Value: TIdeEstabCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeEstabCollection.New: TIdeEstabCollectionItem;
begin
  Result := TIdeEstabCollectionItem.Create;
  Self.Add(Result);
end;

{ TIdeEstabCollectionItem }

constructor TIdeEstabCollectionItem.Create;
begin
  inherited Create;

  FIdeLotacao := nil;
end;

destructor TIdeEstabCollectionItem.Destroy;
begin
  if IdeLotacaoInst() then
    FIdeLotacao.Free;

  inherited;
end;

function TIdeEstabCollectionItem.getIdeLotacao: TIdeLotacaoCollection;
begin
  if not(IdeLotacaoInst()) then
    FIdeLotacao := TIdeLotacaoCollection.Create;
  Result := FIdeLotacao;
end;

function TIdeEstabCollectionItem.IdeLotacaoInst(): Boolean;
begin
  Result := Assigned(FIdeLotacao);
end;

{ TIdeLotacaoCollection }

function TIdeLotacaoCollection.Add: TIdeLotacaoCollectionItem;
begin
  Result := Self.New;
end;

function TIdeLotacaoCollection.GetItem(
  Index: Integer): TIdeLotacaoCollectionItem;
begin
  Result := TIdeLotacaoCollectionItem(inherited Items[Index]);
end;

procedure TIdeLotacaoCollection.SetItem(Index: Integer;
  Value: TIdeLotacaoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TIdeLotacaoCollection.New: TIdeLotacaoCollectionItem;
begin
  Result := TIdeLotacaoCollectionItem.Create;
  Self.Add(Result);
end;

{ TIdeLotacaoCollectionItem }

constructor TIdeLotacaoCollectionItem.Create;
begin
  inherited Create;

  FInfoBaseFGTS := nil;
end;

destructor TIdeLotacaoCollectionItem.Destroy;
begin
  if InfoBaseFGTSInst() then
    FInfoBaseFGTS.Free;

  inherited;
end;

function TIdeLotacaoCollectionItem.InfoBaseFGTSInst(): Boolean;
begin
  Result := Assigned(FInfoBaseFGTS);
end;

function TIdeLotacaoCollectionItem.getInfoBaseFGTS(): TInfoBaseFGTS;
begin
  if not(InfoBaseFGTSInst()) then
    FInfoBaseFGTS := TInfoBaseFGTS.Create;
  Result := FInfoBaseFGTS;
end;

{ TBasePerAntECollection }

function TBasePerAntECollection.Add: TBasePerAntECollectionItem;
begin
  Result := Self.New;
end;

function TBasePerAntECollection.GetItem(
  Index: Integer): TBasePerAntECollectionItem;
begin
  Result := TBasePerAntECollectionItem(inherited Items[Index]);
end;

procedure TBasePerAntECollection.SetItem(Index: Integer;
  Value: TBasePerAntECollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TBasePerAntECollection.New: TBasePerAntECollectionItem;
begin
  Result := TBasePerAntECollectionItem.Create;
  Self.Add(Result);
end;
 
end.
