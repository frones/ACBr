{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrNFComEnvEvento;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrDFeConsts,
  ACBrBase,
  pcnConversao,
  pcnSignature,
  ACBrNFComEventoClass,
  ACBrNFComConsts,
  ACBrXmlBase,
  ACBrXmlWriter,
  ACBrXmlDocument;

type
  EventoException = class(Exception);

  TInfEventoCollectionItem = class(TObject)
  private
    FInfEvento: TInfEvento;
    Fsignature: Tsignature;
    FRetInfEvento: TRetInfEvento;
    FXML: string;
  public
    constructor Create;
    destructor Destroy; override;

    property InfEvento: TInfEvento read FInfEvento write FInfEvento;
    property signature: Tsignature read Fsignature write Fsignature;
    property RetInfEvento: TRetInfEvento read FRetInfEvento write FRetInfEvento;
    property XML: string read FXML write FXML;
  end;

  TInfEventoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfEventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfEventoCollectionItem);
  public
    function Add: TInfEventoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New.'{$EndIf};
    function New: TInfEventoCollectionItem;
    property Items[Index: Integer]: TInfEventoCollectionItem read GetItem write SetItem; default;
  end;

  { TEventoNFCom }

  TEventoNFCom = class(TACBrXmlWriter)
  private
    FidLote: Int64;
    FEvento: TInfEventoCollection;
    FVersao: string;
    FXmlEnvio: string;

    procedure SetEvento(const AValue: TInfEventoCollection);

    function GetOpcoes: TACBrXmlWriterOptions;
    procedure SetOpcoes(const AValue: TACBrXmlWriterOptions);
  protected
    function CreateOptions: TACBrXmlWriterOptions; override;

    function Gerar_InfEvento(AIdx: Integer): TACBrXmlNode;
    function Gerar_DetEvento(AIdx: Integer): TACBrXmlNode;
    function Gerar_Evento_Cancelamento(AIdx: Integer): TACBrXmlNode;
  public
    constructor Create;
    destructor Destroy; override;

    function GerarXml: Boolean; override;

    function LerXML(const ACaminhoArquivo: string): Boolean;
    function LerXMLFromString(const AXML: string): Boolean;
    function ObterNomeArquivo(tpEvento: TpcnTpEvento): string;
    function LerFromIni(const AIniString: string): Boolean;

    property idLote: Int64 read FidLote write FidLote;
    property Evento: TInfEventoCollection read FEvento write SetEvento;
    property Versao: string read FVersao write FVersao;
    property Opcoes: TACBrXmlWriterOptions read GetOpcoes write SetOpcoes;
    property XmlEnvio: string read FXmlEnvio write FXmlEnvio;
  end;

implementation

uses
  IniFiles,
  ACBrDFeUtil,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrNFComRetEnvEvento,
  ACBrNFComConversao;

{ TEventoNFCom }

constructor TEventoNFCom.Create;
begin
  inherited Create;

  FEvento := TInfEventoCollection.Create();
end;

destructor TEventoNFCom.Destroy;
begin
  FEvento.Free;

  inherited;
end;

function TEventoNFCom.CreateOptions: TACBrXmlWriterOptions;
begin
  Result := TACBrXmlWriterOptions.Create();
end;

function TEventoNFCom.ObterNomeArquivo(tpEvento: TpcnTpEvento): string;
begin
  case tpEvento of
    teCancelamento: Result := IntToStr(Self.idLote) + '-can-eve.xml';
  else
    raise EventoException.Create('Obter nome do arquivo de Evento não Implementado!');
  end;
end;

function TEventoNFCom.GerarXml: Boolean;
var
  EventoNode: TACBrXmlNode;
begin
  ListaDeAlertas.Clear;

  FDocument.Clear();
  EventoNode := CreateElement('eventoNFCom');
  EventoNode.SetNamespace('http://www.portalfiscal.inf.br/nfcom');
  EventoNode.SetAttribute('versao', Versao);

  FDocument.Root := EventoNode;

  EventoNode.AppendChild(Gerar_InfEvento(0));

  if Trim(Evento[0].signature.URI) <> '' then
    EventoNode.AppendChild(GerarSignature(Evento[0].signature));

  XmlEnvio := ChangeLineBreak(Document.Xml, '');
  Result := True;
end;

function TEventoNFCom.Gerar_InfEvento(AIdx: Integer): TACBrXmlNode;
var
  sDoc: string;
begin
  Evento[AIdx].InfEvento.id := 'ID'+
                               Evento[AIdx].InfEvento.TipoEvento +
                               OnlyNumber(Evento[AIdx].InfEvento.chNFCom) +
                               Format('%.3d', [Evento[AIdx].InfEvento.nSeqEvento]);

  Result := CreateElement('infEvento');
  Result.SetAttribute('Id', Evento[AIdx].InfEvento.id);

  Result.AppendChild(AddNode(tcInt, 'P05', 'cOrgao', 1, 2, 1,
                                               Evento[AIdx].FInfEvento.cOrgao));

  Result.AppendChild(AddNode(tcStr, 'P06', 'tpAmb', 1, 1, 1,
                   TipoAmbienteToStr(Evento[AIdx].InfEvento.tpAmb), DSC_TPAMB));

  sDoc := OnlyNumber(Evento[AIdx].InfEvento.CNPJ);

  if EstaVazio(sDoc) then
    sDoc := ExtrairCNPJCPFChaveAcesso(Evento[AIdx].InfEvento.chNFCom);

  Result.AppendChild(AddNode(tcStr, 'HP10', 'CNPJ', 14, 14, 1,
                                                              sDoc , DSC_CNPJ));

  if not ValidarCNPJ(sDoc) then
    wAlerta('HP10', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, 'HP12', 'chNFCom', 44, 44, 1,
                                    Evento[AIdx].FInfEvento.chNFCom, DSC_CHAVE));

  if not ValidarChave(Evento[AIdx].InfEvento.chNFCom) then
    wAlerta('HP12', 'chNFCom', '', 'Chave de NFCom inválida');

  Result.AppendChild(AddNode(tcStr, 'HP13', 'dhEvento', 1, 50, 1,
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Evento[AIdx].InfEvento.dhEvento) +
    GetUTC(CodigoUFparaUF(Evento[AIdx].InfEvento.cOrgao),
    Evento[AIdx].InfEvento.dhEvento)));

  Result.AppendChild(AddNode(tcInt, 'HP14', 'tpEvento', 6, 6, 1,
                                           Evento[AIdx].FInfEvento.TipoEvento));

  Result.AppendChild(AddNode(tcInt, 'HP15', 'nSeqEvento', 1, 3, 1,
                                           Evento[AIdx].FInfEvento.nSeqEvento));

  Result.AppendChild(Gerar_DetEvento(AIdx));
end;

function TEventoNFCom.Gerar_DetEvento(AIdx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versaoEvento', Versao);

  case Evento[AIdx].InfEvento.tpEvento of
    teCancelamento:
      Result.AppendChild(Gerar_Evento_Cancelamento(AIdx));
  end;
end;

function TEventoNFCom.Gerar_Evento_Cancelamento(AIdx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evCancNFCom');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                           Evento[AIdx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProt', 15, 15, 1,
                                      Evento[AIdx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcStr, 'EP04', 'xJust', 15, 255, 1,
                                      Evento[AIdx].FInfEvento.detEvento.xJust));
end;

{ TInfEventoCollectionItem }

constructor TInfEventoCollectionItem.Create;
begin
  inherited Create;

  FInfEvento := TInfEvento.Create;
  Fsignature := TSignature.Create;
  FRetInfEvento := TRetInfEvento.Create;
end;

destructor TInfEventoCollectionItem.Destroy;
begin
  FInfEvento.Free;
  fsignature.Free;
  FRetInfEvento.Free;

  inherited;
end;

{ TInfEventoCollection }

function TInfEventoCollection.Add: TInfEventoCollectionItem;
begin
  Result := Self.New;
end;

function TInfEventoCollection.GetItem(
  Index: Integer): TInfEventoCollectionItem;
begin
  Result := TInfEventoCollectionItem(inherited Items[Index]);
end;

procedure TInfEventoCollection.SetItem(Index: Integer;
  Value: TInfEventoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfEventoCollection.New: TInfEventoCollectionItem;
begin
  Result := TInfEventoCollectionItem.Create;
  Self.Add(Result);
end;

function TEventoNFCom.GetOpcoes: TACBrXmlWriterOptions;
begin
  Result := TACBrXmlWriterOptions(FOpcoes);
end;

procedure TEventoNFCom.SetEvento(const AValue: TInfEventoCollection);
begin
  FEvento.Assign(AValue);
end;

procedure TEventoNFCom.SetOpcoes(const AValue: TACBrXmlWriterOptions);
begin
  FOpcoes := AValue;
end;

function TEventoNFCom.LerXML(const ACaminhoArquivo: string): Boolean;
var
  ArqEvento: TStringList;
begin
  ArqEvento := TStringList.Create;
  try
    ArqEvento.LoadFromFile(ACaminhoArquivo);
    Result := LerXMLFromString(ArqEvento.Text);
  finally
    ArqEvento.Free;
  end;
end;

function TEventoNFCom.LerXMLFromString(const AXML: string): Boolean;
var
  RetEventoNFCom: TRetEventoNFCom;
begin
  RetEventoNFCom := TRetEventoNFCom.Create;
  try
    RetEventoNFCom.XmlRetorno := AXML;
    Result := RetEventoNFCom.LerXml;

    with FEvento.New do
    begin
      XML := AXML;

      infEvento.ID := RetEventoNFCom.InfEvento.id;
      infEvento.cOrgao := RetEventoNFCom.InfEvento.cOrgao;
      infEvento.tpAmb := RetEventoNFCom.InfEvento.tpAmb;
      infEvento.CNPJ := RetEventoNFCom.InfEvento.CNPJ;
      infEvento.chNFCom := RetEventoNFCom.InfEvento.chNFCom;
      infEvento.dhEvento := RetEventoNFCom.InfEvento.dhEvento;
      infEvento.tpEvento := RetEventoNFCom.InfEvento.tpEvento;
      infEvento.nSeqEvento := RetEventoNFCom.InfEvento.nSeqEvento;

      infEvento.DetEvento.descEvento := RetEventoNFCom.InfEvento.DetEvento.descEvento;
      infEvento.DetEvento.nProt := RetEventoNFCom.InfEvento.DetEvento.nProt;
      infEvento.DetEvento.xJust := RetEventoNFCom.InfEvento.DetEvento.xJust;

      signature.URI := RetEventoNFCom.signature.URI;
      signature.DigestValue := RetEventoNFCom.signature.DigestValue;
      signature.SignatureValue := RetEventoNFCom.signature.SignatureValue;
      signature.X509Certificate := RetEventoNFCom.signature.X509Certificate;

      FRetInfEvento.Id := RetEventoNFCom.RetInfEvento.Id;
      FRetInfEvento.tpAmb := RetEventoNFCom.RetInfEvento.tpAmb;
      FRetInfEvento.verAplic := RetEventoNFCom.RetInfEvento.verAplic;
      FRetInfEvento.cOrgao := RetEventoNFCom.RetInfEvento.cOrgao;
      FRetInfEvento.cStat := RetEventoNFCom.RetInfEvento.cStat;
      FRetInfEvento.xMotivo := RetEventoNFCom.RetInfEvento.xMotivo;
      FRetInfEvento.chNFCom := RetEventoNFCom.RetInfEvento.chNFCom;
      FRetInfEvento.tpEvento := RetEventoNFCom.RetInfEvento.tpEvento;
      FRetInfEvento.xEvento := RetEventoNFCom.RetInfEvento.xEvento;
      FRetInfEvento.nSeqEvento := RetEventoNFCom.RetInfEvento.nSeqEvento;
      FRetInfEvento.cOrgaoAutor := RetEventoNFCom.RetInfEvento.cOrgaoAutor;
      FRetInfEvento.CNPJDest := RetEventoNFCom.RetInfEvento.CNPJDest;
      FRetInfEvento.emailDest := RetEventoNFCom.RetInfEvento.emailDest;
      FRetInfEvento.dhRegEvento := RetEventoNFCom.RetInfEvento.dhRegEvento;
      FRetInfEvento.nProt := RetEventoNFCom.RetInfEvento.nProt;
      FRetInfEvento.XML := RetEventoNFCom.RetInfEvento.XML;
    end;
  finally
    RetEventoNFCom.Free;
  end;
end;

function TEventoNFCom.LerFromIni(const AIniString: string): Boolean;
var
  I: Integer;
  sSecao, sFim: string;
  INIRec: TMemIniFile;
  ok: Boolean;
begin
{$IFNDEF COMPILER23_UP}
  Result := False;
{$ENDIF}
  Self.Evento.Clear;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);
    idLote := INIRec.ReadInteger('EVENTO', 'idLote', 0);

    I := 1 ;
    while true do
    begin
      sSecao := 'EVENTO' + IntToStrZero(I, 3) ;
      sFim := INIRec.ReadString(sSecao,'chNFCom', 'FIM');

      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break ;

      with Self.Evento.New do
      begin
        infEvento.chNFCom := sFim;
        infEvento.cOrgao := INIRec.ReadInteger(sSecao, 'cOrgao', 0);
        infEvento.CNPJ := INIRec.ReadString(sSecao, 'CNPJ', '');
        infEvento.dhEvento := StringToDateTime(INIRec.ReadString(sSecao, 'dhEvento', ''));
        infEvento.tpEvento := StrToTpEventoNFCom(ok, INIRec.ReadString(sSecao, 'tpEvento', ''));
        infEvento.nSeqEvento := INIRec.ReadInteger(sSecao, 'nSeqEvento', 1);
        infEvento.detEvento.xJust := INIRec.ReadString(sSecao, 'xJust', '');
        infEvento.detEvento.nProt := INIRec.ReadString(sSecao, 'nProt', '');
      end;

      Inc(I);
    end;

    Result := True;
  finally
    INIRec.Free;
  end;
end;

end.
