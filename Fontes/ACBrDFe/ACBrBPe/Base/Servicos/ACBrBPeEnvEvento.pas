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

unit ACBrBPeEnvEvento;

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
  ACBrBPeClass,
  ACBrBPeEventoClass,
  ACBrBPeConsts,
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

  { TEventoBPe }

  TEventoBPe = class(TACBrXmlWriter)
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
    function Gerar_Evento_NaoEmbarque(AIdx: Integer): TACBrXmlNode;
    function Gerar_Evento_AlteracaoPoltrona(AIdx: Integer): TACBrXmlNode;
    function Gerar_Evento_ExcessoBagagem(AIdx: Integer): TACBrXmlNode;

    // Reforma Tributaria
    function Gerar_IBSCBS(IBSCBS: TIBSCBS): TACBrXmlNode;
    function Gerar_IBSCBS_gIBSCBS(gIBSCBS: TgIBSCBS): TACBrXmlNode;

    function Gerar_IBSCBS_gIBSCBS_gIBSUF(gIBSUF: TgIBSUFValores): TACBrXmlNode;
    function Gerar_IBSCBS_gIBSCBS_gIBSMun(gIBSMun: TgIBSMunValores): TACBrXmlNode;
    function Gerar_IBSCBS_gIBSCBS_gCBS(gCBS: TgCBSValores): TACBrXmlNode;

    function Gerar_IBSCBS_gIBSCBS_gIBSUF_gDif(Dif: TgDif): TACBrXmlNode;
    function Gerar_IBSCBS_gIBSCBS_gIBSMun_gDif(Dif: TgDif): TACBrXmlNode;
    function Gerar_IBSCBS_gIBSCBS_gCBS_gDif(Dif: TgDif): TACBrXmlNode;

    function Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gDevTrib(DevTrib: TgDevTrib): TACBrXmlNode;
    function Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gRed(Red: TgRed): TACBrXmlNode;

    function Gerar_IBSCBS_gIBSCBS_gTribRegular(gTribRegular: TgTribRegular): TACBrXmlNode;
    function Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(gIBSCredPres: TgIBSCBSCredPres;
      const Grupo: string): TACBrXmlNode;
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
  ACBrBPeRetEnvEvento,
  ACBrDFe.Conversao,
  ACBrBPeConversao;

{ TEventoBPe }

constructor TEventoBPe.Create;
begin
  inherited Create;

  FEvento := TInfEventoCollection.Create();
end;

destructor TEventoBPe.Destroy;
begin
  FEvento.Free;

  inherited;
end;

function TEventoBPe.CreateOptions: TACBrXmlWriterOptions;
begin
  Result := TACBrXmlWriterOptions.Create();
end;

function TEventoBPe.ObterNomeArquivo(tpEvento: TpcnTpEvento): string;
begin
  case tpEvento of
    teCancelamento: Result := IntToStr(Self.idLote) + '-can-eve.xml';
  else
    raise EventoException.Create('Obter nome do arquivo de Evento não Implementado!');
  end;
end;

function TEventoBPe.GerarXml: Boolean;
var
  EventoNode: TACBrXmlNode;
begin
  ListaDeAlertas.Clear;

  FDocument.Clear();
  EventoNode := CreateElement('eventoBPe');
  EventoNode.SetNamespace('http://www.portalfiscal.inf.br/bpe');
  EventoNode.SetAttribute('versao', Versao);

  FDocument.Root := EventoNode;

  EventoNode.AppendChild(Gerar_InfEvento(0));

  if Trim(Evento[0].signature.URI) <> '' then
    EventoNode.AppendChild(GerarSignature(Evento[0].signature));

  XmlEnvio := ChangeLineBreak(Document.Xml, '');
  Result := True;
end;

function TEventoBPe.Gerar_InfEvento(AIdx: Integer): TACBrXmlNode;
var
  sDoc: string;
begin
  Evento[AIdx].InfEvento.id := 'ID'+
                               Evento[AIdx].InfEvento.TipoEvento +
                               OnlyNumber(Evento[AIdx].InfEvento.chBPe) +
                               Format('%.2d', [Evento[AIdx].InfEvento.nSeqEvento]);

  Result := CreateElement('infEvento');
  Result.SetAttribute('Id', Evento[AIdx].InfEvento.id);

  Result.AppendChild(AddNode(tcInt, 'P05', 'cOrgao', 1, 2, 1,
                                               Evento[AIdx].FInfEvento.cOrgao));

  Result.AppendChild(AddNode(tcStr, 'P06', 'tpAmb', 1, 1, 1,
                   TipoAmbienteToStr(Evento[AIdx].InfEvento.tpAmb), DSC_TPAMB));

  sDoc := OnlyNumber(Evento[AIdx].InfEvento.CNPJ);

  if EstaVazio(sDoc) then
    sDoc := ExtrairCNPJCPFChaveAcesso(Evento[AIdx].InfEvento.chBPe);

  Result.AppendChild(AddNode(tcStr, 'HP10', 'CNPJ', 14, 14, 1,
                                                              sDoc , DSC_CNPJ));

  if not ValidarCNPJ(sDoc) then
    wAlerta('HP10', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);

  Result.AppendChild(AddNode(tcStr, 'HP12', 'chBPe', 44, 44, 1,
                                     Evento[AIdx].FInfEvento.chBPe, DSC_CHAVE));

  if not ValidarChave(Evento[AIdx].InfEvento.chBPe) then
    wAlerta('HP12', 'chBPe', '', 'Chave de BP-e inválida');

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

function TEventoBPe.Gerar_DetEvento(AIdx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versaoEvento', Versao);

  case Evento[AIdx].InfEvento.tpEvento of
    teCancelamento:
      Result.AppendChild(Gerar_Evento_Cancelamento(AIdx));

    teNaoEmbarque:
      Result.AppendChild(Gerar_Evento_NaoEmbarque(AIdx));

    teAlteracaoPoltrona:
      Result.AppendChild(Gerar_Evento_AlteracaoPoltrona(AIdx));

    teExcessoBagagem:
      Result.AppendChild(Gerar_Evento_ExcessoBagagem(AIdx));
  end;
end;

function TEventoBPe.Gerar_Evento_Cancelamento(AIdx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evCancBPe');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                           Evento[AIdx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProt', 15, 15, 1,
                                      Evento[AIdx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcStr, 'EP04', 'xJust', 15, 255, 1,
                                      Evento[AIdx].FInfEvento.detEvento.xJust));
end;

function TEventoBPe.Gerar_Evento_NaoEmbarque(AIdx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evNaoEmbBPe');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                           Evento[AIdx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProt', 15, 15, 1,
                                      Evento[AIdx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcStr, 'EP04', 'xJust', 15, 255, 1,
                                      Evento[AIdx].FInfEvento.detEvento.xJust));
end;

function TEventoBPe.Gerar_Evento_AlteracaoPoltrona(AIdx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evAlteracaoPoltrona');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                           Evento[AIdx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProt', 15, 15, 1,
                                      Evento[AIdx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcInt, 'EP04', 'poltrona', 1, 3, 1,
                                   Evento[AIdx].FInfEvento.detEvento.poltrona));
end;

function TEventoBPe.Gerar_Evento_ExcessoBagagem(AIdx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('evExcessoBagagem');

  Result.AppendChild(AddNode(tcStr, 'EP02', 'descEvento', 4, 60, 1,
                                           Evento[AIdx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'EP03', 'nProt', 15, 15, 1,
                                      Evento[AIdx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcInt, 'EP04', 'qBagagem', 1, 20, 1,
                                   Evento[AIdx].FInfEvento.detEvento.qBagagem));

  Result.AppendChild(AddNode(tcDe2, 'EP05', 'vTotBag', 1, 15, 1,
                                    Evento[AIdx].FInfEvento.detEvento.vTotBag));

  // Reforma Tributária
  Result.AppendChild(Gerar_IBSCBS(Evento[AIdx].FInfEvento.detEvento.IBSCBS));
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

function TEventoBPe.GetOpcoes: TACBrXmlWriterOptions;
begin
  Result := TACBrXmlWriterOptions(FOpcoes);
end;

procedure TEventoBPe.SetEvento(const AValue: TInfEventoCollection);
begin
  FEvento.Assign(AValue);
end;

procedure TEventoBPe.SetOpcoes(const AValue: TACBrXmlWriterOptions);
begin
  FOpcoes := AValue;
end;

function TEventoBPe.LerXML(const ACaminhoArquivo: string): Boolean;
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

function TEventoBPe.LerXMLFromString(const AXML: string): Boolean;
var
  RetEventoBPe: TRetEventoBPe;
begin
  RetEventoBPe := TRetEventoBPe.Create;
  try
    RetEventoBPe.XmlRetorno := AXML;
    Result := RetEventoBPe.LerXml;

    with FEvento.New do
    begin
      XML := AXML;

      infEvento.ID := RetEventoBPe.RetInfEvento.id;
      infEvento.cOrgao := RetEventoBPe.RetInfEvento.cOrgao;
      infEvento.tpAmb := RetEventoBPe.RetInfEvento.tpAmb;
//      infEvento.CNPJ := RetEventoBPe.RetInfEvento.CNPJ;
      infEvento.chBPe := RetEventoBPe.RetInfEvento.chBPe;
//      infEvento.dhEvento := RetEventoBPe.RetInfEvento.dhEvento;
      infEvento.tpEvento := RetEventoBPe.RetInfEvento.tpEvento;
      infEvento.nSeqEvento := RetEventoBPe.RetInfEvento.nSeqEvento;

      infEvento.DetEvento.descEvento := RetEventoBPe.RetInfEvento.xEvento;
      infEvento.DetEvento.nProt := RetEventoBPe.RetInfEvento.nProt;
      {
      infEvento.DetEvento.xJust := RetEventoBPe.RetInfEvento.xJust;
      infEvento.DetEvento.poltrona := RetEventoBPe.InfEvento.DetEvento.poltrona;
      infEvento.DetEvento.qBagagem := RetEventoBPe.InfEvento.DetEvento.qBagagem;
      infEvento.DetEvento.vTotBag := RetEventoBPe.InfEvento.DetEvento.vTotBag;
      }
      signature.URI := RetEventoBPe.signature.URI;
      signature.DigestValue := RetEventoBPe.signature.DigestValue;
      signature.SignatureValue := RetEventoBPe.signature.SignatureValue;
      signature.X509Certificate := RetEventoBPe.signature.X509Certificate;

      FRetInfEvento.Id := RetEventoBPe.RetInfEvento.Id;
      FRetInfEvento.tpAmb := RetEventoBPe.RetInfEvento.tpAmb;
      FRetInfEvento.verAplic := RetEventoBPe.RetInfEvento.verAplic;
      FRetInfEvento.cOrgao := RetEventoBPe.RetInfEvento.cOrgao;
      FRetInfEvento.cStat := RetEventoBPe.RetInfEvento.cStat;
      FRetInfEvento.xMotivo := RetEventoBPe.RetInfEvento.xMotivo;
      FRetInfEvento.chBPe := RetEventoBPe.RetInfEvento.chBPe;
      FRetInfEvento.tpEvento := RetEventoBPe.RetInfEvento.tpEvento;
      FRetInfEvento.xEvento := RetEventoBPe.RetInfEvento.xEvento;
      FRetInfEvento.nSeqEvento := RetEventoBPe.RetInfEvento.nSeqEvento;
      FRetInfEvento.cOrgaoAutor := RetEventoBPe.RetInfEvento.cOrgaoAutor;
      FRetInfEvento.CNPJDest := RetEventoBPe.RetInfEvento.CNPJDest;
      FRetInfEvento.emailDest := RetEventoBPe.RetInfEvento.emailDest;
      FRetInfEvento.dhRegEvento := RetEventoBPe.RetInfEvento.dhRegEvento;
      FRetInfEvento.nProt := RetEventoBPe.RetInfEvento.nProt;
      FRetInfEvento.XML := RetEventoBPe.RetInfEvento.XML;
    end;
  finally
    RetEventoBPe.Free;
  end;
end;

function TEventoBPe.LerFromIni(const AIniString: string): Boolean;
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
      sFim := INIRec.ReadString(sSecao,'chBPe', 'FIM');

      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break ;

      with Self.Evento.New do
      begin
        infEvento.chBPe := sFim;
        infEvento.cOrgao := INIRec.ReadInteger(sSecao, 'cOrgao', 0);
        infEvento.CNPJ := INIRec.ReadString(sSecao, 'CNPJ', '');
        infEvento.dhEvento := StringToDateTime(INIRec.ReadString(sSecao, 'dhEvento', ''));
        infEvento.tpEvento := StrToTpEventoBPe(ok, INIRec.ReadString(sSecao, 'tpEvento', ''));
        infEvento.nSeqEvento := INIRec.ReadInteger(sSecao, 'nSeqEvento', 1);
        infEvento.detEvento.xJust := INIRec.ReadString(sSecao, 'xJust', '');
        infEvento.detEvento.nProt := INIRec.ReadString(sSecao, 'nProt', '');
        infEvento.detEvento.poltrona := INIRec.ReadInteger(sSecao, 'poltrona', 0);
        infEvento.detEvento.qBagagem := INIRec.ReadInteger(sSecao, 'qBagagem', 0);
        infEvento.detEvento.vTotBag := StringToFloatDef(INIRec.ReadString(sSecao, 'vTotBag', ''), 0);

        // Reforma Tributária
        // Falta Implementar
      end;

      Inc(I);
    end;

    Result := True;
  finally
    INIRec.Free;
  end;
end;

// Reforma Tributaria
function TEventoBPe.Gerar_IBSCBS(IBSCBS: TIBSCBS): TACBrXmlNode;
begin
  Result := nil;

  if (IBSCBS.gIBSCBS.vBC > 0) then
  begin
    Result := FDocument.CreateElement('IBSCBS');

    Result.AppendChild(AddNode(tcStr, '#1', 'CST', 3, 3, 1,
                                          CSTIBSCBSToStr(IBSCBS.CST), DSC_CST));

    Result.AppendChild(AddNode(tcStr, '#2', 'cClassTrib', 6, 6, 1,
                           cClassTribToStr(IBSCBS.cClassTrib), DSC_CCLASSTRIB));

    if IBSCBS.gIBSCBS.vBC > 0 then
      Result.AppendChild(Gerar_IBSCBS_gIBSCBS(IBSCBS.gIBSCBS));
  end;
end;

function TEventoBPe.Gerar_IBSCBS_gIBSCBS(gIBSCBS: TgIBSCBS): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gIBSCBS');


  Result.AppendChild(AddNode(tcDe2, '#4', 'vBC', 1, 15, 1,
                                                         gIBSCBS.vBC, DSC_VBC));

  Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUF(gIBSCBS.gIBSUF));
  Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSMun(gIBSCBS.gIBSMun));
  Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gCBS(gIBSCBS.gCBS));

  if gIBSCBS.gTribRegular.pAliqEfetRegIBSUF > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gTribRegular(gIBSCBS.gTribRegular));

  if gIBSCBS.gIBSCredPres.pCredPres > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(gIBSCBS.gIBSCredPres, 'gIBSCredPres'));

  if gIBSCBS.gCBSCredPres.pCredPres > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(gIBSCBS.gCBSCredPres, 'gCBSCredPres'));
end;

function TEventoBPe.Gerar_IBSCBS_gIBSCBS_gIBSUF(
  gIBSUF: TgIBSUFValores): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gIBSUF');

  Result.AppendChild(AddNode(tcDe4, '#6', 'pIBSUF', 1, 7, 1,
                                                      gIBSUF.pIBS, DSC_PIBSUF));

  if gIBSUF.gDif.pDif > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUF_gDif(gIBSUF.gDif));

  if gIBSUF.gDevTrib.vDevTrib > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gDevTrib(gIBSUF.gDevTrib));

  if gIBSUF.gRed.pRedAliq > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gRed(gIBSUF.gRed));

  Result.AppendChild(AddNode(tcDe2, '#23', 'vIBSUF', 1, 15, 1,
                                                      gIBSUF.vIBS, DSC_VIBSUF));
end;

function TEventoBPe.Gerar_IBSCBS_gIBSCBS_gIBSUF_gDif(
  Dif: TgDif): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gDif');

  Result.AppendChild(AddNode(tcDe4, '#10', 'pDif', 1, 7, 1,
                                                           Dif.pDif, DSC_PDIF));

  Result.AppendChild(AddNode(tcDe2, '#11', 'vDif', 1, 15, 1,
                                                           Dif.vDif, DSC_VDIF));
end;

function TEventoBPe.Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gDevTrib(
  DevTrib: TgDevTrib): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gDevTrib');

  Result.AppendChild(AddNode(tcDe2, '#13', 'vDevTrib', 1, 15, 1,
                                               DevTrib.vDevTrib, DSC_VDEVTRIB));
end;

function TEventoBPe.Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gRed(
  Red: TgRed): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gRed');

  Result.AppendChild(AddNode(tcDe4, '#15', 'pRedAliq', 1, 7, 1,
                                                   Red.pRedAliq, DSC_PREDALIQ));

  Result.AppendChild(AddNode(tcDe4, '#16', 'pAliqEfet', 1, 7, 1,
                                                 Red.pAliqEfet, DSC_PALIQEFET));
end;

function TEventoBPe.Gerar_IBSCBS_gIBSCBS_gIBSMun(
  gIBSMun: TgIBSMunValores): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gIBSMun');

  Result.AppendChild(AddNode(tcDe4, '#6', 'pIBSMun', 1, 7, 1,
                                                     gIBSMun.pIBS, DSC_PIBSUF));

  if gIBSMun.gDif.pDif > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSMun_gDif(gIBSMun.gDif));

  if gIBSMun.gDevTrib.vDevTrib > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gDevTrib(gIBSMun.gDevTrib));

  if gIBSMun.gRed.pRedAliq > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gRed(gIBSMun.gRed));

  Result.AppendChild(AddNode(tcDe2, '#23', 'vIBSMun', 1, 15, 1,
                                                      gIBSMun.vIBS, DSC_VIBSUF));
end;

function TEventoBPe.Gerar_IBSCBS_gIBSCBS_gIBSMun_gDif(
  Dif: TgDif): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gDif');

  Result.AppendChild(AddNode(tcDe4, '#10', 'pDif', 1, 7, 1,
                                                           Dif.pDif, DSC_PDIF));

  Result.AppendChild(AddNode(tcDe2, '#11', 'vDif', 1, 15, 1,
                                                           Dif.vDif, DSC_VDIF));
end;

function TEventoBPe.Gerar_IBSCBS_gIBSCBS_gCBS(
  gCBS: TgCBSValores): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gCBS');

  Result.AppendChild(AddNode(tcDe4, '#44', 'pCBS', 1, 7, 1,
                                                          gCBS.pCBS, DSC_PCBS));

  if gCBS.gDif.pDif > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gCBS_gDif(gCBS.gDif));

  if gCBS.gDevTrib.vDevTrib > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gDevTrib(gCBS.gDevTrib));

  if gCBS.gRed.pRedAliq > 0 then
    Result.AppendChild(Gerar_IBSCBS_gIBSCBS_gIBSUFMunCBS_gRed(gCBS.gRed));

  Result.AppendChild(AddNode(tcDe2, '#61', 'vCBS', 1, 15, 1,
                                                          gCBS.vCBS, DSC_VCBS));
end;

function TEventoBPe.Gerar_IBSCBS_gIBSCBS_gCBS_gDif(
  Dif: TgDif): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gDif');

  Result.AppendChild(AddNode(tcDe4, '#10', 'pDif', 1, 7, 1,
                                                           Dif.pDif, DSC_PDIF));

  Result.AppendChild(AddNode(tcDe2, '#11', 'vDif', 1, 15, 1,
                                                           Dif.vDif, DSC_VDIF));
end;

function TEventoBPe.Gerar_IBSCBS_gIBSCBS_gTribRegular(
  gTribRegular: TgTribRegular): TACBrXmlNode;
begin
  Result := FDocument.CreateElement('gTribRegular');

  Result.AppendChild(AddNode(tcStr, '#56', 'CSTReg', 3, 3, 1,
                                 CSTIBSCBSToStr(gTribRegular.CSTReg), DSC_CST));

  Result.AppendChild(AddNode(tcStr, '#57', 'cClassTribReg', 6, 6, 1,
                  cClassTribToStr(gTribRegular.cClassTribReg), DSC_CCLASSTRIB));

  Result.AppendChild(AddNode(tcDe4, '#58', 'pAliqEfetRegIBSUF', 1, 7, 1,
                                    gTribRegular.pAliqEfetRegIBSUF, DSC_PALIQ));

  Result.AppendChild(AddNode(tcDe2, '#59', 'vTribRegIBSUF', 1, 15, 1,
                                     gTribRegular.vTribRegIBSUF, DSC_VTRIBREG));

  Result.AppendChild(AddNode(tcDe4, '#60', 'pAliqEfetRegIBSMun', 1, 7, 1,
                                   gTribRegular.pAliqEfetRegIBSMun, DSC_PALIQ));

  Result.AppendChild(AddNode(tcDe2, '#61', 'vTribRegIBSMun', 1, 15, 1,
                                    gTribRegular.vTribRegIBSMun, DSC_VTRIBREG));

  Result.AppendChild(AddNode(tcDe4, '#62', 'pAliqEfetRegCBS', 1, 7, 1,
                                      gTribRegular.pAliqEfetRegCBS, DSC_PALIQ));

  Result.AppendChild(AddNode(tcDe2, '#63', 'vTribRegCBS', 1, 15, 1,
                                       gTribRegular.vTribRegCBS, DSC_VTRIBREG));
end;

function TEventoBPe.Gerar_IBSCBS_gIBSCBS_gIBSCBSCredPres(
  gIBSCredPres: TgIBSCBSCredPres; const Grupo: string): TACBrXmlNode;
begin
  Result := FDocument.CreateElement(Grupo);

  Result.AppendChild(AddNode(tcStr, '#63', 'cCredPres', 2, 2, 1,
                        cCredPresToStr(gIBSCredPres.cCredPres), DSC_CCREDPRES));

  Result.AppendChild(AddNode(tcDe4, '#64', 'pCredPres', 1, 7, 1,
                                        gIBSCredPres.pCredPres, DSC_PCREDPRES));

  if gIBSCredPres.vCredPres > 0 then
    Result.AppendChild(AddNode(tcDe2, '#65', 'vCredPres', 1, 15, 1,
                                         gIBSCredPres.vCredPres, DSC_VCREDPRES))
  else
    Result.AppendChild(AddNode(tcDe2, '#66', 'vCredPresCondSus', 1, 15, 1,
                          gIBSCredPres.vCredPresCondSus, DSC_VCREDPRESCONDSUS));
end;

end.
