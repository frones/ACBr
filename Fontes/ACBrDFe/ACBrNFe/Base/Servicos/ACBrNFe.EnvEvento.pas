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

unit ACBrNFe.EnvEvento;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrDFeConsts,
  pcnConversao,
  pcnNFeConsts,
  pcnSignature,
  ACBrNFe.EventoClass,
  ACBrBase,
  ACBrXmlBase,
  ACBrXmlWriter,
  ACBrXmlDocument;

type

  TNFeXmlWriterOptions = class(TACBrXmlWriterOptions)
  private
    FAjustarTagNro: boolean;
    FNormatizarMunicipios: boolean;
    FGerarTagAssinatura: TpcnTagAssinatura;
    FPathArquivoMunicipios: string;
    FValidarInscricoes: boolean;

  public
    property AjustarTagNro: boolean read FAjustarTagNro write FAjustarTagNro;
    property NormatizarMunicipios: boolean read FNormatizarMunicipios write FNormatizarMunicipios;
    property GerarTagAssinatura: TpcnTagAssinatura read FGerarTagAssinatura write FGerarTagAssinatura;
    property PathArquivoMunicipios: string read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: boolean read FValidarInscricoes write FValidarInscricoes;
  end;

  TInfEventoCollectionItem = class(TObject)
  private
    FInfEvento: TInfEvento;
    Fsignature: Tsignature;
    FRetInfEvento: TRetInfEvento;
    FXML: string;
  public
    constructor Create;
    destructor Destroy; override;

    property InfEvento: TInfEvento       read FInfEvento    write FInfEvento;
    property signature: Tsignature       read Fsignature    write Fsignature;
    property RetInfEvento: TRetInfEvento read FRetInfEvento write FRetInfEvento;
    property XML: string                 read FXML          write FXML;
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

  { TEventoNFe }

  TEventoNFe = class(TACBrXmlWriter)
  private
    FidLote: Int64;
    FEvento: TInfEventoCollection;
    FVersao: string;
    FXmlEnvio: string;

    procedure SetEvento(const Value: TInfEventoCollection);

    function GetOpcoes: TNFeXmlWriterOptions;
    procedure SetOpcoes(const Value: TNFeXmlWriterOptions);
  protected
    function CreateOptions: TACBrXmlWriterOptions; override;

    function Gerar_InfEvento(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento: TACBrXmlNodeArray;
    function Gerar_Evento_CCe(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_Cancelamento(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_CancSubstituicao(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_ManifDestCiencia(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_ManifDestConfirmacao(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_ManifDestDesconhecimento(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_ManifDestOperNaoRealizada(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_EPEC(Idx: Integer): TACBrXmlNode;
    function Gerar_DestNFe(const EventoItem: TInfEventoCollectionItem): TACBrXmlNode;
    function Gerar_DestNFCe(const EventoItem: TInfEventoCollectionItem): TACBrXmlNode;
    function Gerar_Evento_PedProrrogacao(Idx: Integer): TACBrXmlNode;
    function Gerar_ItemPedido(Idx: Integer): TACBrXmlNodeArray;
    function Gerar_Evento_CancPedProrrogacao(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_ComprEntrega(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_CancComprEntrega(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_AtorInteressado(Idx: Integer): TACBrXmlNode;
    function Gerar_AutXml(Idx: Integer): TACBrXmlNodeArray;
    function Gerar_Evento_InsucessoEntrega(Idx: Integer): TACBrXmlNode;
    function Gerar_Evento_CancInsucessoEntrega(Idx: Integer): TACBrXmlNode;

  public
    constructor Create;
    destructor Destroy; override;

    function GerarXml: Boolean; Override;

    function LerXML(const CaminhoArquivo: string): Boolean;
    function LerXMLFromString(const AXML: string): Boolean;
    function ObterNomeArquivo(tpEvento: TpcnTpEvento): string;
    function LerFromIni(const AIniString: string; CCe: Boolean = True): Boolean;

    property idLote: Int64                read FidLote  write FidLote;
    property Evento: TInfEventoCollection read FEvento  write SetEvento;
    property Versao: string               read FVersao  write FVersao;

    property Opcoes: TNFeXmlWriterOptions read GetOpcoes write SetOpcoes;

    property XmlEnvio: string read FXmlEnvio write FXmlEnvio;
  end;

implementation

uses
  IniFiles,
  ACBrDFeUtil,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.FilesIO,
  ACBrNFe.RetEnvEvento,
  pcnConversaoNFe;

{ TEventoNFe }

constructor TEventoNFe.Create;
begin
  inherited Create;

  FEvento  := TInfEventoCollection.Create();

  Opcoes.AjustarTagNro := True;
  Opcoes.NormatizarMunicipios := False;
  Opcoes.PathArquivoMunicipios := '';
  Opcoes.GerarTagAssinatura := pcnConversao.taSomenteSeAssinada;
  Opcoes.ValidarInscricoes := False;
end;

function TEventoNFe.CreateOptions: TACBrXmlWriterOptions;
begin
  Result := TNFeXmlWriterOptions.Create();
end;

destructor TEventoNFe.Destroy;
begin
  FEvento.Free;

  inherited;
end;

function TEventoNFe.ObterNomeArquivo(tpEvento: TpcnTpEvento): string;
begin
  case tpEvento of
    teCCe: Result := IntToStr(Self.idLote) + '-cce.xml';

    teCancelamento,
    teCancSubst: Result := IntToStr(Self.idLote) + '-can-eve.xml';

    teManifDestCiencia,
    teManifDestConfirmacao,
    teManifDestDesconhecimento,
    teManifDestOperNaoRealizada: Result := IntToStr(Self.idLote) + '-man-des.xml';

    teEPECNFe: Result := Evento[0].InfEvento.chNFe + '-ped-epec.xml';

    tePedProrrog1,
    tePedProrrog2: Result := Evento[0].InfEvento.chNFe + '-ped-prorr.xml';

    teCanPedProrrog1,
    teCanPedProrrog2: Result := Evento[0].InfEvento.chNFe + '-can-prorr.xml';

    teComprEntregaNFe: Result := Evento[0].InfEvento.chNFe + '-comp-entr.xml';

    teCancComprEntregaNFe: Result := Evento[0].InfEvento.chNFe + '-can-entr.xml';

    teAtorInteressadoNFe: Result := Evento[0].InfEvento.chNFe + '-ator-inter.xml';
  else
    raise EventoException.Create('Obter nome do arquivo de Evento não Implementado!');
  end;
end;

function TEventoNFe.GerarXML: Boolean;
var
  EventoNode: TACBrXmlNode;
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  ListaDeAlertas.Clear;

  FDocument.Clear();

  EventoNode := CreateElement('envEvento');
  EventoNode.SetNamespace('http://www.portalfiscal.inf.br/nfe');
  EventoNode.SetAttribute('versao', Versao);

  FDocument.Root := EventoNode;

  EventoNode.AppendChild(AddNode(tcInt64, '#1', 'idLote', 1, 15, 1,
                                                          FidLote, DSC_IDLOTE));

  nodeArray := Gerar_Evento;
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      EventoNode.AppendChild(nodeArray[i]);
    end;
  end;

  Result := True;
  XmlEnvio := ChangeLineBreak(Document.Xml, '');
end;

function TEventoNFe.Gerar_Evento: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Evento.Count);

  for i := 0 to Evento.Count - 1 do
  begin
    Evento[i].InfEvento.id := 'ID' + Evento[i].InfEvento.TipoEvento +
                               OnlyNumber(Evento[i].InfEvento.chNFe) +
                               Format('%.2d', [Evento[i].InfEvento.nSeqEvento]);

    if Length(Evento[i].InfEvento.id) < 54 then
      wAlerta('HP07', 'ID', '', 'ID de Evento inválido');

    Result[i] := CreateElement('evento');
    Result[i].SetNamespace('http://www.portalfiscal.inf.br/nfe');
    Result[i].SetAttribute('versao', Versao);

    Result[i].AppendChild(Gerar_InfEvento(i));

    // Incluir a assinatura no XML
    if Evento[i].signature.URI <> '' then
      Result[i].AppendChild(GerarSignature(Evento[i].signature));
  end;

  if Evento.Count > 20 then
    wAlerta('#1', 'evento', '', ERR_MSG_MAIOR_MAXIMO + '20');
end;

function TEventoNFe.Gerar_AutXml(Idx: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Evento[Idx].FInfEvento.detEvento.autXML.Count);

  for i := 0 to Evento[Idx].FInfEvento.detEvento.autXML.Count - 1 do
  begin
    Result[i] := CreateElement('autXML');

    Result[i].AppendChild(AddNodeCNPJCPF('HP24', 'HP25',
                           Evento[Idx].FInfEvento.detEvento.autXML[i].CNPJCPF));
  end;

  if Evento[Idx].FInfEvento.detEvento.autXML.Count > 1 then
    wAlerta('#1', 'autXML', '', ERR_MSG_MAIOR_MAXIMO + '1');
end;

function TEventoNFe.Gerar_DestNFCe(
  const EventoItem: TInfEventoCollectionItem): TACBrXmlNode;
var
  sDoc: string;
begin
  Result := nil;

  if (EventoItem.InfEvento.detEvento.dest.UF <> '') and
      ((EventoItem.InfEvento.detEvento.dest.CNPJCPF <> '') or
       (EventoItem.InfEvento.detEvento.dest.idEstrangeiro <> '')) then
  begin
    Result := CreateElement('dest');

    Result.AppendChild(AddNode(tcStr, 'HP27', 'UF', 2, 2, 1,
                                       EventoItem.InfEvento.detEvento.dest.UF));

    if (EventoItem.InfEvento.detEvento.dest.idEstrangeiro = '') and
       (EventoItem.InfEvento.detEvento.dest.UF <> 'EX') then
    begin
      sDoc := OnlyNumber( EventoItem.InfEvento.detEvento.dest.CNPJCPF );

      if Length(sDoc) = 14 then
      begin
        Result.AppendChild(AddNode(tcStr, 'HP28', 'CNPJ', 14, 14, 1,
                                                              sDoc , DSC_CNPJ));

        if not ValidarCNPJ(sDoc) then
          wAlerta('HP28', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
      end
      else
      begin
        Result.AppendChild(AddNode(tcStr, 'HP29', 'CPF', 11, 11, 1,
                                                               sDoc , DSC_CPF));

        if not ValidarCPF(sDoc) then
          wAlerta('HP29', 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
      end;
    end
    else
    begin
      Result.AppendChild(AddNode(tcStr, 'HP30', 'idEstrangeiro', 5, 20, 1,
                            EventoItem.InfEvento.detEvento.dest.idEstrangeiro));
    end;
  end;
end;

function TEventoNFe.Gerar_DestNFe(
  const EventoItem: TInfEventoCollectionItem): TACBrXmlNode;
var
  sDoc: string;
begin
  Result := CreateElement('dest');

  Result.AppendChild(AddNode(tcStr, 'HP27', 'UF', 2, 2, 1,
                                       EventoItem.InfEvento.detEvento.dest.UF));

  if (EventoItem.InfEvento.detEvento.dest.idEstrangeiro = '') and
     (EventoItem.InfEvento.detEvento.dest.UF <> 'EX') then
  begin
    sDoc := OnlyNumber( EventoItem.InfEvento.detEvento.dest.CNPJCPF );

    if Length(sDoc) = 14 then
    begin
      Result.AppendChild(AddNode(tcStr, 'HP28', 'CNPJ', 14, 14, 1,
                                                              sDoc , DSC_CNPJ));

      if not ValidarCNPJ(sDoc) then
        wAlerta('HP28', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
    end
    else
    begin
      Result.AppendChild(AddNode(tcStr, 'HP29', 'CPF', 11, 11, 1,
                                                               sDoc , DSC_CPF));

      if not ValidarCPF(sDoc) then
        wAlerta('HP29', 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
    end;
  end
  else
  begin
    Result.AppendChild(AddNode(tcStr, 'HP30', 'idEstrangeiro', 5, 20, 1,
                            EventoItem.InfEvento.detEvento.dest.idEstrangeiro));
  end;

  Result.AppendChild(AddNode(tcStr, 'HP31', 'IE', 2, 14, 0,
                                       EventoItem.InfEvento.detEvento.dest.IE));

  // No EPEC da NF-e segundo o schema as TAGs vNF, vICMS e vST estão dentro do
  // grupo dest.

  Result.AppendChild(AddNode(tcDe2, 'HP32', 'vNF', 1, 15, 1,
                                  EventoItem.InfEvento.detEvento.vNF, DSC_VDF));

  Result.AppendChild(AddNode(tcDe2, 'HP33', 'vICMS', 1, 15, 1,
                             EventoItem.FInfEvento.detEvento.vICMS, DSC_VICMS));

  Result.AppendChild(AddNode(tcDe2, 'HP34', 'vST', 1, 15, 1,
                                 EventoItem.FInfEvento.detEvento.vST, DSC_VST));
end;

function TEventoNFe.Gerar_Evento_AtorInteressado(Idx: Integer): TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcInt, 'HP20', 'cOrgaoAutor', 1, 2, 1,
                                 Evento[Idx].FInfEvento.detEvento.cOrgaoAutor));

  Result.AppendChild(AddNode(tcStr, 'HP21', 'tpAutor', 1, 1, 1, '1'));

  Result.AppendChild(AddNode(tcStr, 'HP22', 'verAplic', 1, 20, 1,
                                    Evento[Idx].FInfEvento.detEvento.verAplic));

  nodeArray := Gerar_AutXml(Idx);
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;

  Result.AppendChild(AddNode(tcStr, 'HP27', 'tpAutorizacao', 1, 1, 1,
              AutorizacaoToStr(Evento[Idx].InfEvento.detEvento.tpAutorizacao)));

  if Evento[Idx].InfEvento.detEvento.tpAutorizacao = taPermite then
    Result.AppendChild(AddNode(tcStr, 'HP28', 'xCondUso', 1, 255, 1,
      'O emitente ou destinatário da NF-e, declara que permite o transportador ' +
      'declarado no campo CNPJ/CPF deste evento a autorizar os transportadores ' +
      'subcontratados ou redespachados a terem acesso ao download da NF-e'));
end;

function TEventoNFe.Gerar_Evento_CancComprEntrega(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcInt, 'HP20', 'cOrgaoAutor', 1, 2, 1,
                                 Evento[Idx].FInfEvento.detEvento.cOrgaoAutor));

  Result.AppendChild(AddNode(tcStr, 'HP21', 'tpAutor', 1, 1, 1, '1'));

  Result.AppendChild(AddNode(tcStr, 'HP22', 'verAplic', 1, 20, 1,
                                    Evento[Idx].FInfEvento.detEvento.verAplic));

  Result.AppendChild(AddNode(tcStr, 'HP23', 'nProtEvento', 15, 15, 1,
                                 Evento[Idx].FInfEvento.detEvento.nProtEvento));
end;

function TEventoNFe.Gerar_Evento_Cancelamento(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'HP20', 'nProt', 15, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcStr, 'HP21', 'xJust', 15, 255, 1,
                                       Evento[Idx].FInfEvento.detEvento.xJust));
end;

function TEventoNFe.Gerar_Evento_CancPedProrrogacao(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'HP20', 'idPedidoCancelado', 54, 54, 1,
                           Evento[Idx].FInfEvento.detEvento.idPedidoCancelado));

  Result.AppendChild(AddNode(tcStr, 'HP21', 'nProt', 15, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));
end;

function TEventoNFe.Gerar_Evento_CancSubstituicao(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcInt, 'HP20', 'cOrgaoAutor', 1, 2, 1,
                                 Evento[Idx].FInfEvento.detEvento.cOrgaoAutor));

  Result.AppendChild(AddNode(tcStr, 'HP21', 'tpAutor', 1, 1, 1, '1'));

  Result.AppendChild(AddNode(tcStr, 'HP22', 'verAplic', 1, 20, 1,
                                    Evento[Idx].FInfEvento.detEvento.verAplic));

  Result.AppendChild(AddNode(tcStr, 'HP23', 'nProt', 15, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));

  Result.AppendChild(AddNode(tcStr, 'HP24', 'xJust', 15, 255, 1,
                                       Evento[Idx].FInfEvento.detEvento.xJust));

  Result.AppendChild(AddNode(tcStr, 'HP25', 'chNFeRef', 44, 44, 1,
                         Evento[Idx].FInfEvento.detEvento.chNFeRef, DSC_CHAVE));

  if not ValidarChave(Evento[Idx].InfEvento.detEvento.chNFeRef) then
    wAlerta('HP31', 'chNFeRef', '', 'Chave de NFe Refenciada inválida');
end;

function TEventoNFe.Gerar_Evento_CCe(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'HP20', 'xCorrecao', 15, 1000, 1,
                                   Evento[Idx].FInfEvento.detEvento.xCorrecao));

  Result.AppendChild(AddNode(tcStr, 'HP20a', 'xCondUso', 1, 5000, 1,
                                    Evento[Idx].FInfEvento.detEvento.xCondUso));
end;

function TEventoNFe.Gerar_Evento_ComprEntrega(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcInt, 'HP20', 'cOrgaoAutor', 1, 2, 1,
                                 Evento[Idx].FInfEvento.detEvento.cOrgaoAutor));

  Result.AppendChild(AddNode(tcStr, 'HP21', 'tpAutor', 1, 1, 1, '1'));

  Result.AppendChild(AddNode(tcStr, 'HP22', 'verAplic', 1, 20, 1,
                                    Evento[Idx].FInfEvento.detEvento.verAplic));

  Result.AppendChild(AddNode(tcStr, 'HP23', 'dhEntrega', 25, 25, 1,
    DateTimeTodh(Evento[Idx].InfEvento.detEvento.dhEntrega) +
    GetUTC(CodigoUFparaUF(Evento[Idx].InfEvento.detEvento.cOrgaoAutor),
                   Evento[Idx].InfEvento.detEvento.dhEntrega), DSC_DEMI));

  Result.AppendChild(AddNode(tcStr, 'HP24', 'nDoc', 2, 20, 1,
                                        Evento[Idx].FInfEvento.detEvento.nDoc));

  Result.AppendChild(AddNode(tcStr, 'HP25', 'xNome', 2, 60, 1,
                                       Evento[Idx].FInfEvento.detEvento.xNome));

  Result.AppendChild(AddNode(tcDe6, 'HP26', 'latGPS', 1, 10, 0,
                                      Evento[Idx].FInfEvento.detEvento.latGPS));

  Result.AppendChild(AddNode(tcDe6, 'HP27', 'longGPS', 1, 11, 0,
                                     Evento[Idx].FInfEvento.detEvento.longGPS));

  Result.AppendChild(AddNode(tcStr, 'HP28', 'hashComprovante', 28, 28, 1,
                             Evento[Idx].FInfEvento.detEvento.hashComprovante));

  Result.AppendChild(AddNode(tcStr, 'HP29', 'dhHashComprovante', 25, 25, 1,
    DateTimeTodh(Evento[Idx].InfEvento.detEvento.dhHashComprovante) +
    GetUTC(CodigoUFparaUF(Evento[Idx].InfEvento.detEvento.cOrgaoAutor),
           Evento[Idx].InfEvento.detEvento.dhHashComprovante), DSC_DEMI));
end;

function TEventoNFe.Gerar_Evento_EPEC(Idx: Integer): TACBrXmlNode;
var
  sModelo: string;
begin
  sModelo := Copy(OnlyNumber(Evento[Idx].InfEvento.chNFe), 21, 2);

  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcInt, 'HP20', 'cOrgaoAutor', 1, 2, 1,
                                 Evento[Idx].FInfEvento.detEvento.cOrgaoAutor));

  Result.AppendChild(AddNode(tcStr, 'HP21', 'tpAutor', 1, 1, 1, '1'));

  Result.AppendChild(AddNode(tcStr, 'HP22', 'verAplic', 1, 20, 1,
                                    Evento[Idx].FInfEvento.detEvento.verAplic));

  Result.AppendChild(AddNode(tcStr, 'HP23', 'dhEmi', 1, 50, 1,
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Evento[Idx].InfEvento.detEvento.dhEmi) +
    GetUTC(CodigoUFparaUF(Evento[Idx].InfEvento.detEvento.cOrgaoAutor),
    Evento[Idx].InfEvento.detEvento.dhEmi)));

  Result.AppendChild(AddNode(tcStr, 'HP24', 'tpNF', 1, 1, 1,
                        tpNFToStr(Evento[Idx].InfEvento.detEvento.tpNF)));

  Result.AppendChild(AddNode(tcStr, 'HP25', 'IE', 2, 14, 1,
                                          Evento[Idx].FInfEvento.detEvento.IE));

  if sModelo = '55' then
    Result.AppendChild(Gerar_DestNFe(Evento[Idx]))
  else
  begin
    Result.AppendChild(Gerar_DestNFCe(Evento[Idx]));
    // No EPEC da NFC-e segundo o schema as TAGs vNF e vICMS estão fora do grupo
    // dest e não tem a TAG vST.

    Result.AppendChild(AddNode(tcDe2, 'HP32', 'vNF', 1, 15, 1,
                                Evento[Idx].FInfEvento.detEvento.vNF, DSC_VDF));

    Result.AppendChild(AddNode(tcDe2, 'HP33', 'vICMS', 1, 15, 1,
                            Evento[Idx].FInfEvento.detEvento.vICMS, DSC_VICMS));
  end;
end;

function TEventoNFe.Gerar_Evento_ManifDestCiencia(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));
end;

function TEventoNFe.Gerar_Evento_ManifDestConfirmacao(
  Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));
end;

function TEventoNFe.Gerar_Evento_ManifDestDesconhecimento(
  Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));
end;

function TEventoNFe.Gerar_Evento_ManifDestOperNaoRealizada(
  Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'HP20', 'xJust', 15, 255, 1,
                                       Evento[Idx].FInfEvento.detEvento.xJust));
end;

function TEventoNFe.Gerar_Evento_PedProrrogacao(Idx: Integer): TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcStr, 'HP20', 'nProt', 15, 15, 1,
                                       Evento[Idx].FInfEvento.detEvento.nProt));

  nodeArray := Gerar_ItemPedido(Idx);
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TEventoNFe.Gerar_Evento_InsucessoEntrega(Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcInt, 'P20', 'cOrgaoAutor', 1, 2, 1,
                                 Evento[Idx].FInfEvento.detEvento.cOrgaoAutor));

  Result.AppendChild(AddNode(tcStr, 'P21', 'verAplic', 1, 20, 1,
                                    Evento[Idx].FInfEvento.detEvento.verAplic));

  Result.AppendChild(AddNode(tcStr, 'P30', 'dhTentativaEntrega', 1, 50, 1,
    DateTimeTodh(Evento[Idx].InfEvento.detEvento.dhTentativaEntrega) +
    GetUTC(Evento[Idx].InfEvento.detEvento.UF,
      Evento[Idx].InfEvento.detEvento.dhTentativaEntrega)));

  Result.AppendChild(AddNode(tcInt, 'P31', 'nTentativa', 3, 3, 0,
                                  Evento[Idx].FInfEvento.detEvento.nTentativa));

  Result.AppendChild(AddNode(tcStr, 'P32', 'tpMotivo', 1, 1, 1,
                      tpMotivoToStr(Evento[Idx].InfEvento.detEvento.tpMotivo)));

  if Evento[Idx].InfEvento.detEvento.tpMotivo = tmOutro then
    Result.AppendChild(AddNode(tcStr, 'P33', 'xJustMotivo', 25, 250, 0,
                                 Evento[Idx].FInfEvento.detEvento.xJustMotivo));

  Result.AppendChild(AddNode(tcDe6, 'P34', 'latGPS', 1, 10, 0,
                                      Evento[Idx].FInfEvento.detEvento.latGPS));

  Result.AppendChild(AddNode(tcDe6, 'P35', 'longGPS', 1, 11, 0,
                                     Evento[Idx].FInfEvento.detEvento.longGPS));

  Result.AppendChild(AddNode(tcStr, 'P36', 'hashTentativaEntrega', 20, 20, 1,
                        Evento[Idx].FInfEvento.detEvento.hashTentativaEntrega));

  Result.AppendChild(AddNode(tcStr, 'P37', 'dhHashTentativaEntrega', 25, 25, 0,
    DateTimeTodh(Evento[Idx].InfEvento.detEvento.dhHashTentativaEntrega) +
    GetUTC(Evento[Idx].InfEvento.detEvento.UF,
      Evento[Idx].InfEvento.detEvento.dhHashTentativaEntrega)));
end;

function TEventoNFe.Gerar_Evento_CancInsucessoEntrega(
  Idx: Integer): TACBrXmlNode;
begin
  Result := CreateElement('detEvento');
  Result.SetAttribute('versao', Versao);

  Result.AppendChild(AddNode(tcStr, 'HP19', 'descEvento', 4, 60, 1,
                                            Evento[Idx].FInfEvento.DescEvento));

  Result.AppendChild(AddNode(tcInt, 'P20', 'cOrgaoAutor', 1, 2, 1,
                                 Evento[Idx].FInfEvento.detEvento.cOrgaoAutor));

  Result.AppendChild(AddNode(tcStr, 'P21', 'verAplic', 1, 20, 1,
                                    Evento[Idx].FInfEvento.detEvento.verAplic));

  Result.AppendChild(AddNode(tcStr, 'HP23', 'nProtEvento', 15, 15, 1,
                                 Evento[Idx].FInfEvento.detEvento.nProtEvento));
end;

function TEventoNFe.Gerar_InfEvento(Idx: Integer): TACBrXmlNode;
var
  sDoc: string;
  Serie: Integer;
begin
  Result := CreateElement('infEvento');
  Result.SetAttribute('Id', Evento[Idx].InfEvento.id);

  Result.AppendChild(AddNode(tcInt, 'HP08', 'cOrgao', 1, 2, 1,
                                                Evento[Idx].FInfEvento.cOrgao));

  Result.AppendChild(AddNode(tcStr, 'HP09', 'tpAmb', 1, 1, 1,
                           TpAmbToStr(Evento[Idx].InfEvento.tpAmb), DSC_TPAMB));

  sDoc := OnlyNumber(Evento[Idx].InfEvento.CNPJ);

  if EstaVazio(sDoc) then
    sDoc := ExtrairCNPJCPFChaveAcesso(Evento[Idx].InfEvento.chNFe);

  // Verifica a Série do Documento, caso esteja no intervalo de 910-969
  // o emitente é pessoa fisica, logo na chave temos um CPF.
  Serie := ExtrairSerieChaveAcesso(Evento[Idx].InfEvento.chNFe);

  if (Length(sDoc) = 14) and (Serie >= 910) and (Serie <= 969) and
     not (Evento[Idx].InfEvento.tpEvento in [teManifDestConfirmacao..teManifDestOperNaoRealizada]) then
  begin
    sDoc := Copy(sDoc, 4, 11);
  end;

  if Length(sDoc) = 14 then
  begin
    Result.AppendChild(AddNode(tcStr, 'HP10', 'CNPJ', 14, 14, 1,
                                                              sDoc , DSC_CNPJ));

    if not ValidarCNPJ(sDoc) then
      wAlerta('HP10', 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
  end
  else
  begin
    Result.AppendChild(AddNode(tcStr, 'HP11', 'CPF', 11, 11, 1,
                                                               sDoc , DSC_CPF));

    if not ValidarCPF(sDoc) then
      wAlerta('HP11', 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
  end;

  Result.AppendChild(AddNode(tcStr, 'HP12', 'chNFe', 44, 44, 1,
                                      Evento[Idx].FInfEvento.chNFe, DSC_CHAVE));

  if not ValidarChave(Evento[Idx].InfEvento.chNFe) then
    wAlerta('HP12', 'chNFe', '', 'Chave de NFe inválida');

  Result.AppendChild(AddNode(tcStr, 'HP13', 'dhEvento', 1, 50, 1,
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Evento[Idx].InfEvento.dhEvento)+
    GetUTC(CodigoUFparaUF(Evento[Idx].InfEvento.cOrgao),
    Evento[Idx].InfEvento.dhEvento)));

  Result.AppendChild(AddNode(tcInt, 'HP14', 'tpEvento', 6, 6, 1,
                                            Evento[Idx].FInfEvento.TipoEvento));

  Result.AppendChild(AddNode(tcInt, 'HP15', 'nSeqEvento', 1, 2, 1,
                                            Evento[Idx].FInfEvento.nSeqEvento));

  Result.AppendChild(AddNode(tcStr, 'HP16', 'verEvento', 1, 4, 1, Versao));

  if Evento[Idx].InfEvento.tpEvento = teAtorInteressadoNFe then
    FOpcoes.RetirarAcentos := False;  // Não funciona sem acentos

  case Evento[Idx].InfEvento.tpEvento of
    teCCe: Result.AppendChild(Gerar_Evento_CCe(Idx));

    teCancelamento: Result.AppendChild(Gerar_Evento_Cancelamento(Idx));

    teCancSubst: Result.AppendChild(Gerar_Evento_CancSubstituicao(Idx));

    teManifDestCiencia: Result.AppendChild(Gerar_Evento_ManifDestCiencia(Idx));

    teManifDestConfirmacao: Result.AppendChild(Gerar_Evento_ManifDestConfirmacao(Idx));

    teManifDestDesconhecimento: Result.AppendChild(Gerar_Evento_ManifDestDesconhecimento(Idx));

    teManifDestOperNaoRealizada: Result.AppendChild(Gerar_Evento_ManifDestOperNaoRealizada(Idx));

    teEPECNFe: Result.AppendChild(Gerar_Evento_EPEC(Idx));

    tePedProrrog1,
    tePedProrrog2: Result.AppendChild(Gerar_Evento_PedProrrogacao(Idx));

    teCanPedProrrog1,
    teCanPedProrrog2: Result.AppendChild(Gerar_Evento_CancPedProrrogacao(Idx));

    teComprEntregaNFe: Result.AppendChild(Gerar_Evento_ComprEntrega(Idx));

    teCancComprEntregaNFe: Result.AppendChild(Gerar_Evento_CancComprEntrega(Idx));

    teAtorInteressadoNFe: Result.AppendChild(Gerar_Evento_AtorInteressado(Idx));

    teInsucessoEntregaNFe: Result.AppendChild(Gerar_Evento_InsucessoEntrega(Idx));

    teCancInsucessoEntregaNFe: Result.AppendChild(Gerar_Evento_CancInsucessoEntrega(Idx));
  end;
end;

function TEventoNFe.Gerar_ItemPedido(Idx: Integer): TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Evento[Idx].FInfEvento.detEvento.itemPedido.Count);

  for i := 0 to Evento[Idx].FInfEvento.detEvento.itemPedido.Count - 1 do
  begin
    Result[i] := CreateElement('itemPedido');
    Result[i].SetAttribute('numItem',
     intToStr(Evento[Idx].InfEvento.detEvento.itemPedido[i].numItem));

    Result[i].AppendChild(AddNode(tcDe2, 'HP22', 'qtdeItem', 1, 15, 1,
                       Evento[Idx].InfEvento.detEvento.itemPedido[i].qtdeItem));
  end;

  if Evento[Idx].FInfEvento.detEvento.itemPedido.Count > 990 then
    wAlerta('#1', 'itemPedido', '', ERR_MSG_MAIOR_MAXIMO + '990');
end;

function TEventoNFe.GetOpcoes: TNFeXmlWriterOptions;
begin
  Result := TNFeXmlWriterOptions(FOpcoes);
end;

procedure TEventoNFe.SetEvento(const Value: TInfEventoCollection);
begin
  FEvento.Assign(Value);
end;

procedure TEventoNFe.SetOpcoes(const Value: TNFeXmlWriterOptions);
begin
  FOpcoes := Value;
end;

function TEventoNFe.LerXML(const CaminhoArquivo: string): Boolean;
var
  ArqEvento: TStringList;
begin
  ArqEvento := TStringList.Create;

  try
    ArqEvento.LoadFromFile(CaminhoArquivo);
    Result := LerXMLFromString(ArqEvento.Text);
  finally
    ArqEvento.Free;
  end;
end;

function TEventoNFe.LerXMLFromString(const AXML: string): Boolean;
var
  RetEventoNFe: TRetEventoNFe;
  i: Integer;
begin
  RetEventoNFe := TRetEventoNFe.Create;

  try
    RetEventoNFe.XmlRetorno := AXML;
    Result := RetEventoNFe.LerXml;

    with FEvento.New do
    begin
      XML                    := AXML;
      infEvento.ID           := RetEventoNFe.InfEvento.id;
      infEvento.cOrgao       := RetEventoNFe.InfEvento.cOrgao;
      infEvento.tpAmb        := RetEventoNFe.InfEvento.tpAmb;
      infEvento.CNPJ         := RetEventoNFe.InfEvento.CNPJ;
      infEvento.chNFe        := RetEventoNFe.InfEvento.chNFe;
      infEvento.dhEvento     := RetEventoNFe.InfEvento.dhEvento;
      infEvento.tpEvento     := RetEventoNFe.InfEvento.tpEvento;
      infEvento.nSeqEvento   := RetEventoNFe.InfEvento.nSeqEvento;
      infEvento.VersaoEvento := RetEventoNFe.InfEvento.VersaoEvento;

      infEvento.DetEvento.descEvento := RetEventoNFe.InfEvento.DetEvento.descEvento;
      infEvento.DetEvento.xCorrecao  := RetEventoNFe.InfEvento.DetEvento.xCorrecao;
      infEvento.DetEvento.xCondUso   := RetEventoNFe.InfEvento.DetEvento.xCondUso;
      infEvento.DetEvento.nProt      := RetEventoNFe.InfEvento.DetEvento.nProt;
      infEvento.DetEvento.xJust      := RetEventoNFe.InfEvento.DetEvento.xJust;
      infEvento.DetEvento.chNFeRef   := RetEventoNFe.InfEvento.DetEvento.chNFeRef;

      infEvento.detEvento.cOrgaoAutor := RetEventoNFe.InfEvento.detEvento.cOrgaoAutor;
      infEvento.detEvento.tpAutor     := RetEventoNFe.InfEvento.detEvento.tpAutor;
      infEvento.detEvento.verAplic    := RetEventoNFe.InfEvento.detEvento.verAplic;
      infEvento.detEvento.dhEmi       := RetEventoNFe.InfEvento.detEvento.dhEmi;
      infEvento.detEvento.tpNF        := RetEventoNFe.InfEvento.detEvento.tpNF;
      infEvento.detEvento.IE          := RetEventoNFe.InfEvento.detEvento.IE;

      infEvento.detEvento.dest.UF            := RetEventoNFe.InfEvento.detEvento.dest.UF;
      infEvento.detEvento.dest.CNPJCPF       := RetEventoNFe.InfEvento.detEvento.dest.CNPJCPF;
      infEvento.detEvento.dest.idEstrangeiro := RetEventoNFe.InfEvento.detEvento.dest.idEstrangeiro;
      infEvento.detEvento.dest.IE            := RetEventoNFe.InfEvento.detEvento.dest.IE;

      infEvento.detEvento.vNF   := RetEventoNFe.InfEvento.detEvento.vNF;
      infEvento.detEvento.vICMS := RetEventoNFe.InfEvento.detEvento.vICMS;
      infEvento.detEvento.vST   := RetEventoNFe.InfEvento.detEvento.vST;

      for i := 0 to RetEventoNFe.InfEvento.detEvento.itemPedido.Count -1 do
      begin
        InfEvento.detEvento.itemPedido[i].numItem := RetEventoNFe.InfEvento.detEvento.itemPedido[i].numItem;
        InfEvento.detEvento.itemPedido[i].qtdeItem := RetEventoNFe.InfEvento.detEvento.itemPedido[i].qtdeItem;
      end;

      infEvento.detEvento.idPedidoCancelado := RetEventoNFe.InfEvento.detEvento.idPedidoCancelado;

      infEvento.detEvento.dhEntrega := RetEventoNFe.InfEvento.detEvento.dhEntrega;
      infEvento.detEvento.nDoc      := RetEventoNFe.InfEvento.detEvento.nDoc;
      infEvento.detEvento.xNome     := RetEventoNFe.InfEvento.detEvento.xNome;
      infEvento.detEvento.latGPS    := RetEventoNFe.InfEvento.detEvento.latGPS;
      infEvento.detEvento.longGPS   := RetEventoNFe.InfEvento.detEvento.longGPS;

      infEvento.detEvento.hashComprovante   := RetEventoNFe.InfEvento.detEvento.hashComprovante;
      infEvento.detEvento.dhHashComprovante := RetEventoNFe.InfEvento.detEvento.dhHashComprovante;

      infEvento.detEvento.nProtEvento := RetEventoNFe.InfEvento.detEvento.nProtEvento;

      infEvento.detEvento.tpAutorizacao := RetEventoNFe.InfEvento.detEvento.tpAutorizacao;

      if RetEventoNFe.InfEvento.detEvento.autXML.Count > 0 then
      begin
        InfEvento.detEvento.autXML.New.CNPJCPF := RetEventoNFe.InfEvento.detEvento.autXML[0].CNPJCPF;
      end;

      // Insucesso na Entrega
      infEvento.detEvento.dhTentativaEntrega := RetEventoNFe.InfEvento.detEvento.dhTentativaEntrega;
      infEvento.detEvento.nTentativa := RetEventoNFe.InfEvento.detEvento.nTentativa;
      infEvento.detEvento.tpMotivo := RetEventoNFe.InfEvento.detEvento.tpMotivo;
      infEvento.detEvento.xJustMotivo := RetEventoNFe.InfEvento.detEvento.xJustMotivo;
      infEvento.detEvento.hashTentativaEntrega := RetEventoNFe.InfEvento.detEvento.hashTentativaEntrega;
      infEvento.detEvento.dhHashTentativaEntrega := RetEventoNFe.InfEvento.detEvento.dhHashTentativaEntrega;
      infEvento.detEvento.UF := RetEventoNFe.InfEvento.detEvento.UF;

      signature.URI             := RetEventoNFe.signature.URI;
      signature.DigestValue     := RetEventoNFe.signature.DigestValue;
      signature.SignatureValue  := RetEventoNFe.signature.SignatureValue;
      signature.X509Certificate := RetEventoNFe.signature.X509Certificate;

      if RetEventoNFe.retEvento.Count > 0 then
      begin
        RetInfEvento.Id := RetEventoNFe.retEvento[0].RetInfEvento.Id;
        RetInfEvento.tpAmb := RetEventoNFe.retEvento[0].RetInfEvento.tpAmb;
        RetInfEvento.verAplic := RetEventoNFe.retEvento[0].RetInfEvento.verAplic;
        RetInfEvento.cOrgao := RetEventoNFe.retEvento[0].RetInfEvento.cOrgao;
        RetInfEvento.cStat := RetEventoNFe.retEvento[0].RetInfEvento.cStat;
        RetInfEvento.xMotivo := RetEventoNFe.retEvento[0].RetInfEvento.xMotivo;
        RetInfEvento.chNFe := RetEventoNFe.retEvento[0].RetInfEvento.chNFe;
        RetInfEvento.tpEvento := RetEventoNFe.retEvento[0].RetInfEvento.tpEvento;
        RetInfEvento.xEvento := RetEventoNFe.retEvento[0].RetInfEvento.xEvento;
        RetInfEvento.nSeqEvento := RetEventoNFe.retEvento[0].RetInfEvento.nSeqEvento;
        RetInfEvento.cOrgaoAutor := RetEventoNFe.retEvento[0].RetInfEvento.cOrgaoAutor;
        RetInfEvento.CNPJDest := RetEventoNFe.retEvento[0].RetInfEvento.CNPJDest;
        RetInfEvento.emailDest := RetEventoNFe.retEvento[0].RetInfEvento.emailDest;
        RetInfEvento.dhRegEvento := RetEventoNFe.retEvento[0].RetInfEvento.dhRegEvento;
        RetInfEvento.nProt := RetEventoNFe.retEvento[0].RetInfEvento.nProt;
        RetInfEvento.XML := RetEventoNFe.retEvento[0].RetInfEvento.XML;
      end;
    end;
  finally
    RetEventoNFe.Free;
  end;
end;

function TEventoNFe.LerFromIni(const AIniString: string; CCe: Boolean): Boolean;
var
  I, J: Integer;
  sSecao, sFim: string;
  INIRec: TMemIniFile;
  ok: Boolean;
  Item: TitemPedidoCollectionItem;
begin
{$IFNDEF COMPILER23_UP}
  Result := False;
{$ENDIF}
  Self.Evento.Clear;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);
    idLote := INIRec.ReadInteger( 'EVENTO', 'idLote',
                                        INIRec.ReadInteger('CCE', 'idLote', 0));

    I := 1;
    while true do
    begin
      sSecao := 'EVENTO' + IntToStrZero(I, 3);
      sFim := INIRec.ReadString(sSecao, 'chNFe', 'FIM');
      if (sFim = 'FIM') or (Length(sFim) <= 0) then
        break ;

      with Self.Evento.New do
      begin
        infEvento.cOrgao := INIRec.ReadInteger(sSecao, 'cOrgao', 0);
        infEvento.CNPJ   := INIRec.ReadString(sSecao, 'CNPJ', '');
        infEvento.chNFe  := sFim;
        infEvento.dhEvento := StringToDateTime(INIRec.ReadString(sSecao, 'dhEvento', ''));

        if CCe then
          infEvento.tpEvento := teCCe
        else
          infEvento.tpEvento := StrToTpEventoNFe(ok,INIRec.ReadString(sSecao, 'tpEvento', ''));

        infEvento.nSeqEvento := INIRec.ReadInteger(sSecao, 'nSeqEvento', 1);
        infEvento.versaoEvento := INIRec.ReadString(sSecao, 'versaoEvento', '1.00');
        infEvento.detEvento.cOrgaoAutor := INIRec.ReadInteger(sSecao, 'cOrgaoAutor', 0);
        infEvento.detEvento.verAplic := INIRec.ReadString(sSecao, 'verAplic', '1.0');

        if (infEvento.tpEvento = teEPECNFe) then
        begin
          infEvento.detEvento.tpAutor := StrToTipoAutor(ok, INIRec.ReadString(sSecao, 'tpAutor', '1'));
          infEvento.detEvento.dhEmi := StringToDateTime(INIRec.ReadString(sSecao, 'dhEmi', ''));
          infEvento.detEvento.tpNF := StrToTpNF(ok, INIRec.ReadString(sSecao, 'tpNF', '1'));
          infEvento.detEvento.IE := INIRec.ReadString(sSecao, 'IE', '');

          infEvento.detEvento.dest.UF := INIRec.ReadString('DEST', 'DestUF', '');
          infEvento.detEvento.dest.CNPJCPF := INIRec.ReadString('DEST', 'DestCNPJCPF', '');
          infEvento.detEvento.dest.IE := INIRec.ReadString('DEST', 'DestIE', '');

          infEvento.detEvento.vNF := StringToFloatDef(INIRec.ReadString(sSecao, 'vNF', ''), 0);
          infEvento.detEvento.vICMS := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMS', ''), 0);
          infEvento.detEvento.vST := StringToFloatDef(INIRec.ReadString(sSecao, 'vST', ''), 0);
        end
        else
        begin
          infEvento.detEvento.xCorrecao := INIRec.ReadString(sSecao, 'xCorrecao', '');
          infEvento.detEvento.xCondUso := INIRec.ReadString(sSecao, 'xCondUso', '');
          infEvento.detEvento.nProt := INIRec.ReadString(sSecao, 'nProt', '');
          infEvento.detEvento.xJust := INIRec.ReadString(sSecao, 'xJust', '');
          infEvento.detEvento.chNFeRef := INIRec.ReadString(sSecao, 'chNFeRef', '');
          infEvento.detEvento.nProtEvento := INIRec.ReadString(sSecao, 'nProtEvento', '');
        end;

        case infEvento.tpEvento of
          tePedProrrog1,
          tePedProrrog2:
            begin
              J := 1;
              while true do
              begin
                sSecao := 'itemPedido' + IntToStrZero(J, 3);
                sFim := OnlyNumber(INIRec.ReadString(sSecao,'qtdeItem', 'FIM'));

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                Item := infEvento.detEvento.itemPedido.New;

                Item.numItem := INIRec.ReadInteger(sSecao, 'numItem', 0);
                Item.qtdeItem := StrToFloat(sFim);

                Inc(J);
              end;
            end;

          teCanPedProrrog1,
          teCanPedProrrog2:
            begin
              infEvento.detEvento.idPedidoCancelado := INIRec.ReadString(sSecao, 'idPedidoCancelado', '');
            end;

          teComprEntregaNFe:
            begin
              infEvento.detEvento.tpAutor := StrToTipoAutor(ok, INIRec.ReadString(sSecao, 'tpAutor', '1'));
              infEvento.detEvento.dhEntrega := StringToDateTime(INIRec.ReadString(sSecao, 'dhEntrega', ''));
              infEvento.detEvento.nDoc := INIRec.ReadString(sSecao, 'nDoc', '');
              infEvento.detEvento.xNome := INIRec.ReadString(sSecao, 'xNome', '');
              infEvento.detEvento.latGPS := StringToFloatDef(INIRec.ReadString(sSecao, 'latGPS', ''), 0);
              infEvento.detEvento.longGPS := StringToFloatDef(INIRec.ReadString(sSecao, 'longGPS', ''), 0);

              infEvento.detEvento.hashComprovante := INIRec.ReadString(sSecao, 'hashComprovante', '');
              infEvento.detEvento.dhHashComprovante := StringToDateTime(INIRec.ReadString(sSecao, 'dhHashComprovante', ''));
            end;

          teCancComprEntregaNFe:
            begin
              infEvento.detEvento.tpAutor := StrToTipoAutor(ok, INIRec.ReadString(sSecao, 'tpAutor', '1'));
              infEvento.detEvento.nProtEvento := INIRec.ReadString(sSecao, 'nProtEvento', '');
            end;

          teAtorInteressadoNFe:
            begin
              infEvento.detEvento.tpAutor := StrToTipoAutor(ok, INIRec.ReadString(sSecao, 'tpAutor', '1'));
              infEvento.detEvento.tpAutorizacao := StrToAutorizacao(ok, INIRec.ReadString(sSecao, 'tpAutorizacao', '0'));

              J := 1;
              while true do
              begin
                sSecao := 'autXML' + IntToStrZero(J, 2);
                sFim := OnlyNumber(INIRec.ReadString(sSecao,'CNPJCPF', 'FIM'));

                if (sFim = 'FIM') or (Length(sFim) <= 0) then
                  break;

                infEvento.detEvento.autXML.New.CNPJCPF := sFim;

                Inc(J);
              end;
            end;

          teInsucessoEntregaNFe:
            begin
              infEvento.detEvento.cOrgaoAutor := INIRec.ReadInteger(sSecao, 'cOrgaoAutor', 92);
              infEvento.detEvento.verAplic := INIRec.ReadString(sSecao, 'verAplic', '1.0');
              infEvento.detEvento.dhTentativaEntrega := StringToDateTime(INIRec.ReadString(sSecao, 'dhTentativaEntrega', ''));
              infEvento.detEvento.nTentativa := INIRec.ReadInteger(sSecao, 'nTentativa', 1);
              infEvento.detEvento.tpMotivo := StrTotpMotivo(ok, INIRec.ReadString(sSecao, 'tpMotivo', '1'));
              infEvento.detEvento.xJustMotivo := INIRec.ReadString(sSecao, 'xJustMotivo', '');
              infEvento.detEvento.hashTentativaEntrega := INIRec.ReadString(sSecao, 'hashTentativaEntrega', '');
              infEvento.detEvento.dhHashTentativaEntrega := StringToDateTime(INIRec.ReadString(sSecao, 'dhHashTentativaEntrega', ''));
              infEvento.detEvento.UF := INIRec.ReadString(sSecao, 'UF', '');
            end;

          teCancInsucessoEntregaNFe:
            begin
              infEvento.detEvento.cOrgaoAutor := INIRec.ReadInteger(sSecao, 'cOrgaoAutor', 92);
              infEvento.detEvento.verAplic := INIRec.ReadString(sSecao, 'verAplic', '1.0');
              infEvento.detEvento.nProtEvento := INIRec.ReadString(sSecao, 'nProtEvento', '');
            end;
        end;
      end;

      Inc(I);
    end;

    Result := True;
  finally
    INIRec.Free;
  end;
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

{ TInfEventoCollectionItem }

constructor TInfEventoCollectionItem.Create;
begin
  inherited Create;

  FInfEvento := TInfEvento.Create;
  Fsignature := Tsignature.Create;
  FRetInfEvento := TRetInfEvento.Create;
end;

destructor TInfEventoCollectionItem.Destroy;
begin
  FInfEvento.Free;
  fsignature.Free;
  FRetInfEvento.Free;

  inherited;
end;

end.

