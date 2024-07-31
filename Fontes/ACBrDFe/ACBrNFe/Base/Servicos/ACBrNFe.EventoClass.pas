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

unit ACBrNFe.EventoClass;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  pcnConversao, pcnConversaoNFe,
  ACBrBase;

type
  EventoException = class(Exception);

  TDestinatario = class(TObject)
  private
    FUF: string;
    FCNPJCPF: string;
    FidEstrangeiro: string;
    FIE: string;
  public
    property UF: string            read FUF            write FUF;
    property CNPJCPF: string       read FCNPJCPF       write FCNPJCPF;
    property idEstrangeiro: string read FidEstrangeiro write FidEstrangeiro;
    property IE: string            read FIE            write FIE;
  end;

  TitemPedidoCollectionItem = class
  private
    FqtdeItem: Currency;
    FnumItem: Integer;
  public
    property numItem: Integer   read FnumItem  write FnumItem;
    property qtdeItem: Currency read FqtdeItem write FqtdeItem;
  end;

  TitemPedidoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TitemPedidoCollectionItem;
    procedure SetItem(Index: Integer; Value: TitemPedidoCollectionItem);
  public
    function Add: TitemPedidoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TitemPedidoCollectionItem;
    property Items[Index: Integer]: TitemPedidoCollectionItem read GetItem write SetItem; default;
  end;

  TautXMLCollectionItem = class(TObject)
  private
    FCNPJCPF: string;
  public
    procedure Assign(Source: TautXMLCollectionItem);

    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
  end;

  TautXMLCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TautXMLCollectionItem;
    procedure SetItem(Index: Integer; Value: TautXMLCollectionItem);
  public
    function Add: TautXMLCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New.'{$EndIf};
    function New: TautXMLCollectionItem;
    property Items[Index: Integer]: TautXMLCollectionItem read GetItem write SetItem; default;
  end;

  TdetPagCollectionItem = class
  private
    FindPag: TpcnIndicadorPagamento;
    FtPag: TpcnFormaPagamento;
    FxPag: string;
    FvPag: Currency;
    FdPag: TDateTime;
    FCNPJPag: string;
    FUFPag: string;
    FCNPJIF: string;
    FtBand: TpcnBandeiraCartao;
    FcAut: string;
    FCNPJReceb: string;
    FUFReceb: string;
  public
    property indPag: TpcnIndicadorPagamento read FindPag write FindPag default ipNenhum;
    property tPag: TpcnFormaPagamento read FtPag write FtPag;
    property xPag: string read FxPag write FxPag;
    property vPag: Currency read FvPag write FvPag;
    property dPag: TDateTime read FdPag write FdPag;
    property CNPJPag: string read FCNPJPag write FCNPJPag;
    property UFPag: string read FUFPag write FUFPag;
    property CNPJIF: string read FCNPJIF write FCNPJIF;
    property tBand: TpcnBandeiraCartao read FtBand write FtBand;
    property cAut: string read FcAut write FcAut;
    property CNPJReceb: string read FCNPJReceb write FCNPJReceb;
    property UFReceb: string read FUFReceb write FUFReceb;
  end;

  TdetPagCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TdetPagCollectionItem;
    procedure SetItem(Index: Integer; Value: TdetPagCollectionItem);
  public
    function Add: TdetPagCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TdetPagCollectionItem;
    property Items[Index: Integer]: TdetPagCollectionItem read GetItem write SetItem; default;
  end;

  TDetEvento = class
  private
    FVersao: string;
    FDescEvento: string;
    FCorrecao: string;     // Carta de Correção
    FCondUso: string;      // Carta de Correção
    FnProt: string;        // Cancelamento
    FxJust: string;        // Cancelamento e Manif. Destinatario
    FcOrgaoAutor: Integer; // EPEC
    FtpAutor: TpcnTipoAutor;
    FverAplic: string;
    FdhEmi: TDateTime;
    FtpNF: TpcnTipoNFe;
    FIE: string;
    Fdest: TDestinatario;
    FvNF: Currency;
    FvICMS: Currency;
    FvST: Currency;
    FitemPedido: TitemPedidoCollection;
    FidPedidoCancelado: string;
    FchNFeRef: string;
    FdhEntrega: TDateTime;
    FnDoc: string;
    FxNome: string;
    FlatGPS: Double;
    FlongGPS: Double;
    FhashComprovante: string;
    FdhHashComprovante: TDateTime;
    FnProtEvento: string;
    FautXML: TautXMLCollection;
    FtpAutorizacao: TAutorizacao;
    // Insucesso na Entrega
    FdhTentativaEntrega: TDateTime;
    FnTentativa: Integer;
    FtpMotivo: TtpMotivo;
    FxJustMotivo: string;
    FhashTentativaEntrega: string;
    FdhHashTentativaEntrega: TDateTime;
    FUF: string;
    FdetPag: TdetPagCollection;

    procedure setxCondUso(const Value: string);
    procedure SetitemPedido(const Value: TitemPedidoCollection);
    procedure SetautXML(const Value: TautXMLCollection);
    procedure SetdetPag(const Value: TdetPagCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property versao: string         read FVersao      write FVersao;
    property descEvento: string     read FDescEvento  write FDescEvento;
    property xCorrecao: string      read FCorrecao    write FCorrecao;
    property xCondUso: string       read FCondUso     write setxCondUso;
    property nProt: string          read FnProt       write FnProt;
    property xJust: string          read FxJust       write FxJust;
    property cOrgaoAutor: Integer   read FcOrgaoAutor write FcOrgaoAutor;
    property tpAutor: TpcnTipoAutor read FtpAutor     write FtpAutor;
    property verAplic: string       read FverAplic    write FverAplic;
    property chNFeRef: string       read FchNFeRef    write FchNFeRef;
    property dhEmi: TDateTime       read FdhEmi       write FdhEmi;
    property tpNF: TpcnTipoNFe      read FtpNF        write FtpNF;
    property IE: string             read FIE          write FIE;
    property dest: TDestinatario    read Fdest        write Fdest;
    property vNF: Currency          read FvNF         write FvNF;
    property vICMS: Currency        read FvICMS       write FvICMS;
    property vST: Currency          read FvST         write FvST;

    property itemPedido: TitemPedidoCollection read FitemPedido        write SetitemPedido;
    property idPedidoCancelado: string         read FidPedidoCancelado write FidPedidoCancelado;

    property dhEntrega: TDateTime         read FdhEntrega         write FdhEntrega;
    property nDoc: string                 read FnDoc              write FnDoc;
    property xNome: string                read FxNome             write FxNome;
    property latGPS: Double               read FlatGPS            write FlatGPS;
    property longGPS: Double              read FlongGPS           write FlongGPS;
    property hashComprovante: string      read FhashComprovante   write FhashComprovante;
    property dhHashComprovante: TDateTime read FdhHashComprovante write FdhHashComprovante;
    property nProtEvento: string          read FnProtEvento       write FnProtEvento;

    property autXML: TautXMLCollection    read FautXML            write SetautXML;
    property tpAutorizacao: TAutorizacao  read FtpAutorizacao     write FtpAutorizacao;

    property dhTentativaEntrega: TDateTime read FdhTentativaEntrega write FdhTentativaEntrega;
    property nTentativa: Integer read FnTentativa write FnTentativa;
    property tpMotivo: TtpMotivo read FtpMotivo write FtpMotivo;
    property xJustMotivo: string read FxJustMotivo write FxJustMotivo;
    property hashTentativaEntrega: string read FhashTentativaEntrega write FhashTentativaEntrega;
    property dhHashTentativaEntrega: TDateTime read FdhHashTentativaEntrega write FdhHashTentativaEntrega;
    property UF: string read FUF write FUF;
    property detPag: TdetPagCollection read FdetPag write SetdetPag;
  end;

  TInfEvento = class
  private
    FID: string;
    FtpAmbiente: TpcnTipoAmbiente;
    FCNPJ: string;
    FcOrgao: Integer;
    FChave: string;
    FDataEvento: TDateTime;
    FTpEvento: TpcnTpEvento;
    FnSeqEvento: Integer;
    FVersaoEvento: string;
    FDetEvento: TDetEvento;

    function getcOrgao: Integer;
    function getDescEvento: string;
    function getTipoEvento: string;
  public
    constructor Create;
    destructor Destroy; override;

    function DescricaoTipoEvento(TipoEvento:TpcnTpEvento): string;

    property id: string              read FID            write FID;
    property cOrgao: Integer         read getcOrgao      write FcOrgao;
    property tpAmb: TpcnTipoAmbiente read FtpAmbiente    write FtpAmbiente;
    property CNPJ: string            read FCNPJ          write FCNPJ;
    property chNFe: string           read FChave         write FChave;
    property dhEvento: TDateTime     read FDataEvento    write FDataEvento;
    property tpEvento: TpcnTpEvento  read FTpEvento      write FTpEvento;
    property nSeqEvento: Integer     read FnSeqEvento    write FnSeqEvento;
    property versaoEvento: string    read FVersaoEvento  write FversaoEvento;
    property detEvento: TDetEvento   read FDetEvento     write FDetEvento;
    property DescEvento: string      read getDescEvento;
    property TipoEvento: string      read getTipoEvento;
  end;

  TRetchNFePendCollectionItem = class
  private
    FChavePend: string;
  public
    property ChavePend: string read FChavePend write FChavePend;
  end;

  TRetchNFePendCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetchNFePendCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetchNFePendCollectionItem);
  public
    function Add: TRetchNFePendCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetchNFePendCollectionItem;
    property Items[Index: Integer]: TRetchNFePendCollectionItem read GetItem write SetItem; default;
  end;

  { TRetInfEvento }

  TRetInfEvento = class(TObject)
  private
    FId: string;
    FNomeArquivo: string;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: string;
    FcOrgao: Integer;
    FcStat: Integer;
    FxMotivo: string;
    FchNFe: string;
    FtpEvento: TpcnTpEvento;
    FxEvento: string;
    FnSeqEvento: Integer;
    FCNPJDest: string;
    FemailDest: string;
    FcOrgaoAutor: Integer;
    FdhRegEvento: TDateTime;
    FnProt: string;
    FchNFePend: TRetchNFePendCollection;
    FXML: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;

    property Id: string                         read FId          write FId;
    property tpAmb: TpcnTipoAmbiente            read FtpAmb       write FtpAmb;
    property verAplic: string                   read FverAplic    write FverAplic;
    property cOrgao: Integer                    read FcOrgao      write FcOrgao;
    property cStat: Integer                     read FcStat       write FcStat;
    property xMotivo: string                    read FxMotivo     write FxMotivo;
    property chNFe: string                      read FchNFe       write FchNFe;
    property tpEvento: TpcnTpEvento             read FtpEvento    write FtpEvento;
    property xEvento: string                    read FxEvento     write FxEvento;
    property nSeqEvento: Integer                read FnSeqEvento  write FnSeqEvento;
    property CNPJDest: string                   read FCNPJDest    write FCNPJDest;
    property emailDest: string                  read FemailDest   write FemailDest;
    property cOrgaoAutor: Integer               read FcOrgaoAutor write FcOrgaoAutor;
    property dhRegEvento: TDateTime             read FdhRegEvento write FdhRegEvento;
    property nProt: string                      read FnProt       write FnProt;
    property chNFePend: TRetchNFePendCollection read FchNFePend   write FchNFePend;
    property XML: AnsiString                    read FXML         write FXML;
    property NomeArquivo: string                read FNomeArquivo write FNomeArquivo;
  end;

implementation

{ TInfEvento }

constructor TInfEvento.Create;
begin
  inherited Create;

  FDetEvento := TDetEvento.Create();
end;

destructor TInfEvento.Destroy;
begin
  FDetEvento.Free;

  inherited;
end;

function TInfEvento.getcOrgao: Integer;
//  (AC,AL,AP,AM,BA,CE,DF,ES,GO,MA,MT,MS,MG,PA,PB,PR,PE,PI,RJ,RN,RS,RO,RR,SC,SP,SE,TO);
//  (12,27,16,13,29,23,53,32,52,21,51,50,31,15,25,41,26,22,33,24,43,11,14,42,35,28,17);
begin
  if FcOrgao <> 0 then
    Result := FcOrgao
  else
    Result := StrToIntDef(copy(FChave, 1, 2), 0);
end;

function TInfEvento.getDescEvento: string;
begin
  case fTpEvento of
    teCCe                      : Result := 'Carta de Correcao';
    teCancelamento             : Result := 'Cancelamento';
    teCancSubst                : Result := 'Cancelamento por substituicao';
    teManifDestConfirmacao     : Result := 'Confirmacao da Operacao';
    teManifDestCiencia         : Result := 'Ciencia da Operacao';
    teManifDestDesconhecimento : Result := 'Desconhecimento da Operacao';
    teManifDestOperNaoRealizada: Result := 'Operacao nao Realizada';
    teEPECNFe                  : Result := 'EPEC';
    teEPEC                     : Result := 'EPEC';
    teMultiModal               : Result := 'Registro Multimodal';
    teRegistroPassagem         : Result := 'Registro de Passagem';
    teRegistroPassagemNFe      : Result := 'Registro de Passagem NF-e';
    teRegistroPassagemBRId     : Result := 'Registro de Passagem BRId';
    teEncerramento             : Result := 'Encerramento';
    teInclusaoCondutor         : Result := 'Inclusao Condutor';
    teRegistroCTe              : Result := 'CT-e Autorizado para NF-e';
    teRegistroPassagemNFeCancelado: Result := 'Registro de Passagem para NF-e Cancelado';
    teRegistroPassagemNFeRFID  : Result := 'Registro de Passagem para NF-e RFID';
    teCTeAutorizado            : Result := 'CT-e Autorizado';
    teCTeCancelado             : Result := 'CT-e Cancelado';
    teMDFeAutorizado,
    teMDFeAutorizado2          : Result := 'MDF-e Autorizado';
    teMDFeCancelado,
    teMDFeCancelado2           : Result := 'MDF-e Cancelado';
    teVistoriaSuframa          : Result := 'Vistoria SUFRAMA';
    tePedProrrog1,
    tePedProrrog2              : Result := 'Pedido de Prorrogacao';
    teCanPedProrrog1,
    teCanPedProrrog2           : Result := 'Cancelamento de Pedido de Prorrogacao';
    teEventoFiscoPP1,
    teEventoFiscoPP2,
    teEventoFiscoCPP1,
    teEventoFiscoCPP2          : Result := 'Evento Fisco';
    teConfInternalizacao       : Result := 'Confirmacao de Internalizacao da Mercadoria na SUFRAMA';
    teComprEntrega             : Result := 'Comprovante de Entrega do CT-e';
    teComprEntregaNFe          : Result := 'Comprovante de Entrega da NF-e';
    teCancComprEntregaNFe      : Result := 'Cancelamento Comprovante de Entrega da NF-e';
    teAtorInteressadoNFe       : Result := 'Ator interessado na NF-e';
    teInsucessoEntregaNFe      : Result := 'Insucesso na Entrega da NF-e';
    teCancInsucessoEntregaNFe  : Result := 'Cancelamento Insucesso na Entrega da NF-e';
    teConcFinanceira           : Result := 'ECONF';
    teCancConcFinanceira       : Result := 'Cancelamento Conciliação Financeira';
  else
    Result := '';
  end;
end;

function TInfEvento.getTipoEvento: string;
begin
  try
    Result := TpEventoToStr( FTpEvento );
  except
    Result := '';
  end;
end;

function TInfEvento.DescricaoTipoEvento(TipoEvento: TpcnTpEvento): string;
begin
  case TipoEvento of
    teCCe                      : Result := 'CARTA DE CORREÇÃO ELETRÔNICA';
    teCancelamento             : Result := 'CANCELAMENTO DE NF-e';
    teCancSubst                : Result := 'Cancelamento por substituicao';
    teManifDestConfirmacao     : Result := 'CONFIRMAÇÃO DA OPERAÇÃO';
    teManifDestCiencia         : Result := 'CIÊNCIA DA OPERAÇÃO';
    teManifDestDesconhecimento : Result := 'DESCONHECIMENTO DA OPERAÇÃO';
    teManifDestOperNaoRealizada: Result := 'OPERAÇÃO NÃO REALIZADA';
    teEPECNFe                  : Result := 'EPEC';
    teEPEC                     : Result := 'EPEC';
    teMultiModal               : Result := 'REGISTRO MULTIMODAL';
    teRegistroPassagem         : Result := 'REGISTRO DE PASSAGEM';
    teRegistroPassagemNFe      : Result := 'REGISTRO DE PASSAGEM NF-e';
    teRegistroPassagemBRId     : Result := 'REGISTRO DE PASSAGEM BRId';
    teEncerramento             : Result := 'ENCERRAMENTO';
    teInclusaoCondutor         : Result := 'INCLUSAO CONDUTOR';
    teRegistroCTe              : Result := 'CT-e Autorizado para NF-e';
    teRegistroPassagemNFeCancelado: Result := 'Registro de Passagem para NF-e Cancelado';
    teRegistroPassagemNFeRFID  : Result := 'Registro de Passagem para NF-e RFID';
    teCTeAutorizado            : Result := 'CT-e Autorizado';
    teCTeCancelado             : Result := 'CT-e Cancelado';
    teMDFeAutorizado,
    teMDFeAutorizado2          : Result := 'MDF-e Autorizado';
    teMDFeCancelado,
    teMDFeCancelado2           : Result := 'MDF-e Cancelado';
    teVistoriaSuframa          : Result := 'Vistoria SUFRAMA';
    tePedProrrog1,
    tePedProrrog2              : Result := 'Pedido de Prorrogacao';
    teCanPedProrrog1,
    teCanPedProrrog2           : Result := 'Cancelamento de Pedido de Prorrogacao';
    teEventoFiscoPP1,
    teEventoFiscoPP2,
    teEventoFiscoCPP1,
    teEventoFiscoCPP2          : Result := 'Evento Fisco';
    teConfInternalizacao       : Result := 'Confirmacao de Internalizacao da Mercadoria na SUFRAMA';
    teComprEntrega             : Result := 'Comprovante de Entrega do CT-e';
    teComprEntregaNFe          : Result := 'Comprovante de Entrega da NF-e';
    teCancComprEntregaNFe      : Result := 'Cancelamento Comprovante de Entrega da NF-e';
    teAtorInteressadoNFe       : Result := 'Ator interessado na NF-e';
    teInsucessoEntregaNFe      : Result := 'Insucesso na Entrega da NF-e';
    teCancInsucessoEntregaNFe  : Result := 'Cancelamento Insucesso na Entrega da NF-e';
    teConcFinanceira           : Result := 'ECONF';
    teCancConcFinanceira       : Result := 'Cancelamento Conciliação Financeira';
  else
    Result := 'Não Definido';
  end;
end;

{ TDetEvento }

constructor TDetEvento.Create();
begin
  inherited Create;

  Fdest := TDestinatario.Create;
  FitemPedido := TitemPedidoCollection.Create;
  FautXML := TautXMLCollection.Create;
  FdetPag := TdetPagCollection.Create;
end;

destructor TDetEvento.Destroy;
begin
  Fdest.Free;
  FitemPedido.Free;
  FautXML.Free;
  FdetPag.Free;

  inherited;
end;

procedure TDetEvento.setxCondUso(const Value: string);
begin
  FCondUso := Value;

  if FCondUso = '' then
    FCondUso := 'A Carta de Correcao e disciplinada pelo paragrafo 1o-A do' +
                ' art. 7o do Convenio S/N, de 15 de dezembro de 1970 e' +
                ' pode ser utilizada para regularizacao de erro ocorrido na' +
                ' emissao de documento fiscal, desde que o erro nao esteja' +
                ' relacionado com: I - as variaveis que determinam o valor' +
                ' do imposto tais como: base de calculo, aliquota, diferenca' +
                ' de preco, quantidade, valor da operacao ou da prestacao;' +
                ' II - a correcao de dados cadastrais que implique mudanca' +
                ' do remetente ou do destinatario; III - a data de emissao ou' +
                ' de saida.'
end;

procedure TDetEvento.SetautXML(const Value: TautXMLCollection);
begin
  FautXML := Value;
end;

procedure TDetEvento.SetdetPag(const Value: TdetPagCollection);
begin
  FdetPag := Value;
end;

procedure TDetEvento.SetitemPedido(const Value: TitemPedidoCollection);
begin
  FitemPedido := Value;
end;

{ TRetchNFePendCollection }

function TRetchNFePendCollection.Add: TRetchNFePendCollectionItem;
begin
  Result := Self.New;
end;

function TRetchNFePendCollection.GetItem(
  Index: Integer): TRetchNFePendCollectionItem;
begin
  Result := TRetchNFePendCollectionItem(inherited Items[Index]);
end;

procedure TRetchNFePendCollection.SetItem(Index: Integer;
  Value: TRetchNFePendCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRetchNFePendCollection.New: TRetchNFePendCollectionItem;
begin
  Result := TRetchNFePendCollectionItem.Create;
  Self.Add(Result);
end;

{ TRetInfEvento }

constructor TRetInfEvento.Create;
begin
  inherited Create;

  FchNFePend := TRetchNFePendCollection.Create();
end;

destructor TRetInfEvento.Destroy;
begin
  FchNFePend.Free;

  inherited;
end;

{ TitemPedidoCollection }

function TitemPedidoCollection.Add: TitemPedidoCollectionItem;
begin
  Result := Self.New;
end;

function TitemPedidoCollection.GetItem(
  Index: Integer): TitemPedidoCollectionItem;
begin
  Result := TitemPedidoCollectionItem(inherited Items[Index]);
end;

procedure TitemPedidoCollection.SetItem(Index: Integer;
  Value: TitemPedidoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TitemPedidoCollection.New: TitemPedidoCollectionItem;
begin
  Result := TitemPedidoCollectionItem.Create;
  Self.Add(Result);
end;

{ TautXMLCollectionItem }

procedure TautXMLCollectionItem.Assign(Source: TautXMLCollectionItem);
begin
  CNPJCPF := Source.CNPJCPF;
end;

{ TautXMLCollection }

function TautXMLCollection.Add: TautXMLCollectionItem;
begin
  Result := Self.New;
end;

function TautXMLCollection.GetItem(Index: Integer): TautXMLCollectionItem;
begin
  Result := TautXMLCollectionItem(inherited Items[Index]);
end;

function TautXMLCollection.New: TautXMLCollectionItem;
begin
  Result := TautXMLCollectionItem.Create;
  Self.Add(Result);
end;

procedure TautXMLCollection.SetItem(Index: Integer;
  Value: TautXMLCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TdetPagCollection }

function TdetPagCollection.Add: TdetPagCollectionItem;
begin
  Result := Self.New;
end;

function TdetPagCollection.GetItem(Index: Integer): TdetPagCollectionItem;
begin
  Result := TdetPagCollectionItem(inherited Items[Index]);
end;

function TdetPagCollection.New: TdetPagCollectionItem;
begin
  Result := TdetPagCollectionItem.Create;
  Self.Add(Result);
end;

procedure TdetPagCollection.SetItem(Index: Integer;
  Value: TdetPagCollectionItem);
begin
  inherited Items[Index] := Value;
end;

end.
