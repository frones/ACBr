////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org                                       //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcnEventoNFe;

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

  TRetchNFePendCollectionItem = class;
  TDetEvento      = class;

  TInfEvento = class
  private
    FID: String;
    FtpAmbiente: TpcnTipoAmbiente;
    FCNPJ: String;
    FcOrgao: Integer;
    FChave: String;
    FDataEvento: TDateTime;
    FTpEvento: TpcnTpEvento;
    FnSeqEvento: Integer;
    FVersaoEvento: String;
    FDetEvento: TDetEvento;

    function getcOrgao: Integer;
//    function getVersaoEvento: String;
    function getDescEvento: String;
    function getTipoEvento: String;
  public
    constructor Create;
    destructor Destroy; override;
    function DescricaoTipoEvento(TipoEvento:TpcnTpEvento): String;

    property id: String              read FID             write FID;
    property cOrgao: Integer         read getcOrgao       write FcOrgao;
    property tpAmb: TpcnTipoAmbiente read FtpAmbiente     write FtpAmbiente;
    property CNPJ: String            read FCNPJ           write FCNPJ;
    property chNFe: String           read FChave          write FChave;
    property dhEvento: TDateTime     read FDataEvento     write FDataEvento;
    property tpEvento: TpcnTpEvento  read FTpEvento       write FTpEvento;
    property nSeqEvento: Integer     read FnSeqEvento     write FnSeqEvento;
//    property versaoEvento: String    read getVersaoEvento write FversaoEvento;
    property versaoEvento: String    read FVersaoEvento   write FversaoEvento;
    property detEvento: TDetEvento   read FDetEvento      write FDetEvento;
    property DescEvento: String      read getDescEvento;
    property TipoEvento: String      read getTipoEvento;
  end;

  TDestinatario = class(TObject)
  private
    FUF: String;
    FCNPJCPF: String;
    FidEstrangeiro: String;
    FIE: String;
  public
    property UF: String            read FUF            write FUF;
    property CNPJCPF: String       read FCNPJCPF       write FCNPJCPF;
    property idEstrangeiro: String read FidEstrangeiro write FidEstrangeiro;
    property IE: String            read FIE            write FIE;
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
    FCNPJCPF: String;
  public
    procedure Assign(Source: TautXMLCollectionItem);
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
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

  TDetEvento = class
  private
    FVersao: String;
    FDescEvento: String;
    FCorrecao: String;     // Carta de Correção
    FCondUso: String;      // Carta de Correção
    FnProt: String;        // Cancelamento
    FxJust: String;        // Cancelamento e Manif. Destinatario
    FcOrgaoAutor: Integer; // EPEC
    FtpAutor: TpcnTipoAutor;
    FverAplic: String;
    FdhEmi: TDateTime;
    FtpNF: TpcnTipoNFe;
    FIE: String;
    Fdest: TDestinatario;
    FvNF: Currency;
    FvICMS: Currency;
    FvST: Currency;
    FitemPedido: TitemPedidoCollection;
    FidPedidoCancelado: String;
    FchNFeRef: String;

    FdhEntrega: TDateTime;
    FnDoc: String;
    FxNome: String;
    FlatGPS: Double;
    FlongGPS: Double;
    FhashComprovante: String;
    FdhHashComprovante: TDateTime;
    FnProtEvento: String;
    FautXML: TautXMLCollection;
    FtpAutorizacao: TAutorizacao;
    // Insucesso na Entrega
    FdhTentativaEntrega: TDateTime;
    FnTentativa: Integer;
    FtpMotivo: TtpMotivo;
    FxJustMotivo: String;
    FhashTentativaEntrega: String;
    FdhHashTentativaEntrega: TDateTime;
    FUF: String;

    procedure setxCondUso(const Value: String);
    procedure SetitemPedido(const Value: TitemPedidoCollection);
    procedure SetautXML(const Value: TautXMLCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property versao: String         read FVersao      write FVersao;
    property descEvento: String     read FDescEvento  write FDescEvento;
    property xCorrecao: String      read FCorrecao    write FCorrecao;
    property xCondUso: String       read FCondUso     write setxCondUso;
    property nProt: String          read FnProt       write FnProt;
    property xJust: String          read FxJust       write FxJust;
    property cOrgaoAutor: Integer   read FcOrgaoAutor write FcOrgaoAutor;
    property tpAutor: TpcnTipoAutor read FtpAutor     write FtpAutor;
    property verAplic: String       read FverAplic    write FverAplic;
    property chNFeRef: String       read FchNFeRef    write FchNFeRef;
    property dhEmi: TDateTime       read FdhEmi       write FdhEmi;
    property tpNF: TpcnTipoNFe      read FtpNF        write FtpNF;
    property IE: String             read FIE          write FIE;
    property dest: TDestinatario    read Fdest        write Fdest;
    property vNF: Currency          read FvNF         write FvNF;
    property vICMS: Currency        read FvICMS       write FvICMS;
    property vST: Currency          read FvST         write FvST;
    property itemPedido: TitemPedidoCollection read FitemPedido write SetitemPedido;
    property idPedidoCancelado: String read FidPedidoCancelado write FidPedidoCancelado;

    property dhEntrega: TDateTime         read FdhEntrega         write FdhEntrega;
    property nDoc: String                 read FnDoc              write FnDoc;
    property xNome: String                read FxNome             write FxNome;
    property latGPS: Double               read FlatGPS            write FlatGPS;
    property longGPS: Double              read FlongGPS           write FlongGPS;
    property hashComprovante: String      read FhashComprovante   write FhashComprovante;
    property dhHashComprovante: TDateTime read FdhHashComprovante write FdhHashComprovante;
    property nProtEvento: String          read FnProtEvento       write FnProtEvento;

    property autXML: TautXMLCollection    read FautXML            write SetautXML;
    property tpAutorizacao: TAutorizacao  read FtpAutorizacao     write FtpAutorizacao;

    property dhTentativaEntrega: TDateTime read FdhTentativaEntrega write FdhTentativaEntrega;
    property nTentativa: Integer read FnTentativa write FnTentativa;
    property tpMotivo: TtpMotivo read FtpMotivo write FtpMotivo;
    property xJustMotivo: String read FxJustMotivo write FxJustMotivo;
    property hashTentativaEntrega: String read FhashTentativaEntrega write FhashTentativaEntrega;
    property dhHashTentativaEntrega: TDateTime read FdhHashTentativaEntrega write FdhHashTentativaEntrega;
    property UF: String read FUF write FUF;
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

  TRetchNFePendCollectionItem = class
  private
    FChavePend: String;
  public
    property ChavePend: String read FChavePend write FChavePend;
  end;

  { TRetInfEvento }

  TRetInfEvento = class(TObject)
  private
    FId: String;
    FNomeArquivo: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcOrgao: Integer;
    FcStat: Integer;
    FxMotivo: String;
    FchNFe: String;
    FtpEvento: TpcnTpEvento;
    FxEvento: String;
    FnSeqEvento: Integer;
    FCNPJDest: String;
    FemailDest: String;
    FcOrgaoAutor: Integer;
    FdhRegEvento: TDateTime;
    FnProt: String;
    FchNFePend: TRetchNFePendCollection;
    FXML: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;

    property Id: String                         read FId          write FId;
    property tpAmb: TpcnTipoAmbiente            read FtpAmb       write FtpAmb;
    property verAplic: String                   read FverAplic    write FverAplic;
    property cOrgao: Integer                    read FcOrgao      write FcOrgao;
    property cStat: Integer                     read FcStat       write FcStat;
    property xMotivo: String                    read FxMotivo     write FxMotivo;
    property chNFe: String                      read FchNFe       write FchNFe;
    property tpEvento: TpcnTpEvento             read FtpEvento    write FtpEvento;
    property xEvento: String                    read FxEvento     write FxEvento;
    property nSeqEvento: Integer                read FnSeqEvento  write FnSeqEvento;
    property CNPJDest: String                   read FCNPJDest    write FCNPJDest;
    property emailDest: String                  read FemailDest   write FemailDest;
    property cOrgaoAutor: Integer               read FcOrgaoAutor write FcOrgaoAutor;
    property dhRegEvento: TDateTime             read FdhRegEvento write FdhRegEvento;
    property nProt: String                      read FnProt       write FnProt;
    property chNFePend: TRetchNFePendCollection read FchNFePend   write FchNFePend;
    property XML: AnsiString                    read FXML         write FXML;
    property NomeArquivo: String                read FNomeArquivo write FNomeArquivo;
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

function TInfEvento.getDescEvento: String;
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
  else
    Result := '';
  end;
end;

function TInfEvento.getTipoEvento: String;
begin
  try
    Result := TpEventoToStr( FTpEvento );
  except
    Result := '';
  end;
end;

//function TInfEvento.getVersaoEvento: String;
//begin
//  Result := '1.00';
//end;

function TInfEvento.DescricaoTipoEvento(TipoEvento: TpcnTpEvento): String;
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
  FautXML  := TautXMLCollection.Create;
end;

destructor TDetEvento.Destroy;
begin
  Fdest.Free;
  FitemPedido.Free;
  FautXML.Free;

  inherited;
end;

procedure TDetEvento.setxCondUso(const Value: String);
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

end.
