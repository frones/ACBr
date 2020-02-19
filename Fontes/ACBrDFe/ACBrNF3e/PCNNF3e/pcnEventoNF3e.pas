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

{$I ACBr.inc}

unit pcnEventoNF3e;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pcnConversaoNF3e;

type
  EventoException = class(Exception);

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
    function New: TitemPedidoCollectionItem;
    property Items[Index: Integer]: TitemPedidoCollectionItem read GetItem write SetItem; default;
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
    FchNF3eRef: String;

    procedure setxCondUso(const Value: String);
    procedure SetitemPedido(const Value: TitemPedidoCollection);
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
    property chNF3eRef: String       read FchNF3eRef    write FchNF3eRef;
    property dhEmi: TDateTime       read FdhEmi       write FdhEmi;
    property tpNF: TpcnTipoNFe      read FtpNF        write FtpNF;
    property IE: String             read FIE          write FIE;
    property dest: TDestinatario    read Fdest        write Fdest;
    property vNF: Currency          read FvNF         write FvNF;
    property vICMS: Currency        read FvICMS       write FvICMS;
    property vST: Currency          read FvST         write FvST;
    property itemPedido: TitemPedidoCollection read FitemPedido write SetitemPedido;
    property idPedidoCancelado: String read FidPedidoCancelado write FidPedidoCancelado;
  end;

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
    property chNF3e: String           read FChave          write FChave;
    property dhEvento: TDateTime     read FDataEvento     write FDataEvento;
    property tpEvento: TpcnTpEvento  read FTpEvento       write FTpEvento;
    property nSeqEvento: Integer     read FnSeqEvento     write FnSeqEvento;
//    property versaoEvento: String    read getVersaoEvento write FversaoEvento;
    property versaoEvento: String    read FVersaoEvento   write FversaoEvento;
    property detEvento: TDetEvento   read FDetEvento      write FDetEvento;
    property DescEvento: String      read getDescEvento;
    property TipoEvento: String      read getTipoEvento;
  end;

  TRetchNF3ePendCollectionItem = class
  private
    FChavePend: String;
  public
    property ChavePend: String read FChavePend write FChavePend;
  end;

  TRetchNF3ePendCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetchNF3ePendCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetchNF3ePendCollectionItem);
  public
    function New: TRetchNF3ePendCollectionItem;
    property Items[Index: Integer]: TRetchNF3ePendCollectionItem read GetItem write SetItem; default;
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
    FchNF3e: String;
    FtpEvento: TpcnTpEvento;
    FxEvento: String;
    FnSeqEvento: Integer;
    FCNPJDest: String;
    FemailDest: String;
    FcOrgaoAutor: Integer;
    FdhRegEvento: TDateTime;
    FnProt: String;
    FchNF3ePend: TRetchNF3ePendCollection;
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
    property chNF3e: String                     read FchNF3e      write FchNF3e;
    property tpEvento: TpcnTpEvento             read FtpEvento    write FtpEvento;
    property xEvento: String                    read FxEvento     write FxEvento;
    property nSeqEvento: Integer                read FnSeqEvento  write FnSeqEvento;
    property CNPJDest: String                   read FCNPJDest    write FCNPJDest;
    property emailDest: String                  read FemailDest   write FemailDest;
    property cOrgaoAutor: Integer               read FcOrgaoAutor write FcOrgaoAutor;
    property dhRegEvento: TDateTime             read FdhRegEvento write FdhRegEvento;
    property nProt: String                      read FnProt       write FnProt;
    property chNF3ePend: TRetchNF3ePendCollection read FchNF3ePend   write FchNF3ePend;
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
    teRegistroPassagemNFeRFID     : Result := 'Registro de Passagem para NF-e RFID';
    teCTeAutorizado               : Result := 'CT-e Autorizado';
    teCTeCancelado                : Result := 'CT-e Cancelado';
    teMDFeAutorizado,
    teMDFeAutorizado2             : Result := 'MDF-e Autorizado';
    teMDFeCancelado,
    teMDFeCancelado2              : Result := 'MDF-e Cancelado';
    teVistoriaSuframa             : Result := 'Vistoria SUFRAMA';
    tePedProrrog1,
    tePedProrrog2              : Result := 'Pedido de Prorrogacao';
    teCanPedProrrog1,
    teCanPedProrrog2           : Result := 'Cancelamento de Pedido de Prorrogacao';
    teEventoFiscoPP1,
    teEventoFiscoPP2,
    teEventoFiscoCPP1,
    teEventoFiscoCPP2          : Result := 'Evento Fisco';
    teConfInternalizacao       : Result := 'Confirmacao de Internalizacao da Mercadoria na SUFRAMA';
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
    teRegistroPassagemNFeRFID     : Result := 'Registro de Passagem para NF-e RFID';
    teCTeAutorizado               : Result := 'CT-e Autorizado';
    teCTeCancelado                : Result := 'CT-e Cancelado';
    teMDFeAutorizado,
    teMDFeAutorizado2             : Result := 'MDF-e Autorizado';
    teMDFeCancelado,
    teMDFeCancelado2              : Result := 'MDF-e Cancelado';
    teVistoriaSuframa             : Result := 'Vistoria SUFRAMA';
    tePedProrrog1,
    tePedProrrog2              : Result := 'Pedido de Prorrogacao';
    teCanPedProrrog1,
    teCanPedProrrog2           : Result := 'Cancelamento de Pedido de Prorrogacao';
    teEventoFiscoPP1,
    teEventoFiscoPP2,
    teEventoFiscoCPP1,
    teEventoFiscoCPP2          : Result := 'Evento Fisco';
    teConfInternalizacao       : Result := 'Confirmacao de Internalizacao da Mercadoria na SUFRAMA';
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
end;

destructor TDetEvento.Destroy;
begin
  Fdest.Free;
  FitemPedido.Free;
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

procedure TDetEvento.SetitemPedido(const Value: TitemPedidoCollection);
begin
  FitemPedido := Value;
end;

{ TRetchNF3ePendCollection }

function TRetchNF3ePendCollection.GetItem(
  Index: Integer): TRetchNF3ePendCollectionItem;
begin
  Result := TRetchNF3ePendCollectionItem(inherited Items[Index]);
end;

procedure TRetchNF3ePendCollection.SetItem(Index: Integer;
  Value: TRetchNF3ePendCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRetchNF3ePendCollection.New: TRetchNF3ePendCollectionItem;
begin
  Result := TRetchNF3ePendCollectionItem.Create;
  Self.Add(Result);
end;

{ TRetInfEvento }

constructor TRetInfEvento.Create;
begin
  inherited Create;
  FchNF3ePend := TRetchNF3ePendCollection.Create();
end;

destructor TRetInfEvento.Destroy;
begin
  FchNF3ePend.Free;
  inherited;
end;

{ TitemPedidoCollection }

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

end.
