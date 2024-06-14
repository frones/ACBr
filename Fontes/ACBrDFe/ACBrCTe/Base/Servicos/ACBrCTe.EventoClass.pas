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

unit ACBrCTe.EventoClass;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  pcnConversao, pcteConversaoCTe,
  ACBrBase;

type
  EventoException = class(Exception);

  TInfCorrecaoCollectionItem = class(TObject)
  private
    FgrupoAlterado: string;
    FcampoAlterado: string;
    FvalorAlterado: string;
    FnroItemAlterado: Integer;
  public
    property grupoAlterado: string    read FgrupoAlterado   write FgrupoAlterado;
    property campoAlterado: string    read FcampoAlterado   write FcampoAlterado;
    property valorAlterado: string    read FvalorAlterado   write FvalorAlterado;
    property nroItemAlterado: Integer read FnroItemAlterado write FnroItemAlterado;
  end;

  TInfCorrecaoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfCorrecaoCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfCorrecaoCollectionItem);
  public
    function Add: TInfCorrecaoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfCorrecaoCollectionItem;
    property Items[Index: Integer]: TInfCorrecaoCollectionItem read GetItem write SetItem; default;
  end;

  TInfEspecieCollectionItem = class(TObject)
  private
    FtpEspecie: TEspecie;
    FvEspecie: Currency;
  public
    property tpEspecie: TEspecie read FtpEspecie write FtpEspecie;
    property vEspecie: Currency  read FvEspecie  write FvEspecie;
  end;

  TInfEspecieCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfEspecieCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfEspecieCollectionItem);
  public
    function Add: TInfEspecieCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfEspecieCollectionItem;
    property Items[Index: Integer]: TInfEspecieCollectionItem read GetItem write SetItem; default;
  end;

  TInfRemDest = class(TObject)
  private
    FCNPJCPF: string;
    FIE: string;
    FUF: string;
    FxNome: string;
  public
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property IE: string      read FIE      write FIE;
    property UF: string      read FUF      write FUF;
    property xNome: string   read FxNome   write FxNome;
  end;

  TInfGTVCollectionItem = class(TObject)
  private
    FnDoc: string;
    Fid: string;
    Fserie: string;
    Fsubserie: string;
    FdEmi: TDateTime;
    FnDV: Integer;
    FqCarga: Currency;
    FinfEspecie: TInfEspecieCollection;
    Frem: TInfRemDest;
    Fdest: TInfRemDest;
    Fplaca: string;
    FUF: string;
    FRNTRC: string;

  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property nDoc: string     read FnDoc     write FnDoc;
    property id: string       read Fid       write Fid;
    property serie: string    read Fserie    write Fserie;
    property subserie: string read Fsubserie write Fsubserie;
    property dEmi: TDateTime  read FdEmi     write FdEmi;
    property nDV: Integer     read FnDV      write FnDV;
    property qCarga: Currency read FqCarga   write FqCarga;
    property infEspecie: TInfEspecieCollection read FinfEspecie write FinfEspecie;
    property rem: TInfRemDest  read Frem   write Frem;
    property dest: TInfRemDest read Fdest  write Fdest;
    property placa: string     read Fplaca write Fplaca;
    property UF: string        read FUF    write FUF;
    property RNTRC: string     read FRNTRC write FRNTRC;
  end;

  TInfGTVCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfGTVCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfGTVCollectionItem);
  public
    function Add: TInfGTVCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfGTVCollectionItem;
    property Items[Index: Integer]: TInfGTVCollectionItem read GetItem write SetItem; default;
  end;

  TInfEntregaCollectionItem = class(TObject)
  private
    FchNFe: string;
  public
    property chNFe: string read FchNFe write FchNFe;
  end;

  TInfEntregaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfEntregaCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfEntregaCollectionItem);
  public
    function Add: TInfEntregaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfEntregaCollectionItem;
    property Items[Index: Integer]: TInfEntregaCollectionItem read GetItem write SetItem; default;
  end;

  TDetEvento = class(TObject)
  private
    FdescEvento: string;
    FnProt: string;

    FxJust: string;   // Cancelamento
    FxOBS: string;    // Cancelamento

    FvICMS: Currency;  // EPEC
    FvICMSST: Currency;
    FvTPrest: Currency;
    FvCarga: Currency;
    Ftoma: TpcteTomador;
    FUF: string;
    FCNPJCPF: string;
    FIE: string;
    Fmodal: TpcteModal;
    FUFIni: string;
    FUFFim: string;
    FtpCTe: TpcteTipoCTe;
    FdhEmi: TDateTime;

    FxRegistro: string; // MultiModal
    FnDoc: string;

    FinfCorrecao: TInfCorrecaoCollection;
    FCondUso: string;
     // GTV
    FinfGTV: TInfGTVCollection;
     // Comprovante de Entrega
    FdhEntrega: TDateTime;
    FxNome: string;
    Flatitude: Double;
    Flongitude: Double;
    FhashEntrega: string;
    FdhHashEntrega: TDateTime;
    FinfEntrega: TInfEntregaCollection;
     // Cancelamento do Comprovante de Entrega
    FnProtCE: string;
    // Insucesso na Entrega
    FdhTentativaEntrega: TDateTime;
    FnTentativa: Integer;
    FtpMotivo: TtpMotivo;
    FxJustMotivo: string;
    FhashTentativaEntrega: string;
    FdhHashTentativaEntrega: TDateTime;
    // Cancelamento do Insucesso na Entrega
    FnProtIE: string;

    procedure SetinfCorrecao(const Value: TInfCorrecaoCollection);
    procedure SetxCondUso(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property descEvento: string read FdescEvento write FdescEvento;
    property nProt: string      read FnProt      write FnProt;
    property xJust: string      read FxJust      write FxJust;
    property xOBS: string       read FxOBS       write FxOBS;
    property vICMS: Currency    read FvICMS      write FvICMS;
    property vICMSST: Currency  read FvICMSST    write FvICMSST;
    property vTPrest: Currency  read FvTPrest    write FvTPrest;
    property vCarga: Currency   read FvCarga     write FvCarga;
    property toma: TpcteTomador read Ftoma       write Ftoma;
    property UF: string         read FUF         write FUF;
    property CNPJCPF: string    read FCNPJCPF    write FCNPJCPF;
    property IE: string         read FIE         write FIE;
    property modal: TpcteModal  read Fmodal      write Fmodal;
    property UFIni: string      read FUFIni      write FUFIni;
    property UFFim: string      read FUFFim      write FUFFim;
    property tpCTe: TpcteTipoCTe read FtpCTe      write FtpCTe;
    property dhEmi: TDateTime   read FdhEmi      write FdhEmi;
    property xRegistro: string  read FxRegistro  write FxRegistro;
    property nDoc: string       read FnDoc       write FnDoc;

    property infCorrecao: TInfCorrecaoCollection read FinfCorrecao write SetinfCorrecao;
    property xCondUso: string                    read FCondUso     write SetxCondUso;

    property dhEntrega: TDateTime     read FdhEntrega     write FdhEntrega;
    property xNome: string            read FxNome         write FxNome;
    property latitude: Double         read Flatitude      write Flatitude;
    property longitude: Double        read Flongitude     write Flongitude;
    property hashEntrega: string      read FhashEntrega   write FhashEntrega;
    property dhHashEntrega: TDateTime read FdhHashEntrega write FdhHashEntrega;

    property infGTV: TInfGTVCollection         read FinfGTV     write FinfGTV;
    property infEntrega: TInfEntregaCollection read FinfEntrega write FinfEntrega;

    property nProtCE: string read FnProtCE write FnProtCE;

    property dhTentativaEntrega: TDateTime read FdhTentativaEntrega write FdhTentativaEntrega;
    property nTentativa: Integer read FnTentativa write FnTentativa;
    property tpMotivo: TtpMotivo read FtpMotivo write FtpMotivo;
    property xJustMotivo: string read FxJustMotivo write FxJustMotivo;
    property hashTentativaEntrega: string read FhashTentativaEntrega write FhashTentativaEntrega;
    property dhHashTentativaEntrega: TDateTime read FdhHashTentativaEntrega write FdhHashTentativaEntrega;

    property nProtIE: string read FnProtIE write FnProtIE;
  end;

  TInfEvento = class
  private
    FId: string;
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

    property Id: string              read FId           write FId;
    property cOrgao: Integer         read getcOrgao     write FcOrgao;
    property tpAmb: TpcnTipoAmbiente read FtpAmbiente   write FtpAmbiente;
    property CNPJ: string            read FCNPJ         write FCNPJ;
    property chCTe: string           read FChave        write FChave;
    property dhEvento: TDateTime     read FDataEvento   write FDataEvento;
    property tpEvento: TpcnTpEvento  read FTpEvento     write FTpEvento;
    property nSeqEvento: Integer     read FnSeqEvento   write FnSeqEvento;
    property versaoEvento: string    read FVersaoEvento write FversaoEvento;
    property detEvento: TDetEvento   read FDetEvento    write FDetEvento;

    property DescEvento: string read getDescEvento;
    property TipoEvento: string read getTipoEvento;
  end;

  { TRetInfEvento }

  TRetInfEvento = class
  private
    FId: string;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: string;
    FcOrgao: Integer;
    FcStat: Integer;
    FxMotivo: string;
    FchCTe: string;
    FtpEvento: TpcnTpEvento;
    FxEvento: string;
    FnSeqEvento: Integer;
    FCNPJDest: string;
    FemailDest: string;
    FdhRegEvento: TDateTime;
    FnProt: string;
    FXML: string;
    FNomeArquivo: string;
  public
    property Id: string              read FId          write FId;
    property tpAmb: TpcnTipoAmbiente read FtpAmb       write FtpAmb;
    property verAplic: string        read FverAplic    write FverAplic;
    property cOrgao: Integer         read FcOrgao      write FcOrgao;
    property cStat: Integer          read FcStat       write FcStat;
    property xMotivo: string         read FxMotivo     write FxMotivo;
    property chCTe: string           read FchCTe       write FchCTe;
    property tpEvento: TpcnTpEvento  read FtpEvento    write FtpEvento;
    property xEvento: string         read FxEvento     write FxEvento;
    property nSeqEvento: Integer     read FnSeqEvento  write FnSeqEvento;
    property CNPJDest: string        read FCNPJDest    write FCNPJDest;
    property emailDest: string       read FemailDest   write FemailDest;
    property dhRegEvento: TDateTime  read FdhRegEvento write FdhRegEvento;
    property nProt: string           read FnProt       write FnProt;
    property XML: string             read FXML         write FXML;
    property NomeArquivo: string     read FNomeArquivo write FNomeArquivo;
  end;


implementation

{ TInfEvento }

constructor TInfEvento.Create;
begin
  inherited Create;

  FDetEvento  := TDetEvento.Create;
  FnSeqEvento := 0;
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
//  Result := 0;

{  if FTpEvento = teEPEC then
  begin
    case StrToIntDef(copy(FChave, 1, 2), 0) of
      0,
     11, // Rondônia
     12, // Acre
     13, // Amazonas
     14, // Roraima
     15, // Pará
     16, // Amapá
     17, // Tocantins
     21, // Maranhão
     22, // Piauí
     23, // Ceará
     24, // Rio Grande do Norte
     25, // Paraibá
     27, // Alagoas
     28, // Sergipe
     29, // Bahia
     31, // Minas Gerais
     32, // Espirito Santo
     33, // Rio de Janeiro
     41, // Paraná
     42, // Santa Catarina
     43, // Rio Grande do Sul
     52, // Goiás
     53: // Distrito Federal
        Result := 35;
     26, // Pernanbuco
     35, // São Paulo
     50, // Mato Grosso do Sul
     51: // Mato Grosso
        Result := 43;
    end;
  end
  else begin }
   if FcOrgao <> 0 then
     Result := FcOrgao
   else
     Result := StrToIntDef(copy(FChave, 1, 2), 0);
//  end;
end;

function TInfEvento.getDescEvento: string;
begin
  case FTpEvento of
    teCCe                         : Result := 'Carta de Correcao';
    teCancelamento                : Result := 'Cancelamento';
    teManifDestConfirmacao        : Result := 'Confirmacao da Operacao';
    teManifDestCiencia            : Result := 'Ciencia da Operacao';
    teManifDestDesconhecimento    : Result := 'Resultonhecimento da Operacao';
    teManifDestOperNaoRealizada   : Result := 'Operacao nao Realizada';
    teEPECNFe                     : Result := 'EPEC';
    teEPEC                        : Result := 'EPEC';
    teMultiModal                  : Result := 'Registro Multimodal';
    teRegistroPassagem            : Result := 'Registro de Passagem';
    teRegistroPassagemBRId        : Result := 'Registro de Passagem BRId';
    teEncerramento                : Result := 'Encerramento';
    teInclusaoCondutor            : Result := 'Inclusao Condutor';
    teRegistroCTe                 : Result := 'CT-e Autorizado para NF-e';
    teRegistroPassagemNFeCancelado: Result := 'Registro de Passagem para NF-e Cancelado';
    teRegistroPassagemNFeRFID     : Result := 'Registro de Passagem para NF-e RFID';
    teCTeAutorizado               : Result := 'CT-e Autorizado';
    teCTeCancelado                : Result := 'CT-e Cancelado';
    teMDFeAutorizado,
    teMDFeAutorizado2             : Result := 'MDF-e Autorizado';
    teMDFeCancelado,
    teMDFeCancelado2              : Result := 'MDF-e Cancelado';
    teVistoriaSuframa             : Result := 'Vistoria SUFRAMA';
    teConfInternalizacao          : Result := 'Confirmacao de Internalizacao da Mercadoria na SUFRAMA';
    tePrestDesacordo              : Result := 'Prestacao do Servico em Desacordo';
    teGTV                         : Result := 'Informacoes da GTV';
    teAutCteComplementar          : Result := 'Autorizado CTe Complemetnar';
    teCancCteComplementar         : Result := 'Cancelado CTe Complementar';
    teCTeSubstituicao             : Result := 'CTe de Substituicao';
    teCTeAnulacao                 : Result := 'CTe de Anulacao';
    teLiberacaoEPEC               : Result := 'Liberacao de EPEC';
    teLiberacaoPrazoCanc          : Result := 'Liberacao Prazo Cancelamento';
    teAutorizadoRedespacho        : Result := 'Autorizado Redespacho';
    teautorizadoRedespIntermed    : Result := 'Autorizado Redespacho Intermediario';
    teAutorizadoSubcontratacao    : Result := 'Autorizado Subcontratacao';
    teautorizadoServMultimodal    : Result := 'Autorizado Servico Vinculado Multimodal';
    teComprEntrega                : Result := 'Comprovante de Entrega do CT-e';
    teCancComprEntrega            : Result := 'Cancelamento do Comprovante de Entrega do CT-e';
    teCancPrestDesacordo          : Result := 'Cancelamento Prestacao do Servico em Desacordo';
    teInsucessoEntregaCTe         : Result := 'Insucesso na Entrega do CT-e';
    teCancInsucessoEntregaCTe     : Result := 'Cancelamento do Insucesso de Entrega do CT-e';
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
    teCancelamento             : Result := 'CANCELAMENTO DO CT-e';
    teManifDestConfirmacao     : Result := 'CONFIRMAÇÃO DA OPERAÇÃO';
    teManifDestCiencia         : Result := 'CIÊNCIA DA OPERAÇÃO';
    teManifDestDesconhecimento : Result := 'DESCONHECIMENTO DA OPERAÇÃO';
    teManifDestOperNaoRealizada: Result := 'OPERAÇÃO NÃO REALIZADA';
    teEPECNFe                  : Result := 'EPEC';
    teEPEC                     : Result := 'EPEC';
    teMultiModal               : Result := 'REGISTRO MULTIMODAL';
    teRegistroPassagem         : Result := 'REGISTRO DE PASSAGEM';
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
    teConfInternalizacao       : Result := 'Confirmacao de Internalizacao da Mercadoria na SUFRAMA';
    tePrestDesacordo           : Result := 'Prestação do Serviço em Desacordo';
    teGTV                      : Result := 'Informações da GTV';
    teAutCteComplementar       : Result := 'Autorizado CTe Complementar';
    teCancCteComplementar      : Result := 'Cancelado CTe Complementar';
    teCTeSubstituicao          : Result := 'CTe de Substituicao';
    teCTeAnulacao              : Result := 'CTe de Anulacao';
    teLiberacaoEPEC            : Result := 'Liberacao de EPEC';
    teLiberacaoPrazoCanc       : Result := 'Liberacao Prazo Cancelamento';
    teAutorizadoRedespacho     : Result := 'Autorizado Redespacho';
    teautorizadoRedespIntermed : Result := 'Autorizado Redespacho Intermediario';
    teAutorizadoSubcontratacao : Result := 'Autorizado Subcontratacao';
    teautorizadoServMultimodal : Result := 'Autorizado Servico Vinculado Multimodal';
    teComprEntrega             : Result := 'Comprovante de Entrega do CT-e';
    teCancComprEntrega         : Result := 'Cancelamento do Comprovante de Entrega do CT-e';
    teCancPrestDesacordo       : Result := 'Cancelamento Prestacao do Servico em Desacordo';
    teInsucessoEntregaCTe      : Result := 'Insucesso na Entrega do CT-e';
    teCancInsucessoEntregaCTe  : Result := 'Cancelamento do Insucesso de Entrega do CT-e';
  else
    Result := 'Não Definido';
  end;
end;

{ TInfCorrecaoCollection }

function TInfCorrecaoCollection.Add: TInfCorrecaoCollectionItem;
begin
  Result := Self.New;
end;

function TInfCorrecaoCollection.GetItem(
  Index: Integer): TInfCorrecaoCollectionItem;
begin
  Result := TInfCorrecaoCollectionItem(inherited Items[Index]);
end;

procedure TInfCorrecaoCollection.SetItem(Index: Integer;
  Value: TInfCorrecaoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfCorrecaoCollection.New: TInfCorrecaoCollectionItem;
begin
  Result := TInfCorrecaoCollectionItem.Create;
  Self.Add(Result);
end;

{ TDetEvento }

constructor TDetEvento.Create;
begin
  inherited Create;

  FinfCorrecao := TInfCorrecaoCollection.Create;
  FinfGTV      := TInfGTVCollection.Create;
  FinfEntrega  := TInfEntregaCollection.Create;
end;

destructor TDetEvento.Destroy;
begin
  FInfCorrecao.Free;
  FinfGTV.Free;
  FinfEntrega.Free;

  inherited;
end;

procedure TDetEvento.SetxCondUso(const Value: string);
begin
  FCondUso := 'A Carta de Correcao e disciplinada pelo Art. 58-B do CONVENIO/' +
              'SINIEF 06/89: Fica permitida a utilizacao de carta de correcao,' +
              ' para regularizacao de erro ocorrido na emissao de documentos ' +
              'fiscais relativos a prestacao de servico de transporte, desde ' +
              'que o erro nao esteja relacionado com: I - as variaveis que ' +
              'determinam o valor do imposto tais como: base de calculo, ' +
              'aliquota, diferenca de preco, quantidade, valor da prestacao;' +
              'II - a correcao de dados cadastrais que implique mudanca do ' +
              'emitente, tomador, remetente ou do destinatario;III - a data ' +
              'de emissao ou de saida.';
end;

procedure TDetEvento.SetinfCorrecao(const Value: TInfCorrecaoCollection);
begin
  FInfCorrecao.Assign(Value);
end;

{ TInfGTVCollection }

function TInfGTVCollection.Add: TInfGTVCollectionItem;
begin
  Result := Self.New;
end;

function TInfGTVCollection.GetItem(Index: Integer): TInfGTVCollectionItem;
begin
  Result := TInfGTVCollectionItem(inherited Items[Index]);
end;

procedure TInfGTVCollection.SetItem(Index: Integer;
  Value: TInfGTVCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfGTVCollection.New: TInfGTVCollectionItem;
begin
  Result := TInfGTVCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfGTVCollectionItem }

constructor TInfGTVCollectionItem.Create;
begin
  inherited Create;
  FinfEspecie := TInfEspecieCollection.Create;
  Frem := TInfRemDest.Create;
  Fdest := TInfRemDest.Create;
end;

destructor TInfGTVCollectionItem.Destroy;
begin
  FinfEspecie.Free;
  Frem.Free;
  Fdest.Free;
  inherited;
end;

{ TInfEspecieCollection }

function TInfEspecieCollection.Add: TInfEspecieCollectionItem;
begin
  Result := Self.New;
end;

function TInfEspecieCollection.GetItem(
  Index: Integer): TInfEspecieCollectionItem;
begin
  Result := TInfEspecieCollectionItem(inherited Items[Index]);
end;

procedure TInfEspecieCollection.SetItem(Index: Integer;
  Value: TInfEspecieCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfEspecieCollection.New: TInfEspecieCollectionItem;
begin
  Result := TInfEspecieCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfEntregaCollection }

function TInfEntregaCollection.Add: TInfEntregaCollectionItem;
begin
  Result := Self.New;
end;

function TInfEntregaCollection.GetItem(Index: Integer): TInfEntregaCollectionItem;
begin
  Result := TInfEntregaCollectionItem(inherited Items[Index]);
end;

function TInfEntregaCollection.New: TInfEntregaCollectionItem;
begin
  Result := TInfEntregaCollectionItem.Create;
  Self.Add(Result);
end;

procedure TInfEntregaCollection.SetItem(Index: Integer; Value: TInfEntregaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

end.
