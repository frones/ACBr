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

unit pmdfeEventoMDFe;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, pmdfeMDFe;

type
  TInfEvento = class;
  TDetEvento = class;
  TRetInfEvento = class;
  EventoException = class(Exception);

  TInfEvento = class
  private
    FId: String;
    FtpAmbiente: TpcnTipoAmbiente;
    FCNPJCPF: String;
    FcOrgao: Integer;
    FChave: String;
    FDataEvento: TDateTime;
    FTpEvento: TpcnTpEvento;
    FnSeqEvento: Integer;
    FVersaoEvento: String;
    FDetEvento: TDetEvento;

    function getcOrgao: Integer;
    function getDescEvento: String;
    function getTipoEvento: String;
  public
    constructor Create;
    destructor Destroy; override;
    function DescricaoTipoEvento(TipoEvento:TpcnTpEvento): String;

    property Id: String              read FId             write FId;
    property cOrgao: Integer         read getcOrgao       write FcOrgao;
    property tpAmb: TpcnTipoAmbiente read FtpAmbiente     write FtpAmbiente;
    property CNPJCPF: String         read FCNPJCPF        write FCNPJCPF;
    property chMDFe: String          read FChave          write FChave;
    property dhEvento: TDateTime     read FDataEvento     write FDataEvento;
    property tpEvento: TpcnTpEvento  read FTpEvento       write FTpEvento;
    property nSeqEvento: Integer     read FnSeqEvento     write FnSeqEvento;
    property versaoEvento: String    read FVersaoEvento   write FversaoEvento;
    property detEvento: TDetEvento   read FDetEvento      write FDetEvento;
    property DescEvento: String      read getDescEvento;
    property TipoEvento: String      read getTipoEvento;
  end;

  TInfDocCollectionItem = class(TObject)
  private
    FcMunDescarga: Integer;
    FxMunDescarga: String;
    FchNFe: String;

  public
    property cMunDescarga: Integer read FcMunDescarga write FcMunDescarga;
    property xMunDescarga: String  read FxMunDescarga write FxMunDescarga;
    property chNFe: String         read FchNFe        write FchNFe;
  end;

  TInfDocCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfDocCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfDocCollectionItem);
  public
    function Add: TInfDocCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfDocCollectionItem;
    property Items[Index: Integer]: TInfDocCollectionItem read GetItem write SetItem; default;
  end;

  TInfViagens = class(TObject)
  private
    FqtdViagens: Integer;
    FnroViagem: Integer;

  public
    property qtdViagens: Integer read FqtdViagens write FqtdViagens;
    property nroViagem: Integer read FnroViagem write FnroViagem;
  end;

  TDetEvento = class
  private
    FdescEvento: String;
    FnProt: String;
    FdtEnc: TDateTime; // Encerramento
    FcUF: Integer;     // Encerramento
    FcMun: Integer;    // Encerramento
    FxJust: String;    // Cancelamento
    FxNome: String;    // Inclusao de Condutor
    FCPF: String;      // Inclusao de Condutor
    // Inclusao de DF-e
    FcMunCarrega: Integer;
    FxMunCarrega: String;
    FinfDoc: TInfDocCollection;
    FinfViagens: TInfViagens;
    FinfPag: TinfPagCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property descEvento: String read FdescEvento write FdescEvento;
    property nProt: String      read FnProt      write FnProt;
    property dtEnc: TDateTime   read FdtEnc      write FdtEnc;
    property cUF: Integer       read FcUF        write FcUF;
    property cMun: Integer      read FcMun       write FcMun;
    property xJust: String      read FxJust      write FxJust;
    property xNome: String      read FxNome      write FxNome;
    property CPF: String        read FCPF        write FCPF;
    // Inclusão de DF-e
    property cMunCarrega: Integer      read FcMunCarrega write FcMunCarrega;
    property xMunCarrega: String       read FxMunCarrega write FxMunCarrega;
    property infDoc: TInfDocCollection read FinfDoc      write FinfDoc;
    // Pagamento Operação MDF-e
    property infViagens: TInfViagens   read FinfViagens  write FinfViagens;
    property infPag: TinfPagCollection read FinfPag      write FinfPag;
  end;

  { TRetInfEvento }

  TRetInfEvento = class
  private
    FId: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcOrgao: Integer;
    FcStat: Integer;
    FxMotivo: String;
    FchMDFe: String;
    FtpEvento: TpcnTpEvento;
    FxEvento: String;
    FnSeqEvento: Integer;
    FCNPJDest: String;
    FemailDest: String;
    FdhRegEvento: TDateTime;
    FnProt: String;
    FXML: String;
    FNomeArquivo: String;
  public
    property Id: String              read FId          write FId;
    property tpAmb: TpcnTipoAmbiente read FtpAmb       write FtpAmb;
    property verAplic: String        read FverAplic    write FverAplic;
    property cOrgao: Integer         read FcOrgao      write FcOrgao;
    property cStat: Integer          read FcStat       write FcStat;
    property xMotivo: String         read FxMotivo     write FxMotivo;
    property chMDFe: String          read FchMDFe      write FchMDFe;
    property tpEvento: TpcnTpEvento  read FtpEvento    write FtpEvento;
    property xEvento: String         read FxEvento     write FxEvento;
    property nSeqEvento: Integer     read FnSeqEvento  write FnSeqEvento;
    property CNPJDest: String        read FCNPJDest    write FCNPJDest;
    property emailDest: String       read FemailDest   write FemailDest;
    property dhRegEvento: TDateTime  read FdhRegEvento write FdhRegEvento;
    property nProt: String           read FnProt       write FnProt;
    property XML: String             read FXML         write FXML;
    property NomeArquivo: String     read FNomeArquivo write FNomeArquivo;
  end;

implementation

uses
  ACBrUtil.Strings;

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
  if FcOrgao <> 0 then
    Result := FcOrgao
  else
    Result := StrToIntDef(copy(FChave, 1, 2), 0);
end;

function TInfEvento.getDescEvento: String;
var
  Desc: String;
begin
  case fTpEvento of
    teCCe                          : Desc := 'Carta de Correcao';
    teCancelamento                 : Desc := 'Cancelamento';
    teManifDestConfirmacao         : Desc := 'Confirmacao da Operacao';
    teManifDestCiencia             : Desc := 'Ciencia da Operacao';
    teManifDestDesconhecimento     : Desc := 'Desconhecimento da Operacao';
    teManifDestOperNaoRealizada    : Desc := 'Operacao nao Realizada';
    teEPECNFe                      : Desc := 'EPEC';
    teEPEC                         : Desc := 'EPEC';
    teMultiModal                   : Desc := 'Registro Multimodal';
    teRegistroPassagem             : Desc := 'Registro de Passagem';
    teRegistroPassagemBRId         : Desc := 'Registro de Passagem BRId';
    teEncerramento                 : Desc := 'Encerramento';
    teEncerramentoFisco            : Desc := 'Encerramento Fisco';
    teInclusaoCondutor             : Desc := 'Inclusao Condutor';
    teInclusaoDFe                  : Desc := 'Inclusao DF-e';
    teRegistroCTe                  : Desc := 'CT-e Autorizado para NF-e';
    teRegistroPassagemNFeCancelado : Desc := 'Registro de Passagem para NF-e Cancelado';
    teRegistroPassagemNFeRFID      : Desc := 'Registro de Passagem para NF-e RFID';
    teCTeAutorizado                : Desc := 'CT-e Autorizado';
    teCTeCancelado                 : Desc := 'CT-e Cancelado';
    teMDFeAutorizado,
    teMDFeAutorizado2              : Desc := 'MDF-e Autorizado';
    teMDFeCancelado,
    teMDFeCancelado2               : Desc := 'MDF-e Cancelado';
    teVistoriaSuframa              : Desc := 'Vistoria SUFRAMA';
    teConfInternalizacao           : Desc := 'Confirmacao de Internalizacao da Mercadoria na SUFRAMA';
    tePagamentoOperacao            : Desc := 'Pagamento Operacao MDF-e';
    teConfirmaServMDFe             : Desc := 'Confirmacao Servico Transporte';
    teAlteracaoPagtoServMDFe       : Desc := 'Alteracao Pagamento Servico MDFe';
  else
    Result := '';
  end;

  Result := ACBrStr(Desc);
end;

function TInfEvento.getTipoEvento: String;
begin
  try
    Result := TpEventoToStr( FTpEvento );
  except
    Result := '';
  end;
end;

function TInfEvento.DescricaoTipoEvento(TipoEvento: TpcnTpEvento): String;
begin
  case TipoEvento of
    teCCe                          : Result := 'CARTA DE CORREÇÃO ELETRÔNICA';
    teCancelamento                 : Result := 'CANCELAMENTO DO MDF-e';
    teManifDestConfirmacao         : Result := 'CONFIRMAÇÃO DA OPERAÇÃO';
    teManifDestCiencia             : Result := 'CIÊNCIA DA OPERAÇÃO';
    teManifDestDesconhecimento     : Result := 'DESCONHECIMENTO DA OPERAÇÃO';
    teManifDestOperNaoRealizada    : Result := 'OPERAÇÃO NÃO REALIZADA';
    teEPECNFe                      : Result := 'EPEC';
    teEPEC                         : Result := 'EPEC';
    teMultiModal                   : Result := 'REGISTRO MULTIMODAL';
    teRegistroPassagem             : Result := 'REGISTRO DE PASSAGEM';
    teRegistroPassagemBRId         : Result := 'REGISTRO DE PASSAGEM BRId';
    teEncerramento                 : Result := 'ENCERRAMENTO';
    teEncerramentoFisco            : Result := 'ENCERRAMENTO FISCO';
    teInclusaoCondutor             : Result := 'INCLUSAO CONDUTOR';
    teInclusaoDFe                  : Result := 'INCLUSAO DF-e';
    teRegistroCTe                  : Result := 'CT-e Autorizado para NF-e';
    teRegistroPassagemNFeCancelado : Result := 'Registro de Passagem para NF-e Cancelado';
    teRegistroPassagemNFeRFID      : Result := 'Registro de Passagem para NF-e RFID';
    teCTeAutorizado                : Result := 'CT-e Autorizado';
    teCTeCancelado                 : Result := 'CT-e Cancelado';
    teMDFeAutorizado,
    teMDFeAutorizado2              : Result := 'MDF-e Autorizado';
    teMDFeCancelado,
    teMDFeCancelado2               : Result := 'MDF-e Cancelado';
    teVistoriaSuframa              : Result := 'Vistoria SUFRAMA';
    teConfInternalizacao           : Result := 'Confirmacao de Internalizacao da Mercadoria na SUFRAMA';
    tePagamentoOperacao            : Result := 'Pagamento Operacao MDF-e';
    teConfirmaServMDFe             : Result := 'Confirmacao Servico Transporte';
    teAlteracaoPagtoServMDFe       : Result := 'Alteracao Pagamento Servico MDFe';
  else
    Result := 'Não Definido';
  end;
end;

{ TInfDocCollection }

function TInfDocCollection.Add: TInfDocCollectionItem;
begin
  Result := Self.New;
end;

function TInfDocCollection.GetItem(Index: Integer): TInfDocCollectionItem;
begin
  Result := TInfDocCollectionItem(inherited Items[Index]);
end;

function TInfDocCollection.New: TInfDocCollectionItem;
begin
  Result := TInfDocCollectionItem.Create;
  Self.Add(Result);
end;

procedure TInfDocCollection.SetItem(Index: Integer;
  Value: TInfDocCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TDetEvento }

constructor TDetEvento.Create;
begin
  inherited Create;

  FinfDoc     := TInfDocCollection.Create;
  FinfViagens := TInfViagens.Create;
  FinfPag     := TinfPagCollection.Create;
end;

destructor TDetEvento.Destroy;
begin
  FinfDoc.Free;
  FinfViagens.Free;
  FinfPag.Free;

  inherited;
end;

end.
