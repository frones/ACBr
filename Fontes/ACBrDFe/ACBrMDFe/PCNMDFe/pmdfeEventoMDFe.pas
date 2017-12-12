{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

{*******************************************************************************
|* Historico
|*
|* 01/08/2012: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pmdfeEventoMDFe;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  pcnAuxiliar, pcnConversao;

type
  TInfEvento = class;
  TDetEvento = class;
  TRetInfEvento = class;
  EventoException = class(Exception);

  TInfEvento = class
  private
    FId: String;
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
    function getDescEvento: String;
    function getTipoEvento: String;
  public
    constructor Create;
    destructor Destroy; override;
    function DescricaoTipoEvento(TipoEvento:TpcnTpEvento): String;

    property Id: String              read FId             write FId;
    property cOrgao: Integer         read getcOrgao       write FcOrgao;
    property tpAmb: TpcnTipoAmbiente read FtpAmbiente     write FtpAmbiente;
    property CNPJ: String            read FCNPJ           write FCNPJ;
    property chMDFe: String          read FChave          write FChave;
    property dhEvento: TDateTime     read FDataEvento     write FDataEvento;
    property tpEvento: TpcnTpEvento  read FTpEvento       write FTpEvento;
    property nSeqEvento: Integer     read FnSeqEvento     write FnSeqEvento;
    property versaoEvento: String    read FVersaoEvento write FversaoEvento;
    property detEvento: TDetEvento   read FDetEvento      write FDetEvento;
    property DescEvento: String      read getDescEvento;
    property TipoEvento: String      read getTipoEvento;
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
  public
    property descEvento: String read FdescEvento write FdescEvento;
    property nProt: String      read FnProt      write FnProt;
    property dtEnc: TDateTime   read FdtEnc      write FdtEnc;
    property cUF: Integer       read FcUF        write FcUF;
    property cMun: Integer      read FcMun       write FcMun;
    property xJust: String      read FxJust      write FxJust;
    property xNome: String      read FxNome      write FxNome;
    property CPF: String        read FCPF        write FCPF;
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
    FXML: AnsiString;
    FNomeArquivo: String;
  public
  published
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
    property XML: AnsiString         read FXML         write FXML;
    property NomeArquivo: String     read FNomeArquivo write FNomeArquivo;
  end;

implementation

uses
  ACBrUtil;

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
  Result := StrToInt(copy(FChave, 1, 2));
  (*
  {Estados que utilizam a SVAN: ES, MA, PA, PI, RN}
  {Devem utilizar 91}
  if Result in [32,21,15,22,24] then
    Result := 91;
  *)
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
    teInclusaoCondutor             : Desc := 'Inclusao Condutor';
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
  else
    raise EventoException.Create('Descrição do Evento não Implementado!');
  end;

  Result := ACBrStr(Desc);
end;

function TInfEvento.getTipoEvento: String;
begin
  try
    Result := TpEventoToStr( FTpEvento );
  except
    raise EventoException.Create('Tipo do Evento não Implementado!');
  end;
end;

function TInfEvento.DescricaoTipoEvento(TipoEvento: TpcnTpEvento): String;
begin
  case TipoEvento of
    teCCe                      : Result := 'CARTA DE CORREÇÃO ELETRÔNICA';
    teCancelamento             : Result := 'CANCELAMENTO DO MDF-e';
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
    teRegistroPassagemNFeRFID     : Result := 'Registro de Passagem para NF-e RFID';
    teCTeAutorizado               : Result := 'CT-e Autorizado';
    teCTeCancelado                : Result := 'CT-e Cancelado';
    teMDFeAutorizado,
    teMDFeAutorizado2             : Result := 'MDF-e Autorizado';
    teMDFeCancelado,
    teMDFeCancelado2              : Result := 'MDF-e Cancelado';
    teVistoriaSuframa             : Result := 'Vistoria SUFRAMA';
    teConfInternalizacao       : Result := 'Confirmacao de Internalizacao da Mercadoria na SUFRAMA';
  else
    Result := 'Não Definido';
  end;
end;

end.
