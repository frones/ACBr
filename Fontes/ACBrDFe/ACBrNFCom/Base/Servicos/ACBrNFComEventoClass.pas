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

unit ACBrNFComEventoClass;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  ACBrXmlBase,
//  ACBrDFeConversao,
  pcnConversao,
  ACBrNFComConversao;

type
  EventoException = class(Exception);

  TDetEvento = class
  private
    FVersao: String;
    FDescEvento: String;
    FnProt: String;        // Cancelamento
    FxJust: String;        // Cancelamento

    FidPedidoCancelado: String;
  public
    property versao: String     read FVersao     write FVersao;
    property descEvento: String read FDescEvento write FDescEvento;
    property nProt: String      read FnProt      write FnProt;
    property xJust: String      read FxJust      write FxJust;

    property idPedidoCancelado: String read FidPedidoCancelado write FidPedidoCancelado;
  end;

  TInfEvento = class
  private
    FID: String;
    FtpAmbiente: TACBrTipoAmbiente;
    FCNPJ: String;
    FcOrgao: Integer;
    FChave: String;
    FDataEvento: TDateTime;
    FTpEvento: TpcnTpEvento;
    FnSeqEvento: Integer;
    FDetEvento: TDetEvento;

    function getcOrgao: Integer;
    function getDescEvento: String;
    function getTipoEvento: String;
  public
    constructor Create;
    destructor Destroy; override;

    function DescricaoTipoEvento(TipoEvento:TpcnTpEvento): String;

    property id: String               read FID             write FID;
    property cOrgao: Integer          read getcOrgao       write FcOrgao;
    property tpAmb: TACBrTipoAmbiente read FtpAmbiente     write FtpAmbiente;
    property CNPJ: String             read FCNPJ           write FCNPJ;
    property chNFCom: String          read FChave          write FChave;
    property dhEvento: TDateTime      read FDataEvento     write FDataEvento;
    property tpEvento: TpcnTpEvento   read FTpEvento       write FTpEvento;
    property nSeqEvento: Integer      read FnSeqEvento     write FnSeqEvento;
    property detEvento: TDetEvento    read FDetEvento      write FDetEvento;
    property DescEvento: String       read getDescEvento;
    property TipoEvento: String       read getTipoEvento;
  end;

  { TRetInfEvento }

  TRetInfEvento = class(TObject)
  private
    FId: String;
    FNomeArquivo: String;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: String;
    FcOrgao: Integer;
    FcStat: Integer;
    FxMotivo: String;
    FchNFCom: String;
    FtpEvento: TpcnTpEvento;
    FxEvento: String;
    FnSeqEvento: Integer;
    FCNPJDest: String;
    FemailDest: String;
    FcOrgaoAutor: Integer;
    FdhRegEvento: TDateTime;
    FnProt: String;
    FXML: AnsiString;
  public
    property Id: String               read FId          write FId;
    property tpAmb: TACBrTipoAmbiente read FtpAmb       write FtpAmb;
    property verAplic: String         read FverAplic    write FverAplic;
    property cOrgao: Integer          read FcOrgao      write FcOrgao;
    property cStat: Integer           read FcStat       write FcStat;
    property xMotivo: String          read FxMotivo     write FxMotivo;
    property chNFCom: String          read FchNFCom     write FchNFCom;
    property tpEvento: TpcnTpEvento   read FtpEvento    write FtpEvento;
    property xEvento: String          read FxEvento     write FxEvento;
    property nSeqEvento: Integer      read FnSeqEvento  write FnSeqEvento;
    property CNPJDest: String         read FCNPJDest    write FCNPJDest;
    property emailDest: String        read FemailDest   write FemailDest;
    property cOrgaoAutor: Integer     read FcOrgaoAutor write FcOrgaoAutor;
    property dhRegEvento: TDateTime   read FdhRegEvento write FdhRegEvento;
    property nProt: String            read FnProt       write FnProt;
    property XML: AnsiString          read FXML         write FXML;
    property NomeArquivo: String      read FNomeArquivo write FNomeArquivo;
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
    teCancelamento: Result := 'Cancelamento';
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

function TInfEvento.DescricaoTipoEvento(TipoEvento: TpcnTpEvento): String;
begin
  case TipoEvento of
    teCancelamento: Result := 'CANCELAMENTO DE NF3-e';
  else
    Result := 'Não Definido';
  end;
end;

end.
