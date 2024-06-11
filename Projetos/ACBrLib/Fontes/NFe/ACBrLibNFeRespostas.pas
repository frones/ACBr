{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                 }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$I ACBr.inc}

unit ACBrLibNFeRespostas;

interface

uses
  SysUtils, Classes, contnrs,
  ACBrNFe.EventoClass,
  ACBrNFe.RetEnvEvento,
  ACBrNFe.RetConsSit,
  ACBrLibResposta, ACBrLibConsReciDFe, ACBrNFe;

type

  { TLibNFeResposta }
  TLibNFeResposta = class(TACBrLibRespostaBase)
  private
    FMsg: string;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

  published
    property Msg: string read FMsg write FMsg;

  end;

  { TLibNFeServiceResposta }
  TLibNFeServiceResposta = class abstract(TACBrLibResposta<TACBrNFe>)
  private
    FMsg: string;
    Fversao: string;
    FtpAmb: string;
    FverAplic: string;
    FcStat: integer;
    FxMotivo: string;
    FcUF: integer;
    FdhRecbto: TDateTime;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBrNFe: TACBrNFe); virtual; abstract; reintroduce;

  published
    property Msg: string read FMsg write FMsg;
    property Versao: string read Fversao write Fversao;
    property tpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property XMotivo: string read FxMotivo write FxMotivo;
    property CUF: integer read FcUF write FcUF;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;

  end;

  { TStatusServicoResposta }
  TStatusServicoResposta = class(TLibNFeServiceResposta)
  private
    FTMed: integer;
    FdhRetorno: TDateTime;
    FxObs: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBrNFe: TACBrNFe); override;

  published
    property TMed: integer read FTMed write FTMed;
    property DhRetorno: TDateTime read FdhRetorno write FdhRetorno;
    property XObs: string read FxObs write FxObs;

  end;

  { TInutilizarNFeResposta }
  TInutilizarNFeResposta = class(TLibNFeServiceResposta)
  private
    FNomeArquivo: String;
    FXml: String;
    FNProt: String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBrNFe: TACBrNFe); override;

  published
    property NomeArquivo: String read FNomeArquivo write FNomeArquivo;
    property Xml: String read FXml write FXml;
    property NProt: String read FNProt write FNProt;

  end;

  { TEnvioResposta }
  TEnvioResposta = class(TLibNFeServiceResposta)
  private
    FtMed: integer;
    FnRec: string;
    FNProt: string;
    FItemRetorno: TRetornoItemResposta;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ACBrNFe: TACBrNFe); override;

  published
   property TMed: integer read FtMed write FtMed;
   property NRec: string read FnRec write FnRec;
   property NProt: String read FNProt write FNProt;
   property Retorno: TRetornoItemResposta read FItemRetorno;

  end;

  { TCancelamentoResposta }
  TCancelamentoResposta = class(TLibNFeServiceResposta)
  private
    FchNFe: string;
    FnProt: string;
    FtpEvento: string;
    FxEvento: string;
    FnSeqEvento: integer;
    FCNPJDest: string;
    FemailDest: string;
    Fxml: string;
    FArquivo: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBrNFe: TACBrNFe); override;

  published
    property chNFe: string read FchNFe write FchNFe;
    property nProt: string read FnProt write FnProt;
    property tpEvento: string read FtpEvento write FtpEvento;
    property xEvento: string read FxEvento write FxEvento;
    property nSeqEvento: integer read FnSeqEvento write FnSeqEvento;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property emailDest: string read FemailDest write FemailDest;
    property XML: string read Fxml write Fxml;
    property Arquivo: string read FArquivo write FArquivo;

  end;

  { TEventoItemResposta }
  TEventoItemResposta = class(TACBrLibRespostaBase)
  private
    FtpAmb: String;
    FverAplic: String;
    FcStat: integer;
    FxMotivo: String;
    Farquivo: String;
    FchNFe: string;
    FCNPJDest: string;
    FcOrgao: integer;
    FdhRegEvento: TDateTime;
    FdigVal: String;
    FemailDest: string;
    FId: string;
    FnProt: String;
    FnSeqEvento: Integer;
    FtpEvento: string;
    FxEvento: string;
    FXML: string;
  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AInfEvento: TRetInfEvento);

  published
    property tpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property XMotivo: string read FxMotivo write FxMotivo;
    property chNFe: string read FchNFe write FchNFe;
    property nProt: String read FnProt write FnProt;
    property arquivo: String read Farquivo write Farquivo;
    property digVal: String read FdigVal write FdigVal;
    property Id: string read FId write FId;
    property cOrgao: integer read FcOrgao write FcOrgao;
    property dhRegEvento: TDateTime read FdhRegEvento write FdhRegEvento;
    property tpEvento: string read FtpEvento write FtpEvento;
    property xEvento: string read FxEvento write FxEvento;
    property nSeqEvento: Integer read FnSeqEvento write FnSeqEvento;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property emailDest: string read FemailDest write FemailDest;
    property XML: string read FXML write FXML;
  end;

  { TEventoResposta }
  TEventoResposta = class(TLibNFeServiceResposta)
  private
    FidLote: Integer;
    FcOrgao: Integer;
    FItems: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ACBrNFe: TACBrNFe); override;

  published
    property idLote: Integer read FidLote write FidLote;
    property cOrgao: Integer read FcOrgao write FcOrgao;
    property Items: TObjectList read FItems;

  end;

  { TConsultaNFeInfCanResposta }
  TConsultaNFeInfCanResposta  = class(TLibNFeServiceResposta)
  private
    FChNFe: String;
    FNProt: String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBrNFe: TACBrNFe); override;

  published
    property ChNFe: String read FChNFe write FChNFe;
    property NProt: String read FNProt write FNProt;
  end;

  { TConsultaNFeItemPedidoResposta }
  TConsultaNFeItemPedidoResposta  = class(TACBrLibRespostaBase)
  private
    FnumItem: Integer;
    FqtdeItem: Double;
  public
    constructor Create(const AId, AIndex: Integer; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const AItem: TitemPedidoCollectionItem);

  published
    property numItem: Integer         read FnumItem        write FnumItem;
    property qtdeItem: Double         read FqtdeItem       write FqtdeItem;

  end;

  { TConsultaNFeChNFePendResposta }
  TConsultaNFeChNFePendResposta  = class(TACBrLibRespostaBase)
  private
    FchNFePend : String;
  public
    constructor Create(const AId, AIndex, ASIndex: Integer; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ARetchNFePend: TRetchNFePendCollectionItem);

  published
    property chNFePend : String       read FchNFePend      write FchNFePend;

  end;

  { TConsultaNFeRetEventoResposta }
  TConsultaNFeRetEventoResposta  = class(TACBrLibRespostaBase)
  private
    FAId: Integer;
    FAIndex: Integer;
    FId: String;
    Fversao: String;
    FtpAmb: String;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FNomeArquivo: String;
    FcOrgao: String;
    FchNFe: String;
    FtpEvento: String;
    FxEvento: String;
    FnSeqEvento: Integer;
    FCNPJDest: String;
    FemailDest: String;
    FcOrgaoAutor: String;
    FdhRegEvento: TDateTime;
    FnProt: String;
    FXML: String;
    FItems: TObjectList;

  public
    constructor Create(const AId, AIndex: Integer; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ARetInfEvento: TRetInfEvento);

  published
    property Id: String read FId;
    property Versao: string read Fversao write Fversao;
    property tpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property XMotivo: string read FxMotivo write FxMotivo;
    property NomeArquivo: String read FNomeArquivo write FNomeArquivo;
    property cOrgao: String read FcOrgao write FcOrgao;
    property chNFe: String read FchNFe write FchNFe;
    property tpEvento: String read FtpEvento write FtpEvento;
    property xEvento: String read FxEvento write FxEvento;
    property nSeqEvento: Integer read FnSeqEvento write FnSeqEvento;
    property CNPJDest: String read FCNPJDest write FCNPJDest;
    property emailDest: String read FemailDest write FemailDest;
    property cOrgaoAutor: String read FcOrgaoAutor write FcOrgaoAutor;
    property dhRegEvento: TDateTime read FdhRegEvento write FdhRegEvento;
    property nProt: String read FnProt write FnProt;
    property XML: String read FXML write FXML;
    property Items: TObjectList read FItems;

  end;

  { TConsultaNFeDetEventoResposta }
  TConsultaNFeDetEventoResposta  = class(TACBrLibRespostaBase)
  private
    FId: Integer;
    Fversao : String;
    FverAplic : String;
    FdescEvento : String;
    FxCorrecao : String;
    FxCondUso : String;
    FnProt : String;
    FxJust : String;
    FcOrgaoAutor : String;
    FtpAutor : String;
    FdhEmi : TDateTime;
    FtpNF : String;
    FIE : String;
    FDESTCNPJCPF : String;
    FDESTidEstrangeiro : String;
    FDESTIE : String;
    FDESTUF : String;
    FvNF : Double;
    FvICMS : Double;
    FvST : Double;
    FidPedidoCancelado : String;
    FItems: TObjectList;

  public
    constructor Create(const AId: Integer; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const AEvento: TDetEvento);

  published
    property Versao: string read Fversao write Fversao;
    property VerAplic: string read FverAplic write FverAplic;
    property descEvento: String read FdescEvento write FdescEvento;
    property xCorrecao: String read FxCorrecao write FxCorrecao;
    property xCondUso: String read FxCondUso write FxCondUso;
    property nProt: String read FnProt write FnProt;
    property xJust: String read FxJust write FxJust;
    property cOrgaoAutor: String read FcOrgaoAutor write FcOrgaoAutor;
    property tpAutor: String read FtpAutor write FtpAutor;
    property dhEmi: TDateTime read FdhEmi write FdhEmi;
    property tpNF: String read FtpNF write FtpNF;
    property IE: String read FIE write FIE;
    property DESTCNPJCPF: String read FDESTCNPJCPF write FDESTCNPJCPF;
    property DESTidEstrangeiro: String read FDESTidEstrangeiro write FDESTidEstrangeiro;
    property DESTIE: String read FDESTIE write FDESTIE;
    property DESTUF: String read FDESTUF write FDESTUF;
    property vNF: Double read FvNF write FvNF;
    property vICMS: Double read FvICMS write FvICMS;
    property vST: Double read FvST write FvST;
    property idPedidoCancelado: String read FidPedidoCancelado write FidPedidoCancelado;
    property Items: TObjectList read FItems;

  end;

  { TConsultaNFeProcEventoResposta }
  TConsultaNFeProcEventoResposta  = class(TACBrLibRespostaBase)
  private
    FID: Integer;
    FtpAmb: String;
    FcOrgao: String;
    FCNPJ: String;
    FchNFe: String;
    FdhEvento: TDateTime;
    FtpEvento: String;
    FnSeqEvento: Integer;
    FverEvento: String;
    FXml: String;
    FDetEvento: TConsultaNFeDetEventoResposta;
    FRetEventos: TObjectList;

  public
    constructor Create(const AId: Integer; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const AEvento: TRetEventoNFeCollectionItem);

  published
    property ID: Integer read FID;
    property tpAmb: string read FtpAmb write FtpAmb;
    property cOrgao: String read FcOrgao write FcOrgao;
    property CNPJ: String read FCNPJ write FCNPJ;
    property chNFe: String read FchNFe write FchNFe;
    property dhEvento: TDateTime read FdhEvento write FdhEvento;
    property tpEvento: String read FtpEvento write FtpEvento;
    property nSeqEvento: Integer read FnSeqEvento write FnSeqEvento;
    property verEvento: String read FverEvento write FverEvento;
    property Xml: String read FXml write FXml;
    property detEvento: TConsultaNFeDetEventoResposta read FDetEvento write FDetEvento;
    property RetEventos: TObjectList read FRetEventos;

  end;

  { TConsultaNFeResposta }
  TConsultaNFeResposta = class(TLibNFeServiceResposta)
  private
    FChNFe: String;
    FNProt: String;
    FDigVal: String;
    FcMsg: Integer;
    FxMsg: String;
    FInfCan: TConsultaNFeInfCanResposta;
    FEventos: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ACBrNFe: TACBrNFe); override;

  published
    property ChNFe: String read FChNFe write FChNFe;
    property NProt: String read FNProt write FNProt;
    property DigVal: String read FDigVal write FDigVal;
    property cMsg: Integer read FcMsg write FcMsg;
    property xMsg: String read FxMsg write FxMsg;
    property InfCan: TConsultaNFeInfCanResposta read FInfCan write FInfCan;
    property Eventos: TObjectList read FEventos;

  end;

  { TPathNFeResposta }
  TPathNFeResposta  = class(TACBrLibRespostaBase)
  private
    FArquivo: String;

  public
    constructor Create(const AnNF: Integer; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const APathNFe: String); reintroduce;

  published
    property Arquivo: string read FArquivo write FArquivo;

  end;

  { TEnvioRetornoResposta }
  TEnvioRetornoResposta = class(TACBrLibRespostaBase)
  private
    FEnvioResposta: TEnvioResposta;
    FRetornoResposta: TRetornoResposta;
    FNFe_Arq: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ACBrNFe: TACBrNFe); reintroduce;

  published

    property EnvioResposta: TEnvioResposta read FEnvioResposta write FEnvioResposta;
    property RetornoResposta: TRetornoResposta read FRetornoResposta write FRetornoResposta;
    property NFe_Arq: TObjectList read FNFe_Arq;

  end;

implementation

uses
  pcnAuxiliar, pcnConversao,
  ACBrLibNFeConsts, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings;

{ TPathNFeResposta }

constructor TPathNFeResposta.Create(const AnNF: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create('NFe_Arq' + Trim(IntToStr(AnNF)), ATipo, AFormato);

end;

destructor TPathNFeResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TPathNFeResposta.Processar(const APathNFe: String);
begin
  FArquivo:= APathNFe;
end;

{ TEnvioAssincronoResposta }

constructor TEnvioRetornoResposta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoResposta, ATipo, AFormato);
  FEnvioResposta := TEnvioResposta.Create( Tipo, Formato);
  FNFe_Arq := TObjectList.Create;

end;

destructor TEnvioRetornoResposta.Destroy;
begin
  if Assigned(FEnvioResposta) then FEnvioResposta.Free;
  if Assigned(FRetornoResposta) then FRetornoResposta.Free;

  FNFe_Arq.Clear;
  FNFe_Arq.Free;

  inherited Destroy;

end;

procedure TEnvioRetornoResposta.Processar(const ACBrNFe: TACBrNFe);
var
  lPathNFe: TPathNFeResposta;
  I, J: Integer;
begin
    EnvioResposta.Processar(ACBrNFe);

    if Assigned(ACBrNFe.WebServices.Retorno) and (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
    begin
      RetornoResposta := TRetornoResposta.Create('NFe', Tipo, Formato);

      RetornoResposta.Processar(ACBrNFe.WebServices.Retorno.NFeRetorno,
                                ACBrNFe.WebServices.Retorno.Recibo,
                                ACBrNFe.WebServices.Retorno.Msg,
                                ACBrNFe.WebServices.Retorno.Protocolo,
                                ACBrNFe.WebServices.Retorno.ChaveNFe);

      for I := 0 to ACBrNFe.WebServices.Retorno.NFeRetorno.ProtDFe.Count - 1 do
      begin
        for J := 0 to ACBrNFe.NotasFiscais.Count - 1 do
        begin
          if ('NFe' + ACBrNFe.WebServices.Retorno.NFeRetorno.ProtDFe.Items[I].chDFe =
          ACBrNFe.NotasFiscais.Items[J].NFe.infNFe.Id) then
          begin
            lPathNFe := TPathNFeResposta.Create(
                         ExtrairNumeroChaveAcesso(ACBrNFe.WebServices.Retorno.NFeRetorno.ProtDFe.Items[I].chDFe),
                         Tipo, Formato);
              lPathNFe.Processar( ACBrNFe.NotasFiscais.Items[J].NomeArq );

              if Assigned(FNFe_Arq) then
                FNFe_Arq.Add(lPathNFe);
            end;

          end;

        end;

    end;

end;

{ TLibNFeResposta }
constructor TLibNFeResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TLibNFeServiceResposta }
constructor TLibNFeServiceResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TConsultaNFeChNFePendResposta }
constructor TConsultaNFeChNFePendResposta.Create(const AId, AIndex, ASIndex: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create('ChNFePend' + Trim(IntToStrZero(AId +1, 3)) + Trim(IntToStrZero(AIndex +1, 3))
                + Trim(IntToStrZero(ASIndex +1, 3)), ATipo, AFormato);
end;

procedure TConsultaNFeChNFePendResposta.Processar(const ARetchNFePend: TRetchNFePendCollectionItem);
begin
  chNFePend:= ARetchNFePend.ChavePend;
end;

{ TConsultaNFeRetEventoResposta }
constructor TConsultaNFeRetEventoResposta.Create(const AId, AIndex: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create('RetEvento' + Trim(IntToStrZero(AId, 3)) + Trim(IntToStrZero(AIndex, 3)), ATipo, AFormato);
  FItems := TObjectList.Create;

  FAId := AId;
  FAIndex := AIndex;

  FId := IntToStr(AIndex);
end;

destructor TConsultaNFeRetEventoResposta.Destroy;
begin
  FItems.Clear;
  FItems.Free;

  inherited Destroy;
end;

procedure TConsultaNFeRetEventoResposta.Processar(const ARetInfEvento: TRetInfEvento);
Var
  i: Integer;
  Item: TConsultaNFeChNFePendResposta;
begin
  NomeArquivo := ARetInfEvento.NomeArquivo;
  tpAmb := TpAmbToStr(ARetInfEvento.tpAmb);
  verAplic := ARetInfEvento.verAplic;
  cOrgao := IntToStr(ARetInfEvento.cOrgao);
  cStat := ARetInfEvento.cStat;
  xMotivo:= ARetInfEvento.xMotivo;
  chNFe := ARetInfEvento.chNFe;
  tpEvento := TpEventoToStr(ARetInfEvento.tpEvento);
  xEvento := ARetInfEvento.xEvento;
  nSeqEvento := ARetInfEvento.nSeqEvento;
  CNPJDest := ARetInfEvento.CNPJDest;
  emailDest := ARetInfEvento.emailDest;
  cOrgaoAutor := IntToStr(ARetInfEvento.cOrgaoAutor);
  dhRegEvento := ARetInfEvento.dhRegEvento;
  nProt := ARetInfEvento.nProt;
  XML := ARetInfEvento.XML;

  for i := 0 to ARetInfEvento.chNFePend.Count - 1  do
   begin
     Item := TConsultaNFeChNFePendResposta.Create(FAId, FAIndex, i+1, Tipo, Formato);
     Item.Processar(ARetInfEvento.chNFePend.Items[i]);
     FItems.Add(Item);
   end;
end;

{ TConsultaNFeItemPedidoResposta }
constructor TConsultaNFeItemPedidoResposta.Create(const AId, AIndex: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create('ItemPedido' + Trim(IntToStrZero(AId, 3)) + Trim(IntToStrZero(AIndex, 3)), ATipo, AFormato);
end;

procedure TConsultaNFeItemPedidoResposta.Processar(const AItem: TitemPedidoCollectionItem);
begin
  numItem := AItem.numItem;
  qtdeItem := AItem.qtdeItem;
end;

{ TConsultaNFeDetEventoResposta }
constructor TConsultaNFeDetEventoResposta.Create(const AId: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create('DetEvento' + Trim(IntToStrZero(AId, 3)), ATipo, AFormato);
  FItems := TObjectList.Create;
  FId := AID;
end;

destructor TConsultaNFeDetEventoResposta.Destroy;
begin
  FItems.Clear;
  FItems.Free;

  inherited Destroy;
end;

procedure TConsultaNFeDetEventoResposta.Processar(const AEvento: TDetEvento);
Var
  i: Integer;
  Item: TConsultaNFeItemPedidoResposta;
begin
   versao := AEvento.versao;
   descEvento:= AEvento.descEvento;
   xCorrecao := AEvento.xCorrecao;
   xCondUso := AEvento.xCondUso;
   nProt := AEvento.nProt;
   xJust := AEvento.xJust;
   cOrgaoAutor:= IntToStr(AEvento.cOrgaoAutor);
   tpAutor := TipoAutorToStr(AEvento.tpAutor);
   verAplic := AEvento.verAplic;
   dhEmi := AEvento.dhEmi;
   tpNF := tpNFToStr(AEvento.tpNF);
   IE := AEvento.IE;
   DESTCNPJCPF := AEvento.dest.CNPJCPF;
   DESTidEstrangeiro := AEvento.dest.idEstrangeiro;
   DESTIE := AEvento.dest.IE;
   DESTUF := AEvento.dest.UF;
   vNF := AEvento.vNF;
   vICMS := AEvento.vICMS;
   vST := AEvento.vST;
   idPedidoCancelado :=  AEvento.idPedidoCancelado;

   for i := 0 to AEvento.itemPedido.Count - 1  do
   begin
     Item := TConsultaNFeItemPedidoResposta.Create(FId, i+1, Tipo, Formato);
     Item.Processar(AEvento.itemPedido.Items[i]);
     FItems.Add(Item);
   end;

end;

{ TConsultaNFeProcEventoResposta }
constructor TConsultaNFeProcEventoResposta.Create(const AId: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create('ProcEventoNFe' + Trim(IntToStrZero(AId, 3)), ATipo, AFormato);
  FDetEvento := TConsultaNFeDetEventoResposta.Create(AId, ATipo, Formato);
  FRetEventos := TObjectList.Create;
  FID := AId;
end;

destructor TConsultaNFeProcEventoResposta.Destroy;
begin
  FDetEvento.Free;

  FRetEventos.Clear;
  FRetEventos.Free;

  inherited Destroy;
end;

procedure TConsultaNFeProcEventoResposta.Processar(const AEvento: TRetEventoNFeCollectionItem);
Var
  i: Integer;
  RetEvento: TConsultaNFeRetEventoResposta;
begin
   cOrgao := IntToStr(AEvento.RetEventoNFe.InfEvento.cOrgao);
   tpAmb := TpAmbToStr(AEvento.RetEventoNFe.InfEvento.tpAmb);
   CNPJ := AEvento.RetEventoNFe.InfEvento.CNPJ;
   chNFe := AEvento.RetEventoNFe.InfEvento.chNFe;
   dhEvento := AEvento.RetEventoNFe.InfEvento.dhEvento;
   tpEvento := TpEventoToStr(AEvento.RetEventoNFe.InfEvento.tpEvento);
   nSeqEvento := AEvento.RetEventoNFe.InfEvento.nSeqEvento;
   verEvento := AEvento.RetEventoNFe.InfEvento.versaoEvento;
   Xml := AEvento.RetEventoNFe.XML;

   FDetEvento.Processar(AEvento.RetEventoNFe.InfEvento.detEvento);

   for i := 0 to AEvento.RetEventoNFe.retEvento.Count - 1  do
   begin
     RetEvento := TConsultaNFeRetEventoResposta.Create(FId, i+1, Tipo, Formato);
     RetEvento.Processar(AEvento.RetEventoNFe.retEvento.Items[i].RetInfEvento);
     FRetEventos.Add(RetEvento);
   end;
end;

{ TConsultaNFeInfCanResposta }
constructor TConsultaNFeInfCanResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsultaInfCan, ATipo, AFormato);
end;

procedure TConsultaNFeInfCanResposta.Processar(const ACBrNFe: TACBrNFe);
begin
  with ACBrNFe.WebServices.Consulta do
  begin
    Self.tpAmb := TpAmbToStr(retCancNFe.tpAmb);
    Self.VerAplic := retCancNFe.verAplic;
    Self.CStat := retCancNFe.cStat;
    Self.XMotivo := retCancNFe.xMotivo;
    Self.CUF := retCancNFe.cUF;
    Self.ChNFe := retCancNFe.chNFE;
    Self.DhRecbto := retCancNFe.dhRecbto;
    Self.NProt := retCancNFe.nProt;
  end;
end;

{ TEventoItemResposta }
constructor TEventoItemResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEventoItemResposta.Processar(const AInfEvento: TRetInfEvento);
begin
  Id := AInfEvento.Id;
  tpAmb := TpAmbToStr(AInfEvento.tpAmb);
  verAplic := AInfEvento.verAplic;
  cOrgao := AInfEvento.cOrgao;
  cStat := AInfEvento.cStat;
  xMotivo := AInfEvento.xMotivo;
  chNFe := AInfEvento.chNFe;
  tpEvento := TpEventoToStr(AInfEvento.tpEvento);
  xEvento := AInfEvento.xEvento;
  nSeqEvento := AInfEvento.nSeqEvento;
  CNPJDest := AInfEvento.CNPJDest;
  emailDest := AInfEvento.emailDest;
  dhRegEvento := AInfEvento.dhRegEvento;
  nProt := AInfEvento.nProt;
  Arquivo := AInfEvento.NomeArquivo;
  XML := AInfEvento.XML;
end;

{ TEventoResposta }
constructor TEventoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEvento, ATipo, AFormato);

  FItems := TObjectList.Create;
end;

destructor TEventoResposta.Destroy;
begin
  FItems.Clear;
  FItems.Free;

  inherited Destroy;
end;

procedure TEventoResposta.Processar(const ACBrNFe: TACBrNFe);
Var
  I: Integer;
  Item: TEventoItemResposta;
begin
  with ACBrNFe.WebServices.EnvEvento do
  begin
    Self.VerAplic := EventoRetorno.VerAplic;
    Self.tpAmb := TpAmbToStr(EventoRetorno.tpAmb);
    Self.CStat := EventoRetorno.cStat;
    Self.XMotivo := EventoRetorno.XMotivo;
    Self.idLote := EventoRetorno.IdLote;
    Self.cOrgao := EventoRetorno.cOrgao;

    for I := 0 to EventoRetorno.retEvento.Count - 1 do
    begin
      Item := TEventoItemResposta.Create('Evento' + Trim(IntToStrZero(I +1, 3)), Tipo, Formato);
      Item.Processar(EventoRetorno.retEvento.Items[i].RetInfEvento);
      FItems.Add(Item);
    end;
  end;
end;

{ TCancelamentoResposta }
constructor TCancelamentoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespCancelamento, ATipo, AFormato);
end;

procedure TCancelamentoResposta.Processar(const ACBrNFe: TACBrNFe);
begin
  with ACBrNFe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0] do
  begin
    Arquivo:= RetInfEvento.NomeArquivo;
    Versao := RetInfevento.verAplic;
    TpAmb := TpAmbToStr(RetInfevento.TpAmb);
    VerAplic := RetInfevento.VerAplic;
    CStat := RetInfevento.cStat;
    XMotivo := RetInfevento.XMotivo;
    CUF := RetInfevento.cOrgao;
    ChNFe := RetInfevento.chNFe;
    DhRecbto := RetInfevento.dhRegEvento;
    NProt := RetInfevento.nProt;
    TpEvento := TpEventoToStr(RetInfevento.tpEvento);
    xEvento := RetInfevento.xEvento;
    nSeqEvento := RetInfevento.nSeqEvento;
    CNPJDest := RetInfevento.CNPJDest;
    emailDest := RetInfevento.emailDest;
    XML := RetInfevento.XML;
    XMotivo := RetInfevento.XMotivo;
  end;
end;

{ TEnvioResposta }
constructor TEnvioResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnvio, ATipo, AFormato);
end;

destructor TEnvioResposta.Destroy;
begin
  if Assigned(FItemRetorno) then FreeAndNil(FItemRetorno);
end;

procedure TEnvioResposta.Processar(const ACBrNFe: TACBrNFe);
Var
  NumeroNota: string;
begin
  if Assigned(FItemRetorno) then FreeAndNil(FItemRetorno);

  with ACBrNFe.WebServices do
  begin
    Versao := Enviar.versao;
    TpAmb := TpAmbToStr(Enviar.TpAmb);
    verAplic := Enviar.verAplic;
    CStat := Enviar.cStat;
    XMotivo := Enviar.xMotivo;
    CUF := Enviar.cUF;
    nRec := Enviar.Recibo;
    DhRecbto := Enviar.dhRecbto;
    Tmed := Enviar.TMed;
    Msg := Enviar.Msg;
    NProt := Enviar.Protocolo;
  end;

  if (ACBrNFe.WebServices.Enviar.Sincrono) and (ACBrNFe.NotasFiscais.Count > 0) then
  begin
    NumeroNota := IntToStr(ExtrairNumeroChaveAcesso(ACBrNFe.NotasFiscais.Items[0].NFe.procNFe.chNFe));
    if Trim(NumeroNota) = '' then Exit;
    if Trim(ACBrNFe.NotasFiscais.Items[0].NFe.procNFe.nProt) = '' then Exit;

    FItemRetorno := TRetornoItemResposta.Create('NFe' + NumeroNota, Tipo, Formato);
    FItemRetorno.Id := 'ID' + ACBrNFe.NotasFiscais.Items[0].NFe.procNFe.nProt;
    FItemRetorno.tpAmb := TpAmbToStr(ACBrNFe.NotasFiscais.Items[0].NFe.procNFe.tpAmb);
    FItemRetorno.verAplic := ACBrNFe.NotasFiscais.Items[0].NFe.procNFe.verAplic;
    FItemRetorno.chDFe := ACBrNFe.NotasFiscais.Items[0].NFe.procNFe.chNFe;
    FItemRetorno.dhRecbto := ACBrNFe.NotasFiscais.Items[0].NFe.procNFe.dhRecbto;
    FItemRetorno.nProt := ACBrNFe.NotasFiscais.Items[0].NFe.procNFe.nProt;
    FItemRetorno.digVal := ACBrNFe.NotasFiscais.Items[0].NFe.procNFe.digVal;
    FItemRetorno.cStat := ACBrNFe.NotasFiscais.Items[0].NFe.procNFe.cStat;
    FItemRetorno.xMotivo := ACBrNFe.NotasFiscais.Items[0].NFe.procNFe.xMotivo;
    FItemRetorno.XML := ACBrNFe.NotasFiscais.Items[0].NFe.procNFe.XML_prot;
  end;
end;

{ TStatusServicoResposta }
constructor TStatusServicoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespStatus, ATipo, AFormato);
end;

procedure TStatusServicoResposta.Processar(const ACBrNFe: TACBrNFe);
begin
  with ACBrNFe.WebServices do
  begin
    Msg := StatusServico.Msg;
    Versao := StatusServico.versao;
    TpAmb := TpAmbToStr(StatusServico.TpAmb);
    VerAplic := StatusServico.VerAplic;
    CStat := StatusServico.CStat;
    XMotivo := StatusServico.XMotivo;
    CUF := StatusServico.CUF;
    DhRecbto := StatusServico.DhRecbto;
    TMed := StatusServico.TMed;
    DhRetorno := StatusServico.DhRetorno;
    XObs := StatusServico.XObs;
  end;
end;

{ TInutilizarNFeResposta }
constructor TInutilizarNFeResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespInutilizacao, ATipo, AFormato);
end;

procedure TInutilizarNFeResposta.Processar(const ACBrNFe: TACBrNFe);
begin
  with ACBrNFe.WebServices do
  begin
    Msg := Inutilizacao.Msg;
    Versao := Inutilizacao.versao;
    TpAmb := TpAmbToStr(Inutilizacao.TpAmb);
    VerAplic := Inutilizacao.VerAplic;
    CStat := Inutilizacao.CStat;
    XMotivo := Inutilizacao.XMotivo;
    CUF := Inutilizacao.cUF;
    DhRecbto := Inutilizacao.DhRecbto;
    NomeArquivo := Inutilizacao.NomeArquivo;
    NProt := Inutilizacao.Protocolo;
    Xml := Inutilizacao.XML_ProcInutNFe;

  end;
end;

{ TConsultaNFeResposta }
constructor TConsultaNFeResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta, ATipo, AFormato);

  FInfCan := nil;
  FEventos := TObjectList.Create;
end;

destructor TConsultaNFeResposta.Destroy;
begin
  if Assigned(FInfCan) then FInfCan.Free;

  FEventos.Clear;
  FEventos.Free;

  inherited Destroy;
end;

procedure TConsultaNFeResposta.Processar(const ACBrNFe: TACBrNFe);
Var
  I: Integer;
  ProcEvento: TConsultaNFeProcEventoResposta;
begin
  with ACBrNFe.WebServices do
  begin
    Msg := Consulta.Msg;
    Versao := Consulta.versao;
    TpAmb := TpAmbToStr(Consulta.TpAmb);
    VerAplic := Consulta.VerAplic;
    CStat := Consulta.CStat;
    XMotivo := Consulta.XMotivo;
    CUF := Consulta.CUF;
    DhRecbto := Consulta.DhRecbto;
    ChNFe := Consulta.NFeChave;
    NProt := Consulta.Protocolo;
    DigVal := Consulta.protNFe.digVal;
    cMsg := Consulta.protNFe.cMsg;
    xMsg := Consulta.ProtNFe.xMsg;
  end;

  if NaoEstaVazio(Trim(ACBrNFe.WebServices.Consulta.retCancNFe.nProt)) then
  begin
    InfCan := TConsultaNFeInfCanResposta.Create(Tipo, Formato);
    InfCan.Processar(ACBrNFe);
  end;

  with ACBrNFe.WebServices do
  begin
    for I:= 0 to Consulta.procEventoNFe.Count-1 do
    begin
      ProcEvento := TConsultaNFeProcEventoResposta.Create(I + 1, Tipo, Formato);
      ProcEvento.Processar(Consulta.procEventoNFe.Items[I]);
      FEventos.Add(ProcEvento);
    end;
  end;
end;

end.

