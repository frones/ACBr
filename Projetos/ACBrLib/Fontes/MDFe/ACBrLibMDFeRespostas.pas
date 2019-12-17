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

unit ACBrLibMDFeRespostas;

interface

uses
  SysUtils, Classes, contnrs,
  pmdfeRetConsMDFeNaoEnc, pmdfeEventoMDFe,
  ACBrMDFe, ACBrLibResposta;

type

  { TLibMDFeResposta }

  TLibMDFeResposta = class(TACBrLibResposta<TACBrMDFe>)
  private
    Fversao: string;
    FtpAmb: string;
    FverAplic: string;
    FcStat: integer;
    FxMotivo: string;
    FcUF: integer;
    FMsg: string;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const Control: TACBrMDFe); override; abstract;

  published
    property Versao: string read Fversao write Fversao;
    property TpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property XMotivo: string read FxMotivo write FxMotivo;
    property CUF: integer read FcUF write FcUF;
    property Msg: string read FMsg write FMsg;

  end;

  { TStatusServicoResposta }

  TStatusServicoResposta = class(TLibMDFeResposta)
  private
    FMsg: string;
    FdhRecbto: TDateTime;
    FTMed: integer;
    FdhRetorno: TDateTime;
    FxObs: string;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const Control: TACBrMDFe); override;

  published
    property Msg: string read FMsg write FMsg;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property TMed: integer read FTMed write FTMed;
    property DhRetorno: TDateTime read FdhRetorno write FdhRetorno;
    property XObs: string read FxObs write FxObs;

  end;

  { TConsultaResposta }

  TConsultaResposta = class(TLibMDFeResposta)
  private
    FMsg: string;
    FchMDFe: string;
    FdhRecbto: TDateTime;
    FnProt: string;
    FdigVal: string;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const Control: TACBrMDFe); override;

  published
    property Msg: string read FMsg write FMsg;
    property ChMDFe: string read FchMDFe write FchMDFe;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property NProt: string read FnProt write FnProt;
    property DigVal: string read FdigVal write FdigVal;

  end;

  { TCancelamentoResposta }

  TCancelamentoResposta = class(TLibMDFeResposta)
  private
    FArquivo: string;
    FchMDFe: string;
    FdhRecbto: TDateTime;
    FnProt: string;
    FtpEvento: string;
    FxEvento: string;
    FnSeqEvento: integer;
    FCNPJDest: string;
    FemailDest: string;
    Fxml: string;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const Control: TACBrMDFe); override;

  published
    property ChMDFe: string read FchMDFe write FchMDFe;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property NProt: string read FnProt write FnProt;
    property TpEvento: string read FtpEvento write FtpEvento;
    property XEvento: string read FxEvento write FxEvento;
    property NSeqEvento: integer read FnSeqEvento write FnSeqEvento;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property EmailDest: string read FemailDest write FemailDest;
    property XML: string read Fxml write Fxml;
    property Arquivo: string read FArquivo write FArquivo;

  end;

  { TNaoEncerradosRespostaItem }

  TNaoEncerradosRespostaItem = class(TACBrLibRespostaBase)
  private
    Fmsg: string;
    FCNPJCPF: string;
    FchMDFe: string;
    FnProt: string;

  public
    constructor Create(const ItemID: Integer; const ATipo: TACBrLibRespostaTipo;
        const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const CNPJCPF: String; const Item: TRetInfMDFeCollectionItem);

  published
    property Msg: string read Fmsg write Fmsg;
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property ChMDFe: string read FchMDFe write FchMDFe;
    property NProt: string read FnProt write FnProt;

  end;

  { TNaoEncerradosResposta }

  TNaoEncerradosResposta = class(TLibMDFeResposta)
  private
    FItems: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const Control: TACBrMDFe); override;

  published
    property Items: TObjectList read FItems;

  end;

  { TEncerramentoResposta }

  TEncerramentoResposta = class(TLibMDFeResposta)
  private
    FArquivo: string;
    FchMDFe: string;
    FdhRecbto: TDateTime;
    FnProt: string;
    FtpEvento: string;
    FxEvento: string;
    FnSeqEvento: integer;
    FCNPJDest: string;
    FemailDest: string;
    Fxml: string;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const Control: TACBrMDFe); override;

  published
    property ChMDFe: string read FchMDFe write FchMDFe;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property NProt: string read FnProt write FnProt;
    property TpEvento: string read FtpEvento write FtpEvento;
    property XEvento: string read FxEvento write FxEvento;
    property NSeqEvento: integer read FnSeqEvento write FnSeqEvento;
    property CNPJDest: string read FCNPJDest write FCNPJDest;
    property EmailDest: string read FemailDest write FemailDest;
    property XML: string read Fxml write Fxml;
    property Arquivo: string read FArquivo write FArquivo;

  end;

  { TEnvioResposta }

  TEnvioResposta = class(TLibMDFeResposta)
  private
    Fmsg: string;
    FdhRecbto: TDateTime;
    FtMed: integer;
    FnRec: string;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const Control: TACBrMDFe); override;

  published
    property Msg: string read Fmsg write Fmsg;
    property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
    property TMed: integer read FtMed write FtMed;
    property NRec: string read FnRec write FnRec;

  end;

  { TEventoItemResposta }

  TEventoItemResposta = class(TACBrLibRespostaBase)
  private
    Fversao: string;
    FtpAmb: string;
    FverAplic: string;
    FcStat: integer;
    FxMotivo: string;
    FcUF: integer;
    FMsg: string;
    Farquivo: String;
    FchMDFe: string;
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
    constructor Create(const ItemID: Integer; const ATipo: TACBrLibRespostaTipo;
         const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const Item: TRetInfEvento);

  published
    property Versao: string read Fversao write Fversao;
    property TpAmb: string read FtpAmb write FtpAmb;
    property VerAplic: string read FverAplic write FverAplic;
    property CStat: integer read FcStat write FcStat;
    property XMotivo: string read FxMotivo write FxMotivo;
    property CUF: integer read FcUF write FcUF;
    property Msg: string read FMsg write FMsg;
    property chMDFe: string read FchMDFe write FchMDFe;
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

  TEventoResposta = class(TLibMDFeResposta)
  private
    FidLote: Integer;
    FcOrgao: Integer;
    FItems: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const Control: TACBrMDFe); override;

  published
    property idLote: Integer read FidLote write FidLote;
    property cOrgao: Integer read FcOrgao write FcOrgao;
    property Items: TObjectList read FItems;

  end;

implementation

uses
  pcnConversao, pcnAuxiliar, ACBrLibMDFeConsts;

{ TEventoItemResposta }

constructor TEventoItemResposta.Create(const ItemID: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create('Evento' + Trim(IntToStrZero(ItemID +1, 3)), ATipo, AFormato);
end;

procedure TEventoItemResposta.Processar(const Item: TRetInfEvento);
begin
  with Item do
  begin
    Self.Id := Id;
    Self.tpAmb := TpAmbToStr(tpAmb);
    Self.verAplic := verAplic;
    Self.cOrgao := cOrgao;
    Self.cStat := cStat;
    Self.xMotivo := xMotivo;
    Self.chMDFe := chMDFe;
    Self.tpEvento := TpEventoToStr(tpEvento);
    Self.xEvento := xEvento;
    Self.nSeqEvento := nSeqEvento;
    Self.CNPJDest := CNPJDest;
    Self.emailDest := emailDest;
    Self.dhRegEvento := dhRegEvento;
    Self.nProt := nProt;
    Self.Arquivo := NomeArquivo;
    Self.XML := XML;
  end;
end;

{ TEventoResposta }

constructor TEventoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEvento, ATipo, AFormato);

  FItems := TObjectList.Create(true);
end;

destructor TEventoResposta.Destroy;
begin
  FItems.Clear;
  FItems.Free;

  inherited Destroy;
end;

procedure TEventoResposta.Processar(const Control: TACBrMDFe);
Var
  i: Integer;
  Item: TEventoItemResposta;
begin
  with Control.WebServices.EnvEvento.EventoRetorno do
  begin
    Self.VerAplic := VerAplic;
    Self.tpAmb := TpAmbToStr(tpAmb);
    Self.CStat := cStat;
    Self.XMotivo := XMotivo;
    Self.idLote := IdLote;
    Self.cOrgao := cOrgao;

    for i := 0 to retEvento.Count - 1 do
    begin
      Item := TEventoItemResposta.Create(i+1, Tipo, Formato);
      Item.Processar(retEvento.Items[i].RetInfEvento);
      FItems.Add(Item);
    end;
  end;
end;

{ TPadraoResposta }

constructor TLibMDFeResposta.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioResposta }

constructor TEnvioResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnvio, ATipo, AFormato);
end;

procedure TEnvioResposta.Processar(const Control: TACBrMDFe);
begin
  with Control.WebServices.Enviar do
  begin
    Self.Versao := verAplic;
    Self.TpAmb := TpAmbToStr(TpAmb);
    Self.verAplic := verAplic;
    Self.CStat := cStat;
    Self.XMotivo := xMotivo;
    Self.CUF := cUF;
    Self.nRec := Recibo;
    Self.DhRecbto := dhRecbto;
    Self.Tmed := TMed;
    Self.Msg := Msg;
  end;
end;

{ TNaoEncerradosRespostaItem }

constructor TNaoEncerradosRespostaItem.Create(const ItemID: Integer; const ATipo: TACBrLibRespostaTipo;
        const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoNaoEncerrados + Trim(IntToStrZero(ItemID +1, 3)), ATipo, AFormato);
end;

procedure TNaoEncerradosRespostaItem.Processar(const CNPJCPF: String; const Item: TRetInfMDFeCollectionItem);
begin
  Self.CNPJCPF := CNPJCPF;
  Self.ChMDFe := Item.chMDFe;
  Self.NProt := Item.nProt;
end;

{ TNaoEncerradosResposta }

constructor TNaoEncerradosResposta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoNaoEncerrados, ATipo, AFormato);

  FItems := TObjectList.Create(true);
end;

destructor TNaoEncerradosResposta.Destroy;
begin
  FItems.Clear;
  FItems.Free;

  inherited Destroy;
end;

procedure TNaoEncerradosResposta.Processar(const Control: TACBrMDFe);
Var
  i: Integer;
  Item: TNaoEncerradosRespostaItem;
begin
  with Control.WebServices.ConsMDFeNaoEnc do
  begin
    Self.Versao := verAplic;
    Self.TpAmb := TpAmbToStr(TpAmb);
    Self.VerAplic := VerAplic;
    Self.CStat := cStat;
    Self.XMotivo := XMotivo;
    Self.CUF := cUF;

    for i := 0 to InfMDFe.Count - 1 do
    begin
      Item := TNaoEncerradosRespostaItem.Create(i+1, Tipo, Formato);
      Item.Processar(CNPJCPF, InfMDFe.Items[i]);
      FItems.Add(Item);
    end;
  end;
end;

{ TEncerramentoResposta }

constructor TEncerramentoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEncerramento, ATipo, AFormato);
end;

procedure TEncerramentoResposta.Processar(const Control: TACBrMDFe);
begin
  with Control.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento do
  begin
    Self.Versao := verAplic;
    Self.TpAmb := TpAmbToStr(TpAmb);
    Self.VerAplic := VerAplic;
    Self.CStat := cStat;
    Self.XMotivo := XMotivo;
    Self.CUF := cOrgao;
    Self.ChMDFe := chMDFe;
    Self.DhRecbto := dhRegEvento;
    Self.NProt := nProt;
    Self.TpEvento := TpEventoToStr(tpEvento);
    Self.xEvento := xEvento;
    Self.nSeqEvento := nSeqEvento;
    Self.CNPJDest := CNPJDest;
    Self.emailDest := emailDest;
    Self.XML := XML;
    Self.Arquivo := NomeArquivo;
  end;
end;

{ TCancelamentoResposta }

constructor TCancelamentoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespCancelamento, ATipo, AFormato);
end;

procedure TCancelamentoResposta.Processar(const Control: TACBrMDFe);
begin
  if Control.WebServices.EnvEvento.EventoRetorno.retEvento.Count > 0 then
  begin
    with Control.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento do
    begin
      Self.Versao := verAplic;
      Self.TpAmb := TpAmbToStr(TpAmb);
      Self.VerAplic := VerAplic;
      Self.CStat := cStat;
      Self.XMotivo := XMotivo;
      Self.CUF := cOrgao;
      Self.ChMDFe := chMDFe;
      Self.DhRecbto := dhRegEvento;
      Self.NProt := nProt;
      Self.TpEvento := TpEventoToStr(tpEvento);
      Self.xEvento := xEvento;
      Self.nSeqEvento := nSeqEvento;
      Self.CNPJDest := CNPJDest;
      Self.emailDest := emailDest;
      Self.XML := XML;
      Self.Arquivo := NomeArquivo;
    end;
  end;
end;

{ TConsultaResposta }

constructor TConsultaResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta, ATipo, AFormato);
end;

procedure TConsultaResposta.Processar(const Control: TACBrMDFe);
begin
  with Control.WebServices.Consulta do
  begin
    Self.Versao := verAplic;
    Self.TpAmb := TpAmbToStr(TpAmb);
    Self.VerAplic := VerAplic;
    Self.CStat := cStat;
    Self.XMotivo := XMotivo;
    Self.CUF := cUF;
    Self.ChMDFe := MDFeChave;
    Self.DhRecbto := dhRecbto;
    Self.NProt := Protocolo;
    Self.digVal := protMDFe.digVal;
    Self.Msg := Msg;
  end;
end;

 { TStatusServicoResposta }

constructor TStatusServicoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespStatus, ATipo, AFormato);
end;

procedure TStatusServicoResposta.Processar(const Control: TACBrMDFe);
begin
  with Control.WebServices.StatusServico do
  begin
    Self.Versao := versao;
    Self.TpAmb := TpAmbToStr(TpAmb);
    Self.VerAplic := VerAplic;
    Self.CStat := cStat;
    Self.XMotivo := XMotivo;
    Self.CUF := cUF;
    Self.DhRecbto := dhRecbto;
    Self.tMed := TMed;
    Self.dhRetorno := dhRetorno;
    Self.xObs := xObs;
    Self.Msg := Msg;
  end;
end;

end.

