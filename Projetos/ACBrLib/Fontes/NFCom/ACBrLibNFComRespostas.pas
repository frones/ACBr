{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrLibNFComRespostas;

interface

uses
  Classes, SysUtils, Contnrs, pcnConversao,
  ACBrNFComEventoClass, ACBrNFComConversao, ACBrXmlBase, ACBrBase,
  ACBrLibResposta, ACBrLibConfig,
  ACBrLibConsReciDFe, ACBrNFCom;

type
  { TLibNFComResposta }
  TLibNFComResposta = class(TACBrLibRespostaBase)
    private
      FMsg: String;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    published
      property Msg: String read FMsg write FMsg;
  end;

  { TLibNFComServiceResposta }
  TLibNFComServiceResposta = class abstract(TACBrLibResposta<TACBrNFCom>)
    private
      FMsg: String;
      Fversao: String;
      FtpAmb: TACBrTipoAmbiente;
      FverAplic: String;
      FcStat: Integer;
      FxMotivo: String;
      FcUF: Integer;
      FdhRecbto: TDateTime;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

      procedure Processar(const ACBrNFCom: TACBrNFCom); virtual; abstract; reintroduce;

    published
      property Msg: String read FMsg write FMsg;
      property Versao: String read Fversao write Fversao;
      property tpAmb: TACBrTipoAmbiente read FtpAmb write FtpAmb;
      property VerAplic: String read FverAplic write FverAplic;
      property CStat: Integer read FcStat write FcStat;
      property XMotivo: String read FxMotivo write FxMotivo;
      property CUF: Integer read FcUF write FcUF;
      property DhRecbto: TDateTime read FdhRecbto write FdhRecbto;
  end;

  { TStatusServicoResposta }
  TStatusServicoResposta = class(TLibNFComServiceResposta)
    private
      FTMed: Integer;
      FdhRetorno: TDateTime;
      FxObs: String;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      procedure Processar(const ACBrNFCom: TACBrNFCom); override;

    published
      property TMed: Integer read FTMed write FTMed;
      property DhRetorno: TDateTime read FdhRetorno write FdhRetorno;
      property XObs: String read FxObs write FxObs;
  end;

  { TEnvioResposta }
  TEnvioResposta = class(TLibNFComServiceResposta)
    private
      FRecibo: String;
      Fversao: String;
      FTpAmb: TACBrTipoAmbiente;
      FverAplic: String;
      FcStat: Integer;
      FcUF: Integer;
      FxMotivo: String;
      FdhRecbto: TDateTime;
      FtMed: Integer;
      FItemRetorno: TRetornoItemResposta;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Processar(const ACBrNFCom: TACBrNFCom); override;

    published
      property Recibo: String read FRecibo write FRecibo;
      property versao: String read Fversao write Fversao;
      property TpAmb: TACBrTipoAmbiente read FTpAmb write FTpAmb;
      property verAplic: String read FverAplic write FverAplic;
      property cStat: Integer read FcStat write FcStat;
      property cUF: Integer read FcUF write FcUF;
      property xMotivo: String read FxMotivo write FxMotivo;
      property dhRecbto: TDateTime read FdhRecbto write FdhRecbto;
      property tMed: Integer read FtMed write FtMed;
      property ItemRetorno: TRetornoItemResposta read FItemRetorno;
  end;

  { TConsultaNFComResposta }
  TConsultaNFComResposta = class(TLibNFComServiceResposta)
    private
      FChNFCom: String;
      FNProt: String;
      FDigVal: String;
      FcMsg: Integer;
      FxMsg: String;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Processar(const ACBrNFCom: TACBrNFCom); override;

    published
      property ChNFCom: String read FChNFCom write FChNFCom;
      property NProt: String read FNProt write FNProt;
      property DigVal: String read FDigVal write FDigVal;
      property cMsg: Integer read FcMsg write FcMsg;
      property xMsg: String read FxMsg write FxMsg;
  end;

  { TCancelamentoResposta }
  TCancelamentoResposta = class(TLibNFComServiceResposta)
    private
      FchNFCom: String;
      FnProt: String;
      FtpEvento: String;
      FxEvento: String;
      FnSeqEvento: Integer;
      FCNPJDest: String;
      FemailDest: String;
      Fxml: String;
      FArquivo: String;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      procedure Processar(const ACBrNFCom: TACBrNFCom); override;

    published
      property chNFCom: string read FchNFCom write FchNFCom;
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
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao);

      procedure Processar(const RetInfEvento: TRetInfEvento);

    published
      property Id: String read FId write FId;
      property NomeArquivo: String read FNomeArquivo write FNomeArquivo;
      property tpAmb: TACBrTipoAmbiente read FtpAmb write FtpAmb;
      property verAplic: String read FverAplic write FverAplic;
      property cOrgao: Integer read FcOrgao write FcOrgao;
      property cStat: Integer read FcStat write FcStat;
      property xMotivo: String read FxMotivo write FxMotivo;
      property chNFCom: String read FchNFCom write FchNFCom;
      property tpEvento: TpcnTpEvento read FtpEvento write FtpEvento;
      property xEvento: String read FxEvento write FxEvento;
      property nSeqEvento: Integer read FnSeqEvento write FnSeqEvento;
      property CNPJDest: String read FCNPJDest write FCNPJDest;
      property emailDest: String read FemailDest write FemailDest;
      property cOrgaoAutor: Integer read FcOrgaoAutor write FcOrgaoAutor;
      property dhRegEvento: TDateTime read FdhRegEvento write FdhRegEvento;
      property nProt: String read FnProt write FnProt;
      property XML: AnsiString read FXML write FXML;
  end;

  { TEventoResposta }
  TEventoResposta = class(TLibNFComServiceResposta)
    private
      FidLote: Integer;
      FcStat: Integer;
      FxMotivo: String;
      FTpAmb: TACBrTipoAmbiente;
      FVersao: String;
      FXmlEnvio: String;
      FItems: TACBrObjectList;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

      procedure Processar(const ACBrNFCom: TACBrNFCom); override;
      destructor Destroy; override;

    published
      property idLote: Integer read FidLote write FidLote;
      property cStat: Integer read FcStat write FcStat;
      property xMotivo: String read FxMotivo write FxMotivo;
      property TpAmb: TACBrTipoAmbiente read FTpAmb write FTpAmb;
      property Versao: String read FVersao write FVersao;
      property XmlEnvio: String read FXmlEnvio write FXmlEnvio;
  end;

implementation

uses
  ACBrDFeUtil,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrLibNFComConsts;

{ TLibNFComResposta }
constructor TLibNFComResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
    inherited Create(ASessao, ATipo, AFormato);
end;

{ TLibNFComServiceResposta }
constructor TLibNFComServiceResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
    inherited Create(ASessao, ATipo, AFormato);
end;

{ TStatusServicoResposta }
constructor TStatusServicoResposta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
    inherited Create(CSessaoRespStatus, ATipo, AFormato);
end;

procedure TStatusServicoResposta.Processar(const ACBrNFCom: TACBrNFCom);
begin
    with ACBrNFCom.WebServices do
    begin
      Msg := StatusServico.Msg;
      Versao := StatusServico.versao;
      TpAmb := TACBrTipoAmbiente(StatusServico.TpAmb);
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

{ TEnvioResposta }
constructor TEnvioResposta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnvio, ATipo, AFormato);
end;

destructor TEnvioResposta.Destroy;
begin
  if Assigned(FItemRetorno) then FreeAndNil(FItemRetorno);
end;

procedure TEnvioResposta.Processar(const ACBrNFCom: TACBrNFCom);
var
  NumeroNota: String;
begin
  if Assigned(FItemRetorno) then FreeAndNil(FItemRetorno);

  with ACBrNFCom.WebServices do
  begin
    Recibo   := Enviar.Recibo;
    Versao   := Enviar.versao;
    tpAmb    := TACBrTipoAmbiente(Enviar.TpAmb);
    VerAplic := Enviar.verAplic;
    CStat    := Enviar.cStat;
    XMotivo  := Enviar.xMotivo;
    CUF      := Enviar.cUF;
    DhRecbto := Enviar.dhRecbto;
    tMed     := Enviar.TMed;
    Msg      := Enviar.Msg;
  end;

  if (ACBrNFCom.NotasFiscais.Count > 0) then
  begin
    NumeroNota := IntToStr(ExtrairNumeroChaveAcesso(ACBrNFCom.NotasFiscais.Items[0].NFCom.procNFCom.chDFe));
    if Trim(NumeroNota) = '' then Exit;
    if Trim(ACBrNFCom.NotasFiscais.Items[0].NFCom.procNFCom.nProt) = '' then Exit;

    FItemRetorno          := TRetornoItemResposta.Create('NFCom' + NumeroNota, Tipo, Codificacao);
    FItemRetorno.Id       := 'ID' + ACBrNFCom.NotasFiscais.Items[0].NFCom.procNFCom.nProt;
    FItemRetorno.tpAmb    := TipoAmbienteToStr(ACBrNFCom.NotasFiscais.Items[0].NFCom.procNFCom.tpAmb);
    FItemRetorno.verAplic := ACBrNFCom.NotasFiscais.Items[0].NFCom.procNFCom.verAplic;
    FItemRetorno.chDFe    := ACBrNFCom.NotasFiscais.Items[0].NFCom.procNFCom.chDFe;
    FItemRetorno.dhRecbto := ACBrNFCom.NotasFiscais.Items[0].NFCom.procNFCom.dhRecbto;
    FItemRetorno.nProt    := ACBrNFCom.NotasFiscais.Items[0].NFCom.procNFCom.nProt;
    FItemRetorno.digVal   := ACBrNFCom.NotasFiscais.Items[0].NFCom.procNFCom.digVal;
    FItemRetorno.cStat    := ACBrNFCom.NotasFiscais.Items[0].NFCom.procNFCom.cStat;
    FItemRetorno.xMotivo  := ACBrNFCom.NotasFiscais.Items[0].NFCom.procNFCom.xMotivo;
    FItemRetorno.XML      := ACBrNFCom.NotasFiscais.Items[0].NFCom.procNFCom.XML_prot;
    FItemRetorno.NomeArq  := ACBrNFCom.NotasFiscais.Items[0].NomeArq;
  end;
end;

{ TConsultaNFComResposta }
constructor TConsultaNFComResposta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta, ATipo, AFormato);
end;

destructor TConsultaNFComResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TConsultaNFComResposta.Processar(const ACBrNFCom: TACBrNFCom);
begin
  with ACBrNFCom.WebServices do
  begin
    Msg      := Consulta.Msg;
    Versao   := Consulta.versao;
    tpAmb    := TACBrTipoAmbiente(Consulta.TpAmb);
    VerAplic := Consulta.verAplic;
    CStat    := Consulta.cStat;
    XMotivo  := Consulta.XMotivo;
    CUF      := Consulta.cUF;
    DhRecbto := Consulta.DhRecbto;
    ChNFCom  := Consulta.NFComChave;
    NProt    := Consulta.Protocolo;
    DigVal   := Consulta.protNFCom.digVal;
    cMsg     := Consulta.protNFCom.cMsg;
    xMsg     := Consulta.protNFCom.xMsg;
  end;
end;

{ TCancelamentoResposta }
constructor TCancelamentoResposta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespCancelamento, ATipo, AFormato);
end;

procedure TCancelamentoResposta.Processar(const ACBrNFCom: TACBrNFCom);
begin
  with ACBrNFCom.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0] do
  begin
    Arquivo    := RetInfEvento.NomeArquivo;
    Versao     := RetInfEvento.verAplic;
    tpAmb      := TACBrTipoAmbiente(RetInfEvento.tpAmb);
    VerAplic   := RetInfEvento.verAplic;
    CStat      := RetInfevento.cStat;
    XMotivo    := RetInfEvento.xMotivo;
    CUF        := RetInfEvento.cOrgao;
    chNFCom    := RetInfEvento.chNFCom;
    DhRecbto   := RetInfEvento.dhRegEvento;
    nProt      := RetInfEvento.nProt;
    tpEvento   := TpEventoToStr(RetInfEvento.tpEvento);
    xEvento    := RetInfEvento.xEvento;
    nSeqEvento := RetInfEvento.nSeqEvento;
    CNPJDest   := RetInfEvento.CNPJDest;
    emailDest  := RetInfEvento.emailDest;
    XML        := RetInfEvento.XML;
  end;
end;

{ TEventoResposta }
constructor TEventoResposta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEvento, ATipo, AFormato);
  FItems := TACBrObjectList.Create(True);
end;

destructor TEventoResposta.Destroy;
begin
  FItems.Destroy;
  inherited Destroy;
end;

procedure TEventoResposta.Processar(const ACBrNFCom: TACBrNFCom);
var
  i: Integer;
  Item: TEventoItemResposta;
begin
  with ACBrNFCom.WebServices do
  begin
    idLote   := EnvEvento.idLote;
    cStat    := EnvEvento.cStat;
    xMotivo  := EnvEvento.xMotivo;
    TpAmb    := TACBrTipoAmbiente(EnvEvento.TpAmb);
    Versao   := EnvEvento.EventoRetorno.versao;
    XmlEnvio := EnvEvento.EventoRetorno.XmlRetorno;

    with EnvEvento.EventoRetorno.retEvento do
    begin
      for i := 0 to EnvEvento.EventoRetorno.retEvento.Count - 1 do
      begin
        Item := TEventoItemResposta.Create('EVENTO' + Trim(IntToStrZero(i + 1, 3)), Tipo, Codificacao);
        Item.Processar(EnvEvento.EventoRetorno.retEvento.Items[i].RetInfevento);
        FItems.Add(Item);
      end;
    end;
  end;
end;

{ TEventoItemResposta }
constructor TEventoItemResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

procedure TEventoItemResposta.Processar(const RetInfEvento: TRetInfEvento);
begin
  Id          := RetInfEvento.Id;
  NomeArquivo := RetInfEvento.NomeArquivo;
  tpAmb       := TACBrTipoAmbiente(RetInfEvento.TpAmb);
  verAplic    := RetInfEvento.verAplic;
  cOrgao      := RetInfEvento.cOrgao;
  cStat       := RetInfEvento.cStat;
  xMotivo     := RetInfEvento.xMotivo;
  chNFCom     := RetInfEvento.chNFCom;
  tpEvento    := RetInfEvento.tpEvento;
  xEvento     := RetInfEvento.xEvento;
  nSeqEvento  := RetInfEvento.nSeqEvento;
  CNPJDest    := RetInfEvento.CNPJDest;
  emailDest   := RetInfEvento.emailDest;
  cOrgaoAutor := RetInfEvento.cOrgaoAutor;
  dhRegEvento := RetInfEvento.dhRegEvento;
  nProt       := RetInfEvento.nProt;
  XML         := RetInfEvento.XML;
end;

end.

