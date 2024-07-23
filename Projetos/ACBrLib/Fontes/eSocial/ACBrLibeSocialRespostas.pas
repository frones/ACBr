{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
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
unit ACBrLibeSocialRespostas;

interface

uses
  Classes, SysUtils, contnrs, ACBrLibResposta, ACBrLibeSocialConsts, ACBreSocial,
  pcnAuxiliar, pcesConversaoeSocial;

type

  { TPadraoeSocialResposta }

  TPadraoeSocialResposta = class abstract(TACBrLibResposta<TACBreSocial>)
  private
    FCodigo: Integer;
    FMensagem: String;

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const Control: TACBreSocial); virtual; abstract; reintroduce;

  published
    property Codigo: Integer read FCodigo write FCodigo;
    property Mensagem: String read FMensagem write FMensagem;

  end;

   { TOcorrenciaResposta }

  TOcorrenciaResposta = class(TACBrLibRespostaBase)
  private
    FCodigo: Integer;
    FMensagem: String;
    FCodigoOco: Integer;
    FDescricao: String;
    FTipo: Integer;
    FLocalizacao: String;

  public
    constructor Create(const ItemID: Integer; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBreSocial: TACBreSocial; const ACont: Integer );

  published
    property Codigo: Integer read FCodigo write FCodigo;
    property Mensagem: String read FMensagem write FMensagem;
    property CodigoOco: Integer read FCodigoOco write FCodigoOco;
    property Descricao: String read FDescricao write FDescricao;
    property Tipo: Integer read FTipo write FTipo;
    property Localizacao: String read FLocalizacao write FLocalizacao;

  end;

  { TEnvioResposta }

  TEnvioResposta = class(TPadraoeSocialResposta)
  private
    FTpInscEmpreg: String;
    FNrInscEmpreg: String;
    FTpInscTransm: String;
    FNrInscTransm: String;
    FDhRecepcao: TDateTime;
    FVersaoAplic: String;
    FProtocolo: String;
    FPathNome: String;
    FItemOcorrencia: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ACBreSocial: TACBreSocial); override;

  published
    property TpInscEmpreg: String read FTpInscEmpreg write FTpInscEmpreg;
    property NrInscEmpreg: String read FNrInscEmpreg write FNrInscEmpreg;
    property TpInscTransm: String read FTpInscTransm write FTpInscTransm;
    property NrInscTransm: String read FNrInscTransm write FNrInscTransm;
    property DhRecepcao: TDateTime read FDhRecepcao write FDhRecepcao;
    property VersaoAplic: String read FVersaoAplic write FVersaoAplic;
    property Protocolo: String read FProtocolo write FProtocolo;
    property PathNome: String read FPathNome write FPathNome;
    property ItemOcorrencia: TObjectList read FItemOcorrencia;

  end;

  {TOcorrenciaConsulta}

  TOcorrenciaConsulta = class(TACBrLibRespostaBase)
  private
    FCodigoOco: Integer;
    FDescricao: String;
    FTipo: Integer;
    FLocalizacao: String;

  public
    constructor Create(const ItemID: Integer; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBreSocial: TACBreSocial; const ACont, AContOcor: Integer );

  published
    property CodigoOco: Integer read FCodigoOco write FCodigoOco;
    property Descricao: String read FDescricao write FDescricao;
    property Tipo: Integer read FTipo write FTipo;
    property Localizacao: String read FLocalizacao write FLocalizacao;

  end;

  { TConsultaTotResposta }

  TConsultaTotResposta = class(TACBrLibRespostaBase)
  private
    FTipo : String;
    FID : String;
    FNrRecArqBase : String;

  public
    constructor Create(const ItemID: Integer; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBreSocial: TACBreSocial; const ACont, AContTot: Integer );

  published
    property Tipo: String read FTipo write FTipo;
    property ID: String read FID write FID;
    property NrRecArqBase: String read FNrRecArqBase write FNrRecArqBase;

  end;

  { TConsultaResposta }

  TConsultaResposta = class(TACBrLibRespostaBase)
  private
    FcdResposta: Integer;
    FdescResposta: String;
    FversaoAplicProcLote: String;
    FdhProcessamento: TDateTime;
    FnrRecibo: String;
    Fhash: String;
    FItemOcorrenciaConsulta: TObjectList;
    FItemTotais: TObjectList;

  public
    constructor Create(const ItemID: Integer; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ACBreSocial: TACBreSocial; const ACont: Integer);

  published
    property cdResposta: Integer read FcdResposta write FcdResposta;
    property descResposta: String read FdescResposta write FdescResposta;
    property versaoAplicProcLote: String read FversaoAplicProcLote write FversaoAplicProcLote;
    property dhProcessamento: TDateTime read FdhProcessamento write FdhProcessamento;
    property nrRecibo: String read FnrRecibo write FnrRecibo;
    property hash: String read Fhash write Fhash;
    property ItemOcorrenciaConsulta: TObjectList read FItemOcorrenciaConsulta;
    property ItemTotais: TObjectList read FItemTotais;

  end;

  {TOcorrenciaConsultaLote}

  TOcorrenciaConsultaLote = class(TACBrLibRespostaBase)
  private
    FCodigo: Integer;
    FMensagem: String;
    FCodigoOco: Integer;
    FDescricao: String;
    FTipo: Integer;
    FLocalizacao: String;

  public
    constructor Create(const ItemID: Integer; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBreSocial: TACBreSocial; const ACont: Integer );

  published
    property Codigo: Integer read FCodigo write FCodigo;
    property Mensagem: String read FMensagem write FMensagem;
    property CodigoOco: Integer read FCodigoOco write FCodigoOco;
    property Descricao: String read FDescricao write FDescricao;
    property Tipo: Integer read FTipo write FTipo;
    property Localizacao: String read FLocalizacao write FLocalizacao;

  end;

  { TConsultaIdentRecibo}

  TConsultaIdentRecibo = class(TACBrLibRespostaBase)
  private
    FIdEvento : String;
    FNRecibo : String;

  public
    constructor Create(const ItemID: Integer; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBreSocial: TACBreSocial; const ACont: Integer );

  published
    property IdEvento: String read FIdEvento write FIdEvento;
    property NRecibo: String read FNRecibo write FNRecibo;

  end;


  { TConsultaTotEventos}

  TConsultaTotEventos = class(TPadraoeSocialResposta)
  private
    FQtdeTotal : Integer;
    FDhUltimoEvento : TDateTime;
    FPathNome: String;
    FItemIdent: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBreSocial: TACBreSocial); override;
    destructor Destroy; override;

  published
    property QtdeTotal: Integer read FQtdeTotal write FQtdeTotal;
    property DhUltimoEvento: TDateTime read FDhUltimoEvento write FDhUltimoEvento;
    property PathNome: String read FPathNome write FPathNome;
    property ItemIdent: TObjectList read FItemIdent;

  end;

  { TConsulta }

  TConsulta = class(TPadraoeSocialResposta)
  private
    FTpInscEmpreg: String;
    FNrInscEmpreg: String;
    FTpInscTransm: String;
    FNrInscTransm: String;
    FDhRecepcao: TDateTime;
    FVersaoAplic: String;
    FProtocolo: String;
    FPathNome: String;
    FItemConsulta: TObjectList;
    FItemOcorrencia: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ACBreSocial: TACBreSocial);

  published
    property TpInscEmpreg: String read FTpInscEmpreg write FTpInscEmpreg;
    property NrInscEmpreg: String read FNrInscEmpreg write FNrInscEmpreg;
    property TpInscTransm: String read FTpInscTransm write FTpInscTransm;
    property NrInscTransm: String read FNrInscTransm write FNrInscTransm;
    property DhRecepcao: TDateTime read FDhRecepcao write FDhRecepcao;
    property VersaoAplic: String read FVersaoAplic write FVersaoAplic;
    property Protocolo: String read FProtocolo write FProtocolo;
    property PathNome: String read FPathNome write FPathNome;
    property ItemConsulta: TObjectList read FItemConsulta;
    property ItemOcorrencia: TObjectList read FItemOcorrencia;

  end;

  { TConsultaIdentEvento}

  TConsultaIdentEvento = class(TACBrLibRespostaBase)
  private
    FIdEvento : String;
    FNRecibo : String;
    FXML : String;

  public
    constructor Create(const ItemID: Integer; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBreSocial: TACBreSocial; const ACont: Integer );

  published
    property IdEvento: String read FIdEvento write FIdEvento;
    property NRecibo: String read FNRecibo write FNRecibo;
    property XML: String read FXML write FXML;

  end;

  { TConsultaEventos}

  TConsultaEventos = class(TPadraoeSocialResposta)
  private
    FPathNome: String;
    FItem: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBreSocial: TACBreSocial); override;
    destructor Destroy; override;

  published
    property PathNome: String read FPathNome write FPathNome;
    property Item: TObjectList read FItem;

  end;

implementation

uses
  pcesS5001, pcesS5002, pcesS5011, pcesS5012;

{ TConsultaEventos }

constructor TConsultaEventos.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta, ATipo, AFormato);
  FItem := TObjectList.Create(true);
end;

procedure TConsultaEventos.Processar(const ACBreSocial: TACBreSocial);
var
  i: Integer;
  Item : TConsultaIdentEvento;
begin
  PathNome := ACBreSocial.WebServices.DownloadEventos.PathNome;

  for i := 0 to ACBreSocial.WebServices.ConsultaLote.RetConsultaLote.Status.Ocorrencias.Count - 1 do
  begin
    Item := TConsultaIdentEvento.Create(i+1, Tipo, Formato);
    Item.Processar(ACBreSocial, i);
    FItem.Add(Item);

  end;

end;

destructor TConsultaEventos.Destroy;
begin
  FItem.Clear;
  FItem.Destroy;

  inherited Destroy;
end;

{ TConsultaIdentRecibo }

constructor TConsultaIdentRecibo.Create(const ItemID: Integer; const ATipo: TACBrLibRespostaTipo;
            const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsultaIdentEventosRecibo + IntToStrZero(ItemID,1), ATipo, AFormato);
end;

procedure TConsultaIdentRecibo.Processar(const ACBreSocial: TACBreSocial;
  const ACont: Integer);
begin
  with ACBreSocial.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt.RetIdentEvts.Items[ACont] do
  begin
    IdEvento:= Id;
    NRecibo:= nrRec;
  end;

end;

{ TOcorrenciaConsultaLote }

constructor TOcorrenciaConsultaLote.Create(const ItemID: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespOcorrencia + IntToStrZero(ItemID,1), ATipo, AFormato);
end;

procedure TOcorrenciaConsultaLote.Processar(const ACBreSocial: TACBreSocial;
  const ACont: Integer);
begin
  with ACBreSocial.WebServices.ConsultaLote.RetConsultaLote do
  begin
    Codigo      := Status.cdResposta;
    Mensagem    := Status.descResposta;
    CodigoOco   := Status.Ocorrencias.Items[ACont].Codigo;
    Descricao   := Status.Ocorrencias.Items[ACont].Descricao;
    Tipo        := Status.Ocorrencias.Items[ACont].Tipo;
    Localizacao := Status.Ocorrencias.Items[ACont].Localizacao;
  end;

end;

{ TOcorrenciaConsulta }

constructor TOcorrenciaConsulta.Create(const ItemID: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespOcorrencia + IntToStrZero(ItemID,1), ATipo, AFormato);
end;

procedure TOcorrenciaConsulta.Processar(const ACBreSocial: TACBreSocial;
  const ACont, AContOcor: Integer);
begin
  with ACBreSocial.WebServices.ConsultaLote.RetConsultaLote.RetEventos.Items[ACont] do
  begin
    CodigoOco   := Processamento.Ocorrencias.Items[AContOcor].Codigo;
    Descricao   := Processamento.Ocorrencias.Items[AContOcor].Descricao;
    Tipo        := Processamento.Ocorrencias.Items[AContOcor].Tipo;
    Localizacao := Processamento.Ocorrencias.Items[AContOcor].Localizacao;
  end;

end;

{ TConsulta }

constructor TConsulta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnvio, ATipo, AFormato);
  FItemConsulta := TObjectList.Create(true);
  FItemOcorrencia := TObjectList.Create(true);

end;

destructor TConsulta.Destroy;
begin
  FItemConsulta.Clear;
  FItemConsulta.Free;
  FItemOcorrencia.Clear;
  FItemOcorrencia.Free;

  inherited Destroy;
end;

procedure TConsulta.Processar(const ACBreSocial: TACBreSocial);
var
  i : integer;
  ItemCons : TConsultaResposta;
  ItemOcor : TOcorrenciaConsultaLote;
begin
  with ACBreSocial.WebServices.ConsultaLote.RetConsultaLote do
  begin
    Codigo       := Status.cdResposta;
    Mensagem     := Status.descResposta;
    TpInscEmpreg := eSTpInscricaoToStr(IdeEmpregador.TpInsc);
    NrInscEmpreg := IdeEmpregador.NrInsc;
    TpInscTransm := eSTpInscricaoToStr(IdeTransmissor.TpInsc);
    NrInscTransm := IdeTransmissor.NrInsc;
    DhRecepcao   := dadosRecLote.dhRecepcao;
    VersaoAplic  := dadosRecLote.versaoAplicRecepcao;
    Protocolo    := dadosRecLote.Protocolo;
    PathNome     := ACBreSocial.WebServices.ConsultaLote.PathNome;

    if Status.cdResposta in [201, 202] then
    begin
      for i := 0 to ACBreSocial.WebServices.ConsultaLote.RetConsultaLote.RetEventos.Count - 1 do
      begin
        ItemCons := TConsultaResposta.Create(i+1, Tipo, Formato);
        ItemCons.Processar(ACBreSocial, i);
        FItemConsulta.Add(ItemCons);
      end;
    end
    else
    begin
      for i := 0 to ACBreSocial.WebServices.ConsultaLote.RetConsultaLote.Status.Ocorrencias.Count - 1 do
      begin
        ItemOcor := TOcorrenciaConsultaLote.Create(i+1, Tipo, Formato);
        ItemOcor.Processar(ACBreSocial, i);
        FItemOcorrencia.Add(ItemOcor);
      end;

    end;

  end;

end;

{ TConsultaIdentEvento }

constructor TConsultaIdentEvento.Create(const ItemID: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsulta + IntToStrZero(ItemID,1), ATipo, AFormato);
end;

procedure TConsultaIdentEvento.Processar(const ACBreSocial: TACBreSocial;
  const ACont: Integer);
begin
  with ACBreSocial.WebServices.DownloadEventos.RetDownloadEvt do
  begin
    IdEvento := Arquivo.Items[ACont].Id;
    NRecibo := Arquivo.Items[ACont].nrRec;
    XML := Arquivo.Items[ACont].XML;

  end;

end;

{ TConsultaTotEventos }

constructor TConsultaTotEventos.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsultaIdentEventos, ATipo, AFormato);
  FItemIdent := TObjectList.Create(true);
end;

procedure TConsultaTotEventos.Processar(const ACBreSocial: TACBreSocial);
var
  i : Integer;
  itemRec : TConsultaIdentRecibo;
begin
  with ACBreSocial.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt do
  begin
    Codigo:= Status.cdResposta;
    Mensagem:= Status.descResposta;
    QtdeTotal:= RetIdentEvts.qtdeTotEvtsConsulta;
    DhUltimoEvento:= RetIdentEvts.dhUltimoEvtRetornado;
    PathNome := ACBreSocial.WebServices.ConsultaIdentEventos.PathNome;

    for i := 0 to RetIdentEvts.Count - 1 do
    begin
      ItemRec :=  TConsultaIdentRecibo.Create(i+1, Tipo, Formato);
      ItemRec.Processar(ACBreSocial, i);
      FItemIdent.Add(itemRec);
    end;

  end;

end;

destructor TConsultaTotEventos.Destroy;
begin
  FItemIdent.Clear;
  FItemIdent.Free;
  inherited Destroy;

end;

{ TConsultaTotResposta }

constructor TConsultaTotResposta.Create(const ItemID: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsultaTot + IntToStrZero(ItemID,1), ATipo, AFormato);

end;

procedure TConsultaTotResposta.Processar(const ACBreSocial: TACBreSocial;
  const ACont, AContTot: Integer);
var
  evtS5001: TS5001;
  evtS5002: TS5002;
  evtS5011: TS5011;
  evtS5012: TS5012;
begin
  with ACBreSocial.WebServices.ConsultaLote.RetConsultaLote.retEventos.Items[ACont] do
  begin
    Tipo := tot[AContTot].tipo;
    case tot[AContTot].Evento.TipoEvento of
      teS5001:
        begin
          evtS5001 := TS5001(tot[AContTot].Evento.GetEvento);
          ID  := evtS5001.EvtBasesTrab.Id;
          NrRecArqBase := evtS5001.EvtBasesTrab.IdeEvento.nrRecArqBase;
        end;
      teS5002:
        begin
          evtS5002 := TS5002(tot[AContTot].Evento.GetEvento);
          ID  := evtS5002.EvtirrfBenef.Id;
          NrRecArqBase := evtS5002.EvtirrfBenef.IdeEvento.nrRecArqBase;
        end;
      teS5011:
        begin
          evtS5011 := TS5011(tot[AContTot].Evento.GetEvento);
          ID  := evtS5011.EvtCS.Id;
          NrRecArqBase := evtS5011.EvtCS.IdeEvento.nrRecArqBase;
        end;
      teS5012:
        begin
          evtS5012 := TS5012(tot[AContTot].Evento.GetEvento);
          ID  := evtS5012.EvtIrrf.Id;
          NrRecArqBase := evtS5012.EvtIrrf.IdeEvento.nrRecArqBase;
        end;
    end;
  end;

end;

{ TConsultaResposta }

constructor TConsultaResposta.Create(const ItemID: Integer; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create( CSessaoRespConsulta + IntToStrZero(ItemID,1), ATipo, AFormato);
  FItemOcorrenciaConsulta := TObjectList.Create(true);
  FItemTotais := TObjectList.Create(true);

end;

destructor TConsultaResposta.Destroy;
begin
  FItemOcorrenciaConsulta.Clear;
  FItemOcorrenciaConsulta.Free;
  FItemTotais.Clear;
  FItemTotais.Free;

  inherited Destroy;
end;

procedure TConsultaResposta.Processar(const ACBreSocial: TACBreSocial; const ACont: Integer);
var
  i : integer;
  Item : TOcorrenciaConsulta;
  ItemTot : TConsultaTotResposta;
begin
  with ACBreSocial.WebServices.ConsultaLote.RetConsultaLote.RetEventos.Items[ACont] do
  begin
    cdResposta      := Processamento.cdResposta;
    descResposta    := Processamento.descResposta;
    versaoAplicProcLote:= Processamento.versaoAplicProcLote;
    dhProcessamento := Processamento.dhProcessamento;
    nrRecibo        := Recibo.nrRecibo;
    hash            := Recibo.Hash;

    for i := 0 to Processamento.Ocorrencias.Count - 1 do
    begin
      Item := TOcorrenciaConsulta.Create(i+1, Tipo, Formato);
      Item.Processar(ACBreSocial, ACont, i);
      FItemOcorrenciaConsulta.Add(Item);
    end;

    for i := 0 to tot.Count - 1 do
    begin
      ItemTot := TConsultaTotResposta.Create(i+1, Tipo, Formato);
      ItemTot.Processar(ACBreSocial, ACont, i);
      FItemTotais.Add(ItemTot);
    end;

  end;

end;

{ TOcorrenciaResposta }

constructor TOcorrenciaResposta.Create(const ItemID: Integer; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespOcorrencia + IntToStrZero(ItemID,1), ATipo, AFormato);
end;

procedure TOcorrenciaResposta.Processar(const ACBreSocial: TACBreSocial; const ACont: Integer);
begin
  with ACBreSocial.WebServices.EnvioLote.RetEnvioLote do
  begin
    Codigo      := Status.cdResposta;
    Mensagem    := Status.descResposta;
    CodigoOco   := Status.Ocorrencias.Items[ACont].Codigo;
    Descricao   := Status.Ocorrencias.Items[ACont].Descricao;
    Tipo        := Status.Ocorrencias.Items[ACont].Tipo;
    Localizacao := Status.Ocorrencias.Items[ACont].Localizacao;
  end;

end;

{ TPadraoeSocialResposta }

constructor TPadraoeSocialResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TEnvioResposta }

constructor TEnvioResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespEnvio, ATipo, AFormato);
  FItemOcorrencia := TObjectList.Create(true);

end;

destructor TEnvioResposta.Destroy;
begin
  FItemOcorrencia.Clear;
  FItemOcorrencia.Free;

  inherited Destroy;
end;

procedure TEnvioResposta.Processar(const ACBreSocial: TACBreSocial);
var
  i : integer;
  Item : TOcorrenciaResposta;
begin
  with ACBreSocial.WebServices.EnvioLote.RetEnvioLote do
  begin
    Codigo       := Status.cdResposta;
    Mensagem     := Status.descResposta;
    TpInscEmpreg := eSTpInscricaoToStr(IdeEmpregador.TpInsc);
    NrInscEmpreg := IdeEmpregador.NrInsc;
    TpInscTransm := eSTpInscricaoToStr(IdeTransmissor.TpInsc);
    NrInscTransm := IdeTransmissor.NrInsc;
    DhRecepcao   := dadosRecLote.dhRecepcao;
    VersaoAplic  := dadosRecLote.versaoAplicRecepcao;
    Protocolo    := dadosRecLote.Protocolo;
    PathNome     := ACBreSocial.WebServices.EnvioLote.PathNome;
  end;

  for i := 0 to ACBreSocial.WebServices.EnvioLote.RetEnvioLote.Status.Ocorrencias.Count - 1 do
  begin
    Item := TOcorrenciaResposta.Create(i+1, Tipo, Formato);
    Item.Processar(ACBreSocial, i);
    FItemOcorrencia.Add(Item);
  end;

end;


end.

