{******************************************************************************}
{ Projeto: ACBrNFeMonitor                                                      }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }

{ Direitos Autorais Reservados (c) 2009 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na página do Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Este programa é software livre; você pode redistribuí-lo e/ou modificá-lo   }
{ sob os termos da Licença Pública Geral GNU, conforme publicada pela Free     }
{ Software Foundation; tanto a versão 2 da Licença como (a seu critério)       }
{ qualquer versão mais nova.                                                   }

{  Este programa é distribuído na expectativa de ser útil, mas SEM NENHUMA     }
{ GARANTIA; nem mesmo a garantia implícita de COMERCIALIZAÇÃO OU DE ADEQUAÇÃO A}
{ QUALQUER PROPÓSITO EM PARTICULAR. Consulte a Licença Pública Geral GNU para  }
{ obter mais detalhes. (Arquivo LICENCA.TXT ou LICENSE.TXT)                    }

{  Você deve ter recebido uma cópia da Licença Pública Geral GNU junto com este}
{ programa; se não, escreva para a Free Software Foundation, Inc., 59 Temple   }
{ Place, Suite 330, Boston, MA 02111-1307, USA. Você também pode obter uma     }
{ copia da licença em:  http://www.opensource.org/licenses/gpl-license.php     }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}
{$I ACBr.inc}

unit DoACBrMDFeUnit;

interface

uses Classes, SysUtils, ACBrUtil, ACBrLibMDFeRespostas,
  ACBrMDFe, ACBrLibResposta, ACBrMonitorConfig,
  ACBrMonitorConsts, ACBrDFeUtil, UtilUnit, DoACBrDFeUnit,
  CmdUnit;

type

{ TACBrObjetoMDFe }

TACBrObjetoMDFe = class(TACBrObjetoDFe)
private
  fACBrMDFe: TACBrMDFe;
public
  constructor Create(AConfig: TMonitorConfig; ACBrMDFe: TACBrMDFe); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  function GerarMDFeIni(XML: string): string;
  procedure RespostaManifesto(pImprimir: boolean; pImpressora: string);
  procedure RespostaItensMDFe(ManifestoID: integer = 0; ItemID: integer = 0; Gerar: boolean = False);
  procedure RespostaMDFeNaoEnc;
  procedure RespostaEncerramento;
  procedure RespostaEnvio;
  procedure RespostaRetorno;
  procedure RespostaStatus;
  procedure RespostaConsulta;
  procedure RespostaCancelamento;
  procedure RespostaRecibo;
  procedure RespostaItensRecibo(ItemID: integer = 0);

  property ACBrMDFe: TACBrMDFe read fACBrMDFe;
end;

{ TACBrCarregarMDFe }

TACBrCarregarMDFe = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;

public
  constructor Create(AACBrDFe: TACBrMDFe; AXMLorFile: String ); reintroduce;
end;

{ TACBrCarregarMDFeEvento }

TACBrCarregarMDFeEvento = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;

public
  constructor Create(AACBrDFe: TACBrMDFe; AXMLorFile: String ); reintroduce;
end;

{ TMetodoCancelarMDFe }

TMetodoCancelarMDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirDaMDFe }

TMetodoImprimirDaMDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarMDFe }

TMetodoCriarMDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAdicionarMDFe }

TMetodoAdicionarMDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarMDFe }
TMetodoEnviarMDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarLoteMDFe }
TMetodoEnviarLoteMDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarEnviarMDFe }
TMetodoCriarEnviarMDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirDaMDFePDF }
TMetodoImprimirDaMDFePDF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirEvento }
TMetodoImprimirEvento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirEventoPDF }
TMetodoImprimirEventoPDF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoStatusServico}
TMetodoStatusServico = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoValidarMDFe}
TMetodoValidarMDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAssinarMDFe}
TMetodoAssinarMDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarMDFe}
TMetodoConsultarMDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEncerrarMDFe}
TMetodoEncerrarMDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultaMDFeNaoEnc}
TMetodoConsultaMDFeNaoEnc = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoReciboMDFe}
TMetodoReciboMDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarEmail}
TMetodoEnviarEmail = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoInutilizarMDFe}
TMetodoInutilizarMDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultaCadastro}
TMetodoConsultaCadastro = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetAmbiente}
TMetodoSetAmbiente = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetLogoMarca}
TMetodoSetLogoMarca = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetformaEmissao}
TMetodoSetformaEmissao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetTipoImpressao}
TMetodoSetTipoImpressao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetVersaoDF}
TMetodoSetVersaoDF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLerMDFe}
TMetodoLerMDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoMDFeToTxt}
TMetodoMDFeToTxt = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoFileExists}
TMetodoFileExists = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCertificadoDataVencimento}
TMetodoCertificadoDataVencimento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoVersao}
TMetodoVersao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGeraChave}
TMetodoGeraChave = class(TACBrMetodo)
private
  FCodUF: integer;
  FCodNumerico: integer;
  FModelo: integer;
  FSerie: integer;
  FnMDFe: integer;
  FTpEmissao: integer;
  FEmissao: TDateTime;
  FCNPJ: string;
public
  procedure Executar; override;
end;

implementation

uses IniFiles, DateUtils, Forms, strutils,
  ACBrDFeConfiguracoes,
  pcnConversao, pmdfeConversaoMDFe,
  pcnAuxiliar, pmdfeMDFeR, DoACBrUnit, pmdfeMDFe;

{ TACBrObjetoMDFe }

constructor TACBrObjetoMDFe.Create(AConfig: TMonitorConfig; ACBrMDFe: TACBrMDFe);
begin
  inherited Create(AConfig);

  fACBrMDFe := ACBrMDFe;

  ListaDeMetodos.Add(CMetodostatusservico);
  ListaDeMetodos.Add(CMetodoValidarmdfe);
  ListaDeMetodos.Add(CMetodoAssinarmdfe);
  ListaDeMetodos.Add(CMetodoConsultarmdfe);
  ListaDeMetodos.Add(CMetodoCancelarmdfe);
  ListaDeMetodos.Add(CMetodoEncerrarmdfe);
  ListaDeMetodos.Add(CMetodoConsultamdfenaoenc);
  ListaDeMetodos.Add(CMetodoImprimirdamdfe);
  ListaDeMetodos.Add(CMetodoImprimirdamdfepdf);
  ListaDeMetodos.Add(CMetodoImprimirevento);
  ListaDeMetodos.Add(CMetodoImprimireventopdf);
  ListaDeMetodos.Add(CMetodoEnviarmdfe);
  ListaDeMetodos.Add(CMetodoCriarmdfe);
  ListaDeMetodos.Add(CMetodoCriarenviarmdfe);
  ListaDeMetodos.Add(CMetodoAdicionarmdfe);
  ListaDeMetodos.Add(CMetodoEnviarlotemdfe);
  ListaDeMetodos.Add(CMetodoRecibomdfe);
  ListaDeMetodos.Add(CMetodoInutilizarmdfe);
  ListaDeMetodos.Add(CMetodoConsultacadastro);
  ListaDeMetodos.Add(CMetodoEnviaremail);
  ListaDeMetodos.Add(CMetodoSetambiente);
  ListaDeMetodos.Add(CMetodoSetlogomarca);
  ListaDeMetodos.Add(CMetodoSetformaemissao);
  ListaDeMetodos.Add(CMetodoSetversaodf);
  ListaDeMetodos.Add(CMetodoLermdfe);
  ListaDeMetodos.Add(CMetodoMdfetotxt);
  ListaDeMetodos.Add(CMetodoFileexist);
  ListaDeMetodos.Add(CMetodoCertificadodatavencimento);
  ListaDeMetodos.Add(CMetodoGerarchave);
  ListaDeMetodos.Add(CMetodoVersao);
  ListaDeMetodos.Add(CMetodoSetTipoImpressao);
  ListaDeMetodos.Add(CMetodoSavetofile);
  ListaDeMetodos.Add(CMetodoLoadfromfile);
  ListaDeMetodos.Add(CMetodoLerini);
  ListaDeMetodos.Add(CMetodoSetcertificado);
  ListaDeMetodos.Add(CMetodoRestaurar);
  ListaDeMetodos.Add(CMetodoOcultar);
  ListaDeMetodos.Add(CMetodoEncerrarmonitor);
  ListaDeMetodos.Add(CMetodoAtivo);
  ListaDeMetodos.Add(CMetodoDatahora);
  ListaDeMetodos.Add(CMetodoData);
  ListaDeMetodos.Add(CMetodoHora);
  ListaDeMetodos.Add(CMetodoExit);
  ListaDeMetodos.Add(CMetodoBye);
  ListaDeMetodos.Add(CMetodoFim);
  ListaDeMetodos.Add(CMetodoSair);
end;

procedure TACBrObjetoMDFe.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoStatusServico;
    1  : AMetodoClass := TMetodoValidarMDFe;
    2  : AMetodoClass := TMetodoAssinarMDFe;
    3  : AMetodoClass := TMetodoConsultarMDFe;
    4  : AMetodoClass := TMetodoCancelarMDFe;
    5  : AMetodoClass := TMetodoEncerrarMDFe;
    6  : AMetodoClass := TMetodoConsultaMDFeNaoEnc;
    7  : AMetodoClass := TMetodoImprimirDaMDFe;
    8  : AMetodoClass := TMetodoImprimirDaMDFePDF;
    9  : AMetodoClass := TMetodoImprimirEvento;
    10 : AMetodoClass := TMetodoImprimirEventoPDF;
    11 : AMetodoClass := TMetodoEnviarMDFe;
    12 : AMetodoClass := TMetodoCriarMDfe;
    13 : AMetodoClass := TMetodoCriarEnviarMDFe;
    14 : AMetodoClass := TMetodoAdicionarMDFe;
    15 : AMetodoClass := TMetodoEnviarLoteMDfe;
    16 : AMetodoClass := TMetodoReciboMDFe;
    17 : AMetodoClass := TMetodoInutilizarMDFe;
    18 : AMetodoClass := TMetodoConsultaCadastro;
    19 : AMetodoClass := TMetodoEnviarEmail;
    20 : AMetodoClass := TMetodoSetAmbiente;
    21 : AMetodoClass := TMetodoSetLogoMarca;
    22 : AMetodoClass := TMetodoSetformaEmissao;
    23 : AMetodoClass := TMetodoSetVersaoDF;
    24 : AMetodoClass := TMetodoLerMDFe;
    25 : AMetodoClass := TMetodoMDFeToTxt;
    26 : AMetodoClass := TMetodoFileExists;
    27 : AMetodoClass := TMetodoCertificadoDataVencimento;
    28 : AMetodoClass := TMetodoGeraChave;
    29 : AMetodoClass := TMetodoVersao;
    30 : AMetodoClass := TMetodoSetTipoImpressao;
    31..45 : DoACbr(ACmd);
  end;

  if Assigned(AMetodoClass) then
  begin
    Ametodo := AMetodoClass.Create(ACmd, Self);
    try
      Ametodo.Executar;
    finally
      Ametodo.Free;
    end;
  end;
end;

procedure TACBrObjetoMDFe.RespostaEnvio;
var
  Resp: TEnvioResposta;
begin
  Resp := TEnvioResposta.Create(resINI);
  try
    with fACBrMDFe.WebServices.Enviar do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.verAplic := verAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := xMotivo;
      Resp.CUF := cUF;
      Resp.nRec := Recibo;
      Resp.DhRecbto := dhRecbto;
      Resp.Tmed := TMed;
      Resp.Msg := Msg;

      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + Msg + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaRetorno;
var
  Resp: TRetornoResposta;
begin
  Resp := TRetornoResposta.Create(resINI);
  try
    with fACBrMDFe.WebServices.Retorno do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.verAplic := verAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := xMotivo;
      Resp.CUF := cUF;
      Resp.nRec := Recibo;
      Resp.Msg := Msg;

      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + Msg + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaManifesto(pImprimir: boolean;
  pImpressora: string);
var
  I, J: integer;
begin
  with fACBrMDFe do
  begin
    for I := 0 to WebServices.Retorno.MDFeRetorno.ProtMDFe.Count - 1 do
    begin
      for J := 0 to Manifestos.Count - 1 do
      begin
        if ('MDFe' + WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[i].chMDFe =
          Manifestos.Items[j].MDFe.infMDFe.Id) then
        begin
          RespostaItensMDFe(J, I, True);

          if NaoEstaVazio(pImpressora) then
            DAMDFe.Impressora := pImpressora;

          if (Manifestos.Items[i].Confirmado) and (pImprimir) then
          begin
            try
              DoAntesDeImprimir(DAMDFe.MostrarPreview);
              Manifestos.Items[i].Imprimir;
            finally
              DoDepoisDeImprimir;
            end;
          end;

          break;
        end;
      end;
    end;
  end;
end;

procedure TACBrObjetoMDFe.RespostaItensMDFe(ManifestoID: integer;
  ItemID: integer; Gerar: boolean);
var
  Resp: TRetornoItemResposta;
begin
  Resp := TRetornoItemResposta.Create(
    'MDFe' + Trim(IntToStr(
    fACBrMDFe.Manifestos.Items[ManifestoID].MDFe.Ide.nMDF)), resINI);
  try
    with fACBrMDFe.WebServices.Retorno.MDFeRetorno.ProtMDFe.Items[ItemID] do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := fACBrMDFe.WebServices.Retorno.MDFeRetorno.cUF;
      Resp.ChMDFe := chMDFe;
      Resp.DhRecbto := dhRecbto;
      Resp.NProt := nProt;
      Resp.DigVal := digVal;
      if Gerar then
        Resp.Arquivo :=
          PathWithDelim(fACBrMDFe.Configuracoes.Arquivos.PathSalvar) +
          OnlyNumber(fACBrMDFe.Manifestos.Items[ManifestoID].MDFe.infMDFe.ID) +
          '-MDFe.xml';

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaStatus;
var
  Resp: TStatusServicoResposta;
begin
  Resp := TStatusServicoResposta.Create(resINI);
  try
    with fACBrMDFe.WebServices.StatusServico do
    begin
      Resp.Versao := versao;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cUF;
      Resp.DhRecbto := dhRecbto;
      Resp.tMed := TMed;
      Resp.dhRetorno := dhRetorno;
      Resp.xObs := xObs;
      Resp.Msg := Msg;

      fpCmd.Resposta := Msg + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaConsulta;
var
  Resp: TConsultaResposta;
begin
  Resp := TConsultaResposta.Create(resINI);
  try
    with fACBrMDFe.WebServices.Consulta do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cUF;
      Resp.ChMDFe := MDFeChave;
      Resp.DhRecbto := dhRecbto;
      Resp.NProt := Protocolo;
      Resp.digVal := protMDFe.digVal;
      Resp.Msg := Msg;

      fpCmd.Resposta := Msg + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaCancelamento;
var
  Resp: TCancelamentoResposta;
begin
  Resp := TCancelamentoResposta.Create(resINI);
  try
    with fACBrMDFe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cOrgao;
      Resp.ChMDFe := chMDFe;
      Resp.DhRecbto := dhRegEvento;
      Resp.NProt := nProt;
      Resp.TpEvento := TpEventoToStr(tpEvento);
      Resp.xEvento := xEvento;
      Resp.nSeqEvento := nSeqEvento;
      Resp.CNPJDest := CNPJDest;
      Resp.emailDest := emailDest;
      Resp.XML := XML;

      fpCmd.Resposta := XMotivo + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaEncerramento;
var
  Resp: TEncerramentoResposta;
begin
  Resp := TEncerramentoResposta.Create(resINI);
  try
    with fACBrMDFe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cOrgao;
      Resp.ChMDFe := chMDFe;
      Resp.DhRecbto := dhRegEvento;
      Resp.NProt := nProt;
      Resp.TpEvento := TpEventoToStr(tpEvento);
      Resp.xEvento := xEvento;
      Resp.nSeqEvento := nSeqEvento;
      Resp.CNPJDest := CNPJDest;
      Resp.emailDest := emailDest;
      Resp.XML := XML;

      fpCmd.Resposta := XMotivo + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaMDFeNaoEnc;
var
  Resp: TNaoEncerradosResposta;
begin
  Resp := TNaoEncerradosResposta.Create(resINI);
  try
    with fACBrMDFe.WebServices.ConsMDFeNaoEnc do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cUF;
      Resp.CNPJ := CNPJ;
      Resp.ChMDFe := InfMDFe.Items[0].chMDFe;
      Resp.NProt := InfMDFe.Items[0].nProt;

      fpCmd.Resposta := Msg + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaRecibo;
var
  Resp: TRetornoResposta;
begin
  Resp := TRetornoResposta.Create(resINI);
  try
    with fACBrMDFe.WebServices.Recibo do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.nRec := Recibo;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cUF;
      Resp.ChMDFe := fACBrMDFe.WebServices.ConsMDFeNaoEnc.InfMDFe.Items[0].chMDFe;
      Resp.NProt := fACBrMDFe.WebServices.ConsMDFeNaoEnc.InfMDFe.Items[0].nProt;
      Resp.MotivoMDFe := MDFeRetorno.ProtMDFe.Items[0].xMotivo;

      fpCmd.Resposta := Msg + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaItensRecibo(ItemID: integer);
var
  Resp: TRetornoItemResposta;
begin
  Resp := TRetornoItemResposta.Create(
    'MDFe' + Trim(IntToStr(StrToInt(copy(
    fACBrMDFe.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items
    [ItemID].chMDFe, 26, 9)))), resINI);
  try
    with fACBrMDFe.WebServices.Recibo.MDFeRetorno.ProtMDFe.Items[ItemID] do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := fACBrMDFe.WebServices.Recibo.MDFeRetorno.cUF;
      Resp.ChMDFe := chMDFe;
      Resp.DhRecbto := dhRecbto;
      Resp.NProt := nProt;
      Resp.digVal := digVal;

      fpCmd.Resposta := Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

function TACBrObjetoMDFe.GerarMDFeIni(XML: string): string;
var
  I, j, y: integer;
  sSecao: string;
  INIRec: TMemIniFile;
  IniMDFe: TStringList;
  LocMDFeR: TMDFeR;
begin
  INIRec := TMemIniFile.Create('MDFe.ini');
  fACBrMDFe.Manifestos.Clear;
  if FilesExists(XML) then
    fACBrMDFe.Manifestos.LoadFromFile(XML)
  else
  begin
    LocMDFeR := TMDFeR.Create(fACBrMDFe.Manifestos.Add.MDFe);
    try
      LocMDFeR.Leitor.Arquivo := ConvertStrRecived(XML);
      LocMDFeR.LerXml;
      fACBrMDFe.Manifestos.Items[0].XML := LocMDFeR.Leitor.Arquivo;
      fACBrMDFe.Manifestos.GerarMDFe;
    finally
      LocMDFeR.Free;
    end;

  end;

  with fACBrMDFe do
  begin
    try
      with Manifestos.Items[0].MDFe do
      begin
        INIRec.WriteInteger('ide', 'cUF', Ide.cUF);
        INIRec.WriteString('ide', 'tpEmit', TpEmitenteToStr(Ide.tpEmit));
        INIRec.WriteString('ide', 'mod', Ide.modelo);
        INIRec.WriteInteger('ide', 'serie', Ide.serie);
        INIRec.WriteInteger('ide', 'nMDF', Ide.nMDF);
        INIRec.WriteInteger('ide', 'cMDF', Ide.cMDF);
        INIRec.WriteString('ide', 'modal', ModalToStr(Ide.modal));
        INIRec.WriteString('ide', 'dhEmi', DateToStr(Ide.dhEmi));
        INIRec.WriteString('ide', 'tpEmis', TpEmisToStr(Ide.tpEmis));
        INIRec.WriteString('ide', 'procEmi', procEmiToStr(Ide.procEmi));
        INIRec.WriteString('ide', 'verProc', Ide.verProc);
        INIRec.WriteString('ide', 'UFIni', Ide.UFIni);
        INIRec.WriteString('ide', 'UFFim', Ide.UFFim);
        INIRec.WriteString('ide', 'dhIniViagem', DateToStr(Ide.dhIniViagem));
        INIRec.WriteString('ide', 'tpTransp', TTransportadorToStr(Ide.tpTransp));
        for i := 0 to ide.infMunCarrega.Count - 1 do
        begin
          sSecao := 'CARR' + IntToStrZero(I + 1, 3);
          with ide.infMunCarrega.Items[i] do
          begin
            INIRec.WriteInteger(sSecao, 'cMunCarrega', cMunCarrega);
            INIRec.WriteString(sSecao, 'xMunCarrega', xMunCarrega);
          end;
        end;
        for i := 0 to ide.infPercurso.Count - 1 do
        begin
          sSecao := 'PERC' + IntToStrZero(I + 1, 3);
          with ide.infPercurso.Items[i] do
          begin
            INIRec.WriteString(sSecao, 'UFPer', UFPer);
          end;
        end;
        INIRec.WriteString('emit', 'CNPJ', Emit.CNPJ);
        INIRec.WriteString('emit', 'IE', Emit.IE);
        INIRec.WriteString('emit', 'xNome', Emit.xNome);
        INIRec.WriteString('emit', 'xFant', Emit.xFant);
        INIRec.WriteString('emit', 'xLgr', Emit.enderEmit.xLgr);
        INIRec.WriteString('emit', 'nro', Emit.enderEmit.nro);
        INIRec.WriteString('emit', 'xCpl', Emit.enderEmit.xCpl);
        INIRec.WriteString('emit', 'xBairro', Emit.enderEmit.xBairro);
        INIRec.WriteInteger('emit', 'cMun', Emit.enderEmit.cMun);
        INIRec.WriteString('emit', 'xMun', Emit.enderEmit.xMun);
        INIRec.WriteInteger('emit', 'CEP', Emit.enderEmit.CEP);
        INIRec.WriteString('emit', 'UF', Emit.enderEmit.UF);
        INIRec.WriteString('emit', 'fone', Emit.enderEmit.fone);
        INIRec.WriteString('emit', 'email', Emit.enderEmit.email);
        case Ide.modal of
          moRodoviario:
          begin
            INIRec.WriteString('Rodo', 'RNTRC', Rodo.RNTRC);
            INIRec.WriteString('Rodo', 'CIOT', Rodo.CIOT);
            INIRec.WriteString('Rodo', 'tpRod',
              TpRodadoToStr(Rodo.veicTracao.tpRod));
            INIRec.WriteString('Rodo', 'tpCar',
              TpCarroceriaToStr(Rodo.veicTracao.tpCar));
            INIRec.WriteString('Rodo', 'UF', Rodo.veicTracao.UF);
            if (Rodo.veicTracao.placa <> '') then
            begin
              INIRec.WriteString('veicTracao',
                'clInt', Rodo.veicTracao.cInt);
              INIRec.WriteString('veicTracao',
                'placa', Rodo.veicTracao.placa);
              INIRec.WriteString('veicTracao',
                'RENAVAN', Rodo.veicTracao.RENAVAM);
              INIRec.WriteInteger('veicTracao',
                'tara', Rodo.veicTracao.tara);
              INIRec.WriteInteger('veicTracao',
                'capKG', Rodo.veicTracao.capKG);
              INIRec.WriteInteger('veicTracao',
                'clInt', Rodo.veicTracao.capM3);
            end;
            if (Rodo.veicTracao.prop.CNPJCPF <> '') then
            begin
              INIRec.WriteString('prop', 'CPFCNPJ',
                Rodo.veicTracao.prop.CNPJCPF);
              INIRec.WriteString(
                'prop', 'RNTRC', Rodo.veicTracao.prop.RNTRC);
              INIRec.WriteString(
                'prop', 'xNome', Rodo.veicTracao.prop.xNome);
              INIRec.WriteString('prop', 'IE', Rodo.veicTracao.prop.IE);
              INIRec.WriteString('prop', 'UF', Rodo.veicTracao.prop.UF);
              INIRec.WriteString(
                'prop', 'tpProp', TpPropToStr(Rodo.veicTracao.prop.tpProp));
            end;
            for y := 1 to Rodo.veicTracao.condutor.Count - 1 do
            begin
              sSecao := 'condutor' + IntToStrZero(y + 1, 3);
              IniRec.WriteString(
                sSecao, 'CPF', Rodo.veicTracao.condutor.Items[y].CPF);
              IniRec.WriteString(
                sSecao, 'xNome', Rodo.veicTracao.condutor.Items[y].xNome);
            end;
          end;
          moAereo:
          begin

          end;
          moAquaviario:
          begin

          end;
          moFerroviario:
          begin

          end;
        end;
        for i := 0 to infDoc.infMunDescarga.Count - 1 do
        begin
          sSecao := 'DESC' + IntToStrZero(I + 1, 3);
          with infDoc.infMunDescarga.Items[i] do
          begin
            INIRec.WriteInteger(sSecao, 'cMunDescarga', cMunDescarga);
            INIRec.WriteString(sSecao, 'xMunDescarga', xMunDescarga);
            for j := 0 to infDoc.infMunDescarga.Items[i].infCTe.Count - 1 do
            begin
              sSecao := 'infCTe' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);
              with infDoc.infMunDescarga.Items[i].infCTe.Items[j] do
              begin
                INIRec.WriteString(sSecao, 'chCTe', chCTe);
                INIRec.WriteString(sSecao, 'SegCodBarra', SegCodBarra);
              end;
            end;
            for j := 0 to infDoc.infMunDescarga.Items[i].infNFe.Count - 1 do
            begin
              sSecao := 'infNFe' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);
              with infDoc.infMunDescarga.Items[i].infNFe.Items[j] do
              begin
                INIRec.WriteString(sSecao, 'chNFe', chNFe);
                INIRec.WriteString(sSecao, 'SegCodBarra', SegCodBarra);
              end;
            end;
            for j := 0 to infDoc.infMunDescarga.Items[i].infMDFeTransp.Count - 1 do
            begin
              sSecao := 'infMDFeTransp' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);
              with infDoc.infMunDescarga.Items[i].infMDFeTransp.Items[j] do
              begin
                INIRec.WriteString(sSecao, 'chMDFe', chMDFe);
              end;
            end;
          end;
        end;
        INIRec.WriteInteger('tot', 'qCTe', tot.qCTe);
        INIRec.WriteInteger('tot', 'qNFe', tot.qNFe);
        INIRec.WriteInteger('tot', 'qMDFe', tot.qMDFe);
        INIRec.WriteFloat('tot', 'vCarga', tot.vCarga);
        INIRec.WriteString('tot', 'cUnid', UnidMedToStr(tot.cUnid));
        INIRec.WriteFloat('tot', 'qCarga', tot.qCarga);
        for i := 0 to lacres.Count - 1 do
        begin
          sSecao := 'lacres' + IntToStrZero(I + 1, 3);
          with lacres.Items[i] do
          begin
            INIRec.WriteString(sSecao, 'nLacre', nLacre);
          end;
        end;
        for i := 0 to autXML.Count - 1 do
        begin
          sSecao := 'autXML' + IntToStrZero(I + 1, 3);
          with autXML.Items[i] do
          begin
            INIRec.WriteString(sSecao, 'CNPJCPF', CNPJCPF);
          end;
        end;
        INIRec.WriteString('infAdic', 'infAdFisco', infAdic.infAdFisco);
        INIRec.WriteString('infAdic', 'infCpl', infAdic.infCpl);
      end;

    finally
      IniMDFe := TStringList.Create;
      INIRec.GetStrings(IniMDFe);
      INIRec.Free;
      Result := StringReplace(IniMDFe.Text, sLineBreak + sLineBreak,
        sLineBreak, [rfReplaceAll]);
      IniMDFe.Free;
    end;
  end;
end;

{ TACBrCarregarMDFe }

procedure TACBrCarregarMDFe.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrMDFe(FpACBrDFe).Manifestos.LoadFromFile( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroMDFeAbrir, [AValue]) ) );
end;

procedure TACBrCarregarMDFe.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrMDFe(FpACBrDFe).Manifestos.LoadFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroMDFeCarregar) );
end;

function TACBrCarregarMDFe.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrMDFe(FpACBrDFe).Manifestos.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrMDFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrMDFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlMdfe ;
end;

constructor TACBrCarregarMDFe.Create(AACBrDFe: TACBrMDFe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

{ TACBrCarregarMDFeEvento }

procedure TACBrCarregarMDFeEvento.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrMDFe(FpACBrDFe).EventoMDFe.LerXML( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroMDFeAbrir, [AValue]) ) );
end;

procedure TACBrCarregarMDFeEvento.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrMDFe(FpACBrDFe).EventoMDFe.LerXMLFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroMDFeCarregar) );
end;

function TACBrCarregarMDFeEvento.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrMDFe(FpACBrDFe).EventoMDFe.Evento.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrMDFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrMDFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlMdfeEve ;
end;

constructor TACBrCarregarMDFeEvento.Create(AACBrDFe: TACBrMDFe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

{ TMetodoVersao }

procedure TMetodoVersao.Executar;
begin
  fpCmd.Resposta := VersaoACBr;
end;

{ TMetodoGeraChave }

{ Params: 0 - Codigo da UF
          1 - Codigo Numerico
          2 - Modelo
          3 - Serie
          4 - Numero MDFe
          5 - Data Emissao
          6 - CNPJ
}
procedure TMetodoGeraChave.Executar;
begin
  FCodUF := StrToInt(fpCmd.Params(0));
  FCodNumerico := StrToInt(fpCmd.Params(1));
  FModelo := StrToInt(fpCmd.Params(2));
  FSerie := StrToInt(fpCmd.Params(3));
  FnMDFe := StrToInt(fpCmd.Params(4));
  FTpEmissao := StrToInt(fpCmd.Params(5));
  FEmissao := StrToDateTime(fpCmd.Params(6));
  FCNPJ := fpCmd.Params(7);

  fpCmd.Resposta := GerarChaveAcesso(FCodUF,
    FEmissao, FCNPJ, FSerie,
    FnMDFe, FTpEmissao,
    FCodNumerico, FModelo);
end;

{ TMetodoCertificadoDataVencimento }

procedure TMetodoCertificadoDataVencimento.Executar;
begin
  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := DateToStr(ACBrMDFe.SSL.CertDataVenc);
  end;
end;

{ TMetodoFileExists }

{ Params: 0 - Arq - String com path do Arquivo
}
procedure TMetodoFileExists.Executar;
var
  Arq: String;
begin
  Arq := fpCmd.Params(0);

  if not FileExists(Arq) then
    raise Exception.Create('Arquivo ' + Arq + ' não encontrado');
end;

{ TMetodoMDFeToTxt }

{ Params: 0 - XML - String com path XML
          1 - Nome - String nome do Txt a ser gerado
}
procedure TMetodoMDFeToTxt.Executar;
var
  CargaDFe: TACBrCarregarMDFe;
  AXML: string;
  ANomeTxt: string;
begin
  AXML := fpCmd.Params(0);
  ANomeTxt := fpCmd.Params(1);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;
    CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, AXML);
    try
      ACBrMDFe.Manifestos.Items[0].GravarXML(ANomeTxt);
    finally
      CargaDFe.Free;
    end;

    fpCmd.Resposta := ChangeFileExt(ACBrMDFe.Manifestos.Items[0].NomeArq, '.txt');
  end;
end;

{ TMetodoLerMDFe }

{ Params: 0 - XML - String com path XML
}
procedure TMetodoLerMDFe.Executar;
var
  AXML: String;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    try
      fpCmd.Resposta := GerarMDFeIni(AXML);
    except
      on E: Exception do
        raise Exception.Create('Erro ao gerar INI do MDFe.' + sLineBreak + E.Message);
    end;
  end;
end;

{ TMetodoSetVersaoDF }

{ Params: 0 - Inteiro com numero da Versao MDFe valores: 1.00 - 3.00
}
procedure TMetodoSetVersaoDF.Executar;
var
  OK: boolean;
  VersaoDF: TVersaoMDFe;
  AVersao: String;
begin
  AVersao := fpCmd.Params(0);
  VersaoDF := StrToVersaoMDFe(OK, AVersao);

  if not OK then
    raise Exception.Create('Versão Inválida.');

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.WebService do
      VersaoMDFe := VersaoMDFeToStr(VersaoDF);

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoSetformaEmissao }

{ Params: 0 - Inteiro com numero da Forma Emissao
1-Normal, 2-Contingencia, 3-SCAN, 4-DPEC, 5-FSDA, 6-SVCAN, 7-SVCRS, 8-SVCSP ou 9-OffLine
}
procedure TMetodoSetformaEmissao.Executar;
var
  OK: boolean;
  FormaEmissao: TpcnTipoEmissao;
  NFormaEmissao: Integer;
begin
  NFormaEmissao := StrToIntDef(fpCmd.Params(0), 1);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    OK := False;
    FormaEmissao := StrToTpEmis(OK, IntToStr(NFormaEmissao));

    if not OK then
      raise Exception.Create('Forma de Emissão Inválida: ' + TpEmisToStr(FormaEmissao));

    with MonitorConfig.DFE.WebService do
      FormaEmissaoMDFe := StrToInt(TpEmisToStr(FormaEmissao));

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoSetTipoImpressao }

{ Params: 0 -  1: Retrato 2: Paisagem
}
procedure TMetodoSetTipoImpressao.Executar;
var
  OK: boolean;
  TipoDAMDFe: TpcnTipoImpressao;
  NTipoImpressao: Integer;
begin
  NTipoImpressao := StrToIntDef(fpCmd.Params(0), 1);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    OK := False;
    TipoDAMDFe := StrToTpImp(OK, IntToStr(NTipoImpressao));

    if not OK then
      raise Exception.Create('Tipo Impressão Inválido: ' + TpImpToStr(TipoDAMDFe));

    with MonitorConfig.DFE.Impressao.Geral do
      DANFE := StrToInt(TpImpToStr(TipoDAMDFe)) -1;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoSetLogoMarca }

{ Params: 0 - Logo: String com path da logo
}
procedure TMetodoSetLogoMarca.Executar;
var
  ALogo: String;
begin
  ALogo := fpCmd.Params(0);

  if not FileExists(ALogo) then
    raise Exception.Create('Arquivo não encontrado.');

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.Impressao.Geral do
        Logomarca := ALogo;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoSetAmbiente }

{ Params: 0 - NumAmbiente : Integer 1- Producao / 2- Homologacao
}
procedure TMetodoSetAmbiente.Executar;
var
  OK: boolean;
  NumAmbiente: Integer;
begin
  NumAmbiente := StrToIntDef(fpCmd.Params(0), 2);

  if (NumAmbiente < 1) or (NumAmbiente > 2) then
    raise Exception.Create('Ambiente Inválido: '+IntToStr(NumAmbiente));

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.WebService do
      Ambiente := NumAmbiente;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoConsultaCadastro }

{ Params: 0 - UF: String Sigla UF consulta
          1 - Documento: String Documento a ser consultado
          2 - IE: Boolean 1: para consulta por IF
}
procedure TMetodoConsultaCadastro.Executar;
var
  AUF, ADocumento: String;
  AIE: Boolean;
begin
  AUF := fpCmd.Params(0);
  ADocumento := fpCmd.Params(1);
  AIE := StrToBoolDef(fpCmd.Params(2), False);

  raise Exception.Create('Método: MDFe.ConsultarCadastro não implementado.');
end;

{ TMetodoInutilizarMDFe }

{ Params: 0 - CNPJ: String com CNPJ
          1 - Justificativa: String
          2 - Ano: Integer com Ano Documento
          3 - Modelo: Integer com Modelo Documento
          4 - Serie: String com a série documento
          5 - NumIinial: Integer Nº inicial inutilização
          6 - NumFinal: Integer Nº final inutilização
}
procedure TMetodoInutilizarMDFe.Executar;
var
  ACNPJ, AJustificativa, ASerie: String;
  AAno, AModelo, ANumIninial, ANumFinal: Integer;
begin
  ACNPJ := fpCmd.Params(0);
  AJustificativa := fpCmd.Params(1);
  AAno := StrToIntDef(fpCmd.Params(2), 0);
  AModelo := StrToIntDef(fpCmd.Params(3), 0);
  ASerie := fpCmd.Params(4);
  ANumIninial := StrToIntDef(fpCmd.Params(5), 0);
  ANumFinal := StrToIntDef(fpCmd.Params(6), 0);

  raise Exception.Create('Método: MDFe.InutilizarMDFe não implementado.');
end;

{ TMetodoEnviarEmail }

{ Params: 0 - Email: String com email Destinatário
          1 - XML: String com path do XML
          2 - Boolean 1 : Envia PDF
          3 - Assunto: String com Assunto do e-mail
          4 - Copia: String com e-mails copia (Separados ;)
          5 - Anexo: String com Path de Anexos (Separados ;)
}
procedure TMetodoEnviarEmail.Executar;
var
  sAssunto, ADestinatario, APathXML, AAssunto, AEmailCopias, AAnexos: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  CargaDFe: TACBrCarregarMDFe;
  AEnviaPDF: Boolean;
begin
  ADestinatario := fpCmd.Params(0);
  APathXML := fpCmd.Params(1);
  AEnviaPDF := StrToBoolDef(fpCmd.Params(2), False);
  AAssunto := fpCmd.Params(3);
  AEmailCopias := fpCmd.Params(4);
  AAnexos := fpCmd.Params(5);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;

    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    try
      CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, APathXML);
      try
        with MonitorConfig.DFE.Email do
        begin
          slMensagemEmail.Text := MensagemMDFe;
          sAssunto := AssuntoMDFe;
        end;

        slCC.Text := StringReplace(AEmailCopias, ';', sLineBreak, [rfReplaceAll]);
        slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

        try
          ACBrMDFe.Manifestos.Items[0].EnviarEmail(ADestinatario,
            IfThen( NaoEstaVazio(AAssunto), AAssunto, sAssunto),
            slMensagemEmail,
            AEnviaPDF,
            // Enviar PDF junto
            slCC,
            // Lista com emails que serão enviado cópias - TStrings
            slAnexos);
          // Lista de slAnexos - TStrings

          fpCmd.Resposta := 'Email enviado com sucesso';
        except
          on E: Exception do
            raise Exception.Create('Erro ao enviar email' + sLineBreak + E.Message);
        end;
      finally
        CargaDFe.Free;
      end;
    finally
      slCC.Free;
      slAnexos.Free;
      slMensagemEmail.Free;
    end;
  end;
end;

{ TMetodoReciboMDFe }

{ Params: 0 - Recibo - String com Numero Recibo para consulta
}
procedure TMetodoReciboMDFe.Executar;
var
  I: integer;
  ARecibo: String;
begin
  ARecibo := fpCmd.Params(0);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.WebServices.Recibo.Recibo := ARecibo;
    if not (ACBrMDFe.WebServices.Recibo.Executar) then
      raise Exception.Create(ACBrMDFe.WebServices.Recibo.xMotivo);

    RespostaRecibo;
    for I := 0 to ACBrMDFe.WebServices.Recibo.MDFeRetorno.ProtMDFe.Count - 1 do
      RespostaItensRecibo(I);

    if ACBrMDFe.Configuracoes.Geral.Salvar then
      fpCmd.Resposta := 'Arquivo=' + ACBrMDFe.Configuracoes.Arquivos.PathSalvar +
                        ARecibo + '-pro-rec.xml';
  end;
end;

{ TMetodoConsultaMDFeNaoEnc }

{ Params: 0 - CNPJ - String com CNPJ para consulta
}
procedure TMetodoConsultaMDFeNaoEnc.Executar;
var
  ACNPJ: String;
begin
  ACNPJ := fpCmd.Params(0);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    if not ValidarCNPJ(ACNPJ) then
      raise Exception.Create('CNPJ ' + ACNPJ + ' invalido.');
    try
      ACBrMDFe.WebServices.ConsultaMDFeNaoEnc(ACNPJ);
      RespostaMDFeNaoEnc;
    except
      raise Exception.Create(ACBrMDFe.WebServices.ConsMDFeNaoEnc.Msg);
    end;
  end;
end;

{ TMetodoEncerrarMDFe }

{ Params: 0 - XML - Uma String com um Path completo XML ou chave MDFe
          1 - DtEncerra - Data para encerramento
          2 - Cod.Municipio - String com Cód Município Descarga
          3 - CNPJ - String com CNPJ para encerramento (Opcional)
          4 - Protocolo - String com Número de Protocolo (Opcional - MDFe Antigos)
}
procedure TMetodoEncerrarMDFe.Executar;
var
  Chave, AXML, AMunicipio, ACNPJ, AProtocolo: string;
  CargaDFe: TACBrCarregarMDFe;
  DtEncerra: TDateTime;
begin
  AXML := fpCmd.Params(0);
  DtEncerra := StrToDateTimeDef(fpCmd.Params(1), 0);
  AMunicipio := fpCmd.Params(2);
  ACNPJ := fpCmd.Params(3);
  AProtocolo := fpCmd.Params(4);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;
    if FilesExists(AXML) then
      CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, AXML);
    try

      if (ACBrMDFe.Manifestos.Count = 0) then
      begin
        if ValidarChave(AXML) then
          Chave := AXML
        else
          raise Exception.Create(
            'Parâmetro inválido. Chave do MDFe inválida ou arquivo não encontrado.');
      end
      else
        Chave := OnlyNumber(ACBrMDFe.Manifestos.Items[0].MDFe.infMDFe.ID);

      ACBrMDFe.EventoMDFe.Evento.Clear;
      with ACBrMDFe.EventoMDFe.Evento.Add do
      begin
        infEvento.CNPJ := ACNPJ;
        if Trim(infEvento.CNPJ) = '' then
          infEvento.CNPJ := copy(chave, 7, 14)
        else
        begin
          if not ValidarCNPJ(ACNPJ) then
            raise Exception.Create('CNPJ ' + ACNPJ + ' inválido.');
        end;

        infEvento.cOrgao := StrToIntDef(copy(OnlyNumber(chave), 1, 2), 0);
        infEvento.dhEvento := now;
        infEvento.tpEvento := teEncerramento;
        infEvento.chMDFe := Chave;

        if (Trim(AProtocolo) <> '') then
          infEvento.detEvento.nProt := Trim(AProtocolo)
        else if ((ACBrMDFe.Manifestos.Count > 0) and
          (ACBrMDFe.Manifestos.Items[0].MDFe.procMDFe.nProt <> '')) then
          infEvento.detEvento.nProt := ACBrMDFe.Manifestos.Items[0].MDFe.procMDFe.nProt
        else
        begin
          //Realiza Consulta na Sefaz
          ACBrMDFe.WebServices.Consulta.MDFeChave := Chave;
          if not (ACBrMDFe.WebServices.Consulta.Executar) then
            raise Exception.Create('Parâmetro inválido. ' + ACBrMDFe.WebServices.Consulta.Msg)
          else
            infEvento.detEvento.nProt := ACBrMDFe.WebServices.Consulta.Protocolo;
        end;

        if (Trim(AMunicipio) <> '') then
        begin
          infEvento.detEvento.cUF := StrToIntDef(copy(AMunicipio, 1, 2), 1);
          infEvento.detEvento.cMun := StrToIntDef(AMunicipio, 1);
        end
        else if ((ACBrMDFe.Manifestos.Count > 0) and
          (ACBrMDFe.Manifestos.Items[0].MDFe.infDoc.infMunDescarga.Items[0].cMunDescarga > 0)) then
        begin
          infEvento.detEvento.cMun :=
            ACBrMDFe.Manifestos.Items[0].MDFe.infDoc.infMunDescarga.Items[0].cMunDescarga;
          infEvento.detEvento.cUF :=
            StrToIntDef(copy(IntToStr(ACBrMDFe.Manifestos.Items[0].MDFe.infDoc.infMunDescarga.Items[
            0].cMunDescarga), 1, 2), 1);
        end;

        infEvento.detEvento.dtEnc := DtEncerra;
      end;

      try
        ACBrMDFe.EnviarEvento(1);
        RespostaEncerramento;
      except
        raise Exception.Create(ACBrMDFe.WebServices.EnvEvento.EventoRetorno.xMotivo);
      end;
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoConsultarMDFe }

{ Params: 0 - XML - Uma String com um Path completo XML ou chave MDFe
}
procedure TMetodoConsultarMDFe.Executar;
var
  CargaDFe: TACBrCarregarMDFe;
  AXML: String;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;
    CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, AXML);
    try
      if (ACBrMDFe.Manifestos.Count = 0) then
      begin
        if ValidarChave(AXML) then
          ACBrMDFe.WebServices.Consulta.MDFeChave := AXML
        else
          raise Exception.Create(
            'Parâmetro inválido. Chave do MDFe inválida ou arquivo não encontrado.');
      end
      else
        ACBrMDFe.WebServices.Consulta.MDFeChave :=
          OnlyNumber(ACBrMDFe.Manifestos.Items[0].MDFe.infMDFe.ID);

      try
        ACBrMDFe.WebServices.Consulta.Executar;
        RespostaConsulta;
      except
        raise Exception.Create(ACBrMDFe.WebServices.Consulta.Msg);
      end;
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoAssinarMDFe }

{ Params: 0 - XML - Uma String com um Path completo XML MDFe
}
procedure TMetodoAssinarMDFe.Executar;
var
  CargaDFe: TACBrCarregarMDFe;
  Salva: boolean;
  AXML: String;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;
    CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, AXML);
    try
      Salva := ACBrMDFe.Configuracoes.Geral.Salvar;

      if not Salva then
      begin
        ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
        ACBrMDFe.Configuracoes.Arquivos.PathSalvar :=
          PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
      end;

      ACBrMDFe.Configuracoes.Geral.Salvar := True;
      ACBrMDFe.Manifestos.Assinar;
      ACBrMDFe.Configuracoes.Geral.Salvar := Salva;

      if NaoEstaVazio(ACBrMDFe.Manifestos.Items[0].NomeArq) then
        fpCmd.Resposta := ACBrMDFe.Manifestos.Items[0].NomeArq
      else
        fpCmd.Resposta := PathWithDelim(ACBrMDFe.Configuracoes.Arquivos.PathSalvar)
          + StringReplace(ACBrMDFe.Manifestos.Items[0].MDFe.infMDFe.ID,
          'MDFe', '', [rfIgnoreCase]) + '-mdfe.xml';
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoValidarMDFe }

{ Params: 0 - XML - Uma String com um Path completo XML MDFe
}
procedure TMetodoValidarMDFe.Executar;
var
  CargaDFe: TACBrCarregarMDFe;
  AXML: String;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;
    CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, AXML);
    try
      ACBrMDFe.Manifestos.Validar;
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoStatusServico }

procedure TMetodoStatusServico.Executar;
begin
  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    if ACBrMDFe.WebServices.StatusServico.Executar then
      RespostaStatus;
  end;
end;

{ TMetodoImprimirEventoPDF }

{ Params: 0 - XMLEvento - Uma String com um Path completo XMLEvento MDFe
          1 - XML       - Uma String com um Path completo XML MDFe
}
procedure TMetodoImprimirEventoPDF.Executar;
var
  CargaDFe: TACBrCarregarMDFe;
  CargaDFeEvento: TACBrCarregarMDFeEvento;
  ArqPDF, AXMLEvento, AXML: string;
begin
  AXMLEvento := fpCmd.Params(0);
  AXML := fpCmd.Params(1);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.EventoMDFe.Evento.Clear;
    CargaDFeEvento := TACBrCarregarMDFeEvento.Create(ACBrMDFe, AXMLEvento);
    ACBrMDFe.Manifestos.Clear;
    CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, AXML);
    try
      try
        ACBrMDFe.ImprimirEventoPDF;
        ArqPDF := OnlyNumber(ACBrMDFe.EventoMDFe.Evento[0].InfEvento.Id);
        ArqPDF := PathWithDelim(ACBrMDFe.DAMDFe.PathPDF) + ArqPDF + '-procEventoMDFe.pdf';

        fpCmd.Resposta := 'Arquivo criado em: ' + ArqPDF;
      except
        raise Exception.Create('Erro ao criar o arquivo PDF');
      end;
    finally
      CargaDFeEvento.Free;
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoImprimirEvento }

{ Params: 0 - XMLEvento - Uma String com um Path completo XMLEvento MDFe
          1 - XMLEvento - Uma String com um Path completo XML MDFe
          2 - String com nome Impressora
          3 - Integer Número de Cópias
}
procedure TMetodoImprimirEvento.Executar;
var
  CargaDFe: TACBrCarregarMDFe;
  CargaDFeEvento: TACBrCarregarMDFeEvento;
  AXMLEvento, AXML, AImpressora: String;
  ACopias: Integer;
begin
  AXMLEvento := fpCmd.Params(0);
  AXML := fpCmd.Params(1);
  AImpressora := fpCmd.Params(2);
  ACopias := StrToIntDef(fpCmd.Params(3), 0);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.EventoMDFe.Evento.Clear;
    CargaDFeEvento := TACBrCarregarMDFeEvento.Create(ACBrMDFe, AXMLEvento);
    ACBrMDFe.Manifestos.Clear;
    CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, AXML);
    try
      if NaoEstaVazio(AImpressora) then
        ACBrMDFe.DAMDFe.Impressora := AImpressora;

      if (ACopias > 0) then
        ACBrMDFe.DAMDFe.NumCopias := ACopias;

      try
        DoAntesDeImprimir(ACBrMDFe.DAMDFE.MostrarPreview);
        ACBrMDFe.ImprimirEvento;
      finally
        DoDepoisDeImprimir;
      end;
    finally
      CargaDFeEvento.Free;
      CargaDFe.Free;
    end;

    fpCmd.Resposta := 'Evento Impresso com sucesso';
  end;
end;

{ TMetodoImprimirDaMDFePDF }

{ Params: 0 - XML - Uma String com um Path completo XML MDFe
          1 - NumeroProtocolo: String com número do lote (opcional)
}
procedure TMetodoImprimirDaMDFePDF.Executar;
var
  ArqPDF, AXML, AProtocolo: string;
  CargaDFe: TACBrCarregarMDFe;
begin
  AXML := fpCmd.Params(0);
  AProtocolo := fpCmd.Params(1);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;
    CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, AXML);
    try
      if NaoEstaVazio(AProtocolo) then
        ACBrMDFe.DAMDFe.ProtocoloMDFe := AProtocolo;

      try
        ACBrMDFe.Manifestos.ImprimirPDF;
        ArqPDF := OnlyNumber(ACBrMDFe.Manifestos.Items[0].MDFe.infMDFe.ID) + '-mdfe.pdf';
        ArqPDF := PathWithDelim(ACBrMDFe.DAMDFe.PathPDF) + ArqPDF;
        fpCmd.Resposta := 'Arquivo criado em: ' + ArqPDF;
      except
        raise Exception.Create('Erro ao criar o arquivo PDF');
      end;
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoCriarEnviarMDFe }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini MDFe
                         ou Uma String com conteúdo txt do MDFe
          1 - NumeroLote: Integer com número do lote a ser adicionado
          2 - Imprime : 1 para imprimir
          3 - String como Nome Impressora
}
procedure TMetodoCriarEnviarMDFe.Executar;
var
  Salva, AImprime: boolean;
  Alertas: ansistring;
  ArqMDFe: string;
  Resp, AIni, AImpressora: string;
  ALote: Integer;
begin

  AIni := fpCmd.Params(0);
  ALote := StrToIntDef(fpCmd.Params(1), 0);
  AImprime := StrToBoolDef(fpCmd.Params(2), False);
  AImpressora := fpCmd.Params(3);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;
    ACBrMDFe.Manifestos.LoadFromIni(AIni);

    Salva := ACBrMDFe.Configuracoes.Geral.Salvar;
    if not Salva then
    begin
      ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
      ACBrMDFe.Configuracoes.Arquivos.PathSalvar :=
        PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
    end;

    ACBrMDFe.Manifestos.GerarMDFe;
    Alertas := ACBrMDFe.Manifestos.Items[0].Alertas;

    ACBrMDFe.Manifestos.Assinar;
    ACBrMDFe.Manifestos.Validar;

    ArqMDFe := PathWithDelim(ACBrMDFe.Configuracoes.Arquivos.PathSalvar) +
      OnlyNumber(ACBrMDFe.Manifestos.Items[0].MDFe.infMDFe.ID) + '-mdfe.xml';
    ACBrMDFe.Manifestos.GravarXML(ArqMDFe);

    if not FileExists(ArqMDFe) then
      raise Exception.Create('Não foi possível criar o arquivo ' + ArqMDFe);

    Resp := ArqMDFe;
    if (Alertas <> '') then
      Resp := Resp + sLineBreak + 'Alertas:' + Alertas;

    fpCmd.Resposta := Resp;

    if (ALote = 0) then
      ACBrMDFe.WebServices.Enviar.Lote := '1'
    else
      ACBrMDFe.WebServices.Enviar.Lote := IntToStr(ALote);

    if not (ACBrMDFe.WebServices.StatusServico.Executar) then
      raise Exception.Create(ACBrMDFe.WebServices.StatusServico.Msg);

    ACBrMDFe.WebServices.Enviar.Executar;
    RespostaEnvio;

    ACBrMDFe.WebServices.Retorno.Recibo := ACBrMDFe.WebServices.Enviar.Recibo;
    ACBrMDFe.WebServices.Retorno.Executar;

    RespostaRetorno;
    RespostaManifesto(AImprime, AImpressora);
  end;
end;

{ TMetodoAdicionarMDFe }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini MDFe
                         ou Uma String com conteúdo txt do MDFe
          1 - NumeroLote: String com número do lote a ser adicionado
}
procedure TMetodoAdicionarMDFe.Executar;
var
  Alertas: ansistring;
  ArqMDFe: string;
  Resp, AIni, ANumeroLote: string;
begin

  AIni := fpCmd.Params(0);
  ANumeroLote := fpCmd.Params(1);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;
    ACBrMDFe.Manifestos.LoadFromIni(AIni);

    ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'Lotes' + PathDelim + 'Lote' + trim(ANumeroLote));

    ACBrMDFe.Manifestos.GerarMDFe;
    Alertas := ACBrMDFe.Manifestos.Items[0].Alertas;

    ACBrMDFe.Manifestos.Assinar;
    ACBrMDFe.Manifestos.Validar;

    ArqMDFe := PathWithDelim(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'Lotes' + PathDelim + 'Lote' + trim(ANumeroLote)) + OnlyNumber(
      ACBrMDFe.Manifestos.Items[0].MDFe.infMDFe.ID) + '-mdfe.xml';
    ACBrMDFe.Manifestos.GravarXML(ExtractFilePath(ArqMDFe));

    if not FileExists(ArqMDFe) then
      raise Exception.Create('Não foi possível criar o arquivo ' + ArqMDFe);

    Resp := ArqMDFe;
    if (Alertas <> '') then
      Resp := Resp + sLineBreak + 'Alertas:' + Alertas;

    fpCmd.Resposta := Resp;
  end;
end;

{ TMetodoEnviarLoteMDFe }

{ Params: 0 - Lote - Integer - Número Lote que contém MDFe
          1 - LoteEnvio: Integer com número do lote. Default = 1
          2 - Imprime: 1 para Imprimir
          3 - Impressora: String Nome da Impressora
}
procedure TMetodoEnviarLoteMDFe.Executar;
var
  RetFind: integer;
  SearchRec: TSearchRec;
  ALote, ALoteEnvio: Integer;
  AImprime: Boolean;
  AImpressora: String;
begin
  ALote := StrToIntDef(fpCmd.Params(0), 0);
  ALoteEnvio := StrToIntDef(fpCmd.Params(1), 0);
  AImprime := StrToBoolDef(fpCmd.Params(2), False);
  AImpressora := fpCmd.Params(3);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    if not DirectoryExists(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'Lotes' + PathDelim + 'Lote' + IntToStr(ALote)) then
      raise Exception.Create('Diretório não encontrado:' + PathWithDelim(
        ExtractFilePath(Application.ExeName)) +
        'Lotes' + PathDelim + 'Lote' + IntToStr(ALote))
    else
    begin
      ACBrMDFe.Manifestos.Clear;
      RetFind := SysUtils.FindFirst(
        PathWithDelim(ExtractFilePath(Application.ExeName)) +
        'Lotes' + PathDelim + 'Lote' + IntToStr(
        ALote) + PathDelim + '*-mdfe.xml', faAnyFile, SearchRec);
      if (RetFind = 0) then
      begin
        while RetFind = 0 do
        begin
          ACBrMDFe.Manifestos.LoadFromFile(PathWithDelim(ExtractFilePath(Application.ExeName)) +
            'Lotes' + PathDelim +
            'Lote' + IntToStr(ALote) + PathDelim + SearchRec.Name);
          RetFind := FindNext(SearchRec);
        end;
        ACBrMDFe.Manifestos.GerarMDFe;
        ACBrMDFe.Manifestos.Assinar;
        ACBrMDFe.Manifestos.Validar;
      end
      else
        raise Exception.Create('Não foi encontrada nenhuma nota para o Lote: ' +
          IntToStr(ALote));
    end;

    ACBrMDFe.WebServices.Enviar.Lote := IntToStr(ALoteEnvio);

    if not (ACBrMDFe.WebServices.StatusServico.Executar) then
      raise Exception.Create(ACBrMDFe.WebServices.StatusServico.Msg);

    ACBrMDFe.WebServices.Enviar.Executar;

    RespostaEnvio;

    ACBrMDFe.WebServices.Retorno.Recibo := ACBrMDFe.WebServices.Enviar.Recibo;
    ACBrMDFe.WebServices.Retorno.Executar;

    RespostaRetorno;
    RespostaManifesto(AImprime, AImpressora);
  end;
end;

{ TMetodoEnviarMDFe }

{ Params: 0 - PathorXML - Uma String com um Path completo arquivo XML MDFe
                         ou Uma String com conteúdo XML do MDFe
          1 - Lote: Integer com número do lote. Default = 1
          2 - Assina: 1 para assinar XML
          3 - Imprime: 1 Para True. Default 0
          4 - Nome Impressora: String com Nome da Impressora
}
procedure TMetodoEnviarMDFe.Executar;
var
  CargaDFe: TACBrCarregarMDFe;
  APathorXML, AImpressora: String;
  ALote: Integer;
  AAssina, AImprime: Boolean;
begin
  APathorXML := fpCmd.Params(0);
  ALote := StrToIntDef(fpCmd.Params(1), 0);
  AAssina := StrToBoolDef(fpCmd.Params(2), False);
  AImprime := StrToBoolDef(fpCmd.Params(3), False);
  AImpressora := fpCmd.Params(4);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;
    CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, APathorXML);
    try
      ACBrMDFe.Manifestos.GerarMDFe;

      if (AAssina) then
        ACBrMDFe.Manifestos.Assinar;

      ACBrMDFe.Manifestos.Validar;

      if (ALote = 0) then
        ACBrMDFe.WebServices.Enviar.Lote := '1'
      else
        ACBrMDFe.WebServices.Enviar.Lote := IntToStr(ALote);

      if not (ACBrMDFe.WebServices.StatusServico.Executar) then
        raise Exception.Create(ACBrMDFe.WebServices.StatusServico.Msg);

      ACBrMDFe.WebServices.Enviar.Executar;

      RespostaEnvio;

      ACBrMDFe.WebServices.Retorno.Recibo := ACBrMDFe.WebServices.Enviar.Recibo;
      ACBrMDFe.WebServices.Retorno.Executar;

      RespostaRetorno;
      RespostaManifesto(AImprime, AImpressora);
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoCriarMDFe }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini MDFe
                         ou Uma String com conteúdo txt do MDFe
          1 - RetornaXML: 1 para Retornar XML Gerado na Resposta
}
procedure TMetodoCriarMDFe.Executar;
var
  Salva, ARetornaXML: boolean;
  Alertas: ansistring;
  ArqMDFe: string;
  SL: TStringList;
  Resp, AIni: string;
begin
  AIni := fpCmd.Params(0);
  ARetornaXML := StrToBoolDef(fpCmd.Params(1), False);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;
    ACBrMDFe.Manifestos.LoadFromIni(AIni);

    Salva := ACBrMDFe.Configuracoes.Geral.Salvar;
    if not Salva then
    begin
      ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
      ACBrMDFe.Configuracoes.Arquivos.PathSalvar :=
        PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
    end;

    ACBrMDFe.Manifestos.GerarMDFe;
    Alertas := ACBrMDFe.Manifestos.Items[0].Alertas;

    ACBrMDFe.Manifestos.Assinar;
    ACBrMDFe.Manifestos.Validar;

    ArqMDFe := PathWithDelim(ACBrMDFe.Configuracoes.Arquivos.PathSalvar) +
      OnlyNumber(ACBrMDFe.Manifestos.Items[0].MDFe.infMDFe.ID) + '-mdfe.xml';
    ACBrMDFe.Manifestos.GravarXML(ArqMDFe);

    if not FileExists(ArqMDFe) then
      raise Exception.Create('Não foi possível criar o arquivo ' + ArqMDFe);

    Resp := ArqMDFe;
    if (Alertas <> '') then
      Resp := Resp + sLineBreak + 'Alertas:' + Alertas;

    if ARetornaXML then
    begin
      SL := TStringList.Create;
      try
        SL.LoadFromFile(ArqMDFe);
        Resp := Resp + sLineBreak + SL.Text;
      finally
        SL.Free;
      end;
    end;

    fpCmd.Resposta := Resp;
  end;
end;

{ TMetodoImprimirDaMDFe }

{ Params: 0 - XMLFile - Uma String com um Path completo para um arquivo XML MDFe
                         ou Uma String com conteúdo XML MDFe
          1 - Impressora: String com Nome da Impressora
          2 - Copias: Integer Número de Copias
          3 - Protocolo: String com Número de Protocolo
}
procedure TMetodoImprimirDaMDFe.Executar;
var
  CargaDFe: TACBrCarregarMDFe;
  AChave, AImpressora, AProtocolo: String;
  ACopias: Integer;
begin
  AChave := fpCmd.Params(0);
  AImpressora := fpCmd.Params(1);
  ACopias := StrToIntDef(fpCmd.Params(2), 0);
  AProtocolo := fpCmd.Params(3);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;
    CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, AChave);
    try
      if NaoEstaVazio(AImpressora) then
        ACBrMDFe.DAMDFe.Impressora := AImpressora;

      if (ACopias > 0) then
        ACBrMDFe.DAMDFe.NumCopias := ACopias;

      if NaoEstaVazio(AProtocolo) then
        ACBrMDFe.DAMDFe.ProtocoloMDFe := AProtocolo;

      try
        DoAntesDeImprimir(ACBrMDFe.DAMDFe.MostrarPreview);
        ACBrMDFe.Manifestos.Imprimir;
      finally
        DoDepoisDeImprimir;
      end;
    finally
      CargaDFe.Free;
    end;

    fpCmd.Resposta := 'DAMDFe Impresso com sucesso';
  end;
end;

{ TMetodoCancelarMDFe }

{ Params: 0 - Chave - Uma String com a Chave XML MDFe
          1 - Justificativa: String (Mínimo de 15 Carcteres)
          2 - CNPJ: String com CNPJ do emitente
          3 - Lote: Integer com Numero do Lote. Default = 1
}
procedure TMetodoCancelarMDFe.Executar;
var
  AChave, AJustificativa, ACNPJ: String;
  ALote: Integer;
begin
  AChave := fpCmd.Params(0);
  AJustificativa := fpCmd.Params(1);
  ACNPJ := fpCmd.Params(2);
  ALote := StrToIntDef(fpCmd.Params(3), 1);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    if not ValidarChave(AChave) then
      raise Exception.Create('Chave ' + AChave + ' inválida.')
    else
      ACBrMDFe.WebServices.Consulta.MDFeChave := AChave;

    if not ACBrMDFe.WebServices.Consulta.Executar then
      raise Exception.Create(ACBrMDFe.WebServices.Consulta.Msg);

    ACBrMDFe.EventoMDFe.Evento.Clear;
    with ACBrMDFe.EventoMDFe.Evento.Add do
    begin
      infEvento.CNPJ := ACNPJ;
      if Trim(infEvento.CNPJ) = '' then
        infEvento.CNPJ := copy(OnlyNumber(ACBrMDFe.WebServices.Consulta.MDFeChave), 7, 14)
      else
      begin
        if not ValidarCNPJ(ACNPJ) then
          raise Exception.Create('CNPJ ' + ACNPJ + ' inválido.');
      end;

      infEvento.cOrgao := StrToIntDef(
        copy(OnlyNumber(ACBrMDFe.WebServices.Consulta.MDFeChave), 1, 2), 0);
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.chMDFe := ACBrMDFe.WebServices.Consulta.MDFeChave;
      infEvento.detEvento.nProt := ACBrMDFe.WebServices.Consulta.Protocolo;
      infEvento.detEvento.xJust := AJustificativa;
    end;
    try
      ACBrMDFe.EnviarEvento(ALote);
      RespostaCancelamento;
    except
      raise Exception.Create(ACBrMDFe.WebServices.EnvEvento.EventoRetorno.xMotivo);
    end;
  end;
end;

end.
