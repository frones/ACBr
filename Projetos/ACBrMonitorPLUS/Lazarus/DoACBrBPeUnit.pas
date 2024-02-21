{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
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

unit DoACBrBPeUnit;

interface

uses
  Classes, SysUtils, ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO, ACBrUtil.Math,
  ACBrLibBPeRespostas, ACBrBPe, ACBrLibResposta, ACBrMonitorConfig,
  ACBrMonitorConsts, ACBrDFeUtil, UtilUnit, DoACBrDFeUnit,
  CmdUnit, ACBrXmlBase, ACBrBPeEventoClass;

type

{ TACBrObjetoBPe }

TACBrObjetoBPe = class(TACBrObjetoDFe)
private
  fACBrBPe: TACBrBPe;
public
  constructor Create(AConfig: TMonitorConfig; ACBrBPe: TACBrBPe); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  function GerarBPeIni(XML: string): string;
  procedure RespostaBilhetes(pImprimir: Boolean; pImpressora: String;
            pPreview: Boolean; pCopias: Integer; pPDF: Boolean);

  procedure RespostaEnvio;

  procedure RespostaStatus;
  procedure RespostaConsulta;
  procedure RespostaCancelamento;

  procedure RespostaEvento;

  procedure RespostaDistribuicaoDFe;
  procedure RespostaItensDistribuicaoDFeResBPe(ItemID: integer = 0);
  procedure RespostaItensDistribuicaoDFeResEve(ItemID: integer = 0);
  procedure RespostaItensDistribuicaoDFeProEve(ItemID: integer = 0);
  procedure RespostaItensDistribuicaoDFeInfeve(ItemID: integer = 0);

  property ACBrBPe: TACBrBPe read fACBrBPe;
end;

{ TACBrCarregarBPe }

TACBrCarregarBPe = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;
public
  constructor Create(AACBrDFe: TACBrBPe; AXMLorFile: String ); reintroduce;
end;

{ TACBrCarregarBPeEvento }

TACBrCarregarBPeEvento = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;
public
  constructor Create(AACBrDFe: TACBrBPe; AXMLorFile: String ); reintroduce;
end;

{ TMetodoCancelarBPe }

TMetodoCancelarBPe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirDaBPe }

TMetodoImprimirDaBPe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarBPe }

TMetodoCriarBPe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarBPe }
TMetodoEnviarBPe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarEnviarBPe }
TMetodoCriarEnviarBPe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirDaBPePDF }
TMetodoImprimirDaBPePDF = class(TACBrMetodo)
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

{ TMetodoValidarBPe}
TMetodoValidarBPe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAssinarBPe}
TMetodoAssinarBPe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarBPe}
TMetodoConsultarBPe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarEmail}
TMetodoEnviarEmail = class(TACBrMetodo)
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

{ TMetodoSetVersaoDF}
TMetodoSetVersaoDF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLerBPe}
TMetodoLerBPe = class(TACBrMetodo)
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
  FnBPe: integer;
  FTpEmissao: integer;
  FEmissao: TDateTime;
  FCNPJ: string;
public
  procedure Executar; override;
end;

{ TMetodoCNPJCertificado}
TMetodoCNPJCertificado = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGetPathBPe}
TMetodoGetPathBPe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGetPathCCe}
TMetodoGetPathCCe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGetPathCan}
TMetodoGetPathCan = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGetPathEvento}
TMetodoGetPathEvento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarEvento}
TMetodoEnviarEvento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoXMLEnviarEvento}
TMetodoXMLEnviarEvento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDistribuicaoDFePorChaveBPe}
TMetodoDistribuicaoDFePorChaveBPe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDistribuicaoDFePorUltNSU}
TMetodoDistribuicaoDFePorUltNSU = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDistribuicaoDFePorNSU}
TMetodoDistribuicaoDFePorNSU = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviaremailEvento}
TMetodoEnviaremailEvento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoValidarRegrasNegocios}
TMetodoValidarRegrasNegocios = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetTipoImpressao}
TMetodoSetTipoImpressao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

uses
  IniFiles, DateUtils, Forms, strutils,
  ACBrDFeConfiguracoes,
  pcnConversao, ACBrBPeConversao,
  pcnAuxiliar, ACBrBPeXmlReader, DoACBrUnit, ACBrBPeClass;

{ TACBrObjetoBPe }

constructor TACBrObjetoBPe.Create(AConfig: TMonitorConfig; ACBrBPe: TACBrBPe);
begin
  inherited Create(AConfig);

  fACBrBPe := ACBrBPe;

  ListaDeMetodos.Add(CMetodostatusservico);
  ListaDeMetodos.Add(CMetodoValidarBPe);
  ListaDeMetodos.Add(CMetodoAssinarBPe);
  ListaDeMetodos.Add(CMetodoConsultarBPe);
  ListaDeMetodos.Add(CMetodoCancelarBPe);
  ListaDeMetodos.Add(CMetodoImprimirDABPe);
  ListaDeMetodos.Add(CMetodoImprimirEvento);
  ListaDeMetodos.Add(CMetodoEnviarBPe);
  ListaDeMetodos.Add(CMetodoCriarBPe);
  ListaDeMetodos.Add(CMetodoCriarEnviarBPe);
  ListaDeMetodos.Add(CMetodoEnviarEmail);
  ListaDeMetodos.Add(CMetodoSetAmbiente);
  ListaDeMetodos.Add(CMetodoSetFormaEmissao);
  ListaDeMetodos.Add(CMetodoSetVersaoDF);
  ListaDeMetodos.Add(CMetodoLerBPe);
  ListaDeMetodos.Add(CMetodoFileexist);
  ListaDeMetodos.Add(CMetodoCertificadodatavencimento);
  ListaDeMetodos.Add(CMetodoGerarchave);
  ListaDeMetodos.Add(CMetodoVersao);
  ListaDeMetodos.Add(CMetodoCNPJCertificado);
  ListaDeMetodos.Add(CMetodoGetPathBPe);
  ListaDeMetodos.Add(CMetodoGetPathEvento);
  ListaDeMetodos.Add(CMetodoGerarINIBPe);
  ListaDeMetodos.Add(CMetodoEnviarEvento);
  ListaDeMetodos.Add(CMetodoXMLEnviarEvento);
  ListaDeMetodos.Add(CMetodoDistribuicaoDFeporChaveBPe);
  ListaDeMetodos.Add(CMetodoDistribuicaoDFeporNSU);
  ListaDeMetodos.Add(CMetodoDistribuicaoDFeporUltNSU);
  ListaDeMetodos.Add(CMetodoEnviarEmailEvento);
  ListaDeMetodos.Add(CMetodoValidarRegrasNegocios);
  ListaDeMetodos.Add(CMetodoDataVencimentoCertificado);

  // DoACBr
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

procedure TACBrObjetoBPe.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
  AACBrUnit: TACBrObjetoACBr;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoStatusServico;
    1  : AMetodoClass := TMetodoValidarBPe;
    2  : AMetodoClass := TMetodoAssinarBPe;
    3  : AMetodoClass := TMetodoConsultarBPe;
    4  : AMetodoClass := TMetodoCancelarBPe;
    5  : AMetodoClass := TMetodoImprimirDaBPe;
    6  : AMetodoClass := TMetodoImprimirEvento;
    7  : AMetodoClass := TMetodoEnviarBPe;
    8  : AMetodoClass := TMetodoCriarBPe;
    9  : AMetodoClass := TMetodoCriarEnviarBPe;
    10 : AMetodoClass := TMetodoEnviarEmail;
    11 : AMetodoClass := TMetodoSetAmbiente;
    12 : AMetodoClass := TMetodoSetformaEmissao;
    13 : AMetodoClass := TMetodoSetVersaoDF;
    14 : AMetodoClass := TMetodoLerBPe;
    15 : AMetodoClass := TMetodoFileExists;
    16 : AMetodoClass := TMetodoCertificadoDataVencimento;
    17 : AMetodoClass := TMetodoGeraChave;
    18 : AMetodoClass := TMetodoVersao;
    19 : AMetodoClass := TMetodoCNPJCertificado;
    20 : AMetodoClass := TMetodoGetPathBPe;
    21 : AMetodoClass := TMetodoGetPathEvento;
    22 : AMetodoClass := TMetodoLerBPe; // GerarIniBPe
    23 : AMetodoClass := TMetodoEnviarEvento;
    24 : AMetodoClass := TMetodoXMLEnviarEvento;
    25 : AMetodoClass := TMetodoDistribuicaoDFeporChaveBPe;
    26 : AMetodoClass := TMetodoDistribuicaoDFeporNSU;
    27 : AMetodoClass := TMetodoDistribuicaoDFeporUltNSU;
    28 : AMetodoClass := TMetodoEnviaremailEvento;
    29 : AMetodoClass := TMetodoValidarRegrasNegocios;
    30 : AMetodoClass := TMetodoCertificadoDataVencimento; // DataVencimentoCertificado

  else
    begin
        AACBrUnit := TACBrObjetoACBr.Create(Nil); //Instancia DoACBrUnit para validar métodos padrão para todos os objetos
        try
          AACBrUnit.Executar(ACmd);
        finally
          AACBrUnit.Free;
        end;

      end;
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

procedure TACBrObjetoBPe.RespostaEnvio;
var
  Resp: TEnvioResposta;
begin
  Resp := TEnvioResposta.Create(TpResp, codUTF8);
  try
    with fACBrBPe.WebServices.Enviar do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TipoAmbienteToStr(TpAmb);
      Resp.verAplic := verAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := xMotivo;
      Resp.CUF := cUF;
//      Resp.nRec := Recibo;
      Resp.DhRecbto := dhRecbto;
      Resp.Tmed := TMed;
      Resp.Msg := Msg;
      Resp.NProt := Protocolo;

      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + Msg + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoBPe.RespostaBilhetes(pImprimir: Boolean; pImpressora: String;
          pPreview: Boolean; pCopias: Integer; pPDF: Boolean);
var
  I: integer;
  ArqPDF: String;
begin
  with fACBrBPe do
  begin
      for I := 0 to Bilhetes.Count - 1 do
      begin
          DoConfiguraDABPe(False, BoolToStr(pPreview,'1',''));
          if NaoEstaVazio(pImpressora) then
            DABPe.Impressora := pImpressora;

          if pCopias > 0 then
            DABPe.NumCopias := pCopias;

          if pPDF then
          begin
            Bilhetes.Items[I].ImprimirPDF;
            ArqPDF := OnlyNumber(ACBrBPe.Bilhetes.Items[I].BPe.infBPe.ID)+'-bpe.pdf';

            fpCmd.Resposta :=  fpCmd.Resposta + sLineBreak +
              'PDF='+ PathWithDelim(ACBrBPe.DABPe.PathPDF) + ArqPDF;
          end;

          if (Bilhetes.Items[I].Confirmada) and (pImprimir) then
          begin
//            try
//              DoAntesDeImprimir((pPreview));
              Bilhetes.Items[I].Imprimir;
//            finally
//              DoDepoisDeImprimir;
//            end;
          end;

          break;
        end;

  end;

end;

procedure TACBrObjetoBPe.RespostaStatus;
var
  Resp: TStatusServicoResposta;
begin
  Resp := TStatusServicoResposta.Create(TpResp, codUTF8);
  try
    with fACBrBPe.WebServices.StatusServico do
    begin
      Resp.Versao := versao;
      Resp.TpAmb := TipoAmbienteToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cUF;
      Resp.DhRecbto := dhRecbto;
      Resp.tMed := TMed;
      Resp.dhRetorno := dhRetorno;
      Resp.xObs := xObs;
      Resp.Msg := Msg;

      fpCmd.Resposta := Msg;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoBPe.RespostaConsulta;
var
  Resp: TConsultaBPeResposta;
begin
  Resp := TConsultaBPeResposta.Create(TpResp, codUTF8);
  try
    with fACBrBPe.WebServices.Consulta do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TipoAmbienteToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cUF;
      Resp.ChBPe := BPeChave;
      Resp.DhRecbto := dhRecbto;
      Resp.NProt := Protocolo;
      Resp.digVal := protBPe.digVal;
      Resp.Msg := Msg;

      fpCmd.Resposta := Msg + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoBPe.RespostaCancelamento;
var
  Resp: TCancelamentoResposta;
begin
  Resp := TCancelamentoResposta.Create(TpResp, codUTF8);
  try
    with fACBrBPe.WebServices.EnvEvento.EventoRetorno.retInfEvento do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TipoAmbienteToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cOrgao;
      Resp.ChBPe := chBPe;
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

procedure TACBrObjetoBPe.RespostaEvento;
var
  Resp: TEventoItemResposta;
  LTipoEvento: TpcnTpEvento;
  LRetInfEvento: TRetInfEvento;
begin
  LTipoEvento := fACBrBPe.WebServices.EnvEvento.EventoRetorno.retInfEvento.tpEvento;
  Resp := TEventoItemResposta.Create(
    'EVENTO' + TpEventoToStr(LTipoEvento), TpResp, codUTF8);
  try
    LRetInfEvento := fACBrBPe.WebServices.EnvEvento.EventoRetorno.retInfEvento;
    Resp.Id := LRetInfEvento.Id;
    Resp.tpAmb := TipoAmbienteToStr(LRetInfEvento.tpAmb);
    Resp.verAplic := LRetInfEvento.verAplic;
    Resp.cOrgao := LRetInfEvento.cOrgao;
    Resp.cStat := LRetInfEvento.cStat;
    Resp.xMotivo := LRetInfEvento.xMotivo;
    Resp.chBPe := LRetInfEvento.chBPe;
    Resp.tpEvento := TpEventoToStr(LTipoEvento);
    Resp.xEvento := LRetInfEvento.xEvento;
    Resp.nSeqEvento := LRetInfEvento.nSeqEvento;
    Resp.CNPJDest := LRetInfEvento.CNPJDest;
    Resp.emailDest := LRetInfEvento.emailDest;
    Resp.dhRegEvento := LRetInfEvento.dhRegEvento;
    Resp.nProt := LRetInfEvento.nProt;
    Resp.Arquivo := LRetInfEvento.NomeArquivo;
    Resp.XML := LRetInfEvento.XML;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoBPe.RespostaDistribuicaoDFe;
var
  Resp: TDistribuicaoDFeResposta;
  sTemMais: String;
begin
  fpCmd.Resposta := 'Serviço ainda não disponibilizado pela Sefaz';
//  Resp := TDistribuicaoDFeResposta.Create(TpResp, codUTF8);
//  try
//    with fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt do
//    begin
//      Resp.Versao := versao;
//      Resp.VerAplic := VerAplic;
//      Resp.tpAmb := TipoAmbienteToStr(tpAmb);
//      Resp.CStat := cStat;
//      Resp.XMotivo := XMotivo;
//      Resp.dhResp := dhResp;
//      Resp.ultNSU := ultNSU;
//      Resp.maxNSU := maxNSU;
//      Resp.arquivo := fACBrBPe.WebServices.DistribuicaoDFe.NomeArq;
//
//      if cStat = 137 then
//        sTemMais := '1'  // Sim
//      else
//        sTemMais := '0'; // Não
//
//      Resp.indCont := sTemMais;
//
//      fpCmd.Resposta := Resp.Gerar;
////      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
//    end;
//  finally
//    Resp.Free;
//  end;
end;

procedure TACBrObjetoBPe.RespostaItensDistribuicaoDFeResBPe(ItemID: integer);
var
  Resp: TDistribuicaoDFeItemResposta;
begin
  fpCmd.Resposta := 'Serviço ainda não disponibilizado pela Sefaz';
  //Resp := TDistribuicaoDFeItemResposta.Create(
  //  'ResBPe' + Trim(IntToStrZero(ItemID +1, 3)), TpResp, codUTF8);
  //try
  //  with fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].resDFe do
  //  begin
  //    Resp.NSU := fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].NSU;
  //    Resp.chBPe := chDFe;
  //    Resp.CNPJCPF := CNPJCPF;
  //    Resp.xNome := xNome;
  //    Resp.IE := IE;
  //    Resp.dhEmi := dhEmi;
  //    Resp.vNF := vNF;
  //    Resp.digVal := digVal;
  //    Resp.dhRecbto := dhRecbto;
  //    Resp.cSitBPe := SituacaoDFeToStr(cSitDFe);
  //    Resp.nProt := nProt;
  //    Resp.XML := fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].XML;
  //    Resp.Arquivo := fACBrBPe.WebServices.DistribuicaoDFe.listaArqs[ItemID];
  //    Resp.schema := SchemaDFeToStr(fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[ItemID].schema);
  //
  //    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  //  end;
  //finally
  //  Resp.Free;
  //end;
end;

procedure TACBrObjetoBPe.RespostaItensDistribuicaoDFeResEve(ItemID: integer);
//var
//  Resp: TDistribuicaoDFeItemResposta;
begin
  // Atualmente o DistribuicaoDFe do BP-e não retorna Resumo de Eventos.

end;

procedure TACBrObjetoBPe.RespostaItensDistribuicaoDFeProEve(ItemID: integer);
var
  Resp: TDistribuicaoDFeItemResposta;
begin
  fpCmd.Resposta := 'Serviço ainda não disponibilizado pela Sefaz';
//  Resp := TDistribuicaoDFeItemResposta.Create(
//    'ProEve' + Trim(IntToStrZero(ItemID +1, 3)), TpResp, codUTF8);
//  try
//    with fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].procEvento do
//    begin
//      Resp.NSU := fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].NSU;
//      Resp.chBPe := chDFe;
//      Resp.cOrgao := cOrgao;
//      Resp.CNPJ := CNPJ;
//      Resp.Id := Id;
//      Resp.dhEvento := dhEvento;
//      Resp.nSeqEvento := nSeqEvento;
//      Resp.tpAmb := TipoAmbienteToStr(tpAmb);
//      Resp.tpEvento := TpEventoToStr(tpEvento);
//      Resp.verEvento := verEvento;
//
//      with detEvento do
//      begin
//        Resp.descEvento := descEvento;
//        Resp.xJust := xJust;
//        Resp.EmiCnpj := emit.CNPJ;
//        Resp.EmiIE := emit.IE;
//        Resp.EmixNome := emit.xNome;
////        Resp.cteNProt := BPe.nProt;
////        Resp.cteChvCte := BPe.chBPe;
////        Resp.cteDhemi := BPe.dhEmi;
////        Resp.cteModal := TpModalToStr(BPe.modal);
////        Resp.cteDhRebcto := BPe.dhRecbto;
//      end;
//
//      Resp.XML := fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].XML;
//      Resp.Arquivo := fACBrBPe.WebServices.DistribuicaoDFe.listaArqs[ItemID];
//      Resp.schema := SchemaDFeToStr(fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[ItemID].schema);
//
//      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
//    end;
//  finally
//    Resp.Free;
//  end;
end;

procedure TACBrObjetoBPe.RespostaItensDistribuicaoDFeInfeve(ItemID: integer);
var
  Resp: TDistribuicaoDFeItemResposta;
begin
  fpCmd.Resposta := 'Serviço ainda não disponibilizado pela Sefaz';
  //Resp := TDistribuicaoDFeItemResposta.Create(
  //  'Infeve' + Trim(IntToStrZero(ItemID +1, 3)), TpResp, codUTF8);
  //try
  //  with fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].procEvento.RetInfevento do
  //  begin
  //    Resp.Id := Id;
  //    Resp.VerAplic := VerAplic;
  //    Resp.tpAmb := TipoAmbienteToStr(tpAmb);
  //    Resp.cOrgao := cOrgao;
  //    Resp.chBPe := chDFe;
  //    Resp.CStat := cStat;
  //    Resp.CNPJDest := CNPJDest;
  //    Resp.cOrgaoAutor := cOrgaoAutor;
  //    Resp.tpEvento := TpEventoToStr(tpEvento);
  //    Resp.nSeqEvento := nSeqEvento;
  //    Resp.xEvento := xEvento;
  //    Resp.XMotivo := XMotivo;
  //    Resp.dhRegEvento := dhRegEvento;
  //    Resp.emailDest := emailDest;
  //    Resp.nProt := nProt;
  //
  //    Resp.XML := fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].XML;
  //    Resp.Arquivo := fACBrBPe.WebServices.DistribuicaoDFe.listaArqs[ItemID];
  //    Resp.schema := SchemaDFeToStr(fACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[ItemID].schema);
  //
  //    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  //  end;
  //finally
  //  Resp.Free;
  //end;
end;

function TACBrObjetoBPe.GerarBPeIni(XML: string): string;
var
  INIRec: TMemIniFile;
  IniBPe: TStringList;
  LocBPeR: TBPeXmlReader;
begin
  INIRec := TMemIniFile.Create('BPe.ini');

  try
    fACBrBPe.Bilhetes.Clear;

    if FilesExists(XML) then
      fACBrBPe.Bilhetes.LoadFromFile(XML)
    else
    begin
      LocBPeR := TBPeXmlReader.Create(fACBrBPe.Bilhetes.Add.BPe);
      try
        LocBPeR.Arquivo := ConvertStrRecived(XML);
        LocBPeR.LerXml;
        fACBrBPe.Bilhetes.Items[0].XML := LocBPeR.Arquivo;
        fACBrBPe.Bilhetes.GerarBPe;
      finally
        LocBPeR.Free;
      end;
    end;

    IniBPe := TStringList.Create;
    try
      IniBPe.Text := fACBrBPe.Bilhetes.GerarIni();
      INIRec.SetStrings(IniBPe);
      Result := IniBPe.Text;
    finally
      IniBPe.Free;
    end;
  finally
    INIRec.Free;
  end;
end;

{ TACBrCarregarBPe }

procedure TACBrCarregarBPe.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrBPe(FpACBrDFe).Bilhetes.LoadFromFile( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroBPeAbrir, [AValue]) ) );
end;

procedure TACBrCarregarBPe.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrBPe(FpACBrDFe).Bilhetes.LoadFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroBPeCarregar) );
end;

function TACBrCarregarBPe.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrBPe(FpACBrDFe).Bilhetes.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrBPe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrBPe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlBPe ;
end;

constructor TACBrCarregarBPe.Create(AACBrDFe: TACBrBPe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

{ TACBrCarregarBPeEvento }

procedure TACBrCarregarBPeEvento.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrBPe(FpACBrDFe).EventoBPe.LerXML( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroBPeAbrir, [AValue]) ) );
end;

procedure TACBrCarregarBPeEvento.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrBPe(FpACBrDFe).EventoBPe.LerXMLFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroBPeCarregar) );
end;

function TACBrCarregarBPeEvento.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrBPe(FpACBrDFe).EventoBPe.Evento.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrBPe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrBPe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlBPeEve ;
end;

constructor TACBrCarregarBPeEvento.Create(AACBrDFe: TACBrBPe; AXMLorFile: String);
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
          4 - Numero BPe
          5 - Tipo de Emissao
          6 - Data Emissao
          7 - CNPJ
}
procedure TMetodoGeraChave.Executar;
begin
  FCodUF := StrToInt(fpCmd.Params(0));
  FCodNumerico := StrToInt(fpCmd.Params(1));
  FModelo := StrToInt(fpCmd.Params(2));
  FSerie := StrToInt(fpCmd.Params(3));
  FnBPe := StrToInt(fpCmd.Params(4));
  FTpEmissao := StrToInt(fpCmd.Params(5));
  FEmissao := StrToDateTime(fpCmd.Params(6));
  FCNPJ := fpCmd.Params(7);

  fpCmd.Resposta := GerarChaveAcesso(FCodUF,
    FEmissao, FCNPJ, FSerie,
    FnBPe, FTpEmissao,
    FCodNumerico, FModelo);
end;

{ TMetodoCertificadoDataVencimento }

procedure TMetodoCertificadoDataVencimento.Executar;
begin
  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    fpCmd.Resposta := DateToStr(ACBrBPe.SSL.CertDataVenc);
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

{ TMetodoLerBPe }

{ Params: 0 - XML - String com path XML
}
procedure TMetodoLerBPe.Executar;
var
  AXML: String;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    try
      fpCmd.Resposta := GerarBPeIni(AXML);
    except
      on E: Exception do
        raise Exception.Create('Erro ao gerar INI da BPe.' + sLineBreak + E.Message);
    end;
  end;
end;

{ TMetodoSetVersaoDF }

{ Params: 0 - Inteiro com numero da Versao BPe valores: 2.00 - 3.00
}
procedure TMetodoSetVersaoDF.Executar;
var
  OK: boolean;
  VersaoDFe: TVersaoBPe;
  AVersao: String;
begin
  AVersao := fpCmd.Params(0);
  VersaoDFe := StrToVersaoBPe(AVersao);

  //if not VersaoDFe then
  //  raise Exception.Create('Versão Inválida.');

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.WebService do
      VersaoBPe := VersaoBPeToStr(VersaoDFe);

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoSetformaEmissao }

{ Params: 0 - Inteiro com numero da Forma Emissao
              1-Normal, 4-EPEC, 5-FSDA, 7-SVC-RS ou 8-SVC-SP
}
procedure TMetodoSetformaEmissao.Executar;
var
  OK: boolean;
  FormaEmissao: TpcnTipoEmissao;
  NFormaEmissao: Integer;
begin
  NFormaEmissao := StrToIntDef(fpCmd.Params(0), 1);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    OK := False;
    FormaEmissao := StrToTpEmis(OK, IntToStr(NFormaEmissao));

    if not OK then
      raise Exception.Create('Forma de Emissão Inválida: ' + TpEmisToStr(FormaEmissao));

    with MonitorConfig.DFE.WebService do
      FormaEmissaoBPe := StrToInt(TpEmisToStr(FormaEmissao)) - 1;

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

  with TACBrObjetoBPe(fpObjetoDono) do
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
  NumAmbiente: Integer;
begin
  NumAmbiente := StrToIntDef(fpCmd.Params(0), 2);

  if (NumAmbiente < 1) or (NumAmbiente > 2) then
    raise Exception.Create('Ambiente Inválido: ' + IntToStr(NumAmbiente));

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.WebService do
      Ambiente := NumAmbiente -1;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoEnviarEmail }

{ Params: 0 - Email: String com email Destinatário
          1 - XML: String com path do XML
          2 - Assunto: String com Assunto do e-mail
          3 - Copia: String com e-mails copia (Separados ;)
          4 - Anexo: String com Path de Anexos (Separados ;)
          5 - Replay: String com endereços replay (Separados ;)
}
procedure TMetodoEnviarEmail.Executar;
var
  sAssunto, ADestinatario, APathXML, AAssunto, AEmailCopias, AAnexos, AReplay: string;
  slMensagemEmail, slCC, slAnexos, slReplay: TStringList;
  CargaDFe: TACBrCarregarBPe;
  AEnviaPDF: Boolean;
begin
  AEnviaPDF := False;
  ADestinatario := fpCmd.Params(0);
  APathXML := fpCmd.Params(1);
  AAssunto := fpCmd.Params(2);
  AEmailCopias := fpCmd.Params(3);
  AAnexos := fpCmd.Params(4);
  AReplay := fpCmd.Params(5);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.Bilhetes.Clear;

    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    slReplay := TStringList.Create;
    try
      CargaDFe := TACBrCarregarBPe.Create(ACBrBPe, APathXML);
      try
        DoConfiguraDABPe(False, '');

        with MonitorConfig.DFE.Email do
        begin
          slMensagemEmail.Text := StringToBinaryString(MensagemBPe);
          sAssunto := AssuntoBPe;
        end;

        slCC.DelimitedText := sLineBreak;
        slCC.Text := StringReplace(AEmailCopias, ';', sLineBreak, [rfReplaceAll]);

        slAnexos.DelimitedText := sLineBreak;
        slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

        slReplay.DelimitedText := sLineBreak;
        slReplay.Text := StringReplace(AReplay, ';', sLineBreak, [rfReplaceAll]);

        try
          ACBrBPe.Bilhetes.Items[0].EnviarEmail(ADestinatario,
            IfThen( NaoEstaVazio(AAssunto), AAssunto, sAssunto),
            slMensagemEmail,
            AEnviaPDF,
            // Enviar PDF junto
            slCC,
            // Lista com emails que serão enviado cópias - TStrings
            slAnexos,
            // Lista de slAnexos - TStrings
            slReplay);
            // Lista de slReplay - TStrings

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
      slReplay.Free;
    end;
  end;
end;

{ TMetodoConsultarBPe }

{ Params: 0 - XML - Uma String com um Path completo XML ou chave BPe
          1 - AExtrairEventos (1 para extrair)
}
procedure TMetodoConsultarBPe.Executar;
var
  CargaDFe: TACBrCarregarBPe;
  AXML: String;
  AExtrairEventos: Boolean;
begin
  AXML := fpCmd.Params(0);
  AExtrairEventos := StrToBoolDef(fpCmd.Params(1), False);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.Bilhetes.Clear;
    CargaDFe := TACBrCarregarBPe.Create(ACBrBPe, AXML);
    try
      ACBrBPe.WebServices.Consulta.ExtrairEventos := AExtrairEventos;

      if (ACBrBPe.Bilhetes.Count = 0) then
      begin
        if ValidarChave(AXML) then
          ACBrBPe.WebServices.Consulta.BPeChave := AXML
        else
          raise Exception.Create(
            'Parâmetro inválido. Chave do BPe inválida ou arquivo não encontrado.');
      end
      else
        ACBrBPe.WebServices.Consulta.BPeChave :=
          OnlyNumber(ACBrBPe.Bilhetes.Items[0].BPe.infBPe.ID);


      ACBrBPe.WebServices.Consulta.Executar;
      RespostaConsulta;
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoAssinarBPe }

{ Params: 0 - XML - Uma String com um Path completo XML BPe
}
procedure TMetodoAssinarBPe.Executar;
var
  CargaDFe: TACBrCarregarBPe;
  Salva: boolean;
  AXML: String;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.Bilhetes.Clear;
    CargaDFe := TACBrCarregarBPe.Create(ACBrBPe, AXML);
    try
      Salva := ACBrBPe.Configuracoes.Arquivos.Salvar;

      if not Salva then
      begin
        ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
        ACBrBPe.Configuracoes.Arquivos.PathSalvar :=
          PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
      end;

      ACBrBPe.Configuracoes.Arquivos.Salvar := True;
      ACBrBPe.Bilhetes.Assinar;
      ACBrBPe.Configuracoes.Arquivos.Salvar := Salva;

      if NaoEstaVazio(ACBrBPe.Bilhetes.Items[0].NomeArq) then
        fpCmd.Resposta := ACBrBPe.Bilhetes.Items[0].NomeArq
      else
        fpCmd.Resposta := PathWithDelim(ACBrBPe.Configuracoes.Arquivos.PathSalvar)
          + StringReplace(ACBrBPe.Bilhetes.Items[0].BPe.infBPe.ID,
          'BPe', '', [rfIgnoreCase]) + '-bpe.xml';
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoValidarBPe }

{ Params: 0 - XML - Uma String com um Path completo XML BPe
}
procedure TMetodoValidarBPe.Executar;
var
  CargaDFe: TACBrCarregarBPe;
  AXML: String;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.Bilhetes.Clear;
    CargaDFe := TACBrCarregarBPe.Create(ACBrBPe, AXML);
    try
      ACBrBPe.Bilhetes.Validar;
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoStatusServico }

procedure TMetodoStatusServico.Executar;
begin
  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    if ACBrBPe.WebServices.StatusServico.Executar then
      RespostaStatus;
  end;
end;

{ TMetodoImprimirEventoPDF }

{ Params: 0 - XMLEvento - Uma String com um Path completo XMLEvento BPe
          1 - XML       - Uma String com um Path completo XML BPe
}
procedure TMetodoImprimirEventoPDF.Executar;
var
  CargaDFe: TACBrCarregarBPe;
  CargaDFeEvento: TACBrCarregarBPeEvento;
  ArqPDF, AXMLEvento, AXML: string;
begin
  AXMLEvento := fpCmd.Params(0);
  AXML := fpCmd.Params(1);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.EventoBPe.Evento.Clear;
    CargaDFeEvento := TACBrCarregarBPeEvento.Create(ACBrBPe, AXMLEvento);
    ACBrBPe.Bilhetes.Clear;
    CargaDFe := TACBrCarregarBPe.Create(ACBrBPe, AXML);
    try
      try
        DoConfiguraDABPe(False, '');
        ACBrBPe.ImprimirEventoPDF;
        ArqPDF := OnlyNumber(ACBrBPe.EventoBPe.Evento[0].Infevento.Id);
        ArqPDF := PathWithDelim(ACBrBPe.DABPe.PathPDF) + ArqPDF + '-procEventoBPe.pdf';

        fpCmd.Resposta := 'Arquivo criado em: ' + ArqPDF;
      except
        on E: Exception do
        raise Exception.Create('Erro ao criar o arquivo PDF. '+ sLineBreak + E.Message);
      end;
    finally
      CargaDFeEvento.Free;
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoImprimirEvento }

{ Params: 0 - XMLEvento - Uma String com um Path completo XMLEvento BPe
          1 - XMLBPe - Uma String com um Path completo XML BPe
          2 - String com nome Impressora
          3 - Integer Número de Cópias
}
procedure TMetodoImprimirEvento.Executar;
var
  CargaDFe: TACBrCarregarBPe;
  CargaDFeEvento: TACBrCarregarBPeEvento;
  AXMLEvento, AXML, AImpressora: String;
  ACopias: Integer;
begin
  AXMLEvento := fpCmd.Params(0);
  AXML := fpCmd.Params(1);
  AImpressora := fpCmd.Params(2);
  ACopias := StrToIntDef(fpCmd.Params(3), 0);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.EventoBPe.Evento.Clear;
    CargaDFeEvento := TACBrCarregarBPeEvento.Create(ACBrBPe, AXMLEvento);
    ACBrBPe.Bilhetes.Clear;
    CargaDFe := TACBrCarregarBPe.Create(ACBrBPe, AXML);
    try
      DoConfiguraDABPe(False, BoolToStr(False,'1',''));
      if NaoEstaVazio(AImpressora) then
        ACBrBPe.DABPe.Impressora := AImpressora;

      if (ACopias > 0) then
        ACBrBPe.DABPe.NumCopias := ACopias;

      ACBrBPe.ImprimirEvento;

    finally
      CargaDFeEvento.Free;
      CargaDFe.Free;
    end;

    fpCmd.Resposta := 'Evento Impresso com sucesso';
  end;
end;

{ TMetodoImprimirDaBPePDF }

{ Params: 0 - XML - Uma String com um Path completo XML BPe
          1 - Protocolo: String com Número de Protocolo
          2 - Cancelado: 1 para CT-e Cancelado
}
procedure TMetodoImprimirDaBPePDF.Executar;
var
  ArqPDF, AChave, AProtocolo: string;
  CargaDFe: TACBrCarregarBPe;
  ACancelado: Boolean;
begin
  AChave := fpCmd.Params(0);
  AProtocolo := fpCmd.Params(1);
  ACancelado := StrToBoolDef(fpCmd.Params(2), False);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.Bilhetes.Clear;
    CargaDFe := TACBrCarregarBPe.Create(ACBrBPe, AChave);
    try
      DoConfiguraDABPe(False, '');

      if NaoEstaVazio(AProtocolo) then
        ACBrBPe.DABPe.Protocolo := AProtocolo;

      ACBrBPe.DABPe.Cancelada := ACancelado;

      try
        ACBrBPe.Bilhetes.ImprimirPDF;
        ArqPDF := OnlyNumber(ACBrBPe.Bilhetes.Items[0].BPe.infBPe.ID) + '-bpe.pdf';
        ArqPDF := PathWithDelim(ACBrBPe.DABPe.PathPDF) + ArqPDF;
        fpCmd.Resposta := 'Arquivo criado em: ' + ArqPDF;
      except
        on E: Exception do
          raise Exception.Create('Erro ao criar o arquivo PDF. '+ sLineBreak + E.Message);
      end;
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoCriarEnviarBPe }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini BPe
                         ou Uma String com conteúdo txt do BPe
          1 - NumeroLote: Integer com número do lote a ser adicionado
          2 - Imprime : 1 para imprimir
          3 - NomeImpressora: String com nome impressora para impressão (Default)
          4 - Numero de Copias: Inteiro com número de cópias (Default)

}
procedure TMetodoCriarEnviarBPe.Executar;
var
  Salva, AImprime: boolean;
  Alertas: ansistring;
  ArqBPe: string;
  Resp, AIni, AImpressora: string;
  ALote: Integer;
  ACopias: Integer;
begin
  AIni := fpCmd.Params(0);
  ALote := StrToIntDef(fpCmd.Params(1), 0);
  AImprime := StrToBoolDef(fpCmd.Params(2), False);
  AImpressora := fpCmd.Params(3);
  ACopias := StrToIntDef(fpCmd.Params(4), 0);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.Bilhetes.Clear;
    ACBrBPe.Bilhetes.LoadFromIni(AIni);

    Salva := ACBrBPe.Configuracoes.Arquivos.Salvar;
    if not Salva then
    begin
      ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
      ACBrBPe.Configuracoes.Arquivos.PathSalvar :=
        PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
    end;

    ACBrBPe.Bilhetes.GerarBPe;
    Alertas := ACBrBPe.Bilhetes.Items[0].Alertas;

    ACBrBPe.Bilhetes.Assinar;
    ACBrBPe.Bilhetes.Validar;

    ArqBPe := PathWithDelim(ACBrBPe.Configuracoes.Arquivos.PathSalvar) +
      OnlyNumber(ACBrBPe.Bilhetes.Items[0].BPe.infBPe.ID) + '-bpe.xml';
    ACBrBPe.Bilhetes.GravarXML(ArqBPe);

    if not FileExists(ArqBPe) then
      raise Exception.Create('Não foi possível criar o arquivo ' + ArqBPe);

    Resp := ArqBPe;
    if (Alertas <> '') then
      Resp := Resp + sLineBreak + 'Alertas:' + Alertas;

    fpCmd.Resposta := Resp;

    if (ALote = 0) then
      ACBrBPe.WebServices.Enviar.Lote := '1'
    else
      ACBrBPe.WebServices.Enviar.Lote := IntToStr(ALote);

    ACBrBPe.WebServices.Enviar.Executar;
    RespostaEnvio;
    RespostaBilhetes(AImprime, AImpressora, False, ACopias, False);

  end;
end;

{ TMetodoEnviarBPe }

{ Params: 0 - PathorXML - Uma String com um Path completo arquivo XML BPe
                         ou Uma String com conteúdo XML do BPe
          1 - Lote: Integer com número do lote. Default = 1
          2 - Assina: 1 para assinar XML
          3 - Imprime: 1 Para True. Default 0
          4 - Nome Impressora: String com Nome da Impressora
}
procedure TMetodoEnviarBPe.Executar;
var
  CargaDFe: TACBrCarregarBPe;
  APathorXML, AImpressora: String;
  ALote: Integer;
  AAssina, AImprime: Boolean;
begin
  APathorXML := fpCmd.Params(0);
  ALote := StrToIntDef(fpCmd.Params(1), 0);
  AAssina := StrToBoolDef(fpCmd.Params(2), False);
  AImprime := StrToBoolDef(fpCmd.Params(3), False);
  AImpressora := fpCmd.Params(4);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.Bilhetes.Clear;
    CargaDFe := TACBrCarregarBPe.Create(ACBrBPe, APathorXML);
    try
      ACBrBPe.Bilhetes.GerarBPe;

      if (AAssina) then
        ACBrBPe.Bilhetes.Assinar;

      ACBrBPe.Bilhetes.Validar;

      if (ALote = 0) then
        ACBrBPe.WebServices.Enviar.Lote := '1'
      else
        ACBrBPe.WebServices.Enviar.Lote := IntToStr(ALote);

      ACBrBPe.WebServices.Enviar.Executar;
      RespostaEnvio;
      RespostaBilhetes(AImprime, AImpressora, False, 0, False);

    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoCriarBPe }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini BPe
                         ou Uma String com conteúdo txt do BPe
          1 - RetornaXML: 1 para Retornar XML Gerado na Resposta
          2 - Assina: 1 para assinar BPe
}
procedure TMetodoCriarBPe.Executar;
var
  Salva, ARetornaXML: boolean;
  Alertas: ansistring;
  ArqBPe: string;
  AAssina: Boolean;
  SL: TStringList;
  Resp, AIni: string;
begin
  AIni := fpCmd.Params(0);
  ARetornaXML := StrToBoolDef(fpCmd.Params(1), False);
  AAssina := StrToBoolDef(fpCmd.Params(2), False);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.Bilhetes.Clear;
    ACBrBPe.Bilhetes.LoadFromIni(AIni);

    Salva := ACBrBPe.Configuracoes.Arquivos.Salvar;
    if not Salva then
    begin
      ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
      ACBrBPe.Configuracoes.Arquivos.PathSalvar :=
        PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
    end;

    ACBrBPe.Bilhetes.GerarBPe;
    Alertas := ACBrBPe.Bilhetes.Items[0].Alertas;

    if AAssina then
    begin
      ACBrBPe.Bilhetes.Assinar;
      ACBrBPe.Bilhetes.Validar;
    end;

    ArqBPe := PathWithDelim(ACBrBPe.Configuracoes.Arquivos.PathSalvar) +
      OnlyNumber(ACBrBPe.Bilhetes.Items[0].BPe.infBPe.ID) + '-bpe.xml';
    ACBrBPe.Bilhetes.GravarXML(ArqBPe);

    if not FileExists(ArqBPe) then
      raise Exception.Create('Não foi possível criar o arquivo ' + ArqBPe);

    Resp := ArqBPe;
    if (Alertas <> '') then
      Resp := Resp + sLineBreak + 'Alertas:' + Alertas;

    if ARetornaXML then
    begin
      SL := TStringList.Create;
      try
        SL.LoadFromFile(ArqBPe);
        Resp := Resp + sLineBreak + SL.Text;
      finally
        SL.Free;
      end;
    end;

    fpCmd.Resposta := Resp;
  end;
end;

{ TMetodoImprimirDaBPe }

{ Params: 0 - XMLFile - Uma String com um Path completo para um arquivo XML BPe
                         ou Uma String com conteúdo XML BPe
          1 - Impressora: String com Nome da Impressora
          2 - Copias: Integer Número de Copias
          3 - Protocolo: String com Número de Protocolo
          4 - Cancelado: 1 para BP-e Cancelado

}
procedure TMetodoImprimirDaBPe.Executar;
var
  CargaDFe: TACBrCarregarBPe;
  AChave, AImpressora, AProtocolo: String;
  ACopias: Integer;
  ACancelado: Boolean;
begin
  AChave := fpCmd.Params(0);
  AImpressora := fpCmd.Params(1);
  ACopias := StrToIntDef(fpCmd.Params(2), 0);
  AProtocolo := fpCmd.Params(3);
  ACancelado := StrToBoolDef(fpCmd.Params(4), False);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.Bilhetes.Clear;
    CargaDFe := TACBrCarregarBPe.Create(ACBrBPe, AChave);
    try
      DoConfiguraDABPe(False, BoolToStr(False,'1',''));

      if NaoEstaVazio(AImpressora) then
        ACBrBPe.DABPe.Impressora := AImpressora;

      if (ACopias > 0) then
        ACBrBPe.DABPe.NumCopias := ACopias;

      if NaoEstaVazio(AProtocolo) then
        ACBrBPe.DABPe.Protocolo := AProtocolo;

      ACBrBPe.DABPe.Cancelada := ACancelado;

      ACBrBPe.Bilhetes.Imprimir;

    finally
      CargaDFe.Free;
    end;

    fpCmd.Resposta := 'DABPe Impresso com sucesso';
  end;
end;

{ TMetodoCancelarBPe }

{ Params: 0 - Chave - Uma String com a Chave XML BPe
          1 - Justificativa: String (Mínimo de 15 Carcteres)
          2 - CNPJ: String com CNPJ do emitente
          3 - Lote: Integer com Numero do Lote. Default = 1
}
procedure TMetodoCancelarBPe.Executar;
var
  AChave, AJustificativa, ACNPJ: String;
  ALote: Integer;
begin
  AChave := fpCmd.Params(0);
  AJustificativa := fpCmd.Params(1);
  ACNPJ := fpCmd.Params(2);
  ALote := StrToIntDef(fpCmd.Params(3), 1);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    if not ValidarChave(AChave) then
      raise Exception.Create('Chave ' + AChave + ' inválida.')
    else
      ACBrBPe.WebServices.Consulta.BPeChave := AChave;

    ACBrBPe.WebServices.Consulta.Executar;

    ACBrBPe.EventoBPe.Evento.Clear;
    with ACBrBPe.EventoBPe.Evento.New do
    begin
      Infevento.CNPJ := ACNPJ;
      if Trim(Infevento.CNPJ) = '' then
        Infevento.CNPJ := copy(OnlyNumber(ACBrBPe.WebServices.Consulta.BPeChave), 7, 14)
      else
      begin
        if not ValidarCNPJouCPF(ACNPJ) then
          raise Exception.Create('CNPJ/CPF ' + ACNPJ + ' inválido.');
      end;

      Infevento.cOrgao := StrToIntDef(
        copy(OnlyNumber(ACBrBPe.WebServices.Consulta.BPeChave), 1, 2), 0);
      Infevento.dhEvento := now;
      Infevento.tpEvento := teCancelamento;
      Infevento.chBPe := ACBrBPe.WebServices.Consulta.BPeChave;
      Infevento.detEvento.nProt := ACBrBPe.WebServices.Consulta.Protocolo;
      Infevento.detEvento.xJust := AJustificativa;
    end;

    ACBrBPe.EnviarEvento(ALote);
    RespostaCancelamento;

  end;
end;

{ TMetodoCNPJCertificado }

procedure TMetodoCNPJCertificado.Executar;
begin
  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrBPe.SSL.CertCNPJ;
  end;
end;

{ TMetodoGetPathBPe }

procedure TMetodoGetPathBPe.Executar;
begin
  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrBPe.Configuracoes.Arquivos.GetPathBPe();
  end;
end;

{ TMetodoGetPathCCe }

procedure TMetodoGetPathCCe.Executar;
begin
  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrBPe.Configuracoes.Arquivos.GetPathEvento(teCCe);
  end;
end;

{ TMetodoGetPathCan }

procedure TMetodoGetPathCan.Executar;
begin
  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrBPe.Configuracoes.Arquivos.GetPathEvento(teCancelamento);
  end;
end;

{ TMetodoGetPathEvento }

{ Params: 0 - Código do evento
}
procedure TMetodoGetPathEvento.Executar;
var
  CodEvento: Integer;
begin
  CodEvento := StrToInt(fpCmd.Params(0));

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrBPe.Configuracoes.Arquivos.GetPathEvento(TpcnTpEvento(CodEvento));
  end;
end;

{ TMetodoEnviarEvento }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini Evento
                         ou Uma String com conteúdo txt do Evento
}
procedure TMetodoEnviarEvento.Executar;
var
  AArq: String;
  I: Integer;
begin
  AArq := fpCmd.Params(0);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.EventoBPe.Evento.Clear;

    ACBrBPe.EventoBPe.LerFromIni( AArq );

    ACBrBPe.EnviarEvento(ACBrBPe.EventoBPe.idLote);

    RespostaEvento;
  end;
end;

{ TMetodoXMLEnviarEvento }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini Evento
                         ou Uma String com conteúdo txt do Evento
}
procedure TMetodoXMLEnviarEvento.Executar;
var
  AArq: String;
  I: Integer;
  CargaDFeEvento: TACBrCarregarBPeEvento;
begin
  AArq := fpCmd.Params(0);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.EventoBPe.Evento.Clear;
    CargaDFeEvento := TACBrCarregarBPeEvento.Create(ACBrBPe, AArq);

    try
      ACBrBPe.EnviarEvento(ACBrBPe.EventoBPe.idLote);

    finally
      CargaDFeEvento.Free;
    end;

    RespostaEvento;
  end;
end;

{ TMetodoDistribuicaoDFePorChaveBPe }

{ Params: 0 - Código da UF do autor da consulta
          1 - CNPJ do autor da consulta
          2 - Chave da NF-e que se deseja baixar o XML
}
procedure TMetodoDistribuicaoDFePorChaveBPe.Executar;
var
  AUF: Integer;
  ACNPJ: String;
  AChave: String;
  I: Integer;
begin
  AUF := StrToIntDef(fpCmd.Params(0), 0);
  ACNPJ := fpCmd.Params(1);
  AChave := fpCmd.Params(2);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    if not ValidarCNPJouCPF(ACNPJ) then
      raise Exception.Create('CNPJ/CPF '+ACNPJ+' inválido.');

    ACBrBPe.DistribuicaoDFePorChaveBPe(AUF, ACNPJ, AChave);

    RespostaDistribuicaoDFe;

    //for I := 0 to ACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
    //  RespostaItensDistribuicaoDFeResBPe(I);
    //
    ////for I := 0 to ACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
    //  //RespostaItensDistribuicaoDFeResEve(I);
    //
    //for I := 0 to ACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
    //  RespostaItensDistribuicaoDFeProEve(I);
    //
    //for I := 0 to ACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
    //  RespostaItensDistribuicaoDFeInfEve(I);
  end;
end;

{ TMetodoDistribuicaoDFePorUltNSU }

{ Params: 0 - Código da UF do autor da consulta
          1 - CNPJ do autor da consulta
          2 - Numero do último NSU retornado na consulta anterior
}
procedure TMetodoDistribuicaoDFePorUltNSU.Executar;
var
  AUF: Integer;
  ACNPJ: String;
  AUltNSU: String;
  I: Integer;
begin
  AUF := StrToIntDef(fpCmd.Params(0), 0);
  ACNPJ := fpCmd.Params(1);
  AUltNSU := fpCmd.Params(2);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    if not ValidarCNPJouCPF(ACNPJ) then
      raise Exception.Create('CNPJ/CPF '+ACNPJ+' inválido.');

    ACBrBPe.DistribuicaoDFePorUltNSU(AUF, ACNPJ, AUltNSU);

    RespostaDistribuicaoDFe;

    //for I := 0 to ACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
    //  RespostaItensDistribuicaoDFeResBPe(I);
    //
    ////for I := 0 to ACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
    //  //RespostaItensDistribuicaoDFeResEve(I);
    //
    //for I := 0 to ACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
    //  RespostaItensDistribuicaoDFeProEve(I);
    //
    //for I := 0 to ACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
    //  RespostaItensDistribuicaoDFeInfeve(I);
  end;
end;

{ TMetodoDistribuicaoDFePorNSU }

{ Params: 0 - Código da UF do autor da consulta
          1 - CNPJ do autor da consulta
          2 - Numero do NSU a ser consultado
}
procedure TMetodoDistribuicaoDFePorNSU.Executar;
var
  AUF: Integer;
  ACNPJ: String;
  ANSU: String;
  I: Integer;
begin
  AUF := StrToIntDef(fpCmd.Params(0), 0);
  ACNPJ := fpCmd.Params(1);
  ANSU := fpCmd.Params(2);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    if not ValidarCNPJouCPF(ACNPJ) then
      raise Exception.Create('CNPJ/CPF '+ACNPJ+' inválido.');

    ACBrBPe.DistribuicaoDFePorNSU(AUF, ACNPJ, ANSU);

    RespostaDistribuicaoDFe;

    //for I := 0 to ACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
    //  RespostaItensDistribuicaoDFeResBPe(I);
    //
    ////for I := 0 to ACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
    //  //RespostaItensDistribuicaoDFeResEve(I);
    //
    //for I := 0 to ACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
    //  RespostaItensDistribuicaoDFeProEve(I);
    //
    //for I := 0 to ACBrBPe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
    //  RespostaItensDistribuicaoDFeInfeve(I);
  end;
end;

{ TMetodoEnviaremailEvento }

{ Params: 0 - Email: String com email Destinatário
          1 - XMLEvento: String com path do XML Evento
          2 - XMLBPe: String com path do XML BPe
          3 - Boolean 1 : Envia PDF
          4 - Assunto: String com Assunto do e-mail
          5 - Copia: String com e-mails copia (Separados ;)
          6 - Anexo: String com Path de Anexos (Separados ;)
          7 - Replay: String com endereços replay (Separados ;)
}
procedure TMetodoEnviaremailEvento.Executar;
var
  sAssunto, ADestinatario, APathXMLEvento, APathXML, AAssunto, AEmailCopias,
  AAnexos, ArqPDF, ArqEvento, AReplay: string;
  slMensagemEmail, slCC, slAnexos, slReplay: TStringList;
  CargaDFeEvento: TACBrCarregarBPeEvento;
  CargaDFe: TACBrCarregarBPe;
  AEnviaPDF: Boolean;
  TipoEvento: TpcnTpEvento;
begin
  AEnviaPDF:= False;
  ADestinatario := fpCmd.Params(0);
  APathXMLEvento := fpCmd.Params(1);
  APathXML := fpCmd.Params(2);
  AAssunto := fpCmd.Params(3);
  AEmailCopias := fpCmd.Params(4);
  AAnexos := fpCmd.Params(5);
  AReplay := fpCmd.Params(6);
  ArqEvento := '';

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.EventoBPe.Evento.Clear;
    ACBrBPe.Bilhetes.Clear;

    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    slReplay := TStringList.Create;
    try
      CargaDFeEvento := TACBrCarregarBPeEvento.Create(ACBrBPe, APathXMLEvento);
      CargaDFe := TACBrCarregarBPe.Create(ACBrBPe, APathXML);
      try
        DoConfiguraDABPe(False, '');
        if AEnviaPDF then
        begin
          try
            ACBrBPe.ImprimirEventoPDF;

            ArqPDF := OnlyNumber(ACBrBPe.EventoBPe.Evento[0].Infevento.id);
            ArqPDF := PathWithDelim(ACBrBPe.DABPe.PathPDF)+ArqPDF+'-procEventoBPe.pdf';
          except
            on E: Exception do
              raise Exception.Create('Erro ao criar o arquivo PDF.'+ sLineBreak + E.Message);
          end;
        end;

        with MonitorConfig.DFE.Email do
        begin
          slMensagemEmail.Text := StringToBinaryString(MensagemBPe);
          sAssunto := AssuntoBPe;
        end;

        slCC.DelimitedText := sLineBreak;
        slCC.Text := StringReplace(AEmailCopias, ';', sLineBreak, [rfReplaceAll]);

        slAnexos.DelimitedText := sLineBreak;
        slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

        slReplay.DelimitedText := sLineBreak;
        slReplay.Text := StringReplace(AReplay, ';', sLineBreak, [rfReplaceAll]);

        // Se carregou evento usando XML como parâmetro, salva XML para poder anexar
        if ( ArqEvento = '' ) then
        begin
          tipoEvento := ACBrBPe.EventoBPe.Evento[0].Infevento.tpEvento;
          ArqEvento  := ACBrBPe.EventoBPe.ObterNomeArquivo(tipoEvento);
          ArqEvento  := PathWithDelim(ACBrBPe.Configuracoes.Arquivos.GetPathEvento(tipoEvento))+ArqEvento;
          //ACBrBPe.EventoBPe.Gerador.SalvarArquivo(ArqEvento);
          WriteToTxt(ArqEvento, ACBrBPe.EventoBPe.Evento[0].RetInfEvento.XML, False, False);
        end;
        slAnexos.Add(ArqEvento);

        if AEnviaPDF then
          slAnexos.Add(ArqPDF);

        try
          ACBrBPe.EnviarEmail(ADestinatario,
            IfThen(NaoEstaVazio(AAssunto), AAssunto, sAssunto),
            slMensagemEmail,
            slCC,      // Lista com emails que serão enviado cópias - TStrings
            slAnexos,  // Lista de slAnexos - TStrings
            nil,
            '',
            slReplay); // Lista de slReplay - TStrings

          fpCmd.Resposta := 'Email enviado com sucesso';
        except
          on E: Exception do
            raise Exception.Create('Erro ao enviar email' + sLineBreak + E.Message);
        end;
      finally
        CargaDFe.Free;
        CargaDFeEvento.Free;
      end;
    finally
      slCC.Free;
      slAnexos.Free;
      slMensagemEmail.Free;
      slReplay.Free;
    end;
  end;
end;

{ TMetodoValidarRegrasNegocios }

{ Params: 0 - XML - String com path XML
}
procedure TMetodoValidarRegrasNegocios.Executar;
var
  CargaDFe: TACBrCarregarBPe;
  AXML, ErrosRegraNegocio: string;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    ACBrBPe.Bilhetes.Clear;
    CargaDFe := TACBrCarregarBPe.Create(ACBrBPe, AXML);
    try
      ACBrBPe.Bilhetes.ValidarRegrasdeNegocios(ErrosRegraNegocio);

      if NaoEstaVazio(ErrosRegraNegocio) then
        raise Exception.Create(ErrosRegraNegocio);
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoSetTipoImpressao }

{ Params: 0 - Inteiro com o Tipo de Impressão
              1 = Retrato
              2 = Paisagem
}
procedure TMetodoSetTipoImpressao.Executar;
var
  TipodeDABPe: TpcnTipoImpressao;
  Ok: Boolean;
  TpImp: string;
begin
  TpImp := fpCmd.Params(0);

  with TACBrObjetoBPe(fpObjetoDono) do
  begin
    OK := False;

    if (TpImp = '1') or (TpImp = '2') then
      TipodeDABPe := StrToTpImp(OK, tpImp);

    if not OK then
      raise Exception.Create('Tipo Impressão Inválido: ' + tpImp);

    ACBrBPe.DABPe.TipoDABPe := TipodeDABPe;
  end;
end;

end.
