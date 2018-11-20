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

unit DoACBrNFeUnit;

interface

uses
  Classes, SysUtils, ACBrUtil, ACBrLibNFeRespostas,
  ACBrNFe, ACBrLibResposta, ACBrMonitorConfig,
  ACBrMonitorConsts, ACBrDFeUtil, UtilUnit, DoACBrDFeUnit,
  CmdUnit, ACBrNFeDANFeRLClass, ACBrPosPrinter, ACBrNFeDANFeESCPOS;

type

{ TACBrObjetoNFe }

TACBrObjetoNFe = class(TACBrObjetoDFe)
private
  fACBrNFe: TACBrNFe;
public
  constructor Create(AConfig: TMonitorConfig; ACBrNFe: TACBrNFe); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  function GerarNFeIni(XML: string): string;
  procedure RespostaNotasFiscais(pImprimir: Boolean; pImpressora: String;
            pPreview: String; pCopias: Integer; pPDF: Boolean);
  procedure RespostaItensNFe(NotasFiscaisID: integer = 0; ItemID: integer = 0; Gerar: boolean = False);
  procedure RespostaEnvio;
  procedure RespostaRetorno;
  procedure RespostaStatus;
  procedure RespostaConsulta;
  procedure RespostaCancelamento;
  procedure RespostaRecibo;
  procedure RespostaItensRecibo(ItemID: integer = 0);
  procedure RespostaInutiliza;
  procedure RespostaConsultaCadastro;
  procedure RespostaItensConsultaCadastro(ItemID: integer = 0);
  procedure RespostaEvento;
  procedure RespostaItensEvento(ItemID: integer = 0);
  procedure RespostaDistribuicaoDFe;
  procedure RespostaItensDistribuicaoDFeResNFe(ItemID: integer = 0; TagID: integer = 1);
  procedure RespostaItensDistribuicaoDFeResEve(ItemID: integer = 0; TagID: integer = 1);
  procedure RespostaItensDistribuicaoDFeProEve(ItemID: integer = 0; TagID: integer = 1);
  procedure RespostaItensDistribuicaoDFeInfEve(ItemID: integer = 0; TagID: integer = 1);
  Procedure LerIniNFe(ArqINI: String);
  procedure ImprimirNFe(pImpressora: String; pPreview: String; pCopias: Integer; pPDF: Boolean);
  procedure RespostaIntegrador;
  procedure RespostaConsultaInfCan;
  procedure RespostaConsultaProcEvento(ItemId: Integer);
  procedure RespostaConsultaDetEvento(ItemId: Integer);
  procedure RespostaConsultaItemPedido(ItemId, ItemRet: Integer);
  procedure RespostaConsultaRetEvento(ItemId, ItemRet: Integer);
  procedure RespostaConsultaChNFePend(ItemId, ItemRet, ItemChave: Integer);

  property ACBrNFe: TACBrNFe read fACBrNFe;
end;

{ TACBrCarregarNFe }

TACBrCarregarNFe = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;
public
  constructor Create(AACBrDFe: TACBrNFe; AXMLorFile: String; ARetornaFalha: Boolean = True ); reintroduce;
end;

{ TACBrCarregarNFeEvento }

TACBrCarregarNFeEvento = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;
public
  constructor Create(AACBrDFe: TACBrNFe; AXMLorFile: String ); reintroduce;
end;

{ TACBrCarregarNFeInut }

TACBrCarregarNFeInut = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;
public
  constructor Create(AACBrDFe: TACBrNFe; AXMLorFile: String ); reintroduce;
end;

{ TMetodoCancelarNFe }

TMetodoCancelarNFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirDaNFe }

TMetodoImprimirDaNFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarNFe }

TMetodoCriarNFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAdicionarNFe }

TMetodoAdicionarNFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarNFe }
TMetodoEnviarNFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarLoteNFe }
TMetodoEnviarLoteNFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarEnviarNFe }
TMetodoCriarEnviarNFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirDaNFePDF }
TMetodoImprimirDaNFePDF = class(TACBrMetodo)
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

{ TMetodoValidarNFe}
TMetodoValidarNFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAssinarNFe}
TMetodoAssinarNFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarNFe}
TMetodoConsultarNFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoReciboNFe}
TMetodoReciboNFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarEmail}
TMetodoEnviarEmail = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoInutilizarNFe}
TMetodoInutilizarNFe = class(TACBrMetodo)
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

{ TMetodoSetVersaoDF}
TMetodoSetVersaoDF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLerNFe}
TMetodoLerNFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNFeToTxt}
TMetodoNFeToTxt = class(TACBrMetodo)
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
  FnNFe: integer;
  FTpEmissao: integer;
  FEmissao: TDateTime;
  FCNPJ: string;
public
  procedure Executar; override;
end;

{ TMetodoSetModeloDF}
TMetodoSetModeloDF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetToken}
TMetodoSetToken = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetIdToken}
TMetodoSetIdToken = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCNPJCertificado}
TMetodoCNPJCertificado = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirInutilizacao }
TMetodoImprimirInutilizacao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirInutilizacaoPDF }
TMetodoImprimirInutilizacaoPDF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGetPathNFe}
TMetodoGetPathNFe = class(TACBrMetodo)
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

{ TMetodoGetPathInu}
TMetodoGetPathInu = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarEvento}
TMetodoEnviarEvento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCartaCorrecao}
TMetodoCartaCorrecao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoXMLEnviarEvento}
TMetodoXMLEnviarEvento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDistribuicaoDFePorChaveNFe}
TMetodoDistribuicaoDFePorChaveNFe = class(TACBrMetodo)
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

{ TMetodoEnviaremailInutilizacao}
TMetodoEnviaremailInutilizacao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoValidarRegrasNegocios}
TMetodoValidarRegrasNegocios = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirRelatorio}
TMetodoImprimirRelatorio = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarNFeSEFAZ}
TMetodoCriarNFeSEFAZ = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarEnviarNFeSEFAZ}
TMetodoCriarEnviarNFeSEFAZ = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAdicionarNFeSEFAZ}
TMetodoAdicionarNFeSEFAZ = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDistribuicaoDFe}
TMetodoDistribuicaoDFe = class(TACBrMetodo)
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
  pcnConversao, pcnConversaoNFe,
  pcnAuxiliar, pcnNFeR, pcnNFeRTXT, DoACBrUnit, pcnNFe;

{ TACBrObjetoNFe }

constructor TACBrObjetoNFe.Create(AConfig: TMonitorConfig; ACBrNFe: TACBrNFe);
begin
  inherited Create(AConfig);

  fACBrNFe := ACBrNFe;

  ListaDeMetodos.Add(CMetodostatusservico);
  ListaDeMetodos.Add(CMetodoValidarNFe);
  ListaDeMetodos.Add(CMetodoAssinarNFe);
  ListaDeMetodos.Add(CMetodoConsultarNFe);
  ListaDeMetodos.Add(CMetodoCancelarNFe);
  ListaDeMetodos.Add(CMetodoImprimirdaNFe);
  ListaDeMetodos.Add(CMetodoImprimirdaNFepdf);
  ListaDeMetodos.Add(CMetodoImprimirevento);
  ListaDeMetodos.Add(CMetodoImprimireventopdf);
  ListaDeMetodos.Add(CMetodoEnviarNFe);
  ListaDeMetodos.Add(CMetodoCriarNFe);
  ListaDeMetodos.Add(CMetodoCriarenviarNFe);
  ListaDeMetodos.Add(CMetodoAdicionarNFe);
  ListaDeMetodos.Add(CMetodoEnviarloteNFe);
  ListaDeMetodos.Add(CMetodoReciboNFe);
  ListaDeMetodos.Add(CMetodoInutilizarNFe);
  ListaDeMetodos.Add(CMetodoConsultacadastro);
  ListaDeMetodos.Add(CMetodoEnviaremail);
  ListaDeMetodos.Add(CMetodoSetambiente);
  ListaDeMetodos.Add(CMetodoSetlogomarca);
  ListaDeMetodos.Add(CMetodoSetformaemissao);
  ListaDeMetodos.Add(CMetodoSetversaodf);
  ListaDeMetodos.Add(CMetodoLerNFe);
  ListaDeMetodos.Add(CMetodoNFetotxt);
  ListaDeMetodos.Add(CMetodoFileexist);
  ListaDeMetodos.Add(CMetodoCertificadodatavencimento);
  ListaDeMetodos.Add(CMetodoGerarchave);
  ListaDeMetodos.Add(CMetodoVersao);
  ListaDeMetodos.Add(CMetodoSetModeloDF);
  ListaDeMetodos.Add(CMetodoSetToken);
  ListaDeMetodos.Add(CMetodoSetCSC);
  ListaDeMetodos.Add(CMetodoSetIdToken);
  ListaDeMetodos.Add(CMetodoSetIdCSC);
  ListaDeMetodos.Add(CMetodoCNPJCertificado);
  ListaDeMetodos.Add(CMetodoImprimirInutilizacao);
  ListaDeMetodos.Add(CMetodoImprimirInutilizacaoPDF);
  ListaDeMetodos.Add(CMetodoGetPathNFe);
  ListaDeMetodos.Add(CMetodoGetPathCCe);
  ListaDeMetodos.Add(CMetodoGetPathCan);
  ListaDeMetodos.Add(CMetodoGetPathEvento);
  ListaDeMetodos.Add(CMetodoGetPathInu);
  ListaDeMetodos.Add(CMetodoGerarININFe);
  ListaDeMetodos.Add(CMetodoEnviarEvento);
  ListaDeMetodos.Add(CMetodoCartaCorrecao);
  ListaDeMetodos.Add(CMetodoXMLEnviarEvento);
  ListaDeMetodos.Add(CMetodoDistribuicaoDFeporChaveNFe);
  ListaDeMetodos.Add(CMetodoDistribuicaoDFeporNSU);
  ListaDeMetodos.Add(CMetodoDistribuicaoDFeporUltNSU);
  ListaDeMetodos.Add(CMetodoEnviaremailEvento);
  ListaDeMetodos.Add(CMetodoEnviaremailInutilizacao);
  ListaDeMetodos.Add(CMetodoValidarRegrasNegocios);
  ListaDeMetodos.Add(CMetodoImprimirRelatorio);
  ListaDeMetodos.Add(CMetodoCriarNFeSEFAZ);
  ListaDeMetodos.Add(CMetodoCriarEnviarNFeSEFAZ);
  ListaDeMetodos.Add(CMetodoAdicionarNFeSEFAZ);
  ListaDeMetodos.Add(CMetodoDistribuicaoDFe);
  ListaDeMetodos.Add(CMetodoDataVencimentoCertificado);
  ListaDeMetodos.Add(CMetodoSetTipoImpressao);

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

procedure TACBrObjetoNFe.Executar(ACmd: TACBrCmd);
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
    1  : AMetodoClass := TMetodoValidarNFe;
    2  : AMetodoClass := TMetodoAssinarNFe;
    3  : AMetodoClass := TMetodoConsultarNFe;
    4  : AMetodoClass := TMetodoCancelarNFe;
    5  : AMetodoClass := TMetodoImprimirDaNFe;
    6  : AMetodoClass := TMetodoImprimirDaNFePDF;
    7  : AMetodoClass := TMetodoImprimirEvento;
    8  : AMetodoClass := TMetodoImprimirEventoPDF;
    9  : AMetodoClass := TMetodoEnviarNFe;
    10 : AMetodoClass := TMetodoCriarNFe;
    11 : AMetodoClass := TMetodoCriarEnviarNFe;
    12 : AMetodoClass := TMetodoAdicionarNFe;
    13 : AMetodoClass := TMetodoEnviarLoteNFe;
    14 : AMetodoClass := TMetodoReciboNFe;
    15 : AMetodoClass := TMetodoInutilizarNFe;
    16 : AMetodoClass := TMetodoConsultaCadastro;
    17 : AMetodoClass := TMetodoEnviarEmail;
    18 : AMetodoClass := TMetodoSetAmbiente;
    19 : AMetodoClass := TMetodoSetLogoMarca;
    20 : AMetodoClass := TMetodoSetformaEmissao;
    21 : AMetodoClass := TMetodoSetVersaoDF;
    22 : AMetodoClass := TMetodoLerNFe;
    23 : AMetodoClass := TMetodoNFeToTxt;
    24 : AMetodoClass := TMetodoFileExists;
    25 : AMetodoClass := TMetodoCertificadoDataVencimento;
    26 : AMetodoClass := TMetodoGeraChave;
    27 : AMetodoClass := TMetodoVersao;
    28 : AMetodoClass := TMetodoSetModeloDF;
    29, 30 : AMetodoClass := TMetodoSetToken;  // SetToken e SetCSC
    31, 32 : AMetodoClass := TMetodoSetIdToken; // SetIdToken e SetIdCSC
    33 : AMetodoClass := TMetodoCNPJCertificado;
    34 : AMetodoClass := TMetodoImprimirInutilizacao;
    35 : AMetodoClass := TMetodoImprimirInutilizacaoPDF;
    36 : AMetodoClass := TMetodoGetPathNFe;
    37 : AMetodoClass := TMetodoGetPathCCe;
    38 : AMetodoClass := TMetodoGetPathCan;
    39 : AMetodoClass := TMetodoGetPathEvento;
    40 : AMetodoClass := TMetodoGetPathInu;
    41 : AMetodoClass := TMetodoLerNFe; // GerarIniNFe
    42 : AMetodoClass := TMetodoEnviarEvento;
    43 : AMetodoClass := TMetodoCartaCorrecao;
    44 : AMetodoClass := TMetodoXMLEnviarEvento;
    45 : AMetodoClass := TMetodoDistribuicaoDFeporChaveNFe;
    46 : AMetodoClass := TMetodoDistribuicaoDFeporUltNSU;
    47 : AMetodoClass := TMetodoDistribuicaoDFeporNSU;
    48 : AMetodoClass := TMetodoEnviaremailEvento;
    49 : AMetodoClass := TMetodoEnviaremailInutilizacao;
    50 : AMetodoClass := TMetodoValidarRegrasNegocios;
    51 : AMetodoClass := TMetodoImprimirRelatorio;
    52 : AMetodoClass := TMetodoCriarNFeSEFAZ;
    53 : AMetodoClass := TMetodoCriarEnviarNFeSEFAZ;
    54 : AMetodoClass := TMetodoAdicionarNFeSEFAZ;
    55 : AMetodoClass := TMetodoDistribuicaoDFe;
    56 : AMetodoClass := TMetodoCertificadoDataVencimento; // DataVencimentoCertificado
    57 : AMetodoClass := TMetodoSetTipoImpressao;

    58..72 : DoACbr(ACmd);
  end;

  if Assigned(AMetodoClass) then
  begin
    Ametodo := AMetodoClass.Create(ACmd, Self);
    try
      Ametodo.Executar;
    finally
      RespostaIntegrador;
      Ametodo.Free;
    end;
  end;
end;

procedure TACBrObjetoNFe.RespostaEnvio;
var
  Resp: TEnvioResposta;
begin
  Resp := TEnvioResposta.Create(resINI);
  try
    with fACBrNFe.WebServices.Enviar do
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

      fpCmd.Resposta := fpCmd.Resposta + Msg + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaRetorno;
var
  Resp: TRetornoResposta;
begin
  Resp := TRetornoResposta.Create(resINI);
  try
    with fACBrNFe.WebServices.Retorno do
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

procedure TACBrObjetoNFe.RespostaNotasFiscais(pImprimir: Boolean; pImpressora: String;
          pPreview: String; pCopias: Integer; pPDF: Boolean);
var
  I, J: integer;
  ArqPDF: String;
begin
  with fACBrNFe do
  begin
    for I := 0 to WebServices.Retorno.NFeRetorno.ProtNFe.Count - 1 do
    begin
      for J := 0 to NotasFiscais.Count - 1 do
      begin
        if ('NFe' + WebServices.Retorno.NFeRetorno.ProtNFe.Items[I].chNFe =
          NotasFiscais.Items[J].NFe.infNFe.Id) then
        begin
          RespostaItensNFe(J, I, True);

          DoConfiguraDANFe(False, Trim(pPreview) );

          if NaoEstaVazio(pImpressora) then
            DANFe.Impressora := pImpressora;

          if pCopias > 0 then
            DANFE.NumCopias := pCopias;

          if pPDF then
          begin
            NotasFiscais.Items[I].ImprimirPDF;
            ArqPDF := OnlyNumber(ACBrNFe.NotasFiscais.Items[I].NFe.infNFe.ID)+'-nfe.pdf';

            fpCmd.Resposta :=  fpCmd.Resposta + sLineBreak +
              'PDF='+ PathWithDelim(ACBrNFe.DANFE.PathPDF) + ArqPDF ;
          end;

          if (NotasFiscais.Items[I].Confirmada) and (pImprimir) then
          begin
            try
              DoAntesDeImprimir(( StrToBoolDef( pPreview, False ) ) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
              NotasFiscais.Items[I].Imprimir;
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

procedure TACBrObjetoNFe.RespostaItensNFe(NotasFiscaisID: integer;
  ItemID: integer; Gerar: boolean);
var
  Resp: TRetornoItemResposta;
begin
  Resp := TRetornoItemResposta.Create(
    'NFe' + Trim(IntToStr(
    fACBrNFe.NotasFiscais.Items[NotasFiscaisID].NFe.Ide.nNF)), resINI);
  try
    with fACBrNFe.WebServices.Retorno.NFeRetorno.ProtNFe.Items[ItemID] do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := fACBrNFe.WebServices.Retorno.NFeRetorno.cUF;
      Resp.ChNFe := chNFe;
      Resp.DhRecbto := dhRecbto;
      Resp.NProt := nProt;
      Resp.DigVal := digVal;

      if Gerar then
        Resp.Arquivo := fACBrNFe.NotasFiscais.Items[NotasFiscaisID].NomeArq+sLineBreak;
          {PathWithDelim(fACBrNFe.Configuracoes.Arquivos.PathSalvar) +
          OnlyNumber(fACBrNFe.NotasFiscais.Items[NotasFiscaisID].NFe.infNFe.ID) +
          '-nfe.xml';}

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaStatus;
var
  Resp: TStatusServicoResposta;
begin
  Resp := TStatusServicoResposta.Create(resINI);
  try
    with fACBrNFe.WebServices.StatusServico do
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

procedure TACBrObjetoNFe.RespostaConsulta;
var
  Resp: TConsultaNFeResposta;
begin
  Resp := TConsultaNFeResposta.Create(resINI);
  try
    with fACBrNFe.WebServices.Consulta do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cUF;
      Resp.ChNFe := NFeChave;
      Resp.DhRecbto := dhRecbto;
      Resp.NProt := Protocolo;
      Resp.digVal := protNFe.digVal;
      Resp.Msg := Msg;

      fpCmd.Resposta := Msg + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaCancelamento;
var
  Resp: TCancelamentoResposta;
begin
  Resp := TCancelamentoResposta.Create(resINI);
  try
    with fACBrNFe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cOrgao;
      Resp.ChNFe := chNFe;
      Resp.DhRecbto := dhRegEvento;
      Resp.NProt := nProt;
      Resp.TpEvento := TpEventoToStr(tpEvento);
      Resp.xEvento := xEvento;
      Resp.nSeqEvento := nSeqEvento;
      Resp.CNPJDest := CNPJDest;
      Resp.emailDest := emailDest;
      Resp.XML := XML;
      Resp.Arquivo := NomeArquivo;

      fpCmd.Resposta := XMotivo + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaRecibo;
var
  Resp: TRetornoResposta;
begin
  Resp := TRetornoResposta.Create(resINI);
  try
    with fACBrNFe.WebServices.Recibo do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.nRec := Recibo;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cUF;

      fpCmd.Resposta := Msg + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaItensRecibo(ItemID: integer);
var
  Resp: TRetornoItemResposta;
begin
  Resp := TRetornoItemResposta.Create(
    'NFe' + Trim(IntToStr(StrToInt(copy(
    fACBrNFe.WebServices.Recibo.NFeRetorno.ProtNFe.Items
    [ItemID].chNFe, 26, 9)))), resINI);
  try
    with fACBrNFe.WebServices.Recibo.NFeRetorno.ProtNFe.Items[ItemID] do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := fACBrNFe.WebServices.Recibo.NFeRetorno.cUF;
      Resp.ChNFe := chNFe;
      Resp.DhRecbto := dhRecbto;
      Resp.NProt := nProt;
      Resp.digVal := digVal;

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaInutiliza;
var
  Resp: TInutilizarNFeResposta;
begin
  Resp := TInutilizarNFeResposta.Create(resINI);
  try
    with fACBrNFe.WebServices.Inutilizacao do
    begin
      Resp.Versao := verAplic;
      Resp.TpAmb := TpAmbToStr(TpAmb);
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cUF;
      Resp.XML := XML_ProcInutNFe;
      Resp.NProt := Protocolo;
      Resp.DhRecbto := dhRecbto;
      Resp.NomeArquivo := NomeArquivo;

      fpCmd.Resposta := Msg + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaConsultaCadastro;
var
  Resp: TConsultaCadastroResposta;
begin
  Resp := TConsultaCadastroResposta.Create(resINI);
  try
    with fACBrNFe.WebServices.ConsultaCadastro do
    begin
      Resp.Versao := verAplic;
      Resp.VerAplic := VerAplic;
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.CUF := cUF;
      Resp.dhCons := dhCons;
      Resp.IE  := RetConsCad.IE;
      Resp.CNPJ := RetConsCad.CNPJ;
      Resp.CPF := RetConsCad.CPF;
      Resp.CPF := RetConsCad.CPF;
      Resp.UF := REtConsCad.UF;

      fpCmd.Resposta := Msg + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaItensConsultaCadastro(ItemID: integer);
var
  Resp: TConsultaCadastroItemResposta;
begin
  Resp := TConsultaCadastroItemResposta.Create(
    'INFCAD' + Trim(IntToStrZero(ItemID +1, 3)), resINI);
  try
    with fACBrNFe.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[ItemID] do
    begin
      Resp.IE := IE;
      Resp.CNPJ := CNPJ;
      Resp.CPF := CPF;
      Resp.UF := UF;
      Resp.cSit := cSit;
      Resp.xNome := xNome;
      Resp.xFant := xFant;
      Resp.xRegApur := xRegApur;
      Resp.CNAE := CNAE;
      Resp.dIniAtiv := dIniAtiv;
      Resp.dUltSit := dUltSit;
      Resp.dBaixa := dBaixa;
      Resp.IEUnica := IEUnica;
      Resp.IEAtual := IEAtual;
      Resp.xLgr := xLgr;
      Resp.nro := nro;
      Resp.xCpl := xCpl;
      Resp.xBairro := xBairro;
      Resp.cMun := cMun;
      Resp.xMun := xMun;
      Resp.CEP := CEP;

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaEvento;
var
  Resp: TEventoResposta;
begin
  Resp := TEventoResposta.Create(resINI);
  try
    with fACBrNFe.WebServices.EnvEvento.EventoRetorno do
    begin
      Resp.VerAplic := VerAplic;
      Resp.tpAmb := TpAmbToStr(tpAmb);
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.idLote := IdLote;
      Resp.cOrgao := cOrgao;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaItensEvento(ItemID: integer);
var
  Resp: TEventoItemResposta;
begin
  Resp := TEventoItemResposta.Create(
    'Evento' + Trim(IntToStrZero(ItemID +1, 3)), resINI);
  try
    with fACBrNFe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[ItemID].RetInfEvento do
    begin
      Resp.Id := Id;
      Resp.tpAmb := TpAmbToStr(tpAmb);
      Resp.verAplic := verAplic;
      Resp.cOrgao := cOrgao;
      Resp.cStat := cStat;
      Resp.xMotivo := xMotivo;
      Resp.chNFe := chNFe;
      Resp.tpEvento := TpEventoToStr(tpEvento);
      Resp.xEvento := xEvento;
      Resp.nSeqEvento := nSeqEvento;
      Resp.CNPJDest := CNPJDest;
      Resp.emailDest := emailDest;
      Resp.dhRegEvento := dhRegEvento;
      Resp.nProt := nProt;
      Resp.Arquivo := NomeArquivo;
      Resp.XML := XML;

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaDistribuicaoDFe;
var
  Resp: TDistribuicaoDFeResposta;
  sTemMais: String;
begin
  Resp := TDistribuicaoDFeResposta.Create(resINI);
  try
    with fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt do
    begin
      Resp.Versao := versao;
      Resp.VerAplic := VerAplic;
      Resp.tpAmb := TpAmbToStr(tpAmb);
      Resp.CStat := cStat;
      Resp.XMotivo := XMotivo;
      Resp.dhResp := dhResp;
      Resp.ultNSU := ultNSU;
      Resp.maxNSU := maxNSU;
      Resp.arquivo := fACBrNFe.WebServices.DistribuicaoDFe.NomeArq;

      if cStat = 137 then
        sTemMais := '1'  // Sim
      else
        sTemMais := '0'; // Não

      Resp.indCont := sTemMais;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaItensDistribuicaoDFeResNFe(ItemID: integer; TagID: integer);
var
  Resp: TDistribuicaoDFeItemResposta;
begin
  Resp := TDistribuicaoDFeItemResposta.Create(
    'ResNFe' + Trim(IntToStrZero(TagID, 3)), resINI);
  try
    with fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].resNFe do
    begin
      Resp.NSU := fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.chNFe := chNFe;
      Resp.CNPJCPF := CNPJCPF;
      Resp.xNome := xNome;
      Resp.IE := IE;
      Resp.dhEmi := dhEmi;
      Resp.tpNF := tpNFToStr(tpNF);
      Resp.vNF := vNF;
      Resp.digVal := digVal;
      Resp.dhRecbto := dhRecbto;
      Resp.cSitNFe := SituacaoDFeToStr(cSitNFe);
      Resp.nProt := nProt;
      Resp.XML := fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := fACBrNFe.WebServices.DistribuicaoDFe.listaArqs[ItemID];
      Resp.schema := SchemaNFeToStr(fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[ItemID].schema);

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaItensDistribuicaoDFeResEve(ItemID: integer; TagID: integer);
var
  Resp: TDistribuicaoDFeItemResposta;
begin
  Resp := TDistribuicaoDFeItemResposta.Create(
    'ResEve' + Trim(IntToStrZero(TagID, 3)), resINI);
  try
    with fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].resEvento do
    begin
      Resp.NSU := fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.chNFe := chNFe;
      Resp.CNPJCPF := CNPJCPF;
      Resp.dhEvento := dhEvento;
      Resp.tpEvento := TpEventoToStr(tpEvento);
      Resp.xEvento := xEvento;
      Resp.nSeqEvento := nSeqEvento;
      Resp.cOrgao := cOrgao;
      Resp.dhRecbto := dhRecbto;
      Resp.nProt := nProt;
      Resp.XML := fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := fACBrNFe.WebServices.DistribuicaoDFe.listaArqs[ItemID];
      Resp.schema := SchemaNFeToStr(fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[ItemID].schema);

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaItensDistribuicaoDFeProEve(ItemID: integer; TagID: integer);
var
  Resp: TDistribuicaoDFeItemResposta;
begin
  Resp := TDistribuicaoDFeItemResposta.Create(
    'ProEve' + Trim(IntToStrZero(TagID, 3)), resINI);
  try
    with fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].procEvento do
    begin
      Resp.NSU := fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].NSU;
      Resp.chNFe := chNFe;
      Resp.cOrgao := cOrgao;
      Resp.CNPJ := CNPJ;
      Resp.Id := Id;
      Resp.dhEvento := dhEvento;
      Resp.nSeqEvento := nSeqEvento;
      Resp.tpAmb := TpAmbToStr(tpAmb);
      Resp.tpEvento := TpEventoToStr(tpEvento);
      Resp.verEvento := verEvento;

      with detEvento do
      begin
        Resp.descEvento := descEvento;
        Resp.xJust := xJust;
        Resp.xMotivo := xCorrecao;
        Resp.EmiCnpj := emit.CNPJ;
        Resp.EmiIE := emit.IE;
        Resp.EmixNome := emit.xNome;
        Resp.cteNProt := CTe.nProt;
        Resp.cteChvCte := CTe.chCTe;
        Resp.cteDhemi := CTe.dhEmi;
        Resp.cteModal := TpModalToStr(CTe.modal);
        Resp.cteDhRebcto := CTe.dhRecbto;
      end;

      Resp.XML := fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := fACBrNFe.WebServices.DistribuicaoDFe.listaArqs[ItemID];
      Resp.schema := SchemaNFeToStr(fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[ItemID].schema);

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaItensDistribuicaoDFeInfEve(ItemID: integer; TagID: integer);
var
  Resp: TDistribuicaoDFeItemResposta;
begin
  Resp := TDistribuicaoDFeItemResposta.Create(
    'InfEve' + Trim(IntToStrZero(TagID, 3)), resINI);
  try
    with fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].procEvento.RetinfEvento do
    begin
      Resp.Id := Id;
      Resp.VerAplic := VerAplic;
      Resp.tpAmb := TpAmbToStr(tpAmb);
      Resp.cOrgao := cOrgao;
      Resp.chNFe := chNFe;
      Resp.CStat := cStat;
      Resp.CNPJDest := CNPJDest;
      Resp.cOrgaoAutor := cOrgaoAutor;
      Resp.tpEvento := TpEventoToStr(tpEvento);
      Resp.nSeqEvento := nSeqEvento;
      Resp.xEvento := xEvento;
      Resp.XMotivo := XMotivo;
      Resp.dhRegEvento := dhRegEvento;
      Resp.emailDest := emailDest;
      Resp.nProt := nProt;

      Resp.XML := fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Items[ItemID].XML;
      Resp.Arquivo := fACBrNFe.WebServices.DistribuicaoDFe.listaArqs[ItemID];
      Resp.schema := SchemaNFeToStr(fACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[ItemID].schema);

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.LerIniNFe(ArqINI: String);
begin
  with fACBrNFe do
  begin
    NotasFiscais.Clear;
    NotasFiscais.LoadFromIni(ArqINI);

    //Campos preenchidos em tela
    if (NotasFiscais.Count > 0) and
       ( NaoEstaVazio(MonitorConfig.DFE.WebService.NFe.CNPJContador) ) then
      with NotasFiscais.Items[0].NFe.autXML.Add do
        CNPJCPF := MonitorConfig.DFE.WebService.NFe.CNPJContador;

  end;
end;

procedure TACBrObjetoNFe.ImprimirNFe(pImpressora: String; pPreview: String;
  pCopias: Integer; pPDF: Boolean);
var
  ArqPDF : String;
begin
  with fACBrNFe do
  begin
    if (NotasFiscais.Items[0].Confirmada) then
    begin
      DoConfiguraDANFe(pPDF, Trim(pPreview) );
      if NaoEstaVazio(pImpressora) then
        DANFe.Impressora := pImpressora;

      if pCopias > 0 then
        DANFE.NumCopias := pCopias;

      if pPDF then
      begin
        NotasFiscais.Items[0].ImprimirPDF;
        ArqPDF := OnlyNumber(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID)+'-nfe.pdf';

        fpCmd.Resposta :=  fpCmd.Resposta + sLineBreak +
                'PDF='+ PathWithDelim(ACBrNFe.DANFE.PathPDF) + ArqPDF ;
      end;

      try
        DoAntesDeImprimir(( StrToBoolDef( pPreview, False) ) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
        NotasFiscais.Items[0].Imprimir;
      finally
        DoDepoisDeImprimir;
      end;

    end;

  end;

end;

function TACBrObjetoNFe.GerarNFeIni(XML: string): string;
var
  INIRec: TMemIniFile;
  IniNFe: TStringList;
  LocNFeR: TNFeR;
begin
  INIRec := TMemIniFile.Create('NFe.ini');

  fACBrNFe.NotasFiscais.Clear;
  if FilesExists(XML) then
    fACBrNFe.NotasFiscais.LoadFromFile(XML)
  else
  begin
    LocNFeR := TNFeR.Create(fACBrNFe.NotasFiscais.Add.NFe);
    try
      LocNFeR.Leitor.Arquivo := ConvertStrRecived(XML);
      LocNFeR.LerXml;
      fACBrNFe.NotasFiscais.Items[0].XML := LocNFeR.Leitor.Arquivo;
      fACBrNFe.NotasFiscais.GerarNFe;
    finally
      LocNFeR.Free;
    end;
  end;

  IniNFe := TStringList.Create;
  IniNFe.Text := fACBrNFe.NotasFiscais.GerarIni();
  INIRec.SetStrings(IniNFe);
  INIRec.Free;
  Result := IniNFe.Text;
  IniNFe.Free;
end;

procedure TACBrObjetoNFe.RespostaIntegrador;
begin
  with fACBrNFe do
    fpCmd.Resposta := fpCmd.Resposta + DoRespostaIntegrador();

end;

procedure TACBrObjetoNFe.RespostaConsultaInfCan;
var
  Resp: TConsultaNFeInfCanResposta;
begin
  Resp := TConsultaNFeInfCanResposta.Create(resINI);
  try
    with fACBrNFe.WebServices.Consulta.retCancNFe do
    begin
      Resp.tpAmb:=     TpAmbToStr(tpAmb);
      Resp.VerAplic:=  verAplic;
      Resp.CStat:=     cStat;
      Resp.XMotivo:=   xMotivo;
      Resp.CUF:=       cUF;
      Resp.ChNFe:=     chNFE;
      Resp.DhRecbto:=  dhRecbto;
      Resp.NProt:=     nProt;

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaConsultaProcEvento(ItemId: Integer);
var
  Resp: TConsultaNFeProcEventoResposta;
begin
  Resp := TConsultaNFeProcEventoResposta.Create(
    'ProcEventoNFe' + Trim(IntToStrZero(ItemID +1, 3)), resINI);
  try
    with fACBrNFe.WebServices.Consulta.procEventoNFe.Items[ItemID].RetEventoNFe.InfEvento do
    begin
      Resp.ID := fACBrNFe.WebServices.Consulta.procEventoNFe.Items[ItemID].ID;
      Resp.cOrgao := IntToStr(cOrgao);
      Resp.tpAmb := TpAmbToStr(tpAmb);
      Resp.CNPJ := CNPJ;
      Resp.chNFe := chNFe;
      Resp.dhEvento := dhEvento;
      Resp.tpEvento := TpEventoToStr(tpEvento);
      Resp.nSeqEvento := nSeqEvento;
      Resp.verEvento := versaoEvento;

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaConsultaDetEvento(ItemId: Integer);
var
  Resp: TConsultaNFeDetEventoResposta;
begin
  Resp := TConsultaNFeDetEventoResposta.Create(
    'DetEvento' + Trim(IntToStrZero(ItemID +1, 3)), resINI);
  try
    with fACBrNFe.WebServices.Consulta.procEventoNFe.Items[ItemID].RetEventoNFe.InfEvento.detEvento do
    begin
      Resp.versao := versao;;
      Resp.descEvento:= descEvento;
      Resp.xCorrecao := xCorrecao;
      Resp.xCondUso := xCondUso;
      Resp.nProt := nProt;
      Resp.xJust := xJust;
      Resp.cOrgaoAutor:= IntToStr( cOrgaoAutor );
      Resp.tpAutor := TipoAutorToStr( tpAutor );
      Resp.verAplic := verAplic;
      Resp.dhEmi := dhEmi;
      Resp.tpNF := tpNFToStr( tpNF );
      Resp.IE := IE;
      Resp.DESTCNPJCPF := dest.CNPJCPF;
      Resp.DESTidEstrangeiro := dest.idEstrangeiro;
      Resp.DESTIE := dest.IE;
      Resp.DESTUF := dest.UF;
      Resp.vNF := vNF;
      Resp.vICMS := vICMS;
      Resp.vST := vST;
      Resp.idPedidoCancelado :=  idPedidoCancelado;

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaConsultaItemPedido(ItemId, ItemRet: Integer);
var
  Resp: TConsultaNFeItemPedidoResposta;
begin
  Resp := TConsultaNFeItemPedidoResposta.Create(
    'ItemPedido' + Trim(IntToStrZero(ItemID +1, 3)) + Trim(IntToStrZero(ItemRet +1, 3)), resINI);
  try
    with fACBrNFe.WebServices.Consulta.procEventoNFe.Items[ItemID].RetEventoNFe.InfEvento.detEvento.itemPedido.Items[ItemRet] do
    begin
      Resp.numItem := numItem;
      Resp.qtdeItem := qtdeItem;

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaConsultaRetEvento(ItemId, ItemRet: Integer);
var
  Resp: TConsultaNFeRetEventoResposta;
begin
  Resp := TConsultaNFeRetEventoResposta.Create(
    'RetEvento' + Trim(IntToStrZero(ItemID +1, 3)) + Trim(IntToStrZero(ItemRet +1, 3)), resINI);
  try
    with fACBrNFe.WebServices.Consulta.procEventoNFe.Items[ItemId].RetEventoNFe.retEvento.Items[ItemRet].RetInfEvento do
    begin
      Resp.Id := IntToStr(fACBrNFe.WebServices.Consulta.procEventoNFe.Items[ItemID].RetEventoNFe.retEvento.Items[ItemRet].ID);
      Resp.NomeArquivo := NomeArquivo;
      Resp.tpAmb := TpAmbToStr(tpAmb);
      Resp.verAplic := verAplic;
      Resp.cOrgao := IntToStr(cOrgao);
      Resp.cStat := cStat;
      Resp.xMotivo:= xMotivo;
      Resp.chNFe := chNFe;
      Resp.tpEvento := TpEventoToStr(tpEvento);
      Resp.xEvento := xEvento;
      Resp.nSeqEvento := nSeqEvento;
      Resp.CNPJDest := CNPJDest;
      Resp.emailDest := emailDest;
      Resp.cOrgaoAutor := IntToStr(cOrgaoAutor);
      Resp.dhRegEvento := dhRegEvento;
      Resp.nProt := nProt;
      Resp.XML := XML;

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoNFe.RespostaConsultaChNFePend(ItemId, ItemRet, ItemChave: Integer);
var
  Resp: TConsultaNFeChNFePendResposta;
begin
  Resp := TConsultaNFeChNFePendResposta.Create(
    'ChNFePend' + Trim(IntToStrZero(ItemID +1, 3)) + Trim(IntToStrZero(ItemRet +1, 3))
                + Trim(IntToStrZero(ItemChave +1, 3)), resINI);
  try
    with fACBrNFe.WebServices.Consulta.procEventoNFe.Items[ItemId].RetEventoNFe.retEvento.Items[ItemRet]
         .RetInfEvento.chNFePend.Items[ItemChave] do
    begin
      Resp.chNFePend:= ChavePend;

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

{ TACBrCarregarNFe }

procedure TACBrCarregarNFe.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrNFe(FpACBrDFe).NotasFiscais.LoadFromFile( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroNFeAbrir, [AValue]) ) );
end;

procedure TACBrCarregarNFe.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrNFe(FpACBrDFe).NotasFiscais.LoadFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroNFeCarregar) );
end;

function TACBrCarregarNFe.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrNFe(FpACBrDFe).NotasFiscais.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrNFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrNFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlNFe ;
end;

constructor TACBrCarregarNFe.Create(AACBrDFe: TACBrNFe; AXMLorFile: String; ARetornaFalha: Boolean);
begin
  inherited Create(AACBrDFe, AXMLorFile, ARetornaFalha);
end;

{ TACBrCarregarNFeEvento }

procedure TACBrCarregarNFeEvento.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrNFe(FpACBrDFe).EventoNFe.LerXML( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroNFeAbrir, [AValue]) ) );
end;

procedure TACBrCarregarNFeEvento.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrNFe(FpACBrDFe).EventoNFe.LerXMLFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroNFeCarregar) );
end;

function TACBrCarregarNFeEvento.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrNFe(FpACBrDFe).EventoNFe.Evento.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrNFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrNFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlNFeEve ;
end;

constructor TACBrCarregarNFeEvento.Create(AACBrDFe: TACBrNFe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

{ TACBrCarregarNFeInut }

procedure TACBrCarregarNFeInut.CarregarDFePath(const AValue: String);
begin
  if not ( TACBrNFe(FpACBrDFe).InutNFe.LerXML( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroNFeAbrir, [AValue]) ) );
end;

procedure TACBrCarregarNFeInut.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrNFe(FpACBrDFe).InutNFe.LerXMLFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroNFeCarregar) );
end;

function TACBrCarregarNFeInut.ValidarDFe(const AValue: String): Boolean;
begin
  Result := False;
  if ( TACBrNFe(FpACBrDFe).InutNFe.ID <> '' ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrNFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrNFe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlNFeInu;
end;

constructor TACBrCarregarNFeInut.Create(AACBrDFe: TACBrNFe; AXMLorFile: String);
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
          4 - Numero NFe
          5 - Data Emissao
          6 - CNPJ
}
procedure TMetodoGeraChave.Executar;
begin
  FCodUF := StrToInt(fpCmd.Params(0));
  FCodNumerico := StrToInt(fpCmd.Params(1));
  FModelo := StrToInt(fpCmd.Params(2));
  FSerie := StrToInt(fpCmd.Params(3));
  FnNFe := StrToInt(fpCmd.Params(4));
  FTpEmissao := StrToInt(fpCmd.Params(5));
  FEmissao := StrToDateTime(fpCmd.Params(6));
  FCNPJ := fpCmd.Params(7);

  fpCmd.Resposta := GerarChaveAcesso(FCodUF,
    FEmissao, FCNPJ, FSerie,
    FnNFe, FTpEmissao,
    FCodNumerico, FModelo);
end;

{ TMetodoCertificadoDataVencimento }

procedure TMetodoCertificadoDataVencimento.Executar;
begin
  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := DateToStr(ACBrNFe.SSL.CertDataVenc);
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

{ TMetodoNFeToTxt }

{ Params: 0 - XML - String com path XML
          1 - Nome - String nome do Txt a ser gerado
}
procedure TMetodoNFeToTxt.Executar;
var
  CargaDFe: TACBrCarregarNFe;
  AXML: string;
  ANomeTxt: string;
begin
  AXML := fpCmd.Params(0);
  ANomeTxt := fpCmd.Params(1);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.NotasFiscais.Clear;
    CargaDFe := TACBrCarregarNFe.Create(ACBrNFe, AXML);
    try
      //ACBrNFe.NotasFiscais.Items[0].GravarTXT(ChangeFileExt(ACBrNFe.NotasFiscais.Items[0].NomeArq,'.txt'), ANomeTxt);
      if EstaVazio(ANomeTxt) then
        ANomeTxt:= ChangeFileExt(ACBrNFe.NotasFiscais.Items[0].NomeArq,'.txt');
      fACBrNFe.NotasFiscais.GravarTXT(ANomeTxt);
    finally
      CargaDFe.Free;
    end;

    fpCmd.Resposta := ANomeTxt;
  end;
end;

{ TMetodoLerNFe }

{ Params: 0 - XML - String com path XML
}
procedure TMetodoLerNFe.Executar;
var
  AXML: String;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    try
      fpCmd.Resposta := GerarNFeIni(AXML);
    except
      on E: Exception do
        raise Exception.Create('Erro ao gerar INI da NFe.' + sLineBreak + E.Message);
    end;
  end;
end;

{ TMetodoSetVersaoDF }

{ Params: 0 - Inteiro com numero da Versao NFe valores: 1.00 - 3.00
}
procedure TMetodoSetVersaoDF.Executar;
var
  OK: boolean;
  VersaoNFe: TpcnVersaoDF;
  AVersao: String;
begin
  AVersao := fpCmd.Params(0);
  VersaoNFe := StrToVersaoDF(OK, AVersao);

  if not OK then
    raise Exception.Create('Versão Inválida.');

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.WebService do
      Versao := VersaoDFToStr(VersaoNFe);

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoSetformaEmissao }

{ Params: 0 - Inteiro com numero da Forma Emissao
1-Normal, 2-Contingencia, 3-SCAN, 4-DPEC, 5-FSDA, 6-SVC-AN, 7-SVC-RS ou 9-OffLine
}
procedure TMetodoSetformaEmissao.Executar;
var
  OK: boolean;
  FormaEmissao: TpcnTipoEmissao;
  NFormaEmissao: Integer;
begin
  NFormaEmissao := StrToIntDef(fpCmd.Params(0), 1);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    OK := False;
    FormaEmissao := StrToTpEmis(OK, IntToStr(NFormaEmissao));

    if not OK then
      raise Exception.Create('Forma de Emissão Inválida: ' + TpEmisToStr(FormaEmissao));

    with MonitorConfig.DFE.WebService do
      FormaEmissaoNFe := StrToInt(TpEmisToStr(FormaEmissao))-1;

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

  with TACBrObjetoNFe(fpObjetoDono) do
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

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.WebService do
      Ambiente := NumAmbiente -1;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoConsultaCadastro }

{ Params: 0 - UF: String Sigla UF consulta
          1 - Documento: String Documento a ser consultado
          2 - IE: Boolean 1: para consulta por UF
}
procedure TMetodoConsultaCadastro.Executar;
var
  AUF, ADocumento: String;
  AIE: Boolean;
  I: Integer;
begin
  AUF := fpCmd.Params(0);
  ADocumento := fpCmd.Params(1);
  AIE := StrToBoolDef(fpCmd.Params(2), False);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.WebServices.ConsultaCadastro.UF   := AUF;
    if AIE then
      ACBrNFe.WebServices.ConsultaCadastro.IE := ADocumento
    else
    begin
      if Length(ADocumento) > 11 then
        ACBrNFe.WebServices.ConsultaCadastro.CNPJ := ADocumento
      else
        ACBrNFe.WebServices.ConsultaCadastro.CPF := ADocumento;
    end;

    DoValidarIntegradorNFCe();
    ACBrNFe.WebServices.ConsultaCadastro.Executar;

    RespostaConsultaCadastro;

    for I := 0 to ACBrNFe.WebServices.ConsultaCadastro.RetConsCad.InfCad.Count - 1 do
       RespostaItensConsultaCadastro(I);

  end;
end;

{ TMetodoInutilizarNFe }

{ Params: 0 - CNPJ: String com CNPJ
          1 - Justificativa: String
          2 - Ano: Integer com Ano Documento
          3 - Modelo: Integer com Modelo Documento
          4 - Serie: String com a série documento
          5 - NumIinial: Integer Nº inicial inutilização
          6 - NumFinal: Integer Nº final inutilização
}
procedure TMetodoInutilizarNFe.Executar;
var
  ACNPJ, AJustificativa: String;
  ASerie, AAno, AModelo, ANumInicial, ANumFinal: Integer;
begin
  ACNPJ := fpCmd.Params(0);
  AJustificativa := fpCmd.Params(1);
  AAno := StrToIntDef(fpCmd.Params(2), 0);
  AModelo := StrToIntDef(fpCmd.Params(3), 0);
  ASerie := StrToIntDef(fpCmd.Params(4), 0);
  ANumInicial := StrToIntDef(fpCmd.Params(5), 0);
  ANumFinal := StrToIntDef(fpCmd.Params(6), 0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    DoValidarIntegradorNFCe( IntToStrZero(0,20) + IntToStr(AModelo));
    ACBrNFe.WebServices.Inutiliza(ACNPJ, AJustificativa, AAno, AModelo, ASerie, ANumInicial, ANumFinal);

    RespostaInutiliza;
  end;
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
  CargaDFe: TACBrCarregarNFe;
  AEnviaPDF: Boolean;
begin
  ADestinatario := fpCmd.Params(0);
  APathXML := fpCmd.Params(1);
  AEnviaPDF := StrToBoolDef(fpCmd.Params(2), False);
  AAssunto := fpCmd.Params(3);
  AEmailCopias := fpCmd.Params(4);
  AAnexos := fpCmd.Params(5);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.NotasFiscais.Clear;

    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    try
      CargaDFe := TACBrCarregarNFe.Create(ACBrNFe, APathXML);
      try
        DoConfiguraDANFe(True, '');

        with MonitorConfig.DFE.Email do
        begin
          slMensagemEmail.Text := DoSubstituirVariaveis( StringToBinaryString(MensagemNFe) );

          sAssunto := AssuntoNFe;
        end;

        QuebrarLinha(AEmailCopias, slCC);

        QuebrarLinha(AAnexos, slAnexos);

        try
          ACBrNFe.NotasFiscais.Items[0].EnviarEmail(ADestinatario,
            DoSubstituirVariaveis( IfThen( NaoEstaVazio(AAssunto), AAssunto, sAssunto) ),
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

{ TMetodoReciboNFe }

{ Params: 0 - Recibo - String com Numero Recibo para consulta
}
procedure TMetodoReciboNFe.Executar;
var
  I: integer;
  ARecibo: String;
begin
  ARecibo := fpCmd.Params(0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.WebServices.Recibo.Recibo := ARecibo;
    DoValidarIntegradorNFCe();
    ACBrNFe.WebServices.Recibo.Executar;

    RespostaRecibo;
    for I := 0 to ACBrNFe.WebServices.Recibo.NFeRetorno.ProtNFe.Count - 1 do
      RespostaItensRecibo(I);

    if ACBrNFe.Configuracoes.Geral.Salvar then
      fpCmd.Resposta := 'Arquivo=' + ACBrNFe.Configuracoes.Arquivos.PathSalvar +
                        ARecibo + '-pro-rec.xml';
  end;
end;

{ TMetodoConsultarNFe }

{ Params: 0 - XML - Uma String com um Path completo XML ou chave NFe
}
procedure TMetodoConsultarNFe.Executar;
var
  CargaDFe: TACBrCarregarNFe;
  AXML: String;
  I, J, K: Integer;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.NotasFiscais.Clear;
    CargaDFe := TACBrCarregarNFe.Create(ACBrNFe, AXML, False);
    try
      if (ACBrNFe.NotasFiscais.Count = 0) then
      begin
        if ValidarChave(AXML) then
          ACBrNFe.WebServices.Consulta.NFeChave := AXML
        else
          raise Exception.Create(
            'Parâmetro inválido. Chave do NFe inválida ou arquivo não encontrado.');
      end
      else
        ACBrNFe.WebServices.Consulta.NFeChave :=
          OnlyNumber(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID);

      DoValidarIntegradorNFCe(ACBrNFe.WebServices.Consulta.NFeChave);
      ACBrNFe.WebServices.Consulta.Executar;
      RespostaConsulta;

      if  FilesExists( AXML ) then
         fpCmd.Resposta :=  fpCmd.Resposta + 'Arquivo=' + AXML + sLineBreak;

      if NaoEstaVazio(Trim(ACBrNFe.WebServices.Consulta.retCancNFe.nProt)) then
         RespostaConsultaInfCan;

      for I:= 0 to ACBrNFe.WebServices.Consulta.procEventoNFe.Count-1 do
      begin
         RespostaConsultaProcEvento(I);
         RespostaConsultaDetEvento(I);

         for J:= 0 to ACBrNFe.WebServices.Consulta.procEventoNFe.Items[I].RetEventoNFe.InfEvento.detEvento.itemPedido.Count-1 do
           RespostaConsultaItemPedido(I, J);

         for J:= 0 to ACBrNFe.WebServices.Consulta.procEventoNFe.Items[I].RetEventoNFe.retEvento.Count-1 do
         begin
           RespostaConsultaRetEvento(I, J);

           for K:= 0 to ACBrNFe.WebServices.Consulta.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.chNFePend.Count-1 do
             RespostaConsultaChNFePend(I, J, K);
         end;

      end;

    finally
      CargaDFe.Free;
    end;
  end;

end;

{ TMetodoAssinarNFe }

{ Params: 0 - XML - Uma String com um Path completo XML NFe
}
procedure TMetodoAssinarNFe.Executar;
var
  CargaDFe: TACBrCarregarNFe;
  Salva: boolean;
  AXML: String;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.NotasFiscais.Clear;
    CargaDFe := TACBrCarregarNFe.Create(ACBrNFe, AXML);
    try
      Salva := ACBrNFe.Configuracoes.Geral.Salvar;

      if not Salva then
      begin
        ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
        ACBrNFe.Configuracoes.Arquivos.PathSalvar :=
          PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
      end;

      ACBrNFe.Configuracoes.Geral.Salvar := True;
      ACBrNFe.NotasFiscais.Assinar;
      ACBrNFe.Configuracoes.Geral.Salvar := Salva;

      if NaoEstaVazio(ACBrNFe.NotasFiscais.Items[0].NomeArq) then
        fpCmd.Resposta := ACBrNFe.NotasFiscais.Items[0].NomeArq
      else
        fpCmd.Resposta := PathWithDelim(ACBrNFe.Configuracoes.Arquivos.PathSalvar)
          + StringReplace(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID,
          'NFe', '', [rfIgnoreCase]) + '-nfe.xml';
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoValidarNFe }

{ Params: 0 - XML - Uma String com um Path completo XML NFe
}
procedure TMetodoValidarNFe.Executar;
var
  CargaDFe: TACBrCarregarNFe;
  AXML: String;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.NotasFiscais.Clear;
    CargaDFe := TACBrCarregarNFe.Create(ACBrNFe, AXML);
    try
      ACBrNFe.NotasFiscais.Validar;
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoStatusServico }

procedure TMetodoStatusServico.Executar;
begin
  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    DoValidarIntegradorNFCe();
    if ACBrNFe.WebServices.StatusServico.Executar then
      RespostaStatus;
  end;
end;

{ TMetodoImprimirEventoPDF }

{ Params: 0 - XMLEvento - Uma String com um Path completo XMLEvento NFe
          1 - XML       - Uma String com um Path completo XML NFe
}
procedure TMetodoImprimirEventoPDF.Executar;
var
  CargaDFe: TACBrCarregarNFe;
  CargaDFeEvento: TACBrCarregarNFeEvento;
  ArqPDF, AXMLEvento, AXML: string;
begin
  AXMLEvento := fpCmd.Params(0);
  AXML := fpCmd.Params(1);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.EventoNFe.Evento.Clear;
    CargaDFeEvento := TACBrCarregarNFeEvento.Create(ACBrNFe, AXMLEvento);
    ACBrNFe.NotasFiscais.Clear;
    if NaoEstaVazio(AXML) then
      CargaDFe := TACBrCarregarNFe.Create(ACBrNFe, AXML);
    try
      try
        DoConfiguraDANFe(False, '');
        ACBrNFe.ImprimirEventoPDF;
        ArqPDF := OnlyNumber(ACBrNFe.EventoNFe.Evento[0].InfEvento.Id);
        ArqPDF := PathWithDelim(ACBrNFe.DANFe.PathPDF) + ArqPDF + '-procEventoNFe.pdf';

        fpCmd.Resposta := 'Arquivo criado em: ' + ArqPDF;
      except
        on E: Exception do
          raise Exception.Create('Erro ao criar o arquivo PDF. '+ sLineBreak + E.Message );
      end;
    finally
      CargaDFeEvento.Free;
      if NaoEstaVazio(AXML) then
        CargaDFe.Free;
    end;
  end;
end;

{ TMetodoImprimirEvento }

{ Params: 0 - XMLEvento - Uma String com um Path completo XMLEvento NFe
          1 - XML - Uma String com um Path completo XML NFe
          2 - String com nome Impressora
          3 - Integer Número de Cópias
          4 - Mostrar Preview (1 - para preview)
}
procedure TMetodoImprimirEvento.Executar;
var
  CargaDFe: TACBrCarregarNFe;
  CargaDFeEvento: TACBrCarregarNFeEvento;
  AXMLEvento, AXML, AImpressora: String;
  ACopias: Integer;
  APreview: String;
  DanfeRL: TACBrNFeDANFeRL;
begin
  AXMLEvento := fpCmd.Params(0);
  AXML := fpCmd.Params(1);
  AImpressora := fpCmd.Params(2);
  ACopias := StrToIntDef(fpCmd.Params(3), 0);
  APreview := fpCmd.Params(4);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.EventoNFe.Evento.Clear;
    CargaDFeEvento := TACBrCarregarNFeEvento.Create(ACBrNFe, AXMLEvento);
    ACBrNFe.NotasFiscais.Clear;
    if NaoEstaVazio(AXML) then
      CargaDFe := TACBrCarregarNFe.Create(ACBrNFe, AXML);
    DanfeRL:= TACBrNFeDANFeRL.Create(ACBrNFe);
    try
      ACBrNFe.DANFE:= DanfeRL;
      DoConfiguraDANFe(False, Trim(APreview) );

      if NaoEstaVazio(AImpressora) then
        ACBrNFe.DANFe.Impressora := AImpressora;

      if (ACopias > 0) then
        ACBrNFe.DANFe.NumCopias := ACopias;

      try
        DoAntesDeImprimir( ( StrToBoolDef( APreview, False ) ) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
        ACBrNFe.ImprimirEvento;
      finally
        DoDepoisDeImprimir;
      end;

    finally
      CargaDFeEvento.Free;
      if NaoEstaVazio(AXML) then
        CargaDFe.Free;
      DanfeRL.Free;
    end;

    fpCmd.Resposta := 'Evento Impresso com sucesso';
  end;
end;

{ TMetodoImprimirDaNFePDF }

{ Params: 0 - XML - Uma String com um Path completo XML NFe
          1 - NumeroProtocolo: String com número do lote (opcional)
          2 - Marca Dagua: String para impressão marca Dagua
          3 - Via Consumidor: 1 para emitir via Consumidor
          4 - Simplificado: 1 Imprimir Danfe Simplificado
}
procedure TMetodoImprimirDaNFePDF.Executar;
var
  ArqPDF, AXML, AProtocolo: string;
  CargaDFe: TACBrCarregarNFe;
  MarcaDagua: String;
  Consumidor: Boolean;
  Simplificado: Boolean;
begin
  AXML := fpCmd.Params(0);
  AProtocolo := fpCmd.Params(1);
  MarcaDagua:= fpCmd.Params(2);
  Consumidor:= StrToBoolDef(fpCmd.Params(3),False);
  Simplificado:= StrToBoolDef(fpCmd.Params(4),False);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.NotasFiscais.Clear;
    CargaDFe := TACBrCarregarNFe.Create(ACBrNFe, AXML);
    try
      DoConfiguraDANFe(True, '');

      if (ACBrNFe.NotasFiscais.Items[0].NFe.Ide.modelo = 55) then
      begin
        if NaoEstaVazio(MarcaDagua) then
          TACBrNFeDANFeRL(ACBrNFe.DANFE).MarcadAgua:= MarcaDagua
        else
          TACBrNFeDANFeRL(ACBrNFe.DANFE).MarcadAgua:= '';
      end;

      ACBrNFe.DANFe.ProtocoloNFe :=  trim( AProtocolo );

      ACBrNFe.DANFE.ViaConsumidor := Consumidor;

      if Simplificado then
        ACBrNFe.DANFE.TipoDANFE := tiSimplificado;

      try
        ACBrNFe.NotasFiscais.ImprimirPDF;
        ArqPDF := OnlyNumber(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID) + '-NFe.pdf';
        ArqPDF := PathWithDelim(ACBrNFe.DANFe.PathPDF) + ArqPDF;
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

{ TMetodoCriarEnviarNFe }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini NFe
                         ou Uma String com conteúdo txt do NFe
          1 - NumeroLote: Integer com número do lote a ser adicionado
          2 - Imprime : 1 para imprimir  (Default)
          3 - Sincrono: 1 para envio Sincrono (Default)
          4 - NomeImpressora: String com nome impressora para impressão (Default)
          5 - MostrarPreview: 1 para mostrar preview (Default)
          6 - Numero de Copias: Inteiro com número de cópias (Default)
          7 - ImprimirPDF: 1 para imprimir PDF (Default)
}
procedure TMetodoCriarEnviarNFe.Executar;
var
  Salva: Boolean;
  Alertas: String;
  ArqNFe: String;
  Resp: String;
  AIni: String;
  ALote: Integer;
  AImprime: Boolean;
  ASincrono: Boolean;
  AImpressora: String;
  APreview: String;
  ACopias: Integer;
  APDF: Boolean;
begin

  AIni          := fpCmd.Params(0);
  ALote         := StrToIntDef(fpCmd.Params(1), 0);
  AImprime      := StrToBoolDef(fpCmd.Params(2), False);
  ASincrono     := StrToBoolDef(fpCmd.Params(3), False);
  AImpressora   := fpCmd.Params(4);
  APreview      := fpCmd.Params(5);
  ACopias       := StrToIntDef(fpCmd.Params(6), 0);
  APDF          := StrToBoolDef(fpCmd.Params(7), False);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin

    LerIniNFe(AIni);

    Salva := ACBrNFe.Configuracoes.Geral.Salvar;
    if not Salva then
    begin
      ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
      ACBrNFe.Configuracoes.Arquivos.PathSalvar :=
        PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
    end;

    ACBrNFe.NotasFiscais.GerarNFe;
    Alertas := ACBrNFe.NotasFiscais.Items[0].Alertas;

    ACBrNFe.NotasFiscais.Assinar;
    ACBrNFe.NotasFiscais.Validar;

    ArqNFe := PathWithDelim(ACBrNFe.Configuracoes.Arquivos.PathSalvar) +
      OnlyNumber(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID) + '-nfe.xml';
    ACBrNFe.NotasFiscais.GravarXML(ArqNFe);

    if not FileExists(ArqNFe) then
      raise Exception.Create('Não foi possível criar o arquivo ' + ArqNFe);

    Resp := ArqNFe;
    if (Alertas <> '') then
      Resp := Resp + sLineBreak + 'Alertas:' + Alertas;

    fpCmd.Resposta := Resp + sLineBreak;

    ACBrNFe.WebServices.Enviar.Sincrono := ASincrono;

    if (ALote = 0) then
      ACBrNFe.WebServices.Enviar.Lote := '1'
    else
      ACBrNFe.WebServices.Enviar.Lote := IntToStr(ALote);

    if ACBrNFe.NotasFiscais.Count > 0 then
      DoValidarIntegradorNFCe(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID);
    ACBrNFe.WebServices.Enviar.Executar;
    RespostaEnvio;

    if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
    begin
      ACBrNFe.WebServices.Retorno.Recibo := ACBrNFe.WebServices.Enviar.Recibo;
      ACBrNFe.WebServices.Retorno.Executar;
      RespostaRetorno;
      RespostaNotasFiscais(AImprime, AImpressora, APreview, ACopias, APDF);

    end
    else
    if AImprime then //Sincrono
      ImprimirNFe(AImpressora, APreview, ACopias, APDF);

  end;
end;

{ TMetodoAdicionarNFe }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini NFe
                         ou Uma String com conteúdo txt do NFe
          1 - NumeroLote: String com número do lote a ser adicionado
}
procedure TMetodoAdicionarNFe.Executar;
var
  Alertas: ansistring;
  ArqNFe: string;
  Resp, AIni, ANumeroLote: string;
begin

  AIni := fpCmd.Params(0);
  ANumeroLote := fpCmd.Params(1);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin

    LerIniNFe(AIni);

    ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'Lotes' + PathDelim + 'Lote' + trim(ANumeroLote));

    ACBrNFe.NotasFiscais.GerarNFe;
    Alertas := ACBrNFe.NotasFiscais.Items[0].Alertas;

    ACBrNFe.NotasFiscais.Assinar;
    ACBrNFe.NotasFiscais.Validar;

    ArqNFe := PathWithDelim(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'Lotes' + PathDelim + 'Lote' + trim(ANumeroLote)) + OnlyNumber(
      ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID) + '-nfe.xml';
    ACBrNFe.NotasFiscais.GravarXML(ExtractFilePath(ArqNFe));

    if not FileExists(ArqNFe) then
      raise Exception.Create('Não foi possível criar o arquivo ' + ArqNFe);

    Resp := ArqNFe;
    if (Alertas <> '') then
      Resp := Resp + sLineBreak + 'Alertas:' + Alertas;

    fpCmd.Resposta := Resp;
  end;
end;

{ TMetodoEnviarLoteNFe }

{ Params: 0 - LoteEnvio: Integer com número do lote. Default = 1
          1 - Imprime: 1 para Imprimir
          2 - Impressora: String Nome da Impressora
          3 - Sincrono: 1 para Envio Sincrono
          4 - Preview: 1 para Mostrar Preview
          5 - Copias: Inteiro com número de cópias para impressão
          6 - PDF: 1 para impressão em PDF
}
procedure TMetodoEnviarLoteNFe.Executar;
var
  RetFind: integer;
  SearchRec: TSearchRec;
  ALoteEnvio: String;
  AImprime: Boolean;
  AImpressora: String;
  ASincrono: Boolean;
  APreview: String;
  ACopias: Integer;
  APDF: Boolean;
begin
  ALoteEnvio   := Trim(fpCmd.Params(0));
  AImprime     := StrToBoolDef(fpCmd.Params(1), False);
  AImpressora  := fpCmd.Params(2);
  ASincrono    := StrToBoolDef(fpCmd.Params(3), False);
  APreview     := fpCmd.Params(4);
  ACopias      := StrToIntDef(fpCmd.Params(5), 0);
  APDF         := StrToBoolDef(fpCmd.Params(6), False);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    if not DirectoryExists(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'Lotes' + PathDelim + 'Lote' + ALoteEnvio) then
        raise Exception.Create('Diretório não encontrado:' + PathWithDelim(
          ExtractFilePath(Application.ExeName)) +
          'Lotes' + PathDelim + 'Lote' + ALoteEnvio)
    else
    begin
      ACBrNFe.NotasFiscais.Clear;
      RetFind := SysUtils.FindFirst(
        PathWithDelim(ExtractFilePath(Application.ExeName)) +
        'Lotes' + PathDelim + 'Lote' +
        ALoteEnvio + PathDelim + '*-nfe.xml', faAnyFile, SearchRec);
      if (RetFind = 0) then
      begin
        while RetFind = 0 do
        begin
          ACBrNFe.NotasFiscais.LoadFromFile(PathWithDelim(ExtractFilePath(Application.ExeName)) +
            'Lotes' + PathDelim +
            'Lote' + ALoteEnvio + PathDelim + SearchRec.Name);
          RetFind := FindNext(SearchRec);
        end;
        ACBrNFe.NotasFiscais.GerarNFe;
        ACBrNFe.NotasFiscais.Assinar;
        ACBrNFe.NotasFiscais.Validar;
      end
      else
        raise Exception.Create('Não foi encontrada nenhuma nota para o Lote: ' +
          ALoteEnvio);
    end;

    ACBrNFe.WebServices.Enviar.Lote := ALoteEnvio;
    ACBrNFe.WebServices.Enviar.Sincrono := ASincrono;

    if ACBrNFe.NotasFiscais.Count > 0 then
      DoValidarIntegradorNFCe(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID);
    ACBrNFe.WebServices.Enviar.Executar;

    RespostaEnvio;

    if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
    begin
      ACBrNFe.WebServices.Retorno.Recibo := ACBrNFe.WebServices.Enviar.Recibo;
      ACBrNFe.WebServices.Retorno.Executar;

      RespostaRetorno;
      RespostaNotasFiscais(AImprime, AImpressora, APreview, ACopias, APDF);

    end
    else
    if AImprime then //Sincrono
      ImprimirNFe(AImpressora, APreview, ACopias, APDF);

  end;
end;

{ TMetodoEnviarNFe }

{ Params: 0 - PathorXML - Uma String com um Path completo arquivo XML NFe
                         ou Uma String com conteúdo XML do NFe
          1 - Lote: Integer com número do lote. Default = 1
          2 - Assina: 1 para assinar XML
          3 - Imprime: 1 Para True. Default 0
          4 - Nome Impressora: String com Nome da Impressora
          5 - Sincrono: 1 Para Sincrono
          6 - ValidarXML: Validar o XML
          7 - GerarXML: Gerar Novamente o XML
}
procedure TMetodoEnviarNFe.Executar;
var
  CargaDFe: TACBrCarregarNFe;
  APathorXML, AImpressora: String;
  ALote: Integer;
  AAssina, AImprime, ASincrono, AGerarXML, AValidaXML: Boolean;
  AErro: String;
begin
  AErro:= '';
  APathorXML := fpCmd.Params(0);
  ALote := StrToIntDef(fpCmd.Params(1), 0);
  AAssina := StrToBoolDef(fpCmd.Params(2), False);
  AImprime := StrToBoolDef(fpCmd.Params(3), False);
  AImpressora := fpCmd.Params(4);
  ASincrono := StrToBoolDef(fpCmd.Params(5), False);
  AValidaXML := StrToBoolDef(fpCmd.Params(6), False);
  AGerarXML := StrToBoolDef(fpCmd.Params(7), False);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.NotasFiscais.Clear;
    CargaDFe := TACBrCarregarNFe.Create(ACBrNFe, APathorXML);
    try
      if AGerarXML and
         ( not (ACBrNFe.NotasFiscais.VerificarAssinatura(AErro)) )  then
        ACBrNFe.NotasFiscais.GerarNFe;

      if (AAssina) then
        ACBrNFe.NotasFiscais.Assinar;

      if (AValidaXML) then
        ACBrNFe.NotasFiscais.Validar;

      if (ALote = 0) then
        ACBrNFe.WebServices.Enviar.Lote := '1'
      else
        ACBrNFe.WebServices.Enviar.Lote := IntToStr(ALote);

      ACBrNFe.WebServices.Enviar.Sincrono := ASincrono;
      if ACBrNFe.NotasFiscais.Count > 0 then
        DoValidarIntegradorNFCe(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID);

      ACBrNFe.WebServices.Enviar.Executar;

      RespostaEnvio;

      if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
      begin
        ACBrNFe.WebServices.Retorno.Recibo := ACBrNFe.WebServices.Enviar.Recibo;
        ACBrNFe.WebServices.Retorno.Executar;

        RespostaRetorno;
        RespostaNotasFiscais(AImprime, AImpressora, '' , 0, False);

      end
      else
      if AImprime then //Sincrono
        ImprimirNFe(AImpressora, '', 0, False);


    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoCriarNFe }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini NFe
                         ou Uma String com conteúdo txt do NFe
          1 - RetornaXML: 1 para Retornar XML Gerado na Resposta
          2 - AssinaNFe: 0 para NÃO assinar, por padrão gera sempre assinando o XML
}
procedure TMetodoCriarNFe.Executar;
var
  Salva, ARetornaXML: boolean;
  Alertas: ansistring;
  ArqNFe: string;
  SL: TStringList;
  Resp, AIni: string;
  AAssina: Boolean;
begin
  AIni := fpCmd.Params(0);
  ARetornaXML := StrToBoolDef(fpCmd.Params(1), False);
  AAssina := StrToBoolDef(fpCmd.Params(2), True);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin

    LerIniNFe(AIni);

    Salva := ACBrNFe.Configuracoes.Geral.Salvar;
    if not Salva then
    begin
      ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
      ACBrNFe.Configuracoes.Arquivos.PathSalvar :=
        PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
    end;

    ACBrNFe.NotasFiscais.GerarNFe;
    Alertas := ACBrNFe.NotasFiscais.Items[0].Alertas;

    if AAssina then
    begin
      ACBrNFe.NotasFiscais.Assinar;
      ACBrNFe.NotasFiscais.Validar;
    end;

    ArqNFe := PathWithDelim(ACBrNFe.Configuracoes.Arquivos.PathSalvar) +
      OnlyNumber(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID) + '-nfe.xml';
    ACBrNFe.NotasFiscais.GravarXML(ArqNFe);

    if not FileExists(ArqNFe) then
      raise Exception.Create('Não foi possível criar o arquivo ' + ArqNFe);

    Resp := ArqNFe;
    if (Alertas <> '') then
      Resp := Resp + sLineBreak + 'Alertas:' + Alertas;

    if ARetornaXML then
    begin
      SL := TStringList.Create;
      try
        SL.LoadFromFile(ArqNFe);
        Resp := Resp + sLineBreak + SL.Text;
      finally
        SL.Free;
      end;
    end;

    fpCmd.Resposta := Resp;
  end;
end;

{ TMetodoImprimirDaNFe }

{ Params: 0 - XMLFile - Uma String com um Path completo para um arquivo XML NFe
                         ou Uma String com conteúdo XML NFe
          1 - Impressora: String com Nome da Impressora
          2 - Copias: Integer Número de Copias
          3 - Protocolo: String com Número de Protocolo
          4 - Preview: 1 para Mostrar Preview
          5 - MarcaDagua: Parâmetro para MarcaDagua
          6 - Consumidor: 1 para imprimir via consumidor
          7 - Simplificado: 1 para imprimir modo simplificado
}
procedure TMetodoImprimirDaNFe.Executar;
var
  CargaDFe: TACBrCarregarNFe;
  AChave, AImpressora, AProtocolo: String;
  ACopias: Integer;
  APreview: String;
  AMarcaDagua: String;
  AConsumidor: Boolean;
  ASimplificado: Boolean;
begin
  AChave := fpCmd.Params(0);
  AImpressora := fpCmd.Params(1);
  ACopias := StrToIntDef(fpCmd.Params(2), 0);
  AProtocolo := fpCmd.Params(3);
  APreview := fpCmd.Params(4);
  AMarcaDagua := fpCmd.Params(5);
  AConsumidor := StrToBoolDef(fpCmd.Params(6), False);
  ASimplificado := StrToBoolDef(fpCmd.Params(7), False);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.NotasFiscais.Clear;
    CargaDFe := TACBrCarregarNFe.Create(ACBrNFe, AChave);
    try
      DoConfiguraDANFe(False, Trim(APreview) );

      if NaoEstaVazio(AImpressora) then
        ACBrNFe.DANFe.Impressora := AImpressora;

      if (ACopias > 0) then
        ACBrNFe.DANFe.NumCopias := ACopias;

      ACBrNFe.DANFe.ProtocoloNFe := trim( AProtocolo );

      if (ACBrNFe.NotasFiscais.Items[0].NFe.Ide.modelo = 55) then
      begin
        if NaoEstaVazio(AMarcaDagua) then
          TACBrNFeDANFeRL(ACBrNFe.DANFE).MarcadAgua:= AMarcaDagua
        else
          TACBrNFeDANFeRL(ACBrNFe.DANFE).MarcadAgua:= '';
      end;

      ACBrNFe.DANFE.ViaConsumidor := AConsumidor;

      if ASimplificado then
          ACBrNFe.DANFE.TipoDANFE := tiSimplificado;

      try
        DoAntesDeImprimir(( StrToBoolDef( APreview, False) ) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
        ACBrNFe.NotasFiscais.Imprimir;
      finally
        DoDepoisDeImprimir;
      end;
    finally
      CargaDFe.Free;
    end;

    fpCmd.Resposta := 'DANFe Impresso com sucesso';
  end;
end;

{ TMetodoCancelarNFe }

{ Params: 0 - Chave - Uma String com a Chave XML NFe
          1 - Justificativa: String (Mínimo de 15 Carcteres)
          2 - CNPJ: String com CNPJ do emitente
          3 - Lote: Integer com Numero do Lote. Default = 1
}
procedure TMetodoCancelarNFe.Executar;
var
  AChave, AJustificativa, ACNPJ: String;
  ALote: Integer;
begin
  AChave := fpCmd.Params(0);
  AJustificativa := fpCmd.Params(1);
  ACNPJ := fpCmd.Params(2);
  ALote := StrToIntDef(fpCmd.Params(3), 1);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    if not ValidarChave(AChave) then
      raise Exception.Create('Chave ' + AChave + ' inválida.')
    else
      ACBrNFe.WebServices.Consulta.NFeChave := AChave;

    ACBrNFe.WebServices.Consulta.Executar;

    ACBrNFe.EventoNFe.Evento.Clear;
    with ACBrNFe.EventoNFe.Evento.Add do
    begin
      infEvento.CNPJ := ACNPJ;
      if Trim(infEvento.CNPJ) = '' then
        infEvento.CNPJ := copy(OnlyNumber(ACBrNFe.WebServices.Consulta.NFeChave), 7, 14)
      else
      begin
        if not ValidarCNPJ(ACNPJ) then
          raise Exception.Create('CNPJ ' + ACNPJ + ' inválido.');
      end;

      infEvento.cOrgao := StrToIntDef(
        copy(OnlyNumber(ACBrNFe.WebServices.Consulta.NFeChave), 1, 2), 0);
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.chNFe := ACBrNFe.WebServices.Consulta.NFeChave;
      infEvento.detEvento.nProt := ACBrNFe.WebServices.Consulta.Protocolo;
      infEvento.detEvento.xJust := AJustificativa;
    end;
    DoValidarIntegradorNFCe(AChave);

    ACBrNFe.EnviarEvento(ALote);
    RespostaCancelamento;

  end;
end;

{ TMetodoSetModeloDF }

{ Params: 0 - NumAmbiente : Integer 55- NFe / 65- NFCe
}
procedure TMetodoSetModeloDF.Executar;
var
  OK: boolean;
  NumModelo: Integer;
begin
  NumModelo := StrToIntDef(fpCmd.Params(0), 2);

  if not (NumModelo in [55, 65]) then
    raise Exception.Create('Modelo Inválido: '+IntToStr(NumModelo));

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.Configuracoes.Geral.ModeloDF := StrToModeloDF(OK, IntToStr(NumModelo));
    DoValidarIntegradorNFCe();

  end;
end;

{ TMetodoSetToken }

{ Params: 0 - Token: Documentação atual é chamado de CSC - Codigo de Segurança do Contribuinte
          1 - IdToken: Documentação atual é chamado de IdCSC - Identificador
}
procedure TMetodoSetToken.Executar;
begin
  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.WebService.NFCe do
    begin
      Token := fpCmd.Params(0);

      if fpCmd.Params(1) <> '' then
        IdToken := fpCmd.Params(1);
    end;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoSetIdToken }

{ Params: 0 - IdToken: Documentação atual é chamado de IdCSC - Identificador
          1 - Token: Documentação atual é chamado de CSC - Codigo de Segurança do Contribuinte
}
procedure TMetodoSetIdToken.Executar;
begin
  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.WebService.NFCe do
    begin
      IdToken := fpCmd.Params(0);

      if fpCmd.Params(1) <> '' then
        Token := fpCmd.Params(1);
    end;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoCNPJCertificado }

procedure TMetodoCNPJCertificado.Executar;
begin
  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrNFe.SSL.CertCNPJ;
  end;
end;

{ TMetodoImprimirInutilizacao }

{ Params: 0 - XMLInutilizacao - Uma String com um Path completo XMLInutilizacao
          1 - String com nome Impressora
          2 - Integer Número de Cópias
          3 - MostrarPreview
}
procedure TMetodoImprimirInutilizacao.Executar;
var
  CargaDFeInut: TACBrCarregarNFeInut;
  AXMLInut, AImpressora: String;
  ACopias: Integer;
  APreview: String;
  DanfeRL: TACBrNFeDANFeRL;
begin
  AXMLInut := fpCmd.Params(0);
  AImpressora := fpCmd.Params(1);
  ACopias := StrToIntDef(fpCmd.Params(2), 0);
  APreview:= fpCmd.Params(3);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.InutNFe.ID := '';
    CargaDFeInut := TACBrCarregarNFeInut.Create(ACBrNFe, AXMLInut);
    DanfeRL:= TACBrNFeDANFeRL.Create(ACBrNFe);
    try
      ACBrNFe.DANFE:= DanfeRL;
      DoConfiguraDANFe(False, Trim(APreview) );

      if NaoEstaVazio(AImpressora) then
        ACBrNFe.DANFe.Impressora := AImpressora;

      if (ACopias > 0) then
        ACBrNFe.DANFe.NumCopias := ACopias;

      try
        DoAntesDeImprimir(( StrToBoolDef(APreview,False) ) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
        ACBrNFe.ImprimirInutilizacao;
      finally
        DoDepoisDeImprimir;
      end;
    finally
      CargaDFeInut.Free;
      DanfeRL.Free;
    end;

    fpCmd.Resposta := 'Inutilização Impresso com sucesso';
  end;
end;

{ TMetodoImprimirInutilizacaoPDF }

{ Params: 0 - XMLInutilizacao - Uma String com um Path completo XMLInutilizacao
}
procedure TMetodoImprimirInutilizacaoPDF.Executar;
var
  CargaDFeInut: TACBrCarregarNFeInut;
  AXMLInut, ArqPDF: String;
  DanfeRL: TACBrNFeDANFeRL;
begin
  AXMLInut := fpCmd.Params(0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.InutNFe.ID := '';
    CargaDFeInut := TACBrCarregarNFeInut.Create(ACBrNFe, AXMLInut);
    DanfeRL:= TACBrNFeDANFeRL.Create(ACBrNFe);
    try
      ACBrNFe.DANFE:= DanfeRL;
      DoConfiguraDANFe(False, '');
      try
        ACBrNFe.ImprimirInutilizacaoPDF;
        ArqPDF := OnlyNumber(ACBrNFe.InutNFe.ID);
        ArqPDF := PathWithDelim(ACBrNFe.DANFe.PathPDF) + ArqPDF + '-procInutNFe.pdf';

        fpCmd.Resposta := 'Arquivo criado em: ' + ArqPDF;
      except
        on E: Exception do
          raise Exception.Create('Erro ao criar o arquivo PDF. ' + sLineBreak + E.Message);
      end;
    finally
      CargaDFeInut.Free;
      DanfeRL.free;
    end;
  end;
end;

{ TMetodoGetPathNFe }

procedure TMetodoGetPathNFe.Executar;
begin
  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrNFe.Configuracoes.Arquivos.GetPathNFe();
  end;
end;

{ TMetodoGetPathCCe }

procedure TMetodoGetPathCCe.Executar;
begin
  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrNFe.Configuracoes.Arquivos.GetPathEvento(teCCe);
  end;
end;

{ TMetodoGetPathCan }

procedure TMetodoGetPathCan.Executar;
begin
  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrNFe.Configuracoes.Arquivos.GetPathEvento(teCancelamento);
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
  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrNFe.Configuracoes.Arquivos.GetPathEvento(TpcnTpEvento(CodEvento));
  end;
end;

{ TMetodoGetPathInu }

procedure TMetodoGetPathInu.Executar;
begin
  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrNFe.Configuracoes.Arquivos.GetPathInu(fpCmd.Params(0));
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

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.EventoNFe.Evento.Clear;

    ACBrNFe.EventoNFe.LerFromIni( AArq, False );

    if ACBrNFe.EventoNFe.Evento.Count > 0 then
      DoValidarIntegradorNFCe(ACBrNFe.EventoNFe.Evento.Items[0].InfEvento.chNFe);
    ACBrNFe.EnviarEvento(ACBrNFe.EventoNFe.idLote);


    RespostaEvento;

    for I := 0 to ACBrNFe.WebServices.EnvEvento.EventoRetorno.retEvento.Count - 1 do
       RespostaItensEvento(I);
  end;
end;

{ TMetodoCartaCorrecao }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini Evento
                         ou Uma String com conteúdo txt do Evento
}
procedure TMetodoCartaCorrecao.Executar;
var
  AArq: String;
  I: Integer;
begin
  AArq := fpCmd.Params(0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.EventoNFe.Evento.Clear;

    ACBrNFe.EventoNFe.LerFromIni( AArq, True );

    if ACBrNFe.EventoNFe.Evento.Count > 0 then
      DoValidarIntegradorNFCe(ACBrNFe.EventoNFe.Evento.Items[0].InfEvento.chNFe);
    ACBrNFe.EnviarEvento(ACBrNFe.EventoNFe.idLote);

    RespostaEvento;

    for I := 0 to ACBrNFe.WebServices.EnvEvento.EventoRetorno.retEvento.Count - 1 do
       RespostaItensEvento(I);
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
  CargaDFeEvento: TACBrCarregarNFeEvento;
begin
  AArq := fpCmd.Params(0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.EventoNFe.Evento.Clear;
    CargaDFeEvento := TACBrCarregarNFeEvento.Create(ACBrNFe, AArq);
    try

      if ACBrNFe.EventoNFe.Evento.Count > 0 then
        DoValidarIntegradorNFCe(ACBrNFe.EventoNFe.Evento.Items[0].InfEvento.chNFe);
      ACBrNFe.EnviarEvento(ACBrNFe.EventoNFe.idLote);

    finally
      CargaDFeEvento.Free;
    end;

    RespostaEvento;

    for I := 0 to ACBrNFe.WebServices.EnvEvento.EventoRetorno.retEvento.Count - 1 do
       RespostaItensEvento(I);
  end;
end;

{ TMetodoDistribuicaoDFePorChaveNFe }

{ Params: 0 - Código da UF do autor da consulta
          1 - CNPJ do autor da consulta
          2 - Chave da NF-e que se deseja baixar o XML
}
procedure TMetodoDistribuicaoDFePorChaveNFe.Executar;
var
  AUF: Integer;
  ACNPJ: String;
  AChave: String;
  I, J: Integer;
begin
  AUF := StrToIntDef(fpCmd.Params(0), 0);
  ACNPJ := fpCmd.Params(1);
  AChave := fpCmd.Params(2);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    if not ValidarCNPJ(ACNPJ) then
      raise Exception.Create('CNPJ '+ACNPJ+' inválido.');

    DoValidarIntegradorNFCe();
    ACBrNFe.DistribuicaoDFePorChaveNFe(AUF, ACNPJ, AChave);

    RespostaDistribuicaoDFe;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].resNFe.chNFe) <> '') then
      begin
        RespostaItensDistribuicaoDFeResNFe(I,J);
        inc(J);
      end;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(AcbrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].resEvento.chNFe) <> '') then
      begin
        RespostaItensDistribuicaoDFeResEve(I,J);
        inc(J);
      end;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(AcbrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].procEvento.detEvento.versao) <> '' ) then
      begin
        RespostaItensDistribuicaoDFeProEve(I,J);
        inc(J);
      end;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(AcbrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].procEvento.RetinfEvento.Id) <> '' ) then
      begin
        RespostaItensDistribuicaoDFeInfEve(I,J);
        inc(J);
      end;

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
  I, J: Integer;
begin
  AUF := StrToIntDef(fpCmd.Params(0), 0);
  ACNPJ := fpCmd.Params(1);
  AUltNSU := fpCmd.Params(2);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    if not ValidarCNPJ(ACNPJ) then
      raise Exception.Create('CNPJ '+ACNPJ+' inválido.');

    DoValidarIntegradorNFCe();
    ACBrNFe.DistribuicaoDFePorUltNSU(AUF, ACNPJ, AUltNSU);

    RespostaDistribuicaoDFe;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].resNFe.chNFe) <> '') then
      begin
        RespostaItensDistribuicaoDFeResNFe(I,J);
        inc(J);
      end;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(AcbrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].resEvento.chNFe) <> '') then
      begin
        RespostaItensDistribuicaoDFeResEve(I,J);
        inc(J);
      end;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(AcbrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].procEvento.detEvento.versao) <> '' ) then
      begin
        RespostaItensDistribuicaoDFeProEve(I,J);
        inc(J);
      end;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(AcbrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].procEvento.RetinfEvento.Id) <> '' ) then
      begin
        RespostaItensDistribuicaoDFeInfEve(I,J);
        inc(J);
      end;

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
  I, J: Integer;
begin
  AUF := StrToIntDef(fpCmd.Params(0), 0);
  ACNPJ := fpCmd.Params(1);
  ANSU := fpCmd.Params(2);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    if not ValidarCNPJ(ACNPJ) then
      raise Exception.Create('CNPJ '+ACNPJ+' inválido.');

    DoValidarIntegradorNFCe();
    ACBrNFe.DistribuicaoDFePorUltNSU(AUF, ACNPJ, ANSU);

    RespostaDistribuicaoDFe;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].resNFe.chNFe) <> '') then
      begin
        RespostaItensDistribuicaoDFeResNFe(I,J);
        inc(J);
      end;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(AcbrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].resEvento.chNFe) <> '') then
      begin
        RespostaItensDistribuicaoDFeResEve(I,J);
        inc(J);
      end;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(AcbrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].procEvento.detEvento.versao) <> '' ) then
      begin
        RespostaItensDistribuicaoDFeProEve(I,J);
        inc(J);
      end;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(AcbrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].procEvento.RetinfEvento.Id) <> '' ) then
      begin
        RespostaItensDistribuicaoDFeInfEve(I,J);
        inc(J);
      end;

  end;

end;

{ TMetodoEnviaremailEvento }

{ Params: 0 - Email: String com email Destinatário
          1 - XML: String com path do XML Evento
          2 - XML: String com path do XML Nota
          3 - Boolean 1 : Envia PDF
          4 - Assunto: String com Assunto do e-mail
          5 - Copia: String com e-mails copia (Separados ;)
          6 - Anexo: String com Path de Anexos (Separados ;)
}
procedure TMetodoEnviaremailEvento.Executar;
var
  sAssunto, ADestinatario, APathXMLEvento, APathXML, AAssunto, AEmailCopias,
  AAnexos, ArqPDF, ArqEvento: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  CargaDFeEvento: TACBrCarregarNFeEvento;
  CargaDFe: TACBrCarregarNFe;
  AEnviaPDF: Boolean;
  TipoEvento: TpcnTpEvento;
begin
  ADestinatario := fpCmd.Params(0);
  APathXMLEvento := fpCmd.Params(1);
  APathXML := fpCmd.Params(2);
  AEnviaPDF := StrToBoolDef(fpCmd.Params(3), False);
  AAssunto := fpCmd.Params(4);
  AEmailCopias := fpCmd.Params(5);
  AAnexos := fpCmd.Params(6);
  ArqEvento := '';

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.EventoNFe.Evento.Clear;
    ACBrNFe.NotasFiscais.Clear;

    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    try
      CargaDFeEvento := TACBrCarregarNFeEvento.Create(ACBrNFe, APathXMLEvento);
      if NaoEstaVazio(APathXML) then
        CargaDFe := TACBrCarregarNFe.Create(ACBrNFe, APathXML);
      try
        DoConfiguraDANFe(True, '');
        if AEnviaPDF then
        begin
          try
            ACBrNFe.ImprimirEventoPDF;

            ArqPDF := OnlyNumber(ACBrNFe.EventoNFe.Evento[0].InfEvento.id);
            ArqPDF := PathWithDelim(ACBrNFe.DANFE.PathPDF)+ArqPDF+'-procEventoNFe.pdf';
          except
            on E: Exception do
              raise Exception.Create('Erro ao criar o arquivo PDF. ' + sLineBreak + E.Message);
          end;
        end;

        with MonitorConfig.DFE.Email do
        begin
          slMensagemEmail.Text := DoSubstituirVariaveis( StringToBinaryString(MensagemNFe) );
          sAssunto := AssuntoNFe;
        end;

        QuebrarLinha(AEmailCopias, slCC);

        QuebrarLinha(AAnexos, slAnexos);

        // Se carregou evento usando XML como parâmetro, salva XML para poder anexar
        if  StringIsXML( APathXMLEvento ) then
        begin
          tipoEvento := ACBrNFe.EventoNFe.Evento[0].InfEvento.tpEvento;
          ArqEvento  := ACBrNFe.EventoNFe.ObterNomeArquivo(tipoEvento);
          ArqEvento  := PathWithDelim(ACBrNFe.Configuracoes.Arquivos.GetPathEvento(tipoEvento))+ArqEvento;
          ACBrNFe.EventoNFe.Gerador.ArquivoFormatoXML := APathXMLEvento;
          ACBrNFe.EventoNFe.Gerador.SalvarArquivo(ArqEvento);
          slAnexos.Add(ArqEvento)
        end
        else
          slAnexos.Add(APathXMLEvento);

        if AEnviaPDF then
          slAnexos.Add(ArqPDF);

        try
          ACBrNFe.EnviarEmail(ADestinatario,
            DoSubstituirVariaveis( IfThen(NaoEstaVazio(AAssunto), AAssunto, sAssunto) ),
            slMensagemEmail,
            slCC,      // Lista com emails que serão enviado cópias - TStrings
            slAnexos); // Lista de slAnexos - TStrings

          fpCmd.Resposta := 'Email enviado com sucesso';
        except
          on E: Exception do
            raise Exception.Create('Erro ao enviar email' + sLineBreak + E.Message);
        end;
      finally
        if NaoEstaVazio(APathXML) then
          CargaDFe.Free;
        CargaDFeEvento.Free;
      end;
    finally
      slCC.Free;
      slAnexos.Free;
      slMensagemEmail.Free;
    end;
  end;
end;

{ TMetodoEnviaremailInutilizacao }

{ Params: 0 - Email: String com email Destinatário
          1 - XML: String com path do XML Inutilização
          2 - Boolean 1 : Envia PDF
          3 - Assunto: String com Assunto do e-mail
          4 - Copia: String com e-mails copia (Separados ;)
          5 - Anexo: String com Path de Anexos (Separados ;)
}
procedure TMetodoEnviaremailInutilizacao.Executar;
var
  sAssunto, ADestinatario, APathXML, AAssunto, AEmailCopias,
  AAnexos, ArqPDF, ArqInut: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  CargaDFe: TACBrCarregarNFeInut;
  AEnviaPDF: Boolean;
begin
  ADestinatario := fpCmd.Params(0);
  APathXML := fpCmd.Params(1);
  AEnviaPDF := StrToBoolDef(fpCmd.Params(2), False);
  AAssunto := fpCmd.Params(3);
  AEmailCopias := fpCmd.Params(4);
  AAnexos := fpCmd.Params(5);
  ArqInut := '';

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    try
      CargaDFe := TACBrCarregarNFeInut.Create(ACBrNFe, APathXML);
      try
        DoConfiguraDANFe(True, '');
        if AEnviaPDF then
        begin
          try
            ACBrNFe.ImprimirInutilizacaoPDF;

            ArqPDF := OnlyNumber(ACBrNFe.InutNFe.ID);
            ArqPDF := PathWithDelim(ACBrNFe.DANFE.PathPDF)+ArqPDF+'-procInutNFe.pdf';
          except
            on E: Exception do
              raise Exception.Create('Erro ao criar o arquivo PDF. '+ sLineBreak + E.Message);
          end;
        end;

        with MonitorConfig.DFE.Email do
        begin
          slMensagemEmail.Text := DoSubstituirVariaveis( StringToBinaryString(MensagemNFe) );
          sAssunto := AssuntoNFe;
        end;

        QuebrarLinha(AEmailCopias, slCC);

        QuebrarLinha(AAnexos, slAnexos);

        // Se carregou evento usando XML como parâmetro, salva XML para poder anexar
        if  StringIsXML( APathXML ) then
        begin
          ArqInut  := ACBrNFe.InutNFe.ObterNomeArquivo;
          ArqInut  := PathWithDelim(ACBrNFe.Configuracoes.Arquivos.GetPathInu()) + ArqInut;
          ACBrNFe.EventoNFe.Gerador.ArquivoFormatoXML := APathXML;
          ACBrNFe.EventoNFe.Gerador.SalvarArquivo(ArqInut);
          slAnexos.Add(ArqInut)
        end
        else
          slAnexos.Add(APathXML);

        if AEnviaPDF then
          slAnexos.Add(ArqPDF);

        try
          ACBrNFe.EnviarEmail(ADestinatario,
            DoSubstituirVariaveis( IfThen(NaoEstaVazio(AAssunto), AAssunto, sAssunto) ),
            slMensagemEmail,
            slCC,      // Lista com emails que serão enviado cópias - TStrings
            slAnexos); // Lista de slAnexos - TStrings

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

{ TMetodoValidarRegrasNegocios }

{ Params: 0 - XML - String com path XML
}
procedure TMetodoValidarRegrasNegocios.Executar;
var
  CargaDFe: TACBrCarregarNFe;
  AXML, ErrosRegraNegocio: string;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.NotasFiscais.Clear;
    CargaDFe := TACBrCarregarNFe.Create(ACBrNFe, AXML);
    try
      ACBrNFe.NotasFiscais.ValidarRegrasdeNegocios(ErrosRegraNegocio);

      if NaoEstaVazio(ErrosRegraNegocio) then
        raise Exception.Create(ErrosRegraNegocio);
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoImprimirRelatorio }

{ Params: 1 - Texto para impressão
}
procedure TMetodoImprimirRelatorio.Executar;
var
  MemoTXT: TStringList;
  ATexto: String;
  DanfeEscPos: TACBrNFeDANFeESCPOS;
  POSPrinter: TACBrPosPrinter;
begin
  ATexto := fpCmd.Params(0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    if (MonitorConfig.DFE.Impressao.NFCe.Emissao.Modelo <> 1 )  then
        raise Exception.Create('Comando disponível apenas para o DANFe modelo DANFe ESCPOS');

    DanfeEscPos := TACBrNFeDANFeESCPOS.Create(ACBrNFe.DANFE);
    POSPrinter  := TACBrPosPrinter.Create(ACBrNFe);
    try
      DanfeEscPos.PosPrinter:= POSPrinter;
      DanfeEscPos.PosPrinter.Device.Porta := MonitorConfig.PosPrinter.Porta;
      DanfeEscPos.PosPrinter.Modelo := TACBrPosPrinterModelo(MonitorConfig.PosPrinter.Modelo);
      ACBrNFe.DANFE := DanfeEscPos;

      MemoTXT := TStringList.Create;
      try
         MemoTXT.Clear ;
         MemoTXT.Text := ConvertStrRecived( ATexto );

         if not ( DanfeEscPos.PosPrinter.Device.Ativo ) then
            DanfeEscPos.PosPrinter.Device.Ativar;
         DanfeEscPos.ImprimirRelatorio(MemoTXT);
      finally
         MemoTXT.Free ;
      end ;

    finally
      DanfeEscPos.Free;
      POSPrinter.Free;
    end;
  end;

end;

{ TMetodoCriarNFeSEFAZ }

{ Params: 0 - TxtFile - Uma String com um Path completo arquivo .txt NFe
                         ou Uma String com conteúdo txt do NFe
          1 - RetornaXML: 1 para Retornar XML Gerado na Resposta
}
procedure TMetodoCriarNFeSEFAZ.Executar;
var
  Salva, ARetornaXML: boolean;
  Alertas: ansistring;
  ArqNFe: string;
  SL: TStringList;
  Resp, ATXT: string;

  NFeRTXT: TNFeRTXT;
  APathTXT: Boolean;
begin
  ATXT := fpCmd.Params(0);
  ARetornaXML := StrToBoolDef(fpCmd.Params(1), False);
  APathTXT := (copy(ATXT, 1, 10) <> 'NOTAFISCAL') and (copy(ATXT, 1, 11) <> 'NOTA FISCAL');

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    if APathTXT and not FileExists(ATXT) then
      raise Exception.Create('Arquivo ' + ATXT + ' não encontrado.');

    ACBrNFe.NotasFiscais.Clear;
    ACBrNFe.NotasFiscais.Add;
    NFeRTXT := TNFeRTXT.Create(ACBrNFe.NotasFiscais.Items[0].NFe);

    try
      if APathTXT then
        NFeRTXT.CarregarArquivo(ATXT)
      else
        NFeRTXT.ConteudoArquivo.Text := ATXT;

      if not NFeRTXT.LerTxt then
        raise Exception.Create('Arquivo inválido!');
    finally
      NFeRTXT.Free;
    end;

    Salva := ACBrNFe.Configuracoes.Geral.Salvar;
    if not Salva then
    begin
      ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
      ACBrNFe.Configuracoes.Arquivos.PathSalvar := PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
    end;

    ACBrNFe.NotasFiscais.GerarNFe;
    Alertas := ACBrNFe.NotasFiscais.Items[0].Alertas;

    ACBrNFe.NotasFiscais.Assinar;
    ACBrNFe.NotasFiscais.Validar;

    ArqNFe := PathWithDelim(ACBrNFe.Configuracoes.Arquivos.PathSalvar) +
      OnlyNumber(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID) + '-nfe.xml';
    ACBrNFe.NotasFiscais.GravarXML(ArqNFe);

    if not FileExists(ArqNFe) then
      raise Exception.Create('Não foi possível criar o arquivo ' + ArqNFe);

    Resp := ArqNFe;
    if (Alertas <> '') then
      Resp := Resp + sLineBreak + 'Alertas:' + Alertas;

    if ARetornaXML then
    begin
      SL := TStringList.Create;
      try
        SL.LoadFromFile(ArqNFe);
        Resp := Resp + sLineBreak + SL.Text;
      finally
        SL.Free;
      end;
    end;

    fpCmd.Resposta := Resp;

  end;
end;

{ TMetodoCriarEnviarNFeSEFAZ }

{ Params: 0 - TxtFile - Uma String com um Path completo arquivo .txt NFe
                         ou Uma String com conteúdo txt do NFe
          1 - NumeroLote: Integer com número do lote a ser adicionado
          2 - Imprime : 1 para imprimir
          3 - Sincrono: 1 para envio Sincrono (Default)
          4 - NomeImpressora: String com nome impressora para impressão (Default)
          5 - MostrarPreview: 1 para mostrar preview (Default)
          6 - Numero de Copias: Inteiro com número de cópias (Default)
}
procedure TMetodoCriarEnviarNFeSEFAZ.Executar;
var
  Salva, AImprime: boolean;
  Alertas: ansistring;
  ArqNFe: string;
  Resp, ATXT, AImpressora: string;
  ALote: Integer;
  NFeRTXT: TNFeRTXT;
  APathTXT: Boolean;
  ASincrono : Boolean;
  APreview : String;
  ACopias : Integer;
begin
  ATXT := fpCmd.Params(0);
  APathTXT := (copy(ATXT, 1, 10) <> 'NOTAFISCAL') and (copy(ATXT, 1, 11) <> 'NOTA FISCAL');
  ALote := StrToIntDef(fpCmd.Params(1), 0);
  AImprime := StrToBoolDef(fpCmd.Params(2), False);
  ASincrono := StrToBoolDef(fpCmd.Params(3), False);
  AImpressora := fpCmd.Params(4);
  APreview := fpCmd.Params(5);
  ACopias := StrToIntDef(fpCmd.Params(6), 0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    if APathTXT and not FileExists(ATXT) then
      raise Exception.Create('Arquivo ' + ATXT + ' não encontrado.');

    ACBrNFe.NotasFiscais.Clear;
    ACBrNFe.NotasFiscais.Add;
    NFeRTXT := TNFeRTXT.Create(ACBrNFe.NotasFiscais.Items[0].NFe);

    try
      if APathTXT then
        NFeRTXT.CarregarArquivo(ATXT)
      else
        NFeRTXT.ConteudoArquivo.Text := ATXT;

      if not NFeRTXT.LerTxt then
        raise Exception.Create('Arquivo inválido!');
    finally
      NFeRTXT.Free;
    end;

    Salva := ACBrNFe.Configuracoes.Geral.Salvar;
    if not Salva then
    begin
      ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
      ACBrNFe.Configuracoes.Arquivos.PathSalvar :=
        PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
    end;

    ACBrNFe.NotasFiscais.GerarNFe;
    Alertas := ACBrNFe.NotasFiscais.Items[0].Alertas;

    ACBrNFe.NotasFiscais.Assinar;
    ACBrNFe.NotasFiscais.Validar;

    ArqNFe := PathWithDelim(ACBrNFe.Configuracoes.Arquivos.PathSalvar) +
      OnlyNumber(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID) + '-nfe.xml';
    ACBrNFe.NotasFiscais.GravarXML(ArqNFe);

    if not FileExists(ArqNFe) then
      raise Exception.Create('Não foi possível criar o arquivo ' + ArqNFe);

    Resp := ArqNFe;
    if (Alertas <> '') then
      Resp := Resp + sLineBreak + 'Alertas:' + Alertas;

    fpCmd.Resposta := Resp;

    if (ALote = 0) then
      ACBrNFe.WebServices.Enviar.Lote := '1'
    else
      ACBrNFe.WebServices.Enviar.Lote := IntToStr(ALote);

    ACBrNFe.WebServices.Enviar.Sincrono := ASincrono;

    if ACBrNFe.NotasFiscais.Count > 0 then
      DoValidarIntegradorNFCe(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID);
    ACBrNFe.WebServices.Enviar.Executar;
    RespostaEnvio;

    if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
    begin
      ACBrNFe.WebServices.Retorno.Recibo := ACBrNFe.WebServices.Enviar.Recibo;
      ACBrNFe.WebServices.Retorno.Executar;

      RespostaRetorno;
      RespostaNotasFiscais(AImprime, AImpressora, APreview, ACopias, False);
    end
    else
    if AImprime then //Sincrono
      ImprimirNFe(AImpressora, APreview, ACopias, False);

  end;
end;

{ TMetodoAdicionarNFeSEFAZ }

{ Params: 0 - TxtFile - Uma String com um Path completo arquivo .txt NFe
                         ou Uma String com conteúdo txt do NFe
          1 - NumeroLote: String com número do lote a ser adicionado
}
procedure TMetodoAdicionarNFeSEFAZ.Executar;
var
  Alertas: ansistring;
  ArqNFe: string;
  Resp, ATXT, ANumeroLote: string;
  NFeRTXT: TNFeRTXT;
  APathTXT: Boolean;
begin
  ATXT := fpCmd.Params(0);
  APathTXT := (copy(ATXT, 1, 10) <> 'NOTAFISCAL') and (copy(ATXT, 1, 11) <> 'NOTA FISCAL');
  ANumeroLote := fpCmd.Params(1);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    if APathTXT and not FileExists(ATXT) then
      raise Exception.Create('Arquivo ' + ATXT + ' não encontrado.');

    ACBrNFe.NotasFiscais.Clear;
    ACBrNFe.NotasFiscais.Add;
    NFeRTXT := TNFeRTXT.Create(ACBrNFe.NotasFiscais.Items[0].NFe);

    try
      if APathTXT then
        NFeRTXT.CarregarArquivo(ATXT)
      else
        NFeRTXT.ConteudoArquivo.Text := ATXT;

      if not NFeRTXT.LerTxt then
        raise Exception.Create('Arquivo inválido!');
    finally
      NFeRTXT.Free;
    end;

    ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'Lotes' + PathDelim + 'Lote' + trim(ANumeroLote));

    ACBrNFe.NotasFiscais.GerarNFe;
    Alertas := ACBrNFe.NotasFiscais.Items[0].Alertas;

    ACBrNFe.NotasFiscais.Assinar;
    ACBrNFe.NotasFiscais.Validar;

    ArqNFe := PathWithDelim(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'Lotes' + PathDelim + 'Lote' + trim(ANumeroLote)) + OnlyNumber(
      ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID) + '-nfe.xml';
    ACBrNFe.NotasFiscais.GravarXML(ExtractFilePath(ArqNFe));

    if not FileExists(ArqNFe) then
      raise Exception.Create('Não foi possível criar o arquivo ' + ArqNFe);

    Resp := ArqNFe;
    if (Alertas <> '') then
      Resp := Resp + sLineBreak + 'Alertas:' + Alertas;

    fpCmd.Resposta := Resp;
  end;
end;

{ TMetodoDistribuicaoDFe }

{ Params: 0 - Código da UF do autor da consulta
          1 - CNPJ do autor da consulta
          2 - Numero do último NSU retornado na consulta anterior
          3 - Numero do NSU a ser consultado
          4 - Chave da NF-e que se deseja baixar o XML
}
procedure TMetodoDistribuicaoDFe.Executar;
var
  AUF: Integer;
  ACNPJ: String;
  AUltNSU: String;
  ANSU: String;
  AChave: String;
  I, J: Integer;
begin
  AUF := StrToIntDef(fpCmd.Params(0), 0);
  ACNPJ := fpCmd.Params(1);
  AUltNSU := fpCmd.Params(2);
  ANSU := fpCmd.Params(3);
  AChave := fpCmd.Params(4);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    if not ValidarCNPJ(ACNPJ) then
      raise Exception.Create('CNPJ ' + ACNPJ + ' inválido.');

    DoValidarIntegradorNFCe();
    ACBrNFe.DistribuicaoDFe(AUF, ACNPJ, AUltNSU, ANSU, AChave);

    RespostaDistribuicaoDFe;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].resNFe.chNFe) <> '') then
      begin
        RespostaItensDistribuicaoDFeResNFe(I,J);
        inc(J);
      end;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(AcbrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].resEvento.chNFe) <> '') then
      begin
        RespostaItensDistribuicaoDFeResEve(I,J);
        inc(J);
      end;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(AcbrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].procEvento.detEvento.versao) <> '' ) then
      begin
        RespostaItensDistribuicaoDFeProEve(I,J);
        inc(J);
      end;

    J := 1;
    for I := 0 to ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count - 1 do
      if ( Trim(AcbrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip[I].procEvento.RetinfEvento.Id) <> '' ) then
      begin
        RespostaItensDistribuicaoDFeInfEve(I,J);
        inc(J);
      end;

  end;
end;

{ TMetodoSetTipoImpressao }

{ Params: 0 - TipoImp: informar 1-Retrato 2-Paisagem
}
procedure TMetodoSetTipoImpressao.Executar;
var
  TipoImp: Integer;
  OK: Boolean;
begin
  TipoImp := StrToIntDef(fpCmd.Params(0), 1);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.DANFE.TipoDANFE:= StrToTpImp( OK, IntToStr(TipoImp) );
    with MonitorConfig.DFE.Impressao.Geral do
      DANFE := TipoImp - 1;
    MonitorConfig.SalvarArquivo;

  end;

end;

end.
