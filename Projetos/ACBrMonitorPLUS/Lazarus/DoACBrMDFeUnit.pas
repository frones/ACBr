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

unit DoACBrMDFeUnit;

interface

uses Classes, SysUtils, ACBrLibMDFeRespostas,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.Math,
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
  procedure RespostaManifesto(pImprimir: boolean; pImpressora: string; pPreview: String; pCopias: Integer; pPDF: Boolean;
    pEncerrado: Boolean);
  procedure RespostaPadrao;
  procedure RespostaEncerramento;
  procedure RespostaEnvio;
  procedure RespostaRetorno;
  procedure RespostaStatus;
  procedure RespostaConsulta;
  procedure RespostaCancelamento;
  procedure RespostaRecibo;
  procedure RespostaEvento;
  procedure RespostaDistribuicaoDFe;
  procedure ImprimirMDFe(pImpressora: String; pPreview: String; pCopias: Integer; pPDF: Boolean; pEncerrado: Boolean);

  property ACBrMDFe: TACBrMDFe read fACBrMDFe;
end;

{ TACBrCarregarMDFe }

TACBrCarregarMDFe = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;

public
  constructor Create(AACBrDFe: TACBrMDFe; AXMLorFile: String; ARetornaFalha: Boolean = True ); reintroduce;
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

{ TMetodoEnviaremailEvento}
TMetodoEnviaremailEvento = class(TACBrMetodo)
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

{ TMetodoEnviarEvento}
TMetodoEnviarEvento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDistribuicaoDFePorChaveMDFe}
TMetodoDistribuicaoDFePorChaveMDFe = class(TACBrMetodo)
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

{ TMetodoGetPathMDFe}
TMetodoGetPathMDFe = class(TACBrMetodo)
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


implementation

uses IniFiles, DateUtils, Forms, strutils,
  ACBrDFeConfiguracoes,
  ACBrLibConsReciDFe, ACBrLibDistribuicaoDFe,
  pcnConversao, pmdfeConversaoMDFe,
  pcnAuxiliar, pmdfeMDFeR, DoACBrUnit, pmdfeMDFe;

{ TMetodoGetPathEvento }

{ Params: 0 - Código do evento
          1 - CNPJ: String - CNPJ para geração do path
          2 - IE: String - IE para geração do path
          3 - Data: TDateTime - Data para geração do path
}
procedure TMetodoGetPathEvento.Executar;
var
  CodEvento: String;
  ACNPJ: String;
  AIE: String;
  AData: TDateTime;
  Ok: Boolean;
begin
  CodEvento := fpCmd.Params(0);
  ACNPJ:= fpCmd.Params(1);
  AIE:= fpCmd.Params(2);
  AData:= StrToDateTimeDef(fpCmd.Params(3),0);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrMDfe.Configuracoes.Arquivos.GetPathEvento(StrToTpEventoMDFe(ok ,CodEvento),ACNPJ, AIE, AData);
  end;
end;

{ TMetodoGetPathCan }

{ Params: 0 - CNPJ: String - CNPJ para geração do path
          1 - IE: String - IE para geração do path
          2 - Data: TDateTime - Data para geração do path
}
procedure TMetodoGetPathCan.Executar;
var
  ACNPJ: String;
  AIE: String;
  AData: TDateTime;
begin
  ACNPJ:= fpCmd.Params(0);
  AIE:= fpCmd.Params(1);
  AData:= StrToDateTimeDef(fpCmd.Params(2),0);
  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrMDFe.Configuracoes.Arquivos.GetPathEvento(teCancelamento, ACNPJ, AIE, AData);
  end;

end;

{ TMetodoGetPathMDFe }

{ Params: 0 - Data: TDateTime - Data para geração do path
          1 - CNPJ: String - CNPJ para geração do path
          2 - IE: String - IE para geração do path
}
procedure TMetodoGetPathMDFe.Executar;
var
  AData: TDateTime;
  ACNPJ: String;
  AIE: String;
begin
  AData:= StrToDateTimeDef(fpCmd.Params(0),0);
  ACNPJ:= fpCmd.Params(1);
  AIE:= fpCmd.Params(2);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrMDFe.Configuracoes.Arquivos.GetPathMDFe(AData, ACNPJ, AIE);
  end;
end;

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
  ListaDeMetodos.Add(CMetodoEnviarEvento);
  ListaDeMetodos.Add(CMetodoDistribuicaoDFeporChaveMDFe);
  ListaDeMetodos.Add(CMetodoDistribuicaoDFeporUltNSU);
  ListaDeMetodos.Add(CMetodoDistribuicaoDFeporNSU);
  ListaDeMetodos.Add(CMetodoGetPathMDFe);
  ListaDeMetodos.Add(CMetodoGetPathCan);
  ListaDeMetodos.Add(CMetodoGetPathEvento);
  ListaDeMetodos.Add(CMetodoEnviaremailEvento);

  ListaDeMetodos.Add(CMetodoSavetofile);
  ListaDeMetodos.Add(CMetodoLoadfromfile);
  ListaDeMetodos.Add(CMetodoLerini);
  ListaDeMetodos.Add(CMetodoSetcertificado);
  ListaDeMetodos.Add(CMetodoObterCertificados);
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
  AACBrUnit: TACBrObjetoACBr;
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
    31 : AMetodoClass := TMetodoEnviarEvento;
    32 : AMetodoClass := TMetodoDistribuicaoDFeporChaveMDFe;
    33 : AMetodoClass := TMetodoDistribuicaoDFeporUltNSU;
    34 : AMetodoClass := TMetodoDistribuicaoDFeporNSU;
    35 : AMetodoClass := TMetodoGetPathMDFe;
    36 : AMetodoClass := TMetodoGetPathCan;
    37 : AMetodoClass := TMetodoGetPathEvento;
    38 : AMetodoClass := TMetodoEnviaremailEvento;


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

procedure TACBrObjetoMDFe.RespostaEnvio;
var
  Resp: TEnvioResposta;
begin
  Resp := TEnvioResposta.Create(TpResp, codUTF8);
  try
    Resp.Processar(fACBrMDFe);

    with fACBrMDFe.WebServices.Enviar do
    begin
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
  Resp := TRetornoResposta.Create('MDFe', TpResp, codUTF8);
  try
    Resp.Processar(fACBrMDFe.WebServices.Retorno.MDFeRetorno,
                   fACBrMDFe.WebServices.Retorno.Recibo,
                   fACBrMDFe.WebServices.Retorno.Msg,
                   fACBrMDFe.WebServices.Retorno.Protocolo,
                   fACBrMDFe.WebServices.Retorno.ChaveMDFe);

    fpCmd.Resposta := fpCmd.Resposta + sLineBreak + fACBrMDFe.WebServices.Retorno.Msg + sLineBreak;
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaManifesto(pImprimir: boolean;
  pImpressora: string; pPreview: String; pCopias: Integer; pPDF: Boolean; pEncerrado: Boolean);
var
  I, J: integer;
  ArqPDF: String;
begin
  with fACBrMDFe do
  begin
    for I := 0 to WebServices.Retorno.MDFeRetorno.ProtDFe.Count - 1 do
    begin
      for J := 0 to Manifestos.Count - 1 do
      begin
        if ('MDFe' + WebServices.Retorno.MDFeRetorno.ProtDFe.Items[i].chDFe =
          Manifestos.Items[j].MDFe.infMDFe.Id) then
        begin
          //RespostaItensMDFe(J, I, True);

          fpCmd.Resposta :=  fpCmd.Resposta + sLineBreak +'[MDFe_Arq' + Trim(IntToStr(
                         fACBrMDFe.Manifestos.Items[J].MDFe.Ide.nMDF)) +']' + sLineBreak +
                         'Arquivo=' + fACBrMDFe.Manifestos.Items[J].NomeArq;

          if NaoEstaVazio(pImpressora) then
            DAMDFe.Impressora := pImpressora;

          if pCopias > 0 then
            DAMDFE.NumCopias := pCopias;

          if StrToBoolDef( pPreview, False ) then
            DAMDFE.MostraPreview:= True;

          DAMDFE.Encerrado:= pEncerrado;

          if (Manifestos.Items[I].Confirmado) and  (pPDF) then
          begin
            Manifestos.Items[I].ImprimirPDF;
            ArqPDF := OnlyNumber(ACBrMDFe.Manifestos.Items[I].MDFe.infMDFe.Id)+'-mdfe.pdf';

            fpCmd.Resposta :=  fpCmd.Resposta + sLineBreak +
              'PDF='+ PathWithDelim(ACBrMDFe.DAMDFE.PathPDF) + ArqPDF + sLineBreak;
          end;

          if (Manifestos.Items[I].Confirmado) and (pImprimir) then
          begin
            try
              DoAntesDeImprimir( ( StrToBoolDef( pPreview, False ) ) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ) );
              Manifestos.Items[I].Imprimir;
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

procedure TACBrObjetoMDFe.RespostaPadrao;
var
  Resp: TNaoEncerradosResposta ;
begin
  Resp := TNaoEncerradosResposta.Create(TpResp, codUTF8);
  try
    Resp.Processar(fACBrMDFe);
    with fACBrMDFe.WebServices.ConsMDFeNaoEnc do
    begin
      fpCmd.Resposta := Msg + sLineBreak;
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
  Resp := TStatusServicoResposta.Create(TpResp, codUTF8);
  try
    Resp.Processar(fACBrMDFe);
    fpCmd.Resposta := Resp.Msg + sLineBreak;
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaConsulta;
var
  Resp: TConsultaResposta;
begin
  Resp := TConsultaResposta.Create(TpResp, codUTF8);
  try
    Resp.Processar(fACBrMDFe);
    fpCmd.Resposta := Resp.Msg + sLineBreak;
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaCancelamento;
var
  Resp: TCancelamentoResposta;
begin
  Resp := TCancelamentoResposta.Create(TpResp, codUTF8);
  try
    Resp.Processar(fACBrMDFe);
    fpCmd.Resposta := Resp.XMotivo + sLineBreak;
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaEncerramento;
var
  Resp: TEncerramentoResposta;
begin
  Resp := TEncerramentoResposta.Create(TpResp, codUTF8);
  try
    Resp.Processar(fACBrMDFe);

    with fACBrMDFe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento do
    begin
      fpCmd.Resposta := XMotivo + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaRecibo;
var
  Resp: TReciboResposta;
begin
  Resp := TReciboResposta.Create('MDFe', TpResp, codUTF8);
  Resp.Processar(fACBrMDFe.WebServices.Recibo.MDFeRetorno, fACBrMDFe.WebServices.Recibo.Recibo);
  try
    with fACBrMDFe.WebServices.Recibo do
    begin
      //Pq tem esta chamada ?
      {
       if fACBrMDFe.WebServices.ConsMDFeNaoEnc.InfMDFe.Count > 0 then
       begin
         Resp.ChMDFe := fACBrMDFe.WebServices.ConsMDFeNaoEnc.InfMDFe.Items[0].chMDFe;
         Resp.NProt := fACBrMDFe.WebServices.ConsMDFeNaoEnc.InfMDFe.Items[0].nProt;
         Resp.MotivoMDFe := MDFeRetorno.ProtDFe.Items[0].xMotivo;
       end;
      }

      fpCmd.Resposta := Msg + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaEvento;
var
  Resp: TEventoResposta;
begin
  Resp := TEventoResposta.Create(TpResp, codUTF8);
  try
    Resp.Processar(fACBrMDFe);
    fpCmd.Resposta := sLineBreak + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.RespostaDistribuicaoDFe;
var
  Resp: TDistribuicaoDFeResposta;
begin
  Resp := TDistribuicaoDFeResposta.Create(TpResp, codUTF8);
  try
    Resp.Processar(fACBrMDFe.WebServices.DistribuicaoDFe.retDistDFeInt,
                   fACBrMDFe.WebServices.DistribuicaoDFe.Msg,
                   fACBrMDFe.WebServices.DistribuicaoDFe.NomeArq,
                   fACBrMDFe.WebServices.DistribuicaoDFe.ListaArqs);

    fpCmd.Resposta := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoMDFe.ImprimirMDFe(pImpressora: String; pPreview: String;
  pCopias: Integer; pPDF: Boolean; pEncerrado: Boolean);
var
  ArqPDF : String;
begin
  with ACBrMDFe do
  begin
    if (Manifestos.Items[0].Confirmado) then
    begin
      if NaoEstaVazio(pImpressora) then
        DAMDFE.Impressora := pImpressora;

      if pCopias > 0 then
        DAMDFE.NumCopias := pCopias;

      if StrToBoolDef( pPreview, False ) then
        DAMDFE.MostraPreview:= True;

      DAMDFE.Encerrado:= pEncerrado;

      if pPDF then
      begin
        Manifestos.Items[0].ImprimirPDF;
        ArqPDF := OnlyNumber(ACBrMDFe.Manifestos.Items[0].MDFe.infMDFe.ID)+'-mdfe.pdf';

        fpCmd.Resposta :=  fpCmd.Resposta + sLineBreak +
                'PDF='+ PathWithDelim(ACBrMDFe.DAMDFE.PathPDF) + ArqPDF + sLineBreak ;
      end;

      try
        DoAntesDeImprimir(( StrToBoolDef( pPreview, False) ) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
        Manifestos.Items[0].Imprimir;
      finally
        DoDepoisDeImprimir;
      end;

    end;

  end;
end;

{ TMetodoDistribuicaoDFePorChaveMDFe }

{ Params:
          0 - CNPJ do autor da consulta
          1 - Chave da MDF-e que se deseja baixar o XML
}
procedure TMetodoDistribuicaoDFePorChaveMDFe.Executar;
var
  ACNPJ: String;
  AChave: String;
begin
  ACNPJ := fpCmd.Params(0);
  AChave := fpCmd.Params(1);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    if not ValidarCNPJouCPF(ACNPJ) then
      raise Exception.Create('CNPJ/CPF '+ACNPJ+' inválido.');

    ACBrMDFe.DistribuicaoDFePorChaveMDFe(ACNPJ, AChave);
    RespostaDistribuicaoDFe;
  end;

end;

{ TMetodoDistribuicaoDFePorUltNSU }

{ Params:
          0 - CNPJ do autor da consulta
          1 - Numero do último NSU retornado na consulta anterior
}
procedure TMetodoDistribuicaoDFePorUltNSU.Executar;
var
  ACNPJ: String;
  AUltNSU: String;
begin
  ACNPJ := fpCmd.Params(0);
  AUltNSU := fpCmd.Params(1);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    if not ValidarCNPJouCPF(ACNPJ) then
      raise Exception.Create('CNPJ/CPF '+ACNPJ+' inválido.');

    ACBrMDFe.DistribuicaoDFePorUltNSU(ACNPJ, AUltNSU);

    RespostaDistribuicaoDFe;
  end;
end;

{ TMetodoDistribuicaoDFePorNSU }

{ Params:
          0 - CNPJ do autor da consulta
          1 - Numero do NSU a ser consultado
}
procedure TMetodoDistribuicaoDFePorNSU.Executar;
var
  ACNPJ: String;
  ANSU: String;
begin
  ACNPJ := fpCmd.Params(0);
  ANSU := fpCmd.Params(1);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    if not ValidarCNPJouCPF(ACNPJ) then
      raise Exception.Create('CNPJ/CPF '+ACNPJ+' inválido.');

    ACBrMDFe.DistribuicaoDFePorNSU(ACNPJ, ANSU);

    RespostaDistribuicaoDFe;
  end;

end;

function TACBrObjetoMDFe.GerarMDFeIni(XML: string): string;
var
  INIRec: TMemIniFile;
  IniMDFe: TStringList;
  LocMDFeR: TMDFeR;
begin
  INIRec := TMemIniFile.Create('MDFe.ini');

  try
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

    IniMDFe := TStringList.Create;
    try
      IniMDFe.Text := fACBrMDFe.Manifestos.GerarIni();
      INIRec.SetStrings(IniMDFe);
      Result := IniMDFe.Text;
    finally
      IniMDFe.Free;
    end;
  finally
    INIRec.Free;
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

constructor TACBrCarregarMDFe.Create(AACBrDFe: TACBrMDFe; AXMLorFile: String; ARetornaFalha: Boolean);
begin
  inherited Create(AACBrDFe, AXMLorFile, ARetornaFalha);
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
      FormaEmissaoMDFe := StrToInt(TpEmisToStr(FormaEmissao)) -1;

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
  NumAmbiente: Integer;
begin
  NumAmbiente := StrToIntDef(fpCmd.Params(0), 2);

  if (NumAmbiente < 1) or (NumAmbiente > 2) then
    raise Exception.Create('Ambiente Inválido: '+IntToStr(NumAmbiente));

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.WebService do
      Ambiente := NumAmbiente -1;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoConsultaCadastro }

{ Params: 0 - UF: String Sigla UF consulta
          1 - Documento: String Documento a ser consultado
          2 - IE: Boolean 1: para consulta por IF
}
procedure TMetodoConsultaCadastro.Executar;
//var
//  AUF, ADocumento: String;
//  AIE: Boolean;
begin
  //AUF := fpCmd.Params(0);
  //ADocumento := fpCmd.Params(1);
  //AIE := StrToBoolDef(fpCmd.Params(2), False);

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
//var
//  ACNPJ, AJustificativa, ASerie: String;
//  AAno, AModelo, ANumIninial, ANumFinal: Integer;
begin
  //ACNPJ := fpCmd.Params(0);
  //AJustificativa := fpCmd.Params(1);
  //AAno := StrToIntDef(fpCmd.Params(2), 0);
  //AModelo := StrToIntDef(fpCmd.Params(3), 0);
  //ASerie := fpCmd.Params(4);
  //ANumIninial := StrToIntDef(fpCmd.Params(5), 0);
  //ANumFinal := StrToIntDef(fpCmd.Params(6), 0);

  raise Exception.Create('Método: MDFe.InutilizarMDFe não implementado.');
end;

{ TMetodoEnviarEmail }

{ Params: 0 - Email: String com email Destinatário
          1 - XML: String com path do XML
          2 - Boolean 1 : Envia PDF
          3 - Assunto: String com Assunto do e-mail
          4 - Copia: String com e-mails copia (Separados ;)
          5 - Anexo: String com Path de Anexos (Separados ;)
          6 - Replay: String com endereços replay (Separados ;)
}
procedure TMetodoEnviarEmail.Executar;
var
  sAssunto, ADestinatario, APathXML, AAssunto, AEmailCopias, AAnexos, AReplay: string;
  slMensagemEmail, slCC, slAnexos, slReplay: TStringList;
  CargaDFe: TACBrCarregarMDFe;
  AEnviaPDF: Boolean;
begin
  ADestinatario := fpCmd.Params(0);
  APathXML := fpCmd.Params(1);
  AEnviaPDF := StrToBoolDef(fpCmd.Params(2), False);
  AAssunto := fpCmd.Params(3);
  AEmailCopias := fpCmd.Params(4);
  AAnexos := fpCmd.Params(5);
  AReplay := fpCmd.Params(6);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;

    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    slReplay := TStringList.Create;
    try
      CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, APathXML);
      try
        with MonitorConfig.DFE.Email do
        begin
          slMensagemEmail.Text := StringToBinaryString(MensagemMDFe);
          sAssunto := AssuntoMDFe;
        end;

        slCC.DelimitedText := sLineBreak;
        slCC.Text := StringReplace(AEmailCopias, ';', sLineBreak, [rfReplaceAll]);

        slAnexos.DelimitedText := sLineBreak;
        slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

        slReplay.DelimitedText := sLineBreak;
        slReplay.Text := StringReplace(AReplay, ';', sLineBreak, [rfReplaceAll]);

        try
          ACBrMDFe.Manifestos.Items[0].EnviarEmail(ADestinatario,
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

          if not(MonitorConfig.Email.SegundoPlano) then
            fpCmd.Resposta := 'E-mail enviado com sucesso!'
          else
            fpCmd.Resposta := 'Enviando e-mail em segundo plano...';

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

{ TMetodoEnviarEmailEvento }
{ Params: 0 - Email: String com email Destinatário
          1 - XML: String com path do XML Evento
          2 - XML: String com path do XML MDFe
          3 - Boolean 1 : Envia PDF
          4 - Assunto: String com Assunto do e-mail
          5 - Copia: String com e-mails copia (Separados ;)
          6 - Anexo: String com Path de Anexos (Separados ;)
          7 - Replay: String com Replay (Separados ;)
}
procedure TMetodoEnviarEmailEvento.Executar;
var
  sAssunto, ADestinatario, APathXMLEvento, APathXML, AAssunto, AEmailCopias,
  AAnexos, ArqPDF, ArqEvento, AReplay: string;
  slMensagemEmail, slCC, slAnexos, slReplay: TStringList;
  CargaDFeEvento: TACBrCarregarMDFeEvento;
  CargaDFe: TACBrCarregarMDFe;
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
  AReplay := fpCmd.Params(7);
  ArqEvento := '';

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.EventoMDFe.Evento.Clear;
    ACBrMDFe.Manifestos.Clear;

    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    slReplay := TStringList.Create;
    try
      CargaDFeEvento := TACBrCarregarMDFeEvento.Create(ACBrMDFe, APathXMLEvento);
      if NaoEstaVazio(APathXML) then
        CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, APathXML);
      try
  //      DoConfiguraDANFe(True, '');

        if AEnviaPDF then
        begin
          try
            ACBrMDFe.ImprimirEventoPDF;
            ArqPDF := ACBrMDFe.DAMDFE.ArquivoPDF;
            if not FileExists(ArqPDF) then
               raise Exception.Create('Arquivo ' + ArqPDF + ' não encontrado.');
          except
            on E: Exception do
              raise Exception.Create('Erro ao criar o arquivo PDF. ' + sLineBreak + E.Message);
          end;
        end;

        with MonitorConfig.DFE.Email do
        begin
          slMensagemEmail.Text := DoSubstituirVariaveis( StringToBinaryString(MensagemMDFe) );
          sAssunto := AssuntoMDFe;
        end;

        QuebrarLinha(AEmailCopias, slCC);
        QuebrarLinha(AAnexos, slAnexos);
        QuebrarLinha(AReplay, slReplay);

        // Se carregou evento usando XML como parâmetro, salva XML para poder anexar
        if  StringIsXML( APathXMLEvento ) then
        begin
          tipoEvento := ACBrMDFe.EventoMDFe.Evento[0].InfEvento.tpEvento;
          ArqEvento  := ACBrMDFe.EventoMDFe.ObterNomeArquivo(tipoEvento);
          ArqEvento  := PathWithDelim(ACBrMDFe.Configuracoes.Arquivos.GetPathEvento(tipoEvento))+ArqEvento;
          WriteToTxt(ArqEvento, ACBrMDFe.EventoMDFe.Evento[0].RetInfEvento.XML, False, False);
          slAnexos.Add(ArqEvento)
        end
        else
          slAnexos.Add(APathXMLEvento);

        if AEnviaPDF then
          slAnexos.Add(ArqPDF);

        try
          ACBrMDFe.EnviarEmail(ADestinatario,
            DoSubstituirVariaveis( IfThen(NaoEstaVazio(AAssunto), AAssunto, sAssunto) ),
            slMensagemEmail,
            slCC,      // Lista com emails que serão enviado cópias - TStrings
            slAnexos,  // Lista de slAnexos - TStrings
            Nil,
            '',
            slReplay); // Lista com Endereços Replay - TStrings

          if not(MonitorConfig.Email.SegundoPlano) then
            fpCmd.Resposta := 'E-mail enviado com sucesso!'
          else
            fpCmd.Resposta := 'Enviando e-mail em segundo plano...';

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
      slReplay.Free;
    end;
  end;
end;

{ TMetodoReciboMDFe }

{ Params: 0 - Recibo - String com Numero Recibo para consulta
}
procedure TMetodoReciboMDFe.Executar;
var
  ARecibo: String;
begin
  ARecibo := fpCmd.Params(0);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.WebServices.Recibo.Recibo := ARecibo;
    ACBrMDFe.WebServices.Recibo.Executar;

    RespostaRecibo;

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
    if not ValidarCNPJouCPF(ACNPJ) then
      raise Exception.Create('CNPJ/CPF ' + ACNPJ + ' invalido.');

    ACBrMDFe.WebServices.ConsultaMDFeNaoEnc(ACNPJ);
    RespostaPadrao;
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
    begin
      CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, AXML);
      try
        if (ACBrMDFe.Manifestos.Count > 0) then
          Chave := OnlyNumber(ACBrMDFe.Manifestos.Items[0].MDFe.infMDFe.ID)
        else
          raise Exception.Create('Arquivo MDFe inválido: ' + AXML);
      finally
        CargaDFe.Free;
      end;
    end
    else if ValidarChave(AXML) then
      Chave := AXML
    else
      raise Exception.Create('Chave ou arquivo MDFe inválido: '+ AXML);

    ACBrMDFe.EventoMDFe.Evento.Clear;
    with ACBrMDFe.EventoMDFe.Evento.New do
    begin
      infEvento.CNPJCPF := ACNPJ;
      if Trim(infEvento.CNPJCPF) = '' then
        infEvento.CNPJCPF := copy(chave, 7, 14)
      else
      begin
        if not ValidarCNPJouCPF(ACNPJ) then
          raise Exception.Create('CNPJ/CPF ' + ACNPJ + ' inválido.');
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
        ACBrMDFe.WebServices.Consulta.Executar;
        if (ACBrMDFe.WebServices.Consulta.protocolo <> '') then
          infEvento.detEvento.nProt := ACBrMDFe.WebServices.Consulta.Protocolo
        else
          raise Exception.Create('Falha na consulta do Protocolo MDFe. ' + ACBrMDFe.WebServices.Consulta.Msg);
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

    ACBrMDFe.EnviarEvento(1);
    RespostaEncerramento;

  end;
end;

{ TMetodoConsultarMDFe }

{ Params: 0 - XML - Uma String com um Path completo XML ou chave MDFe
          1 - AExtrairEventos (1 para extrair)
}
procedure TMetodoConsultarMDFe.Executar;
var
  CargaDFe: TACBrCarregarMDFe;
  AXML: String;
  AExtrairEventos: Boolean;
begin
  AXML := fpCmd.Params(0);
  AExtrairEventos := StrToBoolDef(fpCmd.Params(1), False);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;
    CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, AXML, False);
    try
      ACBrMDFe.WebServices.Consulta.ExtrairEventos := AExtrairEventos;

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

      ACBrMDFe.WebServices.Consulta.Executar;
      RespostaConsulta;

      if  FilesExists( AXML ) then
         fpCmd.Resposta :=  fpCmd.Resposta + sLineBreak + 'Arquivo=' + AXML;

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
        on E: Exception do
          raise Exception.Create('Erro ao criar o arquivo PDF.' + sLineBreak + E.Message);
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
          4 - Mostrar Preview (1 - para preview)
}
procedure TMetodoImprimirEvento.Executar;
var
  CargaDFe: TACBrCarregarMDFe;
  CargaDFeEvento: TACBrCarregarMDFeEvento;
  AXMLEvento, AXML, AImpressora: String;
  ACopias: Integer;
  APreview: String;
begin
  AXMLEvento := fpCmd.Params(0);
  AXML := fpCmd.Params(1);
  AImpressora := fpCmd.Params(2);
  ACopias := StrToIntDef(fpCmd.Params(3), 0);
  APreview := fpCmd.Params(4);

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

      if StrToBoolDef( APreview, False ) then
        ACBrMDFe.DAMDFE.MostraPreview := True;

      try
        DoAntesDeImprimir( ( StrToBoolDef( APreview, False ) ) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
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
          2 - AEncerrado: 1 para imprimir mensagem de Encerrado
}
procedure TMetodoImprimirDaMDFePDF.Executar;
var
  ArqPDF, AXML, AProtocolo: string;
  CargaDFe: TACBrCarregarMDFe;
  AEncerrado: Boolean;
begin
  AXML := fpCmd.Params(0);
  AProtocolo := fpCmd.Params(1);
  AEncerrado:= StrToBoolDef(fpCmd.Params(2), False);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.Manifestos.Clear;
    CargaDFe := TACBrCarregarMDFe.Create(ACBrMDFe, AXML);
    try
      if NaoEstaVazio(AProtocolo) then
        ACBrMDFe.DAMDFe.Protocolo := AProtocolo;

      ACBrMDFe.DAMDFE.Encerrado := AEncerrado;

      try
        ACBrMDFe.Manifestos.ImprimirPDF;
        ArqPDF := OnlyNumber(ACBrMDFe.Manifestos.Items[0].MDFe.infMDFe.ID) + '-mdfe.pdf';
        ArqPDF := PathWithDelim(ACBrMDFe.DAMDFe.PathPDF) + ArqPDF;
        fpCmd.Resposta := 'Arquivo criado em: ' + ArqPDF;
      except
        on E: Exception do
          raise Exception.Create('Erro ao criar o arquivo PDF.' + sLineBreak + E.Message);
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
          4 - Assincrono: Boolean
          5 - MostrarPreview: 1 para mostrar preview (Default)
          6 - Numero de Copias: Inteiro com número de cópias (Default)
          7 - ImprimirPDF: 1 para imprimir PDF (Default)
          8 - Encerrado: 1 para imprimir Mensagem Encerrado
}
procedure TMetodoCriarEnviarMDFe.Executar;
var
  Salva, AImprime: boolean;
  Alertas: ansistring;
  ArqMDFe: string;
  Resp, AIni, AImpressora: string;
  ALote: Integer;
  Assincrono: Boolean;
  APreview: String;
  ACopias: Integer;
  APDF: Boolean;
  AEncerrado: Boolean;
begin

  AIni := fpCmd.Params(0);
  ALote := StrToIntDef(fpCmd.Params(1), 0);
  AImprime := StrToBoolDef(fpCmd.Params(2), False);
  AImpressora := fpCmd.Params(3);
  Assincrono := StrToBoolDef( fpCmd.Params(4), True);
  APreview := fpCmd.Params(5);
  ACopias := StrToIntDef(fpCmd.Params(6), 0);
  APDF := StrToBoolDef(fpCmd.Params(7), False);
  AEncerrado := StrToBoolDef(fpCmd.Params(8), False);

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

    fpCmd.Resposta := Resp + sLineBreak;

    if (ALote = 0) then
      ACBrMDFe.WebServices.Enviar.Lote := '1'
    else
      ACBrMDFe.WebServices.Enviar.Lote := IntToStr(ALote);

    ACBrMDFe.WebServices.Enviar.Sincrono:= not(Assincrono);

    ACBrMDFe.WebServices.Enviar.Executar;
    RespostaEnvio;
    if (ACBrMDFe.WebServices.Enviar.Recibo <> '') then //Assincrono
    begin
      ACBrMDFe.WebServices.Retorno.Recibo := ACBrMDFe.WebServices.Enviar.Recibo;
      ACBrMDFe.WebServices.Retorno.Executar;

      RespostaRetorno;
      RespostaManifesto(AImprime, AImpressora, APreview, ACopias, APDF, AEncerrado);

    end
    else
    begin
      if AImprime then //Sincrono
        ImprimirMDFe(AImpressora, APreview, ACopias, APDF, AEncerrado);
    end;

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
          4 - Preview: 1 para Mostrar Preview
          5 - Copias: Inteiro com número de cópias para impressão
          6 - PDF: 1 para impressão em PDF
          7 - Encerrado: 1 para imprimir mensagem de Encerrado
}
procedure TMetodoEnviarLoteMDFe.Executar;
var
  RetFind: integer;
  SearchRec: TSearchRec;
  ALote, ALoteEnvio: Integer;
  AImprime: Boolean;
  AImpressora: String;
  APreview: String;
  ACopias: Integer;
  APDF: Boolean;
  AEncerrado: Boolean;
begin
  ALote := StrToIntDef(fpCmd.Params(0), 0);
  ALoteEnvio := StrToIntDef(fpCmd.Params(1), 0);
  AImprime := StrToBoolDef(fpCmd.Params(2), False);
  AImpressora := fpCmd.Params(3);
  APreview     := fpCmd.Params(4);
  ACopias      := StrToIntDef(fpCmd.Params(5), 0);
  APDF         := StrToBoolDef(fpCmd.Params(6), False);
  AEncerrado   := StrToBoolDef(fpCmd.Params(7), False);

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
    ACBrMDFe.WebServices.Enviar.Sincrono := False;

    ACBrMDFe.WebServices.Enviar.Executar;
    RespostaEnvio;

    ACBrMDFe.WebServices.Retorno.Recibo := ACBrMDFe.WebServices.Enviar.Recibo;
    ACBrMDFe.WebServices.Retorno.Executar;

    RespostaRetorno;
    RespostaManifesto(AImprime, AImpressora, APreview, ACopias, APDF, AEncerrado);
  end;
end;

{ TMetodoEnviarMDFe }

{ Params: 0 - PathorXML - Uma String com um Path completo arquivo XML MDFe
                         ou Uma String com conteúdo XML do MDFe
          1 - Lote: Integer com número do lote. Default = 1
          2 - Assina: 1 para assinar XML
          3 - Imprime: 1 Para True. Default 0
          4 - Nome Impressora: String com Nome da Impressora
          5 - Assincrono : Boolean
          6 - Encerrado: 1 para imprimir mensagem Encerrado
}
procedure TMetodoEnviarMDFe.Executar;
var
  CargaDFe: TACBrCarregarMDFe;
  APathorXML, AImpressora: String;
  ALote: Integer;
  AAssina, AImprime, Assincrono, AEncerrado : Boolean;
begin
  APathorXML := fpCmd.Params(0);
  ALote := StrToIntDef(fpCmd.Params(1), 0);
  AAssina := StrToBoolDef(fpCmd.Params(2), False);
  AImprime := StrToBoolDef(fpCmd.Params(3), False);
  AImpressora := fpCmd.Params(4);
  Assincrono := StrToBoolDef( fpCmd.Params(5), True);
  AEncerrado := StrToBoolDef( fpCmd.Params(6), False);

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

      ACBrMDFe.WebServices.Enviar.Sincrono:= not(Assincrono);

      ACBrMDFe.WebServices.Enviar.Executar;
      RespostaEnvio;
      if (ACBrMDFe.WebServices.Enviar.Recibo <> '') then //Assincrono
      begin
        ACBrMDFe.WebServices.Retorno.Recibo := ACBrMDFe.WebServices.Enviar.Recibo;
        ACBrMDFe.WebServices.Retorno.Executar;

        RespostaRetorno;
        RespostaManifesto(AImprime, AImpressora, '' , 0, False, AEncerrado);

      end
      else
      begin
        if AImprime then //Sincrono
          ImprimirMDFe(AImpressora, '', 0, False, AEncerrado);
      end;


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
          4 - Preview: 1 para Mostrar Preview
          5 - Encerrado: 1 para imiprimir mensagem Encerrado
}
procedure TMetodoImprimirDaMDFe.Executar;
var
  CargaDFe: TACBrCarregarMDFe;
  AChave, AImpressora, AProtocolo: String;
  ACopias: Integer;
  APreview: Boolean;
  AEncerrado: Boolean;
begin
  AChave := fpCmd.Params(0);
  AImpressora := fpCmd.Params(1);
  ACopias := StrToIntDef(fpCmd.Params(2), 0);
  AProtocolo := fpCmd.Params(3);
  APreview := StrToBoolDef(fpCmd.Params(4), False);
  AEncerrado := StrToBoolDef(fpCmd.Params(5), False);

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
        ACBrMDFe.DAMDFe.Protocolo := AProtocolo;

      if APreview then
        ACBrMDFe.DAMDFE.MostraPreview := True;

      ACBrMDFe.DAMDFE.Encerrado := AEncerrado;

      try
        DoAntesDeImprimir((APreview) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
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

    ACBrMDFe.WebServices.Consulta.Executar;
    if EstaVazio(ACBrMDFe.WebServices.Consulta.Protocolo) then
      raise Exception.Create(ACBrStr('Não foi possível consultar o número de Protocolo para a Chave: ') + AChave );

    ACBrMDFe.EventoMDFe.Evento.Clear;
    with ACBrMDFe.EventoMDFe.Evento.New do
    begin
      infEvento.CNPJCPF := ACNPJ;
      if Trim(infEvento.CNPJCPF) = '' then
        infEvento.CNPJCPF := copy(OnlyNumber(ACBrMDFe.WebServices.Consulta.MDFeChave), 7, 14)
      else
      begin
        if not ValidarCNPJouCPF(ACNPJ) then
          raise Exception.Create('CNPJ/CPF ' + ACNPJ + ' inválido.');
      end;

      infEvento.cOrgao := StrToIntDef(
        copy(OnlyNumber(ACBrMDFe.WebServices.Consulta.MDFeChave), 1, 2), 0);
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.chMDFe := ACBrMDFe.WebServices.Consulta.MDFeChave;
      infEvento.detEvento.nProt := ACBrMDFe.WebServices.Consulta.Protocolo;
      infEvento.detEvento.xJust := AJustificativa;
    end;
    ACBrMDFe.EnviarEvento(ALote);
    RespostaCancelamento;
  end;
end;

{ TMetodoEnviarEvento }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini Evento
                         ou Uma String com conteúdo txt do Evento
}
procedure TMetodoEnviarEvento.Executar;
var
  AArq: String;
begin
  AArq := fpCmd.Params(0);

  with TACBrObjetoMDFe(fpObjetoDono) do
  begin
    ACBrMDFe.EventoMDFe.Evento.Clear;

    ACBrMDFe.EventoMDFe.LerFromIni( AArq );

    ACBrMDFe.EnviarEvento(ACBrMDFe.EventoMDFe.idLote);

    RespostaEvento;
  end;
end;

end.
