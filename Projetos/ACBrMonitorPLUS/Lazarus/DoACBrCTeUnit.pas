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

unit DoACBrCTeUnit;

interface

uses
  Classes, SysUtils, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.Math,
  ACBrLibCTeRespostas, ACBrCTe, ACBrLibResposta, ACBrMonitorConfig,
  ACBrMonitorConsts, ACBrDFeUtil, UtilUnit, DoACBrDFeUnit,
  CmdUnit, ACBrLibConsReciDFe, ACBrLibDistribuicaoDFe,
  ACBrLibConsultaCadastro;

const
  cHOM_MSG = 'NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';

type

{ TACBrObjetoCTe }

TACBrObjetoCTe = class(TACBrObjetoDFe)
private
  fACBrCTe: TACBrCTe;
public
  constructor Create(AConfig: TMonitorConfig; ACBrCTe: TACBrCTe); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  function GerarCTeIni(XML: string): string;
  procedure RespostaConhecimentos(pImprimir: Boolean; pImpressora: String;
            pPreview: Boolean; pCopias: Integer; pPDF: Boolean);
  Procedure LerIniCTe(ArqINI: String);
  procedure ImprimirCTe(pImpressora: String; pPreview: String; pCopias: Integer; pPDF: Boolean);

  property ACBrCTe: TACBrCTe read fACBrCTe;
end;

{ TACBrCarregarCTe }

TACBrCarregarCTe = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;
public
  constructor Create(AACBrDFe: TACBrCTe; AXMLorFile: String; ARetornaFalha: Boolean = True ); reintroduce;
end;

{ TACBrCarregarCTeEvento }

TACBrCarregarCTeEvento = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;
public
  constructor Create(AACBrDFe: TACBrCTe; AXMLorFile: String ); reintroduce;
end;

{ TACBrCarregarCTeInut }

TACBrCarregarCTeInut = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;
public
  constructor Create(AACBrDFe: TACBrCTe; AXMLorFile: String ); reintroduce;
end;

{ TMetodoCancelarCTe }

TMetodoCancelarCTe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirDaCTe }

TMetodoImprimirDaCTe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarCTe }

TMetodoCriarCTe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAdicionarCTe }

TMetodoAdicionarCTe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarCTe }
TMetodoEnviarCTe = class(TACBrMetodo)
private
  FAssincrono: Boolean;
  FImpressora: String;
  procedure TratarRetorno(const ACTe: TACBrCTe; const ACTeObject: TACBrObjetoCTe);
  procedure Imprimir(const ACTe: TACBrCTe; const ACTeObject: TACBrObjetoCTe);
public
  procedure Executar; override;
end;

{ TMetodoEnviarLoteCTe }
TMetodoEnviarLoteCTe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarEnviarCTe }
TMetodoCriarEnviarCTe = class(TACBrMetodo)
private
  FImpressora : String;
  FPreview    : Boolean;
  FCopias     : Integer;
  FAssincrono : Boolean;
  procedure TratarRetorno(const ACTe: TACBrCTe; const ACTeObject: TACBrObjetoCTe);
  procedure Imprimir(const ACTe: TACBrCTe; const ACTeObject: TACBrObjetoCTe);
  procedure GerarPDF(const ACTe: TACBrCTe; const ACTeObject: TACBrObjetoCTe);
public
  procedure Executar; override;
end;

{ TMetodoImprimirDaCTePDF }
TMetodoImprimirDaCTePDF = class(TACBrMetodo)
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

{ TMetodoValidarCTe}
TMetodoValidarCTe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAssinarCTe}
TMetodoAssinarCTe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarCTe}
TMetodoConsultarCTe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoReciboCTe}
TMetodoReciboCTe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarEmail}
TMetodoEnviarEmail = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoInutilizarCTe}
TMetodoInutilizarCTe = class(TACBrMetodo)
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

{ TMetodoLerCTe}
TMetodoLerCTe = class(TACBrMetodo)
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
  FnCTe: integer;
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

{ TMetodoGetPathCTe}
TMetodoGetPathCTe = class(TACBrMetodo)
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

{ TMetodoDistribuicaoDFePorChaveCTe}
TMetodoDistribuicaoDFePorChaveCTe = class(TACBrMetodo)
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

{ TMetodoSetTipoImpressao}
TMetodoSetTipoImpressao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDistribuicaoDFe}
TMetodoDistribuicaoDFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

uses
  IniFiles, DateUtils, Forms, strutils,
  ACBrDFeConfiguracoes,
  pcnConversao, pcteConversaoCTe,
  pcnAuxiliar, pcteCTeR, DoACBrUnit, pcteCTe;

{ TACBrObjetoCTe }

constructor TACBrObjetoCTe.Create(AConfig: TMonitorConfig; ACBrCTe: TACBrCTe);
begin
  inherited Create(AConfig);

  fACBrCTe := ACBrCTe;

  ListaDeMetodos.Add(CMetodostatusservico);
  ListaDeMetodos.Add(CMetodoValidarCTe);
  ListaDeMetodos.Add(CMetodoAssinarCTe);
  ListaDeMetodos.Add(CMetodoConsultarCTe);
  ListaDeMetodos.Add(CMetodoCancelarCTe);
  ListaDeMetodos.Add(CMetodoImprimirDACTe);
  ListaDeMetodos.Add(CMetodoImprimirDACTePDF);
  ListaDeMetodos.Add(CMetodoImprimirEvento);
  ListaDeMetodos.Add(CMetodoImprimirEventoPDF);
  ListaDeMetodos.Add(CMetodoEnviarCTe);
  ListaDeMetodos.Add(CMetodoCriarCTe);
  ListaDeMetodos.Add(CMetodoCriarEnviarCTe);
  ListaDeMetodos.Add(CMetodoAdicionarCTe);
  ListaDeMetodos.Add(CMetodoEnviarloteCTe);
  ListaDeMetodos.Add(CMetodoReciboCTe);
  ListaDeMetodos.Add(CMetodoInutilizarCTe);
  ListaDeMetodos.Add(CMetodoConsultaCadastro);
  ListaDeMetodos.Add(CMetodoEnviarEmail);
  ListaDeMetodos.Add(CMetodoSetAmbiente);
  ListaDeMetodos.Add(CMetodoSetLogoMarca);
  ListaDeMetodos.Add(CMetodoSetFormaEmissao);
  ListaDeMetodos.Add(CMetodoSetVersaoDF);
  ListaDeMetodos.Add(CMetodoLerCTe);
  ListaDeMetodos.Add(CMetodoFileexist);
  ListaDeMetodos.Add(CMetodoCertificadodatavencimento);
  ListaDeMetodos.Add(CMetodoGerarchave);
  ListaDeMetodos.Add(CMetodoVersao);
  ListaDeMetodos.Add(CMetodoSetModeloDF);
  ListaDeMetodos.Add(CMetodoCNPJCertificado);
  ListaDeMetodos.Add(CMetodoImprimirInutilizacao);
  ListaDeMetodos.Add(CMetodoImprimirInutilizacaoPDF);
  ListaDeMetodos.Add(CMetodoGetPathCTe);
  ListaDeMetodos.Add(CMetodoGetPathCCe);
  ListaDeMetodos.Add(CMetodoGetPathCan);
  ListaDeMetodos.Add(CMetodoGetPathEvento);
  ListaDeMetodos.Add(CMetodoGetPathInu);
  ListaDeMetodos.Add(CMetodoGerarINICTe);
  ListaDeMetodos.Add(CMetodoEnviarEvento);
  ListaDeMetodos.Add(CMetodoCartaCorrecao);
  ListaDeMetodos.Add(CMetodoXMLEnviarEvento);
  ListaDeMetodos.Add(CMetodoDistribuicaoDFeporChaveCTe);
  ListaDeMetodos.Add(CMetodoDistribuicaoDFeporNSU);
  ListaDeMetodos.Add(CMetodoDistribuicaoDFeporUltNSU);
  ListaDeMetodos.Add(CMetodoEnviarEmailEvento);
  ListaDeMetodos.Add(CMetodoEnviarEmailInutilizacao);
  ListaDeMetodos.Add(CMetodoValidarRegrasNegocios);
  ListaDeMetodos.Add(CMetodoDataVencimentoCertificado);
  ListaDeMetodos.Add(CMetodoSetTipoImpressao);
  ListaDeMetodos.Add(CMetodoDistribuicaoDFe);

  // DoACBr
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

procedure TACBrObjetoCTe.Executar(ACmd: TACBrCmd);
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
    1  : AMetodoClass := TMetodoValidarCTe;
    2  : AMetodoClass := TMetodoAssinarCTe;
    3  : AMetodoClass := TMetodoConsultarCTe;
    4  : AMetodoClass := TMetodoCancelarCTe;
    5  : AMetodoClass := TMetodoImprimirDaCTe;
    6  : AMetodoClass := TMetodoImprimirDaCTePDF;
    7  : AMetodoClass := TMetodoImprimirEvento;
    8  : AMetodoClass := TMetodoImprimirEventoPDF;
    9  : AMetodoClass := TMetodoEnviarCTe;
    10 : AMetodoClass := TMetodoCriarCTe;
    11 : AMetodoClass := TMetodoCriarEnviarCTe;
    12 : AMetodoClass := TMetodoAdicionarCTe;
    13 : AMetodoClass := TMetodoEnviarLoteCTe;
    14 : AMetodoClass := TMetodoReciboCTe;
    15 : AMetodoClass := TMetodoInutilizarCTe;
    16 : AMetodoClass := TMetodoConsultaCadastro;
    17 : AMetodoClass := TMetodoEnviarEmail;
    18 : AMetodoClass := TMetodoSetAmbiente;
    19 : AMetodoClass := TMetodoSetLogoMarca;
    20 : AMetodoClass := TMetodoSetformaEmissao;
    21 : AMetodoClass := TMetodoSetVersaoDF;
    22 : AMetodoClass := TMetodoLerCTe;
    23 : AMetodoClass := TMetodoFileExists;
    24 : AMetodoClass := TMetodoCertificadoDataVencimento;
    25 : AMetodoClass := TMetodoGeraChave;
    26 : AMetodoClass := TMetodoVersao;
    27 : AMetodoClass := TMetodoSetModeloDF;
    28 : AMetodoClass := TMetodoCNPJCertificado;
    29 : AMetodoClass := TMetodoImprimirInutilizacao;
    30 : AMetodoClass := TMetodoImprimirInutilizacaoPDF;
    31 : AMetodoClass := TMetodoGetPathCTe;
    32 : AMetodoClass := TMetodoGetPathCCe;
    33 : AMetodoClass := TMetodoGetPathCan;
    34 : AMetodoClass := TMetodoGetPathEvento;
    35 : AMetodoClass := TMetodoGetPathInu;
    36 : AMetodoClass := TMetodoLerCTe; // GerarIniCTe
    37 : AMetodoClass := TMetodoEnviarEvento;
    38 : AMetodoClass := TMetodoCartaCorrecao;
    39 : AMetodoClass := TMetodoXMLEnviarEvento;
    40 : AMetodoClass := TMetodoDistribuicaoDFeporChaveCTe;
    41 : AMetodoClass := TMetodoDistribuicaoDFeporNSU;
    42 : AMetodoClass := TMetodoDistribuicaoDFeporUltNSU;
    43 : AMetodoClass := TMetodoEnviaremailEvento;
    44 : AMetodoClass := TMetodoEnviaremailInutilizacao;
    45 : AMetodoClass := TMetodoValidarRegrasNegocios;
    46 : AMetodoClass := TMetodoCertificadoDataVencimento; // DataVencimentoCertificado
    47 : AMetodoClass := TMetodoSetTipoImpressao;
    48 : AMetodoClass := TMetodoDistribuicaoDFe;

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

procedure TACBrObjetoCTe.RespostaConhecimentos(pImprimir: Boolean; pImpressora: String;
          pPreview: Boolean; pCopias: Integer; pPDF: Boolean);
var
  I, J: integer;
  ArqPDF: String;
begin
  with fACBrCTe do
  begin
    for I := 0 to WebServices.Retorno.CTeRetorno.ProtDFe.Count - 1 do
    begin
      for J := 0 to Conhecimentos.Count - 1 do
      begin
        if ('CTe' + WebServices.Retorno.CTeRetorno.ProtDFe.Items[i].chDFe =
          Conhecimentos.Items[j].CTe.infCTe.Id) then
        begin
          //Informa Mensagem de Sem Valor Fiscal para documentos emitidos em Homologação
          if Conhecimentos.Items[J].CTe.Ide.tpAmb = taHomologacao then
            Conhecimentos.Items[J].CTe.dest.xNome:= cHOM_MSG;

          //RespostaItensCTe(J, I, True);
          fpCmd.Resposta :=  fpCmd.Resposta + sLineBreak +'[CTe_Arq' + Trim(IntToStr(
                         fACBrCTe.Conhecimentos.Items[J].CTe.ide.nCT)) +']' + sLineBreak +
                         'Arquivo=' + fACBrCTe.Conhecimentos.Items[J].NomeArq;

          DoConfiguraDACTe(False, BoolToStr(pPreview,'1',''));

          if NaoEstaVazio(pImpressora) then
            DACTe.Impressora := pImpressora;

          if pCopias > 0 then
            DACTE.NumCopias := pCopias;

          if (Conhecimentos.Items[I].Confirmado) and  (pPDF) then
          begin
            Conhecimentos.Items[I].ImprimirPDF;
            ArqPDF := OnlyNumber(ACBrCTe.Conhecimentos.Items[I].CTe.infCTe.ID)+'-cte.pdf';

            fpCmd.Resposta :=  fpCmd.Resposta + sLineBreak +
              'PDF='+ PathWithDelim(ACBrCTe.DACTE.PathPDF) + ArqPDF + sLineBreak;
          end;

          if (Conhecimentos.Items[I].Confirmado) and (pImprimir) then
          begin
            try
              DoAntesDeImprimir((pPreview) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
              Conhecimentos.Items[I].Imprimir;
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

procedure TACBrObjetoCTe.ImprimirCTe(pImpressora: String; pPreview: String;
  pCopias: Integer; pPDF: Boolean);
var
  ArqPDF : String;
begin
  with fACBrCTe do
  begin
    if (Conhecimentos.Items[0].Confirmado) then
    begin
      //Informa Mensagem de Sem Valor Fiscal para documentos emitidos em Homologação
      if Conhecimentos.Items[0].CTe.Ide.tpAmb = taHomologacao then
        Conhecimentos.Items[0].CTe.dest.xNome:= cHOM_MSG;

      DoConfiguraDACTe(pPDF, Trim(pPreview) );
      if NaoEstaVazio(pImpressora) then
        DACTE.Impressora := pImpressora;

      if pCopias > 0 then
        DACTE.NumCopias := pCopias;

      if pPDF then
      begin
        Conhecimentos.Items[0].ImprimirPDF;
        ArqPDF := OnlyNumber(ACBrCTe.Conhecimentos.Items[0].CTe.infCTe.ID)+'-cte.pdf';

        fpCmd.Resposta :=  fpCmd.Resposta + sLineBreak +
                'PDF='+ PathWithDelim(ACBrCTe.DACTE.PathPDF) + ArqPDF + sLineBreak ;
      end;

      try
        DoAntesDeImprimir(( StrToBoolDef( pPreview, False) ) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
        Conhecimentos.Items[0].Imprimir;
      finally
        DoDepoisDeImprimir;
      end;

    end;

  end;
end;

procedure TACBrObjetoCTe.LerIniCTe(ArqINI: String);
begin
  with fACBrCTe do
  begin
    Conhecimentos.Clear;
    Conhecimentos.LoadFromIni(ArqINI);
  end;
end;

function TACBrObjetoCTe.GerarCTeIni(XML: string): string;
var
  INIRec: TMemIniFile;
  IniCTe: TStringList;
  LocCTeR: TCTeR;
begin
  INIRec := TMemIniFile.Create('CTe.ini');

  fACBrCTe.Conhecimentos.Clear;
  if FilesExists(XML) then
    fACBrCTe.Conhecimentos.LoadFromFile(XML)
  else
  begin
    LocCTeR := TCTeR.Create(fACBrCTe.Conhecimentos.Add.CTe);
    try
      LocCTeR.Leitor.Arquivo := ConvertStrRecived(XML);
      LocCTeR.LerXml;
      fACBrCTe.Conhecimentos.Items[0].XML := LocCTeR.Leitor.Arquivo;
      fACBrCTe.Conhecimentos.GerarCTe;
    finally
      LocCTeR.Free;
    end;
  end;

  IniCTe := TStringList.Create;
  IniCTe.Text := fACBrCTe.Conhecimentos.GerarIni();
  INIRec.SetStrings(IniCTe);
  INIRec.Free;
  Result := IniCTe.Text;
  IniCTe.Free;
end;

{ TACBrCarregarCTe }

procedure TACBrCarregarCTe.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrCTe(FpACBrDFe).Conhecimentos.LoadFromFile( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroCTeAbrir, [AValue]) ) );
end;

procedure TACBrCarregarCTe.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrCTe(FpACBrDFe).Conhecimentos.LoadFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroCTeCarregar) );
end;

function TACBrCarregarCTe.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrCTe(FpACBrDFe).Conhecimentos.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrCTe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrCTe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlCTe ;
end;

constructor TACBrCarregarCTe.Create(AACBrDFe: TACBrCTe; AXMLorFile: String; ARetornaFalha: Boolean = True);
begin
  inherited Create(AACBrDFe, AXMLorFile, ARetornaFalha);
end;

{ TACBrCarregarCTeEvento }

procedure TACBrCarregarCTeEvento.CarregarDFePath( const AValue: String);
begin
  if not ( TACBrCTe(FpACBrDFe).EventoCTe.LerXML( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroCTeAbrir, [AValue]) ) );
end;

procedure TACBrCarregarCTeEvento.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrCTe(FpACBrDFe).EventoCTe.LerXMLFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroCTeCarregar) );
end;

function TACBrCarregarCTeEvento.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  if ( TACBrCTe(FpACBrDFe).EventoCTe.Evento.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrCTe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrCTe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlCTeEve ;
end;

constructor TACBrCarregarCTeEvento.Create(AACBrDFe: TACBrCTe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

{ TACBrCarregarCTeInut }

procedure TACBrCarregarCTeInut.CarregarDFePath(const AValue: String);
begin
  if not ( TACBrCTe(FpACBrDFe).InutCTe.LerXML( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroCTeAbrir, [AValue]) ) );
end;

procedure TACBrCarregarCTeInut.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrCTe(FpACBrDFe).InutCTe.LerXMLFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroCTeCarregar) );
end;

function TACBrCarregarCTeInut.ValidarDFe(const AValue: String): Boolean;
begin
  Result := False;
  if ( TACBrCTe(FpACBrDFe).InutCTe.ID <> '' ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrCTe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrCTe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlCTeInu;
end;

constructor TACBrCarregarCTeInut.Create(AACBrDFe: TACBrCTe; AXMLorFile: String);
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
          4 - Numero CTe
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
  FnCTe := StrToInt(fpCmd.Params(4));
  FTpEmissao := StrToInt(fpCmd.Params(5));
  FEmissao := StrToDateTime(fpCmd.Params(6));
  FCNPJ := fpCmd.Params(7);

  fpCmd.Resposta := GerarChaveAcesso(FCodUF,
    FEmissao, FCNPJ, FSerie,
    FnCTe, FTpEmissao,
    FCodNumerico, FModelo);
end;

{ TMetodoCertificadoDataVencimento }

procedure TMetodoCertificadoDataVencimento.Executar;
begin
  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    fpCmd.Resposta := DateToStr(ACBrCTe.SSL.CertDataVenc);
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

{ TMetodoLerCTe }

{ Params: 0 - XML - String com path XML
}
procedure TMetodoLerCTe.Executar;
var
  AXML: String;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    try
      fpCmd.Resposta := GerarCTeIni(AXML);
    except
      on E: Exception do
        raise Exception.Create('Erro ao gerar INI da CTe.' + sLineBreak + E.Message);
    end;
  end;
end;

{ TMetodoSetVersaoDF }

{ Params: 0 - Inteiro com numero da Versao CTe valores: 2.00 - 3.00 - 4.00
}
procedure TMetodoSetVersaoDF.Executar;
var
  OK: boolean;
  VersaoDFe: TVersaoCTe;
  AVersao: String;
begin
  AVersao := fpCmd.Params(0);
  VersaoDFe := StrToVersaoCTe(OK, AVersao);

  if not OK then
    raise Exception.Create('Versão Inválida.');

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.WebService do
      VersaoCTe := VersaoCTeToStr(VersaoDFe);

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

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    OK := False;
    FormaEmissao := StrToTpEmis(OK, IntToStr(NFormaEmissao));

    if not OK then
      raise Exception.Create('Forma de Emissão Inválida: ' + TpEmisToStr(FormaEmissao));

    with MonitorConfig.DFE.WebService do
      FormaEmissaoCTe := StrToInt(TpEmisToStr(FormaEmissao)) - 1;

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

  with TACBrObjetoCTe(fpObjetoDono) do
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

  with TACBrObjetoCTe(fpObjetoDono) do
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
  Resp: TConsultaCadastroResposta;
begin
  AUF := fpCmd.Params(0);
  ADocumento := fpCmd.Params(1);
  AIE := StrToBoolDef(fpCmd.Params(2), False);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.WebServices.ConsultaCadastro.UF   := AUF;
    if AIE then
      ACBrCTe.WebServices.ConsultaCadastro.IE := ADocumento
    else
    begin
      if Length(ADocumento) > 11 then
        ACBrCTe.WebServices.ConsultaCadastro.CNPJ := ADocumento
      else
        ACBrCTe.WebServices.ConsultaCadastro.CPF := ADocumento;
    end;

    ACBrCTe.WebServices.ConsultaCadastro.Executar;
    Resp := TConsultaCadastroResposta.Create(TpResp, codUTF8);
    try
      Resp.Processar(ACBrCTe.WebServices.ConsultaCadastro.RetConsCad);
      fpCmd.Resposta:= Resp.Msg + sLineBreak + Resp.Gerar ;

    finally
      Resp.Free;
    end;

  end;
end;

{ TMetodoInutilizarCTe }

{ Params: 0 - CNPJ: String com CNPJ
          1 - Justificativa: String
          2 - Ano: Integer com Ano Documento
          3 - Modelo: Integer com Modelo Documento
          4 - Serie: String com a série documento
          5 - NumIinial: Integer Nº inicial inutilização
          6 - NumFinal: Integer Nº final inutilização
}
procedure TMetodoInutilizarCTe.Executar;
var
  ACNPJ, AJustificativa: String;
  ASerie, AAno, AModelo, ANumInicial, ANumFinal: Integer;
  Resposta: TInutilizarCTeResposta;
begin
  ACNPJ := fpCmd.Params(0);
  AJustificativa := fpCmd.Params(1);
  AAno := StrToIntDef(fpCmd.Params(2), 0);
  AModelo := StrToIntDef(fpCmd.Params(3), 0);
  ASerie := StrToIntDef(fpCmd.Params(4), 0);
  ANumInicial := StrToIntDef(fpCmd.Params(5), 0);
  ANumFinal := StrToIntDef(fpCmd.Params(6), 0);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.WebServices.Inutiliza(ACNPJ, AJustificativa, AAno, AModelo, ASerie, ANumInicial, ANumFinal);
    Resposta := TInutilizarCTeResposta.Create(TpResp, codUTF8);
    try
      Resposta.Processar(ACBrCTe);
      fpCmd.Resposta:= Resposta.Msg + sLineBreak + Resposta.Gerar ;

    finally
      Resposta.Free;
    end;

  end;
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
  CargaDFe: TACBrCarregarCTe;
  AEnviaPDF: Boolean;
begin
  ADestinatario := fpCmd.Params(0);
  APathXML := fpCmd.Params(1);
  AEnviaPDF := StrToBoolDef(fpCmd.Params(2), False);
  AAssunto := fpCmd.Params(3);
  AEmailCopias := fpCmd.Params(4);
  AAnexos := fpCmd.Params(5);
  AReplay := fpCmd.Params(6);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.Conhecimentos.Clear;

    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    slReplay := TStringList.Create;
    try
      CargaDFe := TACBrCarregarCTe.Create(ACBrCTe, APathXML);
      try
        DoConfiguraDACTe(True, '');

        with MonitorConfig.DFE.Email do
        begin
          slMensagemEmail.Text := StringToBinaryString(MensagemCTe);
          sAssunto := AssuntoCTe;
        end;

        slCC.DelimitedText := sLineBreak;
        slCC.Text := StringReplace(AEmailCopias, ';', sLineBreak, [rfReplaceAll]);

        slAnexos.DelimitedText := sLineBreak;
        slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

        slReplay.DelimitedText := sLineBreak;
        slReplay.Text := StringReplace(AReplay, ';', sLineBreak, [rfReplaceAll]);

        try
          ACBrCTe.Conhecimentos.Items[0].EnviarEmail(ADestinatario,
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

{ TMetodoReciboCTe }

{ Params: 0 - Recibo - String com Numero Recibo para consulta
}
procedure TMetodoReciboCTe.Executar;
var
  ARecibo: String;
  RespRetorno: TRetornoResposta;
begin
  ARecibo := fpCmd.Params(0);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.WebServices.Recibo.Recibo := ARecibo;
    ACBrCTe.WebServices.Recibo.Executar;

    RespRetorno := TRetornoResposta.Create('CTe', TpResp, codUTF8);
    try
      RespRetorno.Processar(ACBrCTe.WebServices.Retorno.CTeRetorno,
                            ACBrCTe.WebServices.Retorno.Recibo,
                            ACBrCTe.WebServices.Retorno.Msg,
                            ACBrCTe.WebServices.Retorno.Protocolo,
                            ACBrCTe.WebServices.Retorno.ChaveCTe);
      fpCmd.Resposta := RespRetorno.Msg + sLineBreak + RespRetorno.Gerar;

      if ACBrCTe.Configuracoes.Geral.Salvar then
        fpCmd.Resposta := 'Arquivo=' + ACBrCTe.Configuracoes.Arquivos.PathSalvar +
                        ARecibo + '-pro-rec.xml';
    finally
      RespRetorno.Free;
    end;

  end;
end;

{ TMetodoConsultarCTe }

{ Params: 0 - XML - Uma String com um Path completo XML ou chave CTe
          1 - AExtrairEventos (1 para extrair)
}
procedure TMetodoConsultarCTe.Executar;
var
  CargaDFe: TACBrCarregarCTe;
  AXML: String;
  Resposta: TConsultaCTeResposta;
  AExtrairEventos: Boolean;
begin
  AXML := fpCmd.Params(0);
  AExtrairEventos := StrToBoolDef(fpCmd.Params(1), False);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.Conhecimentos.Clear;
    CargaDFe := TACBrCarregarCTe.Create(ACBrCTe, AXML, False);
    try
      ACBrCTe.WebServices.Consulta.ExtrairEventos := AExtrairEventos;

      if (ACBrCTe.Conhecimentos.Count = 0) then
      begin
        if ACBrDFeUtil.ValidarChave(AXML) then
          ACBrCTe.WebServices.Consulta.CTeChave := AXML
        else
          raise Exception.Create(
            'Parâmetro inválido. Chave do CTe inválida ou arquivo não encontrado.');
      end
      else
        ACBrCTe.WebServices.Consulta.CTeChave :=
          OnlyNumber(ACBrCTe.Conhecimentos.Items[0].CTe.infCTe.ID);


      ACBrCTe.WebServices.Consulta.Executar;
      Resposta := TConsultaCTeResposta.Create(TpResp, codUTF8);
      try
        Resposta.Processar(ACBrCTe);
        fpCmd.Resposta := Resposta.Msg + sLineBreak + Resposta.Gerar;

        if  FilesExists( AXML ) then
          fpCmd.Resposta :=  fpCmd.Resposta + sLineBreak + 'Arquivo=' + AXML;

      finally
        Resposta.Free;
      end;

    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoAssinarCTe }

{ Params: 0 - XML - Uma String com um Path completo XML CTe
}
procedure TMetodoAssinarCTe.Executar;
var
  CargaDFe: TACBrCarregarCTe;
  Salva: boolean;
  AXML: String;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.Conhecimentos.Clear;
    CargaDFe := TACBrCarregarCTe.Create(ACBrCTe, AXML);
    try
      Salva := ACBrCTe.Configuracoes.Arquivos.Salvar;

      if not Salva then
      begin
        ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
        ACBrCTe.Configuracoes.Arquivos.PathSalvar :=
          PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
      end;

      ACBrCTe.Configuracoes.Arquivos.Salvar := True;
      ACBrCTe.Conhecimentos.Assinar;
      ACBrCTe.Configuracoes.Arquivos.Salvar := Salva;

      if NaoEstaVazio(ACBrCTe.Conhecimentos.Items[0].NomeArq) then
        fpCmd.Resposta := ACBrCTe.Conhecimentos.Items[0].NomeArq
      else
        fpCmd.Resposta := PathWithDelim(ACBrCTe.Configuracoes.Arquivos.PathSalvar)
          + StringReplace(ACBrCTe.Conhecimentos.Items[0].CTe.infCTe.ID,
          'CTe', '', [rfIgnoreCase]) + '-cte.xml';
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoValidarCTe }

{ Params: 0 - XML - Uma String com um Path completo XML CTe
}
procedure TMetodoValidarCTe.Executar;
var
  CargaDFe: TACBrCarregarCTe;
  AXML: String;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.Conhecimentos.Clear;
    CargaDFe := TACBrCarregarCTe.Create(ACBrCTe, AXML);
    try
      ACBrCTe.Conhecimentos.Validar;
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoStatusServico }

procedure TMetodoStatusServico.Executar;
var
  Resposta: TStatusServicoResposta;
begin
  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    if ACBrCTe.WebServices.StatusServico.Executar then
    begin
      Resposta := TStatusServicoResposta.Create(TpResp, codUTF8);
      try
        Resposta.Processar(ACBrCTe);
        fpCmd.Resposta := Resposta.Msg + sLineBreak + Resposta.Gerar;
      finally
        Resposta.Free;
      end;
    end;

  end;
end;

{ TMetodoImprimirEventoPDF }

{ Params: 0 - XMLEvento - Uma String com um Path completo XMLEvento CTe
          1 - XML       - Uma String com um Path completo XML CTe
}
procedure TMetodoImprimirEventoPDF.Executar;
var
  CargaDFe: TACBrCarregarCTe;
  CargaDFeEvento: TACBrCarregarCTeEvento;
  ArqPDF, AXMLEvento, AXML: string;
begin
  AXMLEvento := fpCmd.Params(0);
  AXML := fpCmd.Params(1);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.EventoCTe.Evento.Clear;
    CargaDFeEvento := TACBrCarregarCTeEvento.Create(ACBrCTe, AXMLEvento);
    ACBrCTe.Conhecimentos.Clear;
    CargaDFe := TACBrCarregarCTe.Create(ACBrCTe, AXML);
    try
      try
        DoConfiguraDACTe(False, '');
        ACBrCTe.ImprimirEventoPDF;
        ArqPDF := OnlyNumber(ACBrCTe.EventoCTe.Evento[0].Infevento.Id);
        ArqPDF := PathWithDelim(ACBrCTe.DACTe.PathPDF) + ArqPDF + '-procEventoCTe.pdf';

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

{ Params: 0 - XMLEvento - Uma String com um Path completo XMLEvento CTe
          1 - XMLCTe - Uma String com um Path completo XML CTe
          2 - String com nome Impressora
          3 - Integer Número de Cópias
          4 - Mostrar Preview (1 - para preview)
}
procedure TMetodoImprimirEvento.Executar;
var
  CargaDFe: TACBrCarregarCTe;
  CargaDFeEvento: TACBrCarregarCTeEvento;
  AXMLEvento, AXML, AImpressora: String;
  ACopias: Integer;
  APreview: Boolean;
begin
  AXMLEvento := fpCmd.Params(0);
  AXML := fpCmd.Params(1);
  AImpressora := fpCmd.Params(2);
  ACopias := StrToIntDef(fpCmd.Params(3), 0);
  APreview := StrToBoolDef(fpCmd.Params(4), False);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.EventoCTe.Evento.Clear;
    CargaDFeEvento := TACBrCarregarCTeEvento.Create(ACBrCTe, AXMLEvento);
    ACBrCTe.Conhecimentos.Clear;
    CargaDFe := TACBrCarregarCTe.Create(ACBrCTe, AXML);
    try
      DoConfiguraDACTe(False, BoolToStr(APreview,'1',''));
      if NaoEstaVazio(AImpressora) then
        ACBrCTe.DACTe.Impressora := AImpressora;

      if (ACopias > 0) then
        ACBrCTe.DACTe.NumCopias := ACopias;

      try
        DoAntesDeImprimir((APreview) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
        ACBrCTe.ImprimirEvento;
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

{ TMetodoImprimirDaCTePDF }

{ Params: 0 - XML - Uma String com um Path completo XML CTe
          1 - Protocolo: String com Número de Protocolo
          2 - Cancelado: 1 para CT-e Cancelado
}
procedure TMetodoImprimirDaCTePDF.Executar;
var
  ArqPDF, AChave, AProtocolo: string;
  CargaDFe: TACBrCarregarCTe;
  ACancelado: Boolean;
begin
  AChave := fpCmd.Params(0);
  AProtocolo := fpCmd.Params(1);
  ACancelado := StrToBoolDef(fpCmd.Params(2), False);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.Conhecimentos.Clear;
    CargaDFe := TACBrCarregarCTe.Create(ACBrCTe, AChave);
    try
      DoConfiguraDACTe(True, '');

      if NaoEstaVazio(AProtocolo) then
        ACBrCTe.DACTe.Protocolo := AProtocolo;

      ACBrCTe.DACTe.Cancelada := ACancelado;

      try
        ACBrCTe.Conhecimentos.ImprimirPDF;
        ArqPDF := OnlyNumber(ACBrCTe.Conhecimentos.Items[0].CTe.infCTe.ID) + '-cte.pdf';
        ArqPDF := PathWithDelim(ACBrCTe.DACTe.PathPDF) + ArqPDF;
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

{ TMetodoCriarEnviarCTe }


procedure TMetodoCriarEnviarCTe.TratarRetorno(const ACTe: TACBrCTe; const ACTeObject: TACBrObjetoCTe);
var
  LRespEnvio: TEnvioResposta;
  LRespRetorno: TRetornoResposta;
  i, j: Integer;
begin
  if (FAssincrono) then
  begin
    ACTe.WebServices.Retorno.Recibo := ACTe.WebServices.Enviar.Recibo;
    ACTe.WebServices.Retorno.Executar;
    LRespRetorno := TRetornoResposta.Create('CTe', ACTeObject.TpResp, codUTF8);
    try
      LRespRetorno.Processar(ACTe.WebServices.Retorno.CTeRetorno,
                           ACTe.WebServices.Retorno.Recibo,
                           ACTe.WebServices.Retorno.Msg,
                           ACTe.WebServices.Retorno.Protocolo,
                           ACTe.WebServices.Retorno.ChaveCTe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + LRespRetorno.Msg
                    + sLineBreak + LRespRetorno.Gerar;
    finally
      LRespRetorno.Free;
    end;

    for i:=0 to ACTe.WebServices.Retorno.CTeRetorno.ProtDFe.Count - 1 do
    begin
      for j:=0 to ACTe.Conhecimentos.Count - 1 do
      begin
        if ('CTe' + ACTe.WebServices.Retorno.CTeRetorno.ProtDFe[i].chDFe = ACTe.Conhecimentos.Items[j].CTe.infCTe.Id) then
        begin
          //Informa Mensagem de Sem Valor Fiscal para documentos emitidos em Homologação
          if ACTe.Conhecimentos.Items[J].CTe.Ide.tpAmb = taHomologacao then
            ACTe.Conhecimentos.Items[J].CTe.dest.xNome:= cHOM_MSG;
        end;
      end;
    end;
  end else
  begin
    LRespEnvio := TEnvioResposta.Create(ACTeObject.TpResp, codUTF8);
    try
      LRespEnvio.Processar(ACTe);
      fpCmd.Resposta := fpCmd.Resposta + LRespEnvio.Msg + sLineBreak + LRespEnvio.Gerar;
    finally
      LRespEnvio.Free;
    end;
    if {(ACTe.Conhecimentos[0].Confirmado) and }(ACTe.Conhecimentos[0].CTe.Ide.tpAmb = taHomologacao) then
      ACTe.Conhecimentos[0].CTe.dest.xNome := cHOM_MSG;
  end;

  for i:=0 to ACTe.Conhecimentos.Count-1 do
    fpCmd.Resposta := fpCmd.Resposta + sLineBreak + '[CTe_Arq' + Trim(IntToStr(
                      ACTe.Conhecimentos[i].CTe.ide.nCT))+']' + sLineBreak +
                      'Arquivo=' + ACTe.Conhecimentos[i].NomeArq;
end;

procedure TMetodoCriarEnviarCTe.Imprimir(const ACTe: TACBrCTe; const ACTeObject: TACBrObjetoCTe);
var
  i: Integer;
begin
  for i:=0 to ACTe.Conhecimentos.Count -1 do
  begin
    if ACTe.Conhecimentos[i].Confirmado then
    begin
      ACTeObject.DoConfiguraDACTe(False, BoolToStr(FPreview, '1', ''));

      if NaoEstaVazio(FImpressora) then
        ACTe.DACTe.Impressora := FImpressora;

      if FCopias > 0 then
        ACTe.DACTe.NumCopias := FCopias;

      try
        ACTeObject.DoAntesDeImprimir(FPreview or (ACTeObject.MonitorConfig.DFe.Impressao.DANFE.MostrarPreview));
        ACTe.Conhecimentos[i].Imprimir;
      finally
        ACTeObject.DoDepoisDeImprimir;
      end;
    end;
  end;
end;

procedure TMetodoCriarEnviarCTe.GerarPDF(const ACTe: TACBrCTe; const ACTeObject: TACBrObjetoCTe);
var
  i: Integer;
  ArqPDF: String;
begin
  for i:=0 to ACTe.Conhecimentos.Count - 1 do
  begin
    if ACTe.Conhecimentos[i].Confirmado then
    begin
      ACTeObject.DoConfiguraDACTe(True, BoolToStr(FPreview, '1', ''));

      ACTe.Conhecimentos[i].ImprimirPDF;
      ArqPDF := OnlyNumber(ACTe.Conhecimentos[i].CTe.infCTe.Id)+'-cte.pdf';

      fpCmd.Resposta := fpCmd.Resposta + sLineBreak +
                        'PDF='+ PathWithDelim(ACTe.DACTe.PathPDF) + ArqPDF + sLineBreak;

    end;
  end;
end;

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini CTe
                         ou Uma String com conteúdo txt do CTe
          1 - NumeroLote: Integer com número do lote a ser adicionado
          2 - Imprime : 1 para imprimir
          3 - NomeImpressora: String com nome impressora para impressão (Default)
          4 - MostrarPreview: 1 para mostrar preview (Default)
          5 - Numero de Copias: Inteiro com número de cópias (Default)
          6 - ImprimirPDF: 1 para imprimir PDF (Default)
          7 - Assincrono: Boolean
}
procedure TMetodoCriarEnviarCTe.Executar;
var
  LImprimir, LPDF: Boolean;
  LAlertas: AnsiString;
  LArqCTe, LResposta, LArqIni: String;
  LLote: Integer;
  LCTe: TACBrCTe;
  LCTeObject: TACBrObjetoCTe;
begin
  //Preparando paramentros de entrada
  LArqIni     := fpCmd.Params(0);
  LLote       := StrToIntDef(fpCmd.Params(1), 0);
  LImprimir   := StrToBoolDef(fpCmd.Params(2), False);
  FImpressora := fpCmd.Params(3);
  FPreview    := StrToBoolDef(fpCmd.Params(4), False);
  FCopias     := StrToIntDef(fpCmd.Params(5), 0);
  LPDF        := StrToBoolDef(fpCmd.Params(6), False);
  FAssincrono := StrToBoolDef( fpCmd.Params(7), True);
  LCTeObject  := TACBrObjetoCTe(fpObjetoDono);
  LCTe        := LCTeObject.ACBrCTe;

  //Criação
  LCTeObject.LerIniCTe(LArqIni);

  if not LCTe.Configuracoes.Arquivos.Salvar then
  begin
    ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
    LCTe.Configuracoes.Arquivos.PathSalvar := PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
  end;

  LCTe.Conhecimentos.GerarCTe;
  LAlertas := LCTe.Conhecimentos[0].Alertas;

  LCTe.Conhecimentos.Assinar;
  LCTe.Conhecimentos.Validar;

  LArqCTe := PathWithDelim(LCTe.Configuracoes.Arquivos.PathSalvar) + OnlyNumber(LCTe.Conhecimentos[0].CTe.infCTe.ID) + '-cte.xml';
  LCTe.Conhecimentos.GravarXML(LArqCTe);

  if not FileExists(LArqCTe) then
    raise Exception.Create('Não foi possível criar o arquivo ' + LArqCTe);

  LResposta := LArqCTe;
  if (LAlertas <> '') then
    LResposta := LResposta + sLineBreak + 'Alertas:' + LAlertas;

  fpCmd.Resposta := LResposta + sLineBreak ;

  //Enviando
  if (LLote = 0) then
    LLote := 1;

  LCTe.WebServices.Enviar.Lote := IntToStr(LLote);

  LCTe.WebServices.Enviar.Sincrono:= not(FAssincrono);

  LCTe.WebServices.Enviar.Executar;

  //Tratamento do Retorno

  TratarRetorno(LCTe, LCTeObject);

  //Impressão
  if LImprimir then
    Imprimir(LCTe, LCTeObject);

  //PDF
  if LPDF then
    GerarPDF(LCTe, LCTeObject);
end;

{ TMetodoAdicionarCTe }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini CTe
                         ou Uma String com conteúdo txt do CTe
          1 - NumeroLote: String com número do lote a ser adicionado
}
procedure TMetodoAdicionarCTe.Executar;
var
  Alertas: ansistring;
  ArqCTe: string;
  Resp, AIni, ANumeroLote: string;
begin

  AIni := fpCmd.Params(0);
  ANumeroLote := fpCmd.Params(1);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    LerIniCTe(AIni);

    ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'Lotes' + PathDelim + 'Lote' + trim(ANumeroLote));

    ACBrCTe.Conhecimentos.GerarCTe;
    Alertas := ACBrCTe.Conhecimentos.Items[0].Alertas;

    ACBrCTe.Conhecimentos.Assinar;
    ACBrCTe.Conhecimentos.Validar;

    ArqCTe := PathWithDelim(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'Lotes' + PathDelim + 'Lote' + trim(ANumeroLote)) + OnlyNumber(
      ACBrCTe.Conhecimentos.Items[0].CTe.infCTe.ID) + '-cte.xml';
    ACBrCTe.Conhecimentos.GravarXML(ExtractFilePath(ArqCTe));

    if not FileExists(ArqCTe) then
      raise Exception.Create('Não foi possível criar o arquivo ' + ArqCTe);

    Resp := ArqCTe;
    if (Alertas <> '') then
      Resp := Resp + sLineBreak + 'Alertas:' + Alertas;

    fpCmd.Resposta := Resp;
  end;
end;

{ TMetodoEnviarLoteCTe }

{ Params: 0 - Lote - Integer - Número Lote que contém CTe
          1 - LoteEnvio: Integer com número do lote. Default = 1
          2 - Imprime: 1 para Imprimir
          3 - Impressora: String Nome da Impressora
          4 - Preview: 1 para Mostrar Preview
          5 - Copias: Inteiro com número de cópias para impressão
          6 - PDF: 1 para impressão em PDF
}
procedure TMetodoEnviarLoteCTe.Executar;
var
  RetFind: integer;
  SearchRec: TSearchRec;
  ALote: String;
  ALoteEnvio: Integer;
  AImprime: Boolean;
  AImpressora: String;
  APreview: Boolean;
  ACopias: Integer;
  APDF: Boolean;
  RespEnvio: TEnvioResposta;
  RespRetorno: TRetornoResposta;
begin
  ALote := Trim(fpCmd.Params(0));
  ALoteEnvio := StrToIntDef(fpCmd.Params(1), 1);
  AImprime := StrToBoolDef(fpCmd.Params(2), False);
  AImpressora := fpCmd.Params(3);
  APreview     := StrToBoolDef(fpCmd.Params(4), False);
  ACopias      := StrToIntDef(fpCmd.Params(5), 0);
  APDF         := StrToBoolDef(fpCmd.Params(6), False);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    if not DirectoryExists(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'Lotes' + PathDelim + 'Lote' + ALote ) then
      raise Exception.Create('Diretório não encontrado:' + PathWithDelim(
        ExtractFilePath(Application.ExeName)) +
        'Lotes' + PathDelim + 'Lote' + ALote)
    else
    begin
      ACBrCTe.Conhecimentos.Clear;
      RetFind := SysUtils.FindFirst(
        PathWithDelim(ExtractFilePath(Application.ExeName)) +
        'Lotes' + PathDelim + 'Lote' +
        ALote + PathDelim + '*-cte.xml', faAnyFile, SearchRec);
      if (RetFind = 0) then
      begin
        while RetFind = 0 do
        begin
          ACBrCTe.Conhecimentos.LoadFromFile(PathWithDelim(ExtractFilePath(Application.ExeName)) +
            'Lotes' + PathDelim +
            'Lote' + ALote + PathDelim + SearchRec.Name);
          RetFind := FindNext(SearchRec);
        end;
        ACBrCTe.Conhecimentos.GerarCTe;
        ACBrCTe.Conhecimentos.Assinar;
        ACBrCTe.Conhecimentos.Validar;
      end
      else
        raise Exception.Create('Não foi encontrada nenhuma nota para o Lote: ' +
          ALote);
    end;

    ACBrCTe.WebServices.Enviar.Lote := IntToStr(ALoteEnvio);
    ACBrCTe.WebServices.Enviar.Sincrono := False;

    ACBrCTe.WebServices.Enviar.Executar;
    RespEnvio := TEnvioResposta.Create(TpResp, codUTF8);
    try
       RespEnvio.Processar(ACBrCTe);
       fpCmd.Resposta := fpCmd.Resposta + RespEnvio.Msg + sLineBreak + RespEnvio.Gerar;
    finally
       RespEnvio.Free;
    end;

    ACBrCTe.WebServices.Retorno.Recibo := ACBrCTe.WebServices.Enviar.Recibo;
    ACBrCTe.WebServices.Retorno.Executar;
    RespRetorno := TRetornoResposta.Create('CTe', TpResp, codUTF8);
    try
      RespRetorno.Processar(ACBrCTe.WebServices.Retorno.CTeRetorno,
                            ACBrCTe.WebServices.Retorno.Recibo,
                            ACBrCTe.WebServices.Retorno.Msg,
                            ACBrCTe.WebServices.Retorno.Protocolo,
                            ACBrCTe.WebServices.Retorno.ChaveCTe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespRetorno.Msg
                     + sLineBreak + RespRetorno.Gerar;
    finally
      RespRetorno.Free;
    end;

    RespostaConhecimentos(AImprime, AImpressora, APreview, ACopias, APDF);
  end;
end;

{ TMetodoEnviarCTe }


procedure TMetodoEnviarCTe.TratarRetorno(const ACTe: TACBrCTe; const ACTeObject: TACBrObjetoCTe);
var
  RespEnvio: TEnvioResposta;
  RespRetorno: TRetornoResposta;
  i, j: Integer;
begin
  if (FAssincrono) then
  begin
    ACTe.WebServices.Retorno.Recibo := ACTe.WebServices.Enviar.Recibo;
    ACTe.WebServices.Retorno.Executar;
    RespRetorno := TRetornoResposta.Create('CTe', ACTeObject.TpResp, codUTF8);
    try
      RespRetorno.Processar(ACTe.WebServices.Retorno.CTeRetorno,
                            ACTe.WebServices.Retorno.Recibo,
                            ACTe.WebServices.Retorno.Msg,
                            ACTe.WebServices.Retorno.Protocolo,
                            ACTe.WebServices.Retorno.ChaveCTe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespRetorno.Msg
                     + sLineBreak + RespRetorno.Gerar;
    finally
      RespRetorno.Free;
    end;

    for i:=0 to ACTe.WebServices.Retorno.CTeRetorno.ProtDFe.Count - 1 do
    begin
      for j:=0 to ACTe.Conhecimentos.Count - 1 do
      begin
        if ('CTe' + ACTe.WebServices.Retorno.CTeRetorno.ProtDFe[i].chDFe = ACTe.Conhecimentos.Items[j].CTe.infCTe.Id) then
        begin
          //Informa Mensagem de Sem Valor Fiscal para documentos emitidos em Homologação
          if ACTe.Conhecimentos.Items[J].CTe.Ide.tpAmb = taHomologacao then
            ACTe.Conhecimentos.Items[J].CTe.dest.xNome:= cHOM_MSG;
        end;
      end;
    end;
  end else
  begin
    RespEnvio := TEnvioResposta.Create(ACTeObject.TpResp, codUTF8);
    try
       RespEnvio.Processar(ACTe);
       fpCmd.Resposta := fpCmd.Resposta + RespEnvio.Msg + sLineBreak + RespEnvio.Gerar;
    finally
       RespEnvio.Free;
    end;
    if (ACTe.Conhecimentos[0].Confirmado) and (ACTe.Conhecimentos[0].CTe.Ide.tpAmb = taHomologacao) then
      ACTe.Conhecimentos[0].CTe.dest.xNome := cHOM_MSG;
  end;

  for i:=0 to ACTe.Conhecimentos.Count-1 do
    fpCmd.Resposta := fpCmd.Resposta + sLineBreak + '[CTe_Arq' + Trim(IntToStr(
                      ACTe.Conhecimentos[i].CTe.ide.nCT))+']' + sLineBreak +
                      'Arquivo=' + ACTe.Conhecimentos[i].NomeArq;

end;

procedure TMetodoEnviarCTe.Imprimir(const ACTe: TACBrCTe; const ACTeObject: TACBrObjetoCTe);
var
  i: Integer;
begin
  for i:=0 to ACTe.Conhecimentos.Count -1 do
  begin
    if ACTe.Conhecimentos[i].Confirmado then
    begin
      ACTeObject.DoConfiguraDACTe(False, BoolToStr(False));

      if NaoEstaVazio(FImpressora) then
        ACTe.DACTe.Impressora := FImpressora;

      ACTe.DACTe.NumCopias := 1;

      try
        ACTeObject.DoAntesDeImprimir(False);
        ACTe.Conhecimentos[i].Imprimir;
      finally
        ACTeObject.DoDepoisDeImprimir;
      end;
    end;
  end;
end;

{ Params: 0 - PathorXML - Uma String com um Path completo arquivo XML CTe
                         ou Uma String com conteúdo XML do CTe
          1 - Lote: Integer com número do lote. Default = 1
          2 - Assina: 1 para assinar XML
          3 - Imprime: 1 Para True. Default 0
          4 - Nome Impressora: String com Nome da Impressora
          5 - Assincrono : Boolean
}
procedure TMetodoEnviarCTe.Executar;
var
  CargaDFe: TACBrCarregarCTe;
  APathorXML, AImpressora: String;
  ALote: Integer;
  AAssina, AImprime: Boolean;
  LCTeObject: TACBrObjetoCTe;
  LCTe: TACBrCTe;
begin
  APathorXML := fpCmd.Params(0);
  ALote := StrToIntDef(fpCmd.Params(1), 0);
  AAssina := StrToBoolDef(fpCmd.Params(2), False);
  AImprime := StrToBoolDef(fpCmd.Params(3), False);
  FImpressora := fpCmd.Params(4);
  FAssincrono := StrToBoolDef( fpCmd.Params(5), True);
  LCTeObject := TACBrObjetoCTe(fpObjetoDono);
  LCTe := LCTeObject.ACBrCTe;

  LCTeObject.ACBrCTe.Conhecimentos.Clear;
  CargaDFe := TACBrCarregarCTe.Create(LCTe, APathorXML);
  try
    LCTe.Conhecimentos.GerarCTe;

    if (AAssina) then
      LCTe.Conhecimentos.Assinar;

    LCTe.Conhecimentos.Validar;

    if (ALote = 0) then
      LCTe.WebServices.Enviar.Lote := '1'
    else
      LCTe.WebServices.Enviar.Lote := IntToStr(ALote);

    LCTe.WebServices.Enviar.Sincrono:= not(FAssincrono);

    LCTe.WebServices.Enviar.Executar;

    TratarRetorno(LCTe, LCTeObject);

    if AImprime then
      Imprimir(LCTe, LCTeObject);

  finally
    CargaDFe.Free;
  end;
end;

{ TMetodoCriarCTe }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini CTe
                         ou Uma String com conteúdo txt do CTe
          1 - RetornaXML: 1 para Retornar XML Gerado na Resposta
}
procedure TMetodoCriarCTe.Executar;
var
  Salva, ARetornaXML: boolean;
  Alertas: ansistring;
  ArqCTe: string;
  SL: TStringList;
  Resp, AIni: string;
begin
  AIni := fpCmd.Params(0);
  ARetornaXML := StrToBoolDef(fpCmd.Params(1), False);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    LerIniCTe(AIni);

    Salva := ACBrCTe.Configuracoes.Arquivos.Salvar;
    if not Salva then
    begin
      ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
      ACBrCTe.Configuracoes.Arquivos.PathSalvar :=
        PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
    end;

    ACBrCTe.Conhecimentos.GerarCTe;
    Alertas := ACBrCTe.Conhecimentos.Items[0].Alertas;

    ACBrCTe.Conhecimentos.Assinar;
    ACBrCTe.Conhecimentos.Validar;

    ArqCTe := PathWithDelim(ACBrCTe.Configuracoes.Arquivos.PathSalvar) +
      OnlyNumber(ACBrCTe.Conhecimentos.Items[0].CTe.infCTe.ID) + '-cte.xml';
    ACBrCTe.Conhecimentos.GravarXML(ArqCTe);

    if not FileExists(ArqCTe) then
      raise Exception.Create('Não foi possível criar o arquivo ' + ArqCTe);

    Resp := ArqCTe;
    if (Alertas <> '') then
      Resp := Resp + sLineBreak + 'Alertas:' + Alertas;

    if ARetornaXML then
    begin
      SL := TStringList.Create;
      try
        SL.LoadFromFile(ArqCTe);
        Resp := Resp + sLineBreak + SL.Text;
      finally
        SL.Free;
      end;
    end;

    fpCmd.Resposta := Resp;
  end;
end;

{ TMetodoImprimirDaCTe }

{ Params: 0 - XMLFile - Uma String com um Path completo para um arquivo XML CTe
                         ou Uma String com conteúdo XML CTe
          1 - Impressora: String com Nome da Impressora
          2 - Copias: Integer Número de Copias
          3 - Protocolo: String com Número de Protocolo
          4 - Cancelado: 1 para CT-e Cancelado
          5 - Preview: 1 para Mostrar Preview
}
procedure TMetodoImprimirDaCTe.Executar;
var
  CargaDFe: TACBrCarregarCTe;
  AChave, AImpressora, AProtocolo: String;
  ACopias: Integer;
  ACancelado: Boolean;
  APreview: Boolean;
begin
  AChave := fpCmd.Params(0);
  AImpressora := fpCmd.Params(1);
  ACopias := StrToIntDef(fpCmd.Params(2), 0);
  AProtocolo := fpCmd.Params(3);
  ACancelado := StrToBoolDef(fpCmd.Params(4), False);
  APreview := StrToBoolDef(fpCmd.Params(5), False);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.Conhecimentos.Clear;
    CargaDFe := TACBrCarregarCTe.Create(ACBrCTe, AChave);
    try
      DoConfiguraDACTe(False, BoolToStr(APreview,'1',''));

      if NaoEstaVazio(AImpressora) then
        ACBrCTe.DACTe.Impressora := AImpressora;

      if (ACopias > 0) then
        ACBrCTe.DACTe.NumCopias := ACopias;

      if NaoEstaVazio(AProtocolo) then
        ACBrCTe.DACTe.Protocolo := AProtocolo;

      ACBrCTe.DACTe.Cancelada := ACancelado;

      try
        DoAntesDeImprimir((APreview) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
        ACBrCTe.Conhecimentos.Imprimir;
      finally
        DoDepoisDeImprimir;
      end;
    finally
      CargaDFe.Free;
    end;

    fpCmd.Resposta := 'DACTe Impresso com sucesso';
  end;
end;

{ TMetodoCancelarCTe }

{ Params: 0 - Chave - Uma String com a Chave XML CTe
          1 - Justificativa: String (Mínimo de 15 Carcteres)
          2 - CNPJ: String com CNPJ do emitente
          3 - Lote: Integer com Numero do Lote. Default = 1
}
procedure TMetodoCancelarCTe.Executar;
var
  AChave, AJustificativa, ACNPJ: String;
  ALote: Integer;
  Resposta: TCancelamentoResposta;
begin
  AChave := fpCmd.Params(0);
  AJustificativa := fpCmd.Params(1);
  ACNPJ := fpCmd.Params(2);
  ALote := StrToIntDef(fpCmd.Params(3), 1);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    if not ValidarChave(AChave) then
      raise Exception.Create('Chave ' + AChave + ' inválida.')
    else
      ACBrCTe.WebServices.Consulta.CTeChave := AChave;

    ACBrCTe.WebServices.Consulta.Executar;
    if EstaVazio(ACBrCTe.WebServices.Consulta.Protocolo) then
      raise Exception.Create(ACBrStr('Não foi possível consultar o número de Protocolo para a Chave: ') + AChave );

    ACBrCTe.EventoCTe.Evento.Clear;
    with ACBrCTe.EventoCTe.Evento.New do
    begin
      Infevento.CNPJ := ACNPJ;
      if Trim(Infevento.CNPJ) = '' then
        Infevento.CNPJ := copy(OnlyNumber(ACBrCTe.WebServices.Consulta.CTeChave), 7, 14)
      else
      begin
        if not ValidarCNPJouCPF(ACNPJ) then
          raise Exception.Create('CNPJ/CPF ' + ACNPJ + ' inválido.');
      end;

      Infevento.cOrgao := StrToIntDef(
        copy(OnlyNumber(ACBrCTe.WebServices.Consulta.CTeChave), 1, 2), 0);
      Infevento.dhEvento := now;
      Infevento.tpEvento := teCancelamento;
      Infevento.chCTe := ACBrCTe.WebServices.Consulta.CTeChave;
      Infevento.detEvento.nProt := ACBrCTe.WebServices.Consulta.Protocolo;
      Infevento.detEvento.xJust := AJustificativa;
    end;

    ACBrCTe.EnviarEvento(ALote);
    Resposta := TCancelamentoResposta.Create(TpResp, codUTF8);
    try
      Resposta.Processar(ACBrCTe);
      fpCmd.Resposta := Resposta.XMotivo + sLineBreak + Resposta.Gerar;
    finally
      Resposta.Free;
    end;

  end;
end;

{ TMetodoSetModeloDF }

{ Params: 0 - NumAmbiente : Integer 57- CTe / 67- CTe OS
}
procedure TMetodoSetModeloDF.Executar;
var
  OK: boolean;
  NumModelo: Integer;
begin
  NumModelo := StrToIntDef(fpCmd.Params(0), 2);

  if not (NumModelo in [57, 67]) then
    raise Exception.Create('Modelo Inválido: '+IntToStr(NumModelo));

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.Configuracoes.Geral.ModeloDF := StrToModeloCTe(OK, IntToStr(NumModelo));
  end;
end;

{ TMetodoCNPJCertificado }

procedure TMetodoCNPJCertificado.Executar;
begin
  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrCTe.SSL.CertCNPJ;
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
  CargaDFeInut: TACBrCarregarCTeInut;
  AXMLInut, AImpressora: String;
  ACopias: Integer;
  APreview: Boolean;
begin
  AXMLInut := fpCmd.Params(0);
  AImpressora := fpCmd.Params(1);
  ACopias := StrToIntDef(fpCmd.Params(2), 0);
  APreview:= StrToBoolDef(fpCmd.Params(3), False);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.InutCTe.ID := '';
    CargaDFeInut := TACBrCarregarCTeInut.Create(ACBrCTe, AXMLInut);

    try
      DoConfiguraDACTe(False, BoolToStr(APreview,'1',''));

      if NaoEstaVazio(AImpressora) then
        ACBrCTe.DACTe.Impressora := AImpressora;

      if (ACopias > 0) then
        ACBrCTe.DACTe.NumCopias := ACopias;

      try
        // Informado DANFE pois o DACTE não possui a propriedade MostrarPrefiew
        DoAntesDeImprimir((APreview) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
        ACBrCTe.ImprimirInutilizacao;
      finally
        DoDepoisDeImprimir;
      end;
    finally
      CargaDFeInut.Free;
    end;

    fpCmd.Resposta := 'Inutilização Impresso com sucesso';
  end;
end;

{ TMetodoImprimirInutilizacaoPDF }

{ Params: 0 - XMLInutilizacao - Uma String com um Path completo XMLInutilizacao
}
procedure TMetodoImprimirInutilizacaoPDF.Executar;
var
  CargaDFeInut: TACBrCarregarCTeInut;
  AXMLInut, ArqPDF: String;
begin
  AXMLInut := fpCmd.Params(0);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.InutCTe.ID := '';
    CargaDFeInut := TACBrCarregarCTeInut.Create(ACBrCTe, AXMLInut);

    try
      DoConfiguraDACTe(False, '');
      try
        ACBrCTe.ImprimirInutilizacaoPDF;
        ArqPDF := OnlyNumber(ACBrCTe.InutCTe.ID);
        ArqPDF := PathWithDelim(ACBrCTe.DACTe.PathPDF) + ArqPDF + '-procInutCTe.pdf';

        fpCmd.Resposta := 'Arquivo criado em: ' + ArqPDF;
      except
        on E: Exception do
          raise Exception.Create('Erro ao criar o arquivo PDF. '+ sLineBreak + E.Message);
      end;
    finally
      CargaDFeInut.Free;
    end;
  end;
end;

{ TMetodoGetPathCTe }

{ Params: 0 - Data: TDateTime - Data para geração do path
          1 - CNPJ: String - CNPJ para geração do path
          2 - IE: String - IE para geração do path
          3 - Modelo: Integer -  Modelo para geração
}
procedure TMetodoGetPathCTe.Executar;
var
  AData: TDateTime;
  ACNPJ: String;
  AIE: String;
  AModelo: Integer;
begin
  AData:= StrToDateTimeDef(fpCmd.Params(0),0);
  ACNPJ:= fpCmd.Params(1);
  AIE:= fpCmd.Params(2);
  AModelo := StrToIntDef( fpCmd.Params(3), 0);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrCTe.Configuracoes.Arquivos.GetPathCTe(AData, ACNPJ, AIE, AModelo);
  end;
end;

{ TMetodoGetPathCCe }

{ Params: 0 - CNPJ: String - CNPJ para geração do path
          1 - IE: String - IE para geração do path
          2 - Data: TDateTime - Data para geração do path
}
procedure TMetodoGetPathCCe.Executar;
var
  ACNPJ: String;
  AIE: String;
  AData: TDateTime;
begin
  ACNPJ:= fpCmd.Params(0);
  AIE:= fpCmd.Params(1);
  AData:= StrToDateTimeDef(fpCmd.Params(2),0);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrCTe.Configuracoes.Arquivos.GetPathEvento(teCCe, ACNPJ, AIE, AData);
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
  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrCTe.Configuracoes.Arquivos.GetPathEvento(teCancelamento, ACNPJ, AIE, AData);
  end;
end;

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
  ok: Boolean;
begin
  CodEvento := fpCmd.Params(0);
  ACNPJ:= fpCmd.Params(1);
  AIE:= fpCmd.Params(2);
  AData:= StrToDateTimeDef(fpCmd.Params(3),0);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrCTe.Configuracoes.Arquivos.GetPathEvento(StrToTpEventoCTe(ok ,CodEvento), ACNPJ, AIE, AData);
  end;
end;

{ TMetodoGetPathInu }

{ Params: 0 - Data: TDateTime - Data para geração do path
          1 - CNPJ: String - CNPJ para geração do path
          2 - IE: String - IE para geração do path
}
procedure TMetodoGetPathInu.Executar;
var
  AData: TDateTime;
  ACNPJ: String;
  AIE: String;
begin
  AData := StrToDateTime(fpCmd.Params(0));
  ACNPJ := fpCmd.Params(1);
  AIE := fpCmd.Params(2);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrCTe.Configuracoes.Arquivos.GetPathInu(AData, ACNPJ, AIE);
  end;
end;

{ TMetodoEnviarEvento }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini Evento
                         ou Uma String com conteúdo txt do Evento
}
procedure TMetodoEnviarEvento.Executar;
var
  AArq: String;
  Resp: TEventoResposta;
begin
  AArq := fpCmd.Params(0);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.EventoCTe.Evento.Clear;

    ACBrCTe.EventoCTe.LerFromIni( AArq, False );

    ACBrCTe.EnviarEvento(ACBrCTe.EventoCTe.idLote);
    Resp := TEventoResposta.Create(TpResp, codUTF8);
    try
      Resp.Processar(ACBrCTe);
      fpCmd.Resposta:= sLineBreak + Resp.Gerar;

    finally
      Resp.Free;
    end;

  end;
end;

{ TMetodoCartaCorrecao }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini Evento
                         ou Uma String com conteúdo txt do Evento
}
procedure TMetodoCartaCorrecao.Executar;
var
  AArq: String;
  Resp: TEventoResposta;
begin
  AArq := fpCmd.Params(0);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.EventoCTe.Evento.Clear;

    ACBrCTe.EventoCTe.LerFromIni( AArq, True );

    ACBrCTe.EnviarEvento(ACBrCTe.EventoCTe.idLote);
    Resp := TEventoResposta.Create(TpResp, codUTF8);
    try
      Resp.Processar(ACBrCTe);
      fpCmd.Resposta:= sLineBreak + Resp.Gerar;

    finally
      Resp.Free;
    end;

  end;
end;

{ TMetodoXMLEnviarEvento }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini Evento
                         ou Uma String com conteúdo txt do Evento
}
procedure TMetodoXMLEnviarEvento.Executar;
var
  AArq: String;
  CargaDFeEvento: TACBrCarregarCTeEvento;
  Resp: TEventoResposta;
begin
  AArq := fpCmd.Params(0);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.EventoCTe.Evento.Clear;
    CargaDFeEvento := TACBrCarregarCTeEvento.Create(ACBrCTe, AArq);

    try
      ACBrCTe.EnviarEvento(ACBrCTe.EventoCTe.idLote);
      Resp := TEventoResposta.Create(TpResp, codUTF8);
      try
        Resp.Processar(ACBrCTe);
        fpCmd.Resposta:= sLineBreak + Resp.Gerar;

      finally
        Resp.Free;
      end;

    finally
      CargaDFeEvento.Free;
    end;

  end;
end;

{ TMetodoDistribuicaoDFePorChaveCTe }

{ Params: 0 - Código da UF do autor da consulta
          1 - CNPJ do autor da consulta
          2 - Chave da NF-e que se deseja baixar o XML
}
procedure TMetodoDistribuicaoDFePorChaveCTe.Executar;
var
  AUF: Integer;
  ACNPJ: String;
  AChave: String;
  Resp: TDistribuicaoDFeResposta;
begin
  AUF := StrToIntDef(fpCmd.Params(0), 0);
  ACNPJ := fpCmd.Params(1);
  AChave := fpCmd.Params(2);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    if not ValidarCNPJouCPF(ACNPJ) then
      raise Exception.Create('CNPJ/CPF '+ACNPJ+' inválido.');

    ACBrCTe.DistribuicaoDFePorChaveCTe(AUF, ACNPJ, AChave);
    Resp:= TDistribuicaoDFeResposta.Create(TpResp, codUTF8);
    try
      Resp.Processar(ACBrCTe.WebServices.DistribuicaoDFe.retDistDFeInt,
                     ACBrCTe.WebServices.DistribuicaoDFe.Msg,
                     ACBrCTe.WebServices.DistribuicaoDFe.NomeArq,
                     ACBrCTe.WebServices.DistribuicaoDFe.ListaArqs);
      fpCmd.Resposta:= sLineBreak + Resp.Gerar ;

    finally
      Resp.Free;
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
  Resp: TDistribuicaoDFeResposta;
begin
  AUF := StrToIntDef(fpCmd.Params(0), 0);
  ACNPJ := fpCmd.Params(1);
  AUltNSU := fpCmd.Params(2);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    if not ValidarCNPJouCPF(ACNPJ) then
      raise Exception.Create('CNPJ/CPF '+ACNPJ+' inválido.');

    ACBrCTe.DistribuicaoDFePorUltNSU(AUF, ACNPJ, AUltNSU);
    Resp:= TDistribuicaoDFeResposta.Create(TpResp, codUTF8);
    try
      Resp.Processar(ACBrCTe.WebServices.DistribuicaoDFe.retDistDFeInt,
                     ACBrCTe.WebServices.DistribuicaoDFe.Msg,
                     ACBrCTe.WebServices.DistribuicaoDFe.NomeArq,
                     ACBrCTe.WebServices.DistribuicaoDFe.ListaArqs);
      fpCmd.Resposta:= sLineBreak + Resp.Gerar ;

    finally
      Resp.Free;
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
  Resp: TDistribuicaoDFeResposta;
begin
  AUF := StrToIntDef(fpCmd.Params(0), 0);
  ACNPJ := fpCmd.Params(1);
  ANSU := fpCmd.Params(2);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    if not ValidarCNPJouCPF(ACNPJ) then
      raise Exception.Create('CNPJ/CPF '+ACNPJ+' inválido.');

    ACBrCTe.DistribuicaoDFePorNSU(AUF, ACNPJ, ANSU);
    Resp:= TDistribuicaoDFeResposta.Create(TpResp, codUTF8);
    try
      Resp.Processar(ACBrCTe.WebServices.DistribuicaoDFe.retDistDFeInt,
                     ACBrCTe.WebServices.DistribuicaoDFe.Msg,
                     ACBrCTe.WebServices.DistribuicaoDFe.NomeArq,
                     ACBrCTe.WebServices.DistribuicaoDFe.ListaArqs);
      fpCmd.Resposta:= sLineBreak + Resp.Gerar ;

    finally
      Resp.Free;
    end;


  end;
end;

{ TMetodoEnviaremailEvento }

{ Params: 0 - Email: String com email Destinatário
          1 - XMLEvento: String com path do XML Evento
          2 - XMLCTe: String com path do XML CTe
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
  CargaDFeEvento: TACBrCarregarCTeEvento;
  CargaDFe: TACBrCarregarCTe;
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

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.EventoCTe.Evento.Clear;
    ACBrCTe.Conhecimentos.Clear;

    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    slReplay := TStringList.Create;
    try
      CargaDFeEvento := TACBrCarregarCTeEvento.Create(ACBrCTe, APathXMLEvento);
      CargaDFe := TACBrCarregarCTe.Create(ACBrCTe, APathXML);
      try
        DoConfiguraDACTe(True, '');
        if AEnviaPDF then
        begin
          try
            ACBrCTe.ImprimirEventoPDF;

            ArqPDF := OnlyNumber(ACBrCTe.EventoCTe.Evento[0].Infevento.id);
            ArqPDF := PathWithDelim(ACBrCTe.DACTe.PathPDF)+ArqPDF+'-procEventoCTe.pdf';
          except
            on E: Exception do
              raise Exception.Create('Erro ao criar o arquivo PDF.'+ sLineBreak + E.Message);
          end;
        end;

        with MonitorConfig.DFE.Email do
        begin
          slMensagemEmail.Text := StringToBinaryString(MensagemCTe);
          sAssunto := AssuntoCTe;
        end;

        slCC.DelimitedText := sLineBreak;
        slCC.Text := StringReplace(AEmailCopias, ';', sLineBreak, [rfReplaceAll]);

        slAnexos.DelimitedText := sLineBreak;
        slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

        slReplay.DelimitedText := sLineBreak;
        slReplay.Text := StringReplace(AReplay, ';', sLineBreak, [rfReplaceAll]);

        // Se carregou evento usando XML como parâmetro, salva XML para poder anexar
{
        if ( ArqEvento = '' ) then
        begin
          tipoEvento := ACBrCTe.EventoCTe.Evento[0].Infevento.tpEvento;
          ArqEvento  := ACBrCTe.EventoCTe.ObterNomeArquivo(tipoEvento);
          ArqEvento  := PathWithDelim(ACBrCTe.Configuracoes.Arquivos.GetPathEvento(tipoEvento))+ArqEvento;
          WriteToTxt(ArqEvento, ACBrCTe.EventoCTe.Evento[0].RetInfEvento.XML, False, False);
        end;
        slAnexos.Add(ArqEvento);
}
        if  StringIsXML( APathXMLEvento ) then
        begin
          tipoEvento := ACBrCTe.EventoCTe.Evento[0].InfEvento.tpEvento;
          ArqEvento  := ACBrCTe.EventoCTe.ObterNomeArquivo(tipoEvento);
          ArqEvento  := PathWithDelim(ACBrCTe.Configuracoes.Arquivos.GetPathEvento(tipoEvento))+ArqEvento;
          WriteToTxt(ArqEvento, ACBrCTe.EventoCTe.Evento[0].RetInfEvento.XML, False, False);
          slAnexos.Add(ArqEvento)
        end
        else
          slAnexos.Add(APathXMLEvento);
		  
        if AEnviaPDF then
          slAnexos.Add(ArqPDF);

        try
          ACBrCTe.EnviarEmail(ADestinatario,
            IfThen(NaoEstaVazio(AAssunto), AAssunto, sAssunto),
            slMensagemEmail,
            slCC,      // Lista com emails que serão enviado cópias - TStrings
            slAnexos,  // Lista de slAnexos - TStrings
            nil,
            '',
            slReplay); // Lista de slReplay - TStrings

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

{ TMetodoEnviaremailInutilizacao }

{ Params: 0 - Email: String com email Destinatário
          1 - XML: String com path do XML Inutilização
          2 - Boolean 1 : Envia PDF
          3 - Assunto: String com Assunto do e-mail
          4 - Copia: String com e-mails copia (Separados ;)
          5 - Anexo: String com Path de Anexos (Separados ;)
          6 - Replay: String com endereços replay (Separados ;)
}
procedure TMetodoEnviaremailInutilizacao.Executar;
var
  sAssunto, ADestinatario, APathXML, AAssunto, AEmailCopias,
  AAnexos, ArqPDF, ArqInut, AReplay: string;
  slMensagemEmail, slCC, slAnexos, slReplay: TStringList;
  CargaDFe: TACBrCarregarCTeInut;
  AEnviaPDF: Boolean;
begin
  ADestinatario := fpCmd.Params(0);
  APathXML := fpCmd.Params(1);
  AEnviaPDF := StrToBoolDef(fpCmd.Params(2), False);
  AAssunto := fpCmd.Params(3);
  AEmailCopias := fpCmd.Params(4);
  AAnexos := fpCmd.Params(5);
  AReplay := fpCmd.Params(6);
  ArqInut := '';

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    slReplay := TStringList.Create;
    try
      CargaDFe := TACBrCarregarCTeInut.Create(ACBrCTe, APathXML);
      try
        DoConfiguraDACTe(True, '');
        if AEnviaPDF then
        begin
          try
            ACBrCTe.ImprimirInutilizacaoPDF;

            ArqPDF := OnlyNumber(ACBrCTe.InutCTe.ID);
            ArqPDF := PathWithDelim(ACBrCTe.DACTe.PathPDF)+ArqPDF+'-procInutCTe.pdf';
          except
            on E: Exception do
              raise Exception.Create('Erro ao criar o arquivo PDF.'+ sLineBreak + E.Message);
          end;
        end;

        with MonitorConfig.DFE.Email do
        begin
          slMensagemEmail.Text := StringToBinaryString(MensagemCTe);
          sAssunto := AssuntoCTe;
        end;

        slCC.DelimitedText := sLineBreak;
        slCC.Text := StringReplace(AEmailCopias, ';', sLineBreak, [rfReplaceAll]);

        slAnexos.DelimitedText := sLineBreak;
        slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

        slReplay.DelimitedText := sLineBreak;
        slReplay.Text := StringReplace(AReplay, ';', sLineBreak, [rfReplaceAll]);

        // Se carregou Inutilizacao usando XML como parâmetro, salva XML para poder anexar
{
        if ( ArqInut = '' ) then
        begin
          ArqInut  := ACBrCTe.InutCTe.ID + '-procInutCTe.xml';
          ArqInut  := PathWithDelim(ACBrCTe.Configuracoes.Arquivos.GetPathInu()) + ArqInut;
          WriteToTxt(ArqInut, APathXML, False, False);
        end;
        slAnexos.Add(ArqInut);
}
        if  StringIsXML( APathXML ) then
        begin
          ArqInut  := ACBrCTe.InutCTe.ID + '-procInutCTe.xml';
          ArqInut  := PathWithDelim(ACBrCTe.Configuracoes.Arquivos.GetPathInu()) + ArqInut;
          WriteToTxt(ArqInut, APathXML, False, False);
          slAnexos.Add(ArqInut)
        end
        else
          slAnexos.Add(APathXML);

        if AEnviaPDF then
          slAnexos.Add(ArqPDF);

        try
          ACBrCTe.EnviarEmail(ADestinatario,
            IfThen(NaoEstaVazio(AAssunto), AAssunto, sAssunto),
            slMensagemEmail,
            slCC,      // Lista com emails que serão enviado cópias - TStrings
            slAnexos,  // Lista de slAnexos - TStrings
            nil,
            '',
            slReplay); // Lista de slreplay - TStrings

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

{ TMetodoValidarRegrasNegocios }

{ Params: 0 - XML - String com path XML
}
procedure TMetodoValidarRegrasNegocios.Executar;
var
  CargaDFe: TACBrCarregarCTe;
  AXML, ErrosRegraNegocio: string;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    ACBrCTe.Conhecimentos.Clear;
    CargaDFe := TACBrCarregarCTe.Create(ACBrCTe, AXML);
    try
      ACBrCTe.Conhecimentos.ValidarRegrasdeNegocios(ErrosRegraNegocio);

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
  TipodeDACTE: TpcnTipoImpressao;
  Ok: Boolean;
  TpImp: string;
begin
  TpImp := fpCmd.Params(0);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    OK := False;

    if (TpImp = '1') or (TpImp = '2') then
      TipodeDACTE := StrToTpImp(OK, tpImp);

    if not OK then
      raise Exception.Create('Tipo Impressão Inválido: ' + tpImp);

    ACBrCTe.DACTE.TipoDACTE := TipodeDACTE;
  end;
end;

{ TMetodoDistribuicaoDFe }

{ Params: 0 - Código da UF do autor da consulta
          1 - CNPJ do autor da consulta
          2 - Numero do último NSU retornado na consulta anterior
          3 - Numero do NSU a ser consultado
          4 - Chave da DF-e que se deseja baixar o XML
          5 - Arquivo XML de Distribuição para leitura
}
procedure TMetodoDistribuicaoDFe.Executar;
var
  AUF: Integer;
  ACNPJ: String;
  AUltNSU: String;
  ANSU: String;
  AChave: String;
  AArquivoOuXML: String;
  Resp: TDistribuicaoDFeResposta;
begin
  AUF := StrToIntDef(fpCmd.Params(0), 0);
  ACNPJ := fpCmd.Params(1);
  AUltNSU := fpCmd.Params(2);
  ANSU := fpCmd.Params(3);
  AChave := fpCmd.Params(4);
  AArquivoOuXML := fpCmd.Params(5);

  with TACBrObjetoCTe(fpObjetoDono) do
  begin
    if AArquivoOuXML <> '' then
    begin
      if not FileExists(AArquivoOuXML) then
        Raise Exception.Create('Arquivo não encontrado ou inacessível [' + AArquivoOuXML + '] ');
    end
    else
    begin
      if not( ValidarCNPJouCPF(ACNPJ) ) then
        raise Exception.Create('CNPJ/CPF ' + ACNPJ + ' inválido.');
    end;

    // Lê o arquivo selecionado
    if AArquivoOuXml <> '' then
    begin
      ACBrCTe.WebServices.DistribuicaoDFe.ListaArqs.Clear;
      ACBrCTe.WebServices.DistribuicaoDFe.Clear;
      ACBrCTe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Clear;

      ACBrCTe.WebServices.DistribuicaoDFe.retDistDFeInt.Leitor.CarregarArquivo(AArquivoOuXml);
      ACBrCTe.WebServices.DistribuicaoDFe.retDistDFeInt.LerXml;

      // Preenche a lista de arquivos extraídos da distribuição, pois a leitura não gera os arquivos individuais
      while ACBrCTe.WebServices.DistribuicaoDFe.ListaArqs.Count <
            ACBrCTe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count do
        ACBrCTe.WebServices.DistribuicaoDFe.ListaArqs.Add('');

      AultNSU := ACBrCTe.WebServices.DistribuicaoDFe.retDistDFeInt.ultNSU;
    end
    // Consulta o WebService
    else
      ACBrCTe.DistribuicaoDFe(AUF, ACNPJ, AUltNSU, ANSU, AChave);

    Resp:= TDistribuicaoDFeResposta.Create(TpResp, codUTF8);
    try
      Resp.Processar(ACBrCTe.WebServices.DistribuicaoDFe.retDistDFeInt,
                     ACBrCTe.WebServices.DistribuicaoDFe.Msg,
                     ACBrCTe.WebServices.DistribuicaoDFe.NomeArq,
                     ACBrCTe.WebServices.DistribuicaoDFe.ListaArqs);
      fpCmd.Resposta:= fpCmd.Resposta + sLineBreak + Resp.Gerar ;

    finally
      Resp.Free;
    end;

  end;
end;

end.
