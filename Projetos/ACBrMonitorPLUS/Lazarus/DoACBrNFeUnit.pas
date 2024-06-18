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

unit DoACBrNFeUnit;

interface

uses
  Classes, SysUtils, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.Math,
  ACBrLibNFeRespostas, ACBrNFe, ACBrMonitorConfig,
  ACBrMonitorConsts, ACBrDFeUtil, UtilUnit, DoACBrDFeUnit,
  CmdUnit, ACBrNFeDANFeRLClass, ACBrPosPrinter, ACBrNFeDANFeESCPOS,
  ACBrLibConsultaCadastro;

const
  cHOM_MSG = 'NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';

type

{ TACBrObjetoNFe }

TACBrObjetoNFe = class(TACBrObjetoDFe)
private
  fACBrNFe: TACBrNFe;
public
  constructor Create(AConfig: TMonitorConfig; ACBrNFe: TACBrNFe); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  function GerarNFeIni(XML: string): string;
  procedure RespostaImpressao(pImprimir: Boolean; pImpressora: String;
            pPreview: String; pCopias: Integer; pPDF: Boolean);
  Procedure LerIniNFe(ArqINI: String);
  procedure ImprimirNFe(pImpressora: String; pPreview: String; pCopias: Integer; pPDF: Boolean);
  procedure RespostaIntegrador;

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
  DateUtils, Forms, strutils,
  ACBrDFeConfiguracoes, ACBrNFeDANFEClass,
  ACBrLibResposta, ACBrLibDistribuicaoDFe, ACBrLibConsReciDFe,
  pcnConversao, pcnConversaoNFe,
  pcnAuxiliar, pcnNFeRTXT, pcnNFe, DoACBrUnit, ACBrDFeSSL;

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

  // DoACBrUnit
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
  ListaDeMetodos.Add(CMetodoSetTimeZone);

end;

procedure TACBrObjetoNFe.Executar(ACmd: TACBrCmd);
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
    46 : AMetodoClass := TMetodoDistribuicaoDFeporNSU;
    47 : AMetodoClass := TMetodoDistribuicaoDFeporUltNSU;
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
      RespostaIntegrador;
      Ametodo.Free;
    end;
  end;
end;

procedure TACBrObjetoNFe.RespostaImpressao(pImprimir: Boolean; pImpressora: String;
          pPreview: String; pCopias: Integer; pPDF: Boolean);
var
  I, J: integer;
  ArqPDF: String;
begin
  with fACBrNFe do
  begin
    for I := 0 to WebServices.Retorno.NFeRetorno.ProtDFe.Count - 1 do
    begin
      for J := 0 to NotasFiscais.Count - 1 do
      begin
        if ('NFe' + WebServices.Retorno.NFeRetorno.ProtDFe.Items[I].chDFe =
          NotasFiscais.Items[J].NFe.infNFe.Id) then
        begin
          //RespostaItensNFe(J, I, True);

          //Informa Mensagem de Sem Valor Fiscal para documentos emitidos em Homologação
          if NotasFiscais.Items[J].NFe.Ide.tpAmb = taHomologacao then
            NotasFiscais.Items[J].NFe.Dest.xNome:= cHOM_MSG;
          if (NotasFiscais.Items[J].NFe.ide.modelo = 65)
             and (NotasFiscais.Items[J].NFe.Ide.tpAmb = taHomologacao)
             and ( NotasFiscais.Items[J].NFe.Det[0].Prod.nItem = 1) then
                 NotasFiscais.Items[J].NFe.Det[0].Prod.xProd:= cHOM_MSG;

          {if not( TpResp in [resJSON, resXML]) then
            fpCmd.Resposta :=  fpCmd.Resposta + sLineBreak +'[NFe_Arq' + Trim(IntToStr(
                         fACBrNFe.NotasFiscais.Items[J].NFe.Ide.nNF)) +']' + sLineBreak +
                         'Arquivo=' + fACBrNFe.NotasFiscais.Items[J].NomeArq;}

          if (NotasFiscais.Items[J].Confirmada) and (pPDF) then
          begin
            DoConfiguraDANFe(pPDF, Trim(pPreview) );
            NotasFiscais.Items[J].ImprimirPDF;
            ArqPDF := OnlyNumber(ACBrNFe.NotasFiscais.Items[J].NFe.infNFe.ID)+'-nfe.pdf';

            if not( TpResp in [resJSON, resXML]) then
              fpCmd.Resposta :=  fpCmd.Resposta + sLineBreak +
              'PDF='+ PathWithDelim(ACBrNFe.DANFE.PathPDF) + ArqPDF + sLineBreak;
          end;

          if ( pImprimir or StrToBoolDef( pPreview, False ) or pPDF ) then
            DoConfiguraDANFe(False, Trim(pPreview) );

          if NaoEstaVazio(pImpressora) then
            DANFe.Impressora := pImpressora;

          if pCopias > 0 then
            DANFE.NumCopias := pCopias;

          if (NotasFiscais.Items[J].Confirmada) and (pImprimir) then
          begin
            try
              DoAntesDeImprimir(( StrToBoolDef( pPreview, False ) ) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
              NotasFiscais.Items[J].Imprimir;
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

procedure TACBrObjetoNFe.LerIniNFe(ArqINI: String);
var
  AXML: String;
  CargaDFe: TACBrCarregarNFe;
begin
  AXML:= '';
  with fACBrNFe do
  begin
    NotasFiscais.Clear;
    NotasFiscais.LoadFromIni( ArqINI );

    //Campos preenchidos em tela
    if (NotasFiscais.Count > 0) and
       ( NaoEstaVazio(MonitorConfig.DFE.WebService.NFe.CNPJContador) ) then
    begin
      with NotasFiscais.Items[0].NFe.autXML.New do
        CNPJCPF := OnlyNumber( MonitorConfig.DFE.WebService.NFe.CNPJContador );
      NotasFiscais.Items[0].GerarXML;
    end;

    //Deve recarregar os dados do XML validado pelo componente
    AXML := NotasFiscais.Items[0].XMLOriginal;
    NotasFiscais.Clear;
    CargaDFe := TACBrCarregarNFe.Create(fACBrNFe, AXML, False);
    try
      if (NotasFiscais.Count = 0) then
        Raise Exception.Create('Nenhuma NFe gerada no componente!');
    finally
      CargaDFe.Free;
    end;

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

      //Informa Mensagem de Sem Valor Fiscal para documentos emitidos em Homologação
      if NotasFiscais.Items[0].NFe.Ide.tpAmb = taHomologacao then
        NotasFiscais.Items[0].NFe.Dest.xNome:= cHOM_MSG;
      if (NotasFiscais.Items[0].NFe.ide.modelo = 65)
         and (NotasFiscais.Items[0].NFe.Ide.tpAmb = taHomologacao)
         and ( NotasFiscais.Items[0].NFe.Det[0].Prod.nItem = 1) then
             NotasFiscais.Items[0].NFe.Det[0].Prod.xProd:= cHOM_MSG;

      DoConfiguraDANFe(pPDF, Trim(pPreview) );
      if NaoEstaVazio(pImpressora) then
        DANFe.Impressora := pImpressora;

      if pCopias > 0 then
        DANFE.NumCopias := pCopias;

      if pPDF then
      begin
        NotasFiscais.Items[0].ImprimirPDF;
        ArqPDF := OnlyNumber(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID)+'-nfe.pdf';

        if not(TpResp in [resJSON, resXML]) then
          fpCmd.Resposta :=  fpCmd.Resposta + sLineBreak +
                'PDF='+ PathWithDelim(ACBrNFe.DANFE.PathPDF) + ArqPDF + sLineBreak ;
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
begin
  fACBrNFe.NotasFiscais.Clear;
  if FilesExists(XML) then
    fACBrNFe.NotasFiscais.LoadFromFile(XML)
  else
  begin
    fACBrNFe.NotasFiscais.LoadFromString(ConvertStrRecived(XML));
    fACBrNFe.NotasFiscais.GerarNFe;
  end;

  Result := fACBrNFe.NotasFiscais.GerarIni();
  WriteToTxt('NFe.ini', Result);
end;

procedure TACBrObjetoNFe.RespostaIntegrador;
begin
  with fACBrNFe do
    fpCmd.Resposta := fpCmd.Resposta + DoRespostaIntegrador();

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
      fpCmd.Resposta := sLineBreak + GerarNFeIni(AXML);
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
          1 - NFCe: 1 para atualizar Logo da NFCe
}
procedure TMetodoSetLogoMarca.Executar;
var
  ALogo: String;
  ANFCe: Boolean;
begin
  ALogo := fpCmd.Params(0);
  ANFCe := StrToBoolDef( fpCmd.Params(1), False);

  if (( ALogo <> EmptyStr) and (not FileExists(ALogo)) ) then
    raise Exception.Create('Arquivo não encontrado.');

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.Impressao.Geral do
    begin
      if not(ANFCe) then
        Logomarca := ALogo
      else
        LogoMarcaNFCeSAT := ALogo ;
    end;

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
  Resp: TConsultaCadastroResposta;
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
    Resp := TConsultaCadastroResposta.Create(TpResp, codUTF8);
    try
      Resp.Processar(ACBrNFe.WebServices.ConsultaCadastro.RetConsCad);
      fpCmd.Resposta:= Resp.Msg + sLineBreak + Resp.Gerar ;

    finally
      Resp.Free;
    end;

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
  Resposta : TInutilizarNFeResposta;
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
    Resposta := TInutilizarNFeResposta.Create(TpResp, codUTF8);
    try
      Resposta.Processar(ACBrNFe);
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
          6 - Replay: String ReplayTo (Separados ;)
}
procedure TMetodoEnviarEmail.Executar;
var
  sAssunto, ADestinatario, APathXML, AAssunto, AEmailCopias, AAnexos, AReplay: string;
  slMensagemEmail, slCC, slAnexos, slReplay: TStringList;
  CargaDFe: TACBrCarregarNFe;
  AEnviaPDF: Boolean;
begin
  ADestinatario := fpCmd.Params(0);
  APathXML := fpCmd.Params(1);
  AEnviaPDF := StrToBoolDef(fpCmd.Params(2), False);
  AAssunto := fpCmd.Params(3);
  AEmailCopias := fpCmd.Params(4);
  AAnexos := fpCmd.Params(5);
  AReplay := fpCmd.Params(6);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.NotasFiscais.Clear;

    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    slReplay := TStringList.Create;
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
        QuebrarLinha(AReplay, slReplay);

        try
          ACBrNFe.NotasFiscais.Items[0].EnviarEmail(ADestinatario,
            DoSubstituirVariaveis( IfThen( NaoEstaVazio(AAssunto), AAssunto, sAssunto) ),
            slMensagemEmail,
            AEnviaPDF,
            // Enviar PDF junto
            slCC,
            // Lista com emails que serão enviado cópias - TStrings
            slAnexos,
            // Lista de slAnexos - TStrings
            slReplay);
            // Lista de ReplayTo - TStrings

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

{ TMetodoReciboNFe }

{ Params: 0 - Recibo - String com Numero Recibo para consulta
}
procedure TMetodoReciboNFe.Executar;
var
  ARecibo: String;
  RespRetorno: TReciboResposta;
begin
  ARecibo := fpCmd.Params(0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.WebServices.Recibo.Recibo := ARecibo;
    DoValidarIntegradorNFCe();

    ACBrNFe.WebServices.Recibo.Executar;
    RespRetorno := TReciboResposta.Create('NFe', TpResp, codUTF8);
    try

      RespRetorno.Processar(ACBrNFe.WebServices.Recibo.NFeRetorno,
                            ACBrNFe.WebServices.Recibo.Recibo);
      fpCmd.Resposta := RespRetorno.XMotivo + sLineBreak + RespRetorno.Gerar;
      if ACBrNFe.Configuracoes.Geral.Salvar then
        fpCmd.Resposta := fpCmd.Resposta + sLineBreak + 'Arquivo=' + ACBrNFe.Configuracoes.Arquivos.PathSalvar +
                      ARecibo + '-pro-rec.xml';
    finally
      RespRetorno.Free;
    end;

  end;
end;

{ TMetodoConsultarNFe }

{ Params: 0 - XML - Uma String com um Path completo XML ou chave NFe
          1 - AExtrairEventos (1 para extrair)
}
procedure TMetodoConsultarNFe.Executar;
var
  CargaDFe: TACBrCarregarNFe;
  AXML: String;
  Resposta: TConsultaNFeResposta;
  AExtrairEventos: Boolean;
begin
  AXML := fpCmd.Params(0);
  AExtrairEventos := StrToBoolDef(fpCmd.Params(1), False);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.NotasFiscais.Clear;
    CargaDFe := TACBrCarregarNFe.Create(ACBrNFe, AXML, False);
    try
      ACBrNFe.WebServices.Consulta.ExtrairEventos := AExtrairEventos;

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
      Resposta := TConsultaNFeResposta.Create(TpResp, codUTF8);
      try
        Resposta.Processar(ACBrNFe);
        fpCmd.Resposta := Resposta.Msg + sLineBreak + Resposta.Gerar;

        if (ACBrNFe.NotasFiscais.Count > 0) then
        begin
          if NaoEstaVazio(ACBrNFe.NotasFiscais.Items[0].NomeArq) then
            fpCmd.Resposta := fpCmd.Resposta + sLineBreak + 'Arquivo=' + ACBrNFe.NotasFiscais.Items[0].NomeArq;
        end
        else if ACBrNFe.Configuracoes.Geral.Salvar then
            fpCmd.Resposta := fpCmd.Resposta + sLineBreak + 'Arquivo='
            + PathWithDelim(ACBrNFe.Configuracoes.Arquivos.PathSalvar) + AXML + '-sit.xml';

      finally
        Resposta.Free;
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
        fpCmd.Resposta := PathWithDelim(ACBrNFe.Configuracoes.Arquivos.GetPathNFe())
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
var
  Resposta: TStatusServicoResposta;
begin
  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    DoValidarIntegradorNFCe();
    ACBrNFe.WebServices.StatusServico.Executar;
    Resposta := TStatusServicoResposta.Create(TpResp, codUTF8);
    try
      Resposta.Processar(ACBrNFe);
      fpCmd.Resposta := Resposta.Msg + sLineBreak + Resposta.Gerar;
    finally
      Resposta.Free;
    end;

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
        ArqPDF := ACBrNFe.DANFE.ArquivoPDF;
        if not FileExists(ArqPDF) then
           raise Exception.Create('Arquivo ' + ArqPDF + ' não encontrado.');

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

      if (ACBrNFe.NotasFiscais.Items[0].NFe.Ide.modelo = CModeloNFe55) then
      begin
        if NaoEstaVazio(MarcaDagua) then
          TACBrNFeDANFeRL(ACBrNFe.DANFE).MarcadAgua:= MarcaDagua
        else
          TACBrNFeDANFeRL(ACBrNFe.DANFE).MarcadAgua:= '';
      end;

      ACBrNFe.DANFe.Protocolo :=  trim( AProtocolo );

      if ACBrNFe.NotasFiscais.Items[0].NFe.Ide.modelo = CModeloNFe65 then
      begin
        TACBrNFeDANFCEClass(ACBrNFe.DANFE).ViaConsumidor := Consumidor;
      end;

      if Simplificado and (ACBrNFe.DANFE is TACBrNFeDANFEClass)  then
        TACBrNFeDANFEClass(ACBrNFe.DANFE).TipoDANFE := tiSimplificado;

      try
        ACBrNFe.NotasFiscais.ImprimirPDF;
        ArqPDF := ACBrNFe.DANFE.ArquivoPDF;
        if not FileExists(ArqPDF) then
           raise Exception.Create('Arquivo ' + ArqPDF + ' não encontrado.');

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
  //RespEnvio: TEnvioResposta;
  //RespRetorno: TRetornoResposta;
  RespEnvioRetorno: TEnvioRetornoResposta;
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

    if TpResp in [resINI] then
      fpCmd.Resposta := Resp + sLineBreak;

    ACBrNFe.WebServices.Enviar.Sincrono := ASincrono;

    if (ALote = 0) then
      ACBrNFe.WebServices.Enviar.Lote := '1'
    else
      ACBrNFe.WebServices.Enviar.Lote := IntToStr(ALote);

    if ACBrNFe.NotasFiscais.Count > 0 then
      DoValidarIntegradorNFCe(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID);

    ACBrNFe.WebServices.Enviar.Executar;

    if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
    begin
      ACBrNFe.WebServices.Retorno.Recibo := ACBrNFe.WebServices.Enviar.Recibo;
      ACBrNFe.WebServices.Retorno.Executar;
    end;

    RespEnvioRetorno := TEnvioRetornoResposta.Create(TpResp, codUTF8);
    try
      RespEnvioRetorno.Processar(ACBrNFe);
      fpCmd.Resposta := fpCmd.Resposta + RespEnvioRetorno.Gerar;

    finally
      RespEnvioRetorno.Free;
    end;

    if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
      RespostaImpressao(AImprime, AImpressora, APreview, ACopias, APDF)
    else
    if AImprime then //Sincrono
       ImprimirNFe(AImpressora, APreview, ACopias, APDF);


    {RespEnvio := TEnvioResposta.Create( TpResp, codUTF8);
    try
       RespEnvio.Processar(ACBrNFe);
       if TpResp in [resJSON, resXML] then
         fpCmd.Resposta := sLineBreak + RespEnvio.Gerar
       else
         fpCmd.Resposta := fpCmd.Resposta + RespEnvio.Msg + sLineBreak + RespEnvio.Gerar;
    finally
       RespEnvio.Free;
    end;}

    {if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
    begin

       RespRetorno := TRetornoResposta.Create('NFe', TpResp, codUTF8);
       try
         RespRetorno.Processar(ACBrNFe.WebServices.Retorno.NFeRetorno,
                               ACBrNFe.WebServices.Retorno.Recibo,
                               ACBrNFe.WebServices.Retorno.Msg,
                               ACBrNFe.WebServices.Retorno.Protocolo,
                               ACBrNFe.WebServices.Retorno.ChaveNFe);
         if TpResp in [resJSON, resXML] then
           fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespRetorno.Gerar
         else
           fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespRetorno.Msg
                        + sLineBreak + RespRetorno.Gerar;
       finally
         RespRetorno.Free;
       end;
       RespostaImpressao(AImprime, AImpressora, APreview, ACopias, APDF);

    end
    else
    if AImprime then //Sincrono
       ImprimirNFe(AImpressora, APreview, ACopias, APDF);  }

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
  //RespEnvio: TEnvioResposta;
  //RespRetorno: TRetornoResposta;
  RespEnvioRetorno: TEnvioRetornoResposta;
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

    if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
    begin
      ACBrNFe.WebServices.Retorno.Recibo := ACBrNFe.WebServices.Enviar.Recibo;
      ACBrNFe.WebServices.Retorno.Executar;
    end;

    RespEnvioRetorno := TEnvioRetornoResposta.Create(TpResp, codUTF8);
    try
      RespEnvioRetorno.Processar(ACBrNFe);
      fpCmd.Resposta :=  RespEnvioRetorno.Gerar;

    finally
      RespEnvioRetorno.Free;
    end;

    if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
      RespostaImpressao(AImprime, AImpressora, APreview, ACopias, APDF)
    else
    if AImprime then //Sincrono
       ImprimirNFe(AImpressora, APreview, ACopias, APDF);


    {RespEnvio := TEnvioResposta.Create(TpResp, codUTF8);
    try
      RespEnvio.Processar(ACBrNFe);
      fpCmd.Resposta := RespEnvio.Msg + sLineBreak + RespEnvio.Gerar;
    finally
      RespEnvio.Free;
    end;
    if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
    begin
      ACBrNFe.WebServices.Retorno.Recibo := ACBrNFe.WebServices.Enviar.Recibo;
      ACBrNFe.WebServices.Retorno.Executar;
      RespRetorno := TRetornoResposta.Create('NFe', TpResp, codUTF8);
      try
         RespRetorno.Processar(ACBrNFe.WebServices.Retorno.NFeRetorno,
                               ACBrNFe.WebServices.Retorno.Recibo,
                               ACBrNFe.WebServices.Retorno.Msg,
                               ACBrNFe.WebServices.Retorno.Protocolo,
                               ACBrNFe.WebServices.Retorno.ChaveNFe);
         fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespRetorno.Msg
                        + sLineBreak + RespRetorno.Gerar;
      finally
         RespRetorno.Free;
      end;
      RespostaImpressao(AImprime, AImpressora, APreview, ACopias, APDF);
    end
    else
    if AImprime then //Sincrono
      ImprimirNFe(AImpressora, APreview, ACopias, APDF);  }

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
  //RespEnvio: TEnvioResposta;
  //RespRetorno: TRetornoResposta;
  RespEnvioRetorno: TEnvioRetornoResposta;
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

      if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
      begin
        ACBrNFe.WebServices.Retorno.Recibo := ACBrNFe.WebServices.Enviar.Recibo;
        ACBrNFe.WebServices.Retorno.Executar;
      end;

      RespEnvioRetorno := TEnvioRetornoResposta.Create(TpResp, codUTF8);
      try
        RespEnvioRetorno.Processar(ACBrNFe);
        fpCmd.Resposta := sLineBreak + RespEnvioRetorno.Gerar;

      finally
        RespEnvioRetorno.Free;
      end;

      if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
        RespostaImpressao(AImprime, AImpressora, '', 0, False)
      else
      if AImprime then //Sincrono
         ImprimirNFe(AImpressora, '', 0, False);


      {RespEnvio := TEnvioResposta.Create(TpResp, codUTF8);
      try
        RespEnvio.Processar(ACBrNFe);
        if TpResp in [resJSON, resXML] then
          fpCmd.Resposta := sLineBreak + RespEnvio.Gerar
        else
          fpCmd.Resposta := RespEnvio.Msg + sLineBreak + RespEnvio.Gerar;
      finally
        RespEnvio.Free;
      end;
      if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
      begin
        ACBrNFe.WebServices.Retorno.Recibo := ACBrNFe.WebServices.Enviar.Recibo;
        ACBrNFe.WebServices.Retorno.Executar;
        RespRetorno := TRetornoResposta.Create('NFe', TpResp, codUTF8);
        try
           RespRetorno.Processar(ACBrNFe.WebServices.Retorno.NFeRetorno,
                                 ACBrNFe.WebServices.Retorno.Recibo,
                                 ACBrNFe.WebServices.Retorno.Msg,
                                 ACBrNFe.WebServices.Retorno.Protocolo,
                                 ACBrNFe.WebServices.Retorno.ChaveNFe);
           if TpResp in [resJSON, resXML] then
             fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespRetorno.Gerar
           else
             fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespRetorno.Msg
                          + sLineBreak + RespRetorno.Gerar;
        finally
           RespRetorno.Free;
        end;
        RespostaImpressao(AImprime, AImpressora, '' , 0, False);
      end
      else
      if AImprime then //Sincrono
        ImprimirNFe(AImpressora, '', 0, False);}

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

      ACBrNFe.DANFe.Protocolo := trim( AProtocolo );

      if (ACBrNFe.NotasFiscais.Items[0].NFe.Ide.modelo = 55) then
      begin
        if NaoEstaVazio(AMarcaDagua) then
          TACBrNFeDANFeRL(ACBrNFe.DANFE).MarcadAgua:= AMarcaDagua
        else
          TACBrNFeDANFeRL(ACBrNFe.DANFE).MarcadAgua:= '';
      end;

      if ACBrNFe.NotasFiscais.Items[0].NFe.Ide.modelo = 65 then
      begin
        TACBrNFeDANFCEClass(ACBrNFe.DANFE).ViaConsumidor := AConsumidor;
      end;

      if ASimplificado and (ACBrNFe.DANFE is TACBrNFeDANFEClass) then
          TACBrNFeDANFEClass(ACBrNFe.DANFE).TipoDANFE := tiSimplificado;

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
  Resposta: TCancelamentoResposta;
begin
  AChave := fpCmd.Params(0);
  AJustificativa := fpCmd.Params(1);
  ACNPJ := fpCmd.Params(2);
  ALote := StrToIntDef(fpCmd.Params(3), 1);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.WebServices.Consulta.Clear;
    ACBrNFe.NotasFiscais.Clear;

    if not ValidarChave(AChave) then
      raise Exception.Create('Chave ' + AChave + ' inválida.')
    else
      ACBrNFe.WebServices.Consulta.NFeChave := AChave;

    ACBrNFe.WebServices.Consulta.Executar;
    if EstaVazio(ACBrNFe.WebServices.Consulta.Protocolo) then
      raise Exception.Create(ACBrStr('Não foi possível consultar o número de Protocolo para a Chave: ') + AChave );

    ACBrNFe.EventoNFe.Evento.Clear;
    with ACBrNFe.EventoNFe.Evento.New do
    begin
      infEvento.CNPJ := ACNPJ;
      if Trim(infEvento.CNPJ) = '' then
        infEvento.CNPJ := copy(OnlyNumber(ACBrNFe.WebServices.Consulta.NFeChave), 7, 14)
      else
      begin
        if not( ValidarCNPJouCPF(ACNPJ) ) then
          raise Exception.Create('CNPJ/CPF ' + ACNPJ + ' inválido.');
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
    Resposta := TCancelamentoResposta.Create(TpResp, codUTF8);
    try
      Resposta.Processar(ACBrNFe);
      fpCmd.Resposta := Resposta.XMotivo + sLineBreak + Resposta.Gerar;
    finally
      Resposta.Free;
    end;

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
    fpCmd.Resposta := ACBrNFe.SSL.CertCNPJ
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
  DanfeEscPos: TACBrNFeDANFeESCPOS;
  POSPrinter: TACBrPosPrinter;
begin
  AXMLInut := fpCmd.Params(0);
  AImpressora := fpCmd.Params(1);
  ACopias := StrToIntDef(fpCmd.Params(2), 0);
  APreview:= fpCmd.Params(3);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.InutNFe.ID := '';
    CargaDFeInut := TACBrCarregarNFeInut.Create(ACBrNFe, AXMLInut);
    try
      if (ACBrNFe.InutNFe.modelo = CModeloNFe65) and
        (MonitorConfig.DFE.Impressao.NFCe.Emissao.Modelo = CEmissaoESCPOS ) then
      begin
        DanfeEscPos := TACBrNFeDANFeESCPOS.Create(ACBrNFe.DANFE);
        POSPrinter  := TACBrPosPrinter.Create(DanfeEscPos.PosPrinter);
        try

          PosPrinter.Device.Porta := MonitorConfig.PosPrinter.Porta;
          PosPrinter.Modelo := TACBrPosPrinterModelo(MonitorConfig.PosPrinter.Modelo);
          POSPrinter.LinhasEntreCupons:= MonitorConfig.PosPrinter.LinhasPular;
          PosPrinter.ColunasFonteNormal := MonitorConfig.PosPrinter.Colunas;
          PosPrinter.CortaPapel := MonitorConfig.PosPrinter.CortarPapel;
          POSPrinter.EspacoEntreLinhas := MonitorConfig.PosPrinter.EspacoEntreLinhas;
          POSPrinter.LinhasBuffer:= MonitorConfig.PosPrinter.LinhasBuffer;
          POSPrinter.TraduzirTags:= MonitorConfig.PosPrinter.TraduzirTags;
          Posprinter.IgnorarTags:= MonitorConfig.PosPrinter.IgnorarTags;
          PosPrinter.PaginaDeCodigo:= TACBrPosPaginaCodigo( MonitorConfig.PosPrinter.PaginaDeCodigo );

          DanfeEscPos.PosPrinter:= POSPrinter;
          ACBrNFe.DANFE := DanfeEscPos;

          if not PosPrinter.ControlePorta then
          begin
            PosPrinter.Ativar;
            if not PosPrinter.Device.Ativo then
              PosPrinter.Device.Ativar;
          end;

          if (ACopias > 0) then
            ACBrNFe.DANFe.NumCopias := ACopias;

          ACBrNFe.ImprimirInutilizacao;

        finally
          DanfeEscPos.Free;
          POSPrinter.Free;
        end;
      end
      else
      begin
        DanfeRL:= TACBrNFeDANFeRL.Create(ACBrNFe);
        try
          if ACBrNFe.NotasFiscais.Count > 0 then
            ACBrNFe.NotasFiscais.Clear;
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
          DanfeRL.Free;
        end;

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
  CargaDFeInut: TACBrCarregarNFeInut;
  AXMLInut, ArqPDF: String;
  DanfeRL: TACBrNFeDANFeRL;
begin
  AXMLInut := fpCmd.Params(0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.InutNFe.ID := '';
    if ACBrNFe.NotasFiscais.Count > 0 then
      ACBrNFe.NotasFiscais.Clear;
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

{ Params: 0 - Data: TDateTime - Data para geração do path
          1 - CNPJ: String - CNPJ para geração do path
          2 - IE: String - IE para geração do path
          3 - Modelo: Integer -  Modelo para geração
}
procedure TMetodoGetPathNFe.Executar;
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

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrNFe.Configuracoes.Arquivos.GetPathNFe(AData, ACNPJ, AIE, AModelo);
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

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrNFe.Configuracoes.Arquivos.GetPathEvento(teCCe, ACNPJ, AIE, AData);
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

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrNFe.Configuracoes.Arquivos.GetPathEvento(teCancelamento, ACNPJ, AIE, AData);
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

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrNFe.Configuracoes.Arquivos.GetPathEvento(StrToTpEventoNFe(ok ,CodEvento), ACNPJ, AIE, AData);
  end;
end;

{ TMetodoGetPathInu }

{ Params:
          0 - CNPJ: String - CNPJ para geração do path
          1 - IE: String - IE para geração do path
}
procedure TMetodoGetPathInu.Executar;
var
  ACNPJ: String;
  AIE: String;
begin
  ACNPJ:= fpCmd.Params(0);
  AIE:= fpCmd.Params(1);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrNFe.Configuracoes.Arquivos.GetPathInu( ACNPJ, AIE );
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

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.EventoNFe.Evento.Clear;

    ACBrNFe.EventoNFe.LerFromIni( AArq, False );

    if ACBrNFe.EventoNFe.Evento.Count > 0 then
      DoValidarIntegradorNFCe(ACBrNFe.EventoNFe.Evento.Items[0].InfEvento.chNFe);

    ACBrNFe.EnviarEvento(ACBrNFe.EventoNFe.idLote);
    Resp := TEventoResposta.Create(TpResp, codUTF8);
    try
      Resp.Processar(ACBrNFe);
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

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.EventoNFe.Evento.Clear;

    ACBrNFe.EventoNFe.LerFromIni( AArq, True );

    if ACBrNFe.EventoNFe.Evento.Count > 0 then
      DoValidarIntegradorNFCe(ACBrNFe.EventoNFe.Evento.Items[0].InfEvento.chNFe);
    ACBrNFe.EnviarEvento(ACBrNFe.EventoNFe.idLote);
    Resp := TEventoResposta.Create(TpResp, codUTF8);
    try
      Resp.Processar(ACBrNFe);
      fpCmd.Resposta:= sLineBreak + Resp.Gerar ;

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
  CargaDFeEvento: TACBrCarregarNFeEvento;
  Resp : TEventoResposta;
begin
  AArq := fpCmd.Params(0);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.EventoNFe.Evento.Clear;
    CargaDFeEvento := TACBrCarregarNFeEvento.Create(ACBrNFe, AArq);
    Resp := TEventoResposta.Create(TpResp, codUTF8);
    try
      if ACBrNFe.EventoNFe.Evento.Count > 0 then
        DoValidarIntegradorNFCe(ACBrNFe.EventoNFe.Evento.Items[0].InfEvento.chNFe);

      ACBrNFe.EnviarEvento(ACBrNFe.EventoNFe.idLote);
      Resp.Processar(ACBrNFe);
      fpCmd.Resposta:= sLineBreak + Resp.Gerar ;


    finally
      CargaDFeEvento.Free;
      Resp.Free
    end;

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
  Resp: TDistribuicaoDFeResposta;
begin
  AUF := StrToIntDef(fpCmd.Params(0), 0);
  ACNPJ := fpCmd.Params(1);
  AChave := fpCmd.Params(2);

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    if not( ValidarCNPJouCPF(ACNPJ) ) then
      raise Exception.Create('CNPJ/CPF '+ACNPJ+' inválido.');

    DoValidarIntegradorNFCe();
    ACBrNFe.DistribuicaoDFePorChaveNFe(AUF, ACNPJ, AChave);
    Resp:= TDistribuicaoDFeResposta.Create(TpResp, codUTF8);
    try
      Resp.Processar(ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt,
                     ACBrNFe.WebServices.DistribuicaoDFe.Msg,
                     ACBrNFe.WebServices.DistribuicaoDFe.NomeArq,
                     ACBrNFe.WebServices.DistribuicaoDFe.ListaArqs);
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

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    if not( ValidarCNPJouCPF(ACNPJ) ) then
      raise Exception.Create('CNPJ/CPF '+ACNPJ+' inválido.');

    DoValidarIntegradorNFCe();

    ACBrNFe.DistribuicaoDFePorUltNSU(AUF, ACNPJ, AUltNSU);
    Resp:= TDistribuicaoDFeResposta.Create(TpResp, codUTF8);
    try
      Resp.Processar(ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt,
                     ACBrNFe.WebServices.DistribuicaoDFe.Msg,
                     ACBrNFe.WebServices.DistribuicaoDFe.NomeArq,
                     ACBrNFe.WebServices.DistribuicaoDFe.ListaArqs);
      fpCmd.Resposta:= fpCmd.Resposta + sLineBreak + Resp.Gerar ;

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

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    if not( ValidarCNPJouCPF(ACNPJ) ) then
      raise Exception.Create('CNPJ/CPF '+ACNPJ+' inválido.');

    DoValidarIntegradorNFCe();
    ACBrNFe.DistribuicaoDFePorNSU(AUF, ACNPJ, ANSU);
    Resp:= TDistribuicaoDFeResposta.Create(TpResp, codUTF8);
    try
      Resp.Processar(ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt,
                     ACBrNFe.WebServices.DistribuicaoDFe.Msg,
                     ACBrNFe.WebServices.DistribuicaoDFe.NomeArq,
                     ACBrNFe.WebServices.DistribuicaoDFe.ListaArqs);
      fpCmd.Resposta:= fpCmd.Resposta + sLineBreak + Resp.Gerar ;
    finally
      Resp.Free;
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
          7 - Replay: String com Replay (Separados ;)
}
procedure TMetodoEnviaremailEvento.Executar;
var
  sAssunto, ADestinatario, APathXMLEvento, APathXML, AAssunto, AEmailCopias,
  AAnexos, ArqPDF, ArqEvento, AReplay: string;
  slMensagemEmail, slCC, slAnexos, slReplay: TStringList;
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
  AReplay := fpCmd.Params(7);
  ArqEvento := '';

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    ACBrNFe.EventoNFe.Evento.Clear;
    ACBrNFe.NotasFiscais.Clear;

    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    slReplay := TStringList.Create;
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
            ArqPDF := ACBrNFe.DANFE.ArquivoPDF;
            if not FileExists(ArqPDF) then
               raise Exception.Create('Arquivo ' + ArqPDF + ' não encontrado.');
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
        QuebrarLinha(AReplay, slReplay);

        // Se carregou evento usando XML como parâmetro, salva XML para poder anexar
        if  StringIsXML( APathXMLEvento ) then
        begin
          tipoEvento := ACBrNFe.EventoNFe.Evento[0].InfEvento.tpEvento;
          ArqEvento  := ACBrNFe.EventoNFe.ObterNomeArquivo(tipoEvento);
          ArqEvento  := PathWithDelim(ACBrNFe.Configuracoes.Arquivos.GetPathEvento(tipoEvento))+ArqEvento;
          WriteToTxt(ArqEvento, ACBrNFe.EventoNFe.Evento[0].RetInfEvento.XML, False, False);
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

{ TMetodoEnviaremailInutilizacao }

{ Params: 0 - Email: String com email Destinatário
          1 - XML: String com path do XML Inutilização
          2 - Boolean 1 : Envia PDF
          3 - Assunto: String com Assunto do e-mail
          4 - Copia: String com e-mails copia (Separados ;)
          5 - Anexo: String com Path de Anexos (Separados ;)
          6 - Replay: String com endereços de Replay (Separados ;)
}
procedure TMetodoEnviaremailInutilizacao.Executar;
var
  sAssunto, ADestinatario, APathXML, AAssunto, AEmailCopias,
  AAnexos, ArqPDF, ArqInut, AReplay: string;
  slMensagemEmail, slCC, slAnexos, slReplay: TStringList;
  CargaDFe: TACBrCarregarNFeInut;
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

  with TACBrObjetoNFe(fpObjetoDono) do
  begin
    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    slReplay := TStringList.Create;
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
        QuebrarLinha(AReplay, slReplay);

        // Se carregou evento usando XML como parâmetro, salva XML para poder anexar
        if  StringIsXML( APathXML ) then
        begin
          ArqInut  := ACBrNFe.InutNFe.ObterNomeArquivo;
          ArqInut  := PathWithDelim(ACBrNFe.Configuracoes.Arquivos.GetPathInu()) + ArqInut;
          WriteToTxt(ArqInut, APathXML, False, False);
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
    if (MonitorConfig.DFE.Impressao.NFCe.Emissao.Modelo <> CEmissaoESCPOS )  then
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
  //RespEnvio : TEnvioResposta;
  //RespRetorno : TRetornoResposta;
  RespEnvioRetorno: TEnvioRetornoResposta;
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

    if TpResp in [resINI] then
      fpCmd.Resposta := Resp + sLineBreak;

    if (ALote = 0) then
      ACBrNFe.WebServices.Enviar.Lote := '1'
    else
      ACBrNFe.WebServices.Enviar.Lote := IntToStr(ALote);

    ACBrNFe.WebServices.Enviar.Sincrono := ASincrono;

    if ACBrNFe.NotasFiscais.Count > 0 then
      DoValidarIntegradorNFCe(ACBrNFe.NotasFiscais.Items[0].NFe.infNFe.ID);

    ACBrNFe.WebServices.Enviar.Executar;

    if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
    begin
      ACBrNFe.WebServices.Retorno.Recibo := ACBrNFe.WebServices.Enviar.Recibo;
      ACBrNFe.WebServices.Retorno.Executar;
    end;

    RespEnvioRetorno := TEnvioRetornoResposta.Create(TpResp, codUTF8);
    try
      RespEnvioRetorno.Processar(ACBrNFe);
      fpCmd.Resposta :=  fpCmd.Resposta + RespEnvioRetorno.Gerar;

    finally
      RespEnvioRetorno.Free;
    end;

    if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
      RespostaImpressao(AImprime, AImpressora, APreview, ACopias, False)
    else
    if AImprime then //Sincrono
       ImprimirNFe(AImpressora, APreview, ACopias, False);


    {RespEnvio := TEnvioResposta.Create(TpResp, codUTF8);
    try
      RespEnvio.Processar(ACBrNFe);
      fpCmd.Resposta := fpCmd.Resposta + RespEnvio.Msg + sLineBreak + RespEnvio.Gerar;
    finally
      RespEnvio.Free;
    end;
    if (ACBrNFe.WebServices.Enviar.Recibo <> '') then //Assincrono
    begin
      ACBrNFe.WebServices.Retorno.Recibo := ACBrNFe.WebServices.Enviar.Recibo;
      ACBrNFe.WebServices.Retorno.Executar;
      RespRetorno := TRetornoResposta.Create('NFe', TpResp, codUTF8);
      try
         RespRetorno.Processar(ACBrNFe.WebServices.Retorno.NFeRetorno,
                               ACBrNFe.WebServices.Retorno.Recibo,
                               ACBrNFe.WebServices.Retorno.Msg,
                               ACBrNFe.WebServices.Retorno.Protocolo,
                               ACBrNFe.WebServices.Retorno.ChaveNFe);
         fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespRetorno.Msg
                        + sLineBreak + RespRetorno.Gerar;
      finally
         RespRetorno.Free;
      end;
      RespostaImpressao(AImprime, AImpressora, APreview, ACopias, False);
    end
    else
    if AImprime then //Sincrono
      ImprimirNFe(AImpressora, APreview, ACopias, False);     }

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

  with TACBrObjetoNFe(fpObjetoDono) do
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

    DoValidarIntegradorNFCe();

    // Lê o arquivo selecionado
    if AArquivoOuXml <> '' then
    begin
      ACBrNFe.WebServices.DistribuicaoDFe.ListaArqs.Clear;
      ACBrNFe.WebServices.DistribuicaoDFe.Clear;
      ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Clear;

      ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.Leitor.CarregarArquivo(AArquivoOuXml);
      ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.LerXml;

      // Preenche a lista de arquivos extraídos da distribuição, pois a leitura não gera os arquivos individuais
      while ACBrNFe.WebServices.DistribuicaoDFe.ListaArqs.Count <
            ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count do
        ACBrNFe.WebServices.DistribuicaoDFe.ListaArqs.Add('');

      AultNSU := ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt.ultNSU;
    end
    // Consulta o WebService
    else
      ACBrNFe.DistribuicaoDFe(AUF, ACNPJ, AUltNSU, ANSU, AChave);

    Resp:= TDistribuicaoDFeResposta.Create(TpResp, codUTF8);
    try
      Resp.Processar(ACBrNFe.WebServices.DistribuicaoDFe.retDistDFeInt,
                     ACBrNFe.WebServices.DistribuicaoDFe.Msg,
                     ACBrNFe.WebServices.DistribuicaoDFe.NomeArq,
                     ACBrNFe.WebServices.DistribuicaoDFe.ListaArqs);
      fpCmd.Resposta:= fpCmd.Resposta + sLineBreak + Resp.Gerar ;

    finally
      Resp.Free;
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
    if not (ACBrNFe.DANFE is TACBrNFeDANFEClass) then Exit;
    TACBrNFeDANFEClass(ACBrNFe.DANFE).TipoDANFE:= StrToTpImp( OK, IntToStr(TipoImp) );
    with MonitorConfig.DFE.Impressao.Geral do
      DANFE := TipoImp - 1;
    MonitorConfig.SalvarArquivo;

  end;

end;

end.
