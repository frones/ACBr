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

unit DoACBrGNReUnit;

interface

uses
  Classes, TypInfo, SysUtils, CmdUnit, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrGNRE2, pcnConversao, ACBrMonitorConsts, ACBrMonitorConfig, ACBrLibResposta, ACBrLibGNReRespostas,
  DoACBrDFeUnit;

type

{ TACBrObjetoGNRe }

TACBrObjetoGNRe = class(TACBrObjetoDFe)
private
  fACBrGNRe: TACBrGNRe;
public
  constructor Create(AConfig: TMonitorConfig; ACBrGNRe: TACBrGNRe); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  procedure RespostaConsulta;
  procedure RespostaEnvio;

  property ACBrGNRe: TACBrGNRe read fACBrGNRe;
end;

{ TACBrCarregarGNRe }

TACBrCarregarGNRe = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;
public
  constructor Create(AACBrDFe: TACBrGNRe; AXMLorFile: String ); reintroduce;
end;

{ TMetodoConsultaConfig}
TMetodoConsultaConfig = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirGNRe}
TMetodoImprimirGNRe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirGNRePDF}
TMetodoImprimirGNRePDF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGerarGuia}
TMetodoGerarGuia = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGerarXML }
TMetodoGerarXML = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetFormaEmissao}
TMetodoSetFormaEmissao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

{ TMetodoGerarXML }

procedure TMetodoGerarXML.Executar;
var
  AIni: string;
begin
  AIni := fpCmd.Params(0);

  with TACBrObjetoGNRe(fpObjetoDono) do
  begin
    ACBrGNRe.Guias.Clear;
    ACBrGNRe.Guias.LoadFromIni(AIni);

    ACBrGNRe.Guias.GerarGNRe;
    ACBrGNRe.Guias[0].GravarXML;
    fpCmd.Resposta:= 'Arquivo guia gerado em: ' +
                     ACBrGNRe.Guias[0].NomeArq + sLineBreak;
  end;
end;

{ TACBrCarregarGNRe }

procedure TACBrCarregarGNRe.CarregarDFePath(const AValue: String);
begin
  if not ( TACBrGNRe(FpACBrDFe).GuiasRetorno.LoadFromFile( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroGNReAbrir, [AValue]) ) );
end;

procedure TACBrCarregarGNRe.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrGNRe(FpACBrDFe).GuiasRetorno.LoadFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroGNReCarregar) );
end;

function TACBrCarregarGNRe.ValidarDFe(const AValue: String): Boolean;
begin
  Result := False;
  if ( TACBrGNRe(FpACBrDFe).GuiasRetorno.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrGNRe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrGNRe(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlGNRe ;
end;

constructor TACBrCarregarGNRe.Create(AACBrDFe: TACBrGNRe; AXMLorFile: String);
begin
  inherited Create(AACBrDFe, AXMLorFile);
end;

{ TACBrObjetoGNRe }

constructor TACBrObjetoGNRe.Create(AConfig: TMonitorConfig; ACBrGNRe: TACBrGNRe);
begin
  inherited Create(AConfig);

  fACBrGNRe := ACBrGNRe;

  ListaDeMetodos.Add(CMetodoConsultaConfig);
  ListaDeMetodos.Add(CMetodoImprimirGNRe);
  ListaDeMetodos.Add(CMetodoImprimirGNRePDF);
  ListaDeMetodos.Add(CMetodoGerarGuia);
  ListaDeMetodos.Add(CMetodoSetFormaEmissao);
  ListaDeMetodos.Add(CMetodoGerarXMLGNRe);
end;

procedure TACBrObjetoGNRe.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoConsultaConfig;
    1  : AMetodoClass := TMetodoImprimirGNRe;
    2  : AMetodoClass := TMetodoImprimirGNRePDF;
    3  : AMetodoClass := TMetodoGerarGuia;
    4  : AMetodoClass := TMetodoSetFormaEmissao;
    5  : AMetodoClass := TMetodoGerarXML;
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

procedure TACBrObjetoGNRe.RespostaConsulta;
var
  Resp: TLibGNReConsulta;
begin
  Resp := TLibGNReConsulta.Create(TpResp, codUTF8);
  try
    Resp.Processar(fACBrGNRe);
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;

    {with fACBrGNRe.WebServices.ConsultaUF do
    begin
      Resp.Ambiente := TpAmbToStr(ambiente);
      Resp.Codigo := IntToStr(codigo);
      Resp.Descricao := Descricao;
      Resp.UF := Uf;
      Resp.ExigeUfFavorecida := IfThen(exigeUfFavorecida = 'S', 'SIM', 'NÃO');
      Resp.ExigeReceita := IfThen(exigeReceita = 'S', 'SIM', 'NÃO');

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end; }
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoGNRe.RespostaEnvio;
var
  Resp: TLibGNReEnvio;
begin
  Resp := TLibGNReEnvio.Create(TpResp, codUTF8);
  try
    Resp.Processar(fACBrGNRe);
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;

    {with fACBrGNRe.WebServices.Retorno do
    begin
      Resp.Ambiente := TpAmbToStr(ambiente);
      Resp.Codigo := IntToStr(codigo);
      Resp.Descricao := Descricao;
      Resp.Recibo := numeroRecibo;
      Resp.Protocolo := Protocolo;

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;}
  finally
    Resp.Free;
  end;
end;

{ TMetodoConsultaConfig }

{ Params: 0 - String com a Sigla da UF
          1 - Inteiro com o código da receita referente a guia
}
procedure TMetodoConsultaConfig.Executar;
var
  AUF, AMsg: String;
  AReceita: Integer;
begin
  AUF := fpCmd.Params(0);
  AReceita := StrToIntDef(fpCmd.Params(1), 0);

  with TACBrObjetoGNRe(fpObjetoDono) do
  begin
    ACBrGNRe.WebServices.ConsultaUF.Uf := AUF;
    ACBrGNRe.WebServices.ConsultaUF.receita := AReceita;

    try
      ACBrGNRe.WebServices.ConsultaUF.Executar;
      AMsg := ACBrGNRe.WebServices.Enviar.Msg;
    except
    on E: Exception do
      begin
        raise Exception.Create(AMsg + sLineBreak + E.Message);
      end;
    end;

    fpCmd.Resposta := AMsg + sLineBreak;
    RespostaConsulta;
  end;
end;

{ TMetodoImprimirGNRe }

{ Params: 0 - XMLFile - Uma String com um Path completo para um arquivo XML GNRe
                         ou Uma String com conteúdo XML CTe
          1 - Impressora: String com Nome da Impressora
          2 - Copias: Integer Número de Copias
          3 - Preview: 1 para Mostrar Preview
}
procedure TMetodoImprimirGNRe.Executar;
var
  AXML: String;
  AImpressora: String;
  ACopias: String;
  APreview: Boolean;
  CargaDFe: TACBrCarregarGNRe;
begin
  AXML := fpCmd.Params(0);
  AImpressora := fpCmd.Params(1);
  ACopias := fpCmd.Params(2);
  APreview := StrToBoolDef(fpCmd.Params(4), False);

  with TACBrObjetoGNRe(fpObjetoDono) do
  begin
    ACBrGNRe.GuiasRetorno.Clear;
    CargaDFe := TACBrCarregarGNRe.Create(ACBrGNRe, AXML);

    try
//    DoConfiguraDACTe(False, BoolToStr(APreview,'1',''));

      if NaoEstaVazio(AImpressora) then
        ACBrGNRe.GNREGuia.Impressora := AImpressora;

      if NaoEstaVazio(ACopias) then
        ACBrGNRe.GNREGuia.NumCopias := StrToIntDef(ACopias,1);

      try
        DoAntesDeImprimir((APreview) or (MonitorConfig.DFE.Impressao.DANFE.MostrarPreview ));
        ACBrGNRe.GuiasRetorno.Imprimir;
      finally
        DoDepoisDeImprimir;
      end;
    finally
      CargaDFe.Free;
    end;

    fpCmd.Resposta := 'Guia GNRe Impressa com sucesso';
  end;
end;

{ TMetodoImprimirGNRePDF }

{ Params: 0 - XML - Uma String com um Path completo XML GNRe
}
procedure TMetodoImprimirGNRePDF.Executar;
var
  ArqPDF, AXML: string;
  CargaDFe: TACBrCarregarGNRe;
begin
  AXML := fpCmd.Params(0);

  with TACBrObjetoGNRe(fpObjetoDono) do
  begin
    ACBrGNRe.GuiasRetorno.Clear;
    CargaDFe := TACBrCarregarGNRe.Create(ACBrGNRe, AXML);
    try
//      DoConfiguraDACTe(True, '');

      try
        ACBrGNRe.GuiasRetorno.ImprimirPDF;

        ArqPDF := OnlyNumber(ACBrGNRe.GuiasRetorno.Items[0].GNRE.IdentificadorGuia) + '-guia.pdf';
        ArqPDF := PathWithDelim(ACBrGNRe.GNREGuia.PathPDF) + ArqPDF;

        fpCmd.Resposta := 'Arquivo criado em: ' + ArqPDF;
      except
        raise Exception.Create('Erro ao criar o arquivo PDF');
      end;
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoGerarGuia }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini GNRe
                         ou uma String com conteúdo txt do GNRe
          1 - bImprimir - Imprimir a Guia
}
procedure TMetodoGerarGuia.Executar;
var
  AIni: string;
  bImprimir: Boolean;
begin
  AIni := fpCmd.Params(0);
  bImprimir := StrToBoolDef(fpCmd.Params(1),False);

  with TACBrObjetoGNRe(fpObjetoDono) do
  begin
    ACBrGNRe.Guias.Clear;
    ACBrGNRe.Guias.LoadFromIni(AIni);

    ACBrGNRe.Guias.GerarGNRE;
    ACBrGNRe.Guias.Items[0].GravarXML;
    fpCmd.Resposta:= 'Arquivo guia gerado em: ' +
                     ACBrGNRe.Guias.Items[0].NomeArq + sLineBreak;

    ACBrGNRe.Enviar(bImprimir);

    RespostaEnvio;

  end;
end;

{ TMetodoSetFormaEmissao }

{ Params: 0 - Inteiro com o código da forma de emissão
}
procedure TMetodoSetFormaEmissao.Executar;
var
  Ok: Boolean;
  FormaEmissao: TpcnTipoEmissao;
  AFormaEmissao: String;
begin
  with TACBrObjetoGNRe(fpObjetoDono) do
  begin
    AFormaEmissao := fpCmd.Params(0);

    if MonitorConfig.DFE.IgnorarComandoModoEmissao then
      exit;

    OK := False;
    FormaEmissao := StrToTpEmis(OK, AFormaEmissao);

    if not OK then
      raise Exception.Create('Forma de Emissão Inválida: ' + AFormaEmissao)
    else
    begin
      ACBrGNRe.Configuracoes.Geral.FormaEmissao := FormaEmissao;

      with MonitorConfig.DFE.WebService do
        FormaEmissaoGNRe := ACBrGNRe.Configuracoes.Geral.FormaEmissaoCodigo-1;

      MonitorConfig.SalvarArquivo;
    end;
  end;
end;

end.
