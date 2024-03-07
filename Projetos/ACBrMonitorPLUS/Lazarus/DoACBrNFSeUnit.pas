{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{ Colaboradores nesse arquivo: 2021 José M. S. Junior                           }
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

unit DoACBrNFSeUnit;

interface

uses
  Classes, SysUtils, CmdUnit,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.Math,
  ACBrMonitorConsts, ACBrMonitorConfig, DoACBrDFeUnit, ACBrNFSeX,
  ACBrNFSeXConversao, ACBrLibResposta, ACBrLibNFSeRespostas;

type

{ TACBrObjetoNFSe}

TACBrObjetoNFSe = class(TACBrObjetoDFe)
private
  fACBrNFSeX: TACBrNFSeX;
public
  constructor Create(AConfig: TMonitorConfig; ACBrNFSex: TACBrNFSeX); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  Procedure LerIniNFSe(ArqINI: String);

  property ACBrNFSeX: TACBrNFSeX read fACBrNFSeX;
end;

{ TACBrCarregarNFSe }

TACBrCarregarNFSe = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;
public
  constructor Create(AACBrDFe: TACBrNFSeX; AXMLorFile: String; ARetornaFalha: Boolean = True ); reintroduce;
end;

{ TMetodoCriarEnviarRPS }
TMetodoCriarEnviarRPS = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{TMetodoAdicionarRPS}
TMetodoAdicionarRPS = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLimparLoteRPS }

TMetodoLimparLoteRPS = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalRPSLote }

TMetodoTotalRPSLote = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{TMetodoEnviarLoteRPS}
TMetodoEnviarLoteRPS = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{TMetodoGerarLoteRPS}
TMetodoGerarLoteRPS = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{TMetodoConsultarSituacaoLote}
TMetodoConsultarSituacaoLote = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarLote}
TMetodoConsultarLote = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSeporRPS}
TMetodoConsultarNFSeporRPS = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSeporNumero}
TMetodoConsultarNFSeporNumero = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSeporPeriodo}
TMetodoConsultarNFSeporPeriodo = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSeporFaixa}
TMetodoConsultarNFSeporFaixa = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSeGenerico}
TMetodoConsultarNFSeGenerico = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarLinkNFSe}
TMetodoConsultarLinkNFSe = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSeServicoPrestadoPorNumero}
TMetodoConsultarNFSeServicoPrestadoPorNumero = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSeServicoPrestadoPorTomador}
TMetodoConsultarNFSeServicoPrestadoPorTomador = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSeServicoPrestadoPorIntermediario}
TMetodoConsultarNFSeServicoPrestadoPorIntermediario = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSeServicoPrestadoPorPeriodo}
TMetodoConsultarNFSeServicoPrestadoPorPeriodo = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSeServicoTomadoPorNumero}
TMetodoConsultarNFSeServicoTomadoPorNumero = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSeServicoTomadoPorPrestador}
TMetodoConsultarNFSeServicoTomadoPorPrestador = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSeServicoTomadoPorTomador}
TMetodoConsultarNFSeServicoTomadoPorTomador = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSeServicoTomadoPorIntermediario}
TMetodoConsultarNFSeServicoTomadoPorIntermediario = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSeServicoTomadoPorPeriodo}
TMetodoConsultarNFSeServicoTomadoPorPeriodo = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoCancelarNFSe}
TMetodoCancelarNFSe = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoLinkNFSe}
TMetodoLinkNFSe = class(TACBrMetodo)
public
   procedure Executar; override;
end;

  {TMetodoSubstituirNFSe}
TMetodoSubstituirNFSe = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoEnviarEmailNFSe}
TMetodoEnviarEmailNFSe = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoImprimirNFSe}
TMetodoImprimirNFSe = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoImprimirPDFNFSe}
TMetodoImprimirPDFNFSe = class(TACBrMetodo)
public
   procedure Executar; override;
end;

// Usado pelos provedores que geram token por WebService
{TMetodoGerarTokenNFSe}
TMetodoGerarTokenNFSe = class(TACBrMetodo)
public
   procedure Executar; override;
end;

// Usado pelo Padrão Nacional
{TMetodoConsultarDPSPorChave}
TMetodoConsultarDPSPorChave = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarNFSePorChave}
TMetodoConsultarNFSePorChave = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoObterDANFSE}
TMetodoObterDANFSE = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoEnviarEventoNFSe}
TMetodoEnviarEventoNFSe = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarEventoNFSe}
TMetodoConsultarEventoNFSe = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarDFeNFSePorNSU}
TMetodoConsultarDFeNFSePorNSU = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarDFeNFSePorChave}
TMetodoConsultarDFeNFSePorChave = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{TMetodoConsultarParametrosNFSe}
TMetodoConsultarParametrosNFSe = class(TACBrMetodo)
public
   procedure Executar; override;
end;

{ TMetodoObterInformacoesProvedor }

TMetodoObterInformacoesProvedor = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetLayoutNFSe }

TMetodoSetLayoutNFSe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetCodigoMunicipio }

TMetodoSetCodigoMunicipio = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetEmitente }

TMetodoSetEmitente = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetAutenticacaoNFSe }

TMetodoSetAutenticacaoNFSe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

uses
  Forms, DoACBrUnit, strutils, IniFiles,
  ACBrNFSeXWebserviceBase;

{ TMetodoSetAutenticacaoNFSe }

{ Params: 0 - WSUser: Usuário
          1 - WSSenha: Senha
          2 - WSChaveAcesso: Chave de acesso
          3 - WSChaveAutoriz: Chave Autenticação
          4 - WSFraseSecr: Frase Secreta
}
procedure TMetodoSetAutenticacaoNFSe.Executar;
var
  AUsuario, ASenha, AChaveAcesso, AChaveAutenticacao, AFraseSecreta: String;
begin
  AUsuario := fpCmd.Params(0);
  ASenha := fpCmd.Params(1);
  AChaveAcesso := fpCmd.Params(2);
  AChaveAutenticacao := fpCmd.Params(3);
  AFraseSecreta := fpCmd.Params(4);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.Configuracoes.Geral.Emitente.WSUser := AUsuario;
    ACBrNFSeX.Configuracoes.Geral.Emitente.WSSenha := ASenha;
    ACBrNFSeX.Configuracoes.Geral.Emitente.WSChaveAcesso := AChaveAcesso;
    ACBrNFSeX.Configuracoes.Geral.Emitente.WSChaveAutoriz := AChaveAutenticacao;
    ACBrNFSeX.Configuracoes.Geral.Emitente.WSFraseSecr := AFraseSecreta;

    with MonitorConfig.NFSE do
    begin
      Usuario := AUsuario;
      Senha := ASenha;
      ChaveAcesso := AChaveAcesso;
      ChaveAutenticacao := AChaveAutenticacao;
      FraseSecreta := AFraseSecreta;
    end;

    MonitorConfig.SalvarArquivo;
  end;

end;

{ TMetodoSetEmitente }

{ Params: 0 - CNPJ do emitente da NFSe
          1 - Inscrição Municipal do emitente da NFSe
          2 - Razão Social do emitente da NFSe
}
procedure TMetodoSetEmitente.Executar;
var
  ACNPJEmitente, AIMEmitente, ARazaoSocial: String;
begin
  ACNPJEmitente := fpCmd.Params(0);
  AIMEmitente   := fpCmd.Params(1);
  ARazaoSocial  := fpCmd.Params(2);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.Configuracoes.Geral.Emitente.CNPJ :=  ACNPJEmitente;
    ACBrNFSeX.Configuracoes.Geral.Emitente.InscMun := AIMEmitente;
    ACBrNFSeX.Configuracoes.Geral.Emitente.RazSocial := ARazaoSocial;

    with MonitorConfig.NFSE do
    begin
      CNPJEmitente := ACNPJEmitente;
      IMEmitente := AIMEmitente;
      NomeEmitente := ARazaoSocial;
    end;
    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoSetCodigoMunicipio }

{ Params: 0 - Código IBGE do Município}
procedure TMetodoSetCodigoMunicipio.Executar;
var
  ACodMunicipio: Integer;
  ANomeMunicipio, AUFMunicipio: String;
  ListaMunicipios: TMemIniFile;
begin
  ACodMunicipio := StrToIntDef(fpCmd.Params(0), 0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin

    ListaMunicipios := TMemIniFile.Create('');
    try
      ListaMunicipios.SetStrings(ACBrNFSeX.Configuracoes.WebServices.Params);
      ANomeMunicipio := ListaMunicipios.ReadString(IntToStr(ACodMunicipio), 'Nome', '');
      AUFMunicipio := ListaMunicipios.ReadString(IntToStr(ACodMunicipio), 'UF', '');
    finally
      ListaMunicipios.Free;
    end;

    ACBrNFSeX.Configuracoes.Geral.CodigoMunicipio := ACodMunicipio;

    with MonitorConfig.NFSE do
    begin
      CodigoMunicipio := ACodMunicipio;
      NomeMunicipio := ANomeMunicipio;
      UFMunicipio := AUFMunicipio;
    end;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoSetLayoutNFSe }

{ Params: 0 - LayoutNFSe}
procedure TMetodoSetLayoutNFSe.Executar;
var
  ALayout: string;
  OK: boolean;
begin
  ALayout := fpCmd.Params(0);
  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.Configuracoes.Geral.LayoutNFSe := StrToLayoutNFSe(Ok, ALayout);

    with MonitorConfig.NFSE do
      LayoutProvedor := StrToIntDef(LayoutNFSeToStr(ACBrNFSeX.Configuracoes.Geral.LayoutNFSe), 0);

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoObterInformacoesProvedor }

procedure TMetodoObterInformacoesProvedor.Executar;
var
  RespObterInfo: TObterInformacoesProvedorResposta;
begin
  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    RespObterInfo := TObterInformacoesProvedorResposta.Create(TpResp, codUTF8);
    try
      RespObterInfo.Processar(ACBrNFSeX.Configuracoes.Geral);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespObterInfo.Gerar;
    finally
      RespObterInfo.Free;
    end;
  end;
end;

{ TMetodoTotalRPSLote }

procedure TMetodoTotalRPSLote.Executar;
begin
  fpCmd.Resposta := IntToStr(TACBrObjetoNFSe(fpObjetoDono).ACBrNFSeX.NotasFiscais.Count);
end;

{ TMetodoLimparLoteRPS }

procedure TMetodoLimparLoteRPS.Executar;
begin
  TACBrObjetoNFSe(fpObjetoDono).ACBrNFSeX.NotasFiscais.Clear;
  fpCmd.Resposta := 'Lote de RPS limpo!';
end;

{ TMetodoImprimirPDFNFSe }

{ Params: 0 - PathXML - Uma String com um Path completo para um arquivo XML NFSe ou Uma String com conteúdo XML NFe }
procedure TMetodoImprimirPDFNFSe.Executar;
var
  APathXML : String;
  CargaDFe: TACBrCarregarNFSe;
begin
  APathXML := fpCmd.Params(0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.NotasFiscais.Clear;

    CargaDFe := TACBrCarregarNFSe.Create(ACBrNFSeX, APathXML);
    try
      ACBrNFSeX.NotasFiscais.ImprimirPDF;

      if ACBrNFSeX.NotasFiscais.Items[0].NomeArqRps <> '' then
        fpCmd.Resposta := fpCmd.Resposta + ACBrNFSeX.NotasFiscais.Items[0].NomeArqRps
      else
        fpCmd.Resposta :=  fpCmd.Resposta + ACBrNFSeX.NotasFiscais.Items[0].NomeArq;
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoImprimirNFSe }

{ Params: 0 - PathXML - Uma String com um Path completo para um arquivo XML NFSe ou Uma String com conteúdo XML NFe
          1 - Impressora: String com Nome da Impressora
          2 - Copias: Integer Número de Copias
          3 - Preview: 1 para Mostrar Preview
}
procedure TMetodoImprimirNFSe.Executar;
var
  APathXML : String;
  AImpressora : String;
  ACopias : Integer;
  APreview : Boolean;
  CargaDFe: TACBrCarregarNFSe;
begin
  APathXML := fpCmd.Params(0);
  AImpressora := fpCmd.Params(1);
  ACopias := StrToIntDef(fpCmd.Params(2), 0);
  APreview := StrToBoolDef( fpCmd.Params(3), False);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.NotasFiscais.Clear;

    CargaDFe := TACBrCarregarNFSe.Create(ACBrNFSeX, APathXML);
    try

      if NaoEstaVazio(AImpressora) then
        ACBrNFSeX.DANFSE.Impressora := AImpressora;

      if (ACopias > 0) then
        ACBrNFSeX.DANFSE.NumCopias := ACopias;

      ACBrNFSeX.DANFSE.MostraPreview:= APreview;

      ACBrNFSeX.NotasFiscais.Imprimir;
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoEnviarEmailNFSe }

{ Params: 0 - Email: String com email Destinatário
          1 - XML: String com path do XML
          2 - Boolean 1 : Envia PDF
          3 - Assunto: String com Assunto do e-mail
          4 - Copia: String com e-mails copia (Separados ;)
          5 - Anexo: String com Path de Anexos (Separados ;)
          6 - Replay: String ReplayTo (Separados ;)
}
procedure TMetodoEnviarEmailNFSe.Executar;
var
  sAssunto, ADestinatario, APathXML, AEmailCopias, AAnexos, AAssunto, AReplay: string;
  AEnviaPDF: Boolean;
  slMensagemEmail, slCC, slAnexos, slReplay: TStringList;
  CargaDFe: TACBrCarregarNFSe;
begin
  ADestinatario := fpCmd.Params(0);
  APathXML := fpCmd.Params(1);
  AEnviaPDF := StrToBoolDef(fpCmd.Params(2), False);
  AAssunto := fpCmd.Params(3);
  AEmailCopias := fpCmd.Params(4);
  AAnexos := fpCmd.Params(5);
  AReplay := fpCmd.Params(6);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.NotasFiscais.Clear;

    slMensagemEmail := TStringList.Create;
    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    slReplay := TStringList.Create;
    try
      CargaDFe := TACBrCarregarNFSe.Create(ACBrNFSeX, APathXML);
      try
        DoConfiguraDANFSe(True, '');

        with MonitorConfig.DFE.Email do
        begin
          slMensagemEmail.Text := DoSubstituirVariaveis( StringToBinaryString(MensagemNFSe) );

          sAssunto := AssuntoNFSe;
        end;

        QuebrarLinha(AEmailCopias, slCC);
        QuebrarLinha(AAnexos, slAnexos);
        QuebrarLinha(AReplay, slReplay);

        try
          ACBrNFSeX.NotasFiscais.Items[0].EnviarEmail(ADestinatario,
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

          if not (MonitorConfig.Email.SegundoPlano) then
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

{ TMetodoSubstituirNFSe }

{ Params: 0 - NumNFSe: String - Numero da NFSe
          1 - SerieNFSe: String - Serie da NFSe
          2 - CodCancelamento: String - Código de Verificação.
          3 - Motivo: String - Motivo do Cancelamento
          4 - NumLote: Integer - Numero do Lote
          5 - CodVerificacao: String - Código de Verificação
}
procedure TMetodoSubstituirNFSe.Executar;
var
  ANumNFSe: String;
  ASerieNFSe: String;
  ACodCancelamento: String;
  AMotivo: String;
  ANumLote: String;
  ACodVerificacao: String;
  RespSubstituir: TSubstituirNFSeResposta;
begin
  ANumNFSe := fpCmd.Params(0);
  ASerieNFSe := fpCmd.Params(1);
  ACodCancelamento := fpCmd.Params(2);
  AMotivo := fpCmd.Params(3);
  ANumLote := fpCmd.Params(4);
  ACodVerificacao := fpCmd.Params(5);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.SubstituirNFSe(ANumNFSe, ASerieNFSe, ACodCancelamento, AMotivo,
                             ANumLote, ACodVerificacao);

    RespSubstituir := TSubstituirNFSeResposta.Create(TpResp, codUTF8);
      try
        RespSubstituir.Processar(ACBrNFSeX.WebService.SubstituiNFSe);
        fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespSubstituir.Gerar;
      finally
        RespSubstituir.Free;
      end;
  end;
end;

{ TMetodoLinkNFSe }

{ Params: 0 - NumNFSe: String - Numero da NFSe
          1 - CodVerificacao: String - Código de Verificação.
          2 - ChaveAcesso: String - Chave de Acesso.
          3 - ValorServico: String - Valor do Serviço.
}
procedure TMetodoLinkNFSe.Executar;
var
  ANumNFSe: String;
  ACodVerificacao: String;
  AChaveAcesso: String;
  AValorServico: String;
  SLink: String;
  RespLink: TLinkNFSeResposta;
begin
  ANumNFSe := fpCmd.Params(0);
  ACodVerificacao := fpCmd.Params(1);
  AChaveAcesso := fpCmd.Params(2);
  AValorServico := fpCmd.Params(3);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    SLink:= ACBrNFSeX.LinkNFSe(ANumNFSe, ACodVerificacao, AChaveAcesso,
                               AValorServico);

    RespLink := TLinkNFSeResposta.Create(TpResp, codUTF8);
    try
      RespLink.Processar(SLink);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespLink.Gerar;
    finally
      RespLink.Free;
    end;
  end;
end;

{ TMetodoCancelarNFSe }

{ Params: 0 - Path_Ini : String Path do ini com dados cancelamento
}
procedure TMetodoCancelarNFSe.Executar;
var
  APathIni: String;
  InfCancelamento: TInfCancelamento;
  RespCancelar: TCancelarNFSeResposta;
begin
  APathIni := fpCmd.Params(0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin

    InfCancelamento := TInfCancelamento.Create;
    try
      InfCancelamento.LerFromIni(APathIni);
      ACBrNFSeX.CancelarNFSe(InfCancelamento);

      RespCancelar := TCancelarNFSeResposta.Create(TpResp, codUTF8);
      try
        RespCancelar.Processar(ACBrNFSeX.WebService.CancelaNFSe);
        fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespCancelar.Gerar;
      finally
        RespCancelar.Free;
      end;

    finally
      InfCancelamento.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSeServicoTomadoPorPeriodo }

{ Params: 0 - DataIni: Date - Data de Inicio
          1 - DataFim: Date - Data Fim
          2 - Pagina: integer - Numero da página
}
procedure TMetodoConsultarNFSeServicoTomadoPorPeriodo.Executar;
var
  ADataIni: TDateTime;
  ADataFim: TDateTime;
  APagina: Integer;
  RespConsulta: TConsultaNFSeResposta;
begin
  ADataIni := StrToDateTimeDef(fpCmd.Params(0), 0);
  ADataFim := StrToDateTimeDef(fpCmd.Params(1), 0);
  APagina := StrToIntDef(fpCmd.Params(2), 1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoTomadoPorPeriodo(ADataIni, ADataFim, APagina);

    RespConsulta := TConsultaNFSeResposta.Create(TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
      RespConsulta.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSeServicoTomadoPorIntermediario }

{ Params: 0 - CNPJCPF: String - cnpj do intermediario
          1 - IM: String - Inscricao Municipal
          2 - Pagina: integer - Numero da página
}
procedure TMetodoConsultarNFSeServicoTomadoPorIntermediario.Executar;
var
  ACNPJCPF: String;
  AIM: String;
  APagina: Integer;
  RespConsulta: TConsultaNFSeResposta;
begin
  ACNPJCPF := fpCmd.Params(0);
  AIM := fpCmd.Params(1);
  APagina := StrToIntDef(fpCmd.Params(2), 1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoTomadoPorIntermediario(ACNPJCPF, AIM, APagina);

    RespConsulta := TConsultaNFSeResposta.Create(TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
      RespConsulta.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSeServicoTomadoPorTomador }

{ Params: 0 - CNPJCPF: String - cnpj do tomador
          1 - IM: String - Inscricao Municipal
          2 - Pagina: integer - Numero da página
}
procedure TMetodoConsultarNFSeServicoTomadoPorTomador.Executar;
var
  ACNPJCPF: String;
  AIM: String;
  APagina: Integer;
  RespConsulta: TConsultaNFSeResposta;
begin
  ACNPJCPF := fpCmd.Params(0);
  AIM := fpCmd.Params(1);
  APagina := StrToIntDef(fpCmd.Params(2), 1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoTomadoPorTomador(ACNPJCPF, AIM, APagina);

    RespConsulta := TConsultaNFSeResposta.Create(TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
      RespConsulta.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSeServicoTomadoPorPrestador }

{ Params: 0 - CNPJCPF: String  - cnpj do prestador
          1 - IM: String - Inscricao Municipal
          2 - Pagina: integer - Numero da página
}
procedure TMetodoConsultarNFSeServicoTomadoPorPrestador.Executar;
var
  ACNPJCPF: String;
  AIM: String;
  APagina: Integer;
  RespConsulta: TConsultaNFSeResposta;
begin
  ACNPJCPF := fpCmd.Params(0);
  AIM := fpCmd.Params(1);
  APagina := StrToIntDef(fpCmd.Params(2), 1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoTomadoPorPrestador(ACNPJCPF, AIM, APagina);

    RespConsulta := TConsultaNFSeResposta.Create(TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
      RespConsulta.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSeServicoTomadoPorNumero }

{ Params: 0 - NumNFSe: String - Número da NFSe
          1 - Pagina: integer - Numero da página
}
procedure TMetodoConsultarNFSeServicoTomadoPorNumero.Executar;
var
  ANumNFSe: String;
  APagina: Integer;
  RespConsulta: TConsultaNFSeResposta;
begin
  ANumNFSe := fpCmd.Params(0);
  APagina := StrToIntDef(fpCmd.Params(1), 1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoTomadoPorNumero(ANumNFSe, APagina);

    RespConsulta := TConsultaNFSeResposta.Create(TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
      RespConsulta.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSeServicoPrestadoPorPeriodo }

{ Params: 0 - DataIni: Date - Data de Inicio
          1 - DataFim: Date - Data Fim
          2 - Pagina: integer - Numero da página
}
procedure TMetodoConsultarNFSeServicoPrestadoPorPeriodo.Executar;
var
  ADataIni: TDateTime;
  ADataFim: TDateTime;
  APagina: Integer;
  RespConsulta: TConsultaNFSeResposta;
begin
  ADataIni := StrToDateTimeDef(fpCmd.Params(0), 0);
  ADataFim := StrToDateTimeDef(fpCmd.Params(1), 0);
  APagina := StrToIntDef(fpCmd.Params(2), 1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoPrestadoPorPeriodo(ADataIni, ADataFim, APagina);

    RespConsulta := TConsultaNFSeResposta.Create(TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
      RespConsulta.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSeServicoPrestadoPorIntermediario }

{ Params: 0 - CNPJ_CPF: String CNPJ do intermediario
          1 - IM: String Inscricao Municipal do intermediario
          2 - Pagina: integer - Numero da página
}
procedure TMetodoConsultarNFSeServicoPrestadoPorIntermediario.Executar;
var
  ACNPJ_CPF: String;
  AIM: String;
  APagina: Integer;
  RespConsulta: TConsultaNFSeResposta;
begin
  ACNPJ_CPF := fpCmd.Params(0);
  AIM := fpCmd.Params(1);
  APagina := StrToIntDef(fpCmd.Params(2), 1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoPrestadoPorIntermediario(ACNPJ_CPF, AIM, APagina);

    RespConsulta := TConsultaNFSeResposta.Create(TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
      RespConsulta.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSeServicoPrestadoPorTomador }

{ Params: 0 - CNPJ_CPF: String CNPJ do tomador
          1 - IM: String Inscricao Municipal do tomador
          2 - Pagina: integer - Numero da página
}
procedure TMetodoConsultarNFSeServicoPrestadoPorTomador.Executar;
var
  ACNPJ_CPF: String;
  AIM: String;
  APagina: Integer;
  RespConsulta: TConsultaNFSeResposta;
begin
  ACNPJ_CPF := fpCmd.Params(0);
  AIM := fpCmd.Params(1);
  APagina := StrToIntDef(fpCmd.Params(2), 1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoPrestadoPorTomador(ACNPJ_CPF, AIM, APagina);

    RespConsulta := TConsultaNFSeResposta.Create(TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
      RespConsulta.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSeServicoPrestadoPorNumero }

{ Params: 0 - NumNFSe String Número da NFSes
          1 - Pagina: integer - Numero da página
}
procedure TMetodoConsultarNFSeServicoPrestadoPorNumero.Executar;
var
  ANumNFSe: String;
  APagina: Integer;
  RespConsulta: TConsultaNFSeResposta;
begin
  ANumNFSe := fpCmd.Params(0);
  APagina := StrToIntDef(fpCmd.Params(1), 1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoPrestadoPorNumero(ANumNFSe, APagina);

    RespConsulta := TConsultaNFSeResposta.Create(TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
      RespConsulta.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSeGenerico }

{ Params: 0 - Path_ini: String  Path com os parametros de consulta geréricos}
procedure TMetodoConsultarNFSeGenerico.Executar;
var
  APathIni: String;
  InfConsultaNFSe: TInfConsultaNFSe;
  RespConsulta: TConsultaNFSeResposta;
begin
  APathIni := fpCmd.Params(0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    InfConsultaNFSe := TInfConsultaNFSe.Create;
    try
      InfConsultaNFSe.LerFromIni(APathIni);
      ACBrNFSeX.ConsultarNFSeGenerico(InfConsultaNFSe);

      RespConsulta := TConsultaNFSeResposta.Create(TpResp, codUTF8);
      try
         RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
         fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
      finally
         RespConsulta.Free;
      end;

    Finally
      InfConsultaNFSe.Free;
    end;
  end;
end;

{ TMetodoConsultarLinkNFSe }

{ Params: 0 - Path_ini: String  Path com os parametros de consulta link NFSe}
procedure TMetodoConsultarLinkNFSe.Executar;
var
  APathIni: String;
  InfConsultaLinkNFSe: TInfConsultaLinkNFSe;
  RespConsultaLinkNFSe: TConsultarLinkNFSeResposta;
begin
  APathIni := fpCmd.Params(0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    InfConsultaLinkNFSe := TInfConsultaLinkNFSe.Create;
    try
      InfConsultaLinkNFSe.LerFromIni(APathIni);
      ACBrNFSeX.ConsultarLinkNFSe(InfConsultaLinkNFSe);

      RespConsultaLinkNFSe := TConsultarLinkNFSeResposta.Create(TpResp, codUTF8);
      try
        RespConsultaLinkNFSe.Processar(ACBrNFSeX.WebService.ConsultaLinkNFSe);
        fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsultaLinkNFSe.Gerar;
      finally
        RespConsultaLinkNFSe.Free;
      end;
    finally
      InfConsultaLinkNFSe.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSeporFaixa }

{ Params: 0 - FaixaIni: String - Faixa NFSe de Inicio
          1 - FaixaFim: String - Faixa NFSe de Fim
          2 - Pagina: integer - Numero da página
}
procedure TMetodoConsultarNFSeporFaixa.Executar;
var
  AFaixaIni: String;
  AFaixaFim: String;
  APagina: Integer;
  RespConsulta: TConsultaNFSeResposta;
begin
  AFaixaIni := fpCmd.Params(0);
  AFaixaFim := fpCmd.Params(1);
  APagina := StrToIntDef(fpCmd.Params(2), 1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSePorFaixa(AFaixaIni, AFaixaFim, APagina);

    RespConsulta := TConsultaNFSeResposta.Create(TpResp, codUTF8);
    try
       RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
       fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
       RespConsulta.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSeporPeriodo }

{ Params: 0 - DataIni: Date - Data de Inicio
          1 - DataFim: Date - Data Fim
          2 - Pagina: integer - Numero da página
          3 - Numero do Lote Consulta
}
procedure TMetodoConsultarNFSeporPeriodo.Executar;
var
  ADataIni: TDateTime;
  ADataFim: TDateTime;
  APagina: Integer;
  ANumeroLote: String;
  RespConsulta: TConsultaNFSeResposta;
begin
  ADataIni := StrToDateTimeDef(fpCmd.Params(0), 0);
  ADataFim := StrToDateTimeDef(fpCmd.Params(1), 0);
  APagina := StrToIntDef(fpCmd.Params(2), 1);
  ANumeroLote := fpCmd.Params(3);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSePorPeriodo(ADataIni, ADataFim, APagina, ANumeroLote);

    RespConsulta := TConsultaNFSeResposta.Create(TpResp, codUTF8);
    try
       RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
       fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
       RespConsulta.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSeporNumero }

{ Params: 0 - NumeroNFSe:  String - Número do NFSe; }
procedure TMetodoConsultarNFSeporNumero.Executar;
var
  ANumeroNFSe: String;
  RespConsulta: TConsultaNFSeResposta;
begin
  ANumeroNFSe := fpCmd.Params(0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSePorNumero(ANumeroNFSe);

    RespConsulta := TConsultaNFSeResposta.Create(TpResp, codUTF8);
    try
       RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
       fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
       RespConsulta.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSeporRPS }

{ Params: 0 - NumeroRPS:  String - Número do RPS;
          1 - SerieRPS: String -  Série do RPS
          2 - TipoRPS: String - Tipo RPS (R1 R2 R3)
          3 - CodigoVerificacao: String - Código de Verificação}
procedure TMetodoConsultarNFSeporRPS.Executar;
var
  ANumeroRPS: String;
  ASerieRPS: String;
  ATipoRPS: String;
  ACodigoVerificacao: String;
  RespConsulta: TConsultaNFSeporRPSResposta;
begin
  ANumeroRPS := fpCmd.Params(0);
  ASerieRPS := fpCmd.Params(1);
  ATipoRPS := fpCmd.Params(2);
  ACodigoVerificacao := fpCmd.Params(3);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSePorRps(ANumeroRPS, ASerieRPS, ATipoRPS, ACodigoVerificacao);

    RespConsulta := TConsultaNFSeporRPSResposta.Create(TpResp, codUTF8);
    try
       RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaNFSeporRps);
       fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
       RespConsulta.Free;
    end;
  end;
end;

{ TMetodoConsultarLote }

{ Params: 0 - Protocolo: Numero do Protocolo String;
          1 - NumeroLote: String com número do lote a ser adicionado }
procedure TMetodoConsultarLote.Executar;
var
  AProtocolo: String;
  ALote: String;
  RespConsulta: TConsultaLoteRpsResposta;
begin
  AProtocolo := fpCmd.Params(0);
  ALote := fpCmd.Params(1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarLoteRps(AProtocolo, ALote);

    RespConsulta := TConsultaLoteRpsResposta.Create(TpResp, codUTF8);
    try
       RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaLoteRps);
       fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
       RespConsulta.Free;
    end;
  end;
end;

{ TMetodoConsultarSituacaoLote }

{ Params: 0 - Protocolo: Numero do Protocolo String;
          1 - NumeroLote: String com número do lote a ser adicionado }
procedure TMetodoConsultarSituacaoLote.Executar;
var
  AProtocolo: String;
  ALote: String;
  RespConsulta: TConsultaSituacaoResposta;
begin
  AProtocolo := fpCmd.Params(0);
  ALote := fpCmd.Params(1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarSituacao(AProtocolo, ALote);

    RespConsulta := TConsultaSituacaoResposta.Create(TpResp, codUTF8);
    try
       RespConsulta.Processar(ACBrNFSeX.WebService.ConsultaSituacao);
       fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsulta.Gerar;
    finally
       RespConsulta.Free;
    end;
  end;
end;

{ TMetodoGerarLoteRPS }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini RPS ou Uma String com conteúdo txt do RPS
          1 - NumeroLote: String com número do lote a ser adicionado }

procedure TMetodoGerarLoteRPS.Executar;
var
  AIni: String;
  ALote: String;
  RespGerar: TGerarLoteResposta;
begin
  AIni := fpCmd.Params(0);
  ALote := fpCmd.Params(1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.NotasFiscais.Clear;
    LerIniNFSe(AIni);
    ACBrNFSeX.NotasFiscais.NumeroLote:= ALote;
    ACBrNFSeX.NotasFiscais.Transacao:= True;

    try
      ACBrNFSeX.GerarLote(ALote);

      RespGerar := TGerarLoteResposta.Create(TpResp, codUTF8);
      try
        RespGerar.Processar(ACBrNFSeX.WebService.Gerar);
        fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespGerar.Gerar;
      finally
        RespGerar.Free;
      end;
    finally
      ACBrNFSeX.NotasFiscais.Clear;
    end;
  end;
end;

{ TMetodoEnviarLoteRPS }

{ Params: 0 - ALote: Integer com número do lote a ser adicionado
          1 - AModoEnvio: Integer com Indice ModoEnvio [0- Automatico, 1- LoteAssincrono, 2- LoteSincrono, 3- Unitario]
}
procedure TMetodoEnviarLoteRPS.Executar;
var
  ALote: String;
  AModoEnvio: Integer;

  RespEnvio: TEmiteResposta;
begin
  ALote         := fpCmd.Params(0);
  AModoEnvio    := StrToIntDef(fpCmd.Params(1), 0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    if (ACBrNFSeX.NotasFiscais.Count = 0) then
      fpCmd.Resposta := 'Nenhum RPS adicionado na Lista!'
    else
    begin
      try
        ACBrNFSeX.Emitir(ALote, TmodoEnvio(AModoEnvio), False);

        RespEnvio := TEmiteResposta.Create(TpResp, codUTF8);
        try
           RespEnvio.Processar(ACBrNFSeX.WebService.Emite);
           fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespEnvio.Gerar;
        finally
           RespEnvio.Free;
        end;

      finally
         ACBrNFSeX.NotasFiscais.Clear;
      end;
    end;
  end;
end;

{ TMetodoAdicionarRPS }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini RPS ou Uma String com conteúdo txt do RPS
          1 - NumeroLote: String com número do lote a ser adicionado }

procedure TMetodoAdicionarRPS.Executar;
var
  AIni: String;
  ALote: String;
begin
  AIni := fpCmd.Params(0);
  ALote := fpCmd.Params(1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    {ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'LotesNFSe' + PathDelim + 'Lote' + trim(ALote)); }

    if ACBrNFSeX.NotasFiscais.Count < 50 then
    begin
      LerIniNFSe(AIni);
      ACBrNFSeX.NotasFiscais.NumeroLote:= ALote;
      ACBrNFSeX.NotasFiscais.Transacao:= True;

      {ArqNFe := PathWithDelim(PathWithDelim(ExtractFilePath(Application.ExeName)) +
        'LotesNFSe' + PathDelim + 'Lote' + trim(ALote)) + OnlyNumber(
        ACBrNFSeX.NotasFiscais.Items[0].) + '-nfe.xml';
      ACBrNFe.NotasFiscais.GravarXML(ExtractFilePath(ArqNFe));}

      fpCmd.Resposta := 'Total RPS Adicionados= ' + IntToStr(ACBrNFSeX.NotasFiscais.Count);
    end else
    begin
      fpCmd.Resposta := 'Limite de RPS por Lote(50) atingido, envie o lote ou limpe a lista';
    end;
  end;
end;

{ TMetodoCriarEnviarRPS }

{ Params: 0 - AIni - Uma String com um Path completo arquivo .ini RPS ou Uma String com conteúdo txt do RPS
          1 - ALote: Integer com número do lote a ser adicionado
          2 - AModoEnvio: Integer com Indice ModoEnvio [0- Automatico, 1- LoteAssincrono, 2- LoteSincrono, 3- Unitario]
          3 - AImprimir: Boolean
}

procedure TMetodoCriarEnviarRPS.Executar;
var
  AIni: String;
  ALote: String;
  AModoEnvio: Integer;
  AImprime: Boolean;

  //Salva: Boolean;
  Alertas: String;
  Resp: String;
  RespEnvio: TEmiteResposta;
begin
  AIni          := fpCmd.Params(0);
  ALote         := fpCmd.Params(1);
  AModoEnvio    := StrToIntDef(fpCmd.Params(2), 0);
  AImprime      := StrToBoolDef(fpCmd.Params(3), False);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.NotasFiscais.Clear;
    LerIniNFSe(AIni);
    ACBrNFSeX.NotasFiscais.NumeroLote:= ALote;
    ACBrNFSeX.NotasFiscais.Transacao:= True;

    {Salva := ACBrNFSeX.Configuracoes.Geral.Salvar;
    if not Salva then
    begin
      ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs');
      ACBrNFSeX.Configuracoes.Arquivos.PathSalvar :=
        PathWithDelim(ExtractFilePath(Application.ExeName)) + 'Logs';
    end; }

    Alertas := ACBrNFSeX.NotasFiscais.Items[0].Alertas;

    if (Alertas <> '') then
      Resp := 'Alertas:' + Alertas;

    fpCmd.Resposta := Resp + sLineBreak;

    try
      ACBrNFSeX.Emitir(ALote, TmodoEnvio(AModoEnvio), AImprime);

      RespEnvio := TEmiteResposta.Create(TpResp, codUTF8);
      try
         RespEnvio.Processar(ACBrNFSeX.WebService.Emite);
         fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespEnvio.Gerar;
      finally
         RespEnvio.Free;
      end;
    finally
       ACBrNFSeX.NotasFiscais.Clear;
    end;
  end;
end;

{ TMetodoGerarTokenNFSe }

procedure TMetodoGerarTokenNFSe.Executar;
var
  RespGerarToken: TGerarTokenResposta;
begin
  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.GerarToken;

    RespGerarToken := TGerarTokenResposta.Create(TpResp, codUTF8);
    try
      RespGerarToken.Processar(ACBrNFSeX.WebService.GerarToken);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespGerarToken.Gerar;
    finally
      RespGerarToken.Free;
    end;
  end;
end;

{ TMetodoConsultarDPSPorChave }

{ Params: 0 - AChaveDPS: String com a chave do DPS
}
procedure TMetodoConsultarDPSPorChave.Executar;
var
  AChaveDPS: String;

  RespConsultarDPS: TConsultaNFSePorRpsResposta;
begin
  AChaveDPS := fpCmd.Params(0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarDPSPorChave(AChaveDPS);

    RespConsultarDPS := TConsultaNFSePorRpsResposta.Create(TpResp, codUTF8);
    try
      RespConsultarDPS.Processar(ACBrNFSeX.WebService.ConsultaNFSeporRps);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsultarDPS.Gerar;
    finally
      RespConsultarDPS.Free;
    end;
  end;
end;

{ TMetodoConsultarNFSePorChave }

{ Params: 0 - AChaveNFSe: String com a chave da NFSe
}
procedure TMetodoConsultarNFSePorChave.Executar;
var
  AChaveNFSe: String;

  RespConsultarNFSe: TConsultaNFSeResposta;
begin
  AChaveNFSe := fpCmd.Params(0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSePorChave(AChaveNFSe);

    RespConsultarNFSe := TConsultaNFSeResposta.Create(TpResp, codUTF8);
    try
      RespConsultarNFSe.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsultarNFSe.Gerar;
    finally
      RespConsultarNFSe.Free;
    end;
  end;
end;

{ TMetodoObterDANFSE }

{ Params: 0 - AChaveNFSe: String com a chave da NFSe
}
procedure TMetodoObterDANFSE.Executar;
var
  AChaveNFSe: String;

  RespObterDANFSE: TConsultaNFSeResposta;
begin
  AChaveNFSe := fpCmd.Params(0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ObterDANFSE(AChaveNFSe);

    RespObterDANFSE := TConsultaNFSeResposta.Create(TpResp, codUTF8);
    try
      RespObterDANFSE.Processar(ACBrNFSeX.WebService.ConsultaNFSe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespObterDANFSE.Gerar;
    finally
      RespObterDANFSE.Free;
    end;
  end;
end;

{ TMetodoEnviarEventoNFSe }

{ Params: 0 - Path_Ini : String Path do ini com dados do evento
}
procedure TMetodoEnviarEventoNFSe.Executar;
var
  APathIni: String;
  InfEvento: TInfEvento;
  RespEnviarEvento: TEnviarEventoResposta;
begin
  APathIni := fpCmd.Params(0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    InfEvento := TInfEvento.Create;
    try
      InfEvento.LerFromIni(APathIni);
      ACBrNFSeX.EnviarEvento(InfEvento);

      RespEnviarEvento := TEnviarEventoResposta.Create(TpResp, codUTF8);
      try
        RespEnviarEvento.Processar(ACBrNFSeX.WebService.EnviarEvento);
        fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespEnviarEvento.Gerar;
      finally
        RespEnviarEvento.Free;
      end;
    finally
      InfEvento.Free;
    end;
  end;
end;

{ TMetodoConsultarEventoNFSe }

{ Params: 0 - AChaveNFSe: String com a chave da NFSe
          1 - ATipoEvento: String com o tipo de evento: [e101101, e105102,
              e101103, e105104, e105105, e202201, e203202, e204203, e205204,
              e202205, e203206, e204207, e205208, e305101, e305102, e305103]
          2 - ANumSeq: Integer com o numero sequencial do evento
}
procedure TMetodoConsultarEventoNFSe.Executar;
var
  AChaveNFSe: String;
  ATipoEvento: String;
  ANumSeq: Integer;
  Ok: Boolean;

  RespConsultarEvento: TConsultaEventoResposta;
begin
  AChaveNFSe  := fpCmd.Params(0);
  ATipoEvento := fpCmd.Params(1);
  ANumSeq     := StrToIntDef(fpCmd.Params(2), 0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    if (AChaveNFSe <> '') and (ATipoEvento = '') and (ANumSeq = 0) then
      ACBrNFSeX.ConsultarEvento(AChaveNFSe);

    if (AChaveNFSe <> '') and (ATipoEvento <> '') and (ANumSeq = 0) then
      ACBrNFSeX.ConsultarEvento(AChaveNFSe, StrTotpEvento(Ok, ATipoEvento));

    if (AChaveNFSe <> '') and (ATipoEvento <> '') and (ANumSeq > 0) then
      ACBrNFSeX.ConsultarEvento(AChaveNFSe, StrTotpEvento(Ok, ATipoEvento),
        ANumSeq);

    RespConsultarEvento := TConsultaEventoResposta.Create(TpResp, codUTF8);
    try
      RespConsultarEvento.Processar(ACBrNFSeX.WebService.ConsultarEvento);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsultarEvento.Gerar;
    finally
      RespConsultarEvento.Free;
    end;
  end;
end;

{ TMetodoConsultarDFeNFSePorNSU }

{ Params: 0 - ANSU: String com o numero sequencial unico
}
procedure TMetodoConsultarDFeNFSePorNSU.Executar;
var
  ANSU: String;

  RespConsultarDFe: TConsultaDFeResposta;
begin
  ANSU := fpCmd.Params(0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarDFe(StrToIntDef(ANSU, 0));

    RespConsultarDFe := TConsultaDFeResposta.Create(TpResp, codUTF8);
    try
      RespConsultarDFe.Processar(ACBrNFSeX.WebService.ConsultarDFe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsultarDFe.Gerar;
    finally
      RespConsultarDFe.Free;
    end;
  end;
end;

{ TMetodoConsultarDFeNFSePorChave }

{ Params: 0 - AChaveNFSe: String com a chave da NFSe
}
procedure TMetodoConsultarDFeNFSePorChave.Executar;
var
  AChaveNFSe: String;

  RespConsultarDFe: TConsultaDFeResposta;
begin
  AChaveNFSe := fpCmd.Params(0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarDFe(AChaveNFSe);

    RespConsultarDFe := TConsultaDFeResposta.Create(TpResp, codUTF8);
    try
      RespConsultarDFe.Processar(ACBrNFSeX.WebService.ConsultarDFe);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsultarDFe.Gerar;
    finally
      RespConsultarDFe.Free;
    end;
  end;
end;

{ TMetodoConsultarParametrosNFSe }

{ Params: 0 - ATipoParam: Integer com o Tipo de Parametro: [0, 1, 2, 3, 4, 5]
               0 = Aliquota;
               1 = HistoricoAliquota;
               2 = Convenio;
               3 = RegimesEspeciais;
               4 = Retencoes;
               5 = Beneficios
          1 - ACodServico: String com o Código do Serviço
          2 - ACompetencia: TDateTime com a data de competencia
          3 - ANumBeneficio: String com o numero do beneficio
}
procedure TMetodoConsultarParametrosNFSe.Executar;
var
  ATipoParam: Integer;
  ACodServico: string;
  ACompetencia: TDateTime;
  ANumBeneficio: string;
  TipoParametro: TParamMunic;

  RespConsultaParametros: TConsultaParametrosResposta;
begin
  ATipoParam := StrToIntDef(fpCmd.Params(0), 0);
  ACodServico := fpCmd.Params(1);
  ACompetencia := StrToDateDef(fpCmd.Params(2), Date);
  ANumBeneficio := fpCmd.Params(3);
  TipoParametro:= TParamMunic(ATipoParam);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarParametros(TipoParametro, ACodServico, ACompetencia, ANumBeneficio);

    RespConsultaParametros := TConsultaParametrosResposta.Create(TpResp, codUTF8);
    try
      RespConsultaParametros.Processar(ACBrNFSeX.WebService.ConsultarParam);
      fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespConsultaParametros.Gerar;
    finally
      RespConsultaParametros.Free;
    end;
  end;
end;

{ TACBrCarregarNFSe }

procedure TACBrCarregarNFSe.CarregarDFePath(const AValue: String);
begin
  if not ( TACBrNFSeX(FpACBrDFe).NotasFiscais.LoadFromFile( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroNFeAbrir, [AValue]) ) );
end;

procedure TACBrCarregarNFSe.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrNFSeX(FpACBrDFe).NotasFiscais.LoadFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroNFeCarregar) );
end;

function TACBrCarregarNFSe.ValidarDFe(const AValue: String): Boolean;
begin
  Result := False;
  if ( TACBrNFSeX(FpACBrDFe).NotasFiscais.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrNFSeX(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

  if EstaVazio( FPathDFeExtensao )then
    FPathDFeExtensao := PathWithDelim(TACBrNFSeX(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                        + AValue + CExtensaoXmlNFSe ;
end;

constructor TACBrCarregarNFSe.Create(AACBrDFe: TACBrNFSeX; AXMLorFile: String;
  ARetornaFalha: Boolean);
begin
  inherited Create(AACBrDFe, AXMLorFile, ARetornaFalha);
end;

{ TACBrObjetoNFSe }

constructor TACBrObjetoNFSe.Create(AConfig: TMonitorConfig;
  ACBrNFSex: TACBrNFSeX);
begin
  inherited Create(AConfig);

  fACBrNFSeX := ACBrNFSex;

  ListaDeMetodos.Add(CMetodoCriarEnviarRPS);
  ListaDeMetodos.Add(CMetodoAdicionarRPS);
  ListaDeMetodos.Add(CMetodoEnviarLoteRPS);
  ListaDeMetodos.Add(CMetodoGerarLoteRPS);
  ListaDeMetodos.Add(CMetodoConsultarSituacaoLote);
  ListaDeMetodos.Add(CMetodoConsultarLote);
  ListaDeMetodos.Add(CMetodoConsultarNFSeporRPS);
  ListaDeMetodos.Add(CMetodoConsultarNFSeporNumero);
  ListaDeMetodos.Add(CMetodoConsultarNFSeporPeriodo);
  ListaDeMetodos.Add(CMetodoConsultarNFSeporFaixa);
  ListaDeMetodos.Add(CMetodoConsultarNFSeGenerico);
  ListaDeMetodos.Add(CMetodoConsultarNFSeServicoPrestadoPorNumero);
  ListaDeMetodos.Add(CMetodoConsultarNFSeServicoPrestadoPorTomador);
  ListaDeMetodos.Add(CMetodoConsultarNFSeServicoPrestadoPorIntermediario);
  ListaDeMetodos.Add(CMetodoConsultarNFSeServicoPrestadoPorPeriodo);
  ListaDeMetodos.Add(CMetodoConsultarNFSeServicoTomadoPorNumero);
  ListaDeMetodos.Add(CMetodoConsultarNFSeServicoTomadoPorPrestador);
  ListaDeMetodos.Add(CMetodoConsultarNFSeServicoTomadoPorTomador);
  ListaDeMetodos.Add(CMetodoConsultarNFSeServicoTomadoPorIntermediario);
  ListaDeMetodos.Add(CMetodoConsultarNFSeServicoTomadoPorPeriodo);
  ListaDeMetodos.Add(CMetodoCancelarNFSe);
  ListaDeMetodos.Add(CMetodoLinkNFSe);
  ListaDeMetodos.Add(CMetodoSubstituirNFSe);
  ListaDeMetodos.Add(CMetodoEnviarEmailNFSe);
  ListaDeMetodos.Add(CMetodoImprimirNFSe);
  ListaDeMetodos.Add(CMetodoImprimirPDFNFSe);

  ListaDeMetodos.Add(CMetodoGerarTokenNFSe);

  ListaDeMetodos.Add(CMetodoConsultarDPSPorChave);
  ListaDeMetodos.Add(CMetodoConsultarNFSePorChave);
  ListaDeMetodos.Add(CMetodoObterDANFSE);
  ListaDeMetodos.Add(CMetodoEnviarEventoNFSe);
  ListaDeMetodos.Add(CMetodoConsultarEventoNFSe);
  ListaDeMetodos.Add(CMetodoConsultarDFeNFSePorNSU);
  ListaDeMetodos.Add(CMetodoConsultarDFeNFSePorChave);
  ListaDeMetodos.Add(CMetodoConsultarParametrosNFSe);

  ListaDeMetodos.Add(CMetodoLimparLoteRPS);
  ListaDeMetodos.Add(CMetodoTotalRPSLote);
  ListaDeMetodos.Add(CMetodoObterInformacoesProvedor);
  ListaDeMetodos.Add(CMetodoSetLayoutNFSe);
  ListaDeMetodos.Add(CMetodoSetCodigoMunicipio);
  ListaDeMetodos.Add(CMetodoSetEmitente);
  ListaDeMetodos.Add(CMetodoSetAutenticacaoNFSe);
  ListaDeMetodos.Add(CMetodoConsultarLinkNFSe);

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
end;

procedure TACBrObjetoNFSe.Executar(ACmd: TACBrCmd);
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
    0  : AMetodoClass := TMetodoCriarEnviarRPS;
    1  : AMetodoClass := TMetodoAdicionarRPS;
    2  : AMetodoClass := TMetodoEnviarLoteRPS;
    3  : AMetodoClass := TMetodoGerarLoteRPS;
    4  : AMetodoClass := TMetodoConsultarSituacaoLote;
    5  : AMetodoClass := TMetodoConsultarLote;
    6  : AMetodoClass := TMetodoConsultarNFSeporRPS;
    7  : AMetodoClass := TMetodoConsultarNFSeporNumero;
    8  : AMetodoClass := TMetodoConsultarNFSeporPeriodo;
    9  : AMetodoClass := TMetodoConsultarNFSeporFaixa;
    10  : AMetodoClass := TMetodoConsultarNFSeGenerico;
    11  : AMetodoClass := TMetodoConsultarNFSeServicoPrestadoPorNumero;
    12  : AMetodoClass := TMetodoConsultarNFSeServicoPrestadoPorTomador;
    13  : AMetodoClass := TMetodoConsultarNFSeServicoPrestadoPorIntermediario;
    14  : AMetodoClass := TMetodoConsultarNFSeServicoPrestadoPorPeriodo;
    15  : AMetodoClass := TMetodoConsultarNFSeServicoTomadoPorNumero;
    16  : AMetodoClass := TMetodoConsultarNFSeServicoTomadoPorPrestador;
    17  : AMetodoClass := TMetodoConsultarNFSeServicoTomadoPorTomador;
    18  : AMetodoClass := TMetodoConsultarNFSeServicoTomadoPorIntermediario;
    19  : AMetodoClass := TMetodoConsultarNFSeServicoTomadoPorPeriodo;
    20  : AMetodoClass := TMetodoCancelarNFSe;
    21  : AMetodoClass := TMetodoLinkNFSe;
    22  : AMetodoClass := TMetodoSubstituirNFSe;
    23  : AMetodoClass := TMetodoEnviarEmailNFSe;
    24  : AMetodoClass := TMetodoImprimirNFSe;
    25  : AMetodoClass := TMetodoImprimirPDFNFSe;
    26  : AMetodoClass := TMetodoGerarTokenNFSe;
    27  : AMetodoClass := TMetodoConsultarDPSPorChave;
    28  : AMetodoClass := TMetodoConsultarNFSePorChave;
    29  : AMetodoClass := TMetodoObterDANFSE;
    30  : AMetodoClass := TMetodoEnviarEventoNFSe;
    31  : AMetodoClass := TMetodoConsultarEventoNFSe;
    32  : AMetodoClass := TMetodoConsultarDFeNFSePorNSU;
    33  : AMetodoClass := TMetodoConsultarDFeNFSePorChave;
    34  : AMetodoClass := TMetodoConsultarParametrosNFSe;
    35  : AMetodoClass := TMetodoLimparLoteRPS;
    36  : AMetodoClass := TMetodoTotalRPSLote;
    37  : AMetodoClass := TMetodoObterInformacoesProvedor;
    38  : AMetodoClass := TMetodoSetLayoutNFSe;
    39  : AMetodoClass := TMetodoSetCodigoMunicipio;
    40  : AMetodoClass := TMetodoSetEmitente;
    41  : AMetodoClass := TMetodoSetAutenticacaoNFSe;
    42  : AMetodoClass := TMetodoConsultarLinkNFSe;

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

procedure TACBrObjetoNFSe.LerIniNFSe(ArqINI: String);
begin
  with fACBrNFSeX do
  begin
    NotasFiscais.LoadFromIni ( ArqINI );
  end;
end;

end.

