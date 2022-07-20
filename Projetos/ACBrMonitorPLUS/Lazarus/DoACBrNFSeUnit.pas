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
  Classes, SysUtils, CmdUnit, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrMonitorConsts, ACBrMonitorConfig, DoACBrDFeUnit, ACBrNFSeX,
  ACBrNFSeConversao, ACBrLibResposta, ACBrLibNFSeRespostas;

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


implementation

uses
  Forms, DoACBrUnit, ACBrNFSeGravarXmlEnvioConsultarNFSe, ACBrNFSeGravarXmlEnvioCancelarNFSe;

{ TMetodoImprimirPDFNFSe }

{ Params: 0 - PathXML - Uma String com um Path completo para um arquivo XML NFSe
                         ou Uma String com conteúdo XML NFe
}
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
        fpCmd.Resposta := ACBrNFSeX.NotasFiscais.Items[0].NomeArqRps
      else
        fpCmd.Resposta :=  ACBrNFSeX.NotasFiscais.Items[0].NomeArq;

    finally
      CargaDFe.Free;
    end;

  end;

end;

{ TMetodoImprimirNFSe }

{ Params: 0 - PathXML - Uma String com um Path completo para um arquivo XML NFSe
                         ou Uma String com conteúdo XML NFe
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
          5 - Anexo: String com Path de Anexos (Separados ;) }
procedure TMetodoEnviarEmailNFSe.Executar;
var

  ADestinatario: String;
  APathXML: String;
  AEmailCopias: String;
  AAnexos: String;
  AEnviaPDF: Boolean;
  AAssunto: String;

  slMensagemEmail, slCC, slAnexos: TStringList;
  CargaDFe: TACBrCarregarNFSe;
begin
  ADestinatario := fpCmd.Params(0);
  APathXML := fpCmd.Params(1);
  AEnviaPDF := StrToBoolDef(fpCmd.Params(2), False);
  AAssunto := fpCmd.Params(3);
  AEmailCopias := fpCmd.Params(4);
  AAnexos := fpCmd.Params(5);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.NotasFiscais.Clear;

    slCC := TStringList.Create;
    slAnexos := TStringList.Create;
    slMensagemEmail := TStringList.Create;
    try
      CargaDFe := TACBrCarregarNFSe.Create(ACBrNFSeX, APathXML);
      try

        QuebrarLinha(AEmailCopias, slCC);
        QuebrarLinha(AAnexos, slAnexos);

        try
          ACBrNFSeX.NotasFiscais.Items[0].EnviarEmail(ADestinatario,
            AAssunto,
            slMensagemEmail,
            AEnviaPDF,
            slCC,
            slAnexos);

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

{ TMetodoSubstituirNFSe }

{ Params: 0 - NumNFSe: String - Numero da NFSe
          1 - CodCancelamento: String - Código de Verificação.
          2 - Motivo: String - Motivo do Cancelamento
          3 - NumLote: Integer - Numero do Lote
          4 - CodVerificacao: String - Código de Verificação
}
procedure TMetodoSubstituirNFSe.Executar;
var
  ANumNFSe: String;
  ACodCancelamento: String;
  AMotivo: String;
  ANumLote: String;
  ACodVerificacao: String;
  RespSubstituir: TSubstituirNFSeResposta;
begin
  ANumNFSe := fpCmd.Params(0);
  ACodCancelamento := fpCmd.Params(1);
  AMotivo := fpCmd.Params(2);
  ANumLote := fpCmd.Params(3);
  ACodVerificacao := fpCmd.Params(4);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.SubstituirNFSe(ANumNFSe, ACodCancelamento, AMotivo, ANumLote, ACodVerificacao);

    RespSubstituir := TSubstituirNFSeResposta.Create( TpResp, codUTF8);
      try
        RespSubstituir.Processar(ACBrNFSeX.WebServices.SubNFSe);
        fpCmd.Resposta := sLineBreak + RespSubstituir.Gerar;
      finally
        RespSubstituir.Free;
      end;

  end;

end;

{ TMetodoLinkNFSe }

{ Params: 0 - NumNFSe: String - Numero da NFSe
          1 - CodVerificacao: String - Código de Verificação.
}
procedure TMetodoLinkNFSe.Executar;
var
  ANumNFSe: String;
  ACodVerificacao: String;
  SLink: String;
  RespLink: TLinkResposta;
begin
  ANumNFSe := fpCmd.Params(0);
  ACodVerificacao := fpCmd.Params(1);
  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    SLink:= ACBrNFSeX.LinkNFSe(ANumNFSe, ACodVerificacao);
    RespLink := TLinkResposta.Create( TpResp, codUTF8, SLink);
      try
        RespLink.Processar();
        fpCmd.Resposta := sLineBreak + RespLink.Gerar;
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

      RespCancelar := TCancelarNFSeResposta.Create( TpResp, codUTF8);
      try
        RespCancelar.Processar(ACBrNFSeX.WebServices.CancNFSe);
        fpCmd.Resposta := sLineBreak + RespCancelar.Gerar;
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
  APagina := StrToIntDef(fpCmd.Params(2), 0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoTomadoPorPeriodo(ADataIni, ADataFim, APagina);

    RespConsulta := TConsultaNFSeResposta.Create( TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebServices.ConsNFSe);
      fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
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
  APagina := StrToIntDef(fpCmd.Params(2), 0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoTomadoPorIntermediario(ACNPJCPF, AIM, APagina);

    RespConsulta := TConsultaNFSeResposta.Create( TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebServices.ConsNFSe);
      fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
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
  APagina := StrToIntDef(fpCmd.Params(2), 0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoTomadoPorTomador(ACNPJCPF, AIM, APagina);

    RespConsulta := TConsultaNFSeResposta.Create( TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebServices.ConsNFSe);
      fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
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
  APagina := StrToIntDef(fpCmd.Params(2), 0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoTomadoPorPrestador(ACNPJCPF, AIM, APagina);

    RespConsulta := TConsultaNFSeResposta.Create( TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebServices.ConsNFSe);
      fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
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
  APagina := StrToIntDef(fpCmd.Params(1), 0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoTomadoPorNumero(ANumNFSe, APagina);

    RespConsulta := TConsultaNFSeResposta.Create( TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebServices.ConsNFSe);
      fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
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
  APagina := StrToIntDef(fpCmd.Params(2), 0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoPrestadoPorPeriodo(ADataIni, ADataFim, APagina);

    RespConsulta := TConsultaNFSeResposta.Create( TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebServices.ConsNFSe);
      fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
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
  APagina := StrToIntDef(fpCmd.Params(2), 0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoPrestadoPorIntermediario(ACNPJ_CPF, AIM, APagina);

    RespConsulta := TConsultaNFSeResposta.Create( TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebServices.ConsNFSe);
      fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
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
  APagina := StrToIntDef(fpCmd.Params(2), 0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoPrestadoPorTomador(ACNPJ_CPF, AIM, APagina);

    RespConsulta := TConsultaNFSeResposta.Create( TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebServices.ConsNFSe);
      fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
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
  APagina := StrToIntDef(fpCmd.Params(1), 0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSeServicoPrestadoPorNumero(ANumNFSe, APagina);

    RespConsulta := TConsultaNFSeResposta.Create( TpResp, codUTF8);
    try
      RespConsulta.Processar(ACBrNFSeX.WebServices.ConsNFSe);
      fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
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
      ACBrNFSeX.ConsultarNFSe(InfConsultaNFSe);

      RespConsulta := TConsultaNFSeResposta.Create( TpResp, codUTF8);
      try
         RespConsulta.Processar(ACBrNFSeX.WebServices.ConsNFSe);
         fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
      finally
         RespConsulta.Free;
      end;

    Finally
      InfConsultaNFSe.Free;
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
  APagina := StrToIntDef(fpCmd.Params(2), 0);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSePorFaixa(AFaixaIni, AFaixaFim, APagina);

    RespConsulta := TConsultaNFSeResposta.Create( TpResp, codUTF8);
    try
       RespConsulta.Processar(ACBrNFSeX.WebServices.ConsNFSe);
       fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
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
  APagina := StrToIntDef(fpCmd.Params(2), 0);
  ANumeroLote := fpCmd.Params(3);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarNFSePorPeriodo(ADataIni, ADataFim, APagina, ANumeroLote);

    RespConsulta := TConsultaNFSeResposta.Create( TpResp, codUTF8);
    try
       RespConsulta.Processar(ACBrNFSeX.WebServices.ConsNFSe);
       fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
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

    RespConsulta := TConsultaNFSeResposta.Create( TpResp, codUTF8);
    try
       RespConsulta.Processar(ACBrNFSeX.WebServices.ConsNFSe);
       fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
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

    RespConsulta := TConsultaNFSeporRPSResposta.Create( TpResp, codUTF8);
    try
       RespConsulta.Processar(ACBrNFSeX.WebServices.ConsNfseRps);
       fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
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
  RespConsulta: TConsultaLoteResposta;
begin
  AProtocolo := fpCmd.Params(0);
  ALote := fpCmd.Params(1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarLoteRps(AProtocolo, ALote);

    RespConsulta := TConsultaLoteResposta.Create( TpResp, codUTF8);
    try
       RespConsulta.Processar(ACBrNFSeX.WebServices.ConsLote);
       fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
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
  RespConsulta: TConsultaSituacaoLoteResposta;
begin
  AProtocolo := fpCmd.Params(0);
  ALote := fpCmd.Params(1);

  with TACBrObjetoNFSe(fpObjetoDono) do
  begin
    ACBrNFSeX.ConsultarSituacao(AProtocolo, ALote);

    RespConsulta := TConsultaSituacaoLoteResposta.Create( TpResp, codUTF8);
    try
       RespConsulta.Processar(ACBrNFSeX.WebServices.ConsSitLoteRPS);
       fpCmd.Resposta := sLineBreak + RespConsulta.Gerar;
    finally
       RespConsulta.Free;
    end;

  end;

end;

{ TMetodoGerarLoteRPS }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini RPS
                         ou Uma String com conteúdo txt do RPS
          1 - NumeroLote: String com número do lote a ser adicionado }

procedure TMetodoGerarLoteRPS.Executar;
var
  AIni: String;
  ALote: String;
  RespEnvio: TEnvioResposta;
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
      if ACBrNFSeX.GerarLote(ALote) then

        RespEnvio := TEnvioResposta.Create( TpResp, codUTF8);
        try
           RespEnvio.Processar(ACBrNFSeX);
           fpCmd.Resposta := sLineBreak + RespEnvio.Gerar;
        finally
           RespEnvio.Free;
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

  RespEnvio: TEnvioResposta;
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

        RespEnvio := TEnvioResposta.Create( TpResp, codUTF8);
        try
           RespEnvio.Processar(ACBrNFSeX);
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

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini RPS
                         ou Uma String com conteúdo txt do RPS
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

    LerIniNFSe(AIni);
    ACBrNFSeX.NotasFiscais.NumeroLote:= ALote;
    ACBrNFSeX.NotasFiscais.Transacao:= True;

    {ArqNFe := PathWithDelim(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'LotesNFSe' + PathDelim + 'Lote' + trim(ALote)) + OnlyNumber(
      ACBrNFSeX.NotasFiscais.Items[0].) + '-nfe.xml';
    ACBrNFe.NotasFiscais.GravarXML(ExtractFilePath(ArqNFe));}

    fpCmd.Resposta := 'Total RPS Adicionados= ' + IntToStr(ACBrNFSeX.NotasFiscais.Count) ;

  end;

end;

{ TMetodoCriarEnviarRPS }

{ Params: 0 - AIni - Uma String com um Path completo arquivo .ini RPS
                         ou Uma String com conteúdo txt do RPS
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
  RespEnvio: TEnvioResposta;

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

      RespEnvio := TEnvioResposta.Create( TpResp, codUTF8);
      try
         RespEnvio.Processar(ACBrNFSeX);
         fpCmd.Resposta := fpCmd.Resposta + sLineBreak + RespEnvio.Gerar;
      finally
         RespEnvio.Free;
      end;

    finally
       ACBrNFSeX.NotasFiscais.Clear;
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
    NotasFiscais.Clear;
    NotasFiscais.LoadFromIni ( ArqINI );

  end;

end;

end.

