{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrLibMDFeBase;

interface

uses
  Classes, SysUtils, Forms,
  ACBrLibComum, ACBrLibMDFeDataModule;

type

  { TACBrLibMDFe }

  TACBrLibMDFe = class(TACBrLib)
  private
    FMDFeDM: TLibMDFeDM;

    function SetRetornoMDFeCarregados(const NumMDFe: Integer): Integer;
    function SetRetornoEventoCarregados(const NumEventos: Integer): Integer;

  protected
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property MDFeDM: TLibMDFeDM read FMDFeDM;

    function CarregarXML(const eArquivoOuXML: PAnsiChar): Integer;
    function CarregarINI(const eArquivoOuINI: PAnsiChar): Integer;
    function ObterXml(AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function GravarXml(AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
    function ObterIni(AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function GravarIni(AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
    function CarregarEventoXML(const eArquivoOuXML: PAnsiChar): Integer;
    function CarregarEventoINI(const eArquivoOuINI: PAnsiChar): Integer;
    function LimparLista: Integer;
    function LimparListaEventos: Integer;
    function Assinar: Integer;
    function Validar: Integer;
    function ValidarRegrasdeNegocios(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function VerificarAssinatura(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi: Integer;
                        AEmissao, ACNPJCPF: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function ObterCertificados(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function GetPath(ATipo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function GetPathEvento(ACodEvento: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function StatusServico(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function Consultar(const eChaveOuMDFe: PAnsiChar; AExtrairEventos: Boolean;
                       const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function Enviar(ALote: Integer; AImprimir, ASincrono: Boolean;
                    const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function ConsultarRecibo(ARecibo: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function Cancelar(const eChave, eJustificativa, eCNPJCPF: PAnsiChar; ALote: Integer;
                      const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function EnviarEvento(idLote: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function EncerrarMDFe(const eChaveOuMDFe, eDtEnc, cMunicipioDescarga, nCNPJ, nProtocolo: PAnsiChar;
                          const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function ConsultaMDFeNaoEnc(const nCNPJ: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function DistribuicaoDFePorUltNSU(eCNPJCPF, eultNSU: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function DistribuicaoDFePorNSU(eCNPJCPF, eNSU: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function DistribuicaoDFePorChave(eCNPJCPF, echMDFe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function EnviarEmail(const ePara, eArquivoXmlMDFe: PAnsiChar; const AEnviaPDF: Boolean;
                         const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
    function EnviarEmailEvento(const ePara, eArquivoXmlEvento, eArquivoXmlMDFe: PAnsiChar;
                               const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
    function Imprimir(const cImpressora: PAnsiChar; nNumCopias: Integer; const cProtocolo, bMostrarPreview: PAnsiChar): Integer;
    function ImprimirPDF: Integer;
    function SalvarPDF(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function ImprimirEvento(const eArquivoXmlMDFe, eArquivoXmlEvento: PAnsiChar): Integer;
    function ImprimirEventoPDF(const eArquivoXmlMDFe, eArquivoXmlEvento: PAnsiChar): Integer;
    function SalvarEventoPDF(const eArquivoXmlMDFe, eArquivoXmlEvento, sResposta: PAnsiChar; var esTamanho: Integer): Integer;

  end;

implementation

uses
  ACBrLibConsts, ACBrLibMDFeConsts, ACBrLibConfig, ACBrLibResposta,
  ACBrLibMDFeConfig, ACBrLibMDFeRespostas, ACBrMDFe, ACBrMail,
  ACBrLibConsReciDFe, ACBrLibDistribuicaoDFe, ACBrDFeUtil, ACBrLibCertUtils,
  pcnConversao, pcnAuxiliar, pMDFeConversaoMDFe, blcksock,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings;

{ TACBrLibMDFe }

constructor TACBrLibMDFe.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FMDFeDM := TLibMDFeDM.Create(nil);
  FMDFeDM.Lib := Self;
end;

destructor TACBrLibMDFe.Destroy;
begin
  FMDFeDM.Free;

  inherited Destroy;
end;

procedure TACBrLibMDFe.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibMDFeConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibMDFe.Executar;
begin
  inherited Executar;
  FMDFeDM.AplicarConfiguracoes;
end;

function TACBrLibMDFe.CarregarXML(const eArquivoOuXML: PAnsiChar): Integer;
var
  EhArquivo: boolean;
  ArquivoOuXml: Ansistring;
begin
  try
    ArquivoOuXml := ConverterStringEntrada(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_CarregarXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      GravarLog('MDFE_CarregarXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    MDFeDM.Travar;
    try
      if EhArquivo then
        MDFeDM.ACBrMDFe1.Manifestos.LoadFromFile(ArquivoOuXml)
      else
        MDFeDM.ACBrMDFe1.Manifestos.LoadFromString(ArquivoOuXml);

      Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.CarregarINI(const eArquivoOuINI: PAnsiChar): Integer;
var
  ArquivoOuINI: Ansistring;
begin
  try
    ArquivoOuINI := ConverterStringEntrada(eArquivoOuINI);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_CarregarINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      GravarLog('MDFE_CarregarINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    MDFeDM.Travar;
    try
      MDFeDM.ACBrMDFe1.Manifestos.LoadFromIni(ArquivoOuINI);
      Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.ObterXml(AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
Var
  Resposta: String;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_ObterXml(' + IntToStr(AIndex) + ' )', logCompleto, True)
    else
      GravarLog('MDFE_ObterXml', logNormal);

    MDFeDM.Travar;
    try
      if (AIndex >= MDFeDM.ACBrMDFe1.Manifestos.Count) and (MDFeDM.ACBrMDFe1.Manifestos.Count < 1) then
        raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if EstaVazio(MDFeDM.ACBrMDFe1.Manifestos.Items[AIndex].XMLOriginal) then
        MDFeDM.ACBrMDFe1.Manifestos.Items[AIndex].GerarXML;

      Resposta := MDFeDM.ACBrMDFe1.Manifestos.Items[AIndex].XMLOriginal;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.GravarXml(AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
Var
  ANomeArquivo, APathArquivo: Ansistring;
begin
  try
    ANomeArquivo := ConverterStringEntrada(eNomeArquivo);
    APathArquivo := ConverterStringEntrada(ePathArquivo);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_GravarXml(' + IntToStr(AIndex) + ',' + ANomeArquivo + ',' + APathArquivo + ' )', logCompleto, True)
    else
      GravarLog('MDFE_GravarXml', logNormal);

    MDFeDM.Travar;
    try
      if (AIndex >= MDFeDM.ACBrMDFe1.Manifestos.Count) and (MDFeDM.ACBrMDFe1.Manifestos.Count < 1) then
        raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if MDFeDM.ACBrMDFe1.Manifestos.Items[AIndex].GravarXML(ANomeArquivo, APathArquivo) then
        Result := SetRetorno(ErrOK)
      else
        Result := SetRetorno(ErrGerarXml);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.ObterIni(AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
Var
  Resposta: String;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_ObterIni(' + IntToStr(AIndex) + ' )', logCompleto, True)
    else
      GravarLog('MDFE_ObterIni', logNormal);

    MDFeDM.Travar;
    try
      if (AIndex >= MDFeDM.ACBrMDFe1.Manifestos.Count) and (MDFeDM.ACBrMDFe1.Manifestos.Count < 1) then
        raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if EstaVazio(MDFeDM.ACBrMDFe1.Manifestos.Items[AIndex].XMLOriginal) then
        MDFeDM.ACBrMDFe1.Manifestos.Items[AIndex].GerarXML;

      Resposta := MDFeDM.ACBrMDFe1.Manifestos.Items[AIndex].GerarMDFeIni;
      Resposta := ConverterStringEntrada(Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.GravarIni(AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
Var
  AMDFeIni, ANomeArquivo, APathArquivo: Ansistring;
begin
  try
    ANomeArquivo := ConverterStringEntrada(eNomeArquivo);
    APathArquivo := ConverterStringEntrada(ePathArquivo);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_GravarIni(' + IntToStr(AIndex) + ',' + ANomeArquivo + ',' + APathArquivo + ' )', logCompleto, True)
    else
      GravarLog('MDFE_GravarIni', logNormal);

    MDFeDM.Travar;
    try
      if (AIndex >= MDFeDM.ACBrMDFe1.Manifestos.Count) and (MDFeDM.ACBrMDFe1.Manifestos.Count < 1) then
        raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      ANomeArquivo := ExtractFileName(ANomeArquivo);

      if EstaVazio(ANomeArquivo) then
        raise EACBrLibException.Create(ErrExecutandoMetodo, 'Nome de arquivo não informado');

      if EstaVazio(APathArquivo) then
        APathArquivo := ExtractFilePath(ANomeArquivo);
      if EstaVazio(APathArquivo) then
        APathArquivo := MDFeDM.ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

      APathArquivo := PathWithDelim(APathArquivo);

      if EstaVazio(MDFeDM.ACBrMDFe1.Manifestos.Items[AIndex].XMLOriginal) then
        MDFeDM.ACBrMDFe1.Manifestos.Items[AIndex].GerarXML;

      AMDFeIni := MDFeDM.ACBrMDFe1.Manifestos.Items[AIndex].GerarMDFeIni;

      if not DirectoryExists(APathArquivo) then
        ForceDirectories(APathArquivo);

      WriteToTXT(APathArquivo + ANomeArquivo, AMDFeIni, False, False);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.CarregarEventoXML(const eArquivoOuXML: PAnsiChar): Integer;
var
  EhArquivo: boolean;
  ArquivoOuXml: Ansistring;
begin
  try
    ArquivoOuXml := ConverterStringEntrada(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_CarregarEventoXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      GravarLog('MDFE_CarregarEventoXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    MDFeDM.Travar;
    try
      if EhArquivo then
        MDFeDM.ACBrMDFe1.EventoMDFe.LerXML(ArquivoOuXml)
      else
        MDFeDM.ACBrMDFe1.EventoMDFe.LerXMLFromString(ArquivoOuXml);

      Result := SetRetornoEventoCarregados(MDFeDM.ACBrMDFe1.EventoMDFe.Evento.Count);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.CarregarEventoINI(const eArquivoOuINI: PAnsiChar): Integer;
var
  ArquivoOuINI: Ansistring;
begin
  try
    ArquivoOuINI := ConverterStringEntrada(eArquivoOuINI);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_CarregarEventoINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      GravarLog('MDFE_CarregarEventoINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    MDFeDM.Travar;
    try
      MDFeDM.ACBrMDFe1.EventoMDFe.LerFromIni(ArquivoOuINI);
      Result := SetRetornoEventoCarregados(MDFeDM.ACBrMDFe1.EventoMDFe.Evento.Count);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.LimparLista: Integer;
begin
  try
    GravarLog('MDFE_LimparLista', logNormal);

    MDFeDM.Travar;
    try
      MDFeDM.ACBrMDFe1.Manifestos.Clear;
      Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.LimparListaEventos: Integer;
begin
  try
    GravarLog('MDFE_LimparListaEventos', logNormal);

    MDFeDM.Travar;
    try
      MDFeDM.ACBrMDFe1.EventoMDFe.Evento.Clear;
      Result := SetRetornoEventoCarregados(MDFeDM.ACBrMDFe1.EventoMDFe.Evento.Count);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.Assinar: Integer;
begin
  try
    GravarLog('MDFE_Assinar', logNormal);

    MDFeDM.Travar;
    try
      try
        MDFeDM.ACBrMDFe1.Manifestos.Assinar;
      except
          on E: EACBrMDFeException do
            Result := SetRetorno(ErrAssinarMDFe, ConverterStringSaida(E.Message));
      end;

      Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.Validar: Integer;
begin
  try
    GravarLog('MDFE_Validar', logNormal);

    MDFeDM.Travar;
    try
      try
        MDFeDM.ACBrMDFe1.Manifestos.Validar;
        Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
      except
        on E: EACBrMDFeException do
          Result := SetRetorno(ErrValidacaoMDFe, ConverterStringSaida(E.Message));
      end;
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.ValidarRegrasdeNegocios(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
Var
  Erros: string;
begin
  try
    GravarLog('MDFE_ValidarRegrasdeNegocios', logNormal);

    MDFeDM.Travar;
    try
      Erros := '';
      MDFeDM.ACBrMDFe1.Manifestos.ValidarRegrasdeNegocios(Erros);
      Erros := IfThen<Ansistring>(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Erros), Erros);
      MoverStringParaPChar(Erros, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Erros);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.VerificarAssinatura(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
Var
  Erros: string;
begin
  try
    GravarLog('MDFE_VerificarAssinatura', logNormal);

    MDFeDM.Travar;
    try
      Erros := '';
      MDFeDM.ACBrMDFe1.Manifestos.VerificarAssinatura(Erros);
      Erros := IfThen<Ansistring>(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Erros), Erros);
      MoverStringParaPChar(Erros, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Erros);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi: Integer;
  AEmissao, ACNPJCPF: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
Var
  Resposta, CNPJCPF: Ansistring;
  Emissao: TDateTime;
begin
  try
    Emissao := StrToDate(AEmissao);
    CNPJCPF := ConverterStringEntrada(ACNPJCPF);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_GerarChave(' + IntToStr(ACodigoUF) + ',' + IntToStr(ACodigoNumerico) + ',' +
                      IntToStr(AModelo)  + ',' + IntToStr(AModelo) + ',' + IntToStr(ASerie) + ',' +
                      IntToStr(ANumero) + ',' + IntToStr(ATpEmi) + ',' + DateToStr(Emissao) + ',' +
                      CNPJCPF + ' )', logCompleto, True)
    else
      GravarLog('MDFE_GerarChave', logNormal);

    MDFeDM.Travar;

    try
      Resposta := '';
      Resposta := GerarChaveAcesso(ACodigoUF, Emissao, CNPJCPF, ASerie, ANumero, ATpEmi, ACodigoNumerico, AModelo);
      Resposta := IfThen<Ansistring>(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.ObterCertificados(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
Var
  Resposta: string;
begin
  try
    GravarLog('MDFE_ObterCertificados', logNormal);

    MDFeDM.Travar;
    try
      Resposta := '';
      Resposta := ObterCerticados(MDFeDM.ACBrMDFe1.SSL);
      Resposta := IfThen<Ansistring>(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.GetPath(ATipo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
Var
  Resposta: string;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_GetPath(' + IntToStr(ATipo) + ' )', logCompleto, True)
    else
      GravarLog('MDFE_GetPath', logNormal);

    MDFeDM.Travar;
    try
      with MDFeDM do
      begin
        Resposta := '';

        case ATipo of
          0: Resposta := ACBrMDFe1.Configuracoes.Arquivos.GetPathMDFe();
          1: Resposta := ACBrMDFe1.Configuracoes.Arquivos.GetPathEvento(teCancelamento);
        end;

        Resposta := IfThen<Ansistring>(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.GetPathEvento(ACodEvento: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
Var
  Resposta, CodEvento: string;
  ok: Boolean;
begin
  try
    CodEvento := String(ACodEvento);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_GetPathEvento(' + CodEvento +' )', logCompleto, True)
    else
      GravarLog('MDFE_GetPathEvento', logNormal);

    MDFeDM.Travar;
    try
      with MDFeDM do
      begin
        Resposta := '';
        Resposta := ACBrMDFe1.Configuracoes.Arquivos.GetPathEvento(StrToTpEventoMDFe(ok, CodEvento));
        Resposta := IfThen<Ansistring>(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

{%endregion}

{%region Servicos}

function TACBrLibMDFe.StatusServico(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Resp: TStatusServicoResposta;
  Resposta: String;
begin
  try
    GravarLog('MDFE_StatusServico', logNormal);

    MDFeDM.Travar;
    Resp := TStatusServicoResposta.Create(Config.TipoResposta, Config.CodResposta);
    try
      with MDFeDM.ACBrMDFe1 do
      begin
        WebServices.StatusServico.Executar;

        Resp.Processar(MDFeDM.ACBrMDFe1);
        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      Resp.Free;
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.Consultar(const eChaveOuMDFe: PAnsiChar; AExtrairEventos: Boolean; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
var
  EhArquivo: boolean;
  ChaveOuMDFe, Resposta: Ansistring;
  Resp: TConsultaResposta;
begin
  try
    ChaveOuMDFe := ConverterStringEntrada(eChaveOuMDFe);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_Consultar(' + ChaveOuMDFe  + ', ' + BoolToStr(AExtrairEventos, True) +  ' )', logCompleto, True)
    else
      GravarLog('MDFE_Consultar', logNormal);

    MDFeDM.Travar;
    try
      EhArquivo := StringEhArquivo(ChaveOuMDFe);

      if EhArquivo and not ValidarChave(ChaveOuMDFe) then
      begin
        VerificarArquivoExiste(ChaveOuMDFe);
        MDFeDM.ACBrMDFe1.Manifestos.LoadFromFile(ChaveOuMDFe);
      end;

      if MDFeDM.ACBrMDFe1.Manifestos.Count = 0 then
      begin
        if ValidarChave(ChaveOuMDFe) then
          MDFeDM.ACBrMDFe1.WebServices.Consulta.MDFeChave := ChaveOuMDFe
        else
          raise EACBrLibException.Create(ErrChaveMDFe, Format(SErrChaveInvalida, [ChaveOuMDFe]));
      end
      else
        MDFeDM.ACBrMDFe1.WebServices.Consulta.MDFeChave := StringReplace(
          MDFeDM.ACBrMDFe1.Manifestos.Items[MDFeDM.ACBrMDFe1.Manifestos.Count - 1].MDFe.infMDFe.ID,
          'MDFe','',[rfIgnoreCase]);

      MDFeDM.ACBrMDFe1.WebServices.Consulta.ExtrairEventos := AExtrairEventos;
      Resp := TConsultaResposta.Create(Config.TipoResposta, Config.CodResposta);

      with MDFeDM.ACBrMDFe1 do
      begin
        WebServices.Consulta.Executar;
        Resp.Processar(MDFeDM.ACBrMDFe1);

        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      Resp.Free;
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.Enviar(ALote: Integer; AImprimir, ASincrono: Boolean;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Resposta: Ansistring;
  RespEnvio: TEnvioResposta;
  RespRetorno: TRetornoResposta;
  ImpResp: TLibImpressaoResposta;
  i, j, ImpCount: Integer;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_Enviar(' + IntToStr(ALote) + ','
                                    + BoolToStr(AImprimir, 'Imprimir ,',' , ')
                                    + BoolToStr(ASincrono, 'Sincrono','  ') + ')', logCompleto, True)
    else
      GravarLog('MDFE_Enviar', logNormal);

    MDFeDM.Travar;
    try
      with MDFeDM.ACBrMDFe1 do
      begin
        if Manifestos.Count <= 0 then
          raise EACBrLibException.Create(ErrEnvio, 'ERRO: Nenhuma MDF-e adicionada ao Lote');

        if Manifestos.Count > 50 then
          raise EACBrLibException.Create(ErrEnvio, 'ERRO: Conjunto de MDF-e transmitidas (máximo de 50 MDF-e)' +
                                                   ' excedido. Quantidade atual: ' + IntToStr(Manifestos.Count));

        Resposta := '';
        WebServices.Enviar.Clear;
        WebServices.Retorno.Clear;

        Manifestos.Assinar;
        try
          Manifestos.Validar;
        except
          on E: EACBrMDFeException do
          begin
            Result := SetRetorno(ErrValidacaoMDFe, ConverterStringSaida(E.Message));
            Exit;
          end;
        end;

        if (ALote = 0) then
          WebServices.Enviar.Lote := '1'
        else
          WebServices.Enviar.Lote := IntToStr(ALote);

        WebServices.Enviar.Sincrono := ASincrono;
        WebServices.Enviar.Executar;

        RespEnvio := TEnvioResposta.Create(Config.TipoResposta, Config.CodResposta);
        try
          RespEnvio.Processar(MDFeDM.ACBrMDFe1);
          Resposta := RespEnvio.Gerar;
        finally
          RespEnvio.Free;
        end;

        if not ASincrono or ((NaoEstaVazio(WebServices.Enviar.Recibo)) and (WebServices.Enviar.cStat = 103)) then
        begin
          WebServices.Retorno.Recibo := WebServices.Enviar.Recibo;
          WebServices.Retorno.Executar;

          RespRetorno := TRetornoResposta.Create('MDFe', Config.TipoResposta, Config.CodResposta);
          try
            RespRetorno.Processar(WebServices.Retorno.MDFeRetorno,
                                  WebServices.Retorno.Recibo,
                                  WebServices.Retorno.Msg,
                                  WebServices.Retorno.Protocolo,
                                  WebServices.Retorno.ChaveMDFe);

            Resposta := Resposta + sLineBreak + RespRetorno.Gerar;
          finally
            RespRetorno.Free;
          end;
        end;

        if AImprimir then
        begin
          MDFeDM.ConfigurarImpressao;

          ImpCount := 0;
          for I := 0 to Manifestos.Count - 1 do
          begin
            if Manifestos.Items[I].Confirmado then
            begin
              Manifestos.Items[I].Imprimir;
              Inc(ImpCount);
            end;
          end;

          if ImpCount > 0 then
          begin
            ImpResp := TLibImpressaoResposta.Create(ImpCount, Config.TipoResposta, Config.CodResposta);
            try
              Resposta := Resposta + sLineBreak + ImpResp.Gerar;
            finally
                ImpResp.Free;
            end;
          end;
        end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.ConsultarRecibo(ARecibo: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Resp: TReciboResposta;
  sRecibo, Resposta: Ansistring;
  i, j: Integer;
begin
  try
    sRecibo := ConverterStringEntrada(ARecibo);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_ConsultarRecibo(' + sRecibo + ' )', logCompleto, True)
    else
      GravarLog('MDFE_ConsultarRecibo', logNormal);

    MDFeDM.Travar;

    try
      with MDFeDM.ACBrMDFe1 do
      begin
        WebServices.Recibo.Recibo := sRecibo;
        WebServices.Recibo.Executar;

        Resp := TReciboResposta.Create('MDFe', Config.TipoResposta, Config.CodResposta);
        try
          Resp.Processar(WebServices.Recibo.MDFeRetorno,
                         WebServices.Recibo.Recibo);

          Resposta := Resp.Gerar;
        finally
          Resp.Free;
        end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.Cancelar(const eChave, eJustificativa, eCNPJCPF: PAnsiChar; ALote: Integer;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  AChave, AJustificativa, ACNPJCPF, Resposta: Ansistring;
  Resp: TCancelamentoResposta;
begin
  try
    AChave := ConverterStringEntrada(eChave);
    AJustificativa := ConverterStringEntrada(eJustificativa);
    ACNPJCPF := ConverterStringEntrada(eCNPJCPF);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_Cancelar(' + AChave + ',' + AJustificativa + ',' +
                        ACNPJCPF + ',' + IntToStr(ALote) + ' )', logCompleto, True)
    else
      GravarLog('MDFE_Cancelar', logNormal);

    MDFeDM.Travar;

    try
      if not ValidarChave(AChave) then
        raise EACBrLibException.Create(ErrChaveMDFe, Format(SErrChaveInvalida, [AChave]))
      else
        MDFeDM.ACBrMDFe1.WebServices.Consulta.MDFeChave := AChave;

      if not MDFeDM.ACBrMDFe1.WebServices.Consulta.Executar then
        raise EACBrLibException.Create(ErrConsulta, MDFeDM.ACBrMDFe1.WebServices.Consulta.Msg);

      MDFeDM.ACBrMDFe1.EventoMDFe.Evento.Clear;

      with MDFeDM.ACBrMDFe1.EventoMDFe.Evento.New do
      begin
        Infevento.CNPJCPF := ACNPJCPF;
        if Trim(Infevento.CNPJCPF) = '' then
          Infevento.CNPJCPF := copy(OnlyNumber(MDFeDM.ACBrMDFe1.WebServices.Consulta.MDFeChave), 7, 14)
        else
        begin
          if not ValidarCNPJouCPF(ACNPJCPF) then
            raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));
        end;

        Infevento.nSeqEvento := 1;
        InfEvento.tpAmb := MDFeDM.ACBrMDFe1.Configuracoes.WebServices.Ambiente;
        Infevento.cOrgao := StrToIntDef(copy(OnlyNumber(MDFeDM.ACBrMDFe1.WebServices.Consulta.MDFeChave), 1, 2), 0);
        Infevento.dhEvento := now;
        Infevento.tpEvento := teCancelamento;
        Infevento.chMDFe := MDFeDM.ACBrMDFe1.WebServices.Consulta.MDFeChave;
        Infevento.detEvento.nProt := MDFeDM.ACBrMDFe1.WebServices.Consulta.Protocolo;
        Infevento.detEvento.xJust := AJustificativa;
      end;

      if (ALote = 0) then
        ALote := 1;

      MDFeDM.ACBrMDFe1.WebServices.EnvEvento.idLote := ALote;
      MDFeDM.ACBrMDFe1.WebServices.EnvEvento.Executar;

      Resp := TCancelamentoResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(MDFeDM.ACBrMDFe1);
        Resposta := Resp.Gerar;
      finally
        Resp.Free;
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.EnviarEvento(idLote: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  i, j: integer;
  Resp: TEventoResposta;
  Resposta, chMDFe: String;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_EnviarEvento(' + IntToStr(idLote) + ' )', logCompleto, True)
    else
      GravarLog('MDFE_EnviarEvento', logNormal);

    MDFeDM.Travar;

    try
      with MDFeDM.ACBrMDFe1 do
      begin
        if EventoMDFe.Evento.Count = 0 then
          raise EACBrLibException.Create(ErrEnvioEvento, 'ERRO: Nenhum Evento adicionado ao Lote');

        if EventoMDFe.Evento.Count > 20 then
          raise EACBrLibException.Create(ErrEnvioEvento, 'ERRO: Conjunto de Eventos transmitidos (máximo de 20) ' +
                                                         'excedido. Quantidade atual: ' + IntToStr(EventoMDFe.Evento.Count));

        {Atribuir nSeqEvento, CNPJ, Chave e/ou Protocolo quando não especificar}
        for i := 0 to EventoMDFe.Evento.Count - 1 do
        begin
          if EventoMDFe.Evento.Items[i].InfEvento.nSeqEvento = 0 then
            EventoMDFe.Evento.Items[i].infEvento.nSeqEvento := 1;

          EventoMDFe.Evento.Items[i].InfEvento.tpAmb := Configuracoes.WebServices.Ambiente;

          if Manifestos.Count > 0 then
          begin
            chMDFe := OnlyNumber(EventoMDFe.Evento.Items[i].InfEvento.chMDFe);

            // Se tem a chave da NFe no Evento, procure por ela nas notas carregadas //
            if NaoEstaVazio(chMDFe) then
            begin
              For j := 0 to Manifestos.Count - 1 do
              begin
                if chMDFe = Manifestos.Items[j].NumID then
                  Break;
              end;

              if j = Manifestos.Count then
                raise EACBrLibException.Create(ErrEnvioEvento, 'Não existe MDFe com a chave ['+chMDFe+'] carregada');
            end
            else
              j := 0;

            if trim(EventoMDFe.Evento.Items[i].InfEvento.CNPJCPF) = '' then
              EventoMDFe.Evento.Items[i].InfEvento.CNPJCPF := Manifestos.Items[j].MDFe.Emit.CNPJCPF;

            if chMDFe = '' then
              EventoMDFe.Evento.Items[i].InfEvento.chMDFe := Manifestos.Items[j].NumID;

            if trim(EventoMDFe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
            begin
              if EventoMDFe.Evento.Items[i].infEvento.tpEvento = teCancelamento then
              begin
                EventoMDFe.Evento.Items[i].infEvento.detEvento.nProt := Manifestos.Items[j].MDFe.procMDFe.nProt;

                if trim(EventoMDFe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
                begin
                  WebServices.Consulta.MDFeChave := EventoMDFe.Evento.Items[i].InfEvento.chMDFe;

                  if not WebServices.Consulta.Executar then
                    raise EACBrLibException.Create(ErrEnvioEvento, WebServices.Consulta.Msg);

                  EventoMDFe.Evento.Items[i].infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
                end;
              end;
            end;
          end;
        end;

        if (idLote = 0) then
          idLote := 1;

        WebServices.EnvEvento.idLote := idLote;
        WebServices.EnvEvento.Executar;
      end;

      try
        Resp := TEventoResposta.Create(Config.TipoResposta, Config.CodResposta);
        Resp.Processar(MDFeDM.ACBrMDFe1);
        Resposta := Resp.Gerar;
      finally
        Resp.Free;
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.EncerrarMDFe(const eChaveOuMDFe, eDtEnc, cMunicipioDescarga, nCNPJ, nProtocolo: PAnsiChar;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
Var
  Resp: TEncerramentoResposta;
  ChaveOuMDFe, MunicipioDescarga, CNPJ, Protocolo, Resposta: Ansistring;
  DtEnc: TDateTime;
begin
  try
    ChaveOuMDFe := ConverterStringEntrada(eChaveOuMDFe);
    DtEnc := StrToDateTime(ConverterStringEntrada(eDtEnc));
    MunicipioDescarga := ConverterStringEntrada(cMunicipioDescarga);
    CNPJ := ConverterStringEntrada(nCNPJ);
    Protocolo := ConverterStringEntrada(nProtocolo);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_EncerrarMDFe(' + ChaveOuMDFe + ',' + DateTimeToStr(DtEnc) + ',' +
                     MunicipioDescarga + ',' + CNPJ + ',' + Protocolo + ' )', logCompleto, True)
    else
      GravarLog('MDFE_EncerrarMDFe', logNormal);

    MDFeDM.Travar;

    try
      with MDFeDM.ACBrMDFe1 do
      begin
        Manifestos.Clear;
        if FilesExists(ChaveOuMDFe) then
        begin
          Manifestos.LoadFromFile(ChaveOuMDFe);
          if (Manifestos.Count > 0) then
            ChaveOuMDFe := OnlyNumber(Manifestos.Items[0].MDFe.infMDFe.ID)
          else
            raise EACBrLibException.Create(ErrArquivoNaoExiste, 'Arquivo MDFe inválido: ' + ChaveOuMDFe);
        end
        else if not ValidarChave(ChaveOuMDFe) then
          raise EACBrLibException.Create(ErrChaveMDFe, 'Chave MDFe inválido: '+ ChaveOuMDFe);

        EventoMDFe.Evento.Clear;
        with EventoMDFe.Evento.New do
        begin
          if Trim(CNPJ) = '' then
            CNPJ := copy(ChaveOuMDFe, 7, 14)
          else
          begin
            if not ValidarCNPJouCPF(CNPJ) then
              raise EACBrLibException.Create(ErrCNPJ, 'CNPJ/CPF ' + CNPJ + ' inválido.');
          end;

          infEvento.CNPJCPF := CNPJ;
          infEvento.nSeqEvento := 1;
          infEvento.cOrgao := StrToIntDef(copy(OnlyNumber(ChaveOuMDFe), 1, 2), 0);
          infEvento.tpAmb := Configuracoes.WebServices.Ambiente;
          infEvento.dhEvento := now;
          infEvento.tpEvento := teEncerramento;
          infEvento.chMDFe := ChaveOuMDFe;

          if (Trim(Protocolo) <> '') then
            infEvento.detEvento.nProt := Trim(Protocolo)
          else if ((Manifestos.Count > 0) and (Manifestos.Items[0].MDFe.procMDFe.nProt <> '')) then
            infEvento.detEvento.nProt := Manifestos.Items[0].MDFe.procMDFe.nProt
          else
          begin
            //Realiza Consulta na Sefaz
            WebServices.Consulta.MDFeChave := ChaveOuMDFe;
            WebServices.Consulta.Executar;
            if (WebServices.Consulta.protocolo <> '') then
              infEvento.detEvento.nProt := WebServices.Consulta.Protocolo
            else
              raise EACBrLibException.Create(ErrConsulta, 'Falha na consulta do Protocolo MDFe. ' + WebServices.Consulta.Msg);
          end;

          if (Trim(MunicipioDescarga) <> '') then
          begin
            infEvento.detEvento.cUF := StrToIntDef(copy(MunicipioDescarga, 1, 2), 1);
            infEvento.detEvento.cMun := StrToIntDef(MunicipioDescarga, 1);
          end
          else if ((Manifestos.Count > 0) and
                   (Manifestos.Items[0].MDFe.infDoc.infMunDescarga.Items[0].cMunDescarga > 0)) then
          begin
            infEvento.detEvento.cMun :=
            Manifestos.Items[0].MDFe.infDoc.infMunDescarga.Items[0].cMunDescarga;
            infEvento.detEvento.cUF := StrToIntDef(copy(IntToStr(Manifestos.Items[0].MDFe.infDoc.infMunDescarga.Items[0].cMunDescarga), 1, 2), 1);
          end;

          infEvento.detEvento.dtEnc := DtEnc;
        end;

        WebServices.EnvEvento.idLote := 1;
        WebServices.EnvEvento.Executar;

        Resposta := '';
        Resp := TEncerramentoResposta.Create(Config.TipoResposta, Config.CodResposta);

        try
          Resp.Processar(MDFeDM.ACBrMDFe1);
          Resposta := Resp.Gerar;
        finally
          Resp.Free;
        end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.ConsultaMDFeNaoEnc(const nCNPJ: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
Var
  CNPJ, Resposta: Ansistring;
  Resp: TNaoEncerradosResposta ;
begin
  try
    CNPJ := ConverterStringEntrada(nCNPJ);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFE_ConsultaMDFeNaoEnc(' + CNPJ + ' )', logCompleto, True)
    else
      GravarLog('MDFE_ConsultaMDFeNaoEnc', logNormal);

    MDFeDM.Travar;

    try
      if not ValidarCNPJouCPF(CNPJ) then
        raise EACBrLibException.Create(ErrCNPJ, 'CNPJ/CPF ' + CNPJ + ' invalido.');

      MDFeDM.ACBrMDFe1.WebServices.ConsMDFeNaoEnc.CNPJCPF := CNPJ;
      MDFeDM.ACBrMDFe1.WebServices.ConsMDFeNaoEnc.Executar;

      Resposta := '';
      Resp := TNaoEncerradosResposta.Create(Config.TipoResposta, Config.CodResposta);

      try
        Resp.Processar(MDFeDM.ACBrMDFe1);
        Resposta := Resp.Gerar;
      finally
        Resp.Free;
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      MDFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.DistribuicaoDFePorUltNSU(eCNPJCPF, eultNSU: PAnsiChar;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  AultNSU, ACNPJCPF, Resposta: Ansistring;
  Resp: TDistribuicaoDFeResposta;
begin
  try
    ACNPJCPF := ConverterStringEntrada(eCNPJCPF);
    AultNSU := ConverterStringEntrada(eultNSU);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFe_DistribuicaoDFePorUltNSU(' + ACNPJCPF + ',' + AultNSU + ',' + ' )', logCompleto, True)
    else
      GravarLog('MDFe_DistribuicaoDFePorUltNSU', logNormal);

    MDFeDM.Travar;
    try
      if not ValidarCNPJouCPF(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      with MDFeDM do
      begin
        try
        ACBrMDFe1.WebServices.DistribuicaoDFe.CNPJCPF  := ACNPJCPF;
        ACBrMDFe1.WebServices.DistribuicaoDFe.ultNSU   := AultNSU;
        ACBrMDFe1.WebServices.DistribuicaoDFe.NSU      := '';
        ACBrMDFe1.WebServices.DistribuicaoDFe.chMDFe    := '';

        ACBrMDFe1.WebServices.DistribuicaoDFe.Executar;

        Resp := TDistribuicaoDFeResposta.Create(Config.TipoResposta, Config.CodResposta);
        try
          Resp.Processar(ACBrMDFe1.WebServices.DistribuicaoDFe.retDistDFeInt,
                         ACBrMDFe1.WebServices.DistribuicaoDFe.Msg,
                         ACBrMDFe1.WebServices.DistribuicaoDFe.NomeArq,
                         ACBrMDFe1.WebServices.DistribuicaoDFe.ListaArqs);
          Resposta := Resp.Gerar;
        finally
          Resp.Free;
        end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
        except
          on E: Exception do
          begin
            raise EACBrLibException.Create(ErrRetorno, E.Message + SLineBreak  + '  MotivoRetornadoDoWebService: "' + Trim(ACBrMDFe1.WebServices.DistribuicaoDFe.retDistDFeInt.xMotivo) + '"' );
          end;
        end;
      end;
    finally
      MDFeDM.Destravar;
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.DistribuicaoDFePorNSU(eCNPJCPF, eNSU: PAnsiChar;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  ANSU, ACNPJCPF, Resposta: Ansistring;
  Resp: TDistribuicaoDFeResposta;
begin
  try
    ACNPJCPF := ConverterStringEntrada(eCNPJCPF);
    ANSU := ConverterStringEntrada(eNSU);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFe_DistribuicaoDFePorNSU(' + ACNPJCPF + ',' + ANSU + ',' + ' )', logCompleto, True)
    else
      GravarLog('MDFe_DistribuicaoDFePorNSU', logNormal);

    MDFeDM.Travar;
    try
      if not ValidarCNPJouCPF(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      with MDFeDM do
      begin
        ACBrMDFe1.WebServices.DistribuicaoDFe.CNPJCPF  := ACNPJCPF;
        ACBrMDFe1.WebServices.DistribuicaoDFe.ultNSU   := '';
        ACBrMDFe1.WebServices.DistribuicaoDFe.NSU      := ANSU;
        ACBrMDFe1.WebServices.DistribuicaoDFe.chMDFe    := '';

        ACBrMDFe1.WebServices.DistribuicaoDFe.Executar;

        Resp := TDistribuicaoDFeResposta.Create(Config.TipoResposta, Config.CodResposta);
        try
          Resp.Processar(ACBrMDFe1.WebServices.DistribuicaoDFe.retDistDFeInt,
                         ACBrMDFe1.WebServices.DistribuicaoDFe.Msg,
                         ACBrMDFe1.WebServices.DistribuicaoDFe.NomeArq,
                         ACBrMDFe1.WebServices.DistribuicaoDFe.ListaArqs);

          Resposta := Resp.Gerar;
        finally
          Resp.Free;
        end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      MDFeDM.Destravar;
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.DistribuicaoDFePorChave(eCNPJCPF, echMDFe: PAnsiChar;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  AchMDFe, ACNPJCPF, Resposta: Ansistring;
  Resp: TDistribuicaoDFeResposta;
begin
  try
    ACNPJCPF := ConverterStringEntrada(eCNPJCPF);
    AchMDFe := ConverterStringEntrada(echMDFe);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFe_DistribuicaoDFePorChave(' + ACNPJCPF + ',' + AchMDFe + ' )', logCompleto, True)
    else
      GravarLog('MDFe_DistribuicaoDFePorChave', logNormal);

    MDFeDM.Travar;
    try
      if not ValidarCNPJouCPF(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      if not ValidarChave(AchMDFe) then
        raise EACBrLibException.Create(ErrChaveMDFe, Format(SErrChaveInvalida, [AchMDFe]));

      with MDFeDM do
      begin
        ACBrMDFe1.WebServices.DistribuicaoDFe.CNPJCPF  := ACNPJCPF;
        ACBrMDFe1.WebServices.DistribuicaoDFe.ultNSU   := '';
        ACBrMDFe1.WebServices.DistribuicaoDFe.NSU      := '';
        ACBrMDFe1.WebServices.DistribuicaoDFe.chMDFe    := AchMDFe;

        ACBrMDFe1.WebServices.DistribuicaoDFe.Executar;

        Resp := TDistribuicaoDFeResposta.Create(Config.TipoResposta, Config.CodResposta);
        try
          Resp.Processar(ACBrMDFe1.WebServices.DistribuicaoDFe.retDistDFeInt,
                         ACBrMDFe1.WebServices.DistribuicaoDFe.Msg,
                         ACBrMDFe1.WebServices.DistribuicaoDFe.NomeArq,
                         ACBrMDFe1.WebServices.DistribuicaoDFe.ListaArqs);
          Resposta := Resp.Gerar;
        finally
          Resp.Free;
        end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      MDFeDM.Destravar;
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.EnviarEmail(const ePara, eArquivoXmlMDFe: PAnsiChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
var
  APara, AArquivoXmlMDFe, AAssunto, ACC, AAnexos, AMensagem: Ansistring;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
  Resposta: TLibMDFeResposta;
begin
  try
    APara := ConverterStringEntrada(ePara);
    AArquivoXmlMDFe := ConverterStringEntrada(eArquivoXmlMDFe);
    AAssunto := ConverterStringEntrada(eAssunto);
    ACC := ConverterStringEntrada(eCC);
    AAnexos := ConverterStringEntrada(eAnexos);
    AMensagem := ConverterStringEntrada(eMensagem);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFe_EnviarEmail(' + APara + ',' + AArquivoXmlMDFe + ',' +
                     BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' + ACC + ',' +
                     AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      GravarLog('MDFe_EnviarEmail', logNormal);

    MDFeDM.Travar;

    try
      with MDFeDM do
      begin
        EhArquivo := StringEhArquivo(AArquivoXmlMDFe);

        if EhArquivo then
          VerificarArquivoExiste(AArquivoXmlMDFe);

        if EhArquivo then
          ACBrMDFe1.Manifestos.LoadFromFile(AArquivoXmlMDFe)
        else
          ACBrMDFe1.Manifestos.LoadFromString(AArquivoXmlMDFe);

        if ACBrMDFe1.Manifestos.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfMDFeCarregados, [ACBrMDFe1.Manifestos.Count]))
        else
        begin
          slMensagemEmail := TStringList.Create;
          slCC := TStringList.Create;
          slAnexos := TStringList.Create;
          Resposta := TLibMDFeResposta.Create('EnviaEmail', Config.TipoResposta, Config.CodResposta);

          try
            with ACBrMDFe1.Mail do
            begin
              slMensagemEmail.DelimitedText:= sLineBreak;
              slMensagemEmail.Text := StringReplace(AMensagem, ';', sLineBreak, [rfReplaceAll]);

              slCC.DelimitedText:= sLineBreak;
              slCC.Text := StringReplace(ACC, ';', sLineBreak, [rfReplaceAll]);

              slAnexos.DelimitedText := sLineBreak;
              slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

              if(AEnviaPDF) then
                MDFeDM.ConfigurarImpressao('', True);

              ACBrMDFe1.Manifestos.Items[0].EnviarEmail(APara,
                                                        AAssunto,
                                                        slMensagemEmail,
                                                        AEnviaPDF, // Enviar PDF junto
                                                        slCC,      // Lista com emails que serão enviado cópias - TStrings
                                                        slAnexos); // Lista de slAnexos - TStrings

              Resposta.Msg := 'Email enviado com sucesso';
              Result := SetRetorno(ErrOK, Resposta.Gerar);
            end;
          finally
            Resposta.Free;
            slCC.Free;
            slAnexos.Free;
            slMensagemEmail.Free;
            if AEnviaPDF then MDFeDM.FinalizarImpressao;
          end;
        end;
      end;
    finally
      MDFeDM.Destravar;
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.EnviarEmailEvento(const ePara, eArquivoXmlEvento, eArquivoXmlMDFe: PAnsiChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
var
  APara, AArquivoXmlEvento, AArquivoXmlMDFe, AAssunto, ACC, AAnexos, AMensagem,
  ArqPDF: Ansistring;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
  Resposta: TLibMDFeResposta;
begin
  try
    APara := ConverterStringEntrada(ePara);
    AArquivoXmlEvento := ConverterStringEntrada(eArquivoXmlEvento);
    AArquivoXmlMDFe := ConverterStringEntrada(eArquivoXmlMDFe);
    AAssunto := ConverterStringEntrada(eAssunto);
    ACC := ConverterStringEntrada(eCC);
    AAnexos := ConverterStringEntrada(eAnexos);
    AMensagem := ConverterStringEntrada(eMensagem);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFe_EnviarEmailEvento(' + APara + ',' + AArquivoXmlEvento + ',' +
             AArquivoXmlMDFe + ',' + BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' +
             ACC + ',' + AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      GravarLog('MDFe_EnviarEmailEvento', logNormal);

    MDFeDM.Travar;
    try
      with MDFeDM.ACBrMDFe1 do
      begin
        EventoMDFe.Evento.Clear;
        Manifestos.Clear;

        EhArquivo := StringEhArquivo(AArquivoXmlEvento);

        if EhArquivo then
          VerificarArquivoExiste(AArquivoXmlEvento);

        if EhArquivo then
          EventoMDFe.LerXML(AArquivoXmlEvento)
        else
          EventoMDFe.LerXMLFromString(AArquivoXmlEvento);

        EhArquivo := StringEhArquivo(AArquivoXmlMDFe);

        if EhArquivo then
          VerificarArquivoExiste(AArquivoXmlMDFe);

        if EhArquivo then
          Manifestos.LoadFromFile(AArquivoXmlMDFe)
        else
          Manifestos.LoadFromString(AArquivoXmlMDFe);

        if EventoMDFe.Evento.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfEventosCarregados, [EventoMDFe.Evento.Count]))
        else
        begin
          slMensagemEmail := TStringList.Create;
          slCC := TStringList.Create;
          slAnexos := TStringList.Create;

          Resposta := TLibMDFeResposta.Create('EnviaEmail', Config.TipoResposta, Config.CodResposta);
          try
            if AEnviaPDF then
            begin
              try
                MDFeDM.ConfigurarImpressao('', True);
                ImprimirEventoPDF;

                ArqPDF := OnlyNumber(EventoMDFe.Evento[0].Infevento.id);
                ArqPDF := PathWithDelim(DAMDFe.PathPDF)+ArqPDF+'-procEventoMDFe.pdf';
              except
                raise EACBrLibException.Create(ErrRetorno, 'Erro ao criar o arquivo PDF');
              end;
            end;

            with mail do
            begin
              slMensagemEmail.DelimitedText:= sLineBreak;
              slMensagemEmail.Text := StringReplace(AMensagem, ';', sLineBreak, [rfReplaceAll]);

              slCC.DelimitedText:= sLineBreak;
              slCC.Text := StringReplace(ACC, ';', sLineBreak, [rfReplaceAll]);

              slAnexos.DelimitedText := sLineBreak;
              slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

              slAnexos.Add(AArquivoXmlEvento);

              if AEnviaPDF then
                slAnexos.Add(ArqPDF);
              try
                MDFeDM.ACBrMDFe1.EnviarEmail(APara,
                                             AAssunto,
                                             slMensagemEmail,
                                             slCC,      // Lista com emails que serão enviado cópias - TStrings
                                             slAnexos); // Lista de slAnexos - TStrings

                Resposta.Msg := 'Email enviado com sucesso';
                Result := SetRetorno(ErrOK, Resposta.Gerar);
              except
                on E: Exception do
                  raise EACBrLibException.Create(ErrRetorno, 'Erro ao enviar email' + sLineBreak + E.Message);
              end;
            end;
          finally
            Resposta.Free;
            slCC.Free;
            slAnexos.Free;
            slMensagemEmail.Free;
            if AEnviaPDF then MDFeDM.FinalizarImpressao;
          end;
        end;
      end;
    finally
      MDFeDM.Destravar;
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.Imprimir(const cImpressora: PAnsiChar; nNumCopias: Integer; const cProtocolo, bMostrarPreview: PAnsiChar): Integer;
Var
  Resposta: TLibImpressaoResposta;
  Impressora, Protocolo,
  MostrarPreview: Ansistring;
begin
  try
    Impressora := ConverterStringEntrada(cImpressora);
    Protocolo := ConverterStringEntrada(cProtocolo);
    MostrarPreview := ConverterStringEntrada(bMostrarPreview);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFe_Imprimir(' + Impressora + ',' + IntToStr(nNumCopias) + ',' +
                     Protocolo + ',' + MostrarPreview + ')', logCompleto, True)
    else
      GravarLog('MDFe_Imprimir', logNormal);

    MDFeDM.Travar;

    Resposta := TLibImpressaoResposta.Create(MDFeDM.ACBrMDFe1.Manifestos.Count, Config.TipoResposta, Config.CodResposta);

    try
      MDFeDM.ConfigurarImpressao(Impressora, False, Protocolo, MostrarPreview);
      if nNumCopias > 0 then
        MDFeDM.ACBrMDFe1.DAMDFE.NumCopias := nNumCopias;
      MDFeDM.ACBrMDFe1.Manifestos.Imprimir;
      Result := SetRetorno(ErrOK, Resposta.Gerar);
    finally
      MDFeDM.FinalizarImpressao;
      Resposta.Free;
      MDFeDM.Destravar;
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.ImprimirPDF: Integer;
Var
  Resposta: TLibImpressaoResposta;
begin
  try
    GravarLog('MDFe_ImprimirPDF', logNormal);

    MDFeDM.Travar;
    Resposta := TLibImpressaoResposta.Create(MDFeDM.ACBrMDFe1.Manifestos.Count, Config.TipoResposta,
                                               Config.CodResposta);

    try
      MDFeDM.ConfigurarImpressao('', True);
      MDFeDM.ACBrMDFe1.Manifestos.ImprimirPDF;

      Resposta.Msg := MDFeDM.ACBrMDFe1.DAMDFE.ArquivoPDF;
      Result := SetRetorno(ErrOK, Resposta.Gerar);
    finally
      Resposta.Free;
      MDFeDM.FinalizarImpressao;
      MDFeDM.Destravar;
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.SalvarPDF(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  AStream: TMemoryStream;
  Resposta: Ansistring;
begin
  try
     GravarLog('MDFe_SalvarPDF', logNormal);

     MDFeDM.Travar;

     AStream := TMemoryStream.Create;

     try
       MDFeDM.ConfigurarImpressao('', True);

       MDFeDM.ACBrMDFe1.Manifestos.ImprimirPDF(AStream);
       Resposta := StreamToBase64(AStream);

       MoverStringParaPChar(Resposta, sResposta, esTamanho);
       Result := SetRetorno(ErrOK, Resposta);
     finally
       MDFeDM.FinalizarImpressao;
       AStream.Free;
       MDFeDM.Destravar;
     end;

  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.ImprimirEvento(const eArquivoXmlMDFe, eArquivoXmlEvento: PAnsiChar): Integer;
var
  EhArquivo: boolean;
  AArquivoXmlMDFe, AArquivoXmlEvento: Ansistring;
  Resposta: TLibImpressaoResposta;
begin
  try
    AArquivoXmlMDFe := ConverterStringEntrada(eArquivoXmlMDFe);
    AArquivoXmlEvento := ConverterStringEntrada(eArquivoXmlEvento);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFe_ImprimirEvento(' + AArquivoXmlMDFe + ',' + AArquivoXmlEvento + ' )', logCompleto, True)
    else
      GravarLog('MDFe_ImprimirEvento', logNormal);

    MDFeDM.Travar;
    Resposta := TLibImpressaoResposta.Create(MDFeDM.ACBrMDFe1.EventoMDFe.Evento.Count, Config.TipoResposta,
                                               Config.CodResposta);

    try
      EhArquivo := StringEhArquivo(AArquivoXmlMDFe);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlMDFe);

      if EhArquivo then
        MDFeDM.ACBrMDFe1.Manifestos.LoadFromFile(AArquivoXmlMDFe)
      else
        MDFeDM.ACBrMDFe1.Manifestos.LoadFromString(AArquivoXmlMDFe);

      EhArquivo := StringEhArquivo(AArquivoXmlEvento);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlEvento);

      if EhArquivo then
        MDFeDM.ACBrMDFe1.EventoMDFe.LerXML(AArquivoXmlEvento)
      else
        MDFeDM.ACBrMDFe1.EventoMDFe.LerXMLFromString(AArquivoXmlEvento);

      MDFeDM.ConfigurarImpressao;
      MDFeDM.ACBrMDFe1.ImprimirEvento;

      Result := SetRetorno(ErrOK, Resposta.Gerar);
    finally
      Resposta.Free;
      MDFeDM.FinalizarImpressao;
      MDFeDM.Destravar;
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.ImprimirEventoPDF(const eArquivoXmlMDFe, eArquivoXmlEvento: PAnsiChar): Integer;
var
  EhArquivo: boolean;
  AArquivoXmlMDFe, AArquivoXmlEvento: Ansistring;
  Resposta: TLibImpressaoResposta;
begin
  try
    AArquivoXmlMDFe := ConverterStringEntrada(eArquivoXmlMDFe);
    AArquivoXmlEvento := ConverterStringEntrada(eArquivoXmlEvento);

    if Config.Log.Nivel > logNormal then
      GravarLog('MDFe_ImprimirEventoPDF(' + AArquivoXmlMDFe + ',' + AArquivoXmlEvento + ' )', logCompleto, True)
    else
      GravarLog('MDFe_ImprimirEventoPDF', logNormal);

    MDFeDM.Travar;
    Resposta := TLibImpressaoResposta.Create(MDFeDM.ACBrMDFe1.EventoMDFe.Evento.Count, Config.TipoResposta,
                                               Config.CodResposta);
    try
      EhArquivo := StringEhArquivo(AArquivoXmlMDFe);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlMDFe);

      if EhArquivo then
        MDFeDM.ACBrMDFe1.Manifestos.LoadFromFile(AArquivoXmlMDFe)
      else
        MDFeDM.ACBrMDFe1.Manifestos.LoadFromString(AArquivoXmlMDFe);

      EhArquivo := StringEhArquivo(AArquivoXmlEvento);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlEvento);

      if EhArquivo then
        MDFeDM.ACBrMDFe1.EventoMDFe.LerXML(AArquivoXmlEvento)
      else
        MDFeDM.ACBrMDFe1.EventoMDFe.LerXMLFromString(AArquivoXmlEvento);

      MDFeDM.ConfigurarImpressao('', True);
      MDFeDM.ACBrMDFe1.ImprimirEventoPDF;

      Resposta.Msg := MDFeDM.ACBrMDFe1.DAMDFE.ArquivoPDF;
      Result := SetRetorno(ErrOK, Resposta.Gerar);
    finally
      Resposta.Free;
      MDFeDM.FinalizarImpressao;
      MDFeDM.Destravar;
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.SalvarEventoPDF(const eArquivoXmlMDFe, eArquivoXmlEvento, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  EhArquivo: boolean;
  AArquivoXmlMDFe: string;
  AArquivoXmlEvento: string;
  AStream: TMemoryStream;
  Resposta: Ansistring;
begin
  try
    AArquivoXmlMDFe := ConverterStringEntrada(eArquivoXmlMDFe);
    AArquivoXmlEvento := ConverterStringEntrada(eArquivoXmlEvento);

    if Config.Log.Nivel > logNormal then
       GravarLog('MDFe_SalvarEventoPDF(' + AArquivoXmlMDFe + ',' + AArquivoXmlEvento + ' )', logCompleto, True)
    else
        GravarLog('MDFe_SalvarEventoPDF', logNormal);

    MDFeDM.Travar;
    AStream := TMemoryStream.Create;

    try
      EhArquivo := StringEhArquivo(AArquivoXmlMDFe);

      if EhArquivo then
      VerificarArquivoExiste(AArquivoXmlMDFe);

      if EhArquivo then
      MDFeDM.ACBrMDFe1.Manifestos.LoadFromFile(AArquivoXmlMDFe)
      else
      MDFeDM.ACBrMDFe1.Manifestos.LoadFromString(AArquivoXmlMDFe);

      EhArquivo := StringEhArquivo(AArquivoXmlEvento);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlEvento);

      if EhArquivo then
        MDFeDM.ACBrMDFe1.EventoMDFe.LerXML(AArquivoXmlEvento)
      else
        MDFeDM.ACBrMDFe1.EventoMDFe.LerXMLFromString(AArquivoXmlEvento);

      MDFeDM.ConfigurarImpressao('', True);
      MDFeDM.ACBrMDFe1.DAMDFE.ImprimirEVENTOPDF(AStream);

      Resposta := StreamToBase64(AStream);

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);

    finally
      MDFeDM.FinalizarImpressao;
      AStream.Free;
      MDFeDM.Destravar;
    end;

  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibMDFe.SetRetornoMDFeCarregados(const NumMDFe: Integer): Integer;
begin
  Result := SetRetorno( 0, {NumMDFe,} Format(SInfMDFeCarregados, [NumMDFe]));
end;

function TACBrLibMDFe.SetRetornoEventoCarregados(const NumEventos: Integer): Integer;
begin
  Result := SetRetorno( 0, {NumEventos,} Format(SInfEventosCarregados, [NumEventos]));
end;

end.
