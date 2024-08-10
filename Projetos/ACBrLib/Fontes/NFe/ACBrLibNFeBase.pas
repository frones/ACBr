{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibNFeBase;

interface

uses
  Classes, SysUtils, Forms,
  ACBrLibComum, ACBrLibNFeDataModule;

type

  { TACBrLibNFe }

  TACBrLibNFe = class(TACBrLib)
  private
    FNFeDM: TLibNFeDM;

    function SetRetornoNFeCarregadas(const NumNFe: integer): integer;
    function SetRetornoEventoCarregados(const NumEventos: integer): integer;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property NFeDM: TLibNFeDM read FNFeDM;

    function CarregarXML(const eArquivoOuXML: PAnsiChar): Integer;
    function CarregarINI(const eArquivoOuINI: PAnsiChar): Integer;
    function ObterXml(AIndex: Integer; const sResposta: PAnsiChar;
      var esTamanho: Integer): Integer;
    function GravarXml(AIndex: Integer;
      const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
    function ObterIni(AIndex: Integer; const sResposta: PAnsiChar;
      var esTamanho: Integer): Integer;
    function GravarIni(AIndex: Integer;
      const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
    function CarregarEventoXML(const eArquivoOuXML: PAnsiChar): Integer;
    function CarregarEventoINI(const eArquivoOuINI: PAnsiChar): Integer;
    function LimparLista: Integer;
    function LimparListaEventos: Integer;
    function Assinar: Integer;
    function Validar: Integer;
    function ValidarRegrasdeNegocios(const sResposta: PAnsiChar;
      var esTamanho: Integer): Integer;
    function VerificarAssinatura(const sResposta: PAnsiChar;
      var esTamanho: Integer): Integer;
    function GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie,
      ANumero, ATpEmi: Integer; AEmissao, ACNPJCPF: PAnsiChar;
      const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function ObterCertificados(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function GetPath(ATipo: Integer; const sResposta: PAnsiChar;
      var esTamanho: Integer): Integer;
    function GetPathEvento(ACodEvento: PAnsiChar; const sResposta: PAnsiChar;
      var esTamanho: Integer): Integer;
    function StatusServico(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function Consultar(const eChaveOuNFe: PAnsiChar; AExtrairEventos: boolean;
      const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function Inutilizar(const ACNPJCPF, AJustificativa: PAnsiChar;
      Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
      const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function Enviar(ALote: integer; AImprimir, ASincrono, AZipado: boolean;
      const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function ConsultarRecibo(ARecibo: PAnsiChar; const sResposta: PAnsiChar;
      var esTamanho: Integer): Integer;
    function Cancelar(const eChave, eJustificativa, eCNPJCPF: PAnsiChar;
      ALote: integer; const sResposta: PAnsiChar;
      var esTamanho: Integer): Integer;
    function EnviarEvento(idLote: integer; const sResposta: PAnsiChar;
      var esTamanho: Integer): Integer;
    function ConsultaCadastro(cUF, nDocumento: PAnsiChar; nIE: boolean;
      const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function DistribuicaoDFePorUltNSU(const AcUFAutor: integer;
      eCNPJCPF, eultNSU: PAnsiChar;
      const sResposta: PAnsiChar;
      var esTamanho: Integer): Integer;
    function DistribuicaoDFe(const AcUFAutor: integer;
      eCNPJCPF, eultNSU, eArquivoOuXML: PAnsiChar;
      const sResposta: PAnsiChar;
      var esTamanho: Integer): Integer;
    function DistribuicaoDFePorNSU(const AcUFAutor: integer;
      eCNPJCPF, eNSU: PAnsiChar; const sResposta: PAnsiChar;
      var esTamanho: Integer): Integer;
    function DistribuicaoDFePorChave(const AcUFAutor: integer;
      eCNPJCPF, echNFe: PAnsiChar; const sResposta: PAnsiChar;
      var esTamanho: Integer): Integer;
    function EnviarEmail(const ePara, eXmlNFe: PAnsiChar; const AEnviaPDF: boolean;
      const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
    function EnviarEmailEvento(const ePara, eXmlEvento, eXmlNFe: PAnsiChar;
      const AEnviaPDF: boolean;
      const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
    function Imprimir(const cImpressora: PAnsiChar; nNumCopias: integer;
      const cProtocolo, bMostrarPreview, cMarcaDagua,
      bViaConsumidor, bSimplificado: PAnsiChar): Integer;
    function ImprimirPDF: Integer;
    function SalvarPDF(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function ImprimirEvento(const eArquivoXmlNFe, eArquivoXmlEvento: PAnsiChar): Integer;
    function ImprimirEventoPDF(const eArquivoXmlNFe, eArquivoXmlEvento: PAnsiChar): Integer;
    function SalvarEventoPDF(const eArquivoXmlNFe, eArquivoXmlEvento, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function ImprimirInutilizacao(const eArquivoXml: PAnsiChar): Integer;
    function ImprimirInutilizacaoPDF(const eArquivoXml: PAnsiChar): Integer;
    function SalvarInutilizacaoPDF(const eArquivoXml, sResposta: PAnsiChar; var esTamanho: Integer): Integer;

  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrNFeDANFeESCPOS, ACBrLibConsts, ACBrLibNFeConsts, ACBrLibConfig,
  ACBrLibResposta, ACBrLibDistribuicaoDFe, ACBrLibConsReciDFe,
  ACBrLibConsultaCadastro, ACBrLibNFeConfig, ACBrLibNFeRespostas,
  ACBrDFeUtil, ACBrNFe, ACBrMail, ACBrLibCertUtils,
  pcnConversao, pcnConversaoNFe, pcnAuxiliar, blcksock, strutils;

{ TACBrLibNFe }

constructor TACBrLibNFe.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  FNFeDM := TLibNFeDM.Create(nil);
  FNFeDM.Lib := self;
end;

destructor TACBrLibNFe.Destroy;
begin
  FNFeDM.Free;

  inherited Destroy;
end;

procedure TACBrLibNFe.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibNFe.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibNFe.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibNFeConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibNFe.Executar;
begin
  inherited Executar;
  FNFeDM.AplicarConfiguracoes;
end;

function TACBrLibNFe.SetRetornoNFeCarregadas(const NumNFe: integer): integer;
begin
  Result := SetRetorno(0, Format(SInfNFeCarregadas, [NumNFe]));
end;

function TACBrLibNFe.SetRetornoEventoCarregados(const NumEventos: integer): integer;
begin
  Result := SetRetorno(0, Format(SInfEventosCarregados, [NumEventos]));
end;

function TACBrLibNFe.CarregarXML(const eArquivoOuXML: PAnsiChar): Integer;
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    ArquivoOuXml := ConverterStringEntrada(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_CarregarXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      GravarLog('NFE_CarregarXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    NFeDM.Travar;
    try
      if EhArquivo then
        NFeDM.ACBrNFe1.NotasFiscais.LoadFromFile(ArquivoOuXml)
      else
        NFeDM.ACBrNFe1.NotasFiscais.LoadFromString(ArquivoOuXml);

      Result := SetRetornoNFeCarregadas(NFeDM.ACBrNFe1.NotasFiscais.Count);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.CarregarINI(const eArquivoOuINI: PAnsiChar): Integer;
var
  ArquivoOuINI: string;
begin
  try
    ArquivoOuINI := ConverterStringEntrada(eArquivoOuINI);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_CarregarINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      GravarLog('NFE_CarregarINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    NFeDM.Travar;

    try
      NFeDM.ACBrNFe1.NotasFiscais.LoadFromIni(ArquivoOuINI);
      Result := SetRetornoNFeCarregadas(NFeDM.ACBrNFe1.NotasFiscais.Count);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.ObterXml(AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Resposta: Ansistring;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_ObterXml(' + IntToStr(AIndex) + ' )', logCompleto, True)
    else
      GravarLog('NFE_ObterXml', logNormal);

    NFeDM.Travar;

    try
      if (NFeDM.ACBrNFe1.NotasFiscais.Count < 1) or (AIndex < 0) or
         (AIndex >= NFeDM.ACBrNFe1.NotasFiscais.Count) then
         raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if EstaVazio(NFeDM.ACBrNFe1.NotasFiscais.Items[AIndex].XMLOriginal) then
        NFeDM.ACBrNFe1.NotasFiscais.Items[AIndex].GerarXML;

      Resposta := NFeDM.ACBrNFe1.NotasFiscais.Items[AIndex].XMLOriginal;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.GravarXml(AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
var
  ANomeArquivo, APathArquivo: string;
begin
  try
    ANomeArquivo := ConverterStringEntrada(eNomeArquivo);
    APathArquivo := ConverterStringEntrada(ePathArquivo);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_GravarXml(' + IntToStr(AIndex) + ',' +
        ANomeArquivo + ',' + APathArquivo + ' )', logCompleto, True)
    else
      GravarLog('NFE_GravarXml', logNormal);

    NFeDM.Travar;
    try
      if (NFeDM.ACBrNFe1.NotasFiscais.Count < 1) or (AIndex < 0) or
         (AIndex >= NFeDM.ACBrNFe1.NotasFiscais.Count) then
         raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if NFeDM.ACBrNFe1.NotasFiscais.Items[AIndex].GravarXML(ANomeArquivo, APathArquivo) then
        Result := SetRetorno(ErrOK)
      else
        Result := SetRetorno(ErrGerarXml);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.ObterIni(AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Resposta: Ansistring;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_ObterIni(' + IntToStr(AIndex) + ' )', logCompleto, True)
    else
      GravarLog('NFE_ObterIni', logNormal);

    NFeDM.Travar;
    try
      if (NFeDM.ACBrNFe1.NotasFiscais.Count < 1) or (AIndex < 0) or (AIndex >= NFeDM.ACBrNFe1.NotasFiscais.Count) then
        raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if EstaVazio(NFeDM.ACBrNFe1.NotasFiscais.Items[AIndex].XMLOriginal) then
        NFeDM.ACBrNFe1.NotasFiscais.Items[AIndex].GerarXML;

      Resposta := NFeDM.ACBrNFe1.NotasFiscais.Items[AIndex].GerarNFeIni;
      Resposta := ConverterStringSaida( Resposta );
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.GravarIni(AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
var
  ANFeIni, ANomeArquivo, APathArquivo: string;
begin
  try
    ANomeArquivo := ConverterStringEntrada(eNomeArquivo);
    APathArquivo := ConverterStringEntrada(ePathArquivo);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_GravarIni(' + IntToStr(AIndex) + ',' +
        ANomeArquivo + ',' + APathArquivo + ' )', logCompleto, True)
    else
      GravarLog('NFE_GravarIni', logNormal);

    NFeDM.Travar;
    try
      if (NFeDM.ACBrNFe1.NotasFiscais.Count < 1) or (AIndex < 0) or (AIndex >= NFeDM.ACBrNFe1.NotasFiscais.Count) then
        raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      ANomeArquivo := ExtractFileName(ANomeArquivo);

      if EstaVazio(ANomeArquivo) then
        raise EACBrLibException.Create(ErrExecutandoMetodo, 'Nome de arquivo não informado');

      if EstaVazio(APathArquivo) then
        APathArquivo := ExtractFilePath(ANomeArquivo);
      if EstaVazio(APathArquivo) then
        APathArquivo := NFeDM.ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

      APathArquivo := PathWithDelim(APathArquivo);

      if EstaVazio(NFeDM.ACBrNFe1.NotasFiscais.Items[AIndex].XMLOriginal) then
        NFeDM.ACBrNFe1.NotasFiscais.Items[AIndex].GerarXML;

      ANFeIni := NFeDM.ACBrNFe1.NotasFiscais.Items[AIndex].GerarNFeIni;
      if not DirectoryExists(APathArquivo) then
        ForceDirectories(APathArquivo);

      WriteToTXT(APathArquivo + ANomeArquivo, ANFeIni, False, False);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.CarregarEventoXML(const eArquivoOuXML: PAnsiChar): Integer;
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    ArquivoOuXml := ConverterStringEntrada(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_CarregarEventoXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      GravarLog('NFE_CarregarEventoXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    NFeDM.Travar;
    try
      if EhArquivo then
        NFeDM.ACBrNFe1.EventoNFe.LerXML(ArquivoOuXml)
      else
        NFeDM.ACBrNFe1.EventoNFe.LerXMLFromString(ArquivoOuXml);

      Result := SetRetornoEventoCarregados(NFeDM.ACBrNFe1.EventoNFe.Evento.Count);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.CarregarEventoINI(const eArquivoOuINI: PAnsiChar): Integer;
var
  ArquivoOuINI: string;
begin
  try
    ArquivoOuINI := ConverterStringEntrada(eArquivoOuINI);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_CarregarEventoINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      GravarLog('NFE_CarregarEventoINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    NFeDM.Travar;
    try
      NFeDM.ACBrNFe1.EventoNFe.LerFromIni(ArquivoOuINI, False);
      Result := SetRetornoEventoCarregados(NFeDM.ACBrNFe1.EventoNFe.Evento.Count);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.LimparLista: Integer;
begin
  try
    GravarLog('NFE_LimparLista', logNormal);

    NFeDM.Travar;
    try
      NFeDM.ACBrNFe1.NotasFiscais.Clear;
      Result := SetRetornoNFeCarregadas(NFeDM.ACBrNFe1.NotasFiscais.Count);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.LimparListaEventos: Integer;
begin
  try
    GravarLog('NFE_LimparListaEventos', logNormal);


    NFeDM.Travar;
    try
      NFeDM.ACBrNFe1.EventoNFe.Evento.Clear;
      Result := SetRetornoEventoCarregados(NFeDM.ACBrNFe1.EventoNFe.Evento.Count);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.Assinar: Integer;
begin
  try
    GravarLog('NFe_Assinar', logNormal);

    NFeDM.Travar;
    try
      try
        NFeDM.ACBrNFe1.NotasFiscais.Assinar;
      except
        on E: EACBrNFeException do
          Result := SetRetorno(ErrAssinarNFe, ConverterStringSaida(E.Message));
      end;

      Result := SetRetornoNFeCarregadas(NFeDM.ACBrNFe1.NotasFiscais.Count);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.Validar: Integer;
begin
  try
    GravarLog('NFE_Validar', logNormal);

    NFeDM.Travar;
    try
      try
        NFeDM.ACBrNFe1.NotasFiscais.Validar;
        Result := SetRetornoNFeCarregadas(NFeDM.ACBrNFe1.NotasFiscais.Count);
      except
        on E: EACBrNFeException do
          Result := SetRetorno(ErrValidacaoNFe, ConverterStringSaida(E.Message));
      end;
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.ValidarRegrasdeNegocios(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Erros: Ansistring;
begin
  try
    GravarLog('NFE_ValidarRegrasdeNegocios', logNormal);

    NFeDM.Travar;
    try
      Erros := '';
      NFeDM.ACBrNFe1.NotasFiscais.ValidarRegrasdeNegocios(Erros);
      Erros := ConverterStringSaida(Erros);
      MoverStringParaPChar(Erros, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Erros);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.VerificarAssinatura(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Erros: Ansistring;
begin
  try
    GravarLog('NFE_VerificarAssinatura', logNormal);

    NFeDM.Travar;
    try
      Erros := '';
      NFeDM.ACBrNFe1.NotasFiscais.VerificarAssinatura(Erros);
      Erros := ConverterStringSaida(Erros);
      MoverStringParaPChar(Erros, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Erros);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.GerarChave(ACodigoUF, ACodigoNumerico,  AModelo, ASerie, ANumero, ATpEmi: Integer;
                                AEmissao, ACNPJCPF: PAnsiChar;const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  CNPJCPF: string;
  Emissao: TDateTime;
  Resposta: Ansistring;
begin
  try
    Emissao := StrToDate(AEmissao);
    CNPJCPF := ConverterStringEntrada(ACNPJCPF);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_GerarChave(' + IntToStr(ACodigoUF) + ',' + IntToStr(ACodigoNumerico) + ',' + IntToStr(AModelo) +
        ',' + IntToStr(AModelo) + ',' + IntToStr(ASerie) + ',' + IntToStr(ANumero) + ',' + IntToStr(ATpEmi) +
        ',' + DateToStr(Emissao) + ',' + CNPJCPF + ' )', logCompleto, True)
    else
      GravarLog('NFE_GerarChave', logNormal);


    NFeDM.Travar;

    try
      Resposta := '';
      Resposta := GerarChaveAcesso(ACodigoUF, Emissao, CNPJCPF, ASerie, ANumero, ATpEmi, ACodigoNumerico, AModelo);
      Resposta := ConverterStringSaida(Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.ObterCertificados(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Resposta: Ansistring;
begin
  try
    GravarLog('NFE_ObterCertificados', logNormal);

    NFeDM.Travar;

    try
      Resposta := '';
      Resposta := ObterCerticados(NFeDM.ACBrNFe1.SSL);
      Resposta := ConverterStringSaida(Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.GetPath(ATipo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Resposta: Ansistring;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_GetPath(' + IntToStr(ATipo) + ' )', logCompleto, True)
    else
      GravarLog('NFE_GetPath', logNormal);

    NFeDM.Travar;

    try
      with NFeDM do
      begin
        Resposta := '';

        case ATipo of
          0: Resposta := ACBrNFe1.Configuracoes.Arquivos.GetPathNFe();
          1: Resposta := ACBrNFe1.Configuracoes.Arquivos.GetPathInu();
          2: Resposta := ACBrNFe1.Configuracoes.Arquivos.GetPathEvento(teCCe);
          3: Resposta := ACBrNFe1.Configuracoes.Arquivos.GetPathEvento(teCancelamento);
        end;

        Resposta := ConverterStringSaida(Resposta);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.GetPathEvento(ACodEvento: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  CodEvento: string;
  Resposta: Ansistring;
  ok: boolean;
begin
  try
    CodEvento := ConverterStringEntrada(ACodEvento);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_GetPathEvento(' + CodEvento + ' )', logCompleto, True)
    else
      GravarLog('NFE_GetPathEvento', logNormal);

    NFeDM.Travar;

    try
      with NFeDM do
      begin
        Resposta := '';
        Resposta := ACBrNFe1.Configuracoes.Arquivos.GetPathEvento(StrToTpEventoNFe(ok, CodEvento));
        Resposta := ConverterStringSaida(Resposta);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.StatusServico(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Resp: TStatusServicoResposta;
  Resposta: Ansistring;
begin
  try
    GravarLog('NFE_StatusServico', logNormal);

    NFeDM.Travar;
    Resp := TStatusServicoResposta.Create(Config.TipoResposta, Config.CodResposta);
    try
      with NFeDM.ACBrNFe1 do
      begin
        WebServices.StatusServico.Executar;

        Resp.Processar(NFeDM.ACBrNFe1);
        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      Resp.Free;
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.Consultar(const eChaveOuNFe: PAnsiChar; AExtrairEventos: boolean; const sResposta: PAnsiChar;
                                      var esTamanho: Integer): Integer;
var
  EhArquivo: boolean;
  ChaveOuNFe: string;
  Resp: TConsultaNFeResposta;
  Resposta: Ansistring;
begin
  try
    ChaveOuNFe := ConverterStringSaida(eChaveOuNFe);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_Consultar(' + ChaveOuNFe + ', ' +
        BoolToStr(AExtrairEventos, True) + ' )', logCompleto, True)
    else
      GravarLog('NFE_Consultar', logNormal);

    NFeDM.Travar;

    EhArquivo := StringEhArquivo(ChaveOuNFe);

    if EhArquivo and not ValidarChave(ChaveOuNFe) then
    begin
      VerificarArquivoExiste(ChaveOuNFe);
      NFeDM.ACBrNFe1.NotasFiscais.LoadFromFile(ChaveOuNFe);
    end;

    if NFeDM.ACBrNFe1.NotasFiscais.Count = 0 then
    begin
      if ValidarChave(ChaveOuNFe) then
        NFeDM.ACBrNFe1.WebServices.Consulta.NFeChave := ChaveOuNFe
      else
        raise EACBrLibException.Create(ErrChaveNFe, Format(SErrChaveInvalida, [ChaveOuNFe]));
    end
    else
      NFeDM.ACBrNFe1.WebServices.Consulta.NFeChave := NFeDM.ACBrNFe1.NotasFiscais.Items[NFeDM.ACBrNFe1.NotasFiscais.Count - 1].NumID;

    NFeDM.ACBrNFe1.WebServices.Consulta.ExtrairEventos := AExtrairEventos;
    Resp := TConsultaNFeResposta.Create(Config.TipoResposta, Config.CodResposta);

    try
      with NFeDM.ACBrNFe1 do
      begin
        WebServices.Consulta.Executar;
        Resp.Processar(NFeDM.ACBrNFe1);

        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      Resp.Free;
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.Inutilizar(const ACNPJCPF, AJustificativa: PAnsiChar; Ano, Modelo, Serie, NumeroInicial,
                                       NumeroFinal: integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Resp: TInutilizarNFeResposta;
  Resposta, CNPJCPF, Justificativa: string;
begin
  try
    Justificativa := ConverterStringEntrada(AJustificativa);
    CNPJCPF := ConverterStringEntrada(ACNPJCPF);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_InutilizarNFe(' + CNPJCPF + ',' + Justificativa + ',' + IntToStr(Ano) + ',' + IntToStr(modelo) + ','
             + IntToStr(Serie) + ',' + IntToStr(NumeroInicial) + ',' + IntToStr(NumeroFinal) + ' )', logCompleto, True)
    else
      GravarLog('NFE_InutilizarNFe', logNormal);

    CNPJCPF := OnlyNumber(CNPJCPF);

    if not ValidarCNPJouCPF(CNPJCPF) then
      raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [CNPJCPF]));

    NFeDM.Travar;
    Resp := TInutilizarNFeResposta.Create(Config.TipoResposta, Config.CodResposta);

    try
      with NFeDM.ACBrNFe1 do
      begin
        with WebServices do
        begin
          Inutilizacao.CNPJ := CNPJCPF;
          Inutilizacao.Justificativa := Justificativa;
          Inutilizacao.Modelo := Modelo;
          Inutilizacao.Serie := Serie;
          Inutilizacao.Ano := Ano;
          Inutilizacao.NumeroInicial := NumeroInicial;
          Inutilizacao.NumeroFinal := NumeroFinal;

          Inutilizacao.Executar;
          Resp.Processar(NFeDM.ACBrNFe1);
          Resposta := Resp.Gerar;
          MoverStringParaPChar(Resposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, Resposta);
        end;
      end;
    finally
      Resp.Free;
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.Enviar(ALote: integer; AImprimir, ASincrono, AZipado: boolean; const sResposta: PAnsiChar;
                                   var esTamanho: Integer): Integer;
var
  Resposta: Ansistring;
  RespEnvio: TEnvioResposta;
  RespRetorno: TRetornoResposta;
  ImpResp: TLibImpressaoResposta;
  i, ImpCount: integer;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_Enviar(' + IntToStr(ALote) +
                 BoolToStr(AImprimir, ', Imprimir', '') +
                 BoolToStr(ASincrono, ', Sincrono', '') +
                 BoolToStr(AZipado, ', Zipado', '') + ' )', logCompleto, True)
    else
      GravarLog('NFe_Enviar', logNormal);


    NFeDM.Travar;

    try
      with NFeDM.ACBrNFe1 do
      begin
        if NotasFiscais.Count <= 0 then
          raise EACBrLibException.Create(ErrEnvio, 'ERRO: Nenhuma NF-e adicionada ao Lote');

        if NotasFiscais.Count > 50 then
           raise EACBrLibException.Create(ErrEnvio, 'ERRO: Conjunto de NF-e transmitidas (máximo de 50 NF-e)' +
                                                    ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count));

        GravarLog('NFe_Enviar, Limpando Resp', logParanoico);
        Resposta := '';
        WebServices.Enviar.Clear;
        WebServices.Retorno.Clear;

        GravarLog('NFe_Enviar, Assinando', logCompleto);
        NotasFiscais.Assinar;

        try
          GravarLog('NFe_Enviar, Validando', logCompleto);
          NotasFiscais.Validar;
        except
          on E: EACBrNFeException do
          begin
            Result := SetRetorno(ErrValidacaoNFe, ConverterStringSaida(E.Message) );
            Exit;
          end;
        end;

        if (ALote = 0) then
          WebServices.Enviar.Lote := '1'
        else
          WebServices.Enviar.Lote := IntToStr(ALote);

        GravarLog('NFe_Enviar, Enviando', logCompleto);
        WebServices.Enviar.Sincrono := ASincrono;
        WebServices.Enviar.Zipado := AZipado;
        WebServices.Enviar.Executar;

        RespEnvio := TEnvioResposta.Create(Config.TipoResposta, Config.CodResposta);
        try
          GravarLog('NFe_Enviar, Proces.Resp Enviar', logParanoico);
          RespEnvio.Processar(NFeDM.ACBrNFe1);
          Resposta := RespEnvio.Gerar;
        finally
          RespEnvio.Free;
        end;

        if not ASincrono or ((NaoEstaVazio(WebServices.Enviar.Recibo)) and (WebServices.Enviar.cStat = 103)) then
        begin
          GravarLog('NFe_Enviar, Consultando Retorno', logCompleto);
          WebServices.Retorno.Recibo := WebServices.Enviar.Recibo;
          WebServices.Retorno.Executar;

          RespRetorno := TRetornoResposta.Create('NFe', Config.TipoResposta, Config.CodResposta);
          try
            GravarLog('NFe_Enviar, Proces.Resp Retorno', logParanoico);
            RespRetorno.Processar(WebServices.Retorno.NFeRetorno,
                                  WebServices.Retorno.Recibo,
                                  WebServices.Retorno.Msg,
                                  WebServices.Retorno.Protocolo,
                                  WebServices.Retorno.ChaveNFe);

            if Config.TipoResposta = resJSON then
              Resposta := '[' + Resposta + ',' + RespRetorno.Gerar + ']'
            else
              Resposta := Resposta + sLineBreak + RespRetorno.Gerar;
          finally
            RespRetorno.Free;
          end;
        end;

        if AImprimir then
        begin
          try
            NFeDM.ConfigurarImpressao;

            ImpCount := 0;
            for I := 0 to NotasFiscais.Count - 1 do
            begin
              if NotasFiscais.Items[I].Confirmada then
              begin
                GravarLog('NFe_Enviar, Imprindo NFe['+IntToStr(I+1)+'], '+NotasFiscais.Items[I].NFe.infNFe.ID, logNormal);
                NotasFiscais.Items[I].Imprimir;
                Inc(ImpCount);
              end;
            end;

            if ImpCount > 0 then
            begin
              ImpResp := TLibImpressaoResposta.Create(ImpCount, Config.TipoResposta, Config.CodResposta);
              try
                GravarLog('NFe_Enviar, Proces.Resp Impressao', logParanoico);
                Resposta := Resposta + sLineBreak + ImpResp.Gerar;
              finally
                ImpResp.Free;
              end;
            end;
          finally
            NFeDM.FinalizarImpressao;
          end;
        end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.ConsultarRecibo(ARecibo: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Resp: TReciboResposta;
  sRecibo, Resposta: Ansistring;
begin
  try
    sRecibo := ConverterStringEntrada(ARecibo);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_ConsultarRecibo(' + sRecibo + ' )', logCompleto, True)
    else
      GravarLog('NFE_ConsultarRecibo', logNormal);

    NFeDM.Travar;

    try
      with NFeDM.ACBrNFe1 do
      begin
        WebServices.Recibo.Recibo := sRecibo;
        WebServices.Recibo.Executar;

        Resp := TReciboResposta.Create('NFe', Config.TipoResposta, Config.CodResposta);

        try
          Resp.Processar(WebServices.Recibo.NFeRetorno, WebServices.Recibo.Recibo);
          Resposta := Resp.Gerar;
        finally
          Resp.Free;
        end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.Cancelar(const eChave, eJustificativa, eCNPJCPF: PAnsiChar; ALote: integer;
                                     const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  AChave, AJustificativa, ACNPJCPF: string;
  Resp: TCancelamentoResposta;
  Resposta: Ansistring;
begin
  try
    AChave := ConverterStringEntrada(eChave);
    AJustificativa := ConverterStringEntrada(eJustificativa);
    ACNPJCPF := ConverterStringEntrada(eCNPJCPF);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_Cancelar(' + AChave + ',' + AJustificativa + ',' + ACNPJCPF + ','
                 + IntToStr(ALote) + ' )', logCompleto, True)
    else
      GravarLog('NFe_Cancelar', logNormal);

    NFeDM.Travar;

    try
      if not ValidarChave(AChave) then
        raise EACBrLibException.Create(ErrChaveNFe, Format(SErrChaveInvalida, [AChave]))
      else
        NFeDM.ACBrNFe1.WebServices.Consulta.NFeChave := AChave;

      if not NFeDM.ACBrNFe1.WebServices.Consulta.Executar then
        raise EACBrLibException.Create(ErrConsulta, NFeDM.ACBrNFe1.WebServices.Consulta.Msg);

      NFeDM.ACBrNFe1.EventoNFe.Evento.Clear;
      with NFeDM.ACBrNFe1.EventoNFe.Evento.New do
      begin
        Infevento.CNPJ := ACNPJCPF;
        if Trim(Infevento.CNPJ) = '' then
          Infevento.CNPJ := copy(OnlyNumber(NFeDM.ACBrNFe1.WebServices.Consulta.NFeChave), 7, 14)
        else
        begin
          if not ValidarCNPJouCPF(ACNPJCPF) then
            raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));
        end;

        Infevento.nSeqEvento := 1;
        InfEvento.tpAmb := NFeDM.ACBrNFe1.Configuracoes.WebServices.Ambiente;
        Infevento.cOrgao := StrToIntDef(copy(OnlyNumber(NFeDM.ACBrNFe1.WebServices.Consulta.NFeChave), 1, 2), 0);
        Infevento.dhEvento := now;
        Infevento.tpEvento := teCancelamento;
        Infevento.chNFe := NFeDM.ACBrNFe1.WebServices.Consulta.NFeChave;
        Infevento.detEvento.nProt := NFeDM.ACBrNFe1.WebServices.Consulta.Protocolo;
        Infevento.detEvento.xJust := AJustificativa;
      end;

      if (ALote = 0) then
        ALote := 1;

      NFeDM.ACBrNFe1.WebServices.EnvEvento.idLote := ALote;
      NFeDM.ACBrNFe1.WebServices.EnvEvento.Executar;

      Resp := TCancelamentoResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFeDM.ACBrNFe1);
        Resposta := Resp.Gerar;
      finally
        Resp.Free;
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.EnviarEvento(idLote: integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  i, j: integer;
  Resp: TEventoResposta;
  Resposta, chNfe: string;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_EnviarEvento(' + IntToStr(idLote) + ' )', logCompleto, True)
    else
      GravarLog('NFe_EnviarEvento', logNormal);

    NFeDM.Travar;

    try
      with NFeDM.ACBrNFe1 do
      begin
        if EventoNFe.Evento.Count = 0 then
          raise EACBrLibException.Create(ErrEnvioEvento, 'ERRO: Nenhum Evento adicionado ao Lote');

        if EventoNFe.Evento.Count > 20 then
          raise EACBrLibException.Create(ErrEnvioEvento,  'ERRO: Conjunto de Eventos transmitidos (máximo de 20) ' +
                                                          'excedido. Quantidade atual: ' + IntToStr(EventoNFe.Evento.Count));

        {Atribuir nSeqEvento, CNPJ, Chave e/ou Protocolo quando não especificar}
        for i := 0 to EventoNFe.Evento.Count - 1 do
        begin
          if EventoNFe.Evento.Items[i].InfEvento.nSeqEvento = 0 then
            EventoNFe.Evento.Items[i].infEvento.nSeqEvento := 1;

          EventoNFe.Evento.Items[i].InfEvento.tpAmb := Configuracoes.WebServices.Ambiente;

          if NotasFiscais.Count > 0 then
          begin
            chNfe := OnlyNumber(EventoNFe.Evento.Items[i].InfEvento.chNfe);

            // Se tem a chave da NFe no Evento, procure por ela nas notas carregadas //
            if NaoEstaVazio(chNfe) then
            begin
              for j := 0 to NotasFiscais.Count - 1 do
              begin
                if chNfe = NotasFiscais.Items[j].NumID then
                  Break;
              end;

              if j = NotasFiscais.Count then
                raise EACBrLibException.Create(ErrEnvioEvento, 'Não existe NFe com a chave [' + chNfe + '] carregada');
            end
            else
              j := 0;

            if trim(EventoNFe.Evento.Items[i].InfEvento.CNPJ) = '' then
              EventoNFe.Evento.Items[i].InfEvento.CNPJ := NotasFiscais.Items[j].NFe.Emit.CNPJCPF;

            if chNfe = '' then
              EventoNFe.Evento.Items[i].InfEvento.chNfe := NotasFiscais.Items[j].NumID;

            if trim(EventoNFe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
            begin
              if EventoNFe.Evento.Items[i].infEvento.tpEvento = teCancelamento then
              begin
                EventoNFe.Evento.Items[i].infEvento.detEvento.nProt := NotasFiscais.Items[j].NFe.procNFe.nProt;

                if trim(EventoNFe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
                begin
                  WebServices.Consulta.NFeChave := EventoNFe.Evento.Items[i].InfEvento.chNfe;

                  if not WebServices.Consulta.Executar then
                    raise EACBrLibException.Create(ErrEnvioEvento, WebServices.Consulta.Msg);

                  EventoNFe.Evento.Items[i].infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
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
        Resp.Processar(NFeDM.ACBrNFe1);
        Resposta := Resp.Gerar;
      finally
        Resp.Free;
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.ConsultaCadastro(cUF, nDocumento: PAnsiChar; nIE: boolean;
                                             const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  Resp: TConsultaCadastroResposta;
  AUF, ADocumento: string;
  Resposta: Ansistring;
begin
  try
    AUF := ConverterStringEntrada(cUF);
    ADocumento := ConverterStringEntrada(nDocumento);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFE_ConsultaCadastro(' + AUF + ',' + ADocumento + ',' + BoolToStr(nIE, True) + ' )', logCompleto, True)
    else
      GravarLog('NFE_ConsultaCadastro', logNormal);

    NFeDM.Travar;
    try
      NFeDM.ACBrNFe1.WebServices.ConsultaCadastro.UF := AUF;
      if nIE then
        NFeDM.ACBrNFe1.WebServices.ConsultaCadastro.IE := ADocumento
      else
      begin
        if Length(ADocumento) > 11 then
          NFeDM.ACBrNFe1.WebServices.ConsultaCadastro.CNPJ := ADocumento
        else
          NFeDM.ACBrNFe1.WebServices.ConsultaCadastro.CPF := ADocumento;
      end;

      NFeDM.ACBrNFe1.WebServices.ConsultaCadastro.Executar;
      Resp := TConsultaCadastroResposta.Create(Config.TipoResposta, Config.CodResposta);

      try
        Resp.Processar(NFeDM.ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad);
        Resposta := Resp.Gerar;
      finally
        Resp.Free;
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.DistribuicaoDFePorUltNSU(const AcUFAutor: integer;  eCNPJCPF, eultNSU: PAnsiChar;
                                                     const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
begin
  result := DistribuicaoDFe(AcUFAutor, eCNPJCPF, eultNSU, '',
                             sResposta, esTamanho);

end;

function TACBrLibNFe.DistribuicaoDFe(const AcUFAutor: integer;  eCNPJCPF, eultNSU, eArquivoOuXML: PAnsiChar;
                                            const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
  AultNSU, ACNPJCPF: string;
  Resposta: Ansistring;
  Resp: TDistribuicaoDFeResposta;
begin
  try
    ACNPJCPF := ConverterStringEntrada(eCNPJCPF);
    AultNSU := ConverterStringEntrada(eultNSU);
    ArquivoOuXml := ConverterStringEntrada(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_DistribuicaoDFePorUltNSU(' + IntToStr(AcUFAutor) + ',' + ACNPJCPF + ',' + AultNSU + ')', logCompleto, True)
    else
      GravarLog('NFe_DistribuicaoDFePorUltNSU', logNormal);

    if ArquivoOuXml <> '' then
    begin
      EhArquivo := StringEhArquivo(ArquivoOuXml);
      if EhArquivo then
        VerificarArquivoExiste(ArquivoOuXml);
    end;

    NFeDM.Travar;

    try
      if ArquivoOuXml = '' then
        if not ValidarCNPJouCPF(ACNPJCPF) then
          raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      with NFeDM do
      begin
        try
        GravarLog('NFe_DistribuicaoDFePorUltNSU, Executar', logCompleto);

        // Lê o arquivo selecionado
        if ArquivoOuXml <> '' then
        begin
          try
            ACBrNFe1.WebServices.DistribuicaoDFe.ListaArqs.Clear;
            ACBrNFe1.WebServices.DistribuicaoDFe.Clear;
            ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Clear;

            if EhArquivo then
              ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.Leitor.CarregarArquivo(ArquivoOuXml)
            else
              ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.Leitor.Arquivo := ArquivoOuXml;

            ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.LerXml;

            // Preenche a lista de arquivos extraídos da distribuição, pois a leitura não gera os arquivos individuais
            while ACBrNFe1.WebServices.DistribuicaoDFe.ListaArqs.Count <
                  ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count do
              ACBrNFe1.WebServices.DistribuicaoDFe.ListaArqs.Add('');

            AultNSU := ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.ultNSU;
          except
            on E:Exception do
              raise EACBrLibException.Create(ErrCNPJ, E.Message);
          end;
        end
        // Consulta o WebService
        else
        begin
          ACBrNFe1.WebServices.DistribuicaoDFe.cUFAutor := AcUFAutor;
          ACBrNFe1.WebServices.DistribuicaoDFe.CNPJCPF := ACNPJCPF;
          ACBrNFe1.WebServices.DistribuicaoDFe.ultNSU := AultNSU;
          ACBrNFe1.WebServices.DistribuicaoDFe.NSU := '';
          ACBrNFe1.WebServices.DistribuicaoDFe.chNFe := '';
          ACBrNFe1.WebServices.DistribuicaoDFe.Executar;
        end;

        GravarLog('NFe_DistribuicaoDFePorUltNSU, Proces.Resp DistribuicaoDFe', logParanoico);
        Resp := TDistribuicaoDFeResposta.Create(Config.TipoResposta, Config.CodResposta);
        try
          Resp.Processar(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt,
                         ACBrNFe1.WebServices.DistribuicaoDFe.Msg,
                         ACBrNFe1.WebServices.DistribuicaoDFe.NomeArq,
                         ACBrNFe1.WebServices.DistribuicaoDFe.ListaArqs);
          Resposta := Resp.Gerar;
        finally
          Resp.Free;
        end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
        except
          on E: Exception do
          begin
            raise EACBrLibException.Create(ErrRetorno, E.Message + SLineBreak  + '  MotivoRetornadoDoWebService: "' + Trim(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.xMotivo) + '"' );
          end;
        end;
      end;
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.DistribuicaoDFePorNSU(const AcUFAutor: integer; eCNPJCPF, eNSU: PAnsiChar;
                                                  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  ANSU, ACNPJCPF: string;
  Resposta: Ansistring;
  Resp: TDistribuicaoDFeResposta;
begin
  try
    ACNPJCPF := ConverterStringEntrada(eCNPJCPF);
    ANSU := ConverterStringEntrada(eNSU);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_DistribuicaoDFePorNSU(' + IntToStr(AcUFAutor) +
        ',' + ACNPJCPF + ',' + ANSU + ')', logCompleto, True)
    else
      GravarLog('NFe_DistribuicaoDFePorNSU', logNormal);

    NFeDM.Travar;

    try
      if not ValidarCNPJouCPF(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      with NFeDM do
      begin
        ACBrNFe1.WebServices.DistribuicaoDFe.cUFAutor := AcUFAutor;
        ACBrNFe1.WebServices.DistribuicaoDFe.CNPJCPF := ACNPJCPF;
        ACBrNFe1.WebServices.DistribuicaoDFe.ultNSU := '';
        ACBrNFe1.WebServices.DistribuicaoDFe.NSU := ANSU;
        ACBrNFe1.WebServices.DistribuicaoDFe.chNFe := '';

        ACBrNFe1.WebServices.DistribuicaoDFe.Executar;

        Resp := TDistribuicaoDFeResposta.Create(Config.TipoResposta, Config.CodResposta);

        try
          Resp.Processar(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt,
                         ACBrNFe1.WebServices.DistribuicaoDFe.Msg,
                         ACBrNFe1.WebServices.DistribuicaoDFe.NomeArq,
                         ACBrNFe1.WebServices.DistribuicaoDFe.ListaArqs);
          Resposta := Resp.Gerar;
        finally
          Resp.Free;
        end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.DistribuicaoDFePorChave(const AcUFAutor: integer; eCNPJCPF, echNFe: PAnsiChar;
                                                    const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  AchNFe, ACNPJCPF: string;
  Resposta: Ansistring;
  Resp: TDistribuicaoDFeResposta;
begin
  try
    ACNPJCPF := ConverterStringEntrada(eCNPJCPF);
    AchNFe := ConverterStringEntrada(echNFe);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_DistribuicaoDFePorChave(' + IntToStr(AcUFAutor) + ',' + ACNPJCPF + ',' + AchNFe + ')', logCompleto, True)
    else
      GravarLog('NFe_DistribuicaoDFePorChave', logNormal);

    NFeDM.Travar;

    try
      if not ValidarCNPJouCPF(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      if not ValidarChave(AchNFe) then
        raise EACBrLibException.Create(ErrChaveNFe, Format(SErrChaveInvalida, [AchNFe]));

      with NFeDM do
      begin
        ACBrNFe1.WebServices.DistribuicaoDFe.cUFAutor := AcUFAutor;
        ACBrNFe1.WebServices.DistribuicaoDFe.CNPJCPF := ACNPJCPF;
        ACBrNFe1.WebServices.DistribuicaoDFe.ultNSU := '';
        ACBrNFe1.WebServices.DistribuicaoDFe.NSU := '';
        ACBrNFe1.WebServices.DistribuicaoDFe.chNFe := AchNFe;

        ACBrNFe1.WebServices.DistribuicaoDFe.Executar;

        Resp := TDistribuicaoDFeResposta.Create(Config.TipoResposta, Config.CodResposta);

        try
          Resp.Processar(ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt,
                         ACBrNFe1.WebServices.DistribuicaoDFe.Msg,
                         ACBrNFe1.WebServices.DistribuicaoDFe.NomeArq,
                         ACBrNFe1.WebServices.DistribuicaoDFe.ListaArqs);
          Resposta := Resp.Gerar;
        finally
          Resp.Free;
        end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.EnviarEmail(const ePara, eXmlNFe: PAnsiChar; const AEnviaPDF: boolean;
                                        const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
var
  Resposta, APara, AXmlNFe, AAssunto, ACC, AAnexos, AMensagem: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
  Resp: TLibNFeResposta;
begin
  try
    APara := ConverterStringEntrada(ePara);
    AXmlNFe := ConverterStringEntrada(eXmlNFe);
    AAssunto := ConverterStringEntrada(eAssunto);
    ACC := ConverterStringEntrada(eCC);
    AAnexos := ConverterStringEntrada(eAnexos);
    AMensagem := ConverterStringEntrada(eMensagem);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_EnviarEmail(' + APara + ',' + AXmlNFe + ',' + BoolToStr(AEnviaPDF, 'PDF', '') + ',' + AAssunto
                 + ',' + ACC + ',' + AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      GravarLog('NFe_EnviarEmail', logNormal);

    NFeDM.Travar;

    try
      with NFeDM do
      begin
        EhArquivo := StringEhArquivo(AXmlNFe);

        if EhArquivo then
          VerificarArquivoExiste(AXmlNFe);

        if EhArquivo then
          ACBrNFe1.NotasFiscais.LoadFromFile(AXmlNFe)
        else
          ACBrNFe1.NotasFiscais.LoadFromString(AXmlNFe);

        if ACBrNFe1.NotasFiscais.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfNFeCarregadas, [ACBrNFe1.NotasFiscais.Count]))
        else
        begin
          slMensagemEmail := TStringList.Create;
          slCC := TStringList.Create;
          slAnexos := TStringList.Create;

          Resp := TLibNFeResposta.Create('EnviaEmail', Config.TipoResposta, Config.CodResposta);

          try
            with ACBrNFe1 do
            begin
              slMensagemEmail.DelimitedText := sLineBreak;
              slMensagemEmail.Text := StringReplace(AMensagem, ';', sLineBreak, [rfReplaceAll]);

              slCC.DelimitedText := sLineBreak;
              slCC.Text := StringReplace(ACC, ';', sLineBreak, [rfReplaceAll]);

              slAnexos.DelimitedText := sLineBreak;
              slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

              if (AEnviaPDF) then
                NFeDM.ConfigurarImpressao('', True);

              NotasFiscais.Items[0].EnviarEmail(
                  APara,
                  AAssunto,
                  slMensagemEmail,
                  AEnviaPDF, // Enviar PDF junto
                  slCC,      // Lista com emails que serão enviado cópias - TStrings
                  slAnexos); // Lista de slAnexos - TStrings

              Resp.Msg := 'Email enviado com sucesso';
              Resposta := Resp.Gerar;

              Result := SetRetorno(ErrOK, Resposta);
            end;
          finally
            Resp.Free;
            slCC.Free;
            slAnexos.Free;
            slMensagemEmail.Free;
            if (AEnviaPDF) then NFeDM.FinalizarImpressao;
          end;
        end;
      end;
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.EnviarEmailEvento(const ePara, eXmlEvento, eXmlNFe: PAnsiChar; const AEnviaPDF: boolean;
                                              const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
var
  APara, AXmlEvento, AXmlNFe, AAssunto, ACC, AAnexos, AMensagem, ArqPDF: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
  Resposta: TLibNFeResposta;
begin
  try
    APara := ConverterStringEntrada(ePara);
    AXmlEvento := ConverterStringEntrada(eXmlEvento);
    AXmlNFe := ConverterStringEntrada(eXmlNFe);
    AAssunto := ConverterStringEntrada(eAssunto);
    ACC := ConverterStringEntrada(eCC);
    AAnexos := ConverterStringEntrada(eAnexos);
    AMensagem := ConverterStringEntrada(eMensagem);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_EnviarEmailEvento(' + APara + ',' + AXmlEvento + ',' + AXmlNFe + ',' +
                 BoolToStr(AEnviaPDF, 'PDF', '') + ',' + AAssunto + ',' + ACC + ',' + AAnexos + ',' + AMensagem +
                 ' )', logCompleto, True)
    else
      GravarLog('NFe_EnviarEmailEvento', logNormal);

    NFeDM.Travar;

    try
      with NFeDM.ACBrNFe1 do
      begin
        EventoNFe.Evento.Clear;
        NotasFiscais.Clear;

        EhArquivo := StringEhArquivo(AXmlEvento);

        if EhArquivo then
          VerificarArquivoExiste(AXmlEvento);

        if EhArquivo then
          EventoNFe.LerXML(AXmlEvento)
        else
         EventoNFe.LerXMLFromString(AXmlEvento);

        EhArquivo := StringEhArquivo(AXmlNFe);

        if EhArquivo then
          VerificarArquivoExiste(AXmlNFe);

        if EhArquivo then
          NotasFiscais.LoadFromFile(AXmlNFe)
        else
          NotasFiscais.LoadFromString(AXmlNFe);

        if EventoNFe.Evento.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfEventosCarregados, [EventoNFe.Evento.Count]))
        else
        begin
          slMensagemEmail := TStringList.Create;
          slCC := TStringList.Create;
          slAnexos := TStringList.Create;
          Resposta := TLibNFeResposta.Create('EnviaEmail', Config.TipoResposta, Config.CodResposta);

          try
            if AEnviaPDF then
            begin
              try
                NFeDM.ConfigurarImpressao('', True);
                ImprimirEventoPDF;

                ArqPDF := OnlyNumber(EventoNFe.Evento[0].Infevento.id);
                ArqPDF := PathWithDelim(DANFe.PathPDF) + ArqPDF + '-procEventoNFe.pdf';
              except
                raise EACBrLibException.Create(ErrRetorno, 'Erro ao criar o arquivo PDF');
              end;
            end;

            with Mail do
            begin
              slMensagemEmail.DelimitedText := sLineBreak;
              slMensagemEmail.Text := StringReplace(AMensagem, ';', sLineBreak, [rfReplaceAll]);

              slCC.DelimitedText := sLineBreak;
              slCC.Text := StringReplace(ACC, ';', sLineBreak, [rfReplaceAll]);

              slAnexos.DelimitedText := sLineBreak;
              slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

              slAnexos.Add(AXmlEvento);

              if AEnviaPDF then
                slAnexos.Add(ArqPDF);

              try
                NFeDM.ACBrNFe1.EnviarEmail(
                    APara,
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
            if (AEnviaPDF) then NFeDM.FinalizarImpressao;
          end;
        end;
      end;
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.Imprimir(const cImpressora: PAnsiChar; nNumCopias: integer; const cProtocolo, bMostrarPreview,
                                     cMarcaDagua, bViaConsumidor, bSimplificado: PAnsiChar): Integer;
var
  Resposta: TLibImpressaoResposta;
  Impressora, Protocolo, MostrarPreview, MarcaDagua, ViaConsumidor,
  Simplificado: string;
begin
  try
    Impressora := ConverterStringEntrada(cImpressora);
    Protocolo := ConverterStringEntrada(cProtocolo);
    MostrarPreview := ConverterStringEntrada(bMostrarPreview);
    MarcaDagua := ConverterStringEntrada(cMarcaDagua);
    ViaConsumidor := ConverterStringEntrada(bViaConsumidor);
    Simplificado := ConverterStringEntrada(bSimplificado);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_Imprimir(' + Impressora + ',' + IntToStr(nNumCopias) + ',' + Protocolo + ',' + MostrarPreview +
                 ',' + MarcaDagua + ',' + ViaConsumidor + ',' + Simplificado + ')', logCompleto, True)
    else
      GravarLog('NFe_Imprimir', logNormal);


    NFeDM.Travar;
    Resposta := TLibImpressaoResposta.Create(NFeDM.ACBrNFe1.NotasFiscais.Count, Config.TipoResposta, Config.CodResposta);

    try
      NFeDM.ConfigurarImpressao(Impressora, False, Protocolo, MostrarPreview, MarcaDagua, ViaConsumidor, Simplificado);
      if nNumCopias > 0 then
        NFeDM.ACBrNFe1.DANFE.NumCopias := nNumCopias;

      NFeDM.ACBrNFe1.NotasFiscais.Imprimir;
      Result := SetRetorno(ErrOK, Resposta.Gerar);
    finally
      NFeDM.FinalizarImpressao;
      Resposta.Free;
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.ImprimirPDF: Integer;
var
  Resposta: TLibImpressaoResposta;
begin
  try
    GravarLog('NFe_ImprimirPDF', logNormal);
    NFeDM.Travar;
    try
      Resposta := TLibImpressaoResposta.Create(NFeDM.ACBrNFe1.NotasFiscais.Count, Config.TipoResposta, Config.CodResposta);
      try
        NFeDM.ConfigurarImpressao('', True);
        try
          NFeDM.ACBrNFe1.NotasFiscais.ImprimirPDF;
          Resposta.Msg := NFeDM.ACBrNFe1.DANFE.ArquivoPDF;
          Result := SetRetorno(ErrOK, Resposta.Gerar);
        finally
          NFeDM.FinalizarImpressao;
        end;
      finally
        Resposta.Free;
      end;
    finally
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.SalvarPDF(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
Var
  AStream: TMemoryStream;
  Resposta: Ansistring;
begin
  try
    GravarLog('NFe_SalvarPDF', logNormal);

    NFeDM.Travar;

    AStream := TMemoryStream.Create;

    try
      NFeDM.ConfigurarImpressao('', True);

      NFeDM.ACBrNFe1.NotasFiscais.ImprimirPDF(AStream);
      Resposta := StreamToBase64(AStream);

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFeDM.FinalizarImpressao;
      AStream.Free;
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.ImprimirEvento(const eArquivoXmlNFe, eArquivoXmlEvento: PAnsiChar): Integer;
var
  EhArquivo: boolean;
  AArquivoXmlNFe: string;
  AArquivoXmlEvento: string;
  Resposta: TLibImpressaoResposta;
begin
  try
    AArquivoXmlNFe := ConverterStringEntrada(eArquivoXmlNFe);
    AArquivoXmlEvento := ConverterStringEntrada(eArquivoXmlEvento);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_ImprimirEvento(' + AArquivoXmlNFe + ',' + AArquivoXmlEvento + ' )', logCompleto, True)
    else
      GravarLog('NFe_ImprimirEvento', logNormal);

    NFeDM.Travar;
    Resposta := TLibImpressaoResposta.Create(NFeDM.ACBrNFe1.EventoNFe.Evento.Count, Config.TipoResposta, Config.CodResposta);

    try
      EhArquivo := StringEhArquivo(AArquivoXmlNFe);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlNFe);

      if EhArquivo then
        NFeDM.ACBrNFe1.NotasFiscais.LoadFromFile(AArquivoXmlNFe)
      else
        NFeDM.ACBrNFe1.NotasFiscais.LoadFromString(AArquivoXmlNFe);

      EhArquivo := StringEhArquivo(AArquivoXmlEvento);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlEvento);

      if EhArquivo then
        NFeDM.ACBrNFe1.EventoNFe.LerXML(AArquivoXmlEvento)
      else
        NFeDM.ACBrNFe1.EventoNFe.LerXMLFromString(AArquivoXmlEvento);

      NFeDM.ConfigurarImpressao;
      NFeDM.ACBrNFe1.ImprimirEvento;

      Result := SetRetorno(ErrOK, Resposta.Gerar);
    finally
      NFeDM.FinalizarImpressao;
      Resposta.Free;
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.ImprimirEventoPDF(const eArquivoXmlNFe, eArquivoXmlEvento: PAnsiChar): Integer;
var
  EhArquivo: boolean;
  AArquivoXmlNFe: string;
  AArquivoXmlEvento: string;
  Resposta: TLibImpressaoResposta;
begin
  try
    AArquivoXmlNFe := ConverterStringEntrada(eArquivoXmlNFe);
    AArquivoXmlEvento := ConverterStringEntrada(eArquivoXmlEvento);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_ImprimirEventoPDF(' + AArquivoXmlNFe + ',' + AArquivoXmlEvento + ' )', logCompleto, True)
    else
      GravarLog('NFe_ImprimirEventoPDF', logNormal);

    NFeDM.Travar;
    Resposta := TLibImpressaoResposta.Create(NFeDM.ACBrNFe1.EventoNFe.Evento.Count, Config.TipoResposta, Config.CodResposta);

    try
      EhArquivo := StringEhArquivo(AArquivoXmlNFe);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlNFe);

      if EhArquivo then
        NFeDM.ACBrNFe1.NotasFiscais.LoadFromFile(AArquivoXmlNFe)
      else
        NFeDM.ACBrNFe1.NotasFiscais.LoadFromString(AArquivoXmlNFe);

      EhArquivo := StringEhArquivo(AArquivoXmlEvento);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlEvento);

      if EhArquivo then
        NFeDM.ACBrNFe1.EventoNFe.LerXML(AArquivoXmlEvento)
      else
        NFeDM.ACBrNFe1.EventoNFe.LerXMLFromString(AArquivoXmlEvento);

      NFeDM.ConfigurarImpressao('', True);
      NFeDM.ACBrNFe1.ImprimirEventoPDF;

      Resposta.Msg := NFeDM.ACBrNFe1.DANFE.ArquivoPDF;
      Result := SetRetorno(ErrOK, Resposta.Gerar);
    finally
      NFeDM.FinalizarImpressao;
      Resposta.Free;
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.SalvarEventoPDF(const eArquivoXmlNFe, eArquivoXmlEvento, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  EhArquivo: boolean;
  AArquivoXmlNFe: string;
  AArquivoXmlEvento: string;
  AStream: TMemoryStream;
  Resposta: Ansistring;
begin
  try
    AArquivoXmlNFe := ConverterStringEntrada(eArquivoXmlNFe);
    AArquivoXmlEvento := ConverterStringEntrada(eArquivoXmlEvento);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_SalvarEventoPDF(' + AArquivoXmlNFe + ',' + AArquivoXmlEvento + ' )', logCompleto, True)
    else
      GravarLog('NFe_SalvarEventoPDF', logNormal);

    NFeDM.Travar;
    AStream := TMemoryStream.Create;

    try
      EhArquivo := StringEhArquivo(AArquivoXmlNFe);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlNFe);

      if EhArquivo then
        NFeDM.ACBrNFe1.NotasFiscais.LoadFromFile(AArquivoXmlNFe)
      else
        NFeDM.ACBrNFe1.NotasFiscais.LoadFromString(AArquivoXmlNFe);

      EhArquivo := StringEhArquivo(AArquivoXmlEvento);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlEvento);

      if EhArquivo then
        NFeDM.ACBrNFe1.EventoNFe.LerXML(AArquivoXmlEvento)
      else
        NFeDM.ACBrNFe1.EventoNFe.LerXMLFromString(AArquivoXmlEvento);

      NFeDM.ConfigurarImpressao('', True);
      NFeDM.ACBrNFe1.DANFE.ImprimirEventoPDF(AStream);

      Resposta := StreamToBase64(AStream);

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFeDM.FinalizarImpressao;
      AStream.Free;
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.ImprimirInutilizacao(const eArquivoXml: PAnsiChar): Integer;
var
  EhArquivo: boolean;
  AArquivoXml: string;
  Resposta: TLibImpressaoResposta;
begin
  try
    AArquivoXml := ConverterStringEntrada(eArquivoXml);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_ImprimirInutilizacao(' + AArquivoXml + ' )', logCompleto, True)
    else
      GravarLog('NFe_ImprimirInutilizacao', logNormal);

    EhArquivo := StringEhArquivo(AArquivoXml);

    if EhArquivo then
      VerificarArquivoExiste(AArquivoXml);

    NFeDM.Travar;
    Resposta := TLibImpressaoResposta.Create(1, Config.TipoResposta, Config.CodResposta);

    try
      if EhArquivo then
        NFeDM.ACBrNFe1.InutNFe.LerXML(AArquivoXml)
      else
        NFeDM.ACBrNFe1.InutNFe.LerXMLFromString(AArquivoXml);

      NFeDM.ConfigurarImpressao;
      NFeDM.ACBrNFe1.ImprimirInutilizacao;

      Result := SetRetorno(ErrOK, Resposta.Gerar);
    finally
      NFeDM.FinalizarImpressao;
      Resposta.Free;
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.ImprimirInutilizacaoPDF(const eArquivoXml: PAnsiChar): Integer;
var
  EhArquivo: boolean;
  AArquivoXml: string;
  Resposta: TLibImpressaoResposta;
begin
  try
    AArquivoXml := ConverterStringEntrada(eArquivoXml);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_ImprimirInutilizacaoPDF(' + AArquivoXml + ' )', logCompleto, True)
    else
      GravarLog('NFe_ImprimirInutilizacaoPDF', logNormal);

    EhArquivo := StringEhArquivo(AArquivoXml);

    if EhArquivo then
      VerificarArquivoExiste(AArquivoXml);

    NFeDM.Travar;
    Resposta := TLibImpressaoResposta.Create(1, Config.TipoResposta, Config.CodResposta);
    try
      if EhArquivo then
        NFeDM.ACBrNFe1.InutNFe.LerXML(AArquivoXml)
      else
        NFeDM.ACBrNFe1.InutNFe.LerXMLFromString(AArquivoXml);

      NFeDM.ConfigurarImpressao('', True);
      NFeDM.ACBrNFe1.ImprimirInutilizacaoPDF;

      Result := SetRetorno(ErrOK, Resposta.Gerar);
    finally
      NFeDM.FinalizarImpressao;
      Resposta.Free;
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFe.SalvarInutilizacaoPDF(const eArquivoXml, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  EhArquivo: boolean;
  AArquivoXml: string;
  AStream: TMemoryStream;
  Resposta: Ansistring;
begin
  try
    AArquivoXml := ConverterStringEntrada(eArquivoXml);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFe_SalvarInutilizacaoPDF(' + AArquivoXml + ' )', logCompleto, True)
    else
      GravarLog('NFe_SalvarInutilizacaoPDF', logNormal);

    EhArquivo := StringEhArquivo(AArquivoXml);

    if EhArquivo then
      VerificarArquivoExiste(AArquivoXml);

    NFeDM.Travar;
    AStream := TMemoryStream.Create;

    try
      if EhArquivo then
        NFeDM.ACBrNFe1.InutNFe.LerXML(AArquivoXml)
      else
        NFeDM.ACBrNFe1.InutNFe.LerXMLFromString(AArquivoXml);

      NFeDM.ConfigurarImpressao('', True);
      NFeDM.ACBrNFe1.DANFE.ImprimirINUTILIZACAOPDF(AStream);

      Resposta := StreamToBase64(AStream);

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFeDM.FinalizarImpressao;
      AStream.Free;
      NFeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

end.
