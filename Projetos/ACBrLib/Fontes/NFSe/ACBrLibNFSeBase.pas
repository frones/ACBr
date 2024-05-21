{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrLibNFSeBase;

interface

uses
  Classes, SysUtils,
  ACBrLibComum, ACBrLibNFSeDataModule;

type

  { TACBrLibNFSe }

  TACBrLibNFSe = class(TACBrLib)
  private
    FNFSeDM: TLibNFSeDM;

    function SetRetornoNFSeRPSCarregadas(const Count: integer): integer;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property NFSeDM: TLibNFSeDM read FNFSeDM;

    function CarregarXML(const eArquivoOuXML: PChar): longint;
    function CarregarLoteXML(const eArquivoOuXML: PChar): longint;
    function CarregarINI(const eArquivoOuINI: PChar): longint;
    function ObterXml(aIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function GravarXml(aIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
    function ObterIni(AIndex: longint; const sResposta: PChar; var esTamanho: longint):longint;
    function GravarIni(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar):longint;
    function LimparLista: longint;
    function ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
    function Emitir(const aLote:PChar; aModoEnvio: longint;  aImprimir: Boolean; const sResposta: PChar; var esTamanho: longint): longint;
    function Cancelar(aInfCancelamentoNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function SubstituirNFSe(const aNumeroNFSe, aSerieNFSe, aCodigoCancelamento, aMotivoCancelamento, aNumeroLote, aCodigoVerificacao, sResposta: PChar; var esTamanho: longint): longint;
    function LinkNFSe(aNumeroNFSe: PChar; const aCodigoVerificacao, aChaveAcesso, aValorServico, sResposta: PChar; var esTamanho: longint): longint;
    function GerarLote(const aLote: PChar; aQtdMaximaRps, aModoEnvio: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function GerarToken(const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarSituacao(const aProtocolo, aNumLote, sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarLoteRps(const aProtocolo, aNumLote, sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarNFSePorRps(const aNumeroRps, aSerie, aTipo, aCodigoVerificacao, sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarNFSePorNumero(const aNumero:PChar; aPagina: longint; const sResposta: PChar; var esTamanho: longint):longint;
    function ConsultarNFSePorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: longint; aNumeroLote: PChar; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarNFSePorFaixa(const aNumeroInicial, aNumeroFinal: PChar; aPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarNFSeGenerico(aInfConsultaNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarLinkNFSe(aInfConsultaLinkNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function EnviarEmail(const ePara, eXmlNFSe: PChar; const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar):longint;
    function Imprimir(const cImpressora: PChar; nNumCopias: integer; const bGerarPDF, bMostrarPreview, cCancelada: PChar): longint;
    function ImprimirPDF: longint;
    function SalvarPDF (const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarNFSeServicoPrestadoPorNumero(const aNumero: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: longint; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarNFSeServicoPrestadoPorTomador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarNFSeServicoPrestadoPorIntermediario(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarNFSeServicoTomadoPorNumero(const aNumero: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarNFSeServicoTomadoPorPrestador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarNFSeServicoTomadoPorTomador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: longint; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarNFSeServicoTomadoPorIntermediario(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function EnviarEvento(aInfEvento: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarDPSPorChave(const aChaveDPS: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarNFSePorChave(const aChaveNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarEvento(const aChave: PChar; aTipoEvento: longint; aNumSeq: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarDFe(aNSU: longint; sResposta: PChar; var esTamanho: longint): longint;
    function ObterDANFSE(const aChaveNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarParametros(aTipoParametroMunicipio: longint; const aCodigoServico: PChar; aCompetencia: TDateTime; aNumeroBeneficio: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function ObterInformacoesProvedor(const sResposta: PChar; var esTamanho: longint): longint;

  end;

implementation

uses
  StrUtils, ACBrLibResposta, ACBrLibHelpers,
  ACBrLibConsts, ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO, ACBrLibCertUtils,
  ACBrLibNFSeConsts, ACBrLibConfig, ACBrLibNFSeConfig, ACBrNFSeXConversao,
  ACBrLibNFSeRespostas, ACBrNFSeXWebserviceBase;

{ TACBrLibNFSe }

constructor TACBrLibNFSe.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  FNFSeDM := TLibNFSeDM.Create(nil);
  FNFSeDM.Lib := Self;
end;

destructor TACBrLibNFSe.Destroy;
begin
  FNFSeDM.Free;

  inherited Destroy;
end;

procedure TACBrLibNFSe.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibNFSe.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibNFSe.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibNFSeConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibNFSe.Executar;
begin
  inherited Executar;
  FNFSeDM.AplicarConfiguracoes;
end;

function TACBrLibNFSe.SetRetornoNFSeRPSCarregadas(const Count: integer): integer;
begin
  Result := SetRetorno(0, Format(SInfNFSeCarregadas, [Count]));
end;

function TACBrLibNFSe.CarregarXML(const eArquivoOuXML: PChar): longint;
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    ArquivoOuXml := ConverterAnsiParaUTF8(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_CarregarXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      GravarLog('NFSE_CarregarXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    NFSeDM.Travar;
    try
      if EhArquivo then
        NFSeDM.ACBrNFSeX1.NotasFiscais.LoadFromFile(ArquivoOuXml)
      else
        NFSeDM.ACBrNFSeX1.NotasFiscais.LoadFromString(ArquivoOuXml);

      Result := SetRetornoNFSeRPSCarregadas(NFSeDM.ACBrNFSeX1.NotasFiscais.Count);
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.CarregarLoteXML(const eArquivoOuXML: PChar): longint;
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    ArquivoOuXml := ConverterAnsiParaUTF8(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_CarregarLoteXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      GravarLog('NFSE_CarregarLoteXML', logNormal);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.NotasFiscais.LoadFromLoteNfse(ArquivoOuXml);

      Result := SetRetornoNFSeRPSCarregadas(NFSeDM.ACBrNFSeX1.NotasFiscais.Count);
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.CarregarINI(const eArquivoOuINI: PChar): longint;
var
  ArquivoOuIni: string;
begin
  try
    ArquivoOuIni := ConverterAnsiParaUTF8(eArquivoOuINI);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_CarregarINI(' + ArquivoOuIni + ' )', logCompleto, True)
    else
      GravarLog('NFSE_CarregarINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.NotasFiscais.LoadFromIni(ArquivoOuIni);
      Result := SetRetornoNFSeRPSCarregadas(NFSeDM.ACBrNFSeX1.NotasFiscais.Count);
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ObterXml(aIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resposta: Ansistring;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ObterXml(' + IntToStr(aIndex) + ' )', logCompleto, True)
    else
      GravarLog('NFSE_ObterXml', logNormal);

    NFSeDM.Travar;

    try
      if (NFSeDM.ACBrNFSeX1.NotasFiscais.Count < 1) or (aIndex < 0) or
         (aIndex >= NFSeDM.ACBrNFSeX1.NotasFiscais.Count) then
         raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [aIndex]));

      if EstaVazio(NFSeDM.ACBrNFSeX1.NotasFiscais.Items[aIndex].XmlNfse) then
        NFSeDM.ACBrNFSeX1.NotasFiscais.Items[aIndex].GerarXML;

      Resposta := NFSeDM.ACBrNFSeX1.NotasFiscais.Items[aIndex].XmlNfse;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.GravarXml(aIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
var
  ANomeArquivo, APathArquivo: string;
begin
  try
    ANomeArquivo := ConverterAnsiParaUTF8(eNomeArquivo);
    APathArquivo := ConverterAnsiParaUTF8(ePathArquivo);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_GravarXml(' + IntToStr(aIndex) + ',' +
        ANomeArquivo + ',' + APathArquivo + ' )', logCompleto, True)
    else
      GravarLog('NFSE_GravarXml', logNormal);

    NFSeDM.Travar;
    try
      if (NFSeDM.ACBrNFSeX1.NotasFiscais.Count < 1) or (aIndex < 0) or
         (aIndex >= NFSeDM.ACBrNFSeX1.NotasFiscais.Count) then
         raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [aIndex]));

      if NFSeDM.ACBrNFSeX1.NotasFiscais.Items[aIndex].GravarXML(ANomeArquivo, APathArquivo) then
        Result := SetRetorno(ErrOK)
      else
        Result := SetRetorno(ErrGerarXml);
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ObterIni(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resposta: AnsiString;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ObterIni(' + IntToStr(AIndex) + ' )', logCompleto, True)
    else
      GravarLog('NFSE_ObterIni', logNormal);

    NFSeDM.Travar;
    try
      if (NFSeDM.ACBrNFSeX1.NotasFiscais.Count < 1) or (AIndex < 0) or (AIndex >= NFSeDM.ACBrNFSeX1.NotasFiscais.Count) then
        raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if EstaVazio(NFSeDM.ACBrNFSeX1.NotasFiscais.Items[AIndex].XmlNfse) then
        NFSeDM.ACBrNFSeX1.NotasFiscais.Items[AIndex].GerarXML;

      Resposta := NFSeDM.ACBrNFSeX1.NotasFiscais.Items[AIndex].GerarNFSeIni;
      Resposta := ConverterUTF8ParaAnsi(Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.GravarIni(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
var
  ANFSeIni, ANomeArquivo, APathArquivo: string;
begin
  try
    ANomeArquivo:= ConverterAnsiParaUTF8(eNomeArquivo);
    APathArquivo:= ConverterAnsiParaUTF8(ePathArquivo);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_GravarIni(' + IntToStr(AIndex) + ',' + ANomeArquivo + ',' + APathArquivo + ' )', logCompleto, True)
    else
      GravarLog('NFSE_GravarIni', logNormal);

    NFSeDM.Travar;
    try
      if (NFSeDM.ACBrNFSeX1.NotasFiscais.Count < 1) or (AIndex < 0) or (AIndex >= NFSeDM.ACBrNFSeX1.NotasFiscais.Count) then
        raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      ANomeArquivo := ExtractFileName(ANomeArquivo);

      if EstaVazio(ANomeArquivo) then
        raise EACBrLibException.Create(ErrExecutandoMetodo, 'Nome de arquivo não informado');

      if EstaVazio(APathArquivo) then
        APathArquivo := ExtractFilePath(ANomeArquivo);
      if EstaVazio(APathArquivo) then
        APathArquivo := NFSeDM.ACBrNFSeX1.Configuracoes.Arquivos.PathSalvar;

      APathArquivo := PathWithDelim(APathArquivo);

      if EstaVazio(NFSeDM.ACBrNFSeX1.NotasFiscais.Items[AIndex].XmlNfse) then
        NFSeDM.ACBrNFSeX1.NotasFiscais.Items[AIndex].GerarXML;

      ANFSeIni := NFSeDM.ACBrNFSeX1.NotasFiscais.Items[AIndex].GerarNFSeIni;
      if not DirectoryExists(APathArquivo) then
        ForceDirectories(APathArquivo);

      WriteToTXT(APathArquivo + ANomeArquivo, ANFSeIni, False, False);
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.LimparLista: longint;
begin
  try
    GravarLog('NFSE_LimparLista', logNormal);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.NotasFiscais.Clear;
      Result := SetRetornoNFSeRPSCarregadas(NFSeDM.ACBrNFSeX1.NotasFiscais.Count);
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
var
  Resposta: Ansistring;
begin
  try
    GravarLog('NFSE_ObterCertificados', logNormal);

    NFSeDM.Travar;

    try
      Resposta := '';
      Resposta := ObterCerticados(NFSeDM.ACBrNFSeX1.SSL);
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.Emitir(const aLote: PChar; aModoEnvio: longint; aImprimir: Boolean; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TEmiteResposta;
  Resposta: Ansistring;
  ModoEnvio: TmodoEnvio;
begin
  try
    if not (TEnum.TryParse<TmodoEnvio>(aModoEnvio, ModoEnvio)) then
      ModoEnvio := TmodoEnvio.meAutomatico;

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_Emitir(' + aLote + ',' + ModoEnvioToStr(ModoEnvio) +
                 ',' + BoolToStr(aImprimir, True) + ' )', logCompleto, True)
    else
      GravarLog('NFSE_Emitir', logNormal);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.Emitir(aLote, ModoEnvio, aImprimir);
      Resp := TEmiteResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.Emite);

        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.Cancelar(aInfCancelamentoNFSe: PChar; const sResposta: PChar; var esTamanho: longint):longint;
var
  Resp: TCancelarNFSeResposta;
  InfCancelamentoNFSe: TInfCancelamento;
  Resposta: AnsiString;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_Cancelar (' + aInfCancelamentoNFSe + ')', logCompleto, True)
    else
      GravarLog('NFSE_Cancelar', logNormal);

    if StringEhArquivo(aInfCancelamentoNFSe) then
      VerificarArquivoExiste(aInfCancelamentoNFSe);

    NFSeDM.Travar;
    try
      InfCancelamentoNFSe:= TInfCancelamento.Create;
       try
         InfCancelamentoNFSe.LerFromIni(aInfCancelamentoNFSe);
         NFSeDM.ACBrNFSeX1.CancelarNFSe(InfCancelamentoNFSe);
         Resp := TCancelarNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
         try
           Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.CancelaNFSe);

           Resposta:= Resp.Gerar;
           MoverStringParaPChar(Resposta, sResposta, esTamanho);
           Result := SetRetorno(ErrOK, Resposta);
         finally
           Resp.Free;
         end;
       finally
         InfCancelamentoNFSe.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.SubstituirNFSe(const aNumeroNFSe, aSerieNFSe, aCodigoCancelamento, aMotivoCancelamento, aNumeroLote, aCodigoVerificacao, sResposta: PChar; var esTamanho: longint):longint;
var
  Resp: TSubstituirNFSeResposta;
  NumeroNFSe, SerieNFSe, CodigoCancelamento, MotivoCancelamento, NumeroLote, CodigoVerificacao: String;
  Resposta: AnsiString;
begin
  try
    NumeroNFSe := ConverterAnsiParaUTF8(aNumeroNFSe);
    SerieNFSe := ConverterAnsiParaUTF8(aSerieNFSe);
    CodigoCancelamento := ConverterAnsiParaUTF8(aCodigoCancelamento);
    MotivoCancelamento := ConverterAnsiParaUTF8(aMotivoCancelamento);
    NumeroLote := ConverterAnsiParaUTF8(aNumeroLote);
    CodigoVerificacao := ConverterAnsiParaUTF8(aCodigoVerificacao);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_SubstituirNFSe(' + NumeroNFSe + ',' + SerieNFSe + ',' + CodigoCancelamento + ',' + MotivoCancelamento + ',' + NumeroLote + ',' + CodigoVerificacao + ' )', logCompleto, True)
      else
      GravarLog('NFSE_SubstituirNFSe', logNormal);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.SubstituirNFSe(NumeroNFSe, SerieNFSe, CodigoCancelamento, MotivoCancelamento, NumeroLote, CodigoCancelamento);
      Resp := TSubstituirNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.SubstituiNFSe);

        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.LinkNFSe(aNumeroNFSe: PChar; const aCodigoVerificacao, aChaveAcesso, aValorServico, sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TLinkNFSeResposta;
  NumeroNFSe, CodigoVerificacao, ChaveAcesso, ValorServico: String;
  Resposta: AnsiString;
begin
  try
    NumeroNFSe := ConverterAnsiParaUTF8(aNumeroNFSe);
    CodigoVerificacao := ConverterAnsiParaUTF8(aCodigoVerificacao);
    ChaveAcesso := ConverterAnsiParaUTF8(aChaveAcesso);
    ValorServico := ConverterAnsiParaUTF8(aValorServico);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_LinkNFSe(' + NumeroNFSe + ',' + CodigoVerificacao + ',' + ChaveAcesso + ',' + ValorServico + ' )', logCompleto, True)
      else
      GravarLog('NFSE_LinkNFSe', logNormal);

    NFSeDM.Travar;
    try
      Resposta := 'LinkNFSe: '+ NFSeDM.ACBrNFSeX1.LinkNFSe(NumeroNFSe, CodigoVerificacao, ChaveAcesso, ValorServico);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.GerarLote(const aLote: PChar; aQtdMaximaRps, aModoEnvio: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TGerarLoteResposta;
  Resposta: Ansistring;
  ModoEnvio: TmodoEnvio;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_GerarLote(' + aLote + ',' + IntToStr(aQtdMaximaRps) + ',' + IntToStr(aModoEnvio) + ' )', logCompleto, True)
      else
      GravarLog('NFSE_GerarLote', logNormal);

    ModoEnvio:= TmodoEnvio(aModoEnvio);
    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.GerarLote(aLote, aQtdMaximaRps, ModoEnvio);
      Resp := TGerarLoteResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.Gerar);
        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.GerarToken(const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TGerarTokenResposta;
  Resposta: Ansistring;
begin
  try
    GravarLog('NFSE_GerarToken', logNormal);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.GerarToken;
      Resp:= TGerarTokenResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.GerarToken);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarSituacao(const aProtocolo, aNumLote, sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaSituacaoResposta;
  Protocolo, NumLote: string;
  Resposta: Ansistring;
begin
  try
    Protocolo := ConverterAnsiParaUTF8(aProtocolo);
    NumLote := ConverterAnsiParaUTF8(aNumLote);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarSituacao(' + Protocolo + ',' + NumLote + ' )', logCompleto, True)
    else
      GravarLog('NFSE_ConsultarSituacao', logNormal);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarSituacao(Protocolo, NumLote);
      Resp := TConsultaSituacaoResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaSituacao);

        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarLoteRps(const aProtocolo, aNumLote, sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaLoteRpsResposta;
  Protocolo, NumLote: string;
  Resposta: Ansistring;
begin
  try
    Protocolo := ConverterAnsiParaUTF8(aProtocolo);
    NumLote := ConverterAnsiParaUTF8(aNumLote);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarLoteRps(' + Protocolo + ',' + NumLote + ' )', logCompleto, True)
    else
      GravarLog('NFSE_ConsultarLoteRps', logNormal);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarLoteRps(Protocolo, NumLote);
      Resp := TConsultaLoteRpsResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaLoteRps);

        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSePorRps(const aNumeroRps, aSerie, aTipo, aCodigoVerificacao, sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaNFSePorRpsResposta;
  NumeroRps, Serie, Tipo, CodigoVerificacao: String;
  Resposta: AnsiString;
begin
  try
    NumeroRps:= ConverterAnsiParaUTF8(aNumeroRps);
    Serie:= ConverterAnsiParaUTF8(aSerie);
    Tipo:= ConverterAnsiParaUTF8(aTipo);
    CodigoVerificacao:= ConverterAnsiParaUTF8(aCodigoVerificacao);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSePorRps(' + NumeroRps + ',' + Serie + ',' + Tipo + ',' + CodigoVerificacao + ' )', logCompleto, True)
      else
        GravarLog('NFSE_ConsultarNFSePorRps', logNormal);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarNFSePorRps(NumeroRps, Serie, Tipo, CodigoVerificacao);
      Resp := TConsultaNFSePorRpsResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSeporRps);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSePorNumero(const aNumero:PChar; aPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaNFSeResposta;
  Numero: String;
  Resposta: AnsiString;
begin
  try
    Numero:= ConverterAnsiParaUTF8(aNumero);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSePorNumero(' + Numero + ',' + IntToStr(aPagina) + ' )', logCompleto, True)
    else
      GravarLog('NFSE_ConsultarNFSePorNumero', logNormal);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarNFSePorNumero(Numero, aPagina);
      Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSePorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: longint; aNumeroLote: PChar; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaNFSeResposta;
  NumeroLote: String;
  Resposta: AnsiString;
  TipoPeriodo: TtpPeriodo;
begin
  try
    NumeroLote:= ConverterAnsiParaUTF8(aNumeroLote);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSePorPeriodo(' + DateToStr(aDataInicial) +  ',' + DateToStr(aDataFinal) +  ',' + IntToStr(aPagina) +  ',' + NumeroLote +  ',' + IntToStr(aTipoPeriodo) + ' )', logCompleto, True)
      else
        GravarLog('NFSE_ConsultarNFSePorPeriodo', logNormal);

    TipoPeriodo:= TtpPeriodo(aTipoPeriodo);
    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarNFSePorPeriodo(aDataInicial, aDataFinal, aPagina, NumeroLote, TipoPeriodo);
      Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSePorFaixa(const aNumeroInicial, aNumeroFinal: PChar; aPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaNFSeResposta;
  NumeroInicial, NumeroFinal: String;
  Resposta: AnsiString;
begin
  try
    NumeroInicial:= ConverterAnsiParaUTF8(aNumeroInicial);
    NumeroFinal:= ConverterAnsiParaUTF8(aNumeroFinal);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSePorFaixa(' + NumeroInicial + ',' + NumeroFinal + ',' + IntToStr(aPagina) + ' )', logCompleto, True)
      else
        GravarLog('NFSE_ConsultarNFSePorFaixa', logNormal);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarNFSePorFaixa(NumeroInicial, NumeroFinal, aPagina);
      Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSeGenerico(aInfConsultaNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaNFSeResposta;
  InfConsultaNFSe: TInfConsultaNFSe;
  Resposta: AnsiString;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSeGenerico (' + aInfConsultaNFSe + ')', logCompleto, True)
    else
      GravarLog('NFSE_ConsultarNFSeGenerico', logNormal);

    if StringEhArquivo(aInfConsultaNFSe) then
      VerificarArquivoExiste(aInfConsultaNFSe);

    NFSeDM.Travar;
    try
      InfConsultaNFSe:= TInfConsultaNFSe.Create;
      try
        InfConsultaNFSe.LerFromIni(aInfConsultaNFSe);
        NFSeDM.ACBrNFSeX1.ConsultarNFSeGenerico(InfConsultaNFSe);
        Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
        try
          Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

          Resposta:= Resp.Gerar;
          MoverStringParaPChar(Resposta, sResposta, esTamanho);

          Result := SetRetorno(ErrOK, Resposta);
        finally
          Resp.Free;
        end;
    finally
      InfConsultaNFSe.Free;
    end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarLinkNFSe(aInfConsultaLinkNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultarLinkNFSeResposta;
  InfConsultaLinkNFSe: TInfConsultaLinkNFSe;
  Resposta: AnsiString;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarLinkNFSe (' + aInfConsultaLinkNFSe + ')', logCompleto, True)
    else
      GravarLog('NFSE_ConsultarLinkNFSe', logNormal);

    if StringEhArquivo(aInfConsultaLinkNFSe) then
      VerificarArquivoExiste(aInfConsultaLinkNFSe);

    NFSeDM.Travar;
    try
      InfConsultaLinkNFSe:= TInfConsultaLinkNFSe.Create;
      try
        InfConsultaLinkNFSe.LerFromIni(aInfConsultaLinkNFSe);
        NFSeDM.ACBrNFSeX1.ConsultarLinkNFSe(InfConsultaLinkNFSe);
        Resp := TConsultarLinkNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
        try
          Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaLinkNFSe);

          Resposta:= Resp.Gerar;
          MoverStringParaPChar(Resposta, sResposta, esTamanho);

          Result := SetRetorno(ErrOK, Resposta);
        finally
          Resp.Free;
        end;
    finally
      InfConsultaLinkNFSe.Free;
    end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.EnviarEmail(const ePara, eXmlNFSe: PChar; const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar):longint;
var
  Resposta, APara, AXmlNFSe, AAssunto, ACC, AAnexos, AMensagem: String;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
  Resp: TLibNFSeResposta;
begin
  try
    APara := ConverterAnsiParaUTF8(ePara);
    AXmlNFSe := ConverterAnsiParaUTF8(eXmlNFSe);
    AAssunto := ConverterAnsiParaUTF8(eAssunto);
    ACC := ConverterAnsiParaUTF8(eCC);
    AAnexos := ConverterAnsiParaUTF8(eAnexos);
    AMensagem := ConverterAnsiParaUTF8(eMensagem);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_EnviarEmail(' + APara + ',' + AXmlNFSe + ',' + BoolToStr(AEnviaPDF, 'PDF', '') + ',' + AAssunto
                 + ',' + ACC + ',' + AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      GravarLog('NFSE_EnviarEmail', logNormal);

    NFSeDM.Travar;
    try
      with NFSeDM do
      begin
        EhArquivo:= StringEhArquivo(AXmlNFSe);

        if EhArquivo then
          VerificarArquivoExiste(AXmlNFSe);

        if EhArquivo then
          ACBrNFSeX1.NotasFiscais.LoadFromFile(AXmlNFSe)
          else
          ACBrNFSeX1.NotasFiscais.LoadFromString(AXmlNFSe);

        if ACBrNFSeX1.NotasFiscais.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfNFSeCarregadas, [ACBrNFSeX1.NotasFiscais.Count]))
          else
          begin
            slMensagemEmail := TStringList.Create;
            slCC := TStringList.Create;
            slAnexos := TStringList.Create;
            Resp := TLibNFSeResposta.Create('EnviarEmail', Config.TipoResposta, Config.CodResposta);
            try
              with ACBrNFSeX1 do
              begin
                slMensagemEmail.DelimitedText := sLineBreak;
                slMensagemEmail.Text := StringReplace(AMensagem, ';', sLineBreak, [rfReplaceAll]);

                slCC.DelimitedText := sLineBreak;
                slCC.Text := StringReplace(ACC, ';', sLineBreak, [rfReplaceAll]);

                slAnexos.DelimitedText := sLineBreak;
                slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

                if (AEnviaPDF) then
                  NFSeDM.ConfigurarImpressao('', True);

                NotasFiscais.Items[0].EnviarEmail(APara, AAssunto, slMensagemEmail, AEnviaPDF, slCC, slAnexos);

                Resp.Msg := 'Email enviado com sucesso';
                Resposta := Resp.Gerar;

                Result := SetRetorno(ErrOK, Resposta);
              end;
            finally
              Resp.Free;
              slCC.Free;
              slAnexos.Free;
              slMensagemEmail.Free;
              if (AEnviaPDF) then NFSeDM.FinalizarImpressao;
            end;
          end;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.Imprimir(const cImpressora: PChar; nNumCopias: integer; const bGerarPDF, bMostrarPreview, cCancelada: PChar): longint;
var
  Resposta: TLibImpressaoResposta;
  Impressora, MostrarPreview, GerarPDF, Cancelada: String;
begin
  try
    Impressora := ConverterAnsiParaUTF8(cImpressora);
    MostrarPreview := ConverterAnsiParaUTF8(bMostrarPreview);
    GerarPDF:= ConverterAnsiParaUTF8(bGerarPDF);
    Cancelada := ConverterAnsiParaUTF8(cCancelada);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_Imprimir(' + Impressora + ',' + IntToStr(nNumCopias)+ ',' + GerarPDF + ',' + MostrarPreview + ',' + Cancelada + ' )', logCompleto, True)
    else
      GravarLog('NFSE_Imprimir', logNormal);

    NFSeDM.Travar;
    Resposta := TLibImpressaoResposta.Create(NFSeDM.ACBrNFSeX1.NotasFiscais.Count, Config.TipoResposta, Config.CodResposta);

    try
      NFSeDM.ConfigurarImpressao(Impressora, False, MostrarPreview, Cancelada);
      if nNumCopias > 0 then
        NFSeDM.ACBrNFSeX1.DANFSE.NumCopias := nNumCopias;

      NFSeDM.ACBrNFSeX1.NotasFiscais.Imprimir;
      Result := SetRetorno(ErrOK, Resposta.Gerar);
    finally
    NFSeDM.FinalizarImpressao;
    Resposta.Free;
    NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ImprimirPDF: longint;
var
  Resposta: TLibImpressaoResposta;
begin
  try
    GravarLog('NFSE_ImprimirPDF', logNormal);

    NFSeDM.Travar;

    try
      Resposta := TLibImpressaoResposta.Create(NFSeDM.ACBrNFSeX1.NotasFiscais.Count, Config.TipoResposta, Config.CodResposta);
      try
        NFSeDM.ConfigurarImpressao('', True);
        try
          NFSeDM.ACBrNFSeX1.NotasFiscais.ImprimirPDF;
          Resposta.Msg := NFSeDM.ACBrNFSeX1.DANFSE.ArquivoPDF;
          Result := SetRetorno(ErrOK, Resposta.Gerar);
        finally
        NFSeDM.FinalizarImpressao;
        end;
      finally
      Resposta.Free;
      end;
    finally
    NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.SalvarPDF(const sResposta: PChar; var esTamanho: longint):longint;
var
  AStream: TMemoryStream;
  Resposta: Ansistring;
begin
  try
    GravarLog('NFSE_SalvarPDF', logNormal);

    NFSeDM.Travar;

    AStream := TMemoryStream.Create;

    try
      NFSeDM.ConfigurarImpressao('', True);

      NFSeDM.ACBrNFSeX1.NotasFiscais.ImprimirPDF(AStream);
      Resposta := StreamToBase64(AStream);

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFSeDM.FinalizarImpressao;
      AStream.Free;
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSeServicoPrestadoPorNumero(
 const aNumero: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime;
 aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint
 ): longint;
var
  Resp: TConsultaNFSeResposta;
  Numero: String;
  Resposta: AnsiString;
  TipoPeriodo: TtpPeriodo;
begin
  try
    Numero:= ConverterAnsiParaUTF8(aNumero);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSeServicoPrestadoPorNumero(' + Numero + ',' + IntToStr(aPagina) + ',' + DateToStr(aDataInicial) + ',' + DateToStr(aDataFinal) + ',' + IntToStr(aTipoPeriodo) + ' )', logCompleto, True)
      else
        GravarLog('NFSE_ConsultarNFSeServicoPrestadoPorNumero', logNormal);

    TipoPeriodo:= TtpPeriodo(aTipoPeriodo);
    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarNFSeServicoPrestadoPorNumero(aNumero, aPagina, aDataInicial, aDataFinal, TipoPeriodo);
      Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: longint; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaNFSeResposta;
  Resposta: AnsiString;
  TipoPeriodo: TtpPeriodo;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(' + DateToStr(aDataInicial) + ',' + DateToStr(aDataFinal) + ',' + IntToStr(aPagina) + ',' + IntToStr(aTipoPeriodo) + ' )', logCompleto, True)
      else
      GravarLog('NFSE_ConsultarNFSeServicoPrestadoPorPeriodo', logNormal);

    TipoPeriodo:= TtpPeriodo(aTipoPeriodo);
    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal, aPagina, TipoPeriodo);
      Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSeServicoPrestadoPorTomador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaNFSeResposta;
  CNPJ, InscMunicipal: String;
  Resposta: AnsiString;
  TipoPeriodo: TtpPeriodo;
begin
  try
    CNPJ:= ConverterAnsiParaUTF8(aCNPJ);
    InscMunicipal:= ConverterAnsiParaUTF8(aInscMun);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSeServicoPrestadoPorTomador(' + CNPJ + ',' + InscMunicipal + ',' + IntToStr(aPagina) + ',' + DateToStr(aDataInicial) + ',' + DateToStr(aDataFinal) + ',' + IntToStr(aTipoPeriodo) + ' )', logCompleto, True)
      else
      GravarLog('NFSE_ConsultarNFSeServicoPrestadoPorTomador', logNormal);

    TipoPeriodo:= TtpPeriodo(aTipoPeriodo);
    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarNFSeServicoPrestadoPorTomador(CNPJ, InscMunicipal, aPagina, aDataInicial, aDataFinal, TipoPeriodo);
      Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSeServicoPrestadoPorIntermediario(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaNFSeResposta;
  CNPJ, InscMunicipal: String;
  Resposta: AnsiString;
  TipoPeriodo: TtpPeriodo;
begin
  try
    CNPJ:= ConverterAnsiParaUTF8(aCNPJ);
    InscMunicipal:= ConverterAnsiParaUTF8(aInscMun);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(' + CNPJ + ',' + InscMunicipal + ',' + IntToStr(aPagina) + ',' + DateToStr(aDataInicial) + ',' + DateToStr(aDataFinal) + ',' + IntToStr(aTipoPeriodo) + ' )' , logCompleto, True)
      else
      GravarLog('NFSE_ConsultarNFSeServicoPrestadoPorIntermediario', logNormal);

    TipoPeriodo:= TtpPeriodo(aTipoPeriodo);
    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarNFSeServicoTomadoPorIntermediario(CNPJ, InscMunicipal, aPagina, aDataInicial, aDataFinal, TipoPeriodo);
      Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSeServicoTomadoPorNumero(const aNumero: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint):longint;
var
  Resp: TConsultaNFSeResposta;
  Numero: String;
  Resposta: AnsiString;
  TipoPeriodo: TtpPeriodo;
begin
  try
    Numero:= ConverterAnsiParaUTF8(aNumero);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSeServicoTomadoPorNumero(' + Numero + ',' + IntToStr(aPagina) + ',' + DateToStr(aDataInicial) + ',' + DateToStr(aDataFinal) + ',' + IntToStr(aTipoPeriodo) + ' )', logCompleto, True)
      else
      GravarLog('NFSE_ConsultarNFSeServicoTomadoPorNumero', logNormal);

    TipoPeriodo:= TtpPeriodo(aTipoPeriodo);
    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarNFSeServicoTomadoPorNumero(Numero, aPagina, aDataInicial, aDataFinal, TipoPeriodo);
      Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSeServicoTomadoPorPrestador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaNFSeResposta;
  CNPJ, InscMunicipal: String;
  Resposta: AnsiString;
  TipoPeriodo: TtpPeriodo;
begin
  try
    CNPJ:= ConverterAnsiParaUTF8(aCNPJ);
    InscMunicipal:= ConverterAnsiParaUTF8(aInscMun);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSeServicoTomadoPorPrestador(' + CNPJ + ',' + InscMunicipal + ',' + IntToStr(aPagina) + ',' + DateToStr(aDataInicial) + ',' + DateToStr(aDataFinal) + ',' + IntToStr(aTipoPeriodo) + ' )', logCompleto, True)
      else
      GravarLog('NFSE_ConsultarNFSeServicoTomadoPorPrestador ', logNormal);

    TipoPeriodo:= TtpPeriodo(aTipoPeriodo);
    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarNFSeServicoTomadoPorPrestador(CNPJ, InscMunicipal, aPagina, aDataInicial, aDataFinal, TipoPeriodo);
      Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSeServicoTomadoPorTomador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaNFSeResposta;
  CNPJ, InscMunicipal: String;
  Resposta: AnsiString;
  TipoPeriodo: TtpPeriodo;
begin
  try
    CNPJ:= ConverterAnsiParaUTF8(aCNPJ);
    InscMunicipal:= ConverterAnsiParaUTF8(aInscMun);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSeServicoTomadoPorTomador(' + CNPJ + ',' + InscMunicipal + ',' + IntToStr(aPagina) + ',' + DateToStr(aDataInicial) + ',' + DateToStr(aDataFinal) + ',' + IntToStr(aTipoPeriodo) + ' )', logCompleto, True)
      else
      GravarLog('NFSE_ConsultarNFSeServicoTomadoPorTomador ', logNormal);

    TipoPeriodo:=TtpPeriodo(aTipoPeriodo);
    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarNFSeServicoTomadoPorTomador(CNPJ, InscMunicipal, aPagina, aDataInicial, aDataFinal, TipoPeriodo);
      Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: longint; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint):longint;
var
  Resp: TConsultaNFSeResposta;
  Resposta: AnsiString;
  TipoPeriodo: TtpPeriodo;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSeServicoTomadoPorPeriodo(' + DateToStr(aDataInicial) + ',' + DateToStr(aDataFinal) + ',' + IntToStr(aPagina) + ',' + IntToStr(aTipoPeriodo) + ' )', logCompleto, True)
      else
      GravarLog('NFSE_ConsultarNFSeServicoTomadoPorPeriodo', logNormal);

    TipoPeriodo:= TtpPeriodo(aTipoPeriodo);
    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal, aPagina, TipoPeriodo);
      Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSeServicoTomadoPorIntermediario(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaNFSeResposta;
  CNPJ, InscMunicipal: String;
  Resposta: AnsiString;
  TipoPeriodo: TtpPeriodo;
begin
  try
    CNPJ:= ConverterAnsiParaUTF8(aCNPJ);
    InscMunicipal:= ConverterAnsiParaUTF8(aInscMun);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSeServicoTomadoPorIntermediario(' + CNPJ + ',' + InscMunicipal + ',' + IntToStr(aPagina) + ',' + DateToStr(aDataInicial) + ',' + DateToStr(aDataFinal) + ',' + IntToStr(aTipoPeriodo) + ' )', logCompleto, True)
      else
      GravarLog('NFSE_ConsultarNFSeServicoTomadoPorIntermediario', logNormal);

    TipoPeriodo:= TtpPeriodo(aTipoPeriodo);
    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarNFSeServicoTomadoPorIntermediario(CNPJ, InscMunicipal, aPagina, aDataInicial, aDataFinal, TipoPeriodo);
      Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.EnviarEvento(aInfEvento: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TEnviarEventoResposta;
  InfEvento: TInfEvento;
  Resposta: AnsiString;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_EnviarEvento (' + aInfEvento + ')', logCompleto, True)
    else
      GravarLog('NFSE_EnviarEvento', logNormal);

    if StringEhArquivo(aInfEvento) then
      VerificarArquivoExiste(aInfEvento);

    NFSeDM.Travar;
    try
      InfEvento := TInfEvento.Create;
      try
        InfEvento.LerFromIni(aInfEvento);
        NFSeDM.ACBrNFSeX1.EnviarEvento(InfEvento);
        Resp:= TEnviarEventoResposta.Create(Config.TipoResposta, Config.CodResposta);
        try
          Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.EnviarEvento);

          Resposta:= Resp.Gerar;
          MoverStringParaPChar(Resposta, sResposta, esTamanho);

          Result := SetRetorno(ErrOK, sResposta);
        finally
          Resp.Free;
        end;
      finally
        InfEvento.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarDPSPorChave(const aChaveDPS: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaNFSePorRpsResposta;
  ChaveDPS: String;
  Resposta: AnsiString;
begin
  try
    ChaveDPS := ConverterAnsiParaUTF8(aChaveDPS);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarDPSPorChave(' + ChaveDPS + ' )', logCompleto, True)
      else
      GravarLog('NFSE_ConsultarDPSPorChave', logNormal);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarDPSPorChave(ChaveDPS);
      Resp := TConsultaNFSePorRpsResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSeporRps);

        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarNFSePorChave(const aChaveNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaNFSeResposta;
  ChaveNFSe: String;
  Resposta: AnsiString;
begin
  try
    ChaveNFSe := ConverterAnsiParaUTF8(aChaveNFSe);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarNFSePorChave(' + ChaveNFSe + ' )', logCompleto, True)
      else
      GravarLog('NFSE_ConsultarNFSePorChave', logNormal);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarNFSePorChave(ChaveNFSe);
      Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarEvento(const aChave: PChar; aTipoEvento: longint; aNumSeq: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaEventoResposta;
  Chave: String;
  TipoEvento: TtpEvento;
  Resposta: AnsiString;
begin
  try
    Chave := ConverterAnsiParaUTF8(aChave);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarEvento(' + Chave + ',' + IntToStr(aTipoEvento) + ',' + IntToStr(aNumSeq) + ' )', logCompleto, True)
      else
      GravarLog('NFSE_ConsultarEvento', logNormal);

    TipoEvento:= TtpEvento(aTipoEvento);
    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarEvento(Chave, TipoEvento, aNumSeq);
      Resp := TConsultaEventoResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultarEvento);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarDFe(aNSU: longint; sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaDFeResposta;
  Resposta: AnsiString;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarDFe(' + IntToStr(aNSU) + ' )', logCompleto, True)
      else
      GravarLog('NFSE_ConsultarDFe', logNormal);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarDFe(aNSU);
      Resp := TConsultaDFeResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultarDFe);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ObterDANFSE(const aChaveNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaNFSeResposta;
  ChaveNFSe: String;
  Resposta: AnsiString;
begin
  try
    ChaveNFSe := ConverterAnsiParaUTF8(aChaveNFSe);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ObterDANFSe(' + ChaveNFSe + ' )', logCompleto, True)
      else
      GravarLog('NFSE_ObterDANFSe', logNormal);

    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ObterDANFSE(ChaveNFSe);
      Resp := TConsultaNFSeResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultaNFSe);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarParametros(aTipoParametroMunicipio: longint; const aCodigoServico: PChar; aCompetencia: TDateTime; aNumeroBeneficio: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaParametrosResposta;
  TipoParametro: TParamMunic;
  CodigoServico, NumeroBeneficio: String;
  Resposta: AnsiString;
begin
  try
    CodigoServico:= ConverterAnsiParaUTF8(aCodigoServico);
    NumeroBeneficio:= ConverterAnsiParaUTF8(aNumeroBeneficio);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarParametros(' + IntToStr(aTipoParametroMunicipio) + ',' + CodigoServico + ',' + DateToStr(aCompetencia) + ',' + NumeroBeneficio +' )', logCompleto, True)
      else
      GravarLog('NFSE_ConsultarParametros', logNormal);

    TipoParametro:= TParamMunic(aTipoParametroMunicipio);
    NFSeDM.Travar;
    try
      NFSeDM.ACBrNFSeX1.ConsultarParametros(TipoParametro, CodigoServico, aCompetencia, NumeroBeneficio);
      Resp:= TConsultaParametrosResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.WebService.ConsultarParam);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ObterInformacoesProvedor(const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TObterInformacoesProvedorResposta;
  Resposta: AnsiString;
begin
  try
    GravarLog('NFSE_ObterInformacoesProvedor', logNormal);

    NFSeDM.Travar;
    try
      Resp := TObterInformacoesProvedorResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFSeDM.ACBrNFSeX1.Configuracoes.Geral);

        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
    finally
      NFSeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

end.

