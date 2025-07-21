{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
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

unit ACBrLibNFComBase;

interface

uses
  Classes, SysUtils, Forms, ACBrUtil.FilesIO,
  ACBrLibComum, ACBrLibNFComDataModule, ACBrDFeException;

type
{ TACBrLibNFCom }
  TACBrLibNFCom = class(TACBrLib)
  private
    FNFComDM: TLibNFComDM;

    function SetRetornoNFComCarregadas(const NumNFCom: Integer): Integer;
    function SetRetornoEventoCarregados(const NumEventos: Integer): Integer;

  protected
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; Override;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property NFComDM: TLibNFComDM read FNFComDM;

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
    function ObterCertificados(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function GetPath(ATipo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function GetPathEvento(ACodEvento: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function StatusServico(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function Enviar(AImprimir: Boolean; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function Consultar(const eChaveOuNFCom: PAnsiChar; AExtrairEventos: Boolean; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function Cancelar(const eChave, eJustificativa, eCNPJCPF: PAnsiChar; ALote: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function EnviarEvento(idLote: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function EnviarEmail(const ePara, eXmlNFCom: PAnsiChar; const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
    function EnviarEmailEvento(const ePara, eXmlEvento, eXmlNFCom: PAnsiChar; const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
    function Imprimir(const cImpressora: PAnsiChar; nNumCopias: Integer; bMostrarPreview: PAnsiChar): Integer;
    function ImprimirPDF: Integer;
    function SalvarPDF(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    function ImprimirEvento(const eArquivoXmlNFCom, eArquivoXmlEvento: PAnsiChar): Integer;
    function ImprimirEventoPDF(const eArquivoXmlNFCom, eArquivoXmlEvento: PAnsiChar): Integer;
    function SalvarEventoPDF(const eArquivoXmlNFCom, eArquivoXmlEvento, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
end;

implementation

Uses
  ACBrNFCom, ACBrUtil.Base, ACBrUtil.Strings, ACBrDFeUtil, ACBrXmlBase, pcnConversao, ACBrNFComConversao,
  ACBrLibConsts, ACBrLibConfig, ACBrLibResposta,
  ACBrLibNFComConsts, ACBrLibNFComConfig,
  ACBrLibNFComRespostas, ACBrLibHelpers, ACBrLibCertUtils;

{ TACBrLibNFCom }

function TACBrLibNFCom.SetRetornoNFComCarregadas(const NumNFCom: Integer
  ): Integer;
begin
  Result := SetRetorno(0, Format(SInfNFComCarregadas, [NumNFCom]));
end;

function TACBrLibNFCom.SetRetornoEventoCarregados(const NumEventos: Integer
  ): Integer;
begin
  Result := SetRetorno(0, Format(SInfEventosCarregados, [NumEventos]));
end;

procedure TACBrLibNFCom.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibNFComConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibNFCom.Executar;
begin
  inherited Executar;
  FNFComDM.AplicarConfiguracoes;
end;

constructor TACBrLibNFCom.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FNFComDM := TLibNFComDM.Create(Nil);
  FNFComDM.Lib := Self;
end;

destructor TACBrLibNFCom.Destroy;
begin
  FNFComDM.Free;
  inherited Destroy;
end;

function TACBrLibNFCom.CarregarXML(const eArquivoOuXML: PAnsiChar): Integer;
var
  EhArquivo: Boolean;
  ArquivoOuXml: String;
begin
  try
    ArquivoOuXml := ConverterStringEntrada(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
       GravarLog('NFCom_CarregarXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
       GravarLog('NFCom_CarregarXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
       VerificarArquivoExiste(ArquivoOuXml);

    NFComDM.Travar;
    try
      if EhArquivo then
         NFComDM.ACBrNFCom1.NotasFiscais.LoadFromFile(ArquivoOuXml)
      else
         NFComDM.ACBrNFCom1.NotasFiscais.LoadFromString(ArquivoOuXml);

      Result := SetRetornoNFComCarregadas(NFComDM.ACBrNFCom1.NotasFiscais.Count);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.CarregarINI(const eArquivoOuINI: PAnsiChar): Integer;
var
  ArquivoOuINI: String;
begin
  try
    ArquivoOuINI := ConverterStringEntrada(eArquivoOuINI);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_CarregarINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      GravarLog('NFCom_CarregarINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    NFComDM.Travar;
    try
      NFComDM.ACBrNFCom1.NotasFiscais.LoadFromIni(ArquivoOuINI);
      Result := SetRetornoNFComCarregadas(NFComDM.ACBrNFCom1.NotasFiscais.Count);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.ObterXml(AIndex: Integer; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
var
  Resposta: AnsiString;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_ObterXml(' + IntToStr(AIndex) + ' )', logCompleto, True)
    else
      GravarLog('NFCom_ObterXml', logNormal);

    NFComDM.Travar;
    try
      if (NFComDM.ACBrNFCom1.NotasFiscais.Count < 1) or (AIndex < 0) or
         (AIndex >= NFComDM.ACBrNFCom1.NotasFiscais.Count) then
         raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if EstaVazio(NFComDM.ACBrNFCom1.NotasFiscais.Items[AIndex].XMLOriginal) then
        NFComDM.ACBrNFCom1.NotasFiscais.Items[AIndex].GerarXML;

      Resposta := NFComDM.ACBrNFCom1.NotasFiscais.Items[AIndex].XMLOriginal;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.GravarXml(AIndex: Integer; const eNomeArquivo,
  ePathArquivo: PAnsiChar): Integer;
var
  ANomeArquivo, APathArquivo: String;
begin
  try
    ANomeArquivo := ConverterStringEntrada(eNomeArquivo);
    APathArquivo := ConverterStringEntrada(ePathArquivo);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_GravarXml(' + IntToStr(AIndex) + ',' + ANomeArquivo + ',' + APathArquivo + ' )', logCompleto, True)
    else
      GravarLog('NFCom_GravarXml', logNormal);

    NFComDM.Travar;
    try
      if (NFComDM.ACBrNFCom1.NotasFiscais.Count < 1) or (AIndex < 0) or
         (AIndex >= NFComDM.ACBrNFCom1.NotasFiscais.Count) then
         raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if NFComDM.ACBrNFCom1.NotasFiscais.Items[AIndex].GravarXML(ANomeArquivo, APathArquivo) then
        Result := SetRetorno(ErrOK)
      else
        Result := SetRetorno(ErrGerarXml);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.ObterIni(AIndex: Integer; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
var
  Resposta: AnsiString;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_ObterIni(' + IntToStr(AIndex) + ' )', logCompleto, True)
    else
      GravarLog('NFCom_ObterIni', logNormal);

    NFComDM.Travar;
    try
      if (NFComDM.ACBrNFCom1.NotasFiscais.Count < 1) or (AIndex < 0) or (AIndex >= NFComDM.ACBrNFCom1.NotasFiscais.Count) then
        raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if EstaVazio(NFComDM.ACBrNFCom1.NotasFiscais.Items[AIndex].XMLOriginal) then
        NFComDM.ACBrNFCom1.NotasFiscais.Items[AIndex].GerarXML;

      Resposta := NFComDM.ACBrNFCom1.NotasFiscais.Items[AIndex].GerarNFComIni;
      Resposta := ConverterStringSaida( Resposta );
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.GravarIni(AIndex: Integer; const eNomeArquivo,
  ePathArquivo: PAnsiChar): Integer;
var
  ANFComIni, ANomeArquivo, APathArquivo: String;
begin
  try
    ANomeArquivo := ConverterStringEntrada(eNomeArquivo);
    APathArquivo := ConverterStringEntrada(ePathArquivo);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_GravarIni(' + IntToStr(AIndex) + ',' + ANomeArquivo + ',' + APathArquivo + ' )', logCompleto, True)
    else
      GravarLog('NFCom_GravarIni', logNormal);

    NFComDM.Travar;
    try
      if (NFComDM.ACBrNFCom1.NotasFiscais.Count < 1) or (AIndex < 0) or (AIndex >= NFComDM.ACBrNFCom1.NotasFiscais.Count) then
        raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      ANomeArquivo := ExtractFileName(ANomeArquivo);

      if EstaVazio(ANomeArquivo) then
        raise EACBrLibException.Create(ErrExecutandoMetodo, 'Nome de arquivo não informado');

      if EstaVazio(APathArquivo) then
        APathArquivo := ExtractFilePath(ANomeArquivo);
      if EstaVazio(APathArquivo) then
        APathArquivo := NFComDM.ACBrNFCom1.Configuracoes.Arquivos.PathSalvar;

      APathArquivo := PathWithDelim(APathArquivo);

      if EstaVazio(NFComDM.ACBrNFCom1.NotasFiscais.Items[AIndex].XMLOriginal) then
        NFComDM.ACBrNFCom1.NotasFiscais.Items[AIndex].GerarXML;

      ANFComIni := NFComDM.ACBrNFCom1.NotasFiscais.Items[AIndex].GerarNFComIni;
      if not DirectoryExists(APathArquivo) then
        ForceDirectories(APathArquivo);

      WriteToTXT(APathArquivo + ANomeArquivo, ANFComIni, False, False);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.CarregarEventoXML(const eArquivoOuXML: PAnsiChar
  ): Integer;
var
  EhArquivo: Boolean;
  ArquivoOuXml: String;
begin
  try
    ArquivoOuXml := ConverterStringEntrada(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_CarregarEventoXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      GravarLog('NFCom_CarregarEventoXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    NFComDM.Travar;
    try
      if EhArquivo then
        NFComDM.ACBrNFCom1.EventoNFCom.LerXML(ArquivoOuXml)
      else
        NFComDM.ACBrNFCom1.EventoNFCom.LerXMLFromString(ArquivoOuXml);

      Result := SetRetornoEventoCarregados(NFComDM.ACBrNFCom1.EventoNFCom.Evento.Count);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.CarregarEventoINI(const eArquivoOuINI: PAnsiChar
  ): Integer;
var
  ArquivoOuINI: String;
begin
  try
    ArquivoOuINI := ConverterStringEntrada(eArquivoOuINI);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_CarregarEventoINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      GravarLog('NFCom_CarregarEventoINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    NFComDM.Travar;
    try
      NFComDM.ACBrNFCom1.EventoNFCom.LerFromIni(ArquivoOuINI);
      Result := SetRetornoEventoCarregados(NFComDM.ACBrNFCom1.EventoNFCom.Evento.Count);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.LimparLista: Integer;
begin
  try
    GravarLog('NFCom_LimparLista', logNormal);

    NFComDM.Travar;
    try
      NFComDM.ACBrNFCom1.NotasFiscais.Clear;
      Result := SetRetornoNFComCarregadas(NFComDM.ACBrNFCom1.NotasFiscais.Count);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.LimparListaEventos: Integer;
begin
  try
    GravarLog('NFCom_LimparListaEventos', logNormal);

    NFComDM.Travar;
    try
      NFComDM.ACBrNFCom1.EventoNFCom.Evento.Clear;
      Result := SetRetornoEventoCarregados(NFComDM.ACBrNFCom1.EventoNFCom.Evento.Count);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.Assinar: Integer;
begin
  try
    GravarLog('NFCom_Assinar', logNormal);

    NFComDM.Travar;
    try
      try
        NFComDM.ACBrNFCom1.NotasFiscais.Assinar;
      except
        on E: EACBrNFComException do
          Result := SetRetorno(ErrAssinarNFCom, ConverterStringSaida(E.Message));
      end;

      Result := SetRetornoNFComCarregadas(NFComDM.ACBrNFCom1.NotasFiscais.Count);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.Validar: Integer;
begin
  try
    GravarLog('NFCom_Validar', logNormal);

    NFComDM.Travar;
    try
      try
        NFComDM.ACBrNFCom1.NotasFiscais.Validar;
        Result := SetRetornoNFComCarregadas(NFComDM.ACBrNFCom1.NotasFiscais.Count);
      except
        on E: EACBrNFComException do
          Result := SetRetorno(ErrValidacaoNFCom, ConverterStringSaida(E.Message));
      end;
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.ValidarRegrasdeNegocios(const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
var
  Erros: AnsiString;
begin
  try
    GravarLog('NFCom_ValidarRegrasdeNegocios', logNormal);

    NFComDM.Travar;
    try
      Erros := '';
      NFComDM.ACBrNFCom1.NotasFiscais.ValidarRegrasdeNegocios(Erros);
      Erros := ConverterStringSaida(Erros);
      MoverStringParaPChar(Erros, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Erros);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.VerificarAssinatura(const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
var
  Erros: AnsiString;
begin
  try
    GravarLog('NFCom_VerificarAssinatura', logNormal);

    NFComDM.Travar;
    try
      Erros := '';
      NFComDM.ACBrNFCom1.NotasFiscais.VerificarAssinatura(Erros);
      Erros := ConverterStringSaida(Erros);
      MoverStringParaPChar(Erros, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Erros);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.ObterCertificados(const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
var
  Resposta: AnsiString;
begin
  try
    GravarLog('NFCom_ObterCertificados', logNormal);

    NFComDM.Travar;
    try
      Resposta := '';
      Resposta := ObterCerticados(NFComDM.ACBrNFCom1.SSL);
      Resposta := ConverterStringSaida(Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.GetPath(ATipo: Integer; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
var
  Resposta: AnsiString;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_GetPath(' + IntToStr(ATipo) + ' )', logCompleto, True)
    else
      GravarLog('NFCom_GetPath', logNormal);

    NFComDM.Travar;
    try
      with NFComDM do
      begin
        Resposta := EmptyStr;
        case ATipo of
        0: Resposta := ACBrNFCom1.Configuracoes.Arquivos.GetPathNFCom();
        1: Resposta := ACBrNFCom1.Configuracoes.Arquivos.GetPathEvento(teCancelamento);
        end;

        Resposta := ConverterStringSaida(Resposta);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.GetPathEvento(ACodEvento: PAnsiChar;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  CodEvento: String;
  Resposta: AnsiString;
  ok: Boolean;
begin
  try
    CodEvento := ConverterStringEntrada(ACodEvento);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_GetPathEvento(' + CodEvento + ' )', logCompleto, True)
    else
      GravarLog('NFCom_GetPathEvento', logNormal);

    NFComDM.Travar;
    try
      with NFComDM do
      begin
        Resposta := EmptyStr;
        Resposta := ACBrNFCom1.Configuracoes.Arquivos.GetPathEvento(StrToTpEventoNFCom(ok, CodEvento));
        Resposta := ConverterStringSaida(Resposta);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.StatusServico(const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
var
  Resp: TStatusServicoResposta;
  Resposta: AnsiString;
begin
  try
    GravarLog('NFCom_StatusServico', logNormal);

    NFComDM.Travar;
    Resp := TStatusServicoResposta.Create(Config.TipoResposta, Config.CodResposta);
    try
      with NFComDM.ACBrNFCom1 do
      begin
        WebServices.StatusServico.Executar;

        Resp.Processar(NFComDM.ACBrNFCom1);
        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      Resp.Free;
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: EACBrDFeExceptionTimeOut do
       Result := SetRetorno(ErrTimeOut, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.Enviar(AImprimir: Boolean; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
var
  Resposta: AnsiString;
  RespEnvio: TEnvioResposta;
  ImpResp: TLibImpressaoResposta;
  i, ImpCount: Integer;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_Enviar(' + BoolToStr(AImprimir, ', Imprimir', '') + ' )', logCompleto, True)
    else
      GravarLog('NFCom_Enviar', logNormal);

    NFComDM.Travar;
    try
      with NFComDM.ACBrNFCom1 do
      begin
        if NotasFiscais.Count <= 0 then
          raise EACBrLibException.Create(ErrEnvio, 'ERRO: Nenhuma NFCom adicionada ao Lote');

        if NotasFiscais.Count > 50 then
           raise EACBrLibException.Create(ErrEnvio, 'ERRO: Conjunto de NFCom transmitidas (máximo de 50 NFCom)' +
                                                    ' excedido. Quantidade atual: ' + IntToStr(NotasFiscais.Count));

        GravarLog('NFCom_Enviar, Limpando Resp', logParanoico);
        Resposta := '';
        WebServices.Enviar.Clear;

        GravarLog('NFCom_Enviar, Assinando', logCompleto);
        NotasFiscais.Assinar;

        try
          GravarLog('NFCom_Enviar, Validando', logCompleto);
          NotasFiscais.Validar;
        except
          on E: EACBrNFComException do
          begin
            Result := SetRetorno(ErrValidacaoNFCom, ConverterStringSaida(E.Message));
            Exit;
          end;
        end;

        GravarLog('NFCom_Enviar, Enviando', logCompleto);
        WebServices.Enviar.Executar;

        RespEnvio := TEnvioResposta.Create(Config.TipoResposta, Config.CodResposta);
        try
          GravarLog('NFCom_Enviar, Proces.Resp Enviar', logParanoico);
          RespEnvio.Processar(NFComDM.ACBrNFCom1);
          Resposta := RespEnvio.Gerar;
        finally
          RespEnvio.Free;
        end;

        if AImprimir then
          begin
            try
              NFComDM.ConfigurarImpressao;

              ImpCount := 0;
              for i := 0 to NotasFiscais.Count - 1 do
              begin
                if NotasFiscais.Items[i].Confirmada then
                  begin
                    GravarLog('NFCom_Enviar, Imprimindo NFCom['+IntToStr(i+1)+'], '+NotasFiscais.Items[i].NFCom.infNFCom.ID, logNormal);
                    NotasFiscais.Items[i].Imprimir;
                    Inc(ImpCount);
                  end;
              end;

              if ImpCount > 0 then
                begin
                  ImpResp := TLibImpressaoResposta.Create(ImpCount, Config.TipoResposta, Config.CodResposta);
                  try
                    GravarLog('NFCom_Enviar, Proces.Resp Impressao', logParanoico);
                    Resposta := Resposta + sLineBreak + ImpResp.Gerar;
                  finally
                    ImpResp.Free;
                  end;
                end;
            finally
              NFComDM.FinalizarImpressao;
            end;
          end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: EACBrDFeExceptionTimeOut do
      Result := SetRetorno(ErrTimeOut, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.Consultar(const eChaveOuNFCom: PAnsiChar;
  AExtrairEventos: Boolean; const sResposta: PAnsiChar; var esTamanho: Integer
  ): Integer;
var
  EhArquivo: Boolean;
  ChaveOuNFCom: String;
  Resp: TConsultaNFComResposta;
  Resposta: AnsiString;
begin
  try
    ChaveOuNFCom := ConverterStringSaida(eChaveOuNFCom);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_Consultar(' + ChaveOuNFCom + ',' + BoolToStr(AExtrairEventos, True) + ' )', logCompleto, True)
    else
      GravarLog('NFCom_Consultar', logNormal);

    NFComDM.Travar;

    EhArquivo := StringEhArquivo(ChaveOuNFCom);

    if EhArquivo and not ValidarChave(ChaveOuNFCom) then
      begin
        VerificarArquivoExiste(ChaveOuNFCom);
        NFComDM.ACBrNFCom1.NotasFiscais.LoadFromFile(ChaveOuNFCom);
      end;

    if NFComDM.ACBrNFCom1.NotasFiscais.Count = 0 then
      begin
        if ValidarChave(ChaveOuNFCom) then
          NFComDM.ACBrNFCom1.WebServices.Consulta.NFComChave := ChaveOuNFCom
        else
          raise EACBrLibException.Create(ErrChaveNFCom, Format(SErrChaveInvalida, [ChaveOuNFCom]));
      end
    else
    NFComDM.ACBrNFCom1.WebServices.Consulta.NFComChave := NFComDM.ACBrNFCom1.NotasFiscais.Items[NFComDM.ACBrNFCom1.NotasFiscais.Count - 1].NumID;

    NFComDM.ACBrNFCom1.WebServices.Consulta.ExtrairEventos := AExtrairEventos;
    Resp := TConsultaNFComResposta.Create(Config.TipoResposta, Config.CodResposta);

    try
      with NFComDM.ACBrNFCom1 do
      begin
        WebServices.Consulta.Executar;
        Resp.Processar(NFComDM.ACBrNFCom1);

        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      Resp.Free;
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: EACBrDFeExceptionTimeOut do
       Result := SetRetorno(ErrTimeOut, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.Cancelar(const eChave, eJustificativa,
  eCNPJCPF: PAnsiChar; ALote: Integer; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
var
  AChave, AJustificativa, ACNPJCPF: String;
  Resp: TCancelamentoResposta;
  Resposta: AnsiString;
begin
  try
    AChave := ConverterStringEntrada(eChave);
    AJustificativa := ConverterStringEntrada(eJustificativa);
    ACNPJCPF := ConverterStringEntrada(eCNPJCPF);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_Cancelar(' + AChave + ',' + AJustificativa + ',' + ACNPJCPF + ',' + IntToStr(ALote) + ' )', logCompleto, True)
    else
      GravarLog('NFCom_Cancelar', logNormal);

    NFComDM.Travar;
    try
      if not ValidarChave(AChave) then
        raise EACBrLibException.Create(ErrChaveNFCom, Format(SErrChaveInvalida, [AChave]))
      else
        NFComDM.ACBrNFCom1.WebServices.Consulta.NFComChave := AChave;

      if not NFComDM.ACBrNFCom1.WebServices.Consulta.Executar then
        raise EACBrLibException.Create(ErrConsulta, NFComDM.ACBrNFCom1.WebServices.Consulta.Msg);

      NFComDM.ACBrNFCom1.EventoNFCom.Evento.Clear;
      with NFComDM.ACBrNFCom1.EventoNFCom.Evento.New do
      begin
        InfEvento.CNPJ := ACNPJCPF;
        if Trim(Infevento.CNPJ) = '' then
          InfEvento.CNPJ := Copy(OnlyNumber(NFComDM.ACBrNFCom1.WebServices.Consulta.NFComChave), 7, 14)
        else
        begin
          if not ValidarCNPJouCPF(ACNPJCPF) then
            raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));
        end;

        InfEvento.nSeqEvento := 1;
        InfEvento.tpAmb := TACBrTipoAmbiente(NFComDM.ACBrNFCom1.Configuracoes.WebServices.Ambiente);
        InfEvento.cOrgao := StrToIntDef(Copy(OnlyNumber(NFComDM.ACBrNFCom1.WebServices.Consulta.NFComChave), 1, 2), 0);
        InfEvento.dhEvento := Now;
        InfEvento.tpEvento := teCancelamento;
        InfEvento.chNFCom := NFComDM.ACBrNFCom1.WebServices.Consulta.NFComChave;
        InfEvento.detEvento.nProt := NFComDM.ACBrNFCom1.WebServices.Consulta.Protocolo;
        InfEvento.detEvento.xJust := AJustificativa;
      end;

      if (ALote = 0) then
        ALote := 1;

      NFComDM.ACBrNFCom1.WebServices.EnvEvento.idLote := ALote;
      NFComDM.ACBrNFCom1.WebServices.EnvEvento.Executar;

      Resp := TCancelamentoResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(NFComDM.ACBrNFCom1);
        Resposta := Resp.Gerar;
      finally
        Resp.Free;
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: EACBrDFeExceptionTimeOut do
       Result := SetRetorno(ErrTimeOut, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.EnviarEvento(idLote: Integer;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  i, j: Integer;
  Resp: TEventoResposta;
  Resposta, chNFCom: String;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_EnviarEvento(' + IntToStr(idLote) + ' )', logCompleto, True)
    else
      GravarLog('NFCom_EnviarEvento', logNormal);

    NFComDM.Travar;
    try
      with NFComDM.ACBrNFCom1 do
      begin
        if EventoNFCom.Evento.Count = 0 then
          raise EACBrLibException.Create(ErrEnvioEvento, 'ERRO: Nenhum Evento adicionado ao Lote');

        if EventoNFCom.Evento.Count > 20 then
          raise EACBrLibException.Create(ErrEnvioEvento,  'ERRO: Conjunto de Eventos transmitidos (máximo de 20) ' +
                                                          'excedido. Quantidade atual: ' + IntToStr(EventoNFCom.Evento.Count));

        {Atribuir nSeqEvento, CNPJ, Chave e/ou Protocolo quando não especificar}
        for i := 0 to EventoNFCom.Evento.Count - 1 do
        begin
          if EventoNFCom.Evento.Items[i].InfEvento.nSeqEvento = 0 then
            EventoNFCom.Evento.Items[i].InfEvento.nSeqEvento := 1;

          EventoNFCom.Evento.Items[i].InfEvento.tpAmb := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);

          if NotasFiscais.Count > 0 then
          begin
           chNFCom := OnlyNumber(EventoNFCom.Evento.Items[i].InfEvento.chNFCom);

           // Se tem a chave da NFCom no Evento, procure por ela nas notas carregadas //
            if NaoEstaVazio(chNFCom) then
            begin
              for j := 0 to NotasFiscais.Count - 1 do
              begin
                if chNFCom = NotasFiscais.Items[j].NumID then
                 Break;
              end;

              if j = NotasFiscais.Count then
                raise EACBrLibException.Create(ErrEnvioEvento, 'Não existe NFCom com a chave [' + chNFCom + '] carregada');
            end
            else
             j := 0;

            if trim(EventoNFCom.Evento.Items[i].InfEvento.CNPJ) = '' then
              EventoNFCom.Evento.Items[i].InfEvento.CNPJ := NotasFiscais.Items[j].NFCom.Emit.CNPJ;

            if chNFCom = '' then
              EventoNFCom.Evento.Items[i].InfEvento.chNFCom := NotasFiscais.Items[j].NumID;

            if Trim(EventoNFCom.Evento.Items[i].InfEvento.detEvento.nProt) = '' then
            begin
              if EventoNFCom.Evento.Items[i].InfEvento.tpEvento = teCancelamento then
              begin
                EventoNFCom.Evento.Items[i].InfEvento.detEvento.nProt := NotasFiscais.Items[j].NFCom.procNFCom.nProt;

                if Trim(EventoNFCom.Evento.Items[i].infEvento.detEvento.nProt) = '' then
                begin
                  WebServices.Consulta.NFComChave := EventoNFCom.Evento.Items[i].InfEvento.chNFCom;

                  if not WebServices.Consulta.Executar then
                    raise EACBrLibException.Create(ErrEnvioEvento, WebServices.Consulta.Msg);

                  EventoNFCom.Evento.Items[i].InfEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
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
        Resp.Processar(NFComDM.ACBrNFCom1);
        Resposta := Resp.Gerar;
      finally
        Resp.Free;
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: EACBrDFeExceptionTimeOut do
       Result := SetRetorno(ErrTimeOut, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.EnviarEmail(const ePara, eXmlNFCom: PAnsiChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar
  ): Integer;
var
  Resposta, APara, AXmlNFCom, AAssunto, ACC, AAnexos, AMensagem: String;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo, LXmlCarregado: Boolean;
  Resp: TLibNFComResposta;
  LNFComEnviar : TACBrNFCom;
begin
  try
    APara := ConverterStringEntrada(ePara);
    AXmlNFCom := ConverterStringEntrada(eXmlNFCom);
    AAssunto := ConverterStringEntrada(eAssunto);
    ACC := ConverterStringEntrada(eCC);
    AAnexos := ConverterStringEntrada(eAnexos);
    AMensagem := ConverterStringEntrada(eMensagem);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_EnviarEmail(' + APara + ',' + AXmlNFCom + ',' + BoolToStr(AEnviaPDF, 'PDF', '') + ',' + AAssunto + ',' + ACC + ',' + AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      GravarLog('NFCom_EnviarEmail', logNormal);

    NFComDM.Travar;
    LNFComEnviar := TACBrNFCom.Create(NFComDM.ACBrNFCom1);

    try
      if (AEnviaPDF) then
        NFComDM.ConfigurarImpressao('', True);

      LNFComEnviar.MAIL := NFComDM.ACBrMail1;
      LNFComEnviar.DANFCom := NFComDM.ACBrNFCom1.DANFCom;

      EhArquivo := StringEhArquivo(AXmlNFCom);

      if EhArquivo then
        VerificarArquivoExiste(AXmlNFCom);

      if EhArquivo then
        LXmlCarregado := LNFComEnviar.NotasFiscais.LoadFromFile(AXmlNFCom)
        else
        LXmlCarregado := LNFComEnviar.NotasFiscais.LoadFromString(AXmlNFCom);

      if not LXmlCarregado then
        raise EACBrLibException.Create(ErrEnvio, 'Erro Caminho ou conteudo do XML inválido, não foi possível fazer a leitura do conteúdo do XML');

      if LNFComEnviar.NotasFiscais.Count = 0 then
        raise EACBrLibException.Create(ErrEnvio, Format(SInfNFComCarregadas, [LNFComEnviar.NotasFiscais.Count]))
        else
        begin
          slMensagemEmail := TStringList.Create;
          slCC := TStringList.Create;
          slAnexos := TStringList.Create;

          Resp := TLibNFComResposta.Create('EnviaEmail', Config.TipoResposta, Config.CodResposta);
          try
            slMensagemEmail.DelimitedText := sLineBreak;
            slMensagemEmail.Text := StringReplace(AMensagem, ';', sLineBreak, [rfReplaceAll]);

            slCC.DelimitedText := sLineBreak;
            slCC.Text := StringReplace(ACC, ';', sLineBreak, [rfReplaceAll]);

            slAnexos.DelimitedText := sLineBreak;
            slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

            LNFComEnviar.NotasFiscais[0].EnviarEmail(
              APara,
              AAssunto,
              slMensagemEmail,
              AEnviaPDF, // Enviar PDF junto
              slCC,      // Lista com emails que serão enviado cópias - TStrings
              slAnexos); // Lista de slAnexos - TStrings

            Resp.Msg := 'Email enviado com sucesso';
            Resposta := Resp.Gerar;

            Result := SetRetorno(ErrOK, Resposta);
          finally
            Resp.Free;
            slCC.Free;
            slAnexos.Free;
            slMensagemEmail.Free;
            if (AEnviaPDF) then NFComDM.FinalizarImpressao;
          end;
        end;
    finally
      NFComDM.Destravar;
      LNFComEnviar.Free;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.EnviarEmailEvento(const ePara, eXmlEvento,
  eXmlNFCom: PAnsiChar; const AEnviaPDF: Boolean; const eAssunto, eCC,
  eAnexos, eMensagem: PAnsiChar): Integer;
var
  APara, AXmlEvento, AXmlNFCom, AAssunto, ACC, AAnexos, AMensagem, ArqPDF: String;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: Boolean;
  Resposta: TLibNFComResposta;
begin
  try
    APara := ConverterStringEntrada(ePara);
    AXmlEvento := ConverterStringEntrada(eXmlEvento);
    AXmlNFCom := ConverterStringEntrada(eXmlNFCom);
    AAssunto := ConverterStringEntrada(eAssunto);
    ACC := ConverterStringEntrada(eCC);
    AAnexos := ConverterStringEntrada(eAnexos);
    AMensagem := ConverterStringEntrada(eMensagem);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_EnviarEmailEvento(' + APara + ',' + AXmlEvento + ',' + AXmlNFCom + ',' + BoolToStr(AEnviaPDF, 'PDF', '') + ',' + AAssunto + ',' + ACC + ',' + AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      GravarLog('NFCom_EnviarEmailEvento', logNormal);

    NFComDM.Travar;
    try
      with NFComDM.ACBrNFCom1 do
      begin
        EventoNFCom.Evento.Clear;
        NotasFiscais.Clear;

        EhArquivo := StringEhArquivo(AXmlEvento);

        if EhArquivo then
          VerificarArquivoExiste(AXmlEvento);

        if EhArquivo then
          EventoNFCom.LerXML(AXmlEvento)
        else
         EventoNFCom.LerXMLFromString(AXmlEvento);

        EhArquivo := StringEhArquivo(AXmlNFCom);

        if EhArquivo then
          VerificarArquivoExiste(AXmlNFCom);

        if EhArquivo then
          NotasFiscais.LoadFromFile(AXmlNFCom)
        else
          NotasFiscais.LoadFromString(AXmlNFCom);

        if EventoNFCom.Evento.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfEventosCarregados, [EventoNFCom.Evento.Count]))
          else
        begin
          slMensagemEmail := TStringList.Create;
          slCC := TStringList.Create;
          slAnexos := TStringList.Create;
          Resposta := TLibNFComResposta.Create('EnviaEmail', Config.TipoResposta, Config.CodResposta);

          try
            if AEnviaPDF then
            begin
              try
                NFComDM.ConfigurarImpressao('', True);
                ImprimirEventoPDF;

                ArqPDF := OnlyNumber(EventoNFCom.Evento[0].InfEvento.id);
                ArqPDF := PathWithDelim(DANFCom.PathPDF) + ArqPDF + '-procEventoNFCom.pdf';
              except
                raise EACBrLibException.Create(ErrRetorno, 'Erro ao criar o arquivo PDF');
              end;
            end;

            with MAIL do
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
                NFComDM.ACBrNFCom1.EnviarEmail(
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
            if (AEnviaPDF) then NFComDM.FinalizarImpressao;
          end;
        end;
      end;
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.Imprimir(const cImpressora: PAnsiChar;
  nNumCopias: Integer; bMostrarPreview: PAnsiChar): Integer;
var
  Resposta: TLibImpressaoResposta;
  Impressora, MostrarPreview: String;
begin
  try
    Impressora := ConverterStringEntrada(cImpressora);
    MostrarPreview := ConverterStringEntrada(bMostrarPreview);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_Imprimir('  + Impressora + ',' + IntToStr(nNumCopias) + ',' + MostrarPreview + ')', logCompleto, True)
    else
      GravarLog('NFCom_Imprimir', logNormal);

    NFComDM.Travar;
    Resposta := TLibImpressaoResposta.Create(NFComDM.ACBrNFCom1.NotasFiscais.Count, Config.TipoResposta, Config.CodResposta);

    try
      NFComDM.ConfigurarImpressao(Impressora, False, MostrarPreview);
      if nNumCopias > 0 then
        NFComDM.ACBrNFCom1.DANFCom.NumCopias := nNumCopias;

      NFComDM.ACBrNFCom1.NotasFiscais.Imprimir;
      Result := SetRetorno(ErrOK, Resposta.Gerar);
    finally
      NFComDM.FinalizarImpressao;
      Resposta.Free;
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.ImprimirPDF: Integer;
var
  Resposta: TLibImpressaoResposta;
begin
  try
    GravarLog('NFCom_ImprimirPDF', logNormal);

    NFComDM.Travar;
    try
      Resposta := TLibImpressaoResposta.Create(NFComDM.ACBrNFCom1.NotasFiscais.Count, Config.TipoResposta, Config.CodResposta);
      try
        NFComDM.ConfigurarImpressao('', True);
        try
          NFComDM.ACBrNFCom1.NotasFiscais.ImprimirPDF;
          Resposta.Msg := NFComDM.ACBrNFCom1.DANFCom.ArquivoPDF;
          Result := SetRetorno(ErrOK, Resposta.Gerar);
        finally
          NFComDM.FinalizarImpressao;
        end;
      finally
        Resposta.Free;
      end;
    finally
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.SalvarPDF(const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
var
  AStream: TMemoryStream;
  Resposta: AnsiString;
begin
  try
    GravarLog('NFCom_SalvarPDF', logNormal);

    NFComDM.Travar;
    AStream := TMemoryStream.Create;
    try
      NFComDM.ConfigurarImpressao('', True);

      NFComDM.ACBrNFCom1.NotasFiscais.ImprimirPDF(AStream);
      Resposta := StreamToBase64(AStream);

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFComDM.FinalizarImpressao;
      AStream.Free;
      NFComDM.Free;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.ImprimirEvento(const eArquivoXmlNFCom,
  eArquivoXmlEvento: PAnsiChar): Integer;
var
  EhArquivo: Boolean;
  AArquivoXmlNFCom: String;
  AArquivoXmlEvento: String;
  Resposta: TLibImpressaoResposta;
begin
  try
    AArquivoXmlNFCom := ConverterStringEntrada(eArquivoXmlNFCom);
    AArquivoXmlEvento := ConverterStringEntrada(eArquivoXmlEvento);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_ImprimirEvento(' + AArquivoXmlNFCom + ',' + AArquivoXmlEvento + ' )', logCompleto, True)
    else
      GravarLog('NFCom_ImprimirEvento', logNormal);

    NFComDM.Travar;
    Resposta := TLibImpressaoResposta.Create(NFComDM.ACBrNFCom1.EventoNFCom.Evento.Count, Config.TipoResposta, Config.CodResposta);

    try
      EhArquivo := StringEhArquivo(AArquivoXmlNFCom);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlNFCom);

      if EhArquivo then
        NFComDM.ACBrNFCom1.NotasFiscais.LoadFromFile(AArquivoXmlNFCom)
      else
        NFComDM.ACBrNFCom1.NotasFiscais.LoadFromString(AArquivoXmlNFCom);

      EhArquivo := StringEhArquivo(AArquivoXmlEvento);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlEvento);

      if EhArquivo then
        NFComDM.ACBrNFCom1.EventoNFCom.LerXML(AArquivoXmlEvento)
      else
        NFComDM.ACBrNFCom1.EventoNFCom.LerXMLFromString(AArquivoXmlEvento);

      NFComDM.ConfigurarImpressao;
      NFComDM.ACBrNFCom1.ImprimirEvento;

      Result := SetRetorno(ErrOK, Resposta.Gerar);
    finally
      NFComDM.FinalizarImpressao;
      Resposta.Free;
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.ImprimirEventoPDF(const eArquivoXmlNFCom,
  eArquivoXmlEvento: PAnsiChar): Integer;
var
  EhArquivo: Boolean;
  AArquivoXmlNFCom: String;
  AArquivoXmlEvento: String;
  Resposta: TLibImpressaoResposta;
begin
  try
    AArquivoXmlNFCom := ConverterStringEntrada(eArquivoXmlNFCom);
    AArquivoXmlEvento := ConverterStringEntrada(eArquivoXmlEvento);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_ImprimirEventoPDF(' + AArquivoXmlNFCom + ',' + AArquivoXmlEvento + ' )', logCompleto, True)
    else
      GravarLog('NFCom_ImprimirEventoPDF', logNormal);

    NFComDM.Travar;
    Resposta := TLibImpressaoResposta.Create(NFComDM.ACBrNFCom1.EventoNFCom.Evento.Count, Config.TipoResposta, Config.CodResposta);

    try
      EhArquivo := StringEhArquivo(AArquivoXmlNFCom);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlNFCom);

      if EhArquivo then
        NFComDM.ACBrNFCom1.NotasFiscais.LoadFromFile(AArquivoXmlNFCom)
      else
        NFComDM.ACBrNFCom1.NotasFiscais.LoadFromString(AArquivoXmlNFCom);

      EhArquivo := StringEhArquivo(AArquivoXmlEvento);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlEvento);

      if EhArquivo then
        NFComDM.ACBrNFCom1.EventoNFCom.LerXML(AArquivoXmlEvento)
      else
        NFComDM.ACBrNFCom1.EventoNFCom.LerXMLFromString(AArquivoXmlEvento);

      NFComDM.ConfigurarImpressao('', True);
      NFComDM.ACBrNFCom1.ImprimirEventoPDF;

      Resposta.Msg := NFComDM.ACBrNFCom1.DANFCom.ArquivoPDF;
      Result := SetRetorno(ErrOK, Resposta.Gerar);
    finally
      NFComDM.FinalizarImpressao;
      Resposta.Free;
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibNFCom.SalvarEventoPDF(const eArquivoXmlNFCom,
  eArquivoXmlEvento, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
var
  EhArquivo: Boolean;
  AArquivoXmlNFCom: String;
  AArquivoXmlEvento: String;
  AStream: TMemoryStream;
  Resposta: AnsiString;
begin
  try
    AArquivoXmlNFCom := ConverterStringEntrada(eArquivoXmlNFCom);
    AArquivoXmlEvento := ConverterStringEntrada(eArquivoXmlEvento);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFCom_SalvarEventoPDF(' + AArquivoXmlNFCom + ',' + AArquivoXmlEvento + ' )', logCompleto, True)
    else
      GravarLog('NFCom_SalvarEventoPDF', logNormal);

    NFComDM.Travar;
    AStream := TMemoryStream.Create;

    try
      EhArquivo := StringEhArquivo(AArquivoXmlNFCom);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlNFCom);

      if EhArquivo then
        NFComDM.ACBrNFCom1.NotasFiscais.LoadFromFile(AArquivoXmlNFCom)
      else
        NFComDM.ACBrNFCom1.NotasFiscais.LoadFromString(AArquivoXmlNFCom);

      EhArquivo := StringEhArquivo(AArquivoXmlEvento);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlEvento);

      if EhArquivo then
        NFComDM.ACBrNFCom1.EventoNFCom.LerXML(AArquivoXmlEvento)
      else
        NFComDM.ACBrNFCom1.EventoNFCom.LerXMLFromString(AArquivoXmlEvento);

      NFComDM.ConfigurarImpressao('', True);
      NFComDM.ACBrNFCom1.DANFCom.ImprimirEVENTOPDF(AStream);

      Resposta := StreamToBase64(AStream);

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      NFComDM.FinalizarImpressao;
      AStream.Free;
      NFComDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

end.

