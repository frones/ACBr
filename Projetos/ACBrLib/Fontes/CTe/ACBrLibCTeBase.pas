{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrLibCTeBase;

interface

uses
  Classes, SysUtils, Forms,
  ACBrLibComum, ACBrLibCTeDataModule;

type

  { TACBrLibCTe }

  TACBrLibCTe = class(TACBrLib)
  private
    FCTeDM: TLibCTeDM;

    function SetRetornoCTesCarregados(const NumCTe: Integer): Integer;
    function SetRetornoEventoCarregados(const NumEventos: Integer): Integer;

  protected
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');      override;
    procedure Executar; override;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property CTeDM: TLibCTeDM read FCTeDM;

    function CarregarXML(const eArquivoOuXML: PChar): longint;
    function CarregarINI(const eArquivoOuINI: PChar): longint;
    function ObterXml(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function GravarXml(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
    function ObterIni(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function GravarIni(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
    function CarregarEventoXML(const eArquivoOuXML: PChar): longint;
    function CarregarEventoINI(const eArquivoOuINI: PChar): longint;
    function LimparLista: longint;
    function LimparListaEventos: longint;
    function Assinar: longint;
    function Validar: longint;
    function ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
    function VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
    function GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi: longint;
                        AEmissao, ACNPJCPF: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
    function GetPath(ATipo: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function GetPathEvento(ACodEvento: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
    function Consultar(const eChaveOuCTe: PChar; AExtrairEventos: Boolean;
                       const sResposta: PChar; var esTamanho: longint): longint;
    function Inutilizar(const ACNPJ, AJustificativa: PChar; Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
                        const sResposta: PChar; var esTamanho: longint): longint;
    function Enviar(ALote: Integer; AImprimir, ASincrono: Boolean;
                    const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarRecibo(ARecibo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function Cancelar(const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer;
                      const sResposta: PChar; var esTamanho: longint): longint;
    function EnviarEvento(idLote: Integer; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultaCadastro(cUF, nDocumento: PChar; nIE: boolean;
                              const sResposta: PChar; var esTamanho: longint): longint;
    function DistribuicaoDFePorUltNSU(const AcUFAutor: integer; eCNPJCPF, eultNSU: PChar;
                                      const sResposta: PChar; var esTamanho: longint): longint;
    function DistribuicaoDFe(const AcUFAutor: integer;
                             eCNPJCPF, eultNSU, eArquivoOuXML: PChar;
                             const sResposta: PChar;
                             var esTamanho: longint): longint;
    function DistribuicaoDFePorNSU(const AcUFAutor: integer; eCNPJCPF, eNSU: PChar;
                                   const sResposta: PChar; var esTamanho: longint): longint;
    function DistribuicaoDFePorChave(const AcUFAutor: integer; eCNPJCPF, echCTe: PChar;
                                     const sResposta: PChar; var esTamanho: longint): longint;
    function EnviarEmail(const ePara, eChaveCTe: PChar; const AEnviaPDF: Boolean;
                         const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
    function EnviarEmailEvento(const ePara, eChaveEvento, eChaveCTe: PChar;
                               const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
    function Imprimir(const cImpressora: PChar; nNumCopias: Integer; const cProtocolo, bMostrarPreview: PChar): longint;
    function ImprimirPDF: longint;
    function SalvarPDF(const sResposta: PChar; var esTamanho: longint): longint;
    function ImprimirEvento(const eArquivoXmlCTe, eArquivoXmlEvento: PChar): longint;
    function ImprimirEventoPDF(const eArquivoXmlCTe, eArquivoXmlEvento: PChar): longint;
    function SalvarEventoPDF(const eArquivoXmlCTe, eArquivoXmlEvento: PChar;  sResposta: PChar; var esTamanho: longint): longint;
    function ImprimirInutilizacao(const eArquivoXml: PChar): longint;
    function ImprimirInutilizacaoPDF(const eArquivoXml: PChar): longint;
  end;
  

implementation

uses
  StrUtils, ACBrLibConsts, ACBrLibCTeConsts, ACBrLibConfig, ACBrLibResposta,
  ACBrLibConsReciDFe, ACBrLibDistribuicaoDFe, ACBrLibConsultaCadastro,
  ACBrLibCTeConfig, ACBrLibCTeRespostas, ACBrCTe, ACBrMail, ACBrLibCertUtils,
  pcnConversao, pcnAuxiliar, pcteConversaoCTe, blcksock, ACBrDFeUtil,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings;

{ TACBrLibCTe }

constructor TACBrLibCTe.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  TraduzirUltimoRetorno := False;
  FCTeDM := TLibCTeDM.Create(nil);
  FCTeDM.Lib := Self;
end;

destructor TACBrLibCTe.Destroy;
begin
  FCTeDM.Free;

  inherited Destroy;
end;

procedure TACBrLibCTe.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibCTeConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibCTe.Executar;
begin
  inherited Executar;
  FCTeDM.AplicarConfiguracoes;
end;

function TACBrLibCTe.CarregarXML(const eArquivoOuXML: PChar): longint;  
var
  EhArquivo: boolean;
  ArquivoOuXml: Ansistring;
begin
  try
    ArquivoOuXml := ConverterAnsiParaUTF8(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_CarregarXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      GravarLog('CTE_CarregarXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    CTeDM.Travar;
    try
      if EhArquivo then
        CTeDM.ACBrCTe1.Conhecimentos.LoadFromFile(ArquivoOuXml)
      else
        CTeDM.ACBrCTe1.Conhecimentos.LoadFromString(ArquivoOuXml);

      Result := SetRetornoCTesCarregados(CTeDM.ACBrCTe1.Conhecimentos.Count);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.CarregarINI(const eArquivoOuINI: PChar): longint;  
var
  ArquivoOuINI: Ansistring;
begin
  try
    ArquivoOuINI := ConverterAnsiParaUTF8(eArquivoOuINI);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_CarregarINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      GravarLog('CTE_CarregarINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    CTeDM.Travar;
    try
      CTeDM.ACBrCTe1.Conhecimentos.LoadFromIni(ArquivoOuINI);
      Result := SetRetornoCTesCarregados(CTeDM.ACBrCTe1.Conhecimentos.Count);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.ObterXml(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;    
Var
  Resposta: String;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_ObterXml(' + IntToStr(AIndex) + ' )', logCompleto, True)
    else
      GravarLog('CTE_ObterXml', logNormal);

    CTeDM.Travar;
    try
      if (AIndex >= CTeDM.ACBrCTe1.Conhecimentos.Count) and (CTeDM.ACBrCTe1.Conhecimentos.Count < 1) then
        raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if EstaVazio(CTeDM.ACBrCTe1.Conhecimentos.Items[AIndex].XMLOriginal) then
        CTeDM.ACBrCTe1.Conhecimentos.Items[AIndex].GerarXML;

      Resposta := CTeDM.ACBrCTe1.Conhecimentos.Items[AIndex].XMLOriginal;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.GravarXml(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;    
Var
  ANomeArquivo, APathArquivo: AnsiString;
begin
  try
    ANomeArquivo := ConverterAnsiParaUTF8(eNomeArquivo);
    APathArquivo := ConverterAnsiParaUTF8(ePathArquivo);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_GravarXml(' + IntToStr(AIndex) + ',' + ANomeArquivo + ',' + APathArquivo + ' )', logCompleto, True)
    else
      GravarLog('CTE_GravarXml', logNormal);

    CTeDM.Travar;
    try
      if (AIndex >= CTeDM.ACBrCTe1.Conhecimentos.Count) and (CTeDM.ACBrCTe1.Conhecimentos.Count < 1) then
        raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if CTeDM.ACBrCTe1.Conhecimentos.Items[AIndex].GravarXML(ANomeArquivo, APathArquivo) then
        Result := SetRetorno(ErrOK)
      else
        Result := SetRetorno(ErrGerarXml);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.ObterIni(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;    
Var
  Resposta: Ansistring;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_ObterIni(' + IntToStr(AIndex) + ' )', logCompleto, True)
    else
      GravarLog('CTE_ObterIni', logNormal);

    CTeDM.Travar;
    try
      if (AIndex >= CTeDM.ACBrCTe1.Conhecimentos.Count) and (CTeDM.ACBrCTe1.Conhecimentos.Count < 1) then
        raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if EstaVazio(CTeDM.ACBrCTe1.Conhecimentos.Items[AIndex].XMLOriginal) then
        CTeDM.ACBrCTe1.Conhecimentos.Items[AIndex].GerarXML;

      Resposta := CTeDM.ACBrCTe1.Conhecimentos.Items[AIndex].GerarCTeIni;
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.GravarIni(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;    
Var
  ACTeIni, ANomeArquivo, APathArquivo: AnsiString;
begin
  try
    ANomeArquivo := ConverterAnsiParaUTF8(eNomeArquivo);
    APathArquivo := ConverterAnsiParaUTF8(ePathArquivo);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_GravarIni(' + IntToStr(AIndex) + ',' + ANomeArquivo + ',' + APathArquivo + ' )', logCompleto, True)
    else
      GravarLog('CTE_GravarIni', logNormal);

    CTeDM.Travar;
    try
      if (AIndex >= CTeDM.ACBrCTe1.Conhecimentos.Count) and (CTeDM.ACBrCTe1.Conhecimentos.Count < 1) then
        raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      ANomeArquivo := ExtractFileName(ANomeArquivo);

      if EstaVazio(ANomeArquivo) then
        raise EACBrLibException.Create(ErrExecutandoMetodo, 'Nome de arquivo não informado');

      if EstaVazio(APathArquivo) then
        APathArquivo := ExtractFilePath(ANomeArquivo);
      if EstaVazio(APathArquivo) then
        APathArquivo := CTeDM.ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

      APathArquivo := PathWithDelim(APathArquivo);

      if EstaVazio(CTeDM.ACBrCTe1.Conhecimentos.Items[AIndex].XMLOriginal) then
        CTeDM.ACBrCTe1.Conhecimentos.Items[AIndex].GerarXML;

      ACTeIni := CTeDM.ACBrCTe1.Conhecimentos.Items[AIndex].GerarCTeIni;

      if not DirectoryExists(APathArquivo) then
        ForceDirectories(APathArquivo);

      WriteToTXT(APathArquivo + ANomeArquivo, ACTeIni, False, False);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.CarregarEventoXML(const eArquivoOuXML: PChar): longint;  
var
  EhArquivo: boolean;
  ArquivoOuXml: Ansistring;
begin
  try
    ArquivoOuXml := ConverterAnsiParaUTF8(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_CarregarEventoXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      GravarLog('CTE_CarregarEventoXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    CTeDM.Travar;
    try
      if EhArquivo then
        CTeDM.ACBrCTe1.EventoCTe.LerXML(ArquivoOuXml)
      else
        CTeDM.ACBrCTe1.EventoCTe.LerXMLFromString(ArquivoOuXml);

      Result := SetRetornoEventoCarregados(CTeDM.ACBrCTe1.EventoCTe.Evento.Count);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.CarregarEventoINI(const eArquivoOuINI: PChar): longint;  
var
  ArquivoOuINI: Ansistring;
begin
  try
    ArquivoOuINI := ConverterAnsiParaUTF8(eArquivoOuINI);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_CarregarEventoINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      GravarLog('CTE_CarregarEventoINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    CTeDM.Travar;
    try
      CTeDM.ACBrCTe1.EventoCTe.LerFromIni(ArquivoOuINI, False);
      Result := SetRetornoEventoCarregados(CTeDM.ACBrCTe1.EventoCTe.Evento.Count);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.LimparLista: longint;  
begin
  try
    GravarLog('CTE_LimparLista', logNormal);

    CTeDM.Travar;
    try
      CTeDM.ACBrCTe1.Conhecimentos.Clear;
      Result := SetRetornoCTesCarregados(CTeDM.ACBrCTe1.Conhecimentos.Count);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.LimparListaEventos: longint;  
begin
  try
    GravarLog('CTE_LimparListaEventos', logNormal);

    CTeDM.Travar;
    try
      CTeDM.ACBrCTe1.EventoCTe.Evento.Clear;
      Result := SetRetornoEventoCarregados(CTeDM.ACBrCTe1.EventoCTe.Evento.Count);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.Assinar: longint;  
begin
  try
    GravarLog('CTE_Assinar', logNormal);

    CTeDM.Travar;
    try
      try
        CTeDM.ACBrCTe1.Conhecimentos.Assinar;
      except
        on E: EACBrCTeException do
          Result := SetRetorno(ErrAssinarCTe, E.Message);
      end;

      Result := SetRetornoCTesCarregados(CTeDM.ACBrCTe1.Conhecimentos.Count);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.Validar: longint;  
begin
  try
    GravarLog('CTE_Validar', logNormal);

    CTeDM.Travar;
    try
      try
        CTeDM.ACBrCTe1.Conhecimentos.Validar;
        Result := SetRetornoCTesCarregados(CTeDM.ACBrCTe1.Conhecimentos.Count);
      except
        on E: EACBrCTeException do
          Result := SetRetorno(ErrValidacaoCTe, E.Message);
      end;
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;  
Var
  Erros: Ansistring;
begin
  try
    GravarLog('CTE_ValidarRegrasdeNegocios', logNormal);

    CTeDM.Travar;
    try
      Erros := '';
      CTeDM.ACBrCTe1.Conhecimentos.ValidarRegrasdeNegocios(Erros);
      Erros := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Erros), Erros);
      MoverStringParaPChar(Erros, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Erros);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;  
Var
  Erros: Ansistring;
begin
  try
    GravarLog('CTE_VerificarAssinatura', logNormal);

    CTeDM.Travar;
    try
      Erros := '';
      CTeDM.ACBrCTe1.Conhecimentos.VerificarAssinatura(Erros);
      Erros := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Erros), Erros);
      MoverStringParaPChar(Erros, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Erros);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi: longint;
  AEmissao, ACNPJCPF: PChar; const sResposta: PChar; var esTamanho: longint): longint;  
Var
  Resposta, CNPJCPF: Ansistring;
  Emissao: TDateTime;
begin
  try
    Emissao := StrToDate(AEmissao);
    CNPJCPF := ConverterAnsiParaUTF8(ACNPJCPF);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_GerarChave(' + IntToStr(ACodigoUF) + ',' + IntToStr(ACodigoNumerico) + ',' +
                      IntToStr(AModelo)  + ',' + IntToStr(AModelo) + ',' + IntToStr(ASerie) + ',' +
                      IntToStr(ANumero) + ',' + IntToStr(ATpEmi) + ',' + DateToStr(Emissao) + ',' +
                      CNPJCPF + ' )', logCompleto, True)
    else
      GravarLog('CTE_GerarChave', logNormal);

    CTeDM.Travar;
    try
      Resposta := '';
      Resposta := GerarChaveAcesso(ACodigoUF, Emissao, CNPJCPF, ASerie, ANumero, ATpEmi, ACodigoNumerico, AModelo);
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;  
Var
  Resposta: string;
begin
  try
    GravarLog('CTE_ObterCertificados', logNormal);

    CTeDM.Travar;

    try
      Resposta := '';
      Resposta := ObterCerticados(CTeDM.ACBrCTe1.SSL);
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.GetPath(ATipo: longint; const sResposta: PChar; var esTamanho: longint): longint;    
Var
  Resposta: Ansistring;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_GetPath(' + IntToStr(ATipo) + ' )', logCompleto, True)
    else
      GravarLog('CTE_GetPath', logNormal);

    CTeDM.Travar;

    try
      with CTeDM do
      begin
        Resposta := '';

        case ATipo of
          0: Resposta := ACBrCTe1.Configuracoes.Arquivos.GetPathCTe();
          1: Resposta := ACBrCTe1.Configuracoes.Arquivos.GetPathInu();
          2: Resposta := ACBrCTe1.Configuracoes.Arquivos.GetPathEvento(teCCe);
          3: Resposta := ACBrCTe1.Configuracoes.Arquivos.GetPathEvento(teCancelamento);
        end;

        Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.GetPathEvento(ACodEvento: PChar; const sResposta: PChar; var esTamanho: longint): longint;    
Var
  Resposta, CodEvento: Ansistring;
  ok: Boolean;
begin
  try
    CodEvento := ConverterAnsiParaUTF8(ACodEvento);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_GetPathEvento(' + CodEvento +' )', logCompleto, True)
    else
      GravarLog('CTE_GetPathEvento', logNormal);

    CTeDM.Travar;

    try
      with CTeDM do
      begin
        Resposta := '';
        Resposta := ACBrCTe1.Configuracoes.Arquivos.GetPathEvento(StrToTpEventoCTe(ok, CodEvento));
        Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.StatusServico(const sResposta: PChar; var esTamanho: longint): longint;  
var
  Resp: TStatusServicoResposta;
  Resposta: String;
begin
  try
    GravarLog('CTE_StatusServico', logNormal);

    CTeDM.Travar;
    Resp := TStatusServicoResposta.Create(Config.TipoResposta, Config.CodResposta);
    try
      with CTeDM do
      begin
        ACBrCTe1.WebServices.StatusServico.Executar;
        Resp.Processar(CTeDM.ACBrCTe1);
        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end
    finally
      Resp.Free;
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.Consultar(const eChaveOuCTe: PChar; AExtrairEventos: Boolean; const sResposta: PChar; var esTamanho: longint): longint;    
var
  EhArquivo: boolean;
  Resposta, ChaveOuCTe: Ansistring;
  Resp: TConsultaCTeResposta;
begin
  try
    ChaveOuCTe := ConverterAnsiParaUTF8(eChaveOuCTe);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_Consultar(' + ChaveOuCTe + ', ' + BoolToStr(AExtrairEventos, True) + ' )', logCompleto, True)
    else
      GravarLog('CTE_Consultar', logNormal);

    CTeDM.Travar;

    EhArquivo := StringEhArquivo(ChaveOuCTe);

    if EhArquivo and not ValidarChave(ChaveOuCTe) then
    begin
      VerificarArquivoExiste(ChaveOuCTe);
      CTeDM.ACBrCTe1.Conhecimentos.LoadFromFile(ChaveOuCTe);
    end;

    with CTeDM.ACBrCTe1 do
    begin
      if Conhecimentos.Count = 0 then
      begin
        if ValidarChave(ChaveOuCTe) then
          WebServices.Consulta.CTeChave := ChaveOuCTe
        else
          raise EACBrLibException.Create(ErrChaveCTe, Format(SErrChaveInvalida, [ChaveOuCTe]));
      end
      else
        WebServices.Consulta.CTeChave := StringReplace(Conhecimentos.Items[Conhecimentos.Count - 1].CTe.infCTe.ID,
                                                       'CTe', '', [rfIgnoreCase]);

      WebServices.Consulta.ExtrairEventos := AExtrairEventos;

      Resp := TConsultaCTeResposta.Create(Config.TipoResposta, Config.CodResposta);

      try
        WebServices.Consulta.Executar;
        Resp.Processar(CTeDM.ACBrCTe1);
        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
        CTeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.Inutilizar(const ACNPJ, AJustificativa: PChar;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
  const sResposta: PChar; var esTamanho: longint): longint;    
var
  Resp: TInutilizarCTeResposta;
  Resposta, CNPJ, Justificativa: Ansistring;
begin
  try
    Justificativa := ConverterAnsiParaUTF8(AJustificativa);
    CNPJ := ConverterAnsiParaUTF8(ACNPJ);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_Inutilizar(' + CNPJ + ',' + Justificativa + ',' + IntToStr(Ano) + ',' +
        IntToStr(modelo) + ',' + IntToStr(Serie) +  ',' + IntToStr(NumeroInicial) +  ',' +
        IntToStr(NumeroFinal) + ' )', logCompleto, True)
    else
      GravarLog('CTE_Inutilizar', logNormal);

    CNPJ := OnlyNumber(CNPJ);

    if not ValidarCNPJ(CNPJ) then
      raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJInvalido, [ACNPJ]));

    CTeDM.Travar;
    Resp := TInutilizarCTeResposta.Create(Config.TipoResposta, Config.CodResposta);
    try
      with CTeDM.ACBrCTe1.WebServices do
      begin
        Inutilizacao.Clear;
        Inutilizacao.CNPJ := CNPJ;
        Inutilizacao.Justificativa := Justificativa;
        Inutilizacao.Modelo := Modelo;
        Inutilizacao.Serie := Serie;
        Inutilizacao.Ano := Ano;
        Inutilizacao.NumeroInicial := NumeroInicial;
        Inutilizacao.NumeroFinal := NumeroFinal;

        Inutilizacao.Executar;
        Resp.Processar(CTeDM.ACBrCTe1);
        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      Resp.Free;
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.Enviar(ALote: Integer; AImprimir, ASincrono: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;  
var
  RespEnvio: TEnvioResposta;
  RespRetorno: TRetornoResposta;
  ImpResp: TLibImpressaoResposta;
  Resposta: String;
  I, ImpCount: Integer;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_Enviar(' + IntToStr(ALote) + ',' +
                   BoolToStr(AImprimir, 'Imprimir','') + ',' +
                   BoolToStr(ASincrono, 'Sincrono','')+ ' )', logCompleto, True)
    else
      GravarLog('CTE_Enviar', logNormal);

    Resposta := '';
    RespEnvio := nil;
    RespRetorno := nil;
    ImpResp := nil;

    CTeDM.Travar;
    try
      with CTeDM.ACBrCTe1 do
      begin
        if Conhecimentos.Count <= 0 then
          raise EACBrLibException.Create(ErrEnvio, 'ERRO: Nenhuma CT-e adicionada ao Lote');

        if Conhecimentos.Count > 50 then
          raise EACBrLibException.Create(ErrEnvio, 'ERRO: Conjunto de CT-e transmitidas (máximo de 50 CT-e)' +
                                                   ' excedido. Quantidade atual: ' + IntToStr(Conhecimentos.Count));

        Conhecimentos.Assinar;

        try
          Conhecimentos.Validar;
        except
          on E: EACBrCTeException do
          begin
            Result := SetRetorno(ErrValidacaoCTe, ConverterUTF8ParaAnsi(E.Message));
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
          RespEnvio.Processar(CTeDM.ACBrCTe1);
          Resposta := RespEnvio.Gerar;
        finally
          RespEnvio.Free;
        end;

        if CTeDM.ACBrCTe1.Configuracoes.Geral.VersaoDF >= ve400 then
           ASincrono := True;

        if not ASincrono or ((NaoEstaVazio(WebServices.Enviar.Recibo)) and (WebServices.Enviar.cStat = 103)) then
        begin
          WebServices.Retorno.Recibo := WebServices.Enviar.Recibo;
          WebServices.Retorno.Executar;

          RespRetorno := TRetornoResposta.Create('CTe', Config.TipoResposta, Config.CodResposta);

          try
            RespRetorno.Processar(WebServices.Retorno.CTeRetorno,
                                  WebServices.Retorno.Recibo,
                                  WebServices.Retorno.Msg,
                                  WebServices.Retorno.Protocolo,
                                  WebServices.Retorno.ChaveCTe);

            Resposta := Resposta + sLineBreak + RespRetorno.Gerar;
          finally
            RespRetorno.Free;
          end;
        end;

        if AImprimir then
        begin
          CTeDM.ConfigurarImpressao;

          ImpCount := 0;
          for I := 0 to Conhecimentos.Count - 1 do
          begin
            if Conhecimentos.Items[I].Confirmado then
            begin
              Conhecimentos.Items[I].Imprimir;
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
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.ConsultarRecibo(ARecibo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TReciboResposta;
  sRecibo, Resposta: Ansistring;
begin
  try
    sRecibo := ConverterAnsiParaUTF8(ARecibo);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_ConsultarRecibo(' + sRecibo + ' )', logCompleto, True)
    else
      GravarLog('CTE_ConsultarRecibo', logNormal);

    CTeDM.Travar;

    try
      with CTeDM.ACBrCTe1 do
      begin
        WebServices.Recibo.Recibo := sRecibo;
        WebServices.Recibo.Executar;

        Resp := TReciboResposta.Create('CTe', Config.TipoResposta, Config.CodResposta);
        try
          Resp.Processar(WebServices.Recibo.CTeRetorno,
          WebServices.Recibo.Recibo);
          Resposta := Resp.Gerar;
        finally
          Resp.Free;
        end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.Cancelar(const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;  
var
  Resp: TCancelamentoResposta;
  AChave, AJustificativa, ACNPJ: Ansistring;
  Resposta: string;
begin
  try
    AChave := ConverterAnsiParaUTF8(eChave);
    AJustificativa := ConverterAnsiParaUTF8(eJustificativa);
    ACNPJ := ConverterAnsiParaUTF8(eCNPJ);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_Cancelar(' + AChave + ',' + AJustificativa + ',' +
                        ACNPJ + ',' + IntToStr(ALote) + ' )', logCompleto, True)
    else
      GravarLog('CTE_Cancelar', logNormal);

    CTeDM.Travar;

    try
      if not ValidarChave(AChave) then
        raise EACBrLibException.Create(ErrChaveCTe, Format(SErrChaveInvalida, [AChave]))
      else
        CTeDM.ACBrCTe1.WebServices.Consulta.CTeChave := AChave;

      if not CTeDM.ACBrCTe1.WebServices.Consulta.Executar then
        raise EACBrLibException.Create(ErrConsulta, CTeDM.ACBrCTe1.WebServices.Consulta.Msg);

      CTeDM.ACBrCTe1.EventoCTe.Evento.Clear;

      with CTeDM.ACBrCTe1.EventoCTe.Evento.New do
      begin
        Infevento.CNPJ := ACNPJ;
        if Trim(Infevento.CNPJ) = '' then
          Infevento.CNPJ := copy(OnlyNumber(CTeDM.ACBrCTe1.WebServices.Consulta.CTeChave), 7, 14)
        else
        begin
          if not ValidarCNPJouCPF(ACNPJ) then
            raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJ]));
        end;

        Infevento.nSeqEvento := 1;
        InfEvento.tpAmb := CTeDM.ACBrCTe1.Configuracoes.WebServices.Ambiente;
        Infevento.cOrgao := StrToIntDef(copy(OnlyNumber(CTeDM.ACBrCTe1.WebServices.Consulta.CTeChave), 1, 2), 0);
        Infevento.dhEvento := now;
        Infevento.tpEvento := teCancelamento;
        Infevento.chCTe := CTeDM.ACBrCTe1.WebServices.Consulta.CTeChave;
        Infevento.detEvento.nProt := CTeDM.ACBrCTe1.WebServices.Consulta.Protocolo;
        Infevento.detEvento.xJust := AJustificativa;
      end;

      if (ALote = 0) then
        ALote := 1;

      CTeDM.ACBrCTe1.WebServices.EnvEvento.idLote := ALote;
      CTeDM.ACBrCTe1.WebServices.EnvEvento.Executar;

      Resp := TCancelamentoResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(CTeDM.ACBrCTe1);
        Resposta := Resp.Gerar;
      finally
        Resp.Free;
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.EnviarEvento(idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;  
var
  i, j: Integer;
  chCTe, Resposta: Ansistring;
  Resp: TEventoResposta;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_EnviarEvento(' + IntToStr(idLote) + ' )', logCompleto, True)
    else
      GravarLog('CTE_EnviarEvento', logNormal);

    CTeDM.Travar;

    try
      with CTeDM.ACBrCTe1 do
      begin
        if EventoCTe.Evento.Count <= 0 then
          raise EACBrLibException.Create(ErrEnvioEvento, 'ERRO: Nenhum Evento adicionado ao Lote');

        if EventoCTe.Evento.Count > 20 then
          raise EACBrLibException.Create(ErrEnvioEvento,'ERRO: Conjunto de Eventos transmitidos (máximo de 20) ' +
                                                        'excedido. Quantidade atual: ' + IntToStr(EventoCTe.Evento.Count));

        {Atribuir nSeqEvento, CNPJ, Chave e/ou Protocolo quando não especificar}
        for i := 0 to EventoCTe.Evento.Count -1 do
        begin
          if EventoCTe.Evento.Items[i].InfEvento.nSeqEvento = 0 then
            EventoCTe.Evento.Items[i].infEvento.nSeqEvento := 1;

          EventoCTe.Evento.Items[i].InfEvento.tpAmb := Configuracoes.WebServices.Ambiente;

          if Conhecimentos.Count > 0 then
          begin
            chCTe := OnlyNumber(EventoCTe.Evento.Items[i].InfEvento.chCTe);

            // Se tem a chave do CTe no Evento, procure por ela nos conhecimentos carregados //
            if NaoEstaVazio(chCTe) then
            begin
              for j := 0 to Conhecimentos.Count - 1 do
              begin
                if chCTe = Conhecimentos.Items[j].NumID then
                  Break;
              end;
              if j = Conhecimentos.Count then
                raise EACBrLibException.Create(ErrEnvioEvento,'Não existe CTe com a chave ['+chCTe+'] carregado');
            end
            else
              j := 0;

            if trim(EventoCTe.Evento.Items[i].InfEvento.CNPJ) = '' then
              EventoCTe.Evento.Items[i].InfEvento.CNPJ := Conhecimentos.Items[j].CTe.Emit.CNPJ;

            if chCTe = '' then
              EventoCTe.Evento.Items[i].InfEvento.chCTe := Conhecimentos.Items[j].NumID;

            if trim(EventoCTe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
            begin
              if EventoCTe.Evento.Items[i].infEvento.tpEvento = teCancelamento then
              begin
                EventoCTe.Evento.Items[i].infEvento.detEvento.nProt := Conhecimentos.Items[j].CTe.procCTe.nProt;

                if trim(EventoCTe.Evento.Items[i].infEvento.detEvento.nProt) = '' then
                begin
                  WebServices.Consulta.CTeChave := EventoCTe.Evento.Items[i].InfEvento.chCTe;

                  if not WebServices.Consulta.Executar then
                   raise EACBrLibException.Create(ErrEnvioEvento, WebServices.Consulta.Msg);

                  EventoCTe.Evento.Items[i].infEvento.detEvento.nProt := WebServices.Consulta.Protocolo;
                end;
              end;
            end;
          end;
        end;

        WebServices.EnvEvento.idLote := idLote;
        WebServices.EnvEvento.Executar;

        Resp := TEventoResposta.Create(Config.TipoResposta, Config.CodResposta);
        try
          Resp.Processar(CTeDM.ACBrCTe1);
          Resposta := Resp.Gerar;
        finally
          Resp.Free;
        end;

        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      end;
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.ConsultaCadastro(cUF, nDocumento: PChar; nIE: boolean;
    const sResposta: PChar; var esTamanho: longint): longint;    
var
  Resp: TConsultaCadastroResposta;
  AUF, ADocumento: Ansistring;
  Resposta: string;
begin
  try
    AUF := ConverterAnsiParaUTF8(cUF);
    ADocumento := ConverterAnsiParaUTF8(nDocumento);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_ConsultaCadastro(' + AUF + ',' + ADocumento + ',' + BoolToStr(nIE, True) + ' )', logCompleto, True)
    else
      GravarLog('CTE_ConsultaCadastro', logNormal);

    CTeDM.Travar;
    try
      CTeDM.ACBrCTe1.WebServices.ConsultaCadastro.UF   := AUF;
      if nIE then
        CTeDM.ACBrCTe1.WebServices.ConsultaCadastro.IE := ADocumento
      else
      begin
        if Length(ADocumento) > 11 then
          CTeDM.ACBrCTe1.WebServices.ConsultaCadastro.CNPJ := ADocumento
        else
          CTeDM.ACBrCTe1.WebServices.ConsultaCadastro.CPF := ADocumento;
      end;

      CTeDM.ACBrCTe1.WebServices.ConsultaCadastro.Executar;
      Resp := TConsultaCadastroResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(CTeDM.ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad);
        Resposta := Resp.Gerar;
      finally
        Resp.Free;
      end;

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.DistribuicaoDFePorUltNSU(const AcUFAutor: integer; eCNPJCPF, eultNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;  
begin
  result := DistribuicaoDFe(AcUFAutor, eCNPJCPF, eultNSU, '',
                            sResposta, esTamanho);
end;

function TACBrLibCTe.DistribuicaoDFe(const AcUFAutor: integer;  eCNPJCPF, eultNSU, eArquivoOuXML: PChar;
                                     const sResposta: PChar; var esTamanho: longint): longint;
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
var
  AultNSU, ACNPJCPF: Ansistring;
  Resposta: string;
  Resp: TDistribuicaoDFeResposta;
begin
  try
    ACNPJCPF := ConverterAnsiParaUTF8(eCNPJCPF);
    AultNSU := ConverterAnsiParaUTF8(eultNSU);
    ArquivoOuXml := ConverterAnsiParaUTF8(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_DistribuicaoDFePorUltNSU(' + IntToStr(AcUFAutor) + ',' +
                     ACNPJCPF + ',' + AultNSU + ',' + ' )', logCompleto, True)
    else
      GravarLog('CTE_DistribuicaoDFePorUltNSU', logNormal);

    if ArquivoOuXml <> '' then
    begin
      EhArquivo := StringEhArquivo(ArquivoOuXml);
      if EhArquivo then
        VerificarArquivoExiste(ArquivoOuXml);
    end;

    CTeDM.Travar;

    try
      if ArquivoOuXml = '' then
        if not ValidarCNPJouCPF(ACNPJCPF) then
          raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      with CTeDM do
      begin
        try
          // Lê o arquivo selecionado
          if ArquivoOuXml <> '' then
          begin
            try
              ACBrCTe1.WebServices.DistribuicaoDFe.ListaArqs.Clear;
              ACBrCTe1.WebServices.DistribuicaoDFe.Clear;
              ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Clear;

              if EhArquivo then
                ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.Leitor.CarregarArquivo(ArquivoOuXml)
              else
                ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.Leitor.Arquivo := ArquivoOuXml;

              ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.LerXml;

              // Preenche a lista de arquivos extraídos da distribuição, pois a leitura não gera os arquivos individuais
              while ACBrCTe1.WebServices.DistribuicaoDFe.ListaArqs.Count <
                    ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.docZip.Count do
                ACBrCTe1.WebServices.DistribuicaoDFe.ListaArqs.Add('');

              AultNSU := ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.ultNSU;
            except
              on E:Exception do
                raise EACBrLibException.Create(ErrCNPJ, E.Message);
            end;
          end
          // Consulta o WebService
          else
          begin
            ACBrCTe1.WebServices.DistribuicaoDFe.cUFAutor := AcUFAutor;
            ACBrCTe1.WebServices.DistribuicaoDFe.CNPJCPF  := ACNPJCPF;
            ACBrCTe1.WebServices.DistribuicaoDFe.ultNSU   := AultNSU;
            ACBrCTe1.WebServices.DistribuicaoDFe.NSU      := '';
            ACBrCTe1.WebServices.DistribuicaoDFe.chCTe    := '';
            ACBrCTe1.WebServices.DistribuicaoDFe.Executar;
          end;

          Resp := TDistribuicaoDFeResposta.Create(Config.TipoResposta, Config.CodResposta);
          try
            Resp.Processar(ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt,
                           ACBrCTe1.WebServices.DistribuicaoDFe.Msg,
                           ACBrCTe1.WebServices.DistribuicaoDFe.NomeArq,
                           ACBrCTe1.WebServices.DistribuicaoDFe.ListaArqs);
            Resposta := Resp.Gerar;
          finally
            Resp.Free;
          end;

          MoverStringParaPChar(Resposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, Resposta);
        except
          on E: Exception do
          begin
            raise EACBrLibException.Create(ErrRetorno, E.Message + SLineBreak  + '  MotivoRetornadoDoWebService: "' + Trim(ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.xMotivo) + '"' );
          end;
        end;
      end;
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.DistribuicaoDFePorNSU(const AcUFAutor: integer; eCNPJCPF, eNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;  
var
  ANSU, ACNPJCPF: Ansistring;
  Resposta: string;
  Resp: TDistribuicaoDFeResposta;
begin
  try
    ACNPJCPF := ConverterAnsiParaUTF8(eCNPJCPF);
    ANSU := ConverterAnsiParaUTF8(eNSU);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_DistribuicaoDFePorNSU(' + IntToStr(AcUFAutor) + ',' +
                     ACNPJCPF + ',' + ANSU + ',' + ' )', logCompleto, True)
    else
      GravarLog('CTE_DistribuicaoDFePorNSU', logNormal);

    CTeDM.Travar;

    try
      if not ValidarCNPJouCPF(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      with CTeDM do
      begin
        try
          ACBrCTe1.WebServices.DistribuicaoDFe.cUFAutor := AcUFAutor;
          ACBrCTe1.WebServices.DistribuicaoDFe.CNPJCPF  := ACNPJCPF;
          ACBrCTe1.WebServices.DistribuicaoDFe.ultNSU   := '';
          ACBrCTe1.WebServices.DistribuicaoDFe.NSU      := ANSU;
          ACBrCTe1.WebServices.DistribuicaoDFe.chCTe    := '';

          ACBrCTe1.WebServices.DistribuicaoDFe.Executar;

          Resp := TDistribuicaoDFeResposta.Create(Config.TipoResposta, Config.CodResposta);
          try
            Resp.Processar(ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt,
                           ACBrCTe1.WebServices.DistribuicaoDFe.Msg,
                           ACBrCTe1.WebServices.DistribuicaoDFe.NomeArq,
                           ACBrCTe1.WebServices.DistribuicaoDFe.ListaArqs);
            Resposta := Resp.Gerar;
          finally
            Resp.Free;
          end;

          MoverStringParaPChar(Resposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, Resposta);
        except
          on E: Exception do
            raise EACBrLibException.Create(ErrRetorno, E.ToString + sLineBreak +
                'retDistDFeInt.xMotivo: '+ ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.xMotivo);
        end;
      end;
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.DistribuicaoDFePorChave(const AcUFAutor: integer; eCNPJCPF, echCTe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;  
var
  AchCTe, ACNPJCPF: Ansistring;
  Resposta: string;
  Resp: TDistribuicaoDFeResposta;
begin
  try
    ACNPJCPF := ConverterAnsiParaUTF8(eCNPJCPF);
    AchCTe := ConverterAnsiParaUTF8(echCTe);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_DistribuicaoDFePorChave(' + IntToStr(AcUFAutor) + ',' +
                     ACNPJCPF + ',' + AchCTe + ' )', logCompleto, True)
    else
      GravarLog('CTE_DistribuicaoDFePorChave', logNormal);

    CTeDM.Travar;

    try
      if not ValidarCNPJouCPF(ACNPJCPF) then
        raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

      if not ValidarChave(AchCTe) then
        raise EACBrLibException.Create(ErrChaveCTe, Format(SErrChaveInvalida, [AchCTe]));

      with CTeDM do
      begin
        try
          ACBrCTe1.WebServices.DistribuicaoDFe.cUFAutor := AcUFAutor;
          ACBrCTe1.WebServices.DistribuicaoDFe.CNPJCPF  := ACNPJCPF;
          ACBrCTe1.WebServices.DistribuicaoDFe.ultNSU   := '';
          ACBrCTe1.WebServices.DistribuicaoDFe.NSU      := '';
          ACBrCTe1.WebServices.DistribuicaoDFe.chCTe    := AchCTe;

          ACBrCTe1.WebServices.DistribuicaoDFe.Executar;

          Resp := TDistribuicaoDFeResposta.Create(Config.TipoResposta, Config.CodResposta);
          try
            Resp.Processar(ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt,
                           ACBrCTe1.WebServices.DistribuicaoDFe.Msg,
                           ACBrCTe1.WebServices.DistribuicaoDFe.NomeArq,
                           ACBrCTe1.WebServices.DistribuicaoDFe.ListaArqs);
            Resposta := Resp.Gerar;
          finally
            Resp.Free;
          end;

          MoverStringParaPChar(Resposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, Resposta);
        except
          raise EACBrLibException.Create(ErrRetorno, ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.xMotivo);
        end;
      end;
      finally
        CTeDM.Destravar;
      end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.EnviarEmail(const ePara, eChaveCTe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;  
var
  APara, AChaveCTe, AAssunto, ACC, AAnexos, AMensagem: Ansistring;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
begin
  try
    APara := ConverterAnsiParaUTF8(ePara);
    AChaveCTe := ConverterAnsiParaUTF8(eChaveCTe);
    AAssunto := ConverterAnsiParaUTF8(eAssunto);
    ACC := ConverterAnsiParaUTF8(eCC);
    AAnexos := ConverterAnsiParaUTF8(eAnexos);
    AMensagem := ConverterAnsiParaUTF8(eMensagem);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_EnviarEmail(' + APara + ',' + AChaveCTe + ',' +
         BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' + ACC + ',' +
         AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      GravarLog('CTE_EnviarEmail', logNormal);

    CTeDM.Travar;

    try

      with CTeDM.ACBrCTe1 do
      begin
        EhArquivo := StringEhArquivo(AChaveCTe);

        if EhArquivo then
          VerificarArquivoExiste(AChaveCTe);

        if EhArquivo then
          Conhecimentos.LoadFromFile(AchaveCTe)
        else
          Conhecimentos.LoadFromString(AchaveCTe);

        if Conhecimentos.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfCTeCarregados, [Conhecimentos.Count]))
        else
        begin
          slMensagemEmail := TStringList.Create;
          slCC := TStringList.Create;
          slAnexos := TStringList.Create;

          try
            with mail do
            begin
              slMensagemEmail.DelimitedText:= sLineBreak;
              slMensagemEmail.Text := StringReplace(AMensagem, ';', sLineBreak, [rfReplaceAll]);

              slCC.DelimitedText:= sLineBreak;
              slCC.Text := StringReplace(ACC, ';', sLineBreak, [rfReplaceAll]);

              slAnexos.DelimitedText := sLineBreak;
              slAnexos.Text := StringReplace(AAnexos, ';', sLineBreak, [rfReplaceAll]);

              if AEnviaPDF then
                CTeDM.ConfigurarImpressao('', True);

              try
                CTeDM.ACBrCTe1.Conhecimentos.Items[0].EnviarEmail(
                  APara,
                  AAssunto,
                  slMensagemEmail,
                  AEnviaPDF, // Enviar PDF junto
                  slCC,      // Lista com emails que serão enviado cópias - TStrings
                  slAnexos); // Lista de slAnexos - TStrings

                Result := SetRetorno(ErrOK, 'Email enviado com sucesso');
              except
                on E: Exception do
                  raise EACBrLibException.Create(ErrRetorno, 'Erro ao enviar email' + sLineBreak + E.Message);
              end;
            end;
          finally
            slCC.Free;
            slAnexos.Free;
            slMensagemEmail.Free;
            if AEnviaPDF then CTeDM.FinalizarImpressao;
          end;
        end;
      end;
    finally
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.EnviarEmailEvento(const ePara, eChaveEvento, eChaveCTe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;  
var
  APara, AChaveEvento, AChaveCTe, AAssunto, ACC, AAnexos, AMensagem,
  ArqPDF: Ansistring;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
begin
  try
    APara := ConverterAnsiParaUTF8(ePara);
    AChaveEvento := ConverterAnsiParaUTF8(eChaveEvento);
    AChaveCTe := ConverterAnsiParaUTF8(eChaveCTe);
    AAssunto := ConverterAnsiParaUTF8(eAssunto);
    ACC := ConverterAnsiParaUTF8(eCC);
    AAnexos := ConverterAnsiParaUTF8(eAnexos);
    AMensagem := ConverterAnsiParaUTF8(eMensagem);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_EnviarEmailEvento(' + APara + ',' + AChaveEvento + ',' +
         AChaveCTe + ',' + BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' +
         ACC + ',' + AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      GravarLog('CTE_EnviarEmailEvento', logNormal);

      CTeDM.Travar;

      try
        with CTeDM.ACBrCTe1 do
        begin
          EventoCTe.Evento.Clear;
          Conhecimentos.Clear;

          EhArquivo := StringEhArquivo(AChaveEvento);

          if EhArquivo then
            VerificarArquivoExiste(AChaveEvento);

          if EhArquivo then
            EventoCTe.LerXML(AChaveEvento)
          else
            EventoCTe.LerXMLFromString(AChaveEvento);

          EhArquivo := StringEhArquivo(AChaveCTe);

          if EhArquivo then
            VerificarArquivoExiste(AChaveCTe);

          if EhArquivo then
            Conhecimentos.LoadFromFile(AchaveCTe)
          else
            Conhecimentos.LoadFromString(AchaveCTe);

          if EventoCTe.Evento.Count = 0 then
            raise EACBrLibException.Create(ErrEnvio, Format(SInfEventosCarregados, [EventoCTe.Evento.Count]))
          else
          begin
            slMensagemEmail := TStringList.Create;
            slCC := TStringList.Create;
            slAnexos := TStringList.Create;

            try
              if AEnviaPDF then
              begin
                try
                  CTeDM.ConfigurarImpressao('', True);
                  ImprimirEventoPDF;

                  ArqPDF := OnlyNumber(EventoCTe.Evento[0].Infevento.id);
                  ArqPDF := PathWithDelim(DACTe.PathPDF)+ArqPDF+'-procEventoCTe.pdf';
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

                slAnexos.Add(AChaveEvento);

                if AEnviaPDF then
                  slAnexos.Add(ArqPDF);

                try
                  CTeDM.ACBrCTe1.EnviarEmail(
                                            APara,
                                            AAssunto,
                                            slMensagemEmail,
                                            slCC,      // Lista com emails que serão enviado cópias - TStrings
                                            slAnexos); // Lista de slAnexos - TStrings

                  Result := SetRetorno(ErrOK, 'Email enviado com sucesso');
                except
                  on E: Exception do
                    raise EACBrLibException.Create(ErrRetorno, 'Erro ao enviar email' + sLineBreak + E.Message);
                end;
              end;
            finally
              slCC.Free;
              slAnexos.Free;
              slMensagemEmail.Free;
              if AEnviaPDF then CTeDM.FinalizarImpressao;
            end;
          end;
        end;
      finally
        CTeDM.Destravar;
      end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.Imprimir(const cImpressora: PChar; nNumCopias: Integer; const cProtocolo,
  bMostrarPreview: PChar): longint; 
Var
  Resposta: TLibImpressaoResposta;
  Impressora, Protocolo,
  MostrarPreview: Ansistring;
begin
  try
    Impressora := ConverterAnsiParaUTF8(cImpressora);
    Protocolo := ConverterAnsiParaUTF8(cProtocolo);
    MostrarPreview := ConverterAnsiParaUTF8(bMostrarPreview);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_Imprimir(' + Impressora + ',' + IntToStr(nNumCopias) + ','
        + Protocolo + ',' + MostrarPreview + ')', logCompleto, True)
    else
      GravarLog('CTE_Imprimir', logNormal);

    CTeDM.Travar;

    Resposta := TLibImpressaoResposta.Create(CTeDM.ACBrCTe1.Conhecimentos.Count, Config.TipoResposta, Config.CodResposta);
    try
      CTeDM.ConfigurarImpressao(Impressora, False, Protocolo, MostrarPreview);

      if nNumCopias > 0 then
        CTeDM.ACBrCTe1.DACTE.NumCopias := nNumCopias;

      CTeDM.ACBrCTe1.Conhecimentos.Imprimir;
      Result := SetRetorno(ErrOK, Resposta.Gerar);
    finally
      CTeDM.FinalizarImpressao;
      Resposta.Free;
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.ImprimirPDF: longint;  
begin
  try
    GravarLog('CTE_ImprimirPDF', logNormal);

    CTeDM.Travar;
    try
      CTeDM.ConfigurarImpressao('', True);
      CTeDM.ACBrCTe1.Conhecimentos.ImprimirPDF;
      Result := SetRetornoCTesCarregados(CTeDM.ACBrCTe1.Conhecimentos.Count);
    finally
      CTeDM.FinalizarImpressao;
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.SalvarPDF(const sResposta: PChar; var esTamanho: longint): longint;
var
  AStream: TMemoryStream;
  Resposta: AnsiString;
begin
  try
    GravarLog('CTE_SalvarPDF', logNormal);

    CTeDM.Travar;

    AStream := TMemoryStream.Create;

    try
      CTeDM.ConfigurarImpressao('', True);

      CTeDM.ACBrCTe1.Conhecimentos.ImprimirPDF(AStream);
      Resposta := StreamToBase64(AStream);

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      CTeDM.FinalizarImpressao;
      AStream.Free;
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.ImprimirEvento(const eArquivoXmlCTe, eArquivoXmlEvento: PChar): longint;  
var
  EhArquivo: boolean;
  AArquivoXmlCTe, AArquivoXmlEvento: Ansistring;
begin
  try
    AArquivoXmlCTe := ConverterAnsiParaUTF8(eArquivoXmlCTe);
    AArquivoXmlEvento := ConverterAnsiParaUTF8(eArquivoXmlEvento);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_ImprimirEvento(' + AArquivoXmlCTe + ',' + AArquivoXmlEvento + ' )', logCompleto, True)
    else
      GravarLog('CTE_ImprimirEvento', logNormal);

    CTeDM.Travar;

    try
      EhArquivo := StringEhArquivo(AArquivoXmlCTe);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlCTe);

      if EhArquivo then
        CTeDM.ACBrCTe1.Conhecimentos.LoadFromFile(AArquivoXmlCTe)
      else
        CTeDM.ACBrCTe1.Conhecimentos.LoadFromString(AArquivoXmlCTe);

      EhArquivo := StringEhArquivo(AArquivoXmlEvento);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlEvento);

      if EhArquivo then
        CTeDM.ACBrCTe1.EventoCTe.LerXML(AArquivoXmlEvento)
      else
        CTeDM.ACBrCTe1.EventoCTe.LerXMLFromString(AArquivoXmlEvento);

      CTeDM.ConfigurarImpressao;
      CTeDM.ACBrCTe1.ImprimirEvento;

      Result := SetRetorno(ErrOK);
    finally
      CTeDM.FinalizarImpressao;
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.ImprimirEventoPDF(const eArquivoXmlCTe, eArquivoXmlEvento: PChar): longint;  
var
  EhArquivo: boolean;
  AArquivoXmlCTe, AArquivoXmlEvento: Ansistring;
begin
  try
    AArquivoXmlCTe := ConverterAnsiParaUTF8(eArquivoXmlCTe);
    AArquivoXmlEvento := ConverterAnsiParaUTF8(eArquivoXmlEvento);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_ImprimirEventoPDF(' + AArquivoXmlCTe + ',' + AArquivoXmlEvento + ' )', logCompleto, True)
    else
      GravarLog('CTE_ImprimirEventoPDF', logNormal);

    CTeDM.Travar;

    try
      EhArquivo := StringEhArquivo(AArquivoXmlCTe);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlCTe);

      if EhArquivo then
        CTeDM.ACBrCTe1.Conhecimentos.LoadFromFile(AArquivoXmlCTe)
      else
        CTeDM.ACBrCTe1.Conhecimentos.LoadFromString(AArquivoXmlCTe);

      EhArquivo := StringEhArquivo(AArquivoXmlEvento);

      if EhArquivo then
        VerificarArquivoExiste(AArquivoXmlEvento);

      if EhArquivo then
        CTeDM.ACBrCTe1.EventoCTe.LerXML(AArquivoXmlEvento)
      else
        CTeDM.ACBrCTe1.EventoCTe.LerXMLFromString(AArquivoXmlEvento);

      CTeDM.ConfigurarImpressao('', True);
      CTeDM.ACBrCTe1.ImprimirEventoPDF;

      Result := SetRetorno(ErrOK);
    finally
      CTeDM.FinalizarImpressao;
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.SalvarEventoPDF(const eArquivoXmlCTe, eArquivoXmlEvento: PChar;  sResposta: PChar; var esTamanho: longint): longint;
var
  EhArquivo: boolean;
  AArquivoXmlCTe: string;
  AArquivoXmlEvento: string;
  AStream: TMemoryStream;
  Resposta: Ansistring;
begin
  try
    AArquivoXmlCTe := ConverterAnsiParaUTF8(eArquivoXmlCTe);
    AArquivoXmlEvento := ConverterAnsiParaUTF8(eArquivoXmlEvento);

    if Config.Log.Nivel > logNormal then
       GravarLog('CTE_SalvarEventoPDF(' + AArquivoXmlCTe + ',' + AArquivoXmlEvento + ' )', logCompleto, True)
    else
      GravarLog('CTE_SalvarEventoPDF', logNormal);

    CTeDM.Travar;
    AStream := TMemoryStream.Create;

    try
      EhArquivo := StringEhArquivo(AArquivoXmlCTe);

      if EhArquivo then
      VerificarArquivoExiste(AArquivoXmlCTe);

      if EhArquivo then
         CTeDM.ACBrCTe1.Conhecimentos.LoadFromFile(AArquivoXmlCTe)
      else
         CTeDM.ACBrCTe1.Conhecimentos.LoadFromString(AArquivoXmlCTe);

      EhArquivo := StringEhArquivo(AArquivoXmlEvento);

      if EhArquivo then
      VerificarArquivoExiste(AArquivoXmlEvento);

      if EhArquivo then
         CTeDM.ACBrCTe1.EventoCTe.LerXML(AArquivoXmlEvento)
      else
         CTeDM.ACBrCTe1.EventoCTe.LerXMLFromString(AArquivoXmlEvento);

      CTeDM.ConfigurarImpressao('', True);
      CTeDM.ACBrCTe1.DACTE.ImprimirEventoPDF(AStream);

      Resposta := StreamToBase64(AStream);

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      CTeDM.FinalizarImpressao;
      AStream.Free;
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.ImprimirInutilizacao(const eArquivoXml: PChar): longint;  
var
  EhArquivo: boolean;
  AArquivoXml: Ansistring;
begin
  try
    AArquivoXml := ConverterAnsiParaUTF8(eArquivoXml);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_ImprimirInutilizacao(' + AArquivoXml + ' )', logCompleto, True)
    else
      GravarLog('CTE_ImprimirInutilizacao', logNormal);

    EhArquivo := StringEhArquivo(AArquivoXml);

    if EhArquivo then
      VerificarArquivoExiste(AArquivoXml);

    CTeDM.Travar;

    try
      if EhArquivo then
        CTeDM.ACBrCTe1.InutCTe.LerXML(AArquivoXml)
      else
        CTeDM.ACBrCTe1.InutCTe.LerXMLFromString(AArquivoXml);

      CTeDM.ConfigurarImpressao;
      CTeDM.ACBrCTe1.ImprimirInutilizacao;

      Result := SetRetorno(ErrOK);
    finally
      CTeDM.FinalizarImpressao;
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.ImprimirInutilizacaoPDF(const eArquivoXml: PChar): longint;  
var
  EhArquivo: boolean;
  AArquivoXml: Ansistring;
begin
  try
    AArquivoXml := ConverterAnsiParaUTF8(eArquivoXml);

    if Config.Log.Nivel > logNormal then
      GravarLog('CTE_ImprimirInutilizacaoPDF(' + AArquivoXml + ' )', logCompleto, True)
    else
      GravarLog('CTE_ImprimirInutilizacaoPDF', logNormal);

    EhArquivo := StringEhArquivo(AArquivoXml);

    if EhArquivo then
      VerificarArquivoExiste(AArquivoXml);

    CTeDM.Travar;
    try
      if EhArquivo then
        CTeDM.ACBrCTe1.InutCTe.LerXML(AArquivoXml)
      else
        CTeDM.ACBrCTe1.InutCTe.LerXMLFromString(AArquivoXml);

      CTeDM.ConfigurarImpressao('', True);
      CTeDM.ACBrCTe1.ImprimirInutilizacaoPDF;

      Result := SetRetorno(ErrOK);
    finally
      CTeDM.FinalizarImpressao;
      CTeDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCTe.SetRetornoCTesCarregados(const NumCTe: Integer): Integer;
begin
  Result := SetRetorno( 0, {NumCTe,} Format(SInfCTeCarregados, [NumCTe]));
end;

function TACBrLibCTe.SetRetornoEventoCarregados(const NumEventos: Integer): Integer;
begin
  Result := SetRetorno( 0, {NumNFe,} Format(SInfEventosCarregados, [NumEventos]));
end;

end.
