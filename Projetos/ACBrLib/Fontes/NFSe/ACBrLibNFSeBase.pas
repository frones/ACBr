{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
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

    property DM: TLibNFSeDM read FNFSeDM;

    function CarregarXML(const eArquivoOuXML: PChar): longint;
    function CarregarINI(const eArquivoOuINI: PChar): longint;
    function ObterXml(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
    function GravarXml(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
    function LimparLista: longint;
    function ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
    function Emitir(const aLote, aModoEnvio: longint;  aImprimir: Boolean;
                    const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarSituacao(const AProtocolo, ANumLote, sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarLoteRps(const AProtocolo, ANumLote, sResposta: PChar; var esTamanho: longint): longint;


  end;

implementation

uses
  StrUtils, ACBrLibResposta, ACBrLibHelpers,
  ACBrLibConsts, ACBrUtil, ACBrLibCertUtils,
  ACBrLibNFSeConsts, ACBrLibConfig, ACBrNFSeLibConfig,
  ACBrNFSeXConversao, ACBrNFSeXWebservicesResponse,
  ACBrLibNFSeRespostas;

{ TACBrLibNFSe }

constructor TACBrLibNFSe.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  FNFSeDM := TLibNFSeDM.Create(nil);
  FNFSeDM.Lib := self;
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

    DM.Travar;
    try
      if EhArquivo then
        DM.ACBrNFSeX1.NotasFiscais.LoadFromFile(ArquivoOuXml)
      else
        DM.ACBrNFSeX1.NotasFiscais.LoadFromString(ArquivoOuXml);

      Result := SetRetornoNFSeRPSCarregadas(DM.ACBrNFSeX1.NotasFiscais.Count);
    finally
      DM.Destravar;
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

    DM.Travar;
    try
      DM.ACBrNFSeX1.NotasFiscais.LoadFromIni(ArquivoOuIni);
      Result := SetRetornoNFSeRPSCarregadas(DM.ACBrNFSeX1.NotasFiscais.Count);
    finally
      DM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ObterXml(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resposta: Ansistring;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ObterXml(' + IntToStr(AIndex) + ' )', logCompleto, True)
    else
      GravarLog('NFSE_ObterXml', logNormal);

    DM.Travar;

    try
      if (DM.ACBrNFSeX1.NotasFiscais.Count < 1) or (AIndex < 0) or
         (AIndex >= DM.ACBrNFSeX1.NotasFiscais.Count) then
         raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if EstaVazio(DM.ACBrNFSeX1.NotasFiscais.Items[AIndex].XMLOriginal) then
        DM.ACBrNFSeX1.NotasFiscais.Items[AIndex].GerarXML;

      Resposta := DM.ACBrNFSeX1.NotasFiscais.Items[AIndex].XMLOriginal;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      DM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.GravarXml(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
var
  ANomeArquivo, APathArquivo: string;
begin
  try
    ANomeArquivo := ConverterAnsiParaUTF8(eNomeArquivo);
    APathArquivo := ConverterAnsiParaUTF8(ePathArquivo);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_GravarXml(' + IntToStr(AIndex) + ',' +
        ANomeArquivo + ',' + APathArquivo + ' )', logCompleto, True)
    else
      GravarLog('NFSE_GravarXml', logNormal);

    DM.Travar;
    try
      if (DM.ACBrNFSeX1.NotasFiscais.Count < 1) or (AIndex < 0) or
         (AIndex >= DM.ACBrNFSeX1.NotasFiscais.Count) then
         raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

      if DM.ACBrNFSeX1.NotasFiscais.Items[AIndex].GravarXML(ANomeArquivo, APathArquivo) then
        Result := SetRetorno(ErrOK)
      else
        Result := SetRetorno(ErrGerarXml);
    finally
      DM.Destravar;
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

    DM.Travar;
    try
      DM.ACBrNFSeX1.NotasFiscais.Clear;
      Result := SetRetornoNFSeRPSCarregadas(DM.ACBrNFSeX1.NotasFiscais.Count);
    finally
      DM.Destravar;
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

    DM.Travar;

    try
      Resposta := '';
      Resposta := ObterCerticados(DM.ACBrNFSeX1.SSL);
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      DM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.Emitir(const aLote, aModoEnvio: longint; aImprimir: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TEmiteResposta;
  Response: TNFSeEmiteResponse;
  Resposta: Ansistring;
  ModoEnvio: TmodoEnvio;
begin
  try
    if not (TEnum.TryParse<TmodoEnvio>(aModoEnvio, ModoEnvio)) then
      ModoEnvio := TmodoEnvio.meAutomatico;

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_Emitir(' + IntToStr(aLote) + ',' + ModoEnvioToStr(ModoEnvio) +
                 ',' + BoolToStr(aImprimir, True) + ' )', logCompleto, True)
    else
      GravarLog('NFSE_Emitir', logNormal);

    DM.Travar;

    try
      Response := DM.ACBrNFSeX1.Emitir(aLote, ModoEnvio, aImprimir);
      Resp := TEmiteResposta.Create(Config.TipoResposta, Config.CodResposta);
      Resp.Processar(Response);

      Resposta := Resp.Gerar;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      DM.Destravar;
      Resp.Free;
      Response.Free;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarSituacao(const AProtocolo, ANumLote, sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaSituacaoResposta;
  Response: TNFSeConsultaSituacaoResponse;
  Protocolo, NumLote: string;
  Resposta: Ansistring;
begin
  try
    Protocolo := ConverterAnsiParaUTF8(AProtocolo);
    NumLote := ConverterAnsiParaUTF8(ANumLote);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarSituacao(' + Protocolo + ',' + NumLote + ' )', logCompleto, True)
    else
      GravarLog('NFSE_ConsultarSituacao', logNormal);

    DM.Travar;

    try
      Response := DM.ACBrNFSeX1.ConsultarSituacao(Protocolo, NumLote);
      Resp := TConsultaSituacaoResposta.Create(Config.TipoResposta, Config.CodResposta);
      Resp.Processar(Response);

      Resposta := Resp.Gerar;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      DM.Destravar;
      Resp.Free;
      Response.Free;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibNFSe.ConsultarLoteRps(const AProtocolo, ANumLote, sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TConsultaSituacaoResposta;
  Response: TNFSeConsultaLoteRpsResponse;
  Protocolo, NumLote: string;
  Resposta: Ansistring;
begin
  try
    Protocolo := ConverterAnsiParaUTF8(AProtocolo);
    NumLote := ConverterAnsiParaUTF8(ANumLote);

    if Config.Log.Nivel > logNormal then
      GravarLog('NFSE_ConsultarLoteRps(' + Protocolo + ',' + NumLote + ' )', logCompleto, True)
    else
      GravarLog('NFSE_ConsultarLoteRps', logNormal);

    DM.Travar;

    try
      Response := DM.ACBrNFSeX1.ConsultarLoteRps(Protocolo, NumLote);
      //Resp := TConsultaSituacaoResposta.Create(Config.TipoResposta, Config.CodResposta);
      //Resp.Processar(Response);

      //Resposta := Resp.Gerar;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      DM.Destravar;
      Resp.Free;
      Response.Free;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

end.

