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

unit ACBrLibGNReClass;

interface

uses
  Classes, SysUtils, Forms, StrUtils,
  ACBrLibComum, ACBrLibGNReDataModule;

type

  { TACBrLibGNRe }

  TACBrLibGNRe = class(TACBrLib)
  private
    FGNReDM: TLibGNReDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property GNReDM: TLibGNReDM read FGNReDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function GNRE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRe_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRe_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRe_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRe_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRe_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRe_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRe_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region GNRe}
function GNRE_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_ObterXml(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_GravarXml(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_LimparListaGuiaRetorno: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_CarregarGuiaRetorno(const eArquivoOuXml: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_Assinar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_Validar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Servicos}
function GNRE_Enviar(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_Consultar(const eUF: PChar; const AReceita: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_EnviarEmail(const ePara, eArquivoOuXml: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_Imprimir(const eNomeImpressora, eMostrarPreview: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibGNReConsts, ACBrLibConfig, ACBrLibResposta,
  ACBrLibGNReConfig, ACBrLibGNReRespostas, ACBrGNRE2, ACBrMail,
  pcnConversao, pcnAuxiliar, blcksock, ACBrUtil, ACBrLibCertUtils;

{ TACBrLibGNRe }

constructor TACBrLibGNRe.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FGNReDM := TLibGNReDM.Create(nil);
end;

destructor TACBrLibGNRe.Destroy;
begin
  FGNReDM.Free;

  inherited Destroy;
end;

procedure TACBrLibGNRe.Inicializar;
begin
  GravarLog('TACBrLibGNRe.Inicializar', logNormal);

  FGNReDM.CriarACBrMail;

  GravarLog('TACBrLibGNRe.Inicializar - Feito', logParanoico);

  inherited Inicializar;
end;

procedure TACBrLibGNRe.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibGNReConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibGNRe.Executar;
begin
  inherited Executar;
  FGNReDM.AplicarConfiguracoes;
end;

{%region GNRe}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function GNRE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function GNRE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function GNRE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function GNRE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function GNRE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function GNRE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function GNRE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function GNRE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function GNRE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;

{%endregion}

function GNRE_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('GNRE_LimparLista', logNormal);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;
      try
        GNReDM.ACBrGNRe1.Guias.Clear;
        Result := SetRetornoGNReCarregados(GNReDM.ACBrGNRe1.Guias.Count);
      finally
        GNReDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function GNRE_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuXml := string(eArquivoOuXML);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('GNRE_CarregarXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      pLib.GravarLog('GNRE_CarregarXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;
      try
        if EhArquivo then
          GNReDM.ACBrGNRe1.Guias.LoadFromFile(ArquivoOuXml)
        else
          GNReDM.ACBrGNRe1.Guias.LoadFromString(ArquivoOuXml);

        Result := SetRetornoGNReCarregados(GNReDM.ACBrGNRe1.Guias.Count);
      finally
        GNReDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function GNRE_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ArquivoOuINI: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuINI := string(eArquivoOuINI);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('GNRE_CarregarINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      pLib.GravarLog('GNRE_CarregarINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;
      try
        GNReDM.ACBrGNRe1.Guias.LoadFromIni(ArquivoOuINI);
        Result := SetRetornoGNReCarregados(GNReDM.ACBrGNRe1.Guias.Count);
      finally
        GNReDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function GNRE_ObterXml(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Resposta: String;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('GNRE_ObterXml(' + IntToStr(AIndex) + ' )', logCompleto, True)
    else
      pLib.GravarLog('GNRE_ObterXml', logNormal);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;
      try
        if (GNReDM.ACBrGNRe1.Guias.Count < 1) or (AIndex < 0) or (AIndex >= GNReDM.ACBrGNRe1.Guias.Count) then
          raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

        if EstaVazio(GNReDM.ACBrGNRe1.Guias.Items[AIndex].XMLOriginal) then
          GNReDM.ACBrGNRe1.Guias.Items[AIndex].GerarXML;

        Resposta := GNReDM.ACBrGNRe1.Guias.Items[AIndex].XMLOriginal;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        GNReDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function GNRE_GravarXml(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  ANomeArquivo, APathArquivo: String;
begin
  try
    VerificarLibInicializada;
    ANomeArquivo := String(eNomeArquivo);
    APathArquivo := String(ePathArquivo);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('GNRE_GravarXml(' + IntToStr(AIndex) + ',' + ANomeArquivo + ',' + APathArquivo + ' )', logCompleto, True)
    else
      pLib.GravarLog('GNRE_GravarXml', logNormal);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;
      try
        if (GNReDM.ACBrGNRe1.Guias.Count < 1) or (AIndex < 0) or (AIndex >= GNReDM.ACBrGNRe1.Guias.Count) then
          raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

        if GNReDM.ACBrGNRe1.Guias.Items[AIndex].GravarXML(ANomeArquivo, APathArquivo) then
          Result := SetRetorno(ErrOK)
        else
          Result := SetRetorno(ErrGerarXml);
      finally
        GNReDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function GNRE_LimparListaGuiaRetorno: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('GNRE_LimparListaGuiaRetorno', logNormal);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;
      try
        GNReDM.ACBrGNRe1.GuiasRetorno.Clear;
        Result := SetRetornoGNReCarregados(GNReDM.ACBrGNRe1.GuiasRetorno.Count);
      finally
        GNReDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function GNRE_CarregarGuiaRetorno(const eArquivoOuXml: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuXml := string(eArquivoOuXml);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('GNRE_CarregarGuiaRetorno(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      pLib.GravarLog('GNRE_CarregarGuiaRetorno', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;
      try
        if EhArquivo then
          GNReDM.ACBrGNRe1.GuiasRetorno.LoadFromFile(ArquivoOuXml)
        else
          GNReDM.ACBrGNRe1.GuiasRetorno.LoadFromString(ArquivoOuXml);
        Result := SetRetornoGNReCarregados(GNReDM.ACBrGNRe1.GuiasRetorno.Count);
      finally
        GNReDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function GNRE_Assinar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('GNRE_Assinar', logNormal);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;
      try
        try
          GNReDM.ACBrGNRe1.Guias.Assinar;
        except
          on E: EACBrGNReException do
            Result := SetRetorno(ErrAssinarGNRe, E.Message);
        end;
        Result := SetRetornoGNReCarregados(GNReDM.ACBrGNRe1.Guias.Count);
      finally
        GNReDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function GNRE_Validar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('GNRE_Validar', logNormal);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;
      try
        try
          GNReDM.ACBrGNRe1.Guias.Validar;
        except
          on E: EACBrGNReException do
            Result := SetRetorno(ErrValidacaoGNRe, E.Message);
        end;
        Result := SetRetornoGNReCarregados(GNReDM.ACBrGNRe1.Guias.Count);
      finally
        GNReDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function GNRE_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Erros: string;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('GNRE_VerificarAssinatura', logNormal);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;
      try
        Erros := '';
        GNReDM.ACBrGNRe1.Guias.VerificarAssinatura(Erros);
        MoverStringParaPChar(Erros, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Erros);
      finally
        GNReDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function GNRE_ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Resposta: string;
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('GNRE_ObterCertificados', logNormal);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;

      try
        Resposta := '';
        Resposta := ObterCerticados(GNReDM.ACBrGNRe1.SSL);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        GNReDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

{%endregion}

{%region Servicos}
function GNRE_Enviar(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resp: TLibGNReEnvio;
  Resposta: String;
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('GNRE_Enviar', logNormal);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;

      try
        with GNReDM.ACBrGNRe1 do
        begin
          if Guias.Count = 0 then
            raise EACBrLibException.Create(ErrEnvio, Format(SInfGNReCarregados, [Guias.Count]));

          Guias.Assinar;
          Guias.Validar;

          Resp := TLibGNReEnvio.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);

          WebServices.Enviar.Executar;
          Resposta := ''; //RespostaEnvio;

          WebServices.Retorno.numeroRecibo := WebServices.Enviar.numero;
          WebServices.Retorno.Executar;

          Resp.Processar(GNReDM.ACBrGNRe1);

          Resposta := Resposta + Resp.Gerar;
          MoverStringParaPChar(Resposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, StrPas(sResposta));
        end;
      finally
        GNReDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function GNRE_Consultar(const eUF: PChar; const AReceita: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resp: TLibGNReConsulta;
  AUF, Resposta: string;
begin
  try
    VerificarLibInicializada;

    AUF := string(eUF);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('GNRE_Consultar(' + AUF + ',' + IntToStr(AReceita) +' )', logCompleto, True)
    else
      pLib.GravarLog('GNRE_Consultar', logNormal);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;

      try
        with GNReDM.ACBrGNRe1 do
        begin

          WebServices.ConsultaUF.Uf := AUF;
          WebServices.ConsultaUF.receita := AReceita;
          WebServices.ConsultaUF.Executar;

          Resp := TLibGNReConsulta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
          Resp.Processar(GNReDM.ACBrGNRe1);
          Resposta := Resp.Gerar;

          MoverStringParaPChar(Resposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, StrPas(sResposta));
        end;
      finally
        GNReDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function GNRE_EnviarEmail(const ePara, eArquivoOuXml: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  APara, ArquivoOuXml, AAssunto, ACC, AAnexos, AMensagem: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
begin
  try
    VerificarLibInicializada;

    APara := string(ePara);
    ArquivoOuXml := string(eArquivoOuXml);
    AAssunto := string(eAssunto);
    ACC := string(eCC);
    AAnexos := string(eAnexos);
    AMensagem := string(eMensagem);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('GNRE_EnviarEmail(' + APara + ',' + ArquivoOuXml + ',' +
         BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' + ACC + ',' +
         AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      pLib.GravarLog('GNRE_EnviarEmail', logNormal);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;

      with GNReDM.ACBrGNRe1 do
      begin
        EhArquivo := StringEhArquivo(ArquivoOuXml);

        if EhArquivo then
          VerificarArquivoExiste(ArquivoOuXml);

        if EhArquivo then
          Guias.LoadFromFile(ArquivoOuXml)
        else
          Guias.LoadFromString(ArquivoOuXml);

        if Guias.Count = 0 then
          raise EACBrLibException.Create(ErrEnvio, Format(SInfGNReCarregados, [Guias.Count]))
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

              try
                GNReDM.ACBrGNRe1.Guias.Items[0].EnviarEmail(
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
          end;
        end;
      end;

      GNReDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function GNRE_Imprimir(const eNomeImpressora, eMostrarPreview: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  NomeImpressora, MostrarPreview: string;
begin
  try
    VerificarLibInicializada;

    NomeImpressora := string(eNomeImpressora);
    MostrarPreview := string(eMostrarPreview);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('GNRE_Imprimir(' + NomeImpressora + ',' + MostrarPreview + ' )', logCompleto, True)
    else
      pLib.GravarLog('GNRE_Imprimir', logNormal);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;
      with  GNReDM.ACBrGNRe1 do
      begin
        try
          if GuiasRetorno.Count = 0 then
            raise EACBrLibException.Create(ErrEnvio, Format(SInfGNReCarregados, [GuiasRetorno.Count]))
          else
          begin
            GNReDM.ConfigurarImpressao(False, NomeImpressora, MostrarPreview);

            GuiasRetorno.Imprimir;

            Result := SetRetornoGNReCarregados(GuiasRetorno.Count);
          end;
        finally
          GNReDM.Destravar;
        end;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function GNRE_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('GNRE_ImprimirPDF', logNormal);

    with TACBrLibGNRe(pLib) do
    begin
      GNReDM.Travar;
      with  GNReDM.ACBrGNRe1 do
      begin
        try
          if GuiasRetorno.Count = 0 then
            raise EACBrLibException.Create(ErrEnvio, Format(SInfGNReCarregados, [GuiasRetorno.Count]))
          else
          begin
            GNReDM.ConfigurarImpressao(True);
            GuiasRetorno.ImprimirPDF;

            Result := SetRetornoGNReCarregados(GuiasRetorno.Count);
          end;
        finally
          GNReDM.Destravar;
        end;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

{%endregion}

initialization
  pLibClass := TACBrLibGNRe;

end.
