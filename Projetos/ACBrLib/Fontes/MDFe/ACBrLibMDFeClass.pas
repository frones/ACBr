{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                 }
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

unit ACBrLibMDFeClass;

interface

uses
  Classes, SysUtils, Forms,
  ACBrLibComum, ACBrLibMDFeDataModule;

type

  { TACBrLibMDFe }

  TACBrLibMDFe = class(TACBrLib)
  private
    FMDFeDM: TLibMDFeDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property MDFeDM: TLibMDFeDM read FMDFeDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function MDFE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_ImportarConfig(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region MDFe}
function MDFE_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_ObterXml(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_GravarXml(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_ObterIni(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_GravarIni(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_CarregarEventoXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_CarregarEventoINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_LimparListaEventos: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_Assinar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_Validar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi: longint;
  AEmissao, ACNPJCPF: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_GetPath(ATipo: longint; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_GetPathEvento(ACodEvento: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Servicos}
function MDFE_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_Consultar(const eChaveOuMDFe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_Enviar(ALote: Integer; AImprimir, ASincrono: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_ConsultarRecibo(ARecibo: PChar;
    const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_Cancelar(const eChave, eJustificativa, eCNPJCPF: PChar; ALote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_EnviarEvento(idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_EncerrarMDFe(const eChaveOuMDFe, eDtEnc, cMunicipioDescarga, nCNPJ, nProtocolo: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_ConsultaMDFeNaoEnc(const nCNPJ: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_DistribuicaoDFePorUltNSU(eCNPJCPF, eultNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_DistribuicaoDFePorNSU(eCNPJCPF, eNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_DistribuicaoDFePorChave(eCNPJCPF, echMDFe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_EnviarEmail(const ePara, eArquivoXmlMDFe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_EnviarEmailEvento(const ePara, eArquivoXmlEvento, eArquivoXmlMDFe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_Imprimir(const cImpressora: PChar; nNumCopias: Integer; const cProtocolo,
  bMostrarPreview: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_ImprimirEvento(const eArquivoXmlMDFe, eArquivoXmlEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MDFE_ImprimirEventoPDF(const eArquivoXmlMDFe, eArquivoXmlEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibMDFeConsts, ACBrLibConfig, ACBrLibResposta,
  ACBrLibMDFeConfig, ACBrLibMDFeRespostas, ACBrMDFe, ACBrMail,
  ACBrLibConsReciDFe, ACBrLibDistribuicaoDFe, ACBrDFeUtil, ACBrLibCertUtils,
  pcnConversao, pcnAuxiliar, pMDFeConversaoMDFe, blcksock, ACBrUtil;

{ TACBrLibMDFe }

constructor TACBrLibMDFe.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FMDFeDM := TLibMDFeDM.Create(nil);
end;

destructor TACBrLibMDFe.Destroy;
begin
  FMDFeDM.Free;

  inherited Destroy;
end;

procedure TACBrLibMDFe.Inicializar;
begin
  GravarLog('TACBrLibMDFe.Inicializar', logNormal);

  FMDFeDM.CriarACBrMail;

  GravarLog('TACBrLibMDFe.Inicializar - Feito', logParanoico);

  inherited Inicializar;
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

{%region MDFe}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function MDFE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function MDFE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function MDFE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function MDFE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function MDFE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function MDFE_ImportarConfig(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ImportarConfig(eArqConfig);
end;

function MDFE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function MDFE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function MDFE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function MDFE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;

{%endregion}

function MDFE_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuXml := string(eArquivoOuXML);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_CarregarXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_CarregarXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    with TACBrLibMDFe(pLib) do
    begin
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
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ArquivoOuINI: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuINI := string(eArquivoOuINI);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_CarregarINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_CarregarINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        MDFeDM.ACBrMDFe1.Manifestos.LoadFromIni(ArquivoOuINI);
        Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_ObterXml(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Resposta: String;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_ObterXml(' + IntToStr(AIndex) + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_ObterXml', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
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
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_GravarXml(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  ANomeArquivo, APathArquivo: String;
begin
  try
    VerificarLibInicializada;
    ANomeArquivo := String(eNomeArquivo);
    APathArquivo := String(ePathArquivo);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_GravarXml(' + IntToStr(AIndex) + ',' + ANomeArquivo + ',' + APathArquivo + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_GravarXml', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
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
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_ObterIni(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Resposta: String;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_ObterIni(' + IntToStr(AIndex) + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_ObterIni', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        if (AIndex >= MDFeDM.ACBrMDFe1.Manifestos.Count) and (MDFeDM.ACBrMDFe1.Manifestos.Count < 1) then
          raise EACBrLibException.Create(ErrIndex, Format(SErrIndex, [AIndex]));

        if EstaVazio(MDFeDM.ACBrMDFe1.Manifestos.Items[AIndex].XMLOriginal) then
          MDFeDM.ACBrMDFe1.Manifestos.Items[AIndex].GerarXML;

        Resposta := MDFeDM.ACBrMDFe1.Manifestos.Items[AIndex].GerarMDFeIni;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_GravarIni(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  AMDFeIni, ANomeArquivo, APathArquivo: String;
begin
  try
    VerificarLibInicializada;
    ANomeArquivo := String(eNomeArquivo);
    APathArquivo := String(ePathArquivo);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_GravarIni(' + IntToStr(AIndex) + ',' + ANomeArquivo + ',' + APathArquivo + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_GravarIni', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
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
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_CarregarEventoXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuXml := string(eArquivoOuXML);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_CarregarEventoXML(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_CarregarEventoXML', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    with TACBrLibMDFe(pLib) do
    begin
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
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_CarregarEventoINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ArquivoOuINI: string;
begin
  try
    VerificarLibInicializada;
    ArquivoOuINI := string(eArquivoOuINI);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_CarregarEventoINI(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_CarregarEventoINI', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        MDFeDM.ACBrMDFe1.EventoMDFe.LerFromIni(ArquivoOuINI);
        Result := SetRetornoEventoCarregados(MDFeDM.ACBrMDFe1.EventoMDFe.Evento.Count);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFE_LimparLista', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        MDFeDM.ACBrMDFe1.Manifestos.Clear;
        Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_LimparListaEventos: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFE_LimparListaEventos', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        MDFeDM.ACBrMDFe1.EventoMDFe.Evento.Clear;
        Result := SetRetornoEventoCarregados(MDFeDM.ACBrMDFe1.EventoMDFe.Evento.Count);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_Assinar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFE_Assinar', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        try
          MDFeDM.ACBrMDFe1.Manifestos.Assinar;
        except
          on E: EACBrMDFeException do
            Result := SetRetorno(ErrAssinarMDFe, E.Message);
        end;
        Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_Validar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFE_Validar', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        try
          MDFeDM.ACBrMDFe1.Manifestos.Validar;
        except
          on E: EACBrMDFeException do
            Result := SetRetorno(ErrValidacaoMDFe, E.Message);
        end;
        Result := SetRetornoMDFeCarregados(MDFeDM.ACBrMDFe1.Manifestos.Count);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Erros: string;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFE_ValidarRegrasdeNegocios', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        Erros := '';
        MDFeDM.ACBrMDFe1.Manifestos.ValidarRegrasdeNegocios(Erros);
        MoverStringParaPChar(Erros, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Erros);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Erros: string;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFE_VerificarAssinatura', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      try
        Erros := '';
        MDFeDM.ACBrMDFe1.Manifestos.VerificarAssinatura(Erros);
        MoverStringParaPChar(Erros, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Erros);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi: longint;
  AEmissao, ACNPJCPF: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Resposta, CNPJCPF: string;
  Emissao: TDateTime;
begin
  try
    VerificarLibInicializada;

    Emissao := StrToDate(AEmissao);
    CNPJCPF := String(ACNPJCPF);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_GerarChave(' + IntToStr(ACodigoUF) + ',' + IntToStr(ACodigoNumerico) + ',' +
                      IntToStr(AModelo)  + ',' + IntToStr(AModelo) + ',' + IntToStr(ASerie) + ',' +
                      IntToStr(ANumero) + ',' + IntToStr(ATpEmi) + ',' + DateToStr(Emissao) + ',' +
                      CNPJCPF + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_GerarChave', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      try
        Resposta := '';
        Resposta := GerarChaveAcesso(ACodigoUF, Emissao, CNPJCPF, ASerie, ANumero, ATpEmi, ACodigoNumerico, AModelo);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Resposta: string;
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('MDFE_ObterCertificados', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      try
        Resposta := '';
        Resposta := ObterCerticados(MDFeDM.ACBrMDFe1.SSL);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_GetPath(ATipo: longint; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Resposta: string;
  ok: Boolean;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_GetPath(' + IntToStr(ATipo) + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_GetPath', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      try
        with MDFeDM do
        begin
          Resposta := '';

          case ATipo of
            0: Resposta := ACBrMDFe1.Configuracoes.Arquivos.GetPathMDFe();
            1: Resposta := ACBrMDFe1.Configuracoes.Arquivos.GetPathEvento(teCancelamento);
          end;

          MoverStringParaPChar(Resposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, Resposta);
        end;
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_GetPathEvento(ACodEvento: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Resposta, CodEvento: string;
  ok: Boolean;
begin
  try
    VerificarLibInicializada;

    CodEvento := String(ACodEvento);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_GetPathEvento(' + CodEvento +' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_GetPathEvento', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      try
        with MDFeDM do
        begin
          Resposta := '';
          Resposta := ACBrMDFe1.Configuracoes.Arquivos.GetPathEvento(StrToTpEventoMDFe(ok, CodEvento));
          MoverStringParaPChar(Resposta, sResposta, esTamanho);
          Result := SetRetorno(ErrOK, Resposta);
        end;
      finally
        MDFeDM.Destravar;
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

function MDFE_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resp: TStatusServicoResposta;
  Resposta: String;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFE_StatusServico', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      Resp := TStatusServicoResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
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
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_Consultar(const eChaveOuMDFe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  ChaveOuMDFe: string;
  Resp: TConsultaResposta;
  Resposta: string;
begin
  try
    VerificarLibInicializada;

    ChaveOuMDFe := string(eChaveOuMDFe);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_Consultar(' + ChaveOuMDFe + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_Consultar', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

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

      Resp := TConsultaResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
      try
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
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_Enviar(ALote: Integer; AImprimir, ASincrono: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: String;
  RespEnvio: TEnvioResposta;
  RespRetorno: TRetornoResposta;
  ImpResp: TLibImpressaoResposta;
  i, j, ImpCount: Integer;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_Enviar(' + IntToStr(ALote) + ','
                                    + BoolToStr(AImprimir, 'Imprimir ,',' , ')
                                    + BoolToStr(ASincrono, 'Sincrono','  ') + ')', logCompleto, True)
    else
      pLib.GravarLog('MDFE_Enviar', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
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
          Manifestos.Validar;

          if (ALote = 0) then
            WebServices.Enviar.Lote := '1'
          else
            WebServices.Enviar.Lote := IntToStr(ALote);

          WebServices.Enviar.Sincrono := ASincrono;
          WebServices.Enviar.Executar;

          RespEnvio := TEnvioResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
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

            RespRetorno := TRetornoResposta.Create('MDFe', pLib.Config.TipoResposta, pLib.Config.CodResposta);
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
              ImpResp := TLibImpressaoResposta.Create(ImpCount, pLib.Config.TipoResposta, pLib.Config.CodResposta);
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
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_ConsultarRecibo(ARecibo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resp: TReciboResposta;
  sRecibo, Resposta: string;
  i, j: Integer;
begin
  try
    VerificarLibInicializada;

    sRecibo := string(ARecibo);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_ConsultarRecibo(' + sRecibo + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_ConsultarRecibo', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      try
        with MDFeDM.ACBrMDFe1 do
        begin
          WebServices.Recibo.Recibo := sRecibo;
          WebServices.Recibo.Executar;

          Resp := TReciboResposta.Create('MDFe', pLib.Config.TipoResposta, pLib.Config.CodResposta);
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
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_Cancelar(const eChave, eJustificativa, eCNPJCPF: PChar; ALote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AChave, AJustificativa, ACNPJCPF: string;
  Resp: TCancelamentoResposta;
  Resposta: string;
begin
  try
    VerificarLibInicializada;

    AChave := string(eChave);
    AJustificativa := string(eJustificativa);
    ACNPJCPF := string(eCNPJCPF);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_Cancelar(' + AChave + ',' + AJustificativa + ',' +
                        ACNPJCPF + ',' + IntToStr(ALote) + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_Cancelar', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
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

        Resp := TCancelamentoResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
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
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_EnviarEvento(idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  i, j: integer;
  Resp: TEventoResposta;
  Resposta, chMDFe: String;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_EnviarEvento(' + IntToStr(idLote) + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_EnviarEvento', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
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
          Resp := TEventoResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
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
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_EncerrarMDFe(const eChaveOuMDFe, eDtEnc, cMunicipioDescarga, nCNPJ, nProtocolo: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Resp: TEncerramentoResposta;
  ChaveOuMDFe, MunicipioDescarga, CNPJ, Protocolo, Resposta: string;
  DtEnc: TDateTime;
begin
  try
    VerificarLibInicializada;
    ChaveOuMDFe := String(eChaveOuMDFe);
    DtEnc := StrToDateTime(eDtEnc);
    MunicipioDescarga := String(cMunicipioDescarga);
    CNPJ := String(nCNPJ);
    Protocolo := String(nProtocolo);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_EncerrarMDFe(' + ChaveOuMDFe + ',' + DateTimeToStr(DtEnc) + ',' +
                     MunicipioDescarga + ',' + CNPJ + ',' + Protocolo + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_EncerrarMDFe', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
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
           Resp := TEncerramentoResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);

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
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_ConsultaMDFeNaoEnc(const nCNPJ: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  CNPJ, Resposta: String;
  Resp: TNaoEncerradosResposta ;
begin
  try
    VerificarLibInicializada;
    CNPJ := String(nCNPJ);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFE_ConsultaMDFeNaoEnc(' + CNPJ + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFE_ConsultaMDFeNaoEnc', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      try
        if not ValidarCNPJouCPF(CNPJ) then
          raise EACBrLibException.Create(ErrCNPJ, 'CNPJ/CPF ' + CNPJ + ' invalido.');

        MDFeDM.ACBrMDFe1.WebServices.ConsMDFeNaoEnc.CNPJCPF := CNPJ;
        MDFeDM.ACBrMDFe1.WebServices.ConsMDFeNaoEnc.Executar;

        Resposta := '';
        Resp := TNaoEncerradosResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);

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
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_DistribuicaoDFePorUltNSU(eCNPJCPF, eultNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AultNSU, ACNPJCPF: string;
  Resposta: string;
  Resp: TDistribuicaoDFeResposta;
begin
  try
    VerificarLibInicializada;

    ACNPJCPF := string(eCNPJCPF);
    AultNSU := string(eultNSU);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_DistribuicaoDFePorUltNSU(' + ACNPJCPF + ',' + AultNSU + ',' + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_DistribuicaoDFePorUltNSU', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      try
        if not ValidarCNPJ(ACNPJCPF) then
          raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

        with MDFeDM do
        begin
          ACBrMDFe1.WebServices.DistribuicaoDFe.CNPJCPF  := ACNPJCPF;
          ACBrMDFe1.WebServices.DistribuicaoDFe.ultNSU   := AultNSU;
          ACBrMDFe1.WebServices.DistribuicaoDFe.NSU      := '';
          ACBrMDFe1.WebServices.DistribuicaoDFe.chMDFe    := '';

          ACBrMDFe1.WebServices.DistribuicaoDFe.Executar;

          Resp := TDistribuicaoDFeResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
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
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, E.Message);

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_DistribuicaoDFePorNSU(eCNPJCPF, eNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ANSU, ACNPJCPF: string;
  Resposta: string;
  Resp: TDistribuicaoDFeResposta;
begin
  try
    VerificarLibInicializada;

    ACNPJCPF := string(eCNPJCPF);
    ANSU := string(eNSU);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_DistribuicaoDFePorNSU(' + ACNPJCPF + ',' + ANSU + ',' + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_DistribuicaoDFePorNSU', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      try
        if not ValidarCNPJ(ACNPJCPF) then
          raise EACBrLibException.Create(ErrCNPJ, Format(SErrCNPJCPFInvalido, [ACNPJCPF]));

        with MDFeDM do
        begin
          ACBrMDFe1.WebServices.DistribuicaoDFe.CNPJCPF  := ACNPJCPF;
          ACBrMDFe1.WebServices.DistribuicaoDFe.ultNSU   := '';
          ACBrMDFe1.WebServices.DistribuicaoDFe.NSU      := ANSU;
          ACBrMDFe1.WebServices.DistribuicaoDFe.chMDFe    := '';

          ACBrMDFe1.WebServices.DistribuicaoDFe.Executar;

          Resp := TDistribuicaoDFeResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
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
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, E.Message);

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_DistribuicaoDFePorChave(eCNPJCPF, echMDFe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AchMDFe, ACNPJCPF: string;
  Resposta: string;
  Resp: TDistribuicaoDFeResposta;
begin
  try
    VerificarLibInicializada;

    ACNPJCPF := string(eCNPJCPF);
    AchMDFe := string(echMDFe);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_DistribuicaoDFePorChave(' + ACNPJCPF + ',' + AchMDFe + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_DistribuicaoDFePorChave', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      try
        if not ValidarCNPJ(ACNPJCPF) then
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

          Resp := TDistribuicaoDFeResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
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
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, E.Message);

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_EnviarEmail(const ePara, eArquivoXmlMDFe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  APara, AArquivoXmlMDFe, AAssunto, ACC, AAnexos, AMensagem: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
  Resposta: TLibMDFeResposta;
begin
  try
    VerificarLibInicializada;

    APara := string(ePara);
    AArquivoXmlMDFe := string(eArquivoXmlMDFe);
    AAssunto := string(eAssunto);
    ACC := string(eCC);
    AAnexos := string(eAnexos);
    AMensagem := string(eMensagem);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_EnviarEmail(' + APara + ',' + AArquivoXmlMDFe + ',' +
                     BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' + ACC + ',' +
                     AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_EnviarEmail', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      try
        with MDFeDM do
        begin
          EhArquivo := StringEhArquivo(AArquivoXmlMDFe);

          if EhArquivo then
            VerificarArquivoExiste(AArquivoXmlMDFe);

          if EhArquivo then
            ACBrMDFe1.Manifestos.LoadFromFile(AArquivoXmlMDFe);

          if ACBrMDFe1.Manifestos.Count = 0 then
            raise EACBrLibException.Create(ErrEnvio, Format(SInfMDFeCarregados, [ACBrMDFe1.Manifestos.Count]))
          else
          begin
            slMensagemEmail := TStringList.Create;
            slCC := TStringList.Create;
            slAnexos := TStringList.Create;
            Resposta := TLibMDFeResposta.Create('EnviaEmail', pLib.Config.TipoResposta, pLib.Config.CodResposta);
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

                ACBrMDFe1.Manifestos.Items[0].EnviarEmail(
                  APara,
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
            end;
          end;
        end;
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, E.Message);

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_EnviarEmailEvento(const ePara, eArquivoXmlEvento, eArquivoXmlMDFe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  APara, AArquivoXmlEvento, AArquivoXmlMDFe, AAssunto, ACC, AAnexos, AMensagem,
  ArqPDF: string;
  slMensagemEmail, slCC, slAnexos: TStringList;
  EhArquivo: boolean;
  Resposta: TLibMDFeResposta;
begin
  try
    VerificarLibInicializada;

    APara := string(ePara);
    AArquivoXmlEvento := string(eArquivoXmlEvento);
    AArquivoXmlMDFe := string(eArquivoXmlMDFe);
    AAssunto := string(eAssunto);
    ACC := string(eCC);
    AAnexos := string(eAnexos);
    AMensagem := string(eMensagem);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_EnviarEmailEvento(' + APara + ',' + AArquivoXmlEvento + ',' +
             AArquivoXmlMDFe + ',' + BoolToStr(AEnviaPDF, 'PDF','') + ',' + AAssunto + ',' +
             ACC + ',' + AAnexos + ',' + AMensagem + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_EnviarEmailEvento', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
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
            EventoMDFe.LerXML(AArquivoXmlEvento);

          EhArquivo := StringEhArquivo(AArquivoXmlMDFe);

          if EhArquivo then
            VerificarArquivoExiste(AArquivoXmlMDFe);

          if EhArquivo then
            Manifestos.LoadFromFile(AArquivoXmlMDFe);

          if EventoMDFe.Evento.Count = 0 then
            raise EACBrLibException.Create(ErrEnvio, Format(SInfEventosCarregados, [EventoMDFe.Evento.Count]))
          else
          begin
            slMensagemEmail := TStringList.Create;
            slCC := TStringList.Create;
            slAnexos := TStringList.Create;
            Resposta := TLibMDFeResposta.Create('EnviaEmail', pLib.Config.TipoResposta, pLib.Config.CodResposta);
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
                  MDFeDM.ACBrMDFe1.EnviarEmail(
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
            end;
          end;
        end;
      finally
        MDFeDM.Destravar;
      end;
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, E.Message);

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_Imprimir(const cImpressora: PChar; nNumCopias: Integer; const cProtocolo, bMostrarPreview: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Resposta: TLibImpressaoResposta;
  NumCopias: Integer;
  Impressora, Protocolo,
  MostrarPreview: String;
begin
  try
    VerificarLibInicializada;

    Impressora := String(cImpressora);
    Protocolo := String(cProtocolo);
    MostrarPreview := String(bMostrarPreview);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_Imprimir(' + Impressora + ',' + IntToStr(nNumCopias) + ',' +
                     Protocolo + ',' + MostrarPreview + ')', logCompleto, True)
    else
      pLib.GravarLog('MDFe_Imprimir', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;

      Resposta := TLibImpressaoResposta.Create(MDFeDM.ACBrMDFe1.Manifestos.Count, pLib.Config.TipoResposta,
                                               pLib.Config.CodResposta);
      try
        MDFeDM.ConfigurarImpressao(Impressora, False, Protocolo, MostrarPreview);
        NumCopias := MDFeDM.ACBrMDFe1.DAMDFE.NumCopias;
        if nNumCopias > 0 then
          MDFeDM.ACBrMDFe1.DAMDFE.NumCopias := nNumCopias;
        MDFeDM.ACBrMDFe1.Manifestos.Imprimir;
        Result := SetRetorno(ErrOK, Resposta.Gerar);
      finally
          MDFeDM.ACBrMDFe1.DAMDFE.NumCopias := NumCopias;
          Resposta.Free;
          MDFeDM.Destravar;
      end;
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, E.Message);

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Resposta: TLibImpressaoResposta;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MDFe_ImprimirPDF', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      Resposta := TLibImpressaoResposta.Create(MDFeDM.ACBrMDFe1.Manifestos.Count, pLib.Config.TipoResposta,
                                               pLib.Config.CodResposta);
      try
        MDFeDM.ConfigurarImpressao('', True);
        MDFeDM.ACBrMDFe1.Manifestos.ImprimirPDF;

        Resposta.Msg := MDFeDM.ACBrMDFe1.DAMDFE.ArquivoPDF;
        Result := SetRetorno(ErrOK, Resposta.Gerar);
      finally
          Resposta.Free;
          MDFeDM.Destravar;
      end;
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, E.Message);

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_ImprimirEvento(const eArquivoXmlMDFe, eArquivoXmlEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  AArquivoXmlMDFe: string;
  AArquivoXmlEvento: string;
  Resposta: TLibImpressaoResposta;
begin
  try
    VerificarLibInicializada;

    AArquivoXmlMDFe := string(eArquivoXmlMDFe);
    AArquivoXmlEvento := string(eArquivoXmlEvento);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_ImprimirEvento(' + AArquivoXmlMDFe + ',' + AArquivoXmlEvento + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_ImprimirEvento', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      Resposta := TLibImpressaoResposta.Create(MDFeDM.ACBrMDFe1.EventoMDFe.Evento.Count, pLib.Config.TipoResposta,
                                               pLib.Config.CodResposta);
      try
        EhArquivo := StringEhArquivo(AArquivoXmlMDFe);

        if EhArquivo then
          VerificarArquivoExiste(AArquivoXmlMDFe);

        if EhArquivo then
          MDFeDM.ACBrMDFe1.Manifestos.LoadFromFile(AArquivoXmlMDFe);

        EhArquivo := StringEhArquivo(AArquivoXmlEvento);

        if EhArquivo then
          VerificarArquivoExiste(AArquivoXmlEvento);

        if EhArquivo then
          MDFeDM.ACBrMDFe1.EventoMDFe.LerXML(AArquivoXmlEvento);

        MDFeDM.ConfigurarImpressao;
        MDFeDM.ACBrMDFe1.ImprimirEvento;

        Result := SetRetorno(ErrOK, Resposta.Gerar);
      finally
          Resposta.Free;
          MDFeDM.Destravar;
      end;
    end;
  except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, E.Message);

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MDFE_ImprimirEventoPDF(const eArquivoXmlMDFe, eArquivoXmlEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: boolean;
  AArquivoXmlMDFe: string;
  AArquivoXmlEvento: string;
  Resposta: TLibImpressaoResposta;
begin
  try
    VerificarLibInicializada;

    AArquivoXmlMDFe := string(eArquivoXmlMDFe);
    AArquivoXmlEvento := string(eArquivoXmlEvento);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MDFe_ImprimirEventoPDF(' + AArquivoXmlMDFe + ',' + AArquivoXmlEvento + ' )', logCompleto, True)
    else
      pLib.GravarLog('MDFe_ImprimirEventoPDF', logNormal);

    with TACBrLibMDFe(pLib) do
    begin
      MDFeDM.Travar;
      Resposta := TLibImpressaoResposta.Create(MDFeDM.ACBrMDFe1.EventoMDFe.Evento.Count, pLib.Config.TipoResposta,
                                               pLib.Config.CodResposta);
      try
        EhArquivo := StringEhArquivo(AArquivoXmlMDFe);

        if EhArquivo then
          VerificarArquivoExiste(AArquivoXmlMDFe);

        if EhArquivo then
          MDFeDM.ACBrMDFe1.Manifestos.LoadFromFile(AArquivoXmlMDFe);

        EhArquivo := StringEhArquivo(AArquivoXmlEvento);

        if EhArquivo then
          VerificarArquivoExiste(AArquivoXmlEvento);

        if EhArquivo then
          MDFeDM.ACBrMDFe1.EventoMDFe.LerXML(AArquivoXmlEvento);

        MDFeDM.ConfigurarImpressao('', True);
        MDFeDM.ACBrMDFe1.ImprimirEventoPDF;

        Resposta.Msg := MDFeDM.ACBrMDFe1.DAMDFE.ArquivoPDF;
        Result := SetRetorno(ErrOK, Resposta.Gerar);
      finally
          Resposta.Free;
          MDFeDM.Destravar;
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
  pLibClass := TACBrLibMDFe;

end.
