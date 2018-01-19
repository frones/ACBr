{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

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
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibNFeClass;

interface

uses
  Classes, SysUtils, Forms,
  ACBrLibComum, ACBrLibNFeDataModule;

type

  { TACBrLibNFe }

  TACBrLibNFe = class(TACBrLib)
  private
    FNFeDM: TLibNFeDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: String = ''; ChaveCrypt: AnsiString = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: String = ''; ChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    property NFeDM: TLibNFeDM read FNFeDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function NFE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Nome(const sNome: PChar; var esLen: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_Versao(const sVersao: PChar; var esLen: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_UltimoRetorno(const sMensagem: PChar; var esLen: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function NFE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region NFe}
function NFE_CarregarXMLNFe(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFE_CarregarININFe(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFE_LimparListaNFEs: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}


{%region Servicos}
function NFE_StatusServico(const Buffer: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibNFeConsts, ACBrLibConfig, ACBrLibNFeConfig,
  ACBrLibNFeRespostas,
  pcnConversao, pcnAuxiliar, blcksock, ACBrUtil;

{ TACBrLibNFe }

constructor TACBrLibNFe.Create(ArqConfig: String; ChaveCrypt: AnsiString);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibNFeNome;
  fpVersao := CLibNFeVersao;

  FNFeDM := TLibNFeDM.Create(nil);
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

procedure TACBrLibNFe.CriarConfiguracao(ArqConfig: String; ChaveCrypt: AnsiString);
begin
  fpConfig := TLibNFeConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibNFe.Executar;
begin
  inherited Executar;
  FNFeDM.AplicarConfiguracoes;
end;

{%region NFe}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function NFE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function NFE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function NFE_Nome(const sNome: PChar; var esLen: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esLen);
end;

function NFE_Versao(const sVersao: PChar; var esLen: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esLen);
end;

function NFE_UltimoRetorno(const sMensagem: PChar; var esLen: longint): longint;
  cdecl;
begin
  Result := LIB_UltimoRetorno(sMensagem, esLen);
end;

function NFE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function NFE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function NFE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor);
end;

function NFE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;

{%endregion}

function NFE_CarregarXMLNFe(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  EhArquivo: Boolean;
  ArquivoOuXml: String;
begin
  try
    VerificarLibInicializada;
    ArquivoOuXml := string(eArquivoOuXML);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFE_CarregarXMLNFe(' + ArquivoOuXml + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFE_CarregarXMLNFe', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        if EhArquivo then
          NFeDM.ACBrNFe1.NotasFiscais.LoadFromFile(ArquivoOuXml)
        else
          NFeDM.ACBrNFe1.NotasFiscais.LoadFromString(ArquivoOuXml);

        Result := SetRetornoNFesCarregadas( NFeDM.ACBrNFe1.NotasFiscais.Count );
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_CarregarININFe(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ArquivoOuINI: String;
begin
  try
    VerificarLibInicializada;
    ArquivoOuINI := string(eArquivoOuINI);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('NFE_CarregarININFe(' + ArquivoOuINI + ' )', logCompleto, True)
    else
      pLib.GravarLog('NFE_CarregarININFe', logNormal);

    if StringEhArquivo(ArquivoOuINI) then
      VerificarArquivoExiste(ArquivoOuINI);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        NFeDM.ACBrNFe1.NotasFiscais.LoadFromIni(ArquivoOuINI);
        Result := SetRetornoNFesCarregadas( NFeDM.ACBrNFe1.NotasFiscais.Count );
      finally
        NFeDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function NFE_LimparListaNFEs: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('NFE_LimparListaNFEs', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      try
        NFeDM.ACBrNFe1.NotasFiscais.Clear;
        Result := SetRetornoNFesCarregadas( NFeDM.ACBrNFe1.NotasFiscais.Count );
      finally
        NFeDM.Destravar;
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

function NFE_StatusServico(const Buffer: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resposta: TStatusServicoResposta;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('NFE_StatusServico', logNormal);

    with TACBrLibNFe(pLib) do
    begin
      NFeDM.Travar;
      Resposta := TStatusServicoResposta.Create( pLib.Config.TipoResposta );
      try
        with NFeDM.ACBrNFe1 do
        begin
          if WebServices.StatusServico.Executar then
          begin
            Resposta.Msg := WebServices.StatusServico.Msg;
            Resposta.Versao := WebServices.StatusServico.versao;
            Resposta.TpAmb := TpAmbToStr(WebServices.StatusServico.TpAmb);
            Resposta.VerAplic := WebServices.StatusServico.VerAplic;
            Resposta.CStat := WebServices.StatusServico.CStat;
            Resposta.XMotivo := WebServices.StatusServico.XMotivo;
            Resposta.CUF := WebServices.StatusServico.CUF;
            Resposta.DhRecbto := WebServices.StatusServico.DhRecbto;
            Resposta.TMed := WebServices.StatusServico.TMed;
            Resposta.DhRetorno := WebServices.StatusServico.DhRetorno;
            Resposta.XObs := WebServices.StatusServico.XObs;

            StrPCopy(Buffer, Resposta.Gerar);
            Result := SetRetorno(ErrOK);
          end
          else
            Result := SetRetornoWebService(SSL.HTTPResultCode, 'StatusServico');
        end;
      finally
        Resposta.Free;
        NFeDM.Destravar;
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
  pLibClass := TACBrLibNFe;

end.
