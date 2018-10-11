{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }

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

unit ACBrLibCEPClass;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibCEPDataModule, ACBrCEP;

type

  { TACBrLibCEP }

  TACBrLibCEP = class(TACBrLib)
  private
    FCEPDM: TLibCEPDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property CEPDM: TLibCEPDM read FCEPDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function CEP_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CEP_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CEP_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CEP_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CEP_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CEP_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CEP_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CEP_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CEP_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region CEP}
function RespostaItensConsulta(ItemID: integer = 0): String;

function CEP_BuscarPorCEP(eCEP: PChar; var Qtde: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CEP_BuscarPorLogradouro(eCidade, eTipo_Logradouro, eLogradouro, eUF,
  eBairro: PChar; var Qtde: Integer; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibCEPConsts, ACBrLibConfig, ACBrLibCEPConfig,
  ACBrLibResposta, ACBrLibCEPRespostas;

{ TACBrLibCEP }

constructor TACBrLibCEP.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibCEPNome;
  fpVersao := CLibCEPVersao;

  FCEPDM := TLibCEPDM.Create(nil);
end;

destructor TACBrLibCEP.Destroy;
begin
  FCEPDM.Free;
  inherited Destroy;
end;

procedure TACBrLibCEP.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibCEP.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibCEP.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibCEPConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibCEP.Executar;
begin
  inherited Executar;
  FCEPDM.AplicarConfiguracoes;
end;

{%region Comum}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function CEP_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function CEP_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function CEP_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function CEP_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function CEP_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function CEP_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function CEP_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function CEP_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function CEP_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;
{%endregion}

{%region CEP}

function RespostaItensConsulta(ItemID: integer): String;
var
  Resp: TLibCEPResposta;
begin
  Resp := TLibCEPResposta.Create(
          CSessaoRespConsulta + IntToStr(ItemID +1), resINI);
  try
    with TACBrLibCEP(pLib).CEPDM.ACBrCEP1.Enderecos[ItemID] do
    begin
      Resp.CEP := CEP;
      Resp.Tipo_Logradouro := Tipo_Logradouro;
      Resp.Logradouro := Logradouro;
      Resp.Logradouro := Logradouro;
      Resp.Complemento := Complemento;
      Resp.Bairro := Bairro;
      Resp.Municipio := Municipio;
      Resp.UF := UF;
      Resp.IBGE_Municipio := IBGE_Municipio;
      Resp.IBGE_UF := IBGE_UF;

      result := Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

function CEP_BuscarPorCEP(eCEP: PChar; var Qtde: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ACEP: AnsiString;
  AResposta: String;
begin
  try
    VerificarLibInicializada;
    ACEP := AnsiString(eCEP);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CEP_BuscarPorCEP( ' + ACEP + ' )', logCompleto, True)
    else
      pLib.GravarLog('CEP_BuscarPorCEP', logNormal);

    with TACBrLibCEP(pLib) do
    begin
      CEPDM.Travar;
      try
        Qtde := CEPDM.ACBrCEP1.BuscarPorCEP(ACEP);
        AResposta := RespostaItensConsulta(0);
        MoverStringParaPChar(AResposta, sResposta, esTamanho);

        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        CEPDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CEP_BuscarPorLogradouro(eCidade, eTipo_Logradouro, eLogradouro, eUF,
  eBairro: PChar; var Qtde: Integer; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ACidade, ATipo_Logradouro, ALogradouro, AUF, ABairro: AnsiString;
  AResposta: String;
  I: Integer;
begin
  try
    VerificarLibInicializada;
    ACidade := AnsiString(eCidade);
    ATipo_Logradouro := AnsiString(eTipo_Logradouro);
    ALogradouro := AnsiString(eLogradouro);
    AUF := AnsiString(eUF);
    ABairro := AnsiString(eBairro);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CEP_BuscarPorLogradouro( ' + ACidade + ',' + ATipo_Logradouro + ',' +
        ALogradouro + ',' + AUF + ',' +ABairro + ' )', logCompleto, True)
    else
      pLib.GravarLog('CEP_BuscarPorLogradouro', logNormal);

    with TACBrLibCEP(pLib) do
    begin
      CEPDM.Travar;
      try
        Qtde := CEPDM.ACBrCEP1.BuscarPorLogradouro(ACidade, ATipo_Logradouro, ALogradouro, AUF, ABairro);
        AResposta := '';

        for I := 0 to CEPDM.ACBrCEP1.Enderecos.Count - 1 do
          AResposta := AResposta + RespostaItensConsulta(I);

        MoverStringParaPChar(AResposta, sResposta, esTamanho);

        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        CEPDM.Destravar;
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

{%endregion}

end.

