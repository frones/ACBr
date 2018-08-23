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

unit ACBrLibBALClass;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibBALDataModule, ACBrBAL;

type

  { TACBrLibBAL }

  TACBrLibBAL = class(TACBrLib)
  private
    FBALDM: TLibBALDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property BALDM: TLibBALDM read FBALDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function BAL_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Balança}
function BAL_Ativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_Desativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_LePeso(MillisecTimeOut: Integer; var Peso: Double): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_SolicitarPeso: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_InterpretarRespostaPeso(eResposta: PChar; var Peso: Double): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibBALConsts, ACBrLibConfig, ACBrLibBALConfig;

{ TACBrLibBAL }

constructor TACBrLibBAL.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibBALNome;
  fpVersao := CLibBALVersao;

  FBALDM := TLibBALDM.Create(nil);
end;

destructor TACBrLibBAL.Destroy;
begin
  FBALDM.Free;
  inherited Destroy;
end;

procedure TACBrLibBAL.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibBAL.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibBAL.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibBALConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibBAL.Executar;
begin
  inherited Executar;
  FBALDM.AplicarConfiguracoes;
end;

{%region BAL}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function BAL_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function BAL_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function BAL_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function BAL_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function BAL_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function BAL_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function BAL_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function BAL_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function BAL_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;
{%endregion}

{%region Balanço}
function BAL_Ativar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('BAL_Ativar', logNormal);

    with TACBrLibBAL(pLib) do
    begin
      BALDM.Travar;
      try
        BALDM.ACBrBAL1.Ativar;
        Result := SetRetorno(ErrOK);
      finally
        BALDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BAL_Desativar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('BAL_Desativar', logNormal);

    with TACBrLibBAL(pLib) do
    begin
      BALDM.Travar;
      try
        BALDM.ACBrBAL1.Desativar;
        Result := SetRetorno(ErrOK);
      finally
        BALDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BAL_LePeso(MillisecTimeOut: Integer; var Peso: Double): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('BAL_LePeso( ' + IntToStr(MillisecTimeOut) + ' )', logCompleto, True)
    else
      pLib.GravarLog('BAL_LePeso', logNormal);

    with TACBrLibBAL(pLib) do
    begin
      BALDM.Travar;
      try
        Peso := BALDM.ACBrBAL1.LePeso(MillisecTimeOut);
        Result := SetRetorno(ErrOK);
      finally
        BALDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BAL_SolicitarPeso: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('BAL_SolicitarPeso', logNormal);

    with TACBrLibBAL(pLib) do
    begin
      BALDM.Travar;
      try
        BALDM.ACBrBAL1.SolicitarPeso;
        Result := SetRetorno(ErrOK);
      finally
        BALDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function BAL_InterpretarRespostaPeso(eResposta: PChar; var Peso: Double): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AResposta: AnsiString;
begin
  try
    VerificarLibInicializada;
    AResposta := AnsiString(eResposta);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('BAL_InterpretarRepostaPeso( ' + AResposta + ' )', logCompleto, True)
    else
      pLib.GravarLog('BAL_InterpretarRepostaPeso', logNormal);

    with TACBrLibBAL(pLib) do
    begin
      BALDM.Travar;
      try
        Peso := BALDM.ACBrBAL1.InterpretarRepostaPeso(AResposta);
        Result := SetRetorno(ErrOK);
      finally
        BALDM.Destravar;
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

