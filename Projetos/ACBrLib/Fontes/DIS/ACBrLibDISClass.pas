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

unit ACBrLibDISClass;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibDISDataModule, ACBrDIS;

type

  { TACBrLibDIS }

  TACBrLibDIS = class(TACBrLib)
  private
    FDISDM: TLibDISDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property DISDM: TLibDISDM read FDISDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function DIS_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Diplay}
function DIS_Ativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_Desativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_LimparDisplay: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_LimparLinha(const Linha: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_PosicionarCursor(const Linha, Coluna: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_Escrever(const eTexto: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_ExibirLinha(const Linha: Integer ; const eTexto: PChar;
  const Alinhamento, Efeito: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_RolarLinha(const Linha, Efeito: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_Parar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_Continuar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_PararLinha(const Linha: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function DIS_ContinuarLinha(const Linha: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibDISConsts, ACBrLibConfig, ACBrLibDISConfig;

{ TACBrLibDIS }

constructor TACBrLibDIS.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibDISNome;
  fpVersao := CLibDISVersao;

  FDISDM := TLibDISDM.Create(nil);
end;

destructor TACBrLibDIS.Destroy;
begin
  FDISDM.Free;
  inherited Destroy;
end;

procedure TACBrLibDIS.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibDIS.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibDIS.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibDISConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibDIS.Executar;
begin
  inherited Executar;
  FDISDM.AplicarConfiguracoes;
end;

{%region DIS}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function DIS_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function DIS_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function DIS_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function DIS_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function DIS_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function DIS_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function DIS_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function DIS_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function DIS_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;
{%endregion}

{%region Diplay}
function DIS_Ativar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('DIS_Ativar', logNormal);

    with TACBrLibDIS(pLib) do
    begin
      DISDM.Travar;
      try
        DISDM.ACBrDIS1.Ativar;
        Result := SetRetorno(ErrOK);
      finally
        DISDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function DIS_Desativar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('DIS_Desativar', logNormal);

    with TACBrLibDIS(pLib) do
    begin
      DISDM.Travar;
      try
        DISDM.ACBrDIS1.Desativar;
        Result := SetRetorno(ErrOK);
      finally
        DISDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function DIS_LimparDisplay: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('DIS_LimparDisplay', logNormal);

    with TACBrLibDIS(pLib) do
    begin
      DISDM.Travar;
      try
        DISDM.ACBrDIS1.LimparDisplay;
        Result := SetRetorno(ErrOK);
      finally
        DISDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function DIS_LimparLinha(const Linha: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('DIS_LimparLinha( ' + IntToStr(Linha) + ' )', logCompleto, True)
    else
      pLib.GravarLog('DIS_LimparLinha', logNormal);

    with TACBrLibDIS(pLib) do
    begin
      DISDM.Travar;
      try
        DISDM.ACBrDIS1.LimparLinha(Linha);
        Result := SetRetorno(ErrOK);
      finally
        DISDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function DIS_PosicionarCursor(const Linha, Coluna: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('DIS_PosicionarCursor( ' + IntToStr(Linha) + ',' +
               IntToStr(Coluna) + ' )', logCompleto, True)
    else
      pLib.GravarLog('DIS_PosicionarCursor', logNormal);

    with TACBrLibDIS(pLib) do
    begin
      DISDM.Travar;
      try
        DISDM.ACBrDIS1.PosicionarCursor(Linha, Coluna);
        Result := SetRetorno(ErrOK);
      finally
        DISDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function DIS_Escrever(const eTexto: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ATexto: AnsiString;
begin
  try
    VerificarLibInicializada;
    ATexto := AnsiString(eTexto);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('DIS_Escrever( ' + ATexto + ' )', logCompleto, True)
    else
      pLib.GravarLog('DIS_Escrever', logNormal);

    with TACBrLibDIS(pLib) do
    begin
      DISDM.Travar;
      try
        DISDM.ACBrDIS1.Escrever(ATexto);
        Result := SetRetorno(ErrOK);
      finally
        DISDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function DIS_ExibirLinha(const Linha: Integer ; const eTexto: PChar;
  const Alinhamento, Efeito: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ATexto: AnsiString;
begin
  try
    VerificarLibInicializada;
    ATexto := AnsiString(eTexto);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('DIS_ExibirLinha( ' + IntToStr(Linha) + ',' + ATexto + ',' +
       IntToStr(Alinhamento) + ',' + IntToStr(Efeito) + ' )', logCompleto, True)
    else
      pLib.GravarLog('DIS_ExibirLinha', logNormal);

    with TACBrLibDIS(pLib) do
    begin
      DISDM.Travar;
      try
        if (Alinhamento = -1) and (Efeito = -1) then
          DISDM.ACBrDIS1.ExibirLinha(Linha, ATexto)
        else
          if (Alinhamento > -1) and (Efeito = -1) then
            DISDM.ACBrDIS1.ExibirLinha(Linha, ATexto, TACBrDISAlinhamento(Alinhamento))
          else
            if (Alinhamento = -1) and (Efeito > -1) then
              DISDM.ACBrDIS1.ExibirLinha(Linha, ATexto, TACBrDISEfeitoExibir(Efeito));

        Result := SetRetorno(ErrOK);
      finally
        DISDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function DIS_RolarLinha(const Linha, Efeito: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('DIS_RolarLinha( ' + IntToStr(Linha) + ',' +
           IntToStr(Efeito) + ' )', logCompleto, True)
    else
      pLib.GravarLog('DIS_RolarLinha', logNormal);

    with TACBrLibDIS(pLib) do
    begin
      DISDM.Travar;
      try
        DISDM.ACBrDIS1.RolarLinha(Linha, TACBrDISEfeitoRolar(Efeito));
        Result := SetRetorno(ErrOK);
      finally
        DISDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function DIS_Parar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('DIS_Parar', logNormal);

    with TACBrLibDIS(pLib) do
    begin
      DISDM.Travar;
      try
        DISDM.ACBrDIS1.Parar;
        Result := SetRetorno(ErrOK);
      finally
        DISDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function DIS_Continuar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('DIS_Continuar', logNormal);

    with TACBrLibDIS(pLib) do
    begin
      DISDM.Travar;
      try
        DISDM.ACBrDIS1.Continuar;
        Result := SetRetorno(ErrOK);
      finally
        DISDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function DIS_PararLinha(const Linha: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('DIS_PararLinha( ' + IntToStr(Linha) + ' )', logCompleto, True)
    else
      pLib.GravarLog('DIS_PararLinha', logNormal);

    with TACBrLibDIS(pLib) do
    begin
      DISDM.Travar;
      try
        DISDM.ACBrDIS1.PararLinha(Linha);
        Result := SetRetorno(ErrOK);
      finally
        DISDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function DIS_ContinuarLinha(const Linha: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('DIS_ContinuarLinha( ' + IntToStr(Linha) + ' )', logCompleto, True)
    else
      pLib.GravarLog('DIS_ContinuarLinha', logNormal);

    with TACBrLibDIS(pLib) do
    begin
      DISDM.Travar;
      try
        DISDM.ACBrDIS1.ContinuarLinha(Linha);
        Result := SetRetorno(ErrOK);
      finally
        DISDM.Destravar;
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

