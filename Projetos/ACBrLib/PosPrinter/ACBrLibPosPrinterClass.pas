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

unit ACBrLibPosPrinterClass;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibPosPrinterDataModule;

type

  { TACBrLibPosPrinter }

  TACBrLibPosPrinter = class(TACBrLib)
  private
    FPosDM: TLibPosPrinterDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property PosDM: TLibPosPrinterDM read FPosDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function POS_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Ativar}
function POS_Ativar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_Desativar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Impressão}
function POS_Imprimir(eString: PChar; PulaLinha, DecodificarTags,
  CodificarPagina: Boolean; Copias: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ImprimirLinha(eString: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ImprimirCmd(eComando: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ImprimirTags: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Diversos}
function POS_TxRx(eCmd: PChar; BytesToRead: Byte; ATimeOut: Integer; WaitForTerminator: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_Zerar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_InicializarPos: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_Reset: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_PularLinhas(NumLinhas: Integer): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_CortarPapel(Parcial: Boolean): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_AbrirGaveta: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_LerInfoImpressora(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_LerStatusImpressora(Tentativas: Integer; var status: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_RetornarTags(const sResposta: PChar; var esTamanho: longint; IncluiAjuda: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibPosPrinterConsts, ACBrLibConfig, ACBrLibPosPrinterConfig,
  ACBrPosPrinter;

{ TACBrLibPosPrinter }

constructor TACBrLibPosPrinter.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibPosPrinterNome;
  fpVersao := CLibPosPrinterVersao;

  FPosDM := TLibPosPrinterDM.Create(nil);
end;

destructor TACBrLibPosPrinter.Destroy;
begin
  FPosDM.Free;
  inherited Destroy;
end;

procedure TACBrLibPosPrinter.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibPosPrinter.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibPosPrinter.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibPosPrinterConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibPosPrinter.Executar;
begin
  inherited Executar;
  FPosDM.AplicarConfiguracoes;
end;

{%region PosPrinter}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function POS_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function POS_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function POS_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function POS_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function POS_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := POS_UltimoRetorno(sMensagem, esTamanho);
end;

function POS_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function POS_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function POS_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function POS_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;
{%endregion}

{%region Ativar}
function POS_Ativar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('POS_Ativar', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      try
        PosDM.ACBrPosPrinter1.Ativar;
        Result := SetRetorno(ErrOK);
      finally
        PosDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function POS_Desativar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('POS_Desativar', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      try
        PosDM.ACBrPosPrinter1.Desativar;
        Result := SetRetorno(ErrOK);
      finally
        PosDM.Destravar;
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

{%region Impressão}
function POS_Imprimir(eString: PChar; PulaLinha, DecodificarTags,
  CodificarPagina: Boolean; Copias: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AString: AnsiString;
begin
  try
    VerificarLibInicializada;
    AString := AnsiString(eString);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('POS_Imprimir(' + AString + ',' + BoolToStr(PulaLinha, True) + ',' +
        BoolToStr(DecodificarTags, True) + ',' + BoolToStr(CodificarPagina, True) + ',' +
        IntToStr(Copias) +' )', logCompleto, True)
    else
      pLib.GravarLog('POS_Imprimir', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      try
        PosDM.ACBrPosPrinter1.Imprimir(AString, PulaLinha, DecodificarTags, CodificarPagina, Copias);
        Result := SetRetorno(ErrOK);
      finally
        PosDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function POS_ImprimirLinha(eString: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AString: AnsiString;
begin
  try
    VerificarLibInicializada;
    AString := AnsiString(eString);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('POS_ImprimirLinha(' + AString + ')', logCompleto, True)
    else
      pLib.GravarLog('POS_ImprimirLinha', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      try
        PosDM.ACBrPosPrinter1.ImprimirLinha(AString);
        Result := SetRetorno(ErrOK);
      finally
        PosDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function POS_ImprimirCmd(eComando: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AComando: AnsiString;
begin
  try
    VerificarLibInicializada;
    AComando := AnsiString(eComando);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('POS_ImprimirCmd(' + AComando + ')', logCompleto, True)
    else
      pLib.GravarLog('POS_ImprimirCmd', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      try
        PosDM.ACBrPosPrinter1.ImprimirCmd(AComando);
        Result := SetRetorno(ErrOK);
      finally
        PosDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function POS_ImprimirTags: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('POS_ImprimirTags', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      try
        PosDM.ACBrPosPrinter1.ImprimirTags;
        Result := SetRetorno(ErrOK);
      finally
        PosDM.Destravar;
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

{%region Diversos}
function POS_TxRx(eCmd: PChar; BytesToRead: Byte; ATimeOut: Integer; WaitForTerminator: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ACmd, Resposta: AnsiString;
begin
  try
    VerificarLibInicializada;
    ACmd := AnsiString(eCmd);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('POS_TxRx(' + ACmd + ',' + IntToStr(BytesToRead) + ',' +
        IntToStr(ATimeOut) + ',' + BoolToStr(WaitForTerminator, True) + ' )', logCompleto, True)
    else
      pLib.GravarLog('POS_TxRx', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      try
        Resposta := '';
        Resposta := PosDM.ACBrPosPrinter1.TxRx(ACmd, BytesToRead, ATimeOut, WaitForTerminator);
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        PosDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function POS_Zerar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('POS_Zerar', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      try
        PosDM.ACBrPosPrinter1.Zerar;
        Result := SetRetorno(ErrOK);
      finally
        PosDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function POS_InicializarPos: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('POS_Inicializar', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      try
        PosDM.ACBrPosPrinter1.Inicializar;
        Result := SetRetorno(ErrOK);
      finally
        PosDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function POS_Reset: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('POS_Reset', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      try
        PosDM.ACBrPosPrinter1.Reset;
        Result := SetRetorno(ErrOK);
      finally
        PosDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function POS_PularLinhas(NumLinhas: Integer): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('POS_PularLinhas(' + IntToStr(NumLinhas) + ' )', logCompleto, True)
    else
      pLib.GravarLog('POS_PularLinhas', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      try
        PosDM.ACBrPosPrinter1.PularLinhas(NumLinhas);
        Result := SetRetorno(ErrOK);
      finally
        PosDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function POS_CortarPapel(Parcial: Boolean): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('POS_CortarPapel(' + BoolToStr(Parcial, True) + ' )', logCompleto, True)
    else
      pLib.GravarLog('POS_CortarPapel', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      try
        PosDM.ACBrPosPrinter1.CortarPapel(Parcial);
        Result := SetRetorno(ErrOK);
      finally
        PosDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function POS_AbrirGaveta: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('POS_AbrirGaveta', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      try
        PosDM.ACBrPosPrinter1.AbrirGaveta;
        Result := SetRetorno(ErrOK);
      finally
        PosDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function POS_LerInfoImpressora(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Resposta: string;
begin
   try
    VerificarLibInicializada;

    pLib.GravarLog('POS_LerInfoImpressora', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      try
        Resposta := '';
        Resposta := PosDM.ACBrPosPrinter1.LerInfoImpressora;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        PosDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function POS_LerStatusImpressora(Tentativas: Integer; var status: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  RetStatus: TACBrPosPrinterStatus;
  i: TACBrPosTipoStatus;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('POS_LerStatusImpressora(' + IntToStr(Tentativas) + ' )', logCompleto, True)
    else
      pLib.GravarLog('POS_LerStatusImpressora', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      status := 0;
      try
        RetStatus := PosDM.ACBrPosPrinter1.LerStatusImpressora(Tentativas);
        if RetStatus <> [] then
        begin
          for i := Low(TACBrPosTipoStatus) to High(TACBrPosTipoStatus) do
          begin
            if i in RetStatus then
              status := status + (1 << Ord(i));
          end;
        end;

        Result := SetRetorno(ErrOK);
      finally
        PosDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function POS_RetornarTags(const sResposta: PChar; var esTamanho: longint; IncluiAjuda: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  TagList: TStringList;
  Tags: string;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('POS_RetornarTags(' + BoolToStr(IncluiAjuda, True) + ' )', logCompleto, True)
    else
      pLib.GravarLog('POS_RetornarTags', logNormal);

    with TACBrLibPosPrinter(pLib) do
    begin
      PosDM.Travar;
      TagList := TStringList.Create;
      try
        PosDM.ACBrPosPrinter1.RetornarTags(TagList, IncluiAjuda);
        Tags := StringReplace(TagList.Text, sLineBreak, '|', [rfReplaceAll, rfIgnoreCase]);
        MoverStringParaPChar(Tags, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Tags);;
      finally
        PosDM.Destravar;
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

