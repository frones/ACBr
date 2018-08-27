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

unit ACBrLibCHQClass;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibCHQDataModule, ACBrCHQ;

type

  { TACBrLibCHQ }

  TACBrLibCHQ = class(TACBrLib)
  private
    FCHQDM: TLibCHQDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property CHQDM: TLibCHQDM read FCHQDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function CHQ_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Cheque}
procedure StringToMemo( AString : AnsiString; Memo : TStringList );

function CHQ_Ativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_Desativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_ImprimirCheque: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_ImprimirLinha(const eLinha: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_ImprimirVerso(const eLinhas: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_TravarCheque: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_DestravarCheque: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_SetBanco(const eBanco: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_SetValor(const Valor: Double): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_SetData(const Data: TDateTime): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_SetCidade(const eCidade: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_SetFavorecido(const eFavorecido: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_SetObservacao(const eObservacao: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CHQ_SetBomPara(const BomPara: TDateTime): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibCHQConsts, ACBrLibConfig, ACBrLibCHQConfig;

{ TACBrLibCHQ }

constructor TACBrLibCHQ.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibCHQNome;
  fpVersao := CLibCHQVersao;

  FCHQDM := TLibCHQDM.Create(nil);
end;

destructor TACBrLibCHQ.Destroy;
begin
  FCHQDM.Free;
  inherited Destroy;
end;

procedure TACBrLibCHQ.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibCHQ.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibCHQ.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibCHQConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibCHQ.Executar;
begin
  inherited Executar;
  FCHQDM.AplicarConfiguracoes;
end;

{%region CHQ}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function CHQ_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function CHQ_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function CHQ_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function CHQ_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function CHQ_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function CHQ_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function CHQ_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function CHQ_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function CHQ_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;
{%endregion}

{%region Cheque}
procedure StringToMemo( AString : AnsiString; Memo : TStringList );
begin
  AString   := StringReplace(AString,#13+#10,'|',[rfReplaceAll]) ;
  AString   := StringReplace(AString,#10,'|',[rfReplaceAll]) ;
  Memo.Text := StringReplace(AString,'|',sLineBreak,[rfReplaceAll]) ;
end ;

function CHQ_Ativar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('CHQ_Ativar', logNormal);

    with TACBrLibCHQ(pLib) do
    begin
      CHQDM.Travar;
      try
        CHQDM.ACBrCHQ1.Ativar;
        Result := SetRetorno(ErrOK);
      finally
        CHQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CHQ_Desativar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('CHQ_Desativar', logNormal);

    with TACBrLibCHQ(pLib) do
    begin
      CHQDM.Travar;
      try
        CHQDM.ACBrCHQ1.Desativar;
        Result := SetRetorno(ErrOK);
      finally
        CHQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CHQ_ImprimirCheque: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('CHQ_ImprimirCheque', logNormal);

    with TACBrLibCHQ(pLib) do
    begin
      CHQDM.Travar;
      try
        CHQDM.ACBrCHQ1.ImprimirCheque;
        Result := SetRetorno(ErrOK);
      finally
        CHQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CHQ_ImprimirLinha(const eLinha: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ALinha: AnsiString;
begin
  try
    VerificarLibInicializada;
    ALinha := AnsiString(eLinha);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CHQ_ImprimirLinha( ' + ALinha + ' )', logCompleto, True)
    else
      pLib.GravarLog('CHQ_ImprimirLinha', logNormal);

    with TACBrLibCHQ(pLib) do
    begin
      CHQDM.Travar;
      try
        CHQDM.ACBrCHQ1.ImprimirLinha(ALinha);
        Result := SetRetorno(ErrOK);
      finally
        CHQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CHQ_ImprimirVerso(const eLinhas: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ALinha: AnsiString;
  Linhas: TStringList;
begin
  try
    VerificarLibInicializada;
    ALinha := AnsiString(eLinhas);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CHQ_ImprimirVerso( ' + ALinha + ' )', logCompleto, True)
    else
      pLib.GravarLog('CHQ_ImprimirVerso', logNormal);

    with TACBrLibCHQ(pLib) do
    begin
      CHQDM.Travar;
      Linhas := TStringList.Create;
      try
        StringToMemo( ALinha, Linhas ); {Linha separadas por | (pipe)}
        CHQDM.ACBrCHQ1.ImprimirVerso(Linhas);
        Result := SetRetorno(ErrOK);
      finally
        Linhas.Free;
        CHQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CHQ_TravarCheque: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('CHQ_TravarCheque', logNormal);

    with TACBrLibCHQ(pLib) do
    begin
      CHQDM.Travar;
      try
        CHQDM.ACBrCHQ1.TravarCheque;
        Result := SetRetorno(ErrOK);
      finally
        CHQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CHQ_DestravarCheque: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('CHQ_DestravarCheque', logNormal);

    with TACBrLibCHQ(pLib) do
    begin
      CHQDM.Travar;
      try
        CHQDM.ACBrCHQ1.DestravarCheque;
        Result := SetRetorno(ErrOK);
      finally
        CHQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CHQ_SetBanco(const eBanco: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ABanco: AnsiString;
begin
  try
    VerificarLibInicializada;
    ABanco := AnsiString(eBanco);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CHQ_SetBanco( ' + ABanco + ' )', logCompleto, True)
    else
      pLib.GravarLog('CHQ_SetBanco', logNormal);

    with TACBrLibCHQ(pLib) do
    begin
      CHQDM.Travar;
      try
        CHQDM.ACBrCHQ1.Banco := ABanco;
        Result := SetRetorno(ErrOK);
      finally
        CHQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CHQ_SetValor(const Valor: Double): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CHQ_SetValor( ' + FloatToStr(Valor) + ' )', logCompleto, True)
    else
      pLib.GravarLog('CHQ_SetValor', logNormal);

    with TACBrLibCHQ(pLib) do
    begin
      CHQDM.Travar;
      try
        CHQDM.ACBrCHQ1.Valor := Valor;
        Result := SetRetorno(ErrOK);
      finally
        CHQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CHQ_SetData(const Data: TDateTime): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CHQ_SetData( ' + DateTimeToStr(Data) + ' )', logCompleto, True)
    else
      pLib.GravarLog('CHQ_SetData', logNormal);

    with TACBrLibCHQ(pLib) do
    begin
      CHQDM.Travar;
      try
        CHQDM.ACBrCHQ1.Data := Data;
        Result := SetRetorno(ErrOK);
      finally
        CHQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CHQ_SetCidade(const eCidade: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ACidade: AnsiString;
begin
  try
    VerificarLibInicializada;
    ACidade := AnsiString(eCidade);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CHQ_SetCidade( ' + ACidade + ' )', logCompleto, True)
    else
      pLib.GravarLog('CHQ_SetCidade', logNormal);

    with TACBrLibCHQ(pLib) do
    begin
      CHQDM.Travar;
      try
        CHQDM.ACBrCHQ1.Cidade := ACidade;
        Result := SetRetorno(ErrOK);
      finally
        CHQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CHQ_SetFavorecido(const eFavorecido: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AFavorecido: AnsiString;
begin
  try
    VerificarLibInicializada;
    AFavorecido := AnsiString(eFavorecido);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CHQ_SetFavorecido( ' + AFavorecido + ' )', logCompleto, True)
    else
      pLib.GravarLog('CHQ_SetFavorecido', logNormal);

    with TACBrLibCHQ(pLib) do
    begin
      CHQDM.Travar;
      try
        CHQDM.ACBrCHQ1.Favorecido := AFavorecido;
        Result := SetRetorno(ErrOK);
      finally
        CHQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CHQ_SetObservacao(const eObservacao: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AObservacao: AnsiString;
begin
  try
    VerificarLibInicializada;
    AObservacao := AnsiString(eObservacao);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CHQ_SetObservacao( ' + AObservacao + ' )', logCompleto, True)
    else
      pLib.GravarLog('CHQ_SetObservacao', logNormal);

    with TACBrLibCHQ(pLib) do
    begin
      CHQDM.Travar;
      try
        CHQDM.ACBrCHQ1.Observacao := AObservacao;
        Result := SetRetorno(ErrOK);
      finally
        CHQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function CHQ_SetBomPara(const BomPara: TDateTime): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('CHQ_SetBomPara( ' + DateTimeToStr(BomPara) + ' )', logCompleto, True)
    else
      pLib.GravarLog('CHQ_SetBomPara', logNormal);

    with TACBrLibCHQ(pLib) do
    begin
      CHQDM.Travar;
      try
        CHQDM.ACBrCHQ1.BomPara := BomPara;
        Result := SetRetorno(ErrOK);
      finally
        CHQDM.Destravar;
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

