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

unit ACBrLibIBGEClass;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibIBGEDataModule, ACBrIBGE;

type

  { TACBrLibIBGE }

  TACBrLibIBGE = class(TACBrLib)
  private
    FIBGEDM: TLibIBGEDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property IBGEDM: TLibIBGEDM read FIBGEDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function IBGE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function IBGE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function IBGE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function IBGE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function IBGE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function IBGE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function IBGE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function IBGE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function IBGE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region IBGE}
function RespostaItensConsulta(ItemID: integer = 0): String;

function IBGE_BuscarPorCodigo(const ACodMun: Integer; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function IBGE_BuscarPorNome(const eCidade, eUF: PChar; const Exata: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibIBGEConsts, ACBrLibConfig, ACBrLibIBGEConfig,
  ACBrLibResposta, ACBrLibIBGERespostas;

{ TACBrLibIBGE }

constructor TACBrLibIBGE.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibIBGENome;
  fpVersao := CLibIBGEVersao;

  FIBGEDM := TLibIBGEDM.Create(nil);
end;

destructor TACBrLibIBGE.Destroy;
begin
  FIBGEDM.Free;
  inherited Destroy;
end;

procedure TACBrLibIBGE.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibIBGE.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibIBGE.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibIBGEConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibIBGE.Executar;
begin
  inherited Executar;
  FIBGEDM.AplicarConfiguracoes;
end;

{%region IBGE}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function IBGE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function IBGE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function IBGE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function IBGE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function IBGE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function IBGE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function IBGE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function IBGE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function IBGE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;
{%endregion}

{%region IBGE}
function RespostaItensConsulta(ItemID: integer): String;
var
  Resp: TLibIBGEResposta;
begin
  Resp := TLibIBGEResposta.Create(
          CSessaoRespConsulta + IntToStr(ItemID +1), resINI);
  try
    with TACBrLibIBGE(pLib).IBGEDM.ACBrIBGE1.Cidades[ItemID] do
    begin
      Resp.UF := UF;
      Resp.CodUF := IntToStr(CodUF);
      Resp.Municipio := Municipio;
      Resp.CodMunicipio := IntToStr(CodMunicipio);
      Resp.Area := FloatToStr(Area);

      Result := Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

function IBGE_BuscarPorCodigo(const ACodMun: Integer; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AResposta: String;
  I: Integer;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('IBGE_BuscarPorCodigo( ' + IntToStr(ACodMun) + ' )', logCompleto, True)
    else
      pLib.GravarLog('IBGE_BuscarPorCodigo', logNormal);

    with TACBrLibIBGE(pLib) do
    begin
      IBGEDM.Travar;
      try
        IBGEDM.ACBrIBGE1.BuscarPorCodigo(ACodMun);
        AResposta := '';

        if IBGEDM.ACBrIBGE1.Cidades.Count < 1 then
           raise Exception.Create( 'Nenhuma Cidade encontrada' );

        for I := 0 to IBGEDM.ACBrIBGE1.Cidades.Count - 1 do
          AResposta := AResposta + RespostaItensConsulta(I);

        MoverStringParaPChar(AResposta, sResposta, esTamanho);

        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        IBGEDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function IBGE_BuscarPorNome(const eCidade, eUF: PChar; const Exata: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ACidade: AnsiString;
  AUF: AnsiString;
  AResposta: String;
  I: Integer;
begin
  try
    VerificarLibInicializada;
    ACidade := AnsiString(eCidade);
    AUF := AnsiString(eUF);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('IBGE_BuscarPorNome( ' + ACidade + ',' + AUF +
                              BoolToStr(Exata, False) + ' )', logCompleto, True)
    else
      pLib.GravarLog('IBGE_BuscarPorNome', logNormal);

    with TACBrLibIBGE(pLib) do
    begin
      IBGEDM.Travar;
      try
        IBGEDM.ACBrIBGE1.BuscarPorNome(ACidade, AUF, Exata);
        AResposta := '';

        if IBGEDM.ACBrIBGE1.Cidades.Count < 1 then
           raise Exception.Create( 'Nenhuma Cidade encontrada' );

        for I := 0 to IBGEDM.ACBrIBGE1.Cidades.Count - 1 do
          AResposta := AResposta + RespostaItensConsulta(I);

        MoverStringParaPChar(AResposta, sResposta, esTamanho);

        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        IBGEDM.Destravar;
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

