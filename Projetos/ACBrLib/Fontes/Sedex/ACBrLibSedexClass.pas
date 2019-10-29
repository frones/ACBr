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

unit ACBrLibSedexClass;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrUtil, ACBrLibComum, ACBrLibSedexDataModule, ACBrSedex;

type

  { TACBrLibSedex }

  TACBrLibSedex = class(TACBrLib)
  private
    FSedexDM: TLibSedexDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property SedexDM: TLibSedexDM read FSedexDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function Sedex_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Sedex_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Sedex_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Sedex_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Sedex_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Sedex_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Sedex_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Sedex_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Sedex_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Sedex}
function RespostaItensRastreio(ItemID: integer = 0): String;

function Sedex_LerArqIni(const eArqIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Sedex_Consultar(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Sedex_Rastrear(const eCodRastreio: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibSedexConsts, ACBrLibConfig, ACBrLibSedexConfig,
  ACBrLibResposta, ACBrLibSedexRespostas;

{ TACBrLibSedex }

constructor TACBrLibSedex.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FSedexDM := TLibSedexDM.Create(nil);
end;

destructor TACBrLibSedex.Destroy;
begin
  FSedexDM.Free;
  inherited Destroy;
end;

procedure TACBrLibSedex.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibSedex.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibSedex.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibSedexConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibSedex.Executar;
begin
  inherited Executar;
  FSedexDM.AplicarConfiguracoes;
end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function Sedex_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function Sedex_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function Sedex_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function Sedex_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function Sedex_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function Sedex_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function Sedex_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function Sedex_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function Sedex_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;
{%endregion}

{%region Sedex}
function RespostaItensRastreio(ItemID: integer): String;
var
  Resp: TLibSedexRastreio;
begin
  Resp := TLibSedexRastreio.Create(
          CSessaoRespRastreio + Trim(IntToStrZero(ItemID +1, 2)), pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibSedex(pLib).SedexDM.ACBrSedex1.retRastreio[ItemID] do
    begin
      Resp.DataHora := DataHora;
      Resp.Local := Local;
      Resp.Situacao := Situacao;
      Resp.Observacao := Observacao;

      result := Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

function Sedex_LerArqIni(const eArqIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AArqIni: String;
begin
  try
    VerificarLibInicializada;
    AArqIni := AnsiString(eArqIni);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('Sedex_LerArqIni( ' + AArqIni + ' )', logCompleto, True)
    else
      pLib.GravarLog('Sedex_LerArqIni', logNormal);

    with TACBrLibSedex(pLib) do
    begin
      SedexDM.Travar;
      try
        SedexDM.ACBrSedex1.LerArqIni(AArqIni);
        Result := SetRetorno(ErrOK);
      finally
        SedexDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function Sedex_Consultar(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resp: TLibSedexConsulta;
  AResposta: String;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('Sedex_Consultar', logNormal);

    with TACBrLibSedex(pLib) do
    begin
      SedexDM.Travar;
      Resp := TLibSedexConsulta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
      try
        SedexDM.ACBrSedex1.Consultar;
        AResposta := '';

        with SedexDM.ACBrSedex1 do
        begin
          Resp.CodigoServico := retCodigoServico;
          Resp.Valor := retValor;
          Resp.PrazoEntrega := retPrazoEntrega;
          Resp.ValorSemAdicionais := retValorSemAdicionais;
          Resp.ValorMaoPropria := retValorMaoPropria;
          Resp.ValorAvisoRecebimento := retValorAvisoRecebimento;
          Resp.ValorValorDeclarado := retValorValorDeclarado;
          Resp.EntregaDomiciliar := retEntregaDomiciliar;
          Resp.EntregaSabado := retEntregaSabado;
          Resp.Erro := retErro;
          Resp.MsgErro := retMsgErro;

          AResposta := retMsgErro + sLineBreak;
          AResposta := AResposta + Resp.Gerar;
        end;

        MoverStringParaPChar(AResposta, sResposta, esTamanho);

        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        Resp.Free;
        SedexDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function Sedex_Rastrear(const eCodRastreio: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ACodRastreio: AnsiString;
  AResposta: String;
  I: Integer;
begin
  try
    VerificarLibInicializada;
    ACodRastreio := AnsiString(eCodRastreio);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('Sedex_Rastrear( ' + ACodRastreio + ' )', logCompleto, True)
    else
      pLib.GravarLog('Sedex_Rastrear', logNormal);

    with TACBrLibSedex(pLib) do
    begin
      SedexDM.Travar;
      try
        SedexDM.ACBrSedex1.Rastrear(ACodRastreio);
        AResposta := '';

        for I := 0 to SedexDM.ACBrSedex1.retRastreio.Count - 1 do
          AResposta := AResposta + RespostaItensRastreio(I);

        MoverStringParaPChar(AResposta, sResposta, esTamanho);

        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        SedexDM.Destravar;
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

