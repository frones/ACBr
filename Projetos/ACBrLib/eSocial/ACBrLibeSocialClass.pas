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

unit ACBrLibeSocialClass;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrUtil, ACBrLibComum, ACBrLibeSocialDataModule; //, ACBreSocial;

type

  { TACBrLibeSocial }

  TACBrLibeSocial = class(TACBrLib)
  private
    FeSocialDM: TLibeSocialDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property eSocialDM: TLibeSocialDM read FeSocialDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function eSocial_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function eSocial_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function eSocial_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function eSocial_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function eSocial_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function eSocial_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function eSocial_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function eSocial_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function eSocial_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region eSocial}
function RespostaItensRastreio(ItemID: integer = 0): String;

function eSocial_LerArqIni(const eArqIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function eSocial_Enviar(const Agrupo: Integer; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function eSocial_Consultar(const eProtocolo, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibeSocialConsts, ACBrLibConfig, ACBrLibeSocialConfig,
  ACBrLibResposta, ACBrLibeSocialRespostas, pcesConversaoeSocial;

{ TACBrLibeSocial }

constructor TACBrLibeSocial.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibeSocialNome;
  fpVersao := CLibeSocialVersao;

  FeSocialDM := TLibeSocialDM.Create(nil);
end;

destructor TACBrLibeSocial.Destroy;
begin
  FeSocialDM.Free;
  inherited Destroy;
end;

procedure TACBrLibeSocial.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibeSocial.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibeSocial.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibeSocialConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibeSocial.Executar;
begin
  inherited Executar;
  FeSocialDM.AplicarConfiguracoes;
end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function eSocial_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function eSocial_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function eSocial_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function eSocial_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function eSocial_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function eSocial_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function eSocial_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function eSocial_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function eSocial_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;
{%endregion}

{%region eSocial}
function RespostaItensRastreio(ItemID: integer): String;
var
  Resp: TLibeSocialRastreio;
begin
  Resp := TLibeSocialRastreio.Create(
          CSessaoRespRastreio + Trim(IntToStrZero(ItemID +1, 2)), resINI);
  try
    with TACBrLibeSocial(pLib).eSocialDM.ACBreSocial1.retRastreio[ItemID] do
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

function eSocial_LerArqIni(const eArqIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Ok: Boolean;
  AArqIni: String;
begin
  try
    VerificarLibInicializada;
    AArqIni := AnsiString(eArqIni);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('eSocial_LerArqIni( ' + AArqIni + ' )', logCompleto, True)
    else
      pLib.GravarLog('eSocial_LerArqIni', logNormal);

    with TACBrLibeSocial(pLib) do
    begin
      eSocialDM.Travar;
      try
        Ok := eSocialDM.ACBreSocial1.LerArqIni(AArqIni);
        Result := SetRetorno(ErrOK);
      finally
        eSocialDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function eSocial_Enviar(const Agrupo: Integer; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AResposta: String;
  I: Integer;
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('eSocial_Enviar( ' + IntToStr(Agrupo) + ' )', logCompleto, True)
    else
      pLib.GravarLog('eSocial_Enviar', logNormal);

    with TACBrLibeSocial(pLib) do
    begin
      eSocialDM.Travar;
      try
        eSocialDM.ACBreSocial1.Enviar(TeSocialGrupo(Agrupo));
        AResposta := '';

//        for I := 0 to eSocialDM.ACBreSocial1.retRastreio.Count - 1 do
//          AResposta := AResposta + RespostaItensRastreio(I);

//        MoverStringParaPChar(AResposta, sResposta, esTamanho);

        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        eSocialDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function eSocial_Consultar(const eProtocolo, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Resp: TLibeSocialConsulta;
  AProtocolo: String;
  AResposta: String;
begin
  try
    VerificarLibInicializada;
    AProtocolo := AnsiString(eProtocolo);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('eSocial_Consultar( ' + AProtocolo + ' )', logCompleto, True)
    else
      pLib.GravarLog('eSocial_Consultar', logNormal);

    with TACBrLibeSocial(pLib) do
    begin
      eSocialDM.Travar;
      Resp := TLibeSocialConsulta.Create(resINI);
      try
        eSocialDM.ACBreSocial1.Consultar(AProtocolo);
        AResposta := '';
        {
        with eSocialDM.ACBreSocial1 do
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
        }
        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        Resp.Free;
        eSocialDM.Destravar;
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

