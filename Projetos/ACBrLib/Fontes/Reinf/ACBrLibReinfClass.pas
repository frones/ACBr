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

unit ACBrLibReinfClass;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrUtil, ACBrLibComum, ACBrLibReinfDataModule, ACBrReinf;

type

  { TACBrLibReinf }

  TACBrLibReinf = class(TACBrLib)
  private
    FReinfDM: TLibReinfDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property ReinfDM: TLibReinfDM read FReinfDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function Reinf_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Reinf_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Reinf_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Reinf_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Reinf_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Reinf_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Reinf_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Reinf_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Reinf_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Reinf}
function RespostaEnvio: String;
function RespostaEnvioOcorrencias(ACont: Integer): String;
function RespostaEnvioRetorno: String;
function RespostaEnvioideTransmissor: String;
function RespostaEnviostatus: String;
function RespostaEnvioevento(ACont: Integer): String;
function RespostaEnvioevtTotal(ACont: Integer): String;
function RespostaEnvioideEvento(ACont: Integer): String;
function RespostaEnvioideContri(ACont: Integer): String;
function RespostaEnvioideStatus(ACont: Integer): String;
function RespostaEnvioregOcorrs(ACont, ACont2: Integer): String;
function RespostaEnvioinfoRecEv(ACont: Integer): String;
function RespostaEnvioinfoTotal(ACont: Integer): String;
function RespostaEnvioRTom(ACont: Integer): String;
function RespostaEnvioinfoCRTom(ACont, ACont2: Integer): String;
function RespostaEnvioRPrest(ACont: Integer): String;
function RespostaEnvioRRecRepAD(ACont, ACont2: Integer): String;
function RespostaEnvioRComl(ACont, ACont2: Integer): String;
function RespostaEnvioRCPRB(ACont, ACont2: Integer): String;
function RespostaEnvioRRecEspetDest(ACont: Integer): String;

function RespostaConsulta: String;
function RespostaConsultaideEvento: String;
function RespostaConsultaideContri: String;
function RespostaConsultaideStatus: String;
function RespostaConsultaregOcorrs(ACont: Integer): String;
function RespostaConsultainfoRecEv: String;
function RespostaConsultainfoTotalContrib: String;
function RespostaConsultaRTom(ACont: Integer): String;
function RespostaConsultainfoCRTom(ACont, ACont2: Integer): String;
function RespostaConsultaRPrest(ACont: Integer): String;
function RespostaConsultaRRecRepAD(ACont: Integer): String;
function RespostaConsultaRComl(ACont: Integer): String;
function RespostaConsultaRCPRB(ACont: Integer): String;

function Reinf_LerArqIni(const eArqIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Reinf_Enviar(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Reinf_Consultar(const eProtocolo, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibReinfConsts, ACBrLibConfig, ACBrLibReinfConfig,
  ACBrLibResposta, ACBrLibReinfRespostas, pcnConversaoReinf;

{ TACBrLibReinf }

constructor TACBrLibReinf.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibReinfNome;
  fpVersao := CLibReinfVersao;

  FReinfDM := TLibReinfDM.Create(nil);
end;

destructor TACBrLibReinf.Destroy;
begin
  FReinfDM.Free;
  inherited Destroy;
end;

procedure TACBrLibReinf.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibReinf.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibReinf.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibReinfConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibReinf.Executar;
begin
  inherited Executar;
  FReinfDM.AplicarConfiguracoes;
end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function Reinf_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function Reinf_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function Reinf_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function Reinf_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function Reinf_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function Reinf_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function Reinf_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function Reinf_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function Reinf_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;
{%endregion}

{%region Reinf}
function RespostaEnvio: String;
var
  i, j: Integer;
  Retorno: String;
begin
  with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote do
  begin
    Retorno := RespostaEnvioRetorno;
    Retorno := Retorno + RespostaEnvioideTransmissor;
    Retorno := Retorno + RespostaEnviostatus;

    for i := 0 to Status.Ocorrencias.Count - 1 do
      Retorno := Retorno + RespostaEnvioOcorrencias(i);

    for i := 0 to evento.Count - 1 do
    begin
      Retorno := Retorno + RespostaEnvioevento(i);
      Retorno := Retorno + RespostaEnvioevtTotal(i);
      Retorno := Retorno + RespostaEnvioideEvento(i);
      Retorno := Retorno + RespostaEnvioideContri(i);
      Retorno := Retorno + RespostaEnvioideStatus(i);

      for j := 0 to evento.Items[i].evtTotal.IdeStatus.regOcorrs.Count -1 do
        Retorno := Retorno + RespostaEnvioregOcorrs(i, j);

      Retorno := Retorno + RespostaEnvioinfoRecEv(i);
      Retorno := Retorno + RespostaEnvioinfoTotal(i);

      Retorno := Retorno + RespostaEnvioRTom(i);

      for j := 0 to evento.Items[i].evtTotal.InfoTotal.RTom.infoCRTom.Count -1 do
        Retorno := Retorno + RespostaEnvioinfoCRTom(i, j);

      Retorno := Retorno + RespostaEnvioRPrest(i);

      for j := 0 to evento.Items[i].evtTotal.InfoTotal.RRecRepAD.Count -1 do
        Retorno := Retorno + RespostaEnvioRRecRepAD(i, j);

      for j := 0 to evento.Items[i].evtTotal.InfoTotal.RComl.Count -1 do
        Retorno := Retorno + RespostaEnvioRComl(i, j);

      for j := 0 to evento.Items[i].evtTotal.InfoTotal.RCPRB.Count -1 do
        Retorno := Retorno + RespostaEnvioRCPRB(i, j);

      Retorno := Retorno + RespostaEnvioRRecEspetDest(i);
    end;
  end;

  Result := Retorno;
end;

function RespostaEnvioRetorno: String;
var
  Resp: TEnvioResposta;
begin
  Resp := TEnvioResposta.Create(resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote do
    begin
      Resp.Id := Id;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioideTransmissor: String;
var
  Resp: TEnvioRespostaideTransmissor;
begin
  Resp := TEnvioRespostaideTransmissor.Create(resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote do
    begin
      Resp.IdTransmissor := ideTransmissor.IdTransmissor;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnviostatus: String;
var
  Resp: TEnvioRespostastatus;
begin
  Resp := TEnvioRespostastatus.Create(resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote do
    begin
      Resp.cdStatus    := Status.cdStatus;
      Resp.descRetorno := Status.descRetorno;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioOcorrencias(ACont: Integer): String;
var
  Resp: TEnvioRespostaOcorrencias;
begin
  Resp := TEnvioRespostaOcorrencias.Create(CSessaoRespEnvioocorrencias + IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote do
    begin
      Resp.tipo                 := Status.Ocorrencias.Items[ACont].tipo;
      Resp.localizacaoErroAviso := Status.Ocorrencias.Items[ACont].Localizacao;
      Resp.codigo               := Status.Ocorrencias.Items[ACont].Codigo;
      Resp.descricao            := Status.Ocorrencias.Items[ACont].Descricao;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioevento(ACont: Integer): String;
var
  Resp: TEnvioRespostaevento;
begin
  Resp := TEnvioRespostaevento.Create(CSessaoRespEnvioevento + IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote do
    begin
      Resp.Id := evento.Items[ACont].Id;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioevtTotal(ACont: Integer): String;
var
  Resp: TEnvioRespostaevtTotal;
begin
  Resp := TEnvioRespostaevtTotal.Create(CSessaoRespEnvioevtTotal + IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      Resp.Id := Id;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioideEvento(ACont: Integer): String;
var
  Resp: TRespostaideEvento;
begin
  Resp := TRespostaideEvento.Create(CSessaoRetornoideEvento + IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      Resp.perApur := IdeEvento.perApur;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioideContri(ACont: Integer): String;
var
  Resp: TRespostaideContri;
begin
  Resp := TRespostaideContri.Create(CSessaoRetornoideContri + IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      resp.tpInsc := TpInscricaoToStr(IdeContrib.TpInsc);
      resp.nrInsc := IdeContrib.nrInsc;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioideStatus(ACont: Integer): String;
var
  Resp: TRespostaideStatus;
begin
  Resp := TRespostaideStatus.Create(CSessaoRetornoideStatus + IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      resp.cdRetorno   := IdeStatus.cdRetorno;
      resp.descRetorno := IdeStatus.descRetorno;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioregOcorrs(ACont, ACont2: Integer): String;
var
  Resp: TRespostaregOcorrs;
begin
  Resp := TRespostaregOcorrs.Create(CSessaoRetornoideStatus +
                      IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      resp.tpOcorr        := IdeStatus.regOcorrs.Items[ACont2].tpOcorr;
      resp.localErroAviso := IdeStatus.regOcorrs.Items[ACont2].localErroAviso;
      resp.codResp        := IdeStatus.regOcorrs.Items[ACont2].codResp;
      resp.dscResp        := IdeStatus.regOcorrs.Items[ACont2].dscResp;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioinfoRecEv(ACont: Integer): String;
var
  Resp: TRespostainfoRecEv;
begin
  Resp := TRespostainfoRecEv.Create(CSessaoRetornoinfoRecEv + IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      resp.nrProtEntr := InfoRecEv.nrProtEntr;
      resp.dhProcess  := InfoRecEv.dhProcess;
      resp.tpEv       := InfoRecEv.tpEv;
      resp.idEv       := InfoRecEv.idEv;
      resp.hash       := InfoRecEv.hash;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioinfoTotal(ACont: Integer): String;
var
  Resp: TRespostainfoTotal_infoTotalContrib;
begin
  Resp := TRespostainfoTotal_infoTotalContrib.Create(CSessaoRespEnvioinfoTotal +
                                              IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      resp.nrRecArqBase := InfoTotal.nrRecArqBase;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioRTom(ACont: Integer): String;
var
  Resp: TRespostaRTom;
begin
  Resp := TRespostaRTom.Create(CSessaoRetornoRTom +
                                              IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      resp.cnpjPrestador     := InfoTotal.RTom.cnpjPrestador;
      resp.vlrTotalBaseRet   := InfoTotal.RTom.vlrTotalBaseRet;
      resp.vlrTotalRetPrinc  := InfoTotal.RTom.vlrTotalRetPrinc;
      resp.vlrTotalRetAdic   := InfoTotal.RTom.vlrTotalRetAdic;
      resp.vlrTotalNRetPrinc := InfoTotal.RTom.vlrTotalNRetPrinc;
      resp.vlrTotalNRetAdic  := InfoTotal.RTom.vlrTotalNRetAdic;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioinfoCRTom(ACont, ACont2: Integer): String;
var
  Resp: TRespostainfoCRTom;
begin
  Resp := TRespostainfoCRTom.Create(CSessaoRetornoinfoCRTom +
                  IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 1), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      resp.CRTom        := InfoTotal.RTom.infoCRTom.Items[ACont2].CRTom;
      resp.VlrCRTom     := InfoTotal.RTom.infoCRTom.Items[ACont2].VlrCRTom;
      resp.VlrCRTomSusp := InfoTotal.RTom.infoCRTom.Items[ACont2].VlrCRTomSusp;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioRPrest(ACont: Integer): String;
var
  Resp: TRespostaRPrest;
begin
  Resp := TRespostaRPrest.Create(CSessaoRetornoRPrest +
                                              IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      resp.tpInscTomador     := TpInscricaoToStr(InfoTotal.RPrest.tpInscTomador);
      resp.nrInscTomador     := InfoTotal.RPrest.nrInscTomador;
      resp.vlrTotalBaseRet   := InfoTotal.RPrest.vlrTotalBaseRet;
      resp.vlrTotalRetPrinc  := InfoTotal.RPrest.vlrTotalRetPrinc;
      resp.vlrTotalRetAdic   := InfoTotal.RPrest.vlrTotalRetAdic;
      resp.vlrTotalNRetPrinc := InfoTotal.RPrest.vlrTotalNRetPrinc;
      resp.vlrTotalNRetAdic  := InfoTotal.RPrest.vlrTotalNRetAdic;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioRRecRepAD(ACont, ACont2: Integer): String;
var
  Resp: TRespostaRRecRepAD;
begin
  Resp := TRespostaRRecRepAD.Create(CSessaoRetornoRRecRepAD +
                  IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      resp.cnpjAssocDesp     := InfoTotal.RRecRepAD.Items[ACont2].cnpjAssocDesp;
      resp.vlrTotalRep       := InfoTotal.RRecRepAD.Items[ACont2].vlrTotalRep;
      resp.vlrTotalRet       := InfoTotal.RRecRepAD.Items[ACont2].vlrTotalRet;
      resp.vlrTotalNRet      := InfoTotal.RRecRepAD.Items[ACont2].vlrTotalNRet;

      // Versão 1.03.02
      resp.CRRecRepAD        := InfoTotal.RRecRepAD.Items[ACont2].CRRecRepAD;
      resp.vlrCRRecRepAD     := InfoTotal.RRecRepAD.Items[ACont2].vlrCRRecRepAD;
      resp.vlrCRRecRepADSusp := InfoTotal.RRecRepAD.Items[ACont2].vlrCRRecRepADSusp;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioRComl(ACont, ACont2: Integer): String;
var
  Resp: TRespostaRComl;
begin
  Resp := TRespostaRComl.Create(CSessaoRetornoRComl +
                  IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 1), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      resp.vlrCPApur     := InfoTotal.RComl.Items[ACont2].vlrCPApur;
      resp.vlrRatApur    := InfoTotal.RComl.Items[ACont2].vlrRatApur;
      resp.vlrSenarApur  := InfoTotal.RComl.Items[ACont2].vlrSenarApur;
      resp.vlrCPSusp     := InfoTotal.RComl.Items[ACont2].vlrCPSusp;
      resp.vlrRatSusp    := InfoTotal.RComl.Items[ACont2].vlrRatSusp;
      resp.vlrSenarSusp  := InfoTotal.RComl.Items[ACont2].vlrSenarSusp;

      // Versão 1.03.02
      resp.CRComl        := InfoTotal.RComl.Items[ACont2].CRComl;
      resp.vlrCRComl     := InfoTotal.RComl.Items[ACont2].vlrCRComl;
      resp.vlrCRComlSusp := InfoTotal.RComl.Items[ACont2].vlrCRComlSusp;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioRCPRB(ACont, ACont2: Integer): String;
var
  Resp: TRespostaRCPRB;
begin
  Resp := TRespostaRCPRB.Create(CSessaoRetornoRCPRB +
                  IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 1), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      resp.codRec         := InfoTotal.RCPRB.Items[ACont2].codRec;
      resp.vlrCPApurTotal := InfoTotal.RCPRB.Items[ACont2].vlrCPApurTotal;
      resp.vlrCPRBSusp    := InfoTotal.RCPRB.Items[ACont2].vlrCPRBSusp;

      // Versão 1.03.02
      resp.CRCPRB         := InfoTotal.RCPRB.Items[ACont2].CRCPRB;
      resp.vlrCRCPRB      := InfoTotal.RCPRB.Items[ACont2].vlrCRCPRB;
      resp.vlrCRCPRBSusp  := InfoTotal.RCPRB.Items[ACont2].vlrCRCPRBSusp;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioRRecEspetDest(ACont: Integer): String;
var
  Resp: TEnvioRespostaRRecEspetDesp;
begin
  Resp := TEnvioRespostaRRecEspetDesp.Create(CSessaoRetornoRRecEspetDesp +
                  IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      resp.vlrReceitaTotal := InfoTotal.RRecEspetDesp.vlrReceitaTotal;
      resp.vlrCPApurTotal  := InfoTotal.RRecEspetDesp.vlrCPApurTotal;
      resp.vlrCPSuspTotal  := InfoTotal.RRecEspetDesp.vlrCPSuspTotal;

      // Versão 1.03.02
      resp.CRRecEspetDesp        := InfoTotal.RRecEspetDesp.CRRecEspetDesp;
      resp.vlrCRRecEspetDesp     := InfoTotal.RRecEspetDesp.vlrCRRecEspetDesp;
      resp.vlrCRRecEspetDespSusp := InfoTotal.RRecEspetDesp.vlrCRRecEspetDespSusp;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaConsulta: String;
var
  Resp: TConsultaResposta;
begin
  Resp := TConsultaResposta.Create(resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      resp.Id := Id;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaConsultaideEvento: String;
var
  Resp: TRespostaideEvento;
begin
  Resp := TRespostaideEvento.Create(CSessaoRetornoideEvento, resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      resp.perApur := IdeEvento.perApur;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaConsultaideContri: String;
var
  Resp: TRespostaideContri;
begin
  Resp := TRespostaideContri.Create(CSessaoRetornoideContri, resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      resp.tpInsc := TpInscricaoToStr(IdeContri.TpInsc);
      resp.nrInsc := IdeContri.nrInsc;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaConsultaideStatus: String;
var
  Resp: TRespostaideStatus;
begin
  Resp := TRespostaideStatus.Create(CSessaoRetornoideStatus, resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      resp.cdRetorno   := IdeStatus.cdRetorno;
      resp.descRetorno := IdeStatus.descRetorno;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaConsultaregOcorrs(ACont: Integer): String;
var
  Resp: TRespostaregOcorrs;
begin
  Resp := TRespostaregOcorrs.Create(CSessaoRetornoregOcorrs + IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      resp.tpOcorr        := IdeStatus.regOcorrs.Items[ACont].tpOcorr;
      resp.localErroAviso := IdeStatus.regOcorrs.Items[ACont].localErroAviso;
      resp.codResp        := IdeStatus.regOcorrs.Items[ACont].codResp;
      resp.dscResp        := IdeStatus.regOcorrs.Items[ACont].dscResp;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaConsultainfoRecEv: String;
var
  Resp: TRespostainfoRecEv;
begin
  Resp := TRespostainfoRecEv.Create(CSessaoRetornoinfoRecEv, resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      resp.nrProtEntr := InfoRecEv.nrProtEntr;
      resp.dhProcess  := InfoRecEv.dhProcess;
      resp.tpEv       := InfoRecEv.tpEv;
      resp.idEv       := InfoRecEv.idEv;
      resp.hash       := InfoRecEv.hash;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaConsultainfoTotalContrib: String;
var
  Resp: TRespostainfoTotal_infoTotalContrib;
begin
  Resp := TRespostainfoTotal_infoTotalContrib.Create(CSessaoRespConsultainfoTotalContrib, resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      resp.nrRecArqBase := InfoTotalContrib.nrRecArqBase;
      resp.indExistInfo := indExistInfoToStr(InfoTotalContrib.indExistInfo);
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaConsultaRTom(ACont: Integer): String;
var
  Resp: TRespostaRTom;
begin
  Resp := TRespostaRTom.Create(CSessaoRetornoRTom + IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      resp.cnpjPrestador     := InfoTotalContrib.RTom.Items[ACont].cnpjPrestador;
      resp.vlrTotalBaseRet   := InfoTotalContrib.RTom.Items[ACont].vlrTotalBaseRet;
      resp.vlrTotalRetPrinc  := InfoTotalContrib.RTom.Items[ACont].vlrTotalRetPrinc;
      resp.vlrTotalRetAdic   := InfoTotalContrib.RTom.Items[ACont].vlrTotalRetAdic;
      resp.vlrTotalNRetPrinc := InfoTotalContrib.RTom.Items[ACont].vlrTotalNRetPrinc;
      resp.vlrTotalNRetAdic  := InfoTotalContrib.RTom.Items[ACont].vlrTotalNRetAdic;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaConsultainfoCRTom(ACont, ACont2: Integer): String;
var
  Resp: TRespostainfoCRTom;
begin
  Resp := TRespostainfoCRTom.Create(CSessaoRetornoinfoCRTom +
                  intToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 1), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      resp.CRTom        := InfoTotalContrib.RTom.Items[ACont].infoCRTom.Items[ACont2].CRTom;
      resp.VlrCRTom     := InfoTotalContrib.RTom.Items[ACont].infoCRTom.Items[ACont2].VlrCRTom;
      resp.VlrCRTomSusp := InfoTotalContrib.RTom.Items[ACont].infoCRTom.Items[ACont2].VlrCRTomSusp;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaConsultaRPrest(ACont: Integer): String;
var
  Resp: TRespostaRPrest;
begin
  Resp := TRespostaRPrest.Create(CSessaoRetornoRPrest + IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      resp.tpInscTomador     := TpInscricaoToStr(InfoTotalContrib.RPrest.Items[ACont].tpInscTomador);
      resp.nrInscTomador     := InfoTotalContrib.RPrest.Items[ACont].nrInscTomador;
      resp.vlrTotalBaseRet   := InfoTotalContrib.RPrest.Items[ACont].vlrTotalBaseRet;
      resp.vlrTotalRetPrinc  := InfoTotalContrib.RPrest.Items[ACont].vlrTotalRetPrinc;
      resp.vlrTotalRetAdic   := InfoTotalContrib.RPrest.Items[ACont].vlrTotalRetAdic;
      resp.vlrTotalNRetPrinc := InfoTotalContrib.RPrest.Items[ACont].vlrTotalNRetPrinc;
      resp.vlrTotalNRetAdic  := InfoTotalContrib.RPrest.Items[ACont].vlrTotalNRetAdic;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaConsultaRRecRepAD(ACont: Integer): String;
var
  Resp: TRespostaRRecRepAD;
begin
  Resp := TRespostaRRecRepAD.Create(CSessaoRetornoRRecRepAD + IntToStrZero(ACont+1, 3), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      resp.cnpjAssocDesp     := InfoTotalContrib.RRecRepAD.Items[ACont].cnpjAssocDesp;
      resp.vlrTotalRep       := InfoTotalContrib.RRecRepAD.Items[ACont].vlrTotalRep;
      resp.vlrTotalRet       := InfoTotalContrib.RRecRepAD.Items[ACont].vlrTotalRet;
      resp.vlrTotalNRet      := InfoTotalContrib.RRecRepAD.Items[ACont].vlrTotalNRet;

      // Versão 1.03.02
      resp.CRRecRepAD        := InfoTotalContrib.RRecRepAD.Items[ACont].CRRecRepAD;
      resp.vlrCRRecRepAD     := InfoTotalContrib.RRecRepAD.Items[ACont].vlrCRRecRepAD;
      resp.vlrCRRecRepADSusp := InfoTotalContrib.RRecRepAD.Items[ACont].vlrCRRecRepADSusp;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaConsultaRComl(ACont: Integer): String;
var
  Resp: TRespostaRComl;
begin
  Resp := TRespostaRComl.Create(CSessaoRetornoRComl + IntToStrZero(ACont+1, 1), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      resp.vlrCPApur     := InfoTotalContrib.RComl.Items[ACont].vlrCPApur;
      resp.vlrRatApur    := InfoTotalContrib.RComl.Items[ACont].vlrRatApur;
      resp.vlrSenarApur  := InfoTotalContrib.RComl.Items[ACont].vlrSenarApur;
      resp.vlrCPSusp     := InfoTotalContrib.RComl.Items[ACont].vlrCPSusp;
      resp.vlrRatSusp    := InfoTotalContrib.RComl.Items[ACont].vlrRatSusp;
      resp.vlrSenarSusp  := InfoTotalContrib.RComl.Items[ACont].vlrSenarSusp;

      // Versão 1.03.02
      resp.CRComl        := InfoTotalContrib.RComl.Items[ACont].CRComl;
      resp.vlrCRComl     := InfoTotalContrib.RComl.Items[ACont].vlrCRComl;
      resp.vlrCRComlSusp := InfoTotalContrib.RComl.Items[ACont].vlrCRComlSusp;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaConsultaRCPRB(ACont: Integer): String;
var
  Resp: TRespostaRCPRB;
begin
  Resp := TRespostaRCPRB.Create(CSessaoRetornoRCPRB + IntToStrZero(ACont+1, 1), resINI);
  try
    with TACBrLibReinf(pLib).ReinfDM.ACBrReinf1.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      resp.codRec         := InfoTotalContrib.RCPRB.Items[ACont].codRec;
      resp.vlrCPApurTotal := InfoTotalContrib.RCPRB.Items[ACont].vlrCPApurTotal;
      resp.vlrCPRBSusp    := InfoTotalContrib.RCPRB.Items[ACont].vlrCPRBSusp;

      // Versão 1.03.02
      resp.CRCPRB         := InfoTotalContrib.RCPRB.Items[ACont].CRCPRB;
      resp.vlrCRCPRB      := InfoTotalContrib.RCPRB.Items[ACont].vlrCRCPRB;
      resp.vlrCRCPRBSusp  := InfoTotalContrib.RCPRB.Items[ACont].vlrCRCPRBSusp;
    end;

    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function Reinf_LerArqIni(const eArqIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Ok: Boolean;
  AArqIni: String;
begin
  try
    VerificarLibInicializada;
    AArqIni := AnsiString(eArqIni);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('Reinf_LerArqIni( ' + AArqIni + ' )', logCompleto, True)
    else
      pLib.GravarLog('Reinf_LerArqIni', logNormal);

    with TACBrLibReinf(pLib) do
    begin
      ReinfDM.Travar;
      try
        Ok := ReinfDM.ACBrReinf1.Eventos.LoadFromFile(AArqIni, False);
        Result := SetRetorno(ErrOK);
      finally
        ReinfDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function Reinf_Enviar(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AResposta: String;
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('Reinf_Enviar', logNormal);

    with TACBrLibReinf(pLib) do
    begin
      ReinfDM.Travar;
      try
        ReinfDM.ACBrReinf1.Enviar;

        AResposta := RespostaEnvio;

        ReinfDM.ACBrReinf1.Eventos.Clear;

        MoverStringParaPChar(AResposta, sResposta, esTamanho);

        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        ReinfDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function Reinf_Consultar(const eProtocolo, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AProtocolo: String;
  AResposta: String;
  i, j: Integer;
begin
  try
    VerificarLibInicializada;
    AProtocolo := AnsiString(eProtocolo);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('Reinf_Consultar( ' + AProtocolo + ' )', logCompleto, True)
    else
      pLib.GravarLog('Reinf_Consultar', logNormal);

    with TACBrLibReinf(pLib) do
    begin
      ReinfDM.Travar;
      try
        ReinfDM.ACBrReinf1.Eventos.Clear;
        ReinfDM.ACBrReinf1.Consultar(AProtocolo);

        with ReinfDM.ACBrReinf1.WebServices.Consultar.RetConsulta.evtTotalContrib do
        begin
          AResposta := RespostaConsulta;
          AResposta := AResposta + RespostaConsultaideEvento;
          AResposta := AResposta + RespostaConsultaideContri;
          AResposta := AResposta + RespostaConsultaideStatus;

          for i := 0 to IdeStatus.regOcorrs.Count -1 do
            AResposta := AResposta + RespostaConsultaregOcorrs(i);

          AResposta := AResposta + RespostaConsultainfoRecEv;
          AResposta := AResposta + RespostaConsultainfoTotalContrib;

          for i := 0 to infoTotalContrib.RTom.Count -1 do
          begin
            AResposta := AResposta + RespostaConsultaRTom(i);

            for j := 0 to infoTotalContrib.RTom.Items[i].infoCRTom.Count - 1 do
               AResposta := AResposta + RespostaConsultainfoCRTom(i, j);
          end;

          for i := 0 to infoTotalContrib.RPrest.Count -1 do
            AResposta := AResposta + RespostaConsultaRPrest(i);

          for i := 0 to infoTotalContrib.RRecRepAD.Count -1 do
            AResposta := AResposta + RespostaConsultaRRecRepAD(i);

          for i := 0 to infoTotalContrib.RComl.Count -1 do
            AResposta := AResposta + RespostaConsultaRComl(i);

          for i := 0 to infoTotalContrib.RCPRB.Count -1 do
            AResposta := AResposta + RespostaConsultaRCPRB(i);
        end;

        MoverStringParaPChar(AResposta, sResposta, esTamanho);

        Result := SetRetorno(ErrOK, StrPas(sResposta));
      finally
        ReinfDM.Destravar;
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

