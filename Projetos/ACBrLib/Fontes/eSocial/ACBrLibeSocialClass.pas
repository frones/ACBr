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
  ACBrUtil, ACBrLibComum, ACBrLibeSocialDataModule, ACBreSocial,
  pcesS5001, pcesS5002, pcesS5011, pcesS5012;

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
function RespostaEnvio: String;
function RespostaEnvioConsulta: String;
function RespostaEnvioOcorrencia(ACont: Integer): String;
function RespostaOcorrencia1(ACont: Integer): String;
function RespostaOcorrencia2(ACont, ACont2: Integer): String;
function RespostaConsulta(ACont: Integer): String;
function RespostaTot(ACont, ACont2: Integer): String;

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
function RespostaEnvio: String;
var
  Resp: TEnvioResposta;
begin
  Resp := TEnvioResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibeSocial(pLib).eSocialDM.ACBreSocial1.WebServices do
    begin
      with EnvioLote.RetEnvioLote do
      begin
        Resp.Codigo       := Status.cdResposta;
        Resp.Mensagem     := Status.descResposta;
        Resp.TpInscEmpreg := eSTpInscricaoToStr(IdeEmpregador.TpInsc);
        Resp.NrInscEmpreg := IdeEmpregador.NrInsc;
        Resp.TpInscTransm := eSTpInscricaoToStr(IdeTransmissor.TpInsc);
        Resp.NrInscTransm := IdeTransmissor.NrInsc;
        Resp.DhRecepcao   := dadosRecLote.dhRecepcao;
        Resp.VersaoAplic  := dadosRecLote.versaoAplicRecepcao;
        Resp.Protocolo    := dadosRecLote.Protocolo;
      end;
    end;
    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioConsulta: String;
var
  Resp: TEnvioResposta;
begin
  Resp := TEnvioResposta.Create(pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibeSocial(pLib).eSocialDM.ACBreSocial1.WebServices do
    begin
      with ConsultaLote.RetConsultaLote do
      begin
        Resp.Codigo       := Status.cdResposta;
        Resp.Mensagem     := Status.descResposta;
        Resp.TpInscEmpreg := eSTpInscricaoToStr(IdeEmpregador.TpInsc);
        Resp.NrInscEmpreg := IdeEmpregador.NrInsc;
        Resp.TpInscTransm := eSTpInscricaoToStr(IdeTransmissor.TpInsc);
        Resp.NrInscTransm := IdeTransmissor.NrInsc;
        Resp.DhRecepcao   := dadosRecLote.dhRecepcao;
        Resp.VersaoAplic  := dadosRecLote.versaoAplicRecepcao;
        Resp.Protocolo    := dadosRecLote.Protocolo;
      end;
    end;
    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaEnvioOcorrencia(ACont: Integer): String;
var
  Resp: TOcorrenciaResposta;
begin
  Resp := TOcorrenciaResposta.Create(CSessaoRespOcorrencia + IntToStr(ACont), pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibeSocial(pLib).eSocialDM.ACBreSocial1.WebServices do
    begin
      with EnvioLote.RetEnvioLote do
      begin
        Resp.Codigo      := Status.cdResposta;
        Resp.Mensagem    := Status.descResposta;
        Resp.CodigoOco   := Status.Ocorrencias.Items[ACont].Codigo;
        Resp.Descricao   := Status.Ocorrencias.Items[ACont].Descricao;
        Resp.Tipo        := Status.Ocorrencias.Items[ACont].Tipo;
        Resp.Localizacao := Status.Ocorrencias.Items[ACont].Localizacao;
      end;
    end;
    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaOcorrencia1(ACont: Integer): String;
var
  Resp: TOcorrenciaResposta;
begin
  Resp := TOcorrenciaResposta.Create(CSessaoRespOcorrencia + IntToStr(ACont), pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibeSocial(pLib).eSocialDM.ACBreSocial1.WebServices do
    begin
      with ConsultaLote.RetConsultaLote do
      begin
        Resp.Codigo      := Status.cdResposta;
        Resp.Mensagem    := Status.descResposta;
        Resp.CodigoOco   := Status.Ocorrencias.Items[ACont].Codigo;
        Resp.Descricao   := Status.Ocorrencias.Items[ACont].Descricao;
        Resp.Tipo        := Status.Ocorrencias.Items[ACont].Tipo;
        Resp.Localizacao := Status.Ocorrencias.Items[ACont].Localizacao;
      end;
    end;
    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaOcorrencia2(ACont, ACont2: Integer): String;
var
  Resp: TOcorrenciaResposta;
begin
  Resp := TOcorrenciaResposta.Create(CSessaoRespOcorrencia + IntToStr(ACont2), pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibeSocial(pLib).eSocialDM.ACBreSocial1.WebServices do
    begin
      with ConsultaLote.RetConsultaLote.retEventos.Items[ACont].Processamento.Ocorrencias.Items[ACont2] do
      begin
        Resp.CodigoOco   := Codigo;
        Resp.Descricao   := Descricao;
        Resp.Tipo        := Tipo;
        Resp.Localizacao := Localizacao;
      end;
    end;
    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaConsulta(ACont: Integer): String;
var
  Resp: TConsultaResposta;
begin
  Resp := TConsultaResposta.Create(CSessaoRespConsulta + IntToStr(ACont), pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibeSocial(pLib).eSocialDM.ACBreSocial1.WebServices do
    begin
      with ConsultaLote.RetConsultaLote.retEventos.Items[ACont] do
      begin
        resp.cdResposta          := Processamento.cdResposta;
        resp.descResposta        := Processamento.descResposta;
        resp.versaoAplicProcLote := Processamento.versaoAplicProcLote;
        resp.dhProcessamento     := Processamento.dhProcessamento;
        resp.nrRecibo            := Recibo.nrRecibo;
        resp.hash                := Recibo.Hash;
      end;
    end;
    Result := Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function RespostaTot(ACont, ACont2: Integer): String;
var
  Resp: TConsultaTotResposta;
  evtS5001: TS5001;
  evtS5002: TS5002;
  evtS5011: TS5011;
  evtS5012: TS5012;
begin
  Resp := TConsultaTotResposta.Create(CSessaoRespConsultaTot + IntToStr(ACont2), pLib.Config.TipoResposta, pLib.Config.CodResposta);
  try
    with TACBrLibeSocial(pLib).eSocialDM.ACBreSocial1.WebServices do
    begin
      with ConsultaLote.RetConsultaLote.retEventos.Items[ACont].tot[ACont2] do
      begin
        resp.Tipo := Tipo;
        case Evento.TipoEvento of
          teS5001:
            begin
              evtS5001 := TS5001(Evento.GetEvento);
              resp.ID  := evtS5001.EvtBasesTrab.Id;
              resp.NrRecArqBase := evtS5001.EvtBasesTrab.IdeEvento.nrRecArqBase;
            end;
          teS5002:
            begin
              evtS5002 := TS5002(Evento.GetEvento);
              resp.ID  := evtS5002.EvtirrfBenef.Id;
              resp.NrRecArqBase := evtS5002.EvtirrfBenef.IdeEvento.nrRecArqBase;
            end;
          teS5011:
            begin
              evtS5011 := TS5011(Evento.GetEvento);
              resp.ID  := evtS5011.EvtCS.Id;
              resp.NrRecArqBase := evtS5011.EvtCS.IdeEvento.nrRecArqBase;
            end;
          teS5012:
            begin
              evtS5012 := TS5012(Evento.GetEvento);
              resp.ID  := evtS5012.EvtIrrf.Id;
              resp.NrRecArqBase := evtS5012.EvtIrrf.IdeEvento.nrRecArqBase;
            end;
        end;
      end;
      Result := Resp.Gerar;
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
        Ok := eSocialDM.ACBreSocial1.Eventos.LoadFromFile(AArqIni, False);
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

        with eSocialDM.ACBreSocial1.WebServices.EnvioLote.RetEnvioLote do
        begin
          if Status.cdResposta in [201, 202] then
            AResposta := AResposta + RespostaEnvio
          else
            for i := 0 to Status.Ocorrencias.Count - 1 do
              AResposta := AResposta + RespostaEnvioOcorrencia(i);
        end;

        eSocialDM.ACBreSocial1.Eventos.Clear;

        MoverStringParaPChar(AResposta, sResposta, esTamanho);

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
  AProtocolo: String;
  AResposta: String;
  i, j: Integer;
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
      try
        eSocialDM.ACBreSocial1.Eventos.Clear;
        eSocialDM.ACBreSocial1.Consultar(AProtocolo);
        AResposta := '';

        with eSocialDM.ACBreSocial1.WebServices.ConsultaLote.RetConsultaLote do
        begin
          if Status.cdResposta in [201, 202] then
          begin
            AResposta := AResposta + RespostaEnvioConsulta;

            for i := 0 to retEventos.Count - 1 do
            begin
              AResposta := AResposta + RespostaConsulta(i);

              if retEventos.Items[i].Processamento.Ocorrencias.Count > 0 then
                for j := 0 to retEventos.Items[i].Processamento.Ocorrencias.Count - 1 do
                  AResposta := AResposta + RespostaOcorrencia2(i, j);

              for j := 0 to retEventos.Items[i].tot.Count - 1 do
                AResposta := AResposta + RespostaTot(i, j);
            end;
          end
          else
          begin
            for i := 0 to Status.Ocorrencias.Count - 1 do
              AResposta := AResposta + RespostaOcorrencia1(i);
          end;
        end;

        MoverStringParaPChar(AResposta, sResposta, esTamanho);

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
{%endregion}

{%endregion}

end.

