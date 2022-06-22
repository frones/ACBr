{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }

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
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibeSocialBase;

interface

uses
  Classes, SysUtils, Forms, ACBrUtil,
  ACBrLibComum, ACBrLibeSocialDataModule, ACBreSocial, pcesConversaoeSocial, ACBrLibeSocialRespostas,
  pcesS5001, pcesS5002, pcesS5011, pcesS5012;

type

  { TACBrLibeSocial }

  TACBrLibeSocial = class(TACBrLib)
  private
    FeSocialDM: TLibeSocialDM;

  protected
    procedure CriarConfiguracao (ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create (ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    function RespostaEnvio: String;
    function RespostaEnvioConsulta: String;
    function RespostaEnvioOcorrencia(ACont:Integer): String;
    function RespostaOcorrencia1(ACont:Integer): String;
    function RespostaOcorrencia2(ACont, ACont2: Integer): String;
    function RespostaConsulta(ACont: Integer): String;
    function RespostaTot(ACont, ACont2: Integer): String;
    function RespostaConsultaIdentEventosQtd: String;
    function RespostaConsultaIdentEventosRecibo(ACont: Integer): String;
    function RespostaDownload(ACount: Integer): String;
    function RespostaPadrao: String;


    function CriarEventoeSocial (eArqIni: PChar):longint;
    function EnviareSocial (aGrupo: integer; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultareSocial (eProtocolo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function CriarEnviareSocial (const eArqIni: PChar; aGrupo:integer): longint;
    function LimpareSocial: Longint;
    function CarregarXMLEventoeSocial (const eArquivoOuXML: PChar): longint;
    function SetIDEmpregador (const aIdEmpregador: PChar): longint;
    function SetIDTransmissor (const aIdTransmissor: PChar): longint;
    function SetTipoEmpregador (aTipoEmpregador: integer):longint;
    function SetVersaoDF (const sVersao: PChar):longint;
    function ConsultaIdentificadoresEventosEmpregador (const aIdEmpregador: PChar; aTipoEvento: integer; aPeriodoApuracao: TDateTime):longint;
    function ConsultaIdentificadoresEventosTabela (const aIdEmpregador: PChar; aTipoEvento: integer; aChave: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime):longint;
    function ConsultaIdentificadoresEventosTrabalhador (const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial:TDateTime; aDataFinal: TDateTime):longint;
    function DownloadEventos (const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime):longint;

    function SetRetornoEventoCarregados(const NumEventos: integer): integer;

    property eSocialDM: TLibeSocialDM read FeSocialDM;

  end;

implementation

Uses
  ACBrLibConsts, ACBrLibeSocialConsts, ACBrLibConfig, ACBrLibeSocialConfig,
  ACBrLibResposta;

{ TACBrLibeSocial }

constructor TACBrLibeSocial.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FeSocialDM := TLibeSocialDM.Create(Nil);
  FeSocialDM.Lib := Self;
end;

destructor TACBrLibeSocial.Destroy;
begin
  FeSocialDM.Free;

  inherited Destroy;
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

function TACBrLibeSocial.SetRetornoEventoCarregados(const NumEventos: integer): integer;
begin
  Result := SetRetorno(0, Format(SInfEventosCarregados, [NumEventos]));
end;

function TACBrLibeSocial.RespostaEnvio: String;
var
  Resp: TEnvioResposta;
begin
  Resp := TEnvioResposta.Create(pLib.Lib.Config.TipoResposta, pLib.Lib.Config.CodResposta);
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
  finally
    Resp.Free;
  end;
end;

function TACBrLibeSocial.RespostaEnvioConsulta: String;
var
  Resp: TEnvioResposta;
begin
  Resp:= TEnvioResposta.Create(pLib.Lib.Config.TipoResposta, pLib.Lib.Config.CodResposta);
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
     Result:= Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function TACBrLibeSocial.RespostaEnvioOcorrencia(ACont: Integer): String;
var
  Resp: TOcorrenciaResposta;
begin
  Resp:= TOcorrenciaResposta.Create(CSessaoRespOcorrencia + IntToStr(ACont), pLib.Lib.Config.TipoResposta, pLib.Lib.Config.CodResposta);
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
     Result:= Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function TACBrLibeSocial.RespostaOcorrencia1(ACont:Integer): String;
var
  Resp: TOcorrenciaResposta;
begin
  Resp:= TOcorrenciaResposta.Create(CSessaoRespOcorrencia + IntToStr(ACont), pLib.Lib.Config.TipoResposta, pLib.Lib.Config.CodResposta);
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
  finally
    Resp.Free;
  end;
end;

function TACBrLibeSocial.RespostaOcorrencia2(ACont, ACont2:Integer): String;
var
  Resp: TOcorrenciaResposta;
begin
  Resp := TOcorrenciaResposta.Create(CSessaoRespOcorrencia + IntToStr(ACont2), pLib.Lib.Config.TipoResposta, pLib.Lib.Config.CodResposta);
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
  finally
    Resp.Free;
  end;
end;

function TACBrLibeSocial.RespostaConsulta(ACont:Integer): String;
var
  Resp:TConsultaResposta;
begin
  Resp:= TConsultaResposta.Create(CSessaoRespConsulta + IntToStr(ACont), pLib.Lib.Config.TipoResposta, pLib.Lib.Config.CodResposta);;
  try
     with TACBrLibeSocial(pLib).eSocialDM.ACBreSocial1.WebServices do
     begin
       with ConsultaLote.RetConsultaLote.retEventos.Items[ACont] do
       begin
         Resp.cdResposta          := Processamento.cdResposta;
         Resp.descResposta        := Processamento.descResposta;
         Resp.versaoAplicProcLote := Processamento.versaoAplicProcLote;
         Resp.dhProcessamento     := Processamento.dhProcessamento;
         Resp.nrRecibo            := Recibo.nrRecibo;
         Resp.hash                := Recibo.Hash;
       end;
     end;
  finally
    Resp.Free;
  end;
end;

function TACBrLibeSocial.RespostaTot(ACont, ACont2: Integer): String;
var
  Resp: TConsultaTotResposta;
  evtS5001: TS5001;
  evtS5002: TS5002;
  evtS5011: TS5011;
  evtS5012: TS5012;
begin
  Resp := TConsultaTotResposta.Create(CSessaoRespConsultaTot + IntToStr(ACont2), pLib.Lib.Config.TipoResposta, pLib.Lib.Config.CodResposta);
  try
    with TACBrLibeSocial(pLib).eSocialDM.ACBreSocial1.WebServices do
    begin
      with ConsultaLote.RetConsultaLote.retEventos.Items[ACont].tot[ACont2] do
      begin
        Resp.Tipo := Tipo;
        case Evento.TipoEvento of
          teS5001:
            begin
              evtS5001 := TS5001(Evento.GetEvento);
              Resp.ID  := evtS5001.EvtBasesTrab.Id;
              Resp.NrRecArqBase := evtS5001.EvtBasesTrab.IdeEvento.nrRecArqBase;
            end;
          teS5002:
            begin
              evtS5002 := TS5002(Evento.GetEvento);
              Resp.ID  := evtS5002.EvtirrfBenef.Id;
              Resp.NrRecArqBase := evtS5002.EvtirrfBenef.IdeEvento.nrRecArqBase;
            end;
          teS5011:
            begin
              evtS5011 := TS5011(Evento.GetEvento);
              Resp.ID  := evtS5011.EvtCS.Id;
              Resp.NrRecArqBase := evtS5011.EvtCS.IdeEvento.nrRecArqBase;
            end;
          teS5012:
            begin
              evtS5012 := TS5012(Evento.GetEvento);
              Resp.ID  := evtS5012.EvtIrrf.Id;
              Resp.NrRecArqBase := evtS5012.EvtIrrf.IdeEvento.nrRecArqBase;
            end;
        end;
      end;
      Result := Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

function TACBrLibeSocial.RespostaConsultaIdentEventosQtd: String;
var
  Resp: TConsultaTotEventos;
begin
  Resp:= TConsultaTotEventos.Create(CSessaoRespConsultaIdentEventos, pLib.Lib.Config.TipoResposta, pLib.Lib.Config.CodResposta);
  try
    with TACBrLibeSocial(pLib).eSocialDM.ACBreSocial1.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt do
    begin
      Resp.Codigo:= Status.cdResposta;
      Resp.Mensagem:= Status.descResposta;
      Resp.QtdeTotal:= RetIdentEvts.qtdeTotEvtsConsulta;
      Resp.DhUltimoEvento:= RetIdentEvts.dhUltimoEvtRetornado;
    end;
    Result:= Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function TACBrLibeSocial.RespostaConsultaIdentEventosRecibo(ACont: Integer): String;
var
  Resp: TConsultaIdentEvento;
begin
  Resp:= TConsultaIdentEvento.Create(CSessaoRespConsultaIdentEventosRecibo + IntToStr(ACont), pLib.Lib.Config.TipoResposta, pLib.Lib.Config.CodResposta);
  try
    with TACBrLibeSocial(pLib).eSocialDM.ACBreSocial1.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt.RetIdentEvts.Items[ACont] do
    begin
      Resp.IdEvento:= Id;
      Resp.NRecibo:= nrRec;
    end;
    Result:= Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function TACBrLibeSocial.RespostaDownload(ACount: Integer):String;
var
  Resp: TConsultaIdentEvento;
begin
  Resp:= TConsultaIdentEvento.Create(CSessaoRespConsulta, pLib.Lib.Config.TipoResposta, pLib.Lib.Config.CodResposta);
  try
    with TACBrLibeSocial(pLib).eSocialDM.ACBreSocial1.WebServices.DownloadEventos.RetDownloadEvt do
    begin
      Resp.Codigo:= Arquivo.Items[ACount].Status.cdResposta;
      Resp.Mensagem:= Arquivo.Items[ACount].Status.descResposta;
      Resp.IdEvento:= Arquivo.Items[ACount].Id;
      Resp.NRecibo:= Arquivo.Items[ACount].nrRec;
      Resp.XML:= Arquivo.Items[ACount].XML;
    end;
    Result:= Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function TACBrLibeSocial.RespostaPadrao:String;
var
  Resp: TPadraoeSocialResposta;
begin
  Resp:= TPadraoeSocialResposta.Create(CSessaoRespConsulta, pLib.Lib.Config.TipoResposta, pLib.Lib.Config.CodResposta);
  try
    with TACBrLibeSocial(pLib).eSocialDM.ACBreSocial1.WebServices.DownloadEventos.RetDownloadEvt do
    begin
      Resp.Codigo:= Status.cdResposta;
      Resp.Mensagem:= Status.descResposta;
    end;
    Result:= Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

function TACBrLibeSocial.CriarEventoeSocial(eArqIni: PChar): longint;
var
  AArqIni, AResposta:String;
begin
  try

    AArqIni:= AnsiString(eArqIni);

    if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_CriarEvento (' + AArqIni + ' ) ', logCompleto, True)
    else
     GravarLog('eSocial_CriarEvento', logNormal);

    with TACBrLibeSocial(pLib) do
     begin
       eSocialDM.Travar;
       try
         eSocialDM.ACBreSocial1.Eventos.LoadFromFile(AArqIni, False);
         Result := SetRetorno(ErrOK);
       finally
        eSocialDM.Destravar;
       end;
     end;

  except
    on E:EACBrLibException do
       Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibeSocial.EnviareSocial(aGrupo: integer; const sResposta: PChar; var esTamanho: longint): longint;
var
  AResposta: string;
  I: Integer;
begin
  try

    if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_EnviareSocial (' + IntToStr(aGrupo) + ' ) ', logCompleto, True)
     else
      GravarLog('eSocial_EnviareSocial', logNormal);

    with TACBrLibeSocial(pLib) do
     begin
     eSocialDM.Travar;
     try
       eSocialDM.ACBreSocial1.Enviar(TeSocialGrupo(aGrupo));
       AResposta:= '';

       with eSocialDM.ACBreSocial1.WebServices.EnvioLote.RetEnvioLote do
        begin
          if Status.cdResposta in [201, 202] then
           AResposta:= AResposta + RespostaEnvio
           else
            for i := 0 to Status.Ocorrencias.Count -1 do
            AResposta:= AResposta + RespostaEnvioOcorrencia(i);
        end;

       eSocialDM.ACBreSocial1.Eventos.Clear;

       MoverStringParaPChar (AResposta, sResposta, esTamanho);

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

function TACBrLibeSocial.ConsultareSocial(eProtocolo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  AProtocolo, AResposta:String;
  i, j : Integer;
begin
  try
     AProtocolo:= AnsiString(eProtocolo);

     if Config.Log.Nivel > logNormal then
      GravarLog('eSocial_ConsultareSocial (' + AProtocolo + ' ) ', logCompleto, True)
     else
      GravarLog('eSocial_ConsultareSocial', logNormal);

     with TACBrLibeSocial(pLib) do
     begin
     eSocialDM.Travar;
     try
        eSocialDM.ACBreSocial1.Eventos.Clear;
        esocialDM.ACBreSocial1.Consultar(AProtocolo);
        AResposta:= '';

        with eSocialDM.ACBreSocial1.WebServices.ConsultaLote.RetConsultaLote do
        begin
             if Status.cdResposta in [201, 202] then
              begin
                AResposta:= AResposta + RespostaEnvioConsulta;

                for i := 0 to RetEventos.Count - 1 do
                begin
                     AResposta:= AResposta + RespostaConsulta(i);

                     if RetEventos.Items[i].Processamento.Ocorrencias.Count > 0 then
                      for j := 0 to RetEventos.Items[i].Processamento.Ocorrencias.Count - 1 do
                        AResposta := AResposta + RespostaOcorrencia2(i, j);

                     for j := 0 to RetEventos.Items[i].tot.Count - 1 do
                      AResposta := AResposta + RespostaTot(i,j);
                end;
              end
             else
             begin
                  for i:= 0 to Status.Ocorrencias.Count - 1 do
                   AResposta:= AResposta + RespostaOcorrencia1(i);
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

function TACBrLibeSocial.CriarEnviareSocial(const eArqIni: PChar; aGrupo:integer): longint;
var
  AIniFile, grupo, ArqeSocial, Resp : String;
  ASalvar : Boolean;
  i, iEvento : Integer;
begin
  try

     if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_CriarEnviareSocial(' + eArqIni + ', ' + IntToStr(aGrupo) + ' )', logCompleto, True)
     else
      GravarLog('eSocial_CriarEnviareSocial', logNormal);

     if not FileExists(AIniFile) then
      raise Exception.Create(ACBrStr(Format(SErroeSocialAbrir, [AIniFile])));

     with TACBrLibeSocial(pLib) do
     begin
       eSocialDM.Travar;
        try
           eSocialDM.ACBreSocial1.Eventos.LoadFromIni(AIniFile);
           ASalvar:= eSocialDM.ACBreSocial1.Configuracoes.Geral.Salvar;

           if not ASalvar then
            begin
              ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + CChaveLogPath);
              eSocialDM.ACBreSocial1.Configuracoes.Arquivos.PathSalvar:= PathWithDelim(ExtractFilePath(Application.ExeName)) + CChaveLogPath;
            end;

           iEvento:= eSocialDM.ACBreSocial1.Eventos.Gerados.Count - 1;
           ArqeSocial:= eSocialDM.ACBreSocial1.Eventos.Gerados.Items[iEvento].PathNome + '.xml';

           if not FileExists(ArqeSocial) then
            raise Exception.Create(ACBrStr(Format(SErroeSocialAbrir, [ArqeSocial]) ));

           Resp:= ArqeSocial + sLineBreak + ACBrStr(Format(SMsgeSocialEventoAdicionado, [TipoEventoToStr(eSocialDM.ACBreSocial1.Eventos.Gerados.Items[iEvento].TipoEvento)]) ) + sLineBreak;

           Result := SetRetorno(ErrOK, Resp);

           eSocialDM.ACBreSocial1.Enviar(TeSocialGrupo(StrToIntDef(grupo,1)));
           Sleep(3000);

           with eSocialDM.ACBreSocial1.WebServices.EnvioLote.RetEnvioLote do
           begin
            if Status.cdResposta in [201, 202] then
             RespostaEnvio
             else
              for i := 0 to Status.Ocorrencias.Count -1 do
              RespostaEnvioConsulta;
           end;

           eSocialDM.ACBreSocial1.Eventos.Clear;

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

function TACBrLibeSocial.LimpareSocial: Longint;
begin
  try
    if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_Limpar', logCompleto, True)
     else
      GravarLog('eSocial_Limpar', logNormal);

    with TACBrLibeSocial(pLib) do
    begin
     eSocialDM.Travar;
      try
        eSocialDM.ACBreSocial1.Eventos.Clear;
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

function TACBrLibeSocial.CarregarXMLEventoeSocial(const eArquivoOuXML: PChar): longint;
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try

    ArquivoOuXml := ConverterAnsiParaUTF8(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
    GravarLog('eSocial_CarregarXMLEventoeSocial(' + ArquivoOuXml + ' ) ', logCompleto, True)
    else
     GravarLog('eSocial_CarregarXMLEventoeSocial', logNormal);

    EhArquivo:= StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    eSocialDM.Travar;

    try
      eSocialDM.ACBreSocial1.Eventos.LoadFromString(ArquivoOuXml);
      Result := SetRetornoEventoCarregados(eSocialDM.ACBreSocial1.Eventos.Count);
    finally
      eSocialDM.Destravar;
    end;

  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibeSocial.SetIDEmpregador (const aIdEmpregador: PChar): longint;
var
  idEmpregador: AnsiString;
begin
  try
    idEmpregador:= ConverterAnsiParaUTF8(aIdEmpregador);

    if Config.Log.Nivel > logNormal then
    GravarLog('eSocial_SetIDEmpregador (' + idEmpregador + ')', logCompleto, True)
    else
     GravarLog('eSocial_SetIDEmpregador', logNormal);

    if DirectoryExists(idEmpregador) then
    raise EACBrLibException.Create(ErrDiretorioNaoExiste, 'Diretorio não existe');

    eSocialDM.Travar;

    try
      TLibeSocialConfig(Config).SetIDEmpregador := idEmpregador;
      Result := SetRetorno(ErrOK);
    finally
      eSocialDM.Destravar;
    end;

  except
    on E: EACBrLibException do
       Result := SetRetorno(E.Erro, E.Message);
    on E: Exception do
       Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibeSocial.SetIDTransmissor (const aIdTransmissor: PChar): longint;
var
  idTransmissor: AnsiString;
begin
  try
    idTransmissor:= ConverterAnsiParaUTF8(aIdTransmissor);

    if Config.Log.Nivel > logNormal then
    GravarLog('eSocial_SetIDTransmissor(' + idTransmissor + ' ) ', logCompleto, True)
     else
      GravarLog('eSocial_SetIDTransmissor', logNormal);

    if EstaVazio(idTransmissor)then
         raise Exception.Create('Valor Nulo');

    eSocialDM.Travar;

    try
      TLibeSocialConfig(Config).SetIDTransmissor := idTransmissor;
      Result := SetRetorno(ErrOK);
    finally
      eSocialDM.Destravar;
    end;

  except
    on E: EACBrLibException do
    Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
    Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibeSocial.SetTipoEmpregador (aTipoEmpregador: integer):longint;
var
  tipoEmpregador:Integer;
begin
   try
     if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_SetTipoEmpregador(' + IntToStr(aTipoEmpregador) + ' ) ', logCompleto, True)
     else
      GravarLog('eSocial_SetTipoEmpregador', logNormal);

     try
      TLibeSocialConfig(Config).SetTipoEmpregador := tipoEmpregador;
      Result := SetRetorno(ErrOK);
    finally
      eSocialDM.Destravar;
    end;

   except
     on E: EACBrLibException do
     Result := SetRetorno(E.Erro, E.Message);

     on E: Exception do
     Result := SetRetorno(ErrExecutandoMetodo, E.Message);
   end;
end;

function TACBrLibeSocial.SetVersaoDF (const sVersao: PChar):longint;
var
  OK: boolean;
  versao: AnsiString;
begin
  try
    versao:= ConverterAnsiParaUTF8(sVersao);

    if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_SetVersaoDF(' + versao + ' ) ', logCompleto, True)
     else
      GravarLog('eSocial_SetVersaoDF', logNormal);

    if not OK then
       raise Exception.Create('Versão Inválida do eSocial.');

    eSocialDM.Travar;

    try
      TLibeSocialConfig(Config).SetVersaoDF := versao;
      Result := SetRetorno(ErrOK);
    finally
      eSocialDM.Destravar;
    end;
  except
    on E: EACBrLibException do
    Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
    Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function TACBrLibeSocial.ConsultaIdentificadoresEventosEmpregador (const aIdEmpregador: PChar; aTipoEvento: integer; aPeriodoApuracao: TDateTime):longint;
var
  idEmpregador: String;
  APerApur: TDateTime;
  ATpEvento: Integer;
  i: Integer;
begin
  try
    if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_ConsultaIdentificadoresEventosEmpregador(' + aIdEmpregador + ', ' + IntToStr(aTipoEvento) + ',' + DateToStr(aPeriodoApuracao) + ')', logCompleto, True)
     else
      GravarLog('eSocial_ConsultaIdentificadoresEventosEmpregador', logNormal);

    with TACBrLibeSocial(pLib) do
    begin
     eSocialDM.Travar;
     try

       if ((APerApur <= 0) or (EstaVazio(idEmpregador))) then
        raise Exception.Create(ACBrStr(SErroeSocialConsulta));

       eSocialDM.ACBreSocial1.Eventos.Clear;
       if eSocialDM.ACBreSocial1.ConsultaIdentificadoresEventosEmpregador(idEmpregador,
                       TTipoEvento(ATpEvento), APerApur) then
       begin
         with eSocialDM.ACBreSocial1.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt do
         begin
          RespostaConsultaIdentEventosQtd;

          for i:= 0 to RetIdentEvts.Count -1 do
          begin
           RespostaConsultaIdentEventosRecibo(i);
          end;

         end;
       end;

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

function TACBrLibeSocial.ConsultaIdentificadoresEventosTabela(const aIdEmpregador: PChar; aTipoEvento: integer; aChave: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime):longint;
var
  idEmpregador: String;
  tpEvento: Integer;
  Chave: String;
  DataInicial, DataFinal: TDateTime;
  i: Integer;
begin
  try
    if Config.Log.Nivel > logNormal then
    GravarLog('eSocial_ConsultaIdentificadoresEventosTabela (' + aIdEmpregador + ', ' + IntToStr(aTipoEvento) + ', ' + aChave + ', ' + DateToStr(aDataInicial) + ', ' + DateToStr(aDataFinal) + ')', logCompleto, True)
    else
     GravarLog('eSocial_ConsultaIdentificadoresEventosTabela', logNormal);

    with TACBrLibeSocial(pLib) do
    begin
     eSocialDM.Travar;
     try
       if ( (EstaVazio(idEmpregador)) or (EstaVazio(Chave)) or (DataInicial <= 0 ) or (DataFinal <= 0) ) then
       raise Exception.Create(ACBrStr(SErroeSocialConsulta));

       eSocialDM.ACBreSocial1.Eventos.Clear;
       if eSocialDM.ACBreSocial1.ConsultaIdentificadoresEventosTabela(idEmpregador, TTipoEvento(tpEvento), Chave, DataInicial, DataFinal) then
       begin
         with eSocialDM.ACBreSocial1.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt do
         begin
          RespostaConsultaIdentEventosQtd;

          for i := 0 to RetIdentEvts.Count - 1 do
          begin
           RespostaConsultaIdentEventosRecibo(i);
          end;

         end;
       end;

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

function TACBrLibeSocial.ConsultaIdentificadoresEventosTrabalhador (const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial:TDateTime; aDataFinal: TDateTime):longint;
var
  idEmpregador: String;
  CPFTrabalhador: String;
  DataInicial, DataFinal: TDateTime;
  i: Integer;
begin
  try
    if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_ConsultaIdentificadoresEventosTrabalhador(' + aIdEmpregador + ', ' + aCPFTrabalhador + ',' + DateToStr(aDataInicial) + ',' + DateToStr(aDataFinal) + ')', logCompleto, True)
     else
      GravarLog('eSocial_ConsultaIdentificadoresEventosTrabalhador', logNormal);

    with TACBrLibeSocial(pLib) do
    begin
     eSocialDM.Travar;
     try

       if ((EstaVazio(idEmpregador)) or (EstaVazio(CPFTrabalhador))
          or (DataInicial <= 0)  or (DataFinal <= 0 )) then
       raise Exception.Create(ACBrStr(SErroeSocialConsulta));

       eSocialDM.ACBreSocial1.Eventos.Clear;
       if eSocialDM.ACBreSocial1.ConsultaIdentificadoresEventosTrabalhador(idEmpregador,
                       CPFTrabalhador, DataInicial, DataFinal) then
       begin
          with eSocialDM.ACBreSocial1.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt do
          begin
           RespostaConsultaIdentEventosQtd;

           for i := 0 to RetIdentEvts.Count - 1 do
           begin
            RespostaConsultaIdentEventosRecibo(i);
           end;
          end;
       end;

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

function TACBrLibeSocial.DownloadEventos (const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime):longint;
var
  idEmpregador: String;
  AId: String;
  ANrRecibo: String;
  i: Integer;
begin
  try
    if Config.Log.Nivel > logNormal then
     GravarLog('eSocial_DownloadEventos(' + aIdEmpregador + ', ' + aCPFTrabalhador + ',' + DateToStr(aDataInicial) + ',' + DateToStr(aDataFinal) + ')', logCompleto, True)
     else
      GravarLog('eSocial_DownloadEventos', logNormal);

    with TACBrLibeSocial(pLib) do
    begin
     eSocialDM.Travar;
     try

       if ( (EstaVazio(idEmpregador)) ) then
       raise Exception.Create(ACBrStr(SErroeSocialConsulta));

        eSocialDM.ACBreSocial1.Eventos.Clear;
        if eSocialDM.ACBreSocial1.DownloadEventos(idEmpregador, AID, ANrRecibo) then
        begin
          with eSocialDM.ACBreSocial1.WebServices.DownloadEventos.RetDownloadEvt do
          begin
           RespostaPadrao;

           for i := 0 to Arquivo.Count - 1 do
           begin
            RespostaDownload(i);
           end;
          end;
        end;

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

end.

