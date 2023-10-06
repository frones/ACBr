{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$I ACBr.inc}

unit DoACBrReinfUnit;

interface

uses
  Classes, SysUtils, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrReinf, ACBrMonitorConfig, pcnReinfR9005, pcnReinfRetConsulta_R9015,
  pcnReinfRetConsulta_R9011, pcnReinfRetEventos, pcnCommonReinf,
  ACBrMonitorConsts, CmdUnit, pcnConversaoReinf, DoACBrDFeUnit,
  ACBrLibResposta, ACBrLibReinfConsts, ACBrLibReinfRespostas,
  ACBrReinfEventos;

type

{ TACBrObjetoReinf }

TACBrObjetoReinf = class(TACBrObjetoDFe)
private
  fACBrReinf: TACBrReinf;
public
  constructor Create(AConfig: TMonitorConfig; ACBrReinf: TACBrReinf); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  procedure RespostaEnvio;
  procedure RespostaEnvioRetorno;
  procedure RespostaEnvioideTransmissor;
  procedure RespostaEnviostatus;
  procedure RespostaEnviodadosRecepcaoLote(item: TRetEnvioLote);
  procedure RespostaEnvioOcorrencias(ACont: Integer);
  procedure RespostaEnvioevento(ACont: Integer);
  procedure RespostaEnvioevtTotal(ACont: Integer);
  procedure RespostaEnvioideEvento(ACont: Integer);
  procedure RespostaEnvioideContri(ACont: Integer);
  procedure RespostaEnvioideStatus(ACont: Integer; item: TIdeStatus);
  procedure RespostaEnvioregOcorrs(ACont, ACont2: Integer; item: TIdeStatus);
  procedure RespostaEnvioinfoRecEv(ACont: Integer; item: TInfoRecEv);
  procedure RespostaEnvioinfoTotal(ACont: Integer);
  procedure RespostaEnvioRTom(ACont: Integer; item: TeventoCollectionItem);
  procedure RespostaEnvioinfoCRTom(ACont, ACont2: Integer; item: TeventoCollectionItem);
  procedure RespostaEnvioRPrest(ACont: Integer; item: TeventoCollectionItem);
  procedure RespostaEnvioRRecRepAD(ACont, ACont2: Integer; item: TeventoCollectionItem);
  procedure RespostaEnvioRComl(ACont, ACont2: Integer; item: TeventoCollectionItem);
  procedure RespostaEnvioRAquis(ACont, ACont2: Integer; item: TeventoCollectionItem);
  procedure RespostaEnvioRCPRB(ACont, ACont2: Integer; item: TeventoCollectionItem);
  procedure RespostaEnvioRRecEspetDest(ACont: Integer; item: TeventoCollectionItem);

  procedure RespostaConsulta(ASessao: String; AId: String);
  procedure RespostaConsultaideEvento(item: TIdeEvento1);
  procedure RespostaConsultaideContri(item: TideContrib; status: TStatus = nil);
  procedure RespostaConsultaideStatus;
  procedure RespostaConsultaregOcorrs(ACont: Integer; item: TIdeStatus);
  procedure RespostaConsultainfoRecEv;
  procedure RespostaConsultainfoTotalContrib;
  procedure RespostaConsultaRTom(ACont: Integer);
  procedure RespostaConsultainfoCRTom(ACont, ACont2: Integer);
  procedure RespostaConsultaRPrest(ACont: Integer);
  procedure RespostaConsultaRRecRepAD(ACont: Integer);
  procedure RespostaConsultaRComl(ACont: Integer);
  procedure RespostaConsultaRCPRB(ACont: Integer);

  procedure RespostainfoTotalevtRet(ACont: Integer);
  procedure RespostaideEstab(ACont: Integer);
  procedure RespostatotApurMen(ACont, ACont2: Integer);
  procedure RespostatotApurTribMen(ACont, ACont2: Integer);
  procedure RespostatotApurQui(ACont, ACont2: Integer);
  procedure RespostatotApurTribQui(ACont, ACont2: Integer);
  procedure RespostatotApurDec(ACont, ACont2: Integer);
  procedure RespostatotApurTribDec(ACont, ACont2: Integer);
  procedure RespostatotApurSem(ACont, ACont2: Integer);
  procedure RespostatotApurTribSem(ACont, ACont2: Integer);
  procedure RespostatotApurDia(ACont, ACont2: Integer);
  procedure RespostatotApurTribDia(ACont, ACont2: Integer);

  procedure RespostaConsultainfoCR_CNR(item: TinfoCR_CNR);
  procedure RespostaConsultatotApurMen(ACont: Integer; item: TtotApurMenCollection);
  procedure RespostaConsultatotApurQui(ACont: Integer; item: TtotApurQuiCollection);
  procedure RespostaConsultatotApurDec(ACont: Integer; item: TtotApurDecCollection);
  procedure RespostaConsultatotApurSem(ACont: Integer; item: TtotApurSemCollection);
  procedure RespostaConsultatotApurDia(ACont: Integer; item: TtotApurDiaCollection);
  procedure RespostaConsultainfoTotalCR;

  procedure RespostaConsultaReciboStatus;
  procedure RespostaConsultaReciboOcorrs(ACont: Integer);
  procedure RespostaEventoRecibo(ACont: Integer);

  property ACBrReinf: TACBrReinf read fACBrReinf;
end;

{ TACBrCarregarReinf }

TACBrCarregarReinf = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;

public
  constructor Create(AACBrReinf: TACBrReinf; AXMLorFile: String ); reintroduce;
end;

{ TMetodoCriarEventoReinf}

TMetodoCriarEventoReinf = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarReinf}

TMetodoEnviarReinf = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarEnviarReinf}

TMetodoCriarEnviarReinf = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarReinf}

TMetodoConsultarReinf = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCarregarXMLEventoReinf}

TMetodoCarregarXMLEventoReinf = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLimparReinf}

TMetodoLimparReinf = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetIDContribuinte}

TMetodoSetIDContribuinte = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetIDTransmissor}

TMetodoSetIDTransmissor = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarReciboReinf}

TMetodoConsultarReciboReinf = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

uses
  DoACBrUnit, Forms;

{ TACBrCarregarReinf }

procedure TACBrCarregarReinf.CarregarDFePath(const AValue: String);
begin
  if not ( TACBrReinf(FpACBrDFe).Eventos.LoadFromFile( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroReinfAbrir, [AValue]) ) );
end;

procedure TACBrCarregarReinf.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBrReinf(FpACBrDFe).Eventos.LoadFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroReinfCarregar) );
end;

function TACBrCarregarReinf.ValidarDFe(const AValue: String): Boolean;
begin
  Result := False;
  if ( TACBrReinf(FpACBrDFe).Eventos.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBrReinf(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;
end;

constructor TACBrCarregarReinf.Create(AACBrReinf: TACBrReinf;
  AXMLorFile: String);
begin
  inherited Create(AACBrReinf, AXMLorFile);
end;

{ TMetodoCarregarXMLEventoReinf }

{ Params: 0 - pathXML - Uma String com pathXML ou XML completo
}
procedure TMetodoCarregarXMLEventoReinf.Executar;
var
  APathorXML: String;
  CargaDFe: TACBrCarregarReinf;
begin
  APathorXML := fpCmd.Params(0);
  with TACBrObjetoReinf(fpObjetoDono) do
  begin
    CargaDFe := TACBrCarregarReinf.Create(ACBrReinf , APathorXML);
    try
      fpCmd.Resposta := ACBrStr( Format(SMsgReinfEventoAdicionado,[APathorXML]) )
    finally
      CargaDFe.Free;
    end;
  end;
end;

{ TMetodoConsultarReciboReinf }

{ Params: 0 - PerApur : String - Periodo de apuração
          1 - TipoEvento : Integer - Código do Tipo de Evento
          2 - Inscricaoestabelecimento : String - IE do estabelecimento
          3 - CnpjPrestador : String - CNPJ do
          4 - InscricaoTomador : String - IE do tomador
          5 - DataApur : TDateTime - Data de Apuração
}
procedure TMetodoConsultarReciboReinf.Executar;
var
  APerApur: String;
  ATipoEvento: Integer;
  AInscricaoestabelecimento: String;
  ACnpjPrestador: String;
  AInscricaoTomador: String;
  ADataApur: TDateTime;
  i: Integer;
begin
  APerApur := fpCmd.Params(0);
  ATipoEvento := StrToIntDef(fpCmd.Params(1),0);
  AInscricaoEstabelecimento := fpCmd.Params(2);
  ACnpjPrestador := fpCmd.Params(3);
  AInscricaoTomador := fpCmd.Params(4);
  ADataApur := StrToDateTimeDef(fpCmd.Params(5),0);

  with TACBrObjetoReinf(fpObjetoDono) do
  begin
    if (EstaVazio(APerApur)) or (EstaVazio(ACnpjPrestador)) then
      raise Exception.Create(ACBrStr(SErroReinfConsulta));

    ACBrReinf.Eventos.Clear;
    if ACBrReinf.ConsultaReciboEvento(APerApur,
                                      TTipoEvento(ATipoEvento),
                                      AInscricaoestabelecimento,
                                      ACnpjPrestador,
                                      AInscricaoTomador,
                                      ADataApur) then
    begin
      with fACBrReinf.WebServices.ConsultarReciboEvento.evtTotalContribVersao do
      begin
        RespostaConsultaReciboStatus;

        for i := 0 to IdeStatus.regOcorrs.Count -1 do
          RespostaConsultaReciboOcorrs(i);

        for i := 0 to RetornoEventos.Count -1 do
          RespostaEventoRecibo(i);

      end;
    end;
  end;

end;

{ TMetodoConsultarReinf }

{ Params: 0 - Protocolo - Uma String com protocolo Reinf
}
procedure TMetodoConsultarReinf.Executar;
var
  AProtocolo: String;
  i, j: Integer;
begin
  AProtocolo := fpCmd.Params(0);

  with TACBrObjetoReinf(fpObjetoDono) do
  begin
    ACBrReinf.Eventos.Clear;
    if ACBrReinf.Consultar(AProtocolo) then
    begin
      with fACBrReinf.WebServices.Consultar.RetEnvioLote do
      begin
        RespostaConsultaideContri(IdeContribuinte, status);
        RespostaEnviodadosRecepcaoLote(fACBrReinf.WebServices.Consultar.RetEnvioLote);

        for i:=0 to evento.Count - 1 do
        begin
          with evento.Items[i] do
          begin
            if evtTotal.id <> '' then
            begin
              with evtTotal do
              begin
                RespostaConsulta(CSessaoRespEnvioevtTotal, id);
                RespostaConsultaideEvento(IdeEvento);
                RespostaConsultaideContri(IdeContrib);
                RespostaEnvioideStatus(i, IdeStatus);

                for j := 0 to IdeStatus.regOcorrs.Count -1 do
                  RespostaEnvioregOcorrs(i, j, evento.Items[i].evtTotal.IdeStatus);

                RespostaEnvioinfoRecEv(i,evento.Items[i].evtTotal.InfoRecEv);

                RespostaEnvioinfoTotal(i);

                RespostaEnvioRTom(i, evento.Items[i]);

                for j := 0 to evento.Items[i].evtTotal.InfoTotal.RTom.infoCRTom.Count -1 do
                  RespostaEnvioinfoCRTom(i, j, evento.Items[i]);

                RespostaEnvioRPrest(i, evento.Items[i]);

                for j := 0 to evento.Items[i].evtTotal.InfoTotal.RRecRepAD.Count -1 do
                  RespostaEnvioRRecRepAD(i, j, evento.Items[i]);

                for j := 0 to evento.Items[i].evtTotal.InfoTotal.RComl.Count -1 do
                  RespostaEnvioRComl(i, j, evento.Items[i]);

                for j := 0 to evento.Items[i].evtTotal.InfoTotal.RAquis.Count -1 do
                  RespostaEnvioRAquis(i, j, evento.Items[i]);

                for j := 0 to evento.Items[i].evtTotal.InfoTotal.RCPRB.Count -1 do
                  RespostaEnvioRCPRB(i, j, evento.Items[i]);

                RespostaEnvioRRecEspetDest(i, evento.Items[i]);
              end;
            end;

            if evtRet.Id <> '' then
            begin
              with evtRet do
              begin
                RespostaConsulta(CSessaoRespEnvioevtRet, id);
                RespostaConsultaideEvento(IdeEvento);
                RespostaConsultaideContri(IdeContrib);
                RespostaEnvioideStatus(i, IdeStatus);

                for j := 0 to IdeStatus.regOcorrs.Count -1 do
                  RespostaEnvioregOcorrs(i, j, evento.Items[i].evtRet.IdeStatus);

                RespostaEnvioinfoRecEv(i,evento.Items[i].evtRet.InfoRecEv);

                RespostainfoTotalevtRet(i);

                RespostaideEstab(i);

                for j := 0 to evento.Items[i].evtRet.InfoTotal.ideEstab.totApurMen.Count -1 do
                begin
                  RespostatotApurMen(i, j);

                  RespostatotApurTribMen(i, j);
                end;

                for j := 0 to evento.Items[i].evtRet.InfoTotal.ideEstab.totApurQui.Count -1 do
                begin
                  RespostatotApurQui(i, j);

                  RespostatotApurTribQui(i, j);
                end;

                for j := 0 to evento.Items[i].evtRet.InfoTotal.ideEstab.totApurDec.Count -1 do
                begin
                  RespostatotApurDec(i, j);

                  RespostatotApurTribDec(i, j);
                end;

                for j := 0 to evento.Items[i].evtRet.InfoTotal.ideEstab.totApurSem.Count -1 do
                begin
                  RespostatotApurSem(i, j);

                  RespostatotApurTribSem(i, j);
                end;

                for j := 0 to evento.Items[i].evtRet.InfoTotal.ideEstab.totApurDia.Count -1 do
                begin
                  RespostatotApurDia(i, j);

                  RespostatotApurTribDia(i, j);
                end;
              end;
            end;
          end;
        end;
      end;

      if fACBrReinf.WebServices.Consultar.RetConsulta_R9015.evtRetCons.Id <> '' then
      begin
        with fACBrReinf.WebServices.Consultar.RetConsulta_R9015.evtRetCons do
        begin
          RespostaConsulta(CSessaoRespEnvioevtRetCons, Id);
          RespostaConsultaideEvento(IdeEvento);
          RespostaConsultaideContri(IdeContri);
          RespostaEnvioideStatus(0, IdeStatus);

          for i := 0 to IdeStatus.regOcorrs.Count -1 do
            RespostaConsultaregOcorrs(i, IdeStatus);

          RespostaEnvioinfoRecEv(0, InfoRecEv);
          RespostaConsultainfoCR_CNR(infoCR_CNR);

          for i := 0 to infoCR_CNR.totApurMen.Count -1 do
            RespostaConsultatotApurMen(i, infoCR_CNR.totApurMen);

          for i := 0 to infoCR_CNR.totApurQui.Count -1 do
            RespostaConsultatotApurQui(i, infoCR_CNR.totApurQui);

          for i := 0 to infoCR_CNR.totApurDec.Count -1 do
            RespostaConsultatotApurDec(i, infoCR_CNR.totApurDec);

          for i := 0 to infoCR_CNR.totApurSem.Count -1 do
            RespostaConsultatotApurSem(i, infoCR_CNR.totApurSem);

          for i := 0 to infoCR_CNR.totApurDia.Count -1 do
            RespostaConsultatotApurDia(i, infoCR_CNR.totApurDia);

          RespostaConsultainfoTotalCR;

          for i := 0 to infoTotalCR.totApurMen.Count -1 do
            RespostaConsultatotApurMen(i, infoTotalCR.totApurMen);

          for i := 0 to infoTotalCR.totApurQui.Count -1 do
            RespostaConsultatotApurQui(i, infoTotalCR.totApurQui);

          for i := 0 to infoTotalCR.totApurDec.Count -1 do
            RespostaConsultatotApurDec(i, infoTotalCR.totApurDec);

          for i := 0 to infoTotalCR.totApurSem.Count -1 do
            RespostaConsultatotApurSem(i, infoTotalCR.totApurSem);

          for i := 0 to infoTotalCR.totApurDia.Count -1 do
            RespostaConsultatotApurDia(i, infoTotalCR.totApurDia);
        end;
      end;

      if fACBrReinf.WebServices.Consultar.evtTotalContribVersao.Id <> '' then
      begin
        with fACBrReinf.WebServices.Consultar.evtTotalContribVersao do
        begin
          RespostaConsulta(CSessaoRespConsulta, Id);
          RespostaConsultaideEvento(IdeEvento);
          RespostaConsultaideContri(IdeContri);
          RespostaConsultaideStatus;

          for i := 0 to IdeStatus.regOcorrs.Count -1 do
            RespostaConsultaregOcorrs(i, IdeStatus);

          RespostaConsultainfoRecEv;
          RespostaConsultainfoTotalContrib;

          for i := 0 to infoTotalContrib.RTom.Count -1 do
          begin
            RespostaConsultaRTom(i);

            for j := 0 to infoTotalContrib.RTom.Items[i].infoCRTom.Count - 1 do
               RespostaConsultainfoCRTom(i, j);
          end;

          for i := 0 to infoTotalContrib.RPrest.Count -1 do
            RespostaConsultaRPrest(i);

          for i := 0 to infoTotalContrib.RRecRepAD.Count -1 do
            RespostaConsultaRRecRepAD(i);

          for i := 0 to infoTotalContrib.RComl.Count -1 do
            RespostaConsultaRComl(i);

          for i := 0 to infoTotalContrib.RCPRB.Count -1 do
            RespostaConsultaRCPRB(i);
        end;
      end;
    end;
  end;
end;

{ TMetodoCriarEnviarReinf }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini Reinf
                         ou Uma String com conteúdo txt do Reinf
}

procedure TMetodoCriarEnviarReinf.Executar;
var
  AIniFile, ArqReinf, Resp : String;
  ASalvar : Boolean;
  iEvento : Integer;
begin
  AIniFile := fpCmd.Params(0);

  if not FileExists(AIniFile) then
    raise Exception.Create(ACBrStr( Format(SErroReinfAbrir, [AIniFile]) ));

  with TACBrObjetoReinf(fpObjetoDono) do
  begin
    ACBrReinf.Eventos.LoadFromIni(AIniFile);

    ASalvar := ACBrReinf.Configuracoes.Geral.Salvar;

    if not ASalvar then
    begin
      ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + CPathLogs);
      ACBrReinf.Configuracoes.Arquivos.PathSalvar :=
        PathWithDelim(ExtractFilePath(Application.ExeName)) + CPathLogs;
    end;

    iEvento:= ACBrReinf.Eventos.Gerados.Count - 1;
    ArqReinf:= ACBrReinf.Eventos.Gerados.Items[ iEvento ].PathNome + CExtensaoXML ;

    if not FileExists(ArqReinf) then
      raise Exception.Create(ACBrStr( Format(SErroReinfAbrir, [ArqReinf]) ));

    Resp := ArqReinf + sLineBreak + ACBrStr( Format(SMsgReinfEventoAdicionado,
         [TipoEventoToStr(ACBrReinf.Eventos.Gerados.Items[iEvento].TipoEvento)]) ) +
         sLineBreak;

    fpCmd.Resposta := Resp;

    ACBrReinf.Enviar;

    RespostaEnvio;

    ACBrReinf.Eventos.Clear;
  end;
end;

{ TMetodoEnviarReinf }

{ Params: 0 - Assina: 1 para assinar XML
}

procedure TMetodoEnviarReinf.Executar;
var
  AAssina: Boolean;
begin
  AAssina := StrToBoolDef(fpCmd.Params(0), False);

  with TACBrObjetoReinf(fpObjetoDono) do
  begin
    if (ACBrReinf.Eventos.Count <= 0) then
      raise Exception.Create(ACBrStr( SErroReinfNenhumEvento ));

    if (AAssina) then
      ACBrReinf.AssinarEventos;

    ACBrReinf.Enviar;

    RespostaEnvio;

    ACBrReinf.Eventos.Clear;
  end;
end;


{ TMetodoCriarEventoReinf }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini Reinf
                         ou Uma String com conteúdo txt do MDFe
          1 - RetornaXML: 1 para Retornar XML Gerado na Resposta
}

procedure TMetodoCriarEventoReinf.Executar;
var
  ARetornaXML, ASalvar: Boolean;
  AIni, Resp, ArqReinf: String;
  SL: TStringList;
  iEvento: Integer;
begin
  AIni := fpCmd.Params(0);
  ARetornaXML := StrToBoolDef(fpCmd.Params(1), False);

  if not FileExists(AIni) then
      raise Exception.Create(ACBrStr( Format(SErroReinfAbrir, [AIni]) ));

  with TACBrObjetoReinf(fpObjetoDono) do
  begin
    ACBrReinf.Eventos.LoadFromIni(AIni);

    ASalvar := ACBrReinf.Configuracoes.Geral.Salvar;
    if not ASalvar then
    begin
      ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + CPathLogs);
      ACBrReinf.Configuracoes.Arquivos.PathSalvar :=
        PathWithDelim(ExtractFilePath(Application.ExeName)) + CPathLogs;
    end;

    iEvento:= ACBrReinf.Eventos.Gerados.Count - 1;
    ArqReinf:= ACBrReinf.Eventos.Gerados.Items[ iEvento ].PathNome + CExtensaoXML ;

    if not FileExists(ArqReinf) then
      raise Exception.Create(ACBrStr( Format(SErroReinfAbrir, [ArqReinf]) ));

    Resp := ArqReinf + sLineBreak + ACBrStr( Format(SMsgReinfEventoAdicionado,
         [TipoEventoToStr(ACBrReinf.Eventos.Gerados.Items[iEvento].TipoEvento)]) );

    if ARetornaXML then
    begin
      SL := TStringList.Create;
      try
        SL.LoadFromFile(ArqReinf);
        Resp := Resp + sLineBreak + SL.Text;
      finally
        SL.Free;
      end;
    end;

    fpCmd.Resposta := Resp;
  end;
end;

{ TMetodoLimparReinf }

procedure TMetodoLimparReinf.Executar;
begin
  with TACBrObjetoReinf(fpObjetoDono) do
  begin
    ACBrReinf.Eventos.Clear;
    fpCmd.Resposta := SMsgReinfLimparLista;
  end;
end;

{ TMetodoSetIDContribuinte }

{ Params: 0 - idContribuinte: String
}
procedure TMetodoSetIDContribuinte.Executar;
var
  nIDContribuinte: String;
begin
  nIDContribuinte := fpCmd.Params(0);

  if EstaVazio(nIDContribuinte) then
    raise Exception.Create('Valor Nulo.');

  with TACBrObjetoReinf(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.Reinf do
      IdContribuinte := nIDContribuinte;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoSetIDTransmissor }

{ Params: 0 - idTransmissor: String
}
procedure TMetodoSetIDTransmissor.Executar;
var
  nIDTransmissor: String;
begin
  nIDTransmissor := fpCmd.Params(0);

  if EstaVazio(nIDTransmissor) then
    raise Exception.Create('Valor Nulo.');

  with TACBrObjetoReinf(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.Reinf do
      IDTransmissor := nIDTransmissor;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TACBrObjetoReinf }

constructor TACBrObjetoReinf.Create(AConfig: TMonitorConfig;
  ACBrReinf: TACBrReinf);
begin
  inherited Create(AConfig);

  fACBrReinf := ACBrReinf;

  ListaDeMetodos.Add(CMetodoCriarEventoReinf);
  ListaDeMetodos.Add(CMetodoEnviarReinf);
  ListaDeMetodos.Add(CMetodoCriarEnviarReinf);
  ListaDeMetodos.Add(CMetodoConsultarReinf);
  ListaDeMetodos.Add(CMetodoLimparReinf);
  ListaDeMetodos.Add(CMetodoCarregarXMLEventoReinf);
  ListaDeMetodos.Add(CMetodoSetIDContribuinteReinf);
  ListaDeMetodos.Add(CMetodoSetIDTransmissorReinf);
  ListaDeMetodos.Add(CMetodoConsultarReciboReinf);
end;

procedure TACBrObjetoReinf.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
  AACBrUnit: TACBrObjetoACBr;
begin
  inherited Executar(ACmd);

  with fACBrReinf.Configuracoes.Geral do
  begin
    if EstaVazio(IdContribuinte) or EstaVazio(IdTransmissor) then
      raise Exception.Create(ACBrStr( SErroIDContribuinteTransmissor ));
  end;

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoCriarEventoReinf;
    1  : AmetodoClass := TMetodoEnviarReinf;
    2  : AMetodoClass := TMetodoCriarEnviarReinf;
    3  : AMetodoClass := TMetodoConsultarReinf;
    4  : AMetodoClass := TMetodoLimparReinf;
    5  : AMetodoClass := TMetodoCarregarXMLEventoReinf;
    6  : AMetodoClass := TMetodoSetIDContribuinte;
    7  : AMetodoClass := TMetodoSetIDTransmissor;
    8  : AMetodoClass := TMetodoConsultarReciboReinf;
    else
      begin
        AACBrUnit := TACBrObjetoACBr.Create(Nil); //Instancia DoACBrUnit para validar métodos padrão para todos os objetos
        try
          AACBrUnit.Executar(ACmd);
        finally
          AACBrUnit.Free;
        end;

      end;

  end;

  if Assigned(AMetodoClass) then
  begin
    Ametodo := AMetodoClass.Create(ACmd, Self);
    try
      Ametodo.Executar;
    finally
      Ametodo.Free;
    end;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvio;
var
  i, j: Integer;
begin
  with ACBrReinf.WebServices.EnvioLote.RetEnvioLote do
  begin
    RespostaEnvioRetorno;
    RespostaEnvioideTransmissor;
    RespostaEnviostatus;
    RespostaEnviodadosRecepcaoLote(fACBrReinf.WebServices.EnvioLote.RetEnvioLote);

    for i := 0 to Status.Ocorrencias.Count - 1 do
      RespostaEnvioOcorrencias(i);

    for i := 0 to evento.Count - 1 do
    begin
      RespostaEnvioevento(i);
      RespostaEnvioevtTotal(i);
      RespostaEnvioideEvento(i);
      RespostaEnvioideContri(i);
      RespostaEnvioideStatus(i, evento.Items[i].evtTotal.IdeStatus);

      for j := 0 to evento.Items[i].evtTotal.IdeStatus.regOcorrs.Count -1 do
        RespostaEnvioregOcorrs(i, j, evento.Items[i].evtTotal.IdeStatus);

      RespostaEnvioinfoRecEv(i, evento.Items[i].evtTotal.InfoRecEv);
      RespostaEnvioinfoTotal(i);

      RespostaEnvioRTom(i, evento.Items[i]);

      for j := 0 to evento.Items[i].evtTotal.InfoTotal.RTom.infoCRTom.Count -1 do
        RespostaEnvioinfoCRTom(i, j, evento.Items[i]);

      RespostaEnvioRPrest(i, evento.Items[i]);

      for j := 0 to evento.Items[i].evtTotal.InfoTotal.RRecRepAD.Count -1 do
        RespostaEnvioRRecRepAD(i, j, evento.Items[i]);

      for j := 0 to evento.Items[i].evtTotal.InfoTotal.RComl.Count -1 do
        RespostaEnvioRComl(i, j, evento.Items[i]);

      for j := 0 to evento.Items[i].evtTotal.InfoTotal.RCPRB.Count -1 do
        RespostaEnvioRCPRB(i, j, evento.Items[i]);

      RespostaEnvioRRecEspetDest(i, evento.Items[i]);
    end;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioRetorno;
var
  Resp: TEnvioResposta;
begin
  Resp := TEnvioResposta.Create(TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.EnvioLote.RetEnvioLote do
    begin
      Resp.Id := Id;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioideTransmissor;
var
  Resp: TEnvioRespostaideTransmissor;
begin
  Resp := TEnvioRespostaideTransmissor.Create(TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.EnvioLote.RetEnvioLote do
    begin
      Resp.IdTransmissor := ideTransmissor.IdTransmissor;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnviostatus;
var
  Resp: TEnvioRespostastatus;
begin
  Resp := TEnvioRespostastatus.Create(TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.EnvioLote.RetEnvioLote do
    begin
      Resp.cdStatus    := Status.cdStatus;
      Resp.descRetorno := Status.descRetorno;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnviodadosRecepcaoLote(item: TRetEnvioLote);
var
  Resp: TEnvioRespostadadosRecepcaoLote;
begin
  Resp := TEnvioRespostadadosRecepcaoLote.Create(TpResp, codUTF8);
  try
    Resp.dhRecepcao := item.dadosRecepcaoLote.dhRecepcao;
    Resp.versaoAplicativoRecepcao := item.dadosRecepcaoLote.versaoAplicativoRecepcao;
    Resp.protocoloEnvio := item.dadosRecepcaoLote.protocoloEnvio;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioOcorrencias(ACont: Integer);
var
  Resp: TEnvioRespostaOcorrencias;
begin
  Resp := TEnvioRespostaOcorrencias.Create(CSessaoRespEnvioocorrencias + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.EnvioLote.RetEnvioLote do
    begin
      Resp.tipo                 := Status.Ocorrencias.Items[ACont].tipo;
      Resp.localizacaoErroAviso := Status.Ocorrencias.Items[ACont].Localizacao;
      Resp.codigo               := Status.Ocorrencias.Items[ACont].Codigo;
      Resp.descricao            := Status.Ocorrencias.Items[ACont].Descricao;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioevento(ACont: Integer);
var
  Resp: TEnvioRespostaevento;
begin
  Resp := TEnvioRespostaevento.Create(CSessaoRespEnvioevento + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.EnvioLote.RetEnvioLote do
    begin
      Resp.Id := evento.Items[ACont].Id;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioevtTotal(ACont: Integer);
var
  Resp: TEnvioRespostaevtTotal;
begin
  Resp := TEnvioRespostaevtTotal.Create(CSessaoRespEnvioevtTotal + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      Resp.Id := Id;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioideEvento(ACont: Integer);
var
  Resp: TRespostaideEvento;
begin
  Resp := TRespostaideEvento.Create(CSessaoRetornoideEvento + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      Resp.perApur := IdeEvento.perApur;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioideContri(ACont: Integer);
var
  Resp: TRespostaideContri;
begin
  Resp := TRespostaideContri.Create(CSessaoRetornoideContri + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
    begin
      resp.tpInsc := TpInscricaoToStr(IdeContrib.TpInsc);
      resp.nrInsc := IdeContrib.nrInsc;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioideStatus(ACont: Integer; item: TIdeStatus);
var
  Resp: TRespostaideStatus;
begin
  Resp := TRespostaideStatus.Create(CSessaoRetornoideStatus + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    resp.cdRetorno   := item.cdRetorno;
    resp.descRetorno := item.descRetorno;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioregOcorrs(ACont, ACont2: Integer; item: TIdeStatus);
var
  Resp: TRespostaregOcorrs;
begin
  Resp := TRespostaregOcorrs.Create(CSessaoRetornoregOcorrs +
                      IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    resp.tpOcorr        := item.regOcorrs.Items[ACont2].tpOcorr;
    resp.localErroAviso := item.regOcorrs.Items[ACont2].localErroAviso;
    resp.codResp        := item.regOcorrs.Items[ACont2].codResp;
    resp.dscResp        := item.regOcorrs.Items[ACont2].dscResp;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioinfoRecEv(ACont: Integer; item: TInfoRecEv);
var
  Resp: TRespostainfoRecEv;
begin
  Resp := TRespostainfoRecEv.Create(CSessaoRetornoinfoRecEv + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    resp.nrRecArqBase:= item.nrRecArqBase;
    resp.nrProtLote := item.nrProtLote;
    resp.dhRecepcao := item.dhRecepcao;
    resp.nrProtEntr := item.nrProtEntr;
    resp.dhProcess  := item.dhProcess;
    resp.tpEv       := item.tpEv;
    resp.idEv       := item.idEv;
    resp.hash       := item.hash;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioinfoTotal(ACont: Integer);
var
  Resp: TRespostainfoTotal_infoTotalContrib;
begin
  Resp := TRespostainfoTotal_infoTotalContrib.Create(CSessaoRespEnvioinfoTotal +
                                              IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    if fACBrReinf.Configuracoes.Geral.VersaoDF < v2_01_01 then
    begin
      with fACBrReinf.WebServices.EnvioLote.RetEnvioLote.evento.Items[ACont].evtTotal do
      begin
        resp.nrRecArqBase := InfoTotal.nrRecArqBase;
      end;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioRTom(ACont: Integer; item: TeventoCollectionItem);
var
  Resp: TRespostaRTom;
begin
  Resp := TRespostaRTom.Create(CSessaoRetornoRTom +
                                              IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    with item.evtTotal do
    begin
      resp.cnpjPrestador     := InfoTotal.RTom.cnpjPrestador;
      resp.vlrTotalBaseRet   := InfoTotal.RTom.vlrTotalBaseRet;
      resp.vlrTotalRetPrinc  := InfoTotal.RTom.vlrTotalRetPrinc;
      resp.vlrTotalRetAdic   := InfoTotal.RTom.vlrTotalRetAdic;
      resp.vlrTotalNRetPrinc := InfoTotal.RTom.vlrTotalNRetPrinc;
      resp.vlrTotalNRetAdic  := InfoTotal.RTom.vlrTotalNRetAdic;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioinfoCRTom(ACont, ACont2: Integer; item: TeventoCollectionItem);
var
  Resp: TRespostainfoCRTom;
begin
  Resp := TRespostainfoCRTom.Create(CSessaoRetornoinfoCRTom +
                  IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 1), TpResp, codUTF8);
  try
    with item.evtTotal do
    begin
      resp.CRTom        := InfoTotal.RTom.infoCRTom.Items[ACont2].CRTom;
      resp.VlrCRTom     := InfoTotal.RTom.infoCRTom.Items[ACont2].VlrCRTom;
      resp.VlrCRTomSusp := InfoTotal.RTom.infoCRTom.Items[ACont2].VlrCRTomSusp;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioRPrest(ACont: Integer; item: TeventoCollectionItem);
var
  Resp: TRespostaRPrest;
begin
  Resp := TRespostaRPrest.Create(CSessaoRetornoRPrest +
                                              IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    with item.evtTotal do
    begin
      resp.tpInscTomador     := TpInscricaoToStr(InfoTotal.RPrest.tpInscTomador);
      resp.nrInscTomador     := InfoTotal.RPrest.nrInscTomador;
      resp.vlrTotalBaseRet   := InfoTotal.RPrest.vlrTotalBaseRet;
      resp.vlrTotalRetPrinc  := InfoTotal.RPrest.vlrTotalRetPrinc;
      resp.vlrTotalRetAdic   := InfoTotal.RPrest.vlrTotalRetAdic;
      resp.vlrTotalNRetPrinc := InfoTotal.RPrest.vlrTotalNRetPrinc;
      resp.vlrTotalNRetAdic  := InfoTotal.RPrest.vlrTotalNRetAdic;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioRRecRepAD(ACont, ACont2: Integer; item: TeventoCollectionItem);
var
  Resp: TRespostaRRecRepAD;
begin
  Resp := TRespostaRRecRepAD.Create(CSessaoRetornoRRecRepAD +
                  IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    with item.evtTotal do
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

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioRComl(ACont, ACont2: Integer; item: TeventoCollectionItem);
var
  Resp: TRespostaRComl;
begin
  Resp := TRespostaRComl.Create(CSessaoRetornoRComl +
                  IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 1), TpResp, codUTF8);
  try
    with item.evtTotal do
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

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioRAquis(ACont, ACont2: Integer; item: TeventoCollectionItem);
var
  Resp: TRespostaRAquis;
begin
  Resp := TRespostaRAquis.Create(CSessaoRetornoRAquis +
                  IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 1), TpResp, codUTF8);
  try
    with item.evtTotal do
    begin
      resp.CRAquis        := InfoTotal.RAquis.Items[ACont2].CRAquis;
      resp.vlrCRAquis     := InfoTotal.RAquis.Items[ACont2].vlrCRAquis;
      resp.vlrCRAquisSusp := InfoTotal.RAquis.Items[ACont2].vlrCRAquisSusp;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioRCPRB(ACont, ACont2: Integer; item: TeventoCollectionItem);
var
  Resp: TRespostaRCPRB;
begin
  Resp := TRespostaRCPRB.Create(CSessaoRetornoRCPRB +
                  IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 1), TpResp, codUTF8);
  try
    with item.evtTotal do
    begin
      resp.codRec         := InfoTotal.RCPRB.Items[ACont2].codRec;
      resp.vlrCPApurTotal := InfoTotal.RCPRB.Items[ACont2].vlrCPApurTotal;
      resp.vlrCPRBSusp    := InfoTotal.RCPRB.Items[ACont2].vlrCPRBSusp;

      // Versão 1.03.02
      resp.CRCPRB         := InfoTotal.RCPRB.Items[ACont2].CRCPRB;
      resp.vlrCRCPRB      := InfoTotal.RCPRB.Items[ACont2].vlrCRCPRB;
      resp.vlrCRCPRBSusp  := InfoTotal.RCPRB.Items[ACont2].vlrCRCPRBSusp;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioRRecEspetDest(ACont: Integer; item: TeventoCollectionItem);
var
  Resp: TEnvioRespostaRRecEspetDesp;
begin
  Resp := TEnvioRespostaRRecEspetDesp.Create(CSessaoRetornoRRecEspetDesp +
                  IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    with item.evtTotal do
    begin
      resp.vlrReceitaTotal := InfoTotal.RRecEspetDesp.vlrReceitaTotal;
      resp.vlrCPApurTotal  := InfoTotal.RRecEspetDesp.vlrCPApurTotal;
      resp.vlrCPSuspTotal  := InfoTotal.RRecEspetDesp.vlrCPSuspTotal;

      // Versão 1.03.02
      resp.CRRecEspetDesp        := InfoTotal.RRecEspetDesp.CRRecEspetDesp;
      resp.vlrCRRecEspetDesp     := InfoTotal.RRecEspetDesp.vlrCRRecEspetDesp;
      resp.vlrCRRecEspetDespSusp := InfoTotal.RRecEspetDesp.vlrCRRecEspetDespSusp;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsulta(ASessao: String; AId: String);
var
  Resp: TConsultaResposta;
begin
  Resp := TConsultaResposta.Create(TpResp, codUTF8, ASessao);
  try
    resp.Id := AId;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultaideEvento(item: TIdeEvento1);
var
  Resp: TRespostaideEvento;
begin
  Resp := TRespostaideEvento.Create(CSessaoRetornoideEvento, TpResp, codUTF8);
  try
    resp.perApur := item.perApur;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultaideContri(item: TideContrib; status: TStatus);
var
  Resp: TRespostaideContri;
begin
  Resp := TRespostaideContri.Create(CSessaoRetornoideContri, TpResp, codUTF8);
  try
    if Assigned(status) then
    begin
      resp.Codigo   := IntToStr(status.cdStatus);
      resp.Mensagem := status.descRetorno;
    end;

    resp.tpInsc := TpInscricaoToStr(item.TpInsc);
    resp.nrInsc := item.nrInsc;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultaideStatus;
var
  Resp: TRespostaideStatus;
begin
  Resp := TRespostaideStatus.Create(CSessaoRetornoideStatus, TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.evtTotalContribVersao do
    begin
      resp.cdRetorno   := IdeStatus.cdRetorno;
      resp.descRetorno := IdeStatus.descRetorno;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultaregOcorrs(ACont: Integer; item: TIdeStatus);
var
  Resp: TRespostaregOcorrs;
begin
  Resp := TRespostaregOcorrs.Create(CSessaoRetornoregOcorrs + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    resp.tpOcorr        := item.regOcorrs.Items[ACont].tpOcorr;
    resp.localErroAviso := item.regOcorrs.Items[ACont].localErroAviso;
    resp.codResp        := item.regOcorrs.Items[ACont].codResp;
    resp.dscResp        := item.regOcorrs.Items[ACont].dscResp;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultainfoRecEv;
var
  Resp: TRespostainfoRecEv;
begin
  Resp := TRespostainfoRecEv.Create(CSessaoRetornoinfoRecEv, TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.evtTotalContribVersao do
    begin
      resp.nrProtEntr := InfoRecEv.nrProtEntr;
      resp.dhProcess  := InfoRecEv.dhProcess;
      resp.tpEv       := InfoRecEv.tpEv;
      resp.idEv       := InfoRecEv.idEv;
      resp.hash       := InfoRecEv.hash;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultainfoTotalContrib;
var
  Resp: TRespostainfoTotal_infoTotalContrib;
begin
  Resp := TRespostainfoTotal_infoTotalContrib.Create(CSessaoRespConsultainfoTotalContrib, TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.evtTotalContribVersao do
    begin
      resp.nrRecArqBase := InfoTotalContrib.nrRecArqBase;
      resp.indExistInfo := indExistInfoToStr(InfoTotalContrib.indExistInfo);
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultaRTom(ACont: Integer);
var
  Resp: TRespostaRTom;
begin
  Resp := TRespostaRTom.Create(CSessaoRetornoRTom + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.evtTotalContribVersao do
    begin
      resp.cnpjPrestador     := InfoTotalContrib.RTom.Items[ACont].cnpjPrestador;
      resp.vlrTotalBaseRet   := InfoTotalContrib.RTom.Items[ACont].vlrTotalBaseRet;
      resp.vlrTotalRetPrinc  := InfoTotalContrib.RTom.Items[ACont].vlrTotalRetPrinc;
      resp.vlrTotalRetAdic   := InfoTotalContrib.RTom.Items[ACont].vlrTotalRetAdic;
      resp.vlrTotalNRetPrinc := InfoTotalContrib.RTom.Items[ACont].vlrTotalNRetPrinc;
      resp.vlrTotalNRetAdic  := InfoTotalContrib.RTom.Items[ACont].vlrTotalNRetAdic;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultainfoCRTom(ACont, ACont2: Integer);
var
  Resp: TRespostainfoCRTom;
begin
  Resp := TRespostainfoCRTom.Create(CSessaoRetornoinfoCRTom +
                  intToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 1), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.evtTotalContribVersao do
    begin
      resp.CRTom        := InfoTotalContrib.RTom.Items[ACont].infoCRTom.Items[ACont2].CRTom;
      resp.VlrCRTom     := InfoTotalContrib.RTom.Items[ACont].infoCRTom.Items[ACont2].VlrCRTom;
      resp.VlrCRTomSusp := InfoTotalContrib.RTom.Items[ACont].infoCRTom.Items[ACont2].VlrCRTomSusp;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultaRPrest(ACont: Integer);
var
  Resp: TRespostaRPrest;
begin
  Resp := TRespostaRPrest.Create(CSessaoRetornoRPrest + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.evtTotalContribVersao do
    begin
      resp.tpInscTomador     := TpInscricaoToStr(InfoTotalContrib.RPrest.Items[ACont].tpInscTomador);
      resp.nrInscTomador     := InfoTotalContrib.RPrest.Items[ACont].nrInscTomador;
      resp.vlrTotalBaseRet   := InfoTotalContrib.RPrest.Items[ACont].vlrTotalBaseRet;
      resp.vlrTotalRetPrinc  := InfoTotalContrib.RPrest.Items[ACont].vlrTotalRetPrinc;
      resp.vlrTotalRetAdic   := InfoTotalContrib.RPrest.Items[ACont].vlrTotalRetAdic;
      resp.vlrTotalNRetPrinc := InfoTotalContrib.RPrest.Items[ACont].vlrTotalNRetPrinc;
      resp.vlrTotalNRetAdic  := InfoTotalContrib.RPrest.Items[ACont].vlrTotalNRetAdic;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultaRRecRepAD(ACont: Integer);
var
  Resp: TRespostaRRecRepAD;
begin
  Resp := TRespostaRRecRepAD.Create(CSessaoRetornoRRecRepAD + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.evtTotalContribVersao do
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

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultaRComl(ACont: Integer);
var
  Resp: TRespostaRComl;
begin
  Resp := TRespostaRComl.Create(CSessaoRetornoRComl + IntToStrZero(ACont+1, 1), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.evtTotalContribVersao do
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

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultaRCPRB(ACont: Integer);
var
  Resp: TRespostaRCPRB;
begin
  Resp := TRespostaRCPRB.Create(CSessaoRetornoRCPRB + IntToStrZero(ACont+1, 1), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.evtTotalContribVersao do
    begin
      resp.codRec         := InfoTotalContrib.RCPRB.Items[ACont].codRec;
      resp.vlrCPApurTotal := InfoTotalContrib.RCPRB.Items[ACont].vlrCPApurTotal;
      resp.vlrCPRBSusp    := InfoTotalContrib.RCPRB.Items[ACont].vlrCPRBSusp;

      // Versão 1.03.02
      resp.CRCPRB         := InfoTotalContrib.RCPRB.Items[ACont].CRCPRB;
      resp.vlrCRCPRB      := InfoTotalContrib.RCPRB.Items[ACont].vlrCRCPRB;
      resp.vlrCRCPRBSusp  := InfoTotalContrib.RCPRB.Items[ACont].vlrCRCPRBSusp;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostainfoTotalevtRet(ACont: Integer);
var
  Resp: TPadraoReinfResposta;
begin
  Resp := TPadraoReinfResposta.Create(CSessaoRespEnvioinfoTotal +
                                      IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaideEstab(ACont: Integer);
var
  Resp: TEnvioRespostaideEstab;
begin
  Resp := TEnvioRespostaideEstab.Create(CSessaoRespideEstab +
                                        IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal do
    begin
      resp.tpInsc      := TpInscricaoToStr(ideEstab.tpInsc);
      resp.nrInsc      := ideEstab.nrInsc;
      resp.nrInscBenef := ideEstab.nrInscBenef;
      resp.nmBenef     := ideEstab.nmBenef;
      resp.ideEvtAdic  := ideEstab.ideEvtAdic;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostatotApurMen(ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurMen;
begin
  Resp := TEnvioRespostatotApurMen.Create(CSessaoResptottotApurMen +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab do
    begin
      resp.CRMen            := totApurMen.Items[ACont2].CRMen;
      resp.vlrBaseCRMen     := totApurMen.Items[ACont2].vlrBaseCRMen;
      resp.vlrBaseCRMenSusp := totApurMen.Items[ACont2].vlrBaseCRMenSusp;
      resp.natRend          := totApurMen.Items[ACont2].natRend;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostatotApurTribMen(ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurTribMen;
begin
  Resp := TEnvioRespostatotApurTribMen.Create(CSessaoResptotApurTribMen +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab.totApurMen.Items[ACont2] do
    begin
      resp.vlrCRMenInf      := totApurTribMen.vlrCRMenInf;
      resp.vlrCRMenCalc     := totApurTribMen.vlrCRMenCalc;
      resp.vlrCRMenSuspInf  := totApurTribMen.vlrCRMenSuspInf;
      resp.vlrCRMenSuspCalc := totApurTribMen.vlrCRMenSuspCalc;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostatotApurQui(ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurQui;
begin
  Resp := TEnvioRespostatotApurQui.Create(CSessaoResptottotApurQui +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab do
    begin
      resp.perApurQui       := tpPerApurQuiToStr(totApurQui.Items[ACont2].perApurQui);
      resp.CRQui            := totApurQui.Items[ACont2].CRQui;
      resp.vlrBaseCRQui     := totApurQui.Items[ACont2].vlrBaseCRQui;
      resp.vlrBaseCRQuiSusp := totApurQui.Items[ACont2].vlrBaseCRQuiSusp;
      resp.natRend          := totApurQui.Items[ACont2].natRend;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostatotApurTribQui(ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurTribQui;
begin
  Resp := TEnvioRespostatotApurTribQui.Create(CSessaoResptotApurTribQui +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab.totApurQui.Items[ACont2] do
    begin
      resp.vlrCRQuiInf      := totApurTribQui.vlrCRQuiInf;
      resp.vlrCRQuiCalc     := totApurTribQui.vlrCRQuiCalc;
      resp.vlrCRQuiSuspInf  := totApurTribQui.vlrCRQuiSuspInf;
      resp.vlrCRQuiSuspCalc := totApurTribQui.vlrCRQuiSuspCalc;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostatotApurDec(ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurDec;
begin
  Resp := TEnvioRespostatotApurDec.Create(CSessaoResptottotApurDec +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab do
    begin
      resp.perApurDec       := tpPerApurDecToStr(totApurDec.Items[ACont2].perApurDec);
      resp.CRDec            := totApurDec.Items[ACont2].CRDec;
      resp.vlrBaseCRDec     := totApurDec.Items[ACont2].vlrBaseCRDec;
      resp.vlrBaseCRDecSusp := totApurDec.Items[ACont2].vlrBaseCRDecSusp;
      resp.natRend          := totApurDec.Items[ACont2].natRend;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostatotApurTribDec(ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurTribDec;
begin
  Resp := TEnvioRespostatotApurTribDec.Create(CSessaoResptotApurTribDec +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab.totApurDec.Items[ACont2] do
    begin
      resp.vlrCRDecInf      := totApurTribDec.vlrCRDecInf;
      resp.vlrCRDecCalc     := totApurTribDec.vlrCRDecCalc;
      resp.vlrCRDecSuspInf  := totApurTribDec.vlrCRDecSuspInf;
      resp.vlrCRDecSuspCalc := totApurTribDec.vlrCRDecSuspCalc;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostatotApurSem(ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurSem;
begin
  Resp := TEnvioRespostatotApurSem.Create(CSessaoResptottotApurSem +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab do
    begin
      resp.perApurSem       := tpPerApurSemToStr(totApurSem.Items[ACont2].perApurSem);
      resp.CRSem            := totApurSem.Items[ACont2].CRSem;
      resp.vlrBaseCRSem     := totApurSem.Items[ACont2].vlrBaseCRSem;
      resp.vlrBaseCRSemSusp := totApurSem.Items[ACont2].vlrBaseCRSemSusp;
      resp.natRend          := totApurSem.Items[ACont2].natRend;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostatotApurTribSem(ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurTribSem;
begin
  Resp := TEnvioRespostatotApurTribSem.Create(CSessaoResptotApurTribSem +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab.totApurSem.Items[ACont2] do
    begin
      resp.vlrCRSemInf      := totApurTribSem.vlrCRSemInf;
      resp.vlrCRSemCalc     := totApurTribSem.vlrCRSemCalc;
      resp.vlrCRSemSuspInf  := totApurTribSem.vlrCRSemSuspInf;
      resp.vlrCRSemSuspCalc := totApurTribSem.vlrCRSemSuspCalc;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostatotApurDia(ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurDia;
begin
  Resp := TEnvioRespostatotApurDia.Create(CSessaoResptottotApurDia +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab do
    begin
      resp.perApurDia       := totApurDia.Items[ACont2].perApurDia;
      resp.CRDia            := totApurDia.Items[ACont2].CRDia;
      resp.vlrBaseCRDia     := totApurDia.Items[ACont2].vlrBaseCRDia;
      resp.vlrBaseCRDiaSusp := totApurDia.Items[ACont2].vlrBaseCRDiaSusp;
      resp.natRend          := totApurDia.Items[ACont2].natRend;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostatotApurTribDia(ACont, ACont2: Integer);
var
  Resp: TEnvioRespostatotApurTribDia;
begin
  Resp := TEnvioRespostatotApurTribDia.Create(CSessaoResptotApurTribDia +
            IntToStrZero(ACont+1, 3) + IntToStrZero(ACont2+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.Consultar.RetEnvioLote.evento.Items[ACont].evtRet.InfoTotal.ideEstab.totApurDia.Items[ACont2] do
    begin
      resp.vlrCRDiaInf      := totApurTribDia.vlrCRDiaInf;
      resp.vlrCRDiaCalc     := totApurTribDia.vlrCRDiaCalc;
      resp.vlrCRDiaSuspInf  := totApurTribDia.vlrCRDiaSuspInf;
      resp.vlrCRDiaSuspCalc := totApurTribDia.vlrCRDiaSuspCalc;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultainfoCR_CNR(item: TinfoCR_CNR);
var
  Resp: TConsultaRespostainfoCR_CNR;
begin
  Resp := TConsultaRespostainfoCR_CNR.Create(CSessaoRespinfoCR_CNR, TpResp, codUTF8);
  try
    resp.indExistInfo    := indExistInfoToStr(item.indExistInfo);
    resp.identEscritDCTF := item.identEscritDCTF;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultatotApurMen(ACont: Integer;
 item: TtotApurMenCollection);
var
  Resp: TConsultaRespostatotApurMen;
begin
  Resp := TConsultaRespostatotApurMen.Create(CSessaoResptottotApurMen +
            IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    resp.CRMen            := item.Items[ACont].CRMen;
    resp.vlrCRMenInf      := item.Items[ACont].vlrCRMenInf;
    resp.vlrCRMenCalc     := item.Items[ACont].vlrCRMenCalc;
    resp.vlrCRMenDCTF     := item.Items[ACont].vlrCRMenDCTF;
    resp.vlrCRMenSuspInf  := item.Items[ACont].vlrCRMenSuspInf;
    resp.vlrCRMenSuspCalc := item.Items[ACont].vlrCRMenSuspCalc;
    resp.vlrCRMenSuspDCTF := item.Items[ACont].vlrCRMenSuspDCTF;
    resp.natRend          := item.Items[ACont].natRend;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultatotApurQui(ACont: Integer;
 item: TtotApurQuiCollection);
var
  Resp: TConsultaRespostatotApurQui;
begin
  Resp := TConsultaRespostatotApurQui.Create(CSessaoResptottotApurQui +
            IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    resp.perApurQui       := item.Items[ACont].perApurQui;
    resp.CRQui            := item.Items[ACont].CRQui;
    resp.vlrCRQuiInf      := item.Items[ACont].vlrCRQuiInf;
    resp.vlrCRQuiCalc     := item.Items[ACont].vlrCRQuiCalc;
    resp.vlrCRQuiDCTF     := item.Items[ACont].vlrCRQuiDCTF;
    resp.vlrCRQuiSuspInf  := item.Items[ACont].vlrCRQuiSuspInf;
    resp.vlrCRQuiSuspCalc := item.Items[ACont].vlrCRQuiSuspCalc;
    resp.vlrCRQuiSuspDCTF := item.Items[ACont].vlrCRQuiSuspDCTF;
    resp.natRend          := item.Items[ACont].natRend;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultatotApurDec(ACont: Integer;
 item: TtotApurDecCollection);
var
  Resp: TConsultaRespostatotApurDec;
begin
  Resp := TConsultaRespostatotApurDec.Create(CSessaoResptottotApurDec +
            IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    resp.perApurDec       := item.Items[ACont].perApurDec;
    resp.CRDec            := item.Items[ACont].CRDec;
    resp.vlrCRDecInf      := item.Items[ACont].vlrCRDecInf;
    resp.vlrCRDecCalc     := item.Items[ACont].vlrCRDecCalc;
    resp.vlrCRDecDCTF     := item.Items[ACont].vlrCRDecDCTF;
    resp.vlrCRDecSuspInf  := item.Items[ACont].vlrCRDecSuspInf;
    resp.vlrCRDecSuspCalc := item.Items[ACont].vlrCRDecSuspCalc;
    resp.vlrCRDecSuspDCTF := item.Items[ACont].vlrCRDecSuspDCTF;
    resp.natRend          := item.Items[ACont].natRend;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultatotApurSem(ACont: Integer;
 item: TtotApurSemCollection);
var
  Resp: TConsultaRespostatotApurSem;
begin
  Resp := TConsultaRespostatotApurSem.Create(CSessaoResptottotApurSem +
            IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    resp.perApurSem       := item.Items[ACont].perApurSem;
    resp.CRSem            := item.Items[ACont].CRSem;
    resp.vlrCRSemInf      := item.Items[ACont].vlrCRSemInf;
    resp.vlrCRSemCalc     := item.Items[ACont].vlrCRSemCalc;
    resp.vlrCRSemDCTF     := item.Items[ACont].vlrCRSemDCTF;
    resp.vlrCRSemSuspInf  := item.Items[ACont].vlrCRSemSuspInf;
    resp.vlrCRSemSuspCalc := item.Items[ACont].vlrCRSemSuspCalc;
    resp.vlrCRSemSuspDCTF := item.Items[ACont].vlrCRSemSuspDCTF;
    resp.natRend          := item.Items[ACont].natRend;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultatotApurDia(ACont: Integer;
 item: TtotApurDiaCollection);
var
  Resp: TConsultaRespostatotApurDia;
begin
  Resp := TConsultaRespostatotApurDia.Create(CSessaoResptottotApurDia +
            IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    resp.perApurDia       := item.Items[ACont].perApurDia;
    resp.CRDia            := item.Items[ACont].CRDia;
    resp.vlrCRDiaInf      := item.Items[ACont].vlrCRDiaInf;
    resp.vlrCRDiaCalc     := item.Items[ACont].vlrCRDiaCalc;
    resp.vlrCRDiaDCTF     := item.Items[ACont].vlrCRDiaDCTF;
    resp.vlrCRDiaSuspInf  := item.Items[ACont].vlrCRDiaSuspInf;
    resp.vlrCRDiaSuspCalc := item.Items[ACont].vlrCRDiaSuspCalc;
    resp.vlrCRDiaSuspDCTF := item.Items[ACont].vlrCRDiaSuspDCTF;
    resp.natRend          := item.Items[ACont].natRend;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultainfoTotalCR;
var
  Resp: TPadraoReinfResposta;
begin
  Resp := TPadraoReinfResposta.Create(CSessaoRespinfoTotalCR, TpResp, codUTF8);
  try
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultaReciboStatus;
var
  Resp: TRespostaideStatus;
begin
  Resp := TRespostaideStatus.Create(CSessaoRetornoideStatus, TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.ConsultarReciboEvento.evtTotalContribVersao do
    begin
      resp.cdRetorno   := IdeStatus.cdRetorno;
      resp.descRetorno := IdeStatus.descRetorno;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsultaReciboOcorrs(ACont: Integer);
var
  Resp: TRespostaregOcorrs;
begin
  Resp := TRespostaregOcorrs.Create(CSessaoRetornoregOcorrs + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.ConsultarReciboEvento.evtTotalContribVersao do
    begin
      resp.tpOcorr        := IdeStatus.regOcorrs.Items[ACont].tpOcorr;
      resp.localErroAviso := IdeStatus.regOcorrs.Items[ACont].localErroAviso;
      resp.codResp        := IdeStatus.regOcorrs.Items[ACont].codResp;
      resp.dscResp        := IdeStatus.regOcorrs.Items[ACont].dscResp;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoReinf.RespostaEventoRecibo(ACont: Integer);
var
  Resp: TRespostaEventoRecibo;
begin
  Resp := TRespostaEventoRecibo.Create(CSessaoRetornoEventoRecibo + IntToStrZero(ACont+1, 3), TpResp, codUTF8);
  try
    with fACBrReinf.WebServices.ConsultarReciboEvento.evtTotalContribVersao do
    begin
      resp.id              := RetornoEventos.Items[ACont].id;
      resp.InicioValidade  := RetornoEventos.Items[ACont].iniValid;
      resp.DataHoraReceb   := RetornoEventos.Items[ACont].dtHoraRecebimento;
      resp.NrRecibo        := RetornoEventos.Items[ACont].nrRecibo;
      resp.SituacaoEvento  := RetornoEventos.Items[ACont].situacaoEvento;
      resp.AplicacaoRecepcao:= RetornoEventos.Items[ACont].aplicacaoRecepcao;
    end;

    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

end.

