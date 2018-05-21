{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simões de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na página do Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Este programa é software livre; você pode redistribuí-lo e/ou modificá-lo   }
{ sob os termos da Licença Pública Geral GNU, conforme publicada pela Free     }
{ Software Foundation; tanto a versão 2 da Licença como (a seu critério)       }
{ qualquer versão mais nova.                                                   }
{                                                                              }
{  Este programa é distribuído na expectativa de ser útil, mas SEM NENHUMA     }
{ GARANTIA; nem mesmo a garantia implícita de COMERCIALIZAÇÃO OU DE ADEQUAÇÃO A}
{ QUALQUER PROPÓSITO EM PARTICULAR. Consulte a Licença Pública Geral GNU para  }
{ obter mais detalhes. (Arquivo LICENCA.TXT ou LICENSE.TXT)                    }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral GNU junto com este}
{ programa; se não, escreva para a Free Software Foundation, Inc., 59 Temple   }
{ Place, Suite 330, Boston, MA 02111-1307, USA. Você também pode obter uma     }
{ copia da licença em:  http://www.opensource.org/licenses/gpl-license.php     }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170         }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit DoACBrReinfUnit;

interface

uses
  Classes, SysUtils, ACBrUtil, ACBrReinf, ACBrMonitorConfig,
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
  procedure RespostaEnvioOcorrencia(ACont: Integer);

  procedure RespostaEnvioConsulta;
  procedure RespostaConsulta(ACont: Integer);
  procedure RespostaOcorrencia(ACont: Integer);overload;
  procedure RespostaOcorrencia(ACont, ACont2: Integer);overload;

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
      RespostaEnvioConsulta;

//          if Evento. ideStatus.cdRetorno in [201, 202] then
//          begin
//            for i := 0 to retEventos.Count - 1 do
//            begin
//              RespostaConsulta(i);
//              if retEventos.Items[i].Processamento.Ocorrencias.Count > 0 then
//                for J := 0 to retEventos.Items[i].Processamento.Ocorrencias.Count - 1 do
//                  RespostaOcorrencia(i, j);

//              for J := 0 to retEventos.Items[i].tot.Count - 1 do
//                RespostaTot(i, j);
//            end;

//          end
//          else
//            for i := 0 to Status.Ocorrencias.Count - 1 do
//              RespostaOcorrencia(i);

          (*
                    Add(' **Ocorrencias');

                    for j := 0 to IdeStatus.regOcorrs.Count - 1 do
                    begin
                      with IdeStatus.regOcorrs.Items[j] do
                      begin
                        Add('   Tipo............: ' + Inttostr(tpOcorr));
                        Add('   Local Erro Aviso: ' + localErroAviso);
                        Add('   Código Resp.... : ' + codResp);
                        Add('   Descricao Resp..: ' + dscResp);
                      end;
                    end;
                  end;
                end;
            end;
          end;
          *)

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
  i, iEvento : Integer;
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
//    Sleep(3000);

    with ACBrReinf.WebServices.EnvioLote.RetEnvioLote do
    begin
      if Status.cdStatus in [201, 202] then
        RespostaEnvio
      else
        for i := 0 to Status.Ocorrencias.Count - 1 do
          RespostaEnvioOcorrencia(i);

    end;
    ACBrReinf.Eventos.Clear;

  end;

end;

{ TMetodoEnviarReinf }

{ Params: 0 - Assina: 1 para assinar XML
}

procedure TMetodoEnviarReinf.Executar;
var
  AAssina: Boolean;
  i: Integer;
begin
//  AGrupo := fpCmd.Params(0);
  AAssina := StrToBoolDef(fpCmd.Params(0), False);

  with TACBrObjetoReinf(fpObjetoDono) do
  begin
    if (ACBrReinf.Eventos.Count <= 0) then
      raise Exception.Create(ACBrStr( SErroReinfNenhumEvento ));

    if (AAssina) then
      ACBrReinf.AssinarEventos;

    ACBrReinf.Enviar;
//    Sleep(3000);

    with ACBrReinf.WebServices.EnvioLote.RetEnvioLote do
    begin
      if Status.cdStatus in [201, 202] then
        RespostaEnvio
      else
        for i := 0 to Status.Ocorrencias.Count - 1 do
          RespostaEnvioOcorrencia(i);
    end;

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

end;

procedure TACBrObjetoReinf.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
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
    6..20 : DoACbr(ACmd);

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
  Resp: TEnvioResposta;
begin
  Resp := TEnvioResposta.Create(resINI);
  try
    with fACBrReinf.WebServices.EnvioLote.RetEnvioLote do
    begin
      Resp.IdTransmissor := IdeTransmissor.IdTransmissor;
      Resp.Codigo        := IntToStr(Status.cdStatus);
      Resp.Mensagem      := Status.descRetorno;
    end;
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoReinf.RespostaEnvioOcorrencia(ACont: Integer);
var
  Resp: TOcorrenciaResposta;
begin
  Resp := TOcorrenciaResposta.Create(CSessaoRespOcorrencia+inttostr(ACont), resINI);
  try
    with fACBrReinf.WebServices.EnvioLote.RetEnvioLote do
    begin
      Resp.Codigo      := IntToStr(Status.cdStatus);
      Resp.Mensagem    := Status.descRetorno;
      Resp.CodigoOco   := Status.Ocorrencias.Items[ACont].Codigo;
      Resp.Descricao   := Status.Ocorrencias.Items[ACont].Descricao;
      Resp.Tipo        := Status.Ocorrencias.Items[ACont].Tipo;
      Resp.Localizacao := Status.Ocorrencias.Items[ACont].Localizacao;

    end;
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;

  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaEnvioConsulta;
var
  Resp: TConsultaResposta;
begin
  Resp := TConsultaResposta.Create(resINI);
  try
    with fACBrReinf.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      Resp.TpInscContri   := TpInscricaoToStr(IdeContri.TpInsc);
      Resp.NrInscContri   := IdeContri.NrInsc;
      Resp.Codigo         := ideStatus.cdRetorno;
      Resp.Mensagem       := ideStatus.descRetorno;

//      Resp.TpInscTransm   := eSTpInscricaoToStr(IdeTransmissor.TpInsc);
//      Resp.NrInscTransm   := IdeTransmissor.NrInsc;
//      Resp.DhRecepcao     := dadosRecLote.dhRecepcao;
//      Resp.VersaoAplic    := dadosRecLote.versaoAplicRecepcao;
//      Resp.Protocolo      := dadosRecLote.Protocolo;
    end;
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;

  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaOcorrencia(ACont: Integer);
var
  Resp: TOcorrenciaResposta;
begin
  Resp := TOcorrenciaResposta.Create(CSessaoRespOcorrencia+inttostr(ACont), resINI);
  try
    with fACBrReinf.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      (*
      Resp.Codigo         := Status.cdResposta;
      Resp.Mensagem       := Status.descResposta;
      Resp.CodigoOco      := Status.Ocorrencias.Items[ACont].Codigo;
      Resp.Descricao      := Status.Ocorrencias.Items[ACont].Descricao;
      Resp.Tipo           := Status.Ocorrencias.Items[ACont].Tipo;
      Resp.Localizacao    := Status.Ocorrencias.Items[ACont].Localizacao;
      *)
    end;
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoReinf.RespostaOcorrencia(ACont, ACont2: Integer);
var
  Resp: TOcorrenciaResposta;
begin
  Resp := TOcorrenciaResposta.Create(CSessaoRespOcorrencia+inttostr(ACont2), resINI);
  try
    (*
    with fACBrReinf.WebServices.Consultar.RetConsulta.RetEventos.
         Items[ACont].Processamento.Ocorrencias.Items[ACont2] do
    begin
      Resp.CodigoOco      := Codigo;
      Resp.Descricao      := Descricao;
      Resp.Tipo           := Tipo;
      Resp.Localizacao    := Localizacao;

    end;
    *)
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;

  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoReinf.RespostaConsulta(ACont: Integer);
var
  Resp: TConsultaResposta;
  i: integer;
begin
  Resp := TConsultaResposta.Create(resINI);
  try
    with fACBrReinf.WebServices.Consultar.RetConsulta.evtTotalContrib do
    begin
      resp.evtTotalContrib.Id := Id;
      resp.evtTotalContrib.IdeEvento.perApur := IdeEvento.perApur;
      resp.evtTotalContrib.IdeContri.TpInsc  := IdeContri.TpInsc;
      resp.evtTotalContrib.IdeContri.nrInsc  := IdeContri.nrInsc;

      resp.evtTotalContrib.IdeStatus.cdRetorno  := IdeStatus.cdRetorno;
      resp.evtTotalContrib.IdeStatus.descRetorno := IdeStatus.descRetorno;

      for i := 0 to IdeStatus.regOcorrs.Count -1 do
      begin
        resp.evtTotalContrib.IdeStatus.regOcorrs.Add;
        resp.evtTotalContrib.IdeStatus.regOcorrs.Items[i].tpOcorr := IdeStatus.regOcorrs.Items[i].tpOcorr;
        resp.evtTotalContrib.IdeStatus.regOcorrs.Items[i].localErroAviso := IdeStatus.regOcorrs.Items[i].localErroAviso;
        resp.evtTotalContrib.IdeStatus.regOcorrs.Items[i].codResp := IdeStatus.regOcorrs.Items[i].codResp;
        resp.evtTotalContrib.IdeStatus.regOcorrs.Items[i].dscResp := IdeStatus.regOcorrs.Items[i].dscResp;
      end;

//      resp.evtTotalContrib.InfoRecEv.nrProtEntr := InfoRecEv.nrProtEntr;
    end;
    fpCmd.Resposta := Resp.Gerar;

  finally
    Resp.Free;

  end;

end;

end.

