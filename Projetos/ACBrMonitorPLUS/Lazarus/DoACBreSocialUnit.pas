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
unit DoACBreSocialUnit;

interface

uses
  Classes, SysUtils, ACBrUtil, ACBreSocial, ACBrMonitorConfig,
  ACBrMonitorConsts, CmdUnit, pcesConversaoeSocial, DoACBrDFeUnit,
  ACBrLibResposta, ACBrLibeSocialRespostas, ACBrLibeSocialConsts,
  ACBreSocialEventos, pcesS5001, pcesS5002, pcesS5011, pcesS5012;

type

{ TACBrObjetoeSocial }

TACBrObjetoeSocial = class(TACBrObjetoDFe)
private
  fACBreSocial: TACBreSocial;
public
  constructor Create(AConfig: TMonitorConfig; ACBreSocial: TACBreSocial); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  procedure RespostaEnvio;
  procedure RespostaEnvioConsulta;
  procedure RespostaEnvioOcorrencia(ACont: Integer);
  procedure RespostaOcorrencia(ACont: Integer);overload;
  procedure RespostaOcorrencia(ACont, ACont2: Integer);overload;
  procedure RespostaConsulta(ACont: Integer);
  procedure RespostaTot(ACont, ACont2: Integer);
  procedure RespostaConsultaIdentEventosQtd;
  procedure RespostaConsultaIdentEventosRecibo(ACont: Integer);
  procedure RespostaDownload(ACount: Integer);
  procedure RespostaPadrao;

  property ACBreSocial: TACBreSocial read fACBreSocial;
end;

{ TACBrCarregareSocial }

TACBrCarregareSocial = class(TACBrCarregarDFe)
protected
  procedure CarregarDFePath( const AValue: String ); override;
  procedure CarregarDFeXML( const AValue: String ); override;
  function ValidarDFe( const AValue: String ): Boolean; override;

public
  constructor Create(AACBreSocial: TACBreSocial; AXMLorFile: String ); reintroduce;
end;

{ TMetodoCriarEventoeSocial}

TMetodoCriarEventoeSocial = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviareSocial}

TMetodoEnviareSocial = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarEnviareSocial}

TMetodoCriarEnviareSocial = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultareSocial}

TMetodoConsultareSocial = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCarregarXMLEventoeSocial}

TMetodoCarregarXMLEventoeSocial = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLimpareSocial}

TMetodoLimpareSocial = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetIDEmpregador}

TMetodoSetIDEmpregador = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetIDTransmissor}

TMetodoSetIDTransmissor = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultaIdentificadoresEventosEmpregador }

TMetodoConsultaIdentificadoresEventosEmpregador = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultaIdentificadoresEventosTabela }

TMetodoConsultaIdentificadoresEventosTabela = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultaIdentificadoresEventosTrabalhador }

TMetodoConsultaIdentificadoresEventosTrabalhador = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDownloadEventos }

TMetodoDownloadEventos = class(TACBrMetodo)
public
  procedure Executar; override;
end;


implementation

uses
  DoACBrUnit, Forms;

{ TMetodoDownloadEventos }

procedure TMetodoDownloadEventos.Executar;
var
  AIdEmpregador: String;
  AId: String;
  ANrRecibo: String;
  i: Integer;
begin
  AIdEmpregador := fpCmd.Params(0);
  AId := fpCmd.Params(1);
  ANrRecibo := fpCmd.Params(2);

  with TACBrObjetoeSocial(fpObjetoDono) do
  begin
    if ( (EstaVazio(AIdEmpregador)) ) then
      raise Exception.Create(ACBrStr(SErroeSocialConsulta));

    ACBreSocial.Eventos.Clear;
    if ACBreSocial.DownloadEventos(AIdEmpregador,
                       AID, ANrRecibo) then
    begin
      with ACBreSocial.WebServices.DownloadEventos.RetDownloadEvt  do
      begin
        RespostaPadrao;

        for i := 0 to arquivo.Count - 1 do
        begin
          RespostaDownload(i);
        end;

      end;
    end;

  end;
end;

{ TMetodoConsultaIdentificadoresEventosTrabalhador }

procedure TMetodoConsultaIdentificadoresEventosTrabalhador.Executar;
var
  AIdEmpregador: String;
  ACPFTrabalhador: String;
  ADataInicial, ADataFinal: TDateTime;
  i: Integer;
begin
  AIdEmpregador := fpCmd.Params(0);
  ACPFTrabalhador := fpCmd.Params(1);
  ADataInicial := StrToDate(fpCmd.Params(2));
  ADataFinal := StrToDate(fpCmd.Params(3));

  with TACBrObjetoeSocial(fpObjetoDono) do
  begin
    if ( (EstaVazio(AIdEmpregador)) or ( EstaVazio(ACPFTrabalhador))
       or ( ADataInicial <= 0 )  or ( ADataFinal <= 0 ) ) then
      raise Exception.Create(ACBrStr(SErroeSocialConsulta));

    ACBreSocial.Eventos.Clear;
    if ACBreSocial.ConsultaIdentificadoresEventosTrabalhador(AIdEmpregador,
                       ACPFTrabalhador, ADataInicial, ADataFinal) then
    begin
      with ACBreSocial.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt  do
      begin
        RespostaConsultaIdentEventosQtd;

        for i := 0 to RetIdentEvts.Count - 1 do
        begin
          RespostaConsultaIdentEventosRecibo(i);
        end;

      end;
    end;

  end;
end;

{ TMetodoConsultaIdentificadoresEventosTabela }

procedure TMetodoConsultaIdentificadoresEventosTabela.Executar;
var
  AIdEmpregador: String;
  ATpEvento: Integer;
  AChave: String;
  ADataInicial, ADataFinal: TDateTime;
  i: Integer;
begin
  AIdEmpregador := fpCmd.Params(0);
  ATpEvento := StrToIntDef(fpCmd.Params(1),0);
  AChave :=  fpCmd.Params(2);
  ADataInicial := StrToDateTime(fpCmd.Params(3));
  ADataFinal := StrToDate(fpCmd.Params(4));

  with TACBrObjetoeSocial(fpObjetoDono) do
  begin
    if ( (EstaVazio(AIdEmpregador)) or ( EstaVazio(AChave))
       or ( ADataInicial <= 0 )  or ( ADataFinal <= 0 ) ) then
      raise Exception.Create(ACBrStr(SErroeSocialConsulta));

    ACBreSocial.Eventos.Clear;
    if ACBreSocial.ConsultaIdentificadoresEventosTabela(AIdEmpregador,
                       TTipoEvento(ATpEvento), AChave, ADataInicial, ADataFinal) then
    begin
      with ACBreSocial.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt  do
      begin
        RespostaConsultaIdentEventosQtd;

        for i := 0 to RetIdentEvts.Count - 1 do
        begin
          RespostaConsultaIdentEventosRecibo(i);
        end;

      end;
    end;

  end;
end;

{ TMetodoConsultaIdentificadoresEventosEmpregador }

procedure TMetodoConsultaIdentificadoresEventosEmpregador.Executar;
var
  AIdEmpregador: String;
  APerApur: TDateTime;
  ATpEvento: Integer;
  i: Integer;
begin
  AIdEmpregador := fpCmd.Params(0);
  ATpEvento := StrToIntDef(fpCmd.Params(1),0);
  APerApur := StrToDate( fpCmd.Params(2));

  with TACBrObjetoeSocial(fpObjetoDono) do
  begin
    if ((APerApur <= 0) or (EstaVazio(AIdEmpregador))) then
      raise Exception.Create(ACBrStr(SErroeSocialConsulta));

    ACBreSocial.Eventos.Clear;
    if ACBreSocial.ConsultaIdentificadoresEventosEmpregador(AIdEmpregador,
                       TTipoEvento(ATpEvento), APerApur) then
    begin
      with ACBreSocial.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt  do
      begin
        RespostaConsultaIdentEventosQtd;

        for i := 0 to RetIdentEvts.Count - 1 do
        begin
          RespostaConsultaIdentEventosRecibo(i);
        end;

      end;
    end;

  end;
end;

{ TACBrCarregareSocial }

procedure TACBrCarregareSocial.CarregarDFePath(const AValue: String);
begin
  if not ( TACBreSocial(FpACBrDFe).Eventos.LoadFromFile( AValue ) ) then
    raise Exception.Create(ACBrStr( Format(SErroeSocialAbrir, [AValue]) ) );
end;

procedure TACBrCarregareSocial.CarregarDFeXML(const AValue: String);
begin
  if not ( TACBreSocial(FpACBrDFe).Eventos.LoadFromString( AValue ) ) then
    raise Exception.Create(ACBrStr(SErroeSocialCarregar) );
end;

function TACBrCarregareSocial.ValidarDFe(const AValue: String): Boolean;
begin
  Result := False;
  if ( TACBreSocial(FpACBrDFe).Eventos.Count > 0 ) then
    Result:= True;

  if EstaVazio( FPathDFe )then
    FPathDFe := PathWithDelim(TACBreSocial(FpACBrDFe).Configuracoes.Arquivos.PathSalvar)
                + AValue;

end;

constructor TACBrCarregareSocial.Create(AACBreSocial: TACBreSocial;
  AXMLorFile: String);
begin
  inherited Create(AACBreSocial, AXMLorFile);
end;

{ TMetodoCarregarXMLEventoeSocial }

{ Params: 0 - pathXML - Uma String com pathXML ou XML completo
}
procedure TMetodoCarregarXMLEventoeSocial.Executar;
var
  APathorXML: String;
  CargaDFe: TACBrCarregareSocial;
begin
  APathorXML := fpCmd.Params(0);
  with TACBrObjetoeSocial(fpObjetoDono) do
  begin
    CargaDFe := TACBrCarregareSocial.Create(ACBreSocial , APathorXML);
    try
      fpCmd.Resposta := ACBrStr( Format(SMsgeSocialEventoAdicionado,[APathorXML]) )
    finally
      CargaDFe.Free;
    end;

  end;

end;

{ TMetodoConsultareSocial }

{ Params: 0 - Protocolo - Uma String com protocolo eSocial
}
procedure TMetodoConsultareSocial.Executar;
var
  AProtocolo: String;
  i, j: Integer;
begin
  AProtocolo := fpCmd.Params(0);

  with TACBrObjetoeSocial(fpObjetoDono) do
  begin
    ACBreSocial.Eventos.Clear;
    if ACBreSocial.Consultar(AProtocolo) then
    begin
      with ACBreSocial.WebServices.ConsultaLote.RetConsultaLote do
      begin
        if Status.cdResposta in [201, 202] then
        begin
          RespostaEnvioConsulta;
          for i := 0 to retEventos.Count - 1 do
          begin
            RespostaConsulta(i);
            if retEventos.Items[i].Processamento.Ocorrencias.Count > 0 then
              for J := 0 to retEventos.Items[i].Processamento.Ocorrencias.Count - 1 do
                RespostaOcorrencia(i, j);

            for J := 0 to retEventos.Items[i].tot.Count - 1 do
              RespostaTot(i, j);
          end;

        end
        else
          for i := 0 to Status.Ocorrencias.Count - 1 do
            RespostaOcorrencia(i);

      end;

    end;

  end;

end;

{ TMetodoCriarEnviareSocial }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini eSocial
                         ou Uma String com conteúdo txt do eSocial
          1 - ( Grupo: 1- Iniciais/Tabelas 2- Nao Periodicos 3- Periódicos )
}

procedure TMetodoCriarEnviareSocial.Executar;
var
  AIniFile, AGrupo, ArqeSocial, Resp : String;
  ASalvar : Boolean;
  i, iEvento : Integer;
begin
  AIniFile := fpCmd.Params(0);
  AGrupo := fpCmd.Params(1);

  if not FileExists(AIniFile) then
    raise Exception.Create(ACBrStr( Format(SErroeSocialAbrir, [AIniFile]) ));

  with TACBrObjetoeSocial(fpObjetoDono) do
  begin
    ACBreSocial.Eventos.LoadFromIni(AIniFile);

    ASalvar := ACBreSocial.Configuracoes.Geral.Salvar;
    if not ASalvar then
    begin
      ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + CPathLogs);
      ACBreSocial.Configuracoes.Arquivos.PathSalvar :=
        PathWithDelim(ExtractFilePath(Application.ExeName)) + CPathLogs;
    end;

    iEvento:= ACBreSocial.Eventos.Gerados.Count - 1;
    ArqeSocial:= ACBreSocial.Eventos.Gerados.Items[ iEvento ].PathNome + CExtensaoXML ;

    if not FileExists(ArqeSocial) then
      raise Exception.Create(ACBrStr( Format(SErroeSocialAbrir, [ArqeSocial]) ));

    Resp := ArqeSocial + sLineBreak + ACBrStr( Format(SMsgeSocialEventoAdicionado,
         [TipoEventoToStr(ACBreSocial.Eventos.Gerados.Items[iEvento].TipoEvento)]) ) +
         sLineBreak;

    fpCmd.Resposta := Resp;

    ACBreSocial.Enviar(TeSocialGrupo( StrToIntDef(AGrupo,1) ));
    Sleep(3000);

    with ACBreSocial.WebServices.EnvioLote.RetEnvioLote do
    begin
      if Status.cdResposta in [201, 202] then
        RespostaEnvio
      else
        for i := 0 to Status.Ocorrencias.Count - 1 do
          RespostaEnvioOcorrencia(i);

    end;
    ACBreSocial.Eventos.Clear;

  end;

end;

{ TMetodoEnviareSocial }

{ Params: 0 - Grupo: 1- Iniciais/Tabelas 2- Nao Periodicos 3- Periódicos
          1 - Assina: 1 para assinar XML
}

procedure TMetodoEnviareSocial.Executar;
var
  AGrupo: String;
  AAssina: Boolean;
  i: Integer;
begin
  AGrupo := fpCmd.Params(0);
  AAssina := StrToBoolDef(fpCmd.Params(1), False);

  with TACBrObjetoeSocial(fpObjetoDono) do
  begin
    if (ACBreSocial.Eventos.Count <= 0) then
      raise Exception.Create(ACBrStr( SErroeSocialNenhumEvento ));

    if (AAssina) then
      ACBreSocial.AssinarEventos;

    ACBreSocial.Enviar(TeSocialGrupo( StrToIntDef(AGrupo,1) ));
    Sleep(3000);

    with ACBreSocial.WebServices.EnvioLote.RetEnvioLote do
    begin
      if Status.cdResposta in [201, 202] then
        RespostaEnvio
      else
        for i := 0 to Status.Ocorrencias.Count - 1 do
          RespostaEnvioOcorrencia(i);
    end;

    ACBreSocial.Eventos.Clear;
  end;

end;


{ TMetodoCriarEventoeSocial }

{ Params: 0 - IniFile - Uma String com um Path completo arquivo .ini MDFe
                         ou Uma String com conteúdo txt do MDFe
          1 - RetornaXML: 1 para Retornar XML Gerado na Resposta
}

procedure TMetodoCriarEventoeSocial.Executar;
var
  ARetornaXML, ASalvar: Boolean;
  AIni, Resp, ArqeSocial: String;
  SL: TStringList;
  iEvento: Integer;
begin
  AIni := fpCmd.Params(0);
  ARetornaXML := StrToBoolDef(fpCmd.Params(1), False);

  if not FileExists(AIni) then
      raise Exception.Create(ACBrStr( Format(SErroeSocialAbrir, [AIni]) ));

  with TACBrObjetoeSocial(fpObjetoDono) do
  begin
    ACBreSocial.Eventos.LoadFromIni(AIni);

    ASalvar := ACBreSocial.Configuracoes.Geral.Salvar;
    if not ASalvar then
    begin
      ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + CPathLogs);
      ACBreSocial.Configuracoes.Arquivos.PathSalvar :=
        PathWithDelim(ExtractFilePath(Application.ExeName)) + CPathLogs;
    end;

    iEvento:= ACBreSocial.Eventos.Gerados.Count - 1;
    ArqeSocial:= ACBreSocial.Eventos.Gerados.Items[ iEvento ].PathNome + CExtensaoXML ;

    if not FileExists(ArqeSocial) then
      raise Exception.Create(ACBrStr( Format(SErroeSocialAbrir, [ArqeSocial]) ));

    Resp := ArqeSocial + sLineBreak + ACBrStr( Format(SMsgeSocialEventoAdicionado,
         [TipoEventoToStr(ACBreSocial.Eventos.Gerados.Items[iEvento].TipoEvento)]) );

    if ARetornaXML then
    begin
      SL := TStringList.Create;
      try
        SL.LoadFromFile(ArqeSocial);
        Resp := Resp + sLineBreak + SL.Text;
      finally
        SL.Free;
      end;
    end;
    fpCmd.Resposta := Resp;

  end;

end;

{ TMetodoLimpareSocial }

procedure TMetodoLimpareSocial.Executar;
begin
  with TACBrObjetoeSocial(fpObjetoDono) do
  begin
    ACBreSocial.Eventos.Clear;
    fpCmd.Resposta := SMsgeSocialLimparLista;
  end;

end;

{ TMetodoSetIDEmpregador }

{ Params: 0 - idEmpregador: String
}
procedure TMetodoSetIDEmpregador.Executar;
var
  nIDEmpregador: String;
begin
  nIDEmpregador := fpCmd.Params(0);

  if EstaVazio(nIDEmpregador) then
    raise Exception.Create('Valor Nulo.');

  with TACBrObjetoeSocial(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.ESocial do
      IdEmpregador := nIDEmpregador;

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

  with TACBrObjetoeSocial(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.ESocial do
      IDTransmissor := nIDTransmissor;

    MonitorConfig.SalvarArquivo;
  end;

end;

{ TACBrObjetoeSocial }

constructor TACBrObjetoeSocial.Create(AConfig: TMonitorConfig;
  ACBreSocial: TACBreSocial);
begin
  inherited Create(AConfig);

  fACBreSocial := ACBreSocial;

  ListaDeMetodos.Add(CMetodoCriarEventoeSocial);
  ListaDeMetodos.Add(CMetodoEnviareSocial);
  ListaDeMetodos.Add(CMetodoCriarEnviareSocial);
  ListaDeMetodos.Add(CMetodoConsultareSocial);
  ListaDeMetodos.Add(CMetodoLimpareSocial);
  ListaDeMetodos.Add(CMetodoCarregarXMLEventoeSocial);
  ListaDeMetodos.Add(CMetodoSetIDEmpregadoreSocial);
  ListaDeMetodos.Add(CMetodoSetIDTransmissoresocial);
  ListaDeMetodos.Add(CMetodoConsultaIdentEventosEmpreg);
  ListaDeMetodos.Add(CMetodoConsultaIdentEventosTabela);
  ListaDeMetodos.Add(CMetodoConsultaIdentEventosTrab);
  ListaDeMetodos.Add(CMetodoDownloadEventos);

end;

procedure TACBrObjetoeSocial.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
begin
  inherited Executar(ACmd);

  with fACBreSocial.Configuracoes.Geral do
  begin
    if EstaVazio(IdEmpregador) or EstaVazio(IdTransmissor) then
      raise Exception.Create(ACBrStr( SErroIDEmpregadorTransmissor ));
  end;

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoCriarEventoeSocial;
    1  : AmetodoClass := TMetodoEnviareSocial;
    2  : AMetodoClass := TMetodoCriarEnviareSocial;
    3  : AMetodoClass := TMetodoConsultareSocial;
    4  : AMetodoClass := TMetodoLimpareSocial;
    5  : AMetodoClass := TMetodoCarregarXMLEventoeSocial;
    6  : AMetodoClass := TMetodoSetIDEmpregador;
    7  : AMetodoClass := TMetodoSetIDTransmissor;
    8  : AMetodoClass := TMetodoConsultaIdentificadoresEventosEmpregador;
    9  : AMetodoClass := TMetodoConsultaIdentificadoresEventosTabela;
    10 : AMetodoClass := TMetodoConsultaIdentificadoresEventosTrabalhador;
    11 : AMetodoClass := TMetodoDownloadEventos;
    12..25 : DoACbr(ACmd);

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

procedure TACBrObjetoeSocial.RespostaEnvio;
var
  Resp: TEnvioResposta;
begin
  Resp := TEnvioResposta.Create(resINI);
  try
    with fACBreSocial.WebServices.EnvioLote.RetEnvioLote do
    begin
      Resp.Codigo         := Status.cdResposta;
      Resp.Mensagem       := Status.descResposta;
      Resp.TpInscEmpreg   := eSTpInscricaoToStr(IdeEmpregador.TpInsc);
      Resp.NrInscEmpreg   := IdeEmpregador.NrInsc;
      Resp.TpInscTransm   := eSTpInscricaoToStr(IdeTransmissor.TpInsc);
      Resp.NrInscTransm   := IdeTransmissor.NrInsc;
      Resp.DhRecepcao     := dadosRecLote.dhRecepcao;
      Resp.VersaoAplic    := dadosRecLote.versaoAplicRecepcao;
      Resp.Protocolo      := dadosRecLote.Protocolo;

    end;
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoeSocial.RespostaEnvioConsulta;
var
  Resp: TEnvioResposta;
begin
  Resp := TEnvioResposta.Create(resINI);
  try
    with fACBreSocial.WebServices.ConsultaLote.RetConsultaLote do
    begin
      Resp.Codigo         := Status.cdResposta;
      Resp.Mensagem       := Status.descResposta;
      Resp.TpInscEmpreg   := eSTpInscricaoToStr(IdeEmpregador.TpInsc);
      Resp.NrInscEmpreg   := IdeEmpregador.NrInsc;
      Resp.TpInscTransm   := eSTpInscricaoToStr(IdeTransmissor.TpInsc);
      Resp.NrInscTransm   := IdeTransmissor.NrInsc;
      Resp.DhRecepcao     := dadosRecLote.dhRecepcao;
      Resp.VersaoAplic    := dadosRecLote.versaoAplicRecepcao;
      Resp.Protocolo      := dadosRecLote.Protocolo;

    end;
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;

  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoeSocial.RespostaEnvioOcorrencia(ACont: Integer);
var
  Resp: TOcorrenciaResposta;
begin
  Resp := TOcorrenciaResposta.Create(CSessaoRespOcorrencia+inttostr(ACont), resINI);
  try
    with fACBreSocial.WebServices.EnvioLote.RetEnvioLote do
    begin
      Resp.Codigo         := Status.cdResposta;
      Resp.Mensagem       := Status.descResposta;
      Resp.CodigoOco      := Status.Ocorrencias.Items[ACont].Codigo;
      Resp.Descricao      := Status.Ocorrencias.Items[ACont].Descricao;
      Resp.Tipo           := Status.Ocorrencias.Items[ACont].Tipo;
      Resp.Localizacao    := Status.Ocorrencias.Items[ACont].Localizacao;

    end;
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;

  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoeSocial.RespostaOcorrencia(ACont: Integer);
var
  Resp: TOcorrenciaResposta;
begin
  Resp := TOcorrenciaResposta.Create(CSessaoRespOcorrencia+inttostr(ACont), resINI);
  try
    with fACBreSocial.WebServices.ConsultaLote.RetConsultaLote do
    begin
      Resp.Codigo         := Status.cdResposta;
      Resp.Mensagem       := Status.descResposta;
      Resp.CodigoOco      := Status.Ocorrencias.Items[ACont].Codigo;
      Resp.Descricao      := Status.Ocorrencias.Items[ACont].Descricao;
      Resp.Tipo           := Status.Ocorrencias.Items[ACont].Tipo;
      Resp.Localizacao    := Status.Ocorrencias.Items[ACont].Localizacao;

    end;
    fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoeSocial.RespostaOcorrencia(ACont, ACont2: Integer);
var
  Resp: TOcorrenciaResposta;
begin
  Resp := TOcorrenciaResposta.Create(CSessaoRespOcorrencia+inttostr(ACont2), resINI);
  try
    with fACBreSocial.WebServices.ConsultaLote.RetConsultaLote.retEventos.
         Items[ACont].Processamento.Ocorrencias.Items[ACont2] do
    begin
      Resp.CodigoOco      := Codigo;
      Resp.Descricao      := Descricao;
      Resp.Tipo           := Tipo;
      Resp.Localizacao    := Localizacao;

    end;
    fpCmd.Resposta      := fpCmd.Resposta + Resp.Gerar;

  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoeSocial.RespostaConsulta(ACont: Integer);
var
  Resp : TConsultaResposta;
begin
  Resp := TConsultaResposta.Create(CSessaoRespConsulta+inttostr(ACont),resINI);
  try
    with fACBreSocial.WebServices.ConsultaLote.RetConsultaLote.retEventos.Items[ACont] do
    begin
      resp.cdResposta      := Processamento.cdResposta;
      resp.descResposta    := Processamento.descResposta;
      resp.versaoAplicProcLote:= Processamento.versaoAplicProcLote;
      resp.dhProcessamento := Processamento.dhProcessamento;
      resp.nrRecibo        := Recibo.nrRecibo;
      resp.hash            := Recibo.Hash;

    end;
    fpCmd.Resposta       := Resp.Gerar;

  finally
    Resp.Free;

  end;

end;

procedure TACBrObjetoeSocial.RespostaTot(ACont, ACont2: Integer);
var
  Resp : TConsultaTotResposta;
  evtS5001: TS5001;
  evtS5002: TS5002;
  evtS5011: TS5011;
  evtS5012: TS5012;
begin
  Resp := TConsultaTotResposta.Create(CSessaoRespConsultaTot+inttostr(ACont),resINI);
  try
    with fACBreSocial.WebServices.ConsultaLote.RetConsultaLote.retEventos
         .Items[ACont].tot[ACont2] do
    begin
      resp.Tipo          := Tipo;
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
      fpCmd.Resposta       := fpCmd.Resposta + Resp.Gerar;

    end;
  finally
    Resp.Free;

  end;

end;

procedure TACBrObjetoeSocial.RespostaConsultaIdentEventosQtd;
var
  Resp : TConsultaTotEventos;
begin
  Resp := TConsultaTotEventos.Create(CSessaoRespConsultaIdentEventos,resINI);
  try
    with fACBreSocial.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt do
    begin
      resp.Codigo:= Status.cdResposta;
      resp.Mensagem:= Status.descResposta;
      resp.QtdeTotal:= RetIdentEvts.qtdeTotEvtsConsulta;
      resp.DhUltimoEvento:= RetIdentEvts.dhUltimoEvtRetornado;

    end;
    fpCmd.Resposta       := Resp.Gerar;

  finally
    Resp.Free;

  end;

end;

procedure TACBrObjetoeSocial.RespostaConsultaIdentEventosRecibo(ACont: Integer);
var
  Resp : TConsultaIdentEvento;
begin
  Resp := TConsultaIdentEvento.Create(CSessaoRespConsultaIdentEventosRecibo+inttostr(ACont),resINI);
  try
    with fACBreSocial.WebServices.ConsultaIdentEventos.RetConsultaIdentEvt.RetIdentEvts.Items[ACont] do
    begin
      resp.IdEvento:= Id;
      resp.NRecibo:= nrRec;

    end;
    fpCmd.Resposta       := Resp.Gerar;

  finally
    Resp.Free;

  end;

end;

procedure TACBrObjetoeSocial.RespostaDownload(ACount: Integer);
var
  Resp : TConsultaIdentEvento;
begin

  Resp := TConsultaIdentEvento.Create(CSessaoRespConsulta,resINI);
  try
    with fACBreSocial.WebServices.DownloadEventos.RetDownloadEvt do
    begin
      resp.Codigo:= arquivo.Items[ACount].Status.cdResposta;
      resp.Mensagem:= arquivo.Items[ACount].Status.descResposta;
      resp.IdEvento:= arquivo.Items[ACount].Id;
      resp.NRecibo:= arquivo.Items[ACount].nrRec;
      resp.XML:= arquivo.Items[ACount].XML;

    end;
    fpCmd.Resposta       := Resp.Gerar;

  finally
    Resp.Free;

  end;
end;

procedure TACBrObjetoeSocial.RespostaPadrao;
var
  Resp : TPadraoeSocialResposta;
begin
  Resp := TPadraoeSocialResposta.Create(CSessaoRespConsulta,resINI);
  try
    with fACBreSocial.WebServices.DownloadEventos.RetDownloadEvt do
    begin
      resp.Codigo:= Status.cdResposta;
      resp.Mensagem:= Status.descResposta;

    end;
    fpCmd.Resposta       := Resp.Gerar;

  finally
    Resp.Free;

  end;
end;

end.

