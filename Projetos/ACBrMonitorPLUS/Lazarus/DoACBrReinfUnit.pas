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

  property ACBrReinf: TACBrReinf read fACBrReinf;
end;

{ TMetodoSetVersaoDF}

TMetodoSetVersaoDF = class(TACBrMetodo)
public
  procedure Executar; override;
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

{ TMetodoSetVersaoDF }

procedure TMetodoSetVersaoDF.Executar;
var
  OK: boolean;
  VersaoDF: TVersaoReinf;
  AVersao: String;
begin
  AVersao := fpCmd.Params(0);
  VersaoDF := StrToVersaoReinf(ok, AVersao);

  if not OK then
    raise Exception.Create(SErroVersaoInvalida);

  with TACBrObjetoReinf(fpObjetoDono) do
  begin
    with MonitorConfig.DFE.WebService do
      VersaoReinf := VersaoReinfToStr(VersaoDF);

    MonitorConfig.SalvarArquivo;
  end;
end;

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
  Ok: Boolean;
  Resp: TRespostas;
begin
  APerApur := fpCmd.Params(0);
  ATipoEvento := StrToIntDef(fpCmd.Params(1),1000);
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
                                      StrToTipoEvento(Ok, 'R-' + IntToStr(ATipoEvento)),
                                      AInscricaoestabelecimento,
                                      ACnpjPrestador,
                                      AInscricaoTomador,
                                      ADataApur) then
    begin
      Resp := TRespostas.Create(ACBrReinf, TpResp, codUTF8);
      try
        Resp.RespostaConsultaRecibo;
        fpCmd.Resposta := Resp.Resposta;
      finally
        Resp.Free;
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
  Resp: TRespostas;
begin
  AProtocolo := fpCmd.Params(0);

  with TACBrObjetoReinf(fpObjetoDono) do
  begin
    ACBrReinf.Eventos.Clear;
    if ACBrReinf.Consultar(AProtocolo) then
    begin
      Resp := TRespostas.Create(ACBrReinf, TpResp, codUTF8);
      try
        Resp.RespostaConsultaRetorno;
        fpCmd.Resposta := Resp.Resposta;
      finally
        Resp.Free;
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
  AIniFile, ArqReinf: String;
  ASalvar : Boolean;
  iEvento : Integer;
  Resp: TRespostas;
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

    fpCmd.Resposta := ArqReinf + sLineBreak + ACBrStr( Format(SMsgReinfEventoAdicionado,
         [TipoEventoToStr(ACBrReinf.Eventos.Gerados.Items[iEvento].TipoEvento)]) ) +
         sLineBreak;

    if ACBrReinf.Enviar then
    begin
      Resp := TRespostas.Create(ACBrReinf, TpResp, codUTF8);
      try
        Resp.RespostaEnvio;
        fpCmd.Resposta := fpCmd.Resposta + sLineBreak + Resp.Resposta;
      finally
        Resp.Free;
      end;
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
  Resp: TRespostas;
begin
  AAssina := StrToBoolDef(fpCmd.Params(0), False);

  with TACBrObjetoReinf(fpObjetoDono) do
  begin
    if (ACBrReinf.Eventos.Count <= 0) then
      raise Exception.Create(ACBrStr( SErroReinfNenhumEvento ));

    if (AAssina) then
      ACBrReinf.AssinarEventos;

    if ACBrReinf.Enviar then
    begin
      Resp := TRespostas.Create(ACBrReinf, TpResp, codUTF8);
      try
        Resp.RespostaEnvio;
        fpCmd.Resposta := Resp.Resposta;
      finally
        Resp.Free;
      end;
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
  ListaDeMetodos.Add(CMetodoSetVersaoDF);
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
    9  : AMetodoClass := TMetodoSetVersaoDF;
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

end.

