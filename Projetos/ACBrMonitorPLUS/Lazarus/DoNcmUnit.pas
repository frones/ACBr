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

unit DoNcmUnit;

interface

uses
  Classes, TypInfo, SysUtils, CmdUnit,
  ACBrNCMs, ACBrMonitorConsts, ACBrMonitorConfig;

type

{ TACBrObjetoNcm }

TACBrObjetoNcm = class(TACBrObjetoDFe)
private
  fACBrNcm: TACBrNcms;
public
  constructor Create(AConfig: TMonitorConfig; ACBrNcm: TACBrNcms); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  property ACBrNcm: TACBrNcms read fACBrNcm;
end;

{ TMetodoValidar }
TMetodoValidar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoBaixarLista }
TMetodoBaixarLista = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDescricaoNCM }
TMetodoDescricaoNCM = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoBuscarPorCodigo }
TMetodoBuscarPorCodigo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoBuscarPorDescricao }
TMetodoBuscarPorDescricao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoObterNCMs }
TMetodoObterNCMs = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrLibResposta, ACBrLibNCMsRespostas,
  ACBrLibConfig;

{ TACBrObjetoNcm }

constructor TACBrObjetoNcm.Create(AConfig: TMonitorConfig; ACBrNcm: TACBrNcms);
begin
  inherited Create(AConfig);

  fACBrNcm := ACBrNcm;

  ListaDeMetodos.Add(CMetodoValidar);
  ListaDeMetodos.Add(CMetodoBaixarLista);
  ListaDeMetodos.Add(CMetodoDescricaoNCM);
  ListaDeMetodos.Add(CMetodoBuscarPorCodigo);
  ListaDeMetodos.Add(CMetodoBuscarPorDescricao);
  ListaDeMetodos.Add(CMetodoObterNCMs);
end;

procedure TACBrObjetoNcm.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0: AMetodoClass := TMetodoValidar;
    1: AMetodoClass := TMetodoBaixarLista;
    2: AMetodoClass := TMetodoDescricaoNCM;
    3: AMetodoClass := TMetodoBuscarPorCodigo;
    4: AMetodoClass := TMetodoBuscarPorDescricao;
    5: AMetodoClass := TMetodoObterNCMs;
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

{ TMetodoValidar }

// Params: 0 - String com o codigo Ncm
procedure TMetodoValidar.Executar;
var
  aNCM: String;
begin
  aNCM := OnlyNumber(fpCmd.Params(0));

  fpCmd.Resposta := 'NCM Valido';

  if (Length(aNCM) <> 8) then
    raise Exception.Create('O codigo do NCM deve conter 8 Caracteres');

  with TACBrObjetoNcm(fpObjetoDono) do
    if not  ACBrNcm.Validar(aNCM) then
      raise Exception.Create('NCM Invalido');
end;

{ TMetodoBaixarLista }

// Params: 0 - String com o nome do Arquivo a ser salvo em disco
procedure TMetodoBaixarLista.Executar;
var
  aFileName: String;
begin
  aFileName := fpCmd.Params(0);

  with TACBrObjetoNcm(fpObjetoDono) do
  begin
    ACBrNcm.ObterNCMs;
    ACBrNcm.NCMS.SaveToFile(aFileName);
  end;

  fpCmd.Resposta := 'Arquivo salvo em: ' + aFileName;
end;

{ TMetodoDescricaoNCM }

// Params: 0 - String com o NCM
procedure TMetodoDescricaoNCM.Executar;
begin
  with TACBrObjetoNcm(fpObjetoDono) do
    fpCmd.Resposta := ACBrNcm.DescricaoNcm(fpCmd.Params(0));
end;

{ TMetodoBuscarPorCodigo }

// Params: 0 - String com filtro desejado
procedure TMetodoBuscarPorCodigo.Executar;
var
  wFiltro: String;
  NCMsResposta: TNCMsRespostaFactory;
begin
  wFiltro := Trim(fpCmd.Params(0));
  if EstaVazio(wFiltro) then
    raise Exception.Create('Filtro nao informado');

  with TACBrObjetoNcm(fpObjetoDono) do
  begin
    ACBrNcm.BuscarPorCodigo(wFiltro);

    if (ACBrNcm.NCMsFiltrados.Count <= 0) then
      raise Exception.Create('Nenhum NCM encontrado com codigo: ' + wFiltro);

    NCMsResposta := TNCMsRespostaFactory.Create(False, TpResp, codUTF8);
    try
      NCMsResposta.Processar(ACBrNcm);
      fpCmd.Resposta := NCMsResposta.Gerar;
    finally
      NCMsResposta.Free;
    end;
  end;
end;

{ TMetodoBuscarPorDescricao }

// Params:
// 0 - String com filtro desejado
// 1 - Tipo Filtro (0: IniciaCom | 1: Contém | 2: FinalizaCom)
procedure TMetodoBuscarPorDescricao.Executar;
var
  wTipoFiltro: Integer;
  wFiltro, wMsgErro: String;
  NCMsResposta: TNCMsRespostaFactory;
begin
  wMsgErro := EmptyStr;
  wFiltro := Trim(fpCmd.Params(0));
  wTipoFiltro := StrToIntDef(fpCmd.Params(1), 0);

  if EstaVazio(wFiltro) then
    raise Exception.Create('Filtro nao informado');

  with TACBrObjetoNcm(fpObjetoDono) do
  begin
    ACBrNcm.BuscarPorDescricao(wFiltro, TACBrNCMTipoFiltro(wTipoFiltro));

    if (ACBrNcm.NCMsFiltrados.Count <= 0) then
    begin
      case wTipoFiltro of
        1: wMsgErro := 'contendo';
        2: wMsgErro := 'finalizada com';
      else
        wMsgErro := 'iniciando com';
      end;

      raise Exception.Create(Format('Nenhum NCM encontrado com descricao %s a string %s',
        [wMsgErro, QuotedStr(wFiltro)]));
    end;

    NCMsResposta := TNCMsRespostaFactory.Create(False, TpResp, codUTF8);
    try
      NCMsResposta.Processar(ACBrNcm);
      fpCmd.Resposta := NCMsResposta.Gerar;
    finally
      NCMsResposta.Free;
    end;
  end;
end;

{ TMetodoObterNCMs }

procedure TMetodoObterNCMs.Executar;
begin
  with TACBrObjetoNcm(fpObjetoDono) do
    ACBrNcm.ObterNCMs;
  fpCmd.Resposta := 'Lista de NCMs atualizada';
end;

end.
