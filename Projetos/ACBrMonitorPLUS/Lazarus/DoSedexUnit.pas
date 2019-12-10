{*******************************************************************************}
{ Projeto: ACBrMonitor                                                         }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2010 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo:                                  }
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

unit DoSedexUnit;

interface

uses
  Classes, TypInfo, SysUtils, CmdUnit, ACBrUtil, ACBrSedex,
  ACBrMonitorConsts, ACBrMonitorConfig, ACBrLibResposta, ACBrLibSedexRespostas,
  ACBrLibSedexConsts;

type

{ TACBrObjetoSedex }

TACBrObjetoSedex = class(TACBrObjetoDFe)
private
  fACBrSedex: TACBrSedex;
public
  constructor Create(AConfig: TMonitorConfig; ACBrSedex: TACBrSedex); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  procedure LerIniSedex(aStr: String);

  procedure RespostaConsulta;
  procedure RespostaItensRastreio(ItemID: integer = 0);

  function ProcessarRespostaSedex : String;
  function ProcessarRespostaRastreio : String;

  property ACBrSedex: TACBrSedex read fACBrSedex;
end;

{ TMetodoConsultar}
TMetodoConsultar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoRastrear}
TMetodoRastrear = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

{ TACBrObjetoSedex }

constructor TACBrObjetoSedex.Create(AConfig: TMonitorConfig; ACBrSedex: TACBrSedex);
begin
  inherited Create(AConfig);

  fACBrSedex := ACBrSedex;

  ListaDeMetodos.Add(CMetodoConsultar);
  ListaDeMetodos.Add(CMetodoRastrear);
end;

procedure TACBrObjetoSedex.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoConsultar;
    1  : AMetodoClass := TMetodoRastrear;
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

procedure TACBrObjetoSedex.LerIniSedex(aStr: String);
begin
  if not ( ACBrSedex.LerArqIni( aStr ) ) then
      raise exception.Create('Erro ao ler arquivo de entrada ou '+
         'parâmetro incorreto.');

end;

procedure TACBrObjetoSedex.RespostaConsulta;
var
  Resp: TLibSedexConsulta;
begin
  Resp := TLibSedexConsulta.Create(TpResp, codUTF8);
  try
    with fACBrSedex do
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

      fpCmd.Resposta := retMsgErro + sLineBreak;
      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoSedex.RespostaItensRastreio(ItemID: integer);
var
  Resp: TLibSedexRastreio;
begin
  Resp := TLibSedexRastreio.Create(
          CSessaoRespRastreio + Trim(IntToStrZero(ItemID +1, 2)), TpResp, codUTF8);
  try
    with fACBrSedex.retRastreio[ItemID] do
    begin
      Resp.DataHora := DataHora;
      Resp.Local := Local;
      Resp.Situacao := Situacao;
      Resp.Observacao := Observacao;

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

function TACBrObjetoSedex.ProcessarRespostaSedex: String;
begin
  RespostaConsulta;
  Result := fpCmd.Resposta;
end;

function TACBrObjetoSedex.ProcessarRespostaRastreio: String;
var
  I: integer;
begin
  for I := 0 to ACBrSedex.retRastreio.Count - 1 do
    RespostaItensRastreio(I);

  Result := fpCmd.Resposta;
end;

{ TMetodoConsultar }

{ Params: 0 - String com o Path e nome do arquivo ini
}
procedure TMetodoConsultar.Executar;
var
  AIni: String;
begin
  AIni := fpCmd.Params(0);

  with TACBrObjetoSedex(fpObjetoDono) do
  begin
    if AIni <> '' then
      LerIniSedex(AIni);

    ACBrSedex.Consultar;
    RespostaConsulta;
  end;
end;

{ TMetodoRastrear }

{ Params: 0 - String com o código de rastreio
}
procedure TMetodoRastrear.Executar;
var
  I: integer;
begin
  with TACBrObjetoSedex(fpObjetoDono) do
  begin
    ACBrSedex.Rastrear( fpCmd.Params(0) );

    for I := 0 to ACBrSedex.retRastreio.Count - 1 do
      RespostaItensRastreio(I);
  end;
end;

end.
