{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
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

unit DoCEPUnit;

interface

uses
  Classes, TypInfo, SysUtils, CmdUnit, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrCEP, ACBrMonitorConsts, ACBrMonitorConfig, ACBrLibResposta, ACBrLibCEPRespostas;

type

{ TACBrObjetoCEP }

TACBrObjetoCEP = class(TACBrObjetoDFe)
private
  fACBrCEP: TACBrCEP;
public
  constructor Create(AConfig: TMonitorConfig; ACBrCEP: TACBrCEP); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  procedure ProcessarResposta;

  property ACBrCEP: TACBrCEP read fACBrCEP;
end;

{ TMetodoBuscarPorCEP}
TMetodoBuscarPorCEP = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoBuscarPorLogradouro}
TMetodoBuscarPorLogradouro = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

{ TACBrObjetoCEP }

constructor TACBrObjetoCEP.Create(AConfig: TMonitorConfig; ACBrCEP: TACBrCEP);
begin
  inherited Create(AConfig);

  fACBrCEP := ACBrCEP;

  ListaDeMetodos.Add(CMetodoBuscarPorCEP);
  ListaDeMetodos.Add(CMetodoBuscarPorLogradouro);
end;

procedure TACBrObjetoCEP.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoBuscarPorCEP;
    1  : AMetodoClass := TMetodoBuscarPorLogradouro;
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

procedure TACBrObjetoCEP.ProcessarResposta;
var
  Resp: TCepResposta;
begin
   Resp := TCepResposta.Create(TpResp, codUTF8);
  try
    Resp.Processar(ACBrCEP);
    fpCmd.Resposta := sLineBreak + Resp.Gerar;
  finally
    Resp.Free;
  end;
end;

{ TMetodoBuscarPorCEP }

{ Params: 0 - CEP
}
procedure TMetodoBuscarPorCEP.Executar;
begin
  with TACBrObjetoCEP(fpObjetoDono) do
  begin
    ACBrCEP.BuscarPorCEP( fpCmd.Params(0) );
    ProcessarResposta;
  end;
end;

{ TMetodoBuscarPorLogradouro }

{ Params: 0 - Cidade
          1 - Tipo Logradouro Ex: "Rua"
          2 - Logradouro
          3 - UF
          4 - Bairo
}
procedure TMetodoBuscarPorLogradouro.Executar;
begin
  with TACBrObjetoCEP(fpObjetoDono) do
  begin
    ACBrCEP.BuscarPorLogradouro( fpCmd.Params(0), fpCmd.Params(1),
                                 fpCmd.Params(2), fpCmd.Params(3),
                                 fpCmd.Params(4) );
    ProcessarResposta;
  end;
end;

end.
