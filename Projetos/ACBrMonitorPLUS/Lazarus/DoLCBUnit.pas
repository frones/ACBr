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

unit DoLCBUnit;

interface

uses
  Classes, TypInfo, SysUtils, CmdUnit, ACBrUtil, ACBrLCB,
  ACBrMonitorConsts, ACBrMonitorConfig;

type

{ TACBrObjetoLCB }

TACBrObjetoLCB = class(TACBrObjetoDFe)
private
  fACBrLCB: TACBrLCB;
public
  constructor Create(AConfig: TMonitorConfig; ACBrLCB: TACBrLCB); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  property ACBrLCB: TACBrLCB read fACBrLCB;
end;

{ TMetodoAtivar}
TMetodoAtivar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDesativar}
TMetodoDesativar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAtivo}
TMetodoAtivo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPorta}
TMetodoPorta = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoIntervalo}
TMetodoIntervalo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetIntervalo}
TMetodoSetIntervalo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLerFila}
TMetodoLerFila = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoApagarFila}
TMetodoApagarFila = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoFilaCount}
TMetodoFilaCount = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPrefixoaExcluir}
TMetodoPrefixoaExcluir = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetPrefixoaExcluir}
TMetodoSetPrefixoaExcluir = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSufixo}
TMetodoSufixo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetSufixo}
TMetodoSetSufixo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoExcluirSufixo}
TMetodoExcluirSufixo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetExcluirSufixo}
TMetodoSetExcluirSufixo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoUsarFila}
TMetodoUsarFila = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetUsarFila}
TMetodoSetUsarFila = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoFilaMaxItens}
TMetodoFilaMaxItens = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetFilaMaxItens}
TMetodoSetFilaMaxItens = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoUltimaLeitura}
TMetodoUltimaLeitura = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoUltimoCodigo}
TMetodoUltimoCodigo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarString}
TMetodoEnviarString = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLerString}
TMetodoLerString = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

{ TACBrObjetoLCB }

constructor TACBrObjetoLCB.Create(AConfig: TMonitorConfig; ACBrLCB: TACBrLCB);
begin
  inherited Create(AConfig);

  fACBrLCB := ACBrLCB;

  ListaDeMetodos.Add(CMetodoAtivar);
  ListaDeMetodos.Add(CMetodoDesativar);
  ListaDeMetodos.Add(CMetodoAtivo);
  ListaDeMetodos.Add(CMetodoPorta);
  ListaDeMetodos.Add(CMetodoIntervalo);
  ListaDeMetodos.Add(CMetodoSetIntervalo);
  ListaDeMetodos.Add(CMetodoLerFila);
  ListaDeMetodos.Add(CMetodoApagarFila);
  ListaDeMetodos.Add(CMetodoFilaCount);
  ListaDeMetodos.Add(CMetodoPrefixoaExcluir);
  ListaDeMetodos.Add(CMetodoSetPrefixoaExcluir);
  ListaDeMetodos.Add(CMetodoSufixo);
  ListaDeMetodos.Add(CMetodoSetSufixo);
  ListaDeMetodos.Add(CMetodoExcluirSufixo);
  ListaDeMetodos.Add(CMetodoSetExcluirSufixo);
  ListaDeMetodos.Add(CMetodoUsarFila);
  ListaDeMetodos.Add(CMetodoSetUsarFila);
  ListaDeMetodos.Add(CMetodoFilaMaxItens);
  ListaDeMetodos.Add(CMetodoSetFilaMaxItens);
  ListaDeMetodos.Add(CMetodoUltimaLeitura);
  ListaDeMetodos.Add(CMetodoUltimoCodigo);
  ListaDeMetodos.Add(CMetodoEnviarString);
  ListaDeMetodos.Add(CMetodoLerString);
end;

procedure TACBrObjetoLCB.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoAtivar;
    1  : AMetodoClass := TMetodoDesativar;
    2  : AMetodoClass := TMetodoAtivo;
    3  : AMetodoClass := TMetodoPorta;
    4  : AMetodoClass := TMetodoIntervalo;
    5  : AMetodoClass := TMetodoSetIntervalo;
    6  : AMetodoClass := TMetodoLerFila;
    7  : AMetodoClass := TMetodoApagarFila;
    8  : AMetodoClass := TMetodoFilaCount;
    9  : AMetodoClass := TMetodoPrefixoaExcluir;
   10  : AMetodoClass := TMetodoSetPrefixoaExcluir;
   11  : AMetodoClass := TMetodoSufixo;
   12  : AMetodoClass := TMetodoSetSufixo;
   13  : AMetodoClass := TMetodoExcluirSufixo;
   14  : AMetodoClass := TMetodoSetExcluirSufixo;
   15  : AMetodoClass := TMetodoUsarFila;
   16  : AMetodoClass := TMetodoSetUsarFila;
   17  : AMetodoClass := TMetodoFilaMaxItens;
   18  : AMetodoClass := TMetodoSetFilaMaxItens;
   19  : AMetodoClass := TMetodoUltimaLeitura;
   20  : AMetodoClass := TMetodoUltimoCodigo;
   21  : AMetodoClass := TMetodoEnviarString;
   22  : AMetodoClass := TMetodoLerString;
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

{ TMetodoAtivar }

procedure TMetodoAtivar.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    ACBrLCB.Ativar;
  end;
end;

{ TMetodoDesativar }

procedure TMetodoDesativar.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    ACBrLCB.Desativar;
  end;
end;

{ TMetodoAtivo }

procedure TMetodoAtivo.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr(ACBrLCB.Ativo, true);
  end;
end;

{ TMetodoPorta }

procedure TMetodoPorta.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrLCB.Porta;
  end;
end;

{ TMetodoIntervalo }

procedure TMetodoIntervalo.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr( ACBrLCB.Intervalo );
  end;
end;

{ TMetodoSetIntervalo }

{ Params: 0 - Integer com o valor do Intervalo
}
procedure TMetodoSetIntervalo.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    ACBrLCB.Intervalo := StrToInt(fpCmd.Params(0));
  end;
end;

{ TMetodoLerFila }

procedure TMetodoLerFila.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrLCB.LerFila;
  end;
end;

{ TMetodoApagarFila }

procedure TMetodoApagarFila.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    ACBrLCB.ApagarFila;
  end;
end;

{ TMetodoFilaCount }

procedure TMetodoFilaCount.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr( ACBrLCB.FilaCount );
  end;
end;

{ TMetodoPrefixoaExcluir }

procedure TMetodoPrefixoaExcluir.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrLCB.PrefixoAExcluir;
  end;
end;

{ TMetodoSetPrefixoaExcluir }

{ Params: 0 - String com o Prefixo a Excluir
}
procedure TMetodoSetPrefixoaExcluir.Executar;
var
  APrefixo: String;
begin
  APrefixo := fpCmd.Params(0);

  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    ACBrLCB.PrefixoAExcluir := APrefixo;

    with MonitorConfig.LCB do
      PrefixoAExcluir := APrefixo;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoSufixo }

procedure TMetodoSufixo.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrLCB.Sufixo;
  end;
end;

{ TMetodoSetSufixo }

{ Params: 0 - String com o Sufixo
}
procedure TMetodoSetSufixo.Executar;
var
  ASufixo: String;
begin
  ASufixo := fpCmd.Params(0);

  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    ACBrLCB.Sufixo := ASufixo;

    with MonitorConfig.LCB do
      SufixoLeitor := ASufixo;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoExcluirSufixo }

procedure TMetodoExcluirSufixo.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr( ACBrLCB.ExcluirSufixo, True);
  end;
end;

{ TMetodoSetExcluirSufixo }

{ Params: 0 - Integer (0 = Não Excluir, 1 = Excluir)
}
procedure TMetodoSetExcluirSufixo.Executar;
var
  ASufixo: Boolean;
begin
  ASufixo := StrToBool(fpCmd.Params(0));

  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    ACBrLCB.ExcluirSufixo := ASufixo;

    with MonitorConfig.LCB do
      ExcluirSufixo := ASufixo;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoUsarFila }

procedure TMetodoUsarFila.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr( ACBrLCB.UsarFila, True);
  end;
end;

{ TMetodoSetUsarFila }

{ Params: 0 - Integer ( 0 = Não Usar, 1 = Usar)
}
procedure TMetodoSetUsarFila.Executar;
var
  AUsarFila: Boolean;
begin
  AUsarFila := StrToBool(fpCmd.Params(0));

  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    ACBrLCB.UsarFila := AUsarFila;

//    with MonitorConfig.LCB do
//      ExcluirSufixo := AUsarFila;

//    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoFilaMaxItens }

procedure TMetodoFilaMaxItens.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr( ACBrLCB.FilaMaxItens);
  end;
end;

{ TMetodoSetFilaMaxItens }

{ Params: 0 - Integer ( 0 = Não Usar, 1 = Usar)
}
procedure TMetodoSetFilaMaxItens.Executar;
var
  AFilaMaxItens: Integer;
begin
  AFilaMaxItens := StrToInt(fpCmd.Params(0));

  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    ACBrLCB.FilaMaxItens := AFilaMaxItens;
  end;
end;

{ TMetodoUltimaLeitura }

procedure TMetodoUltimaLeitura.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrLCB.UltimaLeitura;
  end;
end;

{ TMetodoUltimoCodigo }

procedure TMetodoUltimoCodigo.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrLCB.UltimoCodigo;
  end;
end;

{ TMetodoEnviarString }

{ Params: 0 - String
}
procedure TMetodoEnviarString.Executar;
var
  AString: String;
begin
  AString := fpCmd.Params(0);

  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    ACBrLCB.EnviarString(AString);
  end;
end;

{ TMetodoLerString }

procedure TMetodoLerString.Executar;
begin
  with TACBrObjetoLCB(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrLCB.LerString;
  end;
end;

end.
