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

unit DoCHQUnit;

interface

uses
  Classes, TypInfo, SysUtils, CmdUnit, ACBrUtil, ACBrCHQ,
  ACBrMonitorConsts, ACBrMonitorConfig;

type

{ TACBrObjetoCHQ }

TACBrObjetoCHQ = class(TACBrObjetoDFe)
private
  fACBrCHQ: TACBrCHQ;
public
  constructor Create(AConfig: TMonitorConfig; ACBrCHQ: TACBrCHQ); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  property ACBrCHQ: TACBrCHQ read fACBrCHQ;
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

{ TMetodoModeloStr}
TMetodoModeloStr = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoModelo}
TMetodoModelo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPorta}
TMetodoPorta = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoChequePronto}
TMetodoChequePronto = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoBanco}
TMetodoBanco = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetBanco}
TMetodoSetBanco = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCidade}
TMetodoCidade = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetCidade}
TMetodoSetCidade = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoFavorecido}
TMetodoFavorecido = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetFavorecido}
TMetodoSetFavorecido = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoObservacao}
TMetodoObservacao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetObservacao}
TMetodoSetObservacao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoValor}
TMetodoValor = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetValor}
TMetodoSetValor = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoData}
TMetodoData = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetData}
TMetodoSetData = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoBomPara}
TMetodoBomPara = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetBomPara}
TMetodoSetBomPara = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirCheque}
TMetodoImprimirCheque = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTravarCheque}
TMetodoTravarCheque = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDestravarCheque}
TMetodoDestravarCheque = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCMC7}
TMetodoCMC7 = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirLinha}
TMetodoImprimirLinha = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirVerso}
TMetodoImprimirVerso = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

{ TACBrObjetoCHQ }

constructor TACBrObjetoCHQ.Create(AConfig: TMonitorConfig; ACBrCHQ: TACBrCHQ);
begin
  inherited Create(AConfig);

  fACBrCHQ := ACBrCHQ;

  ListaDeMetodos.Add(CMetodoAtivar);
  ListaDeMetodos.Add(CMetodoDesativar);
  ListaDeMetodos.Add(CMetodoAtivo);
  ListaDeMetodos.Add(CMetodoModeloStr);
  ListaDeMetodos.Add(CMetodoModelo);
  ListaDeMetodos.Add(CMetodoPorta);
  ListaDeMetodos.Add(CMetodoChequePronto);
  ListaDeMetodos.Add(CMetodoBanco);
  ListaDeMetodos.Add(CMetodoSetBanco);
  ListaDeMetodos.Add(CMetodoCidade);
  ListaDeMetodos.Add(CMetodoSetCidade);
  ListaDeMetodos.Add(CMetodoFavorecido);
  ListaDeMetodos.Add(CMetodoSetFavorecido);
  ListaDeMetodos.Add(CMetodoObservacao);
  ListaDeMetodos.Add(CMetodoSetObservacao);
  ListaDeMetodos.Add(CMetodoValor);
  ListaDeMetodos.Add(CMetodoSetValor);
  ListaDeMetodos.Add(CMetodoData);
  ListaDeMetodos.Add(CMetodoSetData);
  ListaDeMetodos.Add(CMetodoBomPara);
  ListaDeMetodos.Add(CMetodoSetBomPara);
  ListaDeMetodos.Add(CMetodoImprimirCheque);
  ListaDeMetodos.Add(CMetodoTravarCheque);
  ListaDeMetodos.Add(CMetodoDestravarCheque);
  ListaDeMetodos.Add(CMetodoCMC7);
  ListaDeMetodos.Add(CMetodoImprimirLinha);
  ListaDeMetodos.Add(CMetodoImprimirVerso);
end;

procedure TACBrObjetoCHQ.Executar(ACmd: TACBrCmd);
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
    3  : AMetodoClass := TMetodoModeloStr;
    4  : AMetodoClass := TMetodoModelo;
    5  : AMetodoClass := TMetodoPorta;
    6  : AMetodoClass := TMetodoChequePronto;
    7  : AMetodoClass := TMetodoBanco;
    8  : AMetodoClass := TMetodoSetBanco;
    9  : AMetodoClass := TMetodoCidade;
   10  : AMetodoClass := TMetodoSetCidade;
   11  : AMetodoClass := TMetodoFavorecido;
   12  : AMetodoClass := TMetodoSetFavorecido;
   13  : AMetodoClass := TMetodoObservacao;
   14  : AMetodoClass := TMetodoSetObservacao;
   15  : AMetodoClass := TMetodoValor;
   16  : AMetodoClass := TMetodoSetValor;
   17  : AMetodoClass := TMetodoData;
   18  : AMetodoClass := TMetodoSetData;
   19  : AMetodoClass := TMetodoBomPara;
   20  : AMetodoClass := TMetodoSetBomPara;
   21  : AMetodoClass := TMetodoImprimirCheque;
   22  : AMetodoClass := TMetodoTravarCheque;
   23  : AMetodoClass := TMetodoDestravarCheque;
   24  : AMetodoClass := TMetodoCMC7;
   25  : AMetodoClass := TMetodoImprimirLinha;
   26  : AMetodoClass := TMetodoImprimirVerso;
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
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    ACBrCHQ.Ativar;
  end;
end;

{ TMetodoDesativar }

procedure TMetodoDesativar.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    ACBrCHQ.Desativar;
  end;
end;

{ TMetodoAtivo }

procedure TMetodoAtivo.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr(ACBrCHQ.Ativo, true);
  end;
end;

{ TMetodoModeloStr }

procedure TMetodoModeloStr.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrCHQ.ModeloStr;
  end;
end;

{ TMetodoModelo }

procedure TMetodoModelo.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := GetEnumName(TypeInfo(TACBrCHQModelo),Integer(ACBrCHQ.Modelo));
  end;
end;

{ TMetodoPorta }

procedure TMetodoPorta.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrCHQ.Porta;
  end;
end;

{ TMetodoChequePronto }

procedure TMetodoChequePronto.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr(ACBrCHQ.ChequePronto, true);
  end;
end;

{ TMetodoBanco }

procedure TMetodoBanco.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrCHQ.Banco;
  end;
end;

{ TMetodoSetBanco }

{ Params: 0 Inteiro - Código do banco
}
procedure TMetodoSetBanco.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    ACBrCHQ.Banco := fpCmd.Params(0)
  end;
end;

{ TMetodoCidade }

procedure TMetodoCidade.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrCHQ.Cidade;
  end;
end;

{ TMetodoSetCidade }

{ Params: 0 - String com o nome da cidade
}
procedure TMetodoSetCidade.Executar;
var
  ACidade: String;
begin
  ACidade := fpCmd.Params(0);

  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    ACBrCHQ.Cidade := ACidade;

    with MonitorConfig.CHQ do
      Cidade := ACidade;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoFavorecido }

procedure TMetodoFavorecido.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrCHQ.Favorecido;
  end;
end;

{ TMetodoSetFavorecido }

{ Params: 0 - String com o nome do favorecido
}
procedure TMetodoSetFavorecido.Executar;
var
  AFavorecido: String;
begin
  AFavorecido := fpCmd.Params(0);

  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    ACBrCHQ.Favorecido := AFavorecido;

    with MonitorConfig.CHQ do
      Favorecido := AFavorecido;

    MonitorConfig.SalvarArquivo;
  end;
end;

{ TMetodoObservacao }

procedure TMetodoObservacao.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrCHQ.Observacao;
  end;
end;

{ TMetodoSetObservacao }

{ Params: 0 - String com a observação
}
procedure TMetodoSetObservacao.Executar;
var
  AObservacao: String;
begin
  AObservacao := fpCmd.Params(0);

  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    ACBrCHQ.Observacao := AObservacao;
  end;
end;

{ TMetodoValor }

procedure TMetodoValor.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrCHQ.Valor);
  end;
end;

{ TMetodoSetValor }

{ Params: 0 - String com o valor
}
procedure TMetodoSetValor.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    ACBrCHQ.Valor := StringToFloat( fpCmd.Params(0) );
  end;
end;

{ TMetodoData }

procedure TMetodoData.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := FormatDateTime('dd/mm/yy', ACBrCHQ.Data );
  end;
end;

{ TMetodoSetData }

{ Params: 0 - String com a data no formato dd/mm/aaaa
}
procedure TMetodoSetData.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    ACBrCHQ.Data := StringToDateTime( fpCmd.Params(0) );
  end;
end;

{ TMetodoBomPara }

procedure TMetodoBomPara.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := FormatDateTime('dd/mm/yy', ACBrCHQ.BomPara );
  end;
end;

{ TMetodoSetBomPara }

{ Params: 0 - String com a data no formato dd/mm/aaaa
}
procedure TMetodoSetBomPara.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    ACBrCHQ.BomPara := StringToDateTime( fpCmd.Params(0) );
  end;
end;

{ TMetodoImprimirCheque }

procedure TMetodoImprimirCheque.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
//    {$IFNDEF NOGUI}
//      if FrmACBrMonitor.chCHQVerForm.Checked and (not ACBrCHQ.ChequePronto) then
//    {$ELSE}
//      if dm.VerificaCheque then
//    {$ENDIF}
    if (not ACBrCHQ.ChequePronto) then
       raise Exception.Create('Formulário de Cheque não posicionado');

    ACBrCHQ.ImprimirCheque;
  end;
end;

{ TMetodoTravarCheque }

procedure TMetodoTravarCheque.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    ACBrCHQ.TravarCheque;
  end;
end;

{ TMetodoDestravarCheque }

procedure TMetodoDestravarCheque.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    ACBrCHQ.DestravarCheque;
  end;
end;

{ TMetodoCMC7 }

procedure TMetodoCMC7.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrCHQ.CMC7;
  end;
end;

{ TMetodoImprimirLinha }

{ Params: 0 - String com o conteudo da linha a ser impressa
}
procedure TMetodoImprimirLinha.Executar;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    ACBrCHQ.ImprimirLinha( fpCmd.Params(0) );
  end;
end;

{ TMetodoImprimirVerso }

{ Params: 0 - String com o conteudo a ser impresso no verso
}
procedure TMetodoImprimirVerso.Executar;
var
  Linhas: TStringList;
begin
  with TACBrObjetoCHQ(fpObjetoDono) do
  begin
    Linhas := TStringList.Create;
    try
      //StringToMemo( fpCmd.Params(0), Linhas ); {Linha separadas por | (pipe)}
      ACBrCHQ.ImprimirVerso( Linhas );
    finally
      Linhas.Free;
    end;
  end;
end;

end.
