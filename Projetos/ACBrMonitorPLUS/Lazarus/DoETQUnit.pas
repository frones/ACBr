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

unit DoETQUnit;

interface

uses
  Classes, TypInfo, SysUtils, CmdUnit, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrETQ, ACBrDevice, ACBrMonitorConsts, ACBrMonitorConfig, ACBrETQClass;

type

{ TACBrObjetoETQ }

TACBrObjetoETQ = class(TACBrObjetoDFe)
private
  fACBrETQ: TACBrETQ;
public
  constructor Create(AConfig: TMonitorConfig; ACBrETQ: TACBrETQ); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  property ACBrETQ: TACBrETQ read fACBrETQ;
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

{ TMetodoSetPorta}
TMetodoSetPorta = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTemperatura}
TMetodoTemperatura = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetTemperatura}
TMetodoSetTemperatura = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoIniciarEtiqueta}
TMetodoIniciarEtiqueta = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoFinalizarEtiqueta}
TMetodoFinalizarEtiqueta = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAvanco}
TMetodoAvanco = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetAvanco}
TMetodoSetAvanco = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoUnidade}
TMetodoUnidade = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetUnidade}
TMetodoSetUnidade = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDPI}
TMetodoDPI = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetDPI}
TMetodoSetDPI = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoOrigem}
TMetodoOrigem = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetOrigem}
TMetodoSetOrigem = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoBackFeed}
TMetodoBackFeed = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetBackFeed}
TMetodoSetBackFeed = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoVelocidade}
TMetodoVelocidade = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetVelocidade}
TMetodoSetVelocidade = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoMargemEsquerda}
TMetodoMargemEsquerda = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetMargemEsquerda}
TMetodoSetMargemEsquerda = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirTexto}
TMetodoImprimirTexto = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirBarras}
TMetodoImprimirBarras = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirLinha}
TMetodoImprimirLinha = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirCaixa}
TMetodoImprimirCaixa = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirImagem}
TMetodoImprimirImagem = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimir}
TMetodoImprimir = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGerarStreamBase64}
TMetodoGerarStreamBase64 = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCarregarImagem}
TMetodoCarregarImagem = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLimparMemoria}
TMetodoLimparMemoria = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetLimparMemoria}
TMetodoSetLimparMemoria = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirQRCode}
TMetodoImprimirQRCode = class(TACBrMetodo)
public
  procedure Executar; override;
end;


implementation

{ TACBrObjetoETQ }

constructor TACBrObjetoETQ.Create(AConfig: TMonitorConfig; ACBrETQ: TACBrETQ);
begin
  inherited Create(AConfig);

  fACBrETQ := ACBrETQ;

  ListaDeMetodos.Add(CMetodoAtivar);
  ListaDeMetodos.Add(CMetodoDesativar);
  ListaDeMetodos.Add(CMetodoAtivo);
  ListaDeMetodos.Add(CMetodoModeloStr);
  ListaDeMetodos.Add(CMetodoModelo);
  ListaDeMetodos.Add(CMetodoPorta);
  ListaDeMetodos.Add(CMetodoSetPorta);
  ListaDeMetodos.Add(CMetodoTemperatura);
  ListaDeMetodos.Add(CMetodoSetTemperatura);
  ListaDeMetodos.Add(CMetodoIniciarEtiqueta);
  ListaDeMetodos.Add(CMetodoFinalizarEtiqueta);
  ListaDeMetodos.Add(CMetodoAvanco);
  ListaDeMetodos.Add(CMetodoSetAvanco);
  ListaDeMetodos.Add(CMetodoUnidade);
  ListaDeMetodos.Add(CMetodoSetUnidade);
  ListaDeMetodos.Add(CMetodoDPI);
  ListaDeMetodos.Add(CMetodoSetDPI);
  ListaDeMetodos.Add(CMetodoOrigem);
  ListaDeMetodos.Add(CMetodoSetOrigem);
  ListaDeMetodos.Add(CMetodoBackFeed);
  ListaDeMetodos.Add(CMetodoSetBackFeed);
  ListaDeMetodos.Add(CMetodoVelocidade);
  ListaDeMetodos.Add(CMetodoSetVelocidade);
  ListaDeMetodos.Add(CMetodoMargemEsquerda);
  ListaDeMetodos.Add(CMetodoSetMargemEsquerda);
  ListaDeMetodos.Add(CMetodoImprimirTexto);
  ListaDeMetodos.Add(CMetodoImprimirBarras);
  ListaDeMetodos.Add(CMetodoImprimirLinha);
  ListaDeMetodos.Add(CMetodoImprimirCaixa);
  ListaDeMetodos.Add(CMetodoImprimirImagem);
  ListaDeMetodos.Add(CMetodoImprimir);
  ListaDeMetodos.Add(CMetodoCarregarImagem);
  ListaDeMetodos.Add(CMetodoLimparMemoria);
  ListaDeMetodos.Add(CMetodoSetLimparMemoria);
  ListaDeMetodos.Add(cMetodoImprimirQRCode);
  ListaDeMetodos.Add(CMetodoGerarStreamBase64);

end;

procedure TACBrObjetoETQ.Executar(ACmd: TACBrCmd);
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
    6  : AMetodoClass := TMetodoSetPorta;
    7  : AMetodoClass := TMetodoTemperatura;
    8  : AMetodoClass := TMetodoSetTemperatura;
    9  : AMetodoClass := TMetodoIniciarEtiqueta;
   10  : AMetodoClass := TMetodoFinalizarEtiqueta;
   11  : AMetodoClass := TMetodoAvanco;
   12  : AMetodoClass := TMetodoSetAvanco;
   13  : AMetodoClass := TMetodoUnidade;
   14  : AMetodoClass := TMetodoSetUnidade;
   15  : AMetodoClass := TMetodoDPI;
   16  : AMetodoClass := TMetodoSetDPI;
   17  : AMetodoClass := TMetodoOrigem;
   18  : AMetodoClass := TMetodoSetOrigem;
   19  : AMetodoClass := TMetodoBackFeed;
   20  : AMetodoClass := TMetodoSetBackFeed;
   21  : AMetodoClass := TMetodoVelocidade;
   22  : AMetodoClass := TMetodoSetVelocidade;
   23  : AMetodoClass := TMetodoMargemEsquerda;
   24  : AMetodoClass := TMetodoSetMargemEsquerda;
   25  : AMetodoClass := TMetodoImprimirTexto;
   26  : AMetodoClass := TMetodoImprimirBarras;
   27  : AMetodoClass := TMetodoImprimirLinha;
   28  : AMetodoClass := TMetodoImprimirCaixa;
   29  : AMetodoClass := TMetodoImprimirImagem;
   30  : AMetodoClass := TMetodoImprimir;
   31  : AMetodoClass := TMetodoCarregarImagem;
   32  : AMetodoClass := TMetodoLimparMemoria;
   33  : AMetodoClass := TMetodoSetLimparMemoria;
   34  : AMetodoClass := TMetodoImprimirQRCode;
   35  : AMetodoClass := TMetodoGerarStreamBase64;
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
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.Ativar;
  end;
end;

{ TMetodoDesativar }

procedure TMetodoDesativar.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.Desativar;
  end;
end;

{ TMetodoAtivo }

procedure TMetodoAtivo.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr(ACBrETQ.Ativo, true);
  end;
end;

{ TMetodoModeloStr }

procedure TMetodoModeloStr.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrETQ.ModeloStr;
  end;
end;

{ TMetodoModelo }

procedure TMetodoModelo.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := GetEnumName(TypeInfo(TACBrETQModelo),Integer(ACBrETQ.Modelo));
  end;
end;

{ TMetodoPorta }

procedure TMetodoPorta.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrETQ.Porta;
  end;
end;

{ TMetodoSetPorta}

{ Params: 0 - String com o nome da porta
}
procedure TMetodoSetPorta.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.Porta := fpCmd.Params(0);
  end;
end;

{ TMetodoTemperatura }

procedure TMetodoTemperatura.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr(ACBrETQ.Temperatura);
  end;
end;

{ TMetodoSetTemperatura}

{ Params: 0 - Inteiro com o valor da temperatura
}
procedure TMetodoSetTemperatura.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.Temperatura := StrToInt(fpCmd.Params(0));
  end;
end;

{ TMetodoIniciarEtiqueta }

procedure TMetodoIniciarEtiqueta.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.IniciarEtiqueta;
  end;
end;

{ TMetodoFinalizarEtiqueta }

{ Params: 0 - Inteiro com a quantidade de copias
          1 - Inteiro com o valor para o avanço de etiqueta
}
procedure TMetodoFinalizarEtiqueta.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.FinalizarEtiqueta(StrToIntDef(fpCmd.Params(0), 1),
                              StrToIntDef(fpCmd.Params(1), 0));
  end;
end;

{ TMetodoAvanco }

procedure TMetodoAvanco.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr(ACBrETQ.Avanco);
  end;
end;

{ TMetodoSetAvanco }

{ Params: 0 - Inteiro com o valor do avanço
}
procedure TMetodoSetAvanco.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.Avanco := StrToInt(fpCmd.Params(0));
  end;
end;

{ TMetodoUnidade }

procedure TMetodoUnidade.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := GetEnumName(TypeInfo(TACBrETQUnidade),Integer(ACBrETQ.Unidade));
  end;
end;

{ TMetodoSetUnidade }

{ Params: 0 - inteiro com o codigo da unidade
              0 -> etqMilimetros
              1 -> etqPolegadas
              2 -> etqDots
              3 -> etqDecimoDeMilimetros
}
procedure TMetodoSetUnidade.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.Unidade := TACBrETQUnidade( StrToInt( fpCmd.Params(0)));
  end;
end;

{ TMetodoDPI }

procedure TMetodoDPI.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := GetEnumName(TypeInfo(TACBrETQDPI),Integer(ACBrETQ.DPI));
  end;
end;

{ TMetodoSetDPI }

{ Params: 0 - Inteiro com o codigo do DPI
              0 -> dpi203
              1 -> dpi300
              2 -> dpi600
}
procedure TMetodoSetDPI.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.DPI := TACBrETQDPI(StrToInt( fpCmd.Params(0)));
  end;
end;

{ TMetodoOrigem }

procedure TMetodoOrigem.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := GetEnumName(TypeInfo(TACBrETQOrigem),Integer(ACBrETQ.Origem));
  end;
end;

{ TMetodoSetOrigem }

{ Params: 0 - Inteiro com o codigo da Origem
              0 -> ogNone
              1 -> ogTop
              2 -> ogBotton
}
procedure TMetodoSetOrigem.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.Origem := TACBrETQOrigem(StrToInt( fpCmd.Params(0)));
  end;
end;

{ TMetodoBackFeed }

procedure TMetodoBackFeed.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := GetEnumName(TypeInfo(TACBrETQBackFeed),Integer(ACBrETQ.BackFeed));
  end;
end;

{ TMetodoSetBackFeed }

{ Params: 0 - Inteiro com o codigo do BackFeed
              0 -> bfNone
              1 -> bfOn
              2 -> bfOff
}
procedure TMetodoSetBackFeed.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.BackFeed := TACBrETQBackFeed(StrToInt( fpCmd.Params(0)));
  end;
end;

{ TMetodoVelocidade }

procedure TMetodoVelocidade.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr( ACBrETQ.Velocidade );
  end;
end;

{ TMetodoSetVelocidade }

{ Params: 0 - Inteiro com a velocidade
}
procedure TMetodoSetVelocidade.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.Velocidade := StrToInt( fpCmd.Params(0) );
  end;
end;

{ TMetodoMargemEsquerda }

procedure TMetodoMargemEsquerda.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr( ACBrETQ.MargemEsquerda );
  end;
end;

{ TMetodoSetMargemEsquerda }

{ Params: 0 - Inteiro com a velocidade
}
procedure TMetodoSetMargemEsquerda.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.MargemEsquerda := StrToInt( fpCmd.Params(0) );
  end;
end;

{ TMetodoImprimirTexto }

{ Params: 0 - Inteiro - Orientação
          1 - Inteiro - Fonte
          2 - Inteiro - Multiplicador H
          3 - Inteiro - Multiplicador V
          4 - Inteiro - Vertical
          5 - Inteiro - Horizontal
          6 - String com o texto
          7 - Inteiro - Sub Fonte
          8 - Inteiro - Imprimir Reverso ( 0 = False, 1 = True )
}
procedure TMetodoImprimirTexto.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.ImprimirTexto(
                  TACBrETQOrientacao(StrToInt(fpCmd.Params(0))),  { Orientacao }
                  StrToInt(Trim(fpCmd.Params(1))),                { Fonte }
                  StrToInt(Trim(fpCmd.Params(2))),                { MultiplicadorH }
                  StrToInt(Trim(fpCmd.Params(3))),                { MultiplicadorV }
                  StrToInt(Trim(fpCmd.Params(4))),                { Vertical }
                  StrToInt(Trim(fpCmd.Params(5))),                { Horizontal }
                  fpCmd.Params(6),                                { Texto }
                  StrToIntDef(fpCmd.Params(7), 0),                { Subfonte }
                  StrToBoolDef(fpCmd.Params(8), False)            { ImprimirReverso }
                  );
  end;
end;

{ TMetodoImprimirBarras }

{ Params: 0 - Inteiro - Orientação
          1 - Inteiro - Tipo Barras
          2 - Inteiro - Largura Barra Larga
          3 - Inteiro - Largura Barra Fina
          4 - Inteiro - Vertical
          5 - Inteiro - Horizontal
          6 - String com o texto
          7 - Inteiro - Altura Codigo de Barras
          8 - Inteiro - Exibe Codigo Barras
}
procedure TMetodoImprimirBarras.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.ImprimirBarras(
                  TACBrETQOrientacao(StrToInt(fpCmd.Params(0))), { Orientacao }
                  TACBrTipoCodBarra(StrToInt(fpCmd.Params(1))),  { TipoBarras }
                  StrToInt(fpCmd.Params(2)),                     { LarguraBarraLarga }
                  StrToInt(fpCmd.Params(3)),                     { LarguraBarraFina }
                  StrToInt(fpCmd.Params(4)),                     { Vertical }
                  StrToInt(fpCmd.Params(5)),                     { Horizontal }
                  fpCmd.Params(6),                               { Texto }
                  StrToInt(fpCmd.Params(7)),                     { AlturaCodBarras }
                  TACBrETQBarraExibeCodigo(StrToInt(fpCmd.Params(8)))) { Exibe Codigo Barras }
  end;
end;

{ TMetodoImprimirLinha }

{ Params: 0 - Inteiro - Vertical
          1 - Inteiro - Horizontal
          2 - Inteiro - Largura
          3 - Inteiro - Altura
}
procedure TMetodoImprimirLinha.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.ImprimirLinha(StrToInt(fpCmd.Params(0)),   {Vertical}
                  StrToInt(fpCmd.Params(1)),   {Horizontal}
                  StrToInt(fpCmd.Params(2)),   {Largura}
                  StrToInt(fpCmd.Params(3)))   {Altura}
  end;
end;

{ TMetodoImprimirCaixa }

{ Params: 0 - Inteiro - Vertical
          1 - Inteiro - Horizontal
          2 - Inteiro - Largura
          3 - Inteiro - Altura
          4 - Inteiro - Espessura Vertical
          5 - Inteiro - Espessura Horizontal
}
procedure TMetodoImprimirCaixa.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.ImprimirCaixa(StrToInt(fpCmd.Params(0)),   {Vertical}
                  StrToInt(fpCmd.Params(1)),   {Horizontal}
                  StrToInt(fpCmd.Params(2)),   {Largura}
                  StrToInt(fpCmd.Params(3)),   {Altura}
                  StrToInt(fpCmd.Params(4)),   {EspessuraVertical}
                  StrToInt(fpCmd.Params(5)))   {EspessuraHorizontal}
  end;
end;

{ TMetodoImprimirImagem }

{ Params: 0 - Inteiro - Multiplicador Imagem
          1 - Inteiro - Vertical
          2 - Inteiro - Horizontal
          3 - String - Nome da Imagem
}
procedure TMetodoImprimirImagem.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.ImprimirImagem(StrToInt(fpCmd.Params(0)),
                   StrToInt(fpCmd.Params(1)),
                   StrToInt(fpCmd.Params(2)),
                   fpCmd.Params(3));
  end;
end;

{ TMetodoImprimir }

{ Params: 0 - Inteiro - Copias
          1 - Inteiro - Avanco de Etiqueta
}
procedure TMetodoImprimir.Executar;
var
  ACopias: Integer;
  AAvanco: Integer;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    if NaoEstaVazio( fpCmd.Params(0) ) then
      ACopias := StrToInt(fpCmd.Params(0))
    else
      ACopias := MonitorConfig.ETQ.Copias;

    if NaoEstaVazio( fpCmd.Params(1) ) then
      AAvanco := StrToInt(fpCmd.Params(1))
    else
      AAvanco := MonitorConfig.ETQ.Avanco;

    ACBrETQ.Imprimir( ACopias, AAvanco );
  end;
end;

{ TMetodoGerarStreamBase64}

{ Params: 0 - Inteiro - Copias
          1 - Inteiro - Avanco de Etiqueta
}
procedure TMetodoGerarStreamBase64.Executar;
var
  ACopias: Integer;
  AAvanco: Integer;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    if NaoEstaVazio( fpCmd.Params(0) ) then
      ACopias := StrToInt(fpCmd.Params(0))
    else
      ACopias := MonitorConfig.ETQ.Copias;

    if NaoEstaVazio( fpCmd.Params(1) ) then
      AAvanco := StrToInt(fpCmd.Params(1))
    else
      AAvanco := MonitorConfig.ETQ.Avanco;

    fpCmd.Resposta := sLineBreak + ACBrETQ.GerarStreamBase64( ACopias, AAvanco );
  end;
end;

{ TMetodoCarregarImagem }

{ Params: 0 - String - Caminho do Arquivo da imagem
          1 - String - Nome do Arquivo da imagem
          2 - Inteiro - Flipped ( 0 = False, 1 = True )
}
procedure TMetodoCarregarImagem.Executar;
var
  APath: String;
  ANomeImg: String;
  AFlipped: Boolean;
begin
  APath:= fpCmd.Params(0);
  ANomeImg:= fpCmd.Params(1);
  AFlipped:= StrToBoolDef(Trim(fpCmd.Params(2)), true);

  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.CarregarImagem(APath,
                   ANomeImg,
                   AFlipped);
  end;
end;

{ TMetodoLimparMemoria }

procedure TMetodoLimparMemoria.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr( ACBrETQ.LimparMemoria, true )
  end;
end;

{ TMetodoSetLimparMemoria }

{ Params: 0 - Inteiro ( 0 = False, 1 = True )
}
procedure TMetodoSetLimparMemoria.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.LimparMemoria := StrToBool( Trim(fpCmd.Params(0)))
  end;
end;

{ TMetodoImprimirQRCode }

{ Params: 0 - Inteiro - Vertical
          1 - Inteiro - Horizontal
          2 - String  - Texto
          3 - Inteiro - LarguraModulo
          4 - Inteiro - ErrorLevel
          5 - Inteiro - Tipo

}
procedure TMetodoImprimirQRCode.Executar;
begin
  with TACBrObjetoETQ(fpObjetoDono) do
  begin
    ACBrETQ.ImprimirQRCode(StrToInt(fpCmd.Params(0)),
                   StrToInt(fpCmd.Params(1)),
                   fpCmd.Params(2),
                   StrToInt(fpCmd.Params(3)),
                   StrToInt(fpCmd.Params(4)),
                   StrToInt(fpCmd.Params(5)));
  end;
end;

end.
