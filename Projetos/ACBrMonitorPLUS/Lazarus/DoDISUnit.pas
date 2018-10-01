{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }

{ Direitos Autorais Reservados (c) 2009 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na página do Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Este programa é software livre; você pode redistribuí-lo e/ou modificá-lo   }
{ sob os termos da Licença Pública Geral GNU, conforme publicada pela Free     }
{ Software Foundation; tanto a versão 2 da Licença como (a seu critério)       }
{ qualquer versão mais nova.                                                   }

{  Este programa é distribuído na expectativa de ser útil, mas SEM NENHUMA     }
{ GARANTIA; nem mesmo a garantia implícita de COMERCIALIZAÇÃO OU DE ADEQUAÇÃO A}
{ QUALQUER PROPÓSITO EM PARTICULAR. Consulte a Licença Pública Geral GNU para  }
{ obter mais detalhes. (Arquivo LICENCA.TXT ou LICENSE.TXT)                    }

{  Você deve ter recebido uma cópia da Licença Pública Geral GNU junto com este}
{ programa; se não, escreva para a Free Software Foundation, Inc., 59 Temple   }
{ Place, Suite 330, Boston, MA 02111-1307, USA. Você também pode obter uma     }
{ copia da licença em:  http://www.opensource.org/licenses/gpl-license.php     }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}
{$I ACBr.inc}

unit DoDISUnit;

interface

uses
  Classes, TypInfo, SysUtils, CmdUnit, ACBrUtil, ACBrDIS,
  ACBrMonitorConsts, ACBrMonitorConfig;

type

{ TACBrObjetoDIS }

TACBrObjetoDIS = class(TACBrObjetoDFe)
private
  fACBrDIS: TACBrDIS;
public
  constructor Create(AConfig: TMonitorConfig; ACBrDIS: TACBrDIS); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  property ACBrDIS: TACBrDIS read fACBrDIS;
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

{ TMetodoTrabalhando}
TMetodoTrabalhando = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLinhasCount}
TMetodoLinhasCount = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetLinhasCount}
TMetodoSetLinhasCount = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoColunas}
TMetodoColunas = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetColunas}
TMetodoSetColunas = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAlinhamento}
TMetodoAlinhamento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetAlinhamento}
TMetodoSetAlinhamento = class(TACBrMetodo)
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

{ TMetodoPassos}
TMetodoPassos = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetPassos}
TMetodoSetPassos = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLimparDisplay}
TMetodoLimparDisplay = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEscrever}
TMetodoEscrever = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPosicionarCursor}
TMetodoPosicionarCursor = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoParar}
TMetodoParar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoContinuar}
TMetodoContinuar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPararLinha}
TMetodoPararLinha = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoContinuarLinha}
TMetodoContinuarLinha = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoExibirLinha}
TMetodoExibirLinha = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoRolarLinha}
TMetodoRolarLinha = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

{ TACBrObjetoDIS }

constructor TACBrObjetoDIS.Create(AConfig: TMonitorConfig; ACBrDIS: TACBrDIS);
begin
  inherited Create(AConfig);

  fACBrDIS := ACBrDIS;

  ListaDeMetodos.Add(CMetodoAtivar);
  ListaDeMetodos.Add(CMetodoDesativar);
  ListaDeMetodos.Add(CMetodoAtivo);
  ListaDeMetodos.Add(CMetodoModeloStr);
  ListaDeMetodos.Add(CMetodoModelo);
  ListaDeMetodos.Add(CMetodoPorta);
  ListaDeMetodos.Add(CMetodoTrabalhando);
  ListaDeMetodos.Add(CMetodoLinhasCount);
  ListaDeMetodos.Add(CMetodoSetLinhasCount);
  ListaDeMetodos.Add(CMetodoColunas);
  ListaDeMetodos.Add(CMetodoSetColunas);
  ListaDeMetodos.Add(CMetodoAlinhamento);
  ListaDeMetodos.Add(CMetodoSetAlinhamento);
  ListaDeMetodos.Add(CMetodoIntervalo);
  ListaDeMetodos.Add(CMetodoSetIntervalo);
  ListaDeMetodos.Add(CMetodoPassos);
  ListaDeMetodos.Add(CMetodoSetPassos);
  ListaDeMetodos.Add(CMetodoLimparDisplay);
  ListaDeMetodos.Add(CMetodoEscrever);
  ListaDeMetodos.Add(CMetodoPosicionarCursor);
  ListaDeMetodos.Add(CMetodoParar);
  ListaDeMetodos.Add(CMetodoContinuar);
  ListaDeMetodos.Add(CMetodoPararLinha);
  ListaDeMetodos.Add(CMetodoContinuarLinha);
  ListaDeMetodos.Add(CMetodoExibirLinha);
  ListaDeMetodos.Add(CMetodoRolarLinha);
end;

procedure TACBrObjetoDIS.Executar(ACmd: TACBrCmd);
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
    6  : AMetodoClass := TMetodoTrabalhando;
    7  : AMetodoClass := TMetodoLinhasCount;
    8  : AMetodoClass := TMetodoSetLinhasCount;
    9  : AMetodoClass := TMetodoColunas;
   10  : AMetodoClass := TMetodoSetColunas;
   11  : AMetodoClass := TMetodoAlinhamento;
   12  : AMetodoClass := TMetodoSetAlinhamento;
   13  : AMetodoClass := TMetodoIntervalo;
   14  : AMetodoClass := TMetodoSetIntervalo;
   15  : AMetodoClass := TMetodoPassos;
   16  : AMetodoClass := TMetodoSetPassos;
   17  : AMetodoClass := TMetodoLimparDisplay;
   18  : AMetodoClass := TMetodoEscrever;
   19  : AMetodoClass := TMetodoPosicionarCursor;
   20  : AMetodoClass := TMetodoParar;
   21  : AMetodoClass := TMetodoContinuar;
   22  : AMetodoClass := TMetodoPararLinha;
   23  : AMetodoClass := TMetodoContinuarLinha;
   24  : AMetodoClass := TMetodoExibirLinha;
   25  : AMetodoClass := TMetodoRolarLinha;
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
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.Ativar;
  end;
end;

{ TMetodoDesativar }

procedure TMetodoDesativar.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.Desativar;
  end;
end;

{ TMetodoAtivo }

procedure TMetodoAtivo.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr(ACBrDIS.Ativo, true);
  end;
end;

{ TMetodoModeloStr }

procedure TMetodoModeloStr.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrDIS.ModeloStr;
  end;
end;

{ TMetodoModelo }

procedure TMetodoModelo.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    fpCmd.Resposta := GetEnumName(TypeInfo(TACBrDISModelo),Integer(ACBrDIS.Modelo));
  end;
end;

{ TMetodoPorta }

procedure TMetodoPorta.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrDIS.Porta;
  end;
end;

{ TTrabalhando }

procedure TMetodoTrabalhando.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr( ACBrDIS.Trabalhando, true );
  end;
end;

{ TLinhasCount }

procedure TMetodoLinhasCount.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr( ACBrDIS.LinhasCount );
  end;
end;

{ TSetLinhasCount }

{ Params: 0 - Integer - Numero de Linhas
}
procedure TMetodoSetLinhasCount.Executar;
var
  ALinhas: Integer;
begin
  ALinhas := StrToInt(fpCmd.Params(0));

  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.LinhasCount  := ALinhas;
  end;
end;

{ TColunas }

procedure TMetodoColunas.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr( ACBrDIS.Colunas );
  end;
end;

{ TMetodoSetColunas }

{ Params: 0 - Integer
}
procedure TMetodoSetColunas.Executar;
var
  AColunas: Integer;
begin
  AColunas := StrToInt(fpCmd.Params(0));

  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.Colunas := AColunas;
  end;
end;

{ TMetodoAlinhamento }

procedure TMetodoAlinhamento.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    fpCmd.Resposta := GetEnumName(TypeInfo(TACBrDISAlinhamento),Integer(ACBrDIS.Alinhamento));
  end;
end;

{ TMetodoSetAlinhamento }

{ Params: 0 - Inteiro
}
procedure TMetodoSetAlinhamento.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.Alinhamento := TACBrDISAlinhamento( GetEnumValue(
                            TypeInfo(TACBrDISAlinhamento), fpCmd.Params(0)));
  end;
end;

{ TMetodoIntervalo }

procedure TMetodoIntervalo.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr( ACBrDIS.Intervalo );
  end;
end;

{ TMetodoSetIntervalo }

{ Params: 0 - Inteiro
}
procedure TMetodoSetIntervalo.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.Intervalo := StrToInt( fpCmd.Params(0) );
  end;
end;

{ TMetodoPassos }

procedure TMetodoPassos.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr( ACBrDIS.Passos );
  end;
end;

{ TMetodoSetPassos }

{ Params: 0 - Inteiro
}
procedure TMetodoSetPassos.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.Passos := StrToInt( fpCmd.Params(0) );
  end;
end;

{ TMetodoLimparDisplay }

procedure TMetodoLimparDisplay.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.LimparDisplay;
  end;
end;

{ TMetodoEscrever }

{ Params: 0 - String com o texto
}
procedure TMetodoEscrever.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.Escrever( fpCmd.Params(0) );
  end;
end;

{ TMetodoPosicionarCursor }

{ Params: 0 - Inteiro - Linnha
          1 - Inteiro - Coluna
}
procedure TMetodoPosicionarCursor.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.PosicionarCursor( StrToInt(fpCmd.Params(0)),
                              StrToInt(fpCmd.Params(1)) );
  end;
end;

{ TMetodoParar }

procedure TMetodoParar.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.Parar;
  end;
end;

{ TMetodoContinuar }

procedure TMetodoContinuar.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.Continuar;
  end;
end;

{ TMetodoPararLinha }

{ Params: 0 - Inteiro
}
procedure TMetodoPararLinha.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.PararLinha(StrToInt( fpCmd.Params(0) ));
  end;
end;

{ TMetodoContinuarLinha }

{ Params: 0 - Inteiro
}
procedure TMetodoContinuarLinha.Executar;
begin
  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.ContinuarLinha(StrToInt( fpCmd.Params(0) ));
  end;
end;

{ TMetodoExibirLinha }

{ Params: 0 - Inteiro
}
procedure TMetodoExibirLinha.Executar;
var
  ALinha: Integer;
  ATexto: String;
  AComando: String;
begin
  ALinha := StrToInt(fpCmd.Params(0));
  ATexto := fpCmd.Params(1);
  AComando := fpCmd.Params(2);

  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    if AComando <> '' then
    begin
      if LowerCase(copy(AComando, 1, 3)) = 'efe' then
        ACBrDIS.ExibirLinha(ALinha , ATexto,
            TACBrDISEfeitoExibir(GetEnumValue(TypeInfo(TACBrDISEfeitoExibir), AComando)))
      else
        ACBrDIS.ExibirLinha(ALinha, ATexto,
            TACBrDISAlinhamento(GetEnumValue(TypeInfo(TACBrDISAlinhamento), AComando)));
    end
    else
      ACBrDIS.ExibirLinha(ALinha, ATexto);
  end;
end;

{ TMetodoRolarLinha }

{ Params: 0 - Inteiro - Linnha
          1 - String - Efeito
}
procedure TMetodoRolarLinha.Executar;
var
  ALinha: Integer;
  AEfeito: String;
begin
  ALinha := StrToInt(fpCmd.Params(0));
  AEfeito := fpCmd.Params(1);

  with TACBrObjetoDIS(fpObjetoDono) do
  begin
    ACBrDIS.RolarLinha(ALinha,
        TACBrDISEfeitoRolar(GetEnumValue(TypeInfo(TACBrDISEfeitoRolar), AEfeito)));
  end;
end;

end.
