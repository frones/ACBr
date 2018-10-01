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

unit DoIBGEUnit;

interface

uses
  Classes, TypInfo, SysUtils, CmdUnit, ACBrUtil, ACBrIBGE,
  ACBrMonitorConsts, ACBrMonitorConfig, ACBrLibIBGERespostas,
  ACBrLibIBGEConsts, ACBrLibResposta;

type

{ TACBrObjetoIBGE }

TACBrObjetoIBGE = class(TACBrObjetoDFe)
private
  fACBrIBGE: TACBrIBGE;
public
  constructor Create(AConfig: TMonitorConfig; ACBrIBGE: TACBrIBGE); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  procedure RespostaItensConsulta(ItemID: integer = 0);

  property ACBrIBGE: TACBrIBGE read fACBrIBGE;
end;

{ TMetodoBuscarPorCodigo}
TMetodoBuscarPorCodigo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoBuscarPorNome}
TMetodoBuscarPorNome = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

{ TACBrObjetoIBGE }

constructor TACBrObjetoIBGE.Create(AConfig: TMonitorConfig; ACBrIBGE: TACBrIBGE);
begin
  inherited Create(AConfig);

  fACBrIBGE := ACBrIBGE;

  ListaDeMetodos.Add(CMetodoBuscarPorCodigo);
  ListaDeMetodos.Add(CMetodoBuscarPorNome);
end;

procedure TACBrObjetoIBGE.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoBuscarPorCodigo;
    1  : AMetodoClass := TMetodoBuscarPorNome;
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

procedure TACBrObjetoIBGE.RespostaItensConsulta(ItemID: integer);
var
  Resp: TLibIBGEResposta;
begin
  Resp := TLibIBGEResposta.Create(
          CSessaoRespConsulta + IntToStr(ItemID +1), resINI);
  try
    with fACBrIBGE.Cidades[ItemID] do
    begin
      Resp.UF := UF;
      Resp.CodUF := IntToStr(CodUF);
      Resp.Municipio := Municipio;
      Resp.CodMunicipio := IntToStr(CodMunicipio);
      Resp.Area := FloatToStr(Area);

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;
end;

{ TMetodoBuscarPorCodigo }

{ Params: 0 - String com o codigo IBGE da Cidade
}
procedure TMetodoBuscarPorCodigo.Executar;
var
  I: Integer;
begin
  with TACBrObjetoIBGE(fpObjetoDono) do
  begin
    ACBrIBGE.BuscarPorCodigo( StrToInt( fpCmd.Params(0) ) );

    if ACBrIBGE.Cidades.Count < 1 then
       raise Exception.Create( 'Nenhuma Cidade encontrada' );

    for I := 0 to ACBrIBGE.Cidades.Count-1 do
      RespostaItensConsulta(I);
  end;
end;

{ TMetodoBuscarPorNome }

{ Params: 0 - String com o nome da cidade
}
procedure TMetodoBuscarPorNome.Executar;
var
  I: Integer;
begin
  with TACBrObjetoIBGE(fpObjetoDono) do
  begin
    ACBrIBGE.BuscarPorNome( fpCmd.Params(0) );

    if ACBrIBGE.Cidades.Count < 1 then
      raise Exception.Create( 'Nenhuma Cidade encontrada' );

    for I := 0 to ACBrIBGE.Cidades.Count-1 do
      RespostaItensConsulta(I);
  end;
end;

end.
