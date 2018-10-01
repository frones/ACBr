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

unit DoCEPUnit;

interface

uses
  Classes, TypInfo, SysUtils, CmdUnit, ACBrUtil, ACBrCEP,
  ACBrMonitorConsts, ACBrMonitorConfig, ACBrLibResposta, ACBrLibCEPRespostas,
  ACBrLibCEPConsts;

type

{ TACBrObjetoCEP }

TACBrObjetoCEP = class(TACBrObjetoDFe)
private
  fACBrCEP: TACBrCEP;
public
  constructor Create(AConfig: TMonitorConfig; ACBrCEP: TACBrCEP); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  procedure RespostaItensConsulta(ItemID: integer = 0);

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

procedure TACBrObjetoCEP.RespostaItensConsulta(ItemID: integer);
var
  Resp: TLibCEPResposta;
begin
  Resp := TLibCEPResposta.Create(
          CSessaoRespConsulta + IntToStr(ItemID +1), resINI);
  try
    with fACBrCEP.Enderecos[ItemID] do
    begin
      Resp.CEP := CEP;
      Resp.Tipo_Logradouro := Tipo_Logradouro;
      Resp.Logradouro := Logradouro;
      Resp.Logradouro := Logradouro;
      Resp.Complemento := Complemento;
      Resp.Bairro := Bairro;
      Resp.Municipio := Municipio;
      Resp.UF := UF;
      Resp.IBGE_Municipio := IBGE_Municipio;
      Resp.IBGE_UF := IBGE_UF;

      fpCmd.Resposta := fpCmd.Resposta + Resp.Gerar;
    end;
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
    RespostaItensConsulta(0);
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
var
  I: integer;
begin
  with TACBrObjetoCEP(fpObjetoDono) do
  begin
    ACBrCEP.BuscarPorLogradouro( fpCmd.Params(0), fpCmd.Params(1),
                                 fpCmd.Params(2), fpCmd.Params(3),
                                 fpCmd.Params(4) );

    for I := 0 to ACBrCEP.Enderecos.Count - 1 do
      RespostaItensConsulta(I);
  end;
end;

end.
