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

unit DoNcmUnit;

interface

uses
  Classes, TypInfo, SysUtils, CmdUnit, ACBrUtil, ACBrNCMs,
  ACBrMonitorConsts, ACBrMonitorConfig;

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

{ TMetodoValidar}
TMetodoValidar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoBaixarLista}
TMetodoBaixarLista = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDescricaoNCM}
TMetodoDescricaoNCM = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

{ TACBrObjetoNcm }

constructor TACBrObjetoNcm.Create(AConfig: TMonitorConfig; ACBrNcm: TACBrNcms);
begin
  inherited Create(AConfig);

  fACBrNcm := ACBrNcm;

  ListaDeMetodos.Add(CMetodoValidar);
  ListaDeMetodos.Add(CMetodoBaixarLista);
  ListaDeMetodos.Add(CMetodoDescricaoNCM);
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
    0  : AMetodoClass := TMetodoValidar;
    1  : AMetodoClass := TMetodoBaixarLista;
    2  : AMetodoClass := TMetodoDescricaoNCM;
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

{ Params: 0 - String com o codigo Ncm
}
procedure TMetodoValidar.Executar;
var
  ANCM: String;
begin
  ANCM := OnlyNumber(fpCmd.Params(0));

  with TACBrObjetoNcm(fpObjetoDono) do
  begin
    fpCmd.Resposta := 'NCM Valido';

    if (Length(ANCM) <> 8) then
      raise Exception.Create('O codigo do NCM deve conter 8 Caracteres');

    if not  ACBrNcm.validar(ANCM) then
      raise Exception.Create('NCM Invalido');
  end;
end;

{ TMetodoBaixarLista }

{ Params: 0 - String com o nome do Arquivo a ser salvo em disco
}
procedure TMetodoBaixarLista.Executar;
var
  FileName: String;
begin
  FileName := fpCmd.Params(0);

  with TACBrObjetoNcm(fpObjetoDono) do
  begin
    ACBrNcm.ListarNcms();
    ACBrNcm.NCMS.SaveToFile( FileName );

    fpCmd.Resposta := 'Arquivo salvo em: ' + FileName;
  end;
end;

{ TMetodoDescricaoNCM }

{ Params: 0 - String com o NCM
}
procedure TMetodoDescricaoNCM.Executar;
begin
  with TACBrObjetoNcm(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrNcm.DescricaoNcm( fpCmd.Params(0) );
  end;
end;

end.
