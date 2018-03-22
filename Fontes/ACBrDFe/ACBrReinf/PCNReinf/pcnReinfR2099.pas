{******************************************************************************}
{ Projeto: Componente ACBrReinf                                                }
{  Biblioteca multiplataforma de componentes Delphi para envio de eventos do   }
{ Reinf                                                                        }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 04/12/2017: Renato Rubinho
|*  - Implementados registros que faltavam e isoladas as respectivas classes 
*******************************************************************************}

unit pcnReinfR2099;

interface

uses
  Classes, Sysutils, pcnGerador, pcnConversaoReinf, ACBrReinfEventosBase,
  pcnReinfClasses, pcnReinfR2099_Class;

type

  TR2099 = class(TEventoReinfR)
  private
    FideRespInf: TideRespInf;
    FinfoFech: TinfoFech;
  protected
    procedure GerarEventoXML; override;
  public
    property ideRespInf: TideRespInf read FideRespInf;
    property infoFech: TinfoFech read FinfoFech;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

uses
  pcnAuxiliar, ACBrUtil, pcnConversao, DateUtils;

{ TR2099 }

procedure TR2099.AfterConstruction;
begin
  inherited;
  SetSchema(schevtFechaEvPer);
  FideRespInf := TideRespInf.Create;
  FinfoFech := TinfoFech.Create;
end;

procedure TR2099.BeforeDestruction;
begin
  inherited;
  FideRespInf.Free;
  FinfoFech.Free;
end;

procedure TR2099.GerarEventoXML;
begin
  if (FideRespInf.nmResp <> EmptyStr) and (FideRespInf.cpfResp <> EmptyStr) then
  begin
    Gerador.wGrupo('ideRespInf');

    Gerador.wCampo(tcStr, '', 'nmResp',    1, 70, 1, FideRespInf.nmResp);
    Gerador.wCampo(tcStr, '', 'cpfResp',  11, 11, 1, FideRespInf.cpfResp);
    Gerador.wCampo(tcStr, '', 'telefone',  1, 13, 0, FideRespInf.telefone);
    Gerador.wCampo(tcStr, '', 'email',     1, 60, 0, FideRespInf.email);

    Gerador.wGrupo('/ideRespInf');
  end;

  Gerador.wGrupo('infoFech');

  Gerador.wCampo(tcStr, '', 'evtServTm',     1, 1, 1, SimNaoToStr(FinfoFech.evtServTm));
  Gerador.wCampo(tcStr, '', 'evtServPr',     1, 1, 1, SimNaoToStr(FinfoFech.evtServPr));
  Gerador.wCampo(tcStr, '', 'evtAssDespRec', 1, 1, 1, SimNaoToStr(FinfoFech.evtAssDespRec));
  Gerador.wCampo(tcStr, '', 'evtAssDespRep', 1, 1, 1, SimNaoToStr(FinfoFech.evtAssDespRep));
  Gerador.wCampo(tcStr, '', 'evtComProd',    1, 1, 1, SimNaoToStr(FinfoFech.evtComProd));
  Gerador.wCampo(tcStr, '', 'evtCPRB',       1, 1, 1, SimNaoToStr(FinfoFech.evtCPRB));
  Gerador.wCampo(tcStr, '', 'evtPgtos',      1, 1, 1, SimNaoToStr(FinfoFech.evtPgtos));
  Gerador.wCampo(tcStr, '', 'compSemMovto',  1, 7, 0, FinfoFech.compSemMovto);

  Gerador.wGrupo('/infoFech');
end;

end.
