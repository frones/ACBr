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

unit pcnReinfR2040;

interface

uses
  Classes, Sysutils, pcnGerador, pcnConversaoReinf, ACBrReinfEventosBase,
  pcnReinfClasses, pcnReinfR2040_Class;

type

  TR2040 = class(TEventoReinfRet)
  private
    FideEstab : TideEstab;
  protected
    procedure GerarEventoXML; override;
    procedure GerarinfoideEstab;
    procedure GerarrecursosRep(Items: TrecursosReps);
    procedure GerarinfoRecurso(Items: TinfoRecursos);
    procedure GerarinfoProc(Items: TinfoProcs);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property ideEstab: TideEstab read FideEstab;
  end;

implementation

uses
  pcnAuxiliar, ACBrUtil, pcnConversao, DateUtils;


{ TR2040 }

procedure TR2040.AfterConstruction;
begin
  inherited;
  SetSchema(rsevtAssocDespRep);
  FideEstab := TideEstab.Create;
end;

procedure TR2040.BeforeDestruction;
begin
  inherited;
  FideEstab.Free;
end;

procedure TR2040.GerarEventoXML;
begin
  GerarinfoideEstab;
end;

procedure TR2040.GerarinfoideEstab;
begin
  Gerador.wGrupo('ideEstab');
  Gerador.wCampo(tcInt, '', 'tpInscEstab', 0, 0, 1, Ord( Self.FideEstab.tpInscEstab ));
  Gerador.wCampo(tcStr, '', 'nrInscEstab', 0, 0, 1, Self.FideEstab.nrInscEstab);
  GerarrecursosRep(Self.FideEstab.recursosReps);
  Gerador.wGrupo('/ideEstab');
end;

procedure TR2040.GerarrecursosRep(Items: TrecursosReps);
var
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
    with Items.Items[i] do
    begin
      Gerador.wGrupo('recursosRep');
      Gerador.wCampo(tcStr, '', 'cnpjAssocDesp', 0, 0, 1, cnpjAssocDesp);
      Gerador.wCampo(tcDe2, '', 'vlrTotalRep',   0, 0, 1, vlrTotalRep);
      Gerador.wCampo(tcDe2, '', 'vlrTotalRet',   0, 0, 1, vlrTotalRet);
      Gerador.wCampo(tcDe2, '', 'vlrTotalNRet',  0, 0, 0, vlrTotalNRet);
      GerarinfoRecurso(infoRecursos);
      GerarinfoProc(infoProcs);
      Gerador.wGrupo('/recursosRep');
    end;
end;

procedure TR2040.GerarinfoRecurso(Items: TinfoRecursos);
var
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
    with Items.Items[i] do
    begin
      Gerador.wGrupo('infoRecurso');
      Gerador.wCampo(tcInt, '', 'tpRepasse',   0, 0, 1, Ord( tpRepasse ));
      Gerador.wCampo(tcStr, '', 'descRecurso', 0, 0, 1, descRecurso);
      Gerador.wCampo(tcDe2, '', 'vlrBruto',    0, 0, 1, vlrBruto);
      Gerador.wCampo(tcDe2, '', 'vlrRetApur',  0, 0, 1, vlrRetApur);
      Gerador.wGrupo('/infoRecurso');
    end;
end;

procedure TR2040.GerarinfoProc(Items: TinfoProcs);
var
  i: Integer;
begin
  for i:=0 to Items.Count - 1 do
    with Items.Items[i] do
    begin
      Gerador.wGrupo('infoProc');
      Gerador.wCampo(tcInt, '', 'tpProc',  0, 0, 1, Ord( tpProc ));
      Gerador.wCampo(tcStr, '', 'nrProc',  0, 0, 1, nrProc);
      Gerador.wCampo(tcStr, '', 'codSusp', 0, 0, 0, codSusp);
      Gerador.wCampo(tcDe2, '', 'vlrNRet', 0, 0, 1, vlrNRet);
      Gerador.wGrupo('/infoProc');
    end;
end;

end.
