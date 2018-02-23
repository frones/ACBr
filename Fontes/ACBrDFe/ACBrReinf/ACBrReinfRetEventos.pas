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

unit ACBrReinfRetEventos;

interface

uses
  ACBrReinfClasses;

type

  TRetornoLoteEventos = class
  private
    FACBrReinf: TObject;
    FIdeTransmissor: TIdeTransmissor;
    FStatus: TStatus;
    FEventos: TRetEventos;
  public
    constructor Create(AOwner: TObject); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  	property IdeTransmissor : TIdeTransmissor read FIdeTransmissor;
    property Status: TStatus read FStatus;
    property Eventos: TRetEventos read FEventos write FEventos;
  end;

implementation

{ TRetornoLoteEventos }

procedure TRetornoLoteEventos.AfterConstruction;
begin
  inherited;
  FIdeTransmissor := TIdeTransmissor.Create;
  FStatus := TStatus.Create;
  FEventos := TRetEventos.Create;
end;

procedure TRetornoLoteEventos.BeforeDestruction;
begin
  inherited;
  FEventos.Free;
  FIdeTransmissor.Free;
  FStatus.Free;
end;

constructor TRetornoLoteEventos.Create(AOwner: TObject);
begin
  Inherited Create;
  FACBrReinf := AOwner;
end;

end.
