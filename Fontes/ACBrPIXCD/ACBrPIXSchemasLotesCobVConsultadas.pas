{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

(*

  Documentação:
  https://github.com/bacen/pix-api

*)

{$I ACBr.inc}

unit ACBrPIXSchemasLotesCobVConsultadas;

interface

uses
  Classes, SysUtils, ACBrPIXBase, ACBrJSON,
  ACBrPIXSchemasParametrosConsultaLote, ACBrPIXSchemasLoteCobV;

type

  { TACBrPIXLotesCobVConsultados }

  TACBrPIXLotesCobVConsultados = class(TACBrPIXSchema)
  private
    flotes: TACBrPIXLoteCobVConsultadoArray;
    fparametros: TACBrPIXParametrosConsultaLote;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    destructor Destroy; override;
    procedure Assign(Source: TACBrPIXLotesCobVConsultados);

    property parametros: TACBrPIXParametrosConsultaLote read fparametros;
    property lotes: TACBrPIXLoteCobVConsultadoArray read flotes;
  end;

implementation

{ TACBrPIXLotesCobVConsultados }

constructor TACBrPIXLotesCobVConsultados.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  flotes := TACBrPIXLoteCobVConsultadoArray.Create('lotes');
  fparametros := TACBrPIXParametrosConsultaLote.Create('parametros');
end;

destructor TACBrPIXLotesCobVConsultados.Destroy;
begin
  flotes.Free;
  fparametros.Free;
  inherited Destroy;
end;

procedure TACBrPIXLotesCobVConsultados.Clear;
begin
  flotes.Clear;
  fparametros.Clear;
end;

function TACBrPIXLotesCobVConsultados.IsEmpty: Boolean;
begin
  Result := flotes.IsEmpty and
            fparametros.IsEmpty;
end;

procedure TACBrPIXLotesCobVConsultados.Assign(Source: TACBrPIXLotesCobVConsultados);
begin
  fparametros.Assign(Source.parametros);
  flotes.Assign(Source.lotes);
end;

procedure TACBrPIXLotesCobVConsultados.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  fparametros.WriteToJSon(AJSon);
  flotes.WriteToJSon(AJSon);
end;

procedure TACBrPIXLotesCobVConsultados.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  fparametros.ReadFromJSon(AJSon);
  flotes.ReadFromJSon(AJSon);
end;

end.

