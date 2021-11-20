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

unit ACBrPIXSchemasPixConsultados;

interface

uses
  Classes, SysUtils,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr,
  {$Else}
   Jsons,
  {$EndIf}
  ACBrPIXBase,
  ACBrPIXSchemasParametrosConsultaPix, ACBrPIXSchemasPix;

type

  { TACBrPIXConsultados }

  TACBrPIXConsultados = class(TACBrPIXSchema)
  private
    fparametros: TACBrPIXParametrosConsultaPix;
    fpix: TACBrPIXArray;
  public
    constructor Create;
    procedure Clear; override;
    destructor Destroy; override;
    procedure Assign(Source: TACBrPIXConsultados);

    property parametros: TACBrPIXParametrosConsultaPix read fparametros;
    property pix: TACBrPIXArray read fpix;

    procedure WriteToJSon(AJSon: TJsonObject); override;
    procedure ReadFromJSon(AJSon: TJsonObject); override;
  end;

implementation

{ TACBrPIXConsultados }

constructor TACBrPIXConsultados.Create;
begin
  inherited;
  fparametros := TACBrPIXParametrosConsultaPix.Create;
  fpix := TACBrPIXArray.Create('pix');
  Clear;
end;

destructor TACBrPIXConsultados.Destroy;
begin
  fparametros.Free;
  fpix.Free;
  inherited Destroy;
end;

procedure TACBrPIXConsultados.Clear;
begin
  fparametros.Clear;
  fpix.Clear;
end;

procedure TACBrPIXConsultados.Assign(Source: TACBrPIXConsultados);
begin
  fparametros.Assign(Source.parametros);
  fpix.Assign(Source.pix);
end;

procedure TACBrPIXConsultados.WriteToJSon(AJSon: TJsonObject);
var
  jsp: TJsonObject;
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jsp := AJSon.O['parametros'];
  {$Else}
   jsp := AJSon['parametros'].AsObject;
  {$EndIf}
  fparametros.WriteToJSon(jsp);
  fpix.WriteToJSon(AJSon);
end;

procedure TACBrPIXConsultados.ReadFromJSon(AJSon: TJsonObject);
var
  jsp: TJsonObject;
begin
  Clear;
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   jsp := AJSon.O['parametros'];
  {$Else}
   jsp := AJSon['parametros'].AsObject;
  {$EndIf}
  fparametros.ReadFromJSon(jsp);
  fpix.ReadFromJSon(AJSon);
end;

end.

