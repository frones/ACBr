{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

{$I ACBr.inc}

unit ACBrPagForReg;

interface

uses
  SysUtils, Classes,
  {$IFDEF VisualCLX} QDialogs {$ELSE} Dialogs, FileCtrl {$ENDIF},
  {$IFDEF FPC}
     LResources, LazarusPackageIntf, PropEdits, componenteditors
  {$ELSE}
    {$IFNDEF COMPILER6_UP}
       DsgnIntf
    {$ELSE}
       DesignIntf,
       DesignEditors
    {$ENDIF}
  {$ENDIF} ;

type
  { Editor de Proriedades de Componente para mostrar o AboutACBr }
  TACBrAboutDialogProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
  end;

  { Editor de Proriedades de Componente para chamar OpenDialog }
  TACBrPagForDirProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure Register;

implementation

uses
 ACBrPagFor, ACBrPagForConfiguracoes;

{$IFNDEF FPC}
   {$R ACBrPagFor.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBr', [TACBrPagFor]);

//  RegisterPropertyEditor(TypeInfo(TACBrPagForAboutInfo), nil, 'AboutACBrPagFor',
//     TACBrAboutDialogProperty);

  RegisterPropertyEditor(TypeInfo(TConfiguracoes), TACBrPagFor, 'Configuracoes',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(TGeralConf), TConfiguracoes, 'Geral',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(String), TGeralConf, 'PathSalvar',
     TACBrPagForDirProperty);

  RegisterPropertyEditor(TypeInfo(TArquivosConf), TConfiguracoes, 'Arquivos',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(String), TArquivosConf, 'PathSalvar',
     TACBrPagForDirProperty);

end;

{ TACBrAboutDialogProperty }

procedure TACBrAboutDialogProperty.Edit;
begin
//  ACBrAboutDialog ;
end;

function TACBrAboutDialogProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TACBrAboutDialogProperty.GetValue: String;
begin
//  Result := 'Versão: ' + ACBRPagFor_VERSAO;
end;

{ TACBrPagForeDirProperty }

procedure TACBrPagForDirProperty.Edit;
Var
{$IFNDEF VisualCLX} Dir: String; {$ELSE} Dir: WideString; {$ENDIF}
begin
  {$IFNDEF VisualCLX}
  Dir := GetValue;
  if SelectDirectory(Dir, [], 0) then
     SetValue(Dir);
  {$ELSE}
  Dir := '';
  if SelectDirectory('Selecione o Diretório', '', Dir) then
     SetValue(Dir);
  {$ENDIF}
end;

function TACBrPagForDirProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

initialization
{$IFDEF FPC}
   {$i ACBrPagFor.lrs}
{$ENDIF}

end.
