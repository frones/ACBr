{ ****************************************************************************** }
{ Projeto: Componente ACBrMDFe }
{ Biblioteca multiplataforma de componentes Delphi }
{ }
{ Você pode obter a última versão desse arquivo na pagina do Projeto ACBr }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr }
{ }
{ }
{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior. }
{ }
{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT) }
{ }
{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc., }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA. }
{ Você também pode obter uma copia da licença em: }
{ http://www.opensource.org/licenses/lgpl-license.php }
{ }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br }
{ Praça Anita Costa, 34 - Tatuí - SP - 18270-410 }
{ }
{ ****************************************************************************** }
{ ******************************************************************************
  |* Historico
  |*
  |* 18/10/2013: Jeanny Paiva Lopes
  |*  - Inicio do desenvolvimento DAMDFE FastReport
  ****************************************************************************** }
{$I ACBr.inc}

unit ACBrMDFeDAMDFEFRReg;

interface

uses
  SysUtils, Classes, ACBrMDFeDAMDFEFR, ACBrReg
  {$IFDEF FPC}
    , LResources
  {$ELSE}
     {$IFNDEF COMPILER6_UP}
       , DsgnIntf
     {$ELSE}
       , DesignIntf
       , DesignEditors
     {$ENDIF}
   {$ENDIF} ;

Type
  { Editor de Proriedades de Componente para chamar OpenDialog dos Relatorios }

  { TACBrMDFeDAMDFEFRFileNameProperty }

  TACBrMDFeDAMDFEFRFileNameProperty = class(TACBrFileProperty)
  protected
    function GetFilter: String; override;
  end;


procedure Register;

implementation

{$IFNDEF FPC}
   {$R ACBrMDFe.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrMDFe', [TACBrMDFeDAMDFEFR]);

  RegisterPropertyEditor(TypeInfo(String), TACBrMDFeDAMDFEFR, 'FastFile',
     TACBrMDFeDAMDFEFRFileNameProperty);
end;

{ TACBrMDFeDAMDFEFRFileNameProperty }

function TACBrMDFeDAMDFEFRFileNameProperty.GetFilter: String;
begin
  Result := 'Arquivos do FastReport|*.fr3|Todos os arquivos|*.*';
end;

end.
