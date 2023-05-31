{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
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
{$I ACBr.inc}

unit ACBrBoletoFPDFReg;

interface

uses
  SysUtils, Classes, ACBrBoletoFPDF, ACBrReg
  {$IFDEF FPC}
     , LResources, LazarusPackageIntf, PropEdits, componenteditors
  {$ELSE}
     {$IFNDEF COMPILER6_UP}
        , DsgnIntf
     {$ELSE}
        , DesignIntf, DesignEditors
     {$ENDIF}
  {$ENDIF} ;

procedure Register;

implementation



procedure Register;
begin
  RegisterComponents('ACBrBoleto', [TACBrBoletoFPDF]);
  {$IFDEF FPC}
    RegisterPropertyEditor(TypeInfo(Boolean), TACBrBoletoFPDF, 'MostrarPreview', THiddenPropertyEditor);
    RegisterPropertyEditor(TypeInfo(Boolean), TACBrBoletoFPDF, 'MostrarSetup', THiddenPropertyEditor);
    RegisterPropertyEditor(TypeInfo(Boolean), TACBrBoletoFPDF, 'MostrarProgresso', THiddenPropertyEditor);
    RegisterPropertyEditor(TypeInfo(Integer), TACBrBoletoFPDF, 'NumCopias', THiddenPropertyEditor);
    RegisterPropertyEditor(TypeInfo(String), TACBrBoletoFPDF,  'Filtro', THiddenPropertyEditor);
    RegisterPropertyEditor(TypeInfo(String), TACBrBoletoFPDF,  'PrinterName', THiddenPropertyEditor);
    RegisterPropertyEditor(TypeInfo(Boolean), TACBrBoletoFPDF, 'AlterarEscalaPadrao', THiddenPropertyEditor);
    RegisterPropertyEditor(TypeInfo(Integer), TACBrBoletoFPDF, 'NovaEscala', THiddenPropertyEditor);
    RegisterPropertyEditor(TypeInfo(String), TACBrBoletoFPDF,  'TituloPreview', THiddenPropertyEditor);
  {$ELSE}
    UnlistPublishedProperty(TACBrBoletoFPDF, 'MostrarPreview');
    UnlistPublishedProperty(TACBrBoletoFPDF, 'MostrarSetup');
    UnlistPublishedProperty(TACBrBoletoFPDF, 'MostrarProgresso');
    UnlistPublishedProperty(TACBrBoletoFPDF, 'NumCopias');
    UnlistPublishedProperty(TACBrBoletoFPDF, 'Filtro');
    UnlistPublishedProperty(TACBrBoletoFPDF, 'PrinterName');
    UnlistPublishedProperty(TACBrBoletoFPDF, 'AlterarEscalaPadrao');
    UnlistPublishedProperty(TACBrBoletoFPDF, 'NovaEscala');
    UnlistPublishedProperty(TACBrBoletoFPDF, 'TituloPreview');
  {$ENDIF}
end;

end.
