{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}
{$I ACBr.inc}

unit ACBreSocialReg;

interface

uses
 SysUtils, Classes,
  {$IFDEF VisualCLX} QDialogs {$ELSE} Dialogs, FileCtrl {$ENDIF},
  {$IFDEF FPC}
     LResources, LazarusPackageIntf, PropEdits, componenteditors,
  {$ELSE}
    {$IFNDEF COMPILER6_UP}
       DsgnIntf,
    {$ELSE}
       DesignIntf,
       DesignEditors,
    {$ENDIF}
  {$ENDIF}
 ACBreSocial;

procedure Register;

implementation

uses
 ACBreSocialConfiguracoes, ACBrBase, ACBrDFeConfiguracoes, ACBrReg, ACBrDFeRegUtil;
//{$IFNDEF FPC}
//   {$R ACBreSocial.dcr}
//{$ENDIF}

procedure Register;
begin
 RegisterComponents('ACBreSocial', [TACBreSocial]);

// RegisterPropertyEditor(TypeInfo(TACBrAboutInfo), nil, 'AboutACBreSocial',
//     TACBrAboutDialogProperty);
// RegisterPropertyEditor(TypeInfo(TConfiguracoeseSocial), TACBreSocial, 'Configuracoes',
//    TClassProperty);
//// RegisterPropertyEditor(TypeInfo(TGeralConf), TConfiguracoeseSocial, 'Geral',
////    TClassProperty);
//// RegisterPropertyEditor(TypeInfo(String), TGeralConf, 'PathSalvar',
////     TACBreSocialDirProperty);
  RegisterPropertyEditor(TypeInfo(TCertificadosConf), TConfiguracoeseSocial, 'Certificados',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(TConfiguracoeseSocial), TACBreSocial, 'Configuracoes',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(TWebServicesConf), TConfiguracoeseSocial, 'WebServices',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(String), TWebServicesConf, 'UF',
     TACBrUFProperty);

  RegisterPropertyEditor(TypeInfo(TGeralConf), TConfiguracoeseSocial, 'Geral',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(String), TGeralConf, 'PathSalvar',
     TACBrDirProperty);

  RegisterPropertyEditor(TypeInfo(TArquivosConf), TConfiguracoeseSocial, 'Arquivos',
    TClassProperty);
end;

{$ifdef FPC}

initialization
//   {$i ACBrNFe.lrs}
{$endif}

end.
