{******************************************************************************}
{ Projeto: Componente ACBrBPe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Bilhete de }
{ Passagem Eletrônica - BPe                                                    }
{                                                                              }
{ Direitos Autorais Reservados (c) 2017                                        }
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

{*******************************************************************************
|* Historico
|*
|* 20/06/2017: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrBPeReg;

interface

uses
  SysUtils, Classes, ACBrBPe, pcnConversao,
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

procedure Register;

implementation

uses
  ACBrReg, ACBrDFeRegUtil, ACBrDFeConfiguracoes, ACBrBPeConfiguracoes;

{$IFNDEF FPC}
   {$R ACBrBPe.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrBPe', [TACBrBPe]);

  RegisterPropertyEditor(TypeInfo(TCertificadosConf), TConfiguracoes, 'Certificados',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(TConfiguracoes), TACBrBPe, 'Configuracoes',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(TWebServicesConf), TConfiguracoes, 'WebServices',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(String), TWebServicesConf, 'UF',
     TACBrUFProperty);

  RegisterPropertyEditor(TypeInfo(TGeralConfBPe), TConfiguracoes, 'Geral',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(String), TGeralConfBPe, 'PathSalvar',
     TACBrDirProperty);

  RegisterPropertyEditor(TypeInfo(TArquivosConfBPe), TConfiguracoes, 'Arquivos',
    TClassProperty);

  RegisterPropertyEditor(TypeInfo(String), TArquivosConfBPe, 'PathBPe',
     TACBrDirProperty);

  RegisterPropertyEditor(TypeInfo(String), TArquivosConfBPe, 'PathEvento',
     TACBrDirProperty);

  RegisterPropertyEditor(TypeInfo(TRespTecConf), TConfiguracoes, 'RespTec',
     TClassProperty);
end;

{$ifdef FPC}
initialization
   {$i ACBrBPe.lrs}
{$endif}

end.
