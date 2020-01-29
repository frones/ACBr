{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 27/05/2004: Primeira Versao
|*    Daniel Simoes de Almeida
|*    Criaçao de um arquivo unico para registro dos Componentes
******************************************************************************}

{$I ACBr.inc}
unit ACBrDiversosReg;

interface
Uses Classes ,
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
  { Editor de Componente para mostrar a Calculadora}
  TACBrCalculadoraEditor = class( TComponentEditor )
  public
    procedure Edit; override;
  end;

  { Editor de Componente para ACBrFala falar em Design }
  TACBrFalaEditor = class( TComponentEditor )
  public
    procedure Edit; override;
  end;

procedure Register;

implementation
Uses ACBrReg, ACBrEnterTab, ACBrUtil, ACBrGIF, ACBrCargaBal,
     ACBrCalculadora, ACBrExtenso, ACBrTroco, ACBrValidador,
     ACBrCMC7, ACBrFala, ACBrBarCode, ACBrInStore, SysUtils;

{$IFNDEF FPC}
   {$R ACBrDiversos.dcr}
{$ENDIF}

procedure Register;
begin
  {$IFDEF DELPHICOMPILER9_UP}
  ForceDemandLoadState(dlDisable);
  {$ENDIF}
  RegisterComponents('ACBrDiversos', [TACBrCalculadora, TACBrCMC7, TACBrExtenso, TACBrTroco,
     TACBrValidador, TACBrFala, TACBrEnterTab, TACBrGIF, TACBrBarCode, TACBrInStore, TACBrCargaBal]);

  { Registrando os Editores de Propriedade }
  RegisterPropertyEditor(TypeInfo(String), TACBrFala, 'OrigemArquivos',
     TACBrDirProperty);

  { Registrando os Editores de Componente }
  RegisterComponentEditor(TACBrCalculadora, TACBrCalculadoraEditor);
  RegisterComponentEditor(TACBrFala, TACBrFalaEditor);
end;


{ TACBrCalculadoraEditor }

procedure TACBrCalculadoraEditor.Edit;
begin
  with Component as TACBrCalculadora do
     Execute ;
end;

{ TACBrFalaEditor }

procedure TACBrFalaEditor.Edit;
begin
  with Component as TACBrFala do
     Falar ;
end;

{$IFDEF FPC}
initialization
   {$i ACBrDiversos.lrs}
{$ENDIF}

end.
