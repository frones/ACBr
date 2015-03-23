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
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrECFVirtualNaoFiscal ;

interface
uses ACBrECFVirtualPrinter, ACBrECFClass, ACBrUtil, ACBrConsts,
     Classes, SysUtils
     {$IFNDEF NOGUI}
       {$IFDEF VisualCLX}
         ,QControls, QDialogs, QForms
       {$ELSE}
         ,Controls, Dialogs, Forms
       {$ENDIF}
     {$ENDIF} ;

const
  ACBrECFVirtualNaoFiscal_VERSAO = '0.1.0a';

type

{ TACBrECFVirtualNaoFiscal }

TACBrECFVirtualNaoFiscal = class( TACBrECFVirtualPrinter )
  private
    function GetExibeAvisoLegal: Boolean;
    procedure SetExibeAvisoLegal(AValue: Boolean);
  protected
    procedure CreateVirtualClass ; override ;
  published
    property ExibeAvisoLegal: Boolean read GetExibeAvisoLegal
      write SetExibeAvisoLegal;

  published
    property Colunas ;
    property NomeArqINI ;
    property NumSerie ;
    property NumECF ;
    property NumCRO ;
    property CNPJ ;
    property IE ;
    property IM ;

    property CmdImpCondensado ;
    property CmdImpExpandidoUmaLinha ;
    property CmdImpFimExpandido ;
    property CmdImpZera ;
    property CmdGaveta ;
    property CmdCortaPapelCompleto ;
    property CmdCortaPapelParcial ;

    property Cabecalho ;
    property CabecalhoItem ;
    property MascaraItem ;

end ;

{ TACBrECFVirtualNaoFiscalClass }

TACBrECFVirtualNaoFiscalClass = class( TACBrECFVirtualPrinterClass )
  private
    fsExibeAvisoLegal: Boolean;
    Procedure MostraAvisoLegal ;
  protected
    function GetSubModeloECF: String ; override ;
    function GetNumVersao: String; override ;
    procedure AtivarVirtual ; override;
 public
   Constructor create( AOwner : TComponent  )  ;

   property ExibeAvisoLegal: Boolean read fsExibeAvisoLegal write fsExibeAvisoLegal;
 end ;

implementation

{ TACBrECFVirtualNaoFiscal }

procedure TACBrECFVirtualNaoFiscal.CreateVirtualClass;
begin
  fpECFVirtualClass := TACBrECFVirtualNaoFiscalClass.create( self );
end;

function TACBrECFVirtualNaoFiscal.GetExibeAvisoLegal: Boolean;
begin
  Result := TACBrECFVirtualNaoFiscalClass( fpECFVirtualClass ).ExibeAvisoLegal;
end;

procedure TACBrECFVirtualNaoFiscal.SetExibeAvisoLegal(AValue: Boolean);
begin
  TACBrECFVirtualNaoFiscalClass( fpECFVirtualClass ).ExibeAvisoLegal := AValue;
end;

{ TACBrECFVirtualNaoFiscalClass }

constructor TACBrECFVirtualNaoFiscalClass.create(AOwner: TComponent);
begin
  inherited create( AOwner ) ;

  fsExibeAvisoLegal := True;
end;

function TACBrECFVirtualNaoFiscalClass.GetSubModeloECF: String;
begin
  Result := 'VirtualNaoFiscal' ;
end;

function TACBrECFVirtualNaoFiscalClass.GetNumVersao: String ;
begin
  Result := ACBrECFVirtualNaoFiscal_VERSAO ;
end;

procedure TACBrECFVirtualNaoFiscalClass.AtivarVirtual;
begin
  if fsExibeAvisoLegal then
    MostraAvisoLegal ;

  inherited AtivarVirtual;
end;

procedure TACBrECFVirtualNaoFiscalClass.MostraAvisoLegal ;
begin
  {$IFNDEF NOGUI}
   if MessageDlg(ACBrStr( 'Este Emulador destina-se EXCLUSIVAMENTE para auxiliar no '+
                 'desenvolvimento de aplicativos para as impressoras fiscais. '+
                 sLineBreak + sLineBreak +
                 'Usar o emulador para fins comerciais sem a devida impressão '+
                 'do Cupom Fiscal ou Nota Fiscal pode caracterizar crime de '+
                 'Sonegação Fiscal.' + sLineBreak + sLineBreak +
                 'Continua com o uso do Emulador ?' )
                 ,mtWarning,mbYesNoCancel,0) <> mrYes then
     raise EACBrECFERRO.Create( ACBrStr('Uso indevido do emulador'));
  {$ENDIF}
end ;

end.

