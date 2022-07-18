{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit Calculadora;

interface

uses
  ACBrCalculadora,
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
   Dialogs, Menus, StdCtrls, ExtCtrls, Windows,
  {$IFDEF COMPILER6_UP} Variants {$ELSE} ACBrD5 {$ENDIF}  ;

type

  { TFrCalculadora }

  TFrCalculadora = class(TForm)
    b1: TButton;
    b0: TButton;
    b2: TButton;
    b3: TButton;
    b4: TButton;
    b5: TButton;
    b6: TButton;
    b7: TButton;
    b8: TButton;
    b9: TButton;
    pValor : TPanel;
    bponto: TButton;
    bigual: TButton;
    bmais: TButton;
    bmenos: TButton;
    bmulti: TButton;
    bdiv: TButton;
    bapaga: TButton;
    bc: TButton;
    bce: TButton;
    bporc: TButton;
    mBobina: TMemo;
    PopupMenu1: TPopupMenu;
    Limpar1: TMenuItem;
    Salvar1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Copias1: TMenuItem;
    procedure ExecOnCalcKey(Sender : TObject; Key : Char ) ;
    procedure ExecOnDisplayChange(Sender : TObject ) ;
    procedure ZeraDisplay( Sender : TObject ) ;
    
    procedure b1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure bapagaClick(Sender: TObject);
    procedure bpontoClick(Sender: TObject);
    procedure bcClick(Sender: TObject);
    procedure b0Click(Sender: TObject);
    procedure bceClick(Sender: TObject);
    procedure AcaoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Salvar1Click(Sender: TObject);
    procedure bporcClick(Sender: TObject);
    procedure Copiar1Click(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    fValor : Double ;
    fOperacao : String ;
    fDS: Char;

    function GetValorDisplay: String;
    procedure SetValorDisplay(const Value: String);
  public
    { Public declarations }
    pPrecisao  : Integer ;
    pSaiComEsc : Boolean ;
    pOnCalKey  : TKeyPressEvent ; 
    pOnDisplayChange : TACBrCalculadoraDisplayChange ;

    Property ValorDisplay : String read GetValorDisplay write SetValorDisplay ;
  end;

var
  FrCalculadora: TFrCalculadora;

implementation

uses ACBrUtil.Strings, Math, ACBrConsts;

{$R *.dfm}

{$I incCalculadora.pas}

end.
