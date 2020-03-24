{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
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

unit ExtensoTeste1;

interface

uses
  SysUtils,
 {$IFDEF Delphi6_UP} Types, Variants, {$ELSE} Windows,{$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ACBrBase, ACBrExtenso;

type
  TfrExtenso = class(TForm)
    edValor: TEdit;
    Label1: TLabel;
    bExtenso: TButton;
    mExtenso: TMemo;
    ACBrExtenso1: TACBrExtenso;
    cbZeroAEsquerda: TCheckBox;
    Label2: TLabel;
    ComboBox1: TComboBox;
    procedure bExtensoClick(Sender: TObject);
    procedure edValorKeyPress(Sender: TObject; var Key: Char);
    procedure cbZeroAEsquerdaClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frExtenso: TfrExtenso;

implementation

{$R *.dfm}

procedure TfrExtenso.FormCreate(Sender: TObject);
begin
  ComboBox1.ItemIndex := 0 ;
end;

procedure TfrExtenso.bExtensoClick(Sender: TObject);
begin
  ACBrExtenso1.Valor := StrToFloat( edValor.Text ) ;
  mExtenso.Text := ACBrExtenso1.Texto ;
end;

procedure TfrExtenso.edValorKeyPress(Sender: TObject; var Key: Char);
begin
  if not ( Key in ['0'..'9',',','.',#13,#8] ) then
     Key := #0
  else
     if Key in [',','.'] then
        Key := DecimalSeparator ;
end;

procedure TfrExtenso.cbZeroAEsquerdaClick(Sender: TObject);
begin
  ACBrExtenso1.ZeroAEsquerda := cbZeroAEsquerda.Checked ;
end;

procedure TfrExtenso.ComboBox1Change(Sender: TObject);
begin
  ACBrExtenso1.Formato := TACBrExtensoFormato( ComboBox1.ItemIndex ) ;

  if ACBrExtenso1.Formato = extDolar then
   begin
     ACBrExtenso1.StrMoeda  := 'Dolar Americano' ;
     ACBrExtenso1.StrMoedas := 'Dolares Americanos' ;
   end
  else
   begin
     ACBrExtenso1.StrMoeda  := 'Real' ;
     ACBrExtenso1.StrMoedas := 'Reais' ;
   end ;
end;

end.
