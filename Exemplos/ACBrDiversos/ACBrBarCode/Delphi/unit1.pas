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

unit Unit1;


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, Spin, ACBrBarCode;

type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrBarCode1: TACBrBarCode;
    btParaBarCode: TButton;
    btParaImagem: TButton;
    cbBarCodeType: TComboBox;
    edBarcode: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    seRatio: TSpinEdit;
    seModule: TSpinEdit;
    procedure btParaBarCodeClick(Sender: TObject);
    procedure btParaImagemClick(Sender: TObject);
    procedure cbBarCodeTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure seModuleChange(Sender: TObject);
    procedure seRatioChange(Sender: TObject);
  private
    procedure LerParamsBarCode;

  public

  end;

var
  Form1: TForm1;

implementation

uses
  typinfo,
  AJBarcode;

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: TBarcodeType;
begin
  for i := Low(TBarcodeType) to High(TBarcodeType) do
    cbBarCodeType.Items.Add( GetEnumName(TypeInfo(TBarcodeType), integer(i) ) );

  //ACBrBarCode1.Typ := bcCode_2_5_interleaved;
  //ACBrBarCode1.Modul := 1;
  //ACBrBarCode1.Ratio := 3;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  LerParamsBarCode;
end;

procedure TForm1.seModuleChange(Sender: TObject);
begin
  ACBrBarCode1.Modul := seModule.Value;
  LerParamsBarCode;
end;

procedure TForm1.seRatioChange(Sender: TObject);
begin
  ACBrBarCode1.Ratio := seRatio.Value;
  LerParamsBarCode;
end;

procedure TForm1.LerParamsBarCode;
begin
  cbBarCodeType.ItemIndex := Integer(ACBrBarCode1.Typ);
  seRatio.Value := Trunc( ACBrBarCode1.Ratio);
  seModule.Value := ACBrBarCode1.Modul;
end;

procedure TForm1.btParaBarCodeClick(Sender: TObject);
begin
  ACBrBarCode1.Typ :=  TBarcodeType( cbBarCodeType.ItemIndex );
  ACBrBarCode1.Ratio := seRatio.Value;
  ACBrBarCode1.Modul := seModule.Value;

  ACBrBarCode1.Text := edBarcode.Text;
  LerParamsBarCode;
end;

procedure TForm1.btParaImagemClick(Sender: TObject);
begin
  Image1.Width := ACBrBarCode1.Width;
  Image1.Height := ACBrBarCode1.Height;
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.Brush.Style := bsSolid;
  Image1.Canvas.FillRect(ClientRect);
  ACBrBarCode1.DrawBarcode(Image1.Canvas);
end;

procedure TForm1.cbBarCodeTypeChange(Sender: TObject);
begin
  ACBrBarCode1.Typ := TBarcodeType( cbBarCodeType.ItemIndex );
end;

end.

