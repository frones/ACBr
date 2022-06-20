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

unit ExtensoTeste1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ACBrExtenso, StdCtrls, Buttons, ExtCtrls;

type

  { TfrExtenso }

  TfrExtenso = class(TForm)
    ACBrExtenso1: TACBrExtenso;
    btExtenso: TButton;
    cbFormato: TComboBox;
    cbIdioma: TComboBox;
    cbZeroAEsquerda: TCheckBox;
    edInteiroSingular: TEdit;
    edInteiroPlural: TEdit;
    edDecimalSingular: TEdit;
    edDecimalPlural: TEdit;
    edValor: TEdit;
    lbValor: TLabel;
    lbFormato: TLabel;
    lbInteiroSingular: TLabel;
    lbIdioma: TLabel;
    lbInteiroPlural: TLabel;
    lbDecimalSingular: TLabel;
    lbDecimalPlural: TLabel;
    mExtenso: TMemo;
    pnCabecalho: TPanel;
    pnCustomStr: TPanel;
    procedure btExtensoClick(Sender: TObject);
    procedure cbFormatoChange(Sender: TObject);
    procedure cbIdiomaChange(Sender: TObject);
    procedure edValorKeyPress(Sender: TObject; var Key: Char);
    procedure cbZeroAEsquerdaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AvaliarInterface;
    procedure PreencherInfoDefault;
  public
  end; 

var
  frExtenso: TfrExtenso;

implementation 

{$R *.lfm}

uses
  ACBrUtil.Base, ACBrUtil.Strings, TypInfo;


procedure TfrExtenso.btExtensoClick(Sender: TObject);
begin
  with ACBrExtenso1 do
  begin
    if (Idioma = idiCustom) then
    begin
      StrMoeda    := edInteiroSingular.Text;
      StrMoedas   := edInteiroPlural.Text;
      StrCentavo  := edDecimalSingular.Text;
      StrCentavos := edDecimalPlural.Text;
    end;
    
    Valor := StringToFloat(edValor.Text);
    mExtenso.Text := Texto;
  end;
end;

procedure TfrExtenso.cbFormatoChange(Sender: TObject);
begin
  ACBrExtenso1.Formato := TACBrExtensoFormato(cbFormato.ItemIndex);
  AvaliarInterface;
end;

procedure TfrExtenso.cbIdiomaChange(Sender: TObject);
begin
  ACBrExtenso1.Idioma := TACBrExtensoIdioma(cbIdioma.ItemIndex);
  AvaliarInterface;
end;

procedure TfrExtenso.edValorKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0'..'9',',','.',#13,#8]) then
    Key := #0
  else if Key in [',','.'] then
    Key := FormatSettings.DecimalSeparator;
end;

procedure TfrExtenso.cbZeroAEsquerdaClick(Sender: TObject);
begin
  ACBrExtenso1.ZeroAEsquerda := cbZeroAEsquerda.Checked;
end;

procedure TfrExtenso.FormCreate(Sender: TObject);
var
  I: TACBrExtensoIdioma;
  J: TACBrExtensoFormato;
begin
  cbIdioma.Items.Clear;
  for I := Low(TACBrExtensoIdioma) to High(TACBrExtensoIdioma) do
     cbIdioma.Items.Add(GetEnumName(TypeInfo(TACBrExtensoIdioma), Integer(I)));

  cbFormato.Items.Clear;
  for J := Low(TACBrExtensoFormato) to High(TACBrExtensoFormato) do
     cbFormato.Items.Add(GetEnumName(TypeInfo(TACBrExtensoFormato), Integer(J)));

  PreencherInfoDefault;
  AvaliarInterface;
end;

procedure TfrExtenso.AvaliarInterface;
begin
  with ACBrExtenso1 do
  begin
    edInteiroSingular.Text := StrMoeda;
    edInteiroPlural.Text   := StrMoedas;
    edDecimalSingular.Text := StrCentavo;
    edDecimalPlural.Text   := StrCentavos;

    pnCustomStr.Enabled := (Idioma = idiCustom);
    lbFormato.Enabled := (Idioma <> idiCustom);
    cbFormato.Enabled := (Idioma <> idiCustom);
  end;
end;

procedure TfrExtenso.PreencherInfoDefault;
begin
  with ACBrExtenso1 do
  begin
    cbIdioma.ItemIndex := Ord(Idioma);
    cbFormato.ItemIndex := Ord(Formato);

    edInteiroSingular.Text := StrMoeda;
    edInteiroPlural.Text := StrMoedas;
    edDecimalSingular.Text := StrCentavo;
    edDecimalPlural.Text := StrCentavos;
  end;
end;

end.

