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

unit GAVTeste1;

interface

uses
  SysUtils,
 {$IFDEF Delphi6_UP} Types, Variants, {$ELSE} Windows,{$ENDIF}
  Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ACBrBase, ACBrGAV, ACBrECF;

type
  TForm1 = class(TForm)
    btAbrir: TBitBtn;
    gbEstado: TGroupBox;
    lEstado: TLabel;
    cbxMonitorar: TCheckBox;
    btEstado: TBitBtn;
    BitBtn1: TBitBtn;
    Timer1: TTimer;
    ACBrGAV1: TACBrGAV;
    cbxModelo: TComboBox;
    Label1: TLabel;
    cbxPorta: TComboBox;
    Label2: TLabel;
    ACBrECF1: TACBrECF;
    SpeedButton1: TSpeedButton;
    procedure btAbrirClick(Sender: TObject);
    procedure cbxMonitorarClick(Sender: TObject);
    procedure btEstadoClick(Sender: TObject);
    procedure cbxModeloChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxPortaChange(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  cbxModelo.ItemIndex := 2 ;
  cbxModeloChange( Sender );
  cbxPortaChange( Sender );
end;

procedure TForm1.btAbrirClick(Sender: TObject);
begin
  btAbrir.Enabled := false ;

  try
     ACBrGAV1.AbreGaveta ;
     btEstadoClick(Sender);
  finally
     btAbrir.Enabled := True ;
  end ;
end;

procedure TForm1.cbxMonitorarClick(Sender: TObject);
begin
  Timer1.Enabled := cbxMonitorar.Checked ;
end;

procedure TForm1.btEstadoClick(Sender: TObject);
begin
  if not ACBrGAV1.Ativo then
     ACBrGAV1.Ativar ;

  if ACBrGAV1.GavetaAberta then
   begin
     lEstado.Font.Color := clGreen ;
     lEstado.Caption := 'A B E R T A' ;
   end
  else
   begin
     lEstado.Font.Color := clRed ;
     lEstado.Caption := 'F E C H A D A' ;
   end ;

end;

procedure TForm1.cbxModeloChange(Sender: TObject);
Var OldModelo : TACBrGAVModelo ;
begin
  ACBrGAV1.Desativar ;
  OldModelo := ACBrGAV1.Modelo ;

  try
     ACBrGAV1.Modelo := TACBrGAVModelo( cbxModelo.ItemIndex ) ;

     if ACBrGAV1.Modelo = gavImpressoraECF then
     begin
        ACBrGAV1.ECF := ACBrECF1 ;
        ACBrECF1.Ativar ;
     end ;
  except
     ACBrGAV1.Modelo := OldModelo ;
  end ;

  cbxModelo.ItemIndex := Integer( ACBrGAV1.Modelo ) ;
  cbxPorta.Text  := ACBrGAV1.Porta ;
end;

procedure TForm1.cbxPortaChange(Sender: TObject);
begin
  if ACBrGAV1.Modelo <> gavImpressoraECF then
  begin
    ACBrGAV1.Desativar ;
    ACBrGAV1.Porta := cbxPorta.Text ;
  end ;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  close ;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  ACBrECF1.Ativar ;
  ACBrECF1.TestarDialog ;
end;

end.
